use std::{
    collections::HashSet,
    env,
    ffi::{OsStr, OsString},
    fmt::{self, Display},
    fs::{self, OpenOptions, Permissions},
    io::{self, Write as _},
    mem,
    os::unix::fs::{OpenOptionsExt as _, PermissionsExt as _},
    path::{Path, PathBuf},
    process::Command,
};

use color_eyre::{eyre::Context as _, owo_colors::OwoColorize as _, Section as _, SectionExt as _};
use inquire::Select;
use onlyerror::Error;
use serde::{
    de::{Error as _, MapAccess, Visitor},
    Deserialize, Deserializer,
};
use tempfile::NamedTempFile;
use winnow::{
    ascii::escaped_transform,
    combinator::{alt, cut_err, delimited, eof, opt, preceded, repeat, trace},
    token::{take_till, take_until, take_while},
    PResult, Parser,
};

use crate::{
    debug, eyre, info,
    init::{Init, SpawnWindow, WindowCommand},
    trace, Result,
};

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("Error while reading the file")]
    FileReading(#[source] io::Error),
    #[error("Error while parsing the file")]
    FileParsing(#[source] toml::de::Error, String),
    #[error("Command cannot be empty")]
    EmptyCommand,
    #[error("Command contains a semicolon")]
    MultipleCommands,
    #[error("Failed to split command into shell arguments")]
    CommandLexing,
    #[error("The path is not a directory")]
    NotADirectory,
    #[error("Invalid window directory")]
    InvalidWindowDir(#[source] io::Error),
    #[error("Cannot create a new sessionizer config file")]
    CreateConfigFile(#[source] io::Error),
    #[error("No editor found")]
    NoEditor,
    #[error("Error while preparing the config file for editing")]
    PrepareEditing(#[source] io::Error),
    #[error("Editor command did not return success")]
    Editing,
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Error)]
pub enum EnvError {
    #[error("No value found for variable")]
    NotFound,
    #[error("Non UTF-8 value found for variable: {0}")]
    NotUtf8(String),
    #[error("Error executing command [{0}]: {1}")]
    Command(#[source] io::Error, String),
}

impl PartialEq for EnvError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NotFound, Self::NotFound) => true,
            (Self::NotUtf8(a), Self::NotUtf8(b)) => a == b,
            (Self::Command(a, _), Self::Command(b, _)) => a.kind() == b.kind(),
            _ => false,
        }
    }
}
impl Eq for EnvError {}

#[derive(Debug, Clone, Default)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Config {
    env: Vec<(String, EnvValue)>,
    windows: Vec<Window>,
    run: Vec<Run>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnvValue {
    Value(String),
    Dynamic(Vec<EnvVariable>),
    Command {
        program: OsString,
        args: Vec<OsString>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnvVariable {
    Value(String),
    Expand {
        key: String,
        default: Option<EnvValue>,
    },
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(test, derive(PartialEq, Eq))]
struct Window {
    name: Option<String>,
    dir: Option<PathBuf>,
    command: Option<String>,
    on_exit: Option<OnExit>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
enum OnExit {
    Destroy,
    Deactivate,
    Shell,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
struct Run {
    command: String,
}

impl Config {
    pub fn write_empty_config_in(file: &Path) -> Result<(), ConfigError> {
        let mut file = OpenOptions::new()
            .create_new(true)
            .write(true)
            .mode(0o600)
            .open(file)
            .map_err(ConfigError::CreateConfigFile)?;

        let content = include_bytes!("./empty-config.toml");
        file.write_all(content)
            .map_err(ConfigError::CreateConfigFile)?;

        file.flush().map_err(ConfigError::CreateConfigFile)?;
        drop(file);

        Ok(())
    }

    pub fn new_init_file(init_file: &str) -> Self {
        let command = format!("source ./{init_file}");
        Self {
            run: vec![Run { command }],
            ..Default::default()
        }
    }

    pub fn load_from_toml(file: &Path) -> Result<Self, ConfigError> {
        let config_raw = fs::read_to_string(file).map_err(ConfigError::FileReading)?;
        let config = toml::from_str::<Self>(&config_raw);

        debug!(?config, "Parsed config from file");

        config.map_err(|e| ConfigError::FileParsing(e, config_raw))
    }

    pub fn validate(self, root_dir: &Path) -> Result<Init> {
        self.validate_with(root_dir, |k| env::var(k).is_ok())
    }

    fn validate_with(self, root_dir: &Path, root_env: impl Fn(&str) -> bool) -> Result<Init> {
        let run = self
            .run
            .into_iter()
            .enumerate()
            .map(|(idx, run)| {
                let _ = Self::validate_command("Run", idx, &run.command)?;
                Ok(run.command)
            })
            .collect::<Result<_>>()?;

        let windows = self
            .windows
            .into_iter()
            .enumerate()
            .map(|(idx, window)| Self::validate_config_window(root_dir, idx, window))
            .collect::<Result<_>>()?;

        let mut new_keys = HashSet::new();
        self.env.iter().try_for_each(|(k, v)| {
            v.validate(&|k| new_keys.contains(k) || root_env(k))
                .map(|()| {
                    let _ = new_keys.insert(k.as_str());
                })
        })?;

        let action = Init {
            env: self.env,
            run,
            windows,
        };
        Ok(action)
    }

    fn validate_command(section: &str, idx: usize, cmd: &str) -> Result<Vec<String>> {
        if cmd.trim().is_empty() {
            return Err(eyre!(ConfigError::EmptyCommand)
                .with_section(|| format!("{section}[{idx}]").header("Location"))
                .suggestion("To run nothing, remove the `command` entry entirely"));
        }
        if cmd.contains(';') {
            return Err(eyre!(ConfigError::MultipleCommands)
                .with_section(|| format!("{section}[{idx}]").header("Location"))
                .with_section(|| cmd.to_owned().header("Command"))
                .note("Only a single command can be run.")
                .suggestion(concat!(
                    "To run multiple commands, use a shell script ",
                    "or similar and run that one instead."
                )));
        }

        let cmd = shlex::split(cmd).ok_or_else(|| {
            eyre!(ConfigError::CommandLexing)
                .with_section(|| format!("{section}[{idx}]").header("Location"))
                .with_section(|| cmd.to_owned().header("Command"))
                .note(concat!(
                    "The command might end while inside a quotation ",
                    "or right after an unescaped backslash."
                ))
        })?;

        assert!(!cmd.is_empty(), "shlex::split returned an empty command");

        Ok(cmd)
    }

    fn validate_config_window(root_dir: &Path, idx: usize, window: Window) -> Result<SpawnWindow> {
        let command = window
            .command
            .map(|cmd| -> Result<_> {
                let command = Self::validate_command("Window.command", idx, &cmd)?;
                let remain = match window.on_exit {
                    Some(OnExit::Shell) => {
                        return Ok(WindowCommand::Run {
                            run: cmd,
                            name: command
                                .into_iter()
                                .next()
                                .expect("validate returns a non-empty command"),
                        });
                    }
                    Some(OnExit::Destroy) => Some(false),
                    Some(OnExit::Deactivate) => Some(true),
                    None => None,
                };
                Ok(WindowCommand::Command { command, remain })
            })
            .transpose()?;

        let name = window
            .name
            .as_deref()
            .or_else(|| command.as_ref().map(WindowCommand::first))
            .map(str::trim)
            .filter(|o| !o.is_empty())
            .map_or_else(|| format!("Window[{idx}]"), ToOwned::to_owned);

        let mut dir = window.dir;
        if let Some(dir) = dir.as_mut() {
            trace!(dir = %dir.display(), "Attemting to use dir as base for new window");

            if dir.is_relative() {
                *dir = root_dir.join(dir.as_path());
            }

            *dir = dir.canonicalize().map_err(|e| {
                eyre!(ConfigError::InvalidWindowDir(e))
                    .with_section(|| format!("Window[{idx}]").header("Location"))
                    .with_section(|| dir.display().to_string().header("Directory"))
                    .note("The path might not exist or contain non-directory segments")
            })?;

            if !dir.is_dir() {
                return Err(eyre!(ConfigError::NotADirectory)
                    .with_section(|| format!("Window[{idx}]").header("Location"))
                    .with_section(|| dir.display().to_string().header("Directory")));
            }

            trace!(dir = %dir.display(), "Using dir as base for new window");
        }

        let window = SpawnWindow { name, dir, command };

        Ok(window)
    }

    pub fn edit_config_file_in(
        config_file: &Path,
        validate: impl FnMut(&Path) -> Result<()>,
    ) -> Result<()> {
        let editor = env::var_os("VISUAL")
            .or_else(|| env::var_os("EDITOR"))
            .ok_or(ConfigError::NoEditor)
            .suggestion("Define either $VISUAL or $EDITOR")?;

        let tmp =
            Self::copy_config_file_to_tmp(config_file).map_err(ConfigError::PrepareEditing)?;
        let last_modified = tmp.path().metadata().and_then(|m| m.modified()).ok();

        let valid = Self::edit_tmp_config_file(tmp.path(), &editor, validate)?;
        if valid {
            if let Some(last_modified) = last_modified {
                let now_modified = tmp.path().metadata().and_then(|m| m.modified()).ok();
                if now_modified.is_some_and(|m| m <= last_modified) {
                    info!("No changes were made to the configuration file");
                    return Ok(());
                }
            }

            let mut tmp = tmp
                .persist(config_file)
                .map_err(|e| ConfigError::PrepareEditing(e.error))?;
            tmp.flush().map_err(ConfigError::PrepareEditing)?;
            drop(tmp);
        }

        Ok(())
    }

    fn copy_config_file_to_tmp(file: &Path) -> Result<NamedTempFile, io::Error> {
        let dir = file
            .parent()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "The path is not a file"))?;

        let content = fs::read(file)?;

        let mut tmp = tempfile::Builder::new()
            .permissions(Permissions::from_mode(0o600))
            .suffix(".toml")
            .tempfile_in(dir)?;
        tmp.write_all(&content)?;
        tmp.flush()?;

        Ok(tmp)
    }

    fn edit_tmp_config_file(
        path: &Path,
        editor: &OsStr,
        mut validate: impl FnMut(&Path) -> Result<()>,
    ) -> Result<bool> {
        enum Choices {
            EditAgain,
            ExitWithoutSaving,
        }

        impl Display for Choices {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::EditAgain => f.write_str("Edit the file again"),
                    Self::ExitWithoutSaving => f.write_str("Exit without saving changes"),
                }
            }
        }

        let mut line_num = None;

        'editing: loop {
            let mut cmd = Command::new(editor);
            let _ = cmd.arg(path);
            if let Some(line_num) = line_num {
                let _ = cmd.arg(format!("+{line_num}"));
            }

            trace!(?cmd, "Running editor");

            let s = cmd.status().map_err(ConfigError::PrepareEditing)?;
            if !s.success() {
                return Err(eyre!(ConfigError::Editing));
            }

            match validate(path) {
                Ok(()) => return Ok(true),
                Err(e) => {
                    debug!(%e, "Config validation failed");

                    eprintln!();
                    eprintln!("New config did not validate");
                    eprintln!();

                    line_num = if let Some(ConfigError::FileParsing(toml_error, content)) =
                        dbg!(dbg!(&e).downcast_ref::<ConfigError>())
                    {
                        eprintln!("{}", toml_error.bright_red());
                        toml_error.span().map(|span| {
                            if content.is_empty() {
                                return 0;
                            }
                            let input = content.as_bytes();
                            let index = span.start;

                            let safe_index = index.min(input.len() - 1);
                            let index = safe_index;

                            let nl = input[0..index]
                                .iter()
                                .rev()
                                .enumerate()
                                .find(|(_, b)| **b == b'\n')
                                .map(|(nl, _)| index - nl - 1);
                            let line_start = nl.map_or(0, |nl| nl + 1);

                            let line = bytecount::count(&input[0..line_start], b'\n');
                            line + 1
                        })
                    } else {
                        eprintln!("{e:?}");
                        None
                    };

                    eprintln!();

                    let mut esc_count = 0;

                    'selection: loop {
                        let sel = Select::new(
                            "What now?\n",
                            vec![Choices::EditAgain, Choices::ExitWithoutSaving],
                        )
                        .with_vim_mode(true)
                        .without_filtering()
                        .without_help_message();

                        let sel = match esc_count {
                            4 => sel.with_help_message(
                                "↑↓/jk to move, enter to select, ESC to exit without saving",
                            ),
                            3 => sel.with_help_message(
                                "↑↓/jk to move, enter to select, ESC+ESC to exit without saving",
                            ),
                            1..=2 => sel.with_help_message("↑↓/jk to move, enter to select"),
                            _ => sel,
                        };

                        match sel.prompt() {
                            Ok(Choices::EditAgain) => continue 'editing,
                            Err(inquire::InquireError::OperationCanceled) => {
                                esc_count += 1;
                                if esc_count >= 5 {
                                    return Ok(false);
                                }
                                continue 'selection;
                            }
                            Ok(Choices::ExitWithoutSaving) => return Ok(false),
                            Err(e) => return Err(eyre!(ConfigError::Editing)).context(e),
                        }
                    }
                }
            }
        }
    }
}

impl EnvValue {
    fn validate(&self, has_key: &impl Fn(&str) -> bool) -> Result<(), EnvError> {
        match self {
            Self::Value(..) => Ok(()),
            Self::Command { .. } => {
                // TODO: check that the command exists (which)
                Ok(())
            }
            Self::Dynamic(items) => {
                for item in items {
                    match item {
                        EnvVariable::Value(..) => {}
                        EnvVariable::Expand { key, default } => {
                            if !has_key(key) {
                                return Err(EnvError::NotFound);
                            }
                            if let Some(default) = default {
                                default.validate(has_key)?;
                            }
                        }
                    }
                }
                Ok(())
            }
        }
    }

    pub fn resolve<'env, E>(
        self,
        get_env: &impl Fn(&str) -> Result<String, env::VarError>,
        add_env: &'env E,
    ) -> Result<String, EnvError>
    where
        &'env E: IntoIterator<Item = (&'env String, &'env String)>,
    {
        match self {
            Self::Value(v) => Ok(v),
            Self::Dynamic(parts) => parts
                .into_iter()
                .map(|part| match part {
                    EnvVariable::Value(v) => Ok(v),
                    EnvVariable::Expand { key, default } => match get_env(&key) {
                        Ok(v) => Ok(v),
                        Err(env::VarError::NotPresent) => default
                            .map_or(Err(EnvError::NotFound), |default| {
                                default.resolve(get_env, add_env)
                            }),
                        Err(env::VarError::NotUnicode(v)) => {
                            Err(EnvError::NotUtf8(v.to_string_lossy().into_owned()))
                        }
                    },
                })
                .collect::<Result<String, _>>(),
            Self::Command { program, args } => {
                let output = Command::new(program)
                    .args(args)
                    .envs(add_env.into_iter())
                    .output()
                    .map_err(|e| EnvError::Command(e, String::new()))?;
                if !output.status.success() {
                    return Err(EnvError::Command(
                        io::Error::new(io::ErrorKind::Other, "Command failed"),
                        String::from_utf8_lossy(&output.stderr).into_owned(),
                    ));
                }
                String::from_utf8(output.stdout).map_err(|e| {
                    EnvError::NotUtf8(String::from_utf8_lossy(&e.into_bytes()).into_owned())
                })
            }
        }
    }
}

impl EnvValue {
    pub fn parse(s: &str) -> Result<Self> {
        Self::parse_root.parse(s).map_err(|e| eyre!(e.to_string()))
    }

    fn parse_root(input: &mut &str) -> PResult<Self> {
        trace(
            "root",
            alt((
                delimited("$(", Self::parse_command, ")"),
                (Self::parse_value, eof).map(|(res, _)| Self::Value(res)),
                Self::parse_expand,
            )),
        )
        .parse_next(input)
    }

    fn parse_top(input: &mut &str) -> PResult<Self> {
        trace(
            "top",
            alt((
                delimited("$(", Self::parse_command, ")"),
                (Self::parse_value).map(Self::Value),
                Self::parse_expand,
            )),
        )
        .parse_next(input)
    }

    fn parse_command(input: &mut &str) -> PResult<Self> {
        trace(
            "command",
            take_until(1.., ")").verify_map(|cmd| {
                let (program, args) = Self::split_cmd(cmd)?;
                Some(Self::Command { program, args })
            }),
        )
        .parse_next(input)
    }

    fn split_cmd(input: &str) -> Option<(OsString, Vec<OsString>)> {
        let mut shl = shlex::Shlex::new(input);
        let mut args = shl.by_ref().map(OsString::from).collect::<Vec<_>>();
        if shl.had_error || args.is_empty() {
            return None;
        }
        let program = args.remove(0);
        Some((program, args))
    }

    fn parse_expand(input: &mut &str) -> PResult<Self> {
        repeat(1.., Self::parse_expansion)
            .map(Self::Dynamic)
            .parse_next(input)
    }

    fn parse_expansion(input: &mut &str) -> PResult<EnvVariable> {
        trace(
            "expansion",
            alt((
                delimited(
                    "${",
                    trace(
                        "brace_expand",
                        cut_err((Self::parse_key, Self::parse_default)).map(|(k, d)| {
                            EnvVariable::Expand {
                                key: k.to_owned(),
                                default: d,
                            }
                        }),
                    ),
                    "}",
                ),
                preceded(
                    "$",
                    trace(
                        "inline_expand",
                        cut_err(Self::parse_key).map(|k| EnvVariable::Expand {
                            key: k.to_owned(),
                            default: None,
                        }),
                    ),
                ),
                Self::parse_value.map(EnvVariable::Value),
            )),
        )
        .parse_next(input)
    }

    fn parse_default(input: &mut &str) -> PResult<Option<Self>> {
        trace("default", opt(preceded(":-", cut_err(Self::parse_top)))).parse_next(input)
    }

    fn parse_key<'s>(input: &mut &'s str) -> PResult<&'s str> {
        trace(
            "key",
            (
                take_while(1.., |c: char| c.is_ascii_alphabetic() || c == '_'),
                take_while(0.., |c: char| c.is_ascii_alphanumeric() || c == '_'),
            )
                .recognize(),
        )
        .parse_next(input)
    }

    fn parse_value(input: &mut &str) -> PResult<String> {
        trace(
            "value",
            escaped_transform(
                take_till(1.., |c| c == '$' || c == '\\' || c == '{' || c == '}'),
                '\\',
                alt((
                    '\\'.value("\\"),
                    '$'.value("$"),
                    '{'.value("{"),
                    '}'.value("}"),
                    'n'.value("\n"),
                    'r'.value("\r"),
                    't'.value("\t"),
                )),
            )
            .verify(|value: &str| !value.is_empty()),
        )
        .parse_next(input)
    }
}

impl<'de> Deserialize<'de> for Config {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &[&str] = &["env", "window", "windows", "run"];

        struct Vis;
        impl<'de> Visitor<'de> for Vis {
            type Value = Config;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Formatter::write_str(formatter, "struct Config")
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut config = Config::default();
                let mut free = [true; 3];

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "env" => {
                            if !mem::take(&mut free[0]) {
                                return Err(A::Error::duplicate_field("env"));
                            }
                            config.env = map.next_value::<EnvSection>()?.0;
                        }
                        "window" | "windows" => {
                            if !mem::take(&mut free[1]) {
                                return Err(A::Error::duplicate_field("windows"));
                            }
                            config.windows = map.next_value()?;
                        }
                        "run" => {
                            if !mem::take(&mut free[2]) {
                                return Err(A::Error::duplicate_field("run"));
                            }
                            config.run = map.next_value()?;
                        }
                        otherwise => return Err(A::Error::unknown_field(otherwise, FIELDS)),
                    }
                }

                Ok(config)
            }
        }

        deserializer.deserialize_struct("Config", FIELDS, Vis)
    }
}

impl<'de> Deserialize<'de> for Window {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &[&str] = &[
            "name",
            "cwd",
            "dir",
            "path",
            "pwd",
            "wd",
            "workdir",
            "cmd",
            "command",
            "run",
            "keep-alive",
            "on-exit",
            "remain",
        ];

        struct Vis;

        impl<'de> Visitor<'de> for Vis {
            type Value = Window;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Formatter::write_str(formatter, "struct Window")
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut window = Window::default();
                let mut free = [true; 4];

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "name" => {
                            if !mem::take(&mut free[0]) {
                                return Err(A::Error::duplicate_field("name"));
                            }
                            window.name = map.next_value()?;
                        }
                        "cwd" | "dir" | "path" | "pwd" | "wd" | "workdir" => {
                            if !mem::take(&mut free[1]) {
                                return Err(A::Error::duplicate_field("dir"));
                            }
                            window.dir = map.next_value()?;
                        }
                        "cmd" | "command" | "run" => {
                            if !mem::take(&mut free[2]) {
                                return Err(A::Error::duplicate_field("command"));
                            }
                            window.command = map.next_value()?;
                        }
                        "keep-alive" | "on-exit" | "remain" => {
                            if !mem::take(&mut free[3]) {
                                return Err(A::Error::duplicate_field("on-exit"));
                            }
                            window.on_exit = map.next_value()?;
                        }
                        otherwise => return Err(A::Error::unknown_field(otherwise, FIELDS)),
                    }
                }

                Ok(window)
            }
        }

        deserializer.deserialize_struct("Window", FIELDS, Vis)
    }
}

impl<'de> Deserialize<'de> for Run {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const FIELDS: &[&str] = &["cmd", "command", "run"];

        struct Vis;
        impl<'de> Visitor<'de> for Vis {
            type Value = Run;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Formatter::write_str(formatter, "struct Run")
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut command = None;
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "cmd" | "command" | "run" => {
                            if command.is_some() {
                                return Err(A::Error::duplicate_field("command"));
                            }
                            command = Some(map.next_value()?);
                        }
                        otherwise => return Err(A::Error::unknown_field(otherwise, FIELDS)),
                    }
                }
                let command = command.ok_or_else(|| A::Error::missing_field("command"))?;
                Ok(Run { command })
            }
        }

        deserializer.deserialize_struct("Run", FIELDS, Vis)
    }
}

impl<'de> Deserialize<'de> for OnExit {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const VARIANTS: &[&str] = &["destroy", "deactivate", "shell"];

        struct OnExitVisitor;
        impl<'de> Visitor<'de> for OnExitVisitor {
            type Value = OnExit;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str(concat!(
                    "a valid `on-exit` value: One of ",
                    r#"One of "destroy" or `true`, "deactivate", or "shell" or `false`"#
                ))
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v {
                    Ok(OnExit::Shell)
                } else {
                    Ok(OnExit::Destroy)
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "destroy" | "kill" => Ok(OnExit::Destroy),
                    "deactivate" | "inactive" | "remain" => Ok(OnExit::Deactivate),
                    "keep" | "shell" | "stay" => Ok(OnExit::Shell),
                    _ => Err(serde::de::Error::unknown_variant(v, VARIANTS)),
                }
            }
        }

        deserializer.deserialize_any(OnExitVisitor)
    }
}

impl<'de> Deserialize<'de> for EnvValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vis;
        impl<'de> Visitor<'de> for Vis {
            type Value = EnvValue;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a valid `env` value")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                EnvValue::parse(v).map_err(|e| E::custom(format!("Invalid env value: {e}")))
            }
        }

        deserializer.deserialize_str(Vis)
    }
}

struct EnvSection(Vec<(String, EnvValue)>);

impl<'de> Deserialize<'de> for EnvSection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vis;
        impl<'de> Visitor<'de> for Vis {
            type Value = EnvSection;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a valid `env` section")
            }

            fn visit_map<A>(self, mut map: A) -> std::result::Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut env = Vec::new();
                while let Some((key, value)) = map.next_entry()? {
                    env.push((key, value));
                }
                Ok(EnvSection(env))
            }
        }

        deserializer.deserialize_map(Vis)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_env_value() {
        test_parse_env_value("foo", "foo");
        test_parse_env_value("   foo   ", "   foo   ");
        test_parse_env_value("foo bar", "foo bar");
        test_parse_env_value("   'foo' \"bar\"   ", "   'foo' \"bar\"   ");
    }

    #[test]
    fn parse_env_value_escaped() {
        test_parse_env_value(r"\$FOO", r"$FOO");
        test_parse_env_value(r"\\FOO", r"\FOO");
        test_parse_env_value(r"\{FOO", r"{FOO");
        test_parse_env_value(r"\}FOO", r"}FOO");
        test_parse_env_value(r"\rFOO", "\rFOO");
        test_parse_env_value(r"\tFOO", "\tFOO");
        test_parse_env_value(r"\nFOO", "\nFOO");
    }

    #[test]
    fn parse_empty_input() {
        let value = EnvValue::parse("");
        assert!(value.is_err());
    }

    fn test_parse_env_value(input: &str, expected: &str) {
        let value = EnvValue::parse(input).unwrap();
        assert_eq!(value, EnvValue::Value(expected.to_owned()));
    }

    #[test]
    fn parse_env_expansion() {
        let value = EnvValue::parse("${FOO}").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![EnvVariable::Expand {
                key: "FOO".to_owned(),
                default: None,
            }])
        );
    }

    #[test]
    fn parse_env_expansion_with_default() {
        let value = EnvValue::parse("${FOO:-bar}").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![EnvVariable::Expand {
                key: "FOO".to_owned(),
                default: Some(EnvValue::Value("bar".to_owned())),
            }])
        );
    }

    #[test]
    fn parse_env_expansion_default_other_var() {
        let value = EnvValue::parse("${FOO:-$BAR}").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![EnvVariable::Expand {
                key: "FOO".to_owned(),
                default: Some(EnvValue::Dynamic(vec![EnvVariable::Expand {
                    key: "BAR".to_owned(),
                    default: None
                }])),
            }])
        );
    }

    #[test]
    fn parse_env_expansion_no_braces() {
        let value = EnvValue::parse("$FOO").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![EnvVariable::Expand {
                key: "FOO".to_owned(),
                default: None,
            }])
        );
    }

    #[test]
    fn parse_env_expansion_mixed() {
        let value = EnvValue::parse("${FOO}bar${BAZ}").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![
                EnvVariable::Expand {
                    key: "FOO".to_owned(),
                    default: None,
                },
                EnvVariable::Value("bar".to_owned()),
                EnvVariable::Expand {
                    key: "BAZ".to_owned(),
                    default: None,
                },
            ])
        );
    }

    #[test]
    fn parse_env_expansion_dollar_escaped() {
        let value = EnvValue::parse(r"\$FOO").unwrap();
        assert_eq!(value, EnvValue::Value("$FOO".to_owned()));
    }

    #[test]
    fn parse_env_expansion_backslash_escaped() {
        let value = EnvValue::parse(r"\\$FOO").unwrap();
        assert_eq!(
            value,
            EnvValue::Dynamic(vec![
                EnvVariable::Value("\\".to_owned()),
                EnvVariable::Expand {
                    key: "FOO".to_owned(),
                    default: None
                }
            ])
        );
    }

    #[test]
    fn parse_env_value_command() {
        let value = EnvValue::parse("$(echo foo)").unwrap();
        assert_eq!(
            value,
            EnvValue::Command {
                program: "echo".into(),
                args: vec!["foo".into()]
            }
        );
    }

    #[test]
    fn parse_env_value_command_with_args() {
        let value = EnvValue::parse("$(echo foo bar)").unwrap();
        assert_eq!(
            value,
            EnvValue::Command {
                program: "echo".into(),
                args: vec!["foo".into(), "bar".into()]
            }
        );
    }

    #[test]
    fn parse_env_value_command_with_single_quotes() {
        let value = EnvValue::parse("$(echo 'foo bar')").unwrap();
        assert_eq!(
            value,
            EnvValue::Command {
                program: "echo".into(),
                args: vec!["foo bar".into()]
            }
        );
    }

    #[test]
    fn parse_env_value_command_with_double_quotes() {
        let value = EnvValue::parse(r#"$(echo "foo bar")"#).unwrap();
        assert_eq!(
            value,
            EnvValue::Command {
                program: "echo".into(),
                args: vec!["foo bar".into()]
            }
        );
    }

    #[test]
    fn parse_env_value_command_with_escaped_double_quotes() {
        let value = EnvValue::parse(r#"$(echo "foo \"bar\"")"#).unwrap();
        assert_eq!(
            value,
            EnvValue::Command {
                program: "echo".into(),
                args: vec!["foo \"bar\"".into()]
            }
        );
    }

    #[test]
    fn toml_format() {
        let config = r#"
            [env]
            FOO = "bar"
            BAZ = "qux"

            [[run]]
            cmd = "echo hello"

            [[window]]
            name = "test"
            pwd = "dir"
            cmd = "echo world"
            on-exit = "stay"
        "#;

        let config = toml::from_str::<Config>(config).unwrap();
        assert_eq!(
            config,
            Config {
                env: vec![
                    ("FOO".to_owned(), EnvValue::Value("bar".to_owned())),
                    ("BAZ".to_owned(), EnvValue::Value("qux".to_owned())),
                ],
                run: vec![Run {
                    command: "echo hello".to_owned()
                }],
                windows: vec![Window {
                    name: Some("test".to_owned()),
                    dir: Some("dir".into()),
                    command: Some("echo world".to_owned()),
                    on_exit: Some(OnExit::Shell),
                }],
            }
        );
    }

    fn test_run_aliases(cmd: &str) {
        let config = format!(
            r#"
                    [[run]]
                    {cmd} = "echo hello"
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                run: vec![Run {
                    command: "echo hello".to_owned()
                }],
                ..Default::default()
            }
        );
    }

    #[test]
    fn run_aliases_cmd() {
        test_run_aliases("cmd");
    }

    #[test]
    fn run_aliases_run() {
        test_run_aliases("run");
    }

    #[test]
    fn run_aliases_command() {
        test_run_aliases("command");
    }

    fn test_window_aliases(window: &str) {
        let config = format!(
            r#"
                    [[{window}]]
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                windows: vec![Window::default()],
                ..Default::default()
            }
        );
    }

    #[test]
    fn window_aliases_windows() {
        test_window_aliases("windows");
    }

    #[test]
    fn window_aliases_window() {
        test_window_aliases("window");
    }

    fn test_window_run_aliases(cmd: &str) {
        let config = format!(
            r#"
                    [[window]]
                    {cmd} = "echo world"
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                windows: vec![Window {
                    command: Some("echo world".to_owned()),
                    ..Default::default()
                }],
                ..Default::default()
            }
        );
    }

    #[test]
    fn window_run_aliases_cmd() {
        test_window_run_aliases("cmd");
    }

    #[test]
    fn window_run_aliases_run() {
        test_window_run_aliases("run");
    }

    #[test]
    fn window_run_aliases_command() {
        test_window_run_aliases("command");
    }

    fn test_window_dir_aliases(dir: &str) {
        let config = format!(
            r#"
                    [[window]]
                    {dir} = "dir"
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                windows: vec![Window {
                    dir: Some("dir".into()),
                    ..Default::default()
                }],
                ..Default::default()
            }
        );
    }

    #[test]
    fn window_dir_aliases_path() {
        test_window_dir_aliases("path");
    }

    #[test]
    fn window_dir_aliases_workdir() {
        test_window_dir_aliases("workdir");
    }

    #[test]
    fn window_dir_aliases_wd() {
        test_window_dir_aliases("wd");
    }

    #[test]
    fn window_dir_aliases_pwd() {
        test_window_dir_aliases("pwd");
    }

    #[test]
    fn window_dir_aliases_cwd() {
        test_window_dir_aliases("cwd");
    }

    #[test]
    fn window_dir_aliases_dir() {
        test_window_dir_aliases("dir");
    }

    fn test_window_on_exit_aliases(on_exit: &str) {
        let config = format!(
            r#"
                    [[window]]
                    {on_exit} = "stay"
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                windows: vec![Window {
                    on_exit: Some(OnExit::Shell),
                    ..Default::default()
                }],
                ..Default::default()
            }
        );
    }

    #[test]
    fn window_on_exit_aliases_keep_alive() {
        test_window_on_exit_aliases("keep-alive");
    }

    #[test]
    fn window_on_exit_aliases_remain() {
        test_window_on_exit_aliases("remain");
    }

    #[test]
    fn window_on_exit_aliases_on_exit() {
        test_window_on_exit_aliases("on-exit");
    }

    fn test_window_on_exit_values(on_exit: &str, expected: OnExit) {
        let config = format!(
            r#"
                    [[window]]
                    on-exit = {on_exit}
                "#,
        );

        let config = toml::from_str::<Config>(&config).unwrap();
        assert_eq!(
            config,
            Config {
                windows: vec![Window {
                    on_exit: Some(expected),
                    ..Default::default()
                }],
                ..Default::default()
            }
        );
    }

    #[test]
    fn window_on_exit_values_kill_to_destroy() {
        test_window_on_exit_values(r#""kill""#, OnExit::Destroy);
    }

    #[test]
    fn window_on_exit_values_destroy_to_destroy() {
        test_window_on_exit_values(r#""destroy""#, OnExit::Destroy);
    }

    #[test]
    fn window_on_exit_values_false_to_destroy() {
        test_window_on_exit_values(r"false", OnExit::Destroy);
    }

    #[test]
    fn window_on_exit_values_inactive_to_deactivate() {
        test_window_on_exit_values(r#""inactive""#, OnExit::Deactivate);
    }

    #[test]
    fn window_on_exit_values_remain_to_deactivate() {
        test_window_on_exit_values(r#""remain""#, OnExit::Deactivate);
    }

    #[test]
    fn window_on_exit_values_deactivate_to_deactivate() {
        test_window_on_exit_values(r#""deactivate""#, OnExit::Deactivate);
    }

    #[test]
    fn window_on_exit_values_stay_to_shell() {
        test_window_on_exit_values(r#""stay""#, OnExit::Shell);
    }

    #[test]
    fn window_on_exit_values_keep_to_shell() {
        test_window_on_exit_values(r#""keep""#, OnExit::Shell);
    }

    #[test]
    fn window_on_exit_values_shell_to_shell() {
        test_window_on_exit_values(r#""shell""#, OnExit::Shell);
    }

    #[test]
    fn window_on_exit_values_true_to_shell() {
        test_window_on_exit_values(r"true", OnExit::Shell);
    }
}
