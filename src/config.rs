use std::{
    env,
    ffi::OsStr,
    fmt::{self, Display},
    fs::{self, OpenOptions, Permissions},
    io::{self, Write as _},
    mem,
    os::unix::fs::{OpenOptionsExt as _, PermissionsExt as _},
    path::Path,
    path::PathBuf,
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

#[derive(Debug, Clone, Default)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Config {
    env: indexmap::IndexMap<String, String>,
    windows: Vec<Window>,
    run: Vec<Run>,
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

        let env = self.env.into_iter().collect();

        let action = Init { env, run, windows };
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
                            config.env = map.next_value()?;
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

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;

    use super::*;

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
                env: IndexMap::from_iter([
                    ("FOO".to_owned(), "bar".to_owned()),
                    ("BAZ".to_owned(), "qux".to_owned()),
                ]),
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
