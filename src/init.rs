use std::{
    env,
    ffi::OsStr,
    fmt::{self, Display},
    fs::{self, Metadata, OpenOptions, Permissions},
    io::{self, Write as _},
    os::unix::fs::{MetadataExt as _, OpenOptionsExt as _, PermissionsExt as _},
    path::{Path, PathBuf},
    process::Command,
};

use inquire::Select;
use kommandozeile::color_eyre::{
    eyre::{eyre, Context as _},
    owo_colors::OwoColorize as _,
    Section as _, SectionExt as _,
};
use onlyerror::Error;
use serde::{Deserialize, Deserializer};
use tempfile::NamedTempFile;

use crate::{debug, trace, Result};

#[derive(Debug, Clone, Default)]
pub struct Init {
    pub env: Vec<(String, String)>,
    pub run: Vec<String>,
    pub windows: Vec<SpawnWindow>,
}

#[derive(Debug, Clone)]
pub struct SpawnWindow {
    pub name: String,
    pub dir: Option<PathBuf>,
    pub command: Option<WindowCommand>,
}

#[derive(Debug, Clone)]
pub enum WindowCommand {
    /// A command takes over the window. Only that command is run
    /// and the window is closed when the command exits (depending
    /// on the tmux configuration).
    /// If remain is set to Some(true) or tmux is configured to set
    /// remain-on-exit to on, the window will remain open after the
    /// command exits.
    Command {
        command: Vec<String>,
        remain: Option<bool>,
    },
    /// Running a command as if it was typed in the window.
    Run { run: String, name: String },
}

impl WindowCommand {
    pub fn first(&self) -> &str {
        match self {
            Self::Command { command, .. } => command[0].as_str(),
            Self::Run { name, .. } => name.as_str(),
        }
    }
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Error)]
pub enum InitError {
    #[error("File is not readable")]
    FileNotReadable,
    #[error("File can be written or executed by others")]
    PermissionsTooOpen,
    #[error("File is not readable or executable")]
    FileNotReadableOrExecutable,
    #[error("Error while reading the file")]
    FileReading(#[source] io::Error),
    #[error("Error while parsing the file")]
    FileParsing(#[source] toml::de::Error, String),
    #[error("The current working directory does not exist or cannot be accessed")]
    InvalidCwd(#[source] io::Error),
    #[error("Cannot create a new sessionizer config file")]
    CreateConfigFile(#[source] io::Error),
    #[error("No sessionizer config file found")]
    NoConfigFile,
    #[error("No editor found")]
    NoEditor,
    #[error("Error while preparing the config file for editing")]
    PrepareEditing(#[source] io::Error),
    #[error("Editor command did not return success")]
    Editing,
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
}

pub fn create_config_file() -> Result<(), InitError> {
    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;
    write_empty_config_in(&cwd)
}

pub fn edit_config_file(secure: bool) -> Result<()> {
    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;
    edit_config_file_in(&cwd, secure)
}

pub fn validate_config_file(secure: bool) -> Result<()> {
    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;
    validate_config_file_in(&cwd, secure)
}

fn write_empty_config_in(dir: &Path) -> Result<(), InitError> {
    const INIT_FILE: &str = concat!(".", env!("CARGO_PKG_NAME"), ".toml");

    let mut file = OpenOptions::new()
        .create_new(true)
        .write(true)
        .mode(0o600)
        .open(dir.join(INIT_FILE))
        .map_err(InitError::CreateConfigFile)?;

    let content = include_bytes!("./empty-config.toml");
    file.write_all(content)
        .map_err(InitError::CreateConfigFile)?;

    file.flush().map_err(InitError::CreateConfigFile)?;
    drop(file);

    Ok(())
}

pub fn find_action(dir: &Path, secure: bool) -> Option<Result<Init>> {
    let init = find_init_definition(dir)?;
    let init = validated_init_file(dir, secure, &init);
    Some(init)
}

fn validate_config_file_in(dir: &Path, secure: bool) -> Result<()> {
    let _ = find_action(dir, secure)
        .ok_or(InitError::NoConfigFile)
        .suggestion(concat!(
            "Run `",
            env!("CARGO_PKG_NAME"),
            " --config init` to create an empty config"
        ))??;

    Ok(())
}

fn edit_config_file_in(dir: &Path, secure: bool) -> Result<()> {
    let file = find_init_definition(dir)
        .ok_or(InitError::NoConfigFile)
        .suggestion(concat!(
            "Run `",
            env!("CARGO_PKG_NAME"),
            " --config init` to create an empty config"
        ))?;

    let editor = env::var_os("VISUAL")
        .or_else(|| env::var_os("EDITOR"))
        .ok_or(InitError::NoEditor)
        .suggestion("Define either $VISUAL or $EDITOR")?;

    let tmp = copy_config_file_to_tmp(dir, &file.path).map_err(InitError::PrepareEditing)?;

    let valid = edit_tmp_config_file(tmp.path(), &editor, &file, dir)?;
    if valid {
        let mut tmp = tmp
            .persist(file.path)
            .map_err(|e| InitError::PrepareEditing(e.error))?;
        tmp.flush().map_err(InitError::PrepareEditing)?;
        drop(tmp);

        validate_config_file_in(dir, secure)?;
    }

    Ok(())
}

fn copy_config_file_to_tmp(dir: &Path, file: &Path) -> Result<NamedTempFile, io::Error> {
    let content = fs::read(file)?;

    let mut tmp = tempfile::Builder::new()
        .permissions(Permissions::from_mode(0o600))
        .suffix(".toml")
        .tempfile_in(dir)?;
    tmp.write_all(&content)?;
    tmp.flush()?;

    Ok(tmp)
}

fn edit_tmp_config_file(path: &Path, editor: &OsStr, init: &InitFile, dir: &Path) -> Result<bool> {
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

    'editing: loop {
        let mut cmd = Command::new(editor);
        let _ = cmd.arg(path);
        trace!(?cmd, "Running editor");

        let s = cmd.status().map_err(InitError::PrepareEditing)?;
        if !s.success() {
            return Err(eyre!(InitError::Editing));
        }

        let tmp_file = InitFile {
            path: path.to_path_buf(),
            file: init.file,
            kind: init.kind,
            metadata: init.metadata.clone(),
        };

        match validated_init_file(dir, false, &tmp_file) {
            Ok(_) => return Ok(true),
            Err(e) => {
                debug!(%e, "Config validation failed");

                eprintln!();
                eprintln!("New config did not validate");
                eprintln!();

                if let Some(InitError::FileParsing(toml_error, _content)) =
                    e.downcast_ref::<InitError>()
                {
                    eprintln!("{}", toml_error.bright_red());
                } else {
                    eprintln!("{e:?}");
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
                        Err(e) => return Err(eyre!(InitError::Editing)).context(e),
                    }
                }
            }
        }
    }
}

fn validated_init_file(dir: &Path, secure: bool, init: &InitFile) -> Result<Init> {
    let config = load_definition(secure, init).context(concat!(
        "Failed to load ",
        env!("CARGO_PKG_NAME"),
        "init file"
    ))?;
    let init = validate_config(dir, config)?;
    Ok(init)
}

fn find_init_definition(dir: &Path) -> Option<InitFile> {
    const CONFIG_FILES: [&str; 4] = [
        concat!(".", env!("CARGO_PKG_NAME"), ".toml"),
        concat!(env!("CARGO_PKG_NAME"), ".toml"),
        concat!(".", env!("CARGO_PKG_NAME"), ".init"),
        concat!(env!("CARGO_PKG_NAME"), ".init"),
    ];

    CONFIG_FILES.into_iter().find_map(|file| {
        let path = dir.join(file);
        trace!(path = %path.display(), "Checking for sessionizer init file");
        let metadata = fs::metadata(&path).ok().filter(Metadata::is_file)?;
        let kind = path
            .extension()
            .and_then(|e| e.to_str())
            .and_then(|e| match e {
                "toml" => Some(Kind::Toml),
                "init" => Some(Kind::Init),
                _ => None,
            })?;

        trace!(kind = ?kind, path = %path.display(), "Found sessionizer init file");
        Some(InitFile {
            file,
            path,
            kind,
            metadata,
        })
    })
}

fn load_definition(secure: bool, init: &InitFile) -> Result<Config> {
    if secure {
        let err = check_permissions(init.kind, &init.metadata);

        if let Err(err) = err {
            let err = eyre!(err).with_section(|| format!("{}", init.path.display()).header("File"));

            let err = match init.kind {
                Kind::Toml => err.suggestion("Set the file permissions to 600"),
                Kind::Init => err.suggestion("Set the file permissions to 700"),
            };

            return Err(err.note("Running with `--insecure` will disable this check"));
        };
    }

    match init.kind {
        Kind::Toml => load_toml_config(&init.path).map_err(|e| {
            eyre!(e).with_section(|| format!("{}", init.path.display()).header("File"))
        }),
        Kind::Init => Ok(create_init_file_config(init.file)),
    }
}

fn check_permissions(kind: Kind, md: &Metadata) -> Result<(), InitError> {
    match kind {
        Kind::Toml => {
            let permissions = md.mode();
            if permissions & 0o400 != 0o400 {
                return Err(InitError::FileNotReadable);
            }
            if permissions & 0o033 != 0 {
                return Err(InitError::PermissionsTooOpen);
            }
        }
        Kind::Init => {
            let permissions = md.mode();
            if permissions & 0o600 != 0o600 {
                return Err(InitError::FileNotReadableOrExecutable);
            }
            if permissions & 0o033 != 0 {
                return Err(InitError::PermissionsTooOpen);
            }
        }
    }

    Ok(())
}

fn load_toml_config(file: &Path) -> Result<Config, InitError> {
    let config_raw = fs::read_to_string(file).map_err(InitError::FileReading)?;
    let config = toml::from_str::<Config>(&config_raw);

    debug!(?config, "Parsed config from file");

    config.map_err(|e| InitError::FileParsing(e, config_raw))
}

fn create_init_file_config(init_file: &str) -> Config {
    let command = format!("source ./{init_file}");
    Config {
        run: vec![Run { command }],
        ..Default::default()
    }
}

fn validate_config(root_dir: &Path, config: Config) -> Result<Init> {
    let run = config
        .run
        .into_iter()
        .enumerate()
        .map(|(idx, run)| {
            let _ = validate_command("Run", idx, &run.command)?;
            Ok(run.command)
        })
        .collect::<Result<_>>()?;

    let windows = config
        .windows
        .into_iter()
        .enumerate()
        .map(|(idx, window)| validate_config_window(root_dir, idx, window))
        .collect::<Result<_>>()?;

    let env = config.env.into_iter().collect();

    let action = Init { env, run, windows };
    Ok(action)
}

fn validate_command(section: &str, idx: usize, cmd: &str) -> Result<Vec<String>> {
    if cmd.trim().is_empty() {
        return Err(eyre!(InitError::EmptyCommand)
            .with_section(|| format!("{section}[{idx}]").header("Location"))
            .suggestion("To run nothing, remove the `command` entry entirely"));
    }
    if cmd.contains(';') {
        return Err(eyre!(InitError::MultipleCommands)
            .with_section(|| format!("{section}[{idx}]").header("Location"))
            .with_section(|| cmd.to_owned().header("Command"))
            .note("Only a single command can be run.")
            .suggestion(concat!(
                "To run multiple commands, use a shell script ",
                "or similar and run that one instead."
            )));
    }

    let cmd = shlex::split(cmd).ok_or_else(|| {
        eyre!(InitError::CommandLexing)
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
            let command = validate_command("Window.command", idx, &cmd)?;
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
            eyre!(InitError::InvalidWindowDir(e))
                .with_section(|| format!("Window[{idx}]").header("Location"))
                .with_section(|| dir.display().to_string().header("Directory"))
                .note("The path might not exist or contain non-directory segments")
        })?;

        if !dir.is_dir() {
            return Err(eyre!(InitError::NotADirectory)
                .with_section(|| format!("Window[{idx}]").header("Location"))
                .with_section(|| dir.display().to_string().header("Directory")));
        }

        trace!(dir = %dir.display(), "Using dir as base for new window");
    }

    let window = SpawnWindow { name, dir, command };

    Ok(window)
}

#[derive(Copy, Clone, Debug)]
enum Kind {
    Toml,
    Init,
}

#[derive(Debug, Clone)]
struct InitFile {
    file: &'static str,
    path: PathBuf,
    kind: Kind,
    metadata: Metadata,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Config {
    #[serde(default)]
    env: indexmap::IndexMap<String, String>,
    #[serde(default)]
    #[serde(alias = "window")]
    windows: Vec<Window>,
    #[serde(default)]
    run: Vec<Run>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Window {
    name: Option<String>,
    #[serde(alias = "path")]
    #[serde(alias = "workdir")]
    #[serde(alias = "wd")]
    #[serde(alias = "pwd")]
    #[serde(alias = "cwd")]
    dir: Option<PathBuf>,
    #[serde(alias = "cmd")]
    #[serde(alias = "run")]
    command: Option<String>,
    #[serde(alias = "keep-alive")]
    #[serde(alias = "remain")]
    on_exit: Option<OnExit>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
enum OnExit {
    Destroy,
    Deactivate,
    Shell,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Run {
    #[serde(alias = "cmd")]
    #[serde(alias = "run")]
    command: String,
}

impl<'de> Deserialize<'de> for OnExit {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const VARIANTS: &[&str] = &["destroy", "deactivate", "shell"];

        struct OnExitVisitor;
        impl<'de> serde::de::Visitor<'de> for OnExitVisitor {
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
    use rstest::rstest;

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

    #[rstest]
    fn run_aliases(#[values("cmd", "run", "command")] cmd: &str) {
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

    #[rstest]
    fn window_aliases(#[values("window", "windows")] window: &str) {
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

    #[rstest]
    fn window_run_aliases(#[values("cmd", "run", "command")] cmd: &str) {
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

    #[rstest]
    fn window_dir_aliases(#[values("path", "workdir", "wd", "pwd", "cwd", "dir")] dir: &str) {
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

    #[rstest]
    fn window_on_exit_aliases(#[values("keep-alive", "remain", "on-exit")] on_exit: &str) {
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

    #[rstest]
    #[case(r#""kill""#, OnExit::Destroy)]
    #[case(r#""destroy""#, OnExit::Destroy)]
    #[case(r"false", OnExit::Destroy)]
    #[case(r#""inactive""#, OnExit::Deactivate)]
    #[case(r#""remain""#, OnExit::Deactivate)]
    #[case(r#""deactivate""#, OnExit::Deactivate)]
    #[case(r#""stay""#, OnExit::Shell)]
    #[case(r#""keep""#, OnExit::Shell)]
    #[case(r#""shell""#, OnExit::Shell)]
    #[case(r"true", OnExit::Shell)]
    fn window_on_exit_values(#[case] on_exit: &str, #[case] expected: OnExit) {
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
}
