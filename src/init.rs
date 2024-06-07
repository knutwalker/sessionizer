use std::{
    env, fmt,
    fs::{self, Metadata},
    os::unix::fs::MetadataExt as _,
    path::{Path, PathBuf},
};

use kommandozeile::color_eyre::{
    eyre::{eyre, Context as _},
    Section as _,
};
use serde::{Deserialize, Deserializer};

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

pub fn find_action(dir: &Path, secure: bool) -> Option<Result<Init>> {
    let init = find_init_definition(dir)?;
    let init = validated_init_file(dir, secure, &init);
    Some(init)
}

fn validated_init_file(dir: &Path, secure: bool, init: &InitFile) -> Result<Init> {
    let config = load_definition(secure, init).with_context(|| {
        format!(
            "Failed to load sessionizer init file: `{}`",
            init.path.display()
        )
    })?;
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
        let check = check_permissions(init.kind, &init.metadata);

        if let Err(check) = check {
            return Err(match init.kind {
                Kind::Toml => check.suggestion("Set the file permissions to 600"),
                Kind::Init => check.suggestion("Set the file permissions to 700"),
            })
            .note("Running with `--insecure` will disable this check");
        };
    }

    match init.kind {
        Kind::Toml => load_toml_config(&init.path),
        Kind::Init => Ok(create_init_file_config(init.file)),
    }
}

fn check_permissions(kind: Kind, md: &Metadata) -> Result<()> {
    match kind {
        Kind::Toml => {
            let permissions = md.mode();
            if permissions & 0o400 != 0o400 {
                return Err(eyre!("File is not readable"));
            }
            if permissions & 0o033 != 0 {
                return Err(eyre!("File can be written or executed by others"));
            }
        }
        Kind::Init => {
            let permissions = md.mode();
            if permissions & 0o600 != 0o600 {
                return Err(eyre!("File is not readable or executable"));
            }
            if permissions & 0o033 != 0 {
                return Err(eyre!("File can be written or executed by others"));
            }
        }
    }

    Ok(())
}

fn load_toml_config(file: &Path) -> Result<Config> {
    let config = fs::read_to_string(file)?;
    let config = toml::from_str::<Config>(&config);

    debug!(?config, "Parsed config from file");

    Ok(config?)
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
        .map(|(idx, window)| {
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

                *dir = dir
                    .canonicalize()
                    .with_context(|| {
                        format!(
                            "Window[{idx}]: Invalid window directory: `{}`",
                            dir.display()
                        )
                    })
                    .note("The path might not exist or contain non-directory segments")?;

                if !dir.is_dir() {
                    return Err(eyre!(
                        "Window[{idx}]: Invalid window directory: `{}`",
                        dir.display()
                    )
                    .note("The path is not a directory"));
                }

                trace!(dir = %dir.display(), "Using dir as base for new window");
            }

            let window = SpawnWindow { name, dir, command };

            Ok(window)
        })
        .collect::<Result<_>>()?;

    let env = config.env.into_iter().collect();

    let action = Init { env, run, windows };
    Ok(action)
}

fn validate_command(section: &str, idx: usize, cmd: &str) -> Result<Vec<String>> {
    if cmd.trim().is_empty() {
        return Err(eyre!("{section}[{idx}]: Command cannot be empty")
            .note("To run nothing, remove the `command` entry entirely"));
    }
    if cmd.contains(';') {
        return Err(
            eyre!("{section}[{idx}]: Command contains a semicolon: `{cmd}`")
                .note("Only a single command can be run.")
                .note(concat!(
                    "To run multiple commands, use a shell script ",
                    "or similar and run that one instead."
                )),
        );
    }

    let cmd = shlex::split(cmd).ok_or_else(|| {
        eyre!("{section}[{idx}]: Failed to split command into shell arguments").note(concat!(
            "The command might end while inside a quotation ",
            "or right after an unescaped backslash."
        ))
    })?;

    assert!(!cmd.is_empty(), "shlex::split returned an empty command");

    Ok(cmd)
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
