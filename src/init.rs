use std::{
    fs::{self, Metadata},
    os::unix::fs::MetadataExt as _,
    path::{Path, PathBuf},
};

use kommandozeile::color_eyre::{
    eyre::{eyre, Context as _},
    Section as _,
};
use serde::Deserialize;

use crate::{
    action::{Action, SpawnWindow, WindowCommand},
    entry::Project,
};

use super::{debug, trace, Result};

pub fn find_action(project: &Project, secure: bool) -> Option<Result<Action>> {
    let init = find_init_definition(project)?;
    let config = load_definition(secure, &init).with_context(|| {
        format!(
            "Failed to run sessionizer init file: `{}`",
            init.path.display()
        )
    });
    Some(config.and_then(|c| validate_config(project, c)))
}

fn find_init_definition(project: &Project) -> Option<Init> {
    const CONFIG_FILES: [&str; 4] = [
        concat!(".", env!("CARGO_PKG_NAME"), ".toml"),
        concat!(env!("CARGO_PKG_NAME"), ".toml"),
        concat!(".", env!("CARGO_PKG_NAME"), ".init"),
        concat!(env!("CARGO_PKG_NAME"), ".init"),
    ];

    CONFIG_FILES.into_iter().find_map(|file| {
        let path = project.root.join(file);
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
        Some(Init {
            file,
            path,
            kind,
            metadata,
        })
    })
}

fn load_definition(secure: bool, init: &Init) -> Result<Config> {
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

fn validate_config(project: &Project, config: Config) -> Result<Action> {
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
                .as_deref()
                .map(|o| -> Result<_> {
                    let command = validate_command("Window.command", idx, o)?;
                    Ok(WindowCommand::Command {
                        command,
                        remain: window.remain,
                    })
                })
                .transpose()?;

            let run = window
                .run
                .map(|run| -> Result<_> {
                    let cmd = validate_command("Window.run", idx, &run)?;
                    Ok(WindowCommand::Run {
                        run,
                        name: cmd
                            .into_iter()
                            .next()
                            .expect("validate returns a non-empty command"),
                    })
                })
                .transpose()?;

            if command.is_some() && run.is_some() {
                return Err(eyre!("Window[{idx}]: Cannot have both `command` and `run`"));
            }

            let command = command.or(run);

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
                    *dir = project.root.join(dir.as_path());
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

    let action = Action { env, run, windows };
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

#[derive(Debug)]
struct Init {
    file: &'static str,
    path: PathBuf,
    kind: Kind,
    metadata: Metadata,
}

#[derive(Debug, Clone, Deserialize)]
struct Window {
    name: Option<String>,
    #[serde(alias = "path")]
    #[serde(alias = "workdir")]
    #[serde(alias = "wd")]
    #[serde(alias = "pwd")]
    #[serde(alias = "cwd")]
    dir: Option<PathBuf>,
    #[serde(alias = "cmd")]
    command: Option<String>,
    #[serde(alias = "keep-alive")]
    remain: Option<bool>,
    run: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct Run {
    #[serde(alias = "cmd")]
    command: String,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct Config {
    #[serde(default)]
    env: indexmap::IndexMap<String, String>,
    #[serde(default)]
    #[serde(alias = "window")]
    windows: Vec<Window>,
    #[serde(default)]
    run: Vec<Run>,
}
