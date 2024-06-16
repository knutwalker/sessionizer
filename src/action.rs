use std::{env, path::PathBuf, process::Command};

use color_eyre::{Section as _, SectionExt as _};
use indexmap::IndexMap;
use onlyerror::Error;

use crate::{debug, eyre, Init, Result, WindowCommand};

#[derive(Debug, Clone)]
pub enum Action {
    Attach {
        name: String,
    },
    Create {
        name: String,
        root: PathBuf,
        on_init: Init,
    },
}

#[allow(clippy::module_name_repetitions)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Error)]
pub enum ActionError {
    #[error("The variable is not defined")]
    VarNotFound,
    #[error("The variable is not valid utf-8")]
    NotUtf8,
}

pub fn cmd(action: &Action) -> Result<Command> {
    build_command(action, env::var_os("TMUX").is_some(), use_std_env)
}

fn use_std_env(k: &str) -> Result<String, ActionError> {
    env::var(k).map_err(|e| match e {
        env::VarError::NotPresent => ActionError::VarNotFound,
        env::VarError::NotUnicode(_) => ActionError::NotUtf8,
    })
}

fn build_command(
    action: &Action,
    inside_tmux: bool,
    root_env: impl Fn(&str) -> Result<String, ActionError>,
) -> Result<Command> {
    debug!(?action, "Running action on a new session");

    let mut cmd = Command::new("tmux");

    match action {
        Action::Attach { name } => {
            let session_id = format!("={name}");

            let _ = if inside_tmux {
                cmd.args(["switch-client", "-t", &session_id])
            } else {
                cmd.args(["attach-session", "-t", &session_id])
            };
        }
        Action::Create {
            name,
            root,
            on_init,
        } => {
            let get_env = |k: &str| root_env(k).map(Some);
            let cmd = cmd.args(["new-session", "-d", "-s", name, "-c"]).arg(root);

            let mut env = IndexMap::with_capacity(2 + on_init.env.len());
            let _ = env.insert("SESSION_ROOT", root.to_string_lossy().into_owned());
            let _ = env.insert("SESSION_NAME", name.clone());
            for (k, v) in &on_init.env {
                let v = shellexpand::env_with_context(v.as_str(), |k| {
                    env.get(k).cloned().map(Some).map_or_else(|| get_env(k), Ok)
                })
                .map_err(|e| {
                    eyre!(e.cause)
                        .section(e.var_name.header("Variable:"))
                        .note("You can provide a default value by using ${VAR_NAME:default_value}")
                })?;
                let _ = env.insert(k.as_str(), v.into_owned());
            }

            for (key, value) in env {
                let _ = cmd.arg("-e").arg(format!("{key}={value}"));
            }

            let _ = cmd.arg(";");
            let session_id = format!("={name}");

            let _ = if inside_tmux {
                cmd.args(["switch-client", "-t", &session_id])
            } else {
                cmd.args(["attach-session", "-t", &session_id])
            };

            if !on_init.run.is_empty() {
                let session_pane = format!("{session_id}:");
                for run in &on_init.run {
                    let _ = cmd.args([";", "send-keys", "-t", &session_pane, run, "C-m"]);
                }
            }

            for window in &on_init.windows {
                let _ = cmd.args([";", "new-window", "-d", "-n", &window.name]);

                if let Some(dir) = &window.dir {
                    let _ = cmd.arg("-c").arg(dir);
                }

                if let Some(command) = &window.command {
                    match command {
                        WindowCommand::Command { command, remain } => {
                            let _ = cmd.args(command);
                            if let Some(remain) = *remain {
                                let remain = if remain { "on" } else { "off" };
                                let _ = cmd.args([
                                    ";",
                                    "set-option",
                                    "-t",
                                    &window.name,
                                    "remain-on-exit",
                                    remain,
                                ]);
                            }
                        }
                        WindowCommand::Run { run, .. } => {
                            let target = format!("={name}:{}", window.name);
                            let _ = cmd.args([";", "send-keys", "-t", &target, run, "C-m"]);
                        }
                    }
                }
            }
        }
    };

    Ok(cmd)
}

#[cfg(test)]
mod tests {
    use crate::init::SpawnWindow;

    use super::*;

    fn command(action: &Action, inside_tmux: bool) -> Command {
        build_command(action, inside_tmux, |_| Err(ActionError::VarNotFound)).unwrap()
    }

    #[allow(clippy::needless_pass_by_value)]
    fn assert_cmd<const N: usize>(cmd: Command, expected: [&str; N]) {
        assert_eq!(cmd.get_program(), "tmux");

        let cmd = cmd
            .get_args()
            .filter_map(|arg| arg.to_str())
            .collect::<Vec<_>>();

        assert_eq!(cmd, expected.as_ref());
    }

    #[test]
    fn new_session_from_project() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init::default(),
        };

        for (inside_tmux, expected) in [(true, "switch-client"), (false, "attach-session")] {
            let cmd = command(&action, inside_tmux);

            let expected = [
                "new-session",
                "-d",
                "-s",
                "test",
                "-c",
                "/tmp",
                "-e",
                "SESSION_ROOT=/tmp",
                "-e",
                "SESSION_NAME=test",
                ";",
                expected,
                "-t",
                "=test",
            ];

            assert_cmd(cmd, expected);
        }
    }

    fn test_switch_to_existing_session(inside_tmux: bool) {
        let action = Action::Attach {
            name: "test".to_owned(),
        };

        let cmd = command(&action, inside_tmux);

        let expected = if inside_tmux {
            "switch-client"
        } else {
            "attach-session"
        };
        let expected = [expected, "-t", "=test"];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn switch_to_existing_session_within_tmux() {
        test_switch_to_existing_session(true);
    }

    #[test]
    fn switch_to_existing_session_outside_tmux() {
        test_switch_to_existing_session(false);
    }

    #[test]
    fn set_env_vars_when_creating_a_session() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                env: vec![("FOO".to_owned(), "bar".to_owned())],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            "-e",
            "FOO=bar",
            ";",
            "switch-client",
            "-t",
            "=test",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn expand_env_vars_when_creating_a_session() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                env: vec![
                    ("FOO".to_owned(), "let's $SESSIONIZER_TEST".to_owned()),
                    (
                        "BAR".to_owned(),
                        "Can also use previously defined vars: '${FOO}'".to_owned(),
                    ),
                ],
                ..Default::default()
            },
        };

        let cmd = build_command(&action, true, |k| {
            assert_eq!(k, "SESSIONIZER_TEST");
            Ok("frobnicate".to_owned())
        })
        .unwrap();

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            "-e",
            "FOO=let's frobnicate",
            "-e",
            "BAR=Can also use previously defined vars: 'let's frobnicate'",
            ";",
            "switch-client",
            "-t",
            "=test",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn fail_when_expanding_unknown_env() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                env: vec![("FOO".to_owned(), "$SESSIONIZER_TEST_UNSET".to_owned())],
                ..Default::default()
            },
        };

        let err = build_command(&action, true, |_| Err(ActionError::VarNotFound)).unwrap_err();
        let error = err.downcast::<ActionError>().unwrap();

        assert_eq!(error, ActionError::VarNotFound);
    }

    #[test]
    fn run_commands_in_main_window_when_creating_a_session() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                run: vec!["echo 'hello'".to_owned()],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "send-keys",
            "-t",
            "=test:",
            "echo 'hello'",
            "C-m",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn create_windows_plain() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                windows: vec![SpawnWindow {
                    name: "foo".to_owned(),
                    dir: None,
                    command: None,
                }],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "new-window",
            "-d",
            "-n",
            "foo",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn create_windows_with_dir() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                windows: vec![SpawnWindow {
                    name: "bar".to_owned(),
                    dir: Some("/tmp/bar".into()),
                    command: None,
                }],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "new-window",
            "-d",
            "-n",
            "bar",
            "-c",
            "/tmp/bar",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn create_windows_with_inline_run() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                windows: vec![SpawnWindow {
                    name: "baz".to_owned(),
                    dir: None,
                    command: Some(WindowCommand::Run {
                        run: "echo 'baz'".to_owned(),
                        name: "echo".to_owned(),
                    }),
                }],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "new-window",
            "-d",
            "-n",
            "baz",
            ";",
            "send-keys",
            "-t",
            "=test:baz",
            "echo 'baz'",
            "C-m",
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn create_windows_with_command_and_default_remain() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                windows: vec![SpawnWindow {
                    name: "qux".to_owned(),
                    dir: None,
                    command: Some(WindowCommand::Command {
                        command: vec!["echo".to_owned(), "'qux'".to_owned()],
                        remain: None,
                    }),
                }],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "new-window",
            "-d",
            "-n",
            "qux",
            "echo",
            "'qux'",
        ];

        assert_cmd(cmd, expected);
    }

    fn test_create_windows_with_command_and_explicit_remain(remain: bool) {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                windows: vec![SpawnWindow {
                    name: "qax".to_owned(),
                    dir: None,
                    command: Some(WindowCommand::Command {
                        command: vec!["echo".to_owned(), "'qax'".to_owned()],
                        remain: Some(remain),
                    }),
                }],
                ..Default::default()
            },
        };

        let cmd = command(&action, true);

        let expected = if remain { "on" } else { "off" };
        let expected = [
            "new-session",
            "-d",
            "-s",
            "test",
            "-c",
            "/tmp",
            "-e",
            "SESSION_ROOT=/tmp",
            "-e",
            "SESSION_NAME=test",
            ";",
            "switch-client",
            "-t",
            "=test",
            ";",
            "new-window",
            "-d",
            "-n",
            "qax",
            "echo",
            "'qax'",
            ";",
            "set-option",
            "-t",
            "qax",
            "remain-on-exit",
            expected,
        ];

        assert_cmd(cmd, expected);
    }

    #[test]
    fn create_windows_with_command_and_explicit_remain_true() {
        test_create_windows_with_command_and_explicit_remain(true);
    }

    #[test]
    fn create_windows_with_command_and_explicit_remain_false() {
        test_create_windows_with_command_and_explicit_remain(false);
    }
}
