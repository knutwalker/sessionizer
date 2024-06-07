use std::{path::PathBuf, process::Command};

use crate::{debug, Init, WindowCommand};

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

pub fn cmd(action: &Action) -> Command {
    build_command(action, std::env::var_os("TMUX").is_some())
}

fn build_command(action: &Action, inside_tmux: bool) -> Command {
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
            let cmd = cmd.args(["new-session", "-d", "-s", name, "-c"]).arg(root);

            for (key, value) in root
                .to_str()
                .map(|r| ("SESSION_ROOT", r))
                .into_iter()
                .chain([("SESSION_NAME", name.as_str())])
                .chain(on_init.env.iter().map(|(k, v)| (k.as_str(), v.as_str())))
            {
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

    cmd
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use crate::init::SpawnWindow;

    use super::*;

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
            let cmd = build_command(&action, inside_tmux);

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

    #[rstest]
    fn switch_to_existing_session(#[values(true, false)] inside_tmux: bool) {
        let action = Action::Attach {
            name: "test".to_owned(),
        };

        let cmd = build_command(&action, inside_tmux);

        let expected = if inside_tmux {
            "switch-client"
        } else {
            "attach-session"
        };
        let expected = [expected, "-t", "=test"];

        assert_cmd(cmd, expected);
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

        let cmd = build_command(&action, true);

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
    fn run_commands_in_main_window_when_creating_a_session() {
        let action = Action::Create {
            name: "test".to_owned(),
            root: PathBuf::from("/tmp"),
            on_init: Init {
                run: vec!["echo 'hello'".to_owned()],
                ..Default::default()
            },
        };

        let cmd = build_command(&action, true);

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

        let cmd = build_command(&action, true);

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

        let cmd = build_command(&action, true);

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

        let cmd = build_command(&action, true);

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

        let cmd = build_command(&action, true);

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

    #[rstest]
    fn create_windows_with_command_and_explicit_remain(#[values(true, false)] remain: bool) {
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

        let cmd = build_command(&action, true);

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
}
