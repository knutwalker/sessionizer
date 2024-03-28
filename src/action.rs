use std::{path::PathBuf, process::Command};

use crate::entry::Entry;

use super::debug;
#[derive(Debug, Clone, Default)]
pub struct Action {
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

pub fn cmd(entry: &Entry, action: Action) -> Command {
    debug!(?action, "Running action on a new session");

    let mut cmd = Command::new("tmux");
    if let Entry::Project(project) = entry {
        let cmd = cmd
            .args(["new-session", "-d", "-s", &project.name, "-c"])
            .arg(&project.root);

        for (key, value) in action
            .env
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .chain(project.root.to_str().map(|r| ("SESSION_ROOT", r)))
            .chain(Some(("SESSION_NAME", project.name.as_str())))
        {
            let _ = cmd.arg("-e").arg(format!("{key}={value}"));
        }

        let _ = cmd.arg(";");
    }

    let tmux_attach = if std::env::var_os("TMUX").is_some() {
        "switch-client"
    } else {
        "attach-session"
    };

    let session_id = format!("={}", entry.session_name());
    run(&session_id, tmux_attach, action, &mut cmd);

    cmd
}

fn run(session_id: &str, attach: &str, action: Action, cmd: &mut Command) {
    let _ = cmd.args([attach, "-t", &session_id]);

    if !action.run.is_empty() {
        let session_pane = format!("{session_id}:");
        for run in action.run {
            let _ = cmd.args([";", "send-keys", "-t", &session_pane, &run, "C-m"]);
        }
    }

    for window in action.windows {
        let _ = cmd.args([";", "new-window", "-d", "-n", &window.name]);

        if let Some(dir) = window.dir {
            let _ = cmd.arg("-c").arg(dir);
        }

        if let Some(command) = window.command {
            match command {
                WindowCommand::Command { command, remain } => {
                    let _ = cmd.args(command);
                    if let Some(remain) = remain {
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
                    let _ = cmd.args([";", "send-keys", "-t", &window.name, &run, "C-m"]);
                }
            }
        }
    }
}
