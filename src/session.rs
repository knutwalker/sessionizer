use std::{
    cmp::Reverse,
    process::{Command, Output},
};

use crate::entry::{Entry, TmuxSession};

use super::{warn, Result};

pub fn fetch_tmux_sessions(handle: impl FnMut(Entry) -> Result<()>) {
    let mut cmd = Command::new("tmux");

    let cmd = cmd.args([
        "list-sessions",
        "-F",
        concat!(
            "#{session_attached},#{session_last_attached},#{session_name},",
            "#{session_path},created #{t/f/%Y-%m-%d %H#:%M:session_created}"
        ),
    ]);

    match cmd.output() {
        Ok(out) => {
            if let Err(err) = parse_tmux_output(out, handle) {
                warn!("failed to parse tmux sessions: {}", err);
            }
        }
        Err(err) => {
            warn!("failed to get tmux sessions: {}", err);
        }
    }
}

fn parse_tmux_output(out: Output, mut handle: impl FnMut(Entry) -> Result<()>) -> Result<()> {
    if !out.status.success() {
        let err = String::from_utf8_lossy(&out.stderr);
        warn!(status =? out.status, "tmux error: {}", err);
    }

    let out = String::from_utf8(out.stdout)?;
    out.lines()
        .map(parse_session)
        .filter_map(Result::transpose)
        .try_for_each(move |session| handle(Entry::Session(session?)))
}

fn parse_session(tmux_ls_output: &str) -> Result<Option<TmuxSession>> {
    let line = tmux_ls_output.trim();
    if line.is_empty() {
        return Ok(None);
    }
    let parts: [&str; 5] = line
        .splitn(5, ',')
        .collect::<Vec<_>>()
        .try_into()
        .expect("tmux format");

    let attached = parts[0].parse::<u64>()? > 0;
    let last_attached = Reverse(
        Some(parts[1])
            .filter(|s| !s.is_empty())
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(0),
    );
    let name = parts[2].into();
    let root = parts[3].into();
    let info = parts[4].into();
    let session = TmuxSession {
        attached,
        last_attached,
        name,
        root,
        info,
    };

    Ok(Some(session))
}
