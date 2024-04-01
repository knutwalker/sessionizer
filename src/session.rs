use std::{
    cmp::Reverse,
    process::{Command, Output},
};

use crate::{warn, Entry, Result, TmuxSession};

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
#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn ignore_blank_line() {
        assert!(matches!(parse_session(""), Ok(None)));
        assert!(matches!(parse_session("\n"), Ok(None)));
        assert!(matches!(parse_session("     \n"), Ok(None)));
    }

    #[test]
    fn parse_result_line() {
        let line = "1,1711654501,sessionizer,/dev/sessionizer,created 2024-03-28 20:35";
        let result = parse_session(line).unwrap().unwrap();

        assert!(result.attached);
        assert_eq!(result.last_attached.0, 1_711_654_501);
        assert_eq!(result.name, "sessionizer");
        assert_eq!(result.root, Path::new("/dev/sessionizer"));
        assert_eq!(result.info, "created 2024-03-28 20:35");
    }

    #[test]
    fn perse_result_list() {
        let result = r"
0,1711100755,neo4rs,/dev/neo4rs,created 2024-03-22 10:45
1,1711654501,sessionizer,/dev/sessionizer,created 2024-03-28 20:35
        ";

        let output = Output {
            status: std::process::ExitStatus::default(),
            stdout: result.as_bytes().to_vec(),
            stderr: Vec::new(),
        };
        let mut sessions = Vec::new();
        let handle = |entry| {
            sessions.push(entry);
            Ok(())
        };

        parse_tmux_output(output, handle).unwrap();

        assert_eq!(
            sessions,
            [
                Entry::Session(TmuxSession {
                    attached: false,
                    last_attached: Reverse(1_711_100_755),
                    name: "neo4rs".into(),
                    root: Path::new("/dev/neo4rs").into(),
                    info: "created 2024-03-22 10:45".into(),
                }),
                Entry::Session(TmuxSession {
                    attached: true,
                    last_attached: Reverse(1_711_654_501),
                    name: "sessionizer".into(),
                    root: Path::new("/dev/sessionizer").into(),
                    info: "created 2024-03-28 20:35".into(),
                }),
            ]
        );
    }
}
