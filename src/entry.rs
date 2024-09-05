use std::{
    cmp::Reverse,
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Entry {
    Session(TmuxSession),
    Project(Project),
}

impl Entry {
    pub fn path(&self) -> &Path {
        match self {
            Self::Session(tmux) => &tmux.root,
            Self::Project(project) => &project.root,
        }
    }

    pub fn search_content(&self) -> &str {
        match self {
            Self::Session(tmux) => &tmux.name,
            Self::Project(project) => &project.search_path,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TmuxSession {
    pub attached: bool,
    pub last_attached: Reverse<u64>,
    pub name: String,
    pub root: PathBuf,
    pub info: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Project {
    pub priority: usize,
    pub root: PathBuf,
    pub name: String,
    pub search_path: String,
    pub depth: usize,
}

impl Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Session(tmux) => write!(
                f,
                "{} (root: {} {})",
                tmux.name,
                tmux.root.display(),
                tmux.info
            ),
            Self::Project(project) => write!(f, "{}", project.root.display()),
        }
    }
}

impl Display for TmuxSession {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " (root: {} {})", self.root.display(), self.info)
    }
}

impl Display for Project {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::iter::successors(Some(self.root.as_path()), |p| p.parent())
            .nth(self.depth)
            .map_or(Ok(()), |p| write!(f, "{}/", p.display()))
    }
}

/// Collects all entries into a Vec and processes them to be displayed as
/// selection to the user.
///
/// The processing is as follows:
///     - Sort the entries
///         - Sessions first, sorted by least recently attached first
///         - Projects second, sorted by path
///     - Deduplicate the entries
///         - Projects that also have a session attached are removed
pub fn process_entries(entries: impl IntoIterator<Item = Entry>) -> Vec<Entry> {
    let mut entries = entries.into_iter().collect::<Vec<_>>();
    entries.sort();

    if let Some(index) = entries
        .iter()
        .position(|entry| matches!(entry, Entry::Project(_)))
    {
        let (sessions, mut projects) = entries.split_at_mut(index);
        let mut projects_len = projects.len();
        for session in &*sessions {
            if let Ok(pos) = projects.binary_search_by_key(&session.path(), |p| p.path()) {
                // this is basically `entries.remove(index + pos)` but we can't do that
                // because `entries` is allready borrowed mutably.
                // We rotate the slice to move the element to be removed to the last position
                // and then shorten the slice by one.
                projects[pos..].rotate_left(1);
                projects_len -= 1;
                projects = &mut projects[..projects_len];
            }
        }
        projects_len += sessions.len();
        entries.truncate(projects_len);
    }

    entries
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn process_sorts_and_deduplicated() {
        let entries = [
            Entry::Project(Project {
                priority: 0,
                root: PathBuf::from("/a"),
                name: "a".into(),
                search_path: "a".into(),
                depth: 1,
            }),
            Entry::Project(Project {
                priority: 0,
                root: PathBuf::from("/b"),
                name: "b".into(),
                search_path: "b".into(),
                depth: 1,
            }),
            Entry::Project(Project {
                priority: 0,
                root: PathBuf::from("/c"),
                name: "c".into(),
                search_path: "c".into(),
                depth: 1,
            }),
            Entry::Session(TmuxSession {
                attached: false,
                last_attached: Reverse(0),
                name: "a".into(),
                root: PathBuf::from("/a"),
                info: "a".into(),
            }),
            Entry::Session(TmuxSession {
                attached: false,
                last_attached: Reverse(1),
                name: "d".into(),
                root: PathBuf::from("/d"),
                info: "d".into(),
            }),
        ];

        let entries = process_entries(entries);

        assert_eq!(
            entries,
            [
                Entry::Session(TmuxSession {
                    attached: false,
                    last_attached: Reverse(1),
                    name: "d".into(),
                    root: PathBuf::from("/d"),
                    info: "d".into(),
                }),
                Entry::Session(TmuxSession {
                    attached: false,
                    last_attached: Reverse(0),
                    name: "a".into(),
                    root: PathBuf::from("/a"),
                    info: "a".into(),
                }),
                Entry::Project(Project {
                    priority: 0,
                    root: PathBuf::from("/b"),
                    name: "b".into(),
                    search_path: "b".into(),
                    depth: 1,
                }),
                Entry::Project(Project {
                    priority: 0,
                    root: PathBuf::from("/c"),
                    name: "c".into(),
                    search_path: "c".into(),
                    depth: 1,
                }),
            ]
        );
    }
}
