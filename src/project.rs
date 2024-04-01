use std::{
    path::{Path, PathBuf},
    sync::mpsc::SyncSender,
};

use ignore::{DirEntry, WalkBuilder, WalkState};

use crate::{warn, Entry, Project, Result};

pub fn find_projects(home: &Path, tx: &SyncSender<Entry>) {
    let w = WalkBuilder::new(home.join("dev"))
        .add(home.join(".config"))
        .max_depth(Some(2))
        .same_file_system(true)
        .follow_links(false)
        .filter_entry(|e| {
            e.file_type()
                .is_some_and(|ft| ft.is_dir() || ft.is_symlink())
        })
        .build_parallel();

    let handler = |result| process_dir_entry(result, tx);
    w.run(move || Box::new(handler));
}

fn process_dir_entry(entry: Result<DirEntry, ignore::Error>, tx: &SyncSender<Entry>) -> WalkState {
    match entry {
        Ok(entry) => match process_dir(entry.depth(), entry.into_path()) {
            DirResult::Ignored(state) => state,
            DirResult::Handle(entry, state) => {
                if tx.send(entry).is_err() {
                    WalkState::Quit
                } else {
                    state
                }
            }
        },
        Err(e) => {
            warn!("failed to walk entry: {}", e);
            WalkState::Skip
        }
    }
}

enum DirResult {
    Ignored(WalkState),
    Handle(Entry, WalkState),
}

fn process_dir(depth: usize, path: PathBuf) -> DirResult {
    if depth == 0 {
        return DirResult::Ignored(WalkState::Continue);
    }

    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.replace('.', "_"));

    let Some(name) = name else {
        warn!("failed to get project name from path");
        return DirResult::Ignored(WalkState::Skip);
    };

    let search_path = {
        let mut path = path
            .iter()
            .rev()
            .take(depth)
            .filter_map(|p| p.to_str())
            .collect::<Vec<_>>();
        path.reverse();
        path.join("/")
    };

    let git_path = path.join(".git");
    let state = if git_path.exists() {
        WalkState::Skip
    } else {
        WalkState::Continue
    };

    let project = Entry::Project(Project {
        root: path,
        name,
        search_path,
        depth,
    });

    DirResult::Handle(project, state)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_root_entries() {
        let result = process_dir(0, PathBuf::from("/"));
        assert!(matches!(result, DirResult::Ignored(WalkState::Continue)));
    }

    #[test]
    fn replace_invalid_chars_in_name() {
        let result = process_dir(1, PathBuf::from("/home/user/dev/project.name"));
        let DirResult::Handle(Entry::Project(project), _) = result else {
            panic!("expected project entry");
        };
        assert_eq!(project.name, "project_name");
    }

    #[test]
    fn search_path_is_the_last_depth_path_elements() {
        let result = process_dir(3, PathBuf::from("/home/user/dev/project-name/src"));
        let DirResult::Handle(Entry::Project(project), _) = result else {
            panic!("expected project entry");
        };
        assert_eq!(project.search_path, "dev/project-name/src");
    }
}
