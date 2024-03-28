use std::{path::Path, sync::mpsc::SyncSender};

use ignore::{DirEntry, WalkBuilder, WalkState};

use crate::entry::{Entry, Project};

use super::{warn, Result};

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
        Ok(entry) => match process_dir(entry) {
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

fn process_dir(entry: DirEntry) -> DirResult {
    let depth = entry.depth();
    if depth == 0 {
        return DirResult::Ignored(WalkState::Continue);
    }

    let path = entry.into_path();

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
