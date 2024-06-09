use std::{
    os::unix::process::CommandExt as _,
    process::Command,
    sync::mpsc::{self, SyncSender},
    thread,
};

use color_eyre::eyre::eyre;
use panic_message::panic_message;
use tracing::{debug, info, trace, warn};

pub use crate::args::{Action as CliAction, Scope, Search};
pub use color_eyre::Result;

use crate::{
    action::Action,
    args::Config,
    entry::{Entry, Project, TmuxSession},
    init::{create_config_file, edit_config_file, validate_config_file, Init, WindowCommand},
    project::find_projects,
    selection::{prompt_user, Selection},
};

mod action;
mod args;
mod entry;
mod init;
mod project;
mod selection;
mod session;

/// Run the sessionizer command line interface.
///
/// # Errors
/// Since this is the main entry, all possible errors are returned as [`color_eyre::Result`].
pub fn run(action: CliAction) -> Result<()> {
    match action {
        CliAction::Search(args) => run_search(args)?,
        CliAction::Config(Config::Init) => create_config_file()?,
        CliAction::Config(Config::Validate { insecure }) => validate_config_file(!insecure)?,
        CliAction::Config(Config::Edit { insecure }) => edit_config_file(!insecure)?,
    };

    Ok(())
}

fn run_search(
    Search {
        dry_run,
        insecure,
        use_color,
        scope,
        query,
    }: Search,
) -> Result<()> {
    let (tx, entries) = spawn_collector();

    if scope.check_projects() {
        find_projects(&tx)?;
    }

    let tmux_ls = (scope.check_tmux()).then(|| find_tmux_sessions(tx.clone()));

    let _ = tmux_ls.map(Thread::join).transpose()?;

    drop(tx);

    let entries = entries.join()?;

    debug!("found {} entries", entries.len());

    let selection = Selection {
        entries,
        query,
        color: use_color,
    };

    let command = prompt_user(selection).and_then(|e| {
        e.map(|e| {
            debug!(eentry =? e, "selected");
            apply_entry(e, !insecure)
        })
        .transpose()
    })?;

    let Some(mut cmd) = command else {
        return Ok(());
    };

    info!(?cmd);

    if dry_run {
        let cmd = shlex::try_join(
            std::iter::once(cmd.get_program())
                .chain(cmd.get_args())
                .filter_map(|s| s.to_str()),
        )?;

        println!("{cmd}");
        return Ok(());
    }

    Err(cmd.exec().into())
}

fn spawn_collector() -> (SyncSender<Entry>, Thread<Vec<Entry>>) {
    let (tx, rx) = mpsc::sync_channel::<Entry>(16);
    let thread = thread::spawn(move || {
        entry::process_entries(rx.into_iter().inspect(|entry| {
            trace!(?entry, "Entry for possible selection");
        }))
    });

    (tx, Thread::new("collector", thread))
}

fn find_tmux_sessions(tx: SyncSender<Entry>) -> Thread<()> {
    let thread = thread::spawn(move || session::fetch_tmux_sessions(|entry| Ok(tx.send(entry)?)));

    Thread::new("tmux ls", thread)
}

fn apply_entry(entry: Entry, secure: bool) -> Result<Command> {
    let action = match entry {
        Entry::Project(project) => {
            let on_init = init::find_action(&project.root, secure)
                .transpose()?
                .unwrap_or_default();
            Action::Create {
                name: project.name,
                root: project.root,
                on_init,
            }
        }
        Entry::Session(session) => Action::Attach { name: session.name },
    };

    action::cmd(&action)
}

struct Thread<T> {
    name: &'static str,
    thread: thread::JoinHandle<T>,
}

impl<T> Thread<T> {
    const fn new(name: &'static str, thread: thread::JoinHandle<T>) -> Self {
        Self { name, thread }
    }

    fn join(self) -> Result<T> {
        self.thread
            .join()
            .map_err(|e| eyre!("{} panicked: {}", self.name, panic_message(&e)))
    }
}
