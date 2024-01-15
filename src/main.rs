//! ## Select a new tmux session from a list of running sessions or a selection of projects.
//!
//! # Installation
//!
//! ```sh
//! $ cargo install sessionizer
//! ```
//!
//! # Usage
//!
//! ```sh
//! $ sessionizer --help
//! ```
use std::{
    cmp::Reverse,
    fmt::Display,
    fs,
    os::unix::{fs::MetadataExt as _, process::CommandExt as _},
    path::{Path, PathBuf},
    process::Command,
    sync::{
        mpsc::{self, SyncSender},
        OnceLock,
    },
    thread,
};

use fuzzy_select::{FuzzySelect, Select};
use ignore::WalkBuilder;
use kommandozeile::{
    clap,
    color_eyre::eyre::{eyre, OptionExt},
    concolor, pkg_name, setup_clap, setup_color_eyre_builder,
    tracing::{debug, info, warn},
    verbosity_filter, Color, Global, Result, Verbose,
};
use panic_message::panic_message;

fn main() -> Result<()> {
    let args = Args::init()?;

    let home = home::home_dir().ok_or_eyre("failed to get user home directory")?;

    let (tx, entries) = spawn_collector();

    let tmux_ls = (!args.selection.projects_only).then(|| find_tmux_sessions(&tx));

    if !args.selection.tmux_only {
        find_projects(&home, &tx);
    }

    let _ = tmux_ls.map(Thread::join).transpose()?;

    drop(tx);

    let entries = entries.join()?;

    debug!("found {} entries", entries.len());

    let Some(mut cmd) = make_selection(entries, args.query(), args.use_color)? else {
        return Ok(());
    };

    info!(?cmd);

    if args.dry_run {
        let cmd = shlex::join(
            std::iter::once(cmd.get_program())
                .chain(cmd.get_args())
                .filter_map(|s| s.to_str()),
        );

        println!("{cmd}");
        return Ok(());
    }

    Err(cmd.exec().into())
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Entry {
    Session(TmuxSession),
    Project(Project),
}

impl Entry {
    fn path(&self) -> &Path {
        match self {
            Self::Session(tmux) => &tmux.root,
            Self::Project(project) => &project.root,
        }
    }

    fn session_name(&self) -> &str {
        match self {
            Self::Session(tmux) => &tmux.name,
            Self::Project(project) => &project.name,
        }
    }

    fn search_content(&self) -> &str {
        match self {
            Self::Session(tmux) => &tmux.name,
            Self::Project(project) => &project.search_path,
        }
    }

    fn cmd(&self) -> Command {
        let mut cmd = Command::new("tmux");

        if let Self::Project(project) = self {
            let _ = cmd
                .arg("new-session")
                .arg("-d")
                .arg("-c")
                .arg(&project.root)
                .arg("-s")
                .arg(&project.name)
                .arg(";");
        }

        let _ = cmd
            .arg(if std::env::var_os("TMUX").is_some() {
                "switch-client"
            } else {
                "attach-session"
            })
            .arg("-t")
            .arg(format!("={}", self.session_name()));

        if let Self::Project(project) = self {
            let init_file = ".sessionizer.init";
            if let Ok(md) = fs::metadata(project.root.join(init_file)) {
                if md.is_file() && md.mode() & 0o111 != 0 {
                    let _ = cmd
                        .arg(";")
                        .arg("send-keys")
                        .arg("-t")
                        .arg(format!("={}:", project.name))
                        .arg(format!("source ./{init_file}"))
                        .arg("C-m");
                }
            }
        }

        cmd
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct TmuxSession {
    attached: bool,
    last_attached: Reverse<u64>,
    name: String,
    root: PathBuf,
    info: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Project {
    root: PathBuf,
    name: String,
    search_path: String,
    depth: usize,
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

fn spawn_collector() -> (SyncSender<Entry>, Thread<Vec<Entry>>) {
    let (tx, rx) = mpsc::sync_channel::<Entry>(16);
    let thread = std::thread::spawn(move || {
        let mut entries = rx
            .into_iter()
            .inspect(|entry| {
                debug!(?entry);
            })
            .collect::<Vec<_>>();
        entries.sort();

        if let Some(index) = entries
            .iter()
            .position(|entry| matches!(entry, Entry::Project(_)))
        {
            let (sessions, mut projects) = entries.split_at_mut(index);
            let mut projects_len = projects.len();
            for session in &*sessions {
                if let Ok(pos) = projects.binary_search_by_key(&session.path(), |p| p.path()) {
                    projects[pos..].rotate_left(1);
                    projects_len -= 1;
                    projects = &mut projects[..projects_len];
                }
            }
            projects_len += sessions.len();
            entries.truncate(projects_len);
        }

        entries
    });

    (tx, Thread::new("collector", thread))
}

fn find_tmux_sessions(tx: &SyncSender<Entry>) -> Thread<()> {
    let tx = tx.clone();
    let thread = thread::spawn(move || {
        let mut cmd = Command::new("tmux");

        let cmd = cmd.arg("list-sessions").arg("-F").arg(concat!(
            "#{session_attached},#{session_last_attached},#{session_name},",
            "#{session_path},created #{t/f/%Y-%m-%d %H#:%M:session_created}"
        ));

        match cmd.output() {
            Ok(out) => {
                let res = (move || -> Result<()> {
                    if !out.status.success() {
                        let err = String::from_utf8_lossy(&out.stderr);
                        warn!(status =? out.status, cmd =? cmd, "tmux error: {}", err);
                    }

                    let out = String::from_utf8(out.stdout)?;
                    for line in out.lines() {
                        let line = line.trim();
                        if line.is_empty() {
                            continue;
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

                        tx.send(Entry::Session(session))?;
                    }
                    Ok(())
                })();

                if let Err(err) = res {
                    warn!("failed to parse tmux sessions: {}", err);
                }
            }
            Err(err) => {
                warn!("failed to get tmux sessions: {}", err);
            }
        }
    });

    Thread::new("tmux ls", thread)
}

fn find_projects(home: &Path, tx: &SyncSender<Entry>) {
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

    w.run(|| {
        let tx = tx.clone();
        Box::new(move |result| match result {
            Ok(entry) => {
                let depth = entry.depth();
                if depth == 0 {
                    return ignore::WalkState::Continue;
                }

                let path = entry.into_path();

                let mut search_path = path
                    .iter()
                    .rev()
                    .take(depth)
                    .filter_map(|p| p.to_str())
                    .collect::<Vec<_>>();
                search_path.reverse();
                let search_path = search_path.join("/");

                let name = path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .map(|name| name.replace('.', "_"));

                let Some(name) = name else {
                    warn!("failed to get project name from path");
                    return ignore::WalkState::Skip;
                };

                let git_path = path.join(".git");
                let state = if git_path.exists() {
                    ignore::WalkState::Skip
                } else {
                    ignore::WalkState::Continue
                };

                let project = Entry::Project(Project {
                    root: path,
                    name,
                    search_path,
                    depth,
                });

                if tx.send(project).is_err() {
                    return ignore::WalkState::Quit;
                }

                state
            }
            Err(e) => {
                warn!("failed to walk entry: {}", e);
                ignore::WalkState::Skip
            }
        })
    });
}

fn make_selection(
    entries: Vec<Entry>,
    query: Option<String>,
    color: bool,
) -> Result<Option<Command>> {
    Ok(FuzzySelect::new()
        .with_prompt("Select a session or project: >")
        .set_query::<String>(query)
        .set_color(color)
        .with_options(entries)
        .select_opt()?
        .map(|entry| entry.cmd()))
}

impl Select for Entry {
    fn search_content(&self) -> &str {
        self.search_content()
    }

    fn render_before_content(&self) -> Option<impl Display + '_> {
        if let Self::Project(ref project) = self {
            Some(project)
        } else {
            None
        }
    }

    fn render_after_content(&self) -> Option<impl Display + '_> {
        if let Self::Session(ref tmux) = self {
            Some(tmux)
        } else {
            None
        }
    }
}

struct Thread<T> {
    name: &'static str,
    thread: std::thread::JoinHandle<T>,
}

impl<T> Thread<T> {
    const fn new(name: &'static str, thread: std::thread::JoinHandle<T>) -> Self {
        Self { name, thread }
    }

    fn join(self) -> Result<T> {
        self.thread
            .join()
            .map_err(|e| eyre!("{} panicked: {}", self.name, panic_message(&e)))
    }
}

/// Select a new tmux session from a list of running sessions or a selection of projects.
#[derive(Debug, clap::Parser)]
#[command(
    version(short_version()),
    long_version(long_version()),
    disable_help_subcommand(true),
    infer_long_args(true)
)]
struct Args {
    #[clap(flatten)]
    verbose: Verbose<Global>,

    #[clap(flatten)]
    color: Color,

    /// Don't switch, just print the final tmux command.
    #[clap(long, short = 'n')]
    dry_run: bool,

    #[clap(flatten)]
    selection: SelectionArgs,

    #[clap(trailing_var_arg = true)]
    query: Vec<String>,

    #[clap(skip)]
    use_color: bool,
}

impl Args {
    pub fn init() -> Result<Self> {
        let mut args = setup_clap::<Self>()
            .color_from(|a| a.color)
            .verbose_from(pkg_name!(), |a| a.verbose)
            .run();

        setup_color_eyre_builder()
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .add_issue_metadata("version", env!("CARGO_PKG_VERSION"))
            .install()?;

        args.use_color = concolor::get(concolor::Stream::Stdout).color();

        debug!(
            ?args,
            color = args.use_color,
            filter =% verbosity_filter!(args.verbose.verbosity()),
        );

        Ok(args)
    }

    pub fn query(&self) -> Option<String> {
        Some(self.query.join(" ")).filter(|q| !q.is_empty())
    }
}

#[derive(Debug, clap::Args)]
#[group(multiple = false)]
struct SelectionArgs {
    /// Only include running tmux sessions.
    #[clap(long, short = 't')]
    tmux_only: bool,

    /// Only include project directories.
    #[clap(long, short = 'p')]
    projects_only: bool,
}

const fn short_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn long_version() -> &'static str {
    static LONG_VERSION: OnceLock<String> = OnceLock::new();
    LONG_VERSION.get_or_init(|| {
        let info = Info::new();
        format!("\n{info}")
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Info {
    build_version: &'static str,
    build_timestamp: &'static str,
    commit_sha: &'static str,
    commit_date: &'static str,
    rustc_version: &'static str,
    rustc_channel: &'static str,
    host_triple: &'static str,
    target_triple: &'static str,
    cargo_profile: &'static str,
}

impl Display for Info {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:<20} {}", "Build Version:", self.build_version)?;
        writeln!(f, "{:<20} {}", "Build Timestamp:", self.build_timestamp)?;
        writeln!(f, "{:<20} {}", "Commit SHA:", self.commit_sha)?;
        writeln!(f, "{:<20} {}", "Commit Date:", self.commit_date)?;
        writeln!(f, "{:<20} {}", "rustc Version:", self.rustc_version)?;
        writeln!(f, "{:<20} {}", "rustc Channel:", self.rustc_channel)?;
        writeln!(f, "{:<20} {}", "Host Triple:", self.host_triple)?;
        writeln!(f, "{:<20} {}", "Target Triple:", self.target_triple)?;
        writeln!(f, "{:<20} {}", "cargo Profile:", self.cargo_profile)?;
        Ok(())
    }
}

impl Info {
    const fn new() -> Self {
        Self {
            build_version: env!("CARGO_PKG_VERSION"),
            build_timestamp: env!("VERGEN_BUILD_TIMESTAMP"),
            commit_sha: env!("VERGEN_GIT_SHA"),
            commit_date: env!("VERGEN_GIT_COMMIT_TIMESTAMP"),
            rustc_version: env!("VERGEN_RUSTC_SEMVER"),
            rustc_channel: env!("VERGEN_RUSTC_CHANNEL"),
            host_triple: env!("VERGEN_RUSTC_HOST_TRIPLE"),
            target_triple: env!("VERGEN_CARGO_TARGET_TRIPLE"),
            cargo_profile: env!("VERGEN_CARGO_PROFILE"),
        }
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Args::command().debug_assert();
}
