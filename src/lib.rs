use std::{
    cmp::Reverse,
    fmt::Display,
    fs::{self, Metadata},
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
    color_eyre::{
        eyre::{eyre, Context, OptionExt},
        ErrorKind, Section,
    },
    concolor, pkg_name, setup_clap, setup_color_eyre_builder,
    tracing::{debug, info, trace, warn},
    verbosity_filter, Color, Global, Result, Verbose,
};
use panic_message::panic_message;
use serde::Deserialize;

/// Run the sessionizer command line interface.
///
/// # Errors
/// Since this is the main entry, all possible errors are returned as [`color_eyre::Result`].
pub fn run(args: &Args) -> Result<()> {
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

    let Some(mut cmd) = make_selection(entries, args.query(), args.use_color, !args.insecure)?
    else {
        return Ok(());
    };

    info!(?cmd);

    if args.dry_run {
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

    fn cmd(&self, secure: bool) -> Result<Command> {
        let action = match self {
            Self::Project(project) => Self::try_init(project, secure).transpose()?,
            Self::Session(_) => None,
        }
        .unwrap_or_default();

        debug!(?action, "Running action on a new session");

        let mut cmd = Command::new("tmux");
        if let Self::Project(project) = self {
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

        let session_id = format!("={}", self.session_name());
        Self::run_action(&session_id, tmux_attach, action, &mut cmd);

        Ok(cmd)
    }

    fn try_init(project: &Project, secure: bool) -> Option<Result<Action>> {
        let init = Self::find_init(project)?;
        let config = Self::load_init(secure, &init).with_context(|| {
            format!(
                "Failed to run sessionizer init file: `{}`",
                init.path.display()
            )
        });
        Some(config.and_then(|c| Self::validate_config(project, c)))
    }

    fn find_init(project: &Project) -> Option<Init> {
        const CONFIG_FILES: [&str; 4] = [
            concat!(".", env!("CARGO_PKG_NAME"), ".toml"),
            concat!(env!("CARGO_PKG_NAME"), ".toml"),
            concat!(".", env!("CARGO_PKG_NAME"), ".init"),
            concat!(env!("CARGO_PKG_NAME"), ".init"),
        ];

        CONFIG_FILES.into_iter().find_map(|file| {
            let path = project.root.join(file);
            trace!(path = %path.display(), "Checking for sessionizer init file");
            let metadata = fs::metadata(&path).ok().filter(Metadata::is_file)?;
            let kind = path
                .extension()
                .and_then(|e| e.to_str())
                .and_then(|e| match e {
                    "toml" => Some(Kind::Toml),
                    "init" => Some(Kind::Init),
                    _ => None,
                })?;

            trace!(kind = ?kind, path = %path.display(), "Found sessionizer init file");
            Some(Init {
                file,
                path,
                kind,
                metadata,
            })
        })
    }

    fn load_init(secure: bool, init: &Init) -> Result<Config> {
        if secure {
            let check = Self::check_permissions(init.kind, &init.metadata);

            if let Err(check) = check {
                return Err(match init.kind {
                    Kind::Toml => check.suggestion("Set the file permissions to 600"),
                    Kind::Init => check.suggestion("Set the file permissions to 700"),
                })
                .note("Running with `--insecure` will disable this check");
            };
        }

        match init.kind {
            Kind::Toml => Self::load_toml_config(&init.path),
            Kind::Init => Ok(Self::create_init_file_config(init.file)),
        }
    }

    fn check_permissions(kind: Kind, md: &Metadata) -> Result<()> {
        match kind {
            Kind::Toml => {
                let permissions = md.mode();
                if permissions & 0o400 != 0o400 {
                    return Err(eyre!("File is not readable"));
                }
                if permissions & 0o033 != 0 {
                    return Err(eyre!("File can be written or executed by others"));
                }
            }
            Kind::Init => {
                let permissions = md.mode();
                if permissions & 0o600 != 0o600 {
                    return Err(eyre!("File is not readable or executable"));
                }
                if permissions & 0o033 != 0 {
                    return Err(eyre!("File can be written or executed by others"));
                }
            }
        }

        Ok(())
    }

    fn load_toml_config(file: &Path) -> Result<Config> {
        let config = fs::read_to_string(file)?;
        let config = toml::from_str::<Config>(&config);

        debug!(?config, "Parsed config from file");

        Ok(config?)
    }

    fn create_init_file_config(init_file: &str) -> Config {
        let command = format!("source ./{init_file}");
        Config {
            run: vec![Run { command }],
            ..Default::default()
        }
    }

    fn validate_config(project: &Project, config: Config) -> Result<Action> {
        let run = config
            .run
            .into_iter()
            .enumerate()
            .map(|(idx, run)| {
                let _ = Self::validate_command("Run", idx, &run.command)?;
                Ok(run.command)
            })
            .collect::<Result<_>>()?;

        let windows = config
            .windows
            .into_iter()
            .enumerate()
            .map(|(idx, window)| {
                let command = window
                    .command
                    .as_deref()
                    .map(|o| -> Result<_> {
                        let command = Self::validate_command("Window.command", idx, o)?;
                        Ok(WindowCommand::Command {
                            command,
                            remain: window.remain,
                        })
                    })
                    .transpose()?;

                let run = window
                    .run
                    .map(|run| -> Result<_> {
                        let cmd = Self::validate_command("Window.run", idx, &run)?;
                        Ok(WindowCommand::Run {
                            run,
                            name: cmd
                                .into_iter()
                                .next()
                                .expect("validate returns a non-empty command"),
                        })
                    })
                    .transpose()?;

                if command.is_some() && run.is_some() {
                    return Err(eyre!("Window[{idx}]: Cannot have both `command` and `run`"));
                }

                let command = command.or(run);

                let name = window
                    .name
                    .as_deref()
                    .or_else(|| command.as_ref().map(WindowCommand::first))
                    .map(str::trim)
                    .filter(|o| !o.is_empty())
                    .map_or_else(|| format!("Window[{idx}]"), ToOwned::to_owned);

                let mut dir = window.dir;
                if let Some(dir) = dir.as_mut() {
                    trace!(dir = %dir.display(), "Attemting to use dir as base for new window");

                    if dir.is_relative() {
                        *dir = project.root.join(dir.as_path());
                    }

                    *dir = dir
                        .canonicalize()
                        .with_context(|| {
                            format!(
                                "Window[{idx}]: Invalid window directory: `{}`",
                                dir.display()
                            )
                        })
                        .note("The path might not exist or contain non-directory segments")?;

                    if !dir.is_dir() {
                        return Err(eyre!(
                            "Window[{idx}]: Invalid window directory: `{}`",
                            dir.display()
                        )
                        .note("The path is not a directory"));
                    }

                    trace!(dir = %dir.display(), "Using dir as base for new window");
                }

                let window = SpawnWindow { name, dir, command };

                Ok(window)
            })
            .collect::<Result<_>>()?;

        let env = config.env.into_iter().collect();

        let action = Action { env, run, windows };
        Ok(action)
    }

    fn validate_command(section: &str, idx: usize, cmd: &str) -> Result<Vec<String>> {
        if cmd.trim().is_empty() {
            return Err(eyre!("{section}[{idx}]: Command cannot be empty")
                .note("To run nothing, remove the `command` entry entirely"));
        }
        if cmd.contains(';') {
            return Err(
                eyre!("{section}[{idx}]: Command contains a semicolon: `{cmd}`")
                    .note("Only a single command can be run.")
                    .note(concat!(
                        "To run multiple commands, use a shell script ",
                        "or similar and run that one instead."
                    )),
            );
        }

        let cmd = shlex::split(cmd).ok_or_else(|| {
            eyre!("{section}[{idx}]: Failed to split command into shell arguments").note(concat!(
                "The command might end while inside a quotation ",
                "or right after an unescaped backslash."
            ))
        })?;

        assert!(!cmd.is_empty(), "shlex::split returned an empty command");

        Ok(cmd)
    }

    fn run_action(session_id: &str, attach: &str, action: Action, cmd: &mut Command) {
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
}

#[derive(Copy, Clone, Debug)]
enum Kind {
    Toml,
    Init,
}

#[derive(Debug)]
struct Init {
    file: &'static str,
    path: PathBuf,
    kind: Kind,
    metadata: Metadata,
}

#[derive(Debug, Clone, Deserialize)]
struct Window {
    name: Option<String>,
    #[serde(alias = "path")]
    #[serde(alias = "workdir")]
    #[serde(alias = "wd")]
    #[serde(alias = "pwd")]
    #[serde(alias = "cwd")]
    dir: Option<PathBuf>,
    #[serde(alias = "cmd")]
    command: Option<String>,
    #[serde(alias = "keep-alive")]
    remain: Option<bool>,
    run: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct Run {
    #[serde(alias = "cmd")]
    command: String,
}

#[derive(Debug, Clone, Default, Deserialize)]
struct Config {
    #[serde(default)]
    env: indexmap::IndexMap<String, String>,
    #[serde(default)]
    #[serde(alias = "window")]
    windows: Vec<Window>,
    #[serde(default)]
    run: Vec<Run>,
}

#[derive(Debug, Clone)]
struct SpawnWindow {
    name: String,
    dir: Option<PathBuf>,
    command: Option<WindowCommand>,
}

#[derive(Debug, Clone)]
enum WindowCommand {
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
    fn first(&self) -> &str {
        match self {
            Self::Command { command, .. } => command[0].as_str(),
            Self::Run { name, .. } => name.as_str(),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Action {
    env: Vec<(String, String)>,
    run: Vec<String>,
    windows: Vec<SpawnWindow>,
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
                trace!(?entry);
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
    secure: bool,
) -> Result<Option<Command>> {
    FuzzySelect::new()
        .with_prompt("Select a session or project: >")
        .set_query::<String>(query)
        .set_color(color)
        .with_options(entries)
        .with_select1()
        .select_opt()?
        .map(|entry| entry.cmd(secure))
        .transpose()
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
pub struct Args {
    #[clap(flatten)]
    verbose: Verbose<Global>,

    #[clap(flatten)]
    color: Color,

    /// Don't switch, just print the final tmux command.
    #[clap(long, short = 'n')]
    pub dry_run: bool,

    /// Skip initialization file permission checks.
    #[clap(long)]
    pub insecure: bool,

    #[clap(flatten)]
    pub selection: SelectionArgs,

    #[clap(trailing_var_arg = true)]
    pub query: Vec<String>,

    #[clap(skip)]
    use_color: bool,
}

impl Args {
    /// Initialize the command line arguments.
    ///
    /// # Panics
    /// Panics if the [`color_eyre`] is already initialized.
    pub fn init() -> Self {
        let mut args = setup_clap::<Self>()
            .color_from(|a| a.color)
            .verbose_from(pkg_name!(), |a| a.verbose)
            .run();

        let info = Info::new();
        setup_color_eyre_builder()
            .capture_span_trace_by_default(cfg!(debug_assertions))
            .display_location_section(cfg!(debug_assertions))
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .add_issue_metadata("version", info.build_version)
            .add_issue_metadata("build_at", info.build_timestamp)
            .add_issue_metadata("built_from", info.commit_sha)
            .add_issue_metadata("build_rust_version", info.rustc_version)
            .add_issue_metadata("host_triple", info.host_triple)
            .add_issue_metadata("target_triple", info.target_triple)
            .add_issue_metadata("cargo_profile", info.cargo_profile)
            .issue_filter(|o| match o {
                ErrorKind::NonRecoverable(_) => true,
                ErrorKind::Recoverable(e) => {
                    !std::iter::successors(Some(e), |e| e.source()).any(|e| {
                        let msg = e.to_string();
                        msg.contains("Window[") || msg.contains("Run[")
                    })
                }
            })
            .install()
            .expect("failed to install color_eyre");

        args.use_color = concolor::get(concolor::Stream::Stdout).color();

        debug!(
            ?args,
            color = args.use_color,
            filter =% verbosity_filter!(args.verbose.verbosity()),
        );

        args
    }

    fn query(&self) -> Option<String> {
        Some(self.query.join(" ")).filter(|q| !q.is_empty())
    }
}

#[derive(Copy, Clone, Debug, clap::Args)]
#[group(multiple = false)]
pub struct SelectionArgs {
    /// Only include running tmux sessions.
    #[clap(long, short = 't')]
    pub tmux_only: bool,

    /// Only include project directories.
    #[clap(long, short = 'p')]
    pub projects_only: bool,
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
