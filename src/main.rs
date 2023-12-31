#![warn(
    bad_style,
    dead_code,
    improper_ctypes,
    missing_copy_implementations,
    missing_debug_implementations,
    no_mangle_generic_items,
    non_shorthand_field_patterns,
    overflowing_literals,
    path_statements,
    patterns_in_fns_without_body,
    private_bounds,
    private_interfaces,
    rust_2018_idioms,
    trivial_casts,
    trivial_numeric_casts,
    unconditional_recursion,
    unsafe_code,
    unused,
    unused_allocation,
    unused_comparisons,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_parens,
    unused_qualifications,
    unused_results
)]
#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(clippy::multiple_crate_versions)]

use std::{
    cmp::Reverse,
    fmt::Display,
    io::{StderrLock, Write},
    os::unix::process::CommandExt,
    path::{Path, PathBuf},
    process::Command,
    sync::{
        mpsc::{self, SyncSender},
        Arc,
    },
    thread,
};

use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyModifiers},
    style::{self, ContentStyle, StyledContent, Stylize},
    terminal::{self, ClearType},
    QueueableCommand,
};
use ignore::WalkBuilder;
use kommandozeile::{
    clap,
    color_eyre::{
        eyre::{eyre, OptionExt, Report},
        Section,
    },
    concolor, pkg_name, setup_clap, setup_color_eyre_builder,
    tracing::{debug, info, warn},
    verbosity_filter, Color, Global, Result, Verbose,
};
use nucleo::{
    chars::graphemes,
    pattern::{CaseMatching, Normalization},
    Config, Matcher, Nucleo, Snapshot,
};
use panic_message::panic_message;

fn main() -> Result<()> {
    let args = setup_clap::<Args>()
        .color_from(|a| a.color)
        .verbose_from(pkg_name!(), |a| a.verbose)
        .run();

    setup_color_eyre_builder()
        .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
        .add_issue_metadata("version", env!("CARGO_PKG_VERSION"))
        .install()?;

    if args.version {
        let info = Info::default();
        println!("{} {}", env!("CARGO_PKG_NAME"), info.build_version);
        if args.verbose.verbosity() >= kommandozeile::Verbosity::Info {
            println!("{info}");
        }

        return Ok(());
    }

    let home = home::home_dir().ok_or_eyre("failed to get user home directory")?;

    let color = concolor::get(concolor::Stream::Stdout).color();
    let filter = verbosity_filter!(args.verbose.verbosity());

    debug!(
        ?args,
        color = color,
        filter =% filter,
    );

    let (tx, entries) = spawn_collector();

    let tmux_ls = (!args.selection.projects_only).then(|| find_tmux_sessions(&tx));

    if !args.selection.tmux_only {
        find_projects(&home, &tx);
    }

    let _ = tmux_ls.map(Thread::join).transpose()?;

    drop(tx);

    let entries = entries.join()?;

    debug!("found {} entries", entries.len());

    let Some(mut cmd) = make_selection(entries, color)? else {
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

    fn project_root(&self) -> Option<impl Display + '_> {
        if let Self::Project(p) = self {
            std::iter::successors(Some(p.root.as_path()), |p| p.parent())
                .nth(p.depth)
                .map(|p| p.display())
        } else {
            None
        }
    }

    fn session_info(&self) -> Option<String> {
        if let Self::Session(tmux) = self {
            Some(format!(" (root: {} {})", tmux.root.display(), tmux.info))
        } else {
            None
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

fn make_selection(entries: Vec<Entry>, color: bool) -> Result<Option<Command>> {
    Prompt::new().make_selection(entries, color)
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

struct Prompt {
    query: String,
    cursor_pos: usize,
    scroll_offset: u32,
    selected: u32,
    height: u32,
    active: bool,
    appending: bool,
    number_of_matches: u32,
    indices: Vec<u32>,
    content: String,
}

impl Prompt {
    fn new() -> Self {
        Self {
            query: String::new(),
            cursor_pos: 0,
            scroll_offset: 0,
            selected: 0,
            height: u32::from(terminal::size().map_or(10, |(_, h)| h.saturating_sub(1))),
            active: true,
            appending: true,
            number_of_matches: u32::MAX,
            indices: Vec::new(),
            content: String::new(),
        }
    }

    fn make_selection(mut self, entries: Vec<Entry>, color: bool) -> Result<Option<Command>> {
        let mut term = Terminal::new(color)?;

        let theme = Theme::default();
        let mut nucleo = Self::prepare_matcher(entries);
        let mut matcher = Matcher::new(Config::DEFAULT);

        term.queue(cursor::MoveTo(0, 0))
            .queue(terminal::Clear(ClearType::All))
            .queue(style::Print("Select a session or project: > "))
            .queue_(cursor::SavePosition);

        let key = loop {
            let res = self.tick(&theme, &mut term, &mut nucleo, &mut matcher)?;

            match res {
                Some(Stop::Quit) => {
                    return Ok(None);
                }
                Some(Stop::Selected(selected)) => {
                    break selected;
                }
                None => {}
            }
        };

        drop(term);

        let cmd = nucleo
            .snapshot()
            .get_matched_item(key)
            .map(|item| item.data.cmd());

        Ok(cmd)
    }

    fn prepare_matcher(entries: Vec<Entry>) -> Nucleo<Entry> {
        let nucleo = Nucleo::new(Config::DEFAULT, Arc::new(|| {}), None, 1);

        let injector = nucleo.injector();

        for entry in entries {
            let column = entry.search_content().to_owned();
            let _ = injector.push(entry, move |cols| {
                cols[0] = column.into();
            });
        }

        nucleo
    }

    fn tick(
        &mut self,
        theme: &Theme,
        term: &mut Terminal,
        nucleo: &mut Nucleo<Entry>,
        matcher: &mut Matcher,
    ) -> Result<Option<Stop>> {
        if self.active {
            let status = nucleo.tick(10);
            if status.changed {
                self.scroll_offset = 0;
                self.selected = 0;
            }

            let snap = nucleo.snapshot();
            self.render_items(theme, term, matcher, snap)?;
        }

        let key = crossterm::event::read()?;
        let handled = self.handle_event(&key);

        let query_changed = match handled {
            Handled::Unchanged => false,
            Handled::Changed => true,
            Handled::Stop(stop) => return Ok(Some(stop)),
        };

        if query_changed {
            nucleo.pattern.reparse(
                0,
                &self.query,
                CaseMatching::Smart,
                Normalization::Smart,
                self.appending,
            );
        }

        Ok(None)
    }

    fn render_items(
        &mut self,
        theme: &Theme,
        term: &mut Terminal,
        matcher: &mut Matcher,
        snap: &Snapshot<Entry>,
    ) -> Result<()> {
        self.number_of_matches = snap.matched_item_count();

        let end = self
            .scroll_offset
            .saturating_add(self.height)
            .min(snap.matched_item_count());
        let start = self.scroll_offset.min(end.saturating_sub(1));

        let matched_items = snap.matched_items(start..end).enumerate();

        for (idx, item) in matched_items {
            #[allow(clippy::cast_possible_truncation)]
            let idx = idx as u32 + start;
            let entry = item.data;

            let (indicator, text, hl) = if idx == self.selected {
                (
                    theme.selected_indicator,
                    &theme.selected_text,
                    &theme.selected_highlight,
                )
            } else {
                (theme.indicator, &theme.text, &theme.highlight)
            };

            term.queue(cursor::MoveToNextLine(1))
                .queue(terminal::Clear(ClearType::CurrentLine))
                .queue(style::PrintStyledContent(indicator))
                .queue_(style::PrintStyledContent(text.apply(" ")));

            if let Some(root) = entry.project_root() {
                term.queue(style::PrintStyledContent(text.apply(root)))
                    .queue_(style::PrintStyledContent(text.apply("/")));
            }

            let _score = snap.pattern().column_pattern(0).indices(
                item.matcher_columns[0].slice(..),
                matcher,
                &mut self.indices,
            );
            self.indices.sort_unstable();
            self.indices.dedup();

            let mut indices = self.indices.drain(..).map(|i| i as usize);
            let mut match_idx = indices.next().unwrap_or(usize::MAX);

            for (grapheme_idx, grapheme) in graphemes(entry.search_content()).enumerate() {
                if grapheme_idx == match_idx {
                    if !self.content.is_empty() {
                        term.queue_(style::PrintStyledContent(text.apply(self.content.as_str())));
                        self.content.clear();
                    }

                    term.queue_(style::PrintStyledContent(hl.apply(grapheme)));
                    match_idx = indices.next().unwrap_or(usize::MAX);
                } else {
                    self.content.push(grapheme);
                }
            }

            if !self.content.is_empty() {
                term.queue_(style::PrintStyledContent(text.apply(self.content.as_str())));
                self.content.clear();
            }

            if let Some(info) = entry.session_info() {
                term.queue_(style::Print(info));
            }
        }

        #[allow(clippy::cast_possible_truncation)]
        let cursor_offset = self
            .query
            .len()
            .saturating_sub(self.cursor_pos)
            .min(usize::from(u16::MAX)) as u16;

        term.queue(terminal::Clear(ClearType::FromCursorDown))
            .queue(cursor::RestorePosition)
            .queue(terminal::Clear(ClearType::UntilNewLine))
            .queue_(style::PrintStyledContent(self.query.as_str().bold()));

        if cursor_offset > 0 {
            term.queue_(cursor::MoveLeft(cursor_offset));
        }

        term.flush()?;

        Ok(())
    }

    fn handle_event(&mut self, event: &Event) -> Handled {
        match event {
            Event::Key(key) => return self.handle_key_event(key.code, key.modifiers),
            Event::FocusLost => self.active = false,
            Event::FocusGained => self.active = true,
            Event::Resize(_, h) => self.height = u32::from(h.saturating_sub(1)),
            Event::Mouse(_) | Event::Paste(_) => {}
        };

        Handled::Unchanged
    }

    fn handle_key_event(&mut self, code: KeyCode, modifiers: KeyModifiers) -> Handled {
        match (code, modifiers) {
            (KeyCode::Esc, _) => {
                if self.query.is_empty() {
                    Handled::Stop(Stop::Quit)
                } else {
                    self.query.clear();
                    self.cursor_pos = 0;
                    self.appending = false;
                    Handled::Changed
                }
            }
            (KeyCode::Char('c'), KeyModifiers::CONTROL) => Handled::Stop(Stop::Quit),
            (KeyCode::Enter | KeyCode::Char('\n' | '\r'), _) => {
                Handled::Stop(Stop::Selected(self.selected))
            }
            (KeyCode::Backspace, _) => match self.cursor_pos.checked_sub(1) {
                Some(pos) => {
                    let _ = self.query.remove(pos);
                    self.cursor_pos = pos;
                    self.appending = false;
                    Handled::Changed
                }
                _ => Handled::Unchanged,
            },
            (KeyCode::Delete, _) => {
                if self.cursor_pos < self.query.len() {
                    let _ = self.query.remove(self.cursor_pos);
                    self.appending = false;
                    Handled::Changed
                } else {
                    Handled::Unchanged
                }
            }
            (KeyCode::Home, _) => {
                self.cursor_pos = 0;
                self.appending = self.query.is_empty();
                Handled::Unchanged
            }
            (KeyCode::End, _) => {
                self.cursor_pos = self.query.len();
                self.appending = true;
                Handled::Unchanged
            }
            (KeyCode::Left, m) => {
                self.move_on_line(isize::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1)
            }
            (KeyCode::Right, m) => {
                self.move_on_line(isize::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1)
            }
            (KeyCode::Up, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * -9 - 1)
            }
            (KeyCode::Down, m) => {
                self.move_selection(i32::from(m.contains(KeyModifiers::SHIFT)) * 9 + 1)
            }
            (KeyCode::PageUp, _) => {
                self.scroll_offset = self.scroll_offset.saturating_sub(self.height);
                Handled::Unchanged
            }
            (KeyCode::PageDown, _) => {
                self.scroll_offset = self.scroll_offset.saturating_add(self.height);
                Handled::Unchanged
            }
            (KeyCode::Char(c), _) => {
                if self.cursor_pos == self.query.len() {
                    self.appending = true;
                    self.query.push(c);
                } else {
                    self.query.insert(self.cursor_pos, c);
                }
                self.cursor_pos += 1;
                Handled::Changed
            }
            _ => Handled::Unchanged,
        }
    }

    fn move_on_line(&mut self, diff: isize) -> Handled {
        self.cursor_pos = self
            .cursor_pos
            .saturating_add_signed(diff)
            .min(self.query.len());
        self.appending = self.cursor_pos == self.query.len();
        Handled::Unchanged
    }

    fn move_selection(&mut self, diff: i32) -> Handled {
        self.selected = self
            .selected
            .saturating_add_signed(diff)
            .min(self.number_of_matches.saturating_sub(1));

        if self.selected < self.scroll_offset {
            self.scroll_offset = self.selected;
        }

        if self.selected >= self.scroll_offset.saturating_add(self.height) {
            self.scroll_offset = self.selected.saturating_sub(self.height).saturating_add(1);
        }

        Handled::Unchanged
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Handled {
    Unchanged,
    Changed,
    Stop(Stop),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Stop {
    Quit,
    Selected(u32),
}

struct Terminal {
    io: StderrLock<'static>,
    err: Option<Report>,
}

impl Terminal {
    fn new(color: bool) -> Result<Self> {
        terminal::enable_raw_mode().map_err(|e| match e.raw_os_error() {
            Some(25 | 6) => eyre!("Cannot run sessionizer in non-interactive mode (not a tty)"),
            _ => eyre!("failed to enable raw mode: {}", e),
        })?;

        style::force_color_output(color);

        let mut io = std::io::stderr().lock();
        let _ = io.queue(terminal::EnterAlternateScreen)?;

        Ok(Self { io, err: None })
    }

    fn queue(&mut self, cmd: impl crossterm::Command) -> &mut Self {
        if let Err(e) = self.io.queue(cmd) {
            let ee = match self.err.take() {
                Some(err) => err.with_error(move || e),
                None => eyre!("failed to queue command: {}", e),
            };
            self.err = Some(ee);
        };
        self
    }

    fn queue_(&mut self, cmd: impl crossterm::Command) {
        let _ = self.queue(cmd);
    }

    fn flush(&mut self) -> Result<()> {
        if let Some(e) = self.err.take() {
            return Err(e);
        }

        self.io
            .flush()
            .map_err(|e| eyre!("failed to flush terminal: {}", e))?;

        Ok(())
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = self.queue(terminal::LeaveAlternateScreen).flush();
        let _ = terminal::disable_raw_mode();
    }
}

struct Theme {
    selected_indicator: StyledContent<&'static str>,
    indicator: StyledContent<&'static str>,
    selected_text: ContentStyle,
    text: ContentStyle,
    selected_highlight: ContentStyle,
    highlight: ContentStyle,
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            selected_indicator: ContentStyle::new().red().on_black().apply(">"),
            indicator: ContentStyle::new().on_black().apply(" "),
            selected_text: ContentStyle::new().on_black(),
            text: ContentStyle::new(),
            selected_highlight: ContentStyle::new().dark_cyan().on_black(),
            highlight: ContentStyle::new().cyan(),
        }
    }
}

/// Select a new tmux session from a list of running sessions or a selection of projects.
#[derive(Debug, clap::Parser)]
#[command(infer_long_args(true))]
struct Args {
    #[clap(flatten)]
    selection: SelectionArgs,

    /// Don't switch, just print the final tmux command.
    #[clap(long, short = 'n')]
    dry_run: bool,

    #[clap(flatten)]
    verbose: Verbose<Global>,

    #[clap(flatten)]
    color: Color,

    /// Print version
    #[arg(short = 'V', long)]
    version: bool,
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

impl Default for Info {
    fn default() -> Self {
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
