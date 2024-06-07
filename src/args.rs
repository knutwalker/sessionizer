use std::{fmt::Display, sync::OnceLock};

use kommandozeile::{
    clap, color_eyre::ErrorKind, concolor, pkg_name, setup_clap, setup_color_eyre_builder,
    tracing::debug, verbosity_filter, Color, Global, Verbose,
};

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Action {
    Search(Search),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Search {
    pub dry_run: bool,
    pub insecure: bool,
    pub use_color: bool,
    pub scope: Scope,
    pub query: Option<String>,
}

impl Search {
    #[must_use]
    pub fn into_query(self) -> Option<String> {
        self.query
    }
}

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Scope {
    #[default]
    Both,
    ProjectsOnly,
    TmuxOnly,
}

impl Scope {
    #[must_use]
    pub const fn check_tmux(&self) -> bool {
        matches!(self, Self::TmuxOnly | Self::Both)
    }

    #[must_use]
    pub const fn check_projects(&self) -> bool {
        matches!(self, Self::ProjectsOnly | Self::Both)
    }
}

impl Action {
    #[must_use]
    pub fn cli() -> Self {
        let (args, use_color) = Args::init();
        Self::from_args(args, use_color)
    }

    #[cfg(test)]
    fn from_flags<const N: usize>(args: [&str; N]) -> Self {
        Self::try_from_flags(args).unwrap()
    }

    #[cfg(test)]
    fn try_from_flags<const N: usize>(args: [&str; N]) -> Result<Self, clap::Error> {
        use kommandozeile::clap::Parser as _;
        let args = std::iter::once(env!("CARGO_PKG_NAME")).chain(args);
        let args = Args::try_parse_from(args)?;
        Ok(Self::from_args(args, false))
    }

    fn from_args(args: Args, use_color: bool) -> Self {
        let search = args.command.search;
        let scope = match (search.selection.tmux_only, search.selection.projects_only) {
            (true, _) => Scope::TmuxOnly,
            (_, true) => Scope::ProjectsOnly,
            _ => Scope::Both,
        };
        Self::Search(Search {
            dry_run: search.dry_run,
            insecure: search.insecure,
            use_color,
            scope,
            query: Some(search.query.join(" ")).filter(|s| !s.is_empty()),
        })
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

    #[clap(flatten)]
    command: Cmd,
}

impl Args {
    /// Initialize the command line arguments.
    ///
    /// # Panics
    /// Panics if the [`color_eyre`] is already initialized.
    fn init() -> (Self, bool) {
        let args = setup_clap::<Self>()
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

        let use_color = concolor::get(concolor::Stream::Stdout).color();

        debug!(
            ?args,
            color = use_color,
            filter =% verbosity_filter!(args.verbose.verbosity()),
        );

        (args, use_color)
    }
}

#[derive(Clone, Debug, clap::Args)]
#[group(multiple = false)]
struct Cmd {
    #[clap(flatten)]
    search: SearchCmd,
}

#[derive(Clone, Debug, clap::Args)]
struct SearchCmd {
    /// Don't switch, just print the final tmux command.
    #[clap(long, short = 'n')]
    dry_run: bool,

    /// Skip initialization file permission checks.
    #[clap(long)]
    insecure: bool,

    #[clap(flatten)]
    selection: Selection,

    #[clap(trailing_var_arg = true)]
    query: Vec<String>,
}

#[derive(Copy, Clone, Debug, clap::Args)]
#[group(multiple = false)]
struct Selection {
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

#[cfg(test)]
mod tests {
    use kommandozeile::clap::error::{ContextKind, ContextValue, ErrorKind};
    use rstest::rstest;

    use super::*;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Args::command().debug_assert();
    }

    fn parse_search<const N: usize>(args: [&str; N]) -> Search {
        let action = Action::from_flags(args);
        let Action::Search(search) = action;
        search
    }

    macro_rules! assert_search {
        ($flags:expr, Search { $($field:ident: $value:expr),* $(,)?}) => {
            let action = parse_search($flags);
            assert_eq!(action, Search { $($field: $value),* , ..Default::default()});
        };
    }

    #[test]
    fn empty_args_is_default_search() {
        let action = parse_search([]);
        assert_eq!(action, Search::default());
    }

    #[test]
    fn trailing_args_are_search_query() {
        assert_search!(
            ["foo"],
            Search {
                query: Some("foo".into()),
            }
        );
    }

    #[test]
    fn concat_all_trailing_args() {
        assert_search!(
            ["foo", "bar", "baz"],
            Search {
                query: Some("foo bar baz".into()),
            }
        );
    }

    #[rstest]
    fn dry_run_flag_for_search(#[values("--dry-run", "-n")] flag: &str) {
        assert_search!([flag], Search { dry_run: true });
    }

    #[test]
    fn insecure_flag_for_search() {
        assert_search!(["--insecure"], Search { insecure: true });
    }

    #[rstest]
    fn tmux_only_scope(#[values("--tmux", "--tmux-only", "-t")] flag: &str) {
        assert_search!(
            [flag],
            Search {
                scope: Scope::TmuxOnly
            }
        );
    }

    #[rstest]
    fn projects_only_scope(#[values("--projects", "--projects-only", "-p")] flag: &str) {
        assert_search!(
            [flag],
            Search {
                scope: Scope::ProjectsOnly
            }
        );
    }

    #[test]
    fn tmux_and_projects_flag_are_mutually_exclusive() {
        let err = Action::try_from_flags(["--tmux", "--projects"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::ArgumentConflict);
        assert_eq!(
            err.get(ContextKind::InvalidArg),
            Some(&ContextValue::String("--tmux-only".into()))
        );
        assert_eq!(
            err.get(ContextKind::PriorArg),
            Some(&ContextValue::String("--projects-only".into()))
        );
    }

    #[test]
    fn all_search_flags() {
        assert_search!(
            ["-n", "--insecure", "--tmux", "foo", "bar"],
            Search {
                dry_run: true,
                insecure: true,
                scope: Scope::TmuxOnly,
                query: Some("foo bar".into())
            }
        );
    }
}
