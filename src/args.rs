use std::{fmt::Display, sync::OnceLock};

use kommandozeile::{
    clap, color_eyre::ErrorKind, concolor, pkg_name, setup_clap, setup_color_eyre_builder,
    tracing::debug, verbosity_filter, Color, Global, Verbose,
};

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
    pub(crate) dry_run: bool,

    /// Skip initialization file permission checks.
    #[clap(long)]
    pub(crate) insecure: bool,

    #[clap(flatten)]
    pub(crate) selection: Selection,

    #[clap(trailing_var_arg = true)]
    query: Vec<String>,

    #[clap(skip)]
    pub(crate) use_color: bool,
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

    pub(crate) fn query(&self) -> Option<String> {
        Some(self.query.join(" ")).filter(|q| !q.is_empty())
    }
}

#[derive(Copy, Clone, Debug, clap::Args)]
#[group(multiple = false)]
pub struct Selection {
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
