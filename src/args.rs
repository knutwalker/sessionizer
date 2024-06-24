use std::{env, fmt, io, iter};

use clap::{value_parser, Arg, ArgAction, ArgGroup, ArgMatches, Command};

use crate::{config::ConfigError, debug};

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Action {
    Search(Search),
    Config(Config),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Config {
    Init,
    Validate { insecure: bool },
    Edit { insecure: bool },
}

impl Action {
    /// Initialize the CLI actions.
    ///
    /// # Panics
    /// Panics if the [`color_eyre`] is already initialized.
    /// Panics if there is an error in the CLI parsing.
    #[must_use]
    pub fn cli() -> Self {
        let app = Self::command().color(app_color());
        let mut matches = app.get_matches();

        let color = Self::init_color(&mut matches).ansi_color();
        Self::init_tracing(&mut matches, color);
        let args = Self::from_matches(&mut matches, color);

        debug!(?color, ?args);

        args
    }

    fn command() -> Command {
        Self::args(Command::new(env!("CARGO_PKG_NAME")))
    }

    fn args(app: Command) -> Command {
        app.about(concat!(
            "Select a new tmux session from a list of ",
            "running sessions or a selection of projects"
        ))
        .version(short_version())
        .long_version(long_version())
        .disable_help_subcommand(true)
        .infer_long_args(true)
        .args(Self::color_args())
        .args(Self::verbose_args())
        .args(Self::search_args())
        .args(Self::config_args())
        .group(ArgGroup::new("search").multiple(true).args([
            "dry_run",
            "tmux_only",
            "projects_only",
            "query",
        ]))
        .group(
            ArgGroup::new("Selection")
                .multiple(false)
                .args(["tmux_only", "projects_only"]),
        )
    }

    fn color_args() -> [Arg; 2] {
        [
            Arg::new("color")
                .long("color")
                .visible_alias("colour")
                .help("Control when to use colors")
                .value_parser(["auto", "always", "never"])
                .value_name("WHEN")
                .default_value("auto")
                .default_missing_value("always")
                .num_args(0..=1)
                .action(ArgAction::Set)
                .required(false),
            Arg::new("no_color")
                .long("no-color")
                .visible_alias("no-colour")
                .help("Disable the use of color. Implies `--color=never`")
                .value_parser(value_parser!(bool))
                .action(ArgAction::SetTrue)
                .required(false)
                .conflicts_with("color"),
        ]
    }

    fn verbose_args() -> [Arg; 2] {
        [
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .help("Print more logs, can be used multiple times")
                .value_parser(value_parser!(u8))
                .action(ArgAction::Count)
                .required(false)
                .conflicts_with("quiet"),
            Arg::new("quiet")
                .short('q')
                .long("quiet")
                .help("Print less logs, can be used multiple times")
                .value_parser(value_parser!(u8))
                .action(ArgAction::Count)
                .required(false)
                .conflicts_with("verbose"),
        ]
    }

    fn search_args() -> [Arg; 5] {
        [
            Arg::new("dry_run")
                .long("dry-run")
                .short('n')
                .help("Don't switch, just print the final tmux command")
                .value_parser(value_parser!(bool))
                .action(ArgAction::SetTrue)
                .required(false),
            Arg::new("insecure")
                .long("insecure")
                .short('i')
                .help("Skip initialization file permission checks")
                .value_parser(value_parser!(bool))
                .action(ArgAction::SetTrue)
                .required(false),
            Arg::new("tmux_only")
                .long("tmux-only")
                .short('t')
                .help("Only include running tmux sessions")
                .value_parser(value_parser!(bool))
                .action(ArgAction::SetTrue)
                .required(false),
            Arg::new("projects_only")
                .long("projects-only")
                .short('p')
                .help("Only include project directories")
                .value_parser(value_parser!(bool))
                .action(ArgAction::SetTrue)
                .required(false),
            Arg::new("query")
                .value_parser(value_parser!(String))
                .action(ArgAction::Append)
                .num_args(1..)
                .trailing_var_arg(true),
        ]
    }

    fn config_args() -> [Arg; 1] {
        use clap::builder::{PossibleValue, PossibleValuesParser};

        [Arg::new("config")
            .long("config")
            .short('c')
            .help("Create a new sessionizer configuration for this directory")
            .value_name("COMMAND")
            .conflicts_with("search")
            .value_parser(PossibleValuesParser::new([
                PossibleValue::new("init")
                    .help("Create a new sessionizer configuration for this directory")
                    .alias("i")
                    .alias("new")
                    .alias("n"),
                PossibleValue::new("validate")
                    .help("Validate the sessionizer config in this directory")
                    .alias("v")
                    .alias("check")
                    .alias("c"),
                PossibleValue::new("edit")
                    .help("Open the sessionizer config for this directory in $VISUAL | $EDITOR")
                    .alias("e"),
            ]))
            .action(ArgAction::Set)
            .required(false)]
    }

    fn init_color(matches: &mut ArgMatches) -> concolor::Color {
        let color = color_from_matches(matches);
        concolor::set(color);
        concolor::get(concolor::Stream::Stderr)
    }

    fn init_tracing(matches: &mut ArgMatches, color: bool) {
        use color_eyre::ErrorKind;
        use tracing_error::ErrorLayer;
        use tracing_subscriber::{fmt, layer::SubscriberExt as _, util::SubscriberInitExt as _};

        let filter = verbosity_from_matches(matches);
        let level = if cfg!(debug_assertions) { "full" } else { "0" };

        if env::var_os("RUST_LIB_BACKTRACE").is_none() {
            env::set_var("RUST_LIB_BACKTRACE", level);
        }
        if env::var_os("RUST_BACKTRACE").is_none() {
            env::set_var("RUST_BACKTRACE", level);
        }

        let fmt_layer = fmt::layer()
            .with_target(true)
            .with_file(true)
            .with_line_number(true)
            .with_writer(io::stderr)
            .with_ansi(color);

        tracing_subscriber::registry()
            .with(filter)
            .with(fmt_layer)
            .with(ErrorLayer::default())
            .init();

        let info = Info::new();
        let mut builder = color_eyre::config::HookBuilder::default();
        if !color {
            builder = builder.theme(color_eyre::config::Theme::new());
        }

        builder
            .display_env_section(false)
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
                ErrorKind::Recoverable(e) => iter::successors(Some(e), |e| e.source())
                    .find_map(|e| e.downcast_ref::<ConfigError>())
                    .map_or(true, |e| {
                        matches!(
                            e,
                            ConfigError::FileReading(_) | ConfigError::InvalidWindowDir(_)
                        )
                    }),
            })
            .install()
            .expect("failed to install color_eyre");
    }

    fn from_matches(matches: &mut ArgMatches, use_color: bool) -> Self {
        let insecure = matches.remove_one::<bool>("insecure").expect("flag");
        let config = matches.remove_one::<String>("config");
        if let Some(config) = config {
            let config = match config.as_str() {
                "init" | "i" | "new" | "n" => Config::Init,
                "validate" | "v" | "check" | "c" => Config::Validate { insecure },
                "edit" | "e" => Config::Edit { insecure },
                otherwise => unreachable!("{otherwise}: not a valid config command"),
            };
            return Self::Config(config);
        }

        let tmux_only = matches.remove_one::<bool>("tmux_only").expect("flag");
        let projects_only = matches.remove_one::<bool>("projects_only").expect("flag");
        let scope = match (tmux_only, projects_only) {
            (true, _) => Scope::TmuxOnly,
            (_, true) => Scope::ProjectsOnly,
            _ => Scope::Both,
        };

        let dry_run = matches.remove_one::<bool>("dry_run").expect("flag");
        let query = matches
            .remove_many::<String>("query")
            .map(|q| {
                q.zip(iter::repeat(" "))
                    .fold(String::new(), |mut s, (q, sep)| {
                        s.push_str(&q);
                        s.push_str(sep);
                        s
                    })
            })
            .and_then(|q| {
                let q = q.trim();
                (!q.is_empty()).then(|| q.to_owned())
            });

        Self::Search(Search {
            dry_run,
            insecure,
            use_color,
            scope,
            query,
        })
    }

    #[cfg(test)]
    fn from_flags<const N: usize>(args: [&str; N]) -> Self {
        Self::try_from_flags(args).unwrap()
    }

    #[cfg(test)]
    fn try_from_flags<const N: usize>(args: [&str; N]) -> Result<Self, clap::Error> {
        Self::get_matches(args)
            .map(|mut m| Self::from_matches(&mut m, false))
            .map_err(|e| e.format(&mut Self::command()))
    }

    #[cfg(test)]
    fn get_matches<const N: usize>(args: [&str; N]) -> Result<ArgMatches, clap::Error> {
        let args = iter::once(env!("CARGO_PKG_NAME")).chain(args);
        Self::command().try_get_matches_from(args)
    }
}

fn app_color() -> clap::ColorChoice {
    let color = concolor::get(concolor::Stream::Stdout);
    if color.ansi_color() {
        clap::ColorChoice::Always
    } else {
        clap::ColorChoice::Never
    }
}

fn color_from_matches(matches: &mut ArgMatches) -> concolor::ColorChoice {
    use concolor::ColorChoice::{Always, Auto, Never};

    let no_color = matches.remove_one::<bool>("no_color").unwrap_or(false);
    if no_color {
        Never
    } else {
        match matches
            .remove_one::<String>("color")
            .expect("default value")
            .as_str()
        {
            "auto" => Auto,
            "always" => Always,
            "never" => Never,
            otherwise => unreachable!("{otherwise}: not a valid color choice"),
        }
    }
}

fn verbosity_from_matches(matches: &mut ArgMatches) -> tracing_subscriber::filter::Targets {
    use tracing::level_filters::LevelFilter;

    let verbose = matches.remove_one::<u8>("verbose").expect("count type");
    let quiet = matches.remove_one::<u8>("quiet").expect("count type");

    let verbosity = i32::from(verbose) - i32::from(quiet);
    let verbosity = 2_u32.saturating_add_signed(verbosity);
    let targets = tracing_subscriber::filter::Targets::default();
    match verbosity {
        0 => targets.with_default(LevelFilter::OFF),
        1 => targets.with_default(LevelFilter::ERROR),
        2 => targets.with_default(LevelFilter::WARN),
        3 => targets.with_target(env!("CARGO_PKG_NAME"), LevelFilter::INFO),
        4 => targets.with_target(env!("CARGO_PKG_NAME"), LevelFilter::DEBUG),
        5 => targets.with_target(env!("CARGO_PKG_NAME"), LevelFilter::TRACE),
        6 => targets
            .with_target(env!("CARGO_PKG_NAME"), LevelFilter::TRACE)
            .with_default(LevelFilter::INFO),
        7 => targets
            .with_target(env!("CARGO_PKG_NAME"), LevelFilter::TRACE)
            .with_default(LevelFilter::DEBUG),
        _ => targets.with_default(LevelFilter::TRACE),
    }
}

const fn short_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn long_version() -> &'static str {
    use std::sync::OnceLock;
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
    commit_sha_short: &'static str,
    commit_date: &'static str,
    rustc_version: &'static str,
    rustc_channel: &'static str,
    host_triple: &'static str,
    target_triple: &'static str,
    cargo_profile: &'static str,
}

impl fmt::Display for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:<20} {}", "Build Version:", self.build_version)?;
        writeln!(f, "{:<20} {}", "Build Timestamp:", self.build_timestamp)?;
        writeln!(f, "{:<20} {}", "Commit SHA:", self.commit_sha)?;
        writeln!(f, "{:<20} {}", "Commit Short SHA:", self.commit_sha_short)?;
        writeln!(f, "{:<20} {}", "Commit Date:", self.commit_date)?;
        writeln!(f, "{:<20} {}", "rustc Version:", self.rustc_version)?;
        writeln!(f, "{:<20} {}", "rustc Channel:", self.rustc_channel)?;
        writeln!(f, "{:<20} {}", "Host Triple:", self.host_triple)?;
        writeln!(f, "{:<20} {}", "Target Triple:", self.target_triple)?;
        writeln!(f, "{:<20} {}", "Cargo Profile:", self.cargo_profile)?;
        Ok(())
    }
}

impl Info {
    const fn new() -> Self {
        Self {
            build_version: env!("CARGO_PKG_VERSION"),
            build_timestamp: env!("SESSIONIZER_BUILD_TIMESTAMP"),
            commit_sha: env!("SESSIONIZER_GIT_SHA"),
            commit_sha_short: env!("SESSIONIZER_GIT_SHA_SHORT"),
            commit_date: env!("SESSIONIZER_GIT_COMMIT_TIMESTAMP"),
            rustc_version: env!("SESSIONIZER_RUSTC_VERSION"),
            rustc_channel: env!("SESSIONIZER_RUSTC_CHANNEL"),
            host_triple: env!("SESSIONIZER_HOST_TRIPLE"),
            target_triple: env!("SESSIONIZER_TARGET_TRIPLE"),
            cargo_profile: env!("SESSIONIZER_CARGO_PROFILE"),
        }
    }
}

#[cfg(test)]
mod tests {
    use clap::error::{ContextKind, ContextValue, ErrorKind};
    use tracing::Level;

    use super::*;

    #[test]
    fn verify_cli() {
        Action::command().debug_assert();
    }

    #[test]
    fn no_color_flag() {
        let mut matches = Action::get_matches(["--no-color"]).unwrap();
        let when = color_from_matches(&mut matches);
        assert_eq!(when, concolor::ColorChoice::Never);
    }

    #[test]
    fn auto_color_flag() {
        let mut matches = Action::get_matches(["--color", "auto"]).unwrap();
        let when = color_from_matches(&mut matches);
        assert_eq!(when, concolor::ColorChoice::Auto);
    }

    #[test]
    fn always_color_flag() {
        let mut matches = Action::get_matches(["--color", "always"]).unwrap();
        let when = color_from_matches(&mut matches);
        assert_eq!(when, concolor::ColorChoice::Always);
    }

    #[test]
    fn never_color_flag() {
        let mut matches = Action::get_matches(["--color", "never"]).unwrap();
        let when = color_from_matches(&mut matches);
        assert_eq!(when, concolor::ColorChoice::Never);
    }

    #[test]
    fn just_color_equals_always() {
        let mut matches = Action::get_matches(["--color"]).unwrap();
        let when = color_from_matches(&mut matches);
        assert_eq!(when, concolor::ColorChoice::Always);
    }

    #[test]
    fn single_verbosity_long_flag() {
        test_single_verbosity_flag("--verbose");
    }

    #[test]
    fn single_verbosity_short_flag() {
        test_single_verbosity_flag("-v");
    }

    fn test_single_verbosity_flag(flag: &str) {
        let mut matches = Action::get_matches([flag]).unwrap();
        let verbosity = verbosity_from_matches(&mut matches);
        assert!(verbosity.would_enable(env!("CARGO_PKG_NAME"), &Level::INFO));
        assert!(!verbosity.would_enable(env!("CARGO_PKG_NAME"), &Level::DEBUG));
    }

    #[test]
    fn single_quiet_long_flag() {
        test_single_quiet_flag("--quiet");
    }

    #[test]
    fn single_quiet_short_flag() {
        test_single_quiet_flag("-q");
    }

    fn test_single_quiet_flag(flag: &str) {
        let mut matches = Action::get_matches([flag]).unwrap();
        let verbosity = verbosity_from_matches(&mut matches);
        assert!(verbosity.would_enable(env!("CARGO_PKG_NAME"), &Level::ERROR));
        assert!(!verbosity.would_enable(env!("CARGO_PKG_NAME"), &Level::WARN));
    }

    #[test]
    fn multiple_verbosity_long_flag() {
        test_level_flags::<1>(
            "--verbose",
            &[
                ("sessionizer", Level::INFO, true),
                ("sessionizer", Level::DEBUG, false),
            ],
        );

        test_level_flags::<2>(
            "--verbose",
            &[
                ("sessionizer", Level::DEBUG, true),
                ("sessionizer", Level::TRACE, false),
            ],
        );

        test_level_flags::<3>(
            "--verbose",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::INFO, false),
            ],
        );

        test_level_flags::<4>(
            "--verbose",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::INFO, true),
                ("global", Level::DEBUG, false),
            ],
        );

        test_level_flags::<5>(
            "--verbose",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::DEBUG, true),
                ("global", Level::TRACE, false),
            ],
        );

        test_level_flags::<6>(
            "--verbose",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::TRACE, true),
            ],
        );

        test_level_flags::<7>(
            "--verbose",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::TRACE, true),
            ],
        );
    }

    #[test]
    fn multiple_verbosity_short_flag() {
        test_level_flags::<1>(
            "-v",
            &[
                ("sessionizer", Level::INFO, true),
                ("sessionizer", Level::DEBUG, false),
            ],
        );

        test_level_flags::<2>(
            "-v",
            &[
                ("sessionizer", Level::DEBUG, true),
                ("sessionizer", Level::TRACE, false),
            ],
        );

        test_level_flags::<3>(
            "-v",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::INFO, false),
            ],
        );

        test_level_flags::<4>(
            "-v",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::INFO, true),
                ("global", Level::DEBUG, false),
            ],
        );

        test_level_flags::<5>(
            "-v",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::DEBUG, true),
                ("global", Level::TRACE, false),
            ],
        );

        test_level_flags::<6>(
            "-v",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::TRACE, true),
            ],
        );

        test_level_flags::<7>(
            "-v",
            &[
                ("sessionizer", Level::TRACE, true),
                ("global", Level::TRACE, true),
            ],
        );
    }

    #[test]
    fn multiple_quiet_long_flag() {
        test_level_flags::<1>(
            "--quiet",
            &[
                ("sessionizer", Level::ERROR, true),
                ("sessionizer", Level::WARN, false),
            ],
        );

        test_level_flags::<2>(
            "--quiet",
            &[
                ("sessionizer", Level::ERROR, false),
                ("sessionizer", Level::WARN, false),
            ],
        );

        test_level_flags::<3>(
            "--quiet",
            &[
                ("sessionizer", Level::ERROR, false),
                ("sessionizer", Level::WARN, false),
            ],
        );
    }

    #[test]
    fn multiple_quiet_short_flag() {
        test_level_flags::<1>(
            "-q",
            &[
                ("sessionizer", Level::ERROR, true),
                ("sessionizer", Level::WARN, false),
            ],
        );

        test_level_flags::<2>(
            "-q",
            &[
                ("sessionizer", Level::ERROR, false),
                ("sessionizer", Level::WARN, false),
            ],
        );

        test_level_flags::<3>(
            "-q",
            &[
                ("sessionizer", Level::ERROR, false),
                ("sessionizer", Level::WARN, false),
            ],
        );
    }

    fn test_level_flags<const REPEAT: usize>(flag: &str, expected: &[(&str, Level, bool)]) {
        let mut matches = Action::get_matches([flag; REPEAT]).unwrap();
        let verbosity = verbosity_from_matches(&mut matches);
        for (name, level, enabled) in expected {
            assert_eq!(verbosity.would_enable(name, level), *enabled);
        }
    }

    fn parse_search<const N: usize>(args: [&str; N]) -> Search {
        let action = Action::from_flags(args);
        let Action::Search(search) = action else {
            panic!("not a search")
        };
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

    #[test]
    fn dry_run_long_flag_for_search() {
        assert_search!(["--dry-run"], Search { dry_run: true });
    }

    #[test]
    fn dry_run_short_flag_for_search() {
        assert_search!(["-n"], Search { dry_run: true });
    }

    #[test]
    fn insecure_flag_for_search() {
        assert_search!(["--insecure"], Search { insecure: true });
    }

    #[test]
    fn tmux_only_scope_long() {
        assert_search!(
            ["--tmux"],
            Search {
                scope: Scope::TmuxOnly
            }
        );
    }

    #[test]
    fn tmux_only_scope_very_long() {
        assert_search!(
            ["--tmux-only"],
            Search {
                scope: Scope::TmuxOnly
            }
        );
    }

    #[test]
    fn tmux_only_scope_short() {
        assert_search!(
            ["-t"],
            Search {
                scope: Scope::TmuxOnly
            }
        );
    }

    #[test]
    fn projects_only_scope_long() {
        assert_search!(
            ["--projects"],
            Search {
                scope: Scope::ProjectsOnly
            }
        );
    }

    #[test]
    fn projects_only_scope_very_long() {
        assert_search!(
            ["--projects-only"],
            Search {
                scope: Scope::ProjectsOnly
            }
        );
    }

    #[test]
    fn projects_only_scope_short() {
        assert_search!(
            ["-p"],
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

    fn parse_config<const N: usize>(args: [&str; N]) -> Config {
        let action = Action::from_flags(args);
        let Action::Config(config) = action else {
            panic!("not a config")
        };
        config
    }

    macro_rules! assert_config {
        ($flags:expr, $config:expr) => {
            let action = parse_config($flags);
            assert_eq!(action, $config);
        };
    }

    #[test]
    fn config_init() {
        assert_config!(["--config", "init"], Config::Init);
        assert_config!(["-c", "init"], Config::Init);
        assert_config!(["--config=init"], Config::Init);
        assert_config!(["--config", "i"], Config::Init);
        assert_config!(["--config", "new"], Config::Init);
        assert_config!(["--config", "n"], Config::Init);
    }

    #[test]
    fn config_validate() {
        assert_config!(
            ["--config", "validate"],
            Config::Validate { insecure: false }
        );
        assert_config!(["-c", "validate"], Config::Validate { insecure: false });
        assert_config!(["--config=validate"], Config::Validate { insecure: false });
        assert_config!(["--config", "v"], Config::Validate { insecure: false });
        assert_config!(["--config", "check"], Config::Validate { insecure: false });
        assert_config!(["--config", "c"], Config::Validate { insecure: false });
    }

    #[test]
    fn config_edit() {
        assert_config!(["--config", "edit"], Config::Edit { insecure: false });
        assert_config!(["-c", "edit"], Config::Edit { insecure: false });
        assert_config!(["--config=edit"], Config::Edit { insecure: false });
        assert_config!(["--config", "e"], Config::Edit { insecure: false });
    }

    #[test]
    fn search_and_config_are_mutually_exclusive() {
        let err = Action::try_from_flags(["--dry-run", "--config", "new"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::ArgumentConflict);
        assert_eq!(
            err.get(ContextKind::InvalidArg),
            Some(&ContextValue::String("--config <COMMAND>".into()))
        );
        assert_eq!(
            err.get(ContextKind::PriorArg),
            Some(&ContextValue::Strings(vec![
                "--dry-run".to_owned(),
                "--tmux-only".to_owned(),
                "--projects-only".to_owned(),
                "[query]...".to_owned(),
            ]))
        );
    }

    #[test]
    fn insecure_and_config_together_are_allowed() {
        assert_config!(
            ["--insecure", "--config", "edit"],
            Config::Edit { insecure: true }
        );
        assert_config!(
            ["--config", "edit", "--insecure"],
            Config::Edit { insecure: true }
        );
    }

    #[test]
    fn config_requires_command() {
        let err = Action::try_from_flags(["--config"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidValue);
        assert_eq!(
            err.get(ContextKind::InvalidArg),
            Some(&ContextValue::String("--config <COMMAND>".into()))
        );
        assert_eq!(
            err.get(ContextKind::InvalidValue),
            Some(&ContextValue::String(String::new()))
        );
        assert_eq!(
            err.get(ContextKind::ValidValue),
            Some(&ContextValue::Strings(vec![
                "init".into(),
                "validate".into(),
                "edit".into()
            ]))
        );
    }

    #[test]
    fn config_requires_valid_command() {
        let err = Action::try_from_flags(["--config", "frobnicate"]).unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidValue);
        assert_eq!(
            err.get(ContextKind::InvalidArg),
            Some(&ContextValue::String("--config <COMMAND>".into()))
        );
        assert_eq!(
            err.get(ContextKind::InvalidValue),
            Some(&ContextValue::String("frobnicate".into()))
        );
        assert_eq!(
            err.get(ContextKind::ValidValue),
            Some(&ContextValue::Strings(vec![
                "init".into(),
                "validate".into(),
                "edit".into()
            ]))
        );
    }
}
