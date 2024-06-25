use std::{
    env,
    ffi::OsStr,
    fmt::Debug,
    ops::Range,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    process::Command,
    sync::mpsc::SyncSender,
};

use ignore::{DirEntry, WalkBuilder, WalkState};
use onlyerror::Error;

use crate::{debug, info, trace, warn, Entry, Project, Result};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct SearchPath {
    path: PathBuf,
    depth: Range<usize>,
}

impl SearchPath {
    fn new(path: impl Into<PathBuf>, depth: Range<usize>) -> Self {
        Self {
            path: path.into(),
            depth,
        }
    }
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Error)]
pub enum ProjectError {
    #[error("Path `{0:?}`: {1}")]
    ParsePathError(PathBuf, #[source] ParsePathError),
    #[error("Path `{0:?}` does not exist")]
    NotFound(PathBuf),
    #[error("Path `{0:?}` could not be canonicalized: {1}")]
    CanonicalizationFailed(PathBuf, #[source] std::io::Error),
}

#[allow(clippy::module_name_repetitions)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Error)]
pub enum ParsePathError {
    #[error("Invalid path component after `_`, only `_` or `*` are allowed")]
    ComponentAfterUnderscore,
    #[error("Invalid path component after `*`, only `*` is allowed")]
    ComponentAfterStar,
    #[error("The path may not begin with `_` or `*`.")]
    PathBeginning,
    #[error("The path may not end with `_`.")]
    PathEnd,
    #[error("The path must not be empty")]
    Empty,
    #[error("The path must be absolute")]
    PathNotAbsolute,
    #[error("Unknown home directory to do tilde expansion")]
    UnknownHomeDirectory,
    #[error("Home expansion is only supported with a username")]
    HomeWithUsername,
    #[error("Unknown error")]
    Unknown,
}

pub fn find_projects(tx: &SyncSender<Entry>) -> Result<(), ProjectError> {
    let paths = read_sessionizer_path()
        .or_else(read_cdpath)
        .or_else(query_zoxide)
        .or_else(use_last_resort)
        .transpose()?
        .unwrap_or_default();

    if paths.is_empty() {
        info!(concat!(
            "Could not find any paths to search for projects.",
            "This could be due to a missing $HOME.",
            "If configured, $SESSIONIZER_PATH or $CDPATH are also used, in that order."
        ));
        return Ok(());
    };

    find_projects_in(paths, tx);

    Ok(())
}

fn read_sessionizer_path() -> Option<Result<Vec<SearchPath>, ProjectError>> {
    env::var_os("SESSIONIZER_PATH").map(|path| parse_paths(env::split_paths(&path), false, Err))
}

fn read_cdpath() -> Option<Result<Vec<SearchPath>, ProjectError>> {
    env::var_os("CDPATH").map(|path| parse_paths(env::split_paths(&path), true, |e| match e {
        ProjectError::ParsePathError(p, ParsePathError::PathNotAbsolute) => {
            debug!(path =% p.display(), "Skipping path from $CDPATH because it is not absolute.");
            Ok(None)
        },
        ProjectError::NotFound(p) => {
            debug!(path =% p.display(), "Skipping path from $CDPTH because it does not exist.");
            Ok(None)
        },
        e => Err(e),
    }))
}

fn query_zoxide() -> Option<Result<Vec<SearchPath>, ProjectError>> {
    Command::new("zoxide")
        .arg("query")
        .arg("--list")
        .output()
        .ok()
        .map(|o| {
            Ok(o.stdout
                .split(|c| *c == b'\n')
                .filter_map(|p| {
                    let path = OsStr::from_bytes(p);
                    if path.is_empty() {
                        return None;
                    }

                    let path = Path::new(path);
                    let path = path.canonicalize().ok()?;

                    if path.is_relative() {
                        warn!("Found relative path in zoxide output: {p:?}");
                        return None;
                    }

                    Some(path)
                })
                .map(|p| SearchPath::new(p, 0..1))
                .collect())
        })
}

fn use_last_resort() -> Option<Result<Vec<SearchPath>, ProjectError>> {
    warn!("No SESSIONIZER_PATH set, showing only ~/.config as result.");
    let home = home_dir()?;
    Some(Ok(vec![SearchPath::new(home.join(".config"), 1..2)]))
}

#[cfg(not(test))]
fn home_dir() -> Option<&'static Path> {
    use std::sync::OnceLock;

    static HOME: OnceLock<Option<PathBuf>> = OnceLock::new();

    HOME.get_or_init(|| {
        home::home_dir().map_or_else(
            || {
                info!(concat!(
                    "Could not find the home directory.",
                    "This can impact the projects that are found.",
                    "That is, tilde expansion and fallback paths are not available."
                ));
                None
            },
            Some,
        )
    })
    .as_deref()
}

#[cfg(test)]
#[allow(clippy::unnecessary_wraps)]
fn home_dir() -> Option<&'static Path> {
    use home as _;
    Some(Path::new("/home/morpheus"))
}

fn parse_paths(
    paths: impl IntoIterator<Item = PathBuf>,
    auto_star: bool,
    recover: impl Fn(ProjectError) -> Result<Option<SearchPath>, ProjectError>,
) -> Result<Vec<SearchPath>, ProjectError> {
    paths
        .into_iter()
        .map(|p| match parse_path(&p, auto_star) {
            Ok(path) => path
                .path
                .canonicalize()
                .map(|p| SearchPath { path: p, ..path })
                .map_err(|e| -> ProjectError {
                    match e.kind() {
                        std::io::ErrorKind::NotFound => ProjectError::NotFound(path.path),
                        _ => ProjectError::CanonicalizationFailed(path.path, e),
                    }
                }),
            Err(e) => Err(ProjectError::ParsePathError(p, e)),
        })
        .filter_map(|path| path.map(Some).or_else(&recover).transpose())
        .collect::<Result<Vec<_>, _>>()
}

fn parse_path2(path: &Path, auto_star: bool) -> Result<SearchPath, ParsePathError> {
    use winnow::{
        combinator::{alt, delimited, eof, fail, repeat, terminated, trace},
        token::take_until,
        PResult, Parser,
    };

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum Tok2<'s> {
        Home(&'s Path),
        Regular(&'s [u8]),
        Current,
        Underscore,
        Star,
    }

    impl AsRef<OsStr> for Tok2<'_> {
        fn as_ref(&self) -> &OsStr {
            match self {
                Self::Home(path) => path.as_os_str(),
                Self::Regular(s) => OsStr::from_bytes(s),
                Self::Current => OsStr::new("."),
                Self::Underscore => OsStr::new("_"),
                Self::Star => OsStr::new("*"),
            }
        }
    }

    const SEP: &[u8] = std::path::MAIN_SEPARATOR_STR.as_bytes();

    fn path_segment<'s>(bytes: &mut &'s [u8]) -> PResult<&'s [u8]> {
        trace("path_segment", take_until(1.., SEP)).parse_next(bytes)
    }

    fn separator(bytes: &mut &[u8]) -> PResult<Option<()>> {
        trace("separator", alt((SEP.value(Some(())), eof.value(None)))).parse_next(bytes)
    }

    fn path_token<'s>(bytes: &mut &'s [u8]) -> PResult<Tok2<'s>> {
        trace(
            "path_token",
            alt((
                terminated(b"_", separator).value(Tok2::Underscore),
                terminated(b"*", separator).value(Tok2::Star),
                terminated(b".", separator).value(Tok2::Current),
                terminated(br"\_", separator).value(Tok2::Regular(b"_")),
                terminated(br"\*", separator).value(Tok2::Regular(b"*")),
                terminated(path_segment, separator).map(Tok2::Regular),
            )),
        )
        .parse_next(bytes)
    }

    fn home(bytes: &mut &[u8]) -> PResult<&'static Path> {
        trace(
            "home",
            delimited(b"~", path_segment, SEP).try_map(|username| {
                if username.is_empty() {
                    home_dir().ok_or(ParsePathError::UnknownHomeDirectory)
                } else {
                    Err(ParsePathError::HomeWithUsername)
                }
            }),
        )
        .parse_next(bytes)
    }

    fn full_path(bytes: &mut &[u8]) -> PResult<PathBuf> {
        trace(
            "full_path",
            alt((
                trace("root", SEP.value(Tok2::Regular(SEP))),
                home.map(Tok2::Home),
                fail,
            ))
            .flat_map(|root| {
                trace(
                    "non_root_path",
                    repeat(0.., path_token).fold(
                        {
                            let len = bytes.len();
                            move || {
                                let mut path = PathBuf::with_capacity(len);
                                path.push(root.as_ref());
                                path
                            }
                        },
                        |mut acc, token| {
                            acc.push(token.as_ref());
                            acc
                        },
                    ),
                )
            }),
        )
        .parse_next(bytes)
    }

    let bytes = path.as_os_str().as_bytes();
    let res = dbg!(full_path.parse(bytes).map_err(|e| {
        let cause = dbg!(dbg!(dbg!(&e).inner().cause())
            .and_then(|e| e.downcast_ref::<ParsePathError>())
            .copied());
        cause.unwrap_or(ParsePathError::PathNotAbsolute)
    }))?;

    Ok(SearchPath {
        path: res,
        depth: 13..37,
    })
}

fn parse_path(path: &Path, auto_star: bool) -> Result<SearchPath, ParsePathError> {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum State {
        Beginning,
        Regular,
        Underscore,
        Star,
    }

    let _ = dbg!(parse_path2(path, auto_star));
    let all_components = tokenize(path);

    let mut path = PathBuf::with_capacity(path.as_os_str().len());
    let mut min_depth = 0;
    let mut search_depth = 1;
    let mut state = State::Beginning;

    for component in all_components {
        match (state, component) {
            (State::Beginning, Token::Underscore | Token::Star) => {
                return Err(ParsePathError::PathBeginning);
            }
            (State::Beginning, Token::End) => {
                return Err(ParsePathError::Empty);
            }
            (State::Beginning, Token::Regular(component)) => {
                if component == "~" {
                    let home = home_dir().ok_or(ParsePathError::UnknownHomeDirectory)?;
                    path.push(home);
                } else {
                    path.push(component);
                }
                state = State::Regular;
            }
            (State::Regular, Token::Regular(component)) => {
                path.push(component);
            }
            (State::Regular, Token::Underscore) => {
                min_depth = 2;
                state = State::Underscore;
            }
            (State::Regular, Token::Star) => {
                min_depth = 1;
                state = State::Star;
            }
            (State::Regular, Token::End) => {
                min_depth = usize::from(auto_star);
            }
            (State::Underscore, Token::Regular(_)) => {
                return Err(ParsePathError::ComponentAfterUnderscore);
            }
            (State::Underscore, Token::Underscore) => {
                min_depth += 1;
            }
            (State::Underscore, Token::Star) => {
                state = State::Star;
            }
            (State::Underscore, Token::End) => {
                if !auto_star {
                    return Err(ParsePathError::PathEnd);
                }
            }
            (State::Star, Token::Underscore | Token::Regular(_)) => {
                return Err(ParsePathError::ComponentAfterStar);
            }
            (State::Star, Token::Star) => {
                search_depth += 1;
            }
            (State::Star, Token::End) => {}
        };
    }

    if path.is_relative() {
        return Err(ParsePathError::PathNotAbsolute);
    }

    Ok(SearchPath {
        path,
        depth: min_depth..(min_depth + search_depth),
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Token<'a> {
    Regular(&'a OsStr),
    Underscore,
    Star,
    End,
}

fn tokenize(path: &Path) -> impl Iterator<Item = Token<'_>> {
    path.iter()
        .map(|component| {
            if component == "_" {
                Token::Underscore
            } else if component == "*" {
                Token::Star
            } else if component == r"\_" {
                Token::Regular(OsStr::new("_"))
            } else if component == r"\*" {
                Token::Regular(OsStr::new("*"))
            } else {
                Token::Regular(component)
            }
        })
        .chain(std::iter::once(Token::End))
}

fn find_projects_in(mut paths: Vec<SearchPath>, tx: &SyncSender<Entry>) {
    debug!(?paths, "Searching for projects in these locations");

    let mut walker = None::<(WalkBuilder, usize)>;

    paths.retain_mut(|path| {
        if path.depth == (0..1) {
            if let Some(project) = accept_dir(std::mem::take(&mut path.path), 0) {
                let _ = tx.send(Entry::Project(project));
            };
            return false;
        }
        if let Some((walker, max_depth)) = walker.as_mut() {
            let _ = walker.add(&path.path);
            if path.depth.end > *max_depth {
                *max_depth = path.depth.end;
            }
        } else {
            let w = WalkBuilder::new(&path.path);
            walker = Some((w, path.depth.end));
        };
        true
    });

    if let Some((mut walker, max_depth)) = walker {
        let walker = walker
            .max_depth(Some(max_depth))
            .same_file_system(true)
            .follow_links(false)
            .filter_entry(|e| {
                e.file_type()
                    .is_some_and(|ft| ft.is_dir() || ft.is_symlink())
            })
            .build_parallel();

        let handler = |result| process_dir_entry(&paths, tx, result);
        walker.run(move || Box::new(handler));
    }
}

fn process_dir_entry(
    paths: &[SearchPath],
    tx: &SyncSender<Entry>,
    entry: Result<DirEntry, ignore::Error>,
) -> WalkState {
    match entry {
        Ok(entry) => match process_dir(paths, entry.depth(), entry.into_path()) {
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

fn process_dir(paths: &[SearchPath], depth: usize, path: PathBuf) -> DirResult {
    trace!(path =% path.display(), depth, "Processing directory");

    if depth == 0 {
        return DirResult::Ignored(WalkState::Continue);
    }

    let search_depth = paths
        .iter()
        .find_map(|p| path.starts_with(&p.path).then_some(&p.depth));

    let at_end = if let Some(search_depth) = search_depth {
        if depth < search_depth.start {
            trace!(path =% path.display(), depth, concat!(
                "Continue traversing but not searching the path because ",
                "it's min depth has not been reached yet."
            ));
            return DirResult::Ignored(WalkState::Continue);
        }
        if depth >= search_depth.end {
            debug!(path =% path.display(), depth, concat!(
                "Stopping recursing the path because ",
                "it's max depth has been exceeded (this should not happen)."
            ));
            return DirResult::Ignored(WalkState::Skip);
        }
        depth + 1 == search_depth.end
    } else {
        false
    };

    let git_path = path.join(".git");
    let state = if git_path.exists() {
        debug!(
            path =% path.display(),
            depth,
            "Stopping recursing the path because it is a git repository."
        );
        WalkState::Skip
    } else if at_end {
        trace!(
            path =% path.display(),
            depth,
            "Stopping recursing the path because it's max depth has been reached."
        );
        WalkState::Skip
    } else {
        WalkState::Continue
    };

    let Some(project) = accept_dir(path, depth) else {
        return DirResult::Ignored(WalkState::Continue);
    };

    let project = Entry::Project(project);
    DirResult::Handle(project, state)
}

fn accept_dir(path: PathBuf, depth: usize) -> Option<Project> {
    trace!(path =% path.display(), depth, "accepting directory");

    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.replace('.', "_"));

    let Some(name) = name else {
        warn!(debug_path =? path, display_path =% path.display() , concat!("Could not ",
        "get a path from the ",
        "directory to sugges",
        "t as a project"));
        return None;
    };

    let depth = depth.max(1);

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

    let project = Project {
        root: path,
        name,
        search_path,
        depth,
    };

    Some(project)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_path_regular() {
        test_parse_path("/dir", SearchPath::new("/dir", 0..1));
    }

    #[test]
    fn parse_path_star() {
        test_parse_path("/dir/*", SearchPath::new("/dir", 1..2));
    }

    #[test]
    fn parse_path_underscore() {
        let result = parse_path(Path::new("/dir/_"), false);
        assert_eq!(result.unwrap_err(), ParsePathError::PathEnd);
    }

    #[test]
    fn parse_path_underscore_star() {
        test_parse_path("/dir/_/*", SearchPath::new("/dir", 2..3));
    }

    #[test]
    fn parse_path_underscore_underscore_star() {
        test_parse_path("/dir/_/_/*", SearchPath::new("/dir", 3..4));
    }

    #[test]
    fn parse_path_star_star() {
        test_parse_path("/dir/*/*", SearchPath::new("/dir", 1..3));
    }

    #[test]
    fn parse_path_escaped_star() {
        test_parse_path("/dir/\\*", SearchPath::new("/dir/*", 0..1));
    }

    #[test]
    fn parse_path_escaped_underscore() {
        test_parse_path("/dir/\\_", SearchPath::new("/dir/_", 0..1));
    }

    #[test]
    fn parse_path_tilde_expansion() {
        test_parse_path("~/dir", SearchPath::new("/home/morpheus/dir", 0..1));
    }

    #[test]
    fn parse_path_no_tilde_expansion() {
        test_parse_path("/dir/~", SearchPath::new("/dir/~", 0..1));
    }

    #[test]
    fn parse_path_auto_star_regular() {
        test_parse_path_auto_star("/dir", SearchPath::new("/dir", 1..2));
    }

    #[test]
    fn parse_path_auto_star_star() {
        test_parse_path_auto_star("/dir/*", SearchPath::new("/dir", 1..2));
    }

    #[test]
    fn parse_path_auto_star_underscore() {
        test_parse_path_auto_star("/dir/_", SearchPath::new("/dir", 2..3));
    }

    #[test]
    fn parse_path_auto_star_underscore_underscore() {
        test_parse_path_auto_star("/dir/_/_", SearchPath::new("/dir", 3..4));
    }

    #[test]
    fn parse_path_auto_star_underscore_star() {
        test_parse_path_auto_star("/dir/_/*", SearchPath::new("/dir", 2..3));
    }

    #[test]
    fn parse_path_empty() {
        test_parse_path_error("", ParsePathError::Empty);
    }

    #[test]
    fn parse_path_regular_after_underscore() {
        test_parse_path_error("/dir/_/dir", ParsePathError::ComponentAfterUnderscore);
    }

    #[test]
    fn parse_path_regular_after_star() {
        test_parse_path_error("/dir/*/dir", ParsePathError::ComponentAfterStar);
    }

    #[test]
    fn parse_path_underscore_after_star() {
        test_parse_path_error("/dir/*/_", ParsePathError::ComponentAfterStar);
    }

    #[test]
    fn parse_path_relative_path() {
        test_parse_path_error("dir", ParsePathError::PathNotAbsolute);
        test_parse_path_error(".", ParsePathError::PathNotAbsolute);
    }

    #[allow(clippy::needless_pass_by_value)]
    fn test_parse_path(path: &str, expected: SearchPath) {
        let result = parse_path(Path::new(path), false).unwrap();
        assert_eq!(result, expected);
    }

    #[allow(clippy::needless_pass_by_value)]
    fn test_parse_path_auto_star(path: &str, expected: SearchPath) {
        let result = parse_path(Path::new(path), true).unwrap();
        assert_eq!(result, expected);
    }

    fn test_parse_path_error(path: &str, expected: ParsePathError) {
        let result = parse_path(Path::new(path), false);
        assert_eq!(result.unwrap_err(), expected);
        let result = parse_path(Path::new(path), true);
        assert_eq!(result.unwrap_err(), expected);
    }

    #[test]
    fn ignore_root_entries() {
        let result = process_dir(&[], 0, PathBuf::from("/"));
        assert!(matches!(result, DirResult::Ignored(WalkState::Continue)));
    }

    #[test]
    fn replace_invalid_chars_in_name() {
        let result = process_dir(&[], 1, PathBuf::from("/home/user/dev/project.name"));
        let DirResult::Handle(Entry::Project(project), _) = result else {
            panic!("expected project entry");
        };
        assert_eq!(project.name, "project_name");
    }

    #[test]
    fn search_path_is_the_last_depth_path_elements() {
        let result = process_dir(&[], 3, PathBuf::from("/home/user/dev/project-name/src"));
        let DirResult::Handle(Entry::Project(project), _) = result else {
            panic!("expected project entry");
        };
        assert_eq!(project.search_path, "dev/project-name/src");
    }

    #[test]
    fn skip_path_becuse_of_min_depth() {
        let result = process_dir(
            &[SearchPath::new("/home/user/dev/project-name", 2..3)],
            1,
            PathBuf::from("/home/user/dev/project-name/src"),
        );
        let DirResult::Ignored(WalkState::Continue) = result else {
            panic!("expected ignored+continue");
        };
    }

    #[test]
    fn skip_path_becuse_of_max_depth() {
        let result = process_dir(
            &[SearchPath::new("/home/user/dev/project-name", 1..2)],
            2,
            PathBuf::from("/home/user/dev/project-name/src/dir"),
        );
        let DirResult::Ignored(WalkState::Skip) = result else {
            panic!("expected ignored+skip");
        };
    }
}
