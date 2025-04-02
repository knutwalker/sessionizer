use std::{
    env,
    fs::{self, Metadata},
    io,
    os::unix::fs::MetadataExt as _,
    path::{Path, PathBuf},
};

use color_eyre::{Section as _, SectionExt as _, eyre::Context as _};
use onlyerror::Error;

use crate::{
    Result,
    config::{Axis, Config, ConfigError, EnvValue},
    eyre, trace,
};

#[derive(Debug, Clone, Default)]
pub struct Init {
    pub env: Vec<(String, EnvValue)>,
    pub run: Vec<String>,
    pub windows: Vec<SpawnWindow>,
    pub layout: Option<Box<Layout>>,
}

#[derive(Debug, Clone)]
pub struct SpawnWindow {
    pub name: String,
    pub dir: Option<PathBuf>,
    pub command: Option<WindowCommand>,
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: Option<String>,
    pub layout: SubLayout,
}

#[derive(Debug, Clone)]
pub enum SubLayout {
    Split(SplitLayout),
    Pane(PaneLayout),
}

#[derive(Debug, Clone)]
pub struct SplitLayout {
    pub axis: Axis,
    pub panes: Vec<Layout>,
}

#[derive(Debug, Clone)]
pub struct PaneLayout {
    pub dir: PathBuf,
    pub command: Option<WindowCommand>,
}

#[derive(Debug, Clone)]
pub enum WindowCommand {
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
    pub fn first(&self) -> &str {
        match self {
            Self::Command { command, .. } => command[0].as_str(),
            Self::Run { name, .. } => name.as_str(),
        }
    }
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Error)]
pub enum InitError {
    #[error("The current working directory does not exist or cannot be accessed")]
    InvalidCwd(#[source] io::Error),
    #[error("No sessionizer config file found")]
    NoConfigFile,
    #[error("File is not readable")]
    FileNotReadable,
    #[error("File can be written or executed by others")]
    PermissionsTooOpen,
    #[error("File is not readable or executable")]
    FileNotReadableOrExecutable,
    #[error("{0}")]
    Config(#[from] ConfigError),
}

pub fn create_config_file() -> Result<(), InitError> {
    const INIT_FILE: &str = concat!(".", env!("CARGO_PKG_NAME"), ".toml");

    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;
    Config::write_empty_config_in(&cwd.join(INIT_FILE))?;

    Ok(())
}

pub fn edit_config_file(secure: bool) -> Result<()> {
    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;

    let init = loop {
        match InitFile::require_in(&cwd) {
            Ok(init) => break init,
            Err(e) => match e.downcast_ref::<InitError>() {
                Some(InitError::NoConfigFile) => {
                    create_config_file()?;
                    continue;
                }
                _ => return Err(e),
            },
        };
    };
    Config::edit_config_file_in(&init.path, |path| {
        let tmp_file = InitFile {
            path: path.to_path_buf(),
            file: init.file,
            kind: init.kind,
            metadata: init.metadata.clone(),
        };

        let _init = tmp_file.validate(&cwd, false)?;
        Ok(())
    })?;
    let _init = init.validate(&cwd, secure)?;
    Ok(())
}

pub fn validate_config_file(secure: bool) -> Result<()> {
    let cwd = env::current_dir().map_err(InitError::InvalidCwd)?;
    let init = InitFile::require_in(&cwd)?;
    let _init = init.validate(&cwd, secure)?;
    Ok(())
}

pub fn find_action(dir: &Path, secure: bool) -> Option<Result<Init>> {
    let init = InitFile::find_in(dir)?;
    let init = init.validate(dir, secure);
    Some(init)
}

#[derive(Debug, Clone)]
struct InitFile {
    file: &'static str,
    path: PathBuf,
    kind: Kind,
    metadata: Metadata,
}

impl InitFile {
    fn find_in(dir: &Path) -> Option<Self> {
        const CONFIG_FILES: [&str; 4] = [
            concat!(".", env!("CARGO_PKG_NAME"), ".toml"),
            concat!(env!("CARGO_PKG_NAME"), ".toml"),
            concat!(".", env!("CARGO_PKG_NAME"), ".init"),
            concat!(env!("CARGO_PKG_NAME"), ".init"),
        ];

        CONFIG_FILES.into_iter().find_map(|file| {
            let path = dir.join(file);
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
            Some(Self {
                file,
                path,
                kind,
                metadata,
            })
        })
    }

    fn require_in(dir: &Path) -> Result<Self> {
        Self::find_in(dir)
            .ok_or(InitError::NoConfigFile)
            .suggestion(concat!(
                "Run `",
                env!("CARGO_PKG_NAME"),
                " --config init` to create an empty config"
            ))
    }

    fn validate(&self, dir: &Path, secure: bool) -> Result<Init> {
        let config = self.load(secure).context(concat!(
            "Failed to load ",
            env!("CARGO_PKG_NAME"),
            "init file"
        ))?;
        let init = config.validate(dir)?;
        Ok(init)
    }

    fn load(&self, secure: bool) -> Result<Config> {
        if secure {
            let err = self.kind.check_permissions(&self.metadata);

            if let Err(err) = err {
                let err =
                    eyre!(err).with_section(|| format!("{}", self.path.display()).header("File"));

                let err = match self.kind {
                    Kind::Toml => err.suggestion("Set the file permissions to 600"),
                    Kind::Init => err.suggestion("Set the file permissions to 700"),
                };

                return Err(err.note("Running with `--insecure` will disable this check"));
            };
        }

        match self.kind {
            Kind::Toml => Config::load_from_toml(&self.path).map_err(|e| {
                eyre!(e).with_section(|| format!("{}", self.path.display()).header("File"))
            }),
            Kind::Init => Ok(Config::new_init_file(self.file)),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Kind {
    Toml,
    Init,
}

impl Kind {
    fn check_permissions(self, md: &Metadata) -> Result<(), InitError> {
        match self {
            Self::Toml => {
                let permissions = md.mode();
                if permissions & 0o400 != 0o400 {
                    return Err(InitError::FileNotReadable);
                }
                if permissions & 0o033 != 0 {
                    return Err(InitError::PermissionsTooOpen);
                }
            }
            Self::Init => {
                let permissions = md.mode();
                if permissions & 0o600 != 0o600 {
                    return Err(InitError::FileNotReadableOrExecutable);
                }
                if permissions & 0o033 != 0 {
                    return Err(InitError::PermissionsTooOpen);
                }
            }
        }

        Ok(())
    }
}
