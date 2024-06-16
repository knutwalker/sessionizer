use rustc_version::version_meta;
use time::format_description::well_known::Iso8601;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let now = time::OffsetDateTime::now_utc();
    let now = now.format(&Iso8601::DEFAULT)?;
    println!("cargo:rustc-env=SESSIONIZER_BUILD_TIMESTAMP={now}");

    let target = std::env::var("TARGET")?;
    println!("cargo:rustc-env=SESSIONIZER_TARGET_TRIPLE={target}");
    println!("cargo:rerun-if-env-changed=TARGET");

    let profile = std::env::var("PROFILE")?;
    println!("cargo:rustc-env=SESSIONIZER_CARGO_PROFILE={profile}");
    println!("cargo:rerun-if-env-changed=PROFILE");

    let rustc = version_meta()?;

    let channel = match rustc.channel {
        rustc_version::Channel::Dev => "dev",
        rustc_version::Channel::Nightly => "nightly",
        rustc_version::Channel::Beta => "beta",
        rustc_version::Channel::Stable => "stable",
    };
    println!("cargo:rustc-env=SESSIONIZER_RUSTC_CHANNEL={channel}");

    let host = rustc.host;
    println!("cargo:rustc-env=SESSIONIZER_HOST_TRIPLE={host}");

    let semver = rustc.semver;
    println!("cargo:rustc-env=SESSIONIZER_RUSTC_VERSION={semver}");

    git_info()?;

    Ok(())
}

macro_rules! git {
    ($($arg:tt)*) => {{
        let mut cmd = std::process::Command::new("git");
        $(let _ = cmd.arg($arg);)*
        let res = cmd.stdout(std::process::Stdio::piped()).stderr(std::process::Stdio::piped()).output()?;
        res.status.success().then(|| std::string::String::from_utf8(res.stdout).ok()).flatten()
    }};
}

fn git_info() -> Result<(), Box<dyn std::error::Error>> {
    let inside_work_tree = git!("rev-parse" "--is-inside-work-tree");
    let inside_work_tree = inside_work_tree.is_some_and(|o| o.trim() == "true");
    if !inside_work_tree {
        return Ok(());
    }

    let commit_timestamp = git!("log" "-1" "--pretty=format:%ct");
    if let Some(ts) = commit_timestamp {
        let ts = ts.trim().parse::<i64>()?;
        let ts = time::OffsetDateTime::from_unix_timestamp(ts)?;
        let ts = ts.format(&Iso8601::DEFAULT)?;
        println!("cargo:rustc-env=SESSIONIZER_GIT_COMMIT_TIMESTAMP={ts}");
    }
    let commit_hash = git!("rev-parse" "HEAD" "--short" "HEAD");
    if let Some(hash) = commit_hash {
        for (hash, id) in hash
            .lines()
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .zip(["", "_SHORT"])
        {
            println!("cargo:rustc-env=SESSIONIZER_GIT_SHA{id}={hash}");
        }
    }

    if let Some(git_dir) = git!("rev-parse" "--git-common-dir") {
        println!("cargo:rerun-if-changed={}/HEAD", git_dir.trim());
    }

    Ok(())
}
