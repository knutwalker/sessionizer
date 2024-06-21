use rustc_version::version_meta;
use time::format_description::well_known::Iso8601;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let now = time::OffsetDateTime::now_utc();
    let now = now.format(&Iso8601::DEFAULT).unwrap_or_default();
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

    git_info();

    Ok(())
}

macro_rules! git {
    ($($arg:tt)*) => {{
        let mut cmd = std::process::Command::new("git");
        $(let _ = cmd.arg($arg);)*
        let res = cmd.stdout(std::process::Stdio::piped()).stderr(std::process::Stdio::piped()).output().ok();
        res.and_then(|res| res.status.success().then(|| std::string::String::from_utf8(res.stdout).ok()).flatten())
    }};
}

fn git_info() {
    let inside_work_tree = git!("rev-parse" "--is-inside-work-tree");
    let inside_work_tree = inside_work_tree.is_some_and(|o| o.trim() == "true");
    if !inside_work_tree {
        println!("cargo:rustc-env=SESSIONIZER_GIT_COMMIT_TIMESTAMP=");
        println!("cargo:rustc-env=SESSIONIZER_GIT_SHA=");
        println!("cargo:rustc-env=SESSIONIZER_GIT_SHA_SHORT=");
        return;
    }

    git_commit_timestamp();
    git_commit_hash();
    git_dir();
}

fn git_commit_timestamp() {
    let ts = (|| {
        let ts = git!("log" "-1" "--pretty=format:%ct")?;
        let ts = ts.trim().parse::<i64>().ok()?;
        let ts = time::OffsetDateTime::from_unix_timestamp(ts).ok()?;
        let ts = ts.format(&Iso8601::DEFAULT).ok()?;
        Some(ts)
    })();
    let ts = ts.unwrap_or_default();

    println!("cargo:rustc-env=SESSIONIZER_GIT_COMMIT_TIMESTAMP={ts}");
}

fn git_commit_hash() {
    let commit_hashes = git!("rev-parse" "HEAD" "--short" "HEAD").unwrap_or_default();
    for (hash, id) in commit_hashes
        .lines()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .chain(std::iter::repeat(""))
        .take(2)
        .zip(["", "_SHORT"])
    {
        println!("cargo:rustc-env=SESSIONIZER_GIT_SHA{id}={hash}");
    }
}

fn git_dir() {
    if let Some(git_dir) = git!("rev-parse" "--git-common-dir") {
        println!("cargo:rerun-if-changed={}/HEAD", git_dir.trim());
    }
}
