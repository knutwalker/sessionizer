use vergen::EmitBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    EmitBuilder::builder()
        .git_sha(true)
        .git_commit_timestamp()
        .build_timestamp()
        .cargo_target_triple()
        .rustc_semver()
        .rustc_channel()
        .rustc_host_triple()
        .emit()?;
    let cargo_profile = std::env::var("PROFILE").unwrap();
    println!("cargo:rustc-env=VERGEN_CARGO_PROFILE={}", cargo_profile);
    Ok(())
}
