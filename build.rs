use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=RUSTC");
    println!("cargo:rerun-if-env-changed=TARGET");
    println!("cargo:rerun-if-env-changed=PROFILE");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_FEATURE");
    println!("cargo:rerun-if-env-changed=CARGO_ENCODED_RUSTFLAGS");

    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let rustc_version = Command::new(rustc)
        .arg("--version")
        .output()
        .ok()
        .filter(|output| output.status.success())
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|version| version.trim().to_owned())
        .unwrap_or_else(|| "unknown".to_owned());

    println!(
        "cargo:rustc-env=SEQPROC_BUILD_TARGET={}",
        std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_owned())
    );
    println!(
        "cargo:rustc-env=SEQPROC_BUILD_PROFILE={}",
        std::env::var("PROFILE").unwrap_or_else(|_| "unknown".to_owned())
    );
    println!("cargo:rustc-env=SEQPROC_RUSTC_VERSION={rustc_version}");
    println!(
        "cargo:rustc-env=SEQPROC_TARGET_FEATURES={}",
        std::env::var("CARGO_CFG_TARGET_FEATURE").unwrap_or_default()
    );
    let compiler_cpu_target = std::env::var("CARGO_ENCODED_RUSTFLAGS")
        .unwrap_or_default()
        .split('\u{1f}')
        .find_map(|flag| {
            flag.strip_prefix("target-cpu=")
                .or_else(|| flag.strip_prefix("-Ctarget-cpu="))
        })
        .unwrap_or("default")
        .to_owned();
    println!("cargo:rustc-env=SEQPROC_COMPILER_CPU_TARGET={compiler_cpu_target}");
}
