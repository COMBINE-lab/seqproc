use std::process::Command;

fn configured_cpu_target(target: &str) -> String {
    let encoded_flags = std::env::var("CARGO_ENCODED_RUSTFLAGS").unwrap_or_default();
    if let Some(cpu) = encoded_flags.split('\u{1f}').find_map(|flag| {
        flag.strip_prefix("target-cpu=")
            .or_else(|| flag.strip_prefix("-Ctarget-cpu="))
    }) {
        return cpu.to_owned();
    }

    // Cargo does not expose target-specific rustflags to build scripts. The
    // release marker is set only by config-release.toml, so keep this mapping
    // beside that file's deliberately fixed artifact targets.
    if std::env::var_os("SEQPROC_RELEASE_BUILD").is_some() {
        return match target {
            "x86_64-unknown-linux-gnu" | "x86_64-apple-darwin" => "x86-64-v3",
            "aarch64-unknown-linux-gnu" => "neoverse-n1",
            "aarch64-apple-darwin" => "apple-a14",
            _ => "default",
        }
        .to_owned();
    }

    "default".to_owned()
}

fn rustc_target_features(rustc: &std::ffi::OsStr, target: &str, cpu: &str) -> Option<String> {
    let mut command = Command::new(rustc);
    command.args(["--print", "cfg", "--target", target]);
    if cpu != "default" {
        command.args(["-C", &format!("target-cpu={cpu}")]);
    }
    let output = command
        .output()
        .ok()
        .filter(|output| output.status.success())?;
    let cfg = String::from_utf8(output.stdout).ok()?;
    let mut features: Vec<_> = cfg
        .lines()
        .filter_map(|line| {
            line.strip_prefix("target_feature=\"")
                .and_then(|value| value.strip_suffix('"'))
        })
        .collect();
    features.sort_unstable();
    Some(features.join(","))
}

fn main() {
    println!("cargo:rerun-if-env-changed=RUSTC");
    println!("cargo:rerun-if-env-changed=TARGET");
    println!("cargo:rerun-if-env-changed=PROFILE");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_FEATURE");
    println!("cargo:rerun-if-env-changed=CARGO_ENCODED_RUSTFLAGS");
    println!("cargo:rerun-if-env-changed=SEQPROC_RELEASE_BUILD");

    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let rustc_version = Command::new(&rustc)
        .arg("--version")
        .output()
        .ok()
        .filter(|output| output.status.success())
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|version| version.trim().to_owned())
        .unwrap_or_else(|| "unknown".to_owned());

    let target = std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_owned());
    println!("cargo:rustc-env=SEQPROC_BUILD_TARGET={target}");
    println!(
        "cargo:rustc-env=SEQPROC_BUILD_PROFILE={}",
        std::env::var("PROFILE").unwrap_or_else(|_| "unknown".to_owned())
    );
    println!("cargo:rustc-env=SEQPROC_RUSTC_VERSION={rustc_version}");
    let compiler_cpu_target = configured_cpu_target(&target);
    let target_features = rustc_target_features(&rustc, &target, &compiler_cpu_target)
        .unwrap_or_else(|| std::env::var("CARGO_CFG_TARGET_FEATURE").unwrap_or_default());
    println!("cargo:rustc-env=SEQPROC_TARGET_FEATURES={target_features}");
    println!("cargo:rustc-env=SEQPROC_COMPILER_CPU_TARGET={compiler_cpu_target}");
}
