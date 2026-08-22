use std::process::Command;

fn encoded_codegen_options() -> Vec<String> {
    let flags = std::env::var("CARGO_ENCODED_RUSTFLAGS").unwrap_or_default();
    let mut flags = flags.split('\u{1f}');
    let mut options = Vec::new();
    while let Some(flag) = flags.next() {
        if flag == "-C" {
            if let Some(option) = flags.next() {
                options.push(option.to_owned());
            }
        } else if let Some(option) = flag.strip_prefix("-C") {
            if !option.is_empty() {
                options.push(option.to_owned());
            }
        }
    }
    options
}

fn configured_cpu_target(target: &str) -> String {
    let codegen_options = encoded_codegen_options();
    if let Some(cpu) = codegen_options
        .iter()
        .find_map(|option| option.strip_prefix("target-cpu="))
    {
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
    let mut features: Vec<String> = cfg
        .lines()
        .filter_map(|line| {
            line.strip_prefix("target_feature=\"")
                .and_then(|value| value.strip_suffix('"'))
        })
        .map(str::to_owned)
        .collect();
    // `rustc --print cfg -C target-cpu=...` does not see additional
    // `-C target-feature=+X,-Y` flags Cargo supplied to the real build. Apply
    // those overrides so provenance and the runtime guard describe the exact
    // compiler contract rather than only the CPU preset.
    for option in encoded_codegen_options() {
        let Some(settings) = option.strip_prefix("target-feature=") else {
            continue;
        };
        for setting in settings.split(',').filter(|setting| !setting.is_empty()) {
            if let Some(feature) = setting.strip_prefix('+') {
                if !features.iter().any(|existing| existing == feature) {
                    features.push(feature.to_owned());
                }
            } else if let Some(feature) = setting.strip_prefix('-') {
                features.retain(|existing| existing != feature);
            }
        }
    }
    features.sort_unstable();
    features.dedup();
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
