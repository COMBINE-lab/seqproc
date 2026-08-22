//! Compile- and run-time provenance for architecture-tuned seqproc builds.

use antisequence::compiled_simd_backend;
use serde::Serialize;
use thiserror::Error;

/// Reproducible build facts recorded by the CLI and run reports.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BuildProvenance {
    pub seqproc_version: String,
    pub rustc_version: String,
    pub target_triple: String,
    pub target_features: Vec<String>,
    pub compiler_cpu_target: String,
    pub build_profile: String,
    pub cpu_floor: String,
    pub simd_backend: String,
}

/// The current executable cannot safely run on this CPU.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("this seqproc build requires {required}, but the current CPU/OS is missing: {missing}")]
pub struct CpuCompatibilityError {
    pub required: &'static str,
    pub missing: String,
}

/// Return the build facts that affect performance and binary compatibility.
pub fn build_provenance() -> BuildProvenance {
    BuildProvenance {
        seqproc_version: env!("CARGO_PKG_VERSION").to_owned(),
        rustc_version: env!("SEQPROC_RUSTC_VERSION").to_owned(),
        target_triple: env!("SEQPROC_BUILD_TARGET").to_owned(),
        target_features: env!("SEQPROC_TARGET_FEATURES")
            .split(',')
            .filter(|feature| !feature.is_empty())
            .map(str::to_owned)
            .collect(),
        compiler_cpu_target: env!("SEQPROC_COMPILER_CPU_TARGET").to_owned(),
        build_profile: env!("SEQPROC_BUILD_PROFILE").to_owned(),
        cpu_floor: seqproc_cpu_floor().to_owned(),
        simd_backend: compiled_simd_backend().as_str().to_owned(),
    }
}

#[allow(unreachable_code)]
const fn seqproc_cpu_floor() -> &'static str {
    #[cfg(all(target_arch = "x86_64", feature = "release-simd"))]
    {
        return "x86-64-v3 (AVX2)";
    }
    compiled_simd_backend().cpu_requirement()
}

/// Best-effort verification of the documented executable CPU floor before any
/// processing begins.
///
/// The raw CPUID path is deliberate: `is_x86_feature_detected!` becomes a
/// compile-time `true` for globally enabled features, which cannot protect an
/// x86-64-v3 binary copied to an older host.
pub fn ensure_runtime_cpu_compatible() -> Result<(), CpuCompatibilityError> {
    #[cfg(all(target_arch = "x86_64", feature = "release-simd"))]
    {
        let missing = missing_x86_64_v3_features();
        if !missing.is_empty() {
            return Err(CpuCompatibilityError {
                required: seqproc_cpu_floor(),
                missing: missing.join(", "),
            });
        }
    }
    Ok(())
}

#[cfg(all(target_arch = "x86_64", feature = "release-simd"))]
fn missing_x86_64_v3_features() -> Vec<&'static str> {
    use std::arch::x86_64::{__cpuid, __cpuid_count};

    let leaf0 = __cpuid(0);
    let leaf1 = __cpuid(1);
    let ecx = leaf1.ecx;
    let mut missing = Vec::new();
    for (bit, name) in [
        (0, "sse3"),
        (9, "ssse3"),
        (12, "fma"),
        (13, "cmpxchg16b"),
        (19, "sse4.1"),
        (20, "sse4.2"),
        (22, "movbe"),
        (23, "popcnt"),
        (29, "f16c"),
    ] {
        if ecx & (1 << bit) == 0 {
            missing.push(name);
        }
    }

    if leaf0.eax < 7 {
        missing.extend(["bmi1", "avx2", "bmi2"]);
    } else {
        let leaf7 = __cpuid_count(7, 0);
        for (bit, name) in [(3, "bmi1"), (5, "avx2"), (8, "bmi2")] {
            if leaf7.ebx & (1 << bit) == 0 {
                missing.push(name);
            }
        }
    }

    let extended_max = __cpuid(0x8000_0000).eax;
    if extended_max < 0x8000_0001 {
        missing.extend(["lahf/sahf", "lzcnt"]);
    } else {
        let extended = __cpuid(0x8000_0001);
        if extended.ecx & 1 == 0 {
            missing.push("lahf/sahf");
        }
        if extended.ecx & (1 << 5) == 0 {
            missing.push("lzcnt");
        }
    }

    let avx_state_bits = (1 << 26) | (1 << 27) | (1 << 28);
    if ecx & avx_state_bits != avx_state_bits {
        missing.push("OS AVX state");
    } else {
        let xcr0: u64;
        // SAFETY: OSXSAVE is present, so XGETBV is legal. XMM and YMM state
        // must both be enabled by the OS before AVX-family code can execute.
        unsafe {
            let eax: u32;
            let edx: u32;
            std::arch::asm!(
                "xgetbv",
                in("ecx") 0_u32,
                out("eax") eax,
                out("edx") edx,
                options(nomem, nostack, preserves_flags)
            );
            xcr0 = ((edx as u64) << 32) | eax as u64;
        }
        if xcr0 & 0b110 != 0b110 {
            missing.push("OS XMM/YMM context support");
        }
    }
    missing
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn provenance_matches_antisequence_backend() {
        let provenance = build_provenance();
        assert_eq!(provenance.simd_backend, compiled_simd_backend().as_str());
        assert_eq!(provenance.cpu_floor, seqproc_cpu_floor());
        assert!(!provenance.target_triple.is_empty());
        assert!(!provenance.rustc_version.is_empty());
        assert!(!provenance.compiler_cpu_target.is_empty());
    }

    #[test]
    fn current_test_host_satisfies_selected_backend() {
        ensure_runtime_cpu_compatible().unwrap();
    }
}
