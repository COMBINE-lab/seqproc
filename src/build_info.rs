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
    pub required: String,
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
        cpu_floor: seqproc_cpu_floor(),
        simd_backend: compiled_simd_backend().as_str().to_owned(),
    }
}

#[allow(unreachable_code)]
fn seqproc_cpu_floor() -> String {
    #[cfg(target_arch = "x86_64")]
    {
        let cpu_target = env!("SEQPROC_COMPILER_CPU_TARGET");
        if cpu_target == "native" {
            return "build-host native ISA (non-portable; exact target features recorded)"
                .to_owned();
        }
        let has_target_feature = |expected: &str| {
            env!("SEQPROC_TARGET_FEATURES")
                .split(',')
                .any(|feature| feature == expected)
        };
        if ["avx512f", "avx512bw", "avx512cd", "avx512dq", "avx512vl"]
            .into_iter()
            .all(has_target_feature)
        {
            return "x86-64-v4 or stronger (exact target features recorded)".to_owned();
        }
        #[cfg(feature = "release-simd")]
        return "x86-64-v3 (AVX2)".to_owned();
    }
    compiled_simd_backend().cpu_requirement().to_owned()
}

/// Verify the compiler-enabled x86 feature set before processing begins.
///
/// The raw CPUID path is deliberate: `is_x86_feature_detected!` becomes a
/// compile-time `true` for globally enabled features, which cannot protect an
/// x86-64-v3 binary copied to an older host. Official Linux artifacts also
/// carry a GNU ISA property so the loader enforces the floor before this Rust
/// code can execute; this check remains important for host-native builds and
/// platforms without an equivalent loader contract.
pub fn ensure_runtime_cpu_compatible() -> Result<(), CpuCompatibilityError> {
    #[cfg(target_arch = "x86_64")]
    {
        let missing = missing_required_x86_features();
        if !missing.is_empty() {
            return Err(CpuCompatibilityError {
                required: seqproc_cpu_floor(),
                missing: missing.join(", "),
            });
        }
    }
    Ok(())
}

#[cfg(target_arch = "x86_64")]
#[inline(never)]
fn missing_required_x86_features() -> Vec<&'static str> {
    use std::arch::x86_64::{__cpuid, __cpuid_count};

    let target_features = env!("SEQPROC_TARGET_FEATURES");
    let x86_64_v3_feature = |feature: &str| {
        matches!(
            feature,
            "avx"
                | "avx2"
                | "bmi1"
                | "bmi2"
                | "cmpxchg16b"
                | "f16c"
                | "fma"
                | "fxsr"
                | "lahfsahf"
                | "lzcnt"
                | "movbe"
                | "popcnt"
                | "sse"
                | "sse2"
                | "sse3"
                | "sse4.1"
                | "sse4.2"
                | "ssse3"
                | "xsave"
        )
    };
    let required = |feature: &str| {
        target_features.split(',').any(|item| item == feature)
            || (x86_64_v3_feature(feature) && cfg!(feature = "release-simd"))
    };
    let leaf0 = __cpuid(0);
    let leaf1 = __cpuid(1);
    let ecx = leaf1.ecx;
    let edx = leaf1.edx;
    let mut missing = Vec::new();
    for (bit, feature, name) in [
        (0, "sse3", "sse3"),
        (1, "pclmulqdq", "pclmulqdq"),
        (9, "ssse3", "ssse3"),
        (12, "fma", "fma"),
        (13, "cmpxchg16b", "cmpxchg16b"),
        (19, "sse4.1", "sse4.1"),
        (20, "sse4.2", "sse4.2"),
        (22, "movbe", "movbe"),
        (23, "popcnt", "popcnt"),
        (25, "aes", "aes"),
        (26, "xsave", "xsave"),
        (28, "avx", "avx"),
        (29, "f16c", "f16c"),
        (30, "rdrand", "rdrand"),
    ] {
        if required(feature) && ecx & (1 << bit) == 0 {
            missing.push(name);
        }
    }
    for (bit, feature, name) in [
        (24, "fxsr", "fxsr"),
        (25, "sse", "sse"),
        (26, "sse2", "sse2"),
    ] {
        if required(feature) && edx & (1 << bit) == 0 {
            missing.push(name);
        }
    }

    if leaf0.eax < 7 {
        for feature in [
            "bmi1",
            "avx2",
            "bmi2",
            "avx512f",
            "avx512dq",
            "rdseed",
            "adx",
            "avx512ifma",
            "avx512cd",
            "sha",
            "avx512bw",
            "avx512vl",
            "avx512vbmi",
            "avx512vbmi2",
            "gfni",
            "vaes",
            "vpclmulqdq",
            "avx512vnni",
            "avx512bitalg",
            "avx512vpopcntdq",
            "avx512vp2intersect",
            "avxvnni",
            "avx512bf16",
        ] {
            if required(feature) {
                missing.push(feature);
            }
        }
    } else {
        let leaf7 = __cpuid_count(7, 0);
        for (bit, feature, name) in [
            (3, "bmi1", "bmi1"),
            (5, "avx2", "avx2"),
            (8, "bmi2", "bmi2"),
            (16, "avx512f", "avx512f"),
            (17, "avx512dq", "avx512dq"),
            (18, "rdseed", "rdseed"),
            (19, "adx", "adx"),
            (21, "avx512ifma", "avx512ifma"),
            (28, "avx512cd", "avx512cd"),
            (29, "sha", "sha"),
            (30, "avx512bw", "avx512bw"),
            (31, "avx512vl", "avx512vl"),
        ] {
            if required(feature) && leaf7.ebx & (1 << bit) == 0 {
                missing.push(name);
            }
        }
        for (bit, feature, name) in [
            (1, "avx512vbmi", "avx512vbmi"),
            (6, "avx512vbmi2", "avx512vbmi2"),
            (8, "gfni", "gfni"),
            (9, "vaes", "vaes"),
            (10, "vpclmulqdq", "vpclmulqdq"),
            (11, "avx512vnni", "avx512vnni"),
            (12, "avx512bitalg", "avx512bitalg"),
            (14, "avx512vpopcntdq", "avx512vpopcntdq"),
        ] {
            if required(feature) && leaf7.ecx & (1 << bit) == 0 {
                missing.push(name);
            }
        }
        if required("avx512vp2intersect") && leaf7.edx & (1 << 8) == 0 {
            missing.push("avx512vp2intersect");
        }
        if leaf7.eax < 1 {
            for feature in ["avxvnni", "avx512bf16"] {
                if required(feature) {
                    missing.push(feature);
                }
            }
        } else {
            let leaf7_1 = __cpuid_count(7, 1);
            for (bit, feature, name) in [(4, "avxvnni", "avxvnni"), (5, "avx512bf16", "avx512bf16")]
            {
                if required(feature) && leaf7_1.eax & (1 << bit) == 0 {
                    missing.push(name);
                }
            }
        }
    }

    let extended_max = __cpuid(0x8000_0000).eax;
    if extended_max < 0x8000_0001 {
        for feature in ["lahfsahf", "lzcnt", "sse4a", "prfchw"] {
            if required(feature) {
                missing.push(feature);
            }
        }
    } else {
        let extended = __cpuid(0x8000_0001);
        for (bit, feature, name) in [
            (0, "lahfsahf", "lahf/sahf"),
            (5, "lzcnt", "lzcnt"),
            (6, "sse4a", "sse4a"),
            (8, "prfchw", "prfchw"),
        ] {
            if required(feature) && extended.ecx & (1 << bit) == 0 {
                missing.push(name);
            }
        }
    }

    let needs_avx_state = target_features.split(',').any(|feature| {
        feature == "avx"
            || feature == "avx2"
            || feature == "fma"
            || feature == "f16c"
            || feature.starts_with("avx512")
            || feature == "avxvnni"
    }) || cfg!(feature = "release-simd");
    let needs_avx512_state = target_features
        .split(',')
        .any(|feature| feature.starts_with("avx512"));
    let avx_state_bits = (1 << 26) | (1 << 27);
    if needs_avx_state && ecx & avx_state_bits != avx_state_bits {
        missing.push("OS XSAVE support");
    } else if needs_avx_state {
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
        if needs_avx512_state && xcr0 & 0b1110_0110 != 0b1110_0110 {
            missing.push("OS AVX-512 context support");
        }
    }

    if ["xsaveopt", "xsavec", "xsaves"].into_iter().any(required) {
        if leaf0.eax < 0xD {
            for feature in ["xsaveopt", "xsavec", "xsaves"] {
                if required(feature) {
                    missing.push(feature);
                }
            }
        } else {
            let leaf_d_1 = __cpuid_count(0xD, 1);
            for (bit, feature, name) in [
                (0, "xsaveopt", "xsaveopt"),
                (1, "xsavec", "xsavec"),
                (3, "xsaves", "xsaves"),
            ] {
                if required(feature) && leaf_d_1.eax & (1 << bit) == 0 {
                    missing.push(name);
                }
            }
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
