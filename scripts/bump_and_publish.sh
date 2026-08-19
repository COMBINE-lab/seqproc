#!/usr/bin/env bash
set -euo pipefail

# Bump and optionally publish the seqproc crate. The pushed version tag is the
# interface to cargo-dist: it builds checksummed platform archives and creates
# the GitHub release, while --publish controls the separate crates.io upload.

die() {
    echo "error: $*" >&2
    exit 1
}

usage() {
    cat <<'EOF'
Usage:
  ./scripts/bump_and_publish.sh <version> [--publish] [--dry-run] [--skip-tests]

Validates seqproc, updates Cargo.toml and Cargo.lock when <version> differs from
the current version, commits the bump, and creates and pushes v<version>. The
tag triggers cargo-dist binary builds. With --publish, the crate is also
published to crates.io. Publish the required ANTISEQUENCE version first.

Passing the current version is supported for the first release and creates the
tag without a version-bump commit.

Options:
  --publish     Publish seqproc to crates.io after pushing the commit and tag
  --dry-run     Validate packaging and print release actions without modifying,
                committing, tagging, pushing, publishing, or triggering dist
  --skip-tests  Skip cargo test --locked --all-targets (use only after running it yourself)
  -h, --help    Show this help message
EOF
}

print_cmd() {
    printf '+'
    printf ' %q' "$@"
    printf '\n'
}

run() {
    print_cmd "$@"
    if [[ "$DRY_RUN" == true ]]; then
        return 0
    fi
    "$@"
}

VERSION=""
PUBLISH=false
DRY_RUN=false
SKIP_TESTS=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --publish) PUBLISH=true ;;
        --dry-run) DRY_RUN=true ;;
        --skip-tests) SKIP_TESTS=true ;;
        -h|--help) usage; exit 0 ;;
        -*) die "unknown option: $1" ;;
        *)
            [[ -z "$VERSION" ]] || die "version specified more than once"
            VERSION="$1"
            ;;
    esac
    shift
done

[[ -n "$VERSION" ]] || { usage; exit 1; }
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([+-][0-9A-Za-z.-]+)*$ ]]; then
    die "version must look like X.Y.Z, optionally with prerelease/build suffixes"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPOSITORY_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPOSITORY_ROOT"

MANIFEST="Cargo.toml"
LOCKFILE="Cargo.lock"
TAG="v${VERSION}"
MANIFEST_BACKUP=""
LOCKFILE_BACKUP=""
MANIFEST_UPDATED=false
COMMIT_CREATED=false

cleanup() {
    local status=$?
    if [[ "$status" -ne 0 && "$DRY_RUN" == false && "$MANIFEST_UPDATED" == true && "$COMMIT_CREATED" == false ]]; then
        [[ -n "$MANIFEST_BACKUP" && -f "$MANIFEST_BACKUP" ]] && cp "$MANIFEST_BACKUP" "$MANIFEST"
        [[ -n "$LOCKFILE_BACKUP" && -f "$LOCKFILE_BACKUP" ]] && cp "$LOCKFILE_BACKUP" "$LOCKFILE"
        echo "restored $MANIFEST and $LOCKFILE after failure" >&2
    fi
    [[ -n "$MANIFEST_BACKUP" && -f "$MANIFEST_BACKUP" ]] && rm -f "$MANIFEST_BACKUP"
    [[ -n "$LOCKFILE_BACKUP" && -f "$LOCKFILE_BACKUP" ]] && rm -f "$LOCKFILE_BACKUP"
    return "$status"
}
trap cleanup EXIT

[[ -f "$MANIFEST" ]] || die "not found: $MANIFEST"
[[ -f "$LOCKFILE" ]] || die "not found: $LOCKFILE"
CURRENT_VERSION="$(sed -n '/^\[package\]/,/^\[/{s/^version = "\(.*\)"/\1/p}' "$MANIFEST" | head -1)"
[[ -n "$CURRENT_VERSION" ]] || die "could not determine package version"

if git rev-parse "$TAG" >/dev/null 2>&1; then
    die "tag $TAG already exists"
fi
if [[ -n "$(git status --porcelain)" ]]; then
    die "working tree is not clean; commit or stash existing changes first"
fi
ORIGIN_URL="$(git remote get-url origin 2>/dev/null || true)"
[[ "$ORIGIN_URL" == "https://github.com/COMBINE-lab/seqproc.git" || "$ORIGIN_URL" == "git@github.com:COMBINE-lab/seqproc.git" ]] || \
    die "origin is not the COMBINE-lab/seqproc GitHub repository"

echo "Current version : $CURRENT_VERSION"
echo "Release version : $VERSION"
echo "Tag             : $TAG"
echo "Publish         : $([[ "$PUBLISH" == true ]] && echo yes || echo no)"
echo "Dry-run         : $([[ "$DRY_RUN" == true ]] && echo yes || echo no)"
echo

echo "Preflight: cargo fmt --all --check"
cargo fmt --all --check
if [[ "$SKIP_TESTS" == false ]]; then
    echo "Preflight: cargo test --locked --all-targets"
    cargo test --locked --all-targets
else
    echo "Preflight: skipping tests (--skip-tests)"
fi

if [[ "$CURRENT_VERSION" != "$VERSION" ]]; then
    echo "Updating package version: $CURRENT_VERSION -> $VERSION"
    if [[ "$DRY_RUN" == false ]]; then
        MANIFEST_BACKUP="$(mktemp "${TMPDIR:-/tmp}/seqproc-Cargo.toml.XXXXXX")"
        LOCKFILE_BACKUP="$(mktemp "${TMPDIR:-/tmp}/seqproc-Cargo.lock.XXXXXX")"
        cp "$MANIFEST" "$MANIFEST_BACKUP"
        cp "$LOCKFILE" "$LOCKFILE_BACKUP"
        sed -i.bak "/^\[package\]/,/^\[/{s/^version = \".*\"/version = \"${VERSION}\"/}" "$MANIFEST"
        rm -f "${MANIFEST}.bak"
        MANIFEST_UPDATED=true

        cargo check
        UPDATED_VERSION="$(sed -n '/^\[package\]/,/^\[/{s/^version = "\(.*\)"/\1/p}' "$MANIFEST" | head -1)"
        [[ "$UPDATED_VERSION" == "$VERSION" ]] || die "package version update failed"
    else
        echo "Dry-run: would rewrite [package] version and refresh $LOCKFILE"
    fi
else
    echo "Package is already at $VERSION; no version-bump commit is needed"
fi

echo "Preflight: cargo package --list"
if [[ "$DRY_RUN" == true && "$CURRENT_VERSION" != "$VERSION" ]]; then
    echo "Dry-run validates the current package contents; the version rewrite is only printed"
fi
cargo package --allow-dirty --list >/dev/null

echo "Preflight: cargo publish --dry-run"
PUBLISH_CHECK_LOG="$(mktemp "${TMPDIR:-/tmp}/seqproc-publish-check.XXXXXX")"
if ! cargo publish --dry-run --allow-dirty 2>&1 | tee "$PUBLISH_CHECK_LOG"; then
    if [[ "$PUBLISH" == false ]] && grep -Eq \
        'no matching package named [`'\''"]?antisequence|failed to select a version for the requirement [`'\''"]?antisequence' \
        "$PUBLISH_CHECK_LOG"; then
        echo "warning: seqproc's source package is valid, but ANTISEQUENCE 0.1.0 is not yet on crates.io" >&2
        echo "warning: publish ANTISEQUENCE first; a --publish release treats this as fatal" >&2
    else
        rm -f "$PUBLISH_CHECK_LOG"
        die "cargo publish --dry-run failed"
    fi
fi
rm -f "$PUBLISH_CHECK_LOG"

echo "Preflight: cargo dist plan"
cargo dist plan --allow-dirty >/dev/null

if [[ "$DRY_RUN" == true ]]; then
    echo
    echo "Dry-run complete; no tracked files, commits, tags, pushes, packages, or releases were changed"
    exit 0
fi

if [[ "$MANIFEST_UPDATED" == true ]]; then
    run git add "$MANIFEST" "$LOCKFILE"
    run git commit -m "chore(release): bump seqproc to v${VERSION}"
    COMMIT_CREATED=true
fi

run git tag -a "$TAG" -m "Release ${VERSION}"
run git push origin HEAD
run git push origin "$TAG"

if [[ "$PUBLISH" == true ]]; then
    run cargo publish
else
    echo "Skipping crates.io publication; pass --publish to publish v${VERSION}"
fi

echo
echo "seqproc release preparation complete for v${VERSION}"
echo "The pushed tag triggers .github/workflows/release.yml, which builds"
echo "checksummed platform archives and creates the GitHub release."
