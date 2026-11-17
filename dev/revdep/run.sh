#!/usr/bin/env bash
## Host-side driver for the BiocStyle reverse-dependency vignette sweep.
##
## Usage (from the repository root; `just revdep-sweep ...` calls this):
##   dev/revdep/run.sh sweep  [baseline=REF] [candidate=REF|PATH] [n=N]
##                            [pkgs=A,B] [forced=FILE] [timeout=SECS]
##                            [jobs=1|2] [platform=linux/amd64] [resume=RUN]
##   dev/revdep/run.sh report run=RUN
##
## A git ref is snapshotted with `git archive`; an existing directory is
## copied as-is (uncommitted edits included). Both snapshots are stored in
## the run directory so `resume=` re-uses exactly the same sources.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
results_root="$here/results"
image="biocstyle-revdep:devel"

mode="${1:-}"
[[ -n "$mode" ]] && shift || { echo "usage: run.sh sweep|report [key=value ...]" >&2; exit 2; }

baseline="origin/devel"
candidate="."
n="20"
pkgs=""
forced="$here/forced.txt"
timeout="600"
jobs="1"
platform=""
resume=""
run=""

for arg in "$@"; do
    case "$arg" in
        baseline=*)  baseline="${arg#*=}" ;;
        candidate=*) candidate="${arg#*=}" ;;
        n=*)         n="${arg#*=}" ;;
        pkgs=*)      pkgs="${arg#*=}" ;;
        forced=*)    forced="${arg#*=}" ;;
        timeout=*)   timeout="${arg#*=}" ;;
        jobs=*)      jobs="${arg#*=}" ;;
        platform=*)  platform="${arg#*=}" ;;
        resume=*)    resume="${arg#*=}" ;;
        run=*)       run="${arg#*=}" ;;
        *) echo "unknown argument: $arg" >&2; exit 2 ;;
    esac
done

## Resolve a run name or path to an absolute run directory.
run_dir_of() {
    if [[ -d "$1" ]]; then (cd "$1" && pwd); else echo "$results_root/$1"; fi
}

## Snapshot BiocStyle sources for one variant into $2; echo a description.
snapshot() {
    local spec="$1" dest="$2"
    mkdir -p "$dest"
    if [[ -d "$spec" ]]; then
        local src; src="$(cd "$spec" && pwd)"
        tar -C "$src" --exclude=./.git --exclude=./dev -cf - . | tar -C "$dest" -xf -
        local sha=""
        sha="$(git -C "$src" rev-parse --short HEAD 2>/dev/null || true)"
        local dirty=""
        [[ -n "$(git -C "$src" status --porcelain 2>/dev/null)" ]] && dirty=" + uncommitted changes"
        echo "path $src${sha:+ (HEAD $sha$dirty)}"
    else
        local sha
        sha="$(git -C "$repo" rev-parse --verify --short "$spec^{commit}")" || {
            echo "not a directory or git ref: $spec" >&2; exit 2; }
        git -C "$repo" archive "$sha" | tar -C "$dest" -xf - --exclude=dev
        echo "ref $spec ($sha)"
    fi
}

build_image() {
    local t0=$SECONDS
    docker build -q ${platform:+--platform "$platform"} \
        --build-context scripts="$repo/.github/scripts" \
        -t "$image" "$here" >/dev/null
    echo $((SECONDS - t0))
}

run_container() {
    local rdir="$1"; shift
    local arch
    arch="$(docker run --rm ${platform:+--platform "$platform"} "$image" uname -m)"
    docker run --rm ${platform:+--platform "$platform"} \
        -v "biocstyle-revdep-cache-$arch:/revdep-cache" \
        -v "$here:/revdep:ro" \
        -v "$rdir:/run-dir" \
        -e XDG_CACHE_HOME=/revdep-cache/xdg \
        "$image" "$@"
}

case "$mode" in
    sweep)
        if [[ -n "$resume" ]]; then
            rdir="$(run_dir_of "$resume")"
            [[ -f "$rdir/config.tsv" ]] || { echo "no run to resume at $rdir" >&2; exit 2; }
            echo "Resuming $rdir"
        else
            rdir="$results_root/$(date +%Y%m%d-%H%M%S)"
            mkdir -p "$rdir"
            cp "$forced" "$rdir/forced.txt"
            {
                printf 'key\tvalue\n'
                printf 'baseline\t%s\n' "$(snapshot "$baseline" "$rdir/biocstyle/baseline")"
                printf 'candidate\t%s\n' "$(snapshot "$candidate" "$rdir/biocstyle/candidate")"
                printf 'n\t%s\n' "$n"
                printf 'pkgs\t%s\n' "$pkgs"
                printf 'timeout\t%s\n' "$timeout"
                printf 'jobs\t%s\n' "$jobs"
                printf 'platform\t%s\n' "${platform:-native}"
                printf 'started\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
            } > "$rdir/config.tsv"
            echo "Run directory: $rdir"
        fi
        [[ -f "$rdir/timings.tsv" ]] || printf 'phase\tpackage\tstep\tseconds\n' > "$rdir/timings.tsv"
        secs="$(build_image)"
        printf 'setup\t-\tdocker_build\t%s\n' "$secs" >> "$rdir/timings.tsv"
        run_container "$rdir" Rscript /revdep/sweep.R /run-dir
        echo "Report: $rdir/report.md"
        ;;
    report)
        [[ -n "$run" ]] || { echo "report needs run=<name or path>" >&2; exit 2; }
        rdir="$(run_dir_of "$run")"
        build_image >/dev/null
        run_container "$rdir" Rscript /revdep/compare.R /run-dir
        echo "Report: $rdir/report.md"
        ;;
    *)
        echo "unknown mode: $mode" >&2; exit 2 ;;
esac
