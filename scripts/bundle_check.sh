#!/usr/bin/env bash
# Rehearse a release bundle locally, without touching the running app or a real install.
#
# It builds the tarball the way `.github/workflows/release.yml` does — by reading the file list OUT
# of that workflow, so this cannot drift from what a real release ships — extracts it into a
# throwaway directory, and checks that everything in scripts/bundle_required_paths.txt actually
# arrived. With --launch it then starts the API server FROM the extracted tree on a spare port with
# an isolated config dir, and waits for /api/health.
#
# That last step is the one that matters: #540 shipped a bundle whose file list looked fine and whose
# server died at load because `pluto/` was not in it. A structural check alone would not have caught
# a wrong path INSIDE an included directory; launching does.
#
# The package test (`release bundle ships every runtime path`) checks the same list against the
# workflow on every CI run. This script checks the bundle that list produces — the list can be right
# while the tar goes wrong, so both exist.
#
#   scripts/bundle_check.sh                 # build + extract + structural check (fast)
#   scripts/bundle_check.sh --launch        # …and boot the server from the bundle, hit /api/health
#   scripts/bundle_check.sh --build         # rebuild frontend/dist first (npm ci && npm run build)
#   scripts/bundle_check.sh --keep          # leave the work dir behind and print its path
#
# Ports: the launch uses CECELIA_PORT (default 8099) — never 8080/5173/7655/7656/7660, so a running
# dev server is untouched. It kills only the process it started.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKFLOW="$ROOT/.github/workflows/release.yml"
REQUIRED="$ROOT/scripts/bundle_required_paths.txt"
PORT="${CECELIA_PORT:-8099}"
LAUNCH=0; BUILD=0; KEEP=0

for arg in "$@"; do
  case "$arg" in
    --launch) LAUNCH=1 ;;
    --build)  BUILD=1 ;;
    --keep)   KEEP=1 ;;
    -h|--help) sed -n '2,26p' "${BASH_SOURCE[0]}"; exit 0 ;;
    *) echo "unknown option: $arg (try --help)" >&2; exit 2 ;;
  esac
done

say()  { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()   { printf '   \033[32mok\033[0m   %s\n' "$*"; }
bad()  { printf '   \033[31mMISS\033[0m %s\n' "$*"; }
note() { printf '   %s\n' "$*"; }

WORK="$(mktemp -d -t cecelia-bundle-XXXXXX)"
STUBBED_DIST=0
MADE_VERSION=0
cleanup() {
  [ "$STUBBED_DIST" = 1 ] && rm -rf "$ROOT/frontend/dist"
  [ "$MADE_VERSION" = 1 ] && rm -f "$ROOT/VERSION"
  if [ "$KEEP" = 1 ]; then echo "work dir kept: $WORK"; else rm -rf "$WORK"; fi
}
trap cleanup EXIT

# ── 1. the file list, read from the workflow ─────────────────────────────────────────────────────
say "reading the tar file list from $(basename "$WORKFLOW")"
FILES="$(awk '
  /tar -czf out\/cecelia\.tar\.gz/ { grabbing = 1 }
  grabbing {
    line = $0
    cont = (line ~ /\\[[:space:]]*$/)
    sub(/\\[[:space:]]*$/, "", line)
    sub(/.*tar -czf[[:space:]]+out\/cecelia\.tar\.gz/, "", line)
    printf "%s ", line
    if (!cont) exit
  }' "$WORKFLOW")"
# shellcheck disable=SC2086
set -- $FILES              # word-split into $@ — the list is plain paths, no spaces
[ "$#" -gt 0 ] || { echo "could not parse the tar file list from $WORKFLOW" >&2; exit 1; }
note "$# entries: $*"

# ── 2. the two files the workflow generates rather than commits ──────────────────────────────────
if [ ! -f "$ROOT/VERSION" ]; then
  (cd "$ROOT" && git describe --tags --always 2>/dev/null || echo "0.0.0-local") > "$ROOT/VERSION"
  MADE_VERSION=1
  note "wrote a temporary VERSION ($(cat "$ROOT/VERSION")) — the workflow writes the tag here"
fi

if [ "$BUILD" = 1 ]; then
  say "building the frontend (npm ci && npm run build)"
  (cd "$ROOT/frontend" && npm ci && npm run build)
elif [ ! -d "$ROOT/frontend/dist" ]; then
  # A real bundle ships a built dist; for a structural/launch rehearsal it only has to exist, and a
  # 2-minute build per run would stop anyone from running this. Said loudly so nobody reads a pass
  # here as "the frontend is fine".
  mkdir -p "$ROOT/frontend/dist"
  printf '<!doctype html><title>stub</title>\n' > "$ROOT/frontend/dist/index.html"
  STUBBED_DIST=1
  note "frontend/dist was missing — using a STUB (pass --build for the real thing)"
else
  note "using the existing frontend/dist"
fi

# ── 3. build + extract ───────────────────────────────────────────────────────────────────────────
say "packing the bundle"
(cd "$ROOT" && tar -czf "$WORK/cecelia.tar.gz" "$@")
note "$(du -h "$WORK/cecelia.tar.gz" | cut -f1) → $WORK/cecelia.tar.gz"

INSTALL="$WORK/install"
mkdir -p "$INSTALL"
tar -xzf "$WORK/cecelia.tar.gz" -C "$INSTALL"

# ── 4. is everything the running app loads actually in there? ────────────────────────────────────
say "checking $(basename "$REQUIRED") against the extracted bundle"
missing=0
while IFS= read -r line; do
  path="${line%%#*}"; path="$(echo "$path" | tr -d '[:space:]')"
  [ -z "$path" ] && continue
  if [ -e "$INSTALL/$path" ]; then ok "$path"; else bad "$path"; missing=$((missing + 1)); fi
done < "$REQUIRED"

if [ "$missing" -gt 0 ]; then
  echo
  echo "$missing required path(s) missing from the bundle — add them to the tar list in $WORKFLOW" >&2
  exit 1
fi

if [ "$LAUNCH" = 0 ]; then
  say "bundle looks complete"
  note "run again with --launch to boot the server from it (that is what catches a load-time include)"
  exit 0
fi

# ── 5. boot the server FROM the bundle ───────────────────────────────────────────────────────────
say "launching the API server from the bundle on :$PORT"
if command -v ss >/dev/null 2>&1 && ss -ltn "sport = :$PORT" 2>/dev/null | grep -q LISTEN; then
  echo "port $PORT is already in use — set CECELIA_PORT to a free one" >&2; exit 1
fi

# Isolated config dir: the server must never read or write the real ~/.cecelia (or a dev CECELIA_DEV_DIR)
# during a rehearsal. A fresh dir also exercises the first-launch path.
CFG="$WORK/config"; mkdir -p "$CFG"

note "instantiating api/ (shared depot, so this downloads nothing; first run precompiles)"
(cd "$INSTALL" && julia --project=api -e 'using Pkg; Pkg.instantiate()' >"$WORK/instantiate.log" 2>&1) \
  || { echo "Pkg.instantiate failed — see $WORK/instantiate.log" >&2; KEEP=1; exit 1; }

# `exec` matters: without it `$!` is the SUBSHELL's pid, and killing that leaves julia holding the
# port — a rehearsal then poisons the next one ("port 8099 is already in use") and leaves a server
# running against a deleted temp dir. With exec, the subshell IS julia.
( cd "$INSTALL/api" && exec env CECELIA_PORT="$PORT" CECELIA_DEV_DIR="$CFG" \
    julia --project src/server.jl >"$WORK/server.log" 2>&1 ) &
SERVER_PID=$!
kill_server() {
  kill "$SERVER_PID" 2>/dev/null || true
  wait "$SERVER_PID" 2>/dev/null || true
  # Belt and braces: if anything is still holding the port, it came from this script.
  if command -v ss >/dev/null 2>&1 && ss -ltn "sport = :$PORT" 2>/dev/null | grep -q LISTEN; then
    echo "warning: something is still listening on :$PORT (pid $SERVER_PID did not free it)" >&2
  fi
}
trap 'kill_server; cleanup' EXIT

note "waiting for http://127.0.0.1:$PORT/api/health (up to 300s — first launch precompiles Cecelia)"
health=""
for _ in $(seq 1 300); do
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then break; fi          # server died — stop waiting
  health="$(curl -fsS "http://127.0.0.1:$PORT/api/health" 2>/dev/null || true)"
  [ -n "$health" ] && break
  sleep 1
done

if [ -n "$health" ]; then
  ok "/api/health → $health"
  say "the bundle boots"
  kill_server
  exit 0
fi

echo
echo "the server never became healthy — the tail of its log:" >&2
tail -n 30 "$WORK/server.log" >&2
KEEP=1
exit 1
