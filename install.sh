#!/usr/bin/env sh
# Cecelia installer — Linux & macOS.
#
# Installs Pixi and Juliaup if missing, downloads Cecelia, provisions the environment, and adds a
# desktop launcher. Re-runnable — it replaces the install in place.
#
# Two channels (CECELIA_CHANNEL):
#   stable (default) — the latest tagged GitHub Release; ships a prebuilt frontend (no Node needed).
#   dev              — the current GitHub state (the `main` branch tarball); the frontend is built
#                      locally, so Node.js (npm) must be on PATH. Tracks HEAD without waiting for a
#                      tagged release — just re-run to update. See docs/SHIPPING.md.
#
#   curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | sh
#   curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | CECELIA_CHANNEL=dev sh
#
# Two install scopes (CECELIA_INSTALL_SCOPE):
#   user (default) — installs into your account only (~/.local/share/cecelia); no root needed.
#   system         — one shared install for all users (/opt/cecelia; /Applications/cecelia on macOS);
#                    needs root. Pixi, Juliaup and the multi-GB env are provisioned INSIDE the install
#                    dir so every account shares one runtime — set via a launcher wrapper that exports
#                    PIXI_HOME + JULIAUP_DEPOT_PATH. Per-user config + projects still live in
#                    ~/.cecelia (never shared). Updates are then admin-only (re-run this as root).
#
#   curl -LsSf .../install.sh | CECELIA_INSTALL_SCOPE=system sudo -E sh
#
# Env overrides:  CECELIA_CHANNEL=stable|dev  CECELIA_VERSION=v0.1.0  CECELIA_BRANCH=main
#                 CECELIA_INSTALL_SCOPE=user|system  CECELIA_HOME=<dir>
set -eu

REPO="schienstockd/cecelia"
CHANNEL="${CECELIA_CHANNEL:-stable}"
VERSION="${CECELIA_VERSION:-latest}"
SCOPE="${CECELIA_INSTALL_SCOPE:-user}"
OS="$(uname -s)"

say() { printf '\033[1;36m[cecelia]\033[0m %s\n' "$1"; }
err() { printf '\033[1;31m[cecelia] error:\033[0m %s\n' "$1" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

have curl || err "curl is required."
have tar  || err "tar is required."

# ── Install location + scope ──────────────────────────────────────────────────
# CECELIA_HOME overrides either default. Expand a leading ~ / ~/ ourselves: a quoted or
# assignment-context value (CECELIA_HOME="~/x" / CECELIA_HOME=~/x sh install.sh) skips the shell's
# tilde expansion, so without this a literal `~` directory would be created instead of $HOME.
if [ -n "${CECELIA_HOME:-}" ]; then
  case "$CECELIA_HOME" in
    "~")   INSTALL_DIR="$HOME" ;;
    "~/"*) INSTALL_DIR="$HOME/${CECELIA_HOME#\~/}" ;;
    *)     INSTALL_DIR="$CECELIA_HOME" ;;
  esac
elif [ "$SCOPE" = "system" ]; then
  case "$OS" in
    Darwin) INSTALL_DIR="/Applications/cecelia" ;;
    *)      INSTALL_DIR="/opt/cecelia" ;;
  esac
else
  INSTALL_DIR="$HOME/.local/share/cecelia"
fi

# In system scope every tool + env lives under the shared install dir (so all accounts share one
# runtime) and the write needs root. In user scope, Pixi/Juliaup keep their usual per-user homes.
if [ "$SCOPE" = "system" ]; then
  [ "$(id -u)" = "0" ] || err "System-wide install writes to $INSTALL_DIR and needs root. Re-run:
       curl -LsSf https://raw.githubusercontent.com/$REPO/main/install.sh | CECELIA_INSTALL_SCOPE=system sudo -E sh"
  PIXI_HOME="$INSTALL_DIR/pixi"                       # Pixi installer + tools honour PIXI_HOME
  JULIAUP_DEPOT_PATH="$INSTALL_DIR/juliaup"           # shared Julia versions + juliaup state
  JULIA_DEPOT_PATH="$INSTALL_DIR/juliaup/depot"       # shared Julia package depot (Manifest)
  export PIXI_HOME JULIAUP_DEPOT_PATH JULIA_DEPOT_PATH
else
  PIXI_HOME="${PIXI_HOME:-$HOME/.pixi}"; export PIXI_HOME
fi

# ── Fetch Cecelia (release bundle, or branch source for the dev channel) ─────
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

if [ "$CHANNEL" = "dev" ]; then
  # Current GitHub state: a branch archive (source only — the frontend is built below). GitHub serves
  # any branch as a tarball at archive/refs/heads/<branch>.tar.gz, so no tag/release is needed.
  have npm || err "The dev channel builds the frontend from source and needs Node.js (npm) on PATH.
       Install Node >= 20 (e.g. via fnm or nvm), then re-run — or use the default stable channel."
  BRANCH="${CECELIA_BRANCH:-main}"
  URL="https://github.com/$REPO/archive/refs/heads/$BRANCH.tar.gz"
  say "Resolving current $BRANCH commit…"
  SHA="$(curl -fsSL "https://api.github.com/repos/$REPO/commits/$BRANCH" \
         | grep -m1 '"sha":' | sed -E 's/.*"sha": *"([^"]+)".*/\1/')"
  PROVENANCE="dev @ $BRANCH ${SHA:-unknown}"
else
  # GitHub's `releases/latest` endpoint only ever resolves to a NON-prerelease release, so while the
  # project is still on release candidates (v*-rcN, all marked prerelease) it 404s. Resolve the newest
  # published release ourselves via the API — it lists prereleases too, newest first.
  if [ "$VERSION" = "latest" ]; then
    say "Resolving the latest release…"
    VERSION="$(curl -fsSL "https://api.github.com/repos/$REPO/releases" \
               | grep -m1 '"tag_name":' | sed -E 's/.*"tag_name": *"([^"]+)".*/\1/')"
    [ -n "$VERSION" ] || err "Could not resolve the latest release from the GitHub API."
    say "Latest release is $VERSION"
  fi
  URL="https://github.com/$REPO/releases/download/$VERSION/cecelia.tar.gz"
  PROVENANCE="$VERSION"
fi

say "Downloading $URL"
curl -fSL "$URL" -o "$TMP/cecelia.tar.gz" \
  || err "Download failed — $([ "$CHANNEL" = dev ] && echo "is branch '$BRANCH' correct?" || echo "does the release exist yet?")"

say "Installing to $INSTALL_DIR"
rm -rf "$INSTALL_DIR"
mkdir -p "$INSTALL_DIR"
# A release bundle extracts flat (api/, app/, …); a branch archive wraps everything in one
# `<repo>-<branch>/` dir, so strip that leading component for the dev channel only. Extract BEFORE
# provisioning tools so the shared runtime (system scope) can be placed inside INSTALL_DIR.
if [ "$CHANNEL" = "dev" ]; then
  tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR" --strip-components=1
else
  tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR"
fi

# ── Pixi (Python env manager) ────────────────────────────────────────────────
# System scope: always into the shared $PIXI_HOME. User scope: reuse one on PATH / in ~/.pixi.
if [ "$SCOPE" = "system" ]; then
  if [ -x "$PIXI_HOME/bin/pixi" ]; then PIXI="$PIXI_HOME/bin/pixi"; else
    say "Installing Pixi into the shared runtime ($PIXI_HOME)…"
    curl -fsSL https://pixi.sh/install.sh | bash
    PIXI="$PIXI_HOME/bin/pixi"
  fi
else
  PIXI="$(command -v pixi 2>/dev/null || true)"
  if [ -z "$PIXI" ]; then
    if [ -x "$PIXI_HOME/bin/pixi" ]; then PIXI="$PIXI_HOME/bin/pixi"; else
      say "Installing Pixi…"
      curl -fsSL https://pixi.sh/install.sh | bash
      PIXI="$PIXI_HOME/bin/pixi"
    fi
  fi
fi
[ -x "$PIXI" ] || have pixi || err "Pixi not found after install."

# ── Julia (via Juliaup) ──────────────────────────────────────────────────────
# System scope: install into the shared depot; user scope: reuse one on PATH / in ~/.juliaup.
if [ "$SCOPE" = "system" ]; then
  if [ ! -x "$JULIAUP_DEPOT_PATH/bin/julia" ]; then
    say "Installing Julia (juliaup) into the shared runtime ($JULIAUP_DEPOT_PATH)…"
    curl -fsSL https://install.julialang.org | sh -s -- --yes --path "$JULIAUP_DEPOT_PATH"
  fi
  JULIA="$JULIAUP_DEPOT_PATH/bin/julia"
else
  if ! have julia && [ ! -x "$HOME/.juliaup/bin/julia" ]; then
    say "Installing Julia (juliaup)…"
    curl -fsSL https://install.julialang.org | sh -s -- --yes
  fi
  JULIA="$(command -v julia 2>/dev/null || echo "$HOME/.juliaup/bin/julia")"
fi
[ -x "$JULIA" ] || err "Julia not found after install — open a new terminal and re-run."

# ── bioformats2raw (image import) ─────────────────────────────────────────────
# ~190 MB, so fetched here rather than shipped in the bundle. The app resolves it at
# <install>/bioformats2raw/bin (bioformats2raw_bin() in config.jl); Java comes from the Pixi env.
# Skipped if a system bioformats2raw is already on PATH (the app falls back to PATH).
if have bioformats2raw; then
  say "Using bioformats2raw already on PATH ($(command -v bioformats2raw))."
else
  have unzip || err "unzip is required to install bioformats2raw."
  # Pinned version (reproducible installs — not their `latest`, so our import engine can't change
  # under us on an upstream release). Override with CECELIA_BIOFORMATS2RAW_VERSION.
  B2R_VERSION="${CECELIA_BIOFORMATS2RAW_VERSION:-0.12.1}"
  B2R_URL="https://github.com/glencoesoftware/bioformats2raw/releases/download/v$B2R_VERSION/bioformats2raw-$B2R_VERSION.zip"
  say "Fetching bioformats2raw $B2R_VERSION (image import; ~190 MB)…"
  curl -fSL "$B2R_URL" -o "$TMP/b2r.zip" || err "bioformats2raw download failed ($B2R_URL)."
  unzip -q "$TMP/b2r.zip" -d "$TMP/b2r"
  mv "$TMP"/b2r/bioformats2raw-*/ "$INSTALL_DIR/bioformats2raw"
  [ -x "$INSTALL_DIR/bioformats2raw/bin/bioformats2raw" ] || err "bioformats2raw missing after unpack."
  say "Installed bioformats2raw."
fi

# ── Provision ────────────────────────────────────────────────────────────────
cd "$INSTALL_DIR"
say "Installing the Python environment (downloads a few GB on first run)…"
"$PIXI" install
say "Precompiling Julia (a few minutes on first run)…"
"$JULIA" --project=api -e 'using Pkg; Pkg.instantiate()'

# The dev channel ships source only — build the frontend the server serves (stable already has it).
if [ "$CHANNEL" = "dev" ]; then
  say "Building the frontend (dev channel)…"
  # `npm install`, not `npm ci`: npm silently skips a platform-specific optional native dep (the
  # rolldown binding vite 8 bundles with) when the lockfile was made on another OS (npm/cli#4828) —
  # `npm ci` can leave the build without its native binding. See .github/workflows/ci.yml.
  ( cd "$INSTALL_DIR/frontend" && npm install && npm run build )
fi

# Record what was installed (channel + tag/commit) for provenance and bug reports, plus the scope so
# the in-app updater knows whether it may self-update (user) or must defer to an admin (system).
printf '%s\n' "$PROVENANCE" > "$INSTALL_DIR/.cecelia-version"
printf '%s\n' "$SCOPE"       > "$INSTALL_DIR/.cecelia-scope"
say "Installed: $PROVENANCE ($SCOPE scope)"

# ── Launcher ───────────────────────────────────────────────────────────────────
ICON="$INSTALL_DIR/frontend/dist/favicon.svg"

if [ "$SCOPE" = "system" ]; then
  # A wrapper any account runs: it exports the shared runtime env so `pixi run app` finds the shared
  # Pixi env + Julia depot regardless of the caller's own PATH/home. World-readable + executable.
  LAUNCH="$INSTALL_DIR/cecelia-launch.sh"
  cat > "$LAUNCH" <<EOF
#!/bin/sh
export PIXI_HOME="$PIXI_HOME"
export JULIAUP_DEPOT_PATH="$JULIAUP_DEPOT_PATH"
export JULIA_DEPOT_PATH="$JULIA_DEPOT_PATH"
export PATH="$PIXI_HOME/bin:$JULIAUP_DEPOT_PATH/bin:\$PATH"
cd "$INSTALL_DIR" && exec "$PIXI" run app
EOF
  chmod 755 "$LAUNCH"
  chmod -R a+rX "$INSTALL_DIR"          # ensure every account can read/execute the shared tree
  case "$OS" in
    Linux)
      APPS="/usr/share/applications"; mkdir -p "$APPS"
      cat > "$APPS/cecelia.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Cecelia
Comment=Image analysis
Exec=$LAUNCH
Icon=$ICON
Terminal=true
Categories=Science;Education;
EOF
      say "Installed a system-wide 'Cecelia' application-menu entry."
      ;;
    Darwin)
      # macOS is multi-user too: put the launcher at the top of /Applications (all-users, root-owned,
      # world-executable) rather than buried inside the install dir, mirroring the Linux all-users
      # /usr/share/applications entry. Points at the shared-runtime wrapper.
      CMD="/Applications/Cecelia.command"
      cat > "$CMD" <<EOF
#!/bin/sh
exec "$LAUNCH"
EOF
      chmod 755 "$CMD"
      say "Installed /Applications/Cecelia.command — any user can double-click to launch."
      ;;
  esac
  say "Done (system-wide). Any user can launch Cecelia; updates are admin-only (re-run this as root)."
else
  case "$OS" in
    Linux)
      APPS="$HOME/.local/share/applications"; mkdir -p "$APPS"
      cat > "$APPS/cecelia.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Cecelia
Comment=Image analysis
Exec=sh -c 'cd "$INSTALL_DIR" && "$PIXI" run app'
Icon=$ICON
Terminal=true
Categories=Science;Education;
EOF
      say "Installed a 'Cecelia' entry in your application menu."
      ;;
    Darwin)
      mkdir -p "$HOME/Applications"
      CMD="$HOME/Applications/Cecelia.command"
      cat > "$CMD" <<EOF
#!/bin/sh
cd "$INSTALL_DIR" && exec "$PIXI" run app
EOF
      chmod +x "$CMD"
      say "Installed ~/Applications/Cecelia.command — double-click to launch."
      ;;
  esac
  say "Done. Launch Cecelia from your menu, or run:  cd \"$INSTALL_DIR\" && \"$PIXI\" run app"
fi
