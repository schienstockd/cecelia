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
# Env overrides:  CECELIA_CHANNEL=stable|dev  CECELIA_VERSION=v0.1.0  CECELIA_BRANCH=main
#                 CECELIA_HOME=~/.local/share/cecelia
set -eu

REPO="schienstockd/cecelia"
INSTALL_DIR="${CECELIA_HOME:-$HOME/.local/share/cecelia}"
CHANNEL="${CECELIA_CHANNEL:-stable}"
VERSION="${CECELIA_VERSION:-latest}"

say() { printf '\033[1;36m[cecelia]\033[0m %s\n' "$1"; }
err() { printf '\033[1;31m[cecelia] error:\033[0m %s\n' "$1" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

have curl || err "curl is required."
have tar  || err "tar is required."

# ── Pixi (Python env manager) ────────────────────────────────────────────────
PIXI="$(command -v pixi 2>/dev/null || true)"
if [ -z "$PIXI" ]; then
  if [ -x "$HOME/.pixi/bin/pixi" ]; then
    PIXI="$HOME/.pixi/bin/pixi"
  else
    say "Installing Pixi…"
    curl -fsSL https://pixi.sh/install.sh | bash
    PIXI="$HOME/.pixi/bin/pixi"
  fi
fi
[ -x "$PIXI" ] || have pixi || err "Pixi not found after install."

# ── Julia (via Juliaup) ──────────────────────────────────────────────────────
if ! have julia && [ ! -x "$HOME/.juliaup/bin/julia" ]; then
  say "Installing Julia (juliaup)…"
  curl -fsSL https://install.julialang.org | sh -s -- --yes
fi
JULIA="$(command -v julia 2>/dev/null || echo "$HOME/.juliaup/bin/julia")"
[ -x "$JULIA" ] || err "Julia not found after install — open a new terminal and re-run."

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
# `<repo>-<branch>/` dir, so strip that leading component for the dev channel only.
if [ "$CHANNEL" = "dev" ]; then
  tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR" --strip-components=1
else
  tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR"
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

# Record what was installed (channel + tag/commit) for provenance and bug reports.
printf '%s\n' "$PROVENANCE" > "$INSTALL_DIR/.cecelia-version"
say "Installed: $PROVENANCE"

# ── Desktop launcher ─────────────────────────────────────────────────────────
ICON="$INSTALL_DIR/frontend/dist/favicon.svg"
case "$(uname -s)" in
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
