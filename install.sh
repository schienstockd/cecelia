#!/usr/bin/env sh
# Cecelia installer — Linux & macOS.
#
# Installs Pixi and Juliaup if missing, downloads the latest Cecelia release (which ships a
# prebuilt frontend, so no Node is needed), provisions the environment, and adds a desktop
# launcher. Re-runnable — it replaces the install in place.
#
#   curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | sh
#
# Env overrides:  CECELIA_VERSION=v0.1.0  CECELIA_HOME=~/.local/share/cecelia
set -eu

REPO="schienstockd/cecelia"
INSTALL_DIR="${CECELIA_HOME:-$HOME/.local/share/cecelia}"
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

# ── Download the release bundle ──────────────────────────────────────────────
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
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
say "Downloading $URL"
curl -fSL "$URL" -o "$TMP/cecelia.tar.gz" || err "Download failed — does the release exist yet?"

say "Installing to $INSTALL_DIR"
rm -rf "$INSTALL_DIR"
mkdir -p "$INSTALL_DIR"
tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR"

# ── Provision ────────────────────────────────────────────────────────────────
cd "$INSTALL_DIR"
say "Installing the Python environment (downloads a few GB on first run)…"
"$PIXI" install
say "Precompiling Julia (a few minutes on first run)…"
"$JULIA" --project=api -e 'using Pkg; Pkg.instantiate()'

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
