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
#   system         — one shared install for all users (/opt/cecelia; /Applications/cecelia on macOS),
#                    OWNED BY THE ADMIN WHO RUNS THIS. Run it as that admin, without sudo: it asks for
#                    sudo itself only to create the install dir and the all-users menu entry. Pixi,
#                    Juliaup and the multi-GB env are provisioned INSIDE the install dir so every
#                    account shares one runtime — set via a launcher wrapper. Other accounts get
#                    read-only access; per-user config + projects still live in ~/.cecelia (never
#                    shared). Updates: the owner re-runs this (no sudo needed once the dir exists).
#                    `sudo sh` still works — the work is handed to $SUDO_USER, so nothing lands in /root.
#
#   curl -LsSf .../install.sh | CECELIA_INSTALL_SCOPE=system sh
#
# Remove with uninstall.sh (kept in the install dir) — see docs/INSTALL.md → Uninstall.
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
# runtime). In user scope, Pixi/Juliaup keep their usual per-user homes.
#
# System scope is OWNED by the admin installing it, not by root (docs/todo/INSTALL_OWNER_UNINSTALL_PLAN.md
# D1/D2). Root is needed only to create the dir under /opt and to write /usr/share/applications.
# Everything else runs as OWNER, so the package caches land in the owner's home, never in /root.
OWNER=""            # set in system scope: the account that owns the install
DELEGATE=""         # "1" when running as root but doing the work as $OWNER (the `sudo sh` case)
as_root() { if [ "$(id -u)" = "0" ]; then "$@"; else
  have sudo || err "This step needs root and sudo is not available: $*"; sudo "$@"; fi; }
as_owner() { if [ -n "$DELEGATE" ]; then
  sudo -u "$OWNER" -H env PIXI_HOME="$PIXI_HOME" PIXI_NO_PATH_UPDATE=1 \
    JULIAUP_DEPOT_PATH="$JULIAUP_DEPOT_PATH" JULIA_DEPOT_PATH="$JULIA_DEPOT_PATH" PATH="$PATH" "$@"
  else "$@"; fi; }

if [ "$SCOPE" = "system" ]; then
  if [ "$(id -u)" != "0" ]; then OWNER="$(id -un)"
  elif [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then OWNER="$SUDO_USER"; DELEGATE=1
  else OWNER="root"                                  # a root login (cloud VM): root owns it
  fi
  PIXI_HOME="$INSTALL_DIR/pixi"                       # Pixi installer + tools honour PIXI_HOME
  JULIAUP_DEPOT_PATH="$INSTALL_DIR/juliaup"           # shared Julia versions + juliaup state
  # Shared package depot first, then the trailing empty entry = Julia's bundled stdlib depot. Without
  # it the stdlibs are recompiled into the shared depot (see the launcher below for the runtime side).
  JULIA_DEPOT_PATH="$INSTALL_DIR/juliaup/depot:"
  PIXI_NO_PATH_UPDATE=1                               # the launcher sets PATH; no ~/.bashrc edit
  export PIXI_HOME JULIAUP_DEPOT_PATH JULIA_DEPOT_PATH PIXI_NO_PATH_UPDATE
else
  PIXI_HOME="${PIXI_HOME:-$HOME/.pixi}"; export PIXI_HOME
  JULIAUP_DEPOT_PATH="${JULIAUP_DEPOT_PATH:-}"; JULIA_DEPOT_PATH="${JULIA_DEPOT_PATH:-}"
fi

# ── Fetch Cecelia (release bundle, or branch source for the dev channel) ─────
# Under `sudo sh` the as_owner steps unpack in here, so it must be reachable by $OWNER: macOS gives
# root a TMPDIR inside a root-only /var/folders/… dir, hence /tmp explicitly.
if [ -n "$DELEGATE" ]; then TMP="$(mktemp -d /tmp/cecelia-install.XXXXXX)"; chown "$OWNER" "$TMP"
else TMP="$(mktemp -d)"; fi
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

# Verify the bundle against the SHA-256 published beside it. HTTPS covers the transport; this covers
# a truncated or swapped asset. Stable channel only — the dev channel pulls a GitHub branch archive,
# which has no digest.
#
# VERIFY-IF-PRESENT: releases up to v0.1.0-rc9 predate the digest asset, so a MISSING one is not
# fatal (that would make every existing release uninstallable). A MISMATCH is fatal.
if [ "$CHANNEL" != "dev" ] && curl -fsSL "$URL.sha256" -o "$TMP/cecelia.tar.gz.sha256" 2>/dev/null; then
  expected="$(awk '{print $1}' "$TMP/cecelia.tar.gz.sha256" 2>/dev/null)"
  # sha256sum is GNU/Linux; macOS ships shasum. Neither guaranteed → skip rather than fail.
  if   have sha256sum; then actual="$(sha256sum "$TMP/cecelia.tar.gz" | awk '{print $1}')"
  elif have shasum;    then actual="$(shasum -a 256 "$TMP/cecelia.tar.gz" | awk '{print $1}')"
  else actual=""; say "Neither sha256sum nor shasum found — skipping checksum verification."
  fi
  if [ -n "$actual" ] && [ -n "$expected" ]; then
    [ "$actual" = "$expected" ] || err "Checksum mismatch for $VERSION.
  expected: $expected
  actual:   $actual
The download is corrupt or has been tampered with — not installing."
    say "Checksum verified."
  fi
fi

say "Installing to $INSTALL_DIR"
# Make the dir exist and be the owner's. In system scope this is the one step that may need sudo: a
# new dir under /opt, or one a previous root-run install left root-owned.
if [ ! -d "$INSTALL_DIR" ] && ! mkdir -p "$INSTALL_DIR" 2>/dev/null; then
  [ "$SCOPE" = "system" ] || err "Cannot create $INSTALL_DIR."
  say "Creating $INSTALL_DIR for $OWNER (sudo may ask for your password)…"
  as_root mkdir -p "$INSTALL_DIR"
fi
if [ "$SCOPE" = "system" ] && [ "$OWNER" != "root" ] \
   && [ "$(ls -ld "$INSTALL_DIR" | awk '{print $3}')" != "$OWNER" ]; then
  say "Handing $INSTALL_DIR to $OWNER (sudo may ask for your password)…"
  as_root chown -R "$OWNER" "$INSTALL_DIR"
fi
[ -w "$INSTALL_DIR" ] || [ -n "$DELEGATE" ] || err "$INSTALL_DIR is not writable by $(id -un)."
# Empty it rather than delete it: the owner can always empty its own dir, but removing it needs
# write access to the parent (/opt).
as_owner find "$INSTALL_DIR" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
# A release bundle extracts flat (api/, app/, …); a branch archive wraps everything in one
# `<repo>-<branch>/` dir, so strip that leading component for the dev channel only. Extract BEFORE
# provisioning tools so the shared runtime (system scope) can be placed inside INSTALL_DIR.
if [ "$CHANNEL" = "dev" ]; then
  as_owner tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR" --strip-components=1
else
  as_owner tar -xzf "$TMP/cecelia.tar.gz" -C "$INSTALL_DIR"
fi

# ── Pixi (Python env manager) ────────────────────────────────────────────────
# System scope: always into the shared $PIXI_HOME. User scope: reuse one on PATH / in ~/.pixi.
if [ "$SCOPE" = "system" ]; then
  if [ -x "$PIXI_HOME/bin/pixi" ]; then PIXI="$PIXI_HOME/bin/pixi"; else
    say "Installing Pixi into the shared runtime ($PIXI_HOME)…"
    curl -fsSL https://pixi.sh/install.sh | as_owner bash
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
# A Cecelia-owned juliaup in <dir>, with its own state (JULIAUP_DEPOT_PATH=<dir>, set by the caller).
# The installer quits with exit 0, having installed nothing, when any `juliaup` is on PATH. So run it
# with a bare PATH. --add-to-path=no keeps it out of the user's shell profile.
juliaup_into() {
  curl -fsSL https://install.julialang.org \
    | as_owner env PATH=/usr/bin:/bin:/usr/sbin:/sbin sh -s -- --yes --add-to-path=no --path "$1"
}

# System scope: install into the shared depot; user scope: reuse one on PATH / in ~/.juliaup.
if [ "$SCOPE" = "system" ]; then
  if [ ! -x "$JULIAUP_DEPOT_PATH/bin/julia" ]; then
    say "Installing Julia (juliaup) into the shared runtime ($JULIAUP_DEPOT_PATH)…"
    juliaup_into "$JULIAUP_DEPOT_PATH"
  fi
  JULIA="$JULIAUP_DEPOT_PATH/bin/julia"
else
  if ! have julia && [ ! -x "$HOME/.juliaup/bin/julia" ]; then
    if have juliaup; then
      # A juliaup with no `julia` launcher: the installer would quit having done nothing (see
      # juliaup_into), so ask the juliaup itself for a default channel; its launcher sits beside it.
      say "Adding a Julia release to the existing juliaup…"
      juliaup add release && juliaup default release
      JULIA="$(dirname "$(command -v juliaup)")/julia"
    else
      say "Installing Julia (juliaup)…"
      curl -fsSL https://install.julialang.org | sh -s -- --yes
    fi
  fi
  [ -n "${JULIA:-}" ] || JULIA="$(command -v julia 2>/dev/null || echo "$HOME/.juliaup/bin/julia")"
fi
[ -x "$JULIA" ] || err "Julia not found after install — open a new terminal and re-run."

# Apple Silicon: the Julia reused above can be an Intel build (an x86 Homebrew, an old .dmg, or a
# ~/.juliaup that Migration Assistant carried over from an Intel Mac). It runs under Rosetta: slower,
# and its children see an Intel `uname`, which broke wgpu's import in the movie renderer (see
# `_native_arm_processor` in python/cecelia/utils/wgpu_host.py).
# An Intel juliaup cannot add an arm64 Julia, so give Cecelia its own native juliaup in
# <install>/juliaup, the same layout as system scope. Every `pixi run` task prefers it when present
# (scripts/activate_juliaup.sh), and so does app.py. The user's own Julia is left alone.
if [ "$SCOPE" != "system" ] && [ "$OS" = "Darwin" ] \
   && [ "$(sysctl -n hw.optional.arm64 2>/dev/null)" = "1" ]; then
  JULIA_ARCH="$("$JULIA" --startup-file=no -e 'print(Sys.ARCH)' 2>/dev/null || true)"
  if [ "$JULIA_ARCH" != "aarch64" ]; then
    JULIAUP_DEPOT_PATH="$INSTALL_DIR/juliaup"; export JULIAUP_DEPOT_PATH
    say "The Julia at $JULIA is not a native Apple Silicon build (${JULIA_ARCH:-unknown}). Installing a native Julia for Cecelia ($JULIAUP_DEPOT_PATH)…"
    juliaup_into "$JULIAUP_DEPOT_PATH"
    JULIA="$JULIAUP_DEPOT_PATH/bin/julia"
    [ "$("$JULIA" --startup-file=no -e 'print(Sys.ARCH)' 2>/dev/null || true)" = "aarch64" ] \
      || err "Could not install a native Julia into $JULIAUP_DEPOT_PATH."
  fi
fi

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
  as_owner unzip -q "$TMP/b2r.zip" -d "$TMP/b2r"
  as_owner mv "$TMP"/b2r/bioformats2raw-*/ "$INSTALL_DIR/bioformats2raw"
  [ -x "$INSTALL_DIR/bioformats2raw/bin/bioformats2raw" ] || err "bioformats2raw missing after unpack."
  say "Installed bioformats2raw."
fi

# ── bftools (import wizard: pyramid-levels advisor for JVM-only formats) ─────
# ~30 MB. The advisor calls `showinf` to peek dims for .czi/.nd2/.oir/.lsm/... before import;
# resolved at <install>/bftools (showinf_bin() in config/binaries.jl). Java comes from the Pixi
# env. Skipped when a system `showinf` is on PATH, OR when we're offline (missing bftools just
# disables the JVM peek path — the wizard degrades to `unsupported` for those formats).
if have showinf; then
  say "Using showinf already on PATH ($(command -v showinf))."
else
  have unzip || err "unzip is required to install bftools."
  BFT_VERSION="${CECELIA_BFTOOLS_VERSION:-8.4.0}"
  BFT_URL="https://downloads.openmicroscopy.org/bio-formats/$BFT_VERSION/artifacts/bftools.zip"
  say "Fetching bftools $BFT_VERSION (import-wizard advisor; ~30 MB)…"
  if curl -fSL "$BFT_URL" -o "$TMP/bft.zip"; then
    as_owner unzip -q "$TMP/bft.zip" -d "$TMP/bft"
    as_owner mv "$TMP"/bft/bftools "$INSTALL_DIR/bftools"
    [ -x "$INSTALL_DIR/bftools/showinf" ] || err "bftools missing after unpack."
    say "Installed bftools."
  else
    say "bftools download failed — the import wizard's dim-peek will be skipped for JVM-only formats."
  fi
fi

# ── Custom cellpose models (segmentation) ─────────────────────────────────────
# NOT fetched. `schienstockd/ceceliaModels` holds cellpose 3 checkpoints (`ccia.fluo`), and
# cellpose 4 cannot load them — it raises "This model does not appear to be a CP4 model". The
# drop-in slot itself is unchanged and takes a v4 checkpoint: <install>/models/cellposeModels/ or
# <config_dir>/models/cellposeModels/ (cellpose_model_path() in config.jl), and `pixi run
# models-fetch` still exists for whenever there is a v4 set to fetch. Cellpose's own weights
# (`cpsam_v2`, ~1.2 GB) are downloaded by cellpose on first use, not here.
# See docs/SEGMENTATION.md → Custom cellpose checkpoints and docs/todo/CELLPOSE_V4_PLAN.md.

# ── Provision ────────────────────────────────────────────────────────────────
cd "$INSTALL_DIR"
say "Installing the Python environment (downloads a few GB on first run)…"
as_owner "$PIXI" install
say "Precompiling Julia (a few minutes on first run)…"
as_owner "$JULIA" --project=api -e 'using Pkg; Pkg.instantiate()'

# The dev channel ships source only — build the frontend the server serves (stable already has it).
if [ "$CHANNEL" = "dev" ]; then
  say "Building the frontend (dev channel)…"
  # `npm install`, not `npm ci`: npm silently skips a platform-specific optional native dep (the
  # rolldown binding vite 8 bundles with) when the lockfile was made on another OS (npm/cli#4828) —
  # `npm ci` can leave the build without its native binding. See .github/workflows/ci.yml.
  #
  # Node/npm come from `pixi exec --spec nodejs` (ephemeral env, ~40 MB cached), NOT the host, so a
  # user with no system Node still gets a working install. Same reasoning as api/src/update_api.jl's
  # dev-channel apply path — see the header there for why not `nodejs` in `pixi.toml`.
  ( cd "$INSTALL_DIR/frontend" && as_owner "$PIXI" exec --spec nodejs -- npm install \
      && as_owner "$PIXI" exec --spec nodejs -- npm run build )
fi

# Record what was installed (channel + tag/commit) for provenance and bug reports, plus the scope so
# the in-app updater knows whether it may self-update (user) or must defer to an admin (system).
printf '%s\n' "$PROVENANCE" > "$INSTALL_DIR/.cecelia-version"
printf '%s\n' "$SCOPE"       > "$INSTALL_DIR/.cecelia-scope"
[ -n "$DELEGATE" ] && chown "$OWNER" "$INSTALL_DIR/.cecelia-version" "$INSTALL_DIR/.cecelia-scope"
say "Installed: $PROVENANCE ($SCOPE scope)"

# ── Launcher ───────────────────────────────────────────────────────────────────
# 256-px PNG rather than an SVG — a handful of older Linux desktop environments still
# don't parse SVG icons in .desktop entries reliably. Source: frontend/public/feijoa.svg,
# regenerated via scripts/regen-icons.sh. See docs/todo/DESKTOP_ICON_PLAN.md.
ICON="$INSTALL_DIR/frontend/dist/icons/cecelia-256.png"

if [ "$SCOPE" = "system" ]; then
  # A wrapper any account runs: it exports the shared runtime env so `pixi run app` finds the shared
  # Pixi env + Julia depot regardless of the caller's own PATH/home. World-readable + executable.
  #
  # JULIA_DEPOT_PATH stacks a per-user writable depot IN FRONT of the shared one: every account but
  # the owner sees the shared depot read-only, and Julia writes compile caches + logs to the FIRST
  # depot. The trailing empty entry appends Julia's bundled stdlib depot — without it the stdlibs
  # look uncompiled and Julia dies precompiling them into the read-only depot (EROFS). The per-user
  # depot stays empty while the shared caches are current. uninstall.sh removes it.
  # `--as-is` (= --frozen --no-install): a plain `pixi run` takes a write lock on the env prefix and
  # dies on a read-only one ("failed to acquire install lock … os error 30"). The installer already
  # provisioned the env; updates re-run the installer.
  LAUNCH="$INSTALL_DIR/cecelia-launch.sh"
  cat > "$LAUNCH" <<EOF
#!/bin/sh
export PIXI_HOME="$PIXI_HOME"
export JULIAUP_DEPOT_PATH="$JULIAUP_DEPOT_PATH"
export JULIA_DEPOT_PATH="\$HOME/.cecelia/julia-depot:$INSTALL_DIR/juliaup/depot:"
export PATH="$PIXI_HOME/bin:$JULIAUP_DEPOT_PATH/bin:\$PATH"
cd "$INSTALL_DIR" && exec "$PIXI" run --as-is app
EOF
  chmod 755 "$LAUNCH"
  chmod -R a+rX "$INSTALL_DIR"          # ensure every account can read/execute the shared tree
  [ -n "$DELEGATE" ] && chown -R "$OWNER" "$INSTALL_DIR"   # catch anything root wrote above
  case "$OS" in
    Linux)
      # The one other root-only write: the all-users menu entry.
      APPS="/usr/share/applications"
      cat > "$TMP/cecelia.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Cecelia
Comment=Image analysis
Exec=$LAUNCH
Icon=$ICON
Terminal=true
Categories=Science;Education;
EOF
      if as_root install -D -m 644 "$TMP/cecelia.desktop" "$APPS/cecelia.desktop"; then
        say "Installed a system-wide 'Cecelia' application-menu entry."
      else
        say "Could not write $APPS/cecelia.desktop (no sudo) — other accounts can start Cecelia with: $LAUNCH"
      fi
      ;;
    Darwin)
      # macOS is multi-user too: put the launcher at the top of /Applications (all-users, root-owned,
      # world-executable) rather than buried inside the install dir, mirroring the Linux all-users
      # /usr/share/applications entry. Minimal .app bundle (not a bare .command) so Finder + Dock
      # pick up the feijoa icon via CFBundleIconFile. See docs/todo/DESKTOP_ICON_PLAN.md.
      # NB: .github/workflows/verify-macos.yml mirrors this block — keep them in sync.
      APP="/Applications/Cecelia.app"
      rm -rf "/Applications/Cecelia.command" "$APP"   # migrate away from the old .command; idempotent
      mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
      # Redirect the supervisor's stdout+stderr to ~/Library/Logs/Cecelia/launcher.log so a silent
      # .app-launched failure (reprovision crash, server crash-loop → app.py `return 1`) leaves a
      # paper trail. Without this, Finder's next Dock click shows only "The application 'Cecelia'
      # is not open anymore." — no cause anywhere. Keep the previous run as .prev so a user can
      # diff before/after an update. Per-user path so system-scope installs (root-owned tree) can
      # still write.
      cat > "$APP/Contents/MacOS/cecelia" <<EOF
#!/bin/sh
LOG_DIR="\$HOME/Library/Logs/Cecelia"
mkdir -p "\$LOG_DIR"
[ -f "\$LOG_DIR/launcher.log" ] && mv -f "\$LOG_DIR/launcher.log" "\$LOG_DIR/launcher.log.prev"
echo "===== \$(date -u +%Y-%m-%dT%H:%M:%SZ) launched =====" >"\$LOG_DIR/launcher.log"
exec "$LAUNCH" >>"\$LOG_DIR/launcher.log" 2>&1
EOF
      chmod 755 "$APP/Contents/MacOS/cecelia"
      cp "$INSTALL_DIR/frontend/dist/icons/cecelia.icns" "$APP/Contents/Resources/cecelia.icns"
      cat > "$APP/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleName</key><string>Cecelia</string>
  <key>CFBundleDisplayName</key><string>Cecelia</string>
  <key>CFBundleIdentifier</key><string>org.cecelia.launcher</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleExecutable</key><string>cecelia</string>
  <key>CFBundleIconFile</key><string>cecelia.icns</string>
</dict></plist>
PLIST
      touch "$APP"    # nudge LaunchServices to refresh the icon cache on re-install
      say "Installed $APP — any user can double-click to launch."
      ;;
  esac
  say "Done (system-wide, owned by $OWNER). Any user can launch Cecelia; $OWNER updates it by re-running this."
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
      # Minimal .app bundle (not a bare .command) so Finder + Dock pick up the feijoa icon via
      # CFBundleIconFile. See docs/todo/DESKTOP_ICON_PLAN.md.
      mkdir -p "$HOME/Applications"
      APP="$HOME/Applications/Cecelia.app"
      rm -rf "$HOME/Applications/Cecelia.command" "$APP"   # migrate away from the old .command; idempotent
      mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
      # Redirect: see the same block under system scope above for the rationale.
      cat > "$APP/Contents/MacOS/cecelia" <<EOF
#!/bin/sh
LOG_DIR="\$HOME/Library/Logs/Cecelia"
mkdir -p "\$LOG_DIR"
[ -f "\$LOG_DIR/launcher.log" ] && mv -f "\$LOG_DIR/launcher.log" "\$LOG_DIR/launcher.log.prev"
echo "===== \$(date -u +%Y-%m-%dT%H:%M:%SZ) launched =====" >"\$LOG_DIR/launcher.log"
cd "$INSTALL_DIR" && exec "$PIXI" run app >>"\$LOG_DIR/launcher.log" 2>&1
EOF
      chmod +x "$APP/Contents/MacOS/cecelia"
      cp "$INSTALL_DIR/frontend/dist/icons/cecelia.icns" "$APP/Contents/Resources/cecelia.icns"
      cat > "$APP/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleName</key><string>Cecelia</string>
  <key>CFBundleDisplayName</key><string>Cecelia</string>
  <key>CFBundleIdentifier</key><string>org.cecelia.launcher</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleExecutable</key><string>cecelia</string>
  <key>CFBundleIconFile</key><string>cecelia.icns</string>
</dict></plist>
PLIST
      touch "$APP"    # nudge LaunchServices to refresh the icon cache on re-install
      say "Installed $APP — double-click to launch."
      ;;
  esac
  say "Done. Launch Cecelia from your menu, or run:  cd \"$INSTALL_DIR\" && \"$PIXI\" run app"
fi

# ── Remote-access handoff ─────────────────────────────────────────────────────
# On a cloud VM (public IP discoverable via metadata) print a connection.json the
# user pastes into the Cecelia laptop launcher's setup wizard — see docs/todo/REMOTE_ACCESS_PLAN.md.
# Local installs (no metadata endpoint) skip this silently. CECELIA_PUBLIC_HOST overrides autodetection.
CONN_HOME="${HOME:-/root}"
CONN="$CONN_HOME/cecelia-connection.json"
if [ -x "$INSTALL_DIR/scripts/print-connection.sh" ]; then
  CONN_JSON=$(sh "$INSTALL_DIR/scripts/print-connection.sh" 2>/dev/null || true)
  if [ -n "$CONN_JSON" ]; then
    printf '%s\n' "$CONN_JSON" > "$CONN"
    chmod 600 "$CONN" 2>/dev/null || true
    say "Remote-access connection profile written to $CONN"
    say "Copy the JSON below into the Cecelia laptop launcher (see docs/INSTALL.md):"
    printf '\n%s\n\n' "$CONN_JSON"
  fi
fi
