#!/usr/bin/env sh
# Cecelia uninstaller — Linux & macOS.
#
# Removes the install (app, Python env and, for a system install, its shared Pixi + Julia), the menu
# entry / Cecelia.app, launcher logs and the Claude observer registration. Your settings (~/.cecelia)
# and your projects are KEPT unless you ask for them to go — it asks on a terminal.
#
#   sh ~/.local/share/cecelia/uninstall.sh            # the copy inside the install
#   curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/uninstall.sh | sh
#
# Options (or the env var in brackets, for `curl | sh`):
#   --wipe-settings   also delete ~/.cecelia: settings, profiles (incl. Claude logins), models,
#                     custom modules                                   [CECELIA_WIPE_SETTINGS=1]
#   --wipe-projects   also delete your projects — each <projects>/<uid>/ with a project.json;
#                     anything else in that folder is left alone        [CECELIA_WIPE_PROJECTS=1]
#   --data-only       keep the install, remove only your own settings/projects (per the flags) —
#                     for another account on a shared machine
#   --yes             don't ask; do exactly what the flags say          [CECELIA_YES=1]
#   CECELIA_HOME=<dir> / CECELIA_INSTALL_SCOPE=system pick the install, as for install.sh.
#
# Never removed: Pixi (~/.pixi), Julia (~/.juliaup, ~/.julia), caches (~/.cache, ~/.cellpose) and
# Claude (~/.claude*) — other software uses them. The summary lists the ones present.
# A system install is removed for everyone; other accounts' settings and projects are never touched.
# Design: docs/todo/INSTALL_OWNER_UNINSTALL_PLAN.md (D6).
set -eu

say()  { printf '\033[1;36m[cecelia]\033[0m %s\n' "$1"; }
err()  { printf '\033[1;31m[cecelia] error:\033[0m %s\n' "$1" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }
as_root() { if [ "$(id -u)" = "0" ]; then "$@"; else
  have sudo || err "Removing $* needs root and sudo is not available."; sudo "$@"; fi; }
size_of() { du -sh "$1" 2>/dev/null | awk '{print $1}'; }

WIPE_SETTINGS="${CECELIA_WIPE_SETTINGS:-0}"
WIPE_PROJECTS="${CECELIA_WIPE_PROJECTS:-0}"
YES="${CECELIA_YES:-0}"
DATA_ONLY=0
for a in "$@"; do
  case "$a" in
    --wipe-settings) WIPE_SETTINGS=1 ;;
    --wipe-projects) WIPE_PROJECTS=1 ;;
    --data-only)     DATA_ONLY=1 ;;
    --yes|-y)        YES=1 ;;
    -h|--help)       sed -n '2,24p' "$0" 2>/dev/null | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) err "Unknown option: $a (see --help)" ;;
  esac
done

# Questions go to the terminal even under `curl | sh` (stdin is the script there).
TTY=""; if [ "$YES" != "1" ] && (: </dev/tty) 2>/dev/null; then TTY=/dev/tty; fi
ask() {  # ask "question" → 0 for yes
  [ -n "$TTY" ] || return 1
  printf '\033[1;33m[cecelia]\033[0m %s [y/N] ' "$1" >/dev/tty
  read -r ans </dev/tty || return 1
  case "$ans" in y|Y|yes|YES) return 0 ;; *) return 1 ;; esac
}

# ── Whose data ────────────────────────────────────────────────────────────────
# Under `sudo` HOME is /root; the data to consider is the invoking user's.
USER_HOME="$HOME"
if [ "$(id -u)" = "0" ] && [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then
  case "$SUDO_USER" in *[!A-Za-z0-9._-]*) err "Unexpected SUDO_USER: $SUDO_USER" ;; esac
  USER_HOME="$(eval echo "~$SUDO_USER")"
fi
CONFIG_DIR="$USER_HOME/.cecelia"

# ── Find the install ──────────────────────────────────────────────────────────
OS="$(uname -s)"
expand_tilde() { case "$1" in "~") echo "$USER_HOME" ;; "~/"*) echo "$USER_HOME/${1#\~/}" ;; *) echo "$1" ;; esac; }
is_install() { [ -f "$1/.cecelia-version" ] || { [ -f "$1/app.py" ] && [ -f "$1/pixi.toml" ] && [ -f "$1/.cecelia-scope" ]; }; }

USER_DEFAULT="$USER_HOME/.local/share/cecelia"
case "$OS" in Darwin) SYSTEM_DEFAULT="/Applications/cecelia" ;; *) SYSTEM_DEFAULT="/opt/cecelia" ;; esac
SCRIPT_DIR="$(cd "$(dirname "$0")" 2>/dev/null && pwd || true)"

INSTALL_DIR=""
if [ -n "${CECELIA_HOME:-}" ]; then INSTALL_DIR="$(expand_tilde "$CECELIA_HOME")"
elif [ -n "$SCRIPT_DIR" ] && is_install "$SCRIPT_DIR"; then INSTALL_DIR="$SCRIPT_DIR"
elif [ "${CECELIA_INSTALL_SCOPE:-}" = "system" ]; then INSTALL_DIR="$SYSTEM_DEFAULT"
elif [ "${CECELIA_INSTALL_SCOPE:-}" = "user" ]; then INSTALL_DIR="$USER_DEFAULT"
elif is_install "$USER_DEFAULT"; then INSTALL_DIR="$USER_DEFAULT"
elif is_install "$SYSTEM_DEFAULT"; then INSTALL_DIR="$SYSTEM_DEFAULT"
fi

if [ "$DATA_ONLY" = "1" ]; then
  INSTALL_DIR=""
elif [ -z "$INSTALL_DIR" ] || [ ! -d "$INSTALL_DIR" ]; then
  say "No Cecelia install found${INSTALL_DIR:+ at $INSTALL_DIR} — only your own data will be considered."
  INSTALL_DIR=""
else
  [ -e "$INSTALL_DIR/.git" ] && err "$INSTALL_DIR is a git checkout, not an install — not removing it."
  is_install "$INSTALL_DIR" || err "$INSTALL_DIR does not look like a Cecelia install (no .cecelia-version) — not removing it."
fi
SCOPE="user"
[ -n "$INSTALL_DIR" ] && [ "$(cat "$INSTALL_DIR/.cecelia-scope" 2>/dev/null)" = "system" ] && SCOPE="system"

# ── Refuse while it runs ──────────────────────────────────────────────────────
# Deleting a live env corrupts whatever the server is writing. The app's processes run with their
# working dir inside the install (`pixi run app` → python app.py → julia in api/), and their command
# lines are mostly relative, so match on the working dir / binary, then keep only Cecelia's runtimes —
# a terminal that merely sits in the install dir is not "running". A non-root uninstaller cannot read
# another account's working dir, so also match command lines naming the install: on a shared install
# the launcher runs `<install>/pixi/bin/pixi run …` and Julia from `<install>/juliaup/…`.
running_in() {  # pids whose cwd, binary or command line is under $1
  ps -eo pid=,args= 2>/dev/null | awk -v d="$1/" 'index($0, d) { print $1 }'
  if [ -d /proc/self ]; then
    for p in /proc/[0-9]*; do
      for l in cwd exe; do
        t="$(readlink "$p/$l" 2>/dev/null || true)"
        case "$t" in "$1"|"$1"/*) echo "${p#/proc/}"; break ;; esac
      done
    done
  elif have lsof; then
    lsof -w -d cwd,txt -Fpn 2>/dev/null | awk -v d="$1" '
      /^p/ { pid = substr($0, 2) } /^n/ { n = substr($0, 2); if (n == d || index(n, d "/") == 1) print pid }'
  fi
}
if [ -n "$INSTALL_DIR" ]; then
  RUNNING=""
  for pid in $(running_in "$INSTALL_DIR" | sort -u); do
    [ "$pid" = "$$" ] && continue
    c="$(ps -o comm= -p "$pid" 2>/dev/null || true)"
    case "${c##*/}" in julia*|python*|pixi*|node*|java*) RUNNING="$RUNNING  $pid ${c##*/}
" ;; esac
  done
  [ -z "$RUNNING" ] || err "Cecelia is still running from $INSTALL_DIR — quit it (Settings → Shut down) and run this again.
$RUNNING"
fi

# ── Projects dir (read before settings can go) ────────────────────────────────
# `[dirs] projects = "…"` in custom.toml — stored as typed, `~` expanded on read (config.jl). The
# placeholder "/path/to/projects" means the setup wizard never ran.
PROJECTS_DIR=""
if [ -f "$CONFIG_DIR/custom.toml" ]; then
  PROJECTS_DIR="$(awk '
    /^[ \t]*\[/ { sec = $0; gsub(/[ \t\[\]]/, "", sec); next }
    sec == "dirs" && /^[ \t]*projects[ \t]*=/ {
      v = $0; sub(/^[^=]*=[ \t]*/, "", v); sub(/[ \t]*(#.*)?$/, "", v)
      if (v ~ /^".*"$/ || v ~ /^\x27.*\x27$/) v = substr(v, 2, length(v) - 2)
      print v; exit }' "$CONFIG_DIR/custom.toml")"
  PROJECTS_DIR="$(expand_tilde "$PROJECTS_DIR")"
  [ "$PROJECTS_DIR" = "/path/to/projects" ] && PROJECTS_DIR=""
fi
PROJECTS=""          # newline-separated project dirs (those holding a project.json)
if [ -n "$PROJECTS_DIR" ] && [ -d "$PROJECTS_DIR" ]; then
  for d in "$PROJECTS_DIR"/*/; do
    [ -f "${d}project.json" ] && PROJECTS="$PROJECTS${d%/}
"
  done
fi
N_PROJECTS="$(printf '%s' "$PROJECTS" | grep -c . || true)"

# ── What's here ──────────────────────────────────────────────────────────────
say "Found:"
[ -n "$INSTALL_DIR" ] && echo "    install    $INSTALL_DIR ($SCOPE scope, $(size_of "$INSTALL_DIR"))"
[ -d "$CONFIG_DIR" ] && echo "    settings   $CONFIG_DIR ($(size_of "$CONFIG_DIR"))"
if [ -n "$PROJECTS_DIR" ]; then
  echo "    projects   $N_PROJECTS in $PROJECTS_DIR ($(size_of "$PROJECTS_DIR"))"
fi
[ -n "$INSTALL_DIR" ] || [ -d "$CONFIG_DIR" ] || [ -n "$PROJECTS_DIR" ] || { say "Nothing to remove."; exit 0; }

# ── Decide ───────────────────────────────────────────────────────────────────
# Without a terminal nothing can be confirmed, so only --yes proceeds (and then only per the flags).
if [ "$YES" != "1" ] && [ -z "$TTY" ]; then
  err "No terminal to confirm on — re-run with --yes (plus --wipe-settings / --wipe-projects if wanted)."
fi
if [ -n "$INSTALL_DIR" ] && [ "$YES" != "1" ]; then
  ask "Remove Cecelia from $INSTALL_DIR?" || { say "Nothing removed."; exit 0; }
fi
if [ "$WIPE_SETTINGS" != "1" ] && [ -d "$CONFIG_DIR" ] && [ "$YES" != "1" ]; then
  ask "Also delete your settings, profiles and models in $CONFIG_DIR? (No keeps them for a reinstall)" && WIPE_SETTINGS=1
fi
if [ "$N_PROJECTS" -gt 0 ] && [ "$YES" != "1" ]; then
  if [ "$WIPE_PROJECTS" = "1" ] || ask "Also delete your $N_PROJECTS project(s) in $PROJECTS_DIR? (No keeps them)"; then
    printf '\033[1;31m[cecelia]\033[0m This permanently deletes %s project(s) — analysis results, notebooks, lab logs. Type delete to confirm: ' "$N_PROJECTS" >/dev/tty
    read -r ans </dev/tty || ans=""
    if [ "$ans" = "delete" ]; then WIPE_PROJECTS=1; else WIPE_PROJECTS=0; say "Projects kept."; fi
  fi
fi

# ── Remove the install ───────────────────────────────────────────────────────
remove_tree() {  # a tree we may or may not own (system scope: owner = the admin, parent = root's)
  if rm -rf "$1" 2>/dev/null && [ ! -e "$1" ]; then return 0; fi
  say "Removing $1 needs root (sudo may ask for your password)…"; as_root rm -rf "$1"
}
remove_file() { [ -e "$1" ] || [ -L "$1" ] || return 0; rm -rf "$1" 2>/dev/null || as_root rm -rf "$1"; echo "    removed $1"; }

if [ -n "$INSTALL_DIR" ]; then
  # The observer MCP registration — only one that points into THIS install, so a dev checkout's
  # registration of the same name survives. Through the CLI, as the app registered it.
  # Read-only peek first: the CLI would create ~/.claude.json on a machine where Claude never ran.
  if have claude && grep -qF cecelia-observer "$USER_HOME/.claude.json" 2>/dev/null \
     && claude mcp get cecelia-observer 2>/dev/null | grep -qF "$INSTALL_DIR"; then
    claude mcp remove cecelia-observer -s user >/dev/null 2>&1 && echo "    removed the cecelia-observer registration from Claude"
  fi

  say "Removing ${INSTALL_DIR}…"
  remove_tree "$INSTALL_DIR"
  case "$OS" in
    Darwin)
      if [ "$SCOPE" = "system" ]; then remove_file /Applications/Cecelia.app; remove_file /Applications/Cecelia.command
      else remove_file "$USER_HOME/Applications/Cecelia.app"; remove_file "$USER_HOME/Applications/Cecelia.command"; fi ;;
    *)
      if [ "$SCOPE" = "system" ]; then D=/usr/share/applications/cecelia.desktop
      else D="$USER_HOME/.local/share/applications/cecelia.desktop"; fi
      # Only our entry: it names this install's path.
      if [ -f "$D" ] && grep -qF "$INSTALL_DIR" "$D"; then remove_file "$D"; fi ;;
  esac
  remove_file "$USER_HOME/Library/Logs/Cecelia"
  remove_file "$USER_HOME/cecelia-connection.json"
fi

# ── Your data ─────────────────────────────────────────────────────────────────
remove_file "$CONFIG_DIR/julia-depot"        # per-user Julia cache for a shared install — always
if [ "$WIPE_PROJECTS" = "1" ] && [ "$N_PROJECTS" -gt 0 ]; then
  say "Deleting $N_PROJECTS project(s)…"
  printf '%s' "$PROJECTS" | while IFS= read -r p; do [ -n "$p" ] && rm -rf "$p" && echo "    removed $p"; done
  rmdir "$PROJECTS_DIR" 2>/dev/null && echo "    removed $PROJECTS_DIR (now empty)" \
    || say "Kept $PROJECTS_DIR — it holds other files besides Cecelia projects."
fi
if [ "$WIPE_SETTINGS" = "1" ] && [ -d "$CONFIG_DIR" ]; then
  say "Deleting settings…"; remove_file "$CONFIG_DIR"
fi

# ── Summary ──────────────────────────────────────────────────────────────────
say "Done."
[ -d "$CONFIG_DIR" ] && echo "    kept settings  $CONFIG_DIR   (remove: --data-only --wipe-settings)"
[ "$WIPE_PROJECTS" != "1" ] && [ "$N_PROJECTS" -gt 0 ] && echo "    kept projects  $PROJECTS_DIR   (remove: --data-only --wipe-projects)"
SHARED=""
for t in .pixi .juliaup .julia .cellpose .cache/rattler; do
  [ -d "$USER_HOME/$t" ] && SHARED="$SHARED    $USER_HOME/$t ($(size_of "$USER_HOME/$t"))
"
done
if [ -n "$SHARED" ]; then
  echo "    Left in place — shared with other software, delete by hand if nothing else needs them:"
  printf '%s' "$SHARED" | sed 's/^/  /'
fi
[ "$SCOPE" = "system" ] && echo "    Other accounts' settings and projects are untouched; each can run this with --data-only."
exit 0
