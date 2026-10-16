#!/usr/bin/env bash
# Set up an Ubuntu GNOME box with an NVIDIA GPU for remote Cecelia use over RDP (GNOME Remote Desktop,
# Remote Login mode): each remote user gets their own GPU-rendered session behind the GNOME login
# screen, and nothing shows on the box's own monitor. Run once with sudo; safe to re-run. Prints what
# it changed. Design and the reasons behind each step: docs/todo/REMOTE_BOX_PLAN.md.
#
#   sudo bash scripts/remote-box/setup-box.sh [--shared-user NAME] [--ssh-admin NAME] [--dry-run]
#
#   --shared-user NAME  the Ubuntu account everyone logs in with (created if missing). People are told
#                       apart inside Cecelia by user profiles; one person at a time.
#   --ssh-admin NAME    restrict SSH to this account, key login only. Refuses unless NAME already has
#                       an authorized key, so the admin cannot be locked out.
#   --dry-run           print what would change, change nothing (works without sudo).
#
# It never touches the firewall (often IT's), the NVIDIA driver (Ubuntu's updates move it together
# with the kernel modules; remote-check says when a reboot is due) or Cecelia's own config.
set -euo pipefail

SHARED_USER=""; SSH_ADMIN=""; DRY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --shared-user) SHARED_USER=${2:?--shared-user needs a name}; shift 2 ;;
    --ssh-admin)   SSH_ADMIN=${2:?--ssh-admin needs a name}; shift 2 ;;
    --dry-run)     DRY=1; shift ;;
    -h|--help)     sed -n '2,16p' "$0"; exit 0 ;;
    *) echo "unknown option: $1 (see --help)" >&2; exit 2 ;;
  esac
done

GRD_HOME=/var/lib/gnome-remote-desktop
GRD_DIR=$GRD_HOME/.local/share/gnome-remote-desktop
DCONF_FILE=/etc/dconf/db/local.d/00-cecelia-remote-box
SSHD_FILE=/etc/ssh/sshd_config.d/10-cecelia-remote-box.conf
CHANGED=()

say()     { printf '[setup-box] %s\n' "$*"; }
changed() { CHANGED+=("$*"); if [ "$DRY" = 1 ]; then say "would change: $*"; else say "changed: $*"; fi; }
die()     { say "ERROR: $*" >&2; exit 1; }
# Run a command for real, or just print it under --dry-run.
run()     { if [ "$DRY" = 1 ]; then say "would run: $*"; else "$@"; fi; }
confirm() {
  [ "$DRY" = 1 ] && return 0
  read -r -p "[setup-box] $* [y/N] " a
  [ "$a" = y ] || [ "$a" = Y ]
}

preflight() {
  [ "$DRY" = 1 ] || [ "$(id -u)" = 0 ] || die "run with sudo (or --dry-run to preview)"
  . /etc/os-release
  [ "${ID:-}" = ubuntu ] || die "Ubuntu only (found ${ID:-unknown})"
  dpkg --compare-versions "${VERSION_ID:-0}" ge 24.04 || die "needs Ubuntu 24.04 or newer"
  [ -d /usr/share/wayland-sessions ] || die "no GNOME Wayland session installed"
  if command -v nvidia-smi >/dev/null && nvidia-smi -L >/dev/null 2>&1; then
    say "GPU: $(nvidia-smi -L | head -n1)"
  else
    say "WARNING: no working NVIDIA GPU. Sessions will render in software and WebGPU will be slow or off."
  fi
  if ! dpkg -s gnome-remote-desktop >/dev/null 2>&1; then
    confirm "gnome-remote-desktop is not installed. Install it?" || die "gnome-remote-desktop is required"
    run apt-get install -y gnome-remote-desktop && changed "installed gnome-remote-desktop"
  fi
}

# Remote Login = the system daemon: RDP clients land on GNOME's login screen and get their own session.
grd_status() { [ "$DRY" = 1 ] && return 0; grdctl --system status --show-credentials 2>/dev/null; }

remote_login() {
  local before=${#CHANGED[@]}
  if [ "$DRY" = 1 ] || [ ! -s "$GRD_DIR/rdp-tls.crt" ]; then
    run install -d -o gnome-remote-desktop -g gnome-remote-desktop -m 700 "$GRD_DIR"
    run sudo -u gnome-remote-desktop openssl req -new -newkey rsa:4096 -days 720 -nodes -x509 \
      -subj "/CN=$(hostname)" -out "$GRD_DIR/rdp-tls.crt" -keyout "$GRD_DIR/rdp-tls.key"
    run grdctl --system rdp set-tls-cert "$GRD_DIR/rdp-tls.crt"
    run grdctl --system rdp set-tls-key "$GRD_DIR/rdp-tls.key"
    changed "created a self-signed RDP certificate"
  fi
  # Settings → Remote Login has been seen to not store these; grdctl does. Called without arguments it
  # prompts, so the password never appears on a command line.
  local user; user=$(grd_status | sed -n 's/^[[:space:]]*Username: //p')
  local ask
  case "$user" in
    ""|"(null)"|"(empty)") ask=1 ;;
    *) ask=0; confirm "The RDP login is set (user '$user'). Replace it?" && ask=1 ;;
  esac
  if [ "$ask" = 1 ]; then
    say "Set the RDP login: one shared gateway login for this box, separate from the Ubuntu accounts."
    run grdctl --system rdp set-credentials
    changed "set the RDP credentials"
  fi
  if [ "$DRY" = 1 ] || ! grd_status | grep -q 'Status: enabled' || ! systemctl is-active -q gnome-remote-desktop.service; then
    run grdctl --system rdp enable
    run systemctl enable --now gnome-remote-desktop.service
    changed "Remote Login enabled on port 3389"
  fi
  # Picks up a new cert or login. Only when something changed: a restart drops everyone connected.
  [ ${#CHANGED[@]} -gt "$before" ] && run systemctl restart gnome-remote-desktop.service
  return 0
}

# The box's own monitor is in a room people walk into: lock it when idle. A default, so a user can
# still change it for their own account.
console_lock() {
  local want='[org/gnome/desktop/screensaver]
lock-enabled=true

[org/gnome/desktop/session]
idle-delay=uint32 300'
  if [ ! -f /etc/dconf/profile/user ]; then
    [ "$DRY" = 1 ] || printf 'user-db:user\nsystem-db:local\n' > /etc/dconf/profile/user
    changed "added the dconf system profile"
  fi
  if [ "$(cat "$DCONF_FILE" 2>/dev/null)" != "$want" ]; then
    run install -d /etc/dconf/db/local.d
    [ "$DRY" = 1 ] || printf '%s\n' "$want" > "$DCONF_FILE"
    run dconf update
    changed "screen locks after 5 idle minutes (default for all users)"
  fi
}

shared_user() {
  [ -n "$SHARED_USER" ] || return 0
  if ! id "$SHARED_USER" >/dev/null 2>&1; then
    confirm "Create the Ubuntu account '$SHARED_USER'?" || die "no shared account"
    run adduser "$SHARED_USER"
    changed "created account $SHARED_USER"
  fi
  local home; home=$(getent passwd "$SHARED_USER" | cut -d: -f6)
  local browser="" d
  for d in brave-browser google-chrome chromium_chromium chromium; do
    if [ -f "/usr/share/applications/$d.desktop" ] || [ -f "/var/lib/snapd/desktop/applications/$d.desktop" ]; then
      browser=$d.desktop; break
    fi
  done
  if [ -z "$browser" ]; then
    say "WARNING: no Brave/Chrome/Chromium installed. Cecelia's viewer needs a Chromium-based browser."
  elif ! grep -qs "^x-scheme-handler/http=$browser" "$home/.config/mimeapps.list"; then
    # app.py opens the sign-in link with the default browser, so that browser must do WebGPU.
    run sudo -H -u "$SHARED_USER" xdg-mime default "$browser" x-scheme-handler/http x-scheme-handler/https text/html
    changed "$browser is $SHARED_USER's default browser"
  fi
  if [ "$DRY" = 1 ] || [ ! -d "$home/.local/share/cecelia" ]; then
    say "next: if $SHARED_USER has no Cecelia yet, log in as them and run install.sh"
  fi
}

ssh_admin() {
  [ -n "$SSH_ADMIN" ] || return 0
  local home; home=$(getent passwd "$SSH_ADMIN" | cut -d: -f6) || die "no account '$SSH_ADMIN'"
  [ -s "$home/.ssh/authorized_keys" ] || die "$SSH_ADMIN has no SSH key in ~/.ssh/authorized_keys; add one first or SSH would lock them out"
  local want="# Written by scripts/remote-box/setup-box.sh: SSH for the admin only, keys only.
AllowUsers $SSH_ADMIN
PasswordAuthentication no
KbdInteractiveAuthentication no"
  [ "$(cat "$SSHD_FILE" 2>/dev/null)" = "$want" ] && return 0
  confirm "Restrict SSH to '$SSH_ADMIN' with key login only? Keep this session open until you have tested a new login." || return 0
  [ "$DRY" = 1 ] || printf '%s\n' "$want" > "$SSHD_FILE"
  if [ "$DRY" = 0 ] && ! sshd -t; then rm -f "$SSHD_FILE"; die "sshd rejected the config; removed it"; fi
  run systemctl reload ssh
  changed "SSH: only $SSH_ADMIN, keys only"
}

preflight
remote_login
console_lock
shared_user
ssh_admin

echo
if [ ${#CHANGED[@]} -eq 0 ]; then say "nothing to change"; else say "${#CHANGED[@]} change(s):"; printf '  - %s\n' "${CHANGED[@]}"; fi
say "Connect with an RDP client (Windows Remote Desktop, Windows App on Mac, Remmina) to $(hostname -I | awk '{print $1}'):3389,"
say "log in, start Cecelia from the menu, and run 'pixi run remote-check' in that session."
say "If the RDP port is not reachable, the network in front of the box is blocking it: ask IT."
