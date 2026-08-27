#!/usr/bin/env sh
# First-run setup wizard for the Cecelia laptop launcher.
#
# Prompts for the connection.json emitted by install.sh on the VM, validates it,
# probes SSH, and saves it at ~/.cecelia/connection.json. The launcher script
# uses it on every subsequent double-click.
#
# See docs/todo/REMOTE_ACCESS_PLAN.md.
set -eu

CONF_DIR="${CECELIA_LAUNCHER_HOME:-$HOME/.cecelia}"
CONF="$CONF_DIR/connection.json"
mkdir -p "$CONF_DIR"

printf 'Cecelia — first-run remote connection setup\n'
printf '===========================================\n\n'
printf 'Paste the connection JSON that "install.sh" printed on the VM, then\n'
printf 'signal end-of-input:  Ctrl-D on Linux/macOS, Ctrl-Z + Enter on Windows.\n\n'

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT
cat > "$TMP"

# Structural validation — must be JSON with required fields. Python3 is our floor
# (universally on Linux, in-box on macOS 12+); no fallback here since a bad paste is
# the most common wizard failure and a proper diagnostic is worth it.
if ! command -v python3 >/dev/null 2>&1; then
  printf '\n[cecelia] python3 not found on this laptop — install python3 (any recent version) and re-run.\n' >&2
  exit 1
fi

_validate=$(python3 - "$TMP" <<'PY'
import json, sys
try:
    d = json.load(open(sys.argv[1]))
except Exception as e:
    print(f"INVALID: not JSON ({e})")
    sys.exit(0)
missing = [k for k in ("host","user","localPort","remotePort") if k not in d]
if missing:
    print(f"INVALID: missing key(s): {', '.join(missing)}")
    sys.exit(0)
if not isinstance(d["host"], str) or not d["host"].strip():
    print("INVALID: host must be a non-empty string")
    sys.exit(0)
if not isinstance(d["user"], str) or not d["user"].strip():
    print("INVALID: user must be a non-empty string")
    sys.exit(0)
print(f"OK {d['user']} {d['host']}")
PY
)

case "$_validate" in
  INVALID:*)
    printf '\n[cecelia] %s\n' "$_validate" >&2
    printf '[cecelia] Copy the block that install.sh printed on the VM exactly (including the braces).\n' >&2
    exit 1
    ;;
esac

CONN_USER=$(printf '%s' "$_validate" | awk '{print $2}')
HOST=$(printf '%s' "$_validate" | awk '{print $3}')

printf '\n[cecelia] Probing SSH to %s@%s ...\n' "$CONN_USER" "$HOST"
if ssh -o BatchMode=yes \
       -o ConnectTimeout=5 \
       -o StrictHostKeyChecking=accept-new \
       "$CONN_USER@$HOST" echo cecelia-probe-ok >/dev/null 2>&1; then
  printf '[cecelia] SSH probe OK.\n'
else
  printf '\n[cecelia] SSH probe FAILED. Common causes:\n' >&2
  printf '  - Your SSH public key is not on the VM. Add it via your cloud provider console.\n' >&2
  printf '  - Firewall on the VM blocks TCP 22 from your laptop.\n' >&2
  printf '  - VPN or corporate network needed to reach the VM.\n' >&2
  printf 'Test the exact command manually to see the real error:\n' >&2
  printf '  ssh -o BatchMode=yes %s@%s echo ok\n' "$CONN_USER" "$HOST" >&2
  printf '\nThe profile has NOT been saved.\n' >&2
  exit 1
fi

cp "$TMP" "$CONF"
chmod 600 "$CONF" 2>/dev/null || true
printf '[cecelia] Connection saved to %s\n' "$CONF"
printf '[cecelia] From now on, launch Cecelia from your desktop shortcut.\n'
