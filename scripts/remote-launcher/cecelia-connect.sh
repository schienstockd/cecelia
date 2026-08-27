#!/usr/bin/env sh
# Cecelia laptop launcher — opens an SSH tunnel to the VM described in
# ~/.cecelia/connection.json and points the default browser at the forwarded
# local port. Runs the first-run setup wizard if no profile exists.
#
# See docs/todo/REMOTE_ACCESS_PLAN.md for the full design.
set -eu

CONF_DIR="${CECELIA_LAUNCHER_HOME:-$HOME/.cecelia}"
CONF="$CONF_DIR/connection.json"
PID_FILE="$CONF_DIR/tunnel.pid"
LOG_FILE="$CONF_DIR/last-connect.log"
LOCAL_PORT_FILE="$CONF_DIR/tunnel.port"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WIZARD="$SCRIPT_DIR/cecelia-connect-setup.sh"

# First run: kick the wizard, which writes the profile.
if [ ! -f "$CONF" ]; then
  if [ -x "$WIZARD" ]; then
    "$WIZARD" || exit $?
  else
    printf '[cecelia] no profile at %s and no wizard at %s\n' "$CONF" "$WIZARD" >&2
    exit 1
  fi
fi
[ -f "$CONF" ] || exit 1

# Pull fields out of the JSON. Python3 first (universally available on macOS 12+ and every Linux),
# naive sed fallback for edge cases.
_readjson() {
  if command -v python3 >/dev/null 2>&1; then
    python3 -c "import json,sys; v=json.load(open(sys.argv[1])).get(sys.argv[2],''); print(v)" "$CONF" "$1"
  else
    sed -n 's/.*"'"$1"'"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p; s/.*"'"$1"'"[[:space:]]*:[[:space:]]*\([0-9][0-9]*\).*/\1/p' "$CONF" | head -n1
  fi
}

HOST=$(_readjson host)
CONN_USER=$(_readjson user)
LOCAL_PORT=$(_readjson localPort)
REMOTE_PORT=$(_readjson remotePort)
LOCAL_PORT="${LOCAL_PORT:-8080}"
REMOTE_PORT="${REMOTE_PORT:-8080}"

[ -n "$HOST" ] || { printf '[cecelia] host missing in %s — re-run setup\n' "$CONF" >&2; exit 1; }
[ -n "$CONN_USER" ] || { printf '[cecelia] user missing in %s — re-run setup\n' "$CONF" >&2; exit 1; }

# Portable TCP-open probe (Linux nc / BSD nc / bash /dev/tcp fallback).
_port_open() {
  if command -v nc >/dev/null 2>&1; then
    nc -z 127.0.0.1 "$1" >/dev/null 2>&1
  else
    ( : </dev/tcp/127.0.0.1/"$1" ) >/dev/null 2>&1
  fi
}

# Reuse an existing tunnel when the PID is alive AND the recorded port still answers.
TUNNEL_UP=0
REUSED_PORT=""
if [ -f "$PID_FILE" ] && [ -f "$LOCAL_PORT_FILE" ]; then
  _PID=$(cat "$PID_FILE" 2>/dev/null || true)
  _PORT=$(cat "$LOCAL_PORT_FILE" 2>/dev/null || true)
  if [ -n "$_PID" ] && kill -0 "$_PID" 2>/dev/null && [ -n "$_PORT" ] && _port_open "$_PORT"; then
    TUNNEL_UP=1
    REUSED_PORT="$_PORT"
  fi
fi

if [ "$TUNNEL_UP" = "0" ]; then
  mkdir -p "$CONF_DIR"

  # Walk upward from the requested localPort to find one that isn't already bound. 20 tries is
  # more than enough — anyone with 20 busy ports has other problems.
  ORIG_PORT="$LOCAL_PORT"
  ATTEMPT=0
  while _port_open "$LOCAL_PORT" && [ "$ATTEMPT" -lt 20 ]; do
    LOCAL_PORT=$((LOCAL_PORT + 1))
    ATTEMPT=$((ATTEMPT + 1))
  done
  if _port_open "$LOCAL_PORT"; then
    printf '[cecelia] no free local port near %s after 20 attempts\n' "$ORIG_PORT" >&2
    exit 1
  fi

  # ExitOnForwardFailure=yes → we notice a bind failure immediately.
  # ServerAliveInterval → drop a dozy tunnel promptly (rather than let it linger unusable).
  # BatchMode → refuse to prompt (any auth failure prints an error we can log).
  # StrictHostKeyChecking=accept-new → first-time acceptance, then pinning; safer than 'no'.
  : > "$LOG_FILE"
  ssh -f -N \
    -o ExitOnForwardFailure=yes \
    -o ServerAliveInterval=30 \
    -o ServerAliveCountMax=3 \
    -o BatchMode=yes \
    -o StrictHostKeyChecking=accept-new \
    -L "$LOCAL_PORT:localhost:$REMOTE_PORT" \
    "$CONN_USER@$HOST" >>"$LOG_FILE" 2>&1 || {
      printf '[cecelia] ssh failed. Log tail:\n' >&2
      tail -n 20 "$LOG_FILE" >&2 || true
      exit 1
    }

  # ssh -f detaches; recover its pid by matching the tunnel command line.
  SSH_PID=$(pgrep -f "ssh -f.* -L $LOCAL_PORT:localhost:$REMOTE_PORT $CONN_USER@$HOST" 2>/dev/null | head -n1 || true)
  if [ -z "$SSH_PID" ]; then
    # Fallback for pgrep pattern differences (BSD vs GNU).
    SSH_PID=$(pgrep -f "$LOCAL_PORT:localhost:$REMOTE_PORT" 2>/dev/null | head -n1 || true)
  fi
  [ -n "$SSH_PID" ] && printf '%s' "$SSH_PID" > "$PID_FILE"
  printf '%s' "$LOCAL_PORT" > "$LOCAL_PORT_FILE"

  # Wait for the forward to accept — up to 15s. Usually <1s over a warm SSH connection.
  WAITED=0
  while ! _port_open "$LOCAL_PORT" && [ "$WAITED" -lt 15 ]; do
    sleep 1; WAITED=$((WAITED + 1))
  done
  if ! _port_open "$LOCAL_PORT"; then
    printf '[cecelia] tunnel did not come up within 15s. Log: %s\n' "$LOG_FILE" >&2
    exit 1
  fi

  if [ "$ORIG_PORT" != "$LOCAL_PORT" ]; then
    printf '[cecelia] local port %s was busy; forwarded on %s\n' "$ORIG_PORT" "$LOCAL_PORT"
  fi
fi

ACTIVE_PORT="${REUSED_PORT:-$LOCAL_PORT}"
URL="http://localhost:$ACTIVE_PORT"

# Open the default browser. xdg-open on Linux, open on macOS, nothing on other Unixes.
if command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$URL" >/dev/null 2>&1 &
elif command -v open >/dev/null 2>&1; then
  open "$URL" &
else
  printf '[cecelia] point your browser at: %s\n' "$URL"
fi
