#!/usr/bin/env sh
# Read-only health check for a lab GPU box reached by remote desktop. No sudo.
# Reports the GPU (and a driver update waiting for a reboot), Vulkan device, desktop session, browser, the remote-desktop service,
# and every Cecelia listener's bind address. Exits non-zero if any check FAILs.
#
# Run it INSIDE the remote session to see what that session gets. See docs/todo/REMOTE_BOX_PLAN.md.
set -u

FAILS=0
ok()   { printf '  ok    %s\n' "$*"; }
warn() { printf '  warn  %s\n' "$*"; }
fail() { printf '  FAIL  %s\n' "$*"; FAILS=$((FAILS + 1)); }
have() { command -v "$1" >/dev/null 2>&1; }

check_gpu() {
  echo "GPU"
  if ! have nvidia-smi; then fail "nvidia-smi missing"; return; fi
  if out=$(nvidia-smi --query-gpu=name,driver_version --format=csv,noheader 2>&1); then
    ok "$out"
  else
    case "$out" in
      # An automatic driver update replaced the libraries but the old kernel module is still loaded;
      # new GPU programs fail until the box is rebooted.
      *mismatch*) fail "driver updated but not yet loaded: reboot the box" ;;
      *) fail "no NVIDIA GPU visible: $(echo "$out" | head -n1)" ;;
    esac
  fi
  if grep -qs nvidia /var/run/reboot-required.pkgs; then
    warn "an NVIDIA update is waiting for a reboot"
  fi
}

check_vulkan() {
  echo "Vulkan"
  if ! have vulkaninfo; then warn "vulkaninfo not installed (vulkan-tools)"; return; fi
  devs=$(vulkaninfo --summary 2>/dev/null | sed -n 's/^[[:space:]]*deviceName[[:space:]]*=[[:space:]]*//p')
  [ -z "$devs" ] && { fail "no Vulkan devices"; return; }
  echo "$devs" | while IFS= read -r d; do printf '        %s\n' "$d"; done
  # llvmpipe/SwiftShader alone means software rendering: WebGPU would not use the GPU.
  echo "$devs" | grep -qiv -E 'llvmpipe|swiftshader' && ok "hardware Vulkan device present" \
    || fail "only software Vulkan (llvmpipe/SwiftShader)"
}

check_session() {
  echo "Session"
  ok "type=${XDG_SESSION_TYPE:-unset} desktop=${XDG_CURRENT_DESKTOP:-unset} display=${DISPLAY:-unset} wayland=${WAYLAND_DISPLAY:-unset}"
  # Probe the session's own platform. On Wayland, glxinfo goes through XWayland, whose GLX can report
  # llvmpipe while the session itself renders on the GPU, so ask EGL for the Wayland platform instead.
  if [ "${XDG_SESSION_TYPE:-}" = wayland ]; then
    probe=eglinfo
    r=$(eglinfo -B 2>/dev/null | sed -n '/^Wayland platform:/,/^$/s/^OpenGL core profile renderer: //p' | head -n1)
  else
    probe=glxinfo
    r=$(glxinfo -B 2>/dev/null | sed -n 's/^OpenGL renderer string: //p')
  fi
  if ! have "$probe"; then
    warn "$probe not installed (mesa-utils); session renderer unchecked"
    return
  fi
  case "$r" in
    "") warn "$probe could not open the display" ;;
    *llvmpipe*|*SwiftShader*|*softpipe*) fail "session renders in software: $r" ;;
    *) ok "session renderer ($probe): $r" ;;
  esac
}

check_browser() {
  echo "Browser (Chromium-based, for WebGPU)"
  browser=""
  for c in brave-browser google-chrome google-chrome-stable chromium chromium-browser; do
    have "$c" && { browser=$c; break; }
  done
  [ -n "$browser" ] && ok "$("$browser" --version 2>/dev/null)" || fail "no Brave/Chrome/Chromium on PATH"
  default=$(xdg-settings get default-web-browser 2>/dev/null || true)
  ok "default browser: ${default:-unknown}"
  warn "WebGPU status needs a look: open brave://gpu (or chrome://gpu) in this session (not scriptable)"
}

check_remote_service() {
  echo "Remote desktop"
  found=0
  for unit in gnome-remote-desktop.service nxserver.service xrdp.service; do
    for scope in --user --system; do
      st=$(systemctl "$scope" is-active "$unit" 2>/dev/null) || true
      [ "$st" = active ] && { ok "$unit ($scope) active"; found=1; }
    done
  done
  [ "$found" = 1 ] || fail "no remote-desktop service active (gnome-remote-desktop / nxserver / xrdp)"
}

# Matched by owning process, not port number: every Cecelia service is a julia, node or python process
# of this user, and `ss -p` names only this user's processes, so other users' listeners are skipped.
check_listeners() {
  echo "Cecelia listeners (this user)"
  have ss || { warn "ss not available"; return; }
  n=0
  for addr in $(ss -ltnpH 2>/dev/null | awk '/users:\(\("(julia|node|python[0-9.]*)"/ {print $4}'); do
    host=${addr%:*}
    n=$((n + 1))
    case "$host" in
      127.0.0.1|\[::1\]|::1) ok "$addr (loopback)" ;;
      *) fail "$addr is reachable from the network; Cecelia must bind loopback only" ;;
    esac
  done
  [ "$n" -gt 0 ] || warn "no Cecelia port is listening (is Cecelia running?)"
}

check_gpu; check_vulkan; check_session; check_browser; check_remote_service; check_listeners
echo
if [ "$FAILS" -gt 0 ]; then echo "$FAILS check(s) failed"; exit 1; fi
echo "all checks passed"
