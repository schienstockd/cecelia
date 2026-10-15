# Sourced by pixi on every `pixi run` / `pixi shell` (see [target.unix.activation] in pixi.toml).
#
# A juliaup inside the install (`<root>/juliaup`) wins over the Julia on PATH. install.sh puts one
# there for system scope, and on Apple Silicon when the Julia it found was an Intel build. Without
# this, every task that runs bare `julia` (stop*, prod, doctor, julia-instantiate, …) starts the
# user's own Julia instead, which on that Mac means Intel under Rosetta. That juliaup keeps its own
# state, so JULIAUP_DEPOT_PATH points at it too.
#
# Same rule as `_find_julia` in app.py, which still applies it for a launch that bypasses pixi.
# A dev checkout has no `<root>/juliaup`, so this does nothing there. POSIX sh: pixi may source it
# from sh, bash or zsh.
_cecelia_juliaup="$PIXI_PROJECT_ROOT/juliaup"
if [ -x "$_cecelia_juliaup/bin/julia" ]; then
  export JULIAUP_DEPOT_PATH="$_cecelia_juliaup"
  case "$PATH" in
    "$_cecelia_juliaup/bin:"*) ;;
    *) export PATH="$_cecelia_juliaup/bin:$PATH" ;;
  esac
fi
# A system-scope install also carries a shared package depot, read-only to every account but the
# owner. Stack a per-user writable depot in front of it and keep Julia's bundled stdlib depot (the
# trailing empty entry), or Julia dies precompiling into the read-only one. Same shape as the
# launcher install.sh writes and `_find_julia` in app.py.
if [ -d "$_cecelia_juliaup/depot" ]; then
  export JULIA_DEPOT_PATH="$HOME/.cecelia/julia-depot:$_cecelia_juliaup/depot:"
fi
# Otherwise the Julia the installer recorded beats whichever `julia` PATH offers, as in `_find_julia` in
# app.py: it is the Julia `Pkg.instantiate` ran under, so `prod`, `doctor`, `julia-instantiate` must
# use it too, not just `app`. Its dir can't go first on PATH (it can be /opt/homebrew/bin, whose python
# would shadow the pixi env's), so a per-user dir holding only a `julia` symlink to it goes first. Keyed
# by the target's checksum, so installs recording different Julias never share one. If that dir can't
# be made, the recorded dir is appended, which still covers a desktop launch's minimal PATH.
if [ ! -x "$_cecelia_juliaup/bin/julia" ] && [ -f "$PIXI_PROJECT_ROOT/.cecelia-julia" ]; then
  _cecelia_julia="$(head -n 1 "$PIXI_PROJECT_ROOT/.cecelia-julia")"
  if [ -x "$_cecelia_julia" ] && [ "$(command -v julia 2>/dev/null)" != "$_cecelia_julia" ]; then
    _cecelia_shim="$HOME/.cecelia/julia-shim/$(printf '%s' "$_cecelia_julia" | cksum | cut -d ' ' -f 1)"
    case "$PATH" in
      "$_cecelia_shim:"*) ;;  # sourced again (nested `pixi shell`)
      *)
        if [ -L "$_cecelia_shim/julia" ] \
           || { mkdir -p "$_cecelia_shim" 2>/dev/null && ln -sf "$_cecelia_julia" "$_cecelia_shim/julia" 2>/dev/null; }; then
          export PATH="$_cecelia_shim:$PATH"
        elif ! command -v julia >/dev/null 2>&1; then
          export PATH="$PATH:${_cecelia_julia%/*}"
        fi ;;
    esac
    unset _cecelia_shim
  fi
  unset _cecelia_julia
fi
unset _cecelia_juliaup
