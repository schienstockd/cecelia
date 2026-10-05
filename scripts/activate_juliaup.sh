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
unset _cecelia_juliaup
