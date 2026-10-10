# The one `pixi` lookup for the API — the system-env install (system_api.jl) and the dev-channel
# update apply (update_api.jl) both shell out to pixi. `app.py:_find_pixi` is the launcher-side twin;
# keep the two orders in step.
#
# `Sys.which("pixi")` alone is not enough. The user-scope macOS `.app` runs under launchd's minimal
# PATH, and a `.desktop` launch or a shell without pixi on PATH arrives the same way. Every launcher
# starts the server through `pixi run app`, which exports `PIXI_EXE` — so that's checked first. The
# disk fallbacks mirror install.sh's locations: system scope in `<root>/pixi/bin`, user scope in
# `$PIXI_HOME/bin` or `~/.pixi/bin`. Returns "" when nothing is found. Windows: `pixi.exe`.

const _PIXI_APP_ROOT = abspath(joinpath(@__DIR__, "..", ".."))   # api/src → repo / install root

function _find_pixi(root::AbstractString = _PIXI_APP_ROOT)::String
    exe = Sys.iswindows() ? "pixi.exe" : "pixi"
    from_run = strip(get(ENV, "PIXI_EXE", ""))
    !isempty(from_run) && isfile(from_run) && return String(from_run)
    on_path = Sys.which("pixi")
    on_path === nothing || return String(on_path)
    for cand in (
        joinpath(root, "pixi", "bin", exe),           # system-scope install
        get(ENV, "PIXI_HOME", "") |> h -> isempty(h) ? "" : joinpath(h, "bin", exe),
        joinpath(expand_user("~/.pixi"), "bin", exe), # user-scope default
    )
        !isempty(cand) && isfile(cand) && return cand
    end
    ""
end
