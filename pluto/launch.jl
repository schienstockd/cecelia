# Launch the persistent Pluto notebook server (the Playground engine).
#
#   pixi run notebooks                      # examples dir, opens a browser
#   CECELIA_NOTEBOOKS_DIR=<proj>/notebooks pixi run notebooks   # a project's notebooks
#
# Design (see docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md):
#   * Pluto runs its OWN Julia session (this `pluto/` env, which path-sources Cecelia) — NOT the API
#     server. Notebooks are a full analysis surface: init_object / pop_df / run_task / plot / export.
#   * A deps-only sysimage (pluto/deps.so, built by build_sysimage.jl) is passed to notebook WORKERS
#     via compiler_options if present — kills Makie's time-to-first-plot (Phase 0: 18.8s → 3.9s).
#     Optional: the server runs fine without it, just with a slow first plot.
#   * CECELIA_PLUTO_ENV is exported so a notebook's first cell can `Pkg.activate` this env (which
#     makes Pluto disable its own registered-only pkg manager and pick up the dev-tracked Cecelia).
import Pkg
Pkg.activate(@__DIR__)

using Pluto

const PLUTO_PORT = parse(Int, get(ENV, "CECELIA_PLUTO_PORT", "7660"))

# Notebooks directory: explicit env override, else the repo's shipped examples (../notebooks).
notebooks_dir = abspath(get(ENV, "CECELIA_NOTEBOOKS_DIR", joinpath(@__DIR__, "..", "notebooks")))
isdir(notebooks_dir) || mkpath(notebooks_dir)

# Export this env's path so notebooks activate it (workers inherit the server process's ENV).
ENV["CECELIA_PLUTO_ENV"] = @__DIR__

launch_browser = get(ENV, "CECELIA_PLUTO_BROWSER", "true") == "true"

# Deps-only sysimage → worker compiler options — but ONLY if it's FRESH. A sysimage left over from a
# previous Julia/package version is stale: at best slower, at worst a Julia-version mismatch that makes
# workers reject it. So we use it only when its stamp matches (sysimage_stamp.jl); otherwise fall back
# to a plain session (slow first plot) and let the app rebuild it. Never hand workers a stale image.
include(joinpath(@__DIR__, "sysimage_stamp.jl"))
sysimg = joinpath(@__DIR__, "deps.so")
compiler = if sysimage_fresh(@__DIR__)
    @info "Pluto workers will use the deps sysimage (fast first plot)" sysimg
    Pluto.Configuration.CompilerOptions(sysimage = sysimg)
else
    isfile(sysimg) ?
        @warn("Ignoring a stale pluto/deps.so (built for a different Julia/package set) — the app will rebuild it. First plot slow (~20s) until then.") :
        @warn("No pluto/deps.so — first plot in each notebook will be slow (~20s). Build it with: pixi run notebooks-sysimage")
    Pluto.Configuration.CompilerOptions()
end

opts = Pluto.Configuration.Options(
    server = Pluto.Configuration.ServerOptions(
        port = PLUTO_PORT,
        launch_browser = launch_browser,
        notebook_path_suggestion = notebooks_dir * "/",
        dismiss_update_notification = true,
        # Reflect on-disk changes live — so a "Restore snapshot" from the app (which overwrites the
        # .jl) shows up in an open notebook without a manual reload.
        auto_reload_from_file = true,
    ),
    # Pluto's secret protection stays ON (its secure default). Pluto is a code-execution surface
    # reachable from the browser, so without the secret ANY website you visit could drive
    # localhost:7660 and run arbitrary code (CSRF/RCE) — that's why we do NOT disable it. Instead we
    # publish the session secret to a runtime file (below) so the API server can build authorized URLs
    # (…/?secret=… and …/open?path=…&secret=…).
    compiler = compiler,
    evaluation = Pluto.Configuration.EvaluationOptions(workspace_use_distributed = true),
)

session = Pluto.ServerSession(; options = opts)
# Publish the secret for the API server to read (git-ignored). Written before run() (which blocks and
# brings the port up), so by the time the port answers the secret is already available.
write(joinpath(@__DIR__, ".plutosecret"), session.secret)

@info "Starting Pluto Playground" port = PLUTO_PORT notebooks_dir env = @__DIR__
Pluto.run(session)
