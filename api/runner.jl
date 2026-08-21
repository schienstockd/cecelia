# Launch script for the detached task runner (`pixi run runner`, and the API server's own
# `runner_launch!`). Runs in the `api/` environment on purpose: the runner needs exactly what the API
# server needs — Cecelia, HTTP, JSON3 — and Cecelia is path-sourced by three environments whose
# Manifests must be re-resolved together (docs/ARCHITECTURE.md → *Adding a Julia dependency to `app/`*). A fourth
# would be maintenance cost for no isolation.
#
# Deliberately thin, and NOT Revise-tracked: the runner pins the code it started with and reports the
# commit on `/ping`, so "which code produced this result" is answerable rather than assumed. See
# docs/todo/TASK_RUNNER_PLAN.md (Decisions 5 and 6).

using Cecelia

init_cecelia!()
Cecelia.load_custom_modules!()   # a drop-in task must be runnable here too, or it runs nowhere

Cecelia.runner_serve(;
    port = parse(Int, get(ENV, "CECELIA_RUNNER_PORT", string(Cecelia.RUNNER_PORT))),
    host = get(ENV, "CECELIA_RUNNER_HOST", "127.0.0.1"))
