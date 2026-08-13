# ── py_runner.jl — the single place Cecelia spawns Python subprocesses ────────────
#
# Every Python task runner and data-layer writer is launched through `run_py`. It sets
# PYTHONPATH to python/ so the scripts resolve `import cecelia.*` directly (no per-script sys.path
# bootstrapping), and centralises the params-file + stdout-stream + exit-check plumbing that used
# to be copy-pasted into every task. (bioformats2raw — a non-Python binary — is still spawned
# directly in importImages/omezarr.jl; this helper is Python-only.)
using JSON3

_app_dir() = dirname(@__DIR__)                       # app/src → app/
# The Python helper package (`cecelia`) is a top-level sibling of app/ — repo-root/python/ — not
# nested under the Julia app. Scripts are invoked by file path here; the editable install (see
# python/pyproject.toml + the pixi `cecelia` dep) is what makes `import cecelia.*` resolve inside them.
_python_dir() = joinpath(dirname(_app_dir()), "python")   # app/ → repo-root/python/

# Version of the JULIA↔PYTHON PARAMS CONTRACT, mirroring `script_utils.CONTRACT_VERSION`. Travels to
# every runner as `CECELIA_PY_CONTRACT` (see `run_py`) and is checked in `script_params`, so the guard
# costs nothing per task and cannot be forgotten by a new runner.
#
# What it protects: a runner is spawned FRESH from disk every run, while the Julia process that builds
# its params can be stale (Revise does not always reload `app/src` after a branch switch or a merge
# under a live server). So the two halves can disagree about the params' shape with nothing to say so.
#
# BUMP THIS whenever the contract changes in a way a stale caller would get wrong: a param key renamed
# or removed, a value's type or units changed, a new REQUIRED key. Not for additive optional keys — an
# older caller omitting one is handled by the runner's own default. Asserted equal to the Python side by
# the "language boundaries agree on their protocol" testset.
#
# 1: the contract as of the guard's introduction — the state after `divisionChannels` →
#    `competingChannels` and `quotientChannel` → `targetChannel` (the rename that motivated it).
const PY_CONTRACT_VERSION = 1

"""
    task_run_dir(base_dir) -> String

The consistent on-disk home for a run's params JSON: `<base_dir>/<conf dirs.tasks>` (default
`tasks`). `base_dir` is an object's `_dir` — `img._dir` for image-scope tasks, the set/project dir
for set-scope. Every `run_py` caller resolves its `config_dir` through this, so run configs always
land under the project tree (never a temp dir)."""
function task_run_dir(base_dir::AbstractString)::String
    sub = get(get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict()), "tasks", "tasks")
    joinpath(base_dir, string(sub))
end

# The user modules root (`<config_dir>/modules`) — put on PYTHONPATH for run_py so a custom
# (drop-in) task's `_run.py` can import across the modules tree. (A runner's OWN category dir is
# already on `sys.path[0]` because it's launched by absolute path, so co-located siblings import
# without this; the root just makes the wider tree reachable.) Standalone (NOT inlined in run_py) on
# purpose: run_py's `task_dir` param must never be able to shadow the `config_dir()` function again —
# that shadow silently made `config_dir()` call the task-dir string, breaking EVERY Python task.
_custom_modules_pydir()::String = joinpath(config_dir(), "modules")

# ── The BLAS thread budget every Python task inherits ─────────────────────────────────────────────
#
# A numpy/scipy call that lands in BLAS takes ALL cores by default, and every runner holds one
# resource-pool slot — so `n` concurrent tasks ask for `n × cores`. This is the ONLY layer that can
# fix that: `OPENBLAS_NUM_THREADS` is read when the child imports numpy, so nothing inside Python can
# set it after the fact (a `threadpoolctl` context manager only bounds the pools already LOADED when
# it is entered, which is a real hole — clustering loads a second BLAS after the first is capped).
#
# Bounded is the right DEFAULT, not a special case, because uncapped is not neutral — measured on a
# 32-core box, nothing gets faster with all cores and one thing gets dramatically slower:
#
#   drift estimation (kSUFux/mkh3Tu)   56.3 s @32  ->  31.8 s @4     (309.7 s -> 70.7 s, 4 at once)
#   scanpy neighbors+leiden+umap       44.6 s @32  ->  44.0 s @4     (flat — not BLAS-bound)
#   spatial KDTree neighbour graph      0.14 s      ->   0.14 s      (flat — not BLAS at all)
#   dense SVD 20000x400                 2.30 s @32  ->   1.28 s @4
#   dense GEMM 4000^2                   1.04 s @32  ->   0.91 s @4   (memory-bound at this size)
#
# **`OPENBLAS_NUM_THREADS` only — deliberately NOT `OMP_NUM_THREADS`.** That one also throttles
# torch's intra-op parallelism, and torch on CPU is the one workload measured that genuinely wants
# the cores: a cellpose-shaped conv stack goes 0.19 s -> 0.34 s at 4 threads. Capping OpenBLAS alone
# leaves torch untouched (0.22 s, 24 threads) while still giving drift the full win.
#
# A task that has MEASURED a need for more raises it locally via
# `cecelia.utils.cpu_utils.limit_blas_threads`. See `docs/SCHEDULER.md` → *Thread budgets*.
const BLAS_THREADS_PER_TASK = 4

# The environment every Python task runs under, as pairs. Standalone so it can be ASSERTED rather
# than grepped for — `run_py` builds and spawns in one call, so there is otherwise no seam to test
# what a task actually inherits, and "is OMP_NUM_THREADS absent?" cannot be answered by reading the
# source (the comment above names it).
_py_task_env(pythonpath::AbstractString) = [
    "PYTHONPATH" => String(pythonpath),
    "CECELIA_IMAGE_COMPRESSOR" => image_compressor(),
    "CECELIA_PY_CONTRACT" => string(PY_CONTRACT_VERSION),
    "OPENBLAS_NUM_THREADS" => string(BLAS_THREADS_PER_TASK),
]

"""
    run_py(script_rel, params, task_dir; on_log, on_progress, on_process) -> Bool

Run `app/src/<script_rel>` (a task runner co-located with its `.jl`) as a subprocess with a JSON `params` file written to `task_dir` (the
run's task dir — see `task_run_dir`; never a temp dir) and passed via `--params`, which the script
reads then deletes (so a clean run leaves nothing behind; a crashed one leaves the params for
inspection — matching the legacy behaviour). Streams stdout/stderr line-by-line: `[PROGRESS] n/total`
lines go to `on_progress(n, total)`, the rest to `on_log`. Registers the process with `on_process`
(so `task:cancel` can kill it) and returns `true` only on a clean exit (`exitcode == 0 &&
termsignal == 0` — libuv reports 0 exitcode for signal-killed procs, so both are checked). PYTHONPATH
is set to python/ so the script can `import cecelia.*` with no sys.path manipulation. This is the one place
Cecelia spawns a Python subprocess — the Julia analogue of the old R `self\$pyScript(name, params)`.
"""
function run_py(script_rel::AbstractString, params, task_dir::AbstractString;
                on_log::Function      = line -> println(line),
                on_progress::Function = (n, t) -> nothing,
                on_process::Function  = _ -> nothing)::Bool
    py_root   = _python_dir()
    # Resolve the script:
    #  • absolute  → a custom (user drop-in) task's own `_run.py`.
    #  • "tasks/…" → a built-in TASK RUNNER, co-located with its `.jl` under app/src/tasks/<cat>/.
    #  • otherwise → a library script in the installable `cecelia` package (e.g. `writers/…`, the
    #    h5ad write-side counterpart to the readers).
    # The `cecelia` package (python/) is the IO library — it holds NO task runners; PYTHONPATH still
    # points there so every runner `import cecelia.*`.
    py_script = if isabspath(String(script_rel))
        String(script_rel)
    elseif startswith(String(script_rel), "tasks/")
        joinpath(_app_dir(), "src", script_rel)
    else
        joinpath(py_root, "cecelia", script_rel)
    end
    isfile(py_script) || (on_log("[ERROR] Python script not found: $py_script"); return false)

    mkpath(task_dir)
    stem        = splitext(basename(String(script_rel)))[1]
    params_file = joinpath(task_dir, "$stem.$(string(rand(UInt32); base = 16)).params.json")
    open(params_file, "w") do io
        JSON3.write(io, params)
    end
    out_pipe = Pipe()
    # PYTHONPATH: python/ (so `import cecelia.*` resolves everywhere) + the user modules python dir
    # (so a custom task's dropped `_run.py` can import its own siblings). See docs/CUSTOM_MODULES.md.
    pythonpath = py_root
    custom_py  = _custom_modules_pydir()
    isdir(custom_py) && (pythonpath = string(custom_py, Sys.iswindows() ? ";" : ":", py_root))
    # The configured image-store compressor travels as an env var rather than a param, so every
    # runner's zarr write picks it up through `zarr_utils.store_compressor` without each task having
    # to declare and forward it. Read per call on the Python side, so flipping the Settings choice
    # applies to the next task with no restart.
    # The params CONTRACT version, so a runner can refuse a params file written by a backend running
    # older code than the Python it is calling. `app/src` is Revise-tracked and a branch switch or a
    # merge under a live server does not always reload it, so the Julia half can be a version behind
    # while every runner is loaded fresh from disk. That has bitten once already: a param rename landed
    # on disk, the running backend kept the previous translator, and the runner died with
    # `invalid literal for int() with base 10: 'CH3'` — naming neither the cause nor the fix.
    #
    # An ENV VAR rather than a params field, on purpose: the params payload stays exactly the shape each
    # runner documents, and a developer replaying a saved params file by hand simply has no variable set,
    # which `script_utils` treats as "skip the check" rather than as a failure.
    cmd  = addenv(`$(python_bin_path()) $py_script --params $params_file`, _py_task_env(pythonpath)...)
    proc = run(pipeline(cmd; stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)
    for line in eachline(out_pipe)
        m = match(r"^\[PROGRESS\] (\d+)/(\d+)$", line)
        isnothing(m) ? on_log(line) : on_progress(parse(Int, m[1]), parse(Int, m[2]))
    end
    wait(proc)
    proc.exitcode == 0 && proc.termsignal == 0
end
