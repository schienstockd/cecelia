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
    cmd  = addenv(`$(python_bin_path()) $py_script --params $params_file`, "PYTHONPATH" => pythonpath)
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
