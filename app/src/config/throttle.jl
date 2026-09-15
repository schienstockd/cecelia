# CPU / worker throttle + runner toggle. Split out of config.jl.
# Names live in the top-level `Cecelia` module; included from config.jl after the bootstrap.

tasks_concurrent_limit()::Int =
    Int(get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "concurrentLimit", 4))

# How many threads ONE task may run its own work on (`CECELIA_TASK_WORKERS`; see `py_runner.jl` and
# docs/SCHEDULER.md → *Thread budgets*). `[tasks].workerThreads`, default derived from the box.
#
# The default divides the cores by how many tasks are expected to be COMPUTING at once, which is not
# the `cpu` pool limit: that limit is 20, and sizing for it would leave a lone task on one core of
# thirty-two. Four matches the assumption `BLAS_THREADS_PER_TASK` was chosen under. Capped at 16
# because nothing measured here keeps scaling past that, and a machine with hundreds of cores is
# more likely running many tasks than one very wide one.
const _TASK_WORKERS_ASSUMED_ACTIVE = 4
const _TASK_WORKERS_MAX_DEFAULT = 16
# A floor: the divisor alone turns a small machine serial (4 cores / 4 = 1 thread), and a 4-core
# laptop is not the one running four heavy tasks at once. Never wider than the box either.
const _TASK_WORKERS_MIN_DEFAULT = 2

# ── How many CPUs this PROCESS may actually use ───────────────────────────────────────────────────
#
# `Sys.CPU_THREADS` counts the MACHINE, which is the wrong number the moment the process is confined:
# a PBS/Slurm job with four cores of a 128-core node, or a container run with `--cpus`. Sizing a
# budget from the box then hands out threads (and, since `LOKY_MAX_CPU_COUNT`, worker PROCESSES) for
# hardware the process cannot touch — the scheduler is not fooled, it just thrashes.
#
# Two limits, both Linux, both cheap to read, and the smallest wins along with the machine count:
#   • the affinity mask (`taskset`, a cluster's cpuset) — `/proc/self/status: Cpus_allowed_list`
#   • the cgroup-v2 CPU quota (`--cpus`, a systemd slice) — `/sys/fs/cgroup/cpu.max`
# Elsewhere (macOS, Windows) the machine count is the best available and is what we use, which is no
# worse than before.
#
# The parsers are pure and separately tested: the file reads cannot be exercised on a box that has no
# limits, and "we would parse a cluster's mask correctly" is exactly the claim worth pinning.

"""
    cpus_from_affinity_list(s) -> Union{Int,Nothing}

`"0-3,8,12-15"` → `8`. `nothing` when the line is absent or unparseable — an unreadable limit must not
read as a limit of zero.
"""
function cpus_from_affinity_list(s::AbstractString)::Union{Int,Nothing}
    n = 0
    for part in split(strip(s), ',')
        isempty(part) && continue
        if occursin('-', part)
            lo, hi = split(part, '-')
            a = tryparse(Int, lo); b = tryparse(Int, hi)
            (isnothing(a) || isnothing(b) || b < a) && return nothing
            n += b - a + 1
        else
            isnothing(tryparse(Int, part)) && return nothing
            n += 1
        end
    end
    n > 0 ? n : nothing
end

"""
    cpus_from_cgroup_max(s) -> Union{Int,Nothing}

cgroup v2 `cpu.max` is `"<quota> <period>"` in microseconds: `"400000 100000"` → 4 CPUs. `"max ..."`
means no quota → `nothing`. Rounded UP, and never below 1: a 0.5-CPU quota still gets one worker,
because zero would mean the task cannot run at all.
"""
function cpus_from_cgroup_max(s::AbstractString)::Union{Int,Nothing}
    parts = split(strip(s))
    length(parts) == 2 || return nothing
    parts[1] == "max" && return nothing
    quota = tryparse(Int, parts[1]); period = tryparse(Int, parts[2])
    (isnothing(quota) || isnothing(period) || period <= 0 || quota <= 0) && return nothing
    max(1, cld(quota, period))
end

_read_limit(path, parse) = try
    isfile(path) ? parse(read(path, String)) : nothing
catch
    nothing
end

"""
    usable_cpus() -> Int

Logical CPUs this process may use: the machine count, narrowed by the affinity mask and the cgroup
quota where those exist. Always at least 1. See the block comment above for why the machine count
alone is not it.
"""
function usable_cpus()::Int
    n = max(Sys.CPU_THREADS, 1)
    if Sys.islinux()
        aff = _read_limit("/proc/self/status", s -> begin
            m = match(r"Cpus_allowed_list:\s*(\S+)", s)
            isnothing(m) ? nothing : cpus_from_affinity_list(m.captures[1])
        end)
        isnothing(aff) || (n = min(n, aff))
        cg = _read_limit("/sys/fs/cgroup/cpu.max", cpus_from_cgroup_max)
        isnothing(cg) || (n = min(n, cg))
    end
    max(n, 1)
end

default_task_worker_threads()::Int =
    let cpus = usable_cpus()
        min(clamp(cpus ÷ _TASK_WORKERS_ASSUMED_ACTIVE,
                  _TASK_WORKERS_MIN_DEFAULT, _TASK_WORKERS_MAX_DEFAULT),
            cpus)
    end

function task_worker_threads()::Int
    conf = get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "workerThreads", nothing)
    n = isnothing(conf) ? 0 : try Int(conf) catch; 0 end
    # A typo in a config file is a slow run, not a failed one — every value here is a perf choice.
    n >= 1 ? n : default_task_worker_threads()
end

# The ceiling the throttle offers: the CPUs this process may actually use, not a round number. It was
# a flat 64 on the reasoning that a task may want more threads than cores when it is I/O-bound between
# them — which stopped being true when the same number began capping joblib's worker PROCESSES
# (`LOKY_MAX_CPU_COUNT`, py_runner.jl). 64 processes on 32 cores is not a perf choice, it is
# contention, and offering it invites exactly the setting nobody wants.
#
# A function, not a const: the answer depends on an affinity mask and a cgroup quota, and a value
# frozen at load time would be the machine's rather than this process's.
task_workers_max()::Int = usable_cpus()

"""
    set_task_worker_threads!(n) -> Int

Persist `[tasks].workerThreads` in the user's `custom.toml` and hot-reload, so the NEXT task spawns
with it. `n <= 0` REMOVES the key, which is how you get back to the machine-derived default rather
than a number that merely happens to equal it today — the derived value follows the box, a written
one does not. Returns the value now in effect.

Mirrors `set_pool_limit!` (merged write, so unrelated keys survive) with one difference that matters:
a pool resize takes effect on the next *admission*, whereas this reaches a task through an env var
`run_py` sets at SPAWN time, so a task already running keeps the threads it started with. Nothing can
change that — the variable is read when the child imports numpy.
"""
function set_task_worker_threads!(n::Integer)::Int
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    tasks = get(cfg, "tasks", Dict{String,Any}())
    if n <= 0
        delete!(tasks, "workerThreads")
    else
        tasks["workerThreads"] = clamp(Int(n), 1, task_workers_max())
    end
    # An empty `[tasks]` table left behind reads as a setting that exists and is blank.
    isempty(tasks) ? delete!(cfg, "tasks") : (cfg["tasks"] = tasks)
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    task_worker_threads()
end

# ── May a LINEARLY-SCALING stage take the whole box? ──────────────────────────────────────────────
#
# `task_worker_threads()` is one number, and the stages it governs disagree about what they want.
# Measured on zolIMa/fXgbTl, 32 z-planes at production geometry (docs/SCHEDULER.md → *Three stages,
# three curves*): coastal's flow metrics keep scaling to the box (14x at 32 threads, 6.8x at 8),
# while its region growing DEGRADES past 4. `cap=` already handles the second — an algorithmic
# ceiling belongs next to its measurement. The first had no expression at all: there was no way for
# a stage to say "take the machine when nobody else is using it", which the plan identified as the
# case that costs the most (~2x unclaimed on a lone run).
#
# Two conditions, both required, because either alone is wrong:
#
#   1. The budget must be DERIVED. An explicit `[tasks].workerThreads` is the user saying how wide a
#      task may go, and a stage widening past it would make the slider a suggestion.
#   2. This flag must be on. Default OFF — and NOT because widening is unproven, but because it
#      interacts with the pool limits: `cpu` admits several tasks at once, so a linear stage in each
#      of them would oversubscribe the box. That is a judgement about how a machine is shared, which
#      is the user's to make, not a default to infer.
#
# Delivered to Python as `CECELIA_TASK_WORKERS_WIDEN` + `CECELIA_USABLE_CPUS` (py_runner.jl). The
# usable count is computed HERE rather than re-derived in Python: `usable_cpus()` already reads the
# affinity mask and the cgroup quota and is tested, and a second implementation of that is exactly
# the drift this codebase keeps one canonical helper to avoid.
task_workers_widen()::Bool =
    Bool(get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "widenLinearStages", false))

"""
    task_workers_derived() -> Bool

Whether the budget in effect came from the MACHINE rather than from `[tasks].workerThreads`. The
condition `task_workers_widen()` is gated on — see the block comment above. Mirrors the `derived`
field the threads API already reports, so the UI and the env var cannot disagree about it.
"""
function task_workers_derived()::Bool
    conf = get(get(cecelia_conf(), "tasks", Dict{String,Any}()), "workerThreads", nothing)
    isnothing(conf) && return true
    n = try Int(conf) catch; 0 end
    n < 1                     # a typo falls back to the derived default, so it IS derived
end

"""
    set_task_workers_widen!(on) -> Bool

Persist `[tasks].widenLinearStages` and hot-reload, so the NEXT task spawns with it. Same shape as
`set_task_worker_threads!`, including that a task already running keeps what it started with — the
value reaches a task as an env var read at spawn.
"""
function set_task_workers_widen!(on::Bool)::Bool
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    tasks = get(cfg, "tasks", Dict{String,Any}())
    # Removed rather than written `false`: the default is the absence of the key, and a written
    # `false` is indistinguishable from it while looking like a decision somebody made.
    on ? (tasks["widenLinearStages"] = true) : delete!(tasks, "widenLinearStages")
    isempty(tasks) ? delete!(cfg, "tasks") : (cfg["tasks"] = tasks)
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    task_workers_widen()
end

# ── The detached task runner (Settings → System) ──────────────────────────────────────────────────
#
# Whether tasks execute in a SEPARATE process, so restarting the backend does not kill work in flight.
# `[runner].enabled`, default **false** while the design has known gaps (no on-disk spool; chains and
# background jobs still run in-process — docs/todo/TASK_RUNNER_PLAN.md).
#
# A SETTING, not just an env var, because "opt-in" has to mean something to a user. It began as
# `CECELIA_RUNNER=1`, which exists only as a pixi task — a packaged install had no way to turn it on
# at all, so what was called opt-in was really "dev-only". The env var is kept as an OVERRIDE (it
# wins, and is how `pixi run dev-runner` and CI ask for it without touching a config file); the
# setting is what a user actually has.
_env_flag(name) = lowercase(strip(get(ENV, name, ""))) in ("1", "true", "yes", "on")

"""
    is_dev_session() -> Bool

Whether this is a development run (`pixi run dev` sets `CECELIA_DEV`; `prod`, `app.py` and the
packaged launcher never do). Mirrors the API layer's `_is_dev`, in the package so the runner and its
gating can read it too.
"""
is_dev_session()::Bool = _env_flag("CECELIA_DEV")

"""
    runner_enabled() -> Bool

**Dev only.** A production install has no Restart button, so the runner's whole benefit — a backend
restart not costing a running task — is unreachable there, while every one of its failure modes
(an idle process with no window, no cancel, nothing to find it by) lands squarely on the user. And a
prod user does not need it: they leave the app running, and closing the browser tab was never what
stopped a task.

So this is deliberately not a thing a user can switch on. The process side of "quit and keep
processing" (`detach = true`) is already built if that ever becomes something someone asks for —
see docs/todo/TASK_RUNNER_PLAN.md → Decision 3b — but it is not built on a guess.
"""
function runner_enabled()::Bool
    is_dev_session() || return false
    haskey(ENV, "CECELIA_RUNNER") && return _env_flag("CECELIA_RUNNER")
    Bool(get(get(cecelia_conf(), "runner", Dict{String,Any}()), "enabled", false))
end

"""
    set_runner_enabled!(on) -> Bool

Persist `[runner].enabled` and hot-reload config. Mirrors `set_image_compressor!` — merged write, so
an unrelated key in `custom.toml` survives.

Returns the value now in EFFECT, which is not always what was asked: `CECELIA_RUNNER` overrides the
file, so a dev session started with the env var reports `true` however the toggle is set. Returning
the effective value rather than the written one is what lets the UI show the truth instead of the
request.
"""
function set_runner_enabled!(on::Bool)::Bool
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    r   = get(cfg, "runner", Dict{String,Any}())
    r["enabled"] = on
    cfg["runner"] = r
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    runner_enabled()
end
