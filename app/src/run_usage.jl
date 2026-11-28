# ── run_usage.jl — what a run USED, gathered with no per-task code ───────────────────────────────
#
# The run-log entry's `usage` (run_log.jl → *What made it*): peak memory, host and — on a cluster —
# the scheduler's own accounting, so the task history can set memory used beside memory requested and
# a spec's `hpc.mem` model can be checked against real runs.
#
# Plumbing. The scheduler binds one `RunUsage` accumulator per job around `_run_task`
# (`with_run_usage`, scheduler/jobs.jl). Whatever runs inside reports into it through
# `record_run_usage!` — `run_py` after each local Python process, the HPC dispatch after each cluster
# job — and `close_run_log!` stamps the total. A `ScopedValue`, so a task's own `Threads.@spawn`
# children report into the same run, and a `run_py` outside any scheduled run (the REPL, a test)
# records nothing.
#
# How the local peak is measured: the Python runner writes it itself at exit (`script_utils.
# register_usage_report`, called from `script_params`, which every runner calls) to the file `run_py`
# names in `CECELIA_USAGE_OUT`. The child is the only side that can read its own high-water mark on all
# three platforms (`getrusage` on Linux/macOS, the peak working set on Windows); libuv reaps the
# process itself, so Julia never gets its rusage, and polling from outside would miss a short peak.
#
# The `usage` dict. Units are in the key names; a key is ABSENT when unknown, never guessed:
#   source         "local" | "slurm" | "pbs" — where the work ran
#   host           the machine a local run ran on (`gethostname`); absent on a cluster
#   peakRssBytes   peak resident memory of the largest single process: a local runner (or the largest
#                  child it waited for), or a cluster job's MaxRSS / resources_used.mem
#   runs           how many Python runs (local processes or cluster jobs) reported figures
#   reqMemBytes    memory booked (cluster)
#   wallSeconds    the jobs' summed walltime (cluster; a local run's is `at`→`finishedAt`)
#   reqWallSeconds the largest walltime booked (cluster; absent = no limit)
#   cpus           the largest CPU count booked (cluster)
#   cpuSeconds     CPU time summed over the jobs (cluster) — ÷ wallSeconds = CPUs busy on average
#   gpuUtilPct     GPU utilisation, the highest of the jobs (cluster, where the site reports it)
#   final          false when a scheduler had not closed a job's accounting yet (cluster)
# Several reports in one run merge by `merge_run_usage!`: maxima for peaks and bookings, sums for
# times and `runs`. A set-scope run stamps the same total on every image it ran over.

"""The per-run usage accumulator `with_run_usage` binds. Use `run_usage(acc)` to read it."""
struct RunUsage
    lock::ReentrantLock
    d::Dict{String,Any}
end
RunUsage() = RunUsage(ReentrantLock(), Dict{String,Any}())

const RUN_USAGE = Base.ScopedValues.ScopedValue{Union{Nothing,RunUsage}}(nothing)

const _USAGE_MAX_KEYS = ("peakRssBytes", "reqMemBytes", "reqWallSeconds", "cpus", "gpuUtilPct")
const _USAGE_SUM_KEYS = ("runs", "wallSeconds", "cpuSeconds")

"""
    merge_run_usage!(acc::Dict, u) -> acc

Fold one report `u` into `acc`: the maximum for peaks and bookings, the sum for times and `runs`,
`final` true only while every report was final, and the first `source`/`host` seen (a task's runs all
go to one target — HPC dispatch is task-wide). Pure apart from `acc`.
"""
function merge_run_usage!(acc::Dict{String,Any}, u::AbstractDict)::Dict{String,Any}
    for (k0, v) in u
        k = String(k0)
        v === nothing && continue
        if k in _USAGE_MAX_KEYS
            acc[k] = haskey(acc, k) ? max(acc[k], v) : v
        elseif k in _USAGE_SUM_KEYS
            acc[k] = get(acc, k, zero(v)) + v
        elseif k == "final"
            acc[k] = get(acc, k, true) && v === true
        else
            haskey(acc, k) || (acc[k] = v)
        end
    end
    acc
end

"""
    record_run_usage!(u) -> Bool

Add one report to the run being executed (`RUN_USAGE`); `false` when no run is bound (a REPL `run_py`).
"""
function record_run_usage!(u::AbstractDict)::Bool
    acc = RUN_USAGE[]
    (acc === nothing || isempty(u)) && return false
    lock(acc.lock) do
        merge_run_usage!(acc.d, u)
    end
    true
end

"""A copy of what `acc` gathered — `{}` when nothing reported."""
run_usage(acc::RunUsage)::Dict{String,Any} = lock(() -> copy(acc.d), acc.lock)

"""
    with_run_usage(f, acc = RunUsage()) -> f()

Run `f` with `acc` as the run's usage accumulator; read it afterwards with `run_usage(acc)`.
"""
with_run_usage(f::Function, acc::RunUsage = RunUsage()) = Base.ScopedValues.with(f, RUN_USAGE => acc)

# ── The run's CPU budget — bound above, read below ───────────────────────────────────────────────
#
# A task sizing its own parallelism (a worker pool, `nThreads`) needs the scheduler's pool headroom
# and, on a cluster, the job's allocation — both owned by layers ABOVE the tasks. So those layers bind
# the answer here, per job, and a task reads it: the scheduler binds the cpu pool's headroom around
# `_run_task` (scheduler/jobs.jl), and the HPC dispatch rebinds it to the job's CPUs inside that
# (`_with_run_target`, hpc/dispatch.jl). Same inversion as `HPC_DISPATCH` / `_run_py_remote`.

"""
`(; cpus, dedicated)` — the CPUs the running task may spread its own work over — or `nothing` outside a
scheduled run (a task body called directly). `dedicated = false`: the cpu pool's headroom, a share of
this machine; `true`: a cluster job's own allocation. Read it through `run_cpu_threads`.
"""
const RUN_CPU_BUDGET = Base.ScopedValues.ScopedValue{Union{Nothing,@NamedTuple{cpus::Int, dedicated::Bool}}}(nothing)

"""
    run_cpu_threads(; cap) -> Int

Threads for a task's own parallelism: a cluster job's whole allocation, else this machine's cpu-pool
headroom capped at `cap`; `1` outside a scheduled run.
"""
function run_cpu_threads(; cap::Integer)::Int
    b = RUN_CPU_BUDGET[]
    isnothing(b) ? 1 : b.dedicated ? b.cpus : min(b.cpus, Int(cap))
end

"""
    read_local_py_usage(path) -> Dict{String,Any}

The report a Python runner wrote at exit (`script_utils.write_usage_report`) as one local `usage`
report — `{source: "local", host, runs: 1, peakRssBytes}` — or `{}` when there is none (killed, or a
runner that never called `script_params`). A peak that is not a positive integer is dropped.
"""
function read_local_py_usage(path::AbstractString)::Dict{String,Any}
    isfile(path) || return Dict{String,Any}()
    peak = try
        get(JSON3.read(read(path, String)), :peakRssBytes, nothing)
    catch
        nothing
    end
    (peak isa Integer && peak > 0) || return Dict{String,Any}()
    u = Dict{String,Any}("source" => "local", "runs" => 1, "peakRssBytes" => Int(peak))
    h = try Libc.gethostname() catch; "" end
    isempty(h) || (u["host"] = h)
    u
end
