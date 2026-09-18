# Resource pools — the ONE bottleneck-rationing mechanism the scheduler exposes.
# `ResourcePool` (limit + persistent queue + dispatcher), `_pools_init!`, `_pool`, `resize_pool!`
# and `set_pool_limit!` (config.toml + custom.toml persistence). See docs/SCHEDULER.md
# → Resource pools. Loaded first so the task-record and reporting files can reference
# `_POOLS`/`_POOLS_LOCK`/`_POOLS_INIT` and dispatch onto the pool queue built here.

# Each named pool owns ONE persistent queue and a resizable SLOT budget. Concurrency is capped by
# `limit` slots, NOT by a worker count: a single dispatcher pulls each job, waits for a free slot,
# then runs the job on its own task (releasing the slot when it finishes). Because the slot is
# acquired at the moment of execution — checked against the current `limit` — a pool never runs more
# than `limit` jobs at once, even the instant after a throttle-down.
#
# run_task submits a job and blocks on take!(done_ch) until it finishes — synchronous from the
# caller. Blocking on a Channel/Condition yields the OS thread to Julia's scheduler (no spin-wait),
# so blocked submitters don't exhaust it.
#
# Resizing just changes the slot budget (same queue, same dispatcher):
#   • grow  → `notify` wakes the dispatcher if it was waiting for a slot; queued backlog fans out
#     immediately up to the new `limit`.
#   • shrink → in-flight jobs finish and release their slots; the dispatcher blocks in `_acquire_slot!`
#     (`while in_flight >= limit: wait`) until enough drain, then admits the next — so it settles to
#     the new `limit` without ever exceeding it.

mutable struct ResourcePool
    name::String
    limit::Int              # slot budget = max concurrent jobs (mutated by resize, guarded by `cond`)
    queue::Channel{Any}     # persistent — the dispatcher pulls TaskJobs from here
    in_flight::Int          # jobs currently executing (guarded by `cond`)
    cond::Threads.Condition # guards limit + in_flight; signalled on slot release and on limit raise
end

const _POOLS      = Dict{String, ResourcePool}()
const _POOLS_LOCK = ReentrantLock()
const _POOLS_INIT = Ref(false)

function _pools_init!()
    lock(_POOLS_LOCK) do
        _POOLS_INIT[] && return
        pool_conf = get(cecelia_conf(), "pools", Dict{String,Any}())
        for (name, limit) in pool_conf
            _start_pool!(string(name), Int(limit))
        end
        haskey(_POOLS, "cpu") ||
            _start_pool!("cpu", tasks_concurrent_limit())
        _POOLS_INIT[] = true
    end
end

# Wait for a free slot, then claim it. Blocks (yielding) while `in_flight >= limit`; a slot release
# or a limit raise `notify`s the condition and re-checks. Checked at execution time, so it honours a
# just-lowered `limit`.
function _acquire_slot!(pool::ResourcePool)
    lock(pool.cond) do
        while pool.in_flight >= pool.limit
            wait(pool.cond)
        end
        pool.in_flight += 1
    end
end

function _release_slot!(pool::ResourcePool)
    lock(pool.cond) do
        pool.in_flight -= 1
        notify(pool.cond)
    end
end

# Create a pool + its single dispatcher. Call holding _POOLS_LOCK.
function _start_pool!(name::String, limit::Int)
    queue = Channel{Any}(512)   # large buffer — put! never blocks in practice
    pool  = ResourcePool(name, limit, queue, 0, Threads.Condition())
    _POOLS[name] = pool
    # dispatcher: pull a job, wait for a free slot, run it on its own task (freeing the slot after).
    # Both `try`s below are backstops for the same failure mode: an exception in a `Threads.@spawn` is
    # SILENT (nobody fetches these tasks), so an unguarded throw either strands a submitter forever
    # (job task) or kills the dispatcher and wedges the whole pool at `:queued` (dispatcher task).
    Threads.@spawn begin
        for job in pool.queue
            try
                _acquire_slot!(pool)
            catch e
                # Never let the loop die — one job's failure must not stop the pool consuming the rest.
                @error "Pool dispatcher could not acquire a slot — job dropped" pool = pool.name exception = (e, catch_backtrace())
                try; put!(job.done, nothing); catch; end   # release the blocked submitter
                continue
            end
            Threads.@spawn try
                _execute_job!(job)
            catch e
                # Unreachable while _execute_job! keeps its contract (it always posts) — logged rather
                # than swallowed so a future regression surfaces as an error, not a hung task.
                @error "Job task died" pool = pool.name task_id = job.id exception = (e, catch_backtrace())
            finally
                _release_slot!(pool)
            end
        end
    end
    pool
end

const _WARNED_MISSING_POOLS = Set{String}()

function _pool(name::String)::ResourcePool
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        haskey(_POOLS, name) && return _POOLS[name]
        # Silent fallback to the (wide) cpu pool is dangerous — a GPU task whose
        # pool is missing from config would run unbounded. Warn once per missing name.
        if name != "cpu" && name ∉ _WARNED_MISSING_POOLS
            push!(_WARNED_MISSING_POOLS, name)
            @warn "Resource pool '$name' not configured — falling back to 'cpu' " *
                  "(limit $(_POOLS["cpu"].limit)). Add it to the [pools] section of config.toml."
        end
        _POOLS["cpu"]
    end
end

"""
Resize a named resource pool's slot budget (max concurrent jobs), keeping its ONE persistent queue
and dispatcher — so already-queued jobs are never orphaned. A raise `notify`s the dispatcher so the
backlog fans out immediately; a lower simply blocks the dispatcher in `_acquire_slot!` until enough
in-flight jobs drain, so concurrency settles to `new_limit` without ever exceeding it. Creates the
pool if absent. config.toml `[pools]` sets the defaults; this is the runtime/REPL/UI path.
"""
function resize_pool!(name::String, new_limit::Int)
    new_limit < 1 && return
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        p = get(_POOLS, name, nothing)
        if isnothing(p)
            _start_pool!(name, new_limit)     # brand-new pool + its dispatcher
            return
        end
        lock(p.cond) do
            p.limit = new_limit
            notify(p.cond)   # a raise may free a slot for the waiting dispatcher; a lower is enforced
        end                  # on the next _acquire_slot! check
    end
end

const POOL_LIMIT_MAX = 64

"""
    set_pool_limit!(name, limit) -> Int

Resize a pool live (`resize_pool!`) **and** persist the new limit to the user's `custom.toml`
`[pools]` so it survives a restart — the runtime/UI counterpart to hand-editing `config.toml`.
The Settings per-pool sliders call this. Clamps `limit` to `[1, POOL_LIMIT_MAX]`; returns the
applied value. Merges into `custom.toml` (other keys survive), mirroring `set_projects_dir!`.
"""
function set_pool_limit!(name::AbstractString, limit::Integer)::Int
    nm  = string(name)
    lim = clamp(Int(limit), 1, POOL_LIMIT_MAX)
    resize_pool!(nm, lim)
    cfg_path = custom_toml_path()
    mkpath(dirname(cfg_path))
    cfg   = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    pools = get(cfg, "pools", Dict{String,Any}())
    pools[nm] = lim
    cfg["pools"] = pools
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    lim
end

function _task_pool_name(task::CciaTask)::String
    spec = _task_spec(task)
    isnothing(spec) && return "cpu"
    string(get(spec, "resource_pool", "cpu"))
end
