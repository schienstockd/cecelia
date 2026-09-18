# Scheduler — task queueing, resource-pool concurrency, cancellation.
#
# concurrency-critical — the comments in the child files document lock ordering, cancellation
# races and silent-failure contracts (post-exactly-once, terminal-state finality,
# `never nest _TASKS_LOCK inside _POOLS_LOCK`). Do not trim for brevity without re-reading them.
# See docs/MAINTAINABILITY.md and docs/SCHEDULER.md.
#
# The runtime is one namespace — nothing here `module`s the sub-files — they were split off by
# responsibility so 786 L of resource-pool machinery, task-record lifecycle, job execution and
# the public run_task/run_tasks entries do not sit on top of one another. Order matters for
# structs and consts only; function bodies resolve at call time, so downstream files can freely
# reference forward.
#
#   pools.jl       — ResourcePool + `_pools_init!`, `_pool`, `resize_pool!`, `set_pool_limit!`.
#   task_record.jl — TaskStatus enum, TaskRecord struct, register/deregister/cancel_task!.
#   reporting.jl   — read-only reporting (`list_pools`, `list_tasks`, `pool_status`) + the
#                    chain-cancellation registry (`cancel_chain_run!`, `is_chain_cancelled`) +
#                    `_publishable_params` (the JSON-safety whitelist).
#   jobs.jl        — TaskJobTarget hierarchy (`SingleImage`/`MultiImage`), TaskJob struct,
#                    `_execute_job!` (the post-exactly-once worker).
#   run.jl         — the public `run_task` (single + set-scope + UID-resolving) and `run_tasks`
#                    (parallel/sequential + UID-resolving) overloads.

using Dates

include("scheduler/pools.jl")
include("scheduler/task_record.jl")
include("scheduler/reporting.jl")
include("scheduler/jobs.jl")
include("scheduler/run.jl")
