# End-to-end test for the detached task runner: launches a REAL second process, submits a task over
# HTTP, and reads the outcome off the event stream. `pixi run test-runner`.
#
# Separate from the main suite deliberately. It pays a full Julia start-up + Cecelia precompile for the
# child, which is minutes on a cold depot — too slow for the suite everyone runs on every change, and
# the thing it proves (a process boundary works) is not something the in-process tests can fake.
#
# What it proves, and what nothing else can:
#   1. a runner launched by `runner_launch!` answers with OUR protocol;
#   2. a task submitted over HTTP actually runs there, in that process, not here;
#   3. its frames arrive on the event stream in the contracted order (result before terminal status);
#   4. `/tasks/recent` still knows how it ended AFTER the task has left the live registry — which is
#      what lets a restarted API server recover an outcome it was not connected for.

using Test
using Cecelia
using JSON3

# Hermetic by default, exactly as `runtests.jl` does it — with no `custom.toml` anywhere,
# `projects_dir()` is the shipped placeholder `/path/to/projects` and `create_project!` fails on
# `mkdir("/path")`. It matters more here than in the suite: the runner is a SEPARATE PROCESS that
# resolves `projects_dir()` itself, so the config has to reach it through the environment it
# inherits — which is also a rehearsal for a remote target resolving paths against its own root.
if !haskey(ENV, "CECELIA_DEV_DIR")
    let cfg = mktempdir(), proj = mktempdir()
        write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * proj * "'\n")
        ENV["CECELIA_DEV_DIR"] = cfg
        @info "Runner e2e: hermetic config" config_dir=cfg projects_dir=proj
    end
end

init_cecelia!()

const PORT = parse(Int, get(ENV, "CECELIA_RUNNER_TEST_PORT", "7698"))   # not the real 7657

@testset "detached task runner — end to end" begin
    h = RunnerHandle(; port = PORT)
    # Never adopt a stranded runner from an earlier failed run — that would test the wrong process.
    runner_alive(h) && (runner_stop!(h); sleep(1.0))

    frames = Vector{Dict{String,Any}}()
    flock  = ReentrantLock()
    sub    = nothing

    proj = create_project!(name = "runner-e2e-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "img")
    tid  = "e2e$(rand(100000:999999))"

    try
        withenv("CECELIA_RUNNER_PORT" => string(PORT)) do
            runner_launch!(h; wait_seconds = 600)   # cold precompile can be minutes
        end
        ping = runner_ping(h)
        @test ping !== nothing
        ping === nothing && return                  # everything below would be noise
        @test Int(ping["protocol"]) == RUNNER_PROTOCOL
        @test Int(ping["pid"]) != getpid()           # (2) a DIFFERENT process — the whole point

        sub = runner_subscribe!(h, f -> lock(flock) do; push!(frames, f); end)
        sleep(1.5)                                   # let the subscription attach before submitting

        resp = runner_submit(h, TaskRequest(; task_id = tid,
                                              fun_name = "testTasks.image_task",
                                              project_uid = proj.uid, image_uid = img.uid,
                                              params = Dict{String,Any}("message" => "ran on the runner",
                                                                        "waitMs"  => 300)))
        @test resp["ok"] == true

        ok = timedwait(30.0; pollint = 0.2) do
            lock(flock) do
                any(f -> get(f, "type", "") == "task:status" &&
                         get(f, "taskId", "") == tid &&
                         get(f, "status", "") in ("done", "failed"), frames)
            end
        end
        @test ok === :ok

        mine = lock(flock) do; [f for f in frames if get(f, "taskId", "") == tid]; end
        types = [get(f, "type", "") for f in mine]
        @test "task:log" in types
        @test any(f -> get(f, "type", "") == "task:log" &&
                       occursin("ran on the runner", get(f, "line", "")), mine)

        statuses = [get(f, "status", "") for f in mine if get(f, "type", "") == "task:status"]
        @test "running" in statuses
        @test last(statuses) == "done"

        # (3) result BEFORE the terminal status — the ordering the frontend depends on, now asserted
        # across a process boundary rather than just inside one function.
        i_result = findfirst(f -> get(f, "type", "") == "task:result", mine)
        i_done   = findlast(f -> get(f, "type", "") == "task:status" &&
                                 get(f, "status", "") == "done", mine)
        @test i_result !== nothing && i_done !== nothing && i_result < i_done

        # (4) the outcome outlives the live registry — this is the recovery path for an API server
        # that restarted while the task was running and reconnected after it finished.
        @test !any(t -> String(get(t, "id", "")) == tid, runner_tasks(h))
        banked = [t for t in runner_recent(h) if String(get(t, "id", "")) == tid]
        @test length(banked) == 1
        if length(banked) == 1
            @test String(banked[1]["status"]) == "done"
            # A real start, not "" — otherwise a client recovering this row can only fall back to its
            # own clock, which is the elapsed-timer bug the start note exists to prevent.
            @test !isempty(String(banked[1]["started_at"]))
            @test !isempty(String(banked[1]["finished_at"]))
        end
        # ...and the live terminal frame agreed with what was banked — one task, one finish time,
        # whichever carrier the client happened to receive.
        term = last([f for f in mine if get(f, "type", "") == "task:status"])
        length(banked) == 1 && @test String(get(term, "finishedAt", "")) == String(banked[1]["finished_at"])

        # Pools are the RUNNER's, and the throttle has to reach them across the boundary or the GPU
        # budget is unenforceable from the UI.
        pools = runner_pools(h)
        @test !isempty(pools)
        @test any(p -> String(get(p, "name", "")) == "cpu", pools)
        # ── A CHAIN, executed in the runner's process ──────────────────────────────
        # Different carrier from a task: chain telemetry rides the event bus (`chain:node:*`), built by
        # `subscribe_chain_frames!` in THAT process. So this asserts the second half of Phase 2 — that
        # the frames a client sees are produced across the boundary and arrive intact.
        chain_img = add_image!(first(sets(proj)); name = "img2")
        make_chain(proj, "e2e-chain", [chain_node("testTasks.image_task")])

        before = lock(flock) do; length(frames); end
        cresp = runner_submit_chain(h, ChainRequest(; project_uid = proj.uid,
                                                      chain_name  = "e2e-chain",
                                                      image_uids  = [chain_img.uid]))
        @test cresp["ok"] == true

        cok = timedwait(60.0; pollint = 0.2) do
            lock(flock) do
                any(f -> get(f, "type", "") in ("chain:run:done", "chain:run:failed"), frames)
            end
        end
        @test cok === :ok

        cframes = lock(flock) do; frames[(before + 1):end]; end
        ctypes  = [get(f, "type", "") for f in cframes]
        @test "chain:run:done" in ctypes                    # …and it SUCCEEDED, not merely finished
        @test "chain:node:done" in ctypes
        # The node frame must carry what a client keys on. A chain run emits no `task:status` at all, so
        # these fields are the only carrier — `taskId` is what correlates the row, and without
        # `startedAt` the Live view times the node from when it happened to see it.
        nodedone = first(f for f in cframes if get(f, "type", "") == "chain:node:done")
        @test String(get(nodedone, "runId", "")) != ""
        @test String(get(nodedone, "nodeId", "")) != ""
        @test String(get(nodedone, "imageUid", "")) == chain_img.uid
        @test !isempty(String(get(nodedone, "taskId", "")))
        @test !isempty(String(get(nodedone, "startedAt", "")))
        @test !isempty(String(get(nodedone, "finishedAt", "")))

        # The claim is released when the run ends, or a resume of it could never be accepted again.
        @test isempty(runner_chain_runs(h))
    finally
        sub === nothing || (try; schedule(sub, InterruptException(); error = true); catch; end)
        runner_stop!(h)
        try; rm(proj.root; recursive = true); catch; end
    end
end
