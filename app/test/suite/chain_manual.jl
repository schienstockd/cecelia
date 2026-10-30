# ── Manual chain steps (chain/manual.jl) ──────────────────────────────────
# A manual node runs prepare, parks at NODE_WAITING and holds its image (image scope) or the whole set
# (set scope) until continued. `run_chain` blocks until the run ends, so each test runs it on a thread
# and drives it from here. The test tasks (`_ManualPrepTask`, `_ManualSetPrepTask`) live in runtests.jl.

const _MANUAL_TASKS_REGISTERED = Ref(false)
function _register_manual_test_tasks!()
    _MANUAL_TASKS_REGISTERED[] && return
    _MANUAL_TASKS_REGISTERED[] = true
    dir = mktempdir()
    for (fun, task, scope) in (("customTest.manualStep", _ManualPrepTask(), "image"),
                               ("customTest.manualSetStep", _ManualSetPrepTask(), "set"))
        spec = joinpath(dir, "$(split(fun, '.')[2]).json")
        write(spec, JSON3.write(Dict(
            "fun_name" => fun, "task" => split(fun, '.')[2], "label" => "Manual test step",
            "scope" => scope, "resource_pool" => "cpu", "params" => Any[],
            "manual" => Dict("prompt" => "Check it", "open" => "gating"))))
        register_task!(fun, task; spec = spec)
    end
end

_wait_for(f; timeout = 30.0) = timedwait(f, timeout; pollint = 0.05) === :ok

_mstate(run, uid, nid) = run.image_states[uid][nid].status

_run_ids(proj) = (d = Cecelia._runs_dir(proj); isdir(d) ? readdir(d) : String[])

# Parked in THIS process right now: the waiter is registered (status alone is set just before).
_parked(run_id, nid, uid) = lock(Cecelia._CHAIN_WAITERS_LOCK) do
    haskey(Cecelia._CHAIN_WAITERS, (run_id, nid, uid))
end
_live_run(proj) = (ids = filter(chain_run_parked_here, _run_ids(proj)); length(ids) == 1 ? ids[1] : "")

# A copy of a run's record under a new id: the run.json a process leaves behind if it dies right now.
function _orphan_copy(proj, run_id)
    new_id = gen_uid()
    src = joinpath(Cecelia._runs_dir(proj), run_id)
    dst = joinpath(Cecelia._runs_dir(proj), new_id)
    mkpath(dst)
    raw = JSON3.read(read(joinpath(src, "run.json"), String), Dict{String,Any})
    raw["id"] = new_id
    write(joinpath(dst, "run.json"), JSON3.write(raw))
    new_id
end

function _manual_project(n)
    empty!(_MANUAL_PREPARED); empty!(_MANUAL_BLOCKED)   # uids repeat across test projects (seeded RNG)
    proj = create_project!(name = "chain-manual-$(rand(1000:9999))")
    s    = add_set!(proj; name = "s")
    proj, [add_image!(s; name = "img-$i") for i in 1:n]
end

_manual_chain(proj, name, fn, scope) = save_chain_template!(proj, ChainTemplate(name,
    [ChainNode(id = "n1", fn = "testTasks.image_task", scope = "image", params = Dict{String,Any}()),
     ChainNode(id = "m",  fn = fn,                     scope = scope,   params = Dict{String,Any}()),
     ChainNode(id = "n3", fn = "testTasks.image_task", scope = "image", params = Dict{String,Any}())],
    [ChainEdge("n1", "m"), ChainEdge("m", "n3")]))

@testset "Manual step — spec block and validator" begin
    _register_manual_test_tasks!()
    @test task_manual(_ManualPrepTask())["prompt"] == "Check it"
    @test isnothing(task_manual(Cecelia._task_from_fun_name("testTasks.image_task")))
    # a manual step holds the run; an incremental node holds nothing — refused at save time
    bad = ChainTemplate("bad", [ChainNode(id = "m", fn = "customTest.manualStep", scope = "incremental",
                                          params = Dict{String,Any}())], ChainEdge[])
    @test_throws ChainTemplateError validate_chain_template(bad)
end

@testset "Manual step — image scope parks and continues per image" begin
    _register_manual_test_tasks!()
    proj, imgs = _manual_project(2)
    a, b = imgs[1].uid, imgs[2].uid
    _manual_chain(proj, "manual-image", "customTest.manualStep", "image")
    empty!(_MANUAL_BLOCKED); push!(_MANUAL_BLOCKED, b)

    t = Threads.@spawn run_chain(proj, [a, b]; chain = "manual-image", on_log = _ -> nothing)
    @test _wait_for(() -> (id = _live_run(proj); !isempty(id) && _parked(id, "m", a) && _parked(id, "m", b)))
    run_id = _live_run(proj)
    run = load_chain_run(proj, run_id)
    @test _mstate(run, a, "m") == NODE_WAITING && _mstate(run, b, "m") == NODE_WAITING
    @test _mstate(run, a, "n3") == NODE_PENDING            # held: downstream has not run

    r = continue_chain_node!(run_id, "m")                  # all parked images; b's check refuses
    @test r["continued"] == [a]
    @test occursin("no gates", r["refused"][b])
    @test _wait_for(() -> _mstate(load_chain_run(proj, run_id), a, "n3") == NODE_DONE)
    @test _mstate(load_chain_run(proj, run_id), b, "m") == NODE_WAITING   # still parked

    empty!(_MANUAL_BLOCKED)
    @test continue_chain_node!(run_id, "m"; image_uids = [b])["continued"] == [b]
    run = fetch(t)
    @test all(_mstate(run, u, n) == NODE_DONE for u in (a, b), n in ("n1", "m", "n3"))
    @test _MANUAL_PREPARED[a] == 1 && _MANUAL_PREPARED[b] == 1   # prepare ran once each
    rm(proj.root; recursive = true)
end

@testset "Manual step — set scope parks once for all" begin
    _register_manual_test_tasks!()
    proj, imgs = _manual_project(3)
    uids = [i.uid for i in imgs]
    _manual_chain(proj, "manual-set", "customTest.manualSetStep", "set")
    empty!(_MANUAL_BLOCKED); push!(_MANUAL_BLOCKED, uids[2])

    t = Threads.@spawn run_chain(proj, uids; chain = "manual-set", on_log = _ -> nothing)
    @test _wait_for(() -> (id = _live_run(proj); !isempty(id) && _parked(id, "m", "")))
    run_id = _live_run(proj)
    @test all(_mstate(load_chain_run(proj, run_id), u, "m") == NODE_WAITING for u in uids)

    r = continue_chain_node!(run_id, "m")                  # one member fails → nobody goes on
    @test isempty(r["continued"])
    @test occursin(uids[2], r["refused"]["m"])
    empty!(_MANUAL_BLOCKED)
    @test continue_chain_node!(run_id, "m")["continued"] == ["m"]
    run = fetch(t)
    @test all(_mstate(run, u, n) == NODE_DONE for u in uids, n in ("m", "n3"))
    @test all(_MANUAL_PREPARED[u] == 1 for u in uids)      # one joint prepare
    rm(proj.root; recursive = true)
end

@testset "Manual step — cancel releases a parked step" begin
    _register_manual_test_tasks!()
    proj, imgs = _manual_project(1)
    a = imgs[1].uid
    _manual_chain(proj, "manual-cancel", "customTest.manualStep", "image")
    t = Threads.@spawn run_chain(proj, [a]; chain = "manual-cancel", on_log = _ -> nothing,
                                 on_cancel_check = is_chain_cancelled)
    @test _wait_for(() -> (id = _live_run(proj); !isempty(id) && _parked(id, "m", a)))
    run_id = _live_run(proj)
    cancel_chain_run!(run_id)
    run = fetch(t)
    @test _mstate(run, a, "m") == NODE_CANCELLED
    @test _mstate(run, a, "n3") == NODE_CANCELLED
    @test !chain_run_parked_here(run_id)
    rm(proj.root; recursive = true)
end

@testset "Manual step — a step parked when the process died" begin
    _register_manual_test_tasks!()
    proj, imgs = _manual_project(1)
    a = imgs[1].uid
    _manual_chain(proj, "manual-crash", "customTest.manualStep", "image")
    empty!(_MANUAL_BLOCKED)
    t = Threads.@spawn run_chain(proj, [a]; chain = "manual-crash", on_log = _ -> nothing)
    @test _wait_for(() -> (id = _live_run(proj); !isempty(id) && _parked(id, "m", a)))
    live = _live_run(proj)
    resumed = _orphan_copy(proj, live)     # parked, with no process behind it
    offline = _orphan_copy(proj, live)
    continue_chain_node!(live, "m"); fetch(t)
    @test _MANUAL_PREPARED[a] == 1

    # Resume re-parks it — prepare does NOT run again — and a continue then finishes the run.
    t2 = Threads.@spawn run_chain(proj, String[]; run_id = resumed, on_log = _ -> nothing)
    @test _wait_for(() -> _parked(resumed, "m", a))
    @test _MANUAL_PREPARED[a] == 1
    @test continue_chain_node!(resumed, "m")["continued"] == [a]
    @test _mstate(fetch(t2), a, "n3") == NODE_DONE

    # Not live at all: continue on disk (the check still runs), then resume skips straight past it.
    @test isempty(continue_chain_node!(offline, "m")["continued"])    # nothing parked here
    push!(_MANUAL_BLOCKED, a)
    @test occursin("no gates", continue_parked_on_disk!(proj, offline, "m")["refused"][a])
    empty!(_MANUAL_BLOCKED)
    @test continue_parked_on_disk!(proj, offline, "m")["continued"] == [a]
    @test _mstate(load_chain_run(proj, offline), a, "m") == NODE_DONE
    run = run_chain(proj, String[]; run_id = offline, on_log = _ -> nothing)
    @test _mstate(run, a, "n3") == NODE_DONE
    @test _MANUAL_PREPARED[a] == 1
    rm(proj.root; recursive = true)
end
