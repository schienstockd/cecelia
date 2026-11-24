# ── "Queue after": a chain node that waits for a run already going (chain/after_live.jl) ──
# A standalone run of the test image task is started first, as the user's segmentation would be; then a
# two-node chain built on the spot — an `after_live` node for that task, then a set-scope follow-up — is
# run inline. `run_chain` blocks, so it runs on a thread and is watched from here.

const _AL_FN = "testTasks.image_task"

_al_wait(f; timeout = 30.0) = timedwait(f, timeout; pollint = 0.05) === :ok
_al_state(run, uid, nid) = run.image_states[uid][nid].status
_al_live(uid) = !isempty(Cecelia._live_runs_of(_AL_FN, uid))

function _al_project(n)
    proj = create_project!(name = "chain-after-live-$(rand(1000:9999))")
    s    = add_set!(proj; name = "s")
    proj, [add_image!(s; name = "img-$i").uid for i in 1:n]
end

_al_template(wait_for = Dict{String,Any}()) = ChainTemplate("Queue after",
    [ChainNode(id = "wait", fn = _AL_FN, scope = "image", params = Dict{String,Any}("waitFor" => wait_for),
               after_live = true),
     ChainNode(id = "next", fn = "testTasks.set_task", scope = "set", params = Dict{String,Any}(),
               barrier_policy = "successful_only")],
    [ChainEdge("wait", "next")])

# The standalone run the chain will queue after, registered before the chain starts.
function _al_start_live(proj, uid; wait_ms)
    id = gen_uid()
    t  = Threads.@spawn run_task(proj.uid, uid; fun_name = _AL_FN, task_id = id,
                                 params = Dict{String,Any}("waitMs" => wait_ms), on_log = _ -> nothing)
    @test _al_wait(() -> _al_live(uid))
    id, t
end

# The run started on a thread, found by its record on disk.
function _al_run_id(proj)
    d = Cecelia._runs_dir(proj)
    _al_wait(() -> isdir(d) && length(readdir(d)) == 1) || error("no chain run appeared")
    only(readdir(d))
end

@testset "Queue after — template field round-trips, and only when set" begin
    t = _al_template()
    back = chain_template_from_raw(JSON3.read(Cecelia._template_json(t)))
    @test [n.after_live for n in back.nodes] == [true, false]
    # a template with no Queue-after step writes (and so hashes) exactly as before the field existed
    plain = ChainTemplate("plain", [ChainNode(id = "a", fn = _AL_FN, scope = "image")], ChainEdge[])
    @test !occursin("after_live", Cecelia._template_json(plain))
    # a Queue-after step waits per image
    bad = ChainTemplate("bad", [ChainNode(id = "w", fn = "testTasks.set_task", scope = "set",
                                          after_live = true)], ChainEdge[])
    @test_throws ChainTemplateError validate_chain_template(bad)
    # carried across the runner boundary
    raw = Dict{String,Any}("name" => "Queue after", "nodes" => [Dict("id" => "w", "fn" => _AL_FN,
                                                                     "scope" => "image", "after_live" => true)])
    req = chain_request(chain_request_dict(ChainRequest(; project_uid = "p", chain_name = "Queue after",
                                                          template = raw)))
    @test only(chain_template_from_raw(req.template).nodes).after_live
end

@testset "Queue after — the follow-up waits for the run already going" begin
    proj, (a, b) = _al_project(2)
    live_id, live = _al_start_live(proj, a; wait_ms = 2000)

    t = Threads.@spawn run_chain(proj, [a, b]; template = _al_template(), on_log = _ -> nothing)
    run_id = _al_run_id(proj)
    @test _al_wait(() -> (r = load_chain_run(proj, run_id);
                          _al_state(r, a, "wait") == NODE_RUNNING && _al_state(r, b, "wait") == NODE_RUNNING))
    mid = load_chain_run(proj, run_id)
    @test _al_live(a)                                   # still going…
    # nothing ran on b, but the fn is still going on a: b holds too (it may be a sweep's early finisher)
    @test _al_state(mid, b, "next") == NODE_PENDING
    @test _al_state(mid, a, "next") == NODE_PENDING     # …and the follow-up has not started
    @test mid.image_states[a]["wait"].task_id == live_id

    run = fetch(t)
    @test fetch(live) isa AbstractDict                  # the live run finished on its own
    @test all(_al_state(run, u, n) == NODE_DONE for u in (a, b), n in ("wait", "next"))
    # the wait step ran nothing: cohort QC and the history readers must not count it as a run of its fn
    @test Cecelia._chain_done_funs(run) == ["testTasks.set_task"]
    @test !any(r -> r.fn == _AL_FN && isempty(r.taskId), Cecelia._history_rows(proj, [a, b]))
    @test run.node_params["wait"] == Dict{String,Any}("waitFor" => Dict{String,Any}())
    @test isempty(readdir(Cecelia._chains_dir(proj)) |> f -> filter(endswith(".json"), f))   # never saved
    rm(proj.root; recursive = true)
end

@testset "Queue after — a cancelled run skips the follow-up there, and resume does not retry it" begin
    proj, (a, b) = _al_project(2)
    live_id, live = _al_start_live(proj, a; wait_ms = 5000)

    t = Threads.@spawn run_chain(proj, [a, b]; template = _al_template(), on_log = _ -> nothing)
    run_id = _al_run_id(proj)
    @test _al_wait(() -> _al_state(load_chain_run(proj, run_id), a, "wait") == NODE_RUNNING)
    @test cancel_task!(live_id)
    run = fetch(t)
    @test _al_state(run, a, "wait") == NODE_CANCELLED
    @test _al_state(run, a, "next") == NODE_SKIPPED     # successful_only: a sits out
    @test _al_state(run, b, "next") == NODE_DONE
    fetch(live)

    # A resume finds nothing running on a; retrying the wait would pass and run the follow-up on an image
    # whose run was cancelled. Its outcome stands instead.
    again = run_chain(proj, String[]; run_id, on_log = _ -> nothing)
    @test _al_state(again, a, "wait") == NODE_CANCELLED
    @test _al_state(again, a, "next") == NODE_SKIPPED
    rm(proj.root; recursive = true)
end

@testset "Queue after — inline template is its own fresh run" begin
    proj, (a,) = _al_project(1)
    @test_throws ErrorException run_chain(proj, [a]; chain = "x", template = _al_template())
    rm(proj.root; recursive = true)
end

@testset "Queue after — waits out a run this process cannot see, by its task id" begin
    # The run is on the task runner while the chain runs here (or ended before the chain looked): no local
    # record, only the id the browser saw. The wait holds until that id's outcome is banked.
    proj, (a, b) = _al_project(2)
    ghost = gen_uid()
    t = Threads.@spawn run_chain(proj, [a, b]; template = _al_template(Dict{String,Any}(a => [ghost])),
                                 on_log = _ -> nothing)
    run_id = _al_run_id(proj)
    @test _al_wait(() -> (r = load_chain_run(proj, run_id);
                          _al_state(r, a, "wait") == NODE_RUNNING && _al_state(r, b, "wait") == NODE_RUNNING))
    @test load_chain_run(proj, run_id).image_states[a]["wait"].task_id == ghost   # b holds on a's id too
    @test _al_state(load_chain_run(proj, run_id), a, "next") == NODE_PENDING
    record_task_outcome!(ghost, "failed"; image_uid = a, fun = _AL_FN)
    run = fetch(t)
    @test _al_state(run, a, "wait") == NODE_FAILED
    @test _al_state(run, a, "next") == NODE_SKIPPED
    @test _al_state(run, b, "next") == NODE_DONE
    rm(proj.root; recursive = true)
end

@testset "Queue after — an image that finishes early holds while the run goes on elsewhere" begin
    # A sequential sweep sends its next value only once the current one has ended on every image, so an
    # image done early sits idle far longer than the grace. Its wait must not end while another image of
    # the chain still runs `fn` — else the follow-up runs on that value's output and the next one overwrites it.
    proj, (a, b) = _al_project(2)
    id_a, live_a = _al_start_live(proj, a; wait_ms = 300)
    _,    live_b = _al_start_live(proj, b; wait_ms = 5000)   # the spec's max
    # a's id passed as seen, so its wait engages even if a ends before the chain looks
    t = Threads.@spawn run_chain(proj, [a, b]; template = _al_template(Dict{String,Any}(a => [id_a])),
                                 on_log = _ -> nothing)
    run_id = _al_run_id(proj)
    @test _al_wait(() -> !_al_live(a))
    sleep(Cecelia._AFTER_LIVE_GRACE + 0.5)           # a's own grace is long over…
    @test _al_live(b)                                # …while b is still going
    @test _al_state(load_chain_run(proj, run_id), a, "wait") == NODE_RUNNING
    run = fetch(t)
    fetch(live_a); fetch(live_b)
    @test all(_al_state(run, u, n) == NODE_DONE for u in (a, b), n in ("wait", "next"))
    rm(proj.root; recursive = true)
end
