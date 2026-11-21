# ── Reuse unchanged steps (app/src/tasks/chain/reuse.jl) ─────────────────────────────────────────────
# docs/SCHEDULER.md → *Reuse unchanged steps*. Two test tasks on a copy of the committed CONTMESH fixture:
# `reuseTest.relabel` reads label set `seg` and registers label set `rel`; `reuseTest.summarise` reads
# `rel` and the gated population `seg/qc` and registers `sum`. They go through `run_task` like any node,
# so their run-log entries (inputs, gates, version) are the scheduler's own. Each run is counted, so
# "dispatched nothing" is a count, not an inference.

# The two task types and their run counter live in runtests.jl (a pool worker must see their methods).
const _REUSE_TASKS_REGISTERED = Ref(false)
function _register_reuse_test_tasks!()
    _REUSE_TASKS_REGISTERED[] && return
    _REUSE_TASKS_REGISTERED[] = true
    dir = mktempdir()
    level = Dict("key" => "level", "type" => "int", "default" => 1, "min" => 0, "max" => 100)
    reads(dflt) = Dict("key" => "labels", "type" => "valueNameSelection", "field" => "labelPropsNames", "default" => dflt)
    writes(dflt) = Dict("key" => "output", "type" => "valueNameInput", "namespace" => "labels", "default" => dflt)
    pops = Dict("key" => "pops", "type" => "popSelection", "accepts" => ["live"], "multiple" => true,
                "acrossSegmentations" => true, "default" => [])
    for (fun, task, ps) in (("reuseTest.relabel", _ReuseRelabel(), Any[reads("seg"), writes("rel"), level]),
                            ("reuseTest.summarise", _ReuseSummarise(), Any[reads("rel"), pops, writes("sum"), level]),
                            ("reuseTest.modelled", _ReuseModelled(),
                             Any[reads("seg"), writes("mod"), level,
                                 Dict("key" => "model", "type" => "text", "optionsFrom" => "cellposeModels", "default" => "m1")]))
        spec = joinpath(dir, "$(split(fun, '.')[2]).json")
        write(spec, JSON3.write(Dict("fun_name" => fun, "task" => split(fun, '.')[2], "label" => fun,
                                     "scope" => "image", "resource_pool" => "cpu", "params" => ps)))
        register_task!(fun, task; spec = spec)
    end
end

_reuse_gate(x0) = RectangleGate("x", "y", x0, 10.0, 0.0, 10.0)

# A fresh copy of the fixture's project with the two-step chain saved and a gate `/qc` on `seg`; `f(proj,
# uid, img)` runs with the projects dir pointed at it (in memory only — never set_projects_dir).
function _with_reuse_project(f)
    _register_reuse_test_tasks!()
    fx = fixture_for(:cells, Symbol("label-store"))
    td = mktempdir()
    pdir = joinpath(td, basename(fx.project))
    cp(fx.project, pdir)
    write(joinpath(pdir, "project.json"), JSON3.write(Dict("class" => "CciaProject", "uid" => basename(fx.project),
                                                          "name" => "reuse", "set_uids" => [])))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = td
        empty!(_REUSE_RUNS)
        proj = load_project(basename(fx.project))
        img = init_object(proj.uid, fx.uid)
        m = PopulationTree(pop_type = "flow", value_name = "seg")
        add_pop!(m, "qc"; parent = ROOT, gate = _reuse_gate(0.0))
        save_pop_tree!(m, img._dir)
        save_chain_template!(proj, ChainTemplate("reuse-chain",
            [ChainNode(id = "r", fn = "reuseTest.relabel", scope = "image",
                       params = Dict{String,Any}("labels" => "seg", "output" => "rel", "level" => 1)),
             ChainNode(id = "s", fn = "reuseTest.summarise", scope = "image",
                       params = Dict{String,Any}("labels" => "rel", "pops" => ["seg/qc"], "output" => "sum", "level" => 1))],
            [ChainEdge("r", "s")]))
        f(proj, fx.uid, img, m)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

_reuse_run(proj, uid; kw...) = run_chain(proj, [uid]; chain = "reuse-chain", on_log = _ -> nothing, kw...)
_reuse_runs() = (get(_REUSE_RUNS, "relabel", 0), get(_REUSE_RUNS, "summarise", 0))
_reused(run, uid, nid) = run.image_states[uid][nid].reused

@testset "Reuse: a second identical run dispatches nothing and says what it reused" begin
    _with_reuse_project() do proj, uid, img, _
        first = _reuse_run(proj, uid; reuse = true)
        @test _reuse_runs() == (1, 1)                       # nothing to reuse yet: both ran
        @test _reused(first, uid, "r") === nothing && _reused(first, uid, "s") === nothing
        tr, ts = first.image_states[uid]["r"].task_id, first.image_states[uid]["s"].task_id

        frames = Dict{String,Any}[]
        subs = subscribe_chain_frames!(f -> push!(frames, f))
        second = try _reuse_run(proj, uid; reuse = true) finally
            for (ev, h) in subs; unsubscribe_chain_events!(ev, h); end
        end
        @test _reuse_runs() == (1, 1)                       # dispatched nothing
        @test second.image_states[uid]["r"].status == NODE_DONE && second.image_states[uid]["s"].status == NODE_DONE
        @test _reused(second, uid, "r") == tr && _reused(second, uid, "s") == ts
        # visible: in run.json, on the wire, and in the image's run log
        back = load_chain_run(proj, second.id)
        @test back.reuse && back.image_states[uid]["r"].reused == tr && back.image_states[uid]["s"].reused == ts
        done = [f for f in frames if f["type"] == "chain:node:done" && f["runId"] == second.id]
        @test sort([String(f["reused"]) for f in done]) == sort([tr, ts]) && all(f -> f["taskId"] == "", done)
        skips = [e for e in read_run_log(img) if get(e, "status", "") == RUN_LOG_SKIPPED]
        @test sort([String(e["reason"]) for e in skips]) == sort(["reused $tr", "reused $ts"])
    end
end

@testset "Reuse: a changed param re-runs that node and what follows it" begin
    _with_reuse_project() do proj, uid, img, _
        _reuse_run(proj, uid; reuse = true)
        tr = only(e for e in read_run_log(img) if e["fun"] == "reuseTest.relabel")["taskId"]
        run = _reuse_run(proj, uid; reuse = true, overrides = Dict{String,Any}("s" => Dict("level" => 2)))
        @test _reuse_runs() == (1, 2)
        @test _reused(run, uid, "r") == tr && _reused(run, uid, "s") === nothing
        # …and a change upstream re-runs both: node 2 reads what node 1 just rewrote
        run = _reuse_run(proj, uid; reuse = true, overrides = Dict{String,Any}("r" => Dict("level" => 3),
                                                                              "s" => Dict("level" => 2)))
        @test _reuse_runs() == (2, 3)
        @test _reused(run, uid, "r") === nothing && _reused(run, uid, "s") === nothing
    end
end

@testset "Reuse: an edited gate re-runs the node that reads it" begin
    _with_reuse_project() do proj, uid, img, m
        _reuse_run(proj, uid; reuse = true)
        set_gate!(m, "/qc", _reuse_gate(2.0)); save_pop_tree!(m, img._dir)
        run = _reuse_run(proj, uid; reuse = true)
        @test _reuse_runs() == (1, 2)
        @test _reused(run, uid, "r") !== nothing && _reused(run, uid, "s") === nothing
    end
end

@testset "Reuse: off by default, everything runs" begin
    _with_reuse_project() do proj, uid, img, _
        _reuse_run(proj, uid; reuse = true)
        run = _reuse_run(proj, uid)
        @test !run.reuse && _reuse_runs() == (2, 2)
        @test _reused(run, uid, "r") === nothing && _reused(run, uid, "s") === nothing
        @test !any(e -> get(e, "status", "") == RUN_LOG_SKIPPED, read_run_log(img))
    end
end

@testset "Reuse: an output deleted or an input replaced since means it runs" begin
    _with_reuse_project() do proj, uid, img, _
        _reuse_run(proj, uid; reuse = true)
        # the label set node 1 made is gone: node 1 runs, and so does node 2 after it
        Cecelia.commit_state!(img) do raw
            raw["labels"] = Dict{String,Any}(String(k) => v for (k, v) in raw["labels"] if String(k) != "rel")
        end
        run = _reuse_run(proj, uid; reuse = true)
        @test _reuse_runs() == (2, 2)
        @test _reused(run, uid, "r") === nothing && _reused(run, uid, "s") === nothing

        # `rel` replaced by a run outside the chain with the same params: node 1's output is now that
        # run's, unchanged since, so it is reused — crediting that run; node 2 read the `rel` it replaced
        task = Cecelia._task_from_fun_name("reuseTest.relabel")
        run_task(task, init_object(proj.uid, uid), Dict{String,Any}("labels" => "seg", "output" => "rel", "level" => 1);
                 task_id = "outside-1")
        @test _reuse_runs() == (3, 2)
        run = _reuse_run(proj, uid; reuse = true)
        @test _reused(run, uid, "r") == "outside-1" && _reused(run, uid, "s") === nothing
        @test _reuse_runs() == (3, 3)

        # replaced with OTHER params: the output is not what the chain asks for, so node 1 runs
        run_task(task, init_object(proj.uid, uid), Dict{String,Any}("labels" => "seg", "output" => "rel", "level" => 9);
                 task_id = "outside-2")
        run = _reuse_run(proj, uid; reuse = true)
        @test _reused(run, uid, "r") === nothing && _reused(run, uid, "s") === nothing
        @test _reuse_runs() == (5, 4)
    end
end

@testset "Reuse: a reused output keeps its original maker" begin
    _with_reuse_project() do proj, uid, img, _
        first = _reuse_run(proj, uid; reuse = true)
        tr, ts = first.image_states[uid]["r"].task_id, first.image_states[uid]["s"].task_id
        made_at = Dict(String(e["fun"]) => String(e["at"]) for e in read_run_log(img))
        _reuse_run(proj, uid; reuse = true)
        _reuse_run(proj, uid; reuse = true)                 # twice: a reuse of a reuse still names the maker
        rows = Dict(r["task"] => r for r in history_params_table(proj, [uid]))
        @test rows["reuseTest.relabel"]["taskIds"] == [tr] && rows["reuseTest.summarise"]["taskIds"] == [ts]
        @test rows["reuseTest.relabel"]["lastRun"] == made_at["reuseTest.relabel"]
        # Only the first run ran the step, so only its chain record is a history row; and a run's record
        # comes before its run-log rows, so on a same-second tie the row with the task id wins. (Asserted on
        # the rows themselves: a record is stamped in UTC, the run log in local time, so which wins the
        # sort above depends on the machine's time zone.)
        hr = [r for r in Cecelia._history_rows(proj, [uid]) if r.fn == "reuseTest.relabel"]
        @test count(r -> isempty(r.taskId), hr) == 1
        @test findfirst(r -> isempty(r.taskId), hr) < findfirst(r -> r.taskId == tr, hr)
        # `rel` replaced outside the chain: `sum` is stale, and its maker is the first run's node 2
        run_task(Cecelia._task_from_fun_name("reuseTest.relabel"), init_object(proj.uid, uid),
                 Dict{String,Any}("labels" => "seg", "output" => "rel", "level" => 7); task_id = "outside-3")
        st = stale_outputs_for(init_object(proj.uid, uid))
        sum = only(d for d in st.stale if d["name"] == "sum")
        @test sum["run"]["taskId"] == ts
        @test only(sum["because"])["read"]["name"] == "rel" && only(sum["because"])["replacedBy"]["taskId"] == "outside-3"
    end
end

@testset "Reuse: the flag crosses the runner boundary, off unless it says true" begin
    rt(r) = Cecelia.chain_request(Cecelia.chain_request_dict(r))
    @test rt(ChainRequest(; project_uid = "p", reuse = true)).reuse
    @test !rt(ChainRequest(; project_uid = "p")).reuse
    @test !Cecelia.chain_request(Dict{String,Any}("projectUid" => "p", "reuse" => "true")).reuse
end

@testset "Reuse: a step with a model picker always runs (a retrained model keeps its name)" begin
    _with_reuse_project() do proj, uid, img, _
        save_chain_template!(proj, ChainTemplate("reuse-model",
            [ChainNode(id = "m", fn = "reuseTest.modelled", scope = "image",
                       params = Dict{String,Any}("labels" => "seg", "output" => "mod", "level" => 1, "model" => "m1"))],
            ChainEdge[]))
        for _ in 1:2
            run = run_chain(proj, [uid]; chain = "reuse-model", on_log = _ -> nothing, reuse = true)
            @test run.image_states[uid]["m"].status == NODE_DONE && _reused(run, uid, "m") === nothing
        end
        @test get(_REUSE_RUNS, "modelled", 0) == 2
    end
end
