# ── Chain template round-trip ─────────────────────────────────────────────
# ── Chain node scope defaults from the task spec (single source of truth) ────
# A node built without an explicit scope inherits the task JSON's "scope": set-scope
# tasks (behaviour.hmm, clustTracks.cluster) become picnic nodes automatically, image
# tasks stay image-scope. An explicit scope always overrides.
@testset "Chain node scope inherits from task spec" begin
    @test Cecelia._task_default_scope("clustTracks.cluster") == CHAIN_SET
    @test Cecelia._task_default_scope("behaviour.hmm")        == CHAIN_SET
    @test Cecelia._task_default_scope("importImages.remove")  == CHAIN_IMAGE
    @test Cecelia._task_default_scope("nonexistent.task")     == CHAIN_IMAGE   # unknown fn → image
    # EVERY set-scope task declares it in its own spec — including the mock, which used to rely on
    # each chain node passing scope="set" (the one task that contradicted "the spec is the single
    # source of truth", and it's the fixture the barrier tests are built on).
    @test Cecelia._task_default_scope("testTasks.set_task")    == CHAIN_SET
    @test chain_node("testTasks.set_task").scope               == CHAIN_SET

    # chain_node / ChainNode with no scope kwarg resolve from the spec …
    @test chain_node("clustTracks.cluster").scope == CHAIN_SET
    @test chain_node("importImages.remove").scope == CHAIN_IMAGE
    @test ChainNode(id="x", fn="behaviour.hmm").scope == CHAIN_SET
    # … and an explicit scope still wins (force a set task to run per-image)
    @test chain_node("clustTracks.cluster"; scope="image").scope == CHAIN_IMAGE

    # Deserialisation: a node dict with no "scope" key also inherits from the spec
    @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster")).scope == CHAIN_SET
    # …while a stored scope (frozen template) is honoured verbatim
    @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster",
                                       "scope"=>"image")).scope == CHAIN_IMAGE
end

# ── Producer output value_name is declared in the JSON spec (introspectable) ──
# The whiteboard reads this to prefill a downstream node's input `valueName`.
@testset "Output value_name from spec" begin
    @test Cecelia._spec_output_value_name(DriftCorrect(),    "fallback") == "driftCorrected"
    @test Cecelia._spec_output_value_name(AfCorrect(),       "fallback") == "afCorrected"
    @test Cecelia._spec_output_value_name(Smooth(),          "fallback") == "smoothed"
    # A task that declares no top-level outputValueName falls back to the caller's default
    @test Cecelia._spec_output_value_name(RemoveImage(), "fallback") == "fallback"
end

@testset "Chain template round-trip" begin
    proj = create_project!(name="chain-tpl-$(rand(1000:9999))")

    tpl = ChainTemplate(
        "test-chain",
        [ChainNode(id="n1", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    # Templates must land under settings/chains — the SAME dir the API reads/writes
    # (api/src/routes.jl _chains_dir_for_project). A divergence here made every whiteboard
    # chain run fail with "template not found" (saved via API, loaded via package).
    @test isfile(joinpath(proj.root, "settings", "chains", "test-chain.json"))
    @test !isfile(joinpath(proj.root, "chains", "test-chain.json"))

    loaded = load_chain_template(proj, "test-chain")
    @test loaded.name == "test-chain"
    @test length(loaded.nodes) == 2
    @test loaded.nodes[1].id == "n1"
    @test loaded.nodes[2].id == "n2"
    @test length(loaded.edges) == 1
    @test loaded.edges[1].from == "n1"
    @test loaded.edges[1].to   == "n2"

    rm(proj.root; recursive=true)
end

# ── Template validation (author-time, for every author that isn't the whiteboard) ──
# The whiteboard can't produce an invalid template; the REPL, a hand-edited file and Claude (via the
# MCP `create_chain`) can. Before this, a typo'd fn or a dangling edge surfaced only when the USER
# pressed Run — see the header comment in chain.jl → Template validation.
@testset "validate_chain_template" begin
    node(id, fn; kw...) = ChainNode(; id=id, fn=fn, kw...)
    ok_node(id) = node(id, "importImages.remove";
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))
    tpl(nodes, edges; starts=String[]) = ChainTemplate("t", nodes, edges, starts)

    # A well-formed template validates, and returns nothing (not a value to test against)
    @test validate_chain_template(
        tpl([ok_node("n1"), ok_node("n2")], [ChainEdge("n1", "n2")])) === nothing

    # SPARSE params must pass — this is how a non-GUI author writes a node: set only what you mean
    # to change and let the whiteboard fill the rest from the spec defaults on load (applyTemplate).
    # If this ever fails, Claude is forced to restate every default, which is the bug.
    @test validate_chain_template(
        tpl([node("n1", "tracking.bayesian_tracking";
                  params=Dict{String,Any}("maxSearchRadius"=>35))], ChainEdge[])) === nothing
    @test validate_chain_template(tpl([node("n1", "importImages.remove")], ChainEdge[])) === nothing

    bad(t) = @test_throws ChainTemplateError validate_chain_template(t)

    # Empty template validates — the whiteboard saves that as its first step when the user clicks
    # "new" and hasn't wired anything yet. run_chain refuses to actually run an empty one, which is
    # the check that matters.
    @test validate_chain_template(tpl(ChainNode[], ChainEdge[])) === nothing
    bad(tpl([node("", "importImages.remove")], ChainEdge[]))        # empty id
    bad(tpl([ok_node("n1"), ok_node("n1")], ChainEdge[]))           # duplicate id
    bad(tpl([node("n1", "importImages.nope")], ChainEdge[]))        # unknown fn
    # scope + barrier_policy — rejected at CONSTRUCTION now (enum types), not at validation.
    @test_throws ArgumentError node("n1", "importImages.remove"; scope="picnic")
    @test_throws ArgumentError node("n1", "importImages.remove"; barrier_policy="maybe")
    bad(tpl([node("n1", "importImages.remove"; resource_pool="gpu-light")], ChainEdge[]))

    # Both edge endpoints. A dangling `from` is a run-time KeyError in _topo_sort; a dangling `to`
    # silently never runs (in-degree never reaches 0) — the worse of the two, so both are errors.
    bad(tpl([ok_node("n1")], [ChainEdge("ghost", "n1")]))
    bad(tpl([ok_node("n1")], [ChainEdge("n1", "ghost")]))
    bad(tpl([ok_node("n1")], [ChainEdge("n1", "n1")]))              # self-dependency
    bad(tpl([ok_node("n1"), ok_node("n2")],                         # cycle
            [ChainEdge("n1", "n2"), ChainEdge("n2", "n1")]))
    bad(tpl([ok_node("n1")], ChainEdge[]; starts=["ghost"]))        # startTargets must be a node
    bad(tpl([node("n1", "tracking.bayesian_tracking";               # param out of spec range
                  params=Dict{String,Any}("maxSearchRadius"=>9999))], ChainEdge[]))

    # A configured pool name and the inherit-from-spec empty string are both fine
    @test validate_chain_template(
        tpl([node("n1", "importImages.remove"; resource_pool="gpu")], ChainEdge[])) === nothing
    @test validate_chain_template(
        tpl([node("n1", "importImages.remove"; resource_pool="")], ChainEdge[])) === nothing

    # The message names the offending node, so a rejected author knows what to fix
    err = try validate_chain_template(tpl([node("bad-one", "importImages.nope")], ChainEdge[]))
          catch e; e end
    @test err isa ChainTemplateError
    @test occursin("bad-one", err.msg) && occursin("importImages.nope", err.msg)

    # Roots = where a run begins. The whiteboard draws no start dot for a template with neither a
    # start target nor a saved position, so an authored chain gets its roots filled from this.
    @test chain_root_ids(tpl([ok_node("a"), ok_node("b")], [ChainEdge("a", "b")])) == ["a"]
    @test chain_root_ids(tpl([ok_node("a")], ChainEdge[])) == ["a"]
    # template order is preserved, and a fan-in has one root per unfed branch
    @test chain_root_ids(tpl([ok_node("a"), ok_node("b"), ok_node("c")],
                             [ChainEdge("a", "c"), ChainEdge("b", "c")])) == ["a", "b"]
    @test isempty(chain_root_ids(tpl([ok_node("a"), ok_node("b")],       # a cycle has no root
                                     [ChainEdge("a", "b"), ChainEdge("b", "a")])))
end

# ── Chain run — template frozen, per-image state, pipelining ─────────────
@testset "Chain run — end-to-end with RemoveImage" begin
    proj = create_project!(name="chain-run-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    # Two images, each with a registered filepath for RemoveImage to remove
    imgs = map(("img-a", "img-b")) do nm
        img = add_image!(s; name=nm)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = IMAGE_DONE
        save!(img)
        img
    end

    tpl = ChainTemplate(
        "remove-chain",
        [ChainNode(id="n1", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="remove-chain",
                    on_log = line -> push!(logs, line))

    # Run record persisted
    @test isfile(joinpath(run._dir, "run.json"))

    # Template frozen in-memory
    @test run.template_snapshot.name == "remove-chain"
    @test length(run.template_snapshot.nodes) == 1

    # Both images completed node n1
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
    end

    # on_log fired for both images
    @test !isempty(logs)

    # run.json stores hash reference, not embedded template
    raw = JSON3.read(read(joinpath(run._dir, "run.json"), String), Dict{String,Any})
    @test length(raw["image_uids"]) == 2
    @test haskey(raw["image_states"], imgs[1].uid)
    @test haskey(raw["image_states"], imgs[2].uid)
    @test haskey(raw, "template_hash")
    @test !isempty(raw["template_hash"])
    @test !haskey(raw, "template_snapshot")

    # Cache entry exists and round-trips back to the original template
    cached = load_template_from_cache(proj, run.template_hash)
    @test cached.name == "remove-chain"
    @test length(cached.nodes) == 1

    rm(proj.root; recursive=true)
end

# ── Chain resume — explicit start node (re-run from here) ─────────────────
@testset "Chain resume — start node force-restart" begin
    # descendants: pure graph reachability over n1→n2→n3
    tpl = ChainTemplate(
        "restart-chain",
        [ChainNode(id="n1", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n3", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    @test Cecelia._descendants(tpl, "n1") == Set(["n2","n3"])
    @test Cecelia._descendants(tpl, "n2") == Set(["n3"])
    @test isempty(Cecelia._descendants(tpl, "n3"))

    # force-restart from n2 on a run whose nodes are all :done → n2,n3 reset to :pending; n1 kept
    proj = create_project!(name="chain-restart-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.status = IMAGE_DONE; save!(img)
    states = Dict(img.uid => Dict(
        "n1" => Cecelia.ImageNodeState(), "n2" => Cecelia.ImageNodeState(),
        "n3" => Cecelia.ImageNodeState()))
    for nid in ("n1","n2","n3")
        states[img.uid][nid].status      = NODE_DONE
        states[img.uid][nid].params_hash = "h"
    end
    run = Cecelia.ChainRun("rid", "restart-chain", proj.uid, [img.uid], tpl,
                           "hash", states, time(), joinpath(Cecelia._runs_dir(proj), "rid"),
                           ReentrantLock(), Dict{String,Channel{Nothing}}(),
                           Dict{String,Channel{Nothing}}())
    mkpath(run._dir)
    Cecelia._force_restart_from!(run, "n2")
    @test run.image_states[img.uid]["n1"].status == NODE_DONE       # upstream untouched
    @test run.image_states[img.uid]["n2"].status == NODE_PENDING    # start node reset
    @test run.image_states[img.uid]["n3"].status == NODE_PENDING    # downstream reset
    @test run.image_states[img.uid]["n2"].params_hash === nothing

    rm(proj.root; recursive=true)
end

# ── Chain start dot — prune to the reachable subgraph ─────────────────────
@testset "Chain start dot — prune to reachable subgraph" begin
    tpl = ChainTemplate(
        "start-chain",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="d", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="x", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="y", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d"), ChainEdge("x","y")],
        ["c"],                                          # start dot → c (mid-chain)
    )
    pruned = Cecelia._prune_to_start(tpl)
    @test Set(n.id for n in pruned.nodes) == Set(["c","d"])        # c + downstream only
    @test Set((e.from, e.to) for e in pruned.edges) == Set([("c","d")])
    @test isempty(pruned.start_targets)                            # consumed into the node set
    # disconnected draft branch (x→y) dropped; a→b upstream of the start dot dropped too
    @test !any(n.id in ("a","b","x","y") for n in pruned.nodes)
    # no start targets ⇒ unchanged (run the whole chain)
    @test length(Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges)).nodes) == 6
    # start dot pointing ONLY at since-deleted nodes ⇒ fall back to run-all, not an empty run
    stale = Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges, ["ghost"]))
    @test length(stale.nodes) == 6
    @test isempty(stale.start_targets)
end

# ── Chain run — set-scope (picnic) node ──────────────────────────────────
@testset "Chain run — picnic node" begin
    proj = create_project!(name="chain-picnic-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm
        add_image!(s; name=nm)
    end

    # n1 (image) → n2 (set-scope) → n3 (image)
    tpl = ChainTemplate(
        "picnic-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="picnic-chain",
                    on_log = line -> push!(logs, line))

    # All per-image nodes completed for every image
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test run.image_states[img.uid]["n3"].status == NODE_DONE
    end

    # Set-scope node: all images show :done, result contains the full image count
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
        @test run.image_states[img.uid]["n2"].result["image_count"] == 3
    end

    # Set-scope task log appeared exactly once (ran once, not once per image)
    set_logs = filter(l -> contains(l, "setTask ran"), logs)
    @test length(set_logs) == 1

    rm(proj.root; recursive=true)
end

# ── Chain start dot — end-to-end run prunes to the reachable subgraph ─────
# Reservation guard: pruning to a start dot must still work when the reachable subgraph contains a
# picnic (set-scope barrier) node. The target becomes a root — its dropped upstream doesn't block
# it — and the barrier still fires once across all images. Also covers the save/load round-trip of
# `startTargets` and that pruned-out nodes never enter the run.
@testset "Chain start dot — run prunes to subgraph (set-scope)" begin
    proj = create_project!(name="chain-startrun-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm); end

    # n1(image) → n2(set) → n3(image); start dot → n2, so n1 is an upstream draft (excluded)
    tpl = ChainTemplate(
        "startrun-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        ["n2"],                                        # start dot → n2 (a set-scope node)
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs]; chain="startrun-chain",
                    on_log = line -> push!(logs, line))

    # only the reachable subgraph exists in the run — n1 pruned out entirely
    @test Set(keys(run.image_states[imgs[1].uid])) == Set(["n2", "n3"])
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == NODE_DONE   # set-scope barrier fired as root
        @test run.image_states[img.uid]["n3"].status == NODE_DONE
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
    @test length(filter(l -> contains(l, "setTask ran"), logs)) == 1   # ran once, not per image

    rm(proj.root; recursive=true)
end

# ── Fault isolation is per-predecessor, not global (DAG fan-out) ─────────────
# A failed branch must not skip a SIBLING branch that shares only an upstream ancestor
# (e.g. driftCorrect → two independent segmentations). Regression guard for the
# over-broad "any node failed → skip" check that skipped independent branches.
@testset "Chain fault isolation — independent fan-out" begin
    proj = create_project!(name="chain-fanout-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # a → { b (fails), c (ok) } — c is independent of b, must still run
    save_chain_template!(proj, ChainTemplate("fanout",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("a","c")]))
    st = run_chain(proj, [img.uid]; chain="fanout").image_states[img.uid]
    @test st["a"].status == NODE_DONE
    @test st["b"].status == NODE_FAILED
    @test st["c"].status == NODE_DONE       # independent sibling — NOT skipped by b's failure

    # a (fails) → { b, c } — the shared ancestor failing skips BOTH branches
    save_chain_template!(proj, ChainTemplate("fanout-root-fail",
        [ChainNode(id="a", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("a","c")]))
    st2 = run_chain(proj, [img.uid]; chain="fanout-root-fail").image_states[img.uid]
    @test st2["a"].status == NODE_FAILED
    @test st2["b"].status == NODE_SKIPPED
    @test st2["c"].status == NODE_SKIPPED

    # transitive: a → b(fail) → c → d — skip propagates down the branch via :skipped
    save_chain_template!(proj, ChainTemplate("chain-transitive",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="d", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d")]))
    st3 = run_chain(proj, [img.uid]; chain="chain-transitive").image_states[img.uid]
    @test st3["a"].status == NODE_DONE
    @test st3["b"].status == NODE_FAILED
    @test st3["c"].status == NODE_SKIPPED   # pred b failed
    @test st3["d"].status == NODE_SKIPPED   # pred c skipped → propagates

    rm(proj.root; recursive=true)
end

# ── Picnic node — require_all aborts if any image failed upstream ────────
@testset "Picnic node — require_all policy" begin
    proj = create_project!(name="picnic-req-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1 is a bad fn — both images will fail there
    # n2 is require_all — should abort since n1 failed
    tpl = ChainTemplate(
        "req-chain",
        [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="require_all")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="req-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_FAILED
        @test run.image_states[img.uid]["n2"].status == NODE_FAILED
    end
    rm(proj.root; recursive=true)
end

# ── Picnic node — successful_only aborts when no images eligible ─────────
@testset "Picnic node — successful_only policy" begin
    proj = create_project!(name="picnic-ok-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1: bad fn — both images fail upstream
    # n2 (successful_only): zero eligible images → should abort, both :failed
    tpl = ChainTemplate(
        "ok-chain",
        [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="successful_only")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="ok-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_FAILED
        # no eligible images → set-scope node also fails
        @test run.image_states[img.uid]["n2"].status == NODE_FAILED
    end
    rm(proj.root; recursive=true)
end

# ── Picnic node — successful_only runs with eligible subset ──────────────
@testset "Picnic node — successful_only with all passing" begin
    proj = create_project!(name="picnic-pass-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1: always succeeds → both images eligible → task runs with all 2
    tpl = ChainTemplate(
        "pass-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="successful_only")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="pass-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
    rm(proj.root; recursive=true)
end

# ── Chain run — overrides applied, bad fn isolated ────────────────────────
@testset "Chain run — overrides + fault isolation" begin
    proj = create_project!(name="chain-iso-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    imgs = map(("img-a", "img-b")) do nm
        img = add_image!(s; name=nm)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = IMAGE_DONE
        save!(img)
        img
    end

    # n1: bad fn (will fail for both images)
    # n2: valid fn — should be skipped because n1 failed
    tpl = ChainTemplate(
        "fault-chain",
        [ChainNode(id="n1", fn="nonexistent.task",  params=Dict{String,Any}()),
         ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="fault-chain",
                    on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_FAILED
        @test run.image_states[img.uid]["n2"].status == NODE_SKIPPED
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — load_chain_run round-trips state ──────────────────────
@testset "Chain resume — load_chain_run round-trip" begin
    proj = create_project!(name="chain-resume-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "resume-rt-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="resume-rt-chain", on_log=_->nothing)

    # Verify states are :done with a params_hash stored
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test !isnothing(run.image_states[img.uid]["n1"].params_hash)
    end

    # Load the run from disk and verify state is preserved
    loaded = load_chain_run(proj, run.id)
    @test loaded.id == run.id
    @test loaded.chain_name == "resume-rt-chain"
    @test length(loaded.image_uids) == 2
    for img in imgs
        @test loaded.image_states[img.uid]["n1"].status == NODE_DONE
        @test loaded.image_states[img.uid]["n1"].params_hash == run.image_states[img.uid]["n1"].params_hash
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — already-done nodes are skipped ────────────────────────
@testset "Chain resume — skip unchanged done nodes" begin
    proj = create_project!(name="chain-resume-skip-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "skip-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    # First run — collects logs
    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs];
                     chain="skip-chain", on_log=line->push!(logs1, line))
    @test !isempty(logs1)

    # Second run via run_id — same params, no work to do
    logs2 = String[]
    run2 = run_chain(proj, String[];
                     run_id=run1.id, on_log=line->push!(logs2, line))

    # No new log lines because all nodes were skipped
    @test isempty(logs2)
    # States still :done
    for img in imgs
        @test run2.image_states[img.uid]["n1"].status == NODE_DONE
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — params change triggers re-run ──────────────────────────
@testset "Chain resume — params change re-runs node" begin
    proj = create_project!(name="chain-resume-rerun-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "rerun-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("message" => "first"))],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    run1 = run_chain(proj, [i.uid for i in imgs]; chain="rerun-chain", on_log=_->nothing)
    @test run1.image_states[imgs[1].uid]["n1"].result["image"] == imgs[1].name

    # Resume with overridden message param — node must re-run
    logs2 = String[]
    run2 = run_chain(proj, String[];
                     run_id=run1.id,
                     overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "second")),
                     on_log=line->push!(logs2, line))

    @test !isempty(logs2)   # node re-ran and produced logs
    @test run2.image_states[imgs[1].uid]["n1"].status == NODE_DONE

    rm(proj.root; recursive=true)
end

# ── Step 5: Resume from mid-chain failure — only failed/downstream nodes rerun ─
@testset "Resume — failure at node 3 does not redo nodes 1-2" begin
    proj = create_project!(name="resume-fail-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    # n1 → n2 → n3(bad) → n4(skipped)
    tpl = ChainTemplate(
        "fail-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n3", fn="nonexistent.task",   scope="image", params=Dict{String,Any}()),
         ChainNode(id="n4", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="fail-chain",
                     on_log=line->push!(logs1, line))

    uid = imgs[1].uid
    @test run1.image_states[uid]["n1"].status == NODE_DONE
    @test run1.image_states[uid]["n2"].status == NODE_DONE
    @test run1.image_states[uid]["n3"].status == NODE_FAILED
    @test run1.image_states[uid]["n4"].status == NODE_SKIPPED

    # Resume — n1/n2 are :done with unchanged params → must be skipped
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     on_log=line->push!(logs2, line))

    # n1 and n2 produced no new log lines — they were skipped
    n1_logs_run1 = count(l -> contains(l, uid*"/n1"), logs1)
    n2_logs_run1 = count(l -> contains(l, uid*"/n2"), logs1)
    n1_logs_run2 = count(l -> contains(l, uid*"/n1"), logs2)
    n2_logs_run2 = count(l -> contains(l, uid*"/n2"), logs2)
    @test n1_logs_run1 > 0    # ran in first pass
    @test n2_logs_run1 > 0    # ran in first pass
    @test n1_logs_run2 == 0   # skipped on resume
    @test n2_logs_run2 == 0   # skipped on resume
    # n3 still fails (fn still missing), n4 still skipped
    @test run2.image_states[uid]["n1"].status == NODE_DONE
    @test run2.image_states[uid]["n2"].status == NODE_DONE
    @test run2.image_states[uid]["n3"].status == NODE_FAILED
    @test run2.image_states[uid]["n4"].status == NODE_SKIPPED

    rm(proj.root; recursive=true)
end

# ── Step 5: Params change on node 4 — only n4 and downstream rerun ──────
@testset "Resume — params change on n4 reruns only n4 and downstream" begin
    proj = create_project!(name="resume-p4-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "p4-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n4", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n5", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4"), ChainEdge("n4","n5")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="p4-chain",
                     on_log=line->push!(logs1, line))
    uid = imgs[1].uid
    for n in ("n1","n2","n3","n4","n5")
        @test run1.image_states[uid][n].status == NODE_DONE
    end

    # Resume with n4 params changed via override → n1,n2,n3 skip; n4,n5 rerun
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     overrides=Dict{String,Any}("n4" => Dict{String,Any}("message" => "changed")),
                     on_log=line->push!(logs2, line))

    for n in ("n1","n2","n3") # not re-run
        @test count(l -> contains(l, uid*"/$n"), logs2) == 0
    end
    for n in ("n4","n5") # re-run
        @test count(l -> contains(l, uid*"/$n"), logs2) > 0
    end
    for n in ("n1","n2","n3","n4","n5")
        @test run2.image_states[uid][n].status == NODE_DONE
    end

    rm(proj.root; recursive=true)
end

# ── Step 5: Picnic node restarts when per-image upstream input changes ───
@testset "Resume — picnic node restarts when upstream stale" begin
    proj = create_project!(name="resume-picnic-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(set) → n3(image)
    tpl = ChainTemplate(
        "picnic-resume-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="picnic-resume-chain",
                     on_log=line->push!(logs1, line))
    for img in imgs
        @test run1.image_states[img.uid]["n1"].status == NODE_DONE
        @test run1.image_states[img.uid]["n2"].status == NODE_DONE
        @test run1.image_states[img.uid]["n3"].status == NODE_DONE
    end

    # setTask log appeared once in run 1
    set_logs1 = filter(l -> contains(l, "setTask ran"), logs1)
    @test length(set_logs1) == 1

    # Resume with n1 params changed → n1 stale → n2 (picnic) stale → n3 stale
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "new")),
                     on_log=line->push!(logs2, line))

    # Picnic re-ran (set log appeared again)
    set_logs2 = filter(l -> contains(l, "setTask ran"), logs2)
    @test length(set_logs2) == 1   # ran exactly once in this resume run
    # All nodes redone
    for img in imgs
        @test run2.image_states[img.uid]["n1"].status == NODE_DONE
        @test run2.image_states[img.uid]["n2"].status == NODE_DONE
        @test run2.image_states[img.uid]["n3"].status == NODE_DONE
    end

    rm(proj.root; recursive=true)
end

# ── Step 6: Incremental plot node fires as images complete ───────────────
@testset "Incremental plot node — fires and sets :done state" begin
    proj = create_project!(name="incr-plot-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(incremental plot)
    # debounce_ms=10 so it fires quickly in the test
    tpl = ChainTemplate(
        "incr-chain",
        [ChainNode(id="n1", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.incremental_plot_task", scope="incremental",
                   params=Dict{String,Any}("debounce_ms" => 10))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="incr-chain", on_log=line->push!(logs, line))

    # All per-image n1 nodes succeeded
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
    end

    # Incremental plot ran — all images' n2 state is :done
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
    end

    # Plot log appeared at least once
    plot_logs = filter(l -> contains(l, "incr/n2"), logs)
    @test !isempty(plot_logs)

    rm(proj.root; recursive=true)
end

# ── Step 6: Incremental node does not block per-image progression ────────
@testset "Incremental plot node — image threads not blocked" begin
    proj = create_project!(name="incr-nob-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(incremental) → n3(image) — n3 should still run
    # (incremental nodes don't gate downstream per-image nodes)
    tpl = ChainTemplate(
        "incr-pass-chain",
        [ChainNode(id="n1", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.incremental_plot_task", scope="incremental",
                   params=Dict{String,Any}("debounce_ms" => 10))],
        [ChainEdge("n1", "n2"), ChainEdge("n1", "n3")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="incr-pass-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test run.image_states[img.uid]["n3"].status == NODE_DONE
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
    end

    rm(proj.root; recursive=true)
end

# ── Step 6: Event bus subscribe/unsubscribe ───────────────────────────────
@testset "Event bus — subscribe and receive node:done events" begin
    proj = create_project!(name="evbus-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "evbus-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    received = String[]
    payloads = Any[]
    handler  = payload -> (push!(received, payload.image_uid); push!(payloads, payload))
    subscribe_chain_events!("node:done", handler)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="evbus-chain", on_log=_->nothing)

    unsubscribe_chain_events!("node:done", handler)

    # Both images fired node:done events
    @test Set(received) ⊇ Set(i.uid for i in imgs)

    # …and every payload carries the scheduler `task_id` the node ran as, matching the state it
    # was recorded under. This is the correlation handle the task console needs: a chain run emits
    # no `task:status` frames, so without it a finished node can only be reported as "outcome
    # unseen". Never `nothing` — absent ⇒ "" (see _update_node_state!).
    for p in payloads
        @test haskey(p, :task_id)
        @test p.task_id isa String
        @test p.task_id == run.image_states[p.image_uid][p.node_id].task_id
        @test !isempty(p.task_id)                     # this node ran, so it has one
    end

    # After unsubscribe, new events don't reach the handler
    n_before = length(received)
    run2 = run_chain(proj, [imgs[1].uid]; chain="evbus-chain", on_log=_->nothing)
    @test length(received) == n_before  # unchanged

    rm(proj.root; recursive=true)
end

# ── Step 7: Resource pool — concurrency limit respected ──────────────────
# Pool limit = 1 on n1. Three images each sleep 40ms in n1. With one worker the
# node executions serialise, so total wall time ≥ 3×40ms (parallel would be ~40ms).
# (Wall-clock, not event counting: node:running now fires from the pool worker and
# node:done from the image thread, so a size-1 pool has a benign running/done
# handoff overlap — execution is still serial, which the timing assertion proves.)
@testset "Resource pool — concurrency limit respected" begin
    proj = create_project!(name="pool-limit-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pool-limit-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 40),
                   resource_pool="slow_pool"),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    # Pools are global (scheduler.jl _POOLS); register the test pool at limit 1.
    resize_pool!("slow_pool", 1)
    t0  = time()
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pool-limit-chain",
                    on_log=_->nothing)
    elapsed = time() - t0

    # Serialised: ≥ 3×40ms of n1 work. Parallel would finish in ~40-60ms.
    @test elapsed >= 0.10
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
    end

    rm(proj.root; recursive=true)
end

# ── Step 7: Resource pool — higher limit allows parallel execution ────────
@testset "Resource pool — limit=3 allows all concurrent" begin
    proj = create_project!(name="pool-par-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pool-par-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 40),
                   resource_pool="par_pool")],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    max_concurrent = Threads.Atomic{Int}(0)
    current        = Threads.Atomic{Int}(0)

    sh = payload -> begin
        payload.node_id == "n1" || return
        n = Threads.atomic_add!(current, 1) + 1
        Threads.atomic_max!(max_concurrent, n)
        nothing
    end
    dh = payload -> (payload.node_id == "n1" && Threads.atomic_sub!(current, 1); nothing)

    subscribe_chain_events!("node:running", sh)
    subscribe_chain_events!("node:done",    dh)

    resize_pool!("par_pool", 3)
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pool-par-chain",
                    on_log=_->nothing)

    unsubscribe_chain_events!("node:running", sh)
    unsubscribe_chain_events!("node:done",    dh)

    # With limit 3 and 3 images all able to run simultaneously, max should be 3
    @test max_concurrent[] == 3

    rm(proj.root; recursive=true)
end

# ── Dynamic resize: throttle UP re-parallelises the already-queued backlog ─────
# One persistent queue per pool (not a swap): grow spawns workers that pick up the backlog.
# Start at limit 1 (1 running, 3 queued), throttle to 4 mid-run → the 3 queued fan out, so the
# observed concurrency rises above 1. A queue-swap (the old bug) would leave them serial at 1.
@testset "Pool throttle-up parallelises the backlog" begin
    proj = create_project!(name="pool-grow-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("a", "b", "c", "d")) do nm; add_image!(s; name=nm) end
    tpl = ChainTemplate("pool-grow-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 300), resource_pool="dyngrow")],
        ChainEdge[])
    save_chain_template!(proj, tpl)

    max_concurrent = Threads.Atomic{Int}(0)
    current        = Threads.Atomic{Int}(0)
    sh = payload -> begin
        payload.node_id == "n1" || return
        n = Threads.atomic_add!(current, 1) + 1
        Threads.atomic_max!(max_concurrent, n); nothing
    end
    dh = payload -> (payload.node_id == "n1" && Threads.atomic_sub!(current, 1); nothing)
    subscribe_chain_events!("node:running", sh)
    subscribe_chain_events!("node:done",    dh)

    resize_pool!("dyngrow", 1)                              # throttled to 1
    runner = Threads.@spawn run_chain(proj, [i.uid for i in imgs];
                                      chain="pool-grow-chain", on_log=_->nothing)
    sleep(0.12)                                             # 1 running, 3 queued
    resize_pool!("dyngrow", 4)                              # throttle up mid-run
    wait(runner)

    unsubscribe_chain_events!("node:running", sh)
    unsubscribe_chain_events!("node:done",    dh)

    @test max_concurrent[] >= 3                             # backlog fanned out (was serial at 1)
    rm(proj.root; recursive=true)
end

# ── Dynamic resize: throttle DOWN settles to the new limit (never oversubscribes) ──
# Start wide (4), throttle to 1 just after the first batch is admitted. The first 4 run
# concurrently (~150ms), then the remaining 4 serialise at limit 1 (~4×150ms) → ≳0.6s total.
# If the shrink hadn't taken (stayed at 4), all 8 would finish in ~2×150 = 300ms. Wall-clock
# (per the existing pool tests) — the slot gate is checked at execution time, so the tail
# serialises even though the first 4 were admitted while the limit was still 4.
@testset "Pool throttle-down settles to the new limit" begin
    proj = create_project!(name="pool-shrink-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(i -> add_image!(s; name="img-$i"), 1:8)
    tpl = ChainTemplate("pool-shrink-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 150), resource_pool="dynshrink")],
        ChainEdge[])
    save_chain_template!(proj, tpl)

    resize_pool!("dynshrink", 4)
    t0 = time()
    runner = Threads.@spawn run_chain(proj, [i.uid for i in imgs];
                                      chain="pool-shrink-chain", on_log=_->nothing)
    sleep(0.05)                                             # first 4 admitted
    resize_pool!("dynshrink", 1)                            # throttle down mid-run
    wait(runner)
    elapsed = time() - t0

    @test elapsed >= 0.55                                   # tail serialised at 1 (not stuck at 4)
    rm(proj.root; recursive=true)
end

# ── Step 7: Pipelining — image A reaches n2 before image B finishes n1 ───
# Pool limit=1 on n1 (each image sleeps 80ms there). Image A exits n1 first
# and immediately enters n2 (instant). B and C are still queuing for n1.
# Verify: A's n2 completion timestamp < B's n1 completion timestamp.
@testset "Pipelining — n2 of first image before n1 of second image" begin
    proj = create_project!(name="pipeline-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pipeline-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 80),
                   resource_pool="serial_pool"),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 0))],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    done_times = Dict{String,Float64}()  # "uid/nid" => time()
    th = payload -> (done_times["$(payload.image_uid)/$(payload.node_id)"] = time(); nothing)
    subscribe_chain_events!("node:done", th)

    resize_pool!("serial_pool", 1)
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pipeline-chain",
                    on_log=_->nothing)

    unsubscribe_chain_events!("node:done", th)

    # Find the image that finished n1 first (earliest n1 completion)
    first_uid  = argmin(uid -> done_times["$(uid)/n1"], [i.uid for i in imgs])
    other_uids = filter(i -> i.uid != first_uid, imgs)

    # The first image's n2 must have finished before any other image's n1 did
    t_first_n2 = done_times["$(first_uid)/n2"]
    for other in other_uids
        @test t_first_n2 < done_times["$(other.uid)/n1"]
    end

    rm(proj.root; recursive=true)
end

# ── Step 7: Cross-image fault isolation ──────────────────────────────────
# Image A fails at n1. Images B and C proceed through n1→n2 unaffected.
# (Different from Step 5's test which checks downstream skips on the SAME image.)
#
# We make n1 = RemoveImage, which requires a registered zarr to succeed.
# img_b and img_c have a real zarr; img_a does not → img_a fails at n1.
# n2 = testTasks.image_task (always succeeds).
# Expected: img_a fails n1, skips n2. img_b and img_c succeed both nodes.
@testset "Cross-image fault isolation" begin
    proj = create_project!(name="xiso-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img_a = add_image!(s; name="img-a")   # no zarr → RemoveImage will fail
    img_b = add_image!(s; name="img-b")
    img_c = add_image!(s; name="img-c")

    for img in (img_b, img_c)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = IMAGE_DONE
        save!(img)
    end

    tpl = ChainTemplate(
        "xiso-chain",
        [ChainNode(id="n1", fn="importImages.remove", scope="image",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [img_a.uid, img_b.uid, img_c.uid];
                    chain="xiso-chain", on_log=_->nothing)

    # img_a: n1 failed (no zarr), n2 skipped
    @test run.image_states[img_a.uid]["n1"].status == NODE_FAILED
    @test run.image_states[img_a.uid]["n2"].status == NODE_SKIPPED

    # img_b and img_c: both nodes succeeded — not affected by img_a's failure
    @test run.image_states[img_b.uid]["n1"].status == NODE_DONE
    @test run.image_states[img_b.uid]["n2"].status == NODE_DONE
    @test run.image_states[img_c.uid]["n1"].status == NODE_DONE
    @test run.image_states[img_c.uid]["n2"].status == NODE_DONE

    rm(proj.root; recursive=true)
end

# ── Step 7: run_chain headless — no api/ loaded ───────────────────────────
# All tests in this file run without `using` api/. This testset makes the
# contract explicit: run_chain on a picnic chain produces correct results
# with nothing but `using Cecelia`.
@testset "run_chain headless (no api/ dependency)" begin
    proj = create_project!(name="headless-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "headless-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="headless-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == NODE_DONE
        @test run.image_states[img.uid]["n2"].status == NODE_DONE
        @test run.image_states[img.uid]["n3"].status == NODE_DONE
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2

    rm(proj.root; recursive=true)
end

