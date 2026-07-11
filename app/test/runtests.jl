using Cecelia
using Test
using JSON3
import DataFrames: DataFrame, nrow   # only the symbols the tests construct/measure with

# ── Smoke tests — no API, no WebSocket, no Vue ────────────────────────────────
# Run from the app/ directory:  julia --project test/runtests.jl
# Requires CECELIA_DEV_DIR to point at a config with a valid projects_dir.

init_cecelia!()

# ── Test data fixtures ────────────────────────────────────────────────────────
# Version-controlled fixtures live at <workspace-root>/test-data/projects, independent
# of the deletable dev projects dir (`projects_dir()`). Override with CECELIA_TEST_DATA.
# (@__DIR__ = app/test → ../../.. = workspace root.)  See test-data/README.md.
test_projects_dir() = get(ENV, "CECELIA_TEST_DATA",
    normpath(joinpath(@__DIR__, "..", "..", "..", "test-data", "projects")))

"""Absolute path to a fixture under test-data/projects (no existence check)."""
fixture_path(relparts...) = joinpath(test_projects_dir(), relparts...)

const _WARNED_FIXTURES = Set{String}()

"""
    have_fixture(path) -> Bool

True if the fixture exists. If not, emit a single strong warning (once per path) and
return false so the caller can `@test_skip`. Generic — works for any fixture file.
"""
function have_fixture(path::AbstractString)::Bool
    isfile(path) && return true
    if !(path in _WARNED_FIXTURES)
        push!(_WARNED_FIXTURES, path)
        @warn """
        ╔══════════════════════════════════════════════════════════════════════════╗
        ║  TEST FIXTURE MISSING — dependent tests will be SKIPPED.                   ║
        ╚══════════════════════════════════════════════════════════════════════════╝
        Expected: $path
        Tests that assert against real data are skipped without it, leaving that path
        unverified. Restore test-data/ (see test-data/README.md) or set CECELIA_TEST_DATA
        to a projects dir containing the file above."""
    end
    false
end

@testset "Cecelia package smoke tests" begin

    # ── Config ────────────────────────────────────────────────────────────────
    @testset "Config" begin
        @test !isempty(projects_dir())
        @test !isempty(python_bin_path())
        @test tasks_concurrent_limit() >= 1
        @test napari_discrete_gpu() isa Bool   # [napari].discreteGpu, default false
    end

    # ── Napari discrete-GPU launch env ──────────────────────────────────────────
    # The bridge command gains the offload env only when discrete_gpu is on (Linux). DRI_PRIME is
    # always applied (safe on single-GPU); the NVIDIA GLX vendor var only when NVIDIA is present.
    @testset "Napari discrete-GPU command" begin
        plain = Cecelia._bridge_cmd(false)
        @test plain.env === nothing                          # no env override → inherits parent
        gpu = Cecelia._bridge_cmd(true)
        if Sys.islinux()
            @test gpu.env !== nothing
            @test any(==("DRI_PRIME=1"), gpu.env)             # always safe → always applied
            # nvidia GLX vendor var is gated on detection (forcing it without NVIDIA breaks GL)
            has_nvidia = any(startswith("__GLX_VENDOR_LIBRARY_NAME=nvidia"), gpu.env)
            @test has_nvidia == Cecelia._nvidia_present()
        else
            @test gpu.env === nothing                         # no-op off Linux
        end
    end

    # ── Model: create project and image ───────────────────────────────────────
    @testset "Model round-trip" begin
        proj = create_project!(name="smoke-test-$(rand(1000:9999))", kind="static")
        @test isdir(proj.root)
        @test isfile(joinpath(proj.root, "project.json"))

        s = add_set!(proj; name="set-A")
        @test isdir(s._dir)

        img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
        @test isdir(img._dir)
        @test isfile(joinpath(img._dir, "ccid.json"))

        # Round-trip via init_object
        loaded = init_object(proj.uid, img.uid)
        @test loaded isa CciaImage
        @test loaded.name == "img-1"
        @test get(loaded.meta, "ori_path", "") == "/tmp/fake.tif"

        # Cleanup
        rm(proj.root; recursive=true)
    end

    # ── Lockfile (naive guard) ──────────────────────────────────────────────────
    @testset "with_transaction" begin
        proj     = create_project!(name="lock-test-$(rand(1000:9999))", kind="static")
        lockfile = joinpath(proj.root, ".cecelia.lock")

        # happy path: returns the body value and releases the lock
        @test (with_transaction(proj) do; 42; end) == 42
        @test !isfile(lockfile)

        # lock is released even when the body throws (no leaked lock)
        @test_throws ErrorException with_transaction(proj) do; error("boom"); end
        @test !isfile(lockfile)

        rm(proj.root; recursive=true)
    end

    # ── Param validation ──────────────────────────────────────────────────────
    @testset "Param validation" begin
        task = ImportOmezarr()

        # Valid params — should not throw
        @test begin
            validate_params(task, Dict{String,Any}("pyramidScale" => 2))
            true
        end

        # Out of range
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("pyramidScale" => 99))

        # Wrong type (string where int expected)
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("pyramidScale" => "not-a-number"))

        # Validation enforced by run_task itself — not just validate_params.
        # Use TestImageTask to confirm _run_task dispatch works.
        # Then confirm ImportOmezarr's run_task rejects bad params before reaching _run_task.
        proj2 = create_project!(name="val-test-$(rand(1000:9999))", kind="static")
        s2 = add_set!(proj2; name="s")
        img2 = add_image!(s2; name="img", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
        @test_throws ParamValidationError run_task(
            ImportOmezarr(), img2, Dict{String,Any}("pyramidScale" => 99))
        rm(proj2.root; recursive=true)
    end

    # ── Param validation — CellposeCorrect (constraints live inside a `group`) ───
    @testset "Param validation — CellposeCorrect" begin
        # modelDiameter is int min=1/max=100, nested in the `models` group
        out_of_range = Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model"=>"denoise_cyto3", "modelChannels"=>["DAPI"], "modelDiameter"=>500)))
        @test_throws ParamValidationError validate_params(CellposeCorrect(), out_of_range)

        # model is a select — an unknown value must be rejected
        bad_select = Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model"=>"not_a_model", "modelChannels"=>["DAPI"], "modelDiameter"=>30)))
        @test_throws ParamValidationError validate_params(CellposeCorrect(), bad_select)

        # NOTE: RemoveImage has only valueNameSelection params (no scalar constraints),
        # so there is nothing for validate_params to reject — no test is meaningful there.
    end

    # ── Dispatch + param validation — ClustPops (clustPops.cluster, set-scope) ───
    @testset "Param validation — ClustPops" begin
        @test _task_from_fun_name("clustPops.cluster") isa ClustPops
        @test task_scope(ClustPops()) == "set"
        # resolution is float min=0/max=5 — out of range must be rejected
        @test_throws ParamValidationError validate_params(
            ClustPops(), Dict{String,Any}("resolution" => 99))
        # wrong type where float expected
        @test_throws ParamValidationError validate_params(
            ClustPops(), Dict{String,Any}("resolution" => "not-a-number"))
    end

    # ── Section-param flattening (whiteboard/chain stores section params NESTED) ───
    # Regression: a chain node saves `section` params under the section key (e.g.
    # measureOptions => {extendedMeasures: true}), but tasks read them flat. run_task must lift them.
    @testset "Section params flatten (chain nesting)" begin
        ml = _task_from_fun_name("segment.measureLabels")
        # measureLabels declares `measureOptions` + `imageTiling` sections
        @test "measureOptions" in Cecelia._section_keys(ml)
        @test "imageTiling"    in Cecelia._section_keys(ml)
        nested = Dict{String,Any}(
            "outputValueName" => "T",
            "measureOptions"  => Dict{String,Any}("extendedMeasures" => true, "saveMeshes" => false),
            "imageTiling"     => Dict{String,Any}("blockSize" => 4096, "overlap" => 0))
        flat = Cecelia._flatten_sections(ml, nested)
        @test flat["extendedMeasures"] == true          # was buried under measureOptions
        @test flat["blockSize"] == 4096                  # was buried under imageTiling
        @test !haskey(flat, "measureOptions")            # section container dropped
        @test flat["outputValueName"] == "T"             # top-level survives
        # composite pulls section keys from its sub-tasks (cellpose + measureLabels)
        comp = _task_from_fun_name("segment.cellposeMeasure")
        @test "measureOptions" in Cecelia._section_keys(comp)
        @test Cecelia._flatten_sections(comp,
            Dict{String,Any}("measureOptions" => Dict{String,Any}("extendedMeasures" => true)))["extendedMeasures"] == true
        # already-flat params are unchanged (idempotent)
        @test Cecelia._flatten_sections(ml, Dict{String,Any}("extendedMeasures" => true))["extendedMeasures"] == true
    end

    # ── Dispatch + param validation — ClustTracks (clustTracks.cluster, set-scope) ───
    @testset "Param validation — ClustTracks" begin
        @test _task_from_fun_name("clustTracks.cluster") isa ClustTracks
        @test task_scope(ClustTracks()) == "set"
        # resolution is float min=0/max=5 — out of range must be rejected
        @test_throws ParamValidationError validate_params(
            ClustTracks(), Dict{String,Any}("resolution" => 99))
        # wrong type where float expected
        @test_throws ParamValidationError validate_params(
            ClustTracks(), Dict{String,Any}("resolution" => "not-a-number"))
    end

    # ── Image round-trip (status + attr) ────────────────────────────────────────
    # Regression guard: save!(img) must persist status and attr, not silently drop them.
    @testset "Image status/attr round-trip" begin
        proj = create_project!(name="rt-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        img.status = "done"
        img.attr["condition"] = "treated"
        save!(img)
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test r.status == "done"
        @test get(r.attr, "condition", "") == "treated"
        rm(proj.root; recursive=true)
    end

    # ── Include/exclude (+ note) round-trip ──────────────────────────────────────
    # Guards: new images default to included; excluded flag + note survive save!/init_object; and a
    # legacy ccid.json with neither field loads as included (the accessor never sees a missing field).
    @testset "Image included/note round-trip" begin
        proj = create_project!(name="incl-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        @test image_included(img)                 # default: included
        @test img.note == ""

        img.included = false
        img.note = "bad drift reference channel"
        save!(img)
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test !image_included(r)
        @test r.note == "bad drift reference channel"

        # legacy file (no included/note keys) → defaults to included, empty note
        ccid = joinpath(r._dir, "ccid.json")
        raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
        delete!(raw, "included"); delete!(raw, "note")
        open(ccid, "w") do io; JSON3.write(io, raw); end
        legacy = init_object(proj.uid, img.uid)
        @test image_included(legacy)
        @test legacy.note == ""
        rm(proj.root; recursive=true)
    end

    # ── Per-task param memory (funParams) — R moduleFunParams parity ─────────────
    # Last-used params are remembered in ccid.json under meta["funParams"][fun], per image and per
    # set. Guards: round-trips through save!/init_object, per-fun keys don't clobber, set-level too.
    @testset "funParams per-object memory" begin
        proj = create_project!(name="fp-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        save!(img)

        @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect") === nothing  # absent

        p = Dict{String,Any}("valueName" => "cpCorrected", "driftChannel" => ["DAPI"])
        write_module_fun_params!(img._dir, "cleanupImages.driftCorrect", p)
        got = read_module_fun_params(img._dir, "cleanupImages.driftCorrect")
        @test !isnothing(got)
        @test got["valueName"] == "cpCorrected"
        @test got["driftChannel"] == ["DAPI"]

        # init_object loads funParams into the object's meta; a load-modify-save then preserves them
        # (the loaded object carries funParams, so save! doesn't drop them — unlike a stale object).
        r = init_object(proj.uid, img.uid)
        @test haskey(r.meta, "funParams")
        r.status = "done"; save!(r)
        r2 = init_object(proj.uid, img.uid)
        @test r2.status == "done"
        @test read_module_fun_params(r2._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"

        # a second task's params coexist under its own key (no clobber)
        write_module_fun_params!(img._dir, "cleanupImages.cellposeCorrect",
                                 Dict{String,Any}("valueName" => "default"))
        @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"
        @test read_module_fun_params(img._dir, "cleanupImages.cellposeCorrect")["valueName"] == "default"

        # set-level memory uses the same dir-based mechanism on the set's ccid.json
        write_module_fun_params!(s._dir, "cleanupImages.driftCorrect",
                                 Dict{String,Any}("valueName" => "setDefault"))
        @test read_module_fun_params(s._dir, "cleanupImages.driftCorrect")["valueName"] == "setDefault"

        rm(proj.root; recursive=true)
    end

    # ── Channel names use the versioned convention ──────────────────────────────
    # Regression guard: channel names were stored unversioned under meta, where the
    # task/API readers (which use top-level versioned imChannelNames) never saw them.
    @testset "Channel names versioned round-trip" begin
        proj = create_project!(name="cn-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        set_channel_names!(img, ["DAPI", "GFP"]; check_length=false)
        save!(img)

        # model accessor round-trips through save!/load
        r = init_object(proj.uid, img.uid)
        @test channel_names(r) == ["DAPI", "GFP"]

        # on-disk: top-level versioned imChannelNames (the shape tasks/API use), not under meta
        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "imChannelNames")
        @test !haskey(Dict{String,Any}(get(raw, "meta", Dict())), "imChannelNames")
        @test versioned_active(raw["imChannelNames"]) == "default"
        # readable via the exact helper tasks/API use
        @test collect(String, versioned_get_field(raw, "imChannelNames")) == ["DAPI", "GFP"]

        rm(proj.root; recursive=true)
    end

    # ── Destructive ops ──────────────────────────────────────────────────────────
    @testset "delete_image! / delete_set!" begin
        proj = create_project!(name="del-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        a    = add_image!(s; name="a")
        b    = add_image!(s; name="b")

        delete_image!(s, b.uid)
        @test !(b.uid in s.image_uids)
        @test !isdir(joinpath(proj.root, "0", b.uid))
        @test !isdir(joinpath(proj.root, "1", b.uid))
        @test !(b.uid in init_object(proj.uid, s.uid).image_uids)   # persisted

        set_uid = s.uid
        delete_set!(proj, set_uid)
        @test !(set_uid in proj.set_uids)
        @test !isdir(joinpath(proj.root, "1", set_uid))
        @test !isdir(joinpath(proj.root, "1", a.uid))               # member removed too
        @test !(set_uid in load_project(proj.uid).set_uids)         # persisted

        rm(proj.root; recursive=true)
    end

    # ── Boundary contract: run a REAL module function end-to-end, no api/ ───────
    # The whole suite loads only `using Cecelia` (api/ is not on the path). This runs
    # an actual task (RemoveImage — real ccid.json + disk work, no external binary) to
    # completion through the public `run_task` entrypoint. Catches coupling creeping back.
    @testset "Boundary contract — real module fn end-to-end" begin
        proj = create_project!(name="bc-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        # register a real on-disk image version
        zarr = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = "done"
        save!(img)

        logs = String[]
        result = run_task(RemoveImage(), img,
            Dict{String,Any}("valueName"=>"default", "newDefault"=>"default");
            on_log = l -> push!(logs, l))

        @test result isa Dict
        @test result["removedValue"] == "default"
        @test result["cleared"] == true          # primary removal clears dims/status
        @test !isdir(zarr)                        # file actually deleted from disk
        @test !isempty(logs)                      # on_log callback fired (no WS needed)

        reloaded = init_object(proj.uid, img.uid)
        @test reloaded.status == "pending"        # primary removal reset status
        @test !haskey(reloaded.filepath, "default")  # version entry gone
        rm(proj.root; recursive=true)
    end

    # ── Set expansion — a set resolves to its correct member UIDs ───────────────
    # run_tasks and the batch accessors depend on this everywhere.
    @testset "Set expansion" begin
        proj = create_project!(name="se-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        a = add_image!(s; name="a"); b = add_image!(s; name="b"); c = add_image!(s; name="c")
        expected = Set([a.uid, b.uid, c.uid])

        @test Set(i.uid for i in images(s))    == expected   # in-memory
        @test Set(i.uid for i in images(proj)) == expected
        reloaded = init_object(proj.uid, s.uid)              # reload from disk
        @test reloaded isa CciaSet
        @test Set(reloaded.image_uids)            == expected
        @test Set(i.uid for i in images(reloaded)) == expected
        rm(proj.root; recursive=true)
    end

    # ── Chain template round-trip ─────────────────────────────────────────────
    # ── Chain node scope defaults from the task spec (single source of truth) ────
    # A node built without an explicit scope inherits the task JSON's "scope": set-scope
    # tasks (behaviour.hmm, clustTracks.cluster) become picnic nodes automatically, image
    # tasks stay image-scope. An explicit scope always overrides.
    @testset "Chain node scope inherits from task spec" begin
        @test Cecelia._task_default_scope("clustTracks.cluster") == "set"
        @test Cecelia._task_default_scope("behaviour.hmm")        == "set"
        @test Cecelia._task_default_scope("importImages.remove")  == "image"
        @test Cecelia._task_default_scope("nonexistent.task")     == "image"   # unknown fn → image

        # chain_node / ChainNode with no scope kwarg resolve from the spec …
        @test chain_node("clustTracks.cluster").scope == "set"
        @test chain_node("importImages.remove").scope == "image"
        @test ChainNode(id="x", fn="behaviour.hmm").scope == "set"
        # … and an explicit scope still wins (force a set task to run per-image)
        @test chain_node("clustTracks.cluster"; scope="image").scope == "image"

        # Deserialisation: a node dict with no "scope" key also inherits from the spec
        @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster")).scope == "set"
        # …while a stored scope (frozen template) is honoured verbatim
        @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster",
                                           "scope"=>"image")).scope == "image"
    end

    # ── Producer output value_name is declared in the JSON spec (introspectable) ──
    # The whiteboard reads this to prefill a downstream node's input `valueName`.
    @testset "Output value_name from spec" begin
        @test Cecelia._spec_output_value_name(CellposeCorrect(), "fallback") == "cpCorrected"
        @test Cecelia._spec_output_value_name(DriftCorrect(),    "fallback") == "driftCorrected"
        @test Cecelia._spec_output_value_name(AfCorrect(),       "fallback") == "afCorrected"
        # A task that declares no top-level outputValueName falls back to the caller's default
        @test Cecelia._spec_output_value_name(RemoveImage(), "fallback") == "fallback"
    end

    @testset "Chain template round-trip" begin
        proj = create_project!(name="chain-tpl-$(rand(1000:9999))", kind="static")

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

    # ── Chain run — template frozen, per-image state, pipelining ─────────────
    @testset "Chain run — end-to-end with RemoveImage" begin
        proj = create_project!(name="chain-run-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")

        # Two images, each with a registered filepath for RemoveImage to remove
        imgs = map(("img-a", "img-b")) do nm
            img = add_image!(s; name=nm)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
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
            @test run.image_states[img.uid]["n1"].status == :done
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
        proj = create_project!(name="chain-restart-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        img.status = "done"; save!(img)
        states = Dict(img.uid => Dict(
            "n1" => Cecelia.ImageNodeState(), "n2" => Cecelia.ImageNodeState(),
            "n3" => Cecelia.ImageNodeState()))
        for nid in ("n1","n2","n3")
            states[img.uid][nid].status      = :done
            states[img.uid][nid].params_hash = "h"
        end
        run = Cecelia.ChainRun("rid", "restart-chain", proj.uid, [img.uid], tpl,
                               "hash", states, time(), joinpath(Cecelia._runs_dir(proj), "rid"),
                               ReentrantLock(), Dict{String,Channel{Nothing}}(),
                               Dict{String,Channel{Nothing}}())
        mkpath(run._dir)
        Cecelia._force_restart_from!(run, "n2")
        @test run.image_states[img.uid]["n1"].status == :done       # upstream untouched
        @test run.image_states[img.uid]["n2"].status == :pending    # start node reset
        @test run.image_states[img.uid]["n3"].status == :pending    # downstream reset
        @test run.image_states[img.uid]["n2"].params_hash === nothing

        rm(proj.root; recursive=true)
    end

    # ── Chain start dot — prune to the reachable subgraph ─────────────────────
    @testset "Chain start dot — prune to reachable subgraph" begin
        tpl = ChainTemplate(
            "start-chain",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="d", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="x", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="y", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
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
        proj = create_project!(name="chain-picnic-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm
            add_image!(s; name=nm)
        end

        # n1 (image) → n2 (set-scope) → n3 (image)
        tpl = ChainTemplate(
            "picnic-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="picnic-chain",
                        on_log = line -> push!(logs, line))

        # All per-image nodes completed for every image
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
        end

        # Set-scope node: all images show :done, result contains the full image count
        for img in imgs
            @test run.image_states[img.uid]["n2"].status == :done
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
        proj = create_project!(name="chain-startrun-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm); end

        # n1(image) → n2(set) → n3(image); start dot → n2, so n1 is an upstream draft (excluded)
        tpl = ChainTemplate(
            "startrun-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
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
            @test run.image_states[img.uid]["n2"].status == :done   # set-scope barrier fired as root
            @test run.image_states[img.uid]["n3"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
        @test length(filter(l -> contains(l, "setTask ran"), logs)) == 1   # ran once, not per image

        rm(proj.root; recursive=true)
    end

    # ── Fault isolation is per-predecessor, not global (DAG fan-out) ─────────────
    # A failed branch must not skip a SIBLING branch that shares only an upstream ancestor
    # (e.g. afDriftCorrect → two independent segmentations). Regression guard for the
    # over-broad "any node failed → skip" check that skipped independent branches.
    @testset "Chain fault isolation — independent fan-out" begin
        proj = create_project!(name="chain-fanout-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        # a → { b (fails), c (ok) } — c is independent of b, must still run
        save_chain_template!(proj, ChainTemplate("fanout",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("a","c")]))
        st = run_chain(proj, [img.uid]; chain="fanout").image_states[img.uid]
        @test st["a"].status == :done
        @test st["b"].status == :failed
        @test st["c"].status == :done       # independent sibling — NOT skipped by b's failure

        # a (fails) → { b, c } — the shared ancestor failing skips BOTH branches
        save_chain_template!(proj, ChainTemplate("fanout-root-fail",
            [ChainNode(id="a", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("a","c")]))
        st2 = run_chain(proj, [img.uid]; chain="fanout-root-fail").image_states[img.uid]
        @test st2["a"].status == :failed
        @test st2["b"].status == :skipped
        @test st2["c"].status == :skipped

        # transitive: a → b(fail) → c → d — skip propagates down the branch via :skipped
        save_chain_template!(proj, ChainTemplate("chain-transitive",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="d", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d")]))
        st3 = run_chain(proj, [img.uid]; chain="chain-transitive").image_states[img.uid]
        @test st3["a"].status == :done
        @test st3["b"].status == :failed
        @test st3["c"].status == :skipped   # pred b failed
        @test st3["d"].status == :skipped   # pred c skipped → propagates

        rm(proj.root; recursive=true)
    end

    # ── Picnic node — require_all aborts if any image failed upstream ────────
    @testset "Picnic node — require_all policy" begin
        proj = create_project!(name="picnic-req-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1 is a bad fn — both images will fail there
        # n2 is require_all — should abort since n1 failed
        tpl = ChainTemplate(
            "req-chain",
            [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="require_all")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="req-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :failed
            @test run.image_states[img.uid]["n2"].status == :failed
        end
        rm(proj.root; recursive=true)
    end

    # ── Picnic node — successful_only aborts when no images eligible ─────────
    @testset "Picnic node — successful_only policy" begin
        proj = create_project!(name="picnic-ok-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1: bad fn — both images fail upstream
        # n2 (successful_only): zero eligible images → should abort, both :failed
        tpl = ChainTemplate(
            "ok-chain",
            [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="successful_only")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="ok-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :failed
            # no eligible images → set-scope node also fails
            @test run.image_states[img.uid]["n2"].status == :failed
        end
        rm(proj.root; recursive=true)
    end

    # ── Picnic node — successful_only runs with eligible subset ──────────────
    @testset "Picnic node — successful_only with all passing" begin
        proj = create_project!(name="picnic-pass-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1: always succeeds → both images eligible → task runs with all 2
        tpl = ChainTemplate(
            "pass-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="successful_only")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="pass-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
        rm(proj.root; recursive=true)
    end

    # ── Chain run — overrides applied, bad fn isolated ────────────────────────
    @testset "Chain run — overrides + fault isolation" begin
        proj = create_project!(name="chain-iso-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")

        imgs = map(("img-a", "img-b")) do nm
            img = add_image!(s; name=nm)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
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
            @test run.image_states[img.uid]["n1"].status == :failed
            @test run.image_states[img.uid]["n2"].status == :skipped
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — load_chain_run round-trips state ──────────────────────
    @testset "Chain resume — load_chain_run round-trip" begin
        proj = create_project!(name="chain-resume-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "resume-rt-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="resume-rt-chain", on_log=_->nothing)

        # Verify states are :done with a params_hash stored
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test !isnothing(run.image_states[img.uid]["n1"].params_hash)
        end

        # Load the run from disk and verify state is preserved
        loaded = load_chain_run(proj, run.id)
        @test loaded.id == run.id
        @test loaded.chain_name == "resume-rt-chain"
        @test length(loaded.image_uids) == 2
        for img in imgs
            @test loaded.image_states[img.uid]["n1"].status == :done
            @test loaded.image_states[img.uid]["n1"].params_hash == run.image_states[img.uid]["n1"].params_hash
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — already-done nodes are skipped ────────────────────────
    @testset "Chain resume — skip unchanged done nodes" begin
        proj = create_project!(name="chain-resume-skip-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "skip-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
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
            @test run2.image_states[img.uid]["n1"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — params change triggers re-run ──────────────────────────
    @testset "Chain resume — params change re-runs node" begin
        proj = create_project!(name="chain-resume-rerun-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "rerun-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
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
        @test run2.image_states[imgs[1].uid]["n1"].status == :done

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Resume from mid-chain failure — only failed/downstream nodes rerun ─
    @testset "Resume — failure at node 3 does not redo nodes 1-2" begin
        proj = create_project!(name="resume-fail-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        # n1 → n2 → n3(bad) → n4(skipped)
        tpl = ChainTemplate(
            "fail-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n3", fn="nonexistent.task",   scope="image", params=Dict{String,Any}()),
             ChainNode(id="n4", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="fail-chain",
                         on_log=line->push!(logs1, line))

        uid = imgs[1].uid
        @test run1.image_states[uid]["n1"].status == :done
        @test run1.image_states[uid]["n2"].status == :done
        @test run1.image_states[uid]["n3"].status == :failed
        @test run1.image_states[uid]["n4"].status == :skipped

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
        @test run2.image_states[uid]["n1"].status == :done
        @test run2.image_states[uid]["n2"].status == :done
        @test run2.image_states[uid]["n3"].status == :failed
        @test run2.image_states[uid]["n4"].status == :skipped

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Params change on node 4 — only n4 and downstream rerun ──────
    @testset "Resume — params change on n4 reruns only n4 and downstream" begin
        proj = create_project!(name="resume-p4-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "p4-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n4", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n5", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4"), ChainEdge("n4","n5")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="p4-chain",
                         on_log=line->push!(logs1, line))
        uid = imgs[1].uid
        for n in ("n1","n2","n3","n4","n5")
            @test run1.image_states[uid][n].status == :done
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
            @test run2.image_states[uid][n].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Picnic node restarts when per-image upstream input changes ───
    @testset "Resume — picnic node restarts when upstream stale" begin
        proj = create_project!(name="resume-picnic-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(set) → n3(image)
        tpl = ChainTemplate(
            "picnic-resume-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="picnic-resume-chain",
                         on_log=line->push!(logs1, line))
        for img in imgs
            @test run1.image_states[img.uid]["n1"].status == :done
            @test run1.image_states[img.uid]["n2"].status == :done
            @test run1.image_states[img.uid]["n3"].status == :done
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
            @test run2.image_states[img.uid]["n1"].status == :done
            @test run2.image_states[img.uid]["n2"].status == :done
            @test run2.image_states[img.uid]["n3"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Incremental plot node fires as images complete ───────────────
    @testset "Incremental plot node — fires and sets :done state" begin
        proj = create_project!(name="incr-plot-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(incremental plot)
        # debounce_ms=10 so it fires quickly in the test
        tpl = ChainTemplate(
            "incr-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.incrementalPlotTask", scope="incremental",
                       params=Dict{String,Any}("debounce_ms" => 10))],
            [ChainEdge("n1", "n2")],
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="incr-chain", on_log=line->push!(logs, line))

        # All per-image n1 nodes succeeded
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
        end

        # Incremental plot ran — all images' n2 state is :done
        for img in imgs
            @test run.image_states[img.uid]["n2"].status == :done
        end

        # Plot log appeared at least once
        plot_logs = filter(l -> contains(l, "incr/n2"), logs)
        @test !isempty(plot_logs)

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Incremental node does not block per-image progression ────────
    @testset "Incremental plot node — image threads not blocked" begin
        proj = create_project!(name="incr-nob-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(incremental) → n3(image) — n3 should still run
        # (incremental nodes don't gate downstream per-image nodes)
        tpl = ChainTemplate(
            "incr-pass-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.incrementalPlotTask", scope="incremental",
                       params=Dict{String,Any}("debounce_ms" => 10))],
            [ChainEdge("n1", "n2"), ChainEdge("n1", "n3")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="incr-pass-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Event bus subscribe/unsubscribe ───────────────────────────────
    @testset "Event bus — subscribe and receive node:done events" begin
        proj = create_project!(name="evbus-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "evbus-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}())],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        received = String[]
        handler  = payload -> push!(received, payload.image_uid)
        subscribe_chain_events!("node:done", handler)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="evbus-chain", on_log=_->nothing)

        unsubscribe_chain_events!("node:done", handler)

        # Both images fired node:done events
        @test Set(received) ⊇ Set(i.uid for i in imgs)

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
        proj = create_project!(name="pool-limit-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pool-limit-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 40),
                       resource_pool="slow_pool"),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
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
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 7: Resource pool — higher limit allows parallel execution ────────
    @testset "Resource pool — limit=3 allows all concurrent" begin
        proj = create_project!(name="pool-par-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pool-par-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
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

    # ── Step 7: Pipelining — image A reaches n2 before image B finishes n1 ───
    # Pool limit=1 on n1 (each image sleeps 80ms there). Image A exits n1 first
    # and immediately enters n2 (instant). B and C are still queuing for n1.
    # Verify: A's n2 completion timestamp < B's n1 completion timestamp.
    @testset "Pipelining — n2 of first image before n1 of second image" begin
        proj = create_project!(name="pipeline-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pipeline-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 80),
                       resource_pool="serial_pool"),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
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
    # n2 = testTasks.imageTask (always succeeds).
    # Expected: img_a fails n1, skips n2. img_b and img_c succeed both nodes.
    @testset "Cross-image fault isolation" begin
        proj = create_project!(name="xiso-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img_a = add_image!(s; name="img-a")   # no zarr → RemoveImage will fail
        img_b = add_image!(s; name="img-b")
        img_c = add_image!(s; name="img-c")

        for img in (img_b, img_c)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
            save!(img)
        end

        tpl = ChainTemplate(
            "xiso-chain",
            [ChainNode(id="n1", fn="importImages.remove", scope="image",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}())],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [img_a.uid, img_b.uid, img_c.uid];
                        chain="xiso-chain", on_log=_->nothing)

        # img_a: n1 failed (no zarr), n2 skipped
        @test run.image_states[img_a.uid]["n1"].status == :failed
        @test run.image_states[img_a.uid]["n2"].status == :skipped

        # img_b and img_c: both nodes succeeded — not affected by img_a's failure
        @test run.image_states[img_b.uid]["n1"].status == :done
        @test run.image_states[img_b.uid]["n2"].status == :done
        @test run.image_states[img_c.uid]["n1"].status == :done
        @test run.image_states[img_c.uid]["n2"].status == :done

        rm(proj.root; recursive=true)
    end

    # ── Step 7: run_chain headless — no api/ loaded ───────────────────────────
    # All tests in this file run without `using` api/. This testset makes the
    # contract explicit: run_chain on a picnic chain produces correct results
    # with nothing but `using Cecelia`.
    @testset "run_chain headless (no api/ dependency)" begin
        proj = create_project!(name="headless-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "headless-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="headless-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2

        rm(proj.root; recursive=true)
    end

    # ── fun_name dispatch ─────────────────────────────────────────────────────
    @testset "fun_name dispatch" begin
        @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
        @test _task_from_fun_name("importImages.remove")  isa RemoveImage
        @test _task_from_fun_name("cleanupImages.cellposeCorrect") isa CellposeCorrect
        @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
        @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
        @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
        @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
        composite = _task_from_fun_name("cleanupImages.afDriftCorrect")
        @test composite isa CompositeTask
        @test composite.fun_name == "cleanupImages.afDriftCorrect"
        cp_measure = _task_from_fun_name("segment.cellposeMeasure")
        @test cp_measure isa CompositeTask
        @test cp_measure.fun_name == "segment.cellposeMeasure"
        @test_throws ErrorException _task_from_fun_name("nonexistent.task")
        @test _task_from_fun_name("tracking.bayesian_tracking")    isa BayesianTracking
        @test _task_from_fun_name("tracking.track_measures")       isa TrackMeasures
        bt_measures = _task_from_fun_name("tracking.bayesian_track_measures")
        @test bt_measures isa CompositeTask
        @test bt_measures.fun_name == "tracking.bayesian_track_measures"
    end

    # ── BayesianTracking param validation ─────────────────────────────────────
    @testset "BayesianTracking params" begin
        task = BayesianTracking()
        good = Dict{String,Any}(
            "valueName" => "default", "popsToTrack" => "NONE",
            "maxSearchRadius" => 20, "maxLost" => 3, "trackBranching" => false,
            "minTimepoints" => 5, "accuracy" => 0.8, "probToAssign" => 0.8,
            # advanced section params are flattened by the frontend before submit
            "noiseInital" => 300, "distThresh" => 10.0, "segmentationMissRate" => 0.1,
        )
        @test begin validate_params(task, good); true end
        # maxSearchRadius max is 200
        @test_throws ParamValidationError validate_params(
            task, merge(good, Dict{String,Any}("maxSearchRadius" => 500)))
        # segmentationMissRate min is 0.001
        @test_throws ParamValidationError validate_params(
            task, merge(good, Dict{String,Any}("segmentationMissRate" => 0.0)))
    end

    # ── TrackMeasures param validation ────────────────────────────────────────
    @testset "TrackMeasures params" begin
        task = TrackMeasures()
        @test begin validate_params(task,
            Dict{String,Any}("valueName" => "B", "forceRecompute" => false)); true end
        # forceRecompute is bool — a non-bool must be rejected. Also a guard that params use
        # "key" (not "id"): with "id" the spec key resolves empty and validation silently skips.
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("valueName" => "B", "forceRecompute" => "yes"))
    end

    # ── Labels field round-trip ───────────────────────────────────────────────
    # Regression guard: the `labels` Dict written by cellposeSegment must survive
    # save!/init_object and land at the agreed location in ccid.json.
    @testset "Labels field round-trip" begin
        proj = create_project!(name="labels-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        img.labels["default"] = ["default.zarr", "default_nuc.zarr"]
        save!(img)

        # Reloaded value is correct in memory
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test haskey(r.labels, "default")
        @test r.labels["default"] == ["default.zarr", "default_nuc.zarr"]

        # On-disk shape: top-level "labels" dict with string-vector values
        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "labels")
        label_val = collect(String, raw["labels"]["default"])
        @test label_val == ["default.zarr", "default_nuc.zarr"]

        rm(proj.root; recursive=true)
    end

    # ── CompositeTask — spec loads and composite array is correct ─────────────
    @testset "CompositeTask spec" begin
        task = CompositeTask("cleanupImages.afDriftCorrect")
        spec = Cecelia._task_spec(task)
        @test !isnothing(spec)
        @test haskey(spec, "composite")
        steps = [string(s) for s in spec["composite"]]
        @test steps == ["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]
        @test get(spec, "fun_name", "") == "cleanupImages.afDriftCorrect"
    end

    # ── $include fragment resolution ──────────────────────────────────────────
    # Verifies that {"$include": "imageTiling"} in cellpose.json is expanded
    # to the 4 shared tiling params (blockSize, overlap, blockSizeZ, overlapZ).
    @testset "\$include fragment resolution" begin
        task = CellposeSegment()
        spec = Cecelia._task_spec(task)
        @test !isnothing(spec)
        # Find the imageTiling section
        tiling_sec = nothing
        for p in spec["params"]
            p isa AbstractDict && string(get(p, "key", "")) == "imageTiling" &&
                (tiling_sec = p; break)
        end
        @test !isnothing(tiling_sec)
        tiling_params = tiling_sec["params"]
        keys_in_tiling = [string(get(p, "key", "")) for p in tiling_params if p isa AbstractDict]
        # Fragment contributes these 4; cellpose.json adds labelOverlap
        @test "blockSize"  ∈ keys_in_tiling
        @test "overlap"    ∈ keys_in_tiling
        @test "blockSizeZ" ∈ keys_in_tiling
        @test "overlapZ"   ∈ keys_in_tiling
        @test "labelOverlap" ∈ keys_in_tiling
        @test length(keys_in_tiling) == 5   # 4 from fragment + 1 inline
        # No raw $include entries should survive
        @test !any(p isa AbstractDict && haskey(p, "\$include") for p in tiling_params)
    end

    # ── label_props field round-trip ──────────────────────────────────────────
    @testset "label_props field round-trip" begin
        proj = create_project!(name="lp-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        img.label_props["default"] = "default.h5ad"
        save!(img)

        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test get(r.label_props, "default", nothing) == "default.h5ad"

        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "label_props")
        @test string(raw["label_props"]["default"]) == "default.h5ad"

        rm(proj.root; recursive=true)
    end

    # ── Param validation — AfCorrect (group with flat sub-params) ─────────────
    @testset "Param validation — AfCorrect" begin
        task = AfCorrect()

        # Valid group entry — should not throw
        good = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("correctionMode" => "divide", "channelPercentile" => 60.0)))
        @test begin validate_params(task, good); true end

        # correctionMode is a select — unknown value must be rejected
        bad_select = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("correctionMode" => "unknown_mode")))
        @test_throws ParamValidationError validate_params(task, bad_select)

        # channelPercentile is float min=0/max=100 — out of range must be rejected
        bad_float = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("channelPercentile" => 150.0)))
        @test_throws ParamValidationError validate_params(task, bad_float)
    end

    # ── Param validation — DriftCorrect ───────────────────────────────────────
    @testset "Param validation — DriftCorrect" begin
        task = DriftCorrect()

        # Valid select value
        @test begin validate_params(task, Dict{String,Any}("driftNormalisation" => "none")); true end
        @test begin validate_params(task, Dict{String,Any}("driftNormalisation" => "phase")); true end

        # Invalid select value
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("driftNormalisation" => "invalid"))
    end

    # ── Versioned helpers ─────────────────────────────────────────────────────
    @testset "Versioned dict helpers" begin
        d = Dict{String,Any}()
        versioned_set_field!(d, "filepath", "ccidImage.ome.zarr")
        @test versioned_get_field(d, "filepath") == "ccidImage.ome.zarr"
        @test versioned_active(d["filepath"]) == "default"

        versioned_set_field!(d, "filepath", "ccidCpCorrected.ome.zarr", "cpCorrected")
        @test versioned_get_field(d, "filepath", "cpCorrected") == "ccidCpCorrected.ome.zarr"
        @test versioned_get_field(d, "filepath") == "ccidCpCorrected.ome.zarr"  # active = cpCorrected

        versioned_set_field!(d, "filepath", nothing, "cpCorrected")
        @test isnothing(get(d["filepath"], "cpCorrected", nothing))
    end

    # ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────
    @testset "LabelProps reader" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "LabelProps reader (fixture missing)"
        else
            # metadata (cheap reads)
            @test length(col_names(label_props(h5); data_type=:vars)) == 27
            @test channel_columns(label_props(h5)) ==
                  ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2", "mean_intensity_3"]
            @test centroid_columns(label_props(h5)) == ["centroid-0", "centroid-1", "centroid-2"]
            @test temporal_columns(label_props(h5)) == ["t"]

            # full frame: label + 27 vars + 3 spatial + 1 temporal + 8 obs (track lineage +
            # live.cell.* from tracking.track_measures) = 40 cols, 1377 rows
            df = label_props(h5) |> as_df
            @test size(df) == (1377, 40)
            @test "label" in names(df)
            @test eltype(df.label) == Int64
            @test df.label[1:5] == [0, 1, 2, 3, 4]

            # X orientation correctness (audited values).
            # NOTE: intentional coupling to the committed fixture state of KDIeEm/B.h5ad — these
            # are the actual bbox values in that file, asserting /X is read with correct row/col
            # orientation (not transposed). If this breaks, it's either (a) the reader regressed,
            # or (b) the fixture was deliberately regenerated (e.g. segmentation rerun) — in which
            # case re-audit and update these constants. A failure here is NOT "the test is wrong".
            @test [df[1, "bbox-$j"] for j in 0:4] == Float32[0, 0, 71, 2, 29]
            @test [df[2, "bbox-$j"] for j in 0:4] == Float32[0, 7, 368, 4, 38]

            # is_tracked's signal: a tracked segmentation carries a track_id obs column (KDIeEm/B is
            # tracked). track_props / the track-grained gating plots key off this to say "track first"
            # (empty) instead of erroring when it's absent.
            obs = col_names(label_props(h5); data_type=:obs)
            @test "track_id" in obs
            @test !("not_a_column" in obs)

            # lazy column selection — only requested columns (+ label) are returned
            @test names(label_props(h5) |> select_cols(["area"]) |> as_df) == ["label", "area"]

            # centroid + intensity selection
            @test Set(names(label_props(h5) |> select_cols(["mean_intensity_0"]) |> view_centroid_cols |> as_df)) ==
                  Set(["label", "mean_intensity_0", "centroid-0", "centroid-1", "centroid-2", "t"])

            # row filter by label
            d4 = label_props(h5) |> filter_rows([0, 1, 2]; by=:label) |> as_df
            @test sort(d4.label) == [0, 1, 2]

            # filter is intersection: nonexistent IDs are silently skipped (≤ requested, no NaN/error)
            d4b = label_props(h5) |> filter_rows([0, 1, 999_999]; by=:label) |> as_df
            @test sort(d4b.label) == [0, 1]

            # sort by area, descending
            d5 = label_props(h5) |> select_cols(["area"]) |> sort_by("area"; rev=true) |> as_df
            @test d5.area[1] == maximum(d5.area)
        end
    end

    # ── LabelProps writer (add_obs / save! — the chain write path) ─────────────
    @testset "LabelProps writer" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "LabelProps writer (fixture missing)"
        else
            tmp = joinpath(mktempdir(), "B.h5ad")
            cp(h5, tmp)

            existing = label_props(tmp) |> as_df
            some = existing.label[1:5]                       # write to a subset of labels
            df = DataFrame("label" => some,
                           "test.measure" => Float64.(1:5),
                           "test.other"   => [10.0, 20.0, 30.0, 40.0, 50.0])
            # the documented chain write idiom
            label_props(tmp) |> add_obs(df) |> save!

            # new columns appear in obs column-order
            obs_cols = col_names(label_props(tmp); data_type=:obs)
            @test "test.measure" in obs_cols
            @test "test.other" in obs_cols

            # read back via the reader, aligned by label; unset labels are NaN
            back = label_props(tmp) |> select_cols(["test.measure", "test.other"]) |> as_df
            byrow = Dict(l => i for (i, l) in enumerate(back.label))
            for (k, lab) in enumerate(some)
                @test back[byrow[lab], "test.measure"] == Float64(k)
            end
            # a label not in df → NaN
            other = first(setdiff(back.label, some))
            @test isnan(back[byrow[other], "test.measure"])

            # original data preserved (var count unchanged — obs append only, no X rewrite)
            @test length(col_names(label_props(tmp); data_type=:vars)) ==
                  length(col_names(label_props(h5);  data_type=:vars))

            # idempotent overwrite: re-writing the same column updates, doesn't duplicate
            df2 = DataFrame("label" => some, "test.measure" => fill(99.0, 5))
            label_props(tmp) |> add_obs(df2) |> save!
            @test count(==("test.measure"), col_names(label_props(tmp); data_type=:obs)) == 1
            back2 = label_props(tmp) |> select_cols(["test.measure"]) |> as_df
            @test back2[Dict(l => i for (i, l) in enumerate(back2.label))[some[1]], "test.measure"] == 99.0

            # drop_obs: remove a column; gone from column-order and from as_df
            label_props(tmp) |> drop_obs(["test.measure"]) |> save!
            @test "test.measure" ∉ col_names(label_props(tmp); data_type=:obs)
            @test "test.other"   ∈ col_names(label_props(tmp); data_type=:obs)   # sibling untouched
            @test "test.measure" ∉ names(label_props(tmp) |> as_df)
            # dropping a nonexistent column is a no-op (idempotent)
            @test begin label_props(tmp) |> drop_obs(["never.existed"]) |> save!; true end

            # combined drop + add in one chain (invalidate-then-write, e.g. btrack rerun)
            df3 = DataFrame("label" => some, "test.fresh" => Float64.(1:5))
            label_props(tmp) |> drop_obs(["test.other"]) |>
                                add_obs(df3) |> save!
            cols3 = col_names(label_props(tmp); data_type=:obs)
            @test "test.other" ∉ cols3
            @test "test.fresh" ∈ cols3

            # drop + re-add the SAME column in one chain → the add wins (column survives with new
            # values). Regression: the drop used to de-list and delete the just-written dataset, so
            # e.g. overwriting a categorical hmm.state with a numeric one in one chain lost it.
            df4 = DataFrame("label" => some, "test.fresh" => Float64.(101:105))
            label_props(tmp) |> drop_obs(["test.fresh"]) |>
                                add_obs(df4) |> save!
            @test "test.fresh" ∈ col_names(label_props(tmp); data_type=:obs)
            back4 = label_props(tmp) |> select_cols(["test.fresh"]) |> as_df
            row4  = Dict(l => i for (i, l) in enumerate(back4.label))
            for (k, lab) in enumerate(some)
                @test back4[row4[lab], "test.fresh"] == Float64(100 + k)
            end
        end
    end

    # ── Julia ↔ Python reader parity (the duplication safety net) ──────────────
    # The Julia LabelProps reader and the Python LabelPropsView are two implementations of
    # ONE spec (docs/DATAMODEL.md). They can drift — a new encoding type added to one and not
    # the other. This runs BOTH against the same fixture and compares. Gated on the napari
    # venv + anndata being importable, so headless CI without Python skips rather than fails.
    @testset "LabelProps Julia/Python parity" begin
        h5    = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        pybin = python_bin_path()
        py_ok = !isempty(pybin) && isfile(pybin) &&
                success(setenv(`$pybin -c "import anndata, numpy, pandas"`, dir=@__DIR__))
        if !have_fixture(h5) || !py_ok
            @test_skip "LabelProps parity (fixture or python+anndata unavailable)"
        else
            # Python side dumps a comparable summary as JSON via the LabelPropsView reader.
            pyscript = """
            import sys, json
            import cecelia.utils.label_props_utils as lpu
            v = lpu.LabelPropsView(sys.argv[1])
            df  = v.view_cols(["mean_intensity_0"]).as_df().sort_values("label")
            cv  = lpu.LabelPropsView(sys.argv[1]).only_centroid_cols().as_df().sort_values("label")
            print(json.dumps({
                "var_names":      list(v.var_names()),
                "obs_cols":       list(v.adata.obs.columns),
                "centroid_cols":  list(v.centroid_columns()),
                "temporal_cols":  list(v.temporal_columns()),
                "n_obs":          int(len(v.labels())),
                "labels5":        [int(x) for x in df["label"].to_numpy()[:5]],
                "mean_int0_5":    [float(x) for x in df["mean_intensity_0"].to_numpy()[:5]],
                "centroid0_5":    [float(x) for x in cv["centroid-0"].to_numpy()[:5]],
            }))
            """
            # python/ on PYTHONPATH so `import cecelia.utils...` resolves (matches run_py's PYTHONPATH).
            py_dir = joinpath(dirname(dirname(@__DIR__)), "python")   # app/test → app → repo-root/python
            penv = copy(ENV); penv["PYTHONPATH"] = py_dir
            out = read(setenv(`$pybin -c $pyscript $h5`, penv; dir=py_dir), String)
            py = JSON3.read(out)

            lp = label_props(h5)
            @test Set(col_names(lp; data_type=:vars)) == Set(String.(py.var_names))
            @test Set(col_names(lp; data_type=:obs))  == Set(String.(py.obs_cols))
            @test Set(centroid_columns(lp))           == Set(String.(py.centroid_cols))
            @test Set(temporal_columns(lp))           == Set(String.(py.temporal_cols))

            # value parity on the same labels (sorted), one var col + one centroid col
            dj = label_props(h5) |> select_cols(["mean_intensity_0"]) |> sort_by("label") |> as_df
            @test dj.label[1:5] == collect(Int, py.labels5)
            @test dj[1:5, "mean_intensity_0"] ≈ Float64.(py.mean_int0_5)
            cj = label_props(h5) |> view_centroid_cols |> sort_by("label") |> as_df
            @test cj[1:5, "centroid-0"] ≈ Float64.(py.centroid0_5)
        end
    end

    # ── Track measures: numeric cross-check vs celltrackR ─────────────────────
    # Golden values — provenance:
    #   Generated from celltrackR 1.2.2 (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006)
    #   on the track below, via the R package's own functions (trackLength/speed/displacement/
    #   straightness/asphericity/overallAngle/meanTurningAngle, degrees=TRUE; per-step via
    #   subtracks()). celltrackR is the reference Cecelia ported these from; it is NOT a runtime
    #   dependency — these constants pin the port to the original. If a measure here changes,
    #   either the port regressed or it was deliberately changed (then re-derive from celltrackR).
    @testset "Track measures (celltrackR golden)" begin
        Track = Cecelia.Track
        t  = [0.0, 10, 20, 30, 40]
        c3 = [0.0 0 0; 3 4 0; 7 4 2; 7 8 2; 10 12 5]    # (x,y,z) per position
        tr = Track(1, t, c3)

        @test Cecelia.track_length(tr)            ≈ 19.3030878498 atol=1e-6
        @test Cecelia.track_duration(tr)          == 40.0
        @test Cecelia.track_speed(tr)             ≈ 0.4825771962  atol=1e-6
        @test Cecelia.track_displacement(tr)      ≈ 16.4012194669 atol=1e-6
        @test Cecelia.max_displacement(tr)        ≈ 16.4012194669 atol=1e-6
        @test Cecelia.track_straightness(tr)      ≈ 0.8496681772  atol=1e-6
        @test Cecelia.track_displacement_ratio(tr) ≈ 1.0          atol=1e-6
        @test Cecelia.track_outreach_ratio(tr)    ≈ 0.8496681772  atol=1e-6
        @test Cecelia.track_asphericity(tr)       ≈ 0.8469835416  atol=1e-6
        @test Cecelia.track_overall_angle(tr)     ≈ 30.9637565321 atol=1e-6
        @test Cecelia.track_mean_turning_angle(tr) ≈ 64.7432782933 atol=1e-6

        # per-cell subtracks (celltrackR subtracks(·,1) speed; subtracks(·,2) overallAngle)
        ss = Cecelia.step_speeds(tr)              # cell_id 1 → NaN; i>1 = step speed to endpoint
        @test isnan(ss[1])
        @test ss[2:5] ≈ [0.5, 0.4472135955, 0.4, 0.5830951895] atol=1e-6
        sa = Cecelia.step_turning_angles(tr)      # cell_id 1,2 → NaN; i≥3 = turn angle (deg)
        @test all(isnan, sa[1:2])
        @test sa[3:5] ≈ [57.5436915381, 90.0, 46.6861433417] atol=1e-6

        # 2D path (drop z) — same functions, no call-site branching
        tr2 = Track(1, t, c3[:, 1:2])
        @test Cecelia.track_straightness(tr2)  ≈ 0.8678055195 atol=1e-6
        @test Cecelia.track_asphericity(tr2)   ≈ 0.8287305960 atol=1e-6
        @test Cecelia.track_overall_angle(tr2) ≈ 0.0          atol=1e-6  # first ∥ last step in xy

        # ── edge cases (Step 4 mandates these) ────────────────────────────────
        # single-step track (2 positions): measures needing ≥3 steps → NaN; no crash
        one = Track(1, [0.0, 10], [0.0 0 0; 3 4 0])
        @test Cecelia.track_length(one)            ≈ 5.0
        @test Cecelia.track_speed(one)             ≈ 0.5
        @test Cecelia.track_straightness(one)      ≈ 1.0          # straight by definition
        @test isnan(Cecelia.track_overall_angle(one))            # n<3
        @test isnan(Cecelia.track_mean_turning_angle(one))
        @test Cecelia.track_asphericity(one)       == 1.0         # celltrackR convention for <3

        # single-position track: no div-by-zero, sane fallbacks
        pt = Track(1, [0.0], reshape([0.0, 0, 0], 1, 3))
        @test Cecelia.track_length(pt)        == 0.0
        @test isnan(Cecelia.track_speed(pt))                     # duration 0
        @test Cecelia.track_straightness(pt)  == 1.0             # length 0 → 1
        @test isnan(Cecelia.track_displacement_ratio(pt))        # maxDisplacement 0

        # zero net displacement (returns to origin): straightness 0, no div-by-zero
        loop = Track(1, [0.0, 1, 2], [0.0 0; 1 0; 0 0])
        @test Cecelia.track_displacement(loop)       ≈ 0.0
        @test Cecelia.track_length(loop)             ≈ 2.0
        @test Cecelia.track_straightness(loop)       ≈ 0.0
        @test Cecelia.track_displacement_ratio(loop) ≈ 0.0       # disp 0 / maxDisp 1
        @test Cecelia.track_outreach_ratio(loop)     ≈ 0.5       # maxDisp 1 / length 2
    end

    # ── Gating engine: transforms ─────────────────────────────────────────────
    @testset "Transforms" begin
        # linear is identity
        @test apply_transform(LinearTransform(), 42.0) == 42.0
        @test invert_transform(LinearTransform(), 42.0) == 42.0

        # log / asinh round-trip
        lg = LogTransform()
        @test invert_transform(lg, apply_transform(lg, 1234.0)) ≈ 1234.0
        ah = AsinhTransform(cofactor=150.0)
        for x in (-500.0, 0.0, 37.0, 9000.0)
            @test invert_transform(ah, apply_transform(ah, x)) ≈ x atol=1e-6
        end

        # Logicle golden values — provenance:
        #   Generated once from FlowUtils' reference C implementation (`logicle_c`, the C port
        #   of Moore & Parks 2012), params T=262144 W=0.5 M=4.5 A=0, via:
        #     flowutils.transforms.logicle(x, channel_indices=[0], t=262144, m=4.5, w=0.5, a=0)
        #   flowutils was used transiently only to produce these numbers; it is NOT a runtime
        #   dependency. The values are baked in below so this test needs no Python at runtime.
        #   See app/src/gating/transforms.jl for the full citation.
        lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        golden = [(-1000.0, -0.2321153540), (-100.0, 0.0090411347), (0.0, 0.1111111111),
                  (1.0, 0.1122315321), (10.0, 0.1223042757), (100.0, 0.2131810875),
                  (1000.0, 0.4543375762), (10000.0, 0.6838326572),
                  (100000.0, 0.9069275915), (262144.0, 1.0)]
        for (x, gy) in golden
            @test apply_transform(lc, x) ≈ gy atol=1e-6
        end
        # round-trip
        for x in (-1000.0, 0.0, 1.0, 1000.0, 262144.0)
            @test invert_transform(lc, apply_transform(lc, x)) ≈ x atol=1e-4
        end
        # vectorised
        @test apply_transform(lc, [0.0, 262144.0]) ≈ [0.1111111111, 1.0] atol=1e-6

        # Range-based auto-linearisation: logicle collapses a bounded 0–1 measure (morphology) but
        # spreads a real intensity range → effective_transform swaps to linear only for the former.
        @test transform_kind(lc) == "logicle"
        @test transform_collapses(lc, 0.02, 1.0)              # solidity ∈ [0,1] → collapses
        @test !transform_collapses(lc, 0.0, 262144.0)         # full intensity range → fine
        @test !transform_collapses(lc, 0.0, 5000.0)           # large-range morphology (area) → keep logicle
        @test effective_transform(lc, 0.02, 1.0) isa LinearTransform
        @test effective_transform(lc, 0.0, 262144.0) === lc   # untouched
        @test effective_transform(LinearTransform(), 0.02, 1.0) isa LinearTransform  # linear never coerces
        @test !transform_collapses(LinearTransform(), 0.02, 1.0)
        # log needs ≥1 decade above the floor; asinh coerces when all data is inside its ~linear core
        @test transform_collapses(LogTransform(floor=1.0), 0.5, 5.0)
        @test !transform_collapses(LogTransform(floor=1.0), 1.0, 1e5)
        @test transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 1.0)
        @test !transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 5000.0)
        # degenerate extent (single value / non-finite) → no coercion, keep requested
        @test effective_transform(lc, 1.0, 1.0) === lc
        @test effective_transform(lc, NaN, 1.0) === lc
    end

    # ── Gating engine: gates ──────────────────────────────────────────────────
    @testset "Gates" begin
        # rectangle (linear): inclusive bounds
        rg = RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0)
        xin = inside(rg, [5.0, 11.0, -1.0, 0.0], [5.0, 5.0, 5.0, 0.0])
        @test xin == BitVector([true, false, false, true])

        # polygon point-in-polygon (unit square)
        sq = [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
        @test point_in_polygon(0.5, 0.5, sq)
        @test !point_in_polygon(1.5, 0.5, sq)
        @test !point_in_polygon(-0.1, 0.5, sq)
        pg = PolygonGate("x", "y", sq)
        @test inside(pg, [0.5, 2.0], [0.5, 2.0]) == BitVector([true, false])

        # transformed-space gate: a rectangle in logicle coords selects by raw value
        lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        # logicle(1000)=0.4543, logicle(100000)=0.9069 → gate [0.45,0.95] keeps 1000 & 100000, not 10
        rgl = RectangleGate("x", "y", 0.45, 0.95, 0.45, 0.95;
                            x_transform=lc, y_transform=lc)
        keep = inside(rgl, [10.0, 1000.0, 100000.0], [1000.0, 1000.0, 100000.0])
        @test keep == BitVector([false, true, true])

        # JSON round-trip preserves membership
        for g in (rg, pg, rgl)
            g2 = gate_from_spec(gate_spec(g))
            @test inside(g2, [5.0, 1000.0], [5.0, 1000.0]) == inside(g, [5.0, 1000.0], [5.0, 1000.0])
        end

        # project_gate: re-express a gate's stored geometry into a DISPLAY transform so its outline
        # aligns with points drawn in that transform (the client has no transform math).
        lcp = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        # a rectangle stored in logicle coords, projected onto a LINEAR display → raw bounds
        rgp = RectangleGate("cd4", "cd8", 0.4543375762, 0.9069275915, 0.4543375762, 0.9069275915;
                            x_transform=lcp, y_transform=lcp)
        lin = LinearTransform()
        pj = project_gate(rgp, "cd4", "cd8", lin, lin)
        @test pj["kind"] == "rectangle"
        @test pj["x_min"] ≈ 1000.0 atol=1e-2      # invert(logicle, 0.4543) ≈ 1000
        @test pj["x_max"] ≈ 100000.0 atol=1e-1    # invert(logicle, 0.9069) ≈ 100000
        # projecting onto the SAME transform is (near) identity
        same = project_gate(rgp, "cd4", "cd8", lcp, lcp)
        @test same["x_min"] ≈ 0.4543375762 atol=1e-4
        # swapped channel order → x/y transposed
        sw = project_gate(rgp, "cd8", "cd4", lin, lin)
        @test sw !== nothing
        @test sw["y_min"] ≈ 1000.0 atol=1e-2
        # not on this channel pair → nothing
        @test project_gate(rgp, "cd4", "cd19", lin, lin) === nothing
        # polygon vertices map pointwise
        pgp = PolygonGate("cd4", "cd8", [(0.4543375762, 0.4543375762), (0.9069275915, 0.4543375762),
                                         (0.9069275915, 0.9069275915)]; x_transform=lcp, y_transform=lcp)
        # onto a DIFFERENT display transform (logicle→linear) the edges curve → sampled 12 pts/edge
        pjp = project_gate(pgp, "cd4", "cd8", lin, lin)
        @test pjp["kind"] == "polygon"
        @test length(pjp["vertices"]) == 36                 # 3 edges × 12 samples
        @test pjp["vertices"][1][1] ≈ 1000.0 atol=1e-2       # first sample = corner 1
        # edge 1 is horizontal (y const ≈1000); its midpoint (idx 7 = t=0.5) bows far below the straight
        # chord's linear midpoint (~50500) — that bow is the curve the sampling captures.
        @test pjp["vertices"][7][2] ≈ 1000.0 atol=1e-2
        @test pjp["vertices"][7][1] < 50000
        # onto the SAME transform edges are already straight → corners kept (clean edit round-trip)
        @test length(project_gate(pgp, "cd4", "cd8", lcp, lcp)["vertices"]) == 3
    end

    # ── Gating engine: density ────────────────────────────────────────────────
    @testset "Density" begin
        x = collect(0.0:0.01:1.0)
        d = density_2d(x, x; bins=10)
        @test sum(d.counts) == length(x)          # every point counted once
        @test size(d.counts) == (10, 10)
        @test d.counts[1, 10] == 0                 # off-diagonal empty (x==y data)
        @test all(d.counts[i, i] >= 1 for i in 1:10)

        # NaN/Inf (object/morphology measures on degenerate objects) must be skipped, not throw —
        # extents come from the finite values, and a non-finite point contributes to no bin.
        xn = [0.0, 0.5, 1.0, NaN, Inf, -Inf]
        dn = density_2d(xn, xn; bins=10)
        @test sum(dn.counts) == 3                  # only the 3 finite points counted
        @test (dn.x_min, dn.x_max) == (0.0, 1.0)   # extents ignore NaN/Inf
        # all-non-finite → falls back to the default extent and empty counts (no throw)
        dz = density_2d([NaN, Inf], [NaN, Inf]; bins=4)
        @test sum(dz.counts) == 0
    end

    # ── Population manager: paths, tree, persistence ──────────────────────────
    @testset "Population manager" begin
        @test pop_parent("/a/b") == "/a"
        @test pop_parent("/a") == ROOT
        @test pop_name("/a/b") == "b"
        @test pop_path("/a", "b") == "/a/b"
        @test pop_path(ROOT, "a") == "/a"
        @test is_root(ROOT) && is_root("/") && !is_root("/a")

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10), colour="#f00")
        add_pop!(m, "cd8"; parent="/cd4", gate=PolygonGate("x", "y", [(0.,0.),(0.,5.),(5.,5.),(5.,0.)]))
        @test pop_paths(m) == ["/cd4", "/cd4/cd8"]
        @test direct_children(m, ROOT) == ["/cd4"]
        @test descendants(m, "/cd4") == ["/cd4/cd8"]

        # save/load round-trip (tree + gate)
        td = mktempdir()
        save_pop_map!(m, td)
        @test isfile(gating_path(td, "B"))
        # atomic write: the temp file is renamed into place, never left behind
        @test !isfile(gating_path(td, "B") * ".tmp")
        m2 = load_pop_map(td, "B")
        @test pop_paths(m2) == pop_paths(m)
        @test pop_at(m2, "/cd4").colour == "#f00"
        @test pop_at(m2, "/cd4").gate isa RectangleGate
        @test pop_at(m2, "/cd4/cd8").gate isa PolygonGate

        # cascade rename
        rename_pop!(m, "/cd4", "tcell")
        @test Set(pop_paths(m)) == Set(["/tcell", "/tcell/cd8"])
        @test pop_at(m, "/tcell/cd8").parent == "/tcell"
        # cascade delete
        del_pop!(m, "/tcell")
        @test isempty(pop_paths(m))
    end

    # ── clust / trackclust pop types (cluster-membership populations) ─────────────
    # A cluster pop is a filter on the `clusters.{suffix}` column (clustPops/clustTracks output):
    # filter_fun="in", filter_values=[ticked cluster ids]. Stored in its own sidecar so it never
    # collides with flow gates. Headless — membership via a recompute! closure (no fixture).
    # gating pop types = the hand-drawn ones (flow=cells, track=tracks); clust/trackclust are filters.
    # Drives copy-to-images + the defining-plot view (one abstraction over both, no flow special-casing).
    @testset "GATING_POP_TYPES" begin
        @test GATING_POP_TYPES == ("flow", "track")
        @test is_gating_pop_type("flow") && is_gating_pop_type("track")
        @test !is_gating_pop_type("clust") && !is_gating_pop_type("trackclust") && !is_gating_pop_type("live")
    end

    # generic value_name presence check on an image (drives copy-to-images target filtering).
    # `_active` is a bookkeeping key, not a value_name → excluded by versioned_keys.
    @testset "img_has_value_name" begin
        proj = create_project!(name="vn-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="i")
        img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "_active" => "A")
        @test Set(img_value_names(img)) == Set(["A", "B"])
        @test img_has_value_name(img, "A") && img_has_value_name(img, "B")
        @test !img_has_value_name(img, "C") && !img_has_value_name(img, "_active")
        rm(proj.root; recursive=true)
    end

    @testset "clust / trackclust pop types" begin
        td = mktempdir()
        # each clustering pop_type routes to its OWN gating sidecar (no collision with flow's {vn}.json)
        @test endswith(gating_path(td, "B"; pop_type="flow"),       joinpath("gating", "B.json"))
        @test endswith(gating_path(td, "B"; pop_type="clust"),      joinpath("gating", "B__clust.json"))
        @test endswith(gating_path(td, "B"; pop_type="trackclust"), joinpath("gating", "B__trackclust.json"))
        @test endswith(gating_path(td, "B"; pop_type="track"),      joinpath("gating", "B__tracks.json"))

        # cluster pop membership = filter "in" over the cluster code column
        m = PopulationMap(pop_type="clust", value_name="B")
        add_pop!(m, "myeloid"; filter_measure="clusters.default", filter_fun="in",
                 filter_values=[1, 3], colour="#10b981")
        fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                               "clusters.default" => [0, 1, 2, 3, 1])
        recompute!(m, fetch)
        @test Set(cells_in_pop(m, "/myeloid")) == Set([11, 13, 14])   # codes ∈ {1,3}

        # save/load round-trip → own file, filter fields preserved, flow file untouched
        save_pop_map!(m, td)
        @test isfile(gating_path(td, "B"; pop_type="clust"))
        @test !isfile(gating_path(td, "B"; pop_type="flow"))
        m2 = load_pop_map(td, "B"; pop_type="clust")
        @test pop_at(m2, "/myeloid").filter_measure == "clusters.default"
        @test pop_at(m2, "/myeloid").filter_fun == "in"
        @test Set(pop_at(m2, "/myeloid").filter_values) == Set([1, 3])
    end

    # ── Summary-canvas population picker (plot_pop_types / plot_population_groups) ──
    # The logic the /api/plots/populations route delegates to — pure, so tested here (the route is a
    # thin wrapper). Covers granularity→pop_type selection, cross-image + cross-pop_type union/dedup,
    # derived-pop injection, and pop_type tagging (the track-pops-in-the-picker fix, docs/TODO #00043).
    @testset "plot population picker" begin
        # pop_type selection by granularity
        @test plot_pop_types("live", "cell") == ["live"]
        @test plot_pop_types("live", "")     == ["live"]
        @test plot_pop_types("live", "track") == ["live", "track"]
        @test plot_pop_types("track", "track") == ["track"]          # no duplicate

        # flatten_pop_tree: pre-order paths + colours
        fm = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
        add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
        flat = flatten_pop_tree(to_tree(fm))
        @test [p for (p, _, _) in flat] == ["/qc", "/qc/sub"]
        @test flat[1][3] == "#ef4444"

        # a track-gated map (pop_type "track") with one pop
        tm = PopulationMap(pop_type="track", value_name="C")
        add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")

        # loaders injected (as the API passes versioned_keys/load_pop_map closures)
        names_for = _ -> ["C"]
        load = (_, vn, pt) -> vn == "C" ? (pt == "track" ? tm : (pt == "live" ? fm : nothing)) : nothing

        # CELL granularity → live pops only, with derived `_tracked` at root AND under each stored pop
        # (so /qc/_tracked is a selectable, indented child); a derived child directly follows its parent.
        cell = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell"))
        @test length(cell) == 1 && cell[1].value_name == "C"
        cpops = cell[1].populations
        @test [p.path for p in cpops] ==
              ["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked"]
        @test all(p.pop_type == "live" for p in cpops)
        @test !any(p.path == "/TEST" for p in cpops)
        # the nested derived pop is named by its leaf (indents under its parent in the UI)
        @test only(p for p in cpops if p.path == "/qc/_tracked").name == "_tracked"
        # a derived child inherits its parent pop's colour (so /qc/_tracked pairs with /qc visually —
        # the derived colour is read-only on the behaviour page, the parent's is editable on gating)
        @test only(p for p in cpops if p.path == "/qc/_tracked").colour == "#ef4444"      # = /qc
        @test only(p for p in cpops if p.path == "/qc/sub/_tracked").colour == "#abc"      # = /qc/sub
        @test only(p for p in cpops if p.path == "/_tracked").colour == "#7c93b8"          # root: no parent → grey

        # TRACK granularity → unions live (incl. nested /qc/_tracked) AND track (/TEST), each tagged
        trk = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "track"))
        tp = trk[1].populations
        @test Set(p.path for p in tp) ==
              Set(["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked", "/TEST"])
        test_pop = only(p for p in tp if p.path == "/TEST")
        @test test_pop.pop_type == "track" && test_pop.colour == "#f59e0b"
        @test only(p for p in tp if p.path == "/qc").pop_type == "live"
        @test !any(p.path == "/TEST/_tracked" for p in tp)          # no track-derived pop registered

        # root_derived_ok predicate: hide the root-level /_tracked (the API passes false when tracking
        # was gated → root is a redundant duplicate of /qc/_tracked) while KEEPING the per-pop derived
        # children. Default (no predicate) still offers root /_tracked — asserted by `cell` above.
        gated = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell");
                                       root_derived_ok = (_v, _pt, dpath) -> dpath != "/_tracked")
        gpaths = [p.path for p in gated[1].populations]
        @test !("/_tracked" in gpaths)                              # root hidden
        @test "/qc/_tracked" in gpaths && "/qc/sub/_tracked" in gpaths   # per-gate derived kept

        # cross-image UNION + dedup: two images both expose "C" → each (pop_type, path) appears once
        dedup = plot_population_groups([:img1, :img2], names_for, load, ["live"])
        @test length(dedup) == 1
        @test length(dedup[1].populations) == length(cpops)         # no duplicates across images

        # LABELS (gateless): no gating map — one selectable pop per segmentation value_name, named by
        # the value_name, tagged pop_type "labels" (segmentation QC: B/T plot side by side).
        names2 = _ -> ["B", "T"]
        lab = plot_population_groups([:img1], names2, (args...) -> error("must not load a map for labels"),
                                     ["labels"])
        @test [g.value_name for g in lab] == ["B", "T"]
        @test all(g -> length(g.populations) == 1, lab)
        bp = only(lab[1].populations)
        @test bp.path == "/labels" && bp.name == "B" && bp.pop_type == "labels"
        @test only(lab[2].populations).name == "T"
    end

    @testset "popScope population picker" begin
        # is_track_pop: the sole cell-vs-track test (Julia parity of the R `isTrack` attribute)
        @test is_track_pop("live", "/qc") == false                  # plain cell gate
        @test is_track_pop("flow", "/qc/sub") == false
        @test is_track_pop("clust", "/myeloid") == false            # cell cluster
        @test is_track_pop("live", "/_tracked") == true             # derived tracked set (root)
        @test is_track_pop("live", "/qc/_tracked") == true          # derived tracked subset of a gate
        @test is_track_pop("track", "/TEST") == true                # per-track gate
        @test is_track_pop("trackclust", "/clusterA") == true       # track cluster

        # scope_pop_types: sources loaded per scope; clusters toggleable; unknown scope throws
        @test scope_pop_types("cells", true)  == ["live", "clust"]
        @test scope_pop_types("cells", false) == ["live"]
        @test scope_pop_types("tracks", true)  == ["live", "track", "trackclust"]
        @test scope_pop_types("tracks", false) == ["live", "track"]
        @test_throws ErrorException scope_pop_types("bogus", true)

        # maps: flow gates (/qc, /qc/sub), a per-track gate (/TEST), a cell cluster (/myeloid),
        # a track cluster (/clusterA)
        fm = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
        add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
        tm = PopulationMap(pop_type="track", value_name="C")
        add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")
        cm = PopulationMap(pop_type="clust", value_name="C")
        add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
        tcm = PopulationMap(pop_type="trackclust", value_name="C")
        add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
        names_for = _ -> ["C"]
        load = (_, vn, pt) -> vn != "C" ? nothing :
            pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm : pt == "trackclust" ? tcm : nothing

        # CELLS scope: all-cells root ("/") + plain gates + cell clusters; NO derived _tracked sets
        cells = population_scope_groups([:img1], names_for, load, "cells")
        @test length(cells) == 1 && cells[1].value_name == "C"
        cpaths = [p.path for p in cells[1].populations]
        @test cpaths == ["/", "/qc", "/qc/sub", "/myeloid"]
        @test cells[1].populations[1].name == "all"                 # backend all-cells root
        @test !any(occursin("_tracked", p) for p in cpaths)         # cells never show tracked sets
        @test all(!is_track_pop(p.pop_type, p.path) for p in cells[1].populations if p.path != "/")

        # CELLS, clusters excluded → drops /myeloid
        cells_nc = population_scope_groups([:img1], names_for, load, "cells"; include_clusters=false)
        @test [p.path for p in cells_nc[1].populations] == ["/", "/qc", "/qc/sub"]

        # TRACKS scope: derived _tracked sets (root + per-gate) + per-track gate + track cluster;
        # NO plain cell gates (/qc, /qc/sub) and NO all-cells root ("/")
        trk = population_scope_groups([:img1], names_for, load, "tracks")
        tpaths = Set(p.path for p in trk[1].populations)
        @test tpaths == Set(["/_tracked", "/qc/_tracked", "/qc/sub/_tracked", "/TEST", "/clusterA"])
        @test !("/qc" in tpaths) && !("/qc/sub" in tpaths) && !("/" in tpaths)
        @test all(is_track_pop(p.pop_type, p.path) for p in trk[1].populations)
        # a derived tracked child keeps its parent gate's colour (visual pairing, read-only)
        @test only(p for p in trk[1].populations if p.path == "/qc/_tracked").colour == "#ef4444"

        # TRACKS, gated tracking → root /_tracked hidden (redundant with /qc/_tracked); children kept
        trk_g = population_scope_groups([:img1], names_for, load, "tracks";
                                        root_derived_ok=(_v, _pt, d) -> d != "/_tracked")
        gpaths = Set(p.path for p in trk_g[1].populations)
        @test !("/_tracked" in gpaths) && "/qc/_tracked" in gpaths

        # TRACKS, clusters excluded → drops /clusterA, keeps the per-track gate /TEST
        trk_nc = population_scope_groups([:img1], names_for, load, "tracks"; include_clusters=false)
        tncpaths = Set(p.path for p in trk_nc[1].populations)
        @test !("/clusterA" in tncpaths) && "/TEST" in tncpaths
    end

    # ── Gating engine: recompute, membership, filtered (tracked) pops ─────────
    @testset "recompute! + cells_in_pop" begin
        df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9], track_id=[0, 5, 9, 0, 7])

        # flow: parent (x≥0) ∩ child (x≥5)
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", 0.0, 1e9, -1e9, 1e9))
        add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        recompute!(m, _ -> df)
        @test cells_in_pop(m, "/p") == [1, 2, 3, 4, 5]
        @test cells_in_pop(m, "/p/c") == [2, 3, 4, 5]         # x≥5
        @test pop_stats(m, "/p/c").pct_parent == 80.0

        # filtered (tracked) pop: track_id > 0
        mt = PopulationMap(pop_type="live", value_name="T")
        add_pop!(mt, "tracked"; filter_measure="track_id", filter_fun="gt", filter_values=0)
        recompute!(mt, _ -> df)
        @test cells_in_pop(mt, "/tracked") == [2, 3, 5]

        @test_throws ErrorException cells_in_pop(PopulationMap(), "/x")  # not recomputed
    end

    # ── explicit-label membership (napari selection) + transient not persisted ─
    @testset "explicit-label (napari) membership" begin
        df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9])

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # x≥5 → 2,3,4,5
        # transient napari selection of labels {2,4,9} ∩ parent(x≥5) → {2,4}
        add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 4, 9],
                 colour="#22d3ee", transient=true)
        recompute!(m, _ -> df)
        @test cells_in_pop(m, "/p/napari") == [2, 4]              # 9 absent, 3/5 not selected

        # root-level selection (no gate parent): exactly the labels present
        m2 = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m2, "sel"; explicit_labels=[1, 3, 99], transient=true)
        recompute!(m2, _ -> df)
        @test cells_in_pop(m2, "/sel") == [1, 3]

        # transient pops are NOT written to disk, but stay in the in-memory/broadcast tree
        td = mktempdir()
        save_pop_map!(m, td)
        reloaded = load_pop_map(td, "B")
        @test !has_pop(reloaded, "/p/napari")                    # dropped on persist
        @test has_pop(reloaded, "/p")                            # real pop kept
        @test "transient" in keys(Cecelia._node_dict(m, "/p/napari"))  # flagged in broadcast tree

        # explicit-label pops carry a membership signature in the broadcast tree (no gate/filter
        # to diff on) so the client refreshes plots when the selection's cell set changes.
        nd1 = Cecelia._node_dict(m, "/p/napari")
        @test haskey(nd1, "membership_sig")
        del_pop!(m, "/p/napari")
        add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 9], colour="#22d3ee", transient=true)
        @test Cecelia._node_dict(m, "/p/napari")["membership_sig"] != nd1["membership_sig"]
    end

    # ── pop_df: pooling across value_names + dedup to most-specific pop ────────
    @testset "pop_df pooling + dedup" begin
        dfA = DataFrame(label=[1, 2, 3], x=[1.0, 6.0, 9.0])
        dfB = DataFrame(label=[10, 11], x=[7.0, 2.0])
        mA = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(mA, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
        add_pop!(mA, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        mB = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(mB, "hi"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        maps = Dict("A" => mA, "B" => mB)
        load_map = vn -> maps[vn]
        fetch = (vn, _) -> (vn == "A" ? dfA : dfB)

        # request /p and /p/c from A, and hi from B (prefixed)
        res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c", "B/hi"];
                              default_vn="A", pop_cols=["x"], unique_labels=true)
        @test Set(unique(res.value_name)) == Set(["A", "B"])      # pooled across value_names
        # dedup: label 2 & 3 are in both /p and /p/c → assigned the most specific (/p/c)
        getpop(l, vn) = only(res[(res.label .== l) .& (res.value_name .== vn), :pop])
        @test getpop(1, "A") == "/p"
        @test getpop(2, "A") == "/p/c"
        @test getpop(3, "A") == "/p/c"
        @test getpop(10, "B") == "/hi"                            # x=7 ≥ 5
        @test nrow(res[res.value_name .== "B", :]) == 1           # label 11 (x=2) excluded
    end

    # ── pop_df: drop_na drops NA/NaN cells in requested pop_cols ──────────────
    @testset "pop_df drop_na" begin
        # NaN is in a measure column (x), NOT the gate column (g) — a NaN gate value would
        # fail the gate comparison and never enter the result, masking what drop_na does.
        df = DataFrame(label=[1, 2, 3], g=[0.0, 0.0, 0.0], x=[1.0, NaN, 9.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "p"; gate=RectangleGate("g", "g", -1e9, 1e9, -1e9, 1e9))  # all pass
        load_map = _ -> m
        fetch = (_, _) -> df
        keep = Cecelia._pop_df(load_map, fetch, "flow", ["/p"]; default_vn="A", pop_cols=["x"])
        @test nrow(keep) == 3                                     # NaN row kept by default
        dropped = Cecelia._pop_df(load_map, fetch, "flow", ["/p"];
                                  default_vn="A", pop_cols=["x"], drop_na=true)
        @test sort(dropped.label) == [1, 3]                       # label 2 (x=NaN) dropped
    end

    # ── pop_df: track_id joins the dedup key when present (most-specific pop still wins) ──
    @testset "pop_df track_id dedup key" begin
        df = DataFrame(label=[1, 2], x=[9.0, 9.0], track_id=[10.0, 20.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
        add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        load_map = _ -> m
        fetch = (_, _) -> df
        res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c"];
                              default_vn="A", pop_cols=["x"], unique_labels=true)
        @test "track_id" in names(res)                            # track_id carried through
        @test nrow(res) == 2                                      # still one row per cell
        @test Set(res.pop) == Set(["/p/c"])                       # most-specific pop wins
    end

    # ── pop_df: derived live "_tracked" pop (track_id>0 filter on a gated parent) ──
    @testset "pop_df live _tracked (derived filter)" begin
        # label4 fails the qc gate (x=1); label2 is in qc but untracked (track_id=NaN).
        df = DataFrame(label=[1, 2, 3, 4], x=[9.0, 9.0, 9.0, 1.0], track_id=[10.0, NaN, 20.0, 30.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # qc = {1,2,3}
        # "_tracked" is derived, not stored — injecting it adds a filtered child of /qc
        Cecelia._inject_derived_pops!(m, ["/qc/_tracked"], "live")
        @test has_pop(m, "/qc/_tracked")
        @test m.pops["/qc/_tracked"].filter_measure == "track_id"
        load_map = _ -> m
        fetch = (_, _) -> df
        res = Cecelia._pop_df(load_map, fetch, "live", ["/qc/_tracked"];
                              default_vn="A", pop_cols=["track_id"])
        @test sort(res.label) == [1, 3]                # qc ∩ track_id>0 (label2 NaN, label4 not in qc)
        @test unique(res.pop) == ["/qc/_tracked"]
        # a derived pop is only injected under its registered pop_type (foreign type → skip)
        m2 = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m2, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        Cecelia._inject_derived_pops!(m2, ["/qc/_tracked"], "clust")  # _tracked is a `live` spec
        @test !has_pop(m2, "/qc/_tracked")
        # an unknown `_`-name is not derived either
        Cecelia._inject_derived_pops!(m2, ["/qc/_nope"], "live")
        @test !has_pop(m2, "/qc/_nope")
    end

    # ── reserved derived-pop namespace: `_`-prefixed names can't be hand-drawn gates ──
    @testset "reserved pop names (_ prefix)" begin
        @test is_reserved_pop_name("_tracked")
        @test is_reserved_pop_name("_anything")
        @test !is_reserved_pop_name("qc")
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        # a hand-drawn gate may not take a reserved name
        @test_throws ErrorException add_pop!(m, "_tracked"; parent="/qc",
                                             gate=RectangleGate("x", "x", 0.0, 1.0, 0.0, 1.0))
        @test_throws ErrorException rename_pop!(m, "/qc", "_qc")
        # the derived injection (reserved_ok) is allowed to create it
        add_pop!(m, "_tracked"; parent="/qc", filter_measure="track_id", filter_fun="gt",
                 filter_values=0, transient=true, reserved_ok=true)
        @test has_pop(m, "/qc/_tracked")
        # round-trips through from_tree (reconstruction bypasses the guard)
        m3 = from_tree(to_tree(m; include_transient=true))
        @test has_pop(m3, "/qc/_tracked")
    end

    # ── pop_df: cache key folds in file mtimes → auto-invalidates on gate/h5ad change ──
    @testset "pop_df cache auto-invalidation" begin
        td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
        img = CciaImage(uid="X", dir=td)
        img.label_props["A"] = "A.h5ad"
        write(joinpath(td, "gating", "A.json"), "{}")
        write(joinpath(td, "labelProps", "A.h5ad"), "x")
        key() = Cecelia._pop_df_cache_key(img, "flow", "A", ["/qc"], nothing,
                                          false, true, true, false, false, :cell, String[], String[])
        k1 = key()
        sleep(0.05); touch(joinpath(td, "labelProps", "A.h5ad"))   # re-track rewrites h5ad
        k2 = key()
        @test k1 != k2
        sleep(0.05); touch(joinpath(td, "gating", "A.json"))        # gate edit rewrites map
        @test key() != k2
    end

    # ── pop_df: integration on real KDIeEm (gate eval over real H5AD) ─────────
    @testset "pop_df integration (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "pop_df integration (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
            thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)] # ~median → discriminating
            truth = sum(full.mean_intensity_0 .>= thr)
            @test 0 < truth < nrow(full)                          # genuinely partial selection

            m = PopulationMap(pop_type="flow", value_name="B")
            add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                  thr, 1e12, -1e12, 1e12))
            save_pop_map!(m, img)
            df = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area", "mean_intensity_0"])
            @test nrow(df) == truth
            @test Set(names(df)) ⊇ Set(["label", "area", "mean_intensity_0", "pop", "value_name"])
            @test all(df.mean_intensity_0 .>= thr)                # every returned cell passes the gate
            @test unique(df.pop) == ["/pos"]

            # channel-name resolution: pop_df renames intensity cols to channel names by default,
            # raw_channel_names=true keeps the {measure}_intensity_{i} names (channel names are
            # stored under the default version, so value_name="B" falls back to it)
            set_channel_names!(img, ["CD4", "CD8", "CD3", "CD19"]; check_length=false)
            named = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true)
            @test "CD4" in names(named) && !("mean_intensity_0" in names(named))
            raw = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true, raw_channel_names=true)
            @test "mean_intensity_0" in names(raw) && !("CD4" in names(raw))

            # value_name=nothing resolves to the active segmentation (img.label_props _active="B")
            auto = pop_df(img, "flow", ["/pos"]; pop_cols=["area"])
            @test nrow(auto) == truth

            # cache: a request is stored under its signature key; flush_cache recomputes
            ck = Cecelia._pop_df_cache_key(img, "flow", "B", ["/pos"], ["area"],
                                           false, true, true, false, false, :cell, String[], String[])
            cached = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
            @test haskey(img._pop_df_cache, ck)
            fresh = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"], flush_cache=true)
            @test nrow(cached) == truth && nrow(fresh) == truth
        end
    end

    # ── resolve_pops: cached, display-ready per-pop membership (napari points overlay) ──
    @testset "resolve_pops (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "resolve_pops (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
            thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
            want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

            m = PopulationMap(pop_type="flow", value_name="B")
            add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                  thr, 1e12, -1e12, 1e12), colour="#ef4444")
            save_pop_map!(m, img)

            layers = resolve_pops(img, "flow"; value_name="B")
            @test length(layers) == 1
            L = layers[1]
            @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
            @test L.show === true && L.is_track === false
            @test sort(L.labels) == want                       # membership == the gate's cells

            # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
            again = resolve_pops(img, "flow"; value_name="B")
            @test again === layers
            @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
        end
    end

    # ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
    # count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
    # See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
    # ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
    @testset "labels pop_type + count (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "labels pop_type (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
            all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
            @test nrow(all) > 0
            @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
            @test unique(all.pop) == ["/labels"]
            @test unique(all.value_name) == ["B"]

            # cell count via the summary aggregator over labels — one series, value == total.
            whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
            @test whole["chartType"] == "count"
            @test length(whole["series"]) == 1
            @test whole["series"][1]["value"] == Float64(nrow(all))

            # count per timepoint (group_by the temporal column) → counts partition the total.
            byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="t")
            @test byT["groupBy"] == "t"
            @test length(byT["series"]) > 1
            @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

            # a morphology distribution over labels, per timepoint
            area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                     measure="area", group_by="t")
            @test area["measure"] == "area"
            @test length(area["series"]) == length(byT["series"])

            # targets signature (the path the summary canvas + whiteboard QC row use: series =
            # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
            tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
            @test tg["chartType"] == "count"
            @test length(tg["series"]) == 1
            @test tg["series"][1]["value"] == Float64(nrow(all))
            @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
        end
    end

    # ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
    @testset "track table helpers" begin
        td = mktempdir()
        img = CciaImage(uid="X", dir=td)
        # companion track table sits next to the cell labelProps with the __tracks suffix
        @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
        @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
        @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
        # reserved value-name suffix (a user segmentation may not end in __tracks)
        @test is_reserved_value_name("A__tracks")
        @test is_reserved_value_name("foo__tracks")
        @test !is_reserved_value_name("A")
        @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
        # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
        @test Cecelia._jsonsafe(NaN) === nothing
        @test Cecelia._jsonsafe(1.5) === 1.5
        @test Cecelia._jsonsafe(3)   === 3
        # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
        mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
        img.label_props["B"] = "B.h5ad"
        kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                       false, true, true, false, false, :cell, String[], String[])
        kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                       false, true, true, false, false, :track, String[], String[])
        @test kc != kt
    end

    # ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
    @testset "pop_df :track (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "pop_df :track (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # track table layout: measures in X/var, lineage in obs, one row per track_id
            tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
            @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
            tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
            @test "track_root" in tobs

            # one row per track; carries measures + track_id + value_name
            tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
            @test nrow(tr) > 0
            @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
            @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
            @test unique(tr.value_name) == ["B"]

            # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
            ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
            ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
            @test nrow(tr) == ntracks_cells
            @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

            # pop_cols restriction returns just that measure (+ bookkeeping)
            sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                        pop_cols=["live.track.speed"])
            @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))
        end
    end

    # ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
    @testset "plot_summary_data (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # histogram of per-track speed (granularity=:track): shared edges, every track binned
            h = plot_summary_data(img, "live", ["B/_tracked"], "histogram";
                                  measure="live.track.speed", granularity=:track, nbins=20)
            @test h["chartType"] == "histogram"
            @test length(h["binEdges"]) == 21
            @test length(h["series"]) == 1
            ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
            @test sum(h["series"][1]["counts"]) == ntr

            # mock a categorical per-cell column, then frequency over the tracked cells
            cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
            mock = DataFrame("label" => cells.label,
                             "mock.state" => [Float64((l % 3) + 1) for l in cells.label])
            label_props(img_label_props_path(img, "B")) |> add_obs(mock) |> save!
            f = plot_summary_data(img, "live", ["B/_tracked"], "frequency";
                                  measure="mock.state", granularity=:cell, normalize=:fraction)
            @test f["chartType"] == "frequency"
            @test Set(f["categories"]) ⊆ Set(["1", "2", "3"])
            @test length(f["series"]) == 1
            props = f["series"][1]["values"]
            @test all(0 .<= props .<= 1) && isapprox(sum(props), 1.0; atol=1e-9)

            # measureType auto-detection: continuous speed → numeric; integer code set → categorical
            @test h["measureType"] == "numeric"
            @test f["measureType"] == "categorical"

            # bar: mean + all three error metrics (sd, sem = sd/√n, ci95 ≈ 1.96·sem)
            br = plot_summary_data(img, "live", ["B/_tracked"], "bar";
                                   measure="live.track.speed", granularity=:track)
            s = br["series"][1]
            @test Set(keys(s)) ⊇ Set(["value", "sd", "sem", "ci95", "n"])
            @test s["sem"] ≈ s["sd"] / sqrt(s["n"])
            @test s["ci95"] ≈ 1.96 * s["sem"]

            # raw points: boxplot with raw_points carries downsampled values (≤ cap); "points" chart
            bx = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track,
                                   raw_points=true, max_points=10)
            @test length(bx["series"][1]["points"]) == min(ntr, 10)
            pts = plot_summary_data(img, "live", ["B/_tracked"], "points";
                                    measure="live.track.speed", granularity=:track, max_points=10)
            @test pts["chartType"] == "points" && length(pts["series"][1]["points"]) == min(ntr, 10)
            # without raw_points, boxplot carries no payload of values
            bx0 = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                    measure="live.track.speed", granularity=:track)
            @test isempty(bx0["series"][1]["points"])
        end
    end

    @testset "motion dimensionality detection (2D vs 3D)" begin
        # build a Track by cumulative-summing per-step [dz,dy,dx] vectors (coords are [z,y,x])
        mk(steps) = begin
            P = zeros(length(steps) + 1, 3)
            for k in 1:length(steps); P[k+1, :] = P[k, :] .+ steps[k]; end
            Cecelia.Track(1, Float64.(0:length(steps)), P)
        end
        dy(k) = 2 + 0.5 * cos(k / 3); dx(k) = 2 + 0.5 * sin(k / 3)   # persistent forward heading in xy
        # z either tracks the persistent xy motion (real 3D) or oscillates with large amplitude (jitter)
        real(rng)   = mk([[2 + 0.5*sin(k/3), dy(k), dx(k)] for k in rng])
        jitter(rng) = mk([[(-1.0)^k * 4.0,   dy(k), dx(k)] for k in rng])
        trks_real   = [real(t*20 : t*20+14)   for t in 1:8]
        trks_jitter = [jitter(t*20 : t*20+14) for t in 1:8]

        d3 = Cecelia._detect_motion_dims(trks_real)
        @test d3.dims == 3 && d3.z_used            # persistent z → keep 3D
        d2 = Cecelia._detect_motion_dims(trks_jitter)
        @test d2.dims == 2 && !d2.z_used           # oscillating/anti-persistent z → in-plane 2D
        @test d2.metrics["autocorrZ"] < 0          # jitter signature

        # a 2D-only track set (no z column) is trivially 2D
        P2 = zeros(12, 2); for k in 1:11; P2[k+1, :] = P2[k, :] .+ [dy(k), dx(k)]; end
        @test Cecelia._detect_motion_dims([Cecelia.Track(1, Float64.(0:11), P2)]).dims == 2
    end

    @testset "plot groupBy (generic categorical sub-axis)" begin
        # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
        # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
        df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                       "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                       "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
        r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=false, group_by="st")
        @test r["groupBy"] == "st"
        @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
        @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
        byg = Dict(s["group"] => s for s in r["series"])
        @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
        @test byg["2"]["n"] == 3
        # no group_by → single series, empty group label (back-compat)
        r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=false)
        @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

        # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
        # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
        dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                        "uID" => ["x","x","y","y","x","y"],
                        "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                        "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
        rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, group_by="st", collapse_series=true)
        @test length(rc["series"]) == 2
        @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
        @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
        @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
        # collapse with no group_by → one pooled series over everything
        rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, collapse_series=true)
        @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
    end

    @testset "plot matrix (heatmap: profile + crosstab)" begin
        # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
        # frame into one grid (no series). Deterministic synthetic frame — no fixture.
        df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                       "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                       "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                       "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
        pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                  normalize=:none, by_image=false,
                                  matrix_mode="profile", measures=["speed", "angle"], category="st")
        @test pr["matrixMode"] == "profile"
        @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
        cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
        @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
        @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
        @test cell(pr, "2", "speed")["n"] == 2
        @test isempty(pr["series"])

        # z-score standardises each row across its levels (mean 0) — the comparable "signature"
        prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false, zscore=true,
                                   matrix_mode="profile", measures=["speed", "angle"], category="st")
        @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
        zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
        @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

        # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
        # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
        dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                        "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
        ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                  normalize=:none, by_image=false,
                                  matrix_mode="crosstab", category="tr")
        @test ct["matrixMode"] == "crosstab"
        @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
        ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
        @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
        # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
        ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false,
                                   matrix_mode="crosstab", category="tr", matrix_normalize=:row)
        @test ctr["valueLabel"] == "P(to|from)"
        ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
        @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
        @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

        # error cases: unknown mode, missing category, profile with no present measure column
        @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
            nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
        @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
            nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

        # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
        dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                        "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
        prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false, matrix_mode="profile",
                                   measures=["speed"], category="st")
        c2 = first(c for c in prn["cells"] if c["x"] == "2")
        @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell
    end

    @testset "plot attribute grouping (compare by attr)" begin
        # group cross-image series by an image attribute: images sharing a value pool into one series
        # labelled by the value; an image with no value falls back to its uID. No fixture.
        df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                       "uID" => ["x1","x1","x2","x2","y1","y1"],
                       "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
        amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
        r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                 normalize=:none, by_image=true, attr_map=amap)
        @test length(r["series"]) == 2
        @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
        byu = Dict(s["uID"] => s for s in r["series"])
        @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
        # image missing the attribute → falls back to its uID
        r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                  normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
        @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
        # no attr_map → group by image as before (3 images)
        r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                  normalize=:none, by_image=true)
        @test length(r3["series"]) == 3
    end

    # ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
    @testset "plot_summary_data cross-image" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data cross-image (fixture missing)"
        else
            # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
            mk = function (uid)
                td = mktempdir(); mkpath(joinpath(td, "labelProps"))
                cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
                cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
                img = CciaImage(uid=uid, dir=td)
                img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
                img
            end
            imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

            # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
            one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
            both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
            @test "uID" in names(both)
            @test Set(unique(both.uID)) == Set(uids)
            @test nrow(both) == 2 * nrow(one)

            # boxplot per_image → one box per image (same data → equal stats), + scope field
            sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
            @test length(sp["series"]) == 2
            @test Set(s["uID"] for s in sp["series"]) == Set(uids)
            @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
            @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
            @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
            @test sp["series"][1]["n"] == nrow(one)

            # summarised → one pooled box across both images
            ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track, scope=:summarised)
            @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

            # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
            br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test br["chartType"] == "bar" && length(br["series"]) == 2
            @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
            @test br["series"][1]["value"] ≈ br["series"][2]["value"]

            # histogram per_image → 2 overlay series sharing bin edges
            hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                                   measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
            @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
            @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
        end
    end

    # ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
    @testset "plot_summary_data multi-segmentation targets" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data multi-segmentation (fixture missing)"
        else
            # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
            # targets path: a (value_name, pop) per series, vcat across segmentations.
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
            img = CciaImage(uid="uM", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
            img.label_props["_active"] = "B"

            targets = [("B", "/_tracked"), ("B2", "/_tracked")]
            # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
            bp = plot_summary_data(img, "live", targets, "boxplot";
                                   measure="live.track.speed", granularity=:track)
            @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
            @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
            @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
            @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

            # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
            img2 = CciaImage(uid="uN", dir=td)
            img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
            img2.label_props["_active"] = "B"
            xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test xp["scope"] == "per_image" && length(xp["series"]) == 4
            @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
                  Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
        end
    end

    # ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
    @testset "plot_summary_data helpers" begin
        @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
        @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
        let edges = Cecelia._hist_edges([0.0, 10.0], 10)
            @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
        end
        @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
        @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
        @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
        # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
        @test derived_pop_paths("live") == ["/_tracked"]
        @test isempty(derived_pop_paths("flow"))
    end

    # ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
    @testset "track_props (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "track_props (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # mock a categorical per-cell column to exercise the freq branch
            cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
            label_props(img_label_props_path(img, "B")) |>
                add_obs(DataFrame("label" => cells.label,
                                          "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

            tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
            ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
            @test nrow(tp) == ntr                                  # one row per track
            @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
            @test tp.label == tp.track_id                          # engine membership key
            # numeric aggregates present
            @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
            # categorical → per-category frequency columns
            @test "st.1" in names(tp) && "st.2" in names(tp)
            # motility joined from the track table
            @test "live.track.speed" in names(tp)
            # num_cells totals the tracked cells
            @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

            # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
            # off the decoded type + values: strings and integer code sets → categorical; continuous
            # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
            @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
            @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
            @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
            @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
            @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
            @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
            # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
            @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
            @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
            @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
            # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
            auto = track_props(img; value_name="B", cell_measures=["st"])
            @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
            # `numeric` escape-hatch forces it back to numeric aggregates when desired
            forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
            @test "st.mean" in names(forced) && !("st.1" in names(forced))
        end
    end

    # ── track_cell_measures: derive base cell measures from track-property column names ──
    @testset "track_cell_measures" begin
        mot = ["live.track.speed", "live.track.meanTurningAngle"]
        # motility axes need no cell aggregation
        @test isempty(track_cell_measures(["live.track.speed"], mot))
        # numeric aggregate columns → their base cell measure (suffix stripped)
        @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
              ["mean_intensity_0", "area"]
        # categorical frequency column `{base}.{cat}` → base
        @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
        # bookkeeping + motility skipped; dedup across aggregates of the same base
        @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                                   "area.mean", "area.sd"], mot) == ["area"]
    end

    # ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
    @testset "pop_df pop_type=track (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "pop_df pop_type=track (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # motility-only track props (the common track-gating case: no cell_measures needed)
            tp = track_props(img; value_name="B")
            @test "live.track.speed" in names(tp)
            spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
            thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
            truth = count(>=(thr), spd)
            @test 0 < truth < length(spd)

            # a TRACK gate (one point per track) on the speed axis, stored under __tracks
            m = PopulationMap(pop_type="track", value_name="B")
            add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                                   thr, 1e12, -1e12, 1e12))
            save_pop_map!(m, img)
            @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
            @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

            # granularity=:track → gated track rows, one point per track, gate genuinely applied
            g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
            @test nrow(g) == truth
            @test length(unique(g.track_id)) == nrow(g)
            @test unique(g.pop) == ["/fast"]
            @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

            # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
            gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
            @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
            @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
            @test nrow(gc) > nrow(g)                                 # many cells per track
            @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))
        end
    end

    @testset "HMM states + transitions" begin
        # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
        # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
        # `missing` (per-cell states are undefined where a measurement can't exist).
        uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
        for img in ("X", "Y"), k in 1:3, t in 1:25
            slow = t <= 12
            s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
            a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
            if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
            push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
            push!(sp, s); push!(an, a)
        end
        df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                       "live.cell.speed" => sp, "live.cell.angle" => an)

        st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
        @test length(st) == nrow(df)

        # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
        # Vector{String}); fit must not MethodError on the normalise/scale step. (#00051)
        let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                                 scale_measures=Union{}[], normalise=Dict{String,String}())
            @test count(!ismissing, stu) == count(!ismissing, st)
        end
        @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
        @test Set(skipmissing(st)) == Set([1, 2])
        df[!, "live.cell.hmm.state.default"] = st

        one = st[(df.uID .== "X") .& (df.track_id .== 1)]
        @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
        decoded = collect(skipmissing(one))
        @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

        tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                             include_start=false, include_self=true)
        @test length(tr) == nrow(df)
        nonmiss = collect(skipmissing(tr))
        @test all(occursin("_", x) for x in nonmiss)
        @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
        @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

        trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                              include_start=false, include_self=false)
        nm2 = Set(skipmissing(trn))
        @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

        # cross-model hybrid: two state columns paste into "a.b" before transitions
        df[!, "live.cell.hmm.state.second"] = st
        trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                              time_col="t", include_start=false, include_self=true)
        @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

        # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
        # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
        # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
        # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
        @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
              ["A/_tracked", "B/_tracked"]
        @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
        @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
              Set(["A", "B", "C"])

        # task registration + set-scope routing
        @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
        @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
        @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
        @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
        @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
        @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
    end

    # ── Physical-size / timing metadata (import review, edit, resync) ──────────────
    # All fixtures are synthetic temp zarrs (no real data) — these functions are pure
    # readers/writers over the bioformats2raw nested layout (`zarr/0/.zattrs`, `zarr/OME`).
    @testset "OME metadata read/edit/resync" begin
        # Build a minimal bioformats2raw-shaped zarr: `0/.zattrs` (multiscales) + optional
        # `0/0/.zarray` (shape → SizeC/T/Z) + optional `OME/METADATA.ome.xml` (planes).
        function make_zarr(dir; axes, level_scales, units = Dict{String,String}(),
                           shape = nothing, planes = nothing)
            mkpath(joinpath(dir, "0"))
            ax_objs = map(axes) do a
                o = Dict{String,Any}("name" => a,
                    "type" => a in ("x", "y", "z") ? "space" : (a == "t" ? "time" : "channel"))
                haskey(units, a) && (o["unit"] = units[a])
                o
            end
            datasets = [Dict{String,Any}("path" => string(i - 1),
                          "coordinateTransformations" =>
                              [Dict{String,Any}("type" => "scale", "scale" => level_scales[i])])
                        for i in eachindex(level_scales)]
            zattrs = Dict{String,Any}("multiscales" =>
                [Dict{String,Any}("axes" => ax_objs, "datasets" => datasets)])
            open(joinpath(dir, "0", ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
            if !isnothing(shape)
                mkpath(joinpath(dir, "0", "0"))
                open(joinpath(dir, "0", "0", ".zarray"), "w") do io
                    JSON3.write(io, Dict{String,Any}("shape" => shape))
                end
            end
            if !isnothing(planes)
                mkpath(joinpath(dir, "OME"))
                body = join([
                    "<Plane TheZ=\"$(p.z)\" TheT=\"$(p.t)\" DeltaT=\"$(p.dt)\"" *
                    (haskey(p, :unit) ? " DeltaTUnit=\"$(p.unit)\"" : "") * "/>"
                    for p in planes], "\n")
                open(joinpath(dir, "OME", "METADATA.ome.xml"), "w") do io
                    write(io, "<OME><Image><Pixels>$body</Pixels></Image></OME>")
                end
            end
            dir
        end

        # ── _delta_t_fallback: per-plane DeltaT (TheZ=0, TheT=1), unit-converted to seconds ──
        @testset "_delta_t_fallback" begin
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 0, dt = 0.0, unit = "ms"),
                                    (z = 0, t = 1, dt = 5000.0, unit = "ms"),
                                    (z = 0, t = 2, dt = 10000.0, unit = "ms")])
                @test Cecelia._delta_t_fallback(d) == 5.0            # 5000 ms → 5 s, from TheT=1
            end
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 1, dt = 2.0, unit = "min")])
                @test Cecelia._delta_t_fallback(d) == 120.0          # 2 min → 120 s
            end
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 1, dt = 30.0)])      # no unit → seconds
                @test Cecelia._delta_t_fallback(d) == 30.0
            end
            # non-self-closing <Plane>…</Plane> (some vendors) — DeltaT is on the opening tag
            mktempdir() do d
                mkpath(joinpath(d, "OME"))
                write(joinpath(d, "OME", "METADATA.ome.xml"),
                      "<OME><Image><Pixels>" *
                      "<Plane TheZ=\"0\" TheT=\"1\" DeltaT=\"3\" DeltaTUnit=\"s\"><Annotation/></Plane>" *
                      "</Pixels></Image></OME>")
                @test Cecelia._delta_t_fallback(d) == 3.0
            end
            @test isnothing(Cecelia._delta_t_fallback(joinpath(tempdir(), "nope-$(rand(UInt32))")))
        end

        # ── read_ome_metadata: unit-less-t placeholder is rejected; DeltaT fills the gap ──
        @testset "read_ome_metadata" begin
            # t axis has a scale (1.0) but NO unit → placeholder, must NOT become TimeIncrement=1.0;
            # SizeT>1 so the DeltaT fallback kicks in and supplies the real interval.
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"],
                          level_scales = [[1.0, 0.6, 0.5, 0.5]],
                          units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                          shape = [3, 1, 4, 4],
                          planes = [(z = 0, t = 1, dt = 7.0, unit = "s")])
                m = read_ome_metadata(d)
                @test m["SizeT"] == 3
                @test m["PhysicalSizeX"] == 0.5
                @test m["PhysicalSizeZ"] == 0.6
                @test m["PhysicalSizeUnit"] == "micrometer"
                @test m["TimeIncrement"] == 7.0                       # from DeltaT, not the 1.0 placeholder
                @test m["TimeIncrementUnit"] == "second"
            end
            # t axis WITH a unit → trusted verbatim, no fallback needed.
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                          units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                          shape = [4, 8, 8])
                m = read_ome_metadata(d)
                @test m["TimeIncrement"] == 2.5
                @test m["TimeIncrementUnit"] == "second"
            end
        end

        # ── update_ome_scale!: level-0 value set, other levels keep their downsample ratio; units ──
        @testset "update_ome_scale!" begin
            mktempdir() do d
                # z doesn't downsample (0.6, 0.6); x halves per level (0.5, 1.0)
                make_zarr(d; axes = ["z", "y", "x"],
                          level_scales = [[0.6, 0.5, 0.5], [0.6, 1.0, 1.0]])
                update_ome_scale!(d, Dict("z" => 3.0, "x" => 0.65);
                    units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
                z = JSON3.read(read(joinpath(d, "0", ".zattrs"), String))
                dss = z[:multiscales][1][:datasets]
                s0 = dss[1][:coordinateTransformations][1][:scale]
                s1 = dss[2][:coordinateTransformations][1][:scale]
                @test s0[1] == 3.0 && s1[1] == 3.0                   # z ratio 5× applied to both levels
                @test s0[3] == 0.65 && isapprox(s1[3], 1.3)          # x ratio 1.3× preserves downsample
                m = read_ome_metadata(d)
                @test m["PhysicalSizeUnit"] == "micrometer"          # axis unit now round-trips
                @test m["PhysicalSizeZ"] == 3.0
            end
            # unit-only edit (no numeric change) still writes the axis unit
            mktempdir() do d
                make_zarr(d; axes = ["y", "x"], level_scales = [[0.5, 0.5]])
                @test isnothing(get(read_ome_metadata(d), "PhysicalSizeUnit", nothing))
                update_ome_scale!(d, Dict{String,Float64}();
                                  units = Dict("x" => "nanometer", "y" => "nanometer"))
                @test read_ome_metadata(d)["PhysicalSizeUnit"] == "nanometer"
            end
        end

        # ── update_ome_xml_pixels!: replace an existing attr, insert a missing one ──
        @testset "update_ome_xml_pixels!" begin
            mktempdir() do d
                mkpath(joinpath(d, "OME"))
                xml_file = joinpath(d, "OME", "METADATA.ome.xml")
                write(xml_file, "<OME><Image><Pixels SizeX=\"4\" PhysicalSizeZ=\"0.6\">" *
                                "<Plane/></Pixels></Image></OME>")
                update_ome_xml_pixels!(d, Dict("PhysicalSizeZ" => "3.0", "TimeIncrement" => "5.0"))
                out = read(xml_file, String)
                @test occursin("PhysicalSizeZ=\"3.0\"", out)         # replaced
                @test !occursin("PhysicalSizeZ=\"0.6\"", out)
                @test occursin("TimeIncrement=\"5.0\"", out)         # inserted
                @test occursin("SizeX=\"4\"", out)                   # untouched
            end
        end

        # ── sync_zarr_calibration!: one translator (meta shape → zarr) for import + editor ──
        @testset "sync_zarr_calibration!" begin
            @test !Cecelia.has_calibration_meta(Dict{String,Any}("SizeC" => 2))
            @test !Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => nothing))  # null clear
            @test Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => 3.0))
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 0.6, 0.5, 0.5]],
                          shape = [3, 1, 4, 4],
                          planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])  # OME/ dir + <Pixels>
                # a meta-shaped correction (as ccid.json / the importer / the editor produce it)
                Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                    "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "micrometer",
                    "TimeIncrement" => 5.0, "TimeIncrementUnit" => "second"))
                # .zattrs now round-trips the corrected spatial value + unit
                m = read_ome_metadata(d)
                @test m["PhysicalSizeZ"] == 3.0
                @test m["PhysicalSizeUnit"] == "micrometer"
                # OME-XML <Pixels> carries the time interval napari reads unconditionally
                xml = read(joinpath(d, "OME", "METADATA.ome.xml"), String)
                @test occursin("TimeIncrement=\"5.0\"", xml)
                @test occursin("TimeIncrementUnit=\"s\"", xml)
            end
        end

        # ── _merge_zarr_meta_into_ccid!: overwrite=true is authoritative; false is fill-only ──
        @testset "merge fill-only vs overwrite" begin
            proj = create_project!(name = "meta-merge-$(rand(1000:9999))", kind = "static")
            s    = add_set!(proj; name = "set")
            # Simulate an ImageJ-corrected image: PhysicalSizeZ + the ccid-only PhysicalSizeZ_raw marker
            img  = add_image!(s; name = "img", meta = Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))

            # Fill-only backfill: existing keys survive, genuinely-missing ones get filled.
            Cecelia._merge_zarr_meta_into_ccid!(img,
                Dict{String,Any}("PhysicalSizeZ" => 0.6, "PhysicalSizeX" => 0.5); overwrite = false)
            r = init_object(proj.uid, img.uid)
            @test r.meta["PhysicalSizeZ"] == 3.0                     # NOT reverted to the raw 0.6
            @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker NOT dropped
            @test r.meta["PhysicalSizeX"] == 0.5                     # filled (was absent)

            # Authoritative import merge: clears derived keys, takes the fresh read verbatim.
            Cecelia._merge_zarr_meta_into_ccid!(r,
                Dict{String,Any}("PhysicalSizeZ" => 0.6); overwrite = true)
            r2 = init_object(proj.uid, img.uid)
            @test r2.meta["PhysicalSizeZ"] == 0.6
            @test !haskey(r2.meta, "PhysicalSizeZ_raw")              # zombie marker cleared
            rm(proj.root; recursive = true)
        end

        # ── resync_ome_meta! end-to-end: fill-only backfill never reverts a correction ──
        @testset "resync_ome_meta! fill-only" begin
            proj = create_project!(name = "meta-resync-$(rand(1000:9999))", kind = "static")
            s    = add_set!(proj; name = "set")
            img  = add_image!(s; name = "img", meta = Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))   # ImageJ-corrected, ccid-only

            # Register a "default" zarr on disk carrying the RAW (pre-correction) calibration.
            zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
            make_zarr(zdir; axes = ["z", "y", "x"], level_scales = [[0.6, 0.5, 0.5]],
                      units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                      shape = [1, 8, 8])
            img.filepath["default"]         = "img.ome.zarr"
            img.filepath[VERSIONED_ACTIVE_KEY] = "default"
            save!(img)

            @test resync_ome_meta!(init_object(proj.uid, img.uid))
            r = init_object(proj.uid, img.uid)
            @test r.meta["PhysicalSizeZ"] == 3.0                     # correction survives resync
            @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker survives
            @test r.meta["PhysicalSizeUnit"] == "micrometer"         # genuinely-missing field filled
            @test r.meta["PhysicalSizeX"] == 0.5
            rm(proj.root; recursive = true)
        end
    end

    @testset "QC framework" begin
        @testset "sidecar round-trip" begin
            img = CciaImage(; dir = mktempdir())
            f = qc_finding("warn", "demo.code", "short text", "long text"; detail = Dict("k" => 1))
            @test f["level"] == "warn" && f["code"] == "demo.code"

            p = write_qc(img, "cleanupImages.driftCorrect", "driftCorrected", [f];
                         source = Dict("shape" => [1, 2, 3]))
            @test isfile(p)
            @test occursin(joinpath("qc", "cleanupImages.driftCorrect", "driftCorrected.json"), p)

            doc = read_qc(img, "cleanupImages.driftCorrect", "driftCorrected")
            @test length(doc["findings"]) == 1
            @test doc["findings"][1]["code"] == "demo.code"

            all = read_all_qc(img)
            @test haskey(all, "cleanupImages.driftCorrect/driftCorrected")

            # no-value_name → falls back to the default key
            write_qc(img, "some.task", "", Dict{String,Any}[])
            @test isfile(qc_path(img, "some.task", VERSIONED_DEFAULT_VAL))
        end

        @testset "canvas-expansion check" begin
            order = "TCZYX"
            # fHqhyb: XY +42%/+21% → flagged; Z doubling is ignored
            bad = qc_canvas_expansion([94, 4, 13, 512, 512], [94, 4, 26, 728, 618], order)
            @test bad !== nothing && bad["code"] == "output.canvas_expansion"
            # LUkCpP (normal): XY +6%/+3% → not flagged even though Z grew +46%
            @test qc_canvas_expansion([64, 4, 13, 512, 512], [64, 4, 19, 541, 527], order) === nothing
        end

        @testset "drift findings" begin
            base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"])
            smooth = [[0.0, 1.0, 1.0] for _ in 1:20]
            spiky  = copy(smooth); spiky[16] = [0.0, 120.0, 90.0]   # jump at frame 16

            # bad ref: canvas ballooned AND a spike → both findings
            meta_bad = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                        "outputShape" => [20, 4, 26, 728, 618], "shifts" => spiky))
            fb, _, _ = Cecelia._drift_qc_findings(meta_bad)
            codes = Set(f["code"] for f in fb)
            @test "drift.canvas_expansion" in codes
            @test "drift.jump" in codes
            jump = first(f for f in fb if f["code"] == "drift.jump")
            @test jump["detail"]["atT"] == 15                       # 0-based frame index of the spike

            # good ref: modest canvas, smooth trajectory → no findings
            meta_ok = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                       "outputShape" => [20, 4, 19, 541, 527], "shifts" => smooth))
            fo, _, _ = Cecelia._drift_qc_findings(meta_ok)
            @test isempty(fo)
        end
    end

end
