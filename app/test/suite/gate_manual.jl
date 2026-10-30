# ── The manual gate step (tasks/gate/manual_gate.jl) + copy_gating! ──────
# Real image: the `testpr` fixture's KDIeEm (label set `B`), copied under a second uid with its gating
# removed — the same two-image shape the API's gating-copy test uses.

function _gate_fixture(f)
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir  = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(fixture_path("testpr"), proj)
    tgt = joinpath(proj, "1", "TgtImg")
    cp(joinpath(proj, "1", "KDIeEm"), tgt)
    rm(joinpath(tgt, "gating"); recursive = true, force = true)
    ccid = joinpath(tgt, "ccid.json")
    write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEm\"" => "\"uid\": \"TgtImg\""))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        src = init_object("testpr", "KDIeEm")
        m = load_pop_map(src; value_name = "B", pop_type = "flow")
        has_pop(m, "/qc") || add_pop!(m, "qc"; filter_measure = "track_id", filter_fun = "gt", filter_values = 0)
        save_pop_map!(m, src)
        f(src, init_object("testpr", "TgtImg"))
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

_gated(img) = has_pop(load_pop_map(img; value_name = "B", pop_type = "flow", backfill_save = false), "/qc")

@testset "copy_gating! — replace = false keeps an image's own gates" begin
    _gate_fixture() do src, tgt
        r = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test r.copied == ["TgtImg"] && _gated(tgt)
        # now gated: a second prefill must not overwrite it
        m = load_pop_map(tgt; value_name = "B", pop_type = "flow")
        add_pop!(m, "mine"; filter_measure = "track_id", filter_fun = "gt", filter_values = 1); save_pop_map!(m, tgt)
        r2 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test isempty(r2.copied) && r2.skipped["TgtImg"] == "already gated"
        @test has_pop(load_pop_map(tgt; value_name = "B", pop_type = "flow"), "/mine")
        # replace = true (the API's copy) does overwrite
        @test copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow").copied == ["TgtImg"]
        @test !has_pop(load_pop_map(tgt; value_name = "B", pop_type = "flow"), "/mine")
        # a source with no gating on that label set has nothing to copy
        @test_throws ArgumentError copy_gating!(tgt, [src]; value_name = "nope", pop_type = "flow")
    end
end

@testset "gate.manual_gate — prepare copies, check names what is missing" begin
    _gate_fixture() do src, tgt
        task = Cecelia._task_from_fun_name("gate.manual_gate")
        @test task_manual(task)["open"] == "gate"
        @test Cecelia._required_pops(" qc, /qc/cells ,") == ["/qc", "/qc/cells"]
        p = Dict{String,Any}("valueName" => "B", "popType" => "flow", "requiredPops" => "/qc")
        # not gated yet → the check refuses, naming the population
        @test occursin("/qc", something(manual_check(task, tgt, p), ""))
        # prepare with an exemplar copies its gates; the check then passes
        out = Cecelia._run_task(task, tgt, merge(p, Dict{String,Any}("copyFrom" => "KDIeEm")); on_log = _ -> nothing)
        @test out["copied"] === true
        @test isnothing(manual_check(task, tgt, p))
        # nothing required → always passes
        @test isnothing(manual_check(task, tgt, Dict{String,Any}("valueName" => "B", "popType" => "flow")))
        # no exemplar → prepare does nothing
        @test Cecelia._run_task(task, src, p; on_log = _ -> nothing)["copied"] === false
    end
end
