# ── Run log: what a run READ, what it REPLACED, and an archive instead of a silent cap ─────────────
# docs/todo/CECELIA_STRUCTURE_PLAN.md → Record. The run log named the params and outcome but not which
# input version a run read (unless pinned), nor that it overwrote an earlier output — re-runs overwrite
# by design, so the record is the only place that says so. And past RUN_LOG_CAP entries the oldest were
# dropped without a trace. See app/src/run_log.jl → *What it read* / *Archive*.

# A fresh copy of the testpr fixture (image KDIeEm: version `default`, label set `B`), with `default`
# made versioned (v1, v2 — latest v2) and a `smoothed` version for a re-smooth to replace.
function _rl_prov_fixture()
    dir = mktempdir(); cp(fixture_path("testpr"), joinpath(dir, "testpr"))
    ccid = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
    raw = JSON3.read(read(ccid, String), Dict{String,Any})
    raw["filepath"]["default"]  = Dict{String,Any}("v1" => "old.ome.zarr", "v2" => "ccidImage.ome.zarr",
                                                   "_latest" => "v2")
    raw["filepath"]["smoothed"] = "smoothed.ome.zarr"
    write(ccid, JSON3.write(raw))
    dir
end

_rl_find(img, tid) = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))

@testset "Run log names the inputs a run read and the run it replaced" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = _rl_prov_fixture()
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    zarr = get!(Cecelia.cecelia_conf(), "zarr", Dict{String,Any}())
    had_kpv = haskey(zarr, "keepPreviousVersion"); old_kpv = get(zarr, "keepPreviousVersion", nothing)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        zarr["keepPreviousVersion"] = false                 # in memory only — never set_keep_previous_version!
        img = init_object("testpr", "KDIeEm")

        # an image task: reads the LATEST version of `default`, will overwrite `smoothed` (no run made it)
        p = run_provenance("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default"))
        @test p.inputs == Any[Dict{String,Any}("param" => "valueName", "name" => "default", "version" => "v2")]
        @test p.replaces == Dict{String,Any}("name" => "smoothed", "version" => "v1")   # predates the log

        # a pinned read records the pin, not `_latest`
        p = run_provenance("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default", "version" => "v1"))
        @test only(p.inputs)["version"] == "v1" && only(p.inputs)["pinned"] == true

        # an empty picker means "the active one" — the record names it
        @test only(run_provenance("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "")).inputs)["name"] == "default"

        # the entry carries both; the next run names THIS one as what it replaced
        tid1 = "rlp1-$(rand(1000:9999))"
        sp = Dict{String,Any}("valueName" => "default")
        pv = run_provenance("cleanupImages.smooth", img, sp)
        Cecelia.open_run_log!(img, "cleanupImages.smooth", "default", sp; task_id = tid1,
                              inputs = pv.inputs, replaces = pv.replaces)
        close_run_log!(img, tid1, "done")
        e1 = _rl_find(img, tid1)
        @test e1["inputs"][1]["version"] == "v2"
        r = run_replaces("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default"))
        @test r["taskId"] == tid1 && r["fun"] == "cleanupImages.smooth" && r["at"] == e1["at"]
        # …but a failed run made nothing, so it is never named as the maker
        tid2 = "rlp2-$(rand(1000:9999))"
        Cecelia.open_run_log!(img, "cleanupImages.smooth", "default", Dict{String,Any}("valueName" => "default"); task_id = tid2)
        close_run_log!(img, tid2, "failed")
        @test run_replaces("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default"))["taskId"] == tid1

        # keepPreviousVersion: an image version is minted beside the old one — nothing is replaced
        zarr["keepPreviousVersion"] = true
        @test run_provenance("cleanupImages.smooth", img, Dict{String,Any}("valueName" => "default")).replaces === nothing
        zarr["keepPreviousVersion"] = false

        # a fixed `default` output is a NEW image (bin, crop, projections) — the source's raw data is
        # not replaced, and an earlier bin is never named as the maker of it
        tb = "rlpb-$(rand(1000:9999))"
        Cecelia.open_run_log!(img, "editImages.bin", "default", Dict{String,Any}("valueName" => "default"); task_id = tb)
        close_run_log!(img, tb, "done")
        @test run_replaces("editImages.bin", img, Dict{String,Any}("valueName" => "default")) === nothing

        # a NEW output replaces nothing
        @test run_replaces("segment.cellposeMeasure", img,
                           Dict{String,Any}("valueName" => "default", "outputValueName" => "fresh")) === nothing
        # a named label set that exists is replaced — label sets overwrite whatever the toggle says
        zarr["keepPreviousVersion"] = true
        r = run_replaces("segment.cellposeMeasure", img, Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        @test r["name"] == "B" && r["ns"] == "labels"
        zarr["keepPreviousVersion"] = false

        # a task that writes INTO its input (tracking): the input is the label set, and it replaces the
        # previous tracking of it — not the segmentation that made `B`
        tp = Dict{String,Any}("valueName" => "B")
        p = run_provenance("tracking.bayesian_tracking", img, tp)
        @test only(p.inputs) == Dict{String,Any}("param" => "valueName", "name" => "B", "ns" => "labels", "version" => "v1")
        @test p.replaces === nothing                                    # never tracked
        tid3 = "rlp3-$(rand(1000:9999))"
        Cecelia.open_run_log!(img, "tracking.bayesian_tracking", "B", tp; task_id = tid3)
        close_run_log!(img, tid3, "done")
        r = run_replaces("tracking.bayesian_tracking", img, tp)
        @test r["name"] == "B" && r["ns"] == "labels" && r["taskId"] == tid3

        # the scheduler path itself stamps them (one place, every task): a real run of a test task with
        # no declared inputs gets neither field — absent, not empty
        tid4 = "rlp4-$(rand(1000:9999))"
        @test run_task(Cecelia.TestImageTask(), img, Dict{String,Any}(); task_id = tid4) !== nothing
        e4 = _rl_find(img, tid4)
        @test !haskey(e4, "inputs") && !haskey(e4, "replaces")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        had_kpv ? (zarr["keepPreviousVersion"] = old_kpv) : delete!(zarr, "keepPreviousVersion")
    end
end

@testset "Scheduler stamps inputs and replaces on the run-log entry" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = _rl_prov_fixture()
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        img = init_object("testpr", "KDIeEm")
        # a real task through `run_task`: the handler fails on a channel the image lacks, before any
        # work — but its entry was opened (and stamped) first, which is the point of opening early
        tid = "rlps-$(rand(1000:9999))"
        task = Cecelia._task_from_fun_name("cleanupImages.smooth")
        run_task(task, img, Dict{String,Any}("valueName" => "default", "channels" => ["nope"]); task_id = tid)
        e = _rl_find(img, tid)
        @test String(e["fun"]) == "cleanupImages.smooth"
        @test e["inputs"][1]["name"] == "default" && e["inputs"][1]["version"] == "v2"
        @test e["replaces"]["name"] == "smoothed" && !haskey(e["replaces"], "taskId")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

@testset "Run log rotates past the cap into an archive instead of dropping" begin
    proj = create_project!(name="runlog-archive-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")
    cap  = Cecelia.RUN_LOG_CAP

    # the first run that made `keep` — soon to be far older than the cap
    append_run_log!(img, "segment.cellposeMeasure", "default", "done",
                    Dict{String,Any}("outputValueName" => "keep", "diameter" => 17))
    for i in 1:(cap + 10); append_run_log!(img, "x.$i"); end

    live = read_run_log(img)
    @test length(live) == cap                                       # the main file stays capped
    @test live[end]["fun"] == "x.$(cap + 10)"
    arch = read_run_log_archive(img)
    @test length(arch) == 11                                        # …and nothing was dropped
    @test arch[1]["fun"] == "segment.cellposeMeasure" && arch[end]["fun"] == "x.10"
    hist = read_run_log_history(img)
    @test length(hist) == cap + 11 && hist[1]["fun"] == "segment.cellposeMeasure"

    # the reader that answers "what made this name" still finds the rotated-out run
    hit = run_log_params_for_output(img._dir, "segment.cellposeMeasure", "keep")
    @test hit !== nothing && hit["diameter"] == 17

    # a torn last line (a kill mid-append) is skipped, not fatal
    open(io -> print(io, "{\"fun\": \"torn"), run_log_archive_path(img), "a")
    @test length(read_run_log_archive(img)) == 11
    append_run_log!(img, "x.more")                                  # rotation continues past it
    @test any(e -> e["fun"] == "x.11", read_run_log_archive(img))

    # a label-set rename reaches the archive too, `inputs` included — else the archive fallback
    # would answer for a name that no longer exists
    Cecelia.open_run_log!(img, "tracking.bayesian_tracking", "keep", Dict{String,Any}("maxLost" => 1);
                          task_id = "rn", inputs = Any[Dict{String,Any}("param" => "valueName",
                                                       "name" => "keep", "ns" => "labels")])
    for i in 1:cap; append_run_log!(img, "y.$i"); end              # rotate it out
    Cecelia._rename_run_log!(img, "keep", "kept")
    rn = only(filter(e -> get(e, "taskId", "") == "rn", read_run_log_archive(img)))
    @test rn["valueName"] == "kept" && rn["inputs"][1]["name"] == "kept"

    # a reset keeps the archive with the main file — it is history, not output
    @test Cecelia.RUN_LOG_ARCHIVE_FILE in Cecelia.ANALYSIS_KEEP
    rm(proj.root; recursive = true)
end

# ── Staleness from the record: an upstream re-run leaves its downstream outputs stale ──────────────
# `stale_outputs_for` (correction_staleness.jl). A downstream output is stale when its maker read an
# input a later run replaced, directly or through an upstream maker that is itself stale; a maker with
# no recorded inputs is unknown, never stale.
@testset "Run log: an upstream re-run marks downstream outputs stale, with the chain of why" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = _rl_prov_fixture()
    ccid = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
    raw = JSON3.read(read(ccid, String), Dict{String,Any})
    raw["filepath"]["afCorrected"] = "af.ome.zarr"
    write(ccid, JSON3.write(raw))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    zarr = get!(Cecelia.cecelia_conf(), "zarr", Dict{String,Any}())
    had_kpv = haskey(zarr, "keepPreviousVersion"); old_kpv = get(zarr, "keepPreviousVersion", nothing)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        zarr["keepPreviousVersion"] = false
        img = init_object("testpr", "KDIeEm")
        n = Ref(0)
        # one run through the record as the scheduler writes it; `prov = false` = an entry from before
        # inputs were recorded; `replaces` overrides what run_provenance would say
        function run!(fn, p; prov = true, replaces = :auto)
            tid = "st$(n[] += 1)"
            pv = run_provenance(fn, img, p)
            Cecelia.open_run_log!(img, fn, string(get(p, "valueName", "")), p; task_id = tid,
                                  inputs = prov ? pv.inputs : Any[],
                                  replaces = replaces === :auto ? pv.replaces : replaces)
            close_run_log!(img, tid, "done")
            tid
        end
        stale_names() = Set((d["name"], get(d, "into", false)) for d in stale_outputs_for(img).stale)
        find(name; into = false) = only(filter(d -> d["name"] == name && get(d, "into", false) == into,
                                               stale_outputs_for(img).stale))

        # a legacy run of a task that reads names: unknown, not stale
        run!("cleanupImages.afCorrect", Dict{String,Any}("valueName" => "default"); prov = false)
        @test only(stale_outputs_for(img).unknown)["name"] == "afCorrected"

        sm1  = run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default"))
        seg1 = run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "outputValueName" => "B"))
        trk  = run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)

        # re-smooth: the segmentation read the `smoothed` it replaced; the tracking is stale through it
        sm2 = run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default"))
        @test stale_names() == Set([("B", false), ("B", true)])
        b = find("B")
        @test b["ns"] == "labels" && b["run"]["taskId"] == seg1
        @test only(b["because"])["read"] == Dict{String,Any}("name" => "smoothed", "version" => "v1")
        @test only(b["because"])["replacedBy"]["taskId"] == sm2
        @test occursin("since replaced by cleanupImages.smooth", b["summary"])
        t = find("B"; into = true)
        @test t["run"]["taskId"] == trk && length(t["because"]) == 2
        @test t["because"][1]["madeBy"]["taskId"] == seg1 && t["because"][2]["replacedBy"]["taskId"] == sm2

        # re-segment: B is current again; the tracking now read a B that was replaced, directly
        seg2 = run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "outputValueName" => "B"))
        @test stale_names() == Set([("B", true)])
        @test only(find("B"; into = true)["because"])["replacedBy"]["taskId"] == seg2
        # …and re-tracking clears it
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)
        # re-tracking again (nothing upstream changed) does not stale or cloud the track before it: a
        # track run reads the labels, not the earlier tracks it replaces
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)
        @test !any(d -> d["name"] == "B", stale_outputs_for(img).unknown)

        # a pinned read is stale only when THAT version is replaced
        run!("cleanupImages.afCorrect", Dict{String,Any}("valueName" => "default", "version" => "v1"))
        @test isempty(stale_outputs_for(img).unknown)           # its current maker records inputs now
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default");
             replaces = Dict{String,Any}("name" => "default", "version" => "v2"))
        @test !(("afCorrected", false) in stale_names())
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default");
             replaces = Dict{String,Any}("name" => "default", "version" => "v1"))
        @test ("afCorrected", false) in stale_names()

        # unknown propagates: a tracking that read a label set made by a run from before inputs were
        # recorded is unknown too, not clean
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "outputValueName" => "B"); prov = false)
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        unk = stale_outputs_for(img).unknown
        @test any(d -> d["name"] == "B" && !haskey(d, "into"), unk)
        @test any(d -> d["name"] == "B" && get(d, "into", false) && occursin("cannot say", d["reason"]), unk)
        # …and once the segmentation is re-run with its inputs recorded, both are known again
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "outputValueName" => "B"))
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test !any(d -> d["name"] == "B", stale_outputs_for(img).unknown)

        # two runs wrote `smoothed` (v1, then a minted v2, so the second records no `replaces`): a
        # segmentation pinned to v1 does not take the v2 maker's word for it. Only v1's maker is stale →
        # the record cannot tell which made the version read: unknown, not clean
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "afCorrected"))       # smoothed v1
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default"); replaces = nothing)  # v2
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "version" => "v1",
                                                          "outputValueName" => "B"))
        run!("cleanupImages.afCorrect", Dict{String,Any}("valueName" => "default"))      # replaces afCorrected
        @test ("smoothed", false) ∉ stale_names()         # its current maker (v2) read `default` only
        @test ("B", false) ∉ stale_names()
        @test any(d -> d["name"] == "B" && !haskey(d, "into"), stale_outputs_for(img).unknown)

        # a PINNED read whose only logged writer may not have made that version (v1 predates the log;
        # the logged run minted v2): that writer going stale makes the reader unknown, not stale
        Cecelia.cecelia_conf()["dirs"]["projects"] = _rl_prov_fixture()
        img = init_object("testpr", "KDIeEm")
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default"); replaces = nothing)  # smoothed v2
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "smoothed", "version" => "v1",
                                                          "outputValueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)
        run!("cleanupImages.smooth", Dict{String,Any}("valueName" => "default", "outputValueName" => "x");
             replaces = Dict{String,Any}("name" => "default", "version" => "v2"))   # `default` v2 replaced
        @test ("B", false) ∉ stale_names()
        @test any(d -> d["name"] == "B", stale_outputs_for(img).unknown)

        # the lineage carries it, per image
        lin = Cecelia._image_lineage(init_object("testpr", "KDIeEm"))
        @test lin.stale == stale_outputs_for(img).stale
        @test lin.staleUnknown isa AbstractVector
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        had_kpv ? (zarr["keepPreviousVersion"] = old_kpv) : delete!(zarr, "keepPreviousVersion")
    end
end
