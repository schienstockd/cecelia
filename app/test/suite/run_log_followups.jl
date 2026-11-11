# ── Run log follow-ups: re-measure `replaces`, a format number, custom weights, the raw source check ──
# docs/SCHEDULER.md → *The durable half*; app/src/run_log.jl → *Format* / *What it read*.
# Reuses `_rl_prov_fixture` / `_rl_find` from run_log_provenance.jl (included before this file).

@testset "Re-measuring a label set records what it replaced, and stales what read the old measures" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = _rl_prov_fixture()
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    zarr = get!(Cecelia.cecelia_conf(), "zarr", Dict{String,Any}())
    had_kpv = haskey(zarr, "keepPreviousVersion"); old_kpv = get(zarr, "keepPreviousVersion", nothing)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        zarr["keepPreviousVersion"] = false
        img = init_object("testpr", "KDIeEm")
        n = Ref(0)
        function run!(fn, p)
            tid = "rm$(n[] += 1)"
            pv = run_provenance(fn, img, p)
            Cecelia.open_run_log!(img, fn, string(get(p, "valueName", "")), p; task_id = tid,
                                  inputs = pv.inputs, replaces = pv.replaces)
            close_run_log!(img, tid, "done")
            tid
        end
        mp = Dict{String,Any}("outputValueName" => "B", "intensityValueName" => "default")

        # measureLabels writes INTO the label set it reads — named, so its params bank per label set
        @test Cecelia.task_params_name("segment.measureLabels", mp) == "B"
        @test Cecelia.task_writes_into_key("segment.measureLabels") == "outputValueName"
        # a composite resolves the key's namespace through the step that declares it
        @test Cecelia.task_writes_into_key("tracking.bayesian_track_measures") == "valueName"
        lv = Cecelia._history_leaves("tracking.bayesian_track_measures")
        @test Cecelia._run_output_identity("tracking.bayesian_track_measures",
                                           Dict{String,Any}("valueName" => "B"), lv) == ("B", "labels", true)

        seg = run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        # the re-measure replaces the measures the segment + measure run made (they share measureLabels)
        r = run_replaces("segment.measureLabels", img, mp)
        @test r !== nothing && r["name"] == "B" && r["ns"] == "labels" && r["taskId"] == seg
        m1 = run!("segment.measureLabels", mp)
        @test _rl_find(img, m1)["replaces"]["taskId"] == seg
        @test run_replaces("segment.measureLabels", img, mp)["taskId"] == m1     # the next names this one
        # …and tracking still replaces only an earlier tracking, never a measure or the segmentation
        @test run_replaces("tracking.bayesian_tracking", img, Dict{String,Any}("valueName" => "B")) === nothing

        # a run that read the label set by name is stale after a re-measure (its output `br` is on the
        # image). Conservative: the record names a label set, not its pixels vs its measures, so a reader
        # of the pixels only (branching) is flagged too — docs/SCHEDULER.md → *What is stale downstream*.
        commit_state!(img) do raw
            l = Dict{String,Any}(String(k) => v for (k, v) in raw["labels"])
            l["br"] = ["br.zarr"]; raw["labels"] = l
        end
        img = init_object("testpr", "KDIeEm")
        br = run!("segment.branching", Dict{String,Any}("valueName" => "B", "outputValueName" => "br"))
        @test isempty(stale_outputs_for(img).stale)
        m2 = run!("segment.measureLabels", mp)
        s = only(filter(d -> d["name"] == "br", stale_outputs_for(img).stale))
        @test s["run"]["taskId"] == br && only(s["because"])["replacedBy"]["taskId"] == m2

        # stale tracks are not hidden by a later re-measure: tracks and measures are separate outputs
        Cecelia.cecelia_conf()["dirs"]["projects"] = _rl_prov_fixture()
        img = init_object("testpr", "KDIeEm")
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        trk = run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        tracks_stale() = any(d -> d["name"] == "B" && get(d, "into", false) && d["run"]["taskId"] == trk,
                             stale_outputs_for(img).stale)
        @test tracks_stale()
        run!("segment.measureLabels", mp)
        @test tracks_stale()

        # CURRENT tracks go stale when the label set is re-measured: tracking read the measured
        # centroids, and a plain re-measure rebuilds the table without the track columns. Only a
        # write-into that shares a step with the reader (a re-track) is skipped as "its own kind".
        Cecelia.cecelia_conf()["dirs"]["projects"] = _rl_prov_fixture()
        img = init_object("testpr", "KDIeEm")
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        trk2 =run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)            # a re-track still does not stale the last
        rm3 = run!("segment.measureLabels", mp)
        s = only(filter(d -> d["name"] == "B" && get(d, "into", false) &&
                             d["run"]["taskId"] == trk2, stale_outputs_for(img).stale))
        @test only(s["because"])["replacedBy"]["taskId"] == rm3
        # the re-measure itself is clean: it read the segmentation's labels, not the tracks before it
        @test !any(d -> d["run"]["taskId"] == rm3, stale_outputs_for(img).stale)
        # …and a re-track after it leaves both current: it adds track columns, the measures stand
        trk3 = run!("tracking.bayesian_tracking", Dict{String,Any}("valueName" => "B"))
        @test isempty(stale_outputs_for(img).stale)
        @test !any(d -> d["run"]["taskId"] in (rm3, trk3), stale_outputs_for(img).unknown)
        # a second re-measure (its `replaces` names the first, not the segmentation) still stales tracks
        rm4 = run!("segment.measureLabels", mp)
        s = only(filter(d -> d["run"]["taskId"] == trk3, stale_outputs_for(img).stale))
        @test only(s["because"])["replacedBy"]["taskId"] == rm4

        # …but a write-into a later run SUPERSEDED (it shares a step) is no longer current: a lone
        # re-measure is replaced by the next segment + measure, a track correction by the next re-track
        Cecelia.cecelia_conf()["dirs"]["projects"] = _rl_prov_fixture()
        img = init_object("testpr", "KDIeEm")
        tp = Dict{String,Any}("valueName" => "B")
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        run!("tracking.bayesian_track_measures", tp)
        run!("tracking.correct_measures", tp)
        m = run!("segment.measureLabels", mp)
        run!("segment.cellposeMeasure", Dict{String,Any}("valueName" => "default", "outputValueName" => "B"))
        run!("tracking.bayesian_track_measures", tp)
        @test isempty(stale_outputs_for(img).stale)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        had_kpv ? (zarr["keepPreviousVersion"] = old_kpv) : delete!(zarr, "keepPreviousVersion")
    end
end

@testset "Run-log entries carry a format number; an entry without one reads as format 0" begin
    proj = create_project!(name = "runlog-format-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "img")
    Cecelia.open_run_log!(img, "segment.cellpose", "B", Dict{String,Any}(); task_id = "f1")
    close_run_log!(img, "f1", "done")
    close_run_log!(img, "f2", "failed"; fun_name = "segment.cellpose")    # nothing to patch → appended
    Cecelia.skip_run_log!(img, "segment.cellpose", "excluded"; task_id = "f3")
    append_run_log!(img, "segment.cellpose", "B", "done", Dict{String,Any}())
    es = read_run_log(img)
    @test length(es) == 4
    @test all(e -> e["format"] == Cecelia.RUN_LOG_FORMAT, es)
    @test all(e -> run_entry_format(e) == Cecelia.RUN_LOG_FORMAT, es)
    @test run_entry_format(Dict{String,Any}("fun" => "x", "status" => "done")) == 0   # legacy
    @test run_entry_format(JSON3.read("""{"format": 1}""")) == 1                   # JSON3 Symbol keys
end

@testset "Custom segmentation weights are fingerprinted into the run env; built-ins are not" begin
    d = mktempdir()
    w = joinpath(d, "my_model")
    write(w, rand(UInt8, 4096))
    env = Cecelia.segment_model_weights_env(Dict{String,Any}(
        "1" => Dict{String,Any}("model" => w), "2" => Dict{String,Any}("model" => "cpsam")))
    fp = only(env["modelWeights"])
    @test fp == Cecelia.source_fingerprint(w)                  # the same fingerprint the import banks — not a new hash
    @test fp["path"] == w && fp["size"] == 4096
    @test isempty(Cecelia.segment_model_weights_env(Dict{String,Any}("1" => Dict{String,Any}("model" => "cpsam"))))
    @test isempty(Cecelia.segment_model_weights_env(Dict{String,Any}("1" => Dict{String,Any}("model" => joinpath(d, "gone")))))
    # both segmenters stamp it into the runEnv they hand back
    for f in ("cellpose.jl", "coastal.jl")
        @test occursin("segment_model_weights_env(models_converted)",
                       read(joinpath(pkgdir(Cecelia), "src", "tasks", "segment", f), String))
    end
    # a composite's result carries its steps' runEnv (the scheduler reads the env off the LAST step's
    # result, so a segment + measure run otherwise recorded none of its segmenter's)
    r = Cecelia._with_steps_run_env(Dict{String,Any}("valueName" => "B"), Dict{String,Any}("device" => "cuda"))
    @test r["valueName"] == "B" && run_env_of(r) == Dict{String,Any}("device" => "cuda")
    @test Cecelia._with_steps_run_env(nothing, Dict{String,Any}("device" => "cuda")) === nothing
    @test Cecelia._with_steps_run_env(Dict{String,Any}("a" => 1), Dict{String,Any}()) == Dict{String,Any}("a" => 1)
end

@testset "Raw source check: re-fingerprints at ori_path and warns when it changed or is gone" begin
    # pure rules
    rec = Dict{String,Any}("path" => "/x", "size" => 10, "sample" => "ab", "mtime" => "t0")
    @test isempty(Cecelia.source_fingerprint_findings(Dict{String,Any}(), Dict{String,Any}()))      # nothing recorded
    @test isempty(Cecelia.source_fingerprint_findings(rec, merge(rec, Dict("mtime" => "t1"))))        # a copy: same file
    f = only(Cecelia.source_fingerprint_findings(rec, merge(rec, Dict("sample" => "cd"))))
    @test f["code"] == "source.changed" && f["level"] == "warn" && f["detail"]["differs"] == ["sample"]
    f = only(Cecelia.source_fingerprint_findings(rec, Dict{String,Any}()))
    @test f["code"] == "source.missing" && f["level"] == "info"
    withc(c) = merge(rec, Dict{String,Any}("companions" => Dict{String,Any}("count" => c, "bytes" => 5)))
    @test only(Cecelia.source_fingerprint_findings(withc(2), withc(3)))["detail"]["differs"] == ["companions"]

    # through `resync_ome_meta!` on a real image (no zarr store: resync is a no-op, the check still runs)
    d = mktempdir()
    src = joinpath(d, "raw.tif")
    write(src, rand(UInt8, 3000))
    proj = create_project!(name = "src-qc-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "img", meta = Dict{String,Any}(
        "ori_path" => src, "oriFingerprint" => Cecelia.import_source_fingerprint(src)))
    qcp() = qc_path(img, Cecelia.SOURCE_QC_FUN)
    codes() = [String(f[:code]) for f in read_qc(img, Cecelia.SOURCE_QC_FUN)[:findings]]
    resync_ome_meta!(init_object(proj.uid, img.uid))
    @test !isfile(qcp())                                   # unchanged: no doc, not an empty placeholder
    write(src, rand(UInt8, 3000))                          # same size, different bytes
    resync_ome_meta!(init_object(proj.uid, img.uid))
    @test codes() == ["source.changed"]
    rm(src)
    resync_ome_meta!(init_object(proj.uid, img.uid))
    @test codes() == ["source.missing"]
    write(src, rand(UInt8, 10))                            # a new file, recorded as the source (a re-import)
    commit_state!(init_object(proj.uid, img.uid)) do raw
        m = Dict{String,Any}(String(k) => v for (k, v) in raw["meta"])
        m["oriFingerprint"] = Cecelia.import_source_fingerprint(src)
        raw["meta"] = m
    end
    resync_ome_meta!(init_object(proj.uid, img.uid))
    @test isempty(codes())                                 # a matching file clears the earlier finding
    # an image imported before fingerprints were recorded gets no check at all
    bare = add_image!(add_set!(proj; name = "s2"); name = "bare", meta = Dict{String,Any}("ori_path" => src))
    @test Cecelia.write_source_qc!(bare) === nothing
end
