# Cohort + Analysis board API testsets — extracted from api/test/runtests.jl.
#
# Seven testsets covering the /api/cohort/* and /api/analysis/* surface:
#  - `API: cohort QC`
#  - `API: cohort runs (per clustering run selector)`
#  - `API: analysis lineage`
#  - `API: analysis populations`
#  - `API: analysis measures`
#  - `API: analysis behaviour + clusters`
#  - `API: analysis chains`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: cohort QC" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _qc(t) = api_qc_cohort(HTTP.Request("GET", "/api/qc/cohort" * t))
    _check(b) = api_qc_cohort_check(Vector{UInt8}(JSON3.write(b)))
    try
        proj = create_project!(name = "api-cohort")
        s    = add_set!(proj; name = "set-A")
        for (nm, n) in [("i1", 800), ("i2", 810), ("i3", 790), ("i4", 805)]
            img = add_image!(s; name = nm, meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
            write_qc(img, "segment.measureLabels", "default", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nCells" => n))
        end
        # clustering banks PER LABEL SET (T/B), not "default" — bank some so discovery has >1 value_name
        for (i, img) in enumerate(images(s))
            write_qc(img, "clustTracks.cluster", "T", Dict{String,Any}[]; metrics = Dict{String,Any}("nTracks" => 40))
            write_qc(img, "clustTracks.cluster", "B", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks" => i == 1 ? 9 : 23))   # i1 sparse in B
        end
        base = "?projectUid=$(proj.uid)&setUid=$(s.uid)"
        sidecar = joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort",
                           "segment.measureLabels", "default.json")
        # GET validation
        @test _qc("")[1] == 400                                              # missing params
        @test _qc("$base&funName=bad.fun")[1] == 400                         # not a metric producer
        @test _qc("?projectUid=$(proj.uid)&setUid=nope&funName=segment.measureLabels")[1] == 404
        # GET with an explicit valueName → single doc; READ-ONLY (no sidecar)
        st, body = _qc("$base&funName=segment.measureLabels&valueName=default")
        @test st == 200
        d = JSON3.read(body)
        @test d.nIncluded == 4 && d.metrics.nCells.n == 4
        @test d.metrics.nCells.mean == 801.25                               # (800+810+790+805)/4
        @test !isfile(sidecar)                                              # a GET must not write
        # GET with NO valueName → per-value_name map (byValueName). segment banks under "default"…
        dv = JSON3.read(_qc("$base&funName=segment.measureLabels")[2])
        @test collect(dv.valueNames) == ["default"] && dv.byValueName.default.nIncluded == 4
        # …clustering under T and B — both discovered, the sparse i1 flags in B only
        dc = JSON3.read(_qc("$base&funName=clustTracks.cluster")[2])
        @test Set(String.(dc.valueNames)) == Set(["B", "T"])
        i1 = images(s)[1].uid
        @test haskey(dc.byValueName.B.metrics.nTracks.outliers, Symbol(i1))
        @test isempty(dc.byValueName.T.metrics.nTracks.outliers)
        # POST /check (no valueName) → checks every label set, persists each sidecar
        @test _check((;))[1] == 400                                          # missing params
        @test _check((; projectUid = proj.uid, setUid = s.uid, funName = "bad.fun"))[1] == 400
        stc, bc = _check((; projectUid = proj.uid, setUid = s.uid, funName = "segment.measureLabels"))
        @test stc == 200 && isfile(sidecar)
        @test haskey(JSON3.read(bc), :byValueName)
        _check((; projectUid = proj.uid, setUid = s.uid, funName = "clustTracks.cluster"))
        @test isfile(joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort", "clustTracks.cluster", "B.json"))
        # the cross-image detail lands in the lab log under a "[Cecelia — Cohort check]" entry, by image
        # UID (refs are uid-based; the panel resolves uid→name on demand), with the label set and
        # value-vs-median — not just a bare count
        ll = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$(proj.uid)"))[2]).content
        @test occursin("Cohort check", ll) && occursin("clustTracks.cluster (B)", ll)
        @test occursin("$(i1) — nTracks", ll) && occursin("cohort median", ll)   # image UID (refs are uid-based) + detail
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: cohort runs (per clustering run selector)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _runs(t)  = api_qc_cohort_runs(HTTP.Request("GET", "/api/qc/cohort/runs" * t))
    _check(b) = api_qc_cohort_check(Vector{UInt8}(JSON3.write(b)))
    try
        proj = create_project!(name = "api-cohort-runs")
        s    = add_set!(proj; name = "set-A")
        imgs = [add_image!(s; name = "i$i", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif")) for i in 1:3]
        # two clustering RUNS (movement, test) over label sets T & B, banked via write_cluster_qc! so the
        # composite {labelSet}.{suffix} keys + runSuffix land on disk (what the real task does)
        mkqc(path) = open(path, "w") do io
            JSON3.write(io, Dict("nClusters" => 4, "perSegment" =>
                [Dict("uID" => img.uid, "valueName" => vn, "n" => 40, "nClusters" => 4, "largestClusterFrac" => 0.4)
                 for img in imgs for vn in ("T", "B")]))
        end
        qcdir = mktempdir()
        for suf in ("movement", "test")
            p = joinpath(qcdir, "$suf.json"); mkqc(p)
            Cecelia.write_cluster_qc!(collect(images(s)), "clustTracks.cluster", p; unit = "tracks", suffix = suf)
        end
        base = "?projectUid=$(proj.uid)&setUid=$(s.uid)"
        # GET /runs → both runs, each with its composite value_names
        str, br = _runs("$base&funName=clustTracks.cluster")
        @test str == 200
        rr = JSON3.read(br)
        @test Set(r.run for r in rr.runs) == Set(["movement", "test"])
        testrun = first(r for r in rr.runs if r.run == "test")
        @test sort(String.(testrun.valueNames)) == ["B.test", "T.test"]
        @test isempty(JSON3.read(_runs("$base&funName=segment.cellpose")[2]).runs)   # a fun with no runs → []
        @test _runs("?projectUid=$(proj.uid)&setUid=$(s.uid)")[1] == 400             # missing funName
        # POST /check with run=test persists ONLY the test run's sidecars, not movement's
        _check((; projectUid = proj.uid, setUid = s.uid, funName = "clustTracks.cluster", run = "test"))
        cdir = joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort", "clustTracks.cluster")
        @test isfile(joinpath(cdir, "T.test.json")) && isfile(joinpath(cdir, "B.test.json"))
        @test !isfile(joinpath(cdir, "T.movement.json"))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis lineage" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _lin(t) = api_analysis_lineage(HTTP.Request("GET", "/api/analysis/lineage" * t))
    try
        proj = create_project!(name = "api-lineage")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        append_run_log!(img, "importImages.omezarr", "default", "done")
        append_run_log!(img, "segment.cellpose", "default", "done")
        append_run_log!(img, "tracking.bayesian_tracking", "default", "failed")
        @test _lin("")[1] == 400                                            # missing projectUid
        @test _lin("?projectUid=nope")[1] == 404
        st, body = _lin("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        e = d.images[1]
        @test [String(x.stage) for x in e.steps] == ["import", "segment", "track"]   # ordered pipeline
        @test any(x -> x.status == "failed", e.steps)                                # the failed track surfaces
        @test "import" in d.rollup.pipeline && "track" in d.rollup.pipeline
        # scope to one image
        @test length(JSON3.read(_lin("?projectUid=$(proj.uid)&imageUid=$(img.uid)")[2]).images) == 1
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis populations" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _pops(t) = api_analysis_populations(HTTP.Request("GET", "/api/analysis/populations" * t))
    try
        proj = create_project!(name = "api-pops")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        img.label_props = Dict("A" => "A.h5ad"); save!(img)
        m = Cecelia.PopulationMap(; pop_type = "flow", value_name = "A")
        Cecelia.add_pop!(m, "CD3"; gate = Cecelia.RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        Cecelia.save_pop_map!(m, img)
        @test _pops("")[1] == 400                                            # missing projectUid
        @test _pops("?projectUid=nope")[1] == 404
        st, body = _pops("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        pops = d.images[1].populations
        cd3 = pops[findfirst(p -> p.name == "CD3", pops)]
        @test cd3.popType == "flow" && cd3.gate.kind == "rectangle" && cd3.gate.x_channel == "c1"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis measures" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _meas(t) = api_analysis_measures(HTTP.Request("GET", "/api/analysis/measures" * t))
    try
        proj = create_project!(name = "api-measures")
        s    = add_set!(proj; name = "set-A")
        add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        @test _meas("")[1] == 400                                            # missing projectUid
        @test _meas("?projectUid=nope")[1] == 404
        # 200 + shape; no label props on disk → summaries empty (the deep read path is the pkg fixture test)
        st, body = _meas("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        @test haskey(d.images[1], :summaries) && haskey(d.images[1], :truncated)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis behaviour + clusters" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _beh(t) = api_analysis_behaviour(HTTP.Request("GET", "/api/analysis/behaviour" * t))
    _clu(t) = api_analysis_clusters(HTTP.Request("GET", "/api/analysis/clusters" * t))
    try
        proj = create_project!(name = "api-behclust")
        s    = add_set!(proj; name = "set-A")
        add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        @test _beh("")[1] == 400 && _clu("")[1] == 400                        # missing projectUid
        @test _beh("?projectUid=nope")[1] == 404 && _clu("?projectUid=nope")[1] == 404
        # 200 + shape; no obs on disk → empty lists (the read path is validated off-suite / pkg fixture)
        bd = JSON3.read(_beh("?projectUid=$(proj.uid)")[2])
        @test bd.projectUid == proj.uid && length(bd.images) == 1 && haskey(bd.images[1], :behaviour)
        cd = JSON3.read(_clu("?projectUid=$(proj.uid)")[2])
        @test cd.projectUid == proj.uid && length(cd.images) == 1 && haskey(cd.images[1], :clusters)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis chains" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _ch(t) = api_analysis_chains(HTTP.Request("GET", "/api/analysis/chains" * t))
    try
        proj = create_project!(name = "api-chains")
        Cecelia.save_chain_template!(proj, Cecelia.ChainTemplate("pipe",
            [Cecelia.ChainNode(; id = "n1", fn = "segment.cellpose")], Cecelia.ChainEdge[]))
        @test _ch("")[1] == 400                                              # missing projectUid
        @test _ch("?projectUid=nope")[1] == 404
        st, body = _ch("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && haskey(d, :runs)
        @test d.templates[findfirst(t -> t.name == "pipe", d.templates)].nodes[1].fun == "segment.cellpose"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
