# Image payload + narrow-query API testsets — extracted from api/test/runtests.jl.
#
# Five testsets covering the small API-shape surface the client sees first: points-only
# value_name reaches the payload (labels vs label_props registries), run-log enrichment
# only labels image-writing tasks with outputValueName (lineage forest), register-legacy
# REPAIRS an existing image (does not clobber the migrated pointers), the funparams
# sources picker (per-image (image, valueName) pairs), and /api/plots/populations
# narrowing to one valueName. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl. The extracted file loads
# at the top level of runtests.jl, so any helpers defined earlier (_post, api_fixture,
# api_have_fixture) are still in scope for the extracted testsets (Julia includes are
# lexical).

@testset "API: a points-only value name reaches the client" begin
    # `labels` and `label_props` are two independent ccid.json registries. A track set imported
    # directly — ImageJ, TrackMate — for an image nothing has segmented registers only the second:
    # there are no mask pixels to register. The payload carried `labels` alone, so such a set was
    # invisible to the client: no viewer row, and therefore no tracks toggle, while gating and the
    # observer listed it happily. Reported from the screen ("the imported tracks do not show up").
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-points-only")
        s    = add_set!(proj; name = "s")
        img  = add_image!(s; name = "a")
        mkpath(joinpath(img._dir, "labelProps"))
        img.labels      = Dict("seg" => ["seg.zarr"])            # a real segmentation, with pixels
        img.label_props = Dict("seg" => "seg.h5ad",              # …its measurement table
                               "trackmate" => "trackmate.h5ad")  # …and an import with NO mask
        save!(img)

        payload = _image_payload(img)
        @test collect(keys(payload.labels)) == ["seg"]                    # unchanged: masks only
        @test Set(payload.labelPropsNames) == Set(["seg", "trackmate"])   # the union is derivable
        # Both registries are surfaced SEPARATELY rather than merged server-side: the client needs the
        # difference to decide which toggles a row can offer (tracks need only a `track_id` column;
        # the show-labels eye needs pixels).
        @test "trackmate" ∉ collect(keys(payload.labels))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: run-log enrichment only labels image-writing tasks with outputValueName" begin
    # `outputValueName` on a run-log entry drives the version-lineage tree in
    # ImageMetadataDialog. `outputValueName` is polymorphic in the task registry: for
    # `cleanupImages.*` / `editImages.*` it names a NEW STORED IMAGE VERSION, for
    # `segment.cellposeMeasure` (and other segmentation/measurement/tracking tasks) it names a
    # LABEL/TRACKS/MEASUREMENT set instead. Enriching indiscriminately produced a real regression
    # on the `zolIMa/fXgbTl`: a `segment.cellposeMeasure` written from `smoothed` with
    # `outputValueName=default` claimed the `default` image version was produced from `smoothed`,
    # closing a cycle (`smoothed → driftCorrected → default → smoothed`) that gave the forest zero
    # roots and hid the whole lineage panel. Enrichment must therefore only annotate tasks that
    # actually write a new image store.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-lineage")
        s    = add_set!(proj; name = "s")
        img  = add_image!(s; name = "a")
        img.filepath = Dict("default" => "ccidImage.ome.zarr",
                            "driftCorrected" => "ccidDriftCorrected.ome.zarr",
                            "smoothed" => "ccidSmoothed.ome.zarr")
        save!(img)
        # A cleanupImages entry: SHOULD be enriched (writes an image version).
        append_run_log!(img, "cleanupImages.driftCorrect", "default", "done", nothing;
                        at = "2026-08-05T15:55:50")
        # A segment.cellposeMeasure entry: MUST NOT be enriched — its `outputValueName` param names
        # a labels output ('default' labels), not the 'default' image version.
        append_run_log!(img, "segment.cellposeMeasure", "smoothed", "done",
                        Dict("outputValueName" => "default"); at = "2026-08-08T12:34:53")

        entries = _enriched_run_log(img)
        clean  = entries[findfirst(e -> e["fun"] == "cleanupImages.driftCorrect", entries)]
        measure = entries[findfirst(e -> e["fun"] == "segment.cellposeMeasure", entries)]
        @test get(clean, "outputValueName", "") == "driftCorrected"     # writes a version
        @test !haskey(measure, "outputValueName")                       # writes labels, not a version
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: register-legacy REPAIRS an existing image instead of clobbering it" begin
    # Recovery path for images migrated on pre-2026-09-17 code: that build overwrote `img.meta`
    # wholesale at the end of the migrate task, wiping `legacySourceDir`/`legacySourceUid`. A later
    # re-run (copy → symlink, or a partial retry) then dies at the "no legacy source" guard because
    # the task falls back to `img.meta` and there is nothing to fall back to. Users landed on the
    # `Migrate legacy` dialog to fix it and pointed at the same source project — but the dialog was
    # calling `add_image!` which would CLOBBER the migrated `filepath` / `label_props` / attr, so
    # repair through the UI destroyed the migrated data. This test pins the repair path: existing UID
    # → PATCH the two pointers, do not touch anything else.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-legacy-repair")
        s    = add_set!(proj; name = "s")
        # Simulate a post-migration image: has filepath / label_props / attr but its meta was
        # clobbered by the pre-fix migrate — no legacy pointers left.
        img  = add_image!(s; name = "a", uid = "leg1",
                          meta = Dict{String,Any}("SizeX" => 512, "SizeY" => 512))
        img.filepath    = Dict{String,String}("default" => "ccidImage.ome.zarr")
        img.label_props = Dict{String,String}("default" => "seg.h5ad")
        img.attr        = Dict{String,String}("mouse" => "m1")
        save!(img)

        body = Dict("projectUid" => proj.uid, "setUid" => s.uid,
                    "sourceProjectDir" => "/legacy/proj",
                    "images" => [Dict("uid" => "leg1", "name" => "a")])
        code, resp = _post(api_import_register_legacy, body)
        @test code == 200
        parsed = JSON3.read(resp)
        @test length(parsed.images) == 1
        @test parsed.images[1].status == "repaired"

        # Reload from disk and check: legacy pointers restored, everything else INTACT.
        proj2 = load_project(proj.uid)
        s2    = proj2._sets[1]
        img2  = image_by_uid(s2; uid = "leg1")
        @test !isnothing(img2)
        @test img2.meta["legacySourceDir"] == "/legacy/proj"
        @test img2.meta["legacySourceUid"] == "leg1"
        @test img2.meta["SizeX"] == 512                   # existing meta preserved
        @test img2.filepath["default"]    == "ccidImage.ome.zarr"   # migrated data untouched
        @test img2.label_props["default"] == "seg.h5ad"
        @test img2.attr["mouse"]          == "m1"
        # image_uids should NOT have grown (existing image reused, not appended)
        @test count(==("leg1"), s2.image_uids) == 1

        # NEW image on the same call goes through add_image! → status "pending".
        body2 = Dict("projectUid" => proj.uid, "setUid" => s.uid,
                     "sourceProjectDir" => "/legacy/proj",
                     "images" => [Dict("uid" => "leg2", "name" => "b")])
        code2, resp2 = _post(api_import_register_legacy, body2)
        @test code2 == 200
        @test JSON3.read(resp2).images[1].status == "pending"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: /api/tasks/funparams/sources lists per-image (image, valueName) pairs" begin
    # The Copy-from-a-previous-run picker reaches for records that live PER IMAGE. Same set as the
    # form; two sources, unioned: `meta.funParamsByName[fun]` keys (the by-name blob) and matching
    # `run_log` `done` entries (the retroactive backfill, so runs that predate the by-name feature
    # are still reachable). Same rule as `run_log_params_for_output`.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-funparams-sources")
        s    = add_set!(proj; name = "s")
        imgA = add_image!(s; name = "imgA")
        imgB = add_image!(s; name = "imgB")
        fun  = "segment.cellposeMeasure"

        # imgA: banked by-name record for "Tcell"
        write_module_fun_params!(imgA._dir, fun,
            Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 12);
            value_name = "Tcell")
        # imgB: only in the run log (Neutrophil), so it must be picked up by the retroactive half
        append_run_log!(imgB, fun, "default", "done",
                        Dict("outputValueName" => "Neutrophil", "cellDiameter" => 8);
                        at = "2026-09-05T12:00:00")

        ask(qs) = api_task_fun_params_sources(HTTP.Request("GET", "/api/tasks/funparams/sources?" * qs))
        st, body = ask("projectUid=$(proj.uid)&setUid=$(s.uid)&fun=$fun")
        @test st == 200
        rows = JSON3.read(body)
        pairs = Set((String(r.imageUid), String(r.valueName)) for r in rows)
        @test (imgA.uid, "Tcell")      in pairs                    # from the by-name blob
        @test (imgB.uid, "Neutrophil") in pairs                    # from the run log
        # `at` present for the log-sourced row (that is what makes newest-first sortable) and empty
        # for the by-name-only row (no timestamp to invent).
        rowB = rows[findfirst(r -> String(r.imageUid) == imgB.uid, rows)]
        @test String(rowB.at) == "2026-09-05T12:00:00"
        @test String(rowB.imageName) == "imgB"

        # An unknown fun answers 200 with an empty list rather than an error — a form for a fun the
        # project has never run is a legitimate empty state, not a failure.
        st2, body2 = ask("projectUid=$(proj.uid)&setUid=$(s.uid)&fun=nope.nope")
        @test st2 == 200
        @test isempty(JSON3.read(body2))

        # 400 on missing args; 404 on wrong project/set.
        @test ask("projectUid=$(proj.uid)&fun=$fun")[1] == 400
        @test ask("projectUid=NOPE&setUid=$(s.uid)&fun=$fun")[1] == 404
        @test ask("projectUid=$(proj.uid)&setUid=NOPE&fun=$fun")[1] == 404

        # And the picker's picked row round-trips through the existing single-image lookup —
        # that is the only reason the sources route needs to exist (the fetch itself is unchanged).
        st3, body3 = api_task_fun_params(HTTP.Request("GET",
            "/api/tasks/funparams?projectUid=$(proj.uid)&fun=$fun&imageUid=$(imgA.uid)&valueName=Tcell"))
        @test st3 == 200
        d = JSON3.read(body3)
        @test d.matched == true
        @test String(d.params.outputValueName) == "Tcell"
        @test Int(d.params.cellDiameter) == 12
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# The picker's third selector. The gating/track canvas is pinned to ONE segmentation (its toolbar
# select) and the summary canvas overlays them all; both ask the same route, so the route has to be
# askable either way. Narrowing matters beyond tidiness: building the answer evaluates every tracked
# segmentation's gates, so an unnarrowed ask for one segmentation buys the rest to throw away.
@testset "API: /api/plots/populations narrows to one valueName" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-pops-vn")
        s    = add_set!(proj; name = "s")
        img  = add_image!(s; name = "a")
        mkpath(joinpath(img._dir, "labelProps"))
        img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad")
        save!(img)
        for vn in ("A", "B")                                     # one gate each, so both have a row
            m = PopulationMap(pop_type = "flow", value_name = vn)
            add_pop!(m, "qc"; gate = RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                   0.0, 1e12, -1e12, 1e12))
            save_pop_map!(m, img)
        end
        ask(extra) = JSON3.read(api_plot_populations(HTTP.Request("GET",
            "/api/plots/populations?projectUid=$(proj.uid)&imageUid=$(img.uid)&popType=live" * extra))[2])

        @test Set(g.valueName for g in ask("")) == Set(["A", "B"])          # absent → every segmentation
        one = ask("&valueName=A")
        @test [g.valueName for g in one] == ["A"]                            # present → that one
        @test [p.path for p in one[1].populations] == ["/qc"]                # …with its populations
        # each population carries its gating-map `uid` — stable per-pop identity minted by
        # `_fresh_pop_uid`, threaded here so the summary-canvas capture/restore path can survive
        # renames (SummaryCanvas.onReshowZoomToSource → reresolvePops matches by uid first).
        @test hasproperty(one[1].populations[1], :uid) && !isempty(one[1].populations[1].uid)
        # a name this image does not have is an empty answer, not a 400: a segmentation can be absent
        # from some images of a set, and "no populations" is the honest reply for those
        @test isempty(ask("&valueName=nope"))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
