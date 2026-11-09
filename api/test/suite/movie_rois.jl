# ── ROI movies (docs/todo/ROI_TRIAGE_PLAN.md Decision 12) ─────────────────────────────────────────
# Record → What: ROIs. The pure rules (an ROI → its crop / planes / frames, the pick order, the sheet
# shape, the whole-factor enlargement of a small box), then one end-to-end run on the small real
# store: two ROIs → two movies + a contact sheet in the project's movies/ folder.

@testset "API: ROI movies — window, pick, sheet, enlargement" begin
    roi(id, rank, t0, t1; z0 = 1, z1 = 1) = Dict{String,Any}("id" => id, "rank" => rank, "t0" => t0, "t1" => t1,
        "box" => Dict("x0" => 2, "x1" => 17, "y0" => 4, "y1" => 13, "z0" => z0, "z1" => z1))
    # the crop gives the box context: twice its size, at least ROI_CONTEXT_MIN_PX, centred, inside
    w = roi_record_window(roi("a", 1, 3, 5), 10, 3, 500, 500; pad_frames = 2)
    @test length(w.crop.x) == ROI_CONTEXT_MIN_PX && length(w.crop.y) == ROI_CONTEXT_MIN_PX
    @test first(w.crop.x) == 0 && first(w.crop.y) == 0                # shifted in at the image edge, not shrunk
    @test w.z == 1 && w.ts == 1:7
    @test _context_span(200, 299, 1000) == 150:349                     # 100 px box → 200 px, centred
    @test _context_span(0, 9, 40) == 0:39                              # image smaller than the minimum
    # frames clamp to the movie, planes to the stack; a box over several planes is their max
    w2 = roi_record_window(roi("a", 1, 0, 9; z0 = 0, z1 = 7), 10, 3, 500, 500; pad_frames = 2)
    @test w2.ts == 0:9 && w2.z == 0:2
    # a sheet's tile: widened to n_frames, centred, shifted inside the movie, never past it
    @test roi_record_window(roi("a", 1, 3, 5), 20, 3, 500, 500; pad_frames = 2, n_frames = 11).ts == 0:10
    @test roi_record_window(roi("a", 1, 10, 12), 30, 3, 500, 500; pad_frames = 2, n_frames = 11).ts == 6:16
    @test roi_record_window(roi("a", 1, 25, 27), 30, 3, 500, 500; pad_frames = 2, n_frames = 11).ts == 19:29
    @test roi_record_window(roi("a", 1, 3, 5), 6, 3, 500, 500; pad_frames = 2, n_frames = 11).ts == 0:5
    # the pick keeps the ROI list's order, skips ids the set no longer has; none picked = all, by rank
    rs = [roi("a", 2, 0, 1), roi("b", 1, 0, 1), roi("c", 3, 0, 1)]
    @test [r["id"] for r in roi_pick(rs, ["c", "gone", "a"])] == ["c", "a"]
    @test [r["id"] for r in roi_pick(rs, String[])] == ["b", "a", "c"]
    @test [roi_sheet_per_row(n) for n in (0, 1, 2, 4, 5, 9, 10)] == [0, 1, 2, 2, 3, 3, 4]
    @test roi_caption(roi("a", 4, 0, 1)) == "#4"
    @test roi_caption(roi("a", 4, 0, 1), Dict("userLabel" => "ingested")) == "#4 ingested"
    @test roi_movie_tags("debris", false) == ["roi", "debris"]
    @test roi_movie_tags("default", true) == ["roi", "sheet"] && roi_movie_tags("", false) == ["roi"]
    @test figure_auto_tags(Dict("layout" => "grid", "rows" => [1, 2]), ["roi"]) == ["roi", "grid", "split"]
    @test isempty(figure_auto_tags(Dict("rows" => [1])))
    @test _roi_min_px(nothing, nothing) === nothing && _roi_min_px(320, nothing) == 320
    # enlargement: whole factors up to `min_px`; never while striding down; off at 0
    @test _enlarge_factor(16, 10, 1, 64) == 4
    @test _enlarge_factor(16, 10, 1, 70) == 4          # whole factors only: 70 ÷ 16 → 4
    @test _enlarge_factor(16, 10, 2, 64) == 1
    @test _enlarge_factor(16, 10, 1, 0) == 1
    @test _enlarge_factor(100, 10, 1, 64) == 1         # already big enough

    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !api_have_fixture(v2)
        @test_skip "zarr format fixture missing"
    else
        specs = [(0.0, 4000.0, "red", true), (0.0, 4000.0, "green", true)]
        # a 16 × 16 crop asked for at least 64 px comes out 64 × 64 (×4), the scale bar's µm/px with it
        r = record_view_movie(v2, joinpath(mktempdir(), "up.mp4"); ts = [0], specs = specs, channels = 0:1,
                              crop = (x = 0:15, y = 0:15), min_px = 64, on_log = _ -> nothing)
        @test (r.width, r.height) == (64, 64)

        # end to end: a project image on the real store, a two-ROI set, Record → ROIs with a sheet
        conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "roi-movies")
            img = add_image!(add_set!(proj; name = "s"); name = "img", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
            mkpath(img_zero_dir(img)); cp(v2, joinpath(img_zero_dir(img), "ccidImage.ome.zarr"))
            img.filepath = Dict{String,Any}("default" => "ccidImage.ome.zarr", "_active" => "default"); save!(img)
            mkpath(img_rois_dir(img))
            write_json_atomic(img_rois_path(img, "debris"), Dict{String,Any}("version" => 2, "labels" => ["real"],
                "rois" => Any[roi("r1", 1, 0, 1), roi("r2", 2, 1, 2; z0 = 0, z1 = 2)]))
            run_roi_offline("t-roi", proj.uid, img.uid; roi_suffix = "debris", roi_ids = ["r2", "r1"],
                            sheet = true, min_px = 64, match_viewer = false,
                            show_timestamp = false, show_scale_bar = false,
                            movie_config = MovieRecordConfig(; imageUid = img.uid))
            mdir = _movies_dir(init_object(proj.uid, img.uid))
            @test isfile(joinpath(mdir, "img_debris_r1.mp4")) && isfile(joinpath(mdir, "img_debris_r2.mp4"))
            @test isfile(joinpath(mdir, "img_debris_sheet.mp4"))
            # each movie banks ITS ROIs under kind "rois" — what the Movies page re-records from
            reg = _read_movies_registry(proj.uid)
            e1 = reg["img_debris_r1.mp4"]; es = reg["img_debris_sheet.mp4"]
            @test e1["configKind"] == "rois" && collect(String, e1["config"]["rois"]["ids"]) == ["r1"]
            @test e1["config"]["rois"]["sheet"] == false
            # with a sheet, a kept movie is cut to the sheet's length and banks it, so a re-record matches
            @test e1["config"]["rois"]["nFrames"] > 0 && e1["config"]["rois"]["nFrames"] == es["config"]["rois"]["nFrames"]
            # tagged by what they are; a user's tag survives a re-record, which adds the auto ones back
            @test collect(String, e1["tags"]) == ["roi", "debris"] && collect(String, es["tags"]) == ["roi", "debris", "sheet"]
            reg["img_debris_r1.mp4"]["tags"] = ["keep"]; _write_movies_registry!(proj.uid, reg)
            register_movie!(proj.uid, "img_debris_r1.mp4"; auto_tags = ["roi"])
            @test collect(String, _read_movies_registry(proj.uid)["img_debris_r1.mp4"]["tags"]) == ["keep", "roi"]
            @test collect(String, es["config"]["rois"]["ids"]) == ["r2", "r1"] && es["config"]["rois"]["sheet"] == true
            # sheet only: the sheet is written and registered, the tiles are not kept
            run_roi_offline("t-roi-only", proj.uid, img.uid; roi_suffix = "debris", roi_ids = ["r2", "r1"],
                            sheet = true, sheet_only = true, suffix = "only", min_px = 64, match_viewer = false,
                            show_timestamp = false, show_scale_bar = false,
                            movie_config = MovieRecordConfig(; imageUid = img.uid))
            @test isfile(joinpath(mdir, "img_debris_only_sheet.mp4"))
            @test !isfile(joinpath(mdir, "img_debris_only_r1.mp4")) && !isfile(joinpath(mdir, "img_debris_only_r2.mp4"))
            reg2 = _read_movies_registry(proj.uid)
            @test reg2["img_debris_only_sheet.mp4"]["config"]["rois"]["sheetOnly"] == true
            @test !any(k -> occursin("only_r", k), keys(reg2))
            # a set that isn't there fails cleanly, writing nothing
            run_roi_offline("t-roi2", proj.uid, img.uid; roi_suffix = "nope")
            @test !any(f -> occursin("nope", f), readdir(mdir))
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
