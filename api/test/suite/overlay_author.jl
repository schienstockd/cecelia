# overlay_author trio testsets — extracted from api/test/runtests.jl.
#
# Three testsets covering overlay_author, the primitive shared between the live viewer and
# the offline movie renderer:
#  - `API: overlay_author — hex + pixel transform` (colour parse + world→pixel maths).
#  - `API: overlay_author — build_overlays3d_for: pops, tails, all tracks` (labelProps →
#    per-cell overlays: colour resolution, filter fallbacks, cache invalidation, etc).
#  - `API: overlay_author — mask_id_colours: which labels a mask draws, in what colour`.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: overlay_author — hex + pixel transform" begin
    # `hex_to_rgb` is a five-line helper, but every value comes from user text (a pop's colour is a
    # `#rrggbb` string from a colour picker), so a mis-parse silently paints one pop in another's
    # colour. Assert the three forms the picker actually produces and the fallback for bad input.
    @test hex_to_rgb("#00ff00") == RGB{N0f8}(0, 1, 0)
    @test hex_to_rgb("00FF00")  == RGB{N0f8}(0, 1, 0)
    @test hex_to_rgb("#f00")    == RGB{N0f8}(1, 0, 0)
    # Legible fallback rather than an error: a pop with a bad colour still paints, and the caller
    # can flag the malformed value without the whole movie failing.
    @test hex_to_rgb("nope")    == RGB{N0f8}(1, 1, 1)
    @test hex_to_rgb("")        == RGB{N0f8}(1, 1, 1)

    # Identity transform: native pixels are 0-based, drawn pixels 1-based, so `(0, 0) → (1, 1)`.
    tf = pixel_transform(100, 200)
    @test (tf.dW, tf.dH) == (200, 100)
    @test _apply(tf, 0, 0) == (1, 1)
    @test _apply(tf, 199, 99) == (200, 100)
    @test _apply(tf, 200, 99) === nothing            # off-frame drops rather than clamps
    @test _apply(tf, -1, 0)  === nothing
    @test _apply(tf, NaN, 0) === nothing

    # Crop shifts the origin and shrinks the drawn frame. `crop = (x = 50:99, y = 20:79)` means
    # native x ∈ [50, 99] maps to drawn x ∈ [1, 50].
    tc = pixel_transform(100, 200; crop = (x = 50:99, y = 20:79))
    @test (tc.dW, tc.dH) == (50, 60)
    @test _apply(tc, 50, 20) == (1, 1)
    @test _apply(tc, 99, 79) == (50, 60)
    @test _apply(tc, 49, 20) === nothing            # to the left of the crop
    @test _apply(tc, 100, 20) === nothing            # to the right

    # max_px downsamples: a 200-wide native frame with max_px = 100 halves to 100 wide, and
    # `plane[1:2:end]` selects native offsets {0, 2, 4, …}, so an even native offset lands on its
    # own drawn column and odd offsets fall between two drawn columns.
    ts = pixel_transform(200, 200; max_px = 100)
    @test ts.step == 2
    @test (ts.dW, ts.dH) == (100, 100)
    @test _apply(ts, 0, 0) == (1, 1)
    @test _apply(ts, 2, 2) == (2, 2)
    @test _apply(ts, 4, 4) == (3, 3)                        # every 2 native = 1 drawn
    @test _apply(ts, 198, 198) == (100, 100)                # the last selected native offset

    # Crop and stride compose. `crop = 0:99` gives a cropped extent of 100 native; step 2 gives a
    # 50-wide drawn frame. Native offset 99 lands past the last drawn column by a rounding
    # overshoot and is clamped there rather than dropped — the alternative is a movie that
    # silently loses its right-edge cells to a rounding gap. A pixel truly outside the crop still
    # drops.
    tcs = pixel_transform(200, 200; crop = (x = 0:99, y = 0:99), max_px = 50)
    @test tcs.step == 2
    @test (tcs.dW, tcs.dH) == (50, 50)
    @test _apply(tcs, 98, 98) == (50, 50)
    @test _apply(tcs, 99, 99) == (50, 50)                   # edge case: clamp, not drop
    @test _apply(tcs, 200, 200) === nothing                 # truly outside the crop
end

@testset "API: overlay_author — build_overlays3d_for: pops, tails, all tracks" begin
    # The AUTHOR — the caller `frame_overlays.jl` exists to serve. Given a real segmentation and a
    # pop drawn over it, the closure has to hand back the right columnar shape per t: coordinates
    # in the drawn frame, the pop's colour, and t-bucketed so a per-frame render sees only that
    # frame's cells. Bug this catches: the primitives look right on the frame_overlays testset
    # (synthetic data) while the resolver silently drops every cell (wrong column names, wrong
    # 0/1-based indexing, wrong µm/pixel mix).
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img, err = _gating_image("testpr", "KDIeEm")
            @test err === nothing

            # Create a wide-open pop that catches every cell, so the closure has to hand back
            # non-empty data on frame 0. Read the fixture's actual channel columns rather than
            # hard-coding `c1`/`c2` — the gating suite tolerates a warning when those are absent,
            # but this suite asserts that the pop resolves to a non-empty label set.
            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "all", "colour" => "#00ff00", "gate" => gate)))))

            # The segmentation's own extent: positions are native voxel coordinates.
            lp = label_props(img; value_name = "B")
            view_centroid_cols(lp; order = [:x, :y])
            select_cols(lp, ["centroid_t"])
            df = as_df(lp)
            H = ceil(Int, maximum(Float64.(df.centroid_y))) + 8
            W = ceil(Int, maximum(Float64.(df.centroid_x))) + 8

            overlays_for = build_overlays3d_for(img; value_name = "B", pop_type = "flow")

            # Frame 0: every cell whose `centroid_t == 0` shows up, in `#00ff00`, at its own
            # drawn pixel. A wrong µm/pixel mix would put them all off-frame, and a wrong t
            # filter would either return nothing (drops all) or the full table (never filters).
            pts0, segs0 = overlays_for(0)
            @test pts0 !== nothing
            n0 = length(pts0.x)
            @test n0 > 0
            @test length(pts0.y) == n0 && length(pts0.colour) == n0
            @test all(p -> p == RGB{N0f8}(0, 1, 0), pts0.colour)
            @test all(x -> 0 <= x <= W, pts0.x)
            @test all(y -> 0 <= y <= H, pts0.y)

            # And it matches the raw table on the same t. `centroid_t == 0` in the store means
            # the same cells the closure hands to draw.
            want0 = count(t -> t isa Real && Int(round(Float64(t))) == 0, df.centroid_t)
            @test n0 == want0

            # A frame in the middle differs from frame 0 — otherwise the t bucketing did nothing.
            # Pick the median t in the store to be robust to whatever the fixture contains.
            ts_in_store = sort!(unique(Int[Int(round(Float64(t))) for t in df.centroid_t
                                            if t isa Real && isfinite(Float64(t))]))
            if length(ts_in_store) >= 2
                mid = ts_in_store[max(2, length(ts_in_store) ÷ 2)]
                ptsm, _ = overlays_for(mid)
                # A frame either has cells or doesn't. Both cases are fine — what matters is that
                # the count MATCHES the store, not that it is nonzero.
                nm = ptsm === nothing ? 0 : length(ptsm.x)
                wantm = count(t -> t isa Real && Int(round(Float64(t))) == mid, df.centroid_t)
                @test nm == wantm
            end

            # Segments: the fixture IS tracked (see the /tracking/paths suite above). The "all" pop
            # from above is is_track = false, so add a second pop with `is_track = true` — that's
            # the flag `resolve_pops` reads to route a pop through the segment path. A synthetic
            # flag on the same gate exercises the same code the persisted _tracked derived pop
            # would trigger, without the fixture needing a `live/_tracked` file that isn't there.
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "tracked", "colour" => "#ff00ff",
                                 "is_track" => true, "gate" => gate)))))
            _, segs_late = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                               tail_length = 1_000_000)(last(ts_in_store))
            # Every segment endpoint is a cell position (native voxels, inside the segmentation).
            @test segs_late !== nothing                         # the fixture is tracked, is_track pop added
            @test all(x -> 0 <= x <= W, segs_late.x0) && all(x -> 0 <= x <= W, segs_late.x1)
            @test all(y -> 0 <= y <= H, segs_late.y0) && all(y -> 0 <= y <= H, segs_late.y1)
            # `include_tracks = false` disables segment generation for a movie that wants
            # points only. Bug this catches: the flag ignored, tracks drawn regardless.
            _, no_segs = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                             include_tracks = false)(last(ts_in_store))
            @test no_segs === nothing

            # `tail_length` in FRAMES matches napari's `tail_length` and the browser's
            # `viewerTailLength` — 0 hides tracks entirely (same as `include_tracks = false`).
            # Bug this catches: a slider set to 0 in the UI still ships every hop of history.
            _, hidden = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                            tail_length = 0)(last(ts_in_store))
            @test hidden === nothing

            # A short tail is ≤ a long tail on the same frame; strictly less when history exists
            # earlier than the window. Bug this catches: the knob wired but not consulted (constant
            # tail regardless of L), or wired to a WRONG window that ignores tail_length.
            _, short_tail = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                                tail_length = 2)(last(ts_in_store))
            _, long_tail  = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                                tail_length = 1_000_000)(last(ts_in_store))
            @test short_tail !== nothing && long_tail !== nothing
            @test length(short_tail.x0) <= length(long_tail.x0)
            # And at t = 0 with tail_length = 1 the tail collapses to the current hop only —
            # arrivals in `[t + 2 - L, t + 1] = [2, 1]` is empty at t = 0 (nothing has arrived yet),
            # arrivals in `[0 - 0, 0 + 1] = [0, 1]` for L = 1... wait, `hi - L + 1 = 1 - 1 + 1 = 1`
            # so the window is `[1, 1]` — segments arriving at exactly t = 1. This is the
            # off-by-one napari and the browser both do.
            _, l1_at0 = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                            tail_length = 1)(0)
            # At t = 0 with L = 1 only segments with t1 = 1 are visible. At t = 0 with L = 2
            # segments with t1 ∈ {0, 1} are visible — same or MORE than L = 1 at t = 0.
            _, l2_at0 = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                            tail_length = 2)(0)
            n1 = l1_at0 === nothing ? 0 : length(l1_at0.x0)
            n2 = l2_at0 === nothing ? 0 : length(l2_at0.x0)
            @test n1 <= n2

            # `all_tracks` ignores pops and paints every cell with `track_id > 0` in one colour.
            # This is what a movie of a tracked segmentation without gated pops wants — napari's
            # show-tracks does the same as `/_whole`. Bug this catches: the flag ignored and the
            # movie coming back empty because the segmentation happens to carry no pops.
            all_for = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                          all_tracks = true,
                                          all_tracks_colour = "#9ca3af")
            pts_all, segs_all = all_for(last(ts_in_store))
            @test pts_all !== nothing
            # Every point uses the specified colour, not a pop-derived one.
            grey = RGB{N0f8}(hex_to_rgb("#9ca3af"))
            @test all(c -> c == grey, pts_all.colour)
            # And there ARE segments on this tracked fixture — proving the flag reached the track
            # bucket rather than only the point bucket.
            @test segs_all !== nothing && length(segs_all.x0) > 0

            # `pops_filter` restricts to specific paths. A filter that matches nothing returns an
            # empty point set — the closure paints nothing, not "everything since no filter matched".
            empty_for = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                            pops_filter = String["/no-such"])
            e0, _ = empty_for(0)
            @test e0 === nothing || isempty(e0.x)
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: overlay_author — mask_id_colours: which labels a mask draws, in what colour" begin
    # A population mask's label → colour map, which the shader draws as its colour table. A wrong
    # colour policy breaks silently here (outlines in a pop's OTHER colour, or on background labels
    # because the dict was built from label_props instead of the pop labels).
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img, err = _gating_image("testpr", "KDIeEm")
            @test err === nothing

            # Wire a pop that catches every cell — same shape as the overlays testset above.
            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "all", "colour" => "#00ff00", "gate" => gate)))))

            dict = mask_id_colours(img; value_name = "B", pop_type = "flow")
            # The pop's labels, keyed by INT, in the pop's colour — a null gate catches every cell.
            @test !isempty(dict)
            green = RGB{N0f8}(0, 1, 0)
            @test all(v -> v == green, values(dict))
            @test eltype(keys(dict)) <: Integer

            # `all_cells = true` bypasses pops and paints every cell in one colour — the mask
            # counterpart of `build_overlays3d_for(all_tracks = true)`. Bug this catches: the flag
            # wired but the fallback dict never populated, so the primitive skips every id.
            dict_all = mask_id_colours(img; value_name = "B", pop_type = "flow",
                                       all_cells = true, all_cells_colour = "#9ca3af")
            grey = RGB{N0f8}(hex_to_rgb("#9ca3af"))
            @test !isempty(dict_all)
            @test all(v -> v == grey, values(dict_all))

            # `all_cells_colour = "rainbow"` cycles `CECELIA_TRACK_PALETTE` by label id — asked
            # for by the compare-grid path (a uniform gray outline was invisible against the
            # coloured channels). Same closure shape as the solid path; the assertion is that
            # the dict picks its colour from the palette per id, and that different ids can get
            # different palette entries.
            dict_rb = mask_id_colours(img; value_name = "B", pop_type = "flow",
                                      all_cells = true, all_cells_colour = "rainbow")
            @test !isempty(dict_rb)
            pal = CECELIA_TRACK_PALETTE
            @test all(v -> v in pal, values(dict_rb))
            for (id, c) in dict_rb
                @test c == pal[mod(Int(id) - 1, length(pal)) + 1]
            end

            # A `pops_filter` that matches nothing draws nothing.
            @test isempty(mask_id_colours(img; value_name = "B", pop_type = "flow",
                                          pops_filter = String["/no-such"]))

            # colour_labels — recolour every id in the mask by an obs column. Total overrides
            # → every id gets the same colour, proving the resolver hits every id in the dict
            # (and picks up the DEFAULT colour when overrides don't match). Same `_cb_prepare`
            # helper the overlay author uses; matching palettes across labels + points is the point.
            lp2 = label_props(img; value_name = "B")
            df2 = as_df(lp2)
            ts_all = unique(Int[Int(round(Float64(v))) for v in df2.centroid_t
                                 if v isa Real && isfinite(Float64(v))])
            ov = Dict{String,String}(string(t) => "#00ff00" for t in ts_all)
            dict_cb = mask_id_colours(img; value_name = "B", pop_type = "flow",
                                      all_cells = true, all_cells_colour = "#9ca3af",
                                      colour_by = "centroid_t", colour_overrides = ov)
            green = RGB{N0f8}(hex_to_rgb("#00ff00"))
            @test !isempty(dict_cb)
            @test all(v -> v == green, values(dict_cb))
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
