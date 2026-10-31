# VN P3b popmap + BIDIR landscape testsets — extracted from api/test/runtests.jl.
#
# Five testsets:
#  - `API: /api/gating/popmap breadcrumb + labelsVersion pin (VN P3b)` — VN_VERSIONING_PLAN
#    P3b guardrail feeding the drift banner (authoredLabelsVersion, currentLabelsVersion, pin).
#  - `API: _bin_centroids_to_tiles (BIDIR landscape Phase 2a)` — tile assignment helper.
#  - `API: _bin_centroids_to_tiles Z filter (BIDIR landscape Phase 6)` — Z-awareness filter
#    that honours the viewer's plane/volume mode (plane sends z±1, volume sends the slab).
#  - `API: _pop_counts_from_label_map (BIDIR landscape Phase 2b)` — per-tile pop counts.
#  - `API: _track_summary_from_binned (BIDIR landscape Phase 3)` — per-tile track summary
#    from label_props (count, meanSpeed, meanDuration; NaN-safe).
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── P3b — /api/gating/popmap returns breadcrumb pair; labelsVersion pin threads ──
# docs/todo/VN_VERSIONING_PLAN.md → P3b. Guardrail so the drift banner is fed a reliable pair:
#   • authoredLabelsVersion       — the map's persisted breadcrumb (`nothing` on legacy/blank)
#   • currentLatestLabelsVersion  — the image's current `_latest` labels vN for this value_name
# The `labelsVersion` query param is only asserted at the routing level here (the endpoint accepts
# it and doesn't error); its effect on the underlying `label_props(img; version=…)` read is unit-
# tested where the reader lives (`app/test/suite/labelprops.jl`) — a positive-value assertion here
# needs a fixture with two labels versions on disk, which the smoke fixture doesn't carry.
@testset "API: /api/gating/popmap breadcrumb + labelsVersion pin (VN P3b)" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir(); cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        common = "projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"

        # Fresh fixture: `gating/B.json` may not exist yet → popmap returns an empty tree with no
        # breadcrumb (authored = nothing). The image *does* have label_props for B (implicit v1),
        # so currentLatestLabelsVersion resolves to "v1" (bare-scalar entries → LATEST_DEFAULT_VAL).
        st, body = api_gating_popmap(HTTP.Request("GET", "/api/gating/popmap?" * common))
        @test st == 200
        d = JSON3.read(body, Dict{String,Any})
        @test isnothing(get(d, "authoredLabelsVersion", nothing))
        @test get(d, "currentLatestLabelsVersion", nothing) == "v1"

        # A save (via any gating mutation → save_pop_map!(m, img)) stamps the breadcrumb. Add a pop
        # so the save path fires — then re-read: authored = "v1", drift = false (matches current).
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => "B", "popType" => "flow")
        gate = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                "x_min" => 0.0, "x_max" => 1.0, "y_min" => 0.0, "y_max" => 1.0)
        api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
            Dict{String,Any}("name" => "qc", "gate" => gate)))))

        st, body = api_gating_popmap(HTTP.Request("GET", "/api/gating/popmap?" * common))
        d = JSON3.read(body, Dict{String,Any})
        @test d["authoredLabelsVersion"] == "v1"
        @test d["currentLatestLabelsVersion"] == "v1"

        # Route-level: the endpoint accepts the pin without erroring. A vN that doesn't exist on the
        # image still returns 200 — the pin is a read directive, not a validation gate (a missing
        # inner version resolves via the composer to `nothing`, the caller handles empty data). We
        # assert 200 and shape here; the reader-level pin behaviour is pinned in the pkg suite.
        st, body = api_gating_popmap(HTTP.Request("GET",
            "/api/gating/popmap?" * common * "&labelsVersion=v1"))
        @test st == 200
        st, _ = api_gating_membership(HTTP.Request("GET",
            "/api/gating/membership?" * common * "&pops=/qc&labelsVersion=v1"))
        @test st == 200
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: _bin_centroids_to_tiles (BIDIR landscape Phase 2a)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2a — the pure binning helper behind segCount.
    # Bins level-0 centroids into a ncols×nrows grid over the full frame; row-major flat output;
    # sparse-friendly (NaN centroids drop; off-frame timepoints drop; not throw).
    # 100×100 frame, 4×4 grid ⇒ each tile is 25 px wide. tileId row-major so (r=0,c=0)="A1".
    xs = Float64[10.0, 12.0, 60.0,  99.0, 0.0, NaN,  50.0]
    ys = Float64[10.0, 20.0, 60.0,  99.0, 0.0, 50.0, NaN]
    ts = Int[    0,    0,    0,     0,    0,   0,    0]
    counts = _bin_centroids_to_tiles(xs, ys, ts, 0, 4, 4, 100, 100)
    @test length(counts) == 16
    @test counts[1] == 3         # (10,10), (12,20), (0,0) all land in top-left tile
    @test counts[11] == 1        # (60,60) → row 2 col 2 → idx 11
    @test counts[16] == 1        # (99,99) → row 3 col 3 → idx 16 (clamp keeps the edge tile in bounds)
    @test sum(counts) == 5       # 7 rows in, 2 NaN-drops

    # Temporal filter: only rows with centroid_t == t are counted
    xs_t = Float64[10.0, 10.0, 10.0]; ys_t = Float64[10.0, 10.0, 10.0]; ts_t = Int[0, 1, 2]
    @test _bin_centroids_to_tiles(xs_t, ys_t, ts_t, 1, 2, 2, 100, 100)[1] == 1
    @test sum(_bin_centroids_to_tiles(xs_t, ys_t, ts_t, 5, 2, 2, 100, 100)) == 0   # no matching t

    # `ts === nothing` (still image, no centroid_t column) counts every centroid against any t
    counts_still = _bin_centroids_to_tiles(Float64[10.0, 50.0], Float64[10.0, 50.0],
                                            nothing, 0, 2, 2, 100, 100)
    @test sum(counts_still) == 2

    # Length mismatch throws — a caller-side bug we want loud, not silent
    @test_throws ArgumentError _bin_centroids_to_tiles(Float64[1.0], Float64[1.0, 2.0],
                                                       nothing, 0, 2, 2, 100, 100)
end

@testset "API: _bin_centroids_to_tiles Z filter (BIDIR landscape Phase 6)" begin
    # Phase 6 — the viewer's plane/volume mode now decides which centroids count. In plane
    # mode the frontend sends z±1 (matches the gating page's pick-rect z-scope); in volume mode
    # it sends the slab-slider range. Backend filters `z_lo ≤ round(centroid_z) ≤ z_hi`.
    # 2×2 grid over 100×100 — everything lands in tile 1.
    xs = Float64[10.0, 10.0, 10.0, 10.0, 10.0]
    ys = Float64[10.0, 10.0, 10.0, 10.0, 10.0]
    zs = Float64[3.0,  4.6,  5.0,  5.4,  10.0]     # rounds to 3, 5, 5, 5, 10
    # No Z filter (zs = nothing OR bounds absent): all 5 rows count. Two ways to spell it —
    # both should behave identically so a caller can pass zs eagerly + bounds lazily.
    @test _bin_centroids_to_tiles(xs, ys, nothing, 0, 2, 2, 100, 100)[1] == 5
    @test _bin_centroids_to_tiles(xs, ys, nothing, 0, 2, 2, 100, 100; zs = zs)[1] == 5
    # Plane mode: z_lo=4, z_hi=6 (viewer at slice 5, ±1). Rows z=4.6/5.0/5.4 count; z=3 and
    # z=10 drop. Three cells in tile 1.
    counts_plane = _bin_centroids_to_tiles(xs, ys, nothing, 0, 2, 2, 100, 100;
                                            zs = zs, z_lo = 4, z_hi = 6)
    @test counts_plane[1] == 3
    # Volume mode with a wider slab covers everything except z=10 (out of [0,7]).
    counts_vol = _bin_centroids_to_tiles(xs, ys, nothing, 0, 2, 2, 100, 100;
                                          zs = zs, z_lo = 0, z_hi = 7)
    @test counts_vol[1] == 4
    # Slab that skips the visible plane entirely → zero cells in tile 1.
    counts_none = _bin_centroids_to_tiles(xs, ys, nothing, 0, 2, 2, 100, 100;
                                           zs = zs, z_lo = 20, z_hi = 30)
    @test sum(counts_none) == 0
    # NaN centroid_z drops (same rule as x/y/t) — the row is skipped rather than counted as z=0.
    zs_nan = Float64[3.0, NaN, 5.0]
    xs_nan = Float64[10.0, 10.0, 10.0]; ys_nan = Float64[10.0, 10.0, 10.0]
    counts_nan = _bin_centroids_to_tiles(xs_nan, ys_nan, nothing, 0, 2, 2, 100, 100;
                                          zs = zs_nan, z_lo = 0, z_hi = 5)
    @test counts_nan[1] == 2

    # Length mismatch throws — same discipline as ts
    @test_throws ArgumentError _bin_centroids_to_tiles(Float64[1.0, 2.0], Float64[1.0, 2.0],
                                                       nothing, 0, 2, 2, 100, 100;
                                                       zs = Float64[1.0])
end

@testset "API: landscape tiles cut the viewer's view, not the whole image" begin
    # 1000×1000 image, the viewer zoomed onto the 200×200 square at (400..600, 400..600), 4×4 tiles —
    # each tile is 50×50 image px. A cell at (425, 475) is tile row 1, col 0 → "A2" (index 5); a cell
    # at (100, 100) is off screen and must NOT be clamped into an edge tile.
    view = (400.0, 400.0, 600.0, 600.0)
    @test _tile_of(425, 475, 4, 4, 1000, 1000, view) == 1 * 4 + 0 + 1
    @test _tile_of(599.9, 599.9, 4, 4, 1000, 1000, view) == 16
    @test _tile_of(100, 100, 4, 4, 1000, 1000, view) == 0
    @test _tile_of(600, 500, 4, 4, 1000, 1000, view) == 0          # right edge is exclusive
    # Whole image (no view) keeps the old clamped behaviour.
    @test _tile_of(1000, 1000, 4, 4, 1000, 1000, nothing) == 16
    counts = _bin_centroids_to_tiles(Float64[425, 100, 590], Float64[475, 100, 410], nothing, 0,
                                     4, 4, 1000, 1000; view = view)
    @test counts[5] == 1 && counts[4] == 1 && sum(counts) == 2

    # Zoomed out: the view hangs past the image. Tiles over empty canvas get no cells.
    wide = (-500.0, 0.0, 1500.0, 1000.0)          # 4 cols of 500 px; image is cols 1–2
    @test _tile_of(10, 10, 4, 1, 1000, 1000, wide) == 2
    @test _tile_of(990, 10, 4, 1, 1000, 1000, wide) == 3
end

@testset "API: _tile_channel_stats over a view crop" begin
    # 8×8 plane, left half 0, right half 1. Whole-plane 2×1 tiles: means 0 and 1.
    plane = zeros(Float32, 8, 8); plane[5:8, :] .= 1
    st = _tile_channel_stats(plane, 2, 1; range = (0, 1))
    @test st[1][1] ≈ 0.0 && st[2][1] ≈ 1.0
    # The view spans 16 px starting 8 px left of the plane: tile 1 is wholly off the plane (nothing),
    # tile 2 covers the whole plane (mean 0.5).
    st2 = _tile_channel_stats(plane, 2, 1; range = (0, 1), frame = (-8.0, 0.0, 8.0, 8.0))
    @test st2[1] === nothing
    @test st2[2][1] ≈ 0.5
    # A tile thinner than a pixel reads the pixel under its centre.
    st3 = _tile_channel_stats(plane, 32, 1; range = (0, 1), frame = (4.0, 0.0, 5.0, 8.0))
    @test all(x -> x !== nothing && x[1] ≈ 1.0, st3)
end

@testset "API: _tile_channel_stats normalises by the fixed range, not the plane's extrema" begin
    # Left tile 100..300 ramp, right tile flat 200. Range (100, 500) → left mean 0.25.
    base = zeros(Float32, 8, 8)
    for x in 1:4, y in 1:8; base[x, y] = 100 + 200 * (y - 1) / 7; end
    base[5:8, :] .= 200
    rng = (100.0, 500.0)
    a = _tile_channel_stats(base, 2, 1; range = rng)
    @test a[1][1] ≈ 0.25 atol = 1e-6
    # A bright object entering the RIGHT tile does not move the LEFT tile's mean or SNR.
    bright = copy(base); bright[7, 4] = 60000
    b = _tile_channel_stats(bright, 2, 1; range = rng)
    @test b[1] == a[1]
    @test b[2][1] > a[2][1]
    # Same left-tile pixels in a different frame (rest of the frame differs) → identical stats.
    other = copy(base); other[5:8, :] .= 0
    @test _tile_channel_stats(other, 2, 1; range = rng)[1] == a[1]
    # Values outside the range clamp to [0, 1].
    sat = fill(Float32(1000), 8, 8); sat[1:4, :] .= 0
    c = _tile_channel_stats(sat, 2, 1; range = rng)
    @test c[1][1] == 0.0 && c[2][1] == 1.0
end

# Handler-level: the compute resolves one fixed range per channel and records it, so two frames of the
# same image report the same mean for an unchanged tile even when the other tile lights up.
@testset "API: landscape compute — channel means use the image's fixed range (sourceRun.channels.range)" begin
    axes_ms(names) = Dict("multiscales" => [Dict("axes" => [Dict("name" => n) for n in names],
                                                 "datasets" => [Dict("path" => "0")])])
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = "LNDTST"; img = "IMGLND"
        img_dir = joinpath(tmp, proj, "1", img); mkpath(img_dir)
        store = joinpath(tmp, proj, "0", img, "ccidImage.ome.zarr"); mkpath(dirname(store))
        g = zgroup(Zarr.DirectoryStore(store); attrs = axes_ms(["t", "c", "z", "y", "x"]))
        nx, ny = 16, 16
        a = zcreate(UInt16, g, "0", nx, ny, 1, 1, 2; chunks = (nx, ny, 1, 1, 1))
        # t=0: textured background. t=1: identical, plus a bright object in the right half only.
        vol = UInt16[100 + x + y for x in 1:nx, y in 1:ny, z in 1:1, c in 1:1, t in 1:2]
        vol[13:14, 5:6, 1, 1, 2] .= 60000
        a[:, :, :, :, :] = vol
        write(joinpath(img_dir, "ccid.json"),
              JSON3.write(Dict("filepath" => Dict("default" => "ccidImage.ome.zarr", "_active" => "default"))))
        compute(t) = begin
            st, body = api_viewer_landscape_compute(Vector{UInt8}(JSON3.write(Dict(
                "projectUid" => proj, "imageUid" => img, "t" => t, "cols" => 2, "rows" => 2,
                "channels" => [Dict("index" => 0, "name" => "c0")]))))
            @test st == 200
            JSON3.read(body, Dict{String,Any})
        end
        mean_of(r, id) = only(x for x in r["tiles"] if x["tileId"] == id)["channels"]["c0"]["mean"]
        r0 = compute(0); r1 = compute(1)
        @test mean_of(r0, "A1") == mean_of(r1, "A1")          # left tiles unchanged …
        @test mean_of(r0, "A2") == mean_of(r1, "A2")
        @test mean_of(r1, "B1") > mean_of(r0, "B1")           # … only the lit tile moves
        ch0 = r0["sourceRun"]["channels"]
        @test ch0["rangeSource"] == "sampled"
        @test ch0["range"]["c0"] == r1["sourceRun"]["channels"]["range"]["c0"]
        # The viewer's saved contrast wins, and is what the record names.
        zp, td, _ = resolve_image_version(proj, img, nothing; version = nothing)
        props = _props_path(td, zp); mkpath(dirname(props))
        write(props, JSON3.write(Dict("webgpu" => Dict("channels" => [Dict("lo" => 100, "hi" => 140, "hex" => "#ffffff")]))))
        r2 = compute(0)
        @test r2["sourceRun"]["channels"]["rangeSource"] == "viewer"
        @test r2["sourceRun"]["channels"]["range"]["c0"] == [100.0, 140.0]
        # Mean of A1 (x, y in 1:8 → 102..116) under (100, 140): (109 - 100) / 40.
        @test mean_of(r2, "A1") ≈ 0.225 atol = 1e-3
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: _pick_view_level + _landscape_view" begin
    lv = [(4096, 4096), (2048, 2048), (1024, 1024), (512, 512)]
    # Whole image visible → the 512 level; a quarter visible → the 2048 level (its crop is 512 px).
    @test _pick_view_level(lv, 4096, 4096, (0.0, 0.0, 4096.0, 4096.0)) == 3
    @test _pick_view_level(lv, 4096, 4096, (0.0, 0.0, 1024.0, 1024.0)) == 1
    # Deeper than any level can serve at 512 → the finest.
    @test _pick_view_level(lv, 4096, 4096, (0.0, 0.0, 64.0, 64.0)) == 0
    # Store order does not matter.
    @test _pick_view_level(reverse(lv), 4096, 4096, (0.0, 0.0, 1024.0, 1024.0)) == 2
    @test _landscape_view(Dict("x0" => 1, "y0" => 2, "x1" => 3, "y1" => 4)) == (1.0, 2.0, 3.0, 4.0)
    @test _landscape_view(Dict("x0" => 3, "y0" => 2, "x1" => 1, "y1" => 4)) === nothing
    @test _landscape_view(Dict("x0" => 1, "y0" => 2, "x1" => NaN, "y1" => 4)) === nothing
    @test _landscape_view(nothing) === nothing
end

@testset "API: on-demand landscape request — the viewer's answer reaches the waiter" begin
    l = Landscape("p", "img", "default", 3, 0, time(), Dict{String,Any}("grid" => Dict("cols" => 8)))
    rid = _open_landscape_request!()
    t = @async _await_landscape_request(rid, 5.0; poll_s = 0.01)
    sleep(0.05)
    @test _deliver_landscape_request!(rid, l)
    @test !_deliver_landscape_request!(rid, l)           # first answer wins
    got = fetch(t)
    @test got !== nothing && got.t == 3
    @test !haskey(_LANDSCAPE_REQUESTS, rid)              # forgotten once answered
    # No viewer answers → nothing, and a late answer is dropped rather than parked forever.
    rid2 = _open_landscape_request!()
    @test _await_landscape_request(rid2, 0.05; poll_s = 0.01) === nothing
    @test !_deliver_landscape_request!(rid2, l)
    @test !haskey(_LANDSCAPE_REQUESTS, rid2)
    # The reply carries the address next to the payload.
    r = _landscape_reply(l)
    @test r["t"] == 3 && r["imageUid"] == "img" && r["grid"]["cols"] == 8
end

@testset "API: _pop_counts_from_label_map (BIDIR landscape Phase 2b)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2b — the pure per-tile pop aggregator.
    # Sparsity discipline (Decision 3): hidden pops drop; pops with no member cells in the
    # visible frame drop; zero-count tiles have no entry in their inner vector.
    # 4-tile grid, label→tile map: label 1→tile 1, 2→tile 1, 3→tile 2, 4→tile 4.
    label_map = Dict(1 => 1, 2 => 1, 3 => 2, 4 => 4)
    pops = [
        (path = "/live/tnaive", name = "T naive", show = true,  labels = [1, 2, 3]),   # 2 in T1, 1 in T2
        (path = "/live/tmem",   name = "T mem",   show = true,  labels = [3, 4, 99]),  # 1 in T2, 1 in T4, 99 absent
        (path = "/live/hidden", name = "Hidden",  show = false, labels = [1, 2, 3, 4]),# entirely skipped
        (path = "/live/empty",  name = "Empty",   show = true,  labels = Int[]),       # no labels, no entries
        (path = "/live/offmap", name = "Off",     show = true,  labels = [77, 88]),    # no labels land in any tile
    ]
    per_tile = _pop_counts_from_label_map(pops, label_map, 4)
    @test length(per_tile) == 4
    # tile 1: T naive count 2, no T mem
    @test length(per_tile[1]) == 1
    @test per_tile[1][1].path == "/live/tnaive" && per_tile[1][1].count == 2
    # tile 2: T naive count 1, T mem count 1
    paths_t2 = sort([p.path for p in per_tile[2]])
    counts_t2 = Dict(p.path => p.count for p in per_tile[2])
    @test paths_t2 == ["/live/tmem", "/live/tnaive"]
    @test counts_t2["/live/tnaive"] == 1 && counts_t2["/live/tmem"] == 1
    # tile 3: no visible pops occupy it
    @test isempty(per_tile[3])
    # tile 4: only T mem
    @test length(per_tile[4]) == 1
    @test per_tile[4][1].path == "/live/tmem" && per_tile[4][1].count == 1
    # Hidden pop never appears anywhere (Decision 3 sparsity)
    @test all(all(p.path != "/live/hidden" for p in bag) for bag in per_tile)
    # Empty / off-map pops likewise
    @test all(all(p.path != "/live/empty" && p.path != "/live/offmap" for p in bag) for bag in per_tile)
end

@testset "API: _track_summary_from_binned (BIDIR landscape Phase 3)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 3 — the pure per-tile tracks aggregator.
    # 4-tile grid: tile 1 has tracks {10, 11} with 2 cells contributing speeds; tile 2 has
    # only track 12; tile 3 is empty; tile 4 has track 10 again (a long track passing through).
    # duration_by_track (whole-lifetime frame counts): 10 → 100, 11 → 25, 12 → 8.
    dur = Dict(10 => 100, 11 => 25, 12 => 8)
    tile_ids = [Set([10, 11]), Set([12]), Set{Int}(), Set([10])]
    speed_sum = Float64[3.0, 1.5, 0.0, 4.0]     # tile 1 sum 3.0 across 2 cells → mean 1.5
    speed_n   = Int[2, 1, 0, 1]
    out = _track_summary_from_binned(tile_ids, speed_sum, speed_n, dur)
    @test length(out) == 4
    @test out[1] !== nothing
    @test out[1].count == 2
    @test out[1].meanDuration == (100 + 25) / 2
    @test out[1].meanSpeed ≈ 1.5
    @test out[2].count == 1
    @test out[2].meanDuration == 8
    @test out[2].meanSpeed ≈ 1.5
    # Empty tile → nothing (sparsity — caller emits no `tracks` key)
    @test out[3] === nothing
    @test out[4].count == 1
    @test out[4].meanDuration == 100
    @test out[4].meanSpeed ≈ 4.0

    # No speed observations at all (tracked segmentation without `live.cell.speed`) →
    # meanSpeed NaN; caller emits `count`/`meanDuration` and omits `meanSpeed`.
    out2 = _track_summary_from_binned([Set([10])], Float64[0.0], Int[0], dur)
    @test out2[1].count == 1
    @test isnan(out2[1].meanSpeed)
    @test out2[1].meanDuration == 100

    # A track with no known duration (stale h5ad) gets skipped in the meanDuration numerator
    # but still counts toward `count` — a defensive drop, not a lie about the tile.
    out3 = _track_summary_from_binned([Set([99, 10])], Float64[0.0], Int[0],
                                       Dict(10 => 50))    # 99 absent
    @test out3[1].count == 2                              # both tracks visible in tile
    @test out3[1].meanDuration == 50                      # only 10 contributes a real duration

    # Length mismatch throws
    @test_throws ArgumentError _track_summary_from_binned([Set([1])], Float64[0.0, 0.0],
                                                          Int[1], Dict{Int,Int}())
end

@testset "API: landscape pop layers — every segmentation × pop type the viewer draws" begin
    b(s) = JSON3.read(s)
    L = _landscape_pop_layers(b("""{"popLayers": [
            {"valueName": "OTI", "popType": "flow", "paths": ["/qc"]},
            {"valueName": "P14", "popType": "clust"},
            {"valueName": "", "popType": "flow"}],
        "popValueName": "ignored", "popType": "flow"}"""))
    @test [(l.valueName, l.popType) for l in L] == [("OTI", "flow"), ("P14", "clust")]
    @test L[1].paths == ["/qc"] && L[2].paths === nothing   # no paths = the manager's `.show`
    # an older caller: the pop manager's one (vn, popType)
    @test [(l.valueName, l.popType) for l in _landscape_pop_layers(b("""{"popValueName": "OTI", "popType": "flow"}"""))] ==
          [("OTI", "flow")]
    @test isempty(_landscape_pop_layers(b("""{"popValueName": "OTI"}""")))
end
