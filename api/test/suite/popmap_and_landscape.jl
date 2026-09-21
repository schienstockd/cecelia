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
