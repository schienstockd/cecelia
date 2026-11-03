# ── Pipeline golden: a synthetic movie with KNOWN truth, run through the real pipeline ─────────────
#
# Hardening gap 1 (docs/todo/HARDENING_RESULT_CORRECTNESS.md): before this, no image ran
# import labels → measure → track → track measures → gate → cluster and had its RESULTS compared with
# an answer. Every guard was a unit input or a value pin, and every result bug in the ledger was found
# by eye on real data.
#
# The answer here is TRUTH BY CONSTRUCTION, not a snapshot of a previous run: `_GOLDEN_CELLS` below
# plants each cell's channel intensities, start position and straight-line velocity, and the
# calibration (µm/px, s/frame) is planted with them. A drift in any stage that changes a scientific
# result — intensity, count, linkage, units, gate membership, cluster separation — fails an assertion
# that says which. Segmentation itself (cellpose/coastal) is out of scope: too heavy and opt-in for CI,
# so the movie arrives with its labels, as `segment.importLabelsMeasure` takes them. Cellpose against
# known truth is `python/cecelia/tests/test_segmentation_golden.py`, run weekly and on segmentation or
# dependency changes (`.github/workflows/verify-segmentation-golden.yml`).
#
# Every stage goes through `run_task` (validation, pool, log file), not `_run_task`.

# Planted calibration. Deliberately not 1.0 anywhere, so a correct read is distinguishable from the
# "uncalibrated" fallback; X ≠ Y so an axis swap in a physical-size lookup changes the speeds.
const _GOLDEN_SX     = 0.5     # µm/px along X
const _GOLDEN_SY     = 0.4     # µm/px along Y
const _GOLDEN_DT_S   = 30.0    # s/frame → 0.5 min/frame
const _GOLDEN_T      = 10
const _GOLDEN_RADIUS = 5       # px
const _GOLDEN_BG     = 100     # background, both channels

# One row per planted cell: (population, CD4 level, CD8 level, start y, start x, vy, vx) in px and
# px/frame. 5 CD4-high vs 3 CD8-high — unequal on purpose, so a swapped channel order changes the gate
# count instead of mapping 4 onto 4. Two columns of four lanes, 18 px apart; the right column drifts
# down at the same rate everywhere so no two cells ever converge.
const _GOLDEN_CELLS = [
    (:cd4, 1000, 200, 10.0, 10.0, 0.0, 1.0),
    (:cd4, 1000, 200, 28.0, 10.0, 0.0, 1.4),
    (:cd4, 1000, 200, 46.0, 10.0, 0.0, 1.8),
    (:cd4, 1000, 200, 64.0, 10.0, 0.0, 2.2),
    (:cd4, 1000, 200, 10.0, 56.0, 0.6, 1.2),
    (:cd8, 200, 1000, 28.0, 56.0, 0.6, 1.6),
    (:cd8, 200, 1000, 46.0, 56.0, 0.6, 2.0),
    (:cd8, 200, 1000, 64.0, 56.0, 0.6, 0.8),
]

# planted centre of cell `i` at frame `t` (0-based), px
_golden_pos(i, t) = (c = _GOLDEN_CELLS[i]; (c[4] + c[6] * t, c[5] + c[7] * t))
# planted speed of cell `i` in µm/min — what `live.track.speed` must report
_golden_speed(i) = (c = _GOLDEN_CELLS[i];
                    hypot(c[6] * _GOLDEN_SY, c[7] * _GOLDEN_SX) / (_GOLDEN_DT_S / 60))

# The image + label store, written by the cecelia IO library (Julia may not touch zarr). Everything it
# draws comes from the params, so the truth above is the only copy.
const _GOLDEN_FIXTURE_PY = raw"""
import numpy as np, ome_types
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils

p = script_utils.script_params()
T, C, Z, Y, X = p['T'], 2, 1, p['Y'], p['X']
xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0" Name="golden">
<Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{T}" SizeC="{C}" SizeZ="{Z}"
 SizeY="{Y}" SizeX="{X}" PhysicalSizeX="{p['sx']}" PhysicalSizeY="{p['sy']}" PhysicalSizeZ="1.0"
 PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm"
 TimeIncrement="{p['dt']}" TimeIncrementUnit="s">
<Channel ID="Channel:0:0" Name="CD4" SamplesPerPixel="1"/><Channel ID="Channel:0:1" Name="CD8" SamplesPerPixel="1"/>
</Pixels></Image></OME>'''
omexml = ome_types.from_xml(xml)
du = DimUtils(omexml, use_channel_axis=True)
du.calc_image_dimensions([T, C, Z, Y, X])

rng = np.random.default_rng(0)
img = np.full((T, C, Z, Y, X), p['bg'], np.float64)
lab = np.zeros((T, Z, Y, X), np.int32)
yy, xx = np.mgrid[0:Y, 0:X]
for t in range(T):
    for i, c in enumerate(p['cells']):
        cy, cx = c['y'] + c['vy'] * t, c['x'] + c['vx'] * t
        disc = (yy - cy) ** 2 + (xx - cx) ** 2 <= p['r'] ** 2
        img[t, 0, 0][disc] = c['cd4']
        img[t, 1, 0][disc] = c['cd8']
        lab[t, 0][disc] = i + 1          # the same id in every frame; the import makes them unique
img += rng.normal(0, 10, img.shape)      # mild per-pixel noise: a mean over ~80 px moves ~1
img = np.clip(np.rint(img), 0, 65535).astype(np.uint16)

g, l0, ch = zarr_utils.open_multiscales_for_writing(p['imPath'], img.shape, np.uint16, du, nscales=1)
l0[:] = img
zarr_utils.write_multiscale_pyramid(g, l0, du, 1, list(ch))
ome_xml_utils.save_meta_in_zarr(p['imPath'], omexml=omexml)

gl, ll, _ = zarr_utils.open_multiscales_for_writing(p['labPath'], lab.shape, lab.dtype, None,
                                                     axes=['T', 'Z', 'Y', 'X'], kind='labels')
ll[:] = lab
"""

# Build the project under `dir` (an in-memory projects dir — never the dev one) and return the image,
# loaded the way the app loads it, with its calibration synced from the store into `meta` exactly as
# an import does (`resync_ome_meta!` → `read_ome_metadata`).
function _golden_project(dir::AbstractString)
    uid = "GoldPl"
    img_dir = joinpath(dir, "golden", "1", uid); mkpath(img_dir)
    data_dir = joinpath(dir, "golden", "0", uid); mkpath(data_dir)
    script = joinpath(dir, "make_golden.py"); write(script, _GOLDEN_FIXTURE_PY)
    cells = [Dict("cd4" => c[2], "cd8" => c[3], "y" => c[4], "x" => c[5], "vy" => c[6], "vx" => c[7])
             for c in _GOLDEN_CELLS]
    params = (imPath = joinpath(data_dir, "ccidImage.ome.zarr"),
              labPath = joinpath(dir, "external", "golden_labels.ome.zarr"),
              T = _GOLDEN_T, Y = 80, X = 100, r = _GOLDEN_RADIUS, bg = _GOLDEN_BG,
              sx = _GOLDEN_SX, sy = _GOLDEN_SY, dt = _GOLDEN_DT_S, cells = cells)
    mkpath(dirname(params.labPath))
    log = String[]
    Cecelia.run_py(script, params, joinpath(dir, "fixture_tasks"); on_log = l -> push!(log, l)) ||
        error("pipeline golden fixture failed:\n" * join(log, "\n"))
    img = CciaImage(; uid = uid, name = "golden", dir = img_dir)
    img.filepath["default"] = "ccidImage.ome.zarr"; img.filepath["_active"] = "default"
    save!(img)
    Cecelia.resync_ome_meta!(init_object("golden", uid)) || error("golden: no OME metadata synced")
    init_object("golden", uid), params.labPath
end

# The planted cell a measured row is: the one whose planted centre at that frame is nearest. Returns
# (cell index, distance in px) so a caller can also assert the match is tight.
function _golden_match(t::Real, y::Real, x::Real)
    d = [hypot(y - _golden_pos(i, t)[1], x - _golden_pos(i, t)[2]) for i in eachindex(_GOLDEN_CELLS)]
    i = argmin(d)
    i, d[i]
end

# Run one stage through the scheduler; a failure surfaces its log instead of a bare `nothing`.
function _golden_run(fun, target, params)
    log = String[]
    res = run_task(Cecelia._task_from_fun_name(fun), target, params; on_log = l -> push!(log, l))
    res === nothing && error("$fun failed:\n" * join(log, "\n"))
    res
end

@testset "pipeline golden — synthetic movie: measure → track → gate → cluster against planted truth" begin
    dir = mktempdir()
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        img, lab_path = _golden_project(dir)
        vn = "gold"
        n_cells, n_cd4 = length(_GOLDEN_CELLS), count(c -> c[1] === :cd4, _GOLDEN_CELLS)

        # fails if the import stops reading the calibration out of the store (speeds would be px/frame)
        @test img_physical_sizes(img) == ([1.0, _GOLDEN_SY, _GOLDEN_SX], _GOLDEN_DT_S / 60)

        # ── 1. import labels + measure ──────────────────────────────────────────────────────────
        _golden_run("segment.importLabelsMeasure", img, Dict{String,Any}(
            "labelsPath" => lab_path, "outputValueName" => vn))
        img = init_object("golden", img.uid)
        cells = label_props(img; value_name = vn) |>
                select_cols(["mean_intensity_0", "mean_intensity_1"]) |> view_centroid_cols |> as_df

        # fails if a cell or a frame is lost or duplicated between the label store and the cell table
        @test nrow(cells) == n_cells * _GOLDEN_T
        # fails if the time axis is dropped or mis-read (every frame must hold every cell)
        @test sort(unique(Int.(cells.centroid_t))) == collect(0:_GOLDEN_T - 1)
        @test all(==(n_cells), [count(==(t), cells.centroid_t) for t in 0:_GOLDEN_T - 1])
        # fails if labels stop being made unique across frames (two cells sharing a label)
        @test length(unique(cells.label)) == nrow(cells)

        truth = [_golden_match(r.centroid_t, r.centroid_y, r.centroid_x) for r in eachrow(cells)]
        cells.cell = first.(truth)
        # fails if centroids are shifted / axis-swapped (a row lands > 0.5 px from its planted centre)
        @test maximum(last.(truth)) < 0.5
        # fails if two rows of one frame claim the same planted cell (centroid mix-up)
        @test all(g -> length(unique(g.cell)) == n_cells,
                  [cells[cells.centroid_t .== t, :] for t in 0:_GOLDEN_T - 1])
        # fails if a channel's mean intensity drifts from the planted level (wrong channel, wrong
        # store version, wrong reduction, background leaking in)
        planted_cd4 = [_GOLDEN_CELLS[i][2] for i in cells.cell]
        planted_cd8 = [_GOLDEN_CELLS[i][3] for i in cells.cell]
        @test all(isapprox.(cells.mean_intensity_0, planted_cd4; rtol = 0.03))
        @test all(isapprox.(cells.mean_intensity_1, planted_cd8; rtol = 0.03))

        # ── 2. track + track measures ───────────────────────────────────────────────────────────
        # A search radius that admits the fastest planted step (1.1 µm) but not a neighbour lane (7.2 µm).
        _golden_run("tracking.bayesian_track_measures", img, Dict{String,Any}(
            "valueName" => vn, "maxSearchRadius" => 4, "minTimepoints" => 3, "dims" => "2D"))
        img = init_object("golden", img.uid)
        tracked = label_props(img; value_name = vn) |> select_cols(["track_id"]) |>
                  view_centroid_cols |> as_df
        tracked.cell = [first(_golden_match(r.centroid_t, r.centroid_y, r.centroid_x))
                        for r in eachrow(tracked)]
        # fails if any cell is left untracked
        @test all(isfinite, tracked.track_id)
        # fails if tracks are split or merged: one track per planted cell…
        @test length(unique(tracked.track_id)) == n_cells
        # …and every track holds exactly one planted cell, in all frames (no identity switch)
        by_track = Dict(tid => tracked[tracked.track_id .== tid, :] for tid in unique(tracked.track_id))
        @test all(g -> length(unique(g.cell)) == 1 && nrow(g) == _GOLDEN_T, values(by_track))
        cell_of_track = Dict(Int(tid) => only(unique(g.cell)) for (tid, g) in by_track)

        tracks = label_props(img_track_props_path(img, vn)) |>
                 select_cols(["live.track.speed", "live.track.duration", "live.track.displacement",
                              "live.track.straightness"]) |> as_df
        @test nrow(tracks) == n_cells
        planted_speed = [_golden_speed(cell_of_track[Int(l)]) for l in tracks.label]
        # fails if speed is not µm/min: a lost / doubled / seconds-vs-minutes frame interval, a pixel
        # size read from the wrong axis (X ≠ Y here), or speeds left in px/frame
        @test all(isapprox.(tracks[!, "live.track.speed"], planted_speed; rtol = 0.05))
        # fails if a track's duration is not the full movie in minutes (dropped frames, frame units)
        @test all(isapprox.(tracks[!, "live.track.duration"], (_GOLDEN_T - 1) * _GOLDEN_DT_S / 60))
        # fails if displacement is not in µm or the track is truncated
        @test all(isapprox.(tracks[!, "live.track.displacement"],
                            planted_speed .* ((_GOLDEN_T - 1) * _GOLDEN_DT_S / 60); rtol = 0.05))
        # fails if linkage zig-zags between lanes (a straight planted path must stay straight)
        @test all(>(0.97), tracks[!, "live.track.straightness"])

        # ── 3. gate: CD4-high, a rectangle on the two channel means ─────────────────────────────
        m = load_pop_map(img; value_name = vn, pop_type = "flow")
        add_pop!(m, "cd4hi"; gate = RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                  600.0, 1e9, -1e9, 1e9))
        save_pop_map!(m, img)
        gated = pop_df(img, "flow", ["/cd4hi"]; value_name = vn, centroids = :pixel)
        # fails if the gate evaluates on the wrong channel or the wrong cells: exactly the CD4-high
        # cells, in every frame
        @test nrow(gated) == n_cd4 * _GOLDEN_T
        @test all(r -> _GOLDEN_CELLS[first(_golden_match(r.centroid_t, r.centroid_y, r.centroid_x))][1] === :cd4,
                  eachrow(gated))

        # ── 4. cluster the whole segmentation on the two channels ───────────────────────────────
        _golden_run("clustPops.cluster", CciaImage[img], Dict{String,Any}(
            "valueNameSuffix" => "gold", "popsToCluster" => ["$vn/"],
            "clusterMeasures" => ["mean_intensity_0", "mean_intensity_1"], "resolution" => 0.5,
            "createUmap" => false))
        img = init_object("golden", img.uid)
        cl = label_props(img; value_name = vn) |> select_cols(["clusters.gold"]) |>
             view_centroid_cols |> as_df
        pops = [_GOLDEN_CELLS[first(_golden_match(r.centroid_t, r.centroid_y, r.centroid_x))][1]
                for r in eachrow(cl)]
        codes = cl[!, "clusters.gold"]
        # fails if a cell is left unclustered
        @test all(x -> !ismissing(x) && !(x isa AbstractFloat && isnan(x)), codes)
        # fails if clustering stops separating the two planted populations: no cluster mixes them…
        @test all(c -> length(unique(pops[codes .== c])) == 1, unique(codes))
        # …without that being satisfied trivially, by one cluster per cell (Leiden may split a
        # population — CD4 lands in two today — so this bounds the count, it doesn't pin it)
        @test 2 <= length(unique(codes)) <= n_cells
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        rm(dir; recursive = true, force = true)
    end
end
