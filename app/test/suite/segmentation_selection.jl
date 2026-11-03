# ── Which segmentation / version did the result come from? ────────────────────────────────────────
# Guards for the "wrong segmentation or version selected" class (docs/todo/HARDENING_RESULT_CORRECTNESS.md
# §6 gap 5). Each testset reads a RESULT and checks it against the right input, on inputs that
# DIFFER — two segmentations that disagree (`distinct_segmentation!`, app/test/distinct_segmentations.jl,
# or two imported label sets), two image versions of different length — so a mix-up changes a number.
# The python-backed ones reuse `_label_import_project` from label_import.jl (included earlier).

# The KDIeEm image with B and a DIFFERENT T (T's Directed tracks relabelled Scanning), both clustered in
# the `movement` run, B's trackclust pop map shared to T.
function _kdieem_distinct_pair()
    src = fixture_path("testpr", "1", "KDIeEm")
    files = (joinpath("labelProps", "B.h5ad"), joinpath("labelProps", "B__tracks.h5ad"),
             joinpath("labelProps", "B__tracks.clustfeatures.json"), joinpath("gating", "B__trackclust.json"))
    all(f -> have_fixture(joinpath(src, f)), files) || return nothing
    td = mktempdir()
    for f in files
        mkpath(dirname(joinpath(td, f))); cp(joinpath(src, f), joinpath(td, f))
    end
    counts = distinct_segmentation!(joinpath(td, "labelProps"))
    img = CciaImage(uid = "KDIeEm", dir = td)
    img.label_props["B"] = "B.h5ad"; img.label_props["T"] = "T.h5ad"; img.label_props["_active"] = "B"
    (; img, counts)
end

# Truth read straight from one segmentation's tables: its tracks in cluster `codes`, and their cells.
function _cluster_truth(img, vn, codes)
    tt = label_props(img_track_props_path(img, vn)) |> as_df
    tracks = tt[[Int(c) in codes for c in tt[!, "clusters.movement"]], :]
    tids = Set(Int.(tracks.label))
    cells = label_props(img; value_name = vn) |> select_cols(["track_id"]) |> as_df
    (; tracks, n_cells = count(t -> t isa Real && isfinite(t) && Int(t) in tids, cells.track_id))
end

const _MOVEMENT_CODES = Dict("/Scanning" => [0], "/Directed" => [1], "/Meandering" => [2])

@testset "cell-card stats footer reads only the medoid's segmentation (distinct B/T)" begin
    pair = _kdieem_distinct_pair()
    if pair === nothing
        @test_skip "cell-cards fixture missing (see test-data/README.md)"
    else
        img = pair.img
        @test pair.counts.from[0] == 21 && pair.counts.to[0] == 41   # the two tables really differ
        pops = [(path = p, cluster_ids = _MOVEMENT_CODES[p]) for p in ("/Scanning", "/Directed", "/Meandering")]
        pool, cards = cell_cards_metadata(img, "B", "movement", pops)
        @test Set(x.value_name for x in pool) == Set(["B", "T"])
        @test length(cards) == 3
        # the footer is "this pop on the medoid's segmentation": the 5-number summary of exactly that
        # segmentation's tracks in the pop. A footer that pools the co-clustered T into B (or B into T)
        # has extra tracks — on an identical copy those are duplicates and every quantile survives, so
        # only a T that differs can show it (Scanning: B 21 tracks, T 41).
        # fails if: `cell_cards_metadata`'s footer `pop_df` expands the cluster pop across segmentations
        for c in cards
            truth = _cluster_truth(img, c.medoid.value_name, _MOVEMENT_CODES[c.path])
            @test c.stats == Cecelia.card_stats(truth.tracks)
        end
    end
end

@testset "AI measure summaries stay on their segmentation (distinct B/T)" begin
    pair = _kdieem_distinct_pair()
    if pair === nothing
        @test_skip "cell-cards fixture missing (see test-data/README.md)"
    else
        img = pair.img
        proj = CciaProject(; uid = "mP", name = "m"); proj.root = mktempdir()
        st = CciaSet(; uid = "mS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
        push!(st._images, img); push!(st.image_uids, img.uid)
        # a track cluster is profiled as motility (`is_track_pop`, ai/measures.jl), one row per track
        summ = measure_summary(proj; kind = "motility").images[1].summaries
        got = Dict((s.valueName, s.population) => s.n for s in summ)
        # each summary is labelled with ONE segmentation, so its n is that segmentation's tracks in the pop
        want = Dict((vn, p) => nrow(_cluster_truth(img, vn, codes).tracks)
                    for vn in ("B", "T") for (p, codes) in _MOVEMENT_CODES)
        filter!(kv -> kv.second > 0, want)            # T has no Directed tracks → no summary
        @test got[("B", "/Scanning")] != got[("T", "/Scanning")]
        # fails if: `_target_summary`'s `pop_df` expands the cluster pop across segmentations
        @test got == want
    end
end

# The correction composite re-measures the segmentation it corrected — two imported label sets that
# differ (B: 10×10 squares, ids 1–12; T: 6×6 nuclei, ids 35–80), a merge on T, and the cell tables
# read back. The cockpit submits `{valueName, labelOps}` only (`frontend/src/utils/labelOpsRun.ts`), so
# `measureLabels` learns the segmentation from what `segment.correct` hands down the composite.
@testset "correct + re-measure composite re-measures the CORRECTED segmentation" begin
    mktempdir() do dir
        img, paths = _label_import_project(dir)
        imp = Cecelia._task_from_fun_name("segment.importLabelsMeasure")
        for (vn, src) in (("B", paths.basePath), ("T", paths.nucPath))
            @test Cecelia._run_task(imp, img, Dict{String,Any}("labelsPath" => src, "outputValueName" => vn);
                                    on_log = _ -> nothing) !== nothing
        end
        tbl(vn) = label_props(joinpath(img._dir, "labelProps", "$vn.h5ad")) |> select_cols(["area"]) |> as_df
        b_before = read(joinpath(img._dir, "labelProps", "B.h5ad"))
        @test Set(tbl("T").label) == Set([35:40; 75:80])

        logs = String[]
        res = Cecelia._run_task(Cecelia._task_from_fun_name("segment.correct_measures"), img,
            Dict{String,Any}("valueName" => "T", "labelOps" => JSON3.write(
                [Dict("op" => "label.merge", "t" => 0, "ids" => [35, 36], "into" => 35)]));
            on_log = l -> push!(logs, l))
        # fails if: the composite does not forward `outputValueName` from `segment.correct` (or correct
        # stops returning it) — measureLabels then falls back to `default`, which has no labels
        @test res !== nothing || error(join(logs, "\n"))
        t = tbl("T")
        @test Set(t.label) == Set([35; 37:40; 75:80])            # T re-measured: 36 merged away
        @test only(t.area[t.label .== 35]) == 72                 # 2 × 36 px
        @test read(joinpath(img._dir, "labelProps", "B.h5ad")) == b_before   # B untouched
        @test !haskey(read_ccid_raw(state_file(img))["label_props"], "default")
    end
end

# Crop cuts the version it is asked for. The picker preselects the ACTIVE version for an image-version
# field (`preferredValueName`, frontend/src/tasks/paramValues.test.ts); this pins the other half — the
# task reads that version's store, not `default` — on two versions of different length.
const _CROP_VERSIONS_PY = raw"""
import numpy as np, ome_types
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils

p = script_utils.script_params()
for path, T in ((p['rawPath'], 2), (p['driftPath'], 3)):   # the two versions differ in length
    C, Z, Y, X = 1, 1, 16, 20
    xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0" Name="t">
<Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{T}" SizeC="{C}" SizeZ="{Z}"
 SizeY="{Y}" SizeX="{X}" PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
 PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm" TimeIncrement="30" TimeIncrementUnit="s">
<Channel ID="Channel:0:0" Name="A" SamplesPerPixel="1"/></Pixels></Image></OME>'''
    omexml = ome_types.from_xml(xml)
    du = DimUtils(omexml, use_channel_axis=True)
    du.calc_image_dimensions([T, C, Z, Y, X])
    g, l0, ch = zarr_utils.open_multiscales_for_writing(path, (T, C, Z, Y, X), np.uint16, du, nscales=1)
    l0[:] = T
    zarr_utils.write_multiscale_pyramid(g, l0, du, 1, list(ch))
    ome_xml_utils.save_meta_in_zarr(path, omexml=omexml)
"""

@testset "crop cuts the requested image version, not default" begin
    # the source param is an image-version picker, so the form preselects the active version
    vparam = only(x for x in Cecelia._task_spec(Cecelia.CropImage())["params"] if x["key"] == "valueName")
    @test vparam["type"] == "valueNameSelection" && vparam["field"] == "filepaths"

    proj = create_project!(name = "crop-version-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "src")
    data_dir = joinpath(proj.root, "0", img.uid); mkpath(data_dir)
    script = joinpath(mktempdir(), "crop_versions.py"); write(script, _CROP_VERSIONS_PY)
    log = String[]
    Cecelia.run_py(script, (rawPath = joinpath(data_dir, "ccidImage.ome.zarr"),
                            driftPath = joinpath(data_dir, "drift.ome.zarr")),
                   joinpath(mktempdir(), "fixture_tasks"); on_log = l -> push!(log, l)) ||
        error("crop fixture failed:\n" * join(log, "\n"))
    commit_state!(img) do raw
        raw["filepath"] = Dict{String,Any}("default" => "ccidImage.ome.zarr",
                                           "driftCorrected" => "drift.ome.zarr", "_active" => "driftCorrected")
        raw["status"] = "done"
    end
    box = Dict{String,Any}("x0" => 2, "x1" => 10, "y0" => 3, "y1" => 9, "z0" => -1, "z1" => -1, "t0" => -1, "t1" => -1)
    for (vn, size_t) in (("driftCorrected", 3), ("default", 2))
        r = Cecelia._run_task(Cecelia.CropImage(), img, Dict{String,Any}("valueName" => vn, "cropBox" => box);
                              on_log = _ -> nothing)
        @test r !== nothing
        meta = read_ccid_raw(state_file(proj.root, r["newImageUid"]))["meta"]
        # fails if: the crop resolves `default`'s store whatever version it was handed — the written
        # crop then has default's 2 frames (SizeT is re-read from the written store)
        @test meta["SizeT"] == size_t
        @test meta["crop_source_value_name"] == vn
    end
end
