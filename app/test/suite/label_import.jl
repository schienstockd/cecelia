# ── Label import (`segment.importLabels` / `segment.importLabelsMeasure`) ─────────────────────────
#
# The streaming rules themselves (shape check, uint32, time-unique ids, id pairing, pyramid depth) are
# pinned in Python: python/cecelia/tests/test_label_import.py. What is pinned here is what Julia owns,
# and the one path only the app can run: a temp project → import (TIFF + zarr) → registered labels →
# Measure labels → `label_props`, through the composite a user actually picks.

# The image + label sources, written by the cecelia IO library (the Julia side may not touch zarr).
const _LABEL_IMPORT_FIXTURE_PY = raw"""
import numpy as np, ome_types, tifffile
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils

p = script_utils.script_params()
T, C, Z, Y, X = 2, 2, 1, 32, 40
xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0" Name="t">
<Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{T}" SizeC="{C}" SizeZ="{Z}"
 SizeY="{Y}" SizeX="{X}" PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
 PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm" TimeIncrement="30" TimeIncrementUnit="s">
<Channel ID="Channel:0:0" Name="A" SamplesPerPixel="1"/><Channel ID="Channel:0:1" Name="B" SamplesPerPixel="1"/>
</Pixels></Image></OME>'''
omexml = ome_types.from_xml(xml)
du = DimUtils(omexml, use_channel_axis=True)
du.calc_image_dimensions([T, C, Z, Y, X])
g, l0, ch = zarr_utils.open_multiscales_for_writing(p['imPath'], (T, C, Z, Y, X), np.uint16, du, nscales=2)
l0[:] = np.random.default_rng(0).integers(100, 1000, size=(T, C, Z, Y, X), dtype=np.uint16)
zarr_utils.write_multiscale_pyramid(g, l0, du, 2, list(ch))
ome_xml_utils.save_meta_in_zarr(p['imPath'], omexml=omexml)

# base: 6 squares per frame, numbered from 1 in EVERY frame (the per-frame-restart case)
base = np.zeros((T, Y, X), np.uint16)
for t in range(T):
    for i in range(6):
        y, x = 2 + (i // 3) * 14, 2 + (i % 3) * 12 + t
        base[t, y:y + 10, x:x + 10] = i + 1
tifffile.imwrite(p['basePath'], base)
# nuc: the centre of each square, independently numbered — only overlap matching pairs them
nuc = np.zeros((T, Z, Y, X), np.int32)
for t in range(T):
    for i in range(6):
        y, x = 2 + (i // 3) * 14, 2 + (i % 3) * 12 + t
        nuc[t, 0, y + 2:y + 8, x + 2:x + 8] = 40 - i   # IoU 0.36 with its cell
gn, ln, _ = zarr_utils.open_multiscales_for_writing(p['nucPath'], nuc.shape, nuc.dtype, None,
                                                     axes=['T', 'Z', 'Y', 'X'], kind='labels')
ln[:] = nuc
tifffile.imwrite(p['badPath'], np.ones((T, Y, X + 3), np.uint16))
"""

function _label_import_project(dir::AbstractString)
    uid = "impLab"
    img_dir = joinpath(dir, "1", uid); mkpath(img_dir)
    data_dir = joinpath(dir, "0", uid); mkpath(data_dir)
    src_dir = joinpath(dir, "external"); mkpath(src_dir)
    script = joinpath(dir, "make_fixture.py"); write(script, _LABEL_IMPORT_FIXTURE_PY)
    paths = (imPath = joinpath(data_dir, "ccidImage.ome.zarr"),
             basePath = joinpath(src_dir, "cells.tif"),
             nucPath = joinpath(src_dir, "nuclei.ome.zarr"),
             badPath = joinpath(src_dir, "wrong.tif"))
    log = String[]
    ok = Cecelia.run_py(script, paths, joinpath(dir, "fixture_tasks"); on_log = l -> push!(log, l))
    ok || error("label import fixture failed:\n" * join(log, "\n"))
    img = CciaImage(; uid = uid, name = "imp", dir = img_dir)
    write(state_file(img), JSON3.write(Dict{String,Any}(
        "uID" => uid,
        "filepath" => Dict{String,Any}("default" => "ccidImage.ome.zarr", "_active" => "default"))))
    img, paths
end

@testset "label import — params, validation, QC helper" begin
    @test Cecelia._task_from_fun_name("segment.importLabels") isa Cecelia.ImportLabels
    @test Cecelia._task_from_fun_name("segment.importLabelsMeasure") isa CompositeTask
    spec = Cecelia._task_spec(CompositeTask("segment.importLabelsMeasure"))
    @test [string(s) for s in spec["composite"]] == ["segment.importLabels", "segment.measureLabels"]

    p = Cecelia.parse_import_labels_params(Dict{String,Any}("labelsPath" => " /a/c.tif ",
                                                            "nucLabelsPath" => nothing))
    @test p.labelsPath == "/a/c.tif" && p.nucLabelsPath == "" && !p.matchIds
    @test Cecelia.import_label_sources(p) == Dict("base" => "/a/c.tif")
    p2 = Cecelia.parse_import_labels_params(Dict{String,Any}("labelsPath" => "/c.tif",
                                                             "nucLabelsPath" => "/n.zarr"))
    @test Cecelia.import_label_sources(p2) == Dict("base" => "/c.tif", "nuc" => "/n.zarr")

    t = Cecelia.ImportLabels()
    # cell labels are required; a zarr STORE (a directory) passes as a filePath, any other folder not
    @test_throws ParamValidationError Cecelia.validate_params(t, Dict{String,Any}())
    mktempdir() do d
        store = joinpath(d, "labels.ome.zarr"); mkpath(store)
        @test Cecelia.validate_params(t, Dict{String,Any}("labelsPath" => store)) === nothing
        @test Cecelia.validate_params(t, Dict{String,Any}("labelsPath" => store * "/")) === nothing
        @test_throws ParamValidationError Cecelia.validate_params(t, Dict{String,Any}("labelsPath" => d))
        @test_throws ParamValidationError Cecelia.validate_params(t,
            Dict{String,Any}("labelsPath" => joinpath(d, "missing.tif")))
    end

    # QC: kept ids that do not name their cells are the one unambiguous bad case
    @test isempty(Cecelia.import_labels_qc_findings(Dict(:idMismatchFrac => 0.01)))
    @test isempty(Cecelia.import_labels_qc_findings(Dict(:idMismatchFrac => nothing)))
    @test isempty(Cecelia.import_labels_qc_findings(Dict(:matchIds => true)))
    f = Cecelia.import_labels_qc_findings(Dict(:idMismatchFrac => 0.9))
    @test length(f) == 1 && f[1]["level"] == "warn" && f[1]["code"] == "segment.import_ids_unpaired"
    @test !occursin("{", f[1]["short"]) && !occursin("{", f[1]["long"])
    # a taken name: registered, or one of the files a run would write already on disk — and the
    # refusal names the next free name, checked against registry AND disk
    mktempdir() do d
        img_dir = joinpath(d, "1", "uidT"); mkpath(img_dir)
        timg = CciaImage(; uid = "uidT", name = "t", dir = img_dir)
        none = Dict{String,Any}()
        @test Cecelia.labels_taken(timg, none, "default") === nothing
        reg = Dict{String,Any}("labels" => Dict{String,Any}("default" => ["default.zarr"],
                                                            "default_2" => ["default_2.zarr"]))
        mkpath(joinpath(img_dir, "labels", "default_3_nuc.zarr"))     # an unregistered store on disk
        @test Cecelia.free_label_name(timg, reg, "default") == "default_4"
        @test Cecelia.labels_taken(timg, reg, "default") ==
              "Labels 'default' already exist — use 'default_4', or tick Overwrite existing to replace them"
        mkpath(joinpath(img_dir, "labels", "seg_nuc.zarr"))
        @test Cecelia.labels_taken(timg, none, "seg") !== nothing
        mkpath(joinpath(img_dir, "labels", "ring_halo.zarr"))          # a halo store alone takes it too
        @test Cecelia.labels_taken(timg, none, "ring") !== nothing
        # the registry is per field: a `labels` entry does not take a `branch_labels` name
        @test Cecelia.labels_taken(timg, reg, "default"; field = "branch_labels") === nothing
        breg = Dict{String,Any}("branch_labels" => Dict{String,Any}("seg" => ["seg.zarr"]))
        @test startswith(Cecelia.labels_taken(timg, breg, "seg"; field = "branch_labels"),
                         "Branches 'seg' already exist — use 'seg_2'")
    end
    # the toggle is in the form, and the composite's form too (not hidden there)
    ow = [x for x in Cecelia._task_spec(t)["params"] if get(x, "key", "") == "overwrite"]
    @test length(ow) == 1 && ow[1]["default"] == false && get(ow[1], "hideInComposite", false) == false
    @test haskey(Cecelia.COHORT_METRICS, "segment.importLabels")
    @test haskey(Cecelia.NOT_RUN_TASKS, "segment.importLabels") &&
          haskey(Cecelia.NOT_RUN_TASKS, "segment.importLabelsMeasure")
end

@testset "label import end to end — import → registered labels → label_props" begin
    mktempdir() do dir
        img, paths = _label_import_project(dir)
        logs = String[]

        # a shape mismatch fails the run, names both shapes, and registers nothing
        bad = Cecelia._run_task(Cecelia.ImportLabels(), img,
            Dict{String,Any}("labelsPath" => paths.badPath, "outputValueName" => "bad");
            on_log = l -> push!(logs, l))
        @test bad === nothing
        msg = join(logs, "\n")
        @test occursin("(2, 32, 43)", msg) && occursin("(2, 1, 32, 40)", msg)
        @test !haskey(get(read_ccid_raw(state_file(img)), "labels", Dict()), "bad")
        @test !isdir(joinpath(img._dir, "labels", "bad.zarr"))

        # the composite: TIFF cells + zarr nuclei, matched, then measured
        empty!(logs)
        task = Cecelia._task_from_fun_name("segment.importLabelsMeasure")
        res = Cecelia._run_task(task, img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "nucLabelsPath" => paths.nucPath,
            "matchIds" => true, "outputValueName" => "imp");
            on_log = l -> push!(logs, l))
        @test res !== nothing || error(join(logs, "\n"))

        raw = read_ccid_raw(state_file(img))
        @test raw["labels"]["imp"] == ["imp.zarr", "imp_nuc.zarr"]
        @test isdir(joinpath(img._dir, "labels", "imp.zarr"))
        @test isdir(joinpath(img._dir, "labels", "imp_nuc.zarr"))
        @test raw["label_props"]["imp"] == "imp.h5ad"

        # 6 cells per frame, ids made unique across the 2 frames → 12 rows, every one with its nucleus
        lp = label_props(joinpath(img._dir, "labelProps", "imp.h5ad"))
        @test n_obs(lp) == 12
        df = lp |> v -> select_cols(v, ["nuc_mean_intensity_0"]) |> as_df
        @test length(unique(df.label)) == 12
        @test all(x -> x isa Real && isfinite(x) && x > 0, df.nuc_mean_intensity_0)

        qc = read_qc(img, "segment.importLabels", "imp")
        @test qc !== nothing
        @test qc["metrics"]["nCells"] == 12
        @test qc["metrics"]["byType"]["nuc"] == 12
        @test isempty(qc["findings"])
        @test read_qc(img, "segment.measureLabels", "imp")["metrics"]["nCells"] == 12

        # the name is taken now: refused up front without Overwrite existing — nothing written,
        # the existing stores, registration and table untouched
        snapshot(p) = sort([(relpath(joinpath(r, f), p), mtime(joinpath(r, f)))
                            for (r, _, fs) in walkdir(p) for f in fs])
        before = (snapshot(joinpath(img._dir, "labels")), read(state_file(img), String))
        empty!(logs)
        @test Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "outputValueName" => "imp");
            on_log = l -> push!(logs, l)) === nothing
        @test any(l -> occursin("Labels 'imp' already exist — use 'imp_2'", l), logs)
        @test (snapshot(joinpath(img._dir, "labels")), read(state_file(img), String)) == before
        @test !any(l -> occursin("PROGRESS", l) || occursin(">> image", l), logs)  # runner never ran
        # …and the composite stops there too: no measure over the old labels
        @test Cecelia._run_task(task, img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "outputValueName" => "imp"); on_log = _ -> nothing) === nothing
        @test (snapshot(joinpath(img._dir, "labels")), read(state_file(img), String)) == before

        # with it: replaced (cells only this time, so the registration drops the nucleus store)
        @test Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "outputValueName" => "imp", "overwrite" => true);
            on_log = _ -> nothing) !== nothing
        @test read_ccid_raw(state_file(img))["labels"]["imp"] == ["imp.zarr"]
        newest(sn) = maximum(last, sn)
        @test newest(snapshot(joinpath(img._dir, "labels", "imp.zarr"))) >
              newest(filter(x -> startswith(x[1], "imp.zarr"), before[1]))   # rewritten, not kept

        # matching OFF on the same, independently numbered nuclei: imported as given, and flagged
        Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "nucLabelsPath" => paths.nucPath,
            "outputValueName" => "raw"); on_log = _ -> nothing)
        codes = [f["code"] for f in read_qc(img, "segment.importLabels", "raw")["findings"]]
        @test "segment.import_ids_unpaired" in codes
    end
end

# ── Every label writer refuses a taken name unless "Overwrite existing" is ticked ──────────────────
# One guard (`assert_labels_free`, segmentation.jl), called before anything is read or written. The
# refusal needs no runner; with the toggle on, the handler gets PAST the guard — shown here by it failing
# on the next check instead (no image registered), so no segmenter has to actually run.
@testset "label writers refuse a taken output name" begin
    for (fun, extra, field, sub) in (
            ("segment.cellpose", Dict{String,Any}(), "labels", "labels"),
            ("segment.coastal", Dict{String,Any}(), "labels", "labels"),
            ("segment.ridges", Dict{String,Any}(), "labels", "labels"),
            ("segment.importLabels", Dict{String,Any}("labelsPath" => "/nonexistent.tif"), "labels", "labels"),
            ("segment.branching", Dict{String,Any}(), "branch_labels", "branchLabels"))
        mktempdir() do dir
            img_dir = joinpath(dir, "1", "uidW"); mkpath(joinpath(img_dir, sub, "seg.zarr"))
            img = CciaImage(; uid = "uidW", name = "w", dir = img_dir)
            write(state_file(img), JSON3.write(Dict{String,Any}(
                field => Dict{String,Any}("seg" => ["seg.zarr"]))))
            before = read(state_file(img), String)
            task = Cecelia._task_from_fun_name(fun)
            logs = String[]
            params = merge(Dict{String,Any}("outputValueName" => "seg"), extra)
            @test Cecelia._run_task(task, img, params; on_log = l -> push!(logs, l)) === nothing
            @test any(l -> occursin("'seg' already exist", l), logs)
            @test read(state_file(img), String) == before
            @test readdir(joinpath(img_dir, sub)) == ["seg.zarr"]

            empty!(logs)
            res = try
                Cecelia._run_task(task, img, merge(params, Dict{String,Any}("overwrite" => true));
                                  on_log = l -> push!(logs, l))
            catch e
                e
            end
            @test res === nothing || res isa Exception
            @test !any(l -> occursin("already exist", l), logs)     # past the guard
        end
    end

    # Which tasks are writers is read off the SPEC, not off one registration call: any task whose output
    # name is a label-set name (`valueNameInput` in the `labels`/`branches` namespace). Each must call
    # the guard BEFORE its runner and offer the shared toggle — so a sixth writer, however it registers
    # its stores, fails here until it does both. `register_label_files!` callers are a second net.
    flat(ps) = reduce(vcat, [p isa AbstractDict && string(get(p, "type", "")) in ("section", "group") ?
                                 flat(get(p, "params", [])) : [p] for p in ps]; init = Any[])
    writers = String[]
    for (fun, task) in Cecelia._fun_name_map()
        task isa CompositeTask && continue
        spec = Cecelia._task_spec(task)
        spec === nothing && continue
        ps = flat(get(spec, "params", []))
        any(p -> p isa AbstractDict && get(p, "type", "") == "valueNameInput" &&
                 string(get(p, "namespace", "")) in ("labels", "branches"), ps) || continue
        push!(writers, fun)
        # the registry the spec declares is one the guard knows (one declaration, read by run + form)
        @test Cecelia.output_registry(task) in keys(Cecelia.LABEL_REGISTRIES)
        jl = replace(Cecelia._spec_path(task), r"\.json$" => ".jl")
        txt = read(jl, String)
        g = findfirst("assert_labels_free(", txt)
        r = findfirst("run_py(", txt)
        @test g !== nothing && r !== nothing && first(g) < first(r)
        @test any(p -> p isa AbstractDict && get(p, "key", "") == "overwrite", ps)
    end
    @test Cecelia.output_registry(Cecelia.Branching()) == "branch_labels"
    @test Cecelia.output_registry(Cecelia.CellposeSegment()) == "labels"
    @test sort(writers) == ["segment.branching", "segment.cellpose", "segment.coastal",
                            "segment.importLabels", "segment.ridges"]
    for (d, _, fs) in walkdir(joinpath(pkgdir(Cecelia), "src", "tasks")), f in fs
        endswith(f, ".jl") && occursin("register_label_files!(", read(joinpath(d, f), String)) || continue
        @test occursin("assert_labels_free(", read(joinpath(d, f), String))
    end

    # …and the composites a user actually runs carry the toggle (merged from their first step)
    for c in ("segment.cellposeMeasure", "segment.coastalMeasure", "segment.importLabelsMeasure")
        step = Cecelia._task_from_fun_name(string(first(Cecelia._task_spec(CompositeTask(c))["composite"])))
        @test any(p -> p isa AbstractDict && get(p, "key", "") == "overwrite",
                  Cecelia._task_spec(step)["params"])   # top level, so the merged form shows it
    end
end
