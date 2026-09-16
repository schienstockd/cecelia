# ── OME metadata, QC framework, process kill, project export ──────────────────
# Extracted from suite.jl (2026-09-16). See suite.jl header for the top-level rule:
# do NOT re-wrap this file in a `begin` block or a single outer @testset — its
# @testset blocks run under the aggregating one that runtests.jl opens.

# ── Physical-size / timing metadata (import review, edit, resync) ──────────────
# All fixtures are synthetic temp zarrs (no real data) — these functions are pure
# readers/writers over BOTH on-disk layouts (CLAUDE.md → OME-ZARR dual-format).
@testset "OME metadata read/edit/resync" begin
    # Build a minimal zarr in either layout, plus optional `.zarray` (shape → SizeC/T/Z) and
    # `OME/METADATA.ome.xml` (planes).
    #   :series (bioformats2raw) — multiscales in `0/.zattrs`, level-0 array at `0/0`
    #   :flat   (create_multiscales) — multiscales at the ROOT `.zattrs`, level-0 array at `0`,
    #           whose own `.zattrs` is `{}`. That empty file is the point: both layouts have a
    #           `0/` child, so only its CONTENT tells them apart.
    function make_zarr(dir; axes, level_scales, units = Dict{String,String}(),
                       shape = nothing, planes = nothing, layout = :series)
        base = layout === :series ? joinpath(dir, "0") : dir
        mkpath(base)
        ax_objs = map(axes) do a
            o = Dict{String,Any}("name" => a,
                "type" => a in ("x", "y", "z") ? "space" : (a == "t" ? "time" : "channel"))
            haskey(units, a) && (o["unit"] = units[a])
            o
        end
        datasets = [Dict{String,Any}("path" => string(i - 1),
                      "coordinateTransformations" =>
                          [Dict{String,Any}("type" => "scale", "scale" => level_scales[i])])
                    for i in eachindex(level_scales)]
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => ax_objs, "datasets" => datasets)])
        open(joinpath(base, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        if layout === :flat
            mkpath(joinpath(dir, "0"))
            write(joinpath(dir, "0", ".zattrs"), "{}")   # level-0 ARRAY, no multiscales
        end
        if !isnothing(shape)
            mkpath(joinpath(base, "0"))
            open(joinpath(base, "0", ".zarray"), "w") do io
                JSON3.write(io, Dict{String,Any}("shape" => shape))
            end
        end
        if !isnothing(planes)
            mkpath(joinpath(dir, "OME"))
            body = join([
                "<Plane TheZ=\"$(p.z)\" TheT=\"$(p.t)\" DeltaT=\"$(p.dt)\"" *
                (haskey(p, :unit) ? " DeltaTUnit=\"$(p.unit)\"" : "") * "/>"
                for p in planes], "\n")
            open(joinpath(dir, "OME", "METADATA.ome.xml"), "w") do io
                write(io, "<OME><Image><Pixels>$body</Pixels></Image></OME>")
            end
        end
        dir
    end

    # ── _delta_t_fallback: median of successive DeltaT diffs at TheZ=0, unit-converted to s ──
    @testset "_delta_t_fallback" begin
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 0, dt = 0.0, unit = "ms"),
                                (z = 0, t = 1, dt = 5000.0, unit = "ms"),
                                (z = 0, t = 2, dt = 10000.0, unit = "ms")])
            @test Cecelia._delta_t_fallback(d) == 5.0            # median of [5, 5] s
        end
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 1, dt = 2.0, unit = "min")])
            @test Cecelia._delta_t_fallback(d) == 120.0          # single interval, TheT=0 anchored at 0
        end
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 1, dt = 30.0)])      # no unit → seconds
            @test Cecelia._delta_t_fallback(d) == 30.0
        end
        # non-self-closing <Plane>…</Plane> (some vendors) — DeltaT is on the opening tag
        mktempdir() do d
            mkpath(joinpath(d, "OME"))
            write(joinpath(d, "OME", "METADATA.ome.xml"),
                  "<OME><Image><Pixels>" *
                  "<Plane TheZ=\"0\" TheT=\"1\" DeltaT=\"3\" DeltaTUnit=\"s\"><Annotation/></Plane>" *
                  "</Pixels></Image></OME>")
            @test Cecelia._delta_t_fallback(d) == 3.0
        end
        # Warm-up outlier at TheT=1: the first interval is longer than the true rate. Median across
        # all successive intervals returns the true 30.26 s, not the 34.69 s the old TheT=1 sampler
        # would have baked in. Matches the c91ICQ LIF that motivated this change.
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 0, dt = 0.0,   unit = "s"),
                                (z = 0, t = 1, dt = 34.69, unit = "s"),
                                (z = 0, t = 2, dt = 64.95, unit = "s"),
                                (z = 0, t = 3, dt = 95.22, unit = "s")])
            r = Cecelia._delta_t_fallback(d)
            @test 30.2 <= r <= 30.3                              # diffs [34.69, 30.26, 30.27] → 30.27
        end
        # A paused frame mid-run must not skew the recorded rate: [30, 170, 30] → 30 (median).
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 0, dt = 0.0,   unit = "s"),
                                (z = 0, t = 1, dt = 30.0,  unit = "s"),
                                (z = 0, t = 2, dt = 200.0, unit = "s"),
                                (z = 0, t = 3, dt = 230.0, unit = "s")])
            @test Cecelia._delta_t_fallback(d) == 30.0
        end
        # bf2raw wrapper layout: OME/METADATA.ome.xml is at the STORE ROOT, and the caller may pass
        # the series subdir (`.../store/0`) instead — which is what `img_filepath` returns and what
        # the import task's `resolved_zarr` is. Must find the sidecar via `dirname(zarr_path)`.
        mktempdir() do d
            mkpath(joinpath(d, "OME"))
            write(joinpath(d, "OME", "METADATA.ome.xml"),
                  "<OME><Image><Pixels>" *
                  "<Plane TheZ=\"0\" TheT=\"0\" DeltaT=\"0\" DeltaTUnit=\"s\"/>" *
                  "<Plane TheZ=\"0\" TheT=\"1\" DeltaT=\"5\" DeltaTUnit=\"s\"/>" *
                  "</Pixels></Image></OME>")
            mkpath(joinpath(d, "0"))
            @test Cecelia._delta_t_fallback(joinpath(d, "0")) == 5.0
        end
        @test isnothing(Cecelia._delta_t_fallback(joinpath(tempdir(), "nope-$(rand(UInt32))")))
    end

    # ── read_ome_metadata: unit-less-t placeholder is rejected; DeltaT fills the gap ──
    @testset "read_ome_metadata" begin
        # t axis has a scale (1.0) but NO unit → placeholder, must NOT become TimeIncrement=1.0;
        # SizeT>1 so the DeltaT fallback kicks in and supplies the real interval.
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"],
                      level_scales = [[1.0, 0.6, 0.5, 0.5]],
                      units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                      shape = [3, 1, 4, 4],
                      planes = [(z = 0, t = 1, dt = 7.0, unit = "s")])
            m = read_ome_metadata(d)
            @test m["SizeT"] == 3
            @test m["PhysicalSizeX"] == 0.5
            @test m["PhysicalSizeZ"] == 0.6
            @test m["PhysicalSizeUnit"] == "micrometer"
            @test m["TimeIncrement"] == 7.0                       # from DeltaT, not the 1.0 placeholder
            @test m["TimeIncrementUnit"] == "second"
        end
        # t axis WITH a unit → trusted verbatim, no fallback needed.
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                      units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                      shape = [4, 8, 8])
            m = read_ome_metadata(d)
            @test m["TimeIncrement"] == 2.5
            @test m["TimeIncrementUnit"] == "second"
        end
    end

    # ── update_ome_scale!: level-0 value set, other levels keep their downsample ratio; units ──
    @testset "update_ome_scale!" begin
        mktempdir() do d
            # z doesn't downsample (0.6, 0.6); x halves per level (0.5, 1.0)
            make_zarr(d; axes = ["z", "y", "x"],
                      level_scales = [[0.6, 0.5, 0.5], [0.6, 1.0, 1.0]])
            update_ome_scale!(d, Dict("z" => 3.0, "x" => 0.65);
                units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
            z = JSON3.read(read(joinpath(d, "0", ".zattrs"), String))
            dss = z[:multiscales][1][:datasets]
            s0 = dss[1][:coordinateTransformations][1][:scale]
            s1 = dss[2][:coordinateTransformations][1][:scale]
            @test s0[1] == 3.0 && s1[1] == 3.0                   # z ratio 5× applied to both levels
            @test s0[3] == 0.65 && isapprox(s1[3], 1.3)          # x ratio 1.3× preserves downsample
            m = read_ome_metadata(d)
            @test m["PhysicalSizeUnit"] == "micrometer"          # axis unit now round-trips
            @test m["PhysicalSizeZ"] == 3.0
        end
        # unit-only edit (no numeric change) still writes the axis unit
        mktempdir() do d
            make_zarr(d; axes = ["y", "x"], level_scales = [[0.5, 0.5]])
            @test isnothing(get(read_ome_metadata(d), "PhysicalSizeUnit", nothing))
            update_ome_scale!(d, Dict{String,Float64}();
                              units = Dict("x" => "nanometer", "y" => "nanometer"))
            @test read_ome_metadata(d)["PhysicalSizeUnit"] == "nanometer"
        end
    end

    # ── update_ome_xml_pixels!: replace an existing attr, insert a missing one ──
    @testset "update_ome_xml_pixels!" begin
        mktempdir() do d
            mkpath(joinpath(d, "OME"))
            xml_file = joinpath(d, "OME", "METADATA.ome.xml")
            write(xml_file, "<OME><Image><Pixels SizeX=\"4\" PhysicalSizeZ=\"0.6\">" *
                            "<Plane/></Pixels></Image></OME>")
            update_ome_xml_pixels!(d, Dict("PhysicalSizeZ" => "3.0", "TimeIncrement" => "5.0"))
            out = read(xml_file, String)
            @test occursin("PhysicalSizeZ=\"3.0\"", out)         # replaced
            @test !occursin("PhysicalSizeZ=\"0.6\"", out)
            @test occursin("TimeIncrement=\"5.0\"", out)         # inserted
            @test occursin("SizeX=\"4\"", out)                   # untouched
        end
    end

    # ── sync_zarr_calibration!: one translator (meta shape → zarr) for import + editor ──
    @testset "sync_zarr_calibration!" begin
        @test !Cecelia.has_calibration_meta(Dict{String,Any}("SizeC" => 2))
        @test !Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => nothing))  # null clear
        @test Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => 3.0))
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 0.6, 0.5, 0.5]],
                      shape = [3, 1, 4, 4],
                      planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])  # OME/ dir + <Pixels>
            # a meta-shaped correction (as ccid.json / the importer / the editor produce it)
            Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "micrometer",
                "TimeIncrement" => 5.0, "TimeIncrementUnit" => "second"))
            # .zattrs now round-trips the corrected spatial value + unit
            m = read_ome_metadata(d)
            @test m["PhysicalSizeZ"] == 3.0
            @test m["PhysicalSizeUnit"] == "micrometer"
            # OME-XML <Pixels> carries the time interval napari reads unconditionally
            xml = read(joinpath(d, "OME", "METADATA.ome.xml"), String)
            @test occursin("TimeIncrement=\"5.0\"", xml)
            @test occursin("TimeIncrementUnit=\"s\"", xml)
        end
    end

    # ── flat (create_multiscales) layout: the 8-bit import + crop write one ──
    # Regression: these readers/writers hardcoded the series `0/.zattrs`. For a flat store that
    # path exists too (the level-0 array's own, empty `.zattrs`), so every one of them found no
    # multiscales and returned silently. `sync_zarr_calibration!` then landed its OME-XML half
    # and dropped its NGFF half — a store claiming `TimeIncrement="10.0"` in XML and
    # `t: {unit: second, scale: 1.0}` in NGFF, which napari renders as 1 s/frame.
    @testset "flat layout (create_multiscales)" begin
        mktempdir() do d
            make_zarr(d; layout = :flat, axes = ["t", "z", "y", "x"],
                      level_scales = [[1.0, 5.0, 0.5, 0.5]], shape = [180, 8, 4, 4],
                      units = Dict("t" => "second", "z" => "micrometer",
                                   "y" => "micrometer", "x" => "micrometer"))
            @test Cecelia.series_base(d) == d                       # not the `0/` array
            m = read_ome_metadata(d)
            @test m["SizeT"] == 180 && m["SizeZ"] == 8              # .zarray found at `0/`
            @test m["PhysicalSizeZ"] == 5.0
            @test m["TimeIncrement"] == 1.0                         # the placeholder, pre-sync

            Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second"))
            @test read_ome_metadata(d)["TimeIncrement"] == 10.0     # NGFF half now lands
            z = JSON3.read(read(joinpath(d, ".zattrs"), String))
            @test z[:multiscales][1][:datasets][1][:coordinateTransformations][1][:scale][1] == 10.0
            @test read(joinpath(d, "0", ".zattrs"), String) == "{}" # array attrs untouched
        end
        # series layout still resolves to `0/` — the discriminator is the multiscales attr
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                      units = Dict("t" => "second"))
            @test Cecelia.series_base(d) == joinpath(d, "0")
        end
        @test Cecelia.series_base(joinpath(tempdir(), "nope-$(rand(UInt32))")) isa String
    end

    # ── Cross-language: the Julia and Python calibration stamps must agree ──
    # Calibration lives in two on-disk copies (NGFF `.zattrs`, OME-XML `<Pixels>`) and has two
    # writers that cannot call each other: Python `zarr_utils.write_calibration` (used by every
    # task runner) and Julia `sync_zarr_calibration!` (the importer + metadata editor). Each
    # therefore carries its own unit table and its own idea of which axis gets what. This is the
    # only thing that stops them drifting: same stale store, one stamped by each, byte-compared.
    @testset "calibration writers agree across languages" begin
        pyroot  = joinpath(dirname(dirname(dirname(@__DIR__))), "python")
        # `success` THROWS (IOError ENOENT) when there is no `python` on PATH at all — which is the
        # ordinary case for `julia --project test/runtests.jl` outside the pixi env, and it errored the
        # suite instead of skipping the way the next line intends.
        haspy   = try
            success(pipeline(addenv(`python -c "import ome_types, zarr, dask, cecelia"`,
                                    "PYTHONPATH" => pyroot);
                             stdout = devnull, stderr = devnull))
        catch
            false
        end
        if !haspy
            @test_skip "analysis-env Python (ome_types/zarr/dask) not importable"
        else
            mktempdir() do d
                a, b = joinpath(d, "py.ome.zarr"), joinpath(d, "jl.ome.zarr")
                script = joinpath(d, "fixture.py")
                write(script, """
import sys, numpy as np, dask.array as da, ome_types, zarr
import cecelia.utils.zarr_utils as zu, cecelia.utils.ome_xml_utils as ox
from cecelia.utils.dim_utils import DimUtils
XML = '''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="x"><Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
    SizeT="3" SizeC="2" SizeZ="5" SizeY="4" SizeX="3"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="\\u00b5m" PhysicalSizeY="0.5" PhysicalSizeYUnit="\\u00b5m"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="\\u00b5m" TimeIncrement="10.0" TimeIncrementUnit="s">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><Channel ID="Channel:0:1" SamplesPerPixel="1"/>
    <MetadataOnly/></Pixels></Image></OME>'''
SHAPE = (3, 2, 5, 4, 3)
du = DimUtils(ome_types.from_xml(XML), use_channel_axis=True)
du.calc_image_dimensions(SHAPE)
for p in sys.argv[1:3]:
    zu.create_multiscales(da.from_array(np.zeros(SHAPE, dtype=np.uint16), chunks=SHAPE),
                          p, dim_utils=du, nscales=2)
    # Reproduce the shipped bug on BOTH stores: NGFF t back to the unit-less 1.0 placeholder and a
    # sidecar carrying someone else's numbers, exactly what a half-landed sync left behind.
    g = zarr.open_group(p, mode='a'); ms = g.attrs['multiscales']
    ms[0]['axes'] = [{k: v for k, v in ax.items() if not (ax['name'] == 't' and k == 'unit')}
                     for ax in ms[0]['axes']]
    for ds in ms[0]['datasets']:
        ds['coordinateTransformations'][0]['scale'][0] = 1.0
    g.attrs['multiscales'] = ms
    stale = ome_types.from_xml(XML)
    stale.images[0].pixels.physical_size_z = 99.0
    stale.images[0].pixels.time_increment  = 99.0
    ox.write_ome_xml(p, stale)
zu.write_calibration(sys.argv[1], du)     # the PYTHON stamp, on the first store only
""")
                @test success(pipeline(addenv(`python $script $a $b`, "PYTHONPATH" => pyroot);
                                       stdout = devnull, stderr = devnull))

                # the JULIA stamp, same calibration, on the second store
                Cecelia.sync_zarr_calibration!(b, Dict{String,Any}(
                    "PhysicalSizeX" => 0.5, "PhysicalSizeY" => 0.5, "PhysicalSizeZ" => 2.0,
                    "PhysicalSizeUnit" => "micrometer",
                    "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second"))

                # NGFF half — identical axes (incl. units) and identical per-level scales.
                # Compared field-by-field, not as raw JSON: the two writers emit the same keys in
                # different ORDER, which is meaningless to every reader.
                za, zb = (JSON3.read(read(joinpath(p, ".zattrs"), String))[:multiscales][1]
                          for p in (a, b))
                axkey(ms) = [(string(get(ax, :name, "")), string(get(ax, :type, "")),
                              string(get(ax, :unit, ""))) for ax in ms[:axes]]
                sckey(ms) = [(string(get(d, :path, "")),
                              collect(Float64, first(d[:coordinateTransformations])[:scale]))
                             for d in ms[:datasets]]
                @test axkey(za) == axkey(zb)
                @test sckey(za) == sckey(zb)
                # …and it is the RIGHT answer, not merely the same wrong one
                @test read_ome_metadata(a)["TimeIncrement"] == 10.0
                @test read_ome_metadata(a)["PhysicalSizeZ"] == 2.0

                # OME-XML half — same <Pixels> calibration attrs from both stamps
                for attr in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ",
                             "PhysicalSizeZUnit", "TimeIncrement", "TimeIncrementUnit")
                    vals = map((a, b)) do p
                        tag = match(r"<Pixels\b[^>]*>",
                                    read(joinpath(p, "OME", "METADATA.ome.xml"), String)).match
                        m = match(Regex(attr * "=\"([^\"]*)\""), tag)
                        isnothing(m) ? nothing : m.captures[1]
                    end
                    @test vals[1] == vals[2] != nothing
                end
                @test occursin("TimeIncrement=\"10.0\"",
                               read(joinpath(a, "OME", "METADATA.ome.xml"), String))
            end
        end
    end

    # ── _merge_zarr_meta_into_ccid!: overwrite=true is authoritative; false is fill-only ──
    @testset "merge fill-only vs overwrite" begin
        proj = create_project!(name = "meta-merge-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        # Simulate an ImageJ-corrected image: PhysicalSizeZ + the ccid-only PhysicalSizeZ_raw marker
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))

        # Fill-only backfill: existing keys survive, genuinely-missing ones get filled.
        Cecelia._merge_zarr_meta_into_ccid!(img,
            Dict{String,Any}("PhysicalSizeZ" => 0.6, "PhysicalSizeX" => 0.5); overwrite = false)
        r = init_object(proj.uid, img.uid)
        @test r.meta["PhysicalSizeZ"] == 3.0                     # NOT reverted to the raw 0.6
        @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker NOT dropped
        @test r.meta["PhysicalSizeX"] == 0.5                     # filled (was absent)

        # Authoritative import merge: clears derived keys, takes the fresh read verbatim.
        Cecelia._merge_zarr_meta_into_ccid!(r,
            Dict{String,Any}("PhysicalSizeZ" => 0.6); overwrite = true)
        r2 = init_object(proj.uid, img.uid)
        @test r2.meta["PhysicalSizeZ"] == 0.6
        @test !haskey(r2.meta, "PhysicalSizeZ_raw")              # zombie marker cleared
        rm(proj.root; recursive = true)
    end

    # ── A re-import must NOT revert renamed channels ────────────────────────────────────────────
    # bioformats2raw always writes the vendor's own CH1..CHn into the store's omero labels, so the
    # fresh read never reproduces a rename — and saved task params reference channels by name.
    @testset "import keeps renamed channel names" begin
        proj = create_project!(name = "meta-chan-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img")

        # First import: nothing stored yet → take the fresh read.
        Cecelia._merge_zarr_meta_into_ccid!(img,
            Dict{String,Any}("SizeC" => 4,
                             "channel_names" => ["CH1", "CH2", "CH3", "CH4"]); overwrite = true)
        r = init_object(proj.uid, img.uid)
        @test channel_names(r) == ["CH1", "CH2", "CH3", "CH4"]

        # The user renames them (the API path).
        set_channel_names!(r, ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]; check_length = false)
        save!(r)

        # Re-import of the same source: same channel count → the renames survive, and the task
        # says so rather than reverting silently.
        logged = String[]
        Cecelia._merge_zarr_meta_into_ccid!(init_object(proj.uid, img.uid),
            Dict{String,Any}("SizeC" => 4,
                             "channel_names" => ["CH1", "CH2", "CH3", "CH4"]);
            overwrite = true, on_log = l -> push!(logged, l))
        r2 = init_object(proj.uid, img.uid)
        @test channel_names(r2) == ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
        @test any(l -> occursin("Kept the existing channel names", l), logged)

        # A source whose channel count changed: the stored list cannot describe it → take the fresh
        # names (and stay silent, nothing was preserved).
        logged2 = String[]
        Cecelia._merge_zarr_meta_into_ccid!(r2,
            Dict{String,Any}("SizeC" => 2, "channel_names" => ["CH1", "CH2"]);
            overwrite = true, on_log = l -> push!(logged2, l))
        r3 = init_object(proj.uid, img.uid)
        @test channel_names(r3) == ["CH1", "CH2"]
        @test isempty(logged2)

        # Fill-only (resync) never touches channel names at all, whatever the count.
        Cecelia._merge_zarr_meta_into_ccid!(r3,
            Dict{String,Any}("channel_names" => ["nope", "nope2"]); overwrite = false)
        @test channel_names(init_object(proj.uid, img.uid)) == ["CH1", "CH2"]

        rm(proj.root; recursive = true)
    end

    # ── resync_ome_meta! end-to-end: fill-only into ccid, then push the merge back to the zarr ──
    @testset "resync_ome_meta! fill-only" begin
        proj = create_project!(name = "meta-resync-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))   # ImageJ-corrected, ccid-only

        # Register a "default" zarr on disk carrying the RAW (pre-correction) calibration.
        zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
        make_zarr(zdir; axes = ["z", "y", "x"], level_scales = [[0.6, 0.5, 0.5]],
                  units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                  shape = [1, 8, 8])
        img.filepath["default"]         = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)

        @test resync_ome_meta!(init_object(proj.uid, img.uid))
        r = init_object(proj.uid, img.uid)
        @test r.meta["PhysicalSizeZ"] == 3.0                     # correction survives resync
        @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker survives
        @test r.meta["PhysicalSizeUnit"] == "micrometer"         # genuinely-missing field filled
        @test r.meta["PhysicalSizeX"] == 0.5
        # …and the ccid-only correction is pushed BACK into the store, so the two agree
        @test read_ome_metadata(zdir)["PhysicalSizeZ"] == 3.0
        rm(proj.root; recursive = true)
    end

    # ── resync_ome_meta! repairs a flat store whose NGFF calibration never landed ──
    # The shipped case: the 8-bit import wrote a flat store, `sync_zarr_calibration!` silently
    # skipped its NGFF half, and the store ended up disagreeing with its own OME-XML. ccid.json
    # has the right number, so resync is the repair path — no re-import.
    @testset "resync_ome_meta! repairs a stale flat store" begin
        proj = create_project!(name = "meta-flat-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second",
            "PhysicalSizeUnit" => "micrometer"))

        zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
        make_zarr(zdir; layout = :flat, axes = ["t", "y", "x"],
                  level_scales = [[1.0, 0.5, 0.5]], shape = [180, 8, 8],
                  units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                  planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])
        img.filepath["default"]            = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)

        @test read_ome_metadata(zdir)["TimeIncrement"] == 1.0     # stale placeholder, pre-repair
        @test resync_ome_meta!(init_object(proj.uid, img.uid))
        @test read_ome_metadata(zdir)["TimeIncrement"] == 10.0    # NGFF now matches ccid
        @test occursin("TimeIncrement=\"10.0\"",
                       read(joinpath(zdir, "OME", "METADATA.ome.xml"), String))
        @test init_object(proj.uid, img.uid).meta["TimeIncrement"] == 10.0  # ccid untouched
        rm(proj.root; recursive = true)
    end
end

@testset "QC framework" begin
    # ── The QC copy catalog (app/src/qc/text.jl → QC_TEXT) ────────────────────────────────────
    #
    # QC prose used to live inline in the analysis functions, which made it the least reviewable
    # copy in the app. It now sits in one table; these pin the contract that table has to keep.
    @testset "copy catalog" begin
        @test length(Cecelia.QC_TEXT) > 15

        # Placeholders are filled from keywords, and the emitted `code` is independent of the
        # catalog key — the two cases the `key` argument exists for.
        f = Cecelia.qc_finding("warn", "drift.canvas_expansion";
                               key = "output.canvas_expansion", pct = 42)
        @test f["code"] == "drift.canvas_expansion"
        @test f["short"] == "Output canvas grew +42% in XY"

        # Loud failures, not a user-visible "{channel}".
        @test_throws ErrorException Cecelia.qc_text("no.such.key")
        @test_throws ErrorException Cecelia.qc_text("output.canvas_expansion")  # missing `pct`

        # House style (docs/UI.md): `short` is a fragment, `long` is a sentence. Checked here
        # because the frontend ratchet cannot see Julia strings.
        bad_short = [k for (k, v) in Cecelia.QC_TEXT if occursin(r"[^.]\.$", v.short)]
        @test isempty(bad_short)
        no_period = [k for (k, v) in Cecelia.QC_TEXT if !endswith(v.long, ".")]
        @test isempty(no_period)

        # Every placeholder the catalog uses must be one a caller actually passes; a typo'd
        # `{metrics}` would otherwise only surface when that finding fires in production.
        KNOWN = Set(["channel", "pct", "unit", "dims", "metric", "value", "dir", "median",
                     "count", "min", "tiles", "suggest",
                     # combined findings (multi-channel roll-ups): `n` count, `s` plural suffix,
                     # `channels` comma-joined list — see import.photon_limited
                     "n", "s", "channels",
                     # correction staleness: `scope` is "labels" or "tracks" — see
                     # correction.stale_artefacts
                     "scope"])
        unknown = [m.captures[1] for (_, v) in Cecelia.QC_TEXT
                   for m in eachmatch(r"\{(\w+)\}", v.short * " " * v.long)
                   if !(m.captures[1] in KNOWN)]
        @test isempty(unknown)

        # The inputs are persisted, not just the output — that's what read-time rendering needs.
        g = Cecelia.qc_finding("info", "hmm.dominant_state"; pct = 91)
        @test g["key"] == "hmm.dominant_state" && g["subs"]["pct"] == 91
    end

    # ── Read-time rendering ───────────────────────────────────────────────────────────────────
    #
    # The point of the catalog: fixing a wording should reach QC that is ALREADY on disk, without
    # re-running the analysis that produced it. (And, later, a locale switch does the same.)
    @testset "findings re-render on read" begin
        img = CciaImage(; dir = mktempdir())
        write_qc(img, "behaviour.hmmStates", "default",
                 [Cecelia.qc_finding("info", "hmm.dominant_state"; pct = 91)])

        @test read_qc(img, "behaviour.hmmStates", "default")["findings"][1]["short"] ==
              "One state holds 91% of cells"

        # Edit the catalog; the banked file on disk is NOT rewritten.
        orig = Cecelia.QC_TEXT["hmm.dominant_state"]
        try
            Cecelia.QC_TEXT["hmm.dominant_state"] =
                (short = "{pct}% of cells in one state", long = orig.long)
            doc = read_qc(img, "behaviour.hmmStates", "default")
            @test doc["findings"][1]["short"] == "91% of cells in one state"
            # Symbol access is what lab_log_context/qc_cohort use — must survive the rebuild.
            @test String(get(doc["findings"][1], :short, "")) == "91% of cells in one state"
            @test get(doc, :funName, "") == "behaviour.hmmStates"
        finally
            Cecelia.QC_TEXT["hmm.dominant_state"] = orig
        end

        # A catalog entry that disappears must fall back to the stored snapshot, not blow up the
        # read — this is a data path shared by every image in the payload.
        saved = Cecelia.QC_TEXT["hmm.dominant_state"]
        try
            delete!(Cecelia.QC_TEXT, "hmm.dominant_state")
            @test read_qc(img, "behaviour.hmmStates", "default")["findings"][1]["short"] ==
                  "One state holds 91% of cells"
        finally
            Cecelia.QC_TEXT["hmm.dominant_state"] = saved
        end
    end

    @testset "pre-catalog sidecars are read unchanged" begin
        # Findings banked before the catalog carry no `key`; they must pass through verbatim.
        img = CciaImage(; dir = mktempdir())
        write_qc(img, "mycat.myTask", "default",
                 [qc_finding("warn", "legacy.code", "Old short", "Old long.")])
        doc = read_qc(img, "mycat.myTask", "default")
        @test doc["findings"][1]["short"] == "Old short"
        @test doc["findings"][1]["long"] == "Old long."
        @test !haskey(doc["findings"][1], :key)
    end

    @testset "sidecar round-trip" begin
        img = CciaImage(; dir = mktempdir())
        f = qc_finding("warn", "demo.code", "short text", "long text"; detail = Dict("k" => 1))
        @test f["level"] == "warn" && f["code"] == "demo.code"

        p = write_qc(img, "cleanupImages.driftCorrect", "driftCorrected", [f];
                     source = Dict("shape" => [1, 2, 3]))
        @test isfile(p)
        @test occursin(joinpath("qc", "cleanupImages.driftCorrect", "driftCorrected.json"), p)

        doc = read_qc(img, "cleanupImages.driftCorrect", "driftCorrected")
        @test length(doc["findings"]) == 1
        @test doc["findings"][1]["code"] == "demo.code"

        all = read_all_qc(img)
        @test haskey(all, "cleanupImages.driftCorrect/driftCorrected")

        # no-value_name → falls back to the default key
        write_qc(img, "some.task", "", Dict{String,Any}[])
        @test isfile(qc_path(img, "some.task", VERSIONED_DEFAULT_VAL))
    end

    @testset "canvas-expansion check" begin
        order = "TCZYX"
        # fHqhyb: XY +42%/+21% → flagged; Z doubling is ignored
        bad = qc_canvas_expansion([94, 4, 13, 512, 512], [94, 4, 26, 728, 618], order)
        @test bad !== nothing && bad["code"] == "output.canvas_expansion"
        # LUkCpP (normal): XY +6%/+3% → not flagged even though Z grew +46%
        @test qc_canvas_expansion([64, 4, 13, 512, 512], [64, 4, 19, 541, 527], order) === nothing
    end

    @testset "drift findings" begin
        base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"])
        smooth = [[0.0, 1.0, 1.0] for _ in 1:20]
        spiky  = copy(smooth); spiky[16] = [0.0, 120.0, 90.0]   # jump at frame 16

        # bad ref: canvas ballooned AND a spike → both findings
        meta_bad = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                    "outputShape" => [20, 4, 26, 728, 618], "shifts" => spiky))
        fb, _, _ = Cecelia._drift_qc_findings(meta_bad)
        codes = Set(f["code"] for f in fb)
        @test "drift.canvas_expansion" in codes
        @test "drift.jump" in codes
        jump = first(f for f in fb if f["code"] == "drift.jump")
        @test jump["detail"]["atT"] == 15                       # 0-based frame index of the spike

        # good ref: modest canvas, smooth trajectory → no findings
        meta_ok = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                   "outputShape" => [20, 4, 19, 541, 527], "shifts" => smooth))
        fo, _, _ = Cecelia._drift_qc_findings(meta_ok)
        @test isempty(fo)

        # A sidecar written before the residual existed carries no `residualRms`. It must not be
        # read as a perfect registration — no finding, and no metric either (a banked 0 would drag
        # the cohort median toward "everything registered").
        @test !haskey(Cecelia._drift_qc_metrics(meta_ok, [20, 4, 13, 512, 512],
                                                [20, 4, 19, 541, 527]), "residualPx")
    end

    @testset "drift reliability findings" begin
        # The registration disagreeing with ITSELF. This is the one check that can tell a broken
        # registration from a movie that genuinely moved a lot — the other two read the trajectory
        # and cannot. Numbers are the measured ones: every movie that registered on this machine
        # sat at 0.13–0.39 px, `4kS67f/fHqhyb` at 24 px.
        base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"],
                                "sourceShape" => [20, 4, 13, 512, 512],
                                "outputShape" => [20, 4, 19, 541, 527],
                                "shifts" => [[0.0, 1.0, 1.0] for _ in 1:20])

        good = merge(base, Dict("residualRms" => 0.39, "residualP90" => 0.5,
                                "nPairs" => 57, "nRejected" => 0))
        @test isempty(Cecelia._drift_qc_findings(good)[1])

        bad = merge(base, Dict("residualRms" => 24.3, "residualP90" => 12.6,
                               "nPairs" => 57, "nRejected" => 8))
        fb = Cecelia._drift_qc_findings(bad)[1]
        unrel = first(f for f in fb if f["code"] == "drift.unreliable")
        @test unrel["level"] == "warn"
        @test unrel["detail"]["nRejected"] == 8
        @test occursin("24.3", unrel["short"])

        # Frames no measurement survived for: their position is predicted, and the sidecar says so.
        gappy = merge(good, Dict("interpolated" => [4, 9]))
        fg = Cecelia._drift_qc_findings(gappy)[1]
        interp = first(f for f in fg if f["code"] == "drift.unregistered_frames")
        @test interp["detail"]["frames"] == [4, 9]
        @test occursin("2 frame", interp["short"])

        # Metrics: the cohort-comparable numbers, and only the ones actually measured.
        m = Cecelia._drift_qc_metrics(bad, [20, 4, 13, 512, 512], [20, 4, 19, 541, 527])
        @test m["residualPx"] == 24.3
        @test m["canvasExpansion"] > 1.0
        @test m["framesInterpolated"] == 0
        # everything the cohort pass is told to aggregate must actually be banked, EXCEPT the
        # rigid-only metric (`maxAngleDeg`) — a translation run has no rotation to measure, so its
        # absence here is the "not measured" discipline `residualPx` follows too. The rigid case
        # is exercised in the "drift rigid findings" testset below.
        for k in Cecelia.COHORT_METRICS["cleanupImages.driftCorrect"]
            k == "maxAngleDeg" && continue
            @test haskey(m, k)
        end
    end

    @testset "drift rigid findings" begin
        # A rigid trajectory carries an `angles` field. Its `interpolated` frames are ones whose
        # rotation exceeded the cap and were predicted from neighbours — a different action for
        # the user than a translation run's lost-lock frames, so a different finding code, and a
        # detail block that names the cap so the QC badge can explain itself without the reader
        # opening the sidecar. `maxAngleDeg` becomes a cohort metric on this run.
        base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Y", "X"],
                                "sourceShape" => [20, 4, 1, 512, 512],
                                "outputShape" => [20, 4, 1, 541, 527],
                                "shifts"      => [[0.5, 0.5] for _ in 1:20],
                                "angles"      => [0.05 * t for t in 0:19],
                                "maxAngleDeg" => 0.95,
                                "maxAngleCap" => 5.0)
        # clean rigid run — no interpolated, no findings other than any translation-side ones
        clean = base
        fc, _, _ = Cecelia._drift_qc_findings(clean)
        @test !any(f -> f["code"] == "drift.rotation.capped", fc)

        # capped rigid run — some frames rejected; the finding code names the rigid path and
        # carries the cap value so a reader knows what threshold was hit
        capped = merge(base, Dict("interpolated" => [3, 12],
                                  "maxAngleDeg" => 12.4))
        fk, _, _ = Cecelia._drift_qc_findings(capped)
        cap = first(f for f in fk if f["code"] == "drift.rotation.capped")
        @test cap["level"] == "warn"
        @test cap["detail"]["frames"] == [3, 12]
        @test cap["detail"]["maxAngleCap"] == 5.0
        @test cap["detail"]["maxAngleDeg"] == 12.4
        # a rigid run must NEVER emit the translation-side unregistered_frames code for the same
        # `interpolated` field — the two codes are mutually exclusive by construction
        @test !any(f -> f["code"] == "drift.unregistered_frames", fk)

        # Metrics: the rigid run banks `maxAngleDeg` alongside the translation metrics
        m = Cecelia._drift_qc_metrics(capped, [20, 4, 1, 512, 512], [20, 4, 1, 541, 527])
        @test m["maxAngleDeg"] == 12.4
        @test m["canvasExpansion"] > 1.0
        # And the CATALOG carries the code, so the QC badge can render its message. Same rule the
        # frontend's `qc.ts` checks — a code without a catalog entry becomes a blank pill.
        @test haskey(Cecelia.QC_TEXT, "drift.rotation.capped")
    end

    @testset "OIR companion-file staging" begin
        # REAL Olympus naming: the registered file already ends in _NNNN.oir and companions are
        # EXTENSIONLESS <mainstem>_00001, _00002, … (this is what shipped broken — only the main
        # matched, so bioformats saw a fraction of the timepoints).
        real = ["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045",
                "M1a-res_0002.oir",           # a DIFFERENT acquisition — must NOT be grabbed
                "M1a-res_0001_notes.txt"]     # non-numeric sibling — excluded
        @test Set(Cecelia._companion_files(real, "M1a-res_0001.oir")) ==
              Set(["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045"])

        # extensioned companions (Img.oir + Img_00001.oir …); sibling Img2 / non-numbered excluded
        names = ["Img.oir", "Img_00001.oir", "Img_00002.oir",
                 "Img2.oir", "Img_processed.oir", "Other.oir", "notes.txt"]
        @test Set(Cecelia._companion_files(names, "Img.oir")) ==
              Set(["Img.oir", "Img_00001.oir", "Img_00002.oir"])
        # regex metacharacters in the stem (the `basal+NECA` bug): literal match, no injection
        plus = ["basal+NECA.oir", "basal+NECA_00001", "basal+NECB.oir"]
        @test Set(Cecelia._companion_files(plus, "basal+NECA.oir")) ==
              Set(["basal+NECA.oir", "basal+NECA_00001"])
        # single self-contained file → just itself
        @test Cecelia._companion_files(["a.tif", "b.tif"], "a.tif") == ["a.tif"]

        # chunked yielding copy is byte-identical (incl. a size that isn't a chunk multiple)
        src = tempname(); dst = tempname()
        data = rand(UInt8, 3 * 1024 * 1024 + 777)
        write(src, data)
        copied = Ref(0)
        Cecelia._copy_file_yielding(src, dst; chunk = 1024 * 1024, on_bytes = n -> (copied[] += n))
        @test read(dst) == data
        @test copied[] == length(data)
        rm(src; force = true); rm(dst; force = true)
    end

    @testset "import metrics" begin
        # base import metric — present for EVERY import (from SizeC/SizeZ/SizeT). An odd channel
        # count or dimensionality vs cohort peers means the wrong file was imported.
        bm = Cecelia.import_metrics(Dict{String,Any}("SizeC" => 4, "SizeZ" => 13, "SizeT" => 20))
        @test bm == Dict{String,Any}("nChannels" => 4, "nZ" => 13, "nT" => 20)
        @test Cecelia.import_metrics(Dict{String,Any}()) === nothing

        # partial metadata: only the keys that are present are banked
        @test Cecelia.import_metrics(Dict{String,Any}("SizeC" => 2)) ==
              Dict{String,Any}("nChannels" => 2)

        # JSON3 round-trip — the real path (meta read back from ccid.json has Symbol keys)
        rt     = JSON3.read(JSON3.write(Dict{String,Any}("SizeC" => 4, "SizeZ" => 13)))
        rtmeta = Dict{String,Any}(String(k) => v for (k, v) in rt)
        @test Cecelia.import_metrics(rtmeta)["nChannels"] == 4

    end

    @testset "clipping-at-acquisition findings" begin
        # `sigfrac` is the one the finding gates on — clipped voxels over SIGNAL voxels. `frac` (over
        # ALL voxels) is still banked, so both are set.
        mk(i, sat; top = 4095, frac = 0.0, n = 0, sigfrac = 0.0) =
            Dict{String,Any}("index" => i, "saturated" => sat, "topValue" => top,
                             "topCount" => n, "topFrac" => frac, "clippedSignalFrac" => sigfrac)
        meta = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, false; top = 1032, frac = 1.0e-6, sigfrac = 1.0e-5),
            # unmistakably clipped: 2% of this channel's SIGNAL voxels piled at the 12-bit ceiling
            mk(1, true;  top = 4095, frac = 0.00018, n = 534, sigfrac = 0.02),
            mk(2, false; top = 2854, frac = 1.0e-6, sigfrac = 1.0e-5),
        ]))

        fs = Cecelia.saturation_qc_findings(meta)
        @test length(fs) == 1                              # only the clipped channel
        @test fs[1]["code"] == "import.channel_saturated"
        @test fs[1]["level"] == "warn"                     # advisory, never a gate
        @test fs[1]["detail"]["channel"] == 1
        # the effective ceiling is reported because it is NOT the dtype maximum on 12-bit-in-16-bit
        @test fs[1]["detail"]["topValue"] == 4095.0
        # the COUNT is what a reader can judge; the fraction is ~1e-6 and rounds away
        @test fs[1]["detail"]["clippedVoxels"] == 534.0

        @test fs[1]["detail"]["clippedSignalPct"] == 2.0     # reported against SIGNAL, not all voxels

        m = Cecelia.saturation_metrics(meta)
        @test m["nChannelsSaturated"] == 1
        @test m["maxClippedFrac"] == 0.00018
        @test m["maxClippedSignalFrac"] == 0.02

        # the check didn't run (pre-existing image, or a non-integer store) → say nothing at all
        @test isempty(Cecelia.saturation_qc_findings(Dict{String,Any}()))
        @test Cecelia.saturation_metrics(Dict{String,Any}()) === nothing
        @test isempty(Cecelia.saturation_qc_findings(
            Dict{String,Any}("saturation" => Dict{String,Any}())))

        # nothing clipped → no findings, but the metrics still bank (a measured zero is a result, and
        # the cohort needs it to tell "clean" apart from "not checked")
        clean = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [mk(0, false)]))
        @test isempty(Cecelia.saturation_qc_findings(clean))
        @test Cecelia.saturation_metrics(clean)["nChannelsSaturated"] == 0

        # TRACE clipping: structurally detected, but far too small to act on. This is the real measured
        # case — 4 of 36 channels across nine kSUFux movies sit at 1.1-1.4e-6, i.e. ~500 voxels of
        # 377 M. No finding (telling someone to lower the gain over 500 voxels is not actionable), but
        # the metric MUST still record it: the cohort comparison is relative, so it is what surfaces an
        # image clipping far more than its session peers.
        # 7.2e-5 of SIGNAL voxels is the worst real case measured across nine movies — three orders
        # below the smoke-alarm level, so it must not warn while still being banked.
        trace = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 1.4e-6, n = 534, sigfrac = 7.2e-5),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(trace))
        tm = Cecelia.saturation_metrics(trace)
        @test tm["nChannelsSaturated"] == 1
        @test tm["maxClippedFrac"] == 1.4e-6
        @test tm["maxClippedSignalFrac"] == 7.2e-5

        # …and just above the level it does warn
        material = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 2.0e-4, n = 75_000, sigfrac = 1.1e-2),
        ]))
        @test length(Cecelia.saturation_qc_findings(material)) == 1

        # the ALL-voxel fraction must not be what decides it: a channel with a large all-voxel fraction
        # but trace signal clipping stays quiet, and vice versa. This is the whole point of the change.
        allvox = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 0.5, n = 999, sigfrac = 1.0e-6),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(allvox))

        # a channel with no recorded fraction is not guessed at
        noneframe = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            Dict{String,Any}("index" => 0, "saturated" => true, "topValue" => 4095),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(noneframe))

        # JSON3 round-trip — the real path: persisted ccid meta comes back with Symbol keys
        rt   = JSON3.read(JSON3.write(meta))
        rtm  = Dict{String,Any}(String(k) => v for (k, v) in rt)
        @test length(Cecelia.saturation_qc_findings(rtm)) == 1
        @test Cecelia.saturation_metrics(rtm)["nChannelsSaturated"] == 1

        # the finding renders — a `{channel}` placeholder with no substitution throws (see qc_text)
        @test occursin("1", fs[1]["short"])
    end

    @testset "photon-limited findings + sparsity metrics" begin
        # The correction-plan photon-limited card's QC signal (CORRECTION_QC_PLAN.md Q-M4). Feeds off
        # the same meta.saturation.channels dict the clipping finding reads — extended in this change
        # with `zeroFrac` / `signalFrac`. Threshold is a smoke alarm, unvalidated: `zeroFrac >= 0.90`
        # per `_PHOTON_LIMITED_ZERO_FRAC`; below it, silent. Photon-limitation is a scanning-mode
        # property (shared laser/PMT settings), so all sparse channels roll up into ONE finding.
        mk(i; zf = nothing, sf = nothing, sat = false, top = 1000) = begin
            d = Dict{String,Any}("index" => i, "saturated" => sat, "topValue" => top,
                                 "topCount" => 0, "topFrac" => 0.0, "clippedSignalFrac" => 0.0)
            isnothing(zf) || (d["zeroFrac"] = zf)
            isnothing(sf) || (d["signalFrac"] = sf)
            d
        end
        meta = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0; zf = 0.60, sf = 0.30),                        # dense — silent
            mk(1; zf = 0.93, sf = 0.04),                        # photon-limited — folded in
            mk(2; zf = 0.89, sf = 0.05),                        # just below the threshold — silent
            mk(3; zf = 0.95, sf = 0.03),                        # photon-limited (worst) — folded in
        ]))

        fs = Cecelia.photon_limited_qc_findings(meta)
        @test length(fs) == 1                                   # ONE combined finding, not N
        @test fs[1]["code"] == "import.photon_limited"
        @test fs[1]["level"] == "info"                          # advisory, denoise nudge — not damage
        @test fs[1]["detail"]["channels"] == [1, 3]
        @test fs[1]["detail"]["nChannels"] == 2
        @test fs[1]["detail"]["worstZeroFrac"] == 0.95
        @test fs[1]["detail"]["worstPct"] == 95.0
        @test length(fs[1]["detail"]["perChannel"]) == 2
        @test fs[1]["detail"]["perChannel"][2]["channel"] == 3
        # the finding renders — `{n}`/`{s}`/`{pct}`/`{channels}` placeholders substituted
        @test occursin("2 photon-limited channels", fs[1]["short"])
        @test occursin("95", fs[1]["short"])
        @test occursin("1, 3", fs[1]["long"])

        m = Cecelia.saturation_metrics(meta)
        @test m["maxZeroFrac"] == 0.95                          # the sparsest channel
        @test m["minSignalFrac"] == 0.03                        # its complement view

        # singular grammar when only ONE channel is photon-limited — `s = ""`, plain "channel"
        singular = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0; zf = 0.60, sf = 0.30), mk(1; zf = 0.92, sf = 0.04),
        ]))
        sfs = Cecelia.photon_limited_qc_findings(singular)
        @test length(sfs) == 1
        @test occursin("1 photon-limited channel ", sfs[1]["short"])
        @test !occursin("channels", sfs[1]["short"])

        # zero photon-limited channels → no finding at all (not an empty one)
        clean = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0; zf = 0.50, sf = 0.40), mk(1; zf = 0.30, sf = 0.60),
        ]))
        @test isempty(Cecelia.photon_limited_qc_findings(clean))

        # the check didn't run: sparsity fields absent → sparsity metrics absent (not zero), but the
        # saturation half still reports. Distinguishes "pre-existing image" from "measured zero".
        pre = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            Dict{String,Any}("index" => 0, "saturated" => false, "topValue" => 1000,
                             "topCount" => 0, "topFrac" => 0.0, "clippedSignalFrac" => 0.0),
        ]))
        @test isempty(Cecelia.photon_limited_qc_findings(pre))
        pm = Cecelia.saturation_metrics(pre)
        @test !haskey(pm, "maxZeroFrac")
        @test !haskey(pm, "minSignalFrac")
        @test pm["nChannelsSaturated"] == 0                     # saturation still banks

        # a channel BOTH saturated AND photon-limited fires both findings (rare: dim channel with a
        # hot pixel driven off-scale). Saturation stays per-channel (each channel has its own gain);
        # photon-limitation is one combined finding.
        both_meta = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            merge(mk(0; zf = 0.97, sf = 0.02),
                  Dict{String,Any}("saturated" => true, "topCount" => 400, "topFrac" => 2.0e-4,
                                   "clippedSignalFrac" => 2.0e-2)),
        ]))
        @test length(Cecelia.saturation_qc_findings(both_meta)) == 1
        @test length(Cecelia.photon_limited_qc_findings(both_meta)) == 1

        # JSON3 round-trip — the real path: persisted ccid meta comes back with Symbol keys
        rt   = JSON3.read(JSON3.write(meta))
        rtm  = Dict{String,Any}(String(k) => v for (k, v) in rt)
        rtfs = Cecelia.photon_limited_qc_findings(rtm)
        @test length(rtfs) == 1
        @test rtfs[1]["detail"]["nChannels"] == 2
        @test Cecelia.saturation_metrics(rtm)["maxZeroFrac"] == 0.95

        # cohort metric registration — the new keys are declared for importImages.omezarr
        keys_declared = Cecelia.COHORT_METRICS["importImages.omezarr"]
        @test "maxZeroFrac"   in keys_declared
        @test "minSignalFrac" in keys_declared
    end

    # CORRECTION_QC_PLAN.md §2.1 — metadata-derived scores. The score-band layer that turns the
    # import probes (Q-M4 shipped in #811) into 0-1 signals the rule engine will trigger on. Pure
    # over meta; no engine yet.
    @testset "correction_plan §2.1 scores" begin
        # QCResult validates and carries the score sentinel `NaN` for "check didn't run"
        r = Cecelia.QCResult("x", 0.5)
        @test r.score == 0.5 && r.level == "info" && r.scope == :image
        @test !Cecelia.qc_score_absent(r)
        absent = Cecelia.QCResult("y", Cecelia.QC_SCORE_ABSENT)
        @test Cecelia.qc_score_absent(absent)
        @test_throws ErrorException Cecelia.QCResult("z", 1.5)   # out-of-range refused

        # ── axis presence: structural, trivial, no cohort story ─────────────────────────────────
        @test Cecelia.qc_axis_t_present(Dict{String,Any}("SizeT" => 100)).score == 1.0
        @test Cecelia.qc_axis_t_present(Dict{String,Any}("SizeT" => 1)).score   == 0.0
        @test Cecelia.qc_axis_t_present(Dict{String,Any}()).score               == 0.0    # missing = 1
        @test Cecelia.qc_axis_z_present(Dict{String,Any}("SizeZ" => 40)).score  == 1.0
        @test Cecelia.qc_axis_z_present(Dict{String,Any}("SizeZ" => 1)).score   == 0.0

        # ── denoise gate: 1.0 = every channel saturated (no clean channel to learn from) ────────
        # `NaN` when the check never ran — a caller must distinguish absent from "measured zero".
        # This is the whole point of `QC_SCORE_ABSENT`: an engine treating absent as 0.0 would
        # silently let denoise run on an image whose saturation state is unknown.
        mk_sat(i, sat) = Dict{String,Any}("index" => i, "saturated" => sat, "topValue" => 1000,
                                          "topCount" => 0, "topFrac" => 0.0,
                                          "clippedSignalFrac" => 0.0)
        all_sat = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                    [mk_sat(0, true), mk_sat(1, true)]))
        s = Cecelia.qc_all_channels_saturated(all_sat)
        @test s.score == 1.0 && s.level == "warn"                # gate signal — worth surfacing
        @test s.subs[:nSaturated] == 2 && s.subs[:nChannels] == 2

        mixed = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                  [mk_sat(0, true), mk_sat(1, false)]))
        @test Cecelia.qc_all_channels_saturated(mixed).score == 0.5

        clean = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                  [mk_sat(0, false), mk_sat(1, false)]))
        cs = Cecelia.qc_all_channels_saturated(clean)
        @test cs.score == 0.0 && cs.level == "info"              # measured zero, not absent

        absent_sat = Cecelia.qc_all_channels_saturated(Dict{String,Any}())
        @test Cecelia.qc_score_absent(absent_sat)                # NaN, not 0.0

        # ── photon-limited: the worst channel's zeroFrac ────────────────────────────────────────
        mk_zf(i, zf) = merge(mk_sat(i, false), Dict{String,Any}("zeroFrac" => zf,
                                                                "signalFrac" => 1.0 - zf))
        with_zf = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                    [mk_zf(0, 0.50), mk_zf(1, 0.94), mk_zf(2, 0.87)]))
        pl = Cecelia.qc_photon_limited_frac(with_zf)
        @test pl.score == 0.94                                   # the sparsest
        @test pl.subs[:channel] == 1                             # ...its index

        # a channel without zeroFrac is skipped (pre-#811 field-absent), not treated as 0
        mixed_zf = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                     [mk_zf(0, 0.55), mk_sat(1, false), mk_zf(2, 0.93)]))
        @test Cecelia.qc_photon_limited_frac(mixed_zf).score == 0.93

        # no channel carries the field → absent (not zero)
        old_meta = Dict{String,Any}("saturation" => Dict{String,Any}("channels" =>
                                     [mk_sat(0, false), mk_sat(1, false)]))
        @test Cecelia.qc_score_absent(Cecelia.qc_photon_limited_frac(old_meta))

        # ── the roll-up returns all four in stable order for downstream diff ────────────────────
        meta = Dict{String,Any}("SizeT" => 100, "SizeZ" => 30,
                                "saturation" => Dict{String,Any}("channels" =>
                                                                  [mk_zf(0, 0.95)]))
        rs = Cecelia.compute_qc_scores(meta)
        @test length(rs) == 4
        @test [r.metric for r in rs] == ["axis.T_present", "axis.Z_present",
                                         "denoise.channel_saturated_frac",
                                         "smooth.photon_limited_frac"]
        @test rs[1].score == 1.0
        @test rs[2].score == 1.0
        @test rs[3].score == 0.0                                 # channel present, not saturated
        @test rs[4].score == 0.95

        # JSON3 round-trip on the meta — the real read path (persisted ccid comes back Symbol-keyed)
        rt   = JSON3.read(JSON3.write(meta))
        rtm  = Dict{String,Any}(String(k) => v for (k, v) in rt)
        @test Cecelia.qc_photon_limited_frac(rtm).score == 0.95

        # ── vault presence: system-level, appended only when the caller supplies the list ──────
        v_empty = Cecelia.qc_denoise_vault_model_present(String[])
        @test v_empty.score == 0.0 && v_empty.subs[:nModels] == 0
        v_some  = Cecelia.qc_denoise_vault_model_present(["m1", "m2"])
        @test v_some.score == 1.0 && v_some.subs[:nModels] == 2
        # compute_qc_scores appends only when vault_models !== nothing — pure meta path unchanged
        @test length(Cecelia.compute_qc_scores(meta)) == 4
        @test length(Cecelia.compute_qc_scores(meta; vault_models = String[])) == 5
        rs_v = Cecelia.compute_qc_scores(meta; vault_models = ["some-model"])
        @test rs_v[end].metric == "denoise.vault_model_present"
        @test rs_v[end].score  == 1.0
    end

    # CORRECTION_QC_PLAN.md §1 (rule table) + §3 (tie-break) + §5 (preset cards). Phase C of the
    # plan: pure meta → CorrectionPlan. No chain wiring, no plan.json write — the engine is a set
    # of pure functions that a Phase D consumer will mount on a `ChainTemplate`.
    @testset "correction_plan engine (§1 + §3 + §5)" begin
        # ── §5 presets: shape + registry ────────────────────────────────────────────────────────
        @test Set(Cecelia.preset_ids()) == Set([:resonance, :galvo, :spinning_disk, :deep_3d, :custom])
        res = Cecelia.preset_by_id(:resonance)
        @test res isa Cecelia.AcquisitionPreset
        @test res.validation_status == :unvalidated          # honest until a fixture exists
        @test ("cleanupImages.smooth", "spatialMethod") in res.hard_commitments
        @test res.params_by_task["cleanupImages.smooth"]["spatialMethod"] == "bilateral_vst"
        # C-Deep3D hard commitments (PR #818 compose): stackAlign + driftCorrect(driftPerPlane)
        d3 = Cecelia.preset_by_id(:deep_3d)
        @test ("cleanupImages.stackAlign",   "referenceMode") in d3.hard_commitments
        @test ("cleanupImages.driftCorrect", "driftPerPlane") in d3.hard_commitments
        @test d3.params_by_task["cleanupImages.driftCorrect"]["driftPerPlane"] === true
        # unknown card id → custom, keeps the engine total for stale plan.json refs
        @test Cecelia.preset_by_id(:not_a_card).id == :custom

        mk_scores(size_t, size_z; sat_frac = Cecelia.QC_SCORE_ABSENT,
                  photon_frac = Cecelia.QC_SCORE_ABSENT,
                  vault_present = 1.0) = Cecelia.QCResult[
            Cecelia.QCResult("axis.T_present", size_t > 1 ? 1.0 : 0.0),
            Cecelia.QCResult("axis.Z_present", size_z > 1 ? 1.0 : 0.0),
            Cecelia.QCResult("denoise.channel_saturated_frac", sat_frac),
            Cecelia.QCResult("smooth.photon_limited_frac",     photon_frac),
            Cecelia.QCResult("denoise.vault_model_present",    vault_present),
        ]

        # ── §1 rule 1: T-axis absent → driftCorrect + flowRegister excluded, even if the card
        #    seeded them. The exclusion row keeps the reason for the audit trail.
        r = Cecelia.apply_rules(mk_scores(1, 1), Cecelia.preset_by_id(:galvo))
        drift_excl = only([s for s in r.excluded if s.fun_name == "cleanupImages.driftCorrect"])
        @test drift_excl.exclusion_reason == "No T axis — drift correction not applicable"
        @test !any(s -> s.fun_name == "cleanupImages.driftCorrect", r.included)

        # ── §1 rule 2: T-axis present but card omits driftCorrect → auto-include (custom card).
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:custom))
        drift = only([s for s in r.included if s.fun_name == "cleanupImages.driftCorrect"])
        @test drift.source == :computed_qc

        # ── §1 rule 3: all channels saturated → denoise excluded, PR #796 refusal. Galvo does not
        #    seed denoise, so the exclusion is a pure engine emission with no card→excluded
        #    transition; a Resonance-card variant of this test is below.
        r = Cecelia.apply_rules(mk_scores(100, 1; sat_frac = 1.0), Cecelia.preset_by_id(:galvo))
        den = only([s for s in r.excluded if s.fun_name == "cleanupImages.denoise"])
        @test occursin("saturated", den.exclusion_reason)

        # partial saturation (< 1.0) does NOT exclude denoise — the gate is all-or-nothing
        r = Cecelia.apply_rules(mk_scores(100, 1; sat_frac = 0.5), Cecelia.preset_by_id(:galvo))
        @test !any(s -> s.fun_name == "cleanupImages.denoise", r.excluded)

        # sat NaN (probe never ran) → no exclusion (a signal-absent metric is NOT "0.0")
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:galvo))
        @test !any(s -> s.fun_name == "cleanupImages.denoise", r.excluded)

        # ── denoise vault gate — Resonance seeds denoise; the vault + saturation scores decide
        #    whether the seed survives. Chained so exactly one exclusion row is emitted.
        # vault present + not saturated → seeded from the card, no exclusion
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:resonance))
        den = only([s for s in r.included if s.fun_name == "cleanupImages.denoise"])
        @test den.source == :card
        @test isempty(den.params)                       # SUPPORT model picked at the task widget
        @test !any(s -> s.fun_name == "cleanupImages.denoise", r.excluded)

        # vault empty → excluded with the vault reason, seed dropped from `included`
        r = Cecelia.apply_rules(mk_scores(100, 1; vault_present = 0.0), Cecelia.preset_by_id(:resonance))
        vault_ex = only([s for s in r.excluded if s.fun_name == "cleanupImages.denoise"])
        @test vault_ex.exclusion_reason == "No trained denoise model in vault"
        @test !any(s -> s.fun_name == "cleanupImages.denoise", r.included)

        # vault empty + saturated → vault wins (single row, vault reason) — see the chained gate
        r = Cecelia.apply_rules(mk_scores(100, 1; vault_present = 0.0, sat_frac = 1.0),
                                Cecelia.preset_by_id(:resonance))
        rows = [s for s in r.excluded if s.fun_name == "cleanupImages.denoise"]
        @test length(rows) == 1
        @test rows[1].exclusion_reason == "No trained denoise model in vault"

        # vault present + saturated → falls through to the saturation reason
        r = Cecelia.apply_rules(mk_scores(100, 1; sat_frac = 1.0), Cecelia.preset_by_id(:resonance))
        sat_ex = only([s for s in r.excluded if s.fun_name == "cleanupImages.denoise"])
        @test occursin("saturated", sat_ex.exclusion_reason)

        # ── §5 C-Deep3D: stackAlign shipped on the card, referenceMode = middle.
        r = Cecelia.apply_rules(mk_scores(100, 30), Cecelia.preset_by_id(:deep_3d))
        sa = only([s for s in r.included if s.fun_name == "cleanupImages.stackAlign"])
        @test sa.params["referenceMode"] == "middle"
        @test sa.source == :card
        # driftCorrect also carries the card's per-plane opinion — PR #818 shipped `driftPerPlane` /
        # `driftZSmoothness`, and the two compose with stackAlign per the peer session's finding
        # (stackAlign = intra-stack per-frame anchor; driftPerPlane = inter-frame per-Z-plane rigid).
        dc = only([s for s in r.included if s.fun_name == "cleanupImages.driftCorrect"])
        @test dc.params["driftEstimator"]   == "multiLag"
        @test dc.params["driftPerPlane"]    === true
        @test dc.params["driftZSmoothness"] == 0.0
        @test dc.source == :card
        # ...and Z-axis absent excludes it regardless
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:deep_3d))
        @test any(s -> s.fun_name == "cleanupImages.stackAlign" &&
                       s.exclusion_reason == "No Z axis", r.excluded)

        # ── §1 order weights: drift (200) before smooth (300) before af (400)
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:resonance))
        order = [s.fun_name for s in r.included]
        drift_i  = findfirst(==("cleanupImages.driftCorrect"), order)
        smooth_i = findfirst(==("cleanupImages.smooth"), order)
        @test drift_i !== nothing && smooth_i !== nothing
        @test drift_i < smooth_i             # 200 < 300

        # ── §3 wizard tier > card tier ──────────────────────────────────────────────────────────
        # W2 = yes → driftEstimator switches from card's `multiLag` to `sitkRigid`
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:galvo),
                                Dict{Symbol,Any}(:W2 => :yes))
        drift = only([s for s in r.included if s.fun_name == "cleanupImages.driftCorrect"])
        @test drift.params["driftEstimator"] == "sitkRigid"
        @test drift.source == :wizard        # provenance stamps the wizard, not the card

        # W2 without T-axis → no autocreate (no driftCorrect step)
        r = Cecelia.apply_rules(mk_scores(1, 1), Cecelia.preset_by_id(:custom),
                                Dict{Symbol,Any}(:W2 => :yes))
        @test !any(s -> s.fun_name == "cleanupImages.driftCorrect", r.included)

        # W3 = yes → include flowRegister (card didn't ship it)
        r = Cecelia.apply_rules(mk_scores(100, 1), Cecelia.preset_by_id(:galvo),
                                Dict{Symbol,Any}(:W3 => :yes))
        fr = only([s for s in r.included if s.fun_name == "cleanupImages.flowRegister"])
        @test fr.source == :wizard

        # W5 = yes on a Z-present image → include stackAlign (custom card has no seed)
        r = Cecelia.apply_rules(mk_scores(100, 30), Cecelia.preset_by_id(:custom),
                                Dict{Symbol,Any}(:W5 => :yes))
        @test any(s -> s.fun_name == "cleanupImages.stackAlign" && s.source == :wizard, r.included)

        # ── recommend_plan(meta) — the persist-facing entrypoint ───────────────────────────────
        meta = Dict{String,Any}("SizeT" => 100, "SizeZ" => 30)
        plan = Cecelia.recommend_plan(meta; image_uid = "img-abc", card_id = :deep_3d)
        @test plan isa Cecelia.CorrectionPlan
        @test plan.image_uid == "img-abc"
        @test plan.preset_id == :deep_3d
        @test length(plan.qc_scores) == 4                    # score snapshot preserved
        # Deep3D includes both stackAlign (card) and driftCorrect (card + T-axis satisfies)
        fns = [s.fun_name for s in plan.included]
        @test "cleanupImages.stackAlign" in fns
        @test "cleanupImages.driftCorrect" in fns
    end

    # Phase D of docs/todo/CORRECTION_QC_PLAN.md — plan.json sidecar. Provenance is deliberate:
    # `ceceliaVersion` invalidates every plan when the code that would run it changes; the
    # `saturationFingerprint` catches a re-import that shifted per-channel saturation numbers
    # (which a version stamp cannot). No per-step writer versions — cecelia ships as one package.
    @testset "correction_plan persistence (§8 sidecar + provenance)" begin
        # ── saturation_fingerprint: stable across a JSON3 round-trip, changes with content ───────
        sat_chan(zf) = Dict{String,Any}("index" => 0, "saturated" => false, "topValue" => 500,
                                        "topCount" => 0, "topFrac" => 0.0,
                                        "clippedSignalFrac" => 0.0,
                                        "zeroFrac" => zf, "signalFrac" => 1.0 - zf)
        meta_a = Dict{String,Any}("SizeT" => 100, "SizeZ" => 30,
                                   "saturation" => Dict{String,Any}("channels" => [sat_chan(0.85)]))
        fp_a   = Cecelia.saturation_fingerprint(meta_a)
        @test !isempty(fp_a) && length(fp_a) == 64          # hex sha256

        # round-trip through JSON3 — Int/Float coercion and Symbol-vs-String keys must not shift it
        raw = JSON3.read(JSON3.write(meta_a))
        rt  = Dict{String,Any}(String(k) => v for (k, v) in raw)
        @test Cecelia.saturation_fingerprint(rt) == fp_a    # THE invariant this exists to enforce

        # different content → different fingerprint
        meta_b = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [sat_chan(0.50)]))
        @test Cecelia.saturation_fingerprint(meta_b) != fp_a
        # missing saturation → empty string sentinel (caller distinguishes "both unknown")
        @test Cecelia.saturation_fingerprint(Dict{String,Any}()) == ""

        # ── plan.json roundtrip on a real CciaImage ───────────────────────────────────────────
        proj = create_project!(name = "plan-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "im")
        img.meta = meta_a
        save!(img)                                          # writes ccid.json with meta

        plan = Cecelia.recommend_plan(img; card_id = :resonance,
                                            wizard = Dict{Symbol,Any}(:W2 => :yes))
        @test plan.image_uid == img.uid
        @test plan.preset_id == :resonance
        @test plan.cecelia_version == cecelia_version()
        @test plan.saturation_fingerprint == fp_a

        path = Cecelia.save_plan(img, plan)
        @test isfile(path)
        @test basename(path) == "plan.json"

        loaded = Cecelia.load_plan(img)
        @test loaded !== nothing
        @test loaded.image_uid == plan.image_uid
        @test loaded.preset_id == plan.preset_id
        @test loaded.wizard_answers == plan.wizard_answers    # Symbol values round-trip via _wizard_v_from_json
        @test loaded.cecelia_version == plan.cecelia_version
        @test loaded.saturation_fingerprint == plan.saturation_fingerprint
        @test [s.fun_name for s in loaded.included] == [s.fun_name for s in plan.included]
        @test [s.source   for s in loaded.included] == [s.source   for s in plan.included]
        @test length(loaded.qc_scores) == length(plan.qc_scores)

        # NaN score (QC_SCORE_ABSENT) survives the JSON null bridge — the case that isn't hit by
        # `meta_a` (which has a saturation dict, so no absent scores) needs its own image:
        img2 = add_image!(s; name = "im-no-sat")
        img2.meta = Dict{String,Any}("SizeT" => 1, "SizeZ" => 1)
        save!(img2)
        p2 = Cecelia.recommend_plan(img2)
        r  = only([x for x in p2.qc_scores if x.metric == "denoise.channel_saturated_frac"])
        @test Cecelia.qc_score_absent(r)                      # no saturation field → absent
        Cecelia.save_plan(img2, p2)
        p2back = Cecelia.load_plan(img2)
        r2 = only([x for x in p2back.qc_scores if x.metric == "denoise.channel_saturated_frac"])
        @test Cecelia.qc_score_absent(r2)                     # absent survives roundtrip

        # ── missing file → nothing ────────────────────────────────────────────────────────────
        img3 = add_image!(s; name = "im-no-plan")
        @test Cecelia.load_plan(img3) === nothing

        # ── unknown planVersion → nothing (caller re-plans, never trusts a schema drift) ──────
        path3 = joinpath(img3._dir, "plan.json")
        open(path3, "w") do io
            JSON3.pretty(io, Dict{String,Any}("planVersion" => 999,
                                              "ceceliaVersion" => "9.9.9",
                                              "imageUid" => img3.uid,
                                              "presetId" => "custom",
                                              "wizardAnswers" => Dict{String,Any}(),
                                              "saturationFingerprint" => "",
                                              "included" => [], "excluded" => [], "qcScores" => []))
        end
        @test Cecelia.load_plan(img3) === nothing

        # ── malformed JSON → nothing (never throws to the caller) ──────────────────────────────
        open(path3, "w") do io; write(io, "{not json"); end
        @test Cecelia.load_plan(img3) === nothing

        rm(proj.root; recursive = true)
    end

    # Phase E of docs/todo/CORRECTION_QC_PLAN.md — chain mount + card recommender. The plan is now
    # an execution surface: `plan_to_chain_template` yields a `ChainTemplate` the executor accepts
    # (validate_chain_template passes). `recommend_card` picks a card from wizard+scores so callers
    # of recommend_plan can leave `card_id` implicit.
    @testset "correction_plan chain mount + recommender (§E)" begin
        # ── recommend_card: wizard > everything, then W1 → card, else :custom ────────────────
        empty_scores = Cecelia.QCResult[]
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}()) == :custom
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}(:W1 => :resonance)) == :resonance
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}(:W1 => :galvo))     == :galvo
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}(:W1 => :spinning_disk)) == :spinning_disk
        # W5 wins over W1 — an intra-stack shear is what defines Deep-3D
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}(:W1 => :galvo, :W5 => :yes)) == :deep_3d
        # unknown W1 or missing → fallback custom
        @test Cecelia.recommend_card(empty_scores, Dict{Symbol,Any}(:W1 => :other)) == :custom

        # ── recommend_plan with no card_id auto-picks via recommend_card ─────────────────────
        meta = Dict{String,Any}("SizeT" => 100, "SizeZ" => 1)
        p_auto = Cecelia.recommend_plan(meta; wizard = Dict{Symbol,Any}(:W1 => :resonance))
        @test p_auto.preset_id == :resonance
        p_custom = Cecelia.recommend_plan(meta)   # no card, no wizard → :custom
        @test p_custom.preset_id == :custom

        # ── plan_to_chain_template: shape + linear edges ─────────────────────────────────────
        p = Cecelia.recommend_plan(meta; card_id = :resonance)
        tmpl = Cecelia.plan_to_chain_template(p; name = "test-mount")
        @test tmpl.name == "test-mount"
        @test length(tmpl.nodes) == length(p.included)
        @test [n.fn for n in tmpl.nodes] == [s.fun_name for s in p.included]
        # Node ids are short-form fun_names — stable across re-plans. Resonance seeds
        # drift (200) + smooth (300) + denoise (400). Denoise's vault gate defaults `1.0` for a
        # pure-meta plan (no vault_models argument), so the seed survives without exclusion —
        # the img-variant of recommend_plan is where a real vault enumeration decides.
        @test [n.id for n in tmpl.nodes] == ["driftCorrect", "smooth", "denoise"]
        @test [(e.from, e.to) for e in tmpl.edges] ==
              [("driftCorrect", "smooth"), ("smooth", "denoise")]
        # Excluded steps are NOT in the template — the audit trail is a plan concept, not chain
        @test !any(n -> n.fn == "cleanupImages.stackAlign", tmpl.nodes)   # Z-absent → excluded

        # ── default name derives from image_uid; re-mount replaces canonically ───────────────
        p2 = Cecelia.recommend_plan(meta; image_uid = "img-42", card_id = :resonance)
        tmpl2 = Cecelia.plan_to_chain_template(p2)
        @test tmpl2.name == "correction-plan-img-42"

        # ── validate_chain_template accepts the mount (the executor would run it) ────────────
        proj = create_project!(name = "mount-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "im"); img.meta = meta; save!(img)
        p_real = Cecelia.recommend_plan(img; card_id = :resonance)
        tmpl_real = Cecelia.plan_to_chain_template(p_real)
        Cecelia.validate_chain_template(tmpl_real)   # throws on failure — no @test needed
        # Deep3D on a Z+T image — stackAlign shipped as card + card ships it before drift
        img.meta = Dict{String,Any}("SizeT" => 100, "SizeZ" => 30); save!(img)
        p_d3 = Cecelia.recommend_plan(img; card_id = :deep_3d)
        t_d3 = Cecelia.plan_to_chain_template(p_d3)
        @test "cleanupImages.stackAlign" in [n.fn for n in t_d3.nodes]
        @test "cleanupImages.driftCorrect" in [n.fn for n in t_d3.nodes]
        # bucket order: stackAlign (100) before driftCorrect (200)
        sa_i = findfirst(n -> n.fn == "cleanupImages.stackAlign", t_d3.nodes)
        dr_i = findfirst(n -> n.fn == "cleanupImages.driftCorrect", t_d3.nodes)
        @test sa_i < dr_i
        Cecelia.validate_chain_template(t_d3)

        # ── empty plan (nothing to run) → empty template, no edges ───────────────────────────
        p_empty = Cecelia.recommend_plan(Dict{String,Any}(); card_id = :custom)
        t_empty = Cecelia.plan_to_chain_template(p_empty)
        @test isempty(t_empty.nodes) && isempty(t_empty.edges)

        rm(proj.root; recursive = true)
    end

    # Pyramid depth QC — synthesised on disk (JSON-only, no pixels) because the function reads the

    # Pyramid depth QC — synthesised on disk (JSON-only, no pixels) because the function reads the
    # multiscales metadata and the L0 `.zarray`, not the array itself. A flat store here rather than
    # a bf2raw wrapper, so the same test exercises `series_base`'s flat branch.
    @testset "pyramid_qc_findings + pyramid_metrics" begin
        function mkzarr(dir; datasets, l0_shape, l0_chunks, deepest_shape, deepest_chunks)
            mkpath(dir)
            # root `.zgroup` + `.zattrs` with multiscales listing every dataset path
            write(joinpath(dir, ".zgroup"), JSON3.write(Dict("zarr_format" => 2)))
            ms = Dict("multiscales" => [Dict(
                "version"  => "0.4",
                "axes"     => [Dict("name"=>"t","type"=>"time"),   Dict("name"=>"c","type"=>"channel"),
                               Dict("name"=>"z","type"=>"space"),  Dict("name"=>"y","type"=>"space"),
                               Dict("name"=>"x","type"=>"space")],
                "datasets" => [Dict("path" => string(i)) for i in 0:datasets-1],
            )])
            write(joinpath(dir, ".zattrs"), JSON3.write(ms))
            # only the FIRST and LAST datasets get a `.zarray` — those are the two `pyramid_layout` reads
            for (i, sh, ch) in ((0, l0_shape, l0_chunks),
                                (datasets - 1, deepest_shape, deepest_chunks))
                sub = joinpath(dir, string(i)); mkpath(sub)
                write(joinpath(sub, ".zarray"), JSON3.write(Dict(
                    "zarr_format" => 2, "shape" => sh, "chunks" => ch,
                    "dtype" => ">u2", "dimension_separator" => "/",
                    "compressor" => Dict("id"=>"blosc","cname"=>"zstd","clevel"=>3,"shuffle"=>1,"blocksize"=>0),
                    "filters" => nothing, "fill_value" => 0, "order" => "C")))
            end
        end

        # Deep enough: bf2raw-style, deepest 1×1. No finding, metrics banked.
        deep = mktempdir()
        mkzarr(deep;
               datasets = 6,
               l0_shape      = [1, 25, 1, 16898, 20329],
               l0_chunks     = [1, 1, 1, 1024, 1024],
               deepest_shape = [1, 25, 1, 528, 635],
               deepest_chunks= [1, 25, 1, 528, 635])   # capped-to-frame
        @test isempty(Cecelia.pyramid_qc_findings(deep))
        pmm = Cecelia.pyramid_metrics(deep)
        @test pmm["nPyramidLevels"] == 6
        @test pmm["deepestGridTiles"] == 1

        # Too shallow: same L0 but only 2 levels. Deepest = 8449×10164, chunk 1024 → 9×10 = 90 tiles.
        shallow = mktempdir()
        mkzarr(shallow;
               datasets = 2,
               l0_shape      = [1, 25, 1, 16898, 20329],
               l0_chunks     = [1, 1, 1, 1024, 1024],
               deepest_shape = [1, 25, 1, 8449, 10164],
               deepest_chunks= [1, 1, 1, 1024, 1024])
        fs = Cecelia.pyramid_qc_findings(shallow)
        @test length(fs) == 1
        @test fs[1]["code"]  == "import.pyramid_too_shallow"
        @test fs[1]["level"] == "warn"     # advisory, never a gate — the store still works
        @test fs[1]["detail"]["nLevels"]            == 2
        @test fs[1]["detail"]["deepestGridWidth"]   == 10
        @test fs[1]["detail"]["deepestGridHeight"]  == 9
        @test fs[1]["detail"]["deepestGridTiles"]   == 90
        # ceil(log2(20329/1024)) + 1 = 5 + 1 = 6 — enough to collapse to one tile
        @test fs[1]["detail"]["suggestedLevels"]    == 6
        # the placeholders got substituted — `qc_text` would throw otherwise (see the saturation test).
        # "9×10" reads Y×X, matching how the modal renders shape as shape[-2]×shape[-1].
        @test occursin("9×10", fs[1]["short"])
        pms = Cecelia.pyramid_metrics(shallow)
        @test pms["nPyramidLevels"]   == 2
        @test pms["deepestGridTiles"] == 90

        # Missing store / no multiscales → nothing to say. Same posture as saturation when the
        # check didn't run: the finding list is empty and the metrics function returns `nothing`.
        gone = mktempdir(); rm(gone; recursive = true)
        @test isempty(Cecelia.pyramid_qc_findings(gone))
        @test Cecelia.pyramid_metrics(gone) === nothing

        # Small image where the default is fine: L0 already fits in one chunk. No finding, and
        # `suggestedLevels` collapses to the current level count (no re-import would help).
        small = mktempdir()
        mkzarr(small;
               datasets = 1,
               l0_shape      = [1, 2, 1, 256, 256],
               l0_chunks     = [1, 1, 1, 1024, 1024],
               deepest_shape = [1, 2, 1, 256, 256],
               deepest_chunks= [1, 1, 1, 1024, 1024])
        @test isempty(Cecelia.pyramid_qc_findings(small))
        @test Cecelia.pyramid_metrics(small)["nPyramidLevels"] == 1
    end

    # Julia and Python each carry the compressor table — a bioformats2raw command line cannot read a
    # Python constant, and the API serves the list to Settings. Same arrangement as the calibration
    # writers (docs/OBJECTMODEL.md -> *Calibration - three copies, one stamp*): two copies, one contract test.
    @testset "image compressor: the two tables agree" begin
        py = read(joinpath(@__DIR__, "..", "..", "..", "python", "cecelia", "utils", "zarr_utils.py"), String)

        # every Julia choice exists in the Python dict with the SAME cname/clevel/shuffle
        for c in Cecelia.IMAGE_COMPRESSOR_CHOICES
            m = match(Regex("'" * c.name * "': *dict\\(cname='(\\w+)', *clevel=(\\d+), *shuffle='(\\w+)'\\)"), py)
            @test !isnothing(m)
            isnothing(m) && continue
            @test m.captures[1] == c.cname
            @test parse(Int, m.captures[2]) == c.clevel
            @test (m.captures[3] == "shuffle") == c.shuffle
        end

        # ...and neither side has a choice the other lacks
        py_names = Set(String(m.captures[1]) for m in
                       eachmatch(r"'([\w-]+)': +dict\(cname=", py))
        @test py_names == Set(c.name for c in Cecelia.IMAGE_COMPRESSOR_CHOICES)

        # the defaults match, on both sides
        @test occursin("IMAGE_COMPRESSOR_DEFAULT = '$(Cecelia.IMAGE_COMPRESSOR_DEFAULT)'", py)
        @test Cecelia.image_compressor() in [c.name for c in Cecelia.IMAGE_COMPRESSOR_CHOICES]

        # The blosc `shuffle` property is spelled DIFFERENTLY per bioformats2raw version, and each
        # version hard-fails on the other's spelling (0.12.0 swapped jzarr → zarr-java). Detected from
        # the bundled jar; asserted here against synthetic lib dirs so this is hermetic — CI has no
        # bioformats2raw install at all, and that must resolve to the current spelling, not error.
        mktempdir() do d
            legacy = joinpath(d, "legacy"); mkpath(legacy)
            touch(joinpath(legacy, "jzarr-0.4.2.jar"))
            @test Cecelia.bf2raw_shuffle_values(legacy) == ("1", "0")

            modern = joinpath(d, "modern"); mkpath(modern)
            touch(joinpath(modern, "zarr-java-0.1.3.jar"))
            @test Cecelia.bf2raw_shuffle_values(modern) == ("shuffle", "noshuffle")

            # neither jar, and a missing dir → the current spelling (a wrong guess fails loudly)
            empty_dir = joinpath(d, "empty"); mkpath(empty_dir)
            @test Cecelia.bf2raw_shuffle_values(empty_dir) == ("shuffle", "noshuffle")
            @test Cecelia.bf2raw_shuffle_values(joinpath(d, "absent")) == ("shuffle", "noshuffle")
        end
        # NOT the literal "1"/"0": that is the legacy spelling, and hardcoding it here is what would
        # hide the incompatibility. Assert against whatever THIS install wants.
        shuf_on, shuf_off = Cecelia.bf2raw_shuffle_values(Cecelia._bf2raw_lib_dir())
        flags = Cecelia.bf2raw_compression_flags("zstd-shuffle")
        @test flags[1:2] == ["--compression", "blosc"]
        props = Dict(split(flags[i], "=")[1] => split(flags[i], "=")[2] for i in 4:2:length(flags))
        @test props == Dict("cname" => "zstd", "clevel" => "3", "shuffle" => shuf_on)
        @test Dict(split(f, "=")[1] => split(f, "=")[2]
                   for f in Cecelia.bf2raw_compression_flags("zstd")[4:2:end])["shuffle"] == shuf_off
        # `byteshuffle` is the alias 0.12's README documents for byte shuffle, and it is BROKEN
        # upstream (null enum → NPE → every chunk write fails). It must never be emitted.
        @test !any(occursin("byteshuffle", f) for f in flags)

        # an unknown name falls back rather than erroring - a typo in custom.toml must not fail a
        # multi-hour import
        @test Cecelia.bf2raw_compression_flags("nope") ==
              Cecelia.bf2raw_compression_flags(Cecelia.IMAGE_COMPRESSOR_DEFAULT)
        @test_throws ArgumentError Cecelia.set_image_compressor!("nope")
    end

    # Two tables on one Settings page, each varying what the other pins: the compressor rows were all
    # measured in ONE layout, the layout rows all with ONE codec. Neither set of sizes is comparable to
    # the other's without that, so each caption must name the variable it held fixed. Asserted because a
    # caption is exactly the kind of string that gets shortened later by someone who reads it as prose.
    @testset "measured-on captions name the other table's variable" begin
        cmp_cap = Cecelia.IMAGE_COMPRESSOR_MEASURED_ON
        lay_cap = Cecelia.STORE_LAYOUT_MEASURED_ON

        # the compressor was measured in one LAYOUT: format, chunk-key style, and chunk shape
        @test occursin("zarr v", cmp_cap)
        @test occursin("keys", cmp_cap)
        @test occursin("chunks", cmp_cap)

        # ...and the layouts with one CODEC — named, and actually the default the other table serves
        # (derived, not spelled out, so re-measuring under a new default has to update the caption)
        default_cname = first(c.cname for c in Cecelia.IMAGE_COMPRESSOR_CHOICES
                              if c.name == Cecelia.IMAGE_COMPRESSOR_DEFAULT)
        @test occursin(default_cname, lay_cap)

        # both stay one short line — this renders as a field hint, not a paragraph (docs/UI.md)
        for cap in (cmp_cap, lay_cap)
            @test !occursin("\n", cap)
            @test length(cap) <= 90
        end
    end

    @testset "count metrics" begin
        # pure: distinct tracks, mean cells/track, tracked-cell total; untracked = missing/NaN/≤0
        nt, ml, ntc = track_count_metrics([1, 1, 1, 2, 2, 0, -1, NaN, missing, 3])
        @test nt == 3                       # tracks 1, 2, 3
        @test ntc == 6                      # 3 + 2 + 1 cells tracked
        @test ml ≈ 2.0                      # 6 cells / 3 tracks

        # no tracks at all → zeros (drives the "No tracks formed" advisory)
        @test track_count_metrics([0, NaN, missing]) == (0, 0.0, 0)
        @test track_count_metrics(Float64[]) == (0, 0.0, 0)

        # floats round to the nearest track id
        n2, _, c2 = track_count_metrics([1.0, 1.0, 2.0])
        @test (n2, c2) == (2, 3)

        # segment counts → findings: 0 base cells warns; any base count is clean
        f0, p0 = Cecelia.segment_qc_findings(Dict("base" => 0))
        @test p0 == 0 && length(f0) == 1 && f0[1]["code"] == "segment.no_cells"
        fN, pN = Cecelia.segment_qc_findings(Dict("base" => 812, "nuc" => 790))
        @test pN == 812 && isempty(fN)
        # no explicit "base" key → primary falls back to the sole type's count
        _, pf = Cecelia.segment_qc_findings(Dict("nuc" => 5))
        @test pf == 5

        # metadata calibration findings (port of the old frontend fieldIssues) — codes + field
        codes(fs) = [f["code"] for f in fs]; fields(fs) = [f["detail"]["field"] for f in fs]
        # clean 3D timelapse with units → nothing
        @test isempty(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"SizeT"=>5,
            "PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0,"PhysicalSizeUnit"=>"micron",
            "TimeIncrement"=>30.0,"TimeIncrementUnit"=>"second")))
        # NO xy pixel size → pixel_size_unknown, per missing axis, and FIRST (it blocks every task
        # that measures in microns, and `metadata_warning` shows the first finding). This case had no
        # finding at all before 2026-08-21: x/y were read only for their unit and their ratio to z.
        pu = Cecelia.metadata_qc_findings(Dict("SizeT"=>1,"SizeZ"=>1))
        @test codes(pu) == ["metadata.pixel_size_unknown", "metadata.pixel_size_unknown"]
        @test fields(pu) == ["x","y"]
        # a zero is not a measurement
        @test codes(Cecelia.metadata_qc_findings(Dict("PhysicalSizeX"=>0,"PhysicalSizeY"=>0.5,
            "PhysicalSizeUnit"=>"micron"))) == ["metadata.pixel_size_unknown"]
        # z stack, no z spacing → z_spacing_unknown (y is missing here too, so it is flagged as well)
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeUnit"=>"micron"))) ==
              ["metadata.pixel_size_unknown", "metadata.z_spacing_unknown"]
        # auto-corrected z (PhysicalSizeZ_raw marker) → z_spacing_corrected
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,
            "PhysicalSizeZ"=>2.0,"PhysicalSizeUnit"=>"micron","PhysicalSizeZ_raw"=>99.0))) ==
              ["metadata.z_spacing_corrected"]
        # unusual z:xy ratio (100:1 > 50) → z_spacing_unusual
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>1.0,"PhysicalSizeY"=>1.0,
            "PhysicalSizeZ"=>100.0,"PhysicalSizeUnit"=>"micron"))) == ["metadata.z_spacing_unusual"]
        # timelapse, no interval → frame_interval_unknown; string values coerce
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>"8","PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,
            "PhysicalSizeUnit"=>"micron"))) == ["metadata.frame_interval_unknown"]
        # interval present, no unit → frame_interval_no_unit
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>8,"TimeIncrement"=>30.0,
            "PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeUnit"=>"micron"))) ==
              ["metadata.frame_interval_no_unit"]
        # no spatial unit, x+y+z present (2D-safe: SizeZ=1 so no z-spacing case) → three no-unit (x,y,z)
        fu = Cecelia.metadata_qc_findings(Dict("PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0))
        @test all(==("metadata.pixel_size_no_unit"), codes(fu)) && fields(fu) == ["x","y","z"]
        # z-spacing case suppresses the z no-unit dup (z already flagged)
        fz = Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5))
        @test codes(fz) == ["metadata.z_spacing_unknown","metadata.pixel_size_no_unit","metadata.pixel_size_no_unit"]
        @test !("metadata.pixel_size_unknown" in codes(fz))    # a size IS recorded; only its unit is not
        @test fields(fz) == ["z","x","y"]     # no second z entry

        # severity symbols: shape-distinct (✅/⚠️/❌), NOT same-shape circles; unknown → ""
        @test Cecelia.severity_symbol("ok")   == "✅"
        @test Cecelia.severity_symbol("warn") == "⚠️"
        @test Cecelia.severity_symbol("fail") == "❌"
        @test length(Set(values(Cecelia.SEVERITY_SYMBOLS))) == 3   # distinct glyphs
        @test !("🟢" in values(Cecelia.SEVERITY_SYMBOLS))          # not the colour-blind trap
        @test Cecelia.severity_symbol("bogus") == ""

        # clustering findings (set-scope: total = run clusters; the rest = one image's slice)
        # run collapsed to 1 cluster → warn (regardless of the per-image numbers)
        c1 = Cecelia.cluster_qc_findings(1, 500, 1, 1.0)
        @test length(c1) == 1 && c1[1]["code"] == "clustering.single_cluster" && c1[1]["level"] == "warn"
        # image's cells all in one cluster while the run found several → warn (batch outlier)
        c2 = Cecelia.cluster_qc_findings(6, 400, 1, 1.0)
        @test length(c2) == 1 && c2[1]["code"] == "clustering.image_one_cluster" && c2[1]["level"] == "warn"
        # one cluster dominates this image (≥90%) but not all → info
        c3 = Cecelia.cluster_qc_findings(6, 400, 3, 0.95; unit = "tracks")
        @test length(c3) == 1 && c3[1]["code"] == "clustering.dominant_cluster" && c3[1]["level"] == "info"
        @test occursin("tracks", c3[1]["short"])
        # a healthy spread flags nothing; an empty image (n=0) never flags
        @test isempty(Cecelia.cluster_qc_findings(6, 400, 5, 0.4))
        @test isempty(Cecelia.cluster_qc_findings(6, 0, 0, 0.0))

        # category distribution metrics (HMM states/transitions): skip NaN/missing/nothing
        m1 = Cecelia.category_dist_metrics([1.0, 1.0, 2.0, NaN, missing])
        @test m1.n == 3 && m1.n_distinct == 2 && m1.dominant_frac ≈ 2/3
        ms = Cecelia.category_dist_metrics(["1_2", "1_2", nothing, "2_1"])
        @test ms.n == 3 && ms.n_distinct == 2
        @test Cecelia.category_dist_metrics(Any[NaN, missing, nothing]).n == 0

        # HMM state findings: no decode → warn; single state → warn; ≥95% one state → info; else none
        @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(Float64[]))[1]["code"] == "hmm.no_states_decoded"
        @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 1.0]))[1]["code"] == "hmm.single_state"
        fd = Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(vcat(fill(1.0, 96), fill(2.0, 4))))
        @test fd[1]["code"] == "hmm.dominant_state" && fd[1]["level"] == "info"
        @test isempty(Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 2.0, 2.0])))
        # HMM transitions: only the no-transitions case flags
        @test Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(Any[nothing]))[1]["code"] == "hmm.no_transitions"
        @test isempty(Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(["1_2", "2_1"])))

        # track measures: auto + low-confidence motion dims → warn; confident/user-set → none
        tm = Cecelia.track_measures_qc_findings(120, "auto", 2, 2, "low", "z ambiguous")
        @test length(tm) == 1 && tm[1]["code"] == "tracking.motion_dims_uncertain" && tm[1]["level"] == "warn"
        @test isempty(Cecelia.track_measures_qc_findings(120, "auto", 3, 3, "high", "clear"))
        @test isempty(Cecelia.track_measures_qc_findings(120, "3D", 3, 2, "low", "user forced"))  # user-set: no flag

        # against the tracked fixture: metrics agree with an independent count of track_id
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "track_count_metrics fixture (missing)"
        else
            tids = (label_props(h5) |> select_cols(["track_id"]) |> as_df).track_id
            valid = Int.(filter(t -> !isnan(t) && t > 0, tids))
            fnt, fml, fntc = track_count_metrics(tids)
            @test fnt == length(unique(valid))
            @test fntc == length(valid)
            @test fml ≈ length(valid) / length(unique(valid))
        end
    end

    @testset "cohort outliers (robust median/MAD)" begin
        # MAD>0 regime: a clear outlier flags even at n=3 (mean/SD couldn't — max |z| there = 1.15)
        r3 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0))
        @test r3.n == 3 && haskey(r3.outliers, "c") && !haskey(r3.outliers, "a")
        @test r3.median == 800.0 && r3.mad > 0
        @test r3.outliers["c"]["z"] |> abs > 3.5             # modified-z carried for the flag

        # MAD==0 regime — THE qcProbe case [800,800,100]: two identical baselines → MAD 0 (and the
        # old mean-abs-dev fallback missed it). Relative-departure rule flags the 100, not the 800s.
        r0 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>100.0))
        @test r0.mad == 0.0 && haskey(r0.outliers, "c") && !haskey(r0.outliers, "a")
        @test r0.outliers["c"]["relDev"] >= 0.5             # relative departure carried for the flag
        # …but a near-identical value (801 vs 800) is NOT flagged (no magnitude → no false positive)
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>801.0)).outliers)

        # too few to judge (no cohort) → no outliers even with a wild value
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>5.0,"b"=>500.0)).outliers)
        # all identical → nothing flagged (no false positive)
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0)).outliers)
        # a lower explicit threshold is honoured in the MAD>0 regime (more sensitive)
        @test haskey(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0), 1.0).outliers, "c")

        # per-image finding from an outlier entry — direction from value vs median; carries detail
        cf = Cecelia._cohort_finding("nCells", Dict{String,Any}("value"=>100.0,"z"=>-5.2), 800.0)
        @test cf["code"] == "cohort.nCells" && cf["level"] == "warn"
        @test cf["detail"]["metric"] == "nCells" && cf["detail"]["value"] == 100.0 && cf["detail"]["z"] == -5.2
        @test occursin("below", cf["long"])
        cf2 = Cecelia._cohort_finding("nTracks", Dict{String,Any}("value"=>900.0,"relDev"=>0.8), 500.0)
        @test occursin("above", cf2["long"]) && cf2["detail"]["relDev"] == 0.8

        # lab-log summary lines + has-outliers predicate (drives whether the check logs at all)
        clean = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
            "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,"outliers"=>Dict{String,Any}())))
        @test !cohort_has_outliers(clean)
        cl = cohort_qc_summary_lines(clean)
        @test length(cl) == 1 && occursin("all 10", cl[1]) && startswith(cl[1], "✅")
        flagged = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
            "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,
                "outliers"=>Dict{String,Any}("j"=>Dict{String,Any}("value"=>100.0,"z"=>-5.2)))))
        @test cohort_has_outliers(flagged)
        fl = cohort_qc_summary_lines(flagged)
        @test startswith(fl[1], "⚠️") && any(l -> occursin("j", l) && occursin("100", l), fl)
    end

    @testset "cohort round-trip (banked metrics → set sidecar)" begin
        set = CciaSet(; dir = mktempdir())
        counts = Dict("a"=>800,"b"=>810,"c"=>790,"d"=>805,"e"=>795,
                      "f"=>808,"g"=>803,"h"=>797,"i"=>802,"j"=>100)
        for (uid, n) in counts
            img = CciaImage(; uid = uid, dir = mktempdir())
            write_qc(img, "segment.measureLabels", "default", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nCells" => n))
            push!(set._images, img); push!(set.image_uids, uid)
        end
        # READ-ONLY path (GET): computes outliers but writes NOTHING (no sidecar, no per-image)
        ro = cohort_qc_for(set, "segment.measureLabels", "default")
        @test haskey(ro["metrics"]["nCells"]["outliers"], "j")
        @test !isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
        @test read_qc(set._images[findfirst(i -> i.uid == "j", set._images)],
                      "cohort.segment.measureLabels", "default") === nothing
        # PERSIST path (the check action): sidecar + per-image findings
        doc = cohort_qc_for!(set, "segment.measureLabels", "default")
        m = doc["metrics"]["nCells"]
        @test m["n"] == 10 && haskey(m["outliers"], "j") && !haskey(m["outliers"], "a")
        @test doc["nIncluded"] == 10
        # sidecar written + re-readable
        @test isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
        @test read_cohort_qc(set, "segment.measureLabels", "default")["nIncluded"] == 10
        @test haskey(read_all_cohort_qc(set), "segment.measureLabels/default")
        # per-image write-back: the outlier (j) gets a cohort finding ON the image. A normal image
        # (a) is NOT written — no empty placeholder (that would put an empty cohort.* doc on every
        # image on every check). Under the cohort.* namespace, merged by read_all_qc.
        byid = Dict(i.uid => i for i in set._images)
        fj = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
        @test fj !== nothing && !isempty(fj["findings"])
        @test fj["findings"][1]["code"] == "cohort.nCells" && fj["findings"][1]["level"] == "warn"
        @test occursin("below", fj["findings"][1]["long"])         # 100 < median 800
        @test read_qc(byid["a"], "cohort.segment.measureLabels", "default") === nothing
        @test haskey(read_all_qc(byid["j"]), "cohort.segment.measureLabels/default")
        # clear-stale: bump the outlier back into range and re-check → j's prior cohort doc is
        # CLEARED (written empty, un-flags), not left as a stale warning
        byid["j"].included = true
        write_qc(byid["j"], "segment.measureLabels", "default", Dict{String,Any}[];
                 metrics = Dict{String,Any}("nCells" => 801))
        cohort_qc_for!(set, "segment.measureLabels", "default")
        fj2 = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
        @test fj2 !== nothing && isempty(fj2["findings"])          # existing doc cleared, not deleted
        # excluded images drop out of the cohort
        set._images[1].included = false                      # exclude one
        @test cohort_qc_for!(set, "segment.measureLabels", "default")["nIncluded"] == 9
        # unknown fun errors (not a metric producer)
        @test_throws ErrorException cohort_qc_for!(set, "not.aTask", "default")
    end

    @testset "cohort value_name discovery (per label set)" begin
        set = CciaSet(; dir = mktempdir())
        # clustering banks per label set: T-tracks tight, B-tracks with one sparse image (c=9)
        for (uid, nT, nB) in [("a", 40, 22), ("b", 39, 24), ("c", 41, 9)]
            img = CciaImage(; uid = uid, dir = mktempdir())
            write_qc(img, "clustTracks.cluster", "T", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks"=>nT, "nClusters"=>4, "largestClusterFrac"=>0.4))
            write_qc(img, "clustTracks.cluster", "B", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks"=>nB, "nClusters"=>3, "largestClusterFrac"=>0.5))
            push!(set._images, img); push!(set.image_uids, uid)
        end
        # discovers the banked label sets (sorted), empty for a fun that banked nothing
        @test cohort_value_names(set, "clustTracks.cluster") == ["B", "T"]
        @test cohort_value_names(set, "segment.cellpose") == String[]
        # per-value_name cohorts: T and B are SEPARATE cohorts
        allc = cohort_qc_for_all(set, "clustTracks.cluster")
        @test Set(keys(allc)) == Set(["B", "T"])
        @test allc["T"]["valueName"] == "T" && allc["B"]["valueName"] == "B"
        # the sparse B image (c) flags in the B cohort, not the T cohort
        @test haskey(allc["B"]["metrics"]["nTracks"]["outliers"], "c")
        @test !haskey(allc["T"]["metrics"]["nTracks"]["outliers"], "c")
        @test !isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))   # read-only wrote nothing
        # persist variant writes each label set's sidecar
        allw = cohort_qc_for_all!(set, "clustTracks.cluster")
        @test isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))
        @test isfile(cohort_qc_path(set, "clustTracks.cluster", "T"))
        @test occursin("(B)", join(cohort_qc_summary_lines(allw["B"])))   # label set named in the summary
    end

    @testset "cluster QC banked per run (suffix, no collision)" begin
        img = CciaImage(; uid = "a", dir = mktempdir())
        qcdir = mktempdir()
        # a cluster_qc.json fixture (what the Python runner writes): two segments T & B
        mkqc(path) = open(path, "w") do io
            JSON3.write(io, Dict("nClusters" => 4, "perSegment" => [
                Dict("uID" => "a", "valueName" => "T", "n" => 40, "nClusters" => 4, "largestClusterFrac" => 0.4),
                Dict("uID" => "a", "valueName" => "B", "n" => 20, "nClusters" => 3, "largestClusterFrac" => 0.5)]))
        end
        p1 = joinpath(qcdir, "run1.json"); mkqc(p1)
        Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p1; unit = "tracks", suffix = "movement")
        p2 = joinpath(qcdir, "run2.json"); mkqc(p2)
        Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p2; unit = "tracks", suffix = "test")
        # BOTH runs retained under composite {labelSet}.{suffix} keys — "test" did NOT overwrite "movement"
        dmov = read_qc(img, "clustTracks.cluster", "T.movement")
        dtst = read_qc(img, "clustTracks.cluster", "T.test")
        @test dmov !== nothing && dtst !== nothing
        @test dmov["metrics"]["nTracks"] == 40 && dmov["runSuffix"] == "movement" && dmov["labelSet"] == "T"
        @test dtst["runSuffix"] == "test"
        # empty suffix ⇒ bank under the bare label set (no trailing dot)
        p3 = joinpath(qcdir, "run3.json"); mkqc(p3)
        Cecelia.write_cluster_qc!([img], "clustPops.cluster", p3; unit = "cells", suffix = "")
        @test read_qc(img, "clustPops.cluster", "T") !== nothing

        set = CciaSet(; dir = mktempdir()); push!(set._images, img); push!(set.image_uids, "a")
        # cohort discovers all (label set × run) value_names
        @test cohort_value_names(set, "clustTracks.cluster") == ["B.movement", "B.test", "T.movement", "T.test"]
        # cohort_runs groups by run (segment/tracking funs bank no run → [])
        crs = cohort_runs(set, "clustTracks.cluster")
        @test Set(r.run for r in crs) == Set(["movement", "test"])
        @test sort(first(r.valueNames for r in crs if r.run == "test")) == ["B.test", "T.test"]  # each run carries its value_names
        @test isempty(cohort_runs(set, "segment.cellpose"))
        # run filter: cohort_qc_for_all!(run="test") persists ONLY the test run's value_names
        allw = cohort_qc_for_all!(set, "clustTracks.cluster"; run = "test")
        @test Set(keys(allw)) == Set(["B.test", "T.test"])
        @test occursin("(T.test)", join(cohort_qc_summary_lines(allw["T.test"])))  # run named in the lab-log line
    end

    @testset "register_cohort_metrics! (custom-module opt-in)" begin
        fun = "customExamples.qcProbeTest"
        @test !haskey(COHORT_METRICS, fun)                       # unknown → cohort errors
        register_cohort_metrics!(fun, ["nCells"])
        @test COHORT_METRICS[fun] == ["nCells"]                  # now a known producer
        register_cohort_metrics!(fun, ["nCells", "nClusters"])   # idempotent overwrite
        @test COHORT_METRICS[fun] == ["nCells", "nClusters"]
        delete!(COHORT_METRICS, fun)                             # don't leak into other testsets
    end

    @testset "board spec expander (MCP board authoring, Phase 2)" begin
        # The expander turns a SEMANTIC spec into a LayoutEntry and refuses anything the project cannot
        # plot. The failure it closes: a bad `tkey` renders an EMPTY panel with NO error, so a board can
        # look authored and show nothing. See docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 2.
        specs = plot_spec_index()
        @test haskey(specs, "track_measures")            # anti-vacuity: the registry actually loaded
        @test length(plot_specs()) >= 5

        # templates: "<cols>x<rows>" only — the named comic plates are a frontend catalogue and stay
        # GUI-only rather than being duplicated server-side
        @test board_template_grid("2x2", 4) == (2, 2)
        @test board_template_grid("3x2", 6) == (3, 2)
        @test board_template_grid("", 4) == (2, 2)       # empty → smallest near-square grid that fits
        @test board_template_grid("", 3) == (2, 2)
        @test board_template_grid("", 1) == (1, 1)
        @test_throws BoardSpecError board_template_grid("comic-banner", 4)
        @test_throws BoardSpecError board_template_grid("9x9", 4)
        # row-major grid areas, the uniform case of plots/layoutTemplates.ts
        @test board_slot_areas(2, 2) == ["1 / 1 / 2 / 2", "1 / 2 / 2 / 3", "2 / 1 / 3 / 2", "2 / 2 / 3 / 3"]

        proj = CciaProject(; uid = "bsp", name = "spec"); proj.root = mktempdir()
        ok(plots; kw...) = expand_board(proj, "B vs T", plots; kw...)

        # A project with no populations still expands a board that references none — pops are optional
        lay = ok([Dict("plot" => "track_measures", "measure" => "live.track.speed", "chart" => "boxplot")])
        @test lay["cols"] == 1 && lay["rows"] == 1
        @test length(lay["contents"]) == 1 && lay["contents"][1] !== nothing
        c = lay["contents"][1]
        @test c["kind"] == "summary" && c["ref"] == "track_measures"
        @test c["state"]["chartType"] == "boxplot" && c["state"]["measure"] == "live.track.speed"
        @test !haskey(c["state"], "vis")        # the panel owns its look — defaultVis() is NOT copied here
        @test !haskey(c["state"], "statUnit")   # nothing said → nothing guessed; the panel fills it

        # unfilled slots stay empty, and the board keeps the requested shape
        lay4 = ok([Dict("plot" => "track_measures"), Dict("plot" => "track_measures")]; template = "2x2")
        @test length(lay4["contents"]) == 4
        @test lay4["contents"][3] === nothing && lay4["contents"][4] === nothing
        @test lay4["activeIndex"] == 0
        # no pops requested → nothing to point the board at, so no scope is forced
        @test lay4["shared"] == Dict{String,Any}()

        # …but when we DO write per-slot `sel`, the board must be told to read it. `shared.scope`
        # defaults to "global" in useSummaryData, and panelSel then takes the board-level `shared.sel`
        # and ignores each slot's own — so an authored board rendered with NO series until the user
        # picked populations by hand. Regression test for exactly that.
        withpops = expand_board(proj, "sel", [Dict("plot" => "track_measures", "pops" => ["B/qc"])];
                                pops = Dict("B/qc" => "live"))
        @test withpops["shared"]["scope"] == "local"
        # A tkey is tagged with the population's OWN family — the same tag the picker puts on it (see
        # plot_population_groups), which is what the frontend builds its tkeys from.
        #
        # This fixture used to say `Dict("B/qc" => "flow")` and expect `live::B/qc`, on the reasoning
        # that "a flow gate is legitimately usable as a live pop". That was a misreading of why the real
        # project stores `live::B/qc/_tracked`: it stores live because the PICKER TAGS those pops live
        # (`load_pop_map(img, vn, "live")` returns the gate tree, and the derived `_tracked` pops are
        # injected under live). Checked against the real project — its families are live (B/qc, T/qc,
        # and their `_tracked` children), labels, region and trackclust; nothing is tagged flow. A
        # genuinely flow-tagged population is NOT reachable on track_measures, which offers only
        # live/track/trackclust, and writing `live::` over it would have produced a blank panel.
        @test withpops["contents"][1]["state"]["sel"] == ["live::B/qc"]
        # …and that case is now refused rather than silently mis-tagged
        @test_throws BoardSpecError expand_board(proj, "sel-flow",
            [Dict("plot" => "track_measures", "pops" => ["B/qc"])]; pops = Dict("B/qc" => "flow"))
        # and nothing else is invented in the shared bag — the rest are frontend defaults
        @test collect(keys(withpops["shared"])) == ["scope"]

        # statUnit and imageAgg travel together (utils/statUnitState.ts)
        su = ok([Dict("plot" => "track_measures", "statUnit" => "image")])["contents"][1]["state"]
        @test su["statUnit"] == "image" && su["imageAgg"] == "mean"
        @test ok([Dict("plot" => "track_measures", "statUnit" => "image",
                       "imageAgg" => "median")])["contents"][1]["state"]["imageAgg"] == "median"

        # ── the rejections. Every message must name the offending value AND what was available, because
        # the caller is an agent that can correct itself if told the options.
        @test_throws BoardSpecError ok([])                                    # a board needs a plot
        @test_throws BoardSpecError expand_board(proj, "  ", [Dict("plot" => "track_measures")])
        @test_throws BoardSpecError ok([Dict("measure" => "x")])               # no `plot`
        @test_throws BoardSpecError ok([Dict("plot" => "no_such_plot")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "chart" => "sankey")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "measure" => "live.track.nope")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "statUnit" => "per-cell")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "statUnit" => "image",
                                             "imageAgg" => "mode")])
        # a population that does not exist would render an empty panel in silence — this is the one
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "pops" => ["Ghost/qc"])])

        # ── DERIVED populations must be accepted. `/_tracked` is injected by the picker at query time
        # and is NOT stored in the gating sidecar, so an earlier version of this validator — which
        # walked the persisted populations — rejected "B/qc/_tracked", the population the real project's
        # own boards plot. board_spec_populations now goes through `plot_population_groups`, the same
        # enumerator that fills the picker, so the validator accepts exactly what the GUI offers.
        avail = Dict("B/qc/_tracked" => "live", "T/qc/_tracked" => "live", "B/qc" => "flow")
        d = expand_board(proj, "derived", [Dict("plot" => "track_measures",
                                                "pops" => ["B/qc/_tracked", "T/qc/_tracked"])];
                         pops = avail)["contents"][1]
        @test d["state"]["sel"] == ["live::B/qc/_tracked", "live::T/qc/_tracked"]
        @test d["state"]["popType"] == "live"        # taken from the picker, not guessed
        # …and the tkeys the expander writes must decode back to the pops it was given — read and write
        # describe a board the same way (Decision 2)
        @test [let t = Cecelia._parse_tkey(k); "$(t.valueName)$(t.pop)" end for k in d["state"]["sel"]] ==
              ["B/qc/_tracked", "T/qc/_tracked"]
        # ── popType must REACH the named populations ────────────────────────────────────────────────
        # The panel fetches its list with `plot_pop_types(popType, granularity)` and tags each pop with
        # the family it was found under; a tkey outside that expansion matches nothing and the panel
        # renders empty with no error. So the check is reachability, not membership.
        #
        # Stating the popType the derivation would pick anyway is redundant but fine:
        @test expand_board(proj, "pt-ok", [Dict("plot" => "track_measures", "popType" => "live",
                                                "pops" => ["B/qc/_tracked"])];
                           pops = avail)["contents"][1]["state"]["sel"] == ["live::B/qc/_tracked"]
        # …and "track" is REFUSED even though track_measures offers it. This is the real bug, traced:
        # `plot_pop_types("track", "track") == ["track"]`, track-family pops are gates drawn on per-track
        # measures (`{vn}__tracks.json`), and the project that hit this has none — so the picker returned
        # ZERO populations, all four plots said "Select one or more populations", and nothing errored.
        # A membership check would have passed it: "track" IS in ["live","track","trackclust"].
        e_pt = try expand_board(proj, "pt-bad", [Dict("plot" => "track_measures", "popType" => "track",
                                                     "pops" => ["B/qc/_tracked"])]; pops = avail)
               catch err; err end
        @test e_pt isa BoardSpecError
        @test occursin("track", e_pt.msg) && occursin("live", e_pt.msg)   # names both, so a caller can fix it
        @test occursin("blank", e_pt.msg)                                 # …and says why it matters

        # A CLUSTER board must still work: population_summary's first offered popType is "flow", but
        # trackclust pops are only reachable under "trackclust", so the derivation must walk past the
        # default rather than stamping it. (the "Clustering" board is exactly this shape — an
        # earlier version of this fix, which allowed only the spec's default, would have refused to
        # re-author it.)
        clust = Dict("B/Directed" => "trackclust", "B/Scanning" => "trackclust")
        cl = expand_board(proj, "clust", [Dict("plot" => "population_summary",
                                               "pops" => ["B/Directed", "B/Scanning"])]; pops = clust)
        @test cl["contents"][1]["state"]["popType"] == "trackclust"
        @test cl["contents"][1]["state"]["sel"] == ["trackclust::B/Directed", "trackclust::B/Scanning"]
        # …and explicitly asking for it is accepted, because it reaches them
        @test expand_board(proj, "clust2", [Dict("plot" => "population_summary", "popType" => "trackclust",
                                                 "pops" => ["B/Directed"])];
                           pops = clust)["contents"][1]["state"]["popType"] == "trackclust"
        # a plot that cannot reach the populations at all is refused, naming what it does offer
        e_reach = try expand_board(proj, "unreach", [Dict("plot" => "spatial_interactions",
                                                          "pops" => ["B/Directed"])]; pops = clust)
                  catch err; err end
        @test e_reach isa BoardSpecError && occursin("flow", e_reach.msg)

        # ── compareBy: what makes a board a FIGURE ───────────────────────────────────────────────────
        # Board-level, because useSummaryData destructures compareMode/compareAttr out of the SHARED
        # bag. Without it an authored board sits on the frontend default (single image) — which is how
        # a board built for a 4-mouse experiment came out comparing images, unable to answer the
        # question it was asked.
        one = [Dict("plot" => "track_measures")]
        have = ["Treatment", "Mouse"]   # injected like `pops` — no project on disk needed
        @test !haskey(expand_board(proj, "c0", one)["shared"], "compareMode")   # omitted → untouched
        @test expand_board(proj, "c1", one; compare_by = "per_image", attrs = have)["shared"]["compareMode"] == "per_image"
        @test expand_board(proj, "c2", one; compare_by = "summarised", attrs = have)["shared"]["compareMode"] == "summarised"
        # an attribute name → by_attr
        s_attr = expand_board(proj, "c3", one; compare_by = "Mouse", attrs = have)["shared"]
        @test s_attr["compareMode"] == "by_attr" && s_attr["compareAttr"] == "Mouse"
        @test !haskey(s_attr, "compareAttr2")
        # two combine, in order
        s_two = expand_board(proj, "c4", one; compare_by = "Treatment,Mouse", attrs = have)["shared"]
        @test s_two["compareAttr"] == "Treatment" && s_two["compareAttr2"] == "Mouse"
        # …and an attribute the project does not have is refused, naming the ones it does — grouping by
        # a name nothing carries silently falls back to per-image, i.e. the wrong figure, drawn.
        e_attr = try expand_board(proj, "c5", one; compare_by = "Genotype", attrs = have) catch err; err end
        @test e_attr isa BoardSpecError && occursin("Genotype", e_attr.msg) && occursin("Mouse", e_attr.msg)
        @test_throws BoardSpecError expand_board(proj, "c6", one; compare_by = "Mouse,Treatment,Location", attrs = have)
        # an UNANNOTATED set gets told what to do instead, not just "no such attribute" — this is the
        # common case (nobody annotated the images) and "Available: " with an empty list is a dead end.
        e_none = try expand_board(proj, "c7", one; compare_by = "Mouse", attrs = String[]) catch err; err end
        @test e_none isa BoardSpecError && occursin("per_image", e_none.msg)
        # …and a case slip says so, like channel_indices does for channel names (`mem-TOM`/`mem-Tom`).
        # "not an attribute" next to a list containing what looks like the same word is a dead end.
        e_case = try expand_board(proj, "c8", one; compare_by = "mouse", attrs = have) catch err; err end
        @test e_case isa BoardSpecError && occursin("differs only in case", e_case.msg) &&
              occursin("\"Mouse\"", e_case.msg)
        # more plots than slots
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures") for _ in 1:5]; template = "2x2")

        e = try ok([Dict("plot" => "no_such_plot")]) catch err; err end
        @test e isa BoardSpecError && occursin("no_such_plot", e.msg) && occursin("track_measures", e.msg)

        # ── append_board: ADD-ONLY (Decision 1) ────────────────────────────────────────────────────
        doc = BoardsDoc(3, Any[Dict("id" => 1, "name" => "Track measures")], 1, 1,
                        Dict{String,Any}("tab:1" => Dict("cols" => 1)), true, true)
        doc2, id = append_board(doc, "B vs T", lay)
        @test id == 2 && length(doc2.tabs) == 2
        @test doc2.tabs[1] == doc.tabs[1]                  # the existing board is untouched
        @test haskey(doc2.layouts, "tab:1") && haskey(doc2.layouts, "tab:2")
        @test doc2.next_id == 2 && doc2.version == doc.version   # version is stamped by the writer
        # the user's ACTIVE tab is not stolen — this writes into a project they may have open
        @test doc2.active_id == doc.active_id
        @test_throws BoardSpecError append_board(doc2, "B vs T", lay)          # duplicate name
        @test_throws BoardSpecError append_board(doc2, "  Track measures ", lay)  # …ignoring whitespace

        # ── the name is STORED as it will render (board_display_name) ───────────────────────────────
        # Vue escapes text, so a stored entity displays as the entity — on a tab the authoring tool
        # cannot rename. A real board shipped as "Behaviour &amp; tracking by image" because the agent
        # HTML-escaped the ampersand and nothing repaired it. Repair, don't reject: the intent is
        # unambiguous and an error would spend a round-trip on punctuation.
        @test board_display_name("  Behaviour &amp; tracking  ") == "Behaviour & tracking"
        @test board_display_name("a &lt;b&gt; &quot;c&quot; &#39;d&#39;") == "a <b> \"c\" 'd'"
        @test board_display_name(board_display_name("A &amp; B")) == "A & B"      # idempotent
        # &amp; decodes LAST, so an escaped entity unwinds one level, not two
        @test board_display_name("&amp;lt;not a tag&amp;gt;") == "&lt;not a tag&gt;"
        doc3, _ = append_board(doc2, "Behaviour &amp; tracking", lay)
        @test doc3.tabs[end]["name"] == "Behaviour & tracking"
        # …and the duplicate check sees through the escaping too: the same name twice is still one name
        @test_throws BoardSpecError append_board(doc3, "Behaviour & tracking", lay)
    end

    @testset "boards document — one reader, both shapes, versioned writes" begin
        # analysisBoards.json has had two shapes. The tab ARRAY used to sit at `tabs.tabs` (a TabGroup
        # nested under `tabs`) — the collision that made a second parser read `b.tabs` as the array and
        # report NO boards on every project that had them. Both are read; only the flat one is written.
        dir = mktempdir()
        p = boards_doc_path(dir)
        @test endswith(p, joinpath("settings", "analysisBoards.json"))

        d0 = read_boards_doc(p)                                   # no file at all
        @test !d0.present && d0.readable && isempty(d0.tabs) && d0.version == 0

        mkpath(dirname(p))
        legacy = Dict("tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "A")], "activeId" => 1, "nextId" => 2),
                      "layouts" => Dict("tab:1" => Dict("cols" => 2, "rows" => 1)))
        write(p, JSON3.write(legacy))
        d = read_boards_doc(p)
        @test d.present && d.readable
        @test length(d.tabs) == 1 && string(d.tabs[1]["name"]) == "A"
        @test d.active_id == 1 && d.next_id == 2
        @test d.version == 0                                      # no version key → 0, not an error
        @test haskey(d.layouts, "tab:1")                          # String keys, not JSON3 Symbols

        # writing converts to the flat shape and stamps the version
        write_boards_doc(p, d; version = d.version + 1)
        raw = JSON3.read(read(p, String))
        @test raw[:version] == 1
        @test raw[:tabs] isa AbstractVector                        # flat: the array is at the top level
        @test raw[:activeId] == 1 && raw[:nextId] == 2
        d2 = read_boards_doc(p)
        @test d2.version == 1 && length(d2.tabs) == 1 && d2.active_id == 1

        # a file that exists but cannot be parsed is NOT "no boards" — that silence hid the last bug
        write(p, "{not json")
        d3 = @test_logs (:warn,) match_mode=:any read_boards_doc(p)
        @test d3.present && !d3.readable

        # normalise_boards is pure, so the autosave route runs an incoming payload through exactly the
        # same reader as a load from disk
        n = normalise_boards(Dict("version" => 7, "tabs" => [Dict("id" => 2, "name" => "B")],
                                  "activeId" => 2, "nextId" => 3, "layouts" => Dict()))
        @test n.version == 7 && n.active_id == 2 && length(n.tabs) == 1
        @test normalise_boards("not a document").readable == false
        # the payload the client gets always carries the version its next write must echo
        @test boards_doc_payload(n)["version"] == 7
    end

    @testset "board read-back summarises what a board plots" begin
        # The boards file is written by the FRONTEND, so every field here is optional by construction:
        # a board from an older schema must degrade to fewer fields, never throw.
        # See docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
        proj = CciaProject(; uid = "bdP", name = "boards"); proj.root = mktempdir()
        @test board_summaries(proj) == Any[]                       # no file yet

        mkpath(joinpath(proj.root, "settings"))
        bf = joinpath(proj.root, "settings", "analysisBoards.json")
        # The slot shape below is COPIED from a real analysisBoards.json, not hand-authored to match
        # the parser — note `title` inside the `vis` bag, which is where the frontend actually puts it.
        # An invented fixture is what certified both bugs in this file's history.
        _slot(unit) = Dict("kind" => "summary", "ref" => "track_measures",
                           "state" => Dict("specId" => "track_measures", "measure" => "live.track.speed",
                                           "chartType" => "boxplot", "popType" => "live",
                                           "sel" => ["live::B/qc", "live::T/qc"],
                                           "groupBy" => "live.cell.hmm.state.movement",
                                           "statUnit" => unit, "imageAgg" => "mean",
                                           "vis" => Dict("title" => "Speed", "logScale" => false)))
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "Track measures"),
                                      Dict("id" => 4, "name" => "Per image measures"),
                                      Dict("id" => 2, "name" => "Empty board")],
                           "activeId" => 1, "nextId" => 5),
            "layouts" => Dict(
                "tab:1" => Dict("cols" => 2, "rows" => 1, "contents" => [
                    _slot("individual"),
                    nothing,                                        # an empty slot is omitted
                ]),
                "tab:4" => Dict("cols" => 2, "rows" => 1, "contents" => [_slot("image")]),
                # tab 2 has no layout entry at all — a real state (tab created, never filled)
            ))))

        b = board_summaries(proj)
        @test length(b) == 3
        @test b[1]["name"] == "Track measures" && b[1]["cols"] == 2 && b[1]["rows"] == 1
        @test length(b[1]["plots"]) == 1                            # the nothing slot is dropped
        pl = b[1]["plots"][1]
        @test pl["kind"] == "summary" && pl["ref"] == "track_measures"
        @test pl["measure"] == "live.track.speed" && pl["chart"] == "boxplot"
        @test pl["groupBy"] == "live.cell.hmm.state.movement"
        @test pl["pops"] == ["B/qc", "T/qc"]                        # tkeys decoded to valueName/pop
        # the caption comes from state.vis.title — reading state.title returned nothing on every real
        # board, and the old fixture put it there and asserted it worked
        @test pl["title"] == "Speed"

        # THE REGRESSION. These two boards differ ONLY in summary level, and the summary must say so:
        # "Track measures" plots every track, "Per image measures" collapses each image to its mean.
        # While `statUnit` was dropped they serialised identically, and the observer reported a
        # duplicate board that wasn't one — a confident false claim about the user's own work.
        pi = b[2]["plots"][1]
        @test pl["statUnit"] == "individual" && pi["statUnit"] == "image"
        @test pl != pi
        @test pl["imageAgg"] == "mean" && pi["imageAgg"] == "mean"  # the pair travels together
        # a tab with no layout is reported, blank, rather than skipped
        @test b[3]["name"] == "Empty board" && isempty(b[3]["plots"])

        # highlighted pops + the clustered feature list define what a cluster plot SAYS
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "Clustering")]),
            "layouts" => Dict("tab:1" => Dict("cols" => 1, "rows" => 1, "contents" => [
                Dict("kind" => "summary", "ref" => "state_signature",
                     "state" => Dict("specId" => "state_signature", "hl" => ["/Directed", "/Scanning"],
                                     "features" => ["live.track.speed", "live.track.straightness"]))])))))
        pl = board_summaries(proj)[1]["plots"][1]
        @test pl["highlight"] == ["/Directed", "/Scanning"]
        @test pl["features"] == ["live.track.speed", "live.track.straightness"]

        # The pair is copied straight through — no default resolved, no guess about which slots have a
        # summary level. The panel persists it explicitly and clears it when the plot has none
        # (frontend/src/utils/statUnitState.ts), so a slot with no `statUnit` genuinely has no summary
        # level. A board written before that (or an interactive view) simply reports neither.
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "no summary level"),
                                      Dict("id" => 2, "name" => "image-level")]),
            "layouts" => Dict(
                "tab:1" => Dict("contents" => [
                    Dict("kind" => "summary", "ref" => "track_measures",
                         "state" => Dict("measure" => "live.track.speed")),      # neither key present
                    Dict("kind" => "interactive", "ref" => "umap", "state" => Dict())]),
                "tab:2" => Dict("contents" => [
                    Dict("kind" => "summary", "ref" => "track_measures",
                         "state" => Dict("measure" => "live.track.speed",
                                         "statUnit" => "image", "imageAgg" => "median"))])))))
        bb = board_summaries(proj)
        @test !haskey(bb[1]["plots"][1], "statUnit") && !haskey(bb[1]["plots"][1], "imageAgg")
        @test !haskey(bb[1]["plots"][2], "statUnit")     # nothing invented for an interactive view
        @test bb[2]["plots"][1]["statUnit"] == "image" && bb[2]["plots"][1]["imageAgg"] == "median"
        @test bb[1]["plots"][1] != bb[2]["plots"][1]     # distinguishable, which is the point

        # degradation: a slot with no state, and an unparseable file
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "bare")]),
            "layouts" => Dict("tab:1" => Dict("contents" => [Dict("kind" => "interactive", "ref" => "umap")])))))
        b = board_summaries(proj)
        @test b[1]["plots"][1]["ref"] == "umap" && !haskey(b[1]["plots"][1], "measure")
        @test b[1]["cols"] == 0                                     # missing grid → 0, not an error
        write(bf, "{not json")
        @test (@test_logs (:warn,) match_mode=:any board_summaries(proj)) == Any[]

        # A top-level `tabs` ARRAY is the CURRENT shape (it was the legacy `tabs.tabs` nesting that this
        # reader used to choke on), so it must read as a real board rather than warn.
        write(bf, JSON3.write(Dict("version" => 4, "tabs" => [Dict("id" => 9, "name" => "flat")],
                                   "activeId" => 9, "nextId" => 10, "layouts" => Dict())))
        fb = board_summaries(proj)
        @test length(fb) == 1 && fb[1]["name"] == "flat" && isempty(fb[1]["plots"])

        # A present-but-unreadable file must WARN, not quietly read as "no boards" — that silence is
        # what hid the `_board_tabs` bug (it read `b.tabs` as the array, a shape the frontend never
        # writes, and reported none on every real project for as long as it existed).
        write(bf, JSON3.write([1, 2, 3]))          # valid JSON, not a document
        @test (@test_logs (:warn,) match_mode=:any board_summaries(proj)) == Any[]
    end

    @testset "analysis lineage (Slice A synthesizer)" begin
        proj = CciaProject(; uid = "linP", name = "lineage"); proj.root = mktempdir()
        s = CciaSet(; uid = "linS", dir = mktempdir())
        push!(proj._sets, s); push!(proj.set_uids, s.uid)

        # i1 — full pipeline: import → segment(A,B) → track(A) → cluster(A/movement); A gated (flow)
        i1 = CciaImage(; uid = "i1", dir = mktempdir())
        i1.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad")
        lp1 = img_label_props_dir(i1); mkpath(lp1)
        touch(img_track_props_path(i1, "A"))                       # A is tracked
        open(joinpath(lp1, "A__tracks.clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["i1"])))
        end
        g1 = PopulationMap(; pop_type = "flow", value_name = "A")
        add_pop!(g1, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(g1, i1)
        append_run_log!(i1, "importImages.omezarr", "default", "done")
        append_run_log!(i1, "segment.cellpose", "A", "done")
        append_run_log!(i1, "segment.cellpose", "B", "done")
        append_run_log!(i1, "tracking.bayesian_tracking", "A", "done")
        append_run_log!(i1, "clustTracks.cluster", "movement", "done")
        push!(s._images, i1); push!(s.image_uids, i1.uid)

        # i2 — partial + excluded: import → segment(A, failed) only
        i2 = CciaImage(; uid = "i2", dir = mktempdir()); i2.included = false
        i2.label_props = Dict("A" => "A.h5ad")
        append_run_log!(i2, "importImages.omezarr", "default", "done")
        append_run_log!(i2, "segment.cellpose", "A", "failed")
        push!(s._images, i2); push!(s.image_uids, i2.uid)

        # a wired chain + board tabs (project-level)
        save_chain_template!(proj, ChainTemplate("pipeline",
            [ChainNode(; id = "n1", fn = "segment.cellpose"),
             ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")], ChainEdge[]))
        mkpath(joinpath(proj.root, "settings"))
        open(joinpath(proj.root, "settings", "analysisBoards.json"), "w") do io
            # the REAL persisted shape: `tabs` is a TabGroup ({tabs, activeId, nextId}), not a bare
            # array. This fixture used to be the bare array — written to match a parser that read
            # `b.tabs` as the list — so the test passed while lineage reported no boards on every real
            # project. Keep this mirroring stores/analysisTabs.ts `serialize`.
            JSON3.write(io, Dict("tabs" => Dict(
                "tabs" => [Dict("id" => 1, "name" => "Behaviour"), Dict("id" => 2, "name" => "Counts")],
                "activeId" => 1, "nextId" => 3)))
        end

        lin = analysis_lineage(proj)
        @test lin.projectUid == "linP" && length(lin.images) == 2
        e1 = lin.images[findfirst(e -> e.uid == "i1", lin.images)]
        e2 = lin.images[findfirst(e -> e.uid == "i2", lin.images)]
        # i1 ordered steps + stage mapping, last step's value_name is the run suffix
        @test [st.stage for st in e1.steps] == ["import", "segment", "segment", "track", "cluster"]
        @test e1.steps[end].fun == "clustTracks.cluster" && e1.steps[end].valueName == "movement"
        @test e1.segmentations == ["A", "B"] && e1.tracked == ["A"]
        @test length(e1.clusterRuns) == 1 && e1.clusterRuns[1].suffix == "movement" &&
              e1.clusterRuns[1].valueNames == ["A"]
        @test length(e1.gatedPops) == 1 && e1.gatedPops[1].valueName == "A" && "/CD3" in e1.gatedPops[1].pops
        # i2 partial + excluded + a failed step is surfaced
        @test e2.included == false && isempty(e2.tracked) && isempty(e2.clusterRuns)
        @test any(st -> st.fun == "segment.cellpose" && st.status == "failed", e2.steps)
        # project-level chains + boards
        @test length(lin.chains) == 1 && lin.chains[1].name == "pipeline"
        @test Set(lin.chains[1].tasks) == Set(["segment.cellpose", "tracking.bayesian_tracking"])
        @test lin.boards == ["Behaviour", "Counts"]
        # rollup: pipeline unions run-log steps AND artifact evidence, so i1's gated pop adds a
        # "gate" stage even though gating isn't a task step. i2 diverges (excluded + missing the
        # track/gate/cluster stages the others reached).
        @test lin.rollup.pipeline == ["import", "segment", "track", "gate", "cluster"]
        dv = lin.rollup.divergences[findfirst(d -> d.uid == "i2", lin.rollup.divergences)]
        @test dv.included == false && Set(dv.missingStages) == Set(["track", "gate", "cluster"])
        # artifact-aware stages: a segmentation/track with NO run-log step still counts as reached
        # (it predates the capped run-log window) — the fix for false "missing segment" divergences
        noStep = (; uid = "x", name = "X", included = true, steps = NamedTuple[],
                    segmentations = ["A"], tracked = ["A"], clusterRuns = Any[], gatedPops = Any[])
        @test Set(Cecelia._image_stages(noStep)) == Set(["segment", "track"])
        # scoping: one image, one set, unknown → empty
        @test length(analysis_lineage(proj; image_uid = "i1").images) == 1
        @test length(analysis_lineage(proj; set_uid = "linS").images) == 2
        @test isempty(analysis_lineage(proj; image_uid = "nope").images)
    end

    @testset "populations summary (Slice B)" begin
        proj = CciaProject(; uid = "popP", name = "pops"); proj.root = mktempdir()
        s = CciaSet(; uid = "popS", dir = mktempdir())
        push!(proj._sets, s); push!(proj.set_uids, s.uid)
        img = CciaImage(; uid = "i1", dir = mktempdir())
        img.label_props = Dict("A" => "A.h5ad")
        # a flow gate on A (CD3) + a cluster pop filtering clusters.movement
        mf = PopulationMap(; pop_type = "flow", value_name = "A")
        add_pop!(mf, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(mf, img)
        mc = PopulationMap(; pop_type = "trackclust", value_name = "A")
        add_pop!(mc, "Directed"; filter_measure = "clusters.movement", filter_fun = "in", filter_values = [3])
        save_pop_map!(mc, img)
        push!(s._images, img); push!(s.image_uids, img.uid)

        out = populations_summary(proj)
        @test out.projectUid == "popP" && length(out.images) == 1
        pops = out.images[1].populations
        @test out.images[1].truncated == false
        cd3 = pops[findfirst(p -> p.name == "CD3", pops)]
        @test cd3.popType == "flow" && cd3.valueName == "A" && cd3.filter === nothing
        @test cd3.gate !== nothing && cd3.gate["kind"] == "rectangle" &&
              cd3.gate["x_channel"] == "c1" && cd3.gate["y_channel"] == "c2"
        dir = pops[findfirst(p -> p.name == "Directed", pops)]
        @test dir.popType == "trackclust" && dir.gate === nothing
        @test dir.filter.measure == "clusters.movement" && dir.filter.fun == "in" &&
              collect(dir.filter.values) == [3]
        # scoping mirrors lineage
        @test length(populations_summary(proj; image_uid = "i1").images) == 1
        @test isempty(populations_summary(proj; image_uid = "nope").images)

        # AUTO-SHARED cluster pops are reported for the borrowing segmentation too. Cluster pops are
        # global to a run: the names are authored under ONE co-clustered value_name (A) and every
        # sibling in the run (B) carries the same `clusters.movement` column, so `load_pop_map` lends
        # them relabeled. The observer used to skip any value_name with no sidecar FILE, so B looked
        # unclustered — and a board authored from that covered half the run's data.
        img.label_props["B"] = "B.h5ad"
        mkpath(Cecelia.img_label_props_dir(img))
        for vn in ("A", "B")      # both segments took part in the `movement` track-clustering run
            write(Cecelia._clustfeatures_path(img_track_props_path(img, vn)),
                  JSON3.write(Dict("clusters.movement" =>
                      Dict("features" => ["live.track.speed"], "partOf" => ["i1"]))))
        end
        shared = populations_summary(proj).images[1].populations
        bdir = shared[findall(p -> p.name == "Directed" && p.valueName == "B", shared)]
        @test length(bdir) == 1 && bdir[1].popType == "trackclust"
        @test bdir[1].filter.measure == "clusters.movement"        # the SAME run, not a new definition
        # a segment that is NOT in the run borrows nothing (no clustfeatures sidecar → no pops)
        img.label_props["C"] = "C.h5ad"
        cpops = populations_summary(proj).images[1].populations
        @test isempty(findall(p -> p.valueName == "C", cpops))
    end

    @testset "measure summary (Slice C)" begin
        # pure summary logic (always runs): median/quantiles/mean over finite values, NaN/missing dropped
        s = Cecelia._summarise_measure("x", Any[1.0, 2.0, 3.0, NaN, missing])
        @test s.n == 3 && s.median == 2.0 && s.q25 <= 2.0 <= s.q75    # mean dropped (payload trim)
        @test !hasproperty(s, :mean)
        @test Cecelia._summarise_measure("y", Any[NaN, missing]) === nothing

        # integration over the real KDIeEm B fixture: UNGATED image → the base fallback (all-cells
        # phenotype + tracked motility). The gated path (T/_qc) is validated separately off-suite.
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "measure summary (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid = "KDIeEm", dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            proj = CciaProject(; uid = "mP", name = "m"); proj.root = mktempdir()
            st = CciaSet(; uid = "mS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
            push!(st._images, img); push!(st.image_uids, img.uid)

            out = measure_summary(proj)
            @test length(out.images) == 1
            summ = out.images[1].summaries
            @test !isempty(summ)
            # motility over the tracked base (no channel-name dependence) — the robust anchor
            mi = findfirst(x -> x.kind == "motility", summ)
            @test mi !== nothing
            moti = summ[mi]
            @test moti.n > 0 && any(m -> m.name == "live.track.speed", moti.measures)
            @test all(m -> isfinite(m.median) && m.n > 0, moti.measures)
            # phenotype over all cells: more rows than tracks (cells collapse to tracks)
            pi = findfirst(x -> x.kind == "phenotype", summ)
            @test pi !== nothing && summ[pi].n > moti.n && !isempty(summ[pi].measures)
        end
    end

    @testset "behaviour + cluster summary (Slice D)" begin
        # pure category-distribution logic (always runs): fractions, distinct count, cap, null-drop
        d = Cecelia._category_distribution(Any[1.0, 1.0, 1.0, 2.0, NaN, missing, nothing])
        @test d.n == 4 && d.nDistinct == 2
        @test d.top[1].value == "1.0" && d.top[1].n == 3 && d.top[1].fraction == 0.75
        @test d.top[2].value == "2.0" && d.top[2].n == 1
        big = Cecelia._category_distribution(collect(1:100); cap = 5)
        @test big.nDistinct == 100 && length(big.top) == 5   # capped, but distinct count is the true total
        @test Cecelia._category_distribution(Any[NaN, missing, nothing]).n == 0

        # integration over the real KDIeEm B fixture: the summaries run and return the right shape
        # (behaviour/cluster entries only if the fixture banked HMM/cluster obs — asserted when present)
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "behaviour/cluster summary (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid = "KDIeEm", dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            proj = CciaProject(; uid = "bP", name = "b"); proj.root = mktempdir()
            st = CciaSet(; uid = "bS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
            push!(st._images, img); push!(st.image_uids, img.uid)

            b = behaviour_summary(proj); c = cluster_summary(proj)
            @test length(b.images) == 1 && b.images[1].behaviour isa AbstractVector
            @test length(c.images) == 1 && c.images[1].clusters isa AbstractVector
            @test haskey(c, :featuresByRun)   # feature lists hoisted out of per-image entries
            # every behaviour entry is a well-formed distribution
            for e in b.images[1].behaviour
                @test e.kind in ("state", "transitions") && e.n > 0 && !isempty(e.distribution)
                @test all(x -> 0.0 <= x.fraction <= 1.0, e.distribution)
            end
        end
    end

    @testset "chains summary (Slice E)" begin
        proj = CciaProject(; uid = "chP", name = "ch"); proj.root = mktempdir()
        save_chain_template!(proj, ChainTemplate("pipe",
            [ChainNode(; id = "n1", fn = "segment.cellpose"),
             ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")],
            [ChainEdge("n1", "n2")]))
        c = chains_summary(proj)
        @test c.projectUid == "chP" && length(c.templates) == 1
        t = c.templates[1]
        @test t.name == "pipe" && length(t.nodes) == 2 && length(t.edges) == 1
        @test t.nodes[1].fun == "segment.cellpose" && t.nodes[1].scope == "image"   # per-task default
        @test t.edges[1].from == "n1" && t.edges[1].to == "n2"
        @test c.runs isa AbstractVector && isempty(c.runs)   # no runs recorded on disk
    end
end

@testset "ss listener PID parse (_kill_listeners_on_port, Linux)" begin
    # Real `ss -tlnpH` lines: a listener appears once for IPv4 and once for IPv6 → one PID.
    raw = "LISTEN 0 128 127.0.0.1:8080 0.0.0.0:* users:((\"julia\",pid=1044704,fd=24))\n" *
          "LISTEN 0 128 [::1]:8080 [::]:* users:((\"julia\",pid=1044704,fd=25))"
    @test Cecelia._listener_pids_from_ss(raw) == [1044704]
    @test isempty(Cecelia._listener_pids_from_ss(""))                # nothing listening
    two = "users:((\"a\",pid=10,fd=1))\nusers:((\"b\",pid=22,fd=3))"
    @test Cecelia._listener_pids_from_ss(two) == [10, 22]            # distinct PIDs kept, in order
end

# Killing an ALREADY-DEAD process must be a no-op, not a segfault.
#
# This is a crash regression, and the only test in the suite whose failure mode is the whole process
# dying: `_kill_proc_tree` used to read the pid with a bare `ccall(:uv_process_get_pid, …,
# proc.handle)`, and Julia sets `proc.handle = C_NULL` the moment a process is reaped — so the ccall
# dereferenced NULL. It killed a running server (SIGSEGV in the napari-close path during a Restart) and
# took the detached task runner with it. An exited process is the COMMON input here: cancelling a task
# whose Python just finished, and closing a napari bridge that failed to start, both arrive this way.
#
# `Libc.getpid` is the fix — the same read with the iolock held and a `handle != C_NULL` check, raising
# a catchable `UV_ESRCH` instead. Asserting "no crash" cannot be done with `@test_throws`; the
# assertions are that we get here at all, and that the call reports the process as gone.
@testset "killing a dead process is a no-op (_kill_proc_tree)" begin
    proc = run(`$(Base.julia_cmd().exec[1]) --startup-file=no -e "exit()"`; wait = false)
    wait(proc)                                     # reaped → handle closed → C_NULL
    @test !process_running(proc)                   # the precondition the crash needed
    @test_throws Base.IOError Libc.getpid(proc)     # Base's guarded read: catchable, not a segfault
    @test Cecelia._kill_proc_tree(proc) === nothing # …and the caller survives it
    # A LIVE process still gets killed — the guard must not have turned the function into a no-op.
    live = run(`$(Base.julia_cmd().exec[1]) --startup-file=no -e "sleep(30)"`; wait = false)
    @test process_running(live)
    Cecelia._kill_proc_tree(live)
    wait(live)
    @test !process_running(live)
end

# Project export → import round-trip (project_io.jl / jobs.jl). Uses its own CECELIA_DEV_DIR +
# temp projects dir so it never touches the real dev/prod config; restores afterwards. Verifies:
# each .zarr store is packed to ONE .zarr.tar (no unpacked stores in the bundle), the lockfile is
# skipped, re-export/re-import refuse to clobber, and import restores byte-identical stores + uid.
@testset "project export/import" begin
    prev_env = get(ENV, "CECELIA_DEV_DIR", nothing)
    mktempdir() do tmp
        ENV["CECELIA_DEV_DIR"] = tmp
        try
            init_cecelia!()
            projroot = joinpath(tmp, "projects"); mkpath(projroot)
            set_projects_dir!(projroot)

            proj = create_project!(name = "io-test")
            uid  = proj.uid
            # fake stores + metadata mirroring the 0/ (data) + 1/ (metadata) layout
            d = joinpath(proj.root, "0", "img1", "data.ome.zarr", "0"); mkpath(d)
            write(joinpath(d, "0.0"), "chunk-bytes")
            write(joinpath(dirname(d), ".zattrs"), "{}")
            lab = joinpath(proj.root, "1", "img1", "labels", "labels.zarr"); mkpath(lab)
            write(joinpath(lab, ".zgroup"), "{}")
            lp = joinpath(proj.root, "1", "img1", "labelProps"); mkpath(lp)
            write(joinpath(lp, "base.h5ad"), "hdf-bytes")
            write(joinpath(proj.root, ".cecelia.lock"), "")
            # staging debris from a cancelled run. Matches neither `_is_store_dir` (no `.zarr`
            # suffix) nor the skip list, so the mirror walker used to RECURSE and copy every
            # chunk in as a loose file — gigabytes of unusable bytes in the bundle.
            deb = joinpath(proj.root, "0", "img1", "data.ome.zarr" * Cecelia.STORE_STAGING_SUFFIX, "0")
            mkpath(deb); write(joinpath(deb, "0.0"), "half-written")
            sup = joinpath(proj.root, "1", "img1", "labels",
                           "labels.zarr" * Cecelia.STORE_SUPERSEDED_SUFFIX)
            mkpath(sup); write(joinpath(sup, ".zgroup"), "{}")
            # a notebook with the uid hardcoded — the one place copy/reidentify must rewrite
            mkpath(joinpath(proj.root, "notebooks"))
            write(joinpath(proj.root, "notebooks", "nb.jl"), "proj = load_project(\"$uid\")\n")

            out    = joinpath(tmp, "exports")
            bundle = export_project(uid; out_dir = out)
            @test !isempty(bundle) && isdir(bundle)
            @test isfile(joinpath(bundle, "ccbundle.json"))
            @test isfile(joinpath(bundle, "0", "img1", "data.ome.zarr.tar"))
            @test isfile(joinpath(bundle, "1", "img1", "labels", "labels.zarr.tar"))
            @test !any(endswith(n, ".zarr") for (_, ds, _) in walkdir(bundle) for n in ds)  # packed, not unpacked
            @test !ispath(joinpath(bundle, ".cecelia.lock"))                                # lock skipped
            # staging debris excluded entirely — neither packed nor mirrored as loose chunks
            @test !any(any(s -> endswith(n, s), Cecelia.STORE_TMP_SUFFIXES)
                       for (_, ds, _) in walkdir(bundle) for n in ds)
            @test !occursin("half-written",
                            join((read(joinpath(r, f), String) for (r, _, fs) in walkdir(bundle)
                                  for f in fs if filesize(joinpath(r, f)) < 4096), "\n"))
            @test isempty(export_project(uid; out_dir = out))                               # refuses existing bundle

            rm(proj.root; recursive = true)                                                 # drop, then restore from bundle
            @test import_project(bundle) == uid
            tgt = joinpath(projroot, uid)
            @test read(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"
            @test read(joinpath(tgt, "1", "img1", "labelProps", "base.h5ad"), String) == "hdf-bytes"
            @test !any(endswith(n, ".tar") for (_, _, fs) in walkdir(tgt) for n in fs)       # no leftover .tar
            @test isempty(import_project(bundle))                                           # default: refuses existing

            # bundle_info reports the collision the UI prompts on
            bi = Cecelia.bundle_info(bundle)
            @test bi.uid == uid && bi.exists == true

            # copy: new uid, both kept, name suffixed, stores intact
            copy_uid = import_project(bundle; mode = "copy")
            @test copy_uid != uid && !isempty(copy_uid)
            @test isdir(joinpath(projroot, uid)) && isdir(joinpath(projroot, copy_uid))     # both present
            cj = JSON3.read(read(joinpath(projroot, copy_uid, "project.json"), String))
            @test String(cj.uid) == copy_uid && endswith(String(cj.name), "(imported)")
            @test read(joinpath(projroot, copy_uid, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"
            # copy re-identified the notebook's hardcoded load_project (best-effort)
            @test occursin("load_project(\"$copy_uid\")",
                           read(joinpath(projroot, copy_uid, "notebooks", "nb.jl"), String))

            # replace: overwrite the existing uid in place (mutate a file first to prove it's rewritten)
            write(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), "STALE")
            @test import_project(bundle; mode = "replace") == uid
            @test read(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"

            # reidentify_project!: rename in place → dir + project.json + notebook re-identified
            rid = reidentify_project!(uid, "riTEST9")
            @test rid == "riTEST9"
            @test !ispath(joinpath(projroot, uid)) && isdir(joinpath(projroot, "riTEST9"))
            @test String(JSON3.read(read(joinpath(projroot, "riTEST9", "project.json"), String)).uid) == "riTEST9"
            @test occursin("load_project(\"riTEST9\")",
                           read(joinpath(projroot, "riTEST9", "notebooks", "nb.jl"), String))
            @test load_project("riTEST9").uid == "riTEST9"                                   # loads under the new id
        finally
            prev_env === nothing ? delete!(ENV, "CECELIA_DEV_DIR") :
                                   (ENV["CECELIA_DEV_DIR"] = prev_env)
            init_cecelia!()
        end
    end
end

# The tar invocation must never hand a Windows drive letter to `-f`. GNU tar reads an archive
# path as `host:path` when a colon precedes any separator, so `-f D:\a\...\x.tar` becomes a
# connection attempt to host `D` and the pack silently produces nothing — which is how `.ccbundle`
# export (the backup mechanism) came to be broken on Windows while every unix path was fine: a unix
# absolute path can NEVER trigger it, because the leading `/` comes before the colon.
#
# That asymmetry is exactly why this needs a PROPERTY test rather than a behaviour test. Running
# tar here would pass on unix no matter what the code does — a test that cannot fail. So assert the
# shape of the command instead: `-f` takes a bare filename, the directory rides on the cwd. Both
# assertions fail against the old absolute-`-f` form on every platform.
@testset "tar commands keep drive letters out of -f" begin
    pack = Cecelia._tar_pack_cmd(joinpath("D:", "a", "out", "store.zarr.tar"),
                                 joinpath("D:", "proj", "0", "img", "store.zarr"))
    # the flag is the combined `-cf`, so the archive is the token after it
    fidx = findfirst(==("-cf"), pack.exec)
    @test fidx !== nothing
    farg = pack.exec[fidx + 1]
    @test farg == "store.zarr.tar"                      # bare filename…
    @test !occursin(':', farg)                          # …so no drive letter can reach tar
    @test !occursin('/', farg) && !occursin('\\', farg)
    @test pack.dir == joinpath("D:", "a", "out")        # the directory rides on the cwd instead
    # -C is NOT subject to the host:path parse, so it stays absolute
    cidx = findfirst(==("-C"), pack.exec)
    @test cidx !== nothing && pack.exec[cidx + 1] == joinpath("D:", "proj", "0", "img")
    @test pack.exec[end] == "store.zarr"                # the member, relative to -C

    unpack = Cecelia._tar_unpack_cmd(joinpath("D:", "a", "bundle", "0", "img", "store.zarr.tar"))
    uidx = findfirst(==("-xf"), unpack.exec)
    @test uidx !== nothing && unpack.exec[uidx + 1] == "store.zarr.tar"
    @test !occursin(':', unpack.exec[uidx + 1])
    @test unpack.dir == joinpath("D:", "a", "bundle", "0", "img")

    # Plugin install uses the SAME builders in their gzip form, and the drive-letter trap does not
    # care which flag it rode in on — so assert it there too, or the property holds only where it
    # was already tested. Compression comes off the extension, not a caller flag.
    gzpack = Cecelia._tar_pack_cmd(joinpath("C:", "tmp", "p.tar.gz"),
                                   joinpath("C:", "work", "ccia-importTracks-main"))
    gidx = findfirst(==("-czf"), gzpack.exec)
    @test gidx !== nothing && gzpack.exec[gidx + 1] == "p.tar.gz"
    @test !occursin(':', gzpack.exec[gidx + 1])
    @test gzpack.dir == joinpath("C:", "tmp")

    # `into` extracts somewhere other than the archive's own directory. It rides on -C, which is NOT
    # parsed as host:path, so it stays absolute — that is the whole point of the split.
    gzun = Cecelia._tar_unpack_cmd(joinpath("C:", "tmp", "p.tar.gz"); into = joinpath("C:", "out", "payload"))
    xidx = findfirst(==("-xzf"), gzun.exec)
    @test xidx !== nothing && gzun.exec[xidx + 1] == "p.tar.gz"
    @test !occursin(':', gzun.exec[xidx + 1])
    cidx2 = findfirst(==("-C"), gzun.exec)
    @test cidx2 !== nothing && gzun.exec[cidx2 + 1] == joinpath("C:", "out", "payload")
    @test gzun.dir == joinpath("C:", "tmp")
end
