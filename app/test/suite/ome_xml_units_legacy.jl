# ── OME metadata: per-axis spatial units + the legacy root-level OME-XML sidecar ──────────────
# Own file because ome_qc.jl is review-guarded. Synthetic temp stores (no real data): the functions
# under test are pure readers/writers over a bioformats2raw series layout.
@testset "OME metadata: mixed spatial units, legacy sidecar" begin
    # bioformats2raw series layout: multiscales in `<dir>/0/.zattrs`; OME-XML at `<dir>/<xml_rel>`
    function series_store(dir; axes, scale, units, xml_rel = joinpath("OME", "METADATA.ome.xml"))
        base = joinpath(dir, "0"); mkpath(base)
        ax_objs = map(axes) do a
            o = Dict{String,Any}("name" => a, "type" => a in ("x", "y", "z") ? "space" : "time")
            haskey(units, a) && (o["unit"] = units[a])
            o
        end
        zattrs = Dict{String,Any}("multiscales" => [Dict{String,Any}("axes" => ax_objs,
            "datasets" => [Dict{String,Any}("path" => "0", "coordinateTransformations" =>
                [Dict{String,Any}("type" => "scale", "scale" => scale)])])])
        open(io -> JSON3.write(io, zattrs), joinpath(base, ".zattrs"), "w")
        xml = joinpath(dir, xml_rel); mkpath(dirname(xml))
        write(xml, "<OME><Image><Pixels SizeX=\"4\"><Plane/></Pixels></Image></OME>")
        xml
    end
    zattr_units(dir) = Dict(string(a[:name]) => string(get(a, :unit, ""))
        for a in JSON3.read(read(joinpath(dir, "0", ".zattrs"), String))[:multiscales][1][:axes])
    xml_attr(xml, k) = (m = match(Regex(k * "=\"([^\"]*)\""), read(xml, String)); isnothing(m) ? nothing : m[1])

    # A z in a different unit from x/y: the reader states every PhysicalSize* in the ONE shared
    # `PhysicalSizeUnit`, so the sync that writes that unit onto all three axes (a timelapse import,
    # a resync) cannot relabel 2000 nm as 2000 µm in `.zattrs` and the root OME-XML.
    @testset "z in nm beside x/y in µm survives read → sync" begin
        mktempdir() do d
            xml = series_store(d; axes = ["z", "y", "x"], scale = [2000.0, 0.5, 0.5],
                               units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "nanometer"))
            m = read_ome_metadata(joinpath(d, "0"))
            @test m["PhysicalSizeUnit"] == "micrometer"
            @test m["PhysicalSizeZ"] ≈ 2.0                    # 2000 nm = 2 µm, by definition
            @test m["PhysicalSizeX"] == 0.5
            @test !haskey(m, "PhysicalSizeZUnit")
            Cecelia.sync_zarr_calibration!(joinpath(d, "0"), m)
            m2 = read_ome_metadata(joinpath(d, "0"))
            @test m2["PhysicalSizeZ"] ≈ 2.0 && m2["PhysicalSizeUnit"] == "micrometer"
            @test parse(Float64, xml_attr(xml, "PhysicalSizeZ")) ≈ 2.0
            @test xml_attr(xml, "PhysicalSizeZUnit") == "µm"
        end
    end

    # A unit with no known factor can't be converted: it keeps its value, names its own unit, and the
    # sync writes that unit back onto z rather than the shared one.
    @testset "an unconvertible z unit stays on z" begin
        mktempdir() do d
            series_store(d; axes = ["z", "y", "x"], scale = [3.0, 0.5, 0.5],
                         units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "parsec"))
            m = read_ome_metadata(joinpath(d, "0"))
            @test m["PhysicalSizeZ"] == 3.0 && m["PhysicalSizeZUnit"] == "parsec"
            Cecelia.sync_zarr_calibration!(joinpath(d, "0"), m)
            u = zattr_units(d)
            @test u["z"] == "parsec" && u["x"] == "micrometer"
        end
    end

    # A meta carrying a per-axis unit (resampleZ output, a ccid edit) syncs it onto its own axis.
    @testset "a per-axis unit in meta syncs onto its axis" begin
        mktempdir() do d
            xml = series_store(d; axes = ["z", "y", "x"], scale = [0.6, 0.5, 0.5],
                               units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
            Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
                "PhysicalSizeX" => 0.5, "PhysicalSizeZ" => 2000.0,
                "PhysicalSizeUnit" => "micrometer", "PhysicalSizeZUnit" => "nanometer"))
            @test zattr_units(d)["z"] == "nanometer"
            @test xml_attr(xml, "PhysicalSizeZUnit") == "nm"
            @test xml_attr(xml, "PhysicalSizeXUnit") == "µm"
            @test read_ome_metadata(joinpath(d, "0"))["PhysicalSizeZ"] ≈ 2.0   # read back in µm
        end
    end

    # OME's UnitsLength is an enumeration: an axis whose unit has no OME symbol is left out of the XML
    # (written bare, OME would read it in µm). Only THAT axis — the others still land with their unit.
    @testset "an axis unit with no OME symbol leaves only that axis out of the XML" begin
        mktempdir() do d
            xml = series_store(d; axes = ["z", "y", "x"], scale = [0.6, 0.5, 0.5],
                               units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
            @test_logs (:warn, r"no OME symbol") match_mode = :any Cecelia.sync_zarr_calibration!(
                joinpath(d, "0"), Dict{String,Any}(
                "PhysicalSizeX" => 0.25, "PhysicalSizeZ" => 3.0,
                "PhysicalSizeUnit" => "micrometer", "PhysicalSizeZUnit" => "parsec"))
            @test xml_attr(xml, "PhysicalSizeX") == "0.25" && xml_attr(xml, "PhysicalSizeXUnit") == "µm"
            @test isnothing(xml_attr(xml, "PhysicalSizeZ")) && isnothing(xml_attr(xml, "PhysicalSizeZUnit"))
            @test zattr_units(d)["z"] == "parsec"              # the NGFF half still lands
            # every unit has a symbol, or the unmapped axis carries no value: nothing left out, no warning
            @test_logs Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
                "PhysicalSizeX" => 0.25, "PhysicalSizeUnit" => "micrometer", "PhysicalSizeZUnit" => "parsec"))
            @test_logs Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
                "PhysicalSizeX" => 0.25, "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "micrometer"))
        end
        mktempdir() do d                                    # shared unit unknown to OME: no spatial XML
            xml = series_store(d; axes = ["y", "x"], scale = [0.5, 0.5], units = Dict{String,String}())
            Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
                "PhysicalSizeX" => 1.0, "PhysicalSizeY" => 1.0, "PhysicalSizeUnit" => "furlong",
                "TimeIncrement" => 5.0, "TimeIncrementUnit" => "second"))
            @test isnothing(xml_attr(xml, "PhysicalSizeX")) && isnothing(xml_attr(xml, "PhysicalSizeY"))
            @test xml_attr(xml, "TimeIncrement") == "5.0"
        end
    end

    # A derived image (crop, copy, bin, project, register, resample) copies an axis's value AND its own
    # unit — dropping the unit leaves the value read under the shared one.
    @testset "derived images keep a per-axis unit" begin
        src = Dict{String,Any}("PhysicalSizeX" => 0.5, "PhysicalSizeY" => 0.5, "PhysicalSizeZ" => 3.0,
                               "PhysicalSizeUnit" => "micrometer", "PhysicalSizeZUnit" => "parsec",
                               "SizeC" => 1, "SizeZ" => 10, "SizeT" => 1, "SizeX" => 100, "SizeY" => 100)
        box = (; x0 = 0, x1 = 50, y0 = 0, y1 = 50, z0 = -1, z1 = -1, t0 = -1, t1 = -1)
        for m in (Cecelia._crop_inherited_meta(src, box), Cecelia._tproj_inherited_meta(src),
                  Cecelia._bin_inherited_meta(src, 2, 2), Cecelia._register_inherited_meta(src, 1),
                  Cecelia._copied_meta(src, "abc", "default"))
            @test get(m, "PhysicalSizeZUnit", nothing) == "parsec"
        end
        @test Cecelia.AXIS_UNIT_META_KEYS == ("PhysicalSizeXUnit", "PhysicalSizeYUnit", "PhysicalSizeZUnit")
    end

    # The import's ImageJ Z fix measures Z in µm: stored in the shared unit (1 µm = 1000 nm), and a
    # per-axis Z unit the reader left for the raw value must not relabel it.
    @testset "ImageJ Z fix: a µm value in the shared unit, no stale Z unit" begin
        m = Dict{String,Any}("PhysicalSizeUnit" => "micrometer", "PhysicalSizeZ" => 3.0,
                             "PhysicalSizeZUnit" => "parsec")
        Cecelia._set_corrected_z_um!(m, 2.0)
        @test m["PhysicalSizeZ"] == 2.0 && !haskey(m, "PhysicalSizeZUnit")
        m = Dict{String,Any}("PhysicalSizeUnit" => "nanometer")
        @test Cecelia._set_corrected_z_um!(m, 2.0)["PhysicalSizeZ"] ≈ 2000.0
        m = Dict{String,Any}("PhysicalSizeUnit" => "furlong")    # no known factor: say µm explicitly
        Cecelia._set_corrected_z_um!(m, 2.0)
        @test m["PhysicalSizeZ"] == 2.0 && m["PhysicalSizeZUnit"] == "micrometer"
    end

    # A metadata edit sends a Z in the shared unit and no per-axis unit: the old Z unit must go, or it
    # relabels the typed value. An axis the edit didn't touch keeps its unit, in ccid and in the sync.
    @testset "metadata edit drops the edited axis's stale unit" begin
        meta = Dict{String,Any}("PhysicalSizeUnit" => "micrometer", "PhysicalSizeZ" => 3.0,
                                "PhysicalSizeZUnit" => "parsec", "PhysicalSizeYUnit" => "parsec",
                                "PhysicalSizeZ_raw" => 1.0)
        sync = Cecelia.apply_calibration_edit!(meta, Dict{String,Any}(
            "PhysicalSizeZ" => 2.0, "PhysicalSizeUnit" => "micrometer", "PhysicalSizeZ_raw" => nothing))
        @test meta["PhysicalSizeZ"] == 2.0 && !haskey(meta, "PhysicalSizeZUnit")
        @test !haskey(meta, "PhysicalSizeZ_raw")                   # a null still deletes
        @test meta["PhysicalSizeYUnit"] == "parsec" && sync["PhysicalSizeYUnit"] == "parsec"
        @test !haskey(sync, "PhysicalSizeZUnit")
        # an edit that carries its own Z unit keeps it
        Cecelia.apply_calibration_edit!(meta, Dict{String,Any}(
            "PhysicalSizeZ" => 500.0, "PhysicalSizeZUnit" => "nanometer"))
        @test meta["PhysicalSizeZUnit"] == "nanometer"
    end

    # The legacy root-level `METADATA.ome.xml` (no `OME/` dir): the same four candidates as Python's
    # `ome_xml_utils.load_ome_xml`, at the path and at its parent; `OME/` wins when both exist.
    @testset "_ome_xml_file finds the legacy root-level sidecar" begin
        mktempdir() do d
            legacy = series_store(d; axes = ["z", "y", "x"], scale = [0.6, 0.5, 0.5],
                                  units = Dict{String,String}(), xml_rel = "METADATA.ome.xml")
            @test Cecelia._ome_xml_file(d) == legacy
            @test Cecelia._ome_xml_file(joinpath(d, "0")) == legacy
            Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}("PhysicalSizeZ" => 3.0))
            @test xml_attr(legacy, "PhysicalSizeZ") == "3.0"
            mkpath(joinpath(d, "OME")); write(joinpath(d, "OME", "METADATA.ome.xml"), "<OME/>")
            @test Cecelia._ome_xml_file(d) == joinpath(d, "OME", "METADATA.ome.xml")
        end
    end
end
