# RESULT-REVIEW: result-783bc24a
#
# Once the calibration sync reaches the store-root OME-XML through a series path, a unit the Julia map
# didn't know was written verbatim: a millisecond timelapse got `TimeIncrementUnit="millisecond"`,
# which is no OME `UnitsTime` symbol. `ome_types` then parses nothing, so `load_ome_xml` returns None
# — the image loses its pixel unit (nm read as µm, 1000x) and its frame interval, with no error.
@testset "RESULT-REVIEW result-783bc24a: synced OME-XML units stay valid OME symbols" begin
    mktempdir() do d
        mkpath(joinpath(d, "0")); mkpath(joinpath(d, "OME"))
        xml_file = joinpath(d, "OME", "METADATA.ome.xml")
        write(xml_file, "<OME><Image><Pixels SizeX=\"4\" PhysicalSizeX=\"0.1\" PhysicalSizeXUnit=\"nm\">" *
                        "<Plane/></Pixels></Image></OME>")
        Cecelia.update_ome_xml_pixels!(joinpath(d, "0"), Dict{String,String}())   # no-op
        Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
            "TimeIncrement" => 500.0, "TimeIncrementUnit" => "millisecond"))
        xml = read(xml_file, String)
        @test occursin("TimeIncrementUnit=\"ms\"", xml)
        @test !occursin("millisecond", xml)
        @test occursin("PhysicalSizeXUnit=\"nm\"", xml)                 # untouched

        # A unit with no OME symbol leaves its value out rather than write it in the wrong unit
        write(xml_file, "<OME><Image><Pixels SizeX=\"4\"><Plane/></Pixels></Image></OME>")
        Cecelia.sync_zarr_calibration!(joinpath(d, "0"), Dict{String,Any}(
            "TimeIncrement" => 2.0, "TimeIncrementUnit" => "fortnight",
            "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "furlong"))
        xml = read(xml_file, String)
        @test !occursin("TimeIncrement", xml) && !occursin("fortnight", xml)
        @test !occursin("PhysicalSizeZ", xml) && !occursin("furlong", xml)
    end
    # Every spelling analysis converts (`_LENGTH_UNIT_UM` / `_TIME_UNIT_S`) gets the symbol analysis
    # reads it as — case-insensitively, so "MS" and "Ms" are ms — and an OME symbol analysis doesn't
    # convert is kept exactly.
    for (u, sym) in (("um", "µm"), ("micron", "µm"), ("Micrometer", "µm"), ("sec", "s"),
                     ("Second", "s"), ("NM", "nm"), ("MS", "ms"), ("Ms", "ms"), ("Min", "min"),
                     ("H", "h"), ("µs", "µs"), ("ns", "ns"), ("cm", "cm"))
        @test Cecelia.ome_xml_unit_name(u) == sym
    end
    @test isnothing(Cecelia.ome_xml_unit_name("furlong"))
    # ...and states the same scale analysis computes with
    for u in keys(Cecelia._LENGTH_UNIT_UM)
        @test Cecelia.length_unit_um(Cecelia.ome_xml_unit_name(u)) == Cecelia.length_unit_um(u)
        @test Cecelia.length_unit_um(Cecelia.ome_xml_unit_name(uppercase(u))) == Cecelia.length_unit_um(u)
    end
    for u in keys(Cecelia._TIME_UNIT_S)
        @test Cecelia.time_unit_seconds(Cecelia.ome_xml_unit_name(u)) == Cecelia.time_unit_seconds(u)
        @test Cecelia.time_unit_seconds(Cecelia.ome_xml_unit_name(uppercase(u))) == Cecelia.time_unit_seconds(u)
    end
    for (name, sym) in Cecelia._OME_XML_UNIT
        @test sym in Cecelia._OME_UNIT_SYMBOLS
    end
end
