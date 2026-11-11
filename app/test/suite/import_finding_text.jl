# Import QC advice has to fit what the metadata can tell: gain advice only where the clipping can be
# the detector's, and a depth power ramp named only where the settings say one ran
# (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md → Open decisions, item 4).

@testset "import finding text fits the data" begin
    sat(bits, top; extra...) = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
        Dict{String,Any}("index" => 0, "saturated" => true, "topValue" => top, "topCount" => 900,
                         "clippedSignalFrac" => 0.03, "containerBits" => bits)]), (String(k) => v for (k, v) in extra)...)
    long_of(meta) = only(Cecelia.saturation_qc_findings(meta))["long"]
    gain = Cecelia.QC_TEXT["import.channel_saturated"].long

    @testset "8-bit export: no gain advice" begin
        # an 8-bit file at 255 with no settings read (an Immunemap / Fiji export)
        f = only(Cecelia.saturation_qc_findings(sat(8, 255)))
        @test f["code"] == "import.channel_saturated"           # the stored contract is unchanged
        @test f["key"] == "import.channel_saturated_export"
        @test f["detail"]["exportCeiling"] === true
        @test occursin("original", f["long"]) && !occursin("gain", f["long"])
        # settings read, but the detector's bits recorded as 12: rescaled on export
        acq12 = Dict{String,Any}("source" => "oir", "channels" => [Dict{String,Any}("order" => 1, "bitDepth" => 12)])
        @test long_of(sat(8, 255; acquisition = acq12)) != gain
        # an ims reader records no detector bits: still an export
        acqims = Dict{String,Any}("source" => "ims", "channels" => [Dict{String,Any}("order" => 1, "name" => "Ch1")])
        @test long_of(sat(8, 255; acquisition = acqims)) != gain
    end

    @testset "detector ceiling: gain advice stays" begin
        @test long_of(sat(16, 4095)) == gain                     # 12-bit sensor in 16-bit words
        @test long_of(sat(8, 200)) == gain                       # below the file's ceiling
        acq8 = Dict{String,Any}("source" => "czi", "channels" => [Dict{String,Any}("order" => 1, "bitDepth" => 8)])
        @test long_of(sat(8, 255; acquisition = acq8)) == gain   # an LSM that acquired in 8 bits
    end

    # a channel brighter deeper with every clipped voxel in the deeper half: fires brighter_with_depth
    # and clipped_deep
    deep = Dict{String,Any}("index" => 0, "saturated" => true, "profile" => Dict{String,Any}(
        "byZ" => Dict{String,Any}("p99" => collect(100.0:10.0:250.0), "p50" => fill(10.0, 16),
                                  "clipped" => vcat(zeros(Int, 8), fill(50, 8)))))
    depth_meta(; extra...) = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [deep]), (String(k) => v for (k, v) in extra)...)
    longs(meta) = Dict(f["code"] => f["long"] for f in Cecelia.profile_qc_findings(meta))
    named(meta) = [c for (c, l) in longs(meta) if occursin("Bright Z", l)]
    DEPTH = ["import.brighter_with_depth", "import.clipped_deep"]

    @testset "depth findings name Bright Z only when it ran" begin
        @test issubset(DEPTH, keys(longs(depth_meta())))
        @test isempty(named(depth_meta()))                                       # no settings read
        @test isempty(named(depth_meta(acquisitionMissing = Dict("reason" => "format", "ext" => ".tif"))))
        @test isempty(named(depth_meta(acquisition = Dict{String,Any}("source" => "ims", "channels" => []))))
        off = Dict{String,Any}("source" => "oir", "brightZ" => Dict{String,Any}("enabled" => false, "points" => []))
        @test isempty(named(depth_meta(acquisition = off)))                      # recorded, switched off
        czi = Dict{String,Any}("source" => "czi", "brightZ" => Dict{String,Any}("enabled" => true, "points" => []))
        @test isempty(named(depth_meta(acquisition = czi)))                      # Zeiss calls it something else
        on = Dict{String,Any}("source" => "oir", "brightZ" => Dict{String,Any}("enabled" => true, "points" => []))
        @test sort(named(depth_meta(acquisition = on))) == DEPTH
        # darker with depth follows the same rule
        dark = deepcopy(deep); dark["profile"]["byZ"]["p99"] = reverse(dark["profile"]["byZ"]["p99"])
        dmeta(; extra...) = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [dark]), (String(k) => v for (k, v) in extra)...)
        @test !occursin("Bright Z", longs(dmeta())["import.darker_with_depth"])
        @test occursin("Bright Z", longs(dmeta(acquisition = on))["import.darker_with_depth"])
    end

    # "Bright Z" is the power-stage name on every two-photon microscope the acquisition knowledge maps
    @testset "Bright Z naming matches the scopes map" begin
        dir = normpath(joinpath(@__DIR__, "..", "..", "..", "mcp", "cecelia_mcp", "acquisition_knowledge", "scopes"))
        scopes = [JSON3.read(read(joinpath(dir, f), String)) for f in readdir(dir) if endswith(f, ".json")]
        tp = [s for s in scopes if s[:technology] == "two-photon"]
        power = [f[:name] for s in tp for f in s[:features] if f[:stage] == "power"]
        @test !isempty(power) && all(==("Bright Z"), power)
        @test Set(String(s[:system]) for s in tp) ⊆ Set(["FV4000", "FVMPE-RS"])  # all Evident `.oir`
    end
end

# The tasks downstream of import that report clipping follow the same rule: denoise's saturation gate
# and AF correction's saturated input give no gain advice on a channel at an 8-bit export's ceiling.
@testset "denoise / AF clipping text on 8-bit exports" begin
    probe(bits, top) = Dict{String,Any}("index" => 0, "saturated" => true, "topValue" => top,
                                        "clippedSignalFrac" => 0.03, "containerBits" => bits)
    img_meta(chs...) = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => collect(chs)))
    exported = Cecelia.export_ceiling_channels(img_meta(probe(8, 255)))
    @test exported == [0]
    @test isempty(Cecelia.export_ceiling_channels(img_meta(probe(16, 4095))))
    # JSON3-read meta (Symbol keys), as the task handlers pass it
    @test Cecelia.export_ceiling_channels(JSON3.read(JSON3.write(img_meta(probe(8, 255))))) == [0]

    skipped = Dict{String,Any}("channelsSkipped" => [0], "mode" => "perChannel")
    dn(ex) = only(f for f in Cecelia._denoise_qc_findings(skipped; export_channels = ex)
                  if f["code"] == "denoise.channel_saturated")
    @test !occursin("gain", dn(exported)["long"]) && occursin("original", dn(exported)["long"])
    @test occursin("gain", dn(Int[])["long"])                    # detector ceiling: advice stays

    sat = Dict{String,Any}("0" => Dict{String,Any}("saturatedFrac" => 0.05, "levelsUsed" => 200,
                                                   "levelsAvailable" => 256))
    af(ex) = only(Cecelia.af_qc_findings(sat; export_channels = ex)[1])
    @test af(exported)["code"] == "af.saturated_input"
    @test !occursin("gain", af(exported)["long"]) && occursin("original", af(exported)["long"])
    @test occursin("gain", af(Int[])["long"])
end

# Clipping concentrated deep, at an 8-bit export's ceiling: no HV / ramp advice either.
@testset "clipped_deep on 8-bit exports" begin
    deep = Dict{String,Any}("index" => 0, "saturated" => true, "topValue" => 255, "containerBits" => 8,
                            "clippedSignalFrac" => 0.03, "profile" => Dict{String,Any}(
        "byZ" => Dict{String,Any}("p99" => fill(200.0, 16), "p50" => fill(10.0, 16),
                                  "clipped" => vcat(zeros(Int, 8), fill(50, 8)))))
    long(ch) = only(f for f in Cecelia.profile_qc_findings(Dict{String,Any}(
        "saturation" => Dict{String,Any}("channels" => [ch]))) if f["code"] == "import.clipped_deep")["long"]
    @test occursin("original", long(deep)) && !occursin("HV", long(deep))
    native = merge(deep, Dict{String,Any}("containerBits" => 16, "topValue" => 4095))
    @test occursin("HV", long(native))
end
