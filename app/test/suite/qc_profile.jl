# Depth / time findings from the import profile (app/src/qc/profile.jl), on real profiles from two
# FVMPE-RS two-photon movies of one session (test-data/acquisition/profile_fvmpe_session.json):
# one ran to the end and fades over time, one stopped early with CH2/CH3 clipped mostly deep.

@testset "import profile: depth and time findings" begin
    doc = JSON3.read(read(normpath(joinpath(test_projects_dir(), "..", "acquisition",
                                            "profile_fvmpe_session.json")), String))
    meta_of(name) = Dict{String,Any}("saturation" => doc["images"][name])
    codes(fs) = sort([f["code"] for f in fs])

    fading = meta_of("movie_full_fading")
    tr = Cecelia.profile_trends(fading)
    @test length(tr) == 4
    fade = Dict(t["channel"] => t["fade"] for t in tr)
    @test isapprox(fade[1], 0.428; atol = 1e-3)
    @test isapprox(fade[2], 0.456; atol = 1e-3)
    @test fade[0] < Cecelia._FADE_OVER_TIME && fade[3] < Cecelia._FADE_OVER_TIME
    fs = Cecelia.profile_qc_findings(fading)
    sf = only(f for f in fs if f["code"] == "import.signal_fades")
    @test sf["level"] == "info"
    @test sf["detail"]["channels"] == [1, 2]                 # only the channels past the alarm
    @test sf["detail"]["worstPct"] == 46.0
    @test !("import.clipped_deep" in codes(fs))              # nothing clipped in this movie

    clipped = meta_of("movie_stopped_early_clipped")
    cs = Cecelia.profile_qc_findings(clipped)
    @test codes(cs) == ["import.background_rises", "import.brighter_with_depth", "import.clipped_deep"]
    deep = only(f for f in cs if f["code"] == "import.clipped_deep")
    @test deep["detail"]["channels"] == [1, 2]               # the two saturated channels
    @test only(f for f in cs if f["code"] == "import.brighter_with_depth")["detail"]["channels"] == [1, 2]
    @test all(f -> f["level"] == "info", cs)                 # a measurement, never a warning

    m = Cecelia.profile_metrics(clipped)
    @test m["maxDepthChange"] ≈ 0.58 atol = 0.01             # signed: brighter with depth
    @test haskey(m, "maxSignalFade")

    # darker with depth: the same series, mirrored — the alarm is symmetric
    mirrored = deepcopy(Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
        Dict{String,Any}("index" => 0, "saturated" => false, "profile" => Dict{String,Any}(
            "byZ" => Dict{String,Any}("p99" => reverse(collect(doc["images"]["movie_stopped_early_clipped"]["channels"][2]["profile"]["byZ"]["p99"])),
                                      "p50" => fill(100.0, 27), "clipped" => zeros(Int, 27))))])))
    @test codes(Cecelia.profile_qc_findings(mirrored)) == ["import.darker_with_depth"]

    # no profile (probed before it existed), or too few planes / frames: say nothing
    @test isempty(Cecelia.profile_qc_findings(Dict{String,Any}()))
    @test Cecelia.profile_metrics(Dict{String,Any}()) === nothing
    short = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
        Dict{String,Any}("index" => 0, "profile" => Dict{String,Any}(
            "byT" => Dict{String,Any}("p99" => [100, 10], "p50" => [5, 50])))]))
    @test isempty(Cecelia.profile_qc_findings(short))

    # every code renders: placeholders filled, nothing left in braces
    for f in vcat(fs, cs)
        @test !occursin('{', f["short"]) && !occursin('{', f["long"])
    end
end
