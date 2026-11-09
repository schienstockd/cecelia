# ── A kept stopped-mid-volume last timepoint, as drift correction reads it ───────────────────────
# `kept_partial_timepoint` (omezarr/partial_timepoint.jl) is what drift_correct.jl passes its runner as
# `partialTimepoint`; the Python side is python/cecelia/tests/test_drift_partial_timepoint.py.
@testset "kept_partial_timepoint — only a timepoint still in the store" begin
    pt(trimmed) = Dict{String,Any}("partialTimepoint" => Dict{String,Any}(
        "t" => 19, "emptyZ" => collect(14:25), "sizeZ" => 26, "trimmed" => trimmed))
    @test Cecelia.kept_partial_timepoint(pt(false)) == 19
    @test isnothing(Cecelia.kept_partial_timepoint(pt(true)))
    @test isnothing(Cecelia.kept_partial_timepoint(Dict{String,Any}()))
    @test isnothing(Cecelia.kept_partial_timepoint(Dict{String,Any}("partialTimepoint" => nothing)))
    # as drift_correct.jl reads it: through read_ccid_raw, nested values still JSON3 objects
    mktempdir() do d
        f = joinpath(d, "ccid.json")
        write(f, Cecelia.JSON3.write(Dict("meta" => pt(false))))
        @test Cecelia.kept_partial_timepoint(Cecelia.read_ccid_raw(f)["meta"]) == 19
    end
end
