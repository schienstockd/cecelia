# RESULT-REVIEW: result-6b2c0b0e
# "Channel order" on a stack that re-images the nuclear stain every cycle ([DAPI, ATTO550, AF750, AF647]
# × 6): the param said "channel i ± this number" while the grouping counted markers, so a user following
# the tip (4) got every group mixing all three dyes. The param is *Markers per cycle* — 3 here.

@testset "afCells order mode: a per-cycle nuclear stain does not count as a marker" begin
    cyc   = ["DAPI", "ATTO550", "AF750", "AF647"]
    names = vcat([["DAPI (cycle $c)", "A$c", "B$c", "C$c"] for c in 1:6]...)
    dyes  = repeat(cyc, 6)
    g = Cecelia.af_order_groups(names; period = 3)
    @test all(isnothing, g[dyes .== "DAPI"])
    m = findall(!=("DAPI"), dyes)
    for i in m, j in m
        @test (g[i] == g[j]) == (dyes[i] == dyes[j])     # grouped together ⇔ same dye
    end
    spec = read(joinpath(@__DIR__, "..", "..", "src", "tasks", "segment", "af_cells.json"), String)
    @test occursin("Markers per cycle", spec)             # the param says what the grouping counts
end
