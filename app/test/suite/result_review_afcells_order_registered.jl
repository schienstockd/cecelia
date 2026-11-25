# RESULT-REVIEW: result-dfca6e0c
# "Channel order" on a stack built by `editImages.register` (default keepRegChannels=false): each cycle
# file is [ATTO550, AF750, AF647, Hoechst] and only the reference cycle keeps its Hoechst, so the stain
# sits once, mid-stack. Grouping by stack distance put every later marker one dye off.

@testset "afCells order mode: a mid-stack nuclear stain does not shift the cycle" begin
    cyc   = ["ATTO550", "AF750", "AF647"]
    names = vcat(["A1", "B1", "C1", "Hoechst"], vcat([["A$c", "B$c", "C$c"] for c in 2:6]...))
    dyes  = vcat(cyc, [nothing], repeat(cyc, 5))
    g = Cecelia.af_order_groups(names; period = 3)
    @test g[4] === nothing
    m = findall(!isnothing, dyes)
    for i in m, j in m
        @test (g[i] == g[j]) == (dyes[i] == dyes[j])     # grouped together ⇔ same dye
    end
end
