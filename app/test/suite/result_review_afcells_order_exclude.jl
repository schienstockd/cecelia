# RESULT-REVIEW: result-8205e86f
# "Channel order" grouping with a marker left as measured: dropping it from the count shifted every later
# channel into the next dye's group, and a non-empty exclude switched the DAPI default off. Excluding a
# marker must remove only that marker; the dye of every other channel follows the stack order.

@testset "afCells order mode: leaving a marker as measured does not shift the cycle" begin
    cyc   = ["ATTO550", "AF750", "AF647"]
    names = vcat(["DAPI"], ["M$i" for i in 1:18])
    dyes  = vcat([nothing], [cyc[(i - 1) % 3 + 1] for i in 1:18])
    g = Cecelia.af_order_groups(names; period = 3)       # the task never feeds exclude into grouping
    @test g[1] === nothing
    for i in 2:19, j in 2:19
        @test (g[i] == g[j]) == (dyes[i] == dyes[j])
    end
    # af_order_groups takes no exclude at all: the runner (cell_af_utils.dye_groups) drops an excluded
    # marker from its own group and nothing else
    @test !hasmethod(Cecelia.af_order_groups, Tuple{Vector{String}, Vector{Int}})
end
