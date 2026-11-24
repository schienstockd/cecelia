# RESULT-REVIEW: result-9ce0a43d
# "Channel order" grouping on the PhenoCycler stack [DAPI, ATTO550, AF750, AF647, ATTO550, AF750, AF647]:
# i % 3 from channel 0 put DAPI with the AF647 markers (its nuclear signal became their background) and
# left ATTO550 / AF750 with 2 members each — uncorrected, silently.

@testset "afCells order mode: a nuclear stain does not join a marker dye" begin
    names = ["DAPI", "CD4", "CD8", "CD3", "CD20", "CD68", "CD45"]
    dyes  = ["DAPI", "ATTO550", "AF750", "AF647", "ATTO550", "AF750", "AF647"]
    g = Cecelia.af_order_groups(names; period = 3)
    @test g[1] === nothing
    for i in 2:7, j in 2:7
        @test (g[i] == g[j]) == (dyes[i] == dyes[j])     # grouped together ⇔ same dye
    end
end
