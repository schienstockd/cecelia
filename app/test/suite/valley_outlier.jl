# RESULT-REVIEW: result-e199742d
# A valley rule on a linear axis with one far outlier (a debris object of huge area): the cut must stay in
# the dip between the two populations, not move into a tail or vanish. Expected cut = the true mixture's
# minimum between the means (equal halves → midway), not the code's output.
using Random: MersenneTwister

@testset "valley_cut: one far outlier does not move the cut" begin
    rng = MersenneTwister(3)
    v = [100 .+ 25 .* randn(rng, 5000); 300 .+ 25 .* randn(rng, 5000)]
    for far in (20_000.0, 50_000.0, 100_000.0)
        c = valley_cut([v; far], 150, 250)
        @test c !== nothing && abs(c - 200) < 10
    end
    w = [randn(rng, 4000); 6 .+ randn(rng, 4000)]
    for far in (1000.0, 5000.0)
        c = valley_cut([w; far], -10, 10)
        @test c !== nothing && abs(c - 3) < 0.3
    end
    # a window reaching out to the outlier still resolves the dip (the grid follows the bandwidth)
    c = valley_cut([w; 5000.0], -10, 6000)
    @test c !== nothing && abs(c - 3) < 0.3
end
