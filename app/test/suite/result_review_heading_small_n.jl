# RESULT-REVIEW: result-8b6c3140
# Below 6 headings the Rayleigh series p (and the critical ring solved from it) is off the exact test:
# its 5% critical R̄ is 0.957 at n = 3 (true 0.940) and 0.837 at n = 4 (true 0.845), so a mean vector
# could be drawn inside the ring yet be significant, or past it yet not be. The true values come from
# the definition — R̄ of n uniform angles — by simulation, not from the code under test.

using Random: MersenneTwister

@testset "too few headings is not assessed; from the floor on the ring is the true 5% point" begin
    for n in 3:(HEADING_MIN_TRACKS - 1)
        h = heading_test(collect(range(0.0, 0.5; length = n)))
        @test isnan(h.p) && isnan(h.critR) && isnan(h.R)
        @test isnan(rayleigh_critical_R(n))
    end
    # at the floor: the ring matches the simulated 5% point of R̄ under uniformity
    rng = MersenneTwister(11)
    n = HEADING_MIN_TRACKS
    Rs = sort!([begin th = 2π .* rand(rng, n); hypot(sum(cos, th), sum(sin, th)) / n end for _ in 1:200_000])
    true_crit = Rs[ceil(Int, 0.95 * length(Rs))]
    @test isapprox(rayleigh_critical_R(n), true_crit; atol = 0.005)
end
