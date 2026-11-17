# RESULT-REVIEW: result-e8313f2f
# The Rayleigh p of a movie's headings must stay inside [0, 1]. HypothesisTests' `pvalue(::RayleighTest)`
# is a series expansion (the one `circular::rayleigh.test` uses) that goes slightly NEGATIVE as R̄ → 1 —
# about −2.9e-6 at R̄ = 1 with n = 10, −5.6e-5 near R̄ = 0.988 with n = 8 — and a negative p-value would
# reach the wire as `headings.movies[].p`. A tightly bunched movie is a real case: every cell leaving
# one way.

@testset "heading p-values stay in [0, 1] when every track goes one way" begin
    for th in (fill(0.3, 10), 0.3 .+ 1e-4 .* (1:10), 0.3 .+ 0.02 .* (1:8))
        h = heading_test(th)
        @test 0 <= h.p <= 1
        @test 0 <= h.axialP <= 1
        @test h.p < 1e-3                       # still unmistakably significant
    end
    # the series itself, for the record: this is what was being shipped unclamped
    @test Cecelia.pvalue(Cecelia.RayleighTest(1.0, 10)) < 0
end
