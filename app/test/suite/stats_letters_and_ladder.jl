# Between-group stats (app/src/plotting/stats.jl): the parts a plot shows that no other test reads.
# Expected values come from the definitions, not from the code: the star ladder from
# docs/todo/STATS_ANNOTATIONS_PLAN.md → S0-1, the compact letter display from its defining property
# (Piepho 2004: two groups share a letter iff their pair is not significant), the letter names and
# the replicate floor from their docstrings.

@testset "significance stars follow the S0-1 ladder, boundaries included" begin
    # ns / * / ** / *** / **** at p > 0.05 / ≤ 0.05 / ≤ 0.01 / ≤ 0.001 / ≤ 0.0001
    for (p, s) in [(0.2, "ns"), (0.05, "*"), (0.02, "*"), (0.01, "**"), (0.005, "**"),
                   (0.001, "***"), (0.0005, "***"), (0.0001, "****"), (0.00001, "****")]
        @test Cecelia._significance(p) == s
    end
    @test Cecelia._significance(NaN) == ""
end

@testset "compact letters: a shared letter means not significantly different" begin
    shares(a, b) = !isempty(intersect(Set(a), Set(b)))
    # pairs as `_pairwise` returns them: (a, b, p_adj, significance)
    pairs_for(labels, sig) = [(labels[i], labels[j], sig[i, j] ? 0.01 : 0.5, "")
                              for i in eachindex(labels) for j in i+1:length(labels)]
    function check(labels, sig)
        letters = Cecelia._cld_letters(labels, pairs_for(labels, sig))
        @test all(!isempty, letters)                          # every group carries a letter
        for i in eachindex(labels), j in i+1:length(labels)
            @test shares(letters[i], letters[j]) == !sig[i, j]
        end
        letters
    end
    sym(n, ps) = (m = falses(n, n); for (i, j) in ps; m[i, j] = m[j, i] = true; end; m)

    # nothing significant → one shared letter
    @test check(["A", "B", "C"], sym(3, [])) == ["a", "a", "a"]
    # only A–C differ → the textbook a / ab / b
    @test check(["A", "B", "C"], sym(3, [(1, 3)])) == ["a", "ab", "b"]
    # everything differs → one letter each
    @test check(["A", "B", "C", "D"], sym(4, [(i, j) for i in 1:4 for j in i+1:4])) == ["a", "b", "c", "d"]
    # every significance pattern over five groups, by the property alone
    labels = ["G$i" for i in 1:5]
    ps = [(i, j) for i in 1:5 for j in i+1:5]
    for mask in 0:(2^length(ps) - 1)
        check(labels, sym(5, [ps[b] for b in eachindex(ps) if (mask >> (b - 1)) & 1 == 1]))
    end
    # alpha is inclusive (p ≤ 0.05 is significant, as on the ladder)
    @test Cecelia._cld_letters(["A", "B"], [("A", "B", 0.05, "*")]) == ["a", "b"]
    # no pairs → no letters
    @test Cecelia._cld_letters(["A", "B"], Tuple{String,String,Float64,String}[]) == ["", ""]
end

@testset "compact letter names: 1 → a, 26 → z, 27 → aa" begin
    @test [Cecelia._cld_letter(k) for k in (1, 2, 26, 27, 28, 52, 53)] ==
          ["a", "b", "z", "aa", "ab", "az", "ba"]
end

@testset "replicate floor: the smallest group's BEST pair, and alpha is reachable" begin
    # 3 groups, smallest = 3 images: vs 10 images 2/C(13,3)·3 ≈ 0.021 ≤ 0.05, so a bracket CAN
    # reach alpha even though its pair with the other 3-image group cannot (2/C(6,3)·3 = 0.3)
    @test isempty(Cecelia.stats_qc_findings([3, 10, 3], [3, 10, 3]))
    # a floor of exactly alpha (2/C(40,1) = 0.05) can still be reached: no warning
    @test isempty(Cecelia.stats_qc_findings([1, 39], [1, 39]))
end
