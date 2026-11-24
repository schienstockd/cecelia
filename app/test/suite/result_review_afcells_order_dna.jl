# RESULT-REVIEW: result-6b2c0b0e
# "Channel order" on a CyCIF/MCMICRO stack, Hoechst re-imaged every cycle and named `DNA_n`: the name match
# missed it, the stains counted as markers, and every group mixed all four dyes — and leaving them "as
# measured" could not help, because that never feeds the grouping. Nuclear stains are their own param;
# the name match is only its default, and it knows the `DNA_n` convention.

@testset "afCells order mode: per-cycle DNA_n stains, by name or named by the user" begin
    cyc   = ["Hoechst", "AF488", "AF555", "AF647"]
    names = vcat([["DNA_$c", "A$c", "B$c", "C$c"] for c in 1:6]...)
    dyes  = repeat(cyc, 6)
    m = findall(!=("Hoechst"), dyes)
    nuclear_named = findall(==("Hoechst"), dyes) .- 1
    for g in (Cecelia.af_order_groups(names; period = 3),                          # by name
              Cecelia.af_order_groups(replace.(names, "DNA_" => "Stain");          # an unknown name,
                                      period = 3, nuclear = nuclear_named))       # named by the user
        @test all(isnothing, g[dyes .== "Hoechst"])
        for i in m, j in m
            @test (g[i] == g[j]) == (dyes[i] == dyes[j])     # grouped together ⇔ same dye
        end
    end
end
