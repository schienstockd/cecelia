# RESULT-REVIEW: result-fc458b9e
# A single counted population (or a parent covering the others) must still read its own distribution.
# With the baseline built from the picked populations alone, it was its own denominator and read
# log2 enrichment 0 in every band. The reference population is now the denominator.

@testset "proximity — one counted population reads its distribution (result-fc458b9e)" begin
    bands = Cecelia.proximity_bands(10, 2)
    # 100 CD8 cells: 90 near, 10 beyond. Reference = those plus 900 other cells: 100 near, 800 beyond.
    cd8 = Dict(("B", i) => (i <= 90 ? 1 : 3) for i in 1:100)
    reference = merge(cd8, Dict(("B", 1000 + i) => (i <= 100 ? 1 : 3) for i in 1:900))
    recs = Cecelia.proximity_records(["CD8"], Dict("CD8" => cd8), reference, bands)
    near = only(r for r in recs if r["band"] == "0–10 µm")
    far  = only(r for r in recs if r["band"] == ">20 µm")
    # by definition: band share 90/190 against image share 100/1000, with the 0.5 pseudocount
    @test near["log2Enrichment"] ≈ log2((90.5 / 190.5) / (100.5 / 1000.5))
    @test near["log2Enrichment"] > 2
    @test far["log2Enrichment"] ≈ log2((10.5 / 810.5) / (100.5 / 1000.5))
    @test far["log2Enrichment"] < 0
end
