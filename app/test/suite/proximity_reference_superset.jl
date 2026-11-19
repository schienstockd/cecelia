# RESULT-REVIEW: result-390e561b
# An explicit reference that does not contain the counted cells (another segmentation, or a sibling
# population) gave band shares above 1 and a confident enrichment. The task now adds every counted cell
# to the reference, so by definition a share is at most 1 and a band's total holds its counted cells.

@testset "proximity — counted cells are always in the reference (result-390e561b)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "proximity reference superset (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid = "KDIeEm", dir = td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        full = label_props(img; value_name = "B") |> select_cols(["mean_intensity_0"]) |> as_df
        v = sort(full.mean_intensity_0); t1 = v[cld(nrow(full), 3)]; t2 = v[cld(2 * nrow(full), 3)]
        m = PopulationMap(pop_type = "flow", value_name = "B")
        band(lo, hi) = RectangleGate("mean_intensity_0", "mean_intensity_1", lo, hi, -1e12, 1e12)
        add_pop!(m, "lo"; gate = band(-1e12, t1)); add_pop!(m, "mid"; gate = band(t1, t2))
        add_pop!(m, "hi"; gate = band(t2, 1e12))
        save_pop_map!(m, img)
        # reference = "lo" only, while "mid" is counted: disjoint populations
        res = Cecelia._run_task(ProximityProfile(), img, Dict{String,Any}(
            "structurePops" => ["B/hi"], "pops" => ["B/mid"], "referencePops" => ["B/lo"],
            "bandWidth" => 10.0, "nBands" => 2); on_log = _ -> nothing)
        @test res !== nothing
        recs = only(Cecelia._collect_proximity_stats(img)).records
        @test all(r -> r[:fraction] === nothing || r[:fraction] <= 1, recs)
        @test all(r -> r[:imageFraction] <= 1, recs)
        @test all(r -> r[:n] <= r[:nBand], recs)
        rm(td; recursive = true)
    end
end
