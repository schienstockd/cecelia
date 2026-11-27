# RESULT-REVIEW: result-db28f8e6
# A Cluster tracks run on a population's share where one image (or segmentation) of the run has no
# population at that path: its tracks' shares are missing, and the clustering's normalisation turns a
# missing value into 0 — those tracks would cluster with the ones that truly have 0 % in it, with no
# error. The shares must report the gap (`add_track_pop_fractions!` → problems) so the run refuses.

@testset "population shares: an image without the population is a gap, not 0 %" begin
    fx = fixture_for(:cells, :movie, :tracked)
    if fx === nothing
        @test_skip "tracked movie fixture (missing)"
    else
        td = mktempdir(); cp(fx.project, joinpath(td, basename(fx.project)))
        old = get(Cecelia.cecelia_conf()["dirs"], "projects", nothing)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = td
            img = init_object(basename(fx.project), fx.uid); vn = fx.value_name
            tid = Cecelia.as_df(Cecelia.select_cols(Cecelia.label_props(img; value_name = vn), ["track_id"])).track_id
            t1 = Int(first(t for t in tid if t isa Real && isfinite(t) && t > 0))
            tok = Cecelia.pop_fraction_token("/qc/TOM+")              # no such population on this image
            pooled = DataFrame(uID = [img.uid], value_name = [vn], label = [t1])
            gaps = Cecelia.add_track_pop_fractions!(pooled, [img], [tok])
            @test gaps == ["$(img.uid)/$vn has no /qc/TOM+"]
            @test isnan(pooled[1, tok])
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
        end
    end
end
