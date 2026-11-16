# RESULT-REVIEW: result-fad1d2f7
# A dotted channel name (`CD45.2`) on an image that has only `CD45` must be reported missing — not read as
# channel `CD45` plus a made-up per-track aggregate `.2` (only `_TRACK_NUM_AGG_SUFFIXES` are aggregates),
# which left the pooled column empty on that image with no error. docs/todo/CHANNEL_NAMES_PLAN.md
@testset "a dotted channel name an image lacks is absent, not an aggregate of another channel" begin
    vars = ["mean_intensity_0", "mean_intensity_1", "area"]
    r = Cecelia.resolve_channel_columns(["CD45.2", "CD45.2.mean"], vars, ["CD45", "CD4"])
    @test r.cols == ["CD45.2", "CD45.2.mean"]                    # unresolved, not `mean_intensity_0.2`
    @test Cecelia._split_channel_column("mean_intensity_0.2") === nothing
    @test Cecelia._split_channel_column("mean_intensity_0.qUp").tail == ".qUp"
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "testpr fixture missing"
    else
        mk = function (uid, chans)
            td = mktempdir(); mkpath(joinpath(td, "labelProps")); cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid = uid, dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img.im_channel_names = Dict{String,Any}("default" => chans, "_active" => "default"); img
        end
        x, y = mk("uX", ["CD45.2", "CD4", "GFP", "TOM"]), mk("uY", ["CD45", "CD4", "GFP", "TOM"])
        ch = Cecelia.resolve_segment_channel_columns([(x, "B"), (y, "B")], ["mean_intensity_0"])
        @test ch.names == ["CD45.2"]
        @test only(ch.problems) == "uY/B: CD45.2 not on this image"
    end
end
