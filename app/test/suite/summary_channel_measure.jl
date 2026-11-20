# A channel measure on a summary plot (boxplot / violin / … of a marker per population) is read by its
# channel NAME on every image. `pop_df` renames intensity columns to names, so a raw request
# (`mean_intensity_1`) used to 400 "column not found", and a set whose images order their channels
# differently must plot the same marker on each — not whatever sits at the same index.
# Expected values come from the raw table, read directly, never from the code under test.
@testset "summary plot of a channel measure reads the channel by name" begin
    # a real cell table with several measured channels; the names are set here, so any will do
    fx = fixture_for(:cells, :real)
    h5 = fixture_table(fx); vn = fx.value_name
    begin
        mk = function (uid, chans)
            td = mktempdir(); mkpath(joinpath(td, "labelProps")); cp(h5, joinpath(td, "labelProps", basename(h5)))
            img = CciaImage(uid = uid, dir = td)
            img.label_props[vn] = basename(h5); img.label_props["_active"] = vn
            img.im_channel_names = Dict{String,Any}("default" => chans, "_active" => "default"); img
        end
        raw = Cecelia.as_df(Cecelia.label_props(h5))
        med(v) = (s = sort(collect(skipmissing(v))); n = length(s); isodd(n) ? s[(n + 1) ÷ 2] : (s[n ÷ 2] + s[n ÷ 2 + 1]) / 2)
        m0, m1 = med(raw[!, "mean_intensity_0"]), med(raw[!, "mean_intensity_1"])
        @test m0 != m1                                   # the swap below must be visible

        x = mk("uX", ["CD45", "CD4", "GFP", "TOM"])
        # by name, and by raw column (a notebook / a saved panel) — the same numbers
        for meas in ("CD4", "mean_intensity_1")
            r = plot_summary_data(x, "flow", ["root"], "boxplot"; value_name = vn, measure = meas)
            @test only(r["series"])["median"] ≈ m1
            @test only(r["series"])["n"] == nrow(raw)
        end

        # two images, channel order swapped: CD4 is column 1 on uX, column 0 on uY
        y = mk("uY", ["CD4", "CD45", "GFP", "TOM"])
        r = plot_summary_data([x, y], ["uX", "uY"], "flow", ["root"], "boxplot";
                              value_name = vn, measure = "CD4", scope = :per_image)
        by = Dict(s["uID"] => s["median"] for s in r["series"])
        @test by["uX"] ≈ m1
        @test by["uY"] ≈ m0
        # a raw request is named against the FIRST image, then resolved per image like a name
        r = plot_summary_data([x, y], ["uX", "uY"], "flow", ["root"], "boxplot";
                              value_name = vn, measure = "mean_intensity_1", scope = :per_image)
        by = Dict(s["uID"] => s["median"] for s in r["series"])
        @test by["uX"] ≈ m1 && by["uY"] ≈ m0

        # the target-list form (what the API calls) does the same
        r = plot_summary_data([x, y], ["uX", "uY"], "flow", [(vn, "root")], "boxplot";
                              measure = "CD4", scope = :per_image)
        by = Dict(s["uID"] => s["median"] for s in r["series"])
        @test by["uX"] ≈ m1 && by["uY"] ≈ m0
        r1 = plot_summary_data(x, "flow", [(vn, "root")], "boxplot"; measure = "mean_intensity_1")
        @test only(r1["series"])["median"] ≈ m1

        # a marker stays a NUMBER however few levels its values take (named, it is no longer in the raw
        # var list, and would otherwise fall to the categorical sniff)
        @test "CD4" in Cecelia._var_measure_set(x, vn)
        @test "mean_intensity_1" in Cecelia._var_measure_set(x, vn)

        # an image without the marker is an error naming it, not an empty box
        z = mk("uZ", ["CD45", "CD8", "GFP", "TOM"])
        @test_throws ArgumentError plot_summary_data([x, z], ["uX", "uZ"], "flow", ["root"], "boxplot";
                                                     value_name = vn, measure = "CD4")
    end
end
