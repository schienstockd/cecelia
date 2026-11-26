# The population positions plot's read: every cell of a segmentation at its centroid (µm when calibrated),
# its frame, and the index of the deepest picked population it belongs to — a track family expanded to its
# cells in every frame. Expected values come from the raw table and the fixture's stated calibration.
@testset "pop_positions: every cell at its position, tagged by population" begin
    fx = fixture_for(:cells, :movie, :tracked, :calibrated)
    img = Cecelia._load_image(fx.dir); vn = fx.value_name
    raw = Cecelia.as_df(Cecelia.label_props(fixture_table(fx)))
    sx = parse(Float64, string(img.meta["PhysicalSizeX"])); sy = parse(Float64, string(img.meta["PhysicalSizeY"]))
    tracked = [x isa Real && isfinite(x) && x > 0 for x in raw.track_id]
    @test 0 < count(tracked) < length(tracked)      # both indices below must be visible

    r = pop_positions(img, vn, ["live~$vn/_tracked", "flow~root"])
    @test length(r.x) == nrow(raw) && r.label == Int.(raw.label)
    @test r.unit == SPATIAL_UNIT_UM
    @test r.x ≈ Float32.(raw.centroid_x .* sx)
    @test r.y ≈ Float32.(raw.centroid_y .* sy)
    @test r.t == Float32.(raw.centroid_t)            # a frame index, never scaled
    # tracked cells are 0, the rest fall to root (1); every row is this segmentation's
    @test r.pop == [t ? 0 : 1 for t in tracked]
    @test all(==(vn), r.value_name)
    # a parent picked BEFORE its child: the child still gets its cells (the deepest wins, not the first)
    @test pop_positions(img, vn, ["flow~root", "live~$vn/_tracked"]).pop == [t ? 1 : 0 for t in tracked]

    # a token that does not resolve contributes nothing; its index is still reserved
    r2 = pop_positions(img, vn, ["flow~/no such pop", "live~$vn/_tracked"])
    @test r2.pop == [t ? 1 : -1 for t in tracked]
    # colour by a cell column: each cell's own value, NaN where it has none; a column the table lacks,
    # or no colour-by, → NaN everywhere (the plot then draws populations only)
    want = Float32[(v isa Real && !ismissing(v)) ? Float32(v) : NaN32 for v in raw.track_id]
    got = pop_positions(img, vn, String[]; colour_by = "track_id").value
    @test isequal(got, want) && any(isnan, got) && any(isfinite, got)
    @test all(isnan, pop_positions(img, vn, String[]; colour_by = "spatial.lens.nosuch").value)
    @test all(isnan, r.value) && length(r.value) == length(r.x)
    # no populations → every cell in none (the grey background)
    @test all(==(-1), pop_positions(img, vn, String[]).pop)

    # populations on a segmentation this image lacks → nothing, not the active segmentation's cells
    @test isempty(pop_positions(img, "", ["flow~nosuchseg/qc"]).x)
    # a population this image does not define → nothing, not its segmentation's cells uncoloured
    @test isempty(pop_positions(img, "", ["flow~$vn/no such pop"]).x)
    # defined here but empty in membership still draws the segmentation: the root is always defined
    @test length(pop_positions(img, "", ["flow~$vn/root"]).x) == nrow(raw)

    # half-calibrated (y size missing) is NOT calibrated: pixels on both axes, under a px label
    sy_raw = img.meta["PhysicalSizeY"]
    try
        delete!(img.meta, "PhysicalSizeY")
        rp = pop_positions(img, vn, String[])
        @test rp.unit == SPATIAL_UNIT_PX
        @test rp.x ≈ Float32.(raw.centroid_x) && rp.y ≈ Float32.(raw.centroid_y)
    finally
        img.meta["PhysicalSizeY"] = sy_raw
    end
    # a segmentation with no cell table → empty, not an error
    @test isempty(pop_positions(img, "no-such-segmentation", String[]).x)
end

# A colour-by can name a MARKER (`CD4`, `nuc_CD4`, `af_CD4`), not just a result column: the feature is
# read by its channel name. Expected values are the raw intensity column the name resolves to.
@testset "colour by a marker: pop_positions reads it by name" begin
    fx = fixture_for(:cells, Symbol("marker-names"))
    if fx === nothing
        @test_skip "no fixture with marker names"
    else
        img = Cecelia._load_image(fx.dir); vn = fx.value_name
        lp  = Cecelia.label_props(img; value_name = vn)
        raw_cols = channel_columns(lp)
        names = something(channel_names(img; value_name = vn), String[])
        named = Cecelia.canonical_channel_columns(raw_cols, names)
        k = findfirst(i -> named[i] != raw_cols[i], eachindex(named))
        @test k !== nothing                                  # the fixture has a marker by name
        marker, raw = named[k], raw_cols[k]
        df = Cecelia.as_df(Cecelia.label_props(img; value_name = vn) |> v -> select_cols(v, [raw]))
        want = Float32[(v isa Real && !ismissing(v)) ? Float32(v) : NaN32 for v in df[!, raw]]
        got = pop_positions(img, vn, String[]; colour_by = marker).value
        @test isequal(got, want) && any(isfinite, got)
    end
end
