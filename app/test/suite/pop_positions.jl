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
