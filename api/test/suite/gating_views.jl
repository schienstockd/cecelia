# Gating as pictures — `/api/gating/plot-image` (+ the pure `render_gate_plot`) and the
# `/api/gating/cells-image` guards. The cells still itself renders on the offline renderer
# (`offline_renderer_plumbing.jl` covers `render_view_stills`); the fixture has no image store.
using Base64, PNGFiles

@testset "API: gate plot raster — clipping, ticks, numbered gates" begin
    @test all(isapprox.(_clip_segment(-1e9, 0.5, 1e9, 0.5, 0.0, 1.0, 0.0, 1.0), (0.0, 0.5, 1.0, 0.5); atol = 1e-5))
    @test _clip_segment(2.0, 2.0, 3.0, 3.0, 0.0, 1.0, 0.0, 1.0) === nothing
    xv = Float32[0.1, 0.1, 0.9, NaN]; yv = Float32[0.1, 0.1, 0.9, 0.5]
    ticks = [Dict("pos" => 0.0, "label" => "0.0"), Dict("pos" => 1.0, "label" => "1.0e3")]
    # a threshold gate spans ±1e9 on its free axis — it must clip, not walk a billion pixels
    gates = [Dict{String,Any}("kind" => "rectangle", "colour" => "#ff0000",
                              "x_min" => 0.5, "x_max" => 1e9, "y_min" => -1e9, "y_max" => 1e9),
             Dict{String,Any}("kind" => "polygon", "colour" => "#ffffff",
                              "vertices" => [[0.0, 0.0], [0.4, 0.0], [0.2, 0.4]])]
    img = @timed render_gate_plot(xv, yv, (0.0, 1.0), (0.0, 1.0), ticks, ticks, gates; width = 300, height = 260)
    @test img.time < 5
    m = img.value
    @test size(m) == (260, 300)
    @test any(==(RGB{N0f8}(1, 0, 0)), m)                         # the red gate is drawn
    @test any(==(_heat_ramp(1.0)), m)                            # the densest dots, in the shared heat ramp
    @test m[1, 1] == _PLOT_BG
    @test any(==(RGB{N0f8}(1, 1, 1)), m)                         # the white gate shows on the dark plot
    # density: two coincident points are the max (1.0); a far lone point is lower; NaN / outside → 0
    d = point_densities([0.1, 0.1, 0.9, NaN, 5.0], [0.1, 0.1, 0.9, 0.5, 5.0], (0.0, 1.0), (0.0, 1.0))
    @test d[1] == d[2] ≈ 1.0 && 0 < d[3] < 1 && d[4] == 0 && d[5] == 0
    @test _text_width("12") == 2 * _GLYPH_ADV - _GLYPH_SCALE
end

@testset "API: /api/gating/plot-image + cells-image guards" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        base = "projectUid=testpr&imageUid=KDIeEm&valueName=B"
        get_(route, q) = route(HTTP.Request("GET", "/x?$base&$q"))
        gate = Dict{String,Any}("kind" => "rectangle", "x_channel" => "mean_intensity_0", "y_channel" => "area",
                                "x_min" => 0.0, "x_max" => 1000.0, "y_min" => 0.0, "y_max" => 1e9)
        _post(api_gating_pop_add, Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
              "valueName" => "B", "popType" => "flow", "name" => "dim", "gate" => gate, "colour" => "#e11d48"))

        st, b = get_(api_gating_plot_image, "x=mean_intensity_0&y=area")
        @test st == 200
        r = JSON3.read(b)
        png = PNGFiles.load(IOBuffer(base64decode(String(r.png))))
        @test size(png) == (440, 520)
        @test r.n == 1377
        @test length(r.gates) == 1 && r.gates[1].n == 1 && r.gates[1].path == "/dim"
        @test r.gates[1].y_max == 1e9                               # stated in full, clipped only in the picture
        @test r.x.extent[1] < r.x.extent[2]

        # a swapped axis pair still draws the gate; a different pair does not
        st, b = get_(api_gating_plot_image, "x=area&y=mean_intensity_0")
        @test length(JSON3.read(b).gates) == 1
        st, b = get_(api_gating_plot_image, "x=mean_intensity_1&y=area")
        @test isempty(JSON3.read(b).gates)
        # the child's own plot: its cells only, nothing gated beneath it yet
        st, b = get_(api_gating_plot_image, "x=mean_intensity_0&y=area&pop=/dim")
        @test st == 200 && JSON3.read(b).n < 1377 && isempty(JSON3.read(b).gates)

        @test get_(api_gating_plot_image, "x=mean_intensity_0&y=area&pop=/nope")[1] == 404
        @test get_(api_gating_plot_image, "x=not_a_column&y=area")[1] == 400
        @test get_(api_gating_plot_image, "y=area")[1] == 400
        @test get_(api_gating_cells_image, "pop=/nope")[1] == 404
        @test get_(api_gating_cells_image, "pop=/dim")[1] == 404      # the fixture has no label store / zarr
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end
