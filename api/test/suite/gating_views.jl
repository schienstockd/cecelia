# Gating as pictures — `/api/gating/plot-image` (+ the pure `render_gate_plot`) and the
# `/api/gating/cells-image` guards. The cells still itself renders on the offline renderer
# (`offline_renderer_plumbing.jl` covers `render_view_stills`); the fixture has no image store.
using Base64, PNGFiles

@testset "API: gate plot raster — clipping, the browser's look, named gates" begin
    @test all(isapprox.(_clip_segment(-1e9, 0.5, 1e9, 0.5, 0.0, 1.0, 0.0, 1.0), (0.0, 0.5, 1.0, 0.5); atol = 1e-5))
    @test _clip_segment(2.0, 2.0, 3.0, 3.0, 0.0, 1.0, 0.0, 1.0) === nothing
    xv = Float32[0.1, 0.1, 0.9, NaN]; yv = Float32[0.1, 0.1, 0.9, 0.5]
    ticks = [Dict("pos" => 0.0, "label" => "0.0"), Dict("pos" => 1.0, "label" => "2130.0")]
    # a threshold gate spans ±1e9 on its free axis — it must clip, not walk a billion pixels
    gates = [Dict{String,Any}("kind" => "rectangle", "colour" => "#ff0000", "path" => "/hi",
                              "x_min" => 0.5, "x_max" => 1e9, "y_min" => -1e9, "y_max" => 1e9),
             Dict{String,Any}("kind" => "polygon", "colour" => "#ffffff", "path" => "/a/qc",
                              "vertices" => [[0.0, 0.0], [0.4, 0.0], [0.2, 0.4]])]
    img = @timed render_gate_plot(xv, yv, (0.0, 1.0), (0.0, 1.0), ticks, ticks, gates;
                                  xtitle = "gBT-CTV", ytitle = "volume (µm³)")
    @test img.time < 10
    m = img.value
    s = _PLOT_SCALE
    @test size(m) == ((_PLOT_PAD.top + _PLOT_SIDE + _PLOT_PAD.bottom) * s, (_PLOT_PAD.left + _PLOT_SIDE + _PLOT_PAD.right) * s)
    @test m[1, 1] == _PLOT_BG                                     # the panel colour, as on screen
    @test any(==(RGB{N0f8}(1, 0, 0)), m)                          # the red gate is drawn
    @test any(==(_PLOT_TEXT), m)                                  # a white gate is drawn in --cc-text
    @test any(c -> c == _heat_ramp(1.0), m)                       # the densest dots, in the shared heat ramp
    # text is drawn: the x title band and the y title column are not empty
    B = (_PLOT_PAD.top + _PLOT_SIDE) * s
    @test any(!=(_PLOT_BG), m[B + 20s:B + 40s, :])
    @test any(!=(_PLOT_BG), m[:, 1:(_PLOT_PAD.left - 50) * s])
    # tick labels read as the browser formats them
    @test _fmt_tick_label("2130.0") == "2.1k" && _fmt_tick_label("262000.0") == "262k"
    @test _fmt_tick_label("910.64") == "910.64" && _fmt_tick_label("0.0") == "0.0" && _fmt_tick_label("x") == "x"
    @test size(_text_mask("tick", "12"), 2) > size(_text_mask("tick", "1"), 2)
    # density: two coincident points are the max (1.0); a far lone point is lower; NaN / outside → 0
    d = point_densities([0.1, 0.1, 0.9, NaN, 5.0], [0.1, 0.1, 0.9, 0.5, 5.0], (0.0, 1.0), (0.0, 1.0))
    @test d[1] == d[2] ≈ 1.0 && 0 < d[3] < 1 && d[4] == 0 && d[5] == 0
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
        @test size(png) == ((_PLOT_PAD.top + _PLOT_SIDE + _PLOT_PAD.bottom) * _PLOT_SCALE,
                            (_PLOT_PAD.left + _PLOT_SIDE + _PLOT_PAD.right) * _PLOT_SCALE)
        @test r.n == 1377
        @test length(r.gates) == 1 && r.gates[1].path == "/dim"
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
        # the 400 says which column is missing, names near ones and lists what there is
        st, b = get_(api_gating_plot_image, "x=intensity&y=area")
        msg = String(JSON3.read(b).error)
        @test st == 400 && occursin("`intensity` is not a column (near: mean_intensity_0", msg)
        @test occursin("Cell columns: ", msg) && !occursin("`area`", msg)
        tcols = track_table_cols(Cecelia.init_object("testpr", "KDIeEm"), "B")
        if !isempty(tcols)                                           # a per-track column, pointed at its table
            msg = String(JSON3.read(get_(api_gating_plot_image, "x=$(first(tcols))&y=area")[2]).error)
            @test occursin("per-track column", msg)
        end
        # the distribution a gate is chosen from — the same read as the plot, so the same n
        st, b = get_(api_gating_summary, "x=mean_intensity_0&y=area")
        sm = JSON3.read(b)
        @test st == 200 && sm.n == 1377 && sm.x.n == 1377 && length(sm.x.bins) == 30
        @test sum(sum, sm.grid.counts) == 1377 && length(sm.grid.x_edges) == 21
        @test sm.x.transform.kind == "linear"
        @test !haskey(JSON3.read(get_(api_gating_summary, "x=area&y=area")[2]), :grid)   # one axis, no grid
        @test JSON3.read(get_(api_gating_summary, "x=mean_intensity_0&y=area&pop=/dim")[2]).n < 1377
        @test get_(api_gating_summary, "x=mean_intensity_0&y=area&pop=/nope")[1] == 404
        @test get_(api_gating_summary, "x=not_a_column&y=area")[1] == 400
        # a population that exists but holds no cells is an answer (n = 0), not an error
        none = merge(gate, Dict{String,Any}("x_min" => -2.0, "x_max" => -1.0))
        _post(api_gating_pop_add, Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
              "valueName" => "B", "popType" => "flow", "name" => "none", "gate" => none, "colour" => "#e11d48"))
        st, b = get_(api_gating_summary, "x=mean_intensity_0&y=area&pop=/none")
        @test st == 200 && JSON3.read(b).n == 0

        @test get_(api_gating_plot_image, "y=area")[1] == 400
        @test get_(api_gating_cells_image, "pop=/nope")[1] == 404
        @test get_(api_gating_cells_image, "pop=/dim")[1] == 404      # the fixture has no label store / zarr
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# The gating plot routes read through the package's `pop_plot_cols` (docs/POPULATION.md → *The
# gate-plot read*); the API adds only the pick selection and the transforms. Pinned on the fixture:
# plotdata returns exactly the package read, the pick selection reaches every plot route on
# cell-grained maps and never on track maps, and a track gate on an aggregate nobody plots evaluates.
@testset "API: gating plot routes are a thin wrapper over pop_plot_cols" begin
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
        f32(b) = collect(reinterpret(Float32, b))
        rect(x, y, x0, x1, y0, y1) = Dict{String,Any}("kind" => "rectangle", "x_channel" => x, "y_channel" => y,
            "x_min" => x0, "x_max" => x1, "y_min" => y0, "y_max" => y1)
        add(pt, name, gate) = _post(api_gating_pop_add, Dict{String,Any}("projectUid" => "testpr",
            "imageUid" => "KDIeEm", "valueName" => "B", "popType" => pt, "name" => name, "gate" => gate))
        add("flow", "dim", rect("mean_intensity_0", "area", 0.0, 1000.0, 0.0, 1e9))
        add("track", "aggr", rect("mean_intensity_0.mean", "area.mean", 0.0, 1000.0, 0.0, 1e9))
        img = Cecelia.init_object("testpr", "KDIeEm")

        # plotdata (linear) == the package read, label-aligned, for root and a gated pop
        for pop in ("root", "/dim")
            st, b = get_(api_gating_plotdata, "popType=flow&pop=$pop&x=mean_intensity_0&y=area&z=centroid_x&withLabels=1")
            v = pop_plot_cols(img, "flow", pop, ["mean_intensity_0", "area", "centroid_x", "label"]; value_name = "B")
            buf = f32(b)
            @test st == 200 && length(buf) == 4 * length(v[1])
            @test buf[1:4:end] == Float32.(v[1]) && buf[3:4:end] == Float32.(v[3]) && buf[4:4:end] == Float32.(v[4])
        end
        @test length(f32(get_(api_gating_plotdata, "popType=flow&pop=root&x=mean_intensity_0&y=area")[2])) == 2 * 1377
        # missing x/y → no points; a missing colour-by measure → NaN per dot, never fewer dots
        @test isempty(f32(get_(api_gating_plotdata, "popType=flow&pop=root&x=mean_intensity_0&y=nope")[2]))
        z = f32(get_(api_gating_plotdata, "popType=flow&pop=root&x=mean_intensity_0&y=area&z=nope")[2])
        @test length(z) == 3 * 1377 && all(isnan, z[3:3:end])
        # unknown pop → empty plot / n 0, but 404 where the route resolves the pop itself
        @test isempty(f32(get_(api_gating_plotdata, "popType=flow&pop=/nope&x=mean_intensity_0&y=area")[2]))
        @test JSON3.read(get_(api_gating_plotmeta, "popType=flow&pop=/nope&x=mean_intensity_0&y=area")[2]).n == 0
        @test get_(api_gating_stats, "popType=flow&pop=/nope")[1] == 404
        @test get_(api_gating_summary, "popType=flow&pop=/nope&x=mean_intensity_0&y=area")[1] == 404
        @test get_(api_gating_summary, "popType=flow&pop=root&x=nope&y=area")[1] == 400

        # the pick selection: a plot of it is exactly the picked cells, on every cell-grained route
        labs = Int.(pop_plot_cols(img, "flow", "root", ["label"]; value_name = "B")[1])
        pick = labs[3:2:80]
        _set_pick_selection!(img._dir, "B", pick)
        try
            st, b = get_(api_gating_plotdata, "popType=flow&pop=/Pick%20selection&x=mean_intensity_0&y=area&withLabels=1")
            @test Int.(f32(b)[3:3:end]) == pick
            @test JSON3.read(get_(api_gating_plotmeta, "popType=flow&pop=/Pick%20selection&x=mean_intensity_0&y=area")[2]).n == length(pick)
            @test JSON3.read(get_(api_gating_stats, "popType=flow&pop=/Pick%20selection")[2]).count == length(pick)
            @test JSON3.read(get_(api_gating_summary, "popType=flow&pop=/Pick%20selection&x=mean_intensity_0&y=area")[2]).n == length(pick)
            # never on a track map — its labels are track ids — so the track tree doesn't list it either
            @test get_(api_gating_stats, "popType=track&pop=/Pick%20selection")[1] == 404
            @test occursin("Pick selection", String(get_(api_gating_popmap, "popType=flow")[2]))
            @test !occursin("Pick selection", String(get_(api_gating_popmap, "popType=track")[2]))
        finally
            _set_pick_selection!(img._dir, "B", Int[])
        end

        # track: one point per track; a gate on aggregates the axes don't name still evaluates
        tp = track_props(img; value_name = "B", cell_measures = ["mean_intensity_0", "area"])
        keep = (0 .<= tp[!, "mean_intensity_0.mean"] .<= 1000) .& (0 .<= tp[!, "area.mean"] .<= 1e9)
        b = f32(get_(api_gating_plotdata, "popType=track&pop=/aggr&x=live.track.speed&y=live.track.duration&withLabels=1")[2])
        @test Int.(b[3:3:end]) == Int.(tp.label[keep]) && 0 < count(keep) < size(tp, 1)
        @test length(f32(get_(api_gating_plotdata, "popType=track&pop=root&x=live.track.speed&y=live.track.duration")[2])) == 2 * size(tp, 1)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# A derived `_tracked` set is what tracking makes and tasks take as input (`B/_tracked`,
# `B/dim/_tracked`), so the read routes must find it too. They defaulted to `popType=flow`, whose map
# never carries the derived sets — an unattended agent's gate_stats on `/P14qc/_tracked` 404'd while
# clustTracks accepted the same path. `read_pop_type` is the shared rule: discover from the path when
# no popType is sent, and read a derived leaf asked for under its stored map (`flow`) as `live`.
@testset "API: gating read routes resolve derived _tracked sets" begin
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
        _post(api_gating_pop_add, Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
            "valueName" => "B", "popType" => "flow", "name" => "dim",
            "gate" => Dict{String,Any}("kind" => "rectangle", "x_channel" => "mean_intensity_0",
                "y_channel" => "area", "x_min" => 0.0, "x_max" => 1000.0, "y_min" => 0.0, "y_max" => 1e9)))
        img = Cecelia.init_object("testpr", "KDIeEm")
        # the counts the task read gives for the same paths
        want(p) = size(pop_df_multi(img, [p]; value_name = "B"), 1)
        @test read_pop_type(img, "B", "/dim/_tracked") == "live"
        @test read_pop_type(img, "B", "/dim/_tracked", "flow") == "live"     # flow's map + derived = live
        @test read_pop_type(img, "B", "/dim", "flow") == "flow"
        @test read_pop_type(img, "B", "/dim/_tracked", "track") == "track"   # an explicit other type stands
        for (q, p) in (("pop=/_tracked", "/_tracked"), ("pop=/dim/_tracked", "/dim/_tracked"),
                       ("popType=flow&pop=/dim/_tracked", "/dim/_tracked"))
            st, b = get_(api_gating_stats, q)
            @test st == 200
            @test JSON3.read(b).count == want(p) > 0
        end
        # a plain gate with no popType still reads as cells
        @test JSON3.read(get_(api_gating_stats, "pop=/dim")[2]).count == want("/dim")
        @test get_(api_gating_stats, "pop=/nope")[1] == 404
        # membership over a gate AND its tracked subset in one request
        st, b = get_(api_gating_membership, "pops=/dim,/dim/_tracked")
        mem = JSON3.read(b).membership
        @test st == 200 && length(mem[Symbol("/dim/_tracked")]) == want("/dim/_tracked") < length(mem[Symbol("/dim")])
        # the summary a gate is chosen from reads the same set
        st, b = get_(api_gating_summary, "pop=/dim/_tracked&x=mean_intensity_0&y=area")
        @test st == 200 && JSON3.read(b).n == want("/dim/_tracked")
        # …and so do the plot reads (they don't 404 — an unresolved pop was a silently EMPTY plot)
        @test length(collect(reinterpret(Float32, get_(api_gating_plotdata,
            "pop=/dim/_tracked&x=mean_intensity_0&y=area")[2]))) == 2 * want("/dim/_tracked")
        @test JSON3.read(get_(api_gating_plotmeta, "pop=/dim/_tracked&x=mean_intensity_0&y=area")[2]).n ==
              want("/dim/_tracked")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end
