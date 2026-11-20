# A figure draws the viewer look it was saved with (docs/todo/FIGURES_PLAN.md, overlay phase): the spec's
# `look` is resolved by the movies' own readers (`figure_look_overlays`) and drawn by the movie renderer's
# pass on every tile — so a figure tile is the movie frame of that look.
# Fixtures: `testpr/1/KDIeEm` labelProps `B.h5ad` + `spatialGraph/net.h5ad` (see spatial_graph_lines.jl);
# the image and mask stores are written here (zeros but one cell), on the cell table's 512 × 512 × 8 grid.

@testset "API: a figure draws its look's mask and neighbour-graph edges" begin
    src = api_fixture("testpr", "1", "KDIeEm")
    if !api_have_fixture(joinpath(src, "spatialGraph", "net.h5ad"))
        @test_skip "spatialGraph fixtures (missing)"
    else
        axes_ms(names) = Dict("multiscales" => [Dict("axes" => [Dict("name" => n) for n in names],
                                                     "datasets" => [Dict("path" => "0")])])
        conf = cecelia_conf()
        dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "figure-overlays")
            img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "figOv1")
            img.filepath = Dict("default" => "ccidImage.ome.zarr", "_active" => "default")
            img.im_channel_names = Dict{String,Union{Vector{String},String}}("default" => ["c0"], "_active" => "default")
            img.labels = Dict("B" => ["B.zarr"])
            img.label_props = Dict("B" => "B.h5ad", "_active" => "B")
            save!(img)
            mkpath(img_label_props_dir(img))
            cp(joinpath(src, "labelProps", "B.h5ad"), img_label_props_path(img, "B"))
            cp(joinpath(src, "spatialGraph"), img_spatial_graph_dir(img))
            nx, ny, nz = 512, 512, 8
            dir = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            g = zgroup(Zarr.DirectoryStore(dir); attrs = axes_ms(["t", "c", "z", "y", "x"]))
            a = zcreate(UInt16, g, "0", nx, ny, nz, 1, 1; chunks = (128, 128, nz, 1, 1))
            a[:, :, :, :, :] = zeros(UInt16, nx, ny, nz, 1, 1)
            ldir = img_labels_path(img, "B"); mkpath(dirname(ldir))
            lg = zgroup(Zarr.DirectoryStore(ldir); attrs = axes_ms(["t", "z", "y", "x"]))
            la = zcreate(UInt32, lg, "0", nx, ny, nz, 1; chunks = (128, 128, nz, 1))
            lab = zeros(UInt32, nx, ny, nz, 1); lab[101:200, 101:200, :, 1] .= UInt32(5)   # one cell
            la[:, :, :, :] = lab
            img, _ = _gating_image(proj.uid, img.uid)

            spec(look = nothing; kw...) = begin
                s = Dict{String,Any}("center" => [256, 256], "sizePx" => 512, "z" => 3, "frames" => [0],
                                     "rows" => [Dict("channels" => [Dict("c" => "c0", "colour" => "#ffffff")])],
                                     "display" => Dict("ranges" => Dict("c0" => [0, 100])), "tilePx" => 512)
                look === nothing || (s["look"] = look)
                merge!(s, Dict{String,Any}(String(k) => v for (k, v) in kw))
            end
            graph = Dict{String,Any}("spatialGraphs" => Dict("net" => "#00ff00"), "labelValueNames" => String[])
            mask  = Dict{String,Any}("labelValueNames" => ["B"], "labelContour" => 0, "labelOpacity" => 1.0)
            # a look as a request delivers it (JSON3, symbol keys) and as a REPL caller writes it: one outline
            outlined = Dict{String,Any}("labelValueNames" => ["B"], "labelContour" => 2)
            for l in (outlined, JSON3.read(JSON3.write(outlined)))
                @test figure_look_overlays(img, spec(l))["params"]["labelContourPx"] == 2
            end

            # the resolved overlays: the movie's params, each frame's edges, each column's planes
            @test figure_look_overlays(img, spec()) === nothing                   # no look: pixels only
            @test figure_look_overlays(img, spec(Dict{String,Any}("labelValueNames" => String[]))) === nothing
            lo = figure_look_overlays(img, spec(graph))
            @test collect(keys(lo["frames"])) == ["0"] && !isempty(lo["frames"]["0"]["segments"]["x0"])
            @test !haskey(lo["params"], "labelsPath")
            @test lo["planeFilters"]["3:3"] == _plane_window(3, nz, movie_overlay_style())[2]   # one plane ± tol
            zs = figure_look_overlays(img, spec(graph; kind = "zstack", t = 0, planes = [2, 5], zHalf = 1))
            @test sort!(collect(keys(zs["planeFilters"]))) == ["1:3", "4:6"]
            @test zs["planeFilters"]["1:3"]["points"] == [1, 3]                       # a range: exactly
            lm = figure_look_overlays(img, spec(mask))
            @test lm["params"]["labelsPath"] == ldir && lm["params"]["labelContourPx"] == 0
            @test lm["params"]["labelColouring"] == "palette" && isempty(lm["frames"])

            # what a figure's source stamps: the segmentations the look reads that the image has
            @test figure_look_segmentations(img, mask) == ["B"]
            @test figure_look_segmentations(img, merge(graph, Dict("showPopulations" => true,
                "popValueName" => "B", "popEyes" => Dict("gone" => Dict("flow::/a" => true))))) == ["B"]
            @test isempty(figure_look_segmentations(img, nothing))

            # end to end: the dry-run preview, with and without the look
            px(look) = PNGFiles.load(IOBuffer(make_figure!(proj.uid, img, spec(look); dry_run = true).png))
            n_green(p) = count(c -> green_of(c) > 0.6 && red_of(c) < 0.3 && blue_of(c) < 0.3, p)
            red_of(c) = Float64(red(c)); green_of(c) = Float64(green(c)); blue_of(c) = Float64(blue(c))
            tinted(p) = count(c -> max(red_of(c), green_of(c), blue_of(c)) - min(red_of(c), green_of(c), blue_of(c)) > 0.3, p)
            plain = px(nothing)
            @test tinted(plain) < 50                                      # black tile, black legend text
            @test n_green(px(graph)) > 500                                  # the edges, in the look's green
            @test tinted(px(mask)) > 0.8 * 100 * 100                      # the cell, filled in its colour
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
