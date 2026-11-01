# Figures (docs/todo/FIGURES_PLAN.md): what `figure_params` fills in from the image. Rendering needs a GPU
# adapter and real data, so it is checked by eye (`render_figure` on a real image), not here.

@testset "figure_params: the image fills in what it knows" begin
    td = mktempdir()
    img = CciaImage(uid = "FIG", name = "fig", dir = td)
    img.filepath = Dict{String,Any}("default" => "ccidImage.ome.zarr", "denoised" => "ccidDenoised.ome.zarr",
                                    "_active" => "default")
    img.im_channel_names = Dict{String,Any}("default" => ["SHG", "mem-TOM", "CD169-Kat"], "_active" => "default")
    img.meta = Dict{String,Any}("PhysicalSizeX" => 0.25, "PhysicalSizeY" => 0.25, "PhysicalSizeZ" => 2.0,
                                "TimeIncrement" => 15)
    rows = [Dict("channels" => [Dict("c" => "mem-TOM", "colour" => "#ff00ff"), Dict("c" => 2, "colour" => "#00ff00")])]
    spec = Dict{String,Any}("valueName" => "denoised", "rows" => rows, "scaleBarUm" => 5)

    p = figure_params(img, spec)
    @test p["zarrPath"] == img_filepath(img, "denoised")
    @test p["channelNames"] == ["SHG", "mem-TOM", "CD169-Kat"]
    @test [c["c"] for c in p["rows"][1]["channels"]] == [1, 2]       # a name and an index, both 0-based
    @test p["pixelSizeUm"] == 0.25 && p["zStepUm"] == 2.0 && p["zAniso"] == 8.0
    @test p["timeIncrementS"] ≈ 15 && p["scaleBarUm"] == 5
    @test !haskey(spec, "zarrPath")                                    # the caller's spec is not mutated

    # display ranges by name (the viewer's contrast) or by index, both land as the index the renderer reads
    r = figure_params(img, merge(spec, Dict("display" => Dict("ranges" => Dict("mem-TOM" => [0, 68], "2" => [0, 10])))))
    @test r["display"]["ranges"] == Dict("1" => [0, 68], "2" => [0, 10])

    # no version given → the active one
    @test figure_params(img, Dict{String,Any}("rows" => rows))["zarrPath"] == img_filepath(img)

    # an unknown channel or version fails by name, before anything renders
    bad = [Dict("channels" => [Dict("c" => "tdTom", "colour" => "#ff00ff")])]
    @test_throws r"tdTom" figure_params(img, Dict{String,Any}("rows" => bad))
    @test_throws r"no image version \"raw\"" figure_params(img, Dict{String,Any}("valueName" => "raw", "rows" => rows))

    # an uncalibrated image: no µm, so no scale bar; no interval, so frames are labelled t<n>
    img.meta = Dict{String,Any}()
    q = figure_params(img, spec)
    @test !haskey(q, "pixelSizeUm") && !haskey(q, "scaleBarUm") && !haskey(q, "timeIncrementS")
end
