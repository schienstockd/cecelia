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

@testset "figure_source / figure_stale_reason: is the image a figure was made from still the same?" begin
    td = mktempdir()
    img = CciaImage(uid = "FIG2", name = "fig", dir = joinpath(td, "1", "FIG2"))
    mkpath(img._dir)
    img.filepath = Dict{String,Any}("default" => "a.zarr", "denoised" => "d.zarr", "_active" => "default")
    for f in ("a.zarr", "d.zarr"); mkpath(joinpath(img_zero_dir(img), f)); end

    src = figure_source(img, "denoised")
    @test src["valueName"] == "denoised" && src["storeMtime"] isa Real
    @test figure_source(img)["valueName"] == "default"             # the active IMAGE version
    @test figure_stale_reason(img, src) === nothing

    # a re-process replaces the store (a staged write renames a new directory into place)
    zp = img_filepath(img, "denoised")
    rm(zp; recursive = true); mkpath(zp)
    @test figure_stale_reason(img, merge(src, Dict("storeMtime" => src["storeMtime"] - 10))) ==
          "image version denoised was re-processed since"
    # the version itself was deleted
    rm(zp; recursive = true)
    @test figure_stale_reason(img, src) == "image version \"denoised\" is gone"
end

@testset "project figure style: validated, merged under the spec, and a change marks figures stale" begin
    schema = figure_style_schema()
    @test schema[:defaults][:fontPt] == 7 && schema[:defaults][:timeFormat] == "clock"   # the viewer's
    style, errs = clean_figure_style(Dict("fontPt" => 8, "timeFormat" => "min", "tileMm" => nothing))
    @test isempty(errs) && style == Dict("fontPt" => 8, "timeFormat" => "min", "tileMm" => nothing)
    _, errs = clean_figure_style(Dict("fontPt" => 99, "timeFormat" => "hours", "colour" => 1, "gapMm" => "x"))
    @test length(errs) == 4 && any(occursin("fontPt: 4", e) for e in errs) &&
          any(occursin("timeFormat: one of clock, min", e) for e in errs) &&
          any(occursin("colour: not a style setting", e) for e in errs)

    td = mktempdir()
    img = CciaImage(uid = "FIG3", name = "fig", dir = joinpath(td, "1", "FIG3"))
    mkpath(img._dir)
    img.filepath = Dict{String,Any}("default" => "a.zarr", "_active" => "default")
    mkpath(joinpath(img_zero_dir(img), "a.zarr"))
    pdir = img_project_dir(img)
    @test project_figure_style(pdir) == Dict{String,Any}()                      # none set: the house style
    @test !isempty(set_project_figure_style!(pdir, Dict("fontPt" => 99)))      # rejected, nothing written
    @test project_figure_style(pdir) == Dict{String,Any}()
    @test isempty(set_project_figure_style!(pdir, Dict("fontPt" => 8)))
    @test project_figure_style(pdir) == Dict("fontPt" => 8)

    # the project's style under the spec's own: the spec wins key by key
    p = figure_params(img, Dict{String,Any}("rows" => [], "style" => Dict("timeFormat" => "min")))
    @test p["style"] == Dict("fontPt" => 8, "timeFormat" => "min")

    # a figure drawn in the project style goes stale when the project style changes, not before —
    # read back through JSON like the registry does
    src = JSON3.read(JSON3.write(figure_source(img)))
    srcd = Dict{String,Any}(String(k) => v for (k, v) in src)
    @test figure_stale_reason(img, srcd) === nothing
    set_project_figure_style!(pdir, Dict("fontPt" => 9))
    @test figure_stale_reason(img, srcd) == "the project's figure style changed since"
    # …unless the figure fixes that key itself
    own = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(JSON3.write(figure_source(img; spec_style = Dict("fontPt" => 6)))))
    set_project_figure_style!(pdir, Dict("fontPt" => 10))
    @test figure_stale_reason(img, own; spec_style = Dict("fontPt" => 6)) === nothing
end
