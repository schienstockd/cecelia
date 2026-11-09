# Whether a source's acquisition settings describe the imported store
# (`app/src/tasks/importImages/omezarr/acquisition.jl` → `_acquisition_describes_store`).
# Loaded from suite.jl inside its aggregating testset — no outer @testset here.

@testset "_acquisition_describes_store: an unrecorded z/t count is unknown, not 1" begin
    # What `ims_meta.acquisition_settings` returns for an Imaris file with no `LSM Dataset
    # Size` group (an Immunemap download): channels and a date, no zStack, no timelapse.
    ims = Dict{String,Any}("source" => "ims", "system" => nothing,
                           "acquiredAt" => "2017-04-03T08:24:48",
                           "channels" => Any[Dict{String,Any}("order" => 1, "name" => "Green"),
                                             Dict{String,Any}("order" => 2, "name" => "Red")])
    @test Cecelia._acquisition_describes_store(ims, Dict{String,Any}("SizeC" => 2, "SizeZ" => 11, "SizeT" => 88))
    @test !Cecelia._acquisition_describes_store(ims, Dict{String,Any}("SizeC" => 3, "SizeZ" => 11, "SizeT" => 88))
    # Recorded counts still catch a derived stack.
    zt = merge(ims, Dict{String,Any}("zStack" => Dict{String,Any}("count" => 11),
                                     "timelapse" => Dict{String,Any}("count" => 88)))
    @test Cecelia._acquisition_describes_store(zt, Dict{String,Any}("SizeC" => 2, "SizeZ" => 11, "SizeT" => 60))
    @test !Cecelia._acquisition_describes_store(zt, Dict{String,Any}("SizeC" => 2, "SizeZ" => 1, "SizeT" => 88))
    @test !Cecelia._acquisition_describes_store(zt, Dict{String,Any}("SizeC" => 2, "SizeZ" => 11, "SizeT" => 1))
end

# ── Refresh metadata on a crop takes its SOURCE image's settings (`_resync_crop_acquisition!`) ──
# The source's settings come from the real FV4000 settings document (test-data/acquisition/), read
# the way an import does. A Z-crop inherits the source's ori_path, whose file describes 31 planes,
# so the file re-read alone records "other-stack" — the bug this pins.
let oir_doc = normpath(joinpath(test_projects_dir(), "..", "acquisition",
                                "oir_fv4000_3.2.1_imageProperties.xml"))
    # metadata-only series-layout store: multiscales on `0/`, level-0 shape for SizeT/C/Z
    function store!(img, shape)
        base = joinpath(img_zero_dir(img), "img.ome.zarr", "0")
        mkpath(joinpath(base, "0"))
        axes = [Dict("name" => "t", "type" => "time"), Dict("name" => "c", "type" => "channel"),
                (Dict("name" => a, "type" => "space", "unit" => "micrometer") for a in ("z", "y", "x"))...]
        write(joinpath(base, ".zattrs"), JSON3.write(Dict("multiscales" => [Dict("axes" => axes,
            "datasets" => [Dict("path" => "0", "coordinateTransformations" =>
                [Dict("type" => "scale", "scale" => [15.0, 1.0, 2.0, 0.5, 0.5])])])])))
        write(joinpath(base, "0", ".zarray"), JSON3.write(Dict("shape" => shape)))
        img.filepath["default"]            = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)
    end
    if have_fixture(oir_doc)
        @testset "resync_ome_meta! gives a Z-crop its source image's acquisition settings" begin
            proj = create_project!(name = "crop-acq-$(rand(1000:9999))")
            s    = add_set!(proj; name = "set")
            src  = joinpath(proj.root, "source.oir")
            write(src, vcat(rand(UInt8, 4096), read(oir_doc), rand(UInt8, 2048)))
            img  = add_image!(s; name = "img", meta = Dict{String,Any}("ori_path" => src))
            store!(img, [181, 4, 31, 8, 8])
            @test resync_ome_meta!(init_object(proj.uid, img.uid))
            @test [p["zFromStartUm"] for p in init_object(proj.uid, img.uid).meta["acquisition"]["brightZ"]["points"]] == [0.0, 60.0]

            # planes [5,15) of it: 10 planes, the ramp measured from plane 5 (2 µm step)
            crop = add_image!(s; name = "crop", meta = Dict{String,Any}("ori_path" => src,
                "crop_source_uid" => img.uid, "crop_source_value_name" => "default",
                "crop_box" => Dict{String,Any}("x0" => 0, "x1" => 8, "y0" => 0, "y1" => 8,
                                               "z0" => 5, "z1" => 15, "t0" => -1, "t1" => -1)))
            store!(crop, [181, 4, 10, 8, 8])
            @test resync_ome_meta!(init_object(proj.uid, crop.uid))
            cm = init_object(proj.uid, crop.uid).meta
            @test !haskey(cm, "acquisitionMissing")
            @test cm["acquisition"]["system"] == "FV4000"
            @test round(Int, cm["acquisition"]["zStack"]["count"]) == 10
            @test [p["zFromStartUm"] for p in cm["acquisition"]["brightZ"]["points"]] == [-10.0, 50.0]
            # the source's own settings are untouched
            @test [p["zFromStartUm"] for p in init_object(proj.uid, img.uid).meta["acquisition"]["brightZ"]["points"]] == [0.0, 60.0]
            rm(proj.root; recursive = true)
        end
    else
        @test_skip "acquisition fixture missing"
    end
end
