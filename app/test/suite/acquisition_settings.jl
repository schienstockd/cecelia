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

# ── Refresh metadata on a crop, copy or bin takes its SOURCE image's settings (`_resync_derived_acquisition!`) ──
# The source's settings come from the real FV4000 settings document (test-data/acquisition/), read
# the way an import does. A Z-crop inherits the source's ori_path, whose file describes 31 planes,
# so the file re-read alone records "other-stack" — the bug this pins.
let oir_doc = normpath(joinpath(test_projects_dir(), "..", "acquisition",
                                "oir_fv4000_3.2.1_imageProperties.xml"))
    # metadata-only series-layout store: multiscales on `0/`, level-0 shape for SizeT/C/Z
    function store!(img, shape; rel = "img.ome.zarr", vn = "default")
        base = joinpath(img_zero_dir(img), rel, "0")
        mkpath(joinpath(base, "0"))
        axes = [Dict("name" => "t", "type" => "time"), Dict("name" => "c", "type" => "channel"),
                (Dict("name" => a, "type" => "space", "unit" => "micrometer") for a in ("z", "y", "x"))...]
        write(joinpath(base, ".zattrs"), JSON3.write(Dict("multiscales" => [Dict("axes" => axes,
            "datasets" => [Dict("path" => "0", "coordinateTransformations" =>
                [Dict("type" => "scale", "scale" => [15.0, 1.0, 2.0, 0.5, 0.5])])])])))
        write(joinpath(base, "0", ".zarray"), JSON3.write(Dict("shape" => shape)))
        img.filepath[vn]                   = rel
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

            # A bin and a copy of that crop, made before the crop had settings: they carry the
            # original's ori_path (31 planes), so only re-inheriting from the crop describes them.
            for (name, prov) in (("bin", ("bin_source_uid", "bin_source_value_name")),
                                 ("copy", ("copy_source_uid", "copy_source_value_name")))
                d = add_image!(s; name = name, meta = Dict{String,Any}("ori_path" => src,
                    prov[1] => crop.uid, prov[2] => "default"))
                store!(d, [181, 4, 10, name == "bin" ? 4 : 8, name == "bin" ? 4 : 8])
                @test resync_ome_meta!(init_object(proj.uid, d.uid))
                dm = init_object(proj.uid, d.uid).meta
                @test !haskey(dm, "acquisitionMissing")
                @test round(Int, dm["acquisition"]["zStack"]["count"]) == 10
                @test [p["zFromStartUm"] for p in dm["acquisition"]["brightZ"]["points"]] == [-10.0, 50.0]
            end

            # A copy whose own store no longer matches its source version (the source was re-imported
            # since): the source's settings would describe a 31-plane stack, the copy has 12.
            stale = add_image!(s; name = "stale", meta = Dict{String,Any}("ori_path" => src,
                "copy_source_uid" => img.uid, "copy_source_value_name" => "default"))
            store!(stale, [181, 4, 12, 8, 8])
            @test resync_ome_meta!(init_object(proj.uid, stale.uid))
            sm = init_object(proj.uid, stale.uid).meta
            @test !haskey(sm, "acquisition")
            @test sm["acquisitionMissing"]["reason"] == "other-stack"

            # Same, with settings stored by an older reader: they are kept as they are, never paired
            # with an `other-stack` reason (the file re-read path, not an inherited verdict).
            old = add_image!(s; name = "old", meta = Dict{String,Any}("ori_path" => src,
                "copy_source_uid" => img.uid, "copy_source_value_name" => "default",
                "acquisition" => Dict{String,Any}("source" => "oir", "readerVersion" => 1, "system" => "old")))
            store!(old, [181, 4, 12, 8, 8])
            resync_ome_meta!(init_object(proj.uid, old.uid))
            olm = init_object(proj.uid, old.uid).meta
            @test olm["acquisition"]["system"] == "old"
            @test !haskey(olm, "acquisitionMissing")

            # A copy made before its source was re-imported from another file: the source's settings
            # are the new file's, so the copy reads its OWN ori_path (gone here → `source`).
            moved = add_image!(s; name = "moved", meta = Dict{String,Any}(
                "ori_path" => joinpath(proj.root, "gone.oir"),
                "copy_source_uid" => img.uid, "copy_source_value_name" => "default"))
            store!(moved, [181, 4, 31, 8, 8])
            resync_ome_meta!(init_object(proj.uid, moved.uid))
            mm = init_object(proj.uid, moved.uid).meta
            @test !haskey(mm, "acquisition")
            @test mm["acquisitionMissing"]["reason"] == "source"

            # A T-crop keeping ONE frame is still the source's acquisition (`acquired = 1`), not a
            # time projection: Refresh keeps the settings the crop task gave it.
            one = add_image!(s; name = "oneframe", meta = Dict{String,Any}("ori_path" => src,
                "crop_source_uid" => img.uid, "crop_source_value_name" => "default",
                "crop_box" => Dict{String,Any}("x0" => 0, "x1" => 8, "y0" => 0, "y1" => 8,
                                               "z0" => -1, "z1" => -1, "t0" => 3, "t1" => 4)))
            store!(one, [1, 4, 31, 8, 8])
            @test resync_ome_meta!(init_object(proj.uid, one.uid))
            om = init_object(proj.uid, one.uid).meta
            @test !haskey(om, "acquisitionMissing")
            @test om["acquisition"]["timelapse"]["acquired"] == 1

            # A Z-flipped version of the source, and an XY-only crop of it (all 31 planes): the counts
            # match the source's, so only the flip record says the ramp runs the other way round.
            ramp(m) = [(p["zFromStartUm"], Dict(String(k) => v for (k, v) in p["transmissionPct"]))
                       for p in m["acquisition"]["brightZ"]["points"]]
            src_ramp = ramp(init_object(proj.uid, img.uid).meta)
            @test src_ramp[1][2] != src_ramp[2][2]                  # a real ramp: reversing it shows
            mirrored = [(src_ramp[1][1], src_ramp[2][2]), (src_ramp[2][1], src_ramp[1][2])]   # 0 and 60 µm
            store!(init_object(proj.uid, img.uid), [181, 4, 31, 8, 8]; rel = "flipped.ome.zarr", vn = "flipped")
            Cecelia.commit_state!(init_object(proj.uid, img.uid)) do raw
                versioned_filepath_write!(raw, "flipped", "flipped.ome.zarr"; as_new_version = false,
                                          from = "img.ome.zarr", z_flip = true)
            end
            # (as the crop task records it: `copy_source_meta!(…; z_flipped)` → `oriZReversed`)
            fc = add_image!(s; name = "flipcrop", meta = Dict{String,Any}("ori_path" => src,
                Cecelia.ORI_Z_REVERSED_KEY => true,
                "crop_source_uid" => img.uid, "crop_source_value_name" => "flipped",
                "crop_box" => Dict{String,Any}("x0" => 0, "x1" => 4, "y0" => 0, "y1" => 4,
                                               "z0" => -1, "z1" => -1, "t0" => -1, "t1" => -1)))
            store!(fc, [181, 4, 31, 4, 4])
            @test resync_ome_meta!(init_object(proj.uid, fc.uid))
            @test ramp(init_object(proj.uid, fc.uid).meta) == mirrored
            # the image's own settings (the acquisition's) are not turned by a flipped version
            @test ramp(init_object(proj.uid, img.uid).meta) == src_ramp
            # The source's `flipped` store re-made since in acquired order (an X flip over it): the
            # crop's pixels still run reversed, and Refresh still says so.
            Cecelia.commit_state!(init_object(proj.uid, img.uid)) do raw
                versioned_filepath_write!(raw, "flipped", "flipped.ome.zarr"; as_new_version = false,
                                          from = "img.ome.zarr")
            end
            Cecelia.commit_state!(init_object(proj.uid, fc.uid)) do raw
                raw["meta"] = Dict{String,Any}(String(k) => v for (k, v) in raw["meta"] if String(k) != "acquisition")
            end
            @test resync_ome_meta!(init_object(proj.uid, fc.uid))
            @test ramp(init_object(proj.uid, fc.uid).meta) == mirrored

            # An image made from a Z-flipped version whose source can't serve it re-reads its file:
            # `oriZReversed` mirrors that read too.
            orphan = add_image!(s; name = "orphan", meta = Dict{String,Any}("ori_path" => src,
                Cecelia.ORI_Z_REVERSED_KEY => true))
            store!(orphan, [181, 4, 31, 8, 8])
            @test resync_ome_meta!(init_object(proj.uid, orphan.uid))
            @test ramp(init_object(proj.uid, orphan.uid).meta) == mirrored
            # a re-import writes the store in the file's order: the flag goes with the old pixels
            Cecelia._merge_zarr_meta_into_ccid!(init_object(proj.uid, orphan.uid),
                Dict{String,Any}("SizeZ" => 31); overwrite = true)
            @test !haskey(init_object(proj.uid, orphan.uid).meta, Cecelia.ORI_Z_REVERSED_KEY)
            rm(proj.root; recursive = true)
        end
    else
        @test_skip "acquisition fixture missing"
    end
end

# ── A Z-flipped version runs the reverse of its image's settings (`store_z_flipped`) ──
# The record follows the store through every version writer; a crop, copy or bin of it mirrors the
# Bright Z ramp. Expected values from the definition of a flip: plane k of the flipped version is
# acquired plane (n − 1 − k).
@testset "Z-flipped versions: the record follows the store, derived settings are mirrored" begin
    raw = Dict{String,Any}("meta" => Dict{String,Any}())
    versioned_filepath_write!(raw, "default", "img.ome.zarr"; as_new_version = false)
    versioned_filepath_write!(raw, "flipped", "flipped.ome.zarr"; as_new_version = false,
                              from = "img.ome.zarr", z_flip = true)
    zf(rel) = Cecelia.store_z_flipped(raw["meta"], rel)
    @test zf("flipped.ome.zarr") && !zf("img.ome.zarr")
    # made from the flipped version (a denoise, an X flip): reversed too; flipped in Z again: not
    versioned_filepath_write!(raw, "denoised", "denoised/v2/x.ome.zarr"; as_new_version = false, from = "flipped.ome.zarr")
    versioned_filepath_write!(raw, "back", "back.ome.zarr"; as_new_version = false,
                              from = "flipped.ome.zarr", z_flip = true)
    @test zf("denoised/v2/x.ome.zarr") && !zf("back.ome.zarr")
    # overwritten in place from an unflipped store (or re-imported): the record goes with the pixels
    versioned_filepath_write!(raw, "flipped", "flipped.ome.zarr"; as_new_version = false, from = "img.ome.zarr")
    @test !zf("flipped.ome.zarr") && zf("denoised/v2/x.ome.zarr")

    # 31 planes, 2 µm: 60 µm deep. Ramp 14.4 % at the first plane, 21 % at the last.
    src = Dict{String,Any}("ori_path" => "/data/raw.oir", "acquisition" => Dict{String,Any}(
        "zStack" => Dict("stepUm" => 2.0, "count" => 31), "channels" => [Dict("detector" => "D1")],
        "brightZ" => Dict("enabled" => true, "points" => [
            Dict("zFromStartUm" => 0.0, "transmissionPct" => Dict("MaiTai" => 14.4)),
            Dict("zFromStartUm" => 60.0, "transmissionPct" => Dict("MaiTai" => 21.0))])))
    counts = Dict{String,Any}("SizeC" => 1, "SizeZ" => 31, "SizeT" => 1)
    pts(d) = [(p["zFromStartUm"], p["transmissionPct"]["MaiTai"]) for p in d["acquisition"]["brightZ"]["points"]]
    @test pts(Cecelia.inherited_acquisition(src, counts)) == [(0.0, 14.4), (60.0, 21.0)]
    @test pts(Cecelia.inherited_acquisition(src, counts; z_flipped = true)) == [(0.0, 21.0), (60.0, 14.4)]
    @test pts(src) == [(0.0, 14.4), (60.0, 21.0)]                          # the source is untouched
    # planes [5,15) of the flipped version: its plane 0 (acquired 30, 60 µm) is 5 planes before the
    # crop's first; its plane 30 (acquired 0) is 25 planes after
    box = Cecelia.CropBox(x0 = 0, x1 = 8, y0 = 0, y1 = 8, z0 = 5, z1 = 15)
    # unflipped, the same box only shifts the ramp: its first plane is 5 × 2 µm below the source's
    @test pts(Cecelia.crop_acquisition(src, box, counts)) == [(-10.0, 14.4), (50.0, 21.0)]
    @test pts(Cecelia.crop_acquisition(src, box, counts; z_flipped = true)) == [(-10.0, 21.0), (50.0, 14.4)]
    m = Cecelia._bin_meta(src, "srcuid", Cecelia.BinImageParams(factorX = 2, factorY = 2), counts; z_flipped = true)
    @test pts(m) == [(0.0, 21.0), (60.0, 14.4)] && m[Cecelia.ORI_Z_REVERSED_KEY] === true
    # a ramp with no plane count to mirror it against is not attached the wrong way round
    nocount = deepcopy(src); delete!(nocount["acquisition"], "zStack")
    @test Cecelia.inherited_acquisition(nocount, counts; z_flipped = true)["acquisitionMissing"]["reason"] == "other-stack"
    # flipping back: a Z-flipped version of an image that is already reversed against its file
    @test !haskey(Cecelia.copy_source_meta!(Dict{String,Any}(),
                  Dict{String,Any}(Cecelia.ORI_Z_REVERSED_KEY => true); z_flipped = true), Cecelia.ORI_Z_REVERSED_KEY)
end

# ── A multi-image `.lif`: the reader registry routes it, and `series` picks the image ──
# The header is the real LAS X fixture (test-data/acquisition/lif_*), wrapped in the LIF block layout
# (`write_lif` in python/cecelia/tests/test_lif_meta.py). Its series 1 (Image002) records two channels, series 2 (Image007) one: a
# store with one channel takes series 2's settings and is refused series 1's.
@testset "acquisition settings: .lif via the registry, by series" begin
    @test ".lif" in Cecelia.ACQUISITION_SETTINGS_FORMATS
    @test Cecelia.ACQUISITION_READER_VERSIONS["lif"] >= 1
    xml = read(normpath(joinpath(test_projects_dir(), "..", "acquisition",
                                 "lif_dm5500b_las-x-3.4.2_header.xml")), String)
    mktempdir() do dir
        lif = joinpath(dir, "speck.lif")
        data = transcode(UInt16, xml)
        open(lif, "w") do io
            write(io, Int32(0x70), Int32(2 * length(data) + 5), UInt8(0x2A), Int32(length(data)))
            write(io, htol.(data))
            # one 1-byte stand-in data block per image (3): readlif refuses a file without them
            for _ in 1:3
                write(io, Int32(0x70), Int32(0), UInt8(0x2A), Int32(1), UInt8(0x2A), Int32(0), UInt8(0))
            end
        end
        one = Dict{String,Any}("SizeC" => 1, "SizeZ" => 1, "SizeT" => 1)
        @test Cecelia._recover_acquisition_settings!(one, lif, dir; series = 2)
        acq = one["acquisition"]
        @test (acq["source"], acq["technology"], acq["system"]) == ("lif", "widefield", "DM5500B")
        @test acq["imageName"] == "Image007" && length(acq["channels"]) == 1
        wrong = Dict{String,Any}("SizeC" => 1, "SizeZ" => 1, "SizeT" => 1)
        @test !Cecelia._recover_acquisition_settings!(wrong, lif, dir; series = 1)
        @test wrong["acquisitionMissing"]["reason"] == "other-stack"
    end
    img_meta(s) = (meta = Dict{String,Any}("ori_series" => s),)
    @test Cecelia._source_series(img_meta(3)) == 3
    @test Cecelia._source_series((meta = Dict{String,Any}(),)) == 0
end
