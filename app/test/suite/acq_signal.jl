# ── The import probe's signal pass (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md) ─────────────────────
# What `data_path.measure_signal` adds to `meta["saturation"]` — photons per channel, shared structure
# — and how `img_cleanup_facts` (get_image_info's cleanupFacts) passes it on. Its own file:
# `ome_qc.jl` carries a result-review repro and is guarded as a whole.

@testset "signal pass — probe writes photons and shared structure" begin
    # Real integer store (ZARRFMT/ZV2img) through the import's own `saturation_run.py`
    src = fixture_path("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    if !have_fixture(src)
        @test_skip "ZARRFMT fixture missing"
    else
        proj = create_project!(name = "signal-pass-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img", meta = Dict{String,Any}("SizeT" => 1))
        cp(src, joinpath(img_zero_dir(img), "img.ome.zarr"))
        img.filepath["default"]            = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)
        @test Cecelia.ensure_saturation_meta!(init_object(proj.uid, img.uid))
        r = init_object(proj.uid, img.uid)
        chans = r.meta["saturation"]["channels"]
        # a photon fit per channel: a number, or why not
        @test all(ch -> haskey(ch["photons"]["fit"], "greyPerPhoton"), chans)
        @test r.meta["saturation"]["sharedStructure"] isa AbstractVector
        @test all(c -> haskey(c, "photons"), Cecelia.img_cleanup_facts(r)["channels"])
        # a stored probe without `photons` is out of date → Refresh metadata re-probes it
        @test "photons" in Cecelia.SATURATION_CURRENT_KEYS
        # the probe estimates no drift (the drift correction task's job), and a current probe isn't re-run
        @test !haskey(r.meta["saturation"], "drift")
        @test !Cecelia.ensure_saturation_meta!(init_object(proj.uid, img.uid))
        rm(proj.root; recursive = true)
    end
end

@testset "signal pass — cleanup facts carry shared structure, no import drift" begin
    proj = create_project!(name = "signal-facts-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    sat  = Dict{String,Any}("channels" => [Dict{String,Any}("index" => 0, "saturated" => false)])
    img  = add_image!(s; name = "img", meta = Dict{String,Any}("saturation" => sat,
                      "PhysicalSizeX" => 0.5, "PhysicalSizeY" => 0.5))
    f = Cecelia.img_cleanup_facts(img)
    @test f["sharedStructure"] === nothing                                   # probed before the signal pass
    # a drift an older probe stored is not passed on: drift is the drift correction task's (`drift`)
    sat["drift"] = Dict{String,Any}("axes" => ["Z", "Y", "X"], "excursionPx" => [9.0, 12.0, 16.0])
    sat["sharedStructure"] = [Dict{String,Any}("a" => 0, "b" => 1, "r" => 0.8)]
    img.meta["saturation"] = sat
    f = Cecelia.img_cleanup_facts(img)
    @test only(f["sharedStructure"])["r"] == 0.8
    @test !haskey(f, "importDrift") && isempty(f["drift"])
    rm(proj.root; recursive = true)
end
