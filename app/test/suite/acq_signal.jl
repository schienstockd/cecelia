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

@testset "signal pass — no photon fit on channels the scanner processed" begin
    # What the PhenoImager reader (`qptiff_meta`) records: shading corrected, autofluorescence
    # subtracted on the marker channels, not on DAPI
    acq = Dict{String,Any}("processing" => "OffsetAndShading",
        "channels" => [Dict{String,Any}("afSubtracted" => false), Dict{String,Any}("afSubtracted" => true)])
    why = Cecelia.photons_untestable(acq)
    @test sort(collect(keys(why))) == ["0", "1"]
    @test occursin("OffsetAndShading", why["0"]) && !occursin("autofluorescence", why["0"])
    @test occursin("autofluorescence", why["1"])
    # as a stored ccid hands it back: nested JSON3 objects with Symbol keys
    @test Cecelia.photons_untestable(JSON3.read(JSON3.write(acq))) == why
    # nothing recorded, "None", or a microscope without these fields: every channel is fitted
    @test isempty(Cecelia.photons_untestable(nothing))
    @test isempty(Cecelia.photons_untestable(Dict{String,Any}("processing" => "None",
        "channels" => [Dict{String,Any}("afSubtracted" => false)])))
    @test isempty(Cecelia.photons_untestable(Dict{String,Any}("channels" => [Dict{String,Any}("name" => "SHG")])))
end

@testset "signal pass — a probe that fitted a scanner-processed channel is not current" begin
    # Refresh metadata re-probes only what `_saturation_is_current` rejects: an image probed before the
    # skip existed carries a photon fit on its AF-subtracted channel, and must be re-probed
    ch(i, photons) = Dict{String,Any}("index" => i, "profile" => Dict(), "range" => Dict(), "floor" => Dict(),
                                      "noise" => Dict(), "photons" => photons)
    acq = Dict{String,Any}("channels" => [Dict{String,Any}("afSubtracted" => false), Dict{String,Any}("afSubtracted" => true)])
    fitted = Dict{String,Any}("fit" => Dict("greyPerPhoton" => 4.4), "objects" => Dict("p50Photons" => 70.0))
    skipped = Dict{String,Any}("fit" => Dict("greyPerPhoton" => nothing), "objects" => nothing,
                               "notMeasured" => "autofluorescence subtracted by the scanner")
    meta(p1) = Dict{String,Any}("acquisition" => acq, "saturation" => Dict{String,Any}("channels" => [ch(0, fitted), ch(1, p1)]))
    @test !Cecelia._saturation_is_current(meta(fitted))
    @test Cecelia._saturation_is_current(meta(skipped))
    # as stored: JSON3 objects with Symbol keys
    @test !Cecelia._saturation_is_current(JSON3.read(JSON3.write(meta(fitted))))
    @test Cecelia._saturation_is_current(JSON3.read(JSON3.write(meta(skipped))))
    # no settings: the old rule (every key present) stands
    m = meta(fitted); delete!(m, "acquisition")
    @test Cecelia._saturation_is_current(m)
end
