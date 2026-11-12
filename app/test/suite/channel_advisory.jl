# ── Channel-pick + denoise model advisories (form-time twins of the import facts and Run refusals) ─
# channel_signal_flags / channel_photon_limited (qc.jl), channel_pick_advisory + its registrations
# (tasks/channel_advisory.jl), denoise_input_frames / denoise_short_movie / _denoise_model_advisory
# (tasks/cleanupImages/denoise_advisory.jl).

# Import meta as the probe writes it, read back through JSON3 (Symbol keys), so the helpers are
# tested on the shape they get from a persisted ccid.json.
_adv_meta(chs) = Dict{String,Any}("saturation" => JSON3.read(JSON3.write(Dict("channels" => chs))))

function _adv_img(uid, names, chs; t = 0)
    img = CciaImage(; uid = uid, name = uid, dir = "")
    img.im_channel_names = Dict{String,Union{Vector{String},String}}("default" => names, "_active" => "default")
    img.meta = _adv_meta(chs)
    t > 0 && (img.meta["SizeT"] = t)
    img
end

_ADV_CHS = [Dict("index" => 0, "saturated" => true,  "zeroFrac" => 0.95),
                  Dict("index" => 1, "saturated" => false, "zeroFrac" => 0.20),
                  Dict("index" => 2, "saturated" => false),                 # probe had no zero fraction
                  Dict("index" => 3, "saturated" => false, "zeroFrac" => 0.93)]

@testset "channel_signal_flags — the import facts a channel pick reads" begin
    f = Cecelia.channel_signal_flags(_adv_meta(_ADV_CHS), [0, 1, 2])
    @test f.saturated == [0]
    @test f.photonLimited == [0]
    @test f.notPhotonLimited == [1]                 # channel 2 has no number: in neither list
    @test Cecelia.channel_signal_flags(_adv_meta(_ADV_CHS), [1]).saturated == Int[]
    @test Cecelia.channel_signal_flags(Dict{String,Any}(), [0]) ==
          (saturated = Int[], photonLimited = Int[], notPhotonLimited = Int[])
    # The two existing readers go through it and keep their answers.
    @test Cecelia._denoise_saturated_channels(_adv_meta(_ADV_CHS), [0, 1]) == [0]
    pl = Cecelia.photon_limited_qc_findings(_adv_meta(_ADV_CHS))
    @test length(pl) == 1
    @test pl[1]["detail"]["channels"] == [0, 3]
end

@testset "channel_pick_advisory — under the channel picker, via the registry" begin
    v(fun, key, val, imgs) = Cecelia.validate_param(fun, key, val, imgs, Dict{String,Any}())
    one = [_adv_img("a", ["SHG", "mem-TOM", "CD8", "GFP"], _ADV_CHS)]

    s = v("cleanupImages.denoise", "channels", ["SHG"], one)
    @test s.severity == "warn"
    @test s.message == "SHG saturated: skipped at Run"
    n = v("cleanupImages.denoise", "channels", ["mem-TOM"], one)
    @test n.severity == "warn"
    @test n.message == "mem-TOM not photon-limited: little noise to remove"
    both = v("cleanupImages.denoise", "channels", ["SHG", "mem-TOM"], one)
    @test startswith(both.message, "SHG saturated")          # saturation first…
    @test occursin("Also not photon-limited: mem-TOM", both.tip)   # …the other in the tip

    # Smoothing gives no saturation reason, but a saturated pick is never a green tick.
    @test v("cleanupImages.smooth", "channels", ["SHG"], one) === nothing
    ok = v("cleanupImages.smooth", "channels", ["GFP"], one)
    @test ok.severity == "ok"
    @test ok.message == "Photon-limited"
    # AF correction has no photon reason: a clean pick says nothing.
    @test v("cleanupImages.afCorrect", "targetChannel", "mem-TOM", one) === nothing
    @test v("cleanupImages.afCorrect", "targetChannel", "SHG", one).severity == "warn"
    @test v("opticalFlow.trainSupportDenoise", "trainChannels", ["SHG"], one).message ==
          "SHG saturated: no shot noise to learn"

    # Several images: names how many carry the flag.
    two = [one[1], _adv_img("b", ["SHG", "mem-TOM", "CD8", "GFP"], [Dict("index" => 0, "zeroFrac" => 0.97)])]
    @test v("cleanupImages.denoise", "channels", ["SHG"], two).message == "SHG saturated: skipped at Run (1 of 2 images)"

    # Silence: no pick, a name no image has, no numbers.
    @test v("cleanupImages.denoise", "channels", String[], one) === nothing
    @test v("cleanupImages.denoise", "channels", ["nope"], one) === nothing
    @test v("cleanupImages.denoise", "channels", ["CD8"], one) === nothing
    @test v("cleanupImages.denoise", "channels", ["SHG"], CciaImage[]) === nothing
end

@testset "denoise model advisory — no model, missing model, temporal window" begin
    pooled(w)  = (kind = :pooled, rootPath = "", manifest = Dict{String,Any}("arch" => Dict("inputFrames" => w)),
                  perChannel = Dict{String,Any}())
    @test Cecelia.denoise_input_frames(pooled(31)) == 31
    @test Cecelia.denoise_input_frames((kind = :pooled, rootPath = "", manifest = Dict{String,Any}(),
                                        perChannel = Dict{String,Any}())) == 61   # the runner's fallback
    bundle = (kind = :perChannel, rootPath = "", manifest = Dict{String,Any}("mode" => "perChannel"),
              perChannel = Dict{String,Any}("A" => (ptPath = "", manifest = Dict("arch" => Dict("inputFrames" => 21))),
                                            "B" => (ptPath = "", manifest = Dict("arch" => Dict("inputFrames" => 41)))))
    @test Cecelia.denoise_input_frames(bundle) == 41                 # the widest sub-model…
    @test Cecelia.denoise_input_frames(bundle, ["A"]) == 21          # …among the picked channels
    top = (kind = :perChannel, rootPath = "", manifest = Dict{String,Any}("arch" => Dict("inputFrames" => 99)),
           perChannel = bundle.perChannel)
    @test Cecelia.denoise_input_frames(top, ["A"]) == 21             # a bundle's top manifest is not a model
    @test Cecelia.denoise_short_movie(61, 30)
    @test !Cecelia.denoise_short_movie(61, 61)
    @test !Cecelia.denoise_short_movie(61, 0)                         # unknown T is not refused

    adv(val, imgs; models = () -> [1], resolve = _ -> pooled(41)) =
        Cecelia._denoise_model_advisory(val, imgs, Dict{String,Any}(); models = models, resolve = resolve)
    mk(uid, t) = _adv_img(uid, ["A"], Dict[]; t = t)

    e = adv("", [mk("a", 100)]; models = () -> [])
    @test e.severity == "warn"
    @test e.message == "No model yet: train one first"
    @test adv("", [mk("a", 100)]) === nothing                         # models exist, none picked yet
    @test adv("gone", [mk("a", 100)]; resolve = _ -> nothing).severity == "fail"
    @test adv("m", [mk("a", 100), mk("b", 50)]).severity == "ok"
    w = adv("m", [mk("a", 100), mk("b", 30)])
    @test w.severity == "warn"
    @test w.message == "1 of 2 movies shorter than the 41-frame window"
    @test adv("m", [mk("a", 30)]).severity == "fail"
    @test adv("m", [mk("a", 0)]) === nothing                          # no SizeT: silence
    # the window follows the picked channels (the form's `channels` sibling)…
    @test Cecelia._denoise_model_advisory("m", [mk("a", 30)], Dict{String,Any}("channels" => ["A"]);
                                          models = () -> [1], resolve = _ -> bundle).severity == "ok"
    # …minus the ones Run's saturation gate drops: B (41 frames) saturated, so A's 21 decides.
    ab = _adv_img("a", ["A", "B"], [Dict("index" => 1, "saturated" => true)]; t = 30)
    @test Cecelia._denoise_model_advisory("m", [ab], Dict{String,Any}("channels" => ["A", "B"]);
                                          models = () -> [1], resolve = _ -> bundle).severity == "ok"
    ab2 = _adv_img("a", ["A", "B"], Dict[]; t = 30)
    @test Cecelia._denoise_model_advisory("m", [ab2], Dict{String,Any}("channels" => ["A", "B"]);
                                          models = () -> [1], resolve = _ -> bundle).severity == "fail"
    @test haskey(Cecelia.PARAM_VALIDATORS, ("cleanupImages.denoise", "model"))
end

@testset "stamp_backend_advisories! — the form asks for exactly the registered params" begin
    spec = Dict{String,Any}("params" => Any[
        Dict{String,Any}("key" => "channels", "type" => "channelSelection"),
        Dict{String,Any}("key" => "batchSize", "type" => "int"),
        Dict{String,Any}("key" => "adv", "type" => "section",
                         "params" => Any[Dict{String,Any}("key" => "model", "type" => "select")])])
    Cecelia.stamp_backend_advisories!(spec, "cleanupImages.denoise")
    @test spec["params"][1]["backendAdvisory"] === true
    @test !haskey(spec["params"][2], "backendAdvisory")
    @test spec["params"][3]["params"][1]["backendAdvisory"] === true          # inside a section
    other = Dict{String,Any}("params" => Any[Dict{String,Any}("key" => "channels")])
    @test !haskey(Cecelia.stamp_backend_advisories!(other, "cleanupImages.driftCorrect")["params"][1],
                  "backendAdvisory")
end
