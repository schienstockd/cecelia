# RESULT-REVIEW: result-00a56292
# A camera image whose settings list fewer channels than the store holds (a qptiff with one settings
# channel and a 3-channel store): every store channel is a camera channel, listed or not, as the
# data-path panel reads it (`acquisitionDataPath.ts` `cameraChannel`).

@testset "camera image — channels the settings do not list are not photon-counted either" begin
    acq = Dict{String,Any}("camera" => Dict{String,Any}("name" => "IMX421"),
                           "channels" => [Dict{String,Any}("name" => "DAPI")])
    on_camera = Cecelia.not_photon_counting(acq)
    @test all(i -> haskey(on_camera, string(i)) || haskey(on_camera, "*"), 0:2)
    # a stored probe with a photon-counted floor on an unlisted store channel is re-probed
    ch(i, kind) = Dict{String,Any}("index" => i, "profile" => Dict(), "range" => Dict(),
                                   "floor" => Dict{String,Any}("kind" => kind), "noise" => Dict(),
                                   "photons" => Dict{String,Any}("fit" => Dict("greyPerPhoton" => 30.0)))
    meta = Dict{String,Any}("acquisition" => acq, "saturation" => Dict{String,Any}("channels" =>
        [ch(0, "indeterminate"), ch(1, "indeterminate"), ch(2, "photon-counted")]))
    @test !Cecelia._saturation_is_current(meta)
end
