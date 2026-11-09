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
