# Acquisition check routes (api/src/routes/acq_check.jl): the set's check runs live from image meta and
# writes nothing; saving a plan makes the set's Blackboard entry (one per set), and a changed plan is a
# new round on it.

@testset "API: acquisition check" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    post(f, b) = (r = f(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    get_(uid, set) = (r = api_acqcheck(HTTP.Request("GET", "/api/acqcheck?projectUid=$uid&setUid=$set")); (r[1], JSON3.read(r[2])))
    meta(bz) = Dict{String,Any}("PhysicalSizeX" => 0.5,
        "acquisition" => Dict("lasers" => [Dict("wavelengthNm" => 850, "transmissionPct" => 15)],
                              "brightZ" => Dict("enabled" => bz), "objective" => Dict("name" => bz ? "25x" : "20x")),
        "saturation" => Dict("channels" => [Dict("index" => 0, "profile" => Dict("byZ" => Dict("p99" => [200.0, 150.0])),
                                                 "photons" => Dict("fit" => Dict("greyPerPhoton" => 30.0),
                                                                   "objects" => Dict("p50Photons" => 1.0, "p99Photons" => 10.0)))],
                             "sharedStructure" => Any[]))
    try
        proj = create_project!(name = "api-acq-check")
        s    = add_set!(proj; name = "T cell types")
        imgs = [add_image!(s; name = nm, meta = meta(bz)) for (nm, bz) in (("a", false), ("b", true))]

        @test api_acqcheck(HTTP.Request("GET", "/api/acqcheck?projectUid=$(proj.uid)"))[1] == 400
        # no plan: the plan-free checks run, nothing is written
        st, r = get_(proj.uid, s.uid)
        @test st == 200 && r.entryId == "" && r.plan === nothing
        @test any(f -> f.code == "acq.settings_differ" && f.subs.variable == "objective", r.findings)
        @test any(f -> f.code == "acq.settings_differ" && f.subs.variable == "Bright Z setting", r.findings)
        @test "photons" in r.skipped && length(r.suggestedDesigns) == 1 && r.nOpen >= 1
        @test any(f -> f.code == "acq.channels_unnamed", r.findings)      # no names yet: asks, doesn't guess
        for img in imgs; Cecelia.set_channel_names!(img, ["OTI-uGFP"]; check_length = false); Cecelia.save!(img); end
        st, r = get_(proj.uid, s.uid)
        @test !any(f -> f.code == "acq.channels_unnamed", r.findings) && r.suggestedDesigns[1].key == "OTI-uGFP"
        @test isempty(_read_bb_registry(proj.uid))                 # a GET writes nothing
        @test _acq_entry_for(proj.uid, s.uid) == ""

        d = r.suggestedDesigns[1]
        plan = Dict("designs" => [Dict("id" => d.id, "key" => d.key)],
                    "objects" => [Dict("name" => "OT-I", "kind" => "cells", "channels" => Dict(String(d.id) => "OTI-uGFP"))])
        @test post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => Dict("objects" => [])))[1] == 400
        st, r1 = post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => plan))
        @test st == 200 && r1.round == 1 && r1.entryId != ""
        id = String(r1.entryId)
        @test any(f -> f.code == "acq.few_photons" && f.subs.what == "OT-I", r1.findings)
        a = _read_bb_meta(proj.uid, id)["acqCheck"]
        @test a["setUid"] == s.uid && length(a["rounds"]) == 1 && !isempty(a["lastCheck"]["open"])
        md = read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String)
        @test occursin("## Across the set", md) && occursin("OT-I carries", md)

        # the GET now finds the entry and runs the plan
        st, r2 = get_(proj.uid, s.uid)
        @test r2.entryId == id && r2.plan !== nothing && !("photons" in r2.skipped)
        # same plan again: same round; changed plan: round 2 on the same entry
        @test post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => plan))[2].round == 1
        plan["objects"][1]["kind"] = "points"
        st, r3 = post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => plan))
        @test st == 200 && r3.round == 2 && r3.entryId == id
        @test length(_read_bb_meta(proj.uid, id)["acqCheck"]["rounds"]) == 2
        @test !any(f -> f.code == "acq.few_photons", r3.findings)        # points are counted, not read for noise
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
