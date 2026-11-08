# Import from Immunemap — the register route. Its input is a real scan manifest
# (`test-data/immunemap/scan_63_two_videos.json`, produced by `cecelia.utils.immunemap` from the real
# captured responses), so the shape under test is the scan's, not one written to suit the route.

@testset "API: /api/import/register-immunemap" begin
    scan_file = normpath(joinpath(API_TEST_DIR, "..", "..", "test-data", "immunemap",
                                  "scan_63_two_videos.json"))
    if !api_have_fixture(scan_file)
        @test_skip false
    else
    m = JSON3.read(read(scan_file, String))
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-immunemap")
        s    = add_set!(proj; name = "exp63")
        body = Dict("projectUid" => proj.uid, "setUid" => s.uid,
                    "experiment" => m.experiment, "videos" => m.videos,
                    # the dialog's group × attribute table, keyed by group id as JSON object keys are
                    "groupAttrs" => Dict("101" => Dict("time" => "0 min", "antigen" => "none")))
        code, resp = _post(api_import_register_immunemap, body)
        @test code == 200
        rows = JSON3.read(resp).images
        @test [r.status for r in rows] == ["pending", "skipped"]   # 310 is private in this fixture
        @test rows[2].reason == "not public"

        img = image_by_uid(load_project(proj.uid)._sets[1]; uid = String(rows[1].uid))
        @test img.name == "Obino-Movie1-SteadyState"
        @test img.attr["condition"] == "Control, before immunization"
        @test img.attr["time"] == "0 min" && img.attr["antigen"] == "none"
        @test channel_names(img) == ["Microspheres (Rhodamine)", "B cells (GFP)",
                                     "Collagen (Autofluorescent)"]
        im = img.meta["immunemap"]
        @test im["videoUid"] == "2JBABTBNRF8MR978J016GM606A"
        @test im["md5"] == "da279b07eb1994d5b09dea2ff77da1c1"
        @test im["experimentId"] == 63
        @test im["dois"] == ["10.1016/j.celrep.2018.11.052"]
        # the interval the file lacks, from the measured timestamps (not the nominal frame rate)
        dt = img.meta["declaredTimeIncrement"]
        @test dt["value"] == 30.0 && dt["unit"] == "second"
        @test occursin("timestamps", dt["source"])

        # registering again neither duplicates nor touches the existing image
        code2, resp2 = _post(api_import_register_immunemap, body)
        @test code2 == 200
        @test JSON3.read(resp2).images[1].status == "exists"
        @test length(load_project(proj.uid)._sets[1].image_uids) == 1

        @test _post(api_import_register_immunemap, Dict("projectUid" => proj.uid))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
    end
end

@testset "immunemap_declared_interval prefers timestamps, then frame rate" begin
    @test immunemap_declared_interval(Dict("acquisitionId" => 1, "intervalFromTimeStepsSec" => 29.9,
                                           "intervalSec" => 30.0))["value"] == 29.9
    @test occursin("frame rate", immunemap_declared_interval(Dict("acquisitionId" => 1,
        "intervalFromTimeStepsSec" => nothing, "intervalSec" => 30.0))["source"])
    @test immunemap_declared_interval(Dict("acquisitionId" => 1)) === nothing
    # irregular stamps (a paused acquisition): no interval, and no fallback to the frame rate
    @test immunemap_declared_interval(Dict("acquisitionId" => 1, "timeStepsIrregular" => true,
                                           "intervalSec" => 30.0)) === nothing
end

@testset "immunemap_image_attrs: condition plus the group's split, blanks dropped" begin
    v = Dict("groupId" => 104, "condition" => "30 min post immunization, specific antigens (HEL)")
    @test immunemap_image_attrs(v, nothing) == Dict("condition" => v["condition"])
    ga = Dict("104" => Dict(" time " => "30 min", "antigen" => " HEL ", "dose" => "",
                            "condition" => "overwritten?"),
              "105" => Dict("time" => "90 min"))
    @test immunemap_image_attrs(v, ga) ==
          Dict("condition" => v["condition"], "time" => "30 min", "antigen" => "HEL")
    @test immunemap_image_attrs(Dict("groupId" => 999, "condition" => "x"), ga) == Dict("condition" => "x")
end
