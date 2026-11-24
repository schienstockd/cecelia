# Data source routes (api/src/routes/data_source.jl): GET reads and writes nothing, POST writes the set's
# source or the listed images' own, a shape error is a 400, and GET /api/images/meta carries the resolved one.

@testset "API: data source" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    save(b) = (r = api_data_source_save(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    get_(uid, set) = (r = api_data_source(HTTP.Request("GET", "/api/data-source?projectUid=$uid&setUid=$set")); (r[1], JSON3.read(r[2])))
    meta(uid, iu) = JSON3.read(api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid&imageUid=$iu"))[2])
    try
        proj = create_project!(name = "api-ds")
        s = add_set!(proj; name = "atlas")
        a = add_image!(s; name = "a"); b = add_image!(s; name = "b")
        other = add_set!(proj; name = "other"); c = add_image!(other; name = "c")

        @test api_data_source(HTTP.Request("GET", "/api/data-source?projectUid=$(proj.uid)"))[1] == 400
        st, r = get_(proj.uid, s.uid)
        @test st == 200 && r.source === nothing && all(i -> i.own === nothing && i.resolved === nothing, r.images)

        src = Dict("citation" => "HuBMAP HBM666.MCZF.973", "doi" => "https://doi.org/10.35079/HBM666.MCZF.973")
        @test save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "source" => src))[1] == 200
        st, r = get_(proj.uid, s.uid)
        @test r.source.doi == "10.35079/HBM666.MCZF.973" && r.source.confirmed
        @test all(i -> i.resolved.from == "set", r.images)
        @test meta(proj.uid, a.uid).image.dataSource.citation == "HuBMAP HBM666.MCZF.973"   # what MCP reads
        @test meta(proj.uid, c.uid).image.dataSource === nothing                            # another set

        # one image's own, then cleared back to the set's
        @test save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "imageUids" => [b.uid],
                        "source" => Dict("citation" => "Collaborator")))[1] == 200
        @test meta(proj.uid, b.uid).image.dataSource.from == "image"
        @test save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "imageUids" => [b.uid], "source" => nothing))[1] == 200
        @test meta(proj.uid, b.uid).image.dataSource.from == "set"

        # shape errors are 400s and write nothing
        st, e = save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "source" => Dict("doi" => "10.1/x", "by" => "robot")))
        @test st == 400 && occursin("expected one of", e.error)
        @test save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "source" => "HuBMAP"))[1] == 400
        @test save(Dict("projectUid" => proj.uid, "setUid" => s.uid, "imageUids" => [c.uid], "source" => src))[1] == 400
        @test get_(proj.uid, s.uid)[2].source.citation == "HuBMAP HBM666.MCZF.973"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
