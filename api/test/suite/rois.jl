# ── ROI list routes (docs/todo/ROI_TRIAGE_PLAN.md §7) ──────────────────────────────────────────
# GET /api/rois + POST /api/rois/review through the router, so `via` is bound the way the MCP binds
# it (`X-Cecelia-Client: claude`): a person's write lands, Claude's accept is refused with 409, and
# every write broadcasts `rois:changed`. The guard rules themselves are unit-tested in app/test.

@testset "API: rois list + review — attribution and the Claude guard" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    cap = Channel{String}(64); key = gensym("test-rois")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    via_router(path, obj; claude = false) = handle_http(
        HTTP.Request("POST", path, claude ? ["X-Cecelia-Client" => "claude"] : Pair{String,String}[]),
        Vector{UInt8}(JSON3.write(obj)))
    try
        proj = create_project!(name = "rois-api")
        img = add_image!(add_set!(proj; name = "s"); name = "i", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        mkpath(img_rois_dir(img))
        roi = Dict{String,Any}("id" => "r0000000a", "rank" => 1, "t0" => 3, "t1" => 9,
            "box" => Dict("x0" => 0, "x1" => 9, "y0" => 0, "y1" => 9, "z0" => 0, "z1" => 2),
            "objects" => Any[Dict("label" => 11), Dict("label" => 12)])
        write_json_atomic(img_rois_path(img, "default"), Dict{String,Any}("version" => 1, "valueName" => "B", "rois" => Any[roi]))

        q = "projectUid=$(proj.uid)&imageUid=$(img.uid)"
        st, body = api_rois_list(HTTP.Request("GET", "/api/rois?$q"))
        @test st == 200
        d = JSON3.read(body)
        @test String(d.suffix) == "default" && collect(String, d.suffixes) == ["default"]
        @test String(d.rois[1].status) == "pending"
        @test collect(Int, d.rois[1].objectLabels) == [11, 12]
        @test api_rois_list(HTTP.Request("GET", "/api/rois?projectUid=$(proj.uid)&imageUid=NOPE"))[1] == 404

        base = Dict{String,Any}("projectUid" => proj.uid, "imageUid" => img.uid, "roiId" => "r0000000a")
        drain()
        # Claude proposes a label (ids + frames inside the ROI) and may not accept
        cl = Dict("type" => "engulfment", "confidence" => 0.7, "reason" => "inside CD169",
                  "objectIds" => [11], "frames" => [5])
        @test via_router("/api/rois/review", merge(base, Dict("claudeLabel" => cl)); claude = true)[1] == 200
        @test via_router("/api/rois/review", merge(base, Dict("status" => "accepted")); claude = true)[1] == 409
        @test via_router("/api/rois/review", merge(base, Dict("claudeLabel" => merge(cl, Dict("objectIds" => [99])))); claude = true)[1] == 400
        frames = drain()
        @test length(frames) == 1 && String(frames[1].type) == "rois:changed"
        # the person accepts
        st, body = via_router("/api/rois/review", merge(base, Dict("status" => "accepted", "userLabel" => "engulfment")))
        @test st == 200
        rv = JSON3.read(body).review
        @test String(rv.status) == "accepted"
        @test String(rv.claudeLabel.by.via) == "claude"
        @test String(rv.history[end].by.via) == "app"
        @test via_router("/api/rois/review", merge(base, Dict("roiId" => "rNOPE", "status" => "rejected")))[1] == 404
        @test via_router("/api/rois/review", base)[1] == 400                          # nothing to change
        d2 = JSON3.read(api_rois_list(HTTP.Request("GET", "/api/rois?$q"))[2])
        @test String(d2.rois[1].status) == "accepted"
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
