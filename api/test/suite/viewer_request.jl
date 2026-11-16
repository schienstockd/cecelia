# Ask-the-viewer routes (`api/src/viewer_request.jl`): MCP get_viewer_screenshot / set_viewer_view
# broadcast a `viewer:request` and wait for the window's answer on `/api/viewer/reply`.

@testset "API: viewer request — screenshot / view reach the viewer and its reply comes back" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    cap = Channel{String}(64); key = gensym("test-vreq")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    try
        uid = "TESTVREQ"; mkpath(joinpath(tmp, uid))
        @test _post(api_viewer_screenshot, Dict("imageUid"=>"I"))[1] == 400
        @test _post(api_viewer_screenshot, Dict("projectUid"=>"NOPE", "imageUid"=>"I"))[1] == 404
        @test _post(api_viewer_view, Dict("projectUid"=>uid, "imageUid"=>"I"))[1] == 400        # view required
        @test _post(api_viewer_reply, Dict("result"=>Dict("a"=>1)))[1] == 400                   # requestId required
        @test JSON3.read(_post(api_viewer_reply, Dict("requestId"=>"stray", "result"=>Dict()))[2]).ok == false

        # A "viewer": answer the broadcast request through the reply route.
        viewer = @async begin
            f = JSON3.read(take!(cap))
            @test f.type == "viewer:request" && f.kind == "view" && f.imageUid == "IMG"
            @test f.args.view.layers.A.contrast_limits == [1, 9]
            _post(api_viewer_reply, Dict("requestId"=>String(f.requestId),
                                         "result"=>Dict("view"=>Dict("dims"=>Dict("ndisplay"=>3)))))
        end
        st, body = _post(api_viewer_view, Dict("projectUid"=>uid, "imageUid"=>"IMG",
                                               "view"=>Dict("layers"=>Dict("A"=>Dict("contrast_limits"=>[1, 9])))))
        @test st == 200 && JSON3.read(body).view.dims.ndisplay == 3
        @test JSON3.read(fetch(viewer)[2]).ok == true
        @test isempty(_VIEWER_REQUESTS)                      # forgotten once answered
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end

@testset "API: viewer request — no viewer answers → reason no_viewer, late answer dropped" begin
    rid = _open_viewer_request!("t")
    @test _await_viewer_request(rid, 0.05; poll_s = 0.01) === nothing
    @test !_deliver_viewer_request!(rid, Dict("png"=>"x"))
    @test !haskey(_VIEWER_REQUESTS, rid)
end
