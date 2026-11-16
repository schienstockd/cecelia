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

# MCP get_plot_image / set_plot_view: a `plot:request` to the panel registered under plotId, answered on the same
# reply route; an unregistered plotId answers "not_open" at once, without a broadcast.
@testset "API: plot image / view — reach the registered panel, not_open for an unknown plot" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    cap = Channel{String}(64); key = gensym("test-preq")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    try
        uid = "TESTPREQ"; mkpath(joinpath(tmp, uid))
        _reset_plots_registry_for_test!()
        @test _post(api_viewer_plot_image, Dict("projectUid"=>uid))[1] == 400                  # plotId required
        @test _post(api_viewer_plot_image, Dict("projectUid"=>"NOPE", "plotId"=>"p"))[1] == 404
        st, body = _post(api_viewer_plot_image, Dict("projectUid"=>uid, "plotId"=>"cluster:9"))
        @test st == 200 && JSON3.read(body).reason == "not_open"
        @test _post(api_viewer_plot_view, Dict("projectUid"=>uid, "plotId"=>"cluster:9"))[1] == 400    # settings required
        st, body = _post(api_viewer_plot_view, Dict("projectUid"=>uid, "plotId"=>"cluster:9", "settings"=>Dict("colourBy"=>"population")))
        @test st == 200 && JSON3.read(body).reason == "not_open"
        @test !isready(cap)                                  # nothing broadcast for an unknown plot

        @test _post(api_viewer_plots_register, Dict("clientId"=>"c", "projectUid"=>uid, "plotId"=>"cluster:3",
                                                    "family"=>"umap", "title"=>"UMAP", "route"=>"/cluster"))[1] == 200
        panel = @async begin
            f = JSON3.read(take!(cap))
            @test f.type == "plot:request" && f.kind == "image" && f.plotId == "cluster:3" && f.projectUid == uid
            @test f.args.maxSide == 800
            _post(api_viewer_reply, Dict("requestId"=>String(f.requestId),
                                         "result"=>Dict("png"=>"data:image/png;base64,AAA", "width"=>800, "height"=>600)))
        end
        st, body = _post(api_viewer_plot_image, Dict("projectUid"=>uid, "plotId"=>"cluster:3", "maxSide"=>800))
        r = JSON3.read(body)
        @test st == 200 && r.png == "data:image/png;base64,AAA" && r.width == 800
        @test JSON3.read(fetch(panel)[2]).ok == true

        # set_plot_view: the patch reaches the panel as `args.settings`; its answer comes back verbatim
        panel = @async begin
            f = JSON3.read(take!(cap))
            @test f.type == "plot:request" && f.kind == "view" && f.args.settings.colourBy == "population"
            _post(api_viewer_reply, Dict("requestId"=>String(f.requestId),
                                         "result"=>Dict("settings"=>Dict("colourBy"=>Dict("value"=>"population")))))
        end
        st, body = _post(api_viewer_plot_view, Dict("projectUid"=>uid, "plotId"=>"cluster:3",
                                                    "settings"=>Dict("colourBy"=>"population")))
        @test st == 200 && JSON3.read(body).settings.colourBy.value == "population"
        @test JSON3.read(fetch(panel)[2]).ok == true
        @test isempty(_VIEWER_REQUESTS)
    finally
        _reset_plots_registry_for_test!()
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
