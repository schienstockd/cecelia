# Viewer navigation testset — RUBBER_DUCK_FIT_PLAN P2 (`docs/todo/RUBBER_DUCK_FIT_PLAN.md`).
# Covers `POST /api/viewer/seek` (Claude's imperative "look at frame (t, z)"). Delivery mirrors
# `viewer:mark`: the HTTP handler broadcasts a WS frame; validation lives in the handler.

@testset "API: viewer/seek — POST publishes viewer:seek WS frame with focus payload" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    cap = Channel{String}(64); key = gensym("test-seek")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    try
        uid = "TESTSEEK"; mkpath(joinpath(tmp, uid))
        s(b) = _post(api_viewer_seek, b)

        # Validation — missing / empty projectUid, unknown project, missing imageUid.
        @test s(Dict("imageUid"=>"I", "t"=>10))[1] == 400                              # projectUid required
        @test s(Dict("projectUid"=>"NOPE", "imageUid"=>"I", "t"=>10))[1] == 400        # unknown project
        @test s(Dict("projectUid"=>uid, "t"=>10))[1] == 400                            # imageUid required
        @test s(Dict("projectUid"=>uid, "imageUid"=>"I"))[1] == 400                    # need t or z

        drain()  # flush anything before the real broadcast

        # t-only seek.
        st, body = s(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "t"=>40))
        @test st == 200
        @test JSON3.read(body).ok == true
        frames = drain()
        @test length(frames) == 1
        f = frames[1]
        @test String(f.type) == "viewer:seek"
        @test String(f.projectUid) == uid
        @test String(f.imageUid)   == "IMG1"
        @test Int(f.focus.t) == 40
        @test !haskey(f.focus, :z)   # z-only omitted stays omitted (frontend preserves current z)

        # z-only seek.
        st2, _ = s(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "z"=>3))
        @test st2 == 200
        f2 = drain()[1]
        @test Int(f2.focus.z) == 3
        @test !haskey(f2.focus, :t)

        # Both.
        st3, _ = s(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "t"=>7, "z"=>2))
        @test st3 == 200
        f3 = drain()[1]
        @test Int(f3.focus.t) == 7 && Int(f3.focus.z) == 2

        # Coercion: string ints work, garbage falls back to nothing → 400 if it leaves neither set.
        st4, _ = s(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "t"=>"12"))
        @test st4 == 200
        f4 = drain()[1]
        @test Int(f4.focus.t) == 12
        @test s(Dict("projectUid"=>uid, "imageUid"=>"IMG1", "t"=>"not-a-number"))[1] == 400
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
