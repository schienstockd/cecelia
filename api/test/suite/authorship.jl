# ── Authorship stamps — who wrote project content (docs/audit/persistence-audit.md gap C) ─────────
# Every writer records `author_stamp()` = {profile, via}: the active profile, and "claude" when the
# request came through the observer MCP (`X-Cecelia-Client: claude`, bound by the router) else "app".

@testset "Authorship — blackboard, notebooks, chains stamp {profile, via}" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()
            _post(api_kiwi_profiles_create, Dict("name" => "alice"))
            _post(api_kiwi_profiles_select, Dict("name" => "alice"))
            alice_app    = Dict("profile" => "alice", "via" => "app")
            alice_claude = Dict("profile" => "alice", "via" => "claude")
            stamp(x) = x === nothing ? nothing : Dict(String(k) => String(v) for (k, v) in x)
            via_router(path, obj; claude = false) = handle_http(
                HTTP.Request("POST", path, claude ? ["X-Cecelia-Client" => "claude"] : Pair{String,String}[]),
                Vector{UInt8}(JSON3.write(obj)))

            # The router binds `via` per request; outside a request it is "app".
            @test author_stamp() == alice_app

            # ── Blackboard: Claude creates, alice revises/tags in the app ─────────────────
            uid = "TESTAUTH"; mkpath(joinpath(tmp, uid))
            st, body = via_router("/api/blackboard/create",
                Dict("projectUid" => uid, "title" => "t", "content" => "a"); claude = true)
            @test st == 200
            eid = String(JSON3.read(body).entryId)
            get_entry() = JSON3.read(api_blackboard_entry_get(HTTP.Request("GET",
                "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))[2]).entry
            e = get_entry()
            @test stamp(e.createdBy) == alice_claude
            @test !haskey(e, :updatedBy)                    # never edited

            @test via_router("/api/blackboard/revise",
                Dict("projectUid" => uid, "entryId" => eid, "content" => "b"))[1] == 200
            e = get_entry()
            @test stamp(e.createdBy) == alice_claude        # creator survives a revise
            @test stamp(e.updatedBy) == alice_app
            @test _post(api_blackboard_outcome, Dict("projectUid" => uid, "entryId" => eid,
                "verdict" => "good", "note" => "held up"))[1] == 200
            e = get_entry()
            @test stamp(e.outcome.taggedBy) == alice_app
            @test stamp(e.createdBy) == alice_claude
            # Claude flips status → updatedBy follows the latest writer; the list row carries both.
            @test via_router("/api/blackboard/status",
                Dict("projectUid" => uid, "entryId" => eid, "status" => "resolved"); claude = true)[1] == 200
            rows = JSON3.read(api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))[2]).entries
            row = only(filter(r -> r.entryId == eid, rows))
            @test stamp(row.updatedBy) == alice_claude && stamp(row.createdBy) == alice_claude
            # The auto-made project profile entry has no author — nobody wrote it.
            prof = only(filter(r -> r.entryId == "profile", rows))
            @test !haskey(prof, :createdBy)

            # ── Notebooks: create stamps createdBy, a revise stamps updatedBy ─────────────
            @test via_router("/api/notebooks/write", Dict("projectUid" => uid, "name" => "nb",
                "cells" => ["1 + 1"]); claude = true)[1] == 200
            nbs() = JSON3.read(api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=$uid"))[2]).notebooks
            nb = only(filter(n -> n.file == "nb.jl", nbs()))
            @test stamp(nb.createdBy) == alice_claude && nb.updatedBy === nothing
            @test _post(api_notebooks_describe, Dict("projectUid" => uid, "file" => "nb.jl",
                "description" => "d"))[1] == 200
            nb = only(filter(n -> n.file == "nb.jl", nbs()))
            @test stamp(nb.createdBy) == alice_claude && stamp(nb.updatedBy) == alice_app
            # A direct snapshot freezes someone's Pluto edits → restamps; the one inside write didn't
            # (the `updatedBy === nothing` above).
            @test via_router("/api/notebooks/snapshot", Dict("projectUid" => uid, "file" => "nb.jl"); claude = true)[1] == 200
            nb = only(filter(n -> n.file == "nb.jl", nbs()))
            @test stamp(nb.updatedBy) == alice_claude
            # A restore is an edit and restamps (sent as Claude only so the change is visible).
            @test via_router("/api/notebooks/restore", Dict("projectUid" => uid, "file" => "nb.jl",
                "version" => 1, "force" => true); claude = true)[1] == 200
            nb = only(filter(n -> n.file == "nb.jl", nbs()))
            @test stamp(nb.updatedBy) == alice_claude

            # ── Chains: authorship comes from the server, not the body ────────────────────
            proj = create_project!(name = "auth-chains")
            node = Dict("id" => "n1", "fn" => "importImages.remove",
                        "params" => Dict("valueName" => "default", "newDefault" => "default"))
            cpath(n) = joinpath(tmp, proj.uid, "settings", "chains", "$(n).json")
            @test via_router("/api/chains/create", Dict("projectUid" => proj.uid,
                "template" => Dict("name" => "c1", "nodes" => [node], "edges" => [])); claude = true)[1] == 200
            @test stamp(JSON3.read(read(cpath("c1"), String)).createdBy) == alice_claude
            # The whiteboard re-saves it, with a forged createdBy in the body: the file's creator stays.
            forged = Dict("name" => "c1", "nodes" => [node], "edges" => [],
                          "createdBy" => Dict("profile" => "mallory", "via" => "app"))
            @test _post(api_chains_save, Dict("projectUid" => proj.uid, "template" => forged))[1] == 200
            raw = JSON3.read(read(cpath("c1"), String))
            @test stamp(raw.createdBy) == alice_claude && stamp(raw.updatedBy) == alice_app
            # A first save from the app is its own creator.
            @test _post(api_chains_save, Dict("projectUid" => proj.uid,
                "template" => Dict("name" => "c2", "nodes" => [node], "edges" => [])))[1] == 200
            @test stamp(JSON3.read(read(cpath("c2"), String)).createdBy) == alice_app
            # A rename is an edit: creator carries over, the renamer is the last editor.
            @test via_router("/api/chains/rename", Dict("projectUid" => proj.uid, "name" => "c1",
                "newName" => "c1b"); claude = true)[1] == 200
            raw = JSON3.read(read(cpath("c1b"), String))
            @test stamp(raw.createdBy) == alice_claude && stamp(raw.updatedBy) == alice_claude

            # ── Task requests carry the asker across the wire to the runner ───────────────
            req = Cecelia.task_request(Cecelia.task_request_dict(
                Cecelia.TaskRequest(; task_id = "t", fun_name = "f", project_uid = uid, by = "alice")))
            @test req.by == "alice"
            @test Cecelia.chain_request(Cecelia.chain_request_dict(
                Cecelia.ChainRequest(; project_uid = uid, by = "alice"))).by == "alice"
            @test Cecelia._request_by(Cecelia.TaskRequest(; task_id = "t", fun_name = "f",
                                                          project_uid = uid)) == "alice"   # "" ⇒ resolve here
        end
        init_cecelia!()   # restore
    end
end
