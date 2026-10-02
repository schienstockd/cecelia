# ── Project ownership — USER_PROFILE_PLAN Phase 5 (create-time stamp + claim/unclaim) ────────────
# `owners: []` on disk means "pre-identity, visible to all" — the migration case. A project *created*
# while a profile is active is not that case: the server knows who created it and stamps them as
# owner at create-time so the "Mine" filter shows it immediately without a Claim round-trip. Claim
# and unclaim (routes shipped in the same Phase 5) round-trip as-is.

@testset "Project ownership — create-time stamp + claim/unclaim" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()

            # Default profile is active → create should stamp `owners: ["default"]`.
            code, body = _post(api_projects_create, Dict("name" => "owned-at-create"))
            @test code == 200
            b = JSON3.read(body)
            uid_default = String(b.project.uid)
            @test [String(x) for x in b.project.owners] == ["default"]
            # On-disk state matches the response — not just the returned meta.
            raw = JSON3.read(read(joinpath(tmp, uid_default, "project.json"), String))
            @test [String(x) for x in raw.owners] == ["default"]

            # Switch to alice → creating a project stamps alice, not the previous default.
            _post(api_kiwi_profiles_create, Dict("name" => "alice"))
            _post(api_kiwi_profiles_select, Dict("name" => "alice"))
            @test active_profile_name() == "alice"
            code, body = _post(api_projects_create, Dict("name" => "owned-by-alice"))
            @test code == 200
            b = JSON3.read(body)
            uid_alice = String(b.project.uid)
            @test [String(x) for x in b.project.owners] == ["alice"]

            # Unclaim by alice → owners empties to `[]` (kept as an explicit "cleared", per the
            # comment on api_projects_unclaim).
            code, body = _post(api_projects_unclaim, Dict("uid" => uid_alice))
            @test code == 200
            @test collect(JSON3.read(body).owners) == []

            # Claim by alice → back to ["alice"], idempotent.
            code, body = _post(api_projects_claim, Dict("uid" => uid_alice))
            @test code == 200
            @test [String(x) for x in JSON3.read(body).owners] == ["alice"]
            code, body = _post(api_projects_claim, Dict("uid" => uid_alice))
            @test code == 200
            @test [String(x) for x in JSON3.read(body).owners] == ["alice"]

            # Switch back to default → claim adds default alongside alice (multi-owner is fine per
            # Decision 5: presence in the list is the grant, no owner/collaborator distinction).
            _post(api_kiwi_profiles_select, Dict("name" => "default"))
            code, body = _post(api_projects_claim, Dict("uid" => uid_alice))
            @test code == 200
            @test sort(String[String(x) for x in JSON3.read(body).owners]) == ["alice", "default"]
        end
        init_cecelia!()   # restore
    end
end

@testset "Project ownership — follows a profile rename, dropped on delete; recents per profile" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()
            _post(api_kiwi_profiles_create, Dict("name" => "alice"))
            _post(api_kiwi_profiles_create, Dict("name" => "bob"))
            _post(api_kiwi_profiles_select, Dict("name" => "alice"))
            uid = String(JSON3.read(_post(api_projects_create, Dict("name" => "p-alice"))[2]).project.uid)
            owners() = [String(x) for x in JSON3.read(read(joinpath(tmp, uid, "project.json"), String)).owners]

            # Opening it records alice's recent, not bob's.
            @test _post(api_projects_load, Dict("uid" => uid))[1] == 200
            @test haskey(Cecelia.read_profile_recents("alice"), uid)
            @test !haskey(Cecelia.read_profile_recents("bob"), uid)

            _post(api_kiwi_profiles_select, Dict("name" => "bob"))
            @test _post(api_kiwi_profiles_rename, Dict("oldName" => "alice", "newName" => "alicia"))[1] == 200
            @test owners() == ["alicia"]
            @test haskey(Cecelia.read_profile_recents("alicia"), uid)   # recents moved with the dir
            @test Cecelia.profile_names("alicia") == ["alicia", "alice"]  # former name kept
            proj = load_project(uid)
            Cecelia.write_json_atomic(Cecelia._dismissed_path(proj), Dict("alice" => ["h1"]))
            @test read_dismissed(proj, "alicia") == ["h1"]             # alice's hides stay hers

            @test _post(api_kiwi_profiles_delete, Dict("name" => "alicia"))[1] == 200
            @test owners() == String[]                                 # visible to all, not orphaned
        end
    end
end
