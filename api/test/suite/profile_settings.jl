# ── Per-profile settings API — reader/writer + PATCH round-trip (USER_PROFILE_PLAN Phase 4) ─────
# Pure store functions against a temp config dir, then the API handlers on top. The frontend
# side of the wire (URL/body/keepalive) is `frontend/src/utils/profileSettingsApi.test.ts`.

@testset "profile_settings — path helpers" begin
    mktempdir() do tmp
        # Pure: never touches disk.
        d = Cecelia.profile_settings_dir("alice"; config_root = tmp)
        @test d == joinpath(tmp, "kiwi-profiles", "alice")
        @test !isdir(d)

        p = Cecelia.profile_settings_path("alice"; config_root = tmp)
        @test p == joinpath(d, "settings.toml")
        @test !ispath(p)

        # `default` gets a settings dir like every other profile — settings need a home even
        # for a single-seat install (credentials still live at ~/.claude for default; only
        # settings.toml sits under kiwi-profiles/default/).
        @test Cecelia.profile_settings_dir("default"; config_root = tmp) ==
              joinpath(tmp, "kiwi-profiles", "default")

        # Live resolver mkpaths the dir.
        p! = Cecelia.profile_settings_path!("alice"; config_root = tmp)
        @test isdir(joinpath(tmp, "kiwi-profiles", "alice"))
        @test p! == p
    end
end

@testset "profile_settings — reader/writer + patch merge" begin
    mktempdir() do tmp
        # Missing file → empty bag, not an error.
        @test Cecelia.read_profile_settings("alice"; config_root = tmp) == Dict{String,Any}()

        # Write then read.
        Cecelia.write_profile_settings!(Dict("theme" => "dark", "ribbonThickness" => 3),
                                        "alice"; config_root = tmp)
        got = Cecelia.read_profile_settings("alice"; config_root = tmp)
        @test got["theme"] == "dark"
        @test got["ribbonThickness"] == 3

        # Patch merges (existing key preserved, new key added).
        merged = Cecelia.patch_profile_settings!(Dict("kiwiOpen" => true),
                                                 "alice"; config_root = tmp)
        @test merged["theme"] == "dark"
        @test merged["ribbonThickness"] == 3
        @test merged["kiwiOpen"] === true

        # Patch overwrites an existing key.
        merged = Cecelia.patch_profile_settings!(Dict("theme" => "light"),
                                                 "alice"; config_root = tmp)
        @test merged["theme"] == "light"

        # `nothing` deletes a key (the wire form is JSON `null`).
        merged = Cecelia.patch_profile_settings!(Dict("ribbonThickness" => nothing),
                                                 "alice"; config_root = tmp)
        @test !haskey(merged, "ribbonThickness")
        @test merged["theme"] == "light"

        # Round-trip through disk.
        disk = Cecelia.read_profile_settings("alice"; config_root = tmp)
        @test disk == merged
    end
end

@testset "profile_settings — reader tolerates a hand-corrupted file" begin
    mktempdir() do tmp
        p = Cecelia.profile_settings_path!("alice"; config_root = tmp)
        write(p, "this is not [valid toml")
        # Corrupt file → empty bag, never a 500 on the API round-trip.
        @test Cecelia.read_profile_settings("alice"; config_root = tmp) == Dict{String,Any}()
    end
end

@testset "profile_settings — API round-trip (GET + PATCH)" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()

            # GET on a fresh profile → empty bag under `default`.
            code, body = api_profile_settings_get(HTTP.Request("GET", "/api/profile/settings"))
            @test code == 200
            b = JSON3.read(body)
            @test b.profile == "default"
            @test isempty(b.settings)

            # PATCH writes into the ACTIVE profile's settings.toml.
            payload = Dict("theme" => "dark", "kiwiOpen" => true, "ribbonThickness" => 3)
            code, body = api_profile_settings_patch(Vector{UInt8}(JSON3.write(payload)))
            @test code == 200
            b = JSON3.read(body)
            @test b.ok === true
            @test b.profile == "default"
            @test b.settings.theme == "dark"
            @test b.settings.kiwiOpen === true
            @test b.settings.ribbonThickness == 3

            # File landed under kiwi-profiles/default/settings.toml.
            @test isfile(joinpath(tmp, "kiwi-profiles", "default", "settings.toml"))

            # GET reflects the merged state.
            code, body = api_profile_settings_get(HTTP.Request("GET", "/api/profile/settings"))
            b = JSON3.read(body)
            @test b.settings.theme == "dark"

            # A second PATCH shallow-merges and can delete via JSON `null`.
            code, body = api_profile_settings_patch(
                Vector{UInt8}(JSON3.write(Dict("theme" => "light", "ribbonThickness" => nothing))))
            @test code == 200
            b = JSON3.read(body)
            @test b.settings.theme == "light"
            @test b.settings.kiwiOpen === true
            @test !haskey(b.settings, :ribbonThickness)
        end
    end
end

@testset "profile_settings — PATCH rejects a non-object body" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()

            # Array body (legal JSON, wrong shape) → 400, not a 500.
            code, body = api_profile_settings_patch(Vector{UInt8}("[1,2,3]"))
            @test code == 400
            @test JSON3.read(body).ok === false

            # Garbage bytes → 400 too.
            code, body = api_profile_settings_patch(Vector{UInt8}("not json"))
            @test code == 400
            @test JSON3.read(body).ok === false
        end
    end
end

@testset "profile_settings — patch scoped to the active profile after a select" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()

            # Create + select alice.
            code, _ = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200
            code, _ = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200
            @test active_profile_name() == "alice"

            # PATCH lands under alice/, not default/.
            code, _ = api_profile_settings_patch(
                Vector{UInt8}(JSON3.write(Dict("theme" => "dark"))))
            @test code == 200
            @test isfile(joinpath(tmp, "kiwi-profiles", "alice", "settings.toml"))

            # Switch back — default's bag is still empty.
            code, _ = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "default"))))
            @test code == 200
            code, body = api_profile_settings_get(HTTP.Request("GET", "/api/profile/settings"))
            b = JSON3.read(body)
            @test b.profile == "default"
            @test isempty(b.settings)

            # And alice's bag is intact when we swing back.
            api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "alice"))))
            code, body = api_profile_settings_get(HTTP.Request("GET", "/api/profile/settings"))
            b = JSON3.read(body)
            @test b.profile == "alice"
            @test b.settings.theme == "dark"
        end
    end
end
