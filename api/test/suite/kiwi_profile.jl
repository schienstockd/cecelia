# ── Kiwi profile API — routes + validators (LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6) ──────────────
# Pure pieces + route round-trip against a temp config dir. Live `claude login` (or spawning a real
# shell) is out of CI — that's what the terminal one-liner exists for.

@testset "Kiwi profile — name validator" begin
    ok = _valid_kiwi_profile_name
    @test ok("alice")
    @test ok("dominik")
    @test ok("lab-user_2")
    @test ok("a") && ok("z" ^ 32)
    @test !ok("")                     # empty
    @test !ok("z" ^ 33)               # too long
    @test !ok("Alice")                # uppercase — case-insensitive filesystems drop it
    @test !ok("has space")
    @test !ok("with/slash")           # path traversal
    @test !ok("with.dot")
    @test !ok("legacy")               # reserved (D10 sentinel — mustn't collide)
    @test !ok("peanut")               # frontend display alias for `default` — create-blocked
end

@testset "Kiwi profile — terminal one-liner (POSIX)" begin
    # Default profile → no CLAUDE_CONFIG_DIR (falls back to ~/.claude*); scrub still applies; the
    # trailing token is `claude` itself, not a shell (the one-liner launches Claude directly).
    cmd_default = Cecelia.kiwi_terminal_command(""; claude_bin = "/usr/local/bin/claude",
                                                    is_windows = false)
    @test occursin("-u ANTHROPIC_API_KEY",     cmd_default)
    @test occursin("-u ANTHROPIC_AUTH_TOKEN",  cmd_default)
    @test !occursin("CLAUDE_CONFIG_DIR",       cmd_default)
    @test endswith(cmd_default,                "/usr/local/bin/claude")

    # Named profile → CLAUDE_CONFIG_DIR set to that dir + scrub + direct `claude` launch.
    cmd_named = Cecelia.kiwi_terminal_command("/tmp/kiwi/alice"; claude_bin = "/usr/local/bin/claude",
                                                                  is_windows = false)
    @test occursin("CLAUDE_CONFIG_DIR=/tmp/kiwi/alice", cmd_named)
    @test occursin("-u ANTHROPIC_API_KEY",              cmd_named)
    @test endswith(cmd_named,                           "/usr/local/bin/claude")
    for k in Cecelia._AMBIENT_CLAUDE_ENV
        @test occursin("-u $(k)", cmd_named)
    end

    # CLI missing (agent_bin_path returned nothing) → bare `claude` at the tail so the missing-CLI
    # error surfaces on paste rather than silently omitting the command.
    cmd_missing = Cecelia.kiwi_terminal_command("/tmp/kiwi/alice"; claude_bin = nothing,
                                                                   is_windows = false)
    @test endswith(cmd_missing, " claude")
end

@testset "Kiwi profile — terminal one-liner (Windows)" begin
    # Default profile → PowerShell branch scrubs but does NOT set CLAUDE_CONFIG_DIR.
    cmd_default = Cecelia.kiwi_terminal_command(""; claude_bin = "C:\\Program Files\\Claude\\claude.cmd",
                                                    is_windows = true)
    @test occursin("powershell -NoProfile",                             cmd_default)
    @test occursin("Remove-Item Env:ANTHROPIC_API_KEY",                 cmd_default)
    @test occursin("Remove-Item Env:ANTHROPIC_AUTH_TOKEN",              cmd_default)
    @test !occursin("CLAUDE_CONFIG_DIR",                                cmd_default)
    @test occursin("& 'C:\\Program Files\\Claude\\claude.cmd'",         cmd_default)

    # Named profile → $env:CLAUDE_CONFIG_DIR = '<dir>' + scrub + direct `claude` launch.
    cmd_named = Cecelia.kiwi_terminal_command("C:\\Users\\alice\\.claude";
                                              claude_bin = "C:\\Program Files\\Claude\\claude.cmd",
                                              is_windows = true)
    @test occursin("\$env:CLAUDE_CONFIG_DIR = 'C:\\Users\\alice\\.claude'", cmd_named)
    @test occursin("Remove-Item Env:ANTHROPIC_API_KEY",                     cmd_named)
    @test occursin("& 'C:\\Program Files\\Claude\\claude.cmd'",             cmd_named)
    for k in Cecelia._AMBIENT_CLAUDE_ENV
        @test occursin("Remove-Item Env:$(k)", cmd_named)
    end
end

@testset "Kiwi profile — roster + select + create round-trip" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()

            # Empty roster → just `default`.
            code, body = api_kiwi_profiles_list(HTTP.Request("GET", "/api/kiwi/profiles"))
            @test code == 200
            b = JSON3.read(body)
            @test b.active == "default"
            names = [String(p.name) for p in b.profiles]
            @test names == ["default"]
            @test "legacy" in [String(x) for x in b.legacyReserved]

            # Create alice → mkpath under user-profiles/alice, response carries terminal cmd.
            code, body = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200
            b = JSON3.read(body)
            @test b.ok === true && b.name == "alice"
            @test isdir(joinpath(tmp, "user-profiles", "alice"))
            # Platform-agnostic — POSIX `CLAUDE_CONFIG_DIR=<path>` and PowerShell
            # `$env:CLAUDE_CONFIG_DIR = '<path>'` both mention the profile name.
            @test occursin("CLAUDE_CONFIG_DIR", String(b.terminalCommand))
            @test occursin("alice",             String(b.terminalCommand))

            # Duplicate create → 409, no rewrite.
            code, _ = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 409

            # Invalid name → 400.
            code, _ = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "Bad Name"))))
            @test code == 400
            code, _ = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "legacy"))))
            @test code == 400
            code, _ = api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "default"))))
            @test code == 400

            # Roster now has default + alice, alphabetically.
            code, body = api_kiwi_profiles_list(HTTP.Request("GET", "/api/kiwi/profiles"))
            b = JSON3.read(body)
            @test [String(p.name) for p in b.profiles] == ["default", "alice"]

            # USER_PROFILE_PLAN Phase 4 introduced `user-profiles/default/` as a REAL on-disk dir
            # (that's where the default profile's settings.toml lives — profile_settings_dir
            # mkpaths it). Before this ratchet, the roster picker showed TWO "default" rows:
            # one synthetic + one from disk. `_kiwi_list_profiles` must skip the literal name
            # when it walks the directory.
            mkpath(joinpath(tmp, "user-profiles", "default"))
            code, body = api_kiwi_profiles_list(HTTP.Request("GET", "/api/kiwi/profiles"))
            b = JSON3.read(body)
            names = [String(p.name) for p in b.profiles]
            @test names == ["default", "alice"]          # only ONE default
            @test count(==( "default"), names) == 1

            # Select alice → hot-reloads config, active_profile_name() sees it.
            code, body = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200 && JSON3.read(body).active == "alice"
            @test active_profile_name() == "alice"
            @test active_profile_dir() == joinpath(tmp, "user-profiles", "alice")

            # Select `default` → resolves back to empty dir marker.
            code, _ = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "default"))))
            @test code == 200
            @test active_profile_name() == "default"
            @test active_profile_dir() == ""

            # Select nonexistent → 404, config unchanged.
            code, _ = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "ghost"))))
            @test code == 404
            @test active_profile_name() == "default"

            # Terminal one-liner route: active vs explicit profile. Platform-agnostic — the
            # default-profile branch omits CLAUDE_CONFIG_DIR on both POSIX and PowerShell; a
            # named-profile branch names the profile in either syntax.
            code, body = api_kiwi_terminal_command(HTTP.Request("GET", "/api/kiwi/terminal/command"))
            @test code == 200
            b = JSON3.read(body)
            @test b.profile == "default"
            @test !occursin("CLAUDE_CONFIG_DIR", String(b.command))
            code, body = api_kiwi_terminal_command(HTTP.Request("GET", "/api/kiwi/terminal/command?profile=alice"))
            b = JSON3.read(body)
            @test b.profile == "alice"
            @test occursin("CLAUDE_CONFIG_DIR", String(b.command))
            @test occursin("alice",             String(b.command))
            code, body = api_kiwi_terminal_command(HTTP.Request("GET", "/api/kiwi/terminal/command?profile=Bad"))
            @test code == 400
        end
        init_cecelia!()   # restore
    end
end

@testset "Kiwi profile — retire (D11)" begin
    mktempdir() do tmp
        write(joinpath(tmp, "custom.toml"), "[dirs]\nprojects = '$(tmp)'\n")
        withenv("CECELIA_DEV_DIR" => tmp) do
            init_cecelia!()
            # Seed two profiles.
            api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "alice"))))
            api_kiwi_profiles_create(Vector{UInt8}(JSON3.write((; name = "bob"))))

            # Retire alice while `default` is active → marker written, active unchanged.
            code, body = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200
            b = JSON3.read(body)
            @test b.ok === true && b.snappedToDefault === false && b.active == "default"
            @test isfile(joinpath(tmp, "user-profiles", "alice", ".kiwi-retired"))

            # Roster still lists alice, now flagged retired; bob stays not-retired.
            code, body = api_kiwi_profiles_list(HTTP.Request("GET", "/api/kiwi/profiles"))
            b = JSON3.read(body)
            byname = Dict{String,Any}(String(p.name) => p for p in b.profiles)
            @test byname["alice"].retired === true
            @test byname["bob"].retired   === false
            @test byname["default"].retired === false

            # Select of retired → 409, active unchanged.
            code, _ = api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 409
            @test active_profile_name() == "default"

            # Idempotent retire → 200 + alreadyRetired flag.
            code, body = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "alice"))))
            @test code == 200
            @test JSON3.read(body).alreadyRetired === true

            # Retire the active profile → auto-snap to default so the next spawn doesn't silently
            # keep using retired credentials.
            api_kiwi_profiles_select(Vector{UInt8}(JSON3.write((; name = "bob"))))
            @test active_profile_name() == "bob"
            code, body = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "bob"))))
            @test code == 200
            b = JSON3.read(body)
            @test b.ok === true && b.snappedToDefault === true && b.active == "default"
            @test active_profile_name() == "default"

            # `default` can't be retired — it maps to ~/.claude.
            code, _ = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "default"))))
            @test code == 400

            # Nonexistent → 404. Invalid name → 400.
            code, _ = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "ghost"))))
            @test code == 404
            code, _ = api_kiwi_profiles_retire(Vector{UInt8}(JSON3.write((; name = "Bad Name"))))
            @test code == 400
        end
        init_cecelia!()   # restore
    end
end
