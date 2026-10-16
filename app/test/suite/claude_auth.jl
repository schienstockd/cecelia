# ── Per-profile Claude login (`ai/claude_auth.jl`) + shared-workstation items ──
# The live verbs (`claude auth status` / `logout`) need a real CLI and login, so CI pins the parser,
# the spawn helper they share, and the config store. Hermetic: `runtests.jl` points
# `CECELIA_DEV_DIR` at a throwaway dir, so the custom.toml writes never reach real settings.

using Test
using Cecelia

@testset "parse_claude_auth_status" begin
    p = Cecelia.parse_claude_auth_status
    # shapes captured from `claude auth status --json`, CLI 2.1.301
    a = p("""{"loggedIn": true, "authMethod": "claude.ai", "email": "alice@lab.org",
             "orgName": "Lab", "subscriptionType": "enterprise"}""")
    @test a.state === :loggedIn
    @test a.email == "alice@lab.org" && a.method == "claude.ai" && a.org == "Lab"
    b = p("""{"loggedIn": false, "authMethod": "none", "apiProvider": "firstParty"}""")
    @test b.state === :loggedOut && b.email == ""
    # noise around the object (an update banner) is tolerated
    @test p("Update available!\n{\"loggedIn\": true, \"email\": \"x@y\"}\n").state === :loggedIn
    # anything that does not say loggedIn as a Bool is unknown — never "logged out"
    @test p("").state === :unknown
    @test p("command not found").state === :unknown
    @test p("{not json}").state === :unknown
    @test p("""{"loggedIn": "yes"}""").state === :unknown
    @test p("""{"email": "x@y"}""").state === :unknown
end

@testset "_run_claude_cli — profile env, output, timeout" begin
    jl = Base.julia_cmd()
    # the child sees the profile's CLAUDE_CONFIG_DIR and not the scrubbed ambient credential
    withenv("ANTHROPIC_API_KEY" => "would-leak") do
        ok, out = Cecelia._run_claude_cli(
            `$jl --startup-file=no -e 'print(ENV["CLAUDE_CONFIG_DIR"], "|", get(ENV, "ANTHROPIC_API_KEY", "-"))'`;
            profile_dir = "/tmp/kiwi/alice")
        @test ok
        @test out == "/tmp/kiwi/alice|-"
    end
    # non-zero exit → not ok, output still captured
    ok, out = Cecelia._run_claude_cli(`$jl --startup-file=no -e 'print("boom"); exit(3)'`; profile_dir = "")
    @test !ok && occursin("boom", out)
    # a hung command is killed at the timeout and reads as failure (libuv reports exitcode 0)
    ok, _ = Cecelia._run_claude_cli(`$jl --startup-file=no -e 'sleep(60)'`; profile_dir = "", timeout_s = 2)
    @test !ok
    # a missing binary is a failure, not a throw
    ok, _ = Cecelia._run_claude_cli(`definitely-not-a-cecelia-binary auth status`; profile_dir = "")
    @test !ok
end

@testset "shared workstation items" begin
    prior = shared_workstation("oneClaudeLogin")
    try
        @test haskey(shared_workstation_settings(), "oneClaudeLogin")
        set_shared_workstation!("oneClaudeLogin", true)
        @test shared_workstation("oneClaudeLogin") === true
        @test shared_workstation_settings()["oneClaudeLogin"] === true
        set_shared_workstation!("oneClaudeLogin", false)
        @test shared_workstation("oneClaudeLogin") === false
        # unknown key: a misspelt caller is a bug, not a silent "off"
        @test_throws ArgumentError shared_workstation("oneClaudeLogn")
        @test_throws ArgumentError set_shared_workstation!("oneClaudeLogn", true)
        # a hand-edited non-Bool falls back to the default instead of changing behaviour
        cfg_path = custom_toml_path()
        cfg = Cecelia.TOML.parsefile(cfg_path)
        cfg["sharedWorkstation"]["oneClaudeLogin"] = "yes"
        open(io -> Cecelia.TOML.print(io, cfg), cfg_path, "w")
        init_cecelia!()
        @test shared_workstation("oneClaudeLogin") === false
    finally
        set_shared_workstation!("oneClaudeLogin", prior)
    end
end
