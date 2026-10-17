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

# ── Logging in (P9): the parts that don't need a real login ──

@testset "resolve_private_browser" begin
    r = Cecelia.resolve_private_browser
    have(names...) = x -> x in names ? "/usr/bin/$x" : nothing
    # the desktop default wins when it's private-capable and installed
    b = r("brave-browser.desktop"; which = have("firefox", "brave-browser"), mac = false)
    @test b.label == "Brave" && b.argv == ["/usr/bin/brave-browser", "--incognito"]
    # snap Firefox id
    b = r("firefox_firefox.desktop"; which = have("firefox", "google-chrome"), mac = false)
    @test b.label == "Firefox" && b.argv == ["/usr/bin/firefox", "--private-window"]
    # default unknown (or not installed) → first installed in catalogue order
    @test r("org.gnome.Epiphany.desktop"; which = have("chromium-browser", "microsoft-edge"), mac = false).argv ==
          ["/usr/bin/chromium-browser", "--incognito"]
    @test r("google-chrome.desktop"; which = have("microsoft-edge"), mac = false).argv ==
          ["/usr/bin/microsoft-edge", "--inprivate"]
    @test r(""; which = have(), mac = false) === nothing
    # macOS: first installed app bundle, through `open -na`
    b = r(""; mac = true, app_exists = ==("Google Chrome"))
    @test b.label == "Chrome" && b.argv == ["open", "-na", "Google Chrome", "--args", "--incognito"]
    @test r(""; mac = true, app_exists = _ -> false) === nothing
end

@testset "private_browser_shim" begin
    s = Cecelia.private_browser_shim
    @test s(nothing) == "#!/bin/sh\nexit 0\n"     # never falls back to a normal window
    @test occursin("exec '/usr/bin/firefox' '--private-window' \"\$1\"",
                   s(Cecelia.PrivateBrowser(["/usr/bin/firefox", "--private-window"], "Firefox")))
    @test occursin("'O'\\''Brien'", s(Cecelia.PrivateBrowser(["O'Brien"], "x")))   # quoted safely
    # run it: the browser gets the flag then the URL, verbatim
    if !Sys.iswindows()
        mktempdir() do d
            rec = joinpath(d, "rec.sh"); got = joinpath(d, "got.txt")
            write(rec, "#!/bin/sh\nprintf '%s\\n' \"\$@\" > '$got'\n"); chmod(rec, 0o700)
            shim = joinpath(d, "open-private")
            write(shim, s(Cecelia.PrivateBrowser([rec, "--private-window"], "Rec"))); chmod(shim, 0o700)
            url = "https://claude.com/cai/oauth/authorize?code=true&state=a b"
            run(`$shim $url`)
            @test readlines(got) == ["--private-window", url]
        end
    end
end

@testset "parse_login_url + _last_line" begin
    u = "https://claude.com/cai/oauth/authorize?code=true&redirect_uri=https%3A%2F%2Fplatform.claude.com%2Foauth%2Fcode%2Fcallback&state=s"
    # as printed to a pipe (CLI 2.1.302)
    out = "Opening browser to sign in…\nIf the browser didn't open, visit: $u\nPaste code here if prompted > "
    @test Cecelia.parse_login_url(out) == u
    # as printed to a terminal: OSC-8 hyperlink wrapping the same URL
    @test Cecelia.parse_login_url("visit: \e]8;;$u\a$u\e]8;;\a\n") == u
    @test Cecelia.parse_login_url("Opening browser to sign in…\n") == ""
    # the CLI's verdict shares a line with the paste prompt
    @test Cecelia._last_line(out * "Login failed: Request failed with status code 400\n") ==
          "Login failed: Request failed with status code 400"
    @test Cecelia._last_line(out) == ""        # killed while waiting: no verdict, not the URL line
    @test Cecelia._last_line("") == ""
end

@testset "claude_login_status — nothing started" begin
    st = Cecelia.claude_login_status("/nonexistent/profile-dir")
    @test st.state == "none" && st.url == "" && st.message == ""
    @test Cecelia.claude_login_code!("/nonexistent/profile-dir", "abc") ==
          (false, "No login is waiting for a code.")
    @test Cecelia.claude_login_code!("/nonexistent/profile-dir", "  ")[1] == false
    @test Cecelia.claude_login_cancel!("/nonexistent/profile-dir") == false
end
