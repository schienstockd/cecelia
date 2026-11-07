# The API token gate (app/src/api_token.jl, `handle_stream` in api/src/server.jl): only the account that
# launched Cecelia — holding `<config_dir>/api-token` — gets anything but the health probe. Exercised
# through a REAL listener on a free port, so the router, the static path and the WS upgrade are all
# behind the same check the shipped server uses.
@testset "API token gate — only the launching user gets in" begin
    @test _auth_exempt("GET", "/api/health") && _auth_exempt("GET", "/api/auth") && _auth_exempt("OPTIONS", "/api/x")
    @test !_auth_exempt("GET", "/api/diagnostics") && !_auth_exempt("GET", "/") && !_auth_exempt("GET", "/ws")
    # the sign-in redirect stays on this server
    @test _safe_next("/analysis?x=1") == "/analysis?x=1"
    @test _safe_next("//evil.example") == "/" && _safe_next("https://evil.example") == "/"
    @test _safe_next("/\\evil.example") == "/"

    old = _API_TOKEN[]
    _API_TOKEN[] = "t0ken"
    srv = HTTP.listen!(handle_stream, "127.0.0.1", 0)
    try
        base = "http://127.0.0.1:$(HTTP.port(srv))"
        get(path, headers = Pair{String,String}[]) =
            HTTP.get(base * path, headers; status_exception = false, redirect = false, retry = false,
                     cookies = false)   # HTTP.jl's global jar would replay the sign-in cookie
        bearer = ["Authorization" => "Bearer t0ken"]

        @test get("/api/health").status == 200                                   # launchers poll this
        r = get("/api/diagnostics")
        @test r.status == 401 && occursin("Not signed in", String(r.body))
        @test get("/api/diagnostics", ["Authorization" => "Bearer wrong"]).status == 401
        @test get("/api/diagnostics", bearer).status == 200
        page = get("/")                                                          # a browser navigation
        @test page.status == 401 && occursin("text/html", HTTP.header(page, "Content-Type"))

        # the launch link: right token → cookie + redirect; the cookie then works on its own
        @test get("/api/auth?token=nope").status == 401
        a = get("/api/auth?token=t0ken&next=/analysis")
        @test a.status == 302 && HTTP.header(a, "Location") == "/analysis"
        cookie = HTTP.header(a, "Set-Cookie")
        @test occursin("HttpOnly", cookie) && occursin("SameSite=Lax", cookie)
        @test startswith(cookie, "$(Cecelia.api_cookie_name(PORT))=t0ken;")
        @test get("/api/diagnostics", ["Cookie" => first(split(cookie, ';'))]).status == 200

        # the WebSocket upgrade is behind the same check
        # (asserted on the 401 itself, so a client-side error cannot pass this vacuously)
        refused = try
            HTTP.WebSockets.open(replace(base, "http" => "ws") * "/ws"; cookies = false) do ws end
            nothing
        catch e
            e
        end
        @test refused !== nothing && occursin("401", sprint(showerror, refused))
        opened = Ref(false)
        HTTP.WebSockets.open(replace(base, "http" => "ws") * "/ws"; headers = bearer, cookies = false) do ws
            opened[] = true
        end
        @test opened[]
    finally
        close(srv)
        _API_TOKEN[] = old
    end
end

# The AI switch (AI_TOGGLE_PLAN D1, app/src/ai/ai_switch.jl): with AI off every request marked
# `X-Cecelia-Client: claude` is refused in the stream handler — JSON routes, the binary routes that skip
# `handle_http`, and the WebSocket upgrade — while the app's own requests go through.
@testset "AI off — requests from Claude are refused at the stream handler" begin
    old = _API_TOKEN[]
    _API_TOKEN[] = "t0ken"
    prior = Cecelia.shared_workstation("aiAllowed")
    srv = HTTP.listen!(handle_stream, "127.0.0.1", 0)
    try
        base = "http://127.0.0.1:$(HTTP.port(srv))"
        get(path, headers) = HTTP.get(base * path, headers; status_exception = false, redirect = false,
                                      retry = false, cookies = false)
        app    = ["Authorization" => "Bearer t0ken"]
        claude = [app; "X-Cecelia-Client" => "claude"]
        ws_try(headers) = try
            HTTP.WebSockets.open(replace(base, "http" => "ws") * "/ws"; headers, cookies = false) do ws end
            nothing
        catch e
            e
        end

        Cecelia.set_shared_workstation!("aiAllowed", true)
        @test get("/api/diagnostics", claude).status == 200
        @test ws_try(claude) === nothing

        Cecelia.set_shared_workstation!("aiAllowed", false)
        r = get("/api/diagnostics", claude)
        @test r.status == 403
        body = JSON3.read(r.body)
        @test body.error == Cecelia.AI_OFF_MESSAGE && body.setBy == "workstation"
        @test get("/api/viewer/slab?projectUid=x", claude).status == 403      # a binary route
        @test get("/api/health", claude).status == 200                        # the launcher probe stays
        refused = ws_try(claude)
        @test refused !== nothing && occursin("403", sprint(showerror, refused))
        @test get("/api/diagnostics", app).status == 200                      # the app itself is not Claude
        @test ws_try(app) === nothing
    finally
        close(srv)
        Cecelia.set_shared_workstation!("aiAllowed", prior)
        _API_TOKEN[] = old
    end
end

@testset "AI off — routes that hand out a model path, the outbound push, the status" begin
    prior = Cecelia.shared_workstation("aiAllowed")
    req = HTTP.Request("GET", "/api/observer/chat")
    try
        Cecelia.set_shared_workstation!("aiAllowed", false)
        for (m, p) in (("GET", "/api/observer/chat"), ("GET", "/api/kiwi/terminal/command"),
                       ("GET", "/api/kiwi/profiles/auth"), ("POST", "/api/observer/register"),
                       ("POST", "/api/kiwi/profiles/login"), ("POST", "/api/kiwi/profiles/login/code"))
            st, out = handle_http(HTTP.Request(m, p), UInt8[])
            @test st == 403 && JSON3.read(out).setBy == "workstation"
        end
        # cleanup stays open with AI off: logging out removes a credential
        @test first(handle_http(HTTP.Request("POST", "/api/kiwi/profiles/logout"), Vector{UInt8}("{}"))) != 403
        @test push_capture_notification("anyproject", "cap1", nothing) == (:ai_off, nothing)
        # creating a profile is not AI, but its reply carried the terminal one-liner — not with AI off
        st, out = api_kiwi_profiles_create(Vector{UInt8}("""{"name":"aioffnew"}"""))
        @test st == 200 && JSON3.read(out).terminalCommand == ""
        rm(Cecelia.profile_settings_dir("aioffnew"); recursive = true, force = true)
        st, out = handle_http(HTTP.Request("GET", "/api/observer/status"), UInt8[])
        @test st == 200 && JSON3.read(out).ai.enabled == false
    finally
        Cecelia.set_shared_workstation!("aiAllowed", prior)
    end
    st, out = handle_http(HTTP.Request("GET", "/api/observer/status"), UInt8[])
    @test JSON3.read(out).ai.enabled == prior
end
