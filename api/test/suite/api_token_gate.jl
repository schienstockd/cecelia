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

# Security audit F3: the cookie is SameSite=Lax and every localhost port is one site, so a page from
# any other local web server carried it. The stream handler now refuses a browser request that did
# not come from this backend's own page — fetch, binary route and WebSocket alike.
@testset "API token gate — another localhost page is refused, cookie or not" begin
    old = _API_TOKEN[]
    _API_TOKEN[] = "t0ken"
    srv = HTTP.listen!(handle_stream, "127.0.0.1", 0)
    try
        host = "127.0.0.1:$(HTTP.port(srv))"
        base = "http://$host"
        cookie = "Cookie" => "$(Cecelia.api_cookie_name(PORT))=t0ken"
        req(m, path, h) = HTTP.request(m, base * path, h, m == "POST" ? "{}" : "";
                                       status_exception = false, redirect = false, retry = false, cookies = false)
        other = [cookie, "Origin" => "http://localhost:3000", "Sec-Fetch-Site" => "same-site", "Sec-Fetch-Mode" => "cors"]
        own   = [cookie, "Origin" => base, "Sec-Fetch-Site" => "same-origin", "Sec-Fetch-Mode" => "cors"]

        @test req("GET", "/api/diagnostics", own).status == 200
        r = req("GET", "/api/diagnostics", other)
        @test r.status == 403 && occursin("another site", String(r.body))
        @test req("POST", "/api/plugins/install", other).status == 403          # the XSS → code sink
        @test req("POST", "/api/repl/config", other).status == 403 && req("POST", "/api/repl", other).status == 403
        @test req("GET", "/api/viewer/slab?projectUid=x", other).status == 403  # a binary route
        # an old browser (no Sec-Fetch-*) is judged by Origin alone
        @test req("GET", "/api/diagnostics", [cookie, "Origin" => "http://localhost:3000"]).status == 403
        @test req("GET", "/api/diagnostics", [cookie, "Origin" => base]).status == 200
        @test req("GET", "/api/health", other).status == 200                    # the launcher probe stays

        ws_try(headers) = try
            HTTP.WebSockets.open("ws://$host/ws"; headers, cookies = false) do ws end
            nothing
        catch e
            e
        end
        refused = ws_try([cookie, "Origin" => "http://localhost:3000", "Sec-Fetch-Site" => "same-site",
                          "Sec-Fetch-Mode" => "websocket"])
        @test refused !== nothing && occursin("403", sprint(showerror, refused))
        @test ws_try([cookie, "Origin" => base, "Sec-Fetch-Site" => "same-origin", "Sec-Fetch-Mode" => "websocket"]) === nothing
        @test ws_try(["Authorization" => "Bearer t0ken"]) === nothing            # programmatic client
    finally
        close(srv)
        _API_TOKEN[] = old
    end
end

# Security audit F7: ~40 handlers join a request's project uid onto projects_dir(), most behind only an
# `isdir` — which an absolute or `../` path to an existing directory passes. Refused at the door.
@testset "API: a request's project uid must be a uid — checked at the door" begin
    old = _API_TOKEN[]
    _API_TOKEN[] = "t0ken"
    srv = HTTP.listen!(handle_stream, "127.0.0.1", 0)
    victim = mktempdir()
    write(joinpath(victim, "keep.txt"), "x")
    try
        base = "http://127.0.0.1:$(HTTP.port(srv))"
        h = ["Authorization" => "Bearer t0ken", "Content-Type" => "application/json"]
        post(path, body) = HTTP.post(base * path, h, JSON3.write(body); status_exception = false, retry = false)
        get(path) = HTTP.get(base * path, h; status_exception = false, retry = false)

        r = post("/api/projects/delete", Dict("uid" => victim))
        @test r.status == 400 && occursin("Invalid project id", String(r.body))
        @test isfile(joinpath(victim, "keep.txt"))                                  # nothing discarded
        @test post("/api/projects/delete", Dict("uid" => "../" * basename(victim))).status == 400
        @test post("/api/projects/delete", Dict("uid" => "noSuch1")).status == 404   # a uid still reaches the handler
        @test post("/api/push/target", Dict("projectUid" => victim, "socketPath" => "s", "token" => "t")).status == 400
        @test get("/api/viewer/slab?projectUid=" * HTTP.escapeuri(victim)).status == 400  # binary route
        @test get("/api/movies/file?projectUid=..%2F..&name=x.mp4").status == 400
    finally
        close(srv)
        _API_TOKEN[] = old
        rm(victim; recursive = true, force = true)
    end
end

# Same containment shapes elsewhere in the API (security audit F14/F15, found by recital's fanout).
@testset "API: confinement is by separator; .zattrs paths stay in the store" begin
    r = mktempdir()
    @test _fs_confined(joinpath(r, "a"), r) && _fs_confined(r, r) && _fs_confined(r * "/", r)
    @test !_fs_confined(r * "_other", r) && !_fs_confined(r * "2/x", r)
    @test _fs_confined("/anything", "")                                         # no sandbox configured
    store = mkpath(joinpath(r, "s.ome.zarr"))
    write(joinpath(store, ".zattrs"), """{"multiscales":[{"datasets":[{"path":"../evil"},{"path":"0"}]}]}""")
    mkpath(joinpath(r, "evil")); write(joinpath(r, "evil", ".zarray"), """{"shape":[7,4,4],"chunks":[1,4,4]}""")
    mkpath(joinpath(store, "0")); write(joinpath(store, "0", ".zarray"), """{"shape":[1,2,2],"chunks":[1,2,2]}""")
    lv = store_pyramid_levels(store)
    @test lv !== nothing && [l.path for l in lv] == ["0"]
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
