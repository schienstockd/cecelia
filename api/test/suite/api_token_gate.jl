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
