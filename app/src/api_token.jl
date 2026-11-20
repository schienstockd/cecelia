# API token — only the user who launched Cecelia can talk to it.
#
# Every Cecelia port is unauthenticated loopback, and loopback is shared by every account on the
# machine: another user (a shared lab Mac, a shared Linux workstation) could otherwise drive your
# backend — read and write your projects, run tasks, or use the debug REPL — as you. And because the
# backend answers `Access-Control-Allow-Origin: *`, so could any web page open in your browser.
#
# The gate is a secret only you can read: `<config_dir>/api-token`, created on first launch with mode
# 0600 (on Windows the per-user profile ACL does the same job) and kept across restarts. Two ways in:
#   • a programmatic client (MCP, runner, preview worker, console, launchers) sends
#     `Authorization: Bearer <token>` — it runs as you, so it reads the file (or is handed it in env);
#   • the browser gets an HttpOnly, SameSite=Lax cookie once, from the launch link `/api/auth?token=…`
#     that `app.py` / `api/dev.jl` open. Lax keeps it off a cross-SITE page's fetch, but every
#     `localhost` port is the SAME site — another local web server's page would carry it. So the
#     server also refuses a browser request that did not come from its own page
#     (`request_origin_ok`; security audit F3).
# The cookie is named per backend port, because cookies ignore ports and two instances (dev + the
# installed app) on one browser must not overwrite each other.
#
# Base + Random only: included by the Cecelia package AND standalone by `api/dev.jl` and
# `api/task_console.jl` (with `config_dir.jl`). Design: docs/ARCHITECTURE.md → *API token*.

import Random

const _API_TOKEN_FILE = "api-token"
const API_TOKEN_ENV   = "CECELIA_API_TOKEN"   # how a spawned child (runner, preview worker) receives it

api_token_path(cfg::AbstractString = config_dir())::String = joinpath(cfg, _API_TOKEN_FILE)

"""
    read_api_token(cfg = config_dir()) -> Union{String,Nothing}

The token, or `nothing` when no backend has created one yet. `CECELIA_API_TOKEN` wins — that is how
the backend hands it to the children it spawns.
"""
function read_api_token(cfg::AbstractString = config_dir())::Union{String,Nothing}
    env = strip(get(ENV, API_TOKEN_ENV, ""))
    isempty(env) || return String(env)
    path = api_token_path(cfg)
    isfile(path) || return nothing
    tok = strip(read(path, String))
    isempty(tok) ? nothing : String(tok)
end

"""
    ensure_api_token!(cfg = config_dir()) -> String

The token, created (32 random bytes, hex) on first use. Written to a temp file that is made
owner-only BEFORE the secret goes in, then renamed into place, so the secret is never readable by
anyone else even for a moment.
"""
function ensure_api_token!(cfg::AbstractString = config_dir())::String
    tok = read_api_token(cfg)
    tok === nothing || return tok
    mkpath(cfg)
    tok = bytes2hex(rand(Random.RandomDevice(), UInt8, 32))
    tmp = api_token_path(cfg) * ".tmp"
    touch(tmp)
    chmod(tmp, 0o600)
    write(tmp, tok)
    mv(tmp, api_token_path(cfg); force = true)
    tok
end

api_cookie_name(port::Integer)::String = "cecelia_auth_$port"

# The `Authorization` header value a client sends.
api_auth_header(tok::AbstractString)::Pair{String,String} = "Authorization" => "Bearer $tok"

# Length-independent comparison, so the check leaks nothing through timing.
function _token_eq(a::AbstractString, b::AbstractString)::Bool
    x, y = codeunits(a), codeunits(b)
    length(x) == length(y) || return false
    acc = 0x00
    for i in eachindex(x)
        acc |= x[i] ⊻ y[i]
    end
    acc == 0x00
end

function _cookie_value(cookie_header::AbstractString, name::AbstractString)::Union{String,Nothing}
    for part in split(cookie_header, ';')
        k, sep, v = _partition_eq(strip(part))
        sep && k == name && return String(v)
    end
    nothing
end
_partition_eq(s::AbstractString) = (i = findfirst('=', s); i === nothing ? (s, false, "") : (s[1:i-1], true, s[i+1:end]))

"""
    token_authorized(tok, authorization, cookie, port) -> Bool

Pure check on the two raw header strings: a matching `Bearer` token, or this backend's cookie.
"""
function token_authorized(tok::AbstractString, authorization::AbstractString,
                          cookie::AbstractString, port::Integer)::Bool
    isempty(tok) && return false
    if startswith(authorization, "Bearer ")
        _token_eq(strip(authorization[8:end]), tok) && return true
    end
    c = _cookie_value(cookie, api_cookie_name(port))
    c !== nothing && _token_eq(c, tok)
end

"""
    request_origin_ok(method, path, sec_fetch_site, sec_fetch_mode, origin, host) -> Bool

Did this request come from a page this backend served (or from no page at all)? The token cookie is
`SameSite=Lax`, and to a browser every port on `localhost` is one site — so a page from any other
local web server (a notebook, a docs preview, a dev server for something else) would send the cookie
with its fetch, form post or WebSocket. Pure check on the raw headers:

  • `Sec-Fetch-Site` (set by the browser, a page cannot forge it): `same-origin` / `none` (typed URL,
    bookmark, launcher) pass; `same-site` / `cross-site` fail — except a plain navigation (GET/HEAD,
    mode `navigate`) to the frontend, which loads our own page and is harmless.
  • No `Sec-Fetch-Site` (a programmatic client, or an old browser): an `Origin`, if sent, must be this
    host (`origin`'s authority == the `Host` header); no `Origin` passes — a non-browser client holds
    the token itself.

Vite's dev proxy forwards `Host`, `Origin` and `Sec-Fetch-Site` unchanged (`changeOrigin: false`),
so the dev frontend is same-origin here too.
"""
function request_origin_ok(method::AbstractString, path::AbstractString,
                           sec_fetch_site::AbstractString, sec_fetch_mode::AbstractString,
                           origin::AbstractString, host::AbstractString)::Bool
    site = lowercase(strip(sec_fetch_site))
    site in ("same-origin", "none") && return true
    if site in ("same-site", "cross-site")
        api = startswith(path, "/api/") || path == "/ws"
        return !api && method in ("GET", "HEAD") && lowercase(strip(sec_fetch_mode)) == "navigate"
    end
    o = strip(origin)
    isempty(o) && return true
    m = match(r"^[A-Za-z][A-Za-z0-9+.-]*://([^/?#]+)/?$", o)
    m === nothing && return false                      # `null` (sandboxed frame, file://) or junk
    lowercase(m.captures[1]) == lowercase(strip(host))
end

"""
    api_launch_url(base, tok; next = "/") -> String

The link that signs a browser in: `/api/auth` sets the cookie and redirects to `next`. It goes
through `/api/` so Vite's dev proxy forwards it to the backend.
"""
api_launch_url(base::AbstractString, tok::AbstractString; next::AbstractString = "/")::String =
    string(rstrip(base, '/'), "/api/auth?token=", tok, next == "/" ? "" : "&next=" * next)
