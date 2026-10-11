# ── TLS opt-in / opt-out — user preference for HTTPS + HTTP/2 ─────────────────────
#
# The server (api/src/server.jl) starts on HTTP/2 over TLS when `tls_desired` returns true.
# Mirrors `runner_enabled` in `throttle.jl` — env-var-overridden bool with a default. The default is
# OFF everywhere: measured in Chromium, HTTP/2 loads the viewer's bricks slower than plain HTTP/1.1
# (`docs/todo/WEBGPU_UPLOAD_PATH_PLAN.md` → U4, outcome). Kept as an opt-in so it can come back if
# the HTTP/2 path gets faster. `api/dev.jl` pins `CECELIA_TLS` so the toggle can't split Vite's
# proxy from the backend.
#
# The actual protocol the server starts with may differ (openssl missing → fallback to
# HTTP/1.1 with a warning); `api_diagnostics` reports the effective protocol separately.

"""
    tls_desired() -> Bool

Whether the server SHOULD start with HTTPS + HTTP/2. Resolution order (first wins):

  1. `CECELIA_TLS` env var — `1` → true, `0` → false. Explicit opt-in/out for dev + CI.
  2. `[tls] enabled` in the user's `custom.toml` (the Settings toggle).
  3. Default — `false`.

The name mirrors `runner_enabled` in `throttle.jl` down to the env-var-overrides-toml-overrides-default
shape; if the setting is ever renamed to something more descriptive, follow that helper.
"""
function tls_desired()::Bool
    haskey(ENV, "CECELIA_TLS") && return _env_flag("CECELIA_TLS")
    cfg = get(cecelia_conf(), "tls", Dict{String,Any}())
    if haskey(cfg, "enabled")
        return Bool(cfg["enabled"])
    end
    false
end

"""
    set_tls_desired!(on) -> Bool

Persist `[tls].enabled` and hot-reload config. Same shape as `set_runner_enabled!` —
merged write so an unrelated key in `custom.toml` survives; `init_cecelia!` refreshes the
in-memory cache so the getter reflects the write before the next request.

Returns the value now in EFFECT (`tls_desired()`), which is not always what was
asked: `CECELIA_TLS` overrides the file, so a dev session started with the env var reports
its env value however the toggle is set. Returning the effective value lets the UI show
the truth instead of the request. Requires a server restart to take effect on the wire —
this only persists the preference.
"""
function set_tls_desired!(on::Bool)::Bool
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    t   = get(cfg, "tls", Dict{String,Any}())
    t["enabled"] = on
    cfg["tls"] = t
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    tls_desired()
end
