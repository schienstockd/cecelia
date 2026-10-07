# ── API thread pool preference — Settings → System → "Use all CPU cores" ──────────
#
# `[server] multithreaded` in the user's `custom.toml`. Default ON: the API runs request handlers,
# HDF5/zarr reads and task scheduling on the thread pool (docs/API.md → Thread safety), and a
# single-threaded server serialises all of it.
#
# A Julia process cannot change its thread count after start, so this is a LAUNCH preference: the
# installed launcher (`app.py` → `_thread_args`) reads the same key and passes `-t auto` or `-t 1`,
# then tells the server what it applied via `CECELIA_LAUNCH_THREADS` (`auto` / `1` / `env`). The dev
# and `prod` pixi tasks always pass `-t auto` and set no such var — the UI shows the toggle locked
# there. Same getter/setter shape as `tls_desired` / `set_tls_desired!` (tls.jl).

"""
    api_multithreaded() -> Bool

Whether the launcher should start the API on every CPU thread (`-t auto`) rather than one. Reads
`[server] multithreaded` from `custom.toml`; default `true`. The Python twin is
`app.py::_multithreaded_setting` — keep the key and default in step (both pinned by tests).
"""
function api_multithreaded()::Bool
    v = get(get(cecelia_conf(), "server", Dict{String,Any}()), "multithreaded", true)
    v isa Bool ? v : true
end

"""
    set_api_multithreaded!(on) -> Bool

Persist `[server].multithreaded` (merged write — unrelated keys in `custom.toml` survive) and
hot-reload config. Returns the value now stored. Takes effect on the next launch / in-app Restart.
"""
function set_api_multithreaded!(on::Bool)::Bool
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    s   = get(cfg, "server", Dict{String,Any}())
    s["multithreaded"] = on
    cfg["server"] = s
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    api_multithreaded()
end
