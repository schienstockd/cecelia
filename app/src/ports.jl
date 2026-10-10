# Port slots — the ports one running Cecelia occupies, so several can share a machine.
#
# Cecelia binds five loopback ports: the API (8080), the dev frontend (5173), the preview worker
# (7656), the task runner (7657) and Pluto (7660). Two users on one machine each run their own
# Cecelia (own home → own config dir → own single-instance lock), and fixed ports made the second
# one collide with the first. A SLOT shifts all five together: port = base + PORT_SLOT_STRIDE × slot.
# Slot 0 is the historic port set, so a machine with one user sees no change.
#
# The stride is 10, not 1, because the child ports sit next to each other (7656/7657/7660): stepping
# by 1 would put slot 1's preview worker on slot 0's runner. Any stride above their spread (4) works;
# 10 keeps the numbers readable (8080 → 8090 → 8100).
#
# Which slot: `CECELIA_PORT_SLOT` if set (the launcher resolved it and children inherit it), else
# the user's STICKY slot from `<config_dir>/port-slot` while its API + frontend ports are bindable,
# else the lowest slot whose five ports are all bindable. Sticky only checks API + frontend because
# the runner is designed to outlive the backend — our own runner holding the sticky slot's runner
# port is the normal case, not a collision. The slot is persisted only once the backend has the
# single-instance lock (`server.jl`), so a refused second launch never moves it.
#
# A per-service env var (`CECELIA_PORT`, `CECELIA_FRONTEND_PORT`, `CECELIA_PREVIEW_PORT`,
# `CECELIA_RUNNER_PORT`, `CECELIA_PLUTO_PORT`) still pins that one port, as before.
#
# Base + Sockets only: included by the Cecelia package AND standalone by `api/dev.jl` and
# `api/portkill.jl` (with `config_dir.jl`), so the supervisor and `pixi run stop` resolve the same
# slot without loading Cecelia. Design: docs/ARCHITECTURE.md → *Ports*.

import Sockets

const SERVICE_PORT_BASE = (backend = 8080, frontend = 5173, preview = 7656, runner = 7657, notebooks = 7660)
const SERVICE_PORT_ENV  = (backend = "CECELIA_PORT", frontend = "CECELIA_FRONTEND_PORT",
                           preview = "CECELIA_PREVIEW_PORT", runner = "CECELIA_RUNNER_PORT",
                           notebooks = "CECELIA_PLUTO_PORT")
const PORT_SLOT_STRIDE = 10
const PORT_SLOT_COUNT  = 10          # slots 0–9: ten concurrent users on one machine
const PORT_SLOT_ENV    = "CECELIA_PORT_SLOT"
const _PORT_SLOT_FILE  = "port-slot"

port_slot_path(cfg::AbstractString = config_dir())::String = joinpath(cfg, _PORT_SLOT_FILE)

"""
    port_slot() -> Int

This process's slot — `CECELIA_PORT_SLOT`, or 0 when unset (a process nobody resolved a slot for,
e.g. a test, gets the historic ports).
"""
port_slot()::Int = something(tryparse(Int, get(ENV, PORT_SLOT_ENV, "0")), 0)

"""
    service_port(service; slot = port_slot()) -> Int

The port `service` (`:backend`, `:frontend`, `:preview`, `:runner`, `:notebooks`) listens on: its
env override when set, else `base + PORT_SLOT_STRIDE × slot`. The ONE place a Cecelia port is
computed — never hardcode 8080/7656/….
"""
function service_port(service::Symbol; slot::Integer = port_slot())::Int
    pinned = tryparse(Int, get(ENV, SERVICE_PORT_ENV[service], ""))
    pinned === nothing ? SERVICE_PORT_BASE[service] + PORT_SLOT_STRIDE * Int(slot) : pinned
end

"""
    port_bindable(port; host = "127.0.0.1") -> Bool

Can we bind `port` right now? Bind, not connect: a bind also fails on another user's listener,
which is exactly the collision this exists to avoid, and needs no permission to see their process.
"""
function port_bindable(port::Integer; host::AbstractString = "127.0.0.1")::Bool
    try
        close(Sockets.listen(parse(Sockets.IPAddr, host), port))
        true
    catch
        false
    end
end

# Pure choice (unit-tested with a fake `bindable`). `nothing` when every slot is taken.
function _choose_port_slot(sticky::Union{Integer,Nothing}, bindable)::Union{Int,Nothing}
    if sticky !== nothing && 0 <= sticky < PORT_SLOT_COUNT &&
       all(s -> bindable(service_port(s; slot = sticky)), (:backend, :frontend))
        return Int(sticky)
    end
    for slot in 0:PORT_SLOT_COUNT-1
        all(s -> bindable(service_port(s; slot)), keys(SERVICE_PORT_BASE)) && return slot
    end
    nothing
end

_read_port_slot(cfg::AbstractString)::Union{Int,Nothing} =
    isfile(port_slot_path(cfg)) ? tryparse(Int, strip(read(port_slot_path(cfg), String))) : nothing

"""
    current_port_slot(cfg = config_dir()) -> Int

The slot of this user's running (or most recent) Cecelia, for a tool that TALKS to it rather than
launching it — `pixi run stop*`, `pixi run console`: `CECELIA_PORT_SLOT`, else the sticky slot, else 0.
"""
current_port_slot(cfg::AbstractString = config_dir())::Int =
    haskey(ENV, PORT_SLOT_ENV) ? port_slot() : something(_read_port_slot(cfg), 0)

"""
    resolve_port_slot!(cfg = config_dir(); bindable = port_bindable) -> Int

Pick this launch's slot (see the file header) and export it as `CECELIA_PORT_SLOT`, so every child
inherits it. A slot already in the env wins untouched. Does NOT persist — see `persist_port_slot!`.
Throws when all `PORT_SLOT_COUNT` slots are occupied.
"""
function resolve_port_slot!(cfg::AbstractString = config_dir(); bindable = port_bindable)::Int
    haskey(ENV, PORT_SLOT_ENV) && return port_slot()
    slot = _choose_port_slot(_read_port_slot(cfg), bindable)
    slot === nothing && error("No free Cecelia port slot: all $PORT_SLOT_COUNT are in use on this machine.")
    ENV[PORT_SLOT_ENV] = string(slot)
    slot
end

"""
    persist_port_slot!(slot, cfg = config_dir())

Make `slot` this user's sticky slot, so the next launch reuses the same ports (a stable URL, and the
observer MCP registration stays valid). Called by the backend once it holds the single-instance lock.
Never throws — a lost sticky slot only costs a different port next time.
"""
function persist_port_slot!(slot::Integer, cfg::AbstractString = config_dir())::Nothing
    try
        _read_port_slot(cfg) == slot && return nothing
        mkpath(cfg)
        # temp + rename: `write_atomic` needs the package, and this file must stay Base-only
        tmp = port_slot_path(cfg) * ".tmp"
        write(tmp, string(slot))
        mv(tmp, port_slot_path(cfg); force = true)
    catch e
        @warn "Could not save the port slot" slot exception = e
    end
    nothing
end
