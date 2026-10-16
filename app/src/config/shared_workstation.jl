# ── Shared workstation — install-wide policies for a box several people use ──────────────────────
#
# `[sharedWorkstation]` in `custom.toml`. One Bool per item, each independently switchable from
# Settings → Shared workstation (shown to the `default` profile only — a visibility gate, not
# security: anyone can pick `default`). There is deliberately no "this machine is shared" master
# switch: a mode that flips several items at once hides which one changed behaviour. Add an item
# here and it appears in the route + section with no other wiring.
#
# Items:
#   oneClaudeLogin — picking a profile logs every OTHER profile out of Claude (`claude_auth.jl`),
#                    so the previous person's login never stays on disk past the next pick.

const SHARED_WORKSTATION_ITEMS = (
    (key = "oneClaudeLogin", default = false),
)

const _SHARED_WORKSTATION_TABLE = "sharedWorkstation"

function _shared_workstation_default(key::AbstractString)::Union{Bool,Nothing}
    i = findfirst(it -> it.key == key, SHARED_WORKSTATION_ITEMS)
    isnothing(i) ? nothing : SHARED_WORKSTATION_ITEMS[i].default
end

"""
    shared_workstation(key) -> Bool

One shared-workstation item, from `custom.toml [sharedWorkstation]`, falling back to its default
when unset or not a Bool (a hand-edited typo must not change behaviour). Throws on an unknown key —
a misspelt caller is a bug, not an "off".
"""
function shared_workstation(key::AbstractString)::Bool
    dflt = _shared_workstation_default(key)
    isnothing(dflt) && throw(ArgumentError("unknown shared-workstation item `$(key)`"))
    v = get(get(cecelia_conf(), _SHARED_WORKSTATION_TABLE, Dict{String,Any}()), String(key), dflt)
    v isa Bool ? v : dflt
end

"""
    shared_workstation_settings() -> Dict{String,Bool}

Every item's current value, keyed by item name — the GET route's payload.
"""
shared_workstation_settings()::Dict{String,Bool} =
    Dict{String,Bool}(it.key => shared_workstation(it.key) for it in SHARED_WORKSTATION_ITEMS)

"""
    set_shared_workstation!(key, val) -> Bool

Persist one item to `custom.toml` (merging — other keys survive) and hot-reload config. Same shape
as `set_keep_previous_version!`. Throws on an unknown key.
"""
function set_shared_workstation!(key::AbstractString, val::Bool)::Bool
    isnothing(_shared_workstation_default(key)) &&
        throw(ArgumentError("unknown shared-workstation item `$(key)`"))
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    t = get(cfg, _SHARED_WORKSTATION_TABLE, Dict{String,Any}())
    t[String(key)] = val
    cfg[_SHARED_WORKSTATION_TABLE] = t
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    val
end
