# Versioned-variable helpers — Julia equivalent of cciaHelpers.R
#
# R stores the active pointer as an R attribute: attr(list, "default").
# That doesn't survive JSON serialisation, so we use a plain key "_active"
# inside the dict instead.  The semantics are identical.
#
# On-disk shape:
#   { "default": "ccidImage.ome.zarr", "_active": "default" }
#
# Equivalent R pattern:
#   .setVersionedVar / .getVersionedVar
#   .setVersionedVarInList / .getVersionedVarInList

const VERSIONED_ACTIVE_KEY = "_active"
const VERSIONED_DEFAULT_VAL = "default"

# ── Low-level: operate on a versioned dict directly ──────────────────────────

# Equivalent: attr(valueList, "default")  →  valueName of the active entry
function versioned_active(d::AbstractDict)::String
    string(get(d, VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL))
end

# Equivalent: .getVersionedVar(valueList, valueName = NULL)
# Returns the value stored under value_name (or the active entry when nothing).
function versioned_get(d::AbstractDict, value_name = nothing)
    name = isnothing(value_name) ? versioned_active(d) : string(value_name)
    # Accept both String and Symbol keys (JSON3 may produce either)
    val = get(d, name, get(d, Symbol(name), nothing))
    val
end

# Equivalent: .setVersionedVar(valueList, itemValue, valueName, setDefault)
# Mutates d in-place.  Pass nothing as item_value to remove the entry and
# reset _active to "default" (mirrors R's NULL behaviour).
#
# Accepts any String-keyed AbstractDict — was `Dict{String,Any}`-only, but the tightened
# `CciaImage.im_channel_names::Dict{String,Union{Vector{String},String}}` field is not a subtype
# of that (Dict is invariant in its value type). Widened so both concrete field types can share
# this helper; Julia checks that the actual `item_value` fits the dict's value type at assignment.
function versioned_set!(d::AbstractDict{String}, item_value, value_name::String = VERSIONED_DEFAULT_VAL;
                        set_active::Bool = true)
    if isnothing(item_value)
        delete!(d, value_name)
        d[VERSIONED_ACTIVE_KEY] = VERSIONED_DEFAULT_VAL
    else
        d[value_name] = item_value
        if set_active
            d[VERSIONED_ACTIVE_KEY] = value_name
        end
    end
    d
end

# ── High-level: operate on a field inside a larger dict ──────────────────────

# Equivalent: .getVersionedVarInList(attrList, itemName, valueName = NULL)
function versioned_get_field(d::AbstractDict, field::String, value_name = nothing)
    # Accept both String and Symbol keys coming from JSON3
    inner = get(d, field, get(d, Symbol(field), nothing))
    isnothing(inner) && return nothing
    inner isa AbstractDict || return inner   # scalar: return as-is (legacy compat)
    versioned_get(inner, value_name)
end

# Equivalent: .setVersionedVarInList(attrList, itemName, itemValue, valueName, setDefault)
# Creates the field dict if absent; migrates a bare scalar to {default: scalar}.
function versioned_set_field!(d::Dict{String,Any}, field::String, item_value,
                              value_name::String = VERSIONED_DEFAULT_VAL;
                              set_active::Bool = true)
    existing = get(d, field, nothing)
    if isnothing(existing)
        d[field] = Dict{String,Any}()
    elseif existing isa Dict{String,Any}
        nothing  # already the right type — use in-place below
    elseif existing isa AbstractDict
        # JSON3.Object or other non-concrete dict — normalize to Dict{String,Any}
        d[field] = Dict{String,Any}(String(k) => v for (k, v) in existing)
    else
        # Bare scalar — migrate to versioned dict
        d[field] = Dict{String,Any}(VERSIONED_DEFAULT_VAL => existing,
                                    VERSIONED_ACTIVE_KEY   => VERSIONED_DEFAULT_VAL)
    end
    versioned_set!(d[field]::Dict{String,Any}, item_value, value_name; set_active = set_active)
    d
end

# ── Convenience: list all user-facing value names (excludes _active) ─────────
function versioned_keys(d::AbstractDict)::Vector{String}
    [string(k) for k in keys(d) if string(k) != VERSIONED_ACTIVE_KEY]
end

# ─────────────────────────────────────────────────────────────────────────────
# Inner-level versioning — multiple VERSIONS per value_name
# ─────────────────────────────────────────────────────────────────────────────
#
# The `versioned_*` helpers above answer "which value_name variant is active?"
# (the OUTER axis: default, dtype, cropped, driftCorrected, …). The `version_*`
# helpers below answer "for a given value_name, which VERSION of its output is
# on disk?" (the INNER axis: v1, v2, v3, …).
#
# On-disk shape — nested inside a versioned_* dict entry:
#   { "filepath": { "default": { "v1": "image.zarr", "v2": "image.zarr", "_latest": "v2" },
#                   "_active": "default" } }
#
# Legacy shape — a bare scalar at the value_name key — is treated as implicit
# v1, so old projects load unchanged and every existing reader keeps working.
#
# Full design: docs/todo/VN_VERSIONING_PLAN.md (Decisions D1, D2, D4).

const LATEST_ACTIVE_KEY = "_latest"
const LATEST_DEFAULT_VAL = "v1"

# True when `x` is an inner versioned entry (has a `_latest` pointer). Handles
# both String and Symbol keys — the same JSON3 gotcha the outer helpers guard.
is_versioned_entry(x)::Bool =
    x isa AbstractDict &&
    (haskey(x, LATEST_ACTIVE_KEY) || haskey(x, Symbol(LATEST_ACTIVE_KEY)))

# Equivalent of `versioned_active`, for the inner axis.
function version_latest(d::AbstractDict)::String
    string(get(d, LATEST_ACTIVE_KEY, get(d, Symbol(LATEST_ACTIVE_KEY), LATEST_DEFAULT_VAL)))
end

# Equivalent of `versioned_get`, for the inner axis. Returns the value stored
# under `version` (or under the latest entry when `version === nothing`).
function version_get(d::AbstractDict, version = nothing)
    ver = isnothing(version) ? version_latest(d) : string(version)
    get(d, ver, get(d, Symbol(ver), nothing))
end

# Equivalent of `versioned_set!`, for the inner axis. Pass `nothing` as
# `item_value` to remove the version entry and reset `_latest` to `v1`
# (mirrors R's NULL behaviour, matching versioned_set!).
function version_set!(d::AbstractDict{String}, item_value, version::String = LATEST_DEFAULT_VAL;
                     set_latest::Bool = true)
    if isnothing(item_value)
        delete!(d, version)
        d[LATEST_ACTIVE_KEY] = LATEST_DEFAULT_VAL
    else
        d[version] = item_value
        if set_latest
            d[LATEST_ACTIVE_KEY] = version
        end
    end
    d
end

# All user-facing version names (excludes `_latest`).
version_keys(d::AbstractDict)::Vector{String} =
    [string(k) for k in keys(d) if string(k) != LATEST_ACTIVE_KEY]

# ── Composer: read a field, resolving BOTH the value_name axis and the ──────
# version axis. Returns:
#   - the leaf value when the entry is a versioned entry (new shape),
#   - the entry unchanged when it is a bare scalar / vector (legacy shape),
#   - `nothing` if the field is absent.
#
# Callers that need to preserve the old behaviour (return the raw inner value
# for a value_name, regardless of shape) keep using `versioned_get_field`.
# Callers that need the leaf value at a specific version use this.
function versioned_get_field_at(d::AbstractDict, field::String, value_name = nothing;
                                version = nothing)
    inner = versioned_get_field(d, field, value_name)
    isnothing(inner) && return nothing
    is_versioned_entry(inner) || return inner   # legacy: bare scalar / vector
    version_get(inner, version)
end

# ── Given ONE inner value (already resolved on the value_name axis), unwrap ──
# it on the version axis. This is the entry point for helpers that hold a
# struct field like `img.filepath` and index it by value_name themselves —
# they get a value back, then call `unversion_value` to reach the leaf. It's
# the shorter half of `versioned_get_field_at` for callers that don't want to
# pass the whole outer dict + field name.
#
# Legacy (bare scalar / vector) — returns unchanged.
# New shape (versioned entry) — returns `version_get(value, version)`.
function unversion_value(value, version = nothing)
    isnothing(value) && return nothing
    is_versioned_entry(value) || return value
    version_get(value, version)
end

# Read a ccid.json / project.json into a String-keyed Dict{String,Any} ready for the versioned_*
# helpers. JSON3 yields Symbol keys that make `get(d, "field", …)` silently miss (see the JSON3
# gotcha in CLAUDE.md); this is the one place that normalizes them. Use it instead of hand-rolling
# `Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(path, String)))`.
# The write counterpart is `write_json_atomic` (app/src/utils.jl) — read here, mutate via the
# `versioned_*` helpers, write back there. Never `open(path, "w")` a ccid.json yourself.
read_ccid_raw(path::AbstractString)::Dict{String,Any} =
    Dict{String,Any}(String(k) => v for (k, v) in read_state_json(path))

"""
    read_state_json(path; as = nothing) -> parsed

Read + parse a state file, **naming the file** if it doesn't parse. JSON3's own message is just
`invalid JSON at byte position 156` — no path, raised from deep inside a project load, so it tells a
user nothing they can act on. `_load_set` has no per-image guard, so one unreadable image ccid.json
fails the whole project load; when that happens the message has to say which file and what to do.

Pass `as` for a typed parse (`JSON3.read(s, T)`); the default is JSON3's untyped read.
"""
function read_state_json(path::AbstractString; as = nothing)
    contents = read(path, String)
    try
        isnothing(as) ? JSON3.read(contents) : JSON3.read(contents, as)
    catch e
        e isa ArgumentError || rethrow()
        # A truncated file is the signature of a write interrupted by a kill/crash. State writes go
        # through `write_atomic` now, so this should only be reachable for a file written by an
        # older build, a hand edit, or genuine disk corruption.
        error("Unreadable state file: $path\n" *
              "  It is not valid JSON ($(sprint(showerror, e))).\n" *
              "  Most likely a write was interrupted by an older version, or the file was edited " *
              "by hand. Restore this one file from a .ccbundle export or a backup — the rest of " *
              "the project is intact.")
    end
end

"""
    json_native(x)

Recursively convert JSON3 values into native String-keyed `Dict`s / `Vector`s.

THE one converter — parsed JSON reaches us in two shapes that both bite: `JSON3.Object` keys are
**Symbols** (so `get(o, "key", nothing)` silently misses), and `JSON3.Object isa Dict` is **false**
while `isa AbstractDict` is true (so a `isa Dict` guard silently skips it). Anything that reads a
request body or a re-read sidecar and then indexes it by string should pass it through here first,
rather than growing another private `_native`. Non-JSON3 values pass through untouched, so it is safe
to call on already-native input.
"""
json_native(x) = x
json_native(x::JSON3.Object) = Dict{String,Any}(String(k) => json_native(v) for (k, v) in x)
json_native(x::JSON3.Array)  = Any[json_native(v) for v in x]
