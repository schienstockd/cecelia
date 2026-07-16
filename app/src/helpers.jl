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
function versioned_set!(d::Dict{String,Any}, item_value, value_name::String = VERSIONED_DEFAULT_VAL;
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

# Read a ccid.json / project.json into a String-keyed Dict{String,Any} ready for the versioned_*
# helpers. JSON3 yields Symbol keys that make `get(d, "field", …)` silently miss (see the JSON3
# gotcha in CLAUDE.md); this is the one place that normalizes them. Use it instead of hand-rolling
# `Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(path, String)))`.
function read_ccid_raw(path::AbstractString)::Dict{String,Any}
    Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(path, String)))
end
