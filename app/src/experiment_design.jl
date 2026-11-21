# Experiment design — the one design a set follows: the question it answers, whether it is designed or
# exploratory, and which image attributes play the roles of group, unit and parents. Plan:
# docs/todo/EXPERIMENT_DESIGN_PLAN.md (Decisions 1–10, "The design record").
#
# Stored on the set as `meta["experiment_design"]` (one design per set), written through `commit_state!`
# like `recipe`. Every change also appends the new state to `experiment_design.history.jsonl` in the
# set's metadata dir (`append_jsonl!` / `read_jsonl`, as the gate history does), with a content hash
# over what the design means. A stats run or an export can name the hash it read, and be marked stale
# when the design moves on. Absent history (a set designed before this existed) = unknown, never an error.
#
# Roles are image attribute NAMES (`img.attr`). The grid — group × parent, with the units in each cell
# — is computed from the attributes on every read and never stored, so editing an attribute in the
# Metadata panel moves an image without touching the design. A design may name an attribute no image
# has yet (set up on an empty set): that is a warning in the grid, never a refusal.
#
# "Design" alone already means a LABEL design in acq_check.jl (movies sharing a channel list), so this
# is always the EXPERIMENT design in code.

@enum DesignMode DESIGN_DESIGNED DESIGN_EXPLORATORY
@enum DesignStatus DESIGN_INFERRED DESIGN_CONFIRMED
@enum ExpectDirection EXPECT_NONE EXPECT_HIGHER EXPECT_LOWER

# The lowercase name is the on-disk / on-wire form via `Base.string`; `parse_*` reads it back (the
# `ImageStatus` pattern). Parsing forgives case and spaces and names the allowed values on a miss,
# because the value comes from a request body, not only from disk.
const _DESIGN_MODE_STR   = Dict(DESIGN_DESIGNED => "designed", DESIGN_EXPLORATORY => "exploratory")
const _DESIGN_STATUS_STR = Dict(DESIGN_INFERRED => "inferred", DESIGN_CONFIRMED => "confirmed")
const _EXPECT_STR        = Dict(EXPECT_NONE => "none", EXPECT_HIGHER => "higher", EXPECT_LOWER => "lower")
const _DESIGN_MODE_PARSE   = Dict(v => k for (k, v) in _DESIGN_MODE_STR)
const _DESIGN_STATUS_PARSE = Dict(v => k for (k, v) in _DESIGN_STATUS_STR)
const _EXPECT_PARSE        = Dict(v => k for (k, v) in _EXPECT_STR)
Base.string(x::DesignMode)      = _DESIGN_MODE_STR[x]
Base.string(x::DesignStatus)    = _DESIGN_STATUS_STR[x]
Base.string(x::ExpectDirection) = _EXPECT_STR[x]

function _parse_design_enum(parse::Dict{String,T}, s, what::String)::T where {T}
    v = get(parse, lowercase(strip(string(s))), nothing)
    v === nothing && error("Not a $(what): \"$(s)\" (one of $(join(sort!(collect(keys(parse))), ", ")))")
    v
end
parse_design_mode(s)      = _parse_design_enum(_DESIGN_MODE_PARSE, s, "design mode")
parse_design_status(s)    = _parse_design_enum(_DESIGN_STATUS_PARSE, s, "design status")
parse_expect_direction(s) = _parse_design_enum(_EXPECT_PARSE, s, "direction")

const EXPERIMENT_DESIGN_KEY = "experiment_design"
const EXPERIMENT_DESIGN_HISTORY = "experiment_design.history.jsonl"

"""
    ExperimentDesign

One set's design. `group`, `unit` and `parents` are image attribute names; `unit = []` means the image
is the unit, and `parents` lists the attributes outermost first (e.g. `["Date", "Mouse"]` when mouse
names restart each day). `planned_parents` is the planned number of parents per group, and
`planned_units` the planned units per parent (per group when there are no parents); `nothing` = not
planned.
"""
Base.@kwdef struct ExperimentDesign
    question::String = ""
    expect::ExpectDirection = EXPECT_NONE
    mode::DesignMode = DESIGN_DESIGNED
    status::DesignStatus = DESIGN_INFERRED
    group::Vector{String} = String[]
    unit::Vector{String} = String[]
    parents::Vector{String} = String[]
    planned_parents::Union{Int,Nothing} = nothing
    planned_units::Union{Int,Nothing} = nothing
end

"""The design as its stored / API dict (camelCase keys, enums by name)."""
experiment_design_dict(d::ExperimentDesign)::Dict{String,Any} = Dict{String,Any}(
    "question" => d.question, "expect" => string(d.expect),
    "mode" => string(d.mode), "status" => string(d.status),
    "group" => d.group, "unit" => d.unit, "parents" => d.parents,
    "plannedParents" => d.planned_parents, "plannedUnits" => d.planned_units)

# Equal when they say the same thing (the default `==` on a struct holding Vectors is identity).
Base.:(==)(a::ExperimentDesign, b::ExperimentDesign) = experiment_design_dict(a) == experiment_design_dict(b)
Base.hash(d::ExperimentDesign, h::UInt) = hash(experiment_design_dict(d), h)

_design_attrs(x)::Vector{String} =
    x === nothing ? String[] : x isa AbstractString ? (isempty(strip(x)) ? String[] : [String(strip(x))]) :
    String[String(strip(string(a))) for a in x]

function _design_planned(x, what::String)::Union{Int,Nothing}
    x === nothing && return nothing
    n = x isa Integer ? Int(x) : x isa Real && isinteger(x) ? Int(x) :
        x isa AbstractString && isempty(strip(x)) ? (return nothing) :
        something(tryparse(Int, strip(string(x))), -1)
    n >= 1 || error("$(what) must be a whole number of at least 1, not \"$(x)\"")
    n
end

"""
    experiment_design_from(raw) -> ExperimentDesign

Read a design from its dict (stored or sent). Errors on a SHAPE problem only: an unknown mode, status or
direction, a blank attribute name, an attribute in two roles (or twice in one), a planned n below 1.
Whether the attributes exist on the images is the grid's business.
"""
function experiment_design_from(raw::AbstractDict)::ExperimentDesign
    g(k, default = nothing) = get(raw, k, get(raw, Symbol(k), default))
    group, unit, parents = _design_attrs(g("group")), _design_attrs(g("unit")), _design_attrs(g("parents"))
    all_attrs = vcat(group, unit, parents)
    any(isempty, all_attrs) && error("An attribute name in the design is blank")
    dup = [a for a in unique(all_attrs) if count(==(a), all_attrs) > 1]
    isempty(dup) || error("\"$(first(dup))\" is in the design twice: an attribute plays one role")
    ExperimentDesign(
        question = strip(string(something(g("question", ""), ""))),
        expect   = parse_expect_direction(something(g("expect", "none"), "none")),
        mode     = parse_design_mode(something(g("mode", "designed"), "designed")),
        status   = parse_design_status(something(g("status", "inferred"), "inferred")),
        group = group, unit = unit, parents = parents,
        planned_parents = _design_planned(g("plannedParents"), "Planned parents per group"),
        planned_units   = _design_planned(g("plannedUnits"), "Planned units per parent"))
end

"""
    experiment_design_hash(d) -> String

16 hex chars of a SHA-256 over the design's canonical dict — the gate history's hashing (`_gate_canon`),
so two equal designs hash equal whatever order their dicts were built in.
"""
experiment_design_hash(d::ExperimentDesign)::String =
    bytes2hex(SHA.sha256(_gate_canon(experiment_design_dict(d))))[1:16]

"""The set's design, or `nothing` when it has none (the no-design state)."""
function experiment_design(s::CciaSet)::Union{ExperimentDesign,Nothing}
    raw = get(s.meta, EXPERIMENT_DESIGN_KEY, nothing)
    raw isa AbstractDict || return nothing
    experiment_design_from(raw)
end

experiment_design_history_path(s::CciaSet)::String = joinpath(s._dir, EXPERIMENT_DESIGN_HISTORY)

"""
    experiment_design_history(s) -> Vector

The set's design changes, oldest first: `{version, hash, at, by, change, design}` (`design` null =
cleared). [] when none was recorded.
"""
experiment_design_history(s::CciaSet)::Vector{Any} = read_jsonl(experiment_design_history_path(s))

"""The current version number (1-based, the history's length), or 0 when there is no recorded change."""
experiment_design_version(s::CciaSet)::Int = length(experiment_design_history(s))

"""
    set_experiment_design!(proj, set_uid, design; change = "") -> CciaSet

Write the set's design (`nothing` clears it) and append the change to its history, both inside the
set's transaction. `change` is a short note for the history row ("confirmed", "unit: Movie → image").
A write that leaves the design as it was records nothing.
"""
function set_experiment_design!(proj::CciaProject, set_uid::String,
                                d::Union{ExperimentDesign,Nothing}; change::AbstractString = "")::CciaSet
    idx = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(idx) && error("Set not found: $set_uid")
    s = proj._sets[idx]
    new = d === nothing ? nothing : experiment_design_dict(d)
    new_hash = d === nothing ? nothing : experiment_design_hash(d)
    commit_state!(s) do raw
        # nested values come back read-only with Symbol keys: rebuild meta String-keyed (as set_recipe!)
        m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
        old = get(m, EXPERIMENT_DESIGN_KEY, nothing)
        old_hash = old isa AbstractDict ? experiment_design_hash(experiment_design_from(old)) : nothing
        old_hash == new_hash && return
        new === nothing ? delete!(m, EXPERIMENT_DESIGN_KEY) : (m[EXPERIMENT_DESIGN_KEY] = new)
        raw["meta"] = m
        append_jsonl!(experiment_design_history_path(s), [Dict{String,Any}(
            "version" => experiment_design_version(s) + 1, "hash" => new_hash,
            "at" => string(Dates.now()), "by" => author_stamp(),
            "change" => strip(String(change)), "design" => new)])
    end
    new === nothing ? delete!(s.meta, EXPERIMENT_DESIGN_KEY) : (s.meta[EXPERIMENT_DESIGN_KEY] = new)
    s
end

set_experiment_design!(proj::CciaProject, s::CciaSet, d::Union{ExperimentDesign,Nothing}; kw...)::CciaSet =
    set_experiment_design!(proj, s.uid, d; kw...)

# ── The grid ───────────────────────────────────────────────────────────────────────────────────────

_design_values(attrs::Dict{String,String}, names::Vector{String}) =
    String[strip(get(attrs, a, "")) for a in names]

"""
    experiment_design_grid(images, d; attrs = Dict()) -> Dict

Place each image by its attributes: one cell per (group, parent) pair, holding its units and each
unit's images. `attrs` (image uid → attribute → value) overrides what the images carry, so a dialog can
show the grid for values it has not written yet.

Reply: `{cells: [{group, parent, units: [{unit, images: [uid]}]}], groups: [{group, nParents, nUnits,
nImages}], unassigned: [{uid, name, missing: [attr]}], missingAttrs: [attr], planned}`. An image is
unassigned when one of its role attributes is blank; `missingAttrs` names role attributes no image
carries. With no unit attribute the image is the unit (its name), and with no parents every cell's
`parent` is [].
"""
function experiment_design_grid(images::AbstractVector, d::ExperimentDesign;
                                attrs::AbstractDict = Dict{String,Any}())::Dict{String,Any}
    roles = vcat(d.group, d.parents, d.unit)
    cells = Dict{Tuple{Vector{String},Vector{String}},Dict{Vector{String},Vector{String}}}()
    unassigned = Any[]
    seen = Set{String}()
    for img in images
        a = Dict{String,String}(img.attr)
        for (k, v) in get(attrs, img.uid, Dict{String,Any}())
            a[string(k)] = string(something(v, ""))
        end
        foreach(r -> isempty(strip(get(a, r, ""))) || push!(seen, r), roles)
        missing_roles = [r for r in roles if isempty(strip(get(a, r, "")))]
        if !isempty(missing_roles)
            push!(unassigned, Dict{String,Any}("uid" => img.uid, "name" => img.name, "missing" => missing_roles))
            continue
        end
        key = (_design_values(a, d.group), _design_values(a, d.parents))
        unit = isempty(d.unit) ? [img.name] : _design_values(a, d.unit)
        push!(get!(get!(cells, key, Dict{Vector{String},Vector{String}}()), unit, String[]), img.uid)
    end
    keys_sorted = sort!(collect(keys(cells)))
    groups = Any[]
    for gk in unique(first.(keys_sorted))
        mine = [k for k in keys_sorted if k[1] == gk]
        push!(groups, Dict{String,Any}("group" => gk, "nParents" => isempty(d.parents) ? 0 : length(mine),
            "nUnits" => sum(length(cells[k]) for k in mine),
            "nImages" => sum(sum(length, values(cells[k])) for k in mine)))
    end
    Dict{String,Any}(
        "cells" => Any[Dict{String,Any}("group" => k[1], "parent" => k[2],
                       "units" => Any[Dict{String,Any}("unit" => u, "images" => cells[k][u]) for u in sort!(collect(keys(cells[k])))])
                       for k in keys_sorted],
        "groups" => groups,
        "unassigned" => unassigned,
        "missingAttrs" => [r for r in roles if !(r in seen)],
        "planned" => Dict{String,Any}("parents" => d.planned_parents, "units" => d.planned_units))
end

experiment_design_grid(s::CciaSet, d::ExperimentDesign; kw...) = experiment_design_grid(s._images, d; kw...)
