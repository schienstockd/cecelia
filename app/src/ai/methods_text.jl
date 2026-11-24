# ── Methods report — one step as a sentence ─────────────────────────────────────────────────────────
#
# A step reads as the task spec's `methodsText` with its slots filled from what the step ran with, so
# the text says "tracked with btrack (maximum search radius 10 µm …)" rather than `maxSearchRadius=10`.
# `docs/MODULES.md` → *methodsText* is the authoring guide. A task with no template, or a template whose
# slot the record cannot fill, gets a plain sentence built from its changed parameters — never a guess.
#
# `methodsText` is a string, or a list of fragments joined in order; a fragment `{"text", "showIf"}`
# is kept only when its `showIf` holds for the run's params (the same evaluator as a form field and a
# conditional citation, `_show_if_satisfied`), so one template covers each setting of a select.
# `{name}` slots are param KEYS (the last segment: a repeatable group's first entry speaks for it).
# Values read as a reader would say them: units from the param label, select option labels, channel
# and population names, measure names in words. Where the images disagree on a slot the spread is
# stated ("gBT-uGFP in 9 images, OTI-uGFP in 7"), never one image's value as the method.

# Units a param label may end with — "Max search radius (µm)" → 10 µm. Anything else in brackets is
# not a unit ("Number of regions (k-means)").
const _MR_UNITS = ("µm", "µm²", "µm³", "°", "frames", "min", "s", "px", "%")
const _MR_TIGHT_UNITS = ("°", "%")

function _mr_unit(p)::String
    p isa AbstractDict || return ""
    m = match(r"\(([^()]+)\)\s*$", string(get(p, "label", "")))
    m === nothing && return ""
    u = String(strip(m.captures[1]))
    u in _MR_UNITS ? u : ""
end

# Measure columns a reader recognises: the few whose key is not already their name
const _MR_MEASURE_WORDS = Dict(
    "volume_mesh" => "volume", "surface_to_volume" => "surface-to-volume ratio",
    "surface_area_mesh" => "surface area", "displacementRatio" => "displacement ratio",
    "outreachRatio" => "outreach ratio", "meanTurningAngle" => "mean turning angle")

"""A measure column as words: `live.track.meanTurningAngle` → "mean turning angle", an HMM column
→ "HMM states (movement_3)"."""
function methods_measure_phrase(col)::String
    s = string(col)
    m = match(r"^live\.cell\.hmm\.(state|transitions)\.(.+)$", s)
    m === nothing || return "HMM $(m.captures[1] == "state" ? "states" : "transitions") ($(m.captures[2]))"
    s = replace(s, r"^live\.(track|cell)\." => "", r"^live\." => "")
    haskey(_MR_MEASURE_WORDS, s) && return _MR_MEASURE_WORDS[s]
    s = replace(s, r"([a-z0-9])([A-Z])" => s"\1 \2")
    lowercase(replace(s, "_" => " "))
end

"""
Population references (`valueName/path`) as names a reader tells apart: `pop_display_labels` with
`tracks = true`, so `Tcells/qc/non-OTI/gBT/_tracked` → "gBT", and two `OTI`s under different
parents → "qc/OTI", "other/OTI". The root (tracked or not) → "all".
"""
function methods_pop_phrases(refs)::Vector{String}
    paths = [(p = last(_split_pop_ref(string(r), VERSIONED_DEFAULT_VAL));
              is_root(p) || p == ROOT_TRACKED ? "" : String(rstrip(p, '/'))) for r in refs]
    labels = pop_display_labels(unique(filter(!isempty, paths)); tracks = true)
    String[isempty(p) ? "all" : labels[p] for p in paths]
end
"""One population reference as its name — `methods_pop_phrases` of one."""
methods_pop_phrase(ref)::String = only(methods_pop_phrases([ref]))

_mr_list(v) = v isa AbstractVector ? collect(v) : v isa AbstractString && !isempty(v) ? Any[v] : Any[]

function _mr_number(v::Real, unit::AbstractString)::String
    s = v isa Integer || (isfinite(v) && v == round(v)) ? string(round(Int, v)) : string(round(v; sigdigits = 4))
    s = startswith(s, "-") ? "−" * s[2:end] : s
    isempty(unit) ? s : unit in _MR_TIGHT_UNITS ? s * unit : "$s $unit"
end

"""One param value as a reader would say it — see the section comment. `p` is its spec (or nothing)."""
function _mr_value_text(v, p)::String
    t = p isa AbstractDict ? string(get(p, "type", "")) : ""
    t == "popSelection" && return _mr_join(unique!(methods_pop_phrases(_mr_list(v))))
    t == "labelPropsColsSelection" && return _mr_join(String[methods_measure_phrase(x) for x in _mr_list(v)])
    t == "channelSelection" && return _mr_join(String[string(x) for x in _mr_list(v)])
    t == "valueNameSelection" && return string(v) == VERSIONED_DEFAULT_VAL ? "raw" : string(v)
    if t == "select" && p isa AbstractDict
        for o in something(get(p, "options", nothing), [])
            o isa AbstractDict && string(get(o, "value", "")) == string(v) || continue
            l = replace(string(get(o, "label", v)), r"\s*\([^()]*\)\s*$" => "")   # "Whole movie (window graphs)"
            # "Whole movie" → "whole movie", but "PCA" stays
            return length(l) > 1 && islowercase(l[nextind(l, 1)]) ? lowercase(l[1:1]) * l[nextind(l, 1):end] : l
        end
    end
    v isa Bool && return v ? "on" : "off"
    v isa Real && return _mr_number(v, _mr_unit(p))
    v isa AbstractVector && return _mr_join(String[string(x) for x in v])
    string(v)
end

# "gaps of up to 1 frames" → "1 frame": a template says the plural, a value of 1 takes the singular
const _MR_ONE_PLURAL = r"(?<![\d.])1 (frame|image|timepoint|region|state|neighbour|cell)s\b"

# Where the images disagree: each value with its image count, or a pointer to the table when the
# values are too long to read inline (a list of channels per image)
function _mr_spread_text(spread::AbstractVector, p)::String
    parts = [(_mr_value_text(v, p), n) for (v, n) in spread]
    long = any(((v, _),) -> v isa AbstractVector && length(v) > 1, spread) ||
           any(((s, _),) -> occursin(",", s) || length(s) > 30, parts)
    long && return "set per image, see the table"
    join(("$s ($n image$(n == 1 ? "" : "s"))" for (s, n) in parts), " or ")
end

# Every param of task `fn` by key: its spec (label, type, options, default, showIf), a composite's
# steps' too. The first spec to name a key wins (the composite's own, then its steps in order).
function _mr_param_index(fn::AbstractString)::Dict{String,Any}
    task = try _task_from_fun_name(String(fn)) catch; nothing end
    out = Dict{String,Any}()
    task === nothing && return out
    specs = Any[_task_spec(task)]
    task isa CompositeTask && append!(specs, Any[_task_spec(s) for s in _composite_steps(task)])
    function walk(ps)
        for p in ps
            p isa AbstractDict || continue
            sub = get(p, "params", nothing)
            if sub isa AbstractVector
                k = string(get(p, "key", ""))              # a group: its label names its entries
                isempty(k) || haskey(out, k) || (out[k] = Dict{String,Any}("label" => get(p, "label", k)))
                walk(sub)
            else
                k = string(get(p, "key", ""))
                isempty(k) || haskey(out, k) || (out[k] = p)
            end
        end
    end
    for sp in specs
        sp isa AbstractDict && walk(something(get(sp, "params", nothing), []))
    end
    out
end

# A flat key (`_hp_flat`) as its slot: `models[0].diameter` → ("diameter", "models", 0); a plain key
# has no group.
function _mr_slot(k::AbstractString)
    m = match(r"^([^\[]+)\[([^\]]+)\]\.(.+)$", k)
    m === nothing && return (String(last(split(k, '.'))), "", 0)
    (String(m.captures[3]), String(m.captures[1]), something(tryparse(Int, m.captures[2]), typemax(Int)))
end

# A repeatable group's first entry speaks for its slots — the template has one `{diameter}`. The
# flat keys of later entries are left to the table: group → how many entries the run had.
function _mr_first_entries(keys)
    first_ix = Dict{String,Int}(); n = Dict{String,Set{Int}}()
    for k in keys
        _, g, i = _mr_slot(string(k))
        isempty(g) && continue
        first_ix[g] = min(get(first_ix, g, typemax(Int)), i)
        push!(get!(Set{Int}, n, g), i)
    end
    keep(k) = (s = _mr_slot(string(k)); isempty(s[2]) || s[3] == first_ix[s[2]])
    keep, Dict(g => length(v) for (g, v) in n), first_ix
end

# What the step ran with, by param key: the spec defaults under the latest done run's params; how
# many entries each repeatable group had, and the index of its first
function _mr_values(fn::AbstractString, entries, idx::Dict{String,Any})
    vals = Dict{String,Any}(k => p["default"] for (k, p) in idx if p isa AbstractDict && haskey(p, "default"))
    done = sort([e for e in entries if run_entry_status(e) == "done"]; by = e -> _rl_str(e, "at"))
    isempty(done) && return vals, Dict{String,Int}(), Dict{String,Int}()
    flat = _hp_flat(run_log_row(last(done)).params, _history_param_defaults(String(fn)), _history_leaves(String(fn)))
    keep, groups, first_ix = _mr_first_entries(keys(flat))
    for (k, v) in flat
        keep(k) && (vals[first(_mr_slot(k))] = v[1])
    end
    vals, groups, first_ix
end

_mr_template(spec) = (t = spec isa AbstractDict ? get(spec, "methodsText", nothing) : nothing;
                      t isa AbstractString ? (isempty(t) ? nothing : Any[t]) : t isa AbstractVector ? collect(t) : nothing)

"""
    _mr_methods_text(fn, entries, spread = Dict()) -> Union{String,Nothing}

Task `fn`'s `methodsText`, filled from what its run log `entries` ran with; `nothing` when the spec
has none, a slot has no value, or a fragment's `showIf` reads a param the images disagree on.
`spread` = `history_params_table`'s row `spread` (flat key → `[(value, n images)]`).
"""
function _mr_methods_text(fn::AbstractString, entries, spread::AbstractDict = Dict{String,Any}())
    spec = try _task_spec(_task_from_fun_name(String(fn))) catch; nothing end
    parts = _mr_template(spec)
    parts === nothing && return nothing
    idx = _mr_param_index(fn)
    vals, groups, first_ix = _mr_values(fn, entries, idx)
    # a group's slots are its first entry's: another entry's spread is that entry's, left to the table
    speaks(k) = ((_, g, i) = _mr_slot(string(k)); isempty(g) || i == get(first_ix, g, 0))
    disputed = Dict{String,Any}(first(_mr_slot(string(k))) => v for (k, v) in spread if speaks(k))
    out = IOBuffer()
    for part in parts
        text = if part isa AbstractString
            part
        elseif part isa AbstractDict
            cond = get(part, "showIf", nothing)
            if cond isa AbstractDict
                any(k -> haskey(disputed, string(k)), keys(cond)) && return nothing
                _show_if_satisfied(part, vals) || continue
            end
            string(get(part, "text", ""))
        else
            continue
        end
        for m in eachmatch(r"\{([A-Za-z0-9_]+)\}", text)
            k = String(m.captures[1])
            p = get(idx, k, nothing)
            filled = haskey(disputed, k) ? _mr_spread_text(disputed[k], p) :
                     haskey(vals, k) && !_hp_empty(vals[k]) ? _mr_value_text(vals[k], p) : nothing
            filled === nothing && return nothing
            text = replace(text, "{$k}" => filled)
        end
        print(out, text)
    end
    s = replace(String(take!(out)), _MR_ONE_PLURAL => s"1 \1")
    isempty(s) && return nothing
    # the template speaks for a group's first entry; the others are named, not dropped. A later
    # entry some image has no value for = the images ran different entries (one ran a second model);
    # an empty list is a value (no nucleus channels), only a missing key (`nothing`) is no entry
    differs = Set(g for (k, vs) in spread for (_, g, i) in (_mr_slot(string(k)),)
                  if !isempty(g) && i != get(first_ix, g, 0) && any(((v, _),) -> v === nothing, vs))
    for g in sort!(collect(union(differs, (g for (g, k) in groups if k > 1))))
        p = get(idx, g, nothing)
        what = p isa AbstractDict ? string(get(p, "label", g)) : g
        s = rstrip(s, '.') * (g in differs ?
            ". The images differ in their entries under \"$what\": the first is stated, all are in the table." :
            ". The run had $(groups[g]) entries under \"$what\": the first is stated, all are in the table.")
    end
    _mr_capitalise(s)
end

# A sentence's first letter up — unless the first word is a name with its own case ("gBT", "mTOR")
function _mr_capitalise(s::AbstractString)::String
    w = first(split(s, r"[\s,]"; limit = 2))
    any(isuppercase, w) ? String(s) : uppercase(s[1:1]) * s[nextind(s, 1):end]
end

# A step with no template: its label and the params it changed, in words
function _mr_plain_text(row)::String
    fn = string(get(row, "task", "")); step = string(get(row, "step", fn))
    idx = _mr_param_index(fn)
    bits = String[]; more = false
    for kv in split(string(get(row, "changedFromDefault", "")), "; ")
        i = findfirst('=', kv)
        i === nothing && continue
        k = String(kv[1:i-1]); v = String(kv[i+1:end])
        p = get(idx, String(last(split(k, '.'))), nothing)
        p isa AbstractDict && string(get(p, "type", "")) in ("valueNameInput",) && continue
        name = p isa AbstractDict ? replace(lowercase(string(get(p, "label", k))), r"\s*\([^()]*\)\s*$" => "") : k
        # a structured value (a channel-role map) does not read as prose: the table has it
        (startswith(v, "{") || startswith(v, "[")) && (more = true; continue)
        num = tryparse(Float64, v)
        push!(bits, "$name " * (v in ("true", "false") ? (v == "true" ? "on" : "off") :
                                num === nothing ? v : _mr_number(num, _mr_unit(p))))
    end
    more && push!(bits, "further settings in the table")
    s = isempty(bits) ? "$step was run." : "$step was run ($(join(bits, "; ")))."
    replace(s, _MR_ONE_PLURAL => s"1 \1")
end

# ── references ──

const _MR_REFERENCES_PATH = joinpath(@__DIR__, "references.toml")
const _MR_REFERENCES = Ref{Union{Nothing,Dict{String,Any}}}(nothing)
"""DOI → its reference line (`ai/references.toml`, generated from Crossref)."""
function methods_references()::Dict{String,Any}
    _MR_REFERENCES[] === nothing && (_MR_REFERENCES[] = TOML.parsefile(_MR_REFERENCES_PATH))
    _MR_REFERENCES[]
end
methods_reference_line(doi::AbstractString) =
    (r = get(methods_references(), String(doi), nothing); r === nothing ? "doi:$doi" : "$r doi:$doi")
