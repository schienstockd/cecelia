# ── Declared tests: the `tests` block of a task spec ──────────────────────────
# Loaded via tasks/task.jl — do NOT `include` this fragment directly.
#
# A spec can say how to check itself: run on a fixture named by its properties, with these params,
# and the outputs must satisfy these assertions. The runner is `app/test/suite/task_spec_tests.jl`;
# the schema is documented in docs/MODULES.md → *Declared tests*. The assertion names follow Galaxy's
# image assertions (`has_image_n_labels`, `has_image_width`, …) — ideas only: here every output is
# read through Cecelia's own readers.
# Plan: docs/todo/GALAXY_LESSONS_PLAN.md → *tests in the module spec*.
#
# Two halves: `parse_spec_tests` + the pure `spec_assertion_observed` / `spec_assertion_check`
# (unit-tested on hand-built inputs), and `evaluate_spec_assertion`, the only part that reads a file.

const SPEC_TEST_OUTPUT_KINDS = ("image", "labels", "cells")

# assertion type => the output kinds it can be asked of
const SPEC_TEST_ASSERTIONS = Dict{String,Tuple}(
    "width"            => ("image", "labels"),
    "height"           => ("image", "labels"),
    "channels"         => ("image", "labels"),
    "depth"            => ("image", "labels"),
    "frames"           => ("image", "labels"),
    "n_labels"         => ("cells",),
    "mean_object_size" => ("cells",),
    "center_of_mass"   => ("cells",),
    "mean_intensity"   => ("cells",),
    "mean_measure"     => ("cells",),
)

# shape assertion => NGFF axis; an axis the store lacks counts as 1 (a label store has no `c`)
const _SPEC_TEST_SHAPE_AXIS = Dict("width" => "x", "height" => "y", "channels" => "c",
                                   "depth" => "z", "frames" => "t")

"""One assertion on one output of a declared test."""
struct SpecAssertion
    type::String
    kind::String                         # "image" | "labels" | "cells"
    value_name::String
    value::Union{Nothing,Float64,Dict{String,Float64}}   # a Dict only for center_of_mass
    delta::Float64                       # |observed - value| <= delta (Euclidean for center_of_mass)
    min::Union{Nothing,Float64}
    max::Union{Nothing,Float64}
    column::Union{Nothing,String}        # the channel (mean_intensity) or column (mean_measure)
end

"""One declared test: which fixture, which params, what the outputs must satisfy."""
struct SpecTest
    name::String
    properties::Vector{Symbol}
    technology::Union{Nothing,String}
    need::Union{Nothing,String}
    params::Dict{String,Any}
    assertions::Vector{SpecAssertion}
end

# A field of a spec object, whichever key type it was parsed with (JSON3 → Symbol, _task_spec → String).
_st_get(d, k, default = nothing) = get(d, k, get(d, Symbol(k), default))
_st_keys(d) = Set(String(k) for k in keys(d))
_st_num(x, what) = x isa Real ? Float64(x) : throw(ArgumentError("$what must be a number, got $(repr(x))"))

function _st_check_keys(d, allowed, where_)
    bad = setdiff(_st_keys(d), allowed)
    isempty(bad) || throw(ArgumentError("$where_: unknown field(s) $(join(sort(collect(bad)), ", ")); " *
                                        "allowed: $(join(sort(collect(allowed)), ", "))"))
end

function _parse_spec_assertion(a, kind::String, vn::String, where_::String)::SpecAssertion
    a isa AbstractDict || throw(ArgumentError("$where_: an assertion is an object"))
    _st_check_keys(a, ("type", "value", "delta", "min", "max", "channel", "column"), where_)
    type = string(something(_st_get(a, "type"), ""))
    haskey(SPEC_TEST_ASSERTIONS, type) ||
        throw(ArgumentError("$where_: unknown assertion type $(repr(type)); one of $(join(sort(collect(keys(SPEC_TEST_ASSERTIONS))), ", "))"))
    kind in SPEC_TEST_ASSERTIONS[type] ||
        throw(ArgumentError("$where_: $type applies to $(join(SPEC_TEST_ASSERTIONS[type], "/")) outputs, not $kind"))
    raw_v, raw_d = _st_get(a, "value"), _st_get(a, "delta")
    lo = _st_get(a, "min"); hi = _st_get(a, "max")
    lo === nothing || (lo = _st_num(lo, "$where_: min"))
    hi === nothing || (hi = _st_num(hi, "$where_: max"))
    (raw_v === nothing && lo === nothing && hi === nothing) &&
        throw(ArgumentError("$where_: $type needs `value` (with optional `delta`) or `min`/`max`"))
    raw_v === nothing && raw_d !== nothing &&
        throw(ArgumentError("$where_: `delta` is a tolerance on `value`, and there is no `value`"))
    delta = raw_d === nothing ? 0.0 : _st_num(raw_d, "$where_: delta")
    delta >= 0 || throw(ArgumentError("$where_: delta must be ≥ 0"))
    value = if raw_v === nothing
        nothing
    elseif type == "center_of_mass"
        raw_v isa AbstractDict && !isempty(raw_v) ||
            throw(ArgumentError("$where_: center_of_mass value is an object of axes, e.g. {\"x\": 3.5, \"y\": 7.5}"))
        bad = setdiff(_st_keys(raw_v), ("x", "y", "z", "t"))
        isempty(bad) || throw(ArgumentError("$where_: center_of_mass axes are x, y, z, t; got $(join(bad, ", "))"))
        Dict{String,Float64}(String(k) => _st_num(v, "$where_: center_of_mass.$k") for (k, v) in raw_v)
    else
        _st_num(raw_v, "$where_: value")
    end
    type == "center_of_mass" && (lo !== nothing || hi !== nothing) &&
        throw(ArgumentError("$where_: center_of_mass takes `value` + `delta` (a distance), not min/max"))
    col = if type == "mean_intensity"
        c = _st_get(a, "channel")
        c isa AbstractString && !isempty(c) || throw(ArgumentError("$where_: mean_intensity names its `channel`"))
        String(c)
    elseif type == "mean_measure"
        c = _st_get(a, "column")
        c isa AbstractString && !isempty(c) || throw(ArgumentError("$where_: mean_measure names its `column`"))
        String(c)
    else
        (_st_get(a, "channel") === nothing && _st_get(a, "column") === nothing) ||
            throw(ArgumentError("$where_: only mean_intensity takes `channel`, only mean_measure takes `column`"))
        nothing
    end
    SpecAssertion(type, kind, vn, value, delta, lo, hi, col)
end

"""
    parse_spec_tests(spec) -> Vector{SpecTest}

The spec's `tests` block, checked: unknown fields, an assertion asked of the wrong kind of output, or a
missing tolerance throw an `ArgumentError` naming the test. A spec with no `tests` gives `[]`.
"""
function parse_spec_tests(spec::AbstractDict)::Vector{SpecTest}
    raw = _st_get(spec, "tests")
    raw === nothing && return SpecTest[]
    raw isa AbstractVector || throw(ArgumentError("`tests` is a list"))
    out = SpecTest[]
    for (i, t) in enumerate(raw)
        t isa AbstractDict || throw(ArgumentError("tests[$i] is an object"))
        name = string(something(_st_get(t, "name"), ""))
        where_ = isempty(name) ? "tests[$i]" : "test \"$name\""
        isempty(name) && throw(ArgumentError("$where_: every test has a `name`"))
        _st_check_keys(t, ("name", "fixture", "params", "outputs"), where_)
        fx = _st_get(t, "fixture")
        fx isa AbstractDict || throw(ArgumentError("$where_: `fixture` is an object with `properties`"))
        _st_check_keys(fx, ("properties", "technology", "need"), "$where_ fixture")
        props = _st_get(fx, "properties")
        props isa AbstractVector && !isempty(props) ||
            throw(ArgumentError("$where_: fixture.properties lists what the data must have"))
        tech = _st_get(fx, "technology"); need = _st_get(fx, "need")
        params = _st_get(t, "params", Dict{String,Any}())
        params isa AbstractDict || throw(ArgumentError("$where_: `params` is an object"))
        outs = _st_get(t, "outputs")
        outs isa AbstractVector && !isempty(outs) ||
            throw(ArgumentError("$where_: `outputs` lists at least one output with assertions"))
        asserts = SpecAssertion[]
        for (j, o) in enumerate(outs)
            ow = "$where_ outputs[$j]"
            o isa AbstractDict || throw(ArgumentError("$ow is an object"))
            kinds = [k for k in SPEC_TEST_OUTPUT_KINDS if _st_get(o, k) !== nothing]
            length(kinds) == 1 ||
                throw(ArgumentError("$ow names exactly one of $(join(SPEC_TEST_OUTPUT_KINDS, ", ")) (its value name)"))
            _st_check_keys(o, (kinds[1], "assert"), ow)
            vn = string(_st_get(o, kinds[1]))
            as = _st_get(o, "assert")
            as isa AbstractVector && !isempty(as) || throw(ArgumentError("$ow: `assert` is a non-empty list"))
            for (k, a) in enumerate(as)
                push!(asserts, _parse_spec_assertion(a, kinds[1], vn, "$ow assert[$k]"))
            end
        end
        push!(out, SpecTest(name, Symbol.(string.(props)),
                            tech === nothing ? nothing : string(tech),
                            need === nothing ? nothing : string(need),
                            Dict{String,Any}(String(k) => v for (k, v) in params), asserts))
    end
    out
end

# ── Pure evaluation ───────────────────────────────────────────────────────────

"""
    spec_assertion_check(a, observed) -> (ok::Bool, message::String)

Whether `observed` satisfies `a`'s tolerance: within `value ± delta` (for `center_of_mass`, a
Euclidean distance over the axes `value` names) and within `[min, max]`, both when both are given.
"""
function spec_assertion_check(a::SpecAssertion, observed)::Tuple{Bool,String}
    label = "$(a.type)$(a.column === nothing ? "" : " $(a.column)") of $(a.kind) $(a.value_name)"
    observed === nothing && return (false, "$label: nothing to measure")
    if a.value isa Dict
        observed isa AbstractDict || return (false, "$label: no centroid")
        missing_axes = [ax for ax in keys(a.value) if !haskey(observed, ax)]
        isempty(missing_axes) || return (false, "$label: the output has no $(join(missing_axes, ", ")) axis")
        d = sqrt(sum(abs2(observed[ax] - v) for (ax, v) in a.value))
        ok = d <= a.delta
        return (ok, "$label: expected $(a.value) ± $(a.delta), found " *
                    "$(Dict(ax => observed[ax] for ax in keys(a.value))) (distance $(round(d; sigdigits = 4)))")
    end
    observed isa Real && isfinite(observed) || return (false, "$label: found $(repr(observed))")
    ok = true; parts = String[]
    if a.value !== nothing
        ok &= abs(observed - a.value) <= a.delta
        push!(parts, "$(a.value) ± $(a.delta)")
    end
    a.min === nothing || (ok &= observed >= a.min)
    a.max === nothing || (ok &= observed <= a.max)
    (a.min !== nothing || a.max !== nothing) &&
        push!(parts, "in [$(something(a.min, -Inf)), $(something(a.max, Inf))]")
    (ok, "$label: expected $(join(parts, " and ")), found $observed")
end

_finite_mean(x) = (v = [Float64(e) for e in x if e !== missing && e !== nothing && isfinite(e)];
                   isempty(v) ? nothing : sum(v) / length(v))

"""
    spec_assertion_observed(a, data) -> number | Dict | nothing

What `a` measures, from data already read: for a shape assertion `data` is the store's axis sizes
(`store_axis_sizes`), for a cell assertion the cell table as a DataFrame with the columns
`spec_assertion_columns(a)` names. Definitions:

- `width`/`height`/`channels`/`depth`/`frames` — level-0 size of the x/y/c/z/t axis; 1 when absent.
- `n_labels` — rows in the cell table (objects; one row per object per frame in a movie).
- `mean_object_size` — mean `area`: pixels in 2D, voxels in 3D (docs/DATAMODEL.md).
- `center_of_mass` — the centre of mass of all labelled pixels, `Σ area·centroid / Σ area`, per axis,
  in pixels (centroid columns are in pixels).
- `mean_intensity` / `mean_measure` — the mean over objects of that column, finite values only.
"""
function spec_assertion_observed(a::SpecAssertion, data)
    if haskey(_SPEC_TEST_SHAPE_AXIS, a.type)
        isempty(data) && return nothing
        return get(data, _SPEC_TEST_SHAPE_AXIS[a.type], 1)
    end
    df = data
    a.type == "n_labels" && return DataFrames.nrow(df)
    if a.type == "mean_object_size"
        "area" in names(df) || return nothing
        return _finite_mean(df[!, "area"])
    elseif a.type == "center_of_mass"
        ("area" in names(df) && DataFrames.nrow(df) > 0) || return nothing
        w = Float64.(df[!, "area"]); tot = sum(w)
        tot > 0 || return nothing
        out = Dict{String,Float64}()
        for ax in ("x", "y", "z", "t")
            c = "centroid_$ax"
            c in names(df) && (out[ax] = sum(w .* Float64.(df[!, c])) / tot)
        end
        return out
    else                                   # mean_intensity / mean_measure
        a.column in names(df) || return nothing
        return _finite_mean(df[!, a.column])
    end
end

"""The cell-table columns `a` reads (`nothing` for a shape assertion)."""
function spec_assertion_columns(a::SpecAssertion)::Union{Nothing,Vector{String}}
    haskey(_SPEC_TEST_SHAPE_AXIS, a.type) && return nothing
    a.type == "n_labels"         && return String[]
    a.type == "mean_object_size" && return ["area"]
    a.type == "center_of_mass"   && return ["area"]   # + the centroids, via view_centroid_cols
    [a.column]
end

# ── Reading the output ────────────────────────────────────────────────────────

"""Where an output lives: the image version's store, the label set's store, or the cell table."""
spec_output_path(img::CciaImage, kind::AbstractString, vn::AbstractString)::Union{Nothing,String} =
    kind == "image"  ? img_filepath(img, String(vn)) :
    kind == "labels" ? img_labels_path(img, String(vn)) :
    kind == "cells"  ? img_label_props_path(img, vn) : nothing

"""
    spec_output_stamp(path) -> String | Tuple | nothing

What identifies one write of an output, `nothing` when absent: `store_rev` for a store (image or
labels), inode + ctime + mtime for a file (the cell table). Every writer replaces its output
(`write_h5ad_atomic`, `staged_store`), so a run that wrote it leaves a different stamp. The runner
compares stamps from before and after the task, so an assertion can never pass on what the fixture
already held.
"""
function spec_output_stamp(path::Union{Nothing,AbstractString})
    (path === nothing || !ispath(path)) && return nothing
    isdir(path) && return store_rev(path)
    st = stat(path)
    (st.inode, st.ctime, st.mtime)
end

"""Whether a run wrote an output: it exists afterwards and its stamp changed (`spec_output_stamp`)."""
spec_output_written(before, after)::Bool = after !== nothing && after != before

"""
    evaluate_spec_assertion(img, a) -> (ok::Bool, message::String)

Read what `a` asks about from `img` — the store's extent (`store_axis_sizes` on `img_filepath` /
`img_labels_path`) or the cell table (`label_props(img; value_name)`, channels resolved by name) —
and check it.
"""
function evaluate_spec_assertion(img::CciaImage, a::SpecAssertion)::Tuple{Bool,String}
    path = spec_output_path(img, a.kind, a.value_name)
    (path === nothing || !ispath(path)) && return (false, "$(a.kind) $(a.value_name): nothing on disk")
    a.kind == "cells" || return spec_assertion_check(a, spec_assertion_observed(a, store_axis_sizes(path)))
    lp = label_props(img; value_name = a.value_name)
    a.type == "n_labels" && return spec_assertion_check(a, n_obs(lp))
    a.type == "center_of_mass" && view_centroid_cols(lp)
    df = lp |> select_cols(spec_assertion_columns(a)) |> as_df
    spec_assertion_check(a, spec_assertion_observed(a, df))
end
