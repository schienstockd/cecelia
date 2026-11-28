# ── Run history: what a run read, what it replaced, which gates it saw ─────────────────────────────
#
# The interpretation half of the run log. run_log.jl (area "run records") owns the file: its path,
# format, the open/close/skip/reap writes and the raw entry readers (`read_run_log`, `run_log_row`,
# `run_entry_status`, …). This file READS entries against the task specs (tasks/task/spec_leaves.jl)
# and the gate sidecars to answer what a run depends on — `run_inputs` / `run_replaces` / `run_gates`
# (stamped at open through `run_provenance`), the maker of an output (`_run_maker`), and the gated
# populations past runs read (`run_pop_reads`). The *inputs* / *replaces* header in run_log.jl
# describes the stamped fields. Area "run history" in scripts/architecture/julia_areas.toml.

# ── Provenance stamped at open: what the run reads, and what it replaces ─────────────────────────
# Resolved from the task spec's declared params (`_history_leaves` — the same leaf reading the chain
# pre-flight and chain-from-history use), so every task gets it with no per-task code.

# The image field a name in namespace `ns` lives in, for a version lookup; `nothing` = no versions
# there. The namespace → field table is the chain pre-flight's (`_preflight_fields`).
_run_input_field(ns::AbstractString) = (fs = _preflight_fields(String(ns)); fs === nothing ? nothing : first(fs))

# Does `img` hold `name` in namespace `ns`? `nothing` = not checkable from the image (models, clusters, …).
_run_has_output(img::CciaImage, name::AbstractString, ns::AbstractString) =
    _preflight_fields(String(ns)) === nothing ? nothing : _preflight_has(img, String(name), String(ns))

# The version `img` holds `name` at, or "" when it does not hold it / the namespace has none.
function _run_name_version(img::CciaImage, name::AbstractString, ns::AbstractString)::String
    f = _run_input_field(ns)
    (f === nothing || !img_has_value_name(img, name; field = f)) && return ""
    resolve_version(img, f, name)
end

"""
    run_inputs(fun_name, img, params) -> Vector{Dict}

The names a run of `fun_name` with `params` reads on `img` — one `{param, name, ns?, version?, pinned?}`
per non-writing `valueNameSelection` value (nested groups and multi-selects included). See the header.
"""
function run_inputs(fun_name::AbstractString, img::CciaImage, params::AbstractDict)::Vector{Any}
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return Any[]
    # a renamed param's saved key (`formerKeys`) reads as the leaf it now is
    bykey = Dict{String,Tuple{String,Any}}()
    for (k, l) in leaves
        bykey[k] = (k, l)
        for f in l.former; haskey(bykey, f) || (bykey[f] = (k, l)); end
    end
    pin = string(something(get(params, "version", nothing), ""))
    out = Any[]
    seen = Set{Tuple{String,String}}()
    for (sk, v) in _keyed_strings(params)
        hit = get(bykey, sk, nothing)
        hit === nothing && continue
        k, l = hit
        (l.type == "valueNameSelection" && !l.writes) || continue
        name = String(strip(v))
        if isempty(name)              # an empty picker means the active one — name it
            name = l.ns == "" ? versioned_active(img.filepath) :
                   l.ns == "labels" ? resolve_value_name(img) : ""
            isempty(name) && continue
        end
        (k, name) in seen && continue
        push!(seen, (k, name))
        d = Dict{String,Any}("param" => k, "name" => name)
        isempty(l.ns) || (d["ns"] = l.ns)
        if k == "valueName" && l.ns == "" && !isempty(pin)
            d["version"] = pin; d["pinned"] = true    # the read is pinned (chain pinning, P3)
        else
            ver = _run_name_version(img, name, l.ns)
            isempty(ver) || (d["version"] = ver)
        end
        push!(out, d)
    end
    out
end

# (name, ns, into) a run writes: its named or fixed output (`_history_output`), else — for a task that
# writes INTO its input (`writesInto`, tracking) — that input, with `into = true`. ("", "", false) = none.
# A name `_history_output` found under no writing param (the pre-registry `outputValueName` spelling,
# ns "") that IS the declared `writesInto` input is that input too: `segment.measureLabels` picks the
# label set it re-measures under `outputValueName`, and its measures go INTO that set — not into an
# image version of the same name, which is what ns "" would claim.
function _run_output_identity(fn::AbstractString, params::Dict{String,Any}, leaves)
    name, ns = _history_output(String(fn), params, leaves)
    key = task_writes_into_key(String(fn))      # folds a composite to the step that declares it
    l = get(leaves, key, nothing)
    if !isempty(name)
        (ns == "" && l !== nothing && !l.writes &&
         strip(string(something(get(params, key, nothing), ""))) == name) || return (name, ns, false)
        return (name, l.ns, true)
    end
    into = task_params_name(String(fn), params)
    isempty(into) && return ("", "", false)
    (into, l === nothing ? "" : l.ns, true)
end

# A task and its composite steps, by name — what two runs must share for one to have made what the
# other writes into (`_run_maker`).
_run_step_funs(fn::AbstractString)::Set{String} =
    (t = try _task_from_fun_name(String(fn)) catch; nothing end;
     Set{String}(vcat(String(fn), t === nothing ? String[] : _composite_step_names(t))))

# The newest `done` entry in `entries` that wrote (name, ns). For an `into` output, a run that SHARES A
# STEP with `fun_name` and wrote or wrote into that name: a re-measure's maker is the segment + measure
# composite that measured the label set (both run `segment.measureLabels`), a re-track's the tracking
# composite — but never the segmentation that made the label set its tracks were written into.
function _run_maker(entries, fun_name::AbstractString, name::AbstractString, ns::AbstractString, into::Bool,
                    leaves_cache::Dict{String,Any})
    mine = into ? _run_step_funs(fun_name) : Set{String}()
    steps_cache = Dict{String,Set{String}}()
    for e in Iterators.reverse(entries)
        run_entry_status(e) == "done" || continue
        # no params at all = logged before params were recorded: its output name is unknowable. (Not
        # `r.legacy`, which also reads a run whose only param was its `valueName` as legacy.)
        _rl_get(e, "params", nothing) isa AbstractDict || continue
        run_entry_unchanged(e) && continue          # wrote nothing, so made nothing
        r = run_log_row(e)
        into && isdisjoint(mine, get!(() -> _run_step_funs(r.fn), steps_cache, r.fn)) && continue
        lv = get!(() -> _history_leaves(r.fn), leaves_cache, r.fn)
        lv === nothing && continue
        (n2, ns2, into2) = try _run_output_identity(r.fn, r.params, lv) catch; ("", "", false) end
        (n2 == name && ns2 == ns && (into || !into2)) && return e
    end
    nothing
end

"""
    run_replaces(fun_name, img, params) -> Dict | nothing

What a run of `fun_name` with `params` will overwrite on `img`, or `nothing`. See the header.
"""
function run_replaces(fun_name::AbstractString, img::CciaImage, params::AbstractDict)
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return nothing
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    name, ns, into = _run_output_identity(fun_name, p, leaves)
    isempty(name) && return nothing
    # a fixed `default` output is a NEW image's (bin, crop, the projections) or a file (export) — the
    # source image's `default` is never written over. Same exclusion as the chain pre-flight.
    (ns == "" && !into && name == VERSIONED_DEFAULT_VAL) && return nothing
    # an image version under keepPreviousVersion is minted beside the old one, not over it
    (ns == "" && !into && keep_previous_version()) && return nothing
    # existence: read off the image where the namespace allows it; else (and for a write-into) a run made it
    on_img = into ? nothing : _run_has_output(img, name, ns)
    on_img === false && return nothing          # a new output: no log walk at all
    cache = Dict{String,Any}()
    maker = _run_maker(read_run_log(img), fun_name, name, ns, into, cache)
    maker === nothing && (maker = _run_maker(read_run_log_archive(img), fun_name, name, ns, into, cache))
    (on_img === true || maker !== nothing) || return nothing
    d = Dict{String,Any}("name" => name)
    isempty(ns) || (d["ns"] = ns)
    ver = into ? "" : _run_name_version(img, name, ns)
    isempty(ver) || (d["version"] = ver)
    if maker !== nothing
        d["taskId"] = _rl_str(maker, "taskId")
        d["at"]     = _rl_str(maker, "at")
        d["fun"]    = _rl_str(maker, "fun")
    end
    d
end

"""
    run_gates(fun_name, img, params) -> Vector{Dict}

The gated populations a run of `fun_name` with `params` reads on `img`, each `{valueName, path, hash}`
(+ `popType: "track"` for a track gate) with the content hash of its gate now (`pop_gate_hashes`). Every
population picker counts, track pickers too: `B/qc/_tracked` is read through the cell gate `/qc`. A path
is looked up in the sidecar `resolve_pop_type` finds it in; cluster/region pops are not gates and a
population in no sidecar is left out.
"""
function run_gates(fun_name::AbstractString, img::CciaImage, params::AbstractDict)::Vector{Any}
    leaves = _history_leaves(String(fun_name))
    leaves === nothing && return Any[]
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    hashes = Dict{Tuple{String,String},Dict{String,String}}()
    out = Any[]
    for (vn, path) in gated_pop_refs(p, leaves; all_pickers = true)
        pt = resolve_pop_type(img, vn, path)
        is_gating_pop_type(pt) || continue
        hs = get!(() -> pop_gate_hashes(load_pop_tree(img._dir, vn; pop_type = pt, backfill_save = false)),
                  hashes, (vn, pt))
        h = get(hs, path, nothing)
        h === nothing && continue
        d = Dict{String,Any}("valueName" => vn, "path" => path, "hash" => h)
        pt == "flow" || (d["popType"] = pt)
        push!(out, d)
    end
    out
end

"""
    run_provenance(fun_name, img, params) -> (; inputs, replaces, gates)

`run_inputs` + `run_replaces` + `run_gates` for one image, read from a FRESH copy of its `ccid.json` (an earlier step
of the same chain may have written a version since `img` was loaded). Never throws: provenance is not
worth failing a run over, so a lookup that errors answers "nothing recorded".
"""
function run_provenance(fun_name::AbstractString, img::CciaImage, params::AbstractDict)
    fresh = try _load_image(img._dir) catch; img end
    inputs = try run_inputs(fun_name, fresh, params) catch e
        @warn "run-log inputs lookup failed" fun_name exception = e
        Any[]
    end
    replaces = try run_replaces(fun_name, fresh, params) catch e
        @warn "run-log replaces lookup failed" fun_name exception = e
        nothing
    end
    gates = try run_gates(fun_name, fresh, params) catch e
        @warn "run-log gate lookup failed" fun_name exception = e
        Any[]
    end
    (; inputs, replaces, gates)
end

"""
    run_pop_reads(img) -> Dict{(value_name, path) => [fun, …]}

The gated populations this image's runs READ, each with the tasks that read it — a clustering on
`B/qc/_tracked` reads `("B", "/qc")`. Runs that finished, or are still running; a failed or cancelled
run left nothing that depends on its input. Read through the task specs' population pickers
(`gated_pop_refs`), so a task that no longer exists is skipped.
"""
function run_pop_reads(img::CciaImage)::Dict{Tuple{String,String},Vector{String}}
    out = Dict{Tuple{String,String},Vector{String}}()
    leaves = Dict{String,Any}()
    # the whole history: this guards a gate edit against runs that depend on it, however old
    for e in read_run_log_history(img)
        r = run_log_row(e)
        r.status in ("done", RUN_LOG_RUNNING) || continue
        lv = get!(() -> _history_leaves(r.fn), leaves, r.fn)
        lv === nothing && continue
        # every population picker: a track analysis on `B/qc/_tracked` reads the cell gate `/qc` too
        for ref in gated_pop_refs(r.params, lv; all_pickers = true)
            fns = get!(out, ref, String[])
            r.fn in fns || push!(fns, r.fn)
        end
    end
    out
end
