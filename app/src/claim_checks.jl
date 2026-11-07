# Claim checks — the mechanical half of the claim recital: does the RECORD let a claim be made, and
# was everything it compares treated the same way? Design + stages: docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md
# (S0 record gate, S1 consistency + confound list, S2 conventions). No model, no significance tests,
# read-only.
#
# Two steps, so the rules are testable without a project on disk:
#   claim_record_table(scope)  — reads the record: one row per (image, population) over EVERY image
#                                in the set, included or not, with the processing variables flattened
#                                to dotted keys ("params.segment.cellposeMeasure.models.0.cellDiameter").
#   claim_checks(table, scope) — PURE: S0 findings from the rows, S1 findings from the rows the claim
#                                actually runs on (included, with data). A gap skips only the checks
#                                that need the missing part.
# Findings are `qc_finding`s (text in qc/text.jl, `claim.*`) whose `detail` carries the stage, the
# provenance (`inferred`: read from the record, nothing recomputed) and the evidence.

"""
    ClaimScope(set, populations, measure; unit_attrs = String[])

What a claim is about: the set, the populations it compares (`"B/qc"`: segmentation `B`, gated
population `qc`), the measure (`"live.track.speed"`) and the image attributes whose values together
name one replicate — the statistical unit (`["Mouse"]`, or `["Mouse", "Location"]` when each site in
each mouse counts once). No claim text — parsing a sentence into this is the Translator's job.
"""
struct ClaimScope
    set::CciaSet
    populations::Vector{String}
    measure::String
    unit_attrs::Vector{String}
end
ClaimScope(set::CciaSet, populations::AbstractVector, measure::AbstractString; unit_attrs = String[]) =
    ClaimScope(set, String.(populations), String(measure), claim_unit_attrs(unit_attrs))

"""
    claim_unit_attrs(v) -> Vector{String}

A replicate's attributes, normalised: a list or one string (`nothing` = none), trimmed, blanks and
repeats dropped. The one rule for every scope that is stored, compared or checked.
"""
claim_unit_attrs(v) = unique!(String[strip(string(a)) for a in (v isa AbstractVector ? v : v === nothing ? () : (v,))
                                     if !isempty(strip(string(a)))])

# The replicate's name for text ("Mouse × Location") and an image's replicate ("M1 × left"; "" when
# any of its attributes is blank — a half-named replicate can't be counted).
_claim_unit_label(scope::ClaimScope) = join(scope.unit_attrs, " × ")
function _claim_unit_of(img, attrs)
    vals = String[string(get(img.attr, a, "")) for a in attrs]
    any(isempty ∘ strip, vals) ? "" : join(vals, " × ")
end

# The segmentation a population path lives on, and its gate path inside that segmentation's gating
# file — the shared pop-ref grammar (`_split_pop_ref`), minus derived markers like `_tracked`, which
# are filters, not gated populations.
_claim_pop_value_name(pop::AbstractString) = first(_split_pop_ref(pop, VERSIONED_DEFAULT_VAL))
function _claim_pop_gate_path(pop::AbstractString)::String
    segs = [s for s in split(last(_split_pop_ref(pop, VERSIONED_DEFAULT_VAL)), "/"; keepempty = false)
            if !haskey(_DERIVED_POPS, String(s))]
    "/" * join(segs, "/")
end

# `live.*` measures need tracking; `live.track.*` read the per-track table, the rest the cell table.
_claim_needs_tracking(measure::AbstractString) = startswith(measure, "live.")
_claim_reads_tracks(measure::AbstractString)   = startswith(measure, "live.track.")

# Image calibration compared across images. Numbers get a relative tolerance; units are categorical.
const _CLAIM_META_NUMERIC = ("TimeIncrement", "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "SizeZ")
const _CLAIM_META_UNITS   = ("TimeIncrementUnit", "PhysicalSizeUnit")
# A numeric difference below this fraction is shown, not flagged (pixel size 0.596 vs 0.5964).
const CLAIM_REL_TOLERANCE = 0.01
# Param keys that name inputs and outputs: equal by construction across images of one population,
# different by construction between populations. Never a processing difference.
const _CLAIM_NAME_KEY = r"(?i)(valueName|valueNameSuffix)$"
# Keys that differ between populations BY DESIGN — each population is segmented from its own channel
# and has its own gate. Compared across images, skipped in the between-group confound list.
const _CLAIM_PER_POP_KEY = r"(?i)(channels?|channel)$"

# The record's producing tasks for a population: segmentation always, tracking for `live.*`.
_claim_kinds(measure) = _claim_needs_tracking(measure) ? ("segment", "tracking") : ("segment",)

# Flatten nested params into dotted keys. Arrays are compared whole, as their JSON text.
function _claim_flatten!(out::Dict{String,Any}, prefix::AbstractString, v)
    if v isa AbstractDict
        for (k, vv) in v
            _claim_flatten!(out, prefix * "." * string(k), vv)
        end
    elseif v isa AbstractVector
        out[prefix] = JSON3.write(v)
    else
        out[prefix] = v
    end
    out
end

# The name a task run wrote under: the spec's output name, else the input name — tracking writes its
# track table beside the segmentation it read (`valueName`), so its spec output name is empty.
function _claim_run_output(fun::AbstractString, params::AbstractDict)::String
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    out = try task_output_name(String(fun), p) catch; "" end
    isempty(out) ? string(get(p, "valueName", "")) : out
end

# Every recorded params blob for (image, value_name, kind), one per task fun, with where it came from:
#   "byName" — banked under this output name (exact);
#   "runLog" — the most recent finished run that wrote this name (exact);
#   "flat"   — the flat last-run blob, ONLY when that run wrote this name (last run per task, so the
#              other population's params are gone — the S0 gap this reports).
function _claim_params_for(img::CciaImage, vn::AbstractString, kind::AbstractString)
    found = Dict{String,Tuple{Dict{String,Any},String}}()
    byname = get(img.meta, FUN_PARAMS_BY_NAME_META_KEY, nothing)
    if byname isa AbstractDict
        for (fun, per) in byname
            startswith(string(fun), kind * ".") || continue
            hit = _fun_params_entry(per, String(vn))
            isnothing(hit) || (found[string(fun)] = (hit, "byName"))
        end
    end
    for e in read_run_log(img._dir)
        r = run_log_row(e)
        (startswith(r.fn, kind * ".") && !haskey(found, r.fn) && r.status == "done" && !r.legacy) || continue
        _claim_run_output(r.fn, r.params) == vn || continue
        hit = run_log_params_for_output(img._dir, r.fn, vn)
        isnothing(hit) || (found[r.fn] = (hit, "runLog"))
    end
    flat = get(img.meta, FUN_PARAMS_META_KEY, nothing)
    if flat isa AbstractDict
        for (fun, p) in flat
            f = string(fun)
            (startswith(f, kind * ".") && !haskey(found, f) && p isa AbstractDict) || continue
            _claim_run_output(f, p) == vn &&
                (found[f] = (Dict{String,Any}(String(k) => v for (k, v) in p), "flat"))
        end
    end
    found
end

# The gate of `path` inside the segmentation's gating file, flattened: kind, axes and transforms as
# keys, the drawn geometry (keys sorted) as one value. Read through the one gating reader, read-only.
# `nothing` when the population isn't there; empty when it has no gate (the segmentation root).
function _claim_gate_vars(img::CciaImage, vn::AbstractString, path::AbstractString)
    path == "/" && return Dict{String,Any}()
    m = try load_pop_map(img._dir, vn; backfill_save = false) catch; return nothing end
    has_pop(m, path) || return nothing
    g = pop_at(m, path).gate
    isnothing(g) && return Dict{String,Any}()
    out = Dict{String,Any}()
    spec = gate_spec(g)
    for k in sort!(collect(keys(spec))); v = spec[k]
        ks = string(k)
        # kind, axes and transforms are compared as settings; everything else is the drawn shape
        if !(ks == "kind" || endswith(ks, "_channel") || endswith(ks, "_transform"))
            out["gate.geometry"] = get(out, "gate.geometry", "") * JSON3.write(v)
        else
            _claim_flatten!(out, "gate." * ks, v)
        end
    end
    # the drawn shape as a short digest: equal shapes compare equal, and evidence stays readable
    haskey(out, "gate.geometry") && (out["gate.geometry"] = bytes2hex(SHA.sha256(out["gate.geometry"]))[1:8])
    out
end

"""
    claim_record_table(scope::ClaimScope) -> Vector{Dict{String,Any}}

One row per (image, population) over EVERY image in the set. Each row: `image`, `pop`, `valueName`,
`included`, `hasData`, `unit` (the replicate: its attributes' values, `" × "`-joined), `paramsSource` (`kind => fun => source`)
and `vars` — the processing variables as flat keys (`meta.*`, `params.<fun>.*`, `gate.*`,
`motionDims`). Read-only.
"""
function claim_record_table(scope::ClaimScope)::Vector{Dict{String,Any}}
    rows = Dict{String,Any}[]
    for img in images(scope.set), pop in scope.populations
        vn = _claim_pop_value_name(pop)
        table = _claim_reads_tracks(scope.measure) ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
        vars = Dict{String,Any}()
        for k in _CLAIM_META_NUMERIC
            v = get(img.meta, k, nothing)
            v isa Real && (vars["meta." * k] = Float64(v))
        end
        for k in _CLAIM_META_UNITS
            v = get(img.meta, k, nothing)
            isnothing(v) || (vars["meta." * k] = string(v))
        end
        sources = Dict{String,Any}()
        for kind in _claim_kinds(scope.measure)
            found = _claim_params_for(img, vn, kind)
            sources[kind] = Dict{String,Any}(f => s for (f, (_, s)) in found)
            for (fun, (p, _)) in found
                flat = _claim_flatten!(Dict{String,Any}(), "params." * fun, p)
                for (k, v) in flat
                    occursin(_CLAIM_NAME_KEY, k) || (vars[k] = v)
                end
            end
        end
        gate = _claim_gate_vars(img, vn, _claim_pop_gate_path(pop))
        isnothing(gate) || merge!(vars, gate)
        if _claim_needs_tracking(scope.measure)
            doc = read_qc(img, "tracking.track_measures", vn)
            m = isnothing(doc) ? nothing : get(doc, :metrics, nothing)
            d = m isa AbstractDict ? get(m, :motionDims, nothing) : nothing
            isnothing(d) || (vars["motionDims"] = d)
        end
        # calibration problems, from the one metadata checker (qc.jl), as `field => [codes]`
        calib = Dict{String,Any}()
        for f in metadata_qc_findings(img.meta)
            push!(get!(calib, f["detail"]["field"], String[]), f["code"])
        end
        push!(rows, Dict{String,Any}(
            "image" => img.uid, "pop" => pop, "valueName" => vn, "included" => image_included(img),
            "calibration" => calib,
            "hasData" => isfile(table), "hasGate" => !isnothing(gate),
            "unit" => isempty(scope.unit_attrs) ? "" : _claim_unit_of(img, scope.unit_attrs),
            "paramsSource" => sources, "vars" => vars))
    end
    rows
end

# ── the rules (pure) ─────────────────────────────────────────────────────────────────────────────

_claim_detail(stage, evidence) = Dict{String,Any}("stage" => stage, "provenance" => "inferred", "evidence" => evidence)
_claim_imgs(rows) = sort!(unique(String[r["image"] for r in rows]))
_claim_imgs_union(xs) = sort!(unique(reduce(vcat, [x["images"] for x in xs]; init = String[])))
# A processing variable as a finding names it: what it is, not its key (`meta.PhysicalSizeX` → "Pixel
# size (x)", `params.segment.cellposeMeasure.models.0.cellDiameter` → "cellposeMeasure: models.0.cellDiameter").
# Evidence keeps the key.
const _CLAIM_VARIABLE_LABEL = Dict(
    "meta.TimeIncrement" => "Frame interval", "meta.TimeIncrementUnit" => "Frame interval unit",
    "meta.PhysicalSizeX" => "Pixel size (x)", "meta.PhysicalSizeY" => "Pixel size (y)",
    "meta.PhysicalSizeZ" => "Z step", "meta.PhysicalSizeUnit" => "Pixel size unit", "meta.SizeZ" => "Number of z slices",
    "gate.geometry" => "Gate shape", "motionDims" => "Track dimensions (2D/3D)")
function _claim_variable_label(k::AbstractString)
    haskey(_CLAIM_VARIABLE_LABEL, k) && return _CLAIM_VARIABLE_LABEL[k]
    startswith(k, "gate.") && return "Gate " * replace(k[6:end], "_" => " ")
    fun = _claim_param_fun(k)
    fun === nothing && return String(k)
    # the task, then the setting's path inside its params — one label per key, so two settings that
    # share a leaf name (models.0 / models.1 cellDiameter) stay two findings
    string(last(split(fun, ".")), ": ", k[length("params." * fun * ".")+1:end])
end
# Images as a finding names them: up to three by name, else a count.
_claim_imgs_text(imgs) = length(imgs) <= 3 ? join(imgs, ", ") : string(length(imgs), " images")
# A population as a finding names it: its segmentation when that tells the compared ones apart (`B`),
# else the whole path.
function _claim_pop_label(pop, scope)
    vns = [_claim_pop_value_name(p) for p in scope.populations]
    vn = _claim_pop_value_name(pop)
    count(==(vn), vns) == 1 ? vn : String(pop)
end

# Most common value, the images holding each value, and whether the mode is a tie.
function _claim_value_groups(vals::AbstractDict)
    groups = Dict{String,Vector{String}}()
    for (img, v) in vals
        push!(get!(groups, string(v), String[]), img)
    end
    foreach(sort!, values(groups))
    counts = sort!(collect(groups); by = kv -> (-length(kv[2]), kv[1]))
    tie = length(counts) > 1 && length(counts[1][2]) == length(counts[2][2])
    (groups = Dict(k => v for (k, v) in counts), mode = counts[1][1], tie = tie)
end

# S0 — can the record say what was done?
function _claim_s0(rows, scope)
    findings = Any[]
    by_img = Dict{String,Vector{Dict{String,Any}}}()
    for r in rows; push!(get!(by_img, r["image"], Dict{String,Any}[]), r); end

    excl = _claim_imgs(filter(r -> !r["included"], rows))
    isempty(excl) || push!(findings, qc_finding("warn", "claim.image_excluded"; n = length(excl), imgs = _claim_imgs_text(excl),
        detail = _claim_detail("S0", Dict{String,Any}("images" => excl))))

    for pop in scope.populations
        nodata = _claim_imgs(filter(r -> r["pop"] == pop && !r["hasData"], rows))
        isempty(nodata) || push!(findings, qc_finding("warn", "claim.no_data"; pop = _claim_pop_label(pop, scope),
            n = length(nodata), imgs = _claim_imgs_text(nodata),
            detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "images" => nodata,
                "excluded" => filter(i -> i in excl, nodata), "measure" => scope.measure))))
    end

    if !isempty(scope.unit_attrs)
        blank = _claim_imgs(filter(r -> isempty(r["unit"]), rows))
        isempty(blank) || push!(findings, qc_finding("warn", "claim.unit_blank"; attr = _claim_unit_label(scope),
            n = length(blank), imgs = _claim_imgs_text(blank), detail = _claim_detail("S0", Dict{String,Any}("attrs" => scope.unit_attrs,
                "images" => blank, "excluded" => filter(i -> i in excl, blank)))))
    end

    # params: only where the claim has data — an image with no output has nothing to have params for.
    # One fact for every (population, step) gap, not one per pair: "B tracking (5 images), T segment (6 images)".
    missing = Dict{String,Any}[]; flat = Dict{String,Any}[]
    for pop in scope.populations, kind in _claim_kinds(scope.measure)
        rs = filter(r -> r["pop"] == pop && r["hasData"], rows)
        m = _claim_imgs(filter(r -> isempty(r["paramsSource"][kind]), rs))
        f = _claim_imgs(filter(r -> !isempty(r["paramsSource"][kind]) && all(==("flat"), values(r["paramsSource"][kind])), rs))
        isempty(m) || push!(missing, Dict{String,Any}("pop" => pop, "kind" => kind, "images" => m))
        isempty(f) || push!(flat, Dict{String,Any}("pop" => pop, "kind" => kind, "images" => f))
    end
    gaps(xs) = join((string(_claim_pop_label(x["pop"], scope), " ", x["kind"], " (", _claim_imgs_text(x["images"]), ")") for x in xs), ", ")
    isempty(missing) || push!(findings, qc_finding("warn", "claim.params_not_recorded"; what = gaps(missing),
        detail = _claim_detail("S0", Dict{String,Any}("gaps" => missing, "images" => _claim_imgs_union(missing)))))
    isempty(flat) || push!(findings, qc_finding("info", "claim.params_last_run_only"; what = gaps(flat),
        detail = _claim_detail("S0", Dict{String,Any}("gaps" => flat, "images" => _claim_imgs_union(flat)))))

    if _claim_needs_tracking(scope.measure)
        for pop in scope.populations
            nodims = _claim_imgs(filter(r -> r["pop"] == pop && r["hasData"] && !haskey(r["vars"], "motionDims"), rows))
            isempty(nodims) || push!(findings, qc_finding("warn", "claim.dims_not_recorded"; pop = _claim_pop_label(pop, scope),
                n = length(nodims), imgs = _claim_imgs_text(nodims), detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "images" => nodims))))
        end
    end
    findings
end

# `params.<fun>.<setting>` → `<fun>` (task names carry one dot: `segment.cellposeMeasure`).
function _claim_param_fun(k::AbstractString)
    parts = split(k, ".")
    (length(parts) >= 4 && parts[1] == "params") ? join(parts[2:3], ".") : nothing
end
_claim_has_fun(r, fun) = any(k -> startswith(k, "params." * fun * "."), keys(r["vars"]))

# S1a — across images, one population at a time.
function _claim_s1_across(rows, scope)
    findings = Any[]
    for pop in scope.populations
        rs = filter(r -> r["pop"] == pop, rows)
        length(rs) < 2 && continue
        keys_all = sort!(unique(reduce(vcat, [collect(keys(r["vars"])) for r in rs]; init = String[])))
        for k in keys_all
            vals = Dict{String,Any}(r["image"] => r["vars"][k] for r in rs if haskey(r["vars"], k))
            # a setting one image's run has and another's run of the SAME task lacks is a difference
            # (a re-run with another version's params), not a gap
            fun = _claim_param_fun(k)
            if !isnothing(fun)
                for r in rs
                    (!haskey(vals, r["image"]) && _claim_has_fun(r, fun)) && (vals[r["image"]] = "(not set)")
                end
            end
            length(vals) < 2 && continue
            nums = collect(values(vals))
            if startswith(k, "meta.") && all(v -> v isa Real, nums)
                lo, hi = extrema(Float64.(nums))
                hi == lo && continue
                rel = (hi - lo) / max(abs(hi), eps())
                ev = Dict{String,Any}("pop" => pop, "variable" => k, "values" => vals, "relDiff" => round(rel; digits = 4))
                if rel <= CLAIM_REL_TOLERANCE
                    push!(findings, qc_finding("info", "claim.within_tolerance"; variable = _claim_variable_label(k),
                        detail = merge(_claim_detail("S1", ev), Dict{String,Any}("display" => true))))
                else
                    push!(findings, qc_finding("warn", "claim.differs_across_images"; variable = _claim_variable_label(k),
                        pop = _claim_pop_label(pop, scope), detail = _claim_detail("S1", ev)))
                end
                continue
            end
            g = _claim_value_groups(vals)
            length(g.groups) == 1 && continue
            all_distinct = all(v -> length(v) == 1, values(g.groups))
            ev = Dict{String,Any}("pop" => pop, "variable" => k, "byValue" => g.groups)
            if all_distinct || g.tie
                ev["split"] = true
            else
                ev["mode"] = g.mode
                ev["images"] = sort!(reduce(vcat, [v for (val, v) in g.groups if val != g.mode]; init = String[]))
            end
            # a gate is drawn per image, so a different shape is a judgement to check, not a setting to re-run
            push!(findings, qc_finding("warn", startswith(k, "gate.") ? "claim.gate_differs" : "claim.differs_across_images";
                variable = _claim_variable_label(k), pop = _claim_pop_label(pop, scope), detail = _claim_detail("S1", ev)))
        end
    end
    findings
end

# S1b — between the compared groups (here the populations): where does a processing variable differ,
# image by image, and does it split the groups perfectly?
function _claim_s1_between(rows, scope)
    findings = Any[]
    not_compared = String[]
    pops = scope.populations
    length(pops) < 2 && return findings, not_compared
    imgs = _claim_imgs(rows)
    byip = Dict((r["image"], r["pop"]) => r for r in rows)
    keys_all = sort!(unique(reduce(vcat, [collect(keys(r["vars"])) for r in rows]; init = String[])))
    for k in keys_all
        (startswith(k, "gate.") || occursin(_CLAIM_PER_POP_KEY, k)) && continue
        # the key as one population's params name it: params.<fun>.* is shared across pops by fun
        per_pop = Dict(p => Dict{String,String}() for p in pops)
        for img in imgs, p in pops
            r = get(byip, (img, p), nothing)
            (isnothing(r) || !haskey(r["vars"], k)) && continue
            per_pop[p][img] = string(r["vars"][k])
        end
        present = filter(p -> !isempty(per_pop[p]), pops)
        if length(present) < length(pops)
            length(present) >= 1 && push!(not_compared, k)
            continue
        end
        within = String[]
        for img in imgs
            vs = [get(per_pop[p], img, nothing) for p in pops]
            any(isnothing, vs) && continue
            length(unique(vs)) > 1 && push!(within, img)
        end
        sets = [Set(values(per_pop[p])) for p in pops]
        disjoint = all(isempty(intersect(sets[i], sets[j])) for i in eachindex(sets) for j in eachindex(sets) if i < j)
        (isempty(within) && !disjoint) && continue
        ev = Dict{String,Any}("variable" => k, "byPop" => per_pop, "imagesDiffering" => within,
                              "separable" => !disjoint)
        if disjoint
            push!(findings, qc_finding("warn", "claim.confounded"; variable = _claim_variable_label(k), detail = _claim_detail("S1", ev)))
        else
            push!(findings, qc_finding("warn", "claim.differs_between_groups"; variable = _claim_variable_label(k),
                n = length(within), imgs = _claim_imgs_text(within), detail = _claim_detail("S1", ev)))
        end
    end
    findings, not_compared
end

# S2 — is the claim built the way the field expects? Counts only; no test is run.

# The smallest two-sided p an exact rank test can reach with this many units: paired (signed-rank,
# every unit has both groups) 2/2^n; unpaired (Mann–Whitney, disjoint units) 2/C(n1+n2, n1). A
# design whose floor is above 0.05 can't show a difference per unit, whatever the data.
claim_min_p_paired(n::Integer) = n < 1 ? 1.0 : min(1.0, 2 / 2.0^n)
claim_min_p_unpaired(n1::Integer, n2::Integer) =
    (n1 < 1 || n2 < 1) ? 1.0 : Float64(min(1, 2 / binomial(big(n1 + n2), big(n1))))
const CLAIM_ALPHA = 0.05

# Measures read in physical units: tracks need space AND time; shape measures need space. Anything
# else (intensities, counts, ids) is calibration-free.
const _CLAIM_SPATIAL_MEASURE = r"(?i)(area|volume|surface|length|axis|diameter|perimeter|extent|distance|displacement|speed|velocity)"
function _claim_calibration_fields(measure::AbstractString)
    _claim_needs_tracking(measure) && return ("x", "y", "z", "t")
    occursin(_CLAIM_SPATIAL_MEASURE, measure) ? ("x", "y", "z") : ()
end

function _claim_s2(rows, scope)
    findings = Any[]
    used = filter(r -> r["included"] && r["hasData"], rows)
    isempty(used) && return findings

    # the statistical unit
    if isempty(scope.unit_attrs)
        push!(findings, qc_finding("warn", "claim.no_unit";
            detail = _claim_detail("S2", Dict{String,Any}("images" => _claim_imgs(used)))))
    else
        attr = _claim_unit_label(scope)
        units = Dict(p => Set(String[r["unit"] for r in used if r["pop"] == p && !isempty(r["unit"])])
                     for p in scope.populations)
        imgs_per_unit = Dict{String,Vector{String}}()
        for r in used
            isempty(r["unit"]) && continue
            v = get!(imgs_per_unit, r["unit"], String[])
            r["image"] in v || push!(v, r["image"])
        end
        foreach(sort!, values(imgs_per_unit))
        ev = Dict{String,Any}("attrs" => scope.unit_attrs, "byPop" => Dict(p => sort!(collect(u)) for (p, u) in units),
                              "imagesPerUnit" => imgs_per_unit)
        if length(scope.populations) >= 2
            shared = reduce(intersect, values(units))
            disjoint = all(isempty(intersect(units[a], units[b]))
                           for a in scope.populations for b in scope.populations if a < b)
            if disjoint && length(scope.populations) == 2
                n1, n2 = (length(units[p]) for p in scope.populations)
                ev["design"] = "unpaired"; ev["minP"] = claim_min_p_unpaired(n1, n2); n = min(n1, n2)
            else
                ev["design"] = "paired"; n = length(shared)
                ev["minP"] = claim_min_p_paired(n)
            end
        else
            n = length(first(values(units)))
            ev["design"] = "one group"; ev["minP"] = claim_min_p_paired(n)
        end
        ev["n"] = n
        pmin = round(ev["minP"]; sigdigits = 3)
        if n == 0
            # no unit filled in where the claim has data — S0's `claim.unit_blank` already says so
        elseif ev["minP"] > CLAIM_ALPHA
            push!(findings, qc_finding("warn", "claim.few_units"; attr = attr, n = n, p = pmin,
                detail = _claim_detail("S2", ev)))
        else
            push!(findings, qc_finding("info", "claim.unit_count"; attr = attr, n = n,
                detail = _claim_detail("S2", ev)))
        end
        # several images from one unit: a test per image would count that unit more than once
        multi = sort!([u for (u, is) in imgs_per_unit if length(is) > 1])
        isempty(multi) || push!(findings, qc_finding("info", "claim.images_per_unit"; attr = attr,
            n_images = length(_claim_imgs(filter(r -> !isempty(r["unit"]), used))), n = length(imgs_per_unit),
            detail = _claim_detail("S2", Dict{String,Any}("attrs" => scope.unit_attrs, "units" => multi,
                "imagesPerUnit" => imgs_per_unit))))
    end

    # physical units: a calibration problem on a field the measure is read in
    fields = _claim_calibration_fields(scope.measure)
    if !isempty(fields)
        bad = Dict{String,Vector{String}}()
        for r in used, f in fields
            for code in get(r["calibration"], f, String[])
                v = get!(bad, r["image"], String[]); code in v || push!(v, code)
            end
        end
        isempty(bad) || push!(findings, qc_finding("warn", "claim.uncalibrated"; n = length(bad),
            imgs = _claim_imgs_text(sort!(collect(keys(bad)))), measure = scope.measure, detail = _claim_detail("S2", Dict{String,Any}("measure" => scope.measure,
                "fields" => collect(fields), "images" => bad))))
    end
    findings
end

# ── the checks as a list of facts ──────────────────────────────────────────────────────────────

# Every check in reading order. What each one looks at, and the fact it states when it finds nothing,
# are copy: `claim.ok.<id>` in qc/text.jl. `S2.unit` always states a fact (the n), so it has no pass line.
const CLAIM_CHECK_IDS = ("S0.membership", "S0.data", "S0.unit", "S0.params", "S0.dims",
                         "S1.acrossImages", "S1.betweenGroups", "S2.unit", "S2.calibration")
# What each check is about, for the reader: the categories the checks are listed under, in order
# (labels in qc/text.jl, `claim.cat.<id>`). The model review uses the same ones, plus `data` (the
# measured values themselves, which no rule reads) and `other`.
const CLAIM_CATEGORIES = ("images", "record", "sameness", "replicates", "units", "data", "other")
const CLAIM_CATEGORY_OF = Dict(
    "S0.membership" => "images", "S0.data" => "images", "S0.unit" => "images",
    "S0.params" => "record", "S0.dims" => "record",
    "S1.acrossImages" => "sameness", "S1.betweenGroups" => "sameness",
    "S2.unit" => "replicates", "S2.calibration" => "units")
# The check each finding comes from.
const CLAIM_CHECK_OF = Dict(
    "claim.image_excluded" => "S0.membership", "claim.no_data" => "S0.data", "claim.unit_blank" => "S0.unit",
    "claim.params_not_recorded" => "S0.params", "claim.params_last_run_only" => "S0.params",
    "claim.dims_not_recorded" => "S0.dims",
    "claim.differs_across_images" => "S1.acrossImages", "claim.within_tolerance" => "S1.acrossImages",
    "claim.gate_differs" => "S1.acrossImages",
    "claim.differs_between_groups" => "S1.betweenGroups", "claim.confounded" => "S1.betweenGroups",
    "claim.not_compared" => "S1.betweenGroups",
    "claim.no_unit" => "S2.unit", "claim.few_units" => "S2.unit", "claim.unit_count" => "S2.unit",
    "claim.images_per_unit" => "S2.unit", "claim.uncalibrated" => "S2.calibration")

# The same finding for several populations is ONE fact ("No B and T data on fHqhyb"): merged when
# its code, its other subjects and its images agree — across-image differences by variable alone,
# since each population's values differ in their own way. Also folds exact repeats (one per pop).
function _claim_merge_pops(findings)
    groups = Dict{Any,Vector{Any}}(); order = Any[]
    for f in findings
        subs = get(f, "subs", Dict{String,Any}())
        ev = get(get(f, "detail", Dict()), "evidence", Dict())
        imgs = f["code"] in ("claim.differs_across_images", "claim.gate_differs") ? nothing : get(ev, "images", nothing)
        k = (f["code"], f["level"], sort!([kv for kv in subs if kv[1] != "pop"]; by = first), imgs)
        haskey(groups, k) || push!(order, k)
        push!(get!(groups, k, Any[]), f)
    end
    out = Any[]
    for k in order
        fs = groups[k]
        pops = unique(String[string(f["subs"]["pop"]) for f in fs if haskey(get(f, "subs", Dict()), "pop")])
        if length(fs) == 1 || length(pops) < 2
            push!(out, fs[1]); continue
        end
        f1 = fs[1]
        subs = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in f1["subs"])
        subs[:pop] = join(pops, " and ")
        d = copy(f1["detail"])
        evs = [f["detail"]["evidence"] for f in fs]
        # what every population's evidence agrees on stays at the top; the rest is per population
        common = Dict{String,Any}(k => v for (k, v) in evs[1] if k != "pop" && all(e -> get(e, k, nothing) == v, evs))
        d["evidence"] = merge(common, Dict{String,Any}("pops" => pops, "byPop" => Dict(zip(pops, evs))))
        push!(out, qc_finding(f1["level"], f1["code"]; detail = d, subs...))
    end
    for f in out
        f["detail"]["check"] = get(CLAIM_CHECK_OF, f["code"], "")
    end
    out
end

"""
    claim_check_lines(result) -> Vector{Dict}

The checks as a list of facts, in reading order: `{id, state, text, tip}`. `state` is `warn` (its
warnings are the findings with `detail.check == id`), `ok` (`text` is the fact it confirmed) or
`skipped` (`text` says why). The plot's popover and the claim entry both read this.
"""
function claim_check_lines(result::AbstractDict)
    lines = Dict{String,Any}[]
    for id in CLAIM_CHECK_IDS
        fs = filter(f -> get(f["detail"], "check", "") == id, result["findings"])
        warn = any(f -> f["level"] == "warn", fs)
        t = qc_text("claim.ok." * id)
        state, text = if warn
            "warn", ""
        elseif haskey(result["skipped"], id)
            "skipped", string(result["skipped"][id])
        elseif id in result["ran"]
            # a check that always states a fact (the n per replicate) says it in its info finding
            info = filter(f -> f["level"] == "info", fs)
            "ok", (id == "S2.unit" && !isempty(info)) ? info[1]["short"] : t.short
        else
            continue
        end
        push!(lines, Dict{String,Any}("id" => id, "category" => CLAIM_CATEGORY_OF[id], "state" => state,
                                      "text" => text, "tip" => t.long))
    end
    lines
end

"""
    claim_categories(lines) -> Vector{Dict}

The categories the check lines fall in, in reading order: `{id, label, tip, state}` — `warn` if any of
its checks found a problem, `ok` if one ran and none did, else `skipped`.
"""
function claim_categories(lines::AbstractVector)
    out = Dict{String,Any}[]
    for cat in CLAIM_CATEGORIES
        ls = filter(l -> l["category"] == cat, lines)
        isempty(ls) && continue
        st = any(l -> l["state"] == "warn", ls) ? "warn" : any(l -> l["state"] == "ok", ls) ? "ok" : "skipped"
        t = qc_text("claim.cat." * cat)
        push!(out, Dict{String,Any}("id" => cat, "label" => t.short, "tip" => t.long, "state" => st))
    end
    out
end

"""
    claim_checks(table, scope) -> Dict

PURE. S0 over every row; S1 and S2 over the rows the claim runs on (included, with data). Returns
`{findings, ran, skipped, notCompared, nRows, nUsed}`: `ran`/`skipped` name the checks so a quiet
result can't read as a full pass, and `notCompared` lists the variables S1 couldn't compare between
groups because one group has no record of them (an S0 gap, not a clean bill).
"""
function claim_checks(table::AbstractVector, scope::ClaimScope)::Dict{String,Any}
    ran = String["S0.membership", "S0.data"]
    skipped = Dict{String,String}()
    isempty(scope.unit_attrs) ? (skipped["S0.unit"] = "no replicate attribute given") : push!(ran, "S0.unit")
    push!(ran, "S0.params")
    _claim_needs_tracking(scope.measure) ? push!(ran, "S0.dims") : (skipped["S0.dims"] = "not a tracking measure")
    findings = _claim_s0(table, scope)
    used = filter(r -> r["included"] && r["hasData"], table)
    if length(_claim_imgs(used)) < 2
        skipped["S1.acrossImages"] = "fewer than 2 images with data"
    else
        push!(ran, "S1.acrossImages")
        append!(findings, _claim_s1_across(used, scope))
    end
    not_compared = String[]
    if length(scope.populations) < 2
        skipped["S1.betweenGroups"] = "one population — nothing to compare"
    elseif isempty(used)
        skipped["S1.betweenGroups"] = "no images with data"
    else
        push!(ran, "S1.betweenGroups")
        f, not_compared = _claim_s1_between(used, scope)
        append!(findings, f)
        if !isempty(not_compared)
            by = Dict{String,Vector{String}}()
            for k in not_compared
                fun = _claim_param_fun(k)
                push!(get!(by, isnothing(fun) ? k : "params." * fun, String[]), k)
            end
            # one note: how many settings, from which steps
            push!(findings, qc_finding("info", "claim.not_compared"; n = length(not_compared),
                variable = join(sort!(collect(keys(by))), ", "),
                detail = _claim_detail("S1", Dict{String,Any}("bySource" => Dict(k => sort!(v) for (k, v) in by)))))
        end
    end
    # S2 — conventions, over the rows the claim runs on
    if isempty(used)
        skipped["S2"] = "no images with data"
    else
        isempty(scope.unit_attrs) || push!(ran, "S2.unit")
        isempty(_claim_calibration_fields(scope.measure)) ? (skipped["S2.calibration"] = "measure needs no calibration") :
            push!(ran, "S2.calibration")
        append!(findings, _claim_s2(table, scope))
    end
    out = Dict{String,Any}("findings" => _claim_merge_pops(findings), "ran" => ran, "skipped" => skipped,
                           "notCompared" => not_compared, "nRows" => length(table), "nUsed" => length(used))
    out["checks"] = claim_check_lines(out)
    out["categories"] = claim_categories(out["checks"])
    out
end

"""
    claim_checks(scope) -> Dict

Read the record (`claim_record_table`) and run the rules. Adds `rows` (the record table) to the
result. Read-only: writes nothing.
"""
function claim_checks(scope::ClaimScope)::Dict{String,Any}
    table = claim_record_table(scope)
    out = claim_checks(table, scope)
    out["rows"] = table
    out
end
