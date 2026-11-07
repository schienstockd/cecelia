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
    ClaimScope(set, populations, measure; unit_attr = "")

What a claim is about: the set, the populations it compares (`"B/qc"`: segmentation `B`, gated
population `qc`), the measure (`"live.track.speed"`) and the attribute that names the statistical
unit (`"Mouse"`). No claim text — parsing a sentence into this is the Translator's job.
"""
struct ClaimScope
    set::CciaSet
    populations::Vector{String}
    measure::String
    unit_attr::String
end
ClaimScope(set::CciaSet, populations::AbstractVector, measure::AbstractString; unit_attr::AbstractString = "") =
    ClaimScope(set, String.(populations), String(measure), String(unit_attr))

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

# The name a task run's params belong to (`task_params_name`): its output, or for tracking the
# segmentation it writes into.
function _claim_run_output(fun::AbstractString, params::AbstractDict)::String
    p = Dict{String,Any}(String(k) => v for (k, v) in params)
    try task_params_name(String(fun), p) catch; "" end
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
`included`, `hasData`, `unit` (the unit attribute's value), `paramsSource` (`kind => fun => source`)
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
            "unit" => isempty(scope.unit_attr) ? "" : string(get(img.attr, scope.unit_attr, "")),
            "paramsSource" => sources, "vars" => vars))
    end
    rows
end

# ── the rules (pure) ─────────────────────────────────────────────────────────────────────────────

_claim_detail(stage, evidence) = Dict{String,Any}("stage" => stage, "provenance" => "inferred", "evidence" => evidence)
_claim_imgs(rows) = sort!(unique(String[r["image"] for r in rows]))

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
    isempty(excl) || push!(findings, qc_finding("warn", "claim.image_excluded"; n = length(excl),
        detail = _claim_detail("S0", Dict{String,Any}("images" => excl))))

    for pop in scope.populations
        nodata = _claim_imgs(filter(r -> r["pop"] == pop && !r["hasData"], rows))
        isempty(nodata) || push!(findings, qc_finding("warn", "claim.no_data"; pop = pop, n = length(nodata),
            detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "images" => nodata,
                "excluded" => filter(i -> i in excl, nodata), "measure" => scope.measure))))
    end

    if !isempty(scope.unit_attr)
        blank = _claim_imgs(filter(r -> isempty(r["unit"]), rows))
        isempty(blank) || push!(findings, qc_finding("warn", "claim.unit_blank"; attr = scope.unit_attr,
            n = length(blank), detail = _claim_detail("S0", Dict{String,Any}("attr" => scope.unit_attr,
                "images" => blank, "excluded" => filter(i -> i in excl, blank)))))
    end

    # params: only where the claim has data — an image with no output has nothing to have params for
    for pop in scope.populations, kind in _claim_kinds(scope.measure)
        rs = filter(r -> r["pop"] == pop && r["hasData"], rows)
        missing_imgs = _claim_imgs(filter(r -> isempty(r["paramsSource"][kind]), rs))
        flat_only = _claim_imgs(filter(r -> !isempty(r["paramsSource"][kind]) &&
                                            all(==("flat"), values(r["paramsSource"][kind])), rs))
        isempty(missing_imgs) || push!(findings, qc_finding("warn", "claim.params_not_recorded";
            kind = kind, pop = pop, n = length(missing_imgs),
            detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "kind" => kind, "images" => missing_imgs))))
        isempty(flat_only) || push!(findings, qc_finding("info", "claim.params_last_run_only";
            kind = kind, pop = pop, n = length(flat_only),
            detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "kind" => kind, "images" => flat_only))))
    end

    if _claim_needs_tracking(scope.measure)
        for pop in scope.populations
            nodims = _claim_imgs(filter(r -> r["pop"] == pop && r["hasData"] && !haskey(r["vars"], "motionDims"), rows))
            isempty(nodims) || push!(findings, qc_finding("warn", "claim.dims_not_recorded"; pop = pop,
                n = length(nodims), detail = _claim_detail("S0", Dict{String,Any}("pop" => pop, "images" => nodims))))
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
                    push!(findings, qc_finding("info", "claim.within_tolerance"; variable = k,
                        detail = merge(_claim_detail("S1", ev), Dict{String,Any}("display" => true))))
                else
                    push!(findings, qc_finding("warn", "claim.differs_across_images"; variable = k, pop = pop,
                        detail = _claim_detail("S1", ev)))
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
            push!(findings, qc_finding("warn", "claim.differs_across_images"; variable = k, pop = pop,
                detail = _claim_detail("S1", ev)))
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
            push!(findings, qc_finding("warn", "claim.confounded"; variable = k, detail = _claim_detail("S1", ev)))
        else
            push!(findings, qc_finding("warn", "claim.differs_between_groups"; variable = k,
                n = length(within), detail = _claim_detail("S1", ev)))
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
    if isempty(scope.unit_attr)
        push!(findings, qc_finding("warn", "claim.no_unit";
            detail = _claim_detail("S2", Dict{String,Any}("images" => _claim_imgs(used)))))
    else
        attr = scope.unit_attr
        units = Dict(p => Set(String[r["unit"] for r in used if r["pop"] == p && !isempty(r["unit"])])
                     for p in scope.populations)
        imgs_per_unit = Dict{String,Vector{String}}()
        for r in used
            isempty(r["unit"]) && continue
            v = get!(imgs_per_unit, r["unit"], String[])
            r["image"] in v || push!(v, r["image"])
        end
        foreach(sort!, values(imgs_per_unit))
        ev = Dict{String,Any}("attr" => attr, "byPop" => Dict(p => sort!(collect(u)) for (p, u) in units),
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
        pmin = round(ev["minP"]; sigdigits = 2)
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
            detail = _claim_detail("S2", Dict{String,Any}("attr" => attr, "units" => multi,
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
            measure = scope.measure, detail = _claim_detail("S2", Dict{String,Any}("measure" => scope.measure,
                "fields" => collect(fields), "images" => bad))))
    end
    findings
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
    isempty(scope.unit_attr) ? (skipped["S0.unit"] = "no unit attribute given") : push!(ran, "S0.unit")
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
            for (what, ks) in sort!(collect(by); by = first)
                push!(findings, qc_finding("info", "claim.not_compared"; variable = what, n = length(ks),
                    detail = _claim_detail("S1", Dict{String,Any}("variable" => what, "settings" => sort!(ks)))))
            end
        end
    end
    # S2 — conventions, over the rows the claim runs on
    if isempty(used)
        skipped["S2"] = "no images with data"
    else
        isempty(scope.unit_attr) || push!(ran, "S2.unit")
        isempty(_claim_calibration_fields(scope.measure)) ? (skipped["S2.calibration"] = "measure needs no calibration") :
            push!(ran, "S2.calibration")
        append!(findings, _claim_s2(table, scope))
    end
    Dict{String,Any}("findings" => findings, "ran" => ran, "skipped" => skipped,
                     "notCompared" => not_compared, "nRows" => length(table), "nUsed" => length(used))
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
