# Gating QC — the hand-drawn gates (`flow`, `track`) checked across a set's images.
#
# Gating has no task run to hang QC on: a gate is drawn on one image, often copied to the rest
# (`copy_gating!`), and the data under it can change later (re-segmentation, re-tracking). What goes
# wrong silently is that a gate stops selecting what it selected where it was drawn, on SOME images:
#   - it reads a column this image's table lacks → the population and everything under it are empty
#     (`recompute!` degrades it to no members + a log warning, never an error);
#   - it is empty here while the same population has cells on other images (a copied gate that misses
#     this image's shifted population);
#   - its share of its parent is a cohort outlier against the other images.
# So this is a SET check, like cohort QC (`qc_cohort.jl`), and runs where cohort QC runs: the "Check
# cohort consistency" button (fun `gating.flow` / `gating.track`, `cohort_check!`) and the end of every
# chain run (`chain_cohort_qc!`). Findings are banked per image as `qc/gating.{pop_type}/{vn}.json`, so
# they show wherever per-image QC shows (image table, briefing, MCP, lab log). Advisory only.
#
# The outlier test reuses the cohort thresholds (`_COHORT_MODZ_THRESHOLD`, `_COHORT_MIN_N`,
# `_COHORT_MAD0_REL`) — no gating-specific constants. A GUI gate edit does not re-run the check; the
# docs stay as of the last check, the same as cohort findings (docs/POPULATION.md → *Gating QC*).

gating_qc_fun(pop_type::AbstractString)::String = "gating." * String(pop_type)
is_gating_qc_fun(fun_name::AbstractString)::Bool =
    startswith(fun_name, "gating.") && fun_name[length("gating.")+1:end] in GATING_POP_TYPES
gating_qc_pop_type(fun_name::AbstractString)::String = String(fun_name[length("gating.")+1:end])

"""
    gating_value_names(img, pop_type) -> Vector{String}

The segmentations `img` has a stored `pop_type` map for — `img_value_names(img)` with the map's file
present (`gating_path`), the idiom the other gating-map readers use (`ai/observer_summary.jl`).
"""
gating_value_names(img::CciaImage, pop_type::AbstractString)::Vector{String} =
    sort!(String[v for v in img_value_names(img) if isfile(gating_path(img._dir, v; pop_type = String(pop_type)))])

"""True when some included image of `set` has a stored `pop_type` gating map."""
set_has_gating(set::CciaSet, pop_type::AbstractString)::Bool =
    any(img -> !isempty(gating_value_names(img, pop_type)), filter(image_included, images(set)))

# One image's map on one segmentation, evaluated: per population its count and its parent's count,
# and the gate columns its table lacks. `nothing` when it has no populations.
function _gating_image_state(img::CciaImage, vn::AbstractString, pop_type::AbstractString)
    m = computed_pop_map(img; value_name = vn, pop_type = pop_type)
    isempty(m.order) && return nothing
    counts = Dict{String,Tuple{Int,Int}}()
    for path in m.order
        s = pop_stats(m, path)
        counts[path] = (s.count, s.parent_count)
    end
    (; counts, missing = missing_gate_columns(m, img; value_name = vn))
end

"""
    gating_qc_findings(states; threshold) -> Dict{uid => (findings, metrics, flags)}

PURE. `states` maps an image uid to `(; counts = Dict(path => (count, parent_count)), missing =
[(column, pops)])` — one segmentation's gating on each image. Per image:
- `gating.missing_column` — one per column the table lacks; the populations it empties are named.
- `gating.empty_pop` — a population with no cells here whose parent has cells here, while it has
  cells on at least one other image (pops emptied by a missing column are not repeated).
- `gating.pct_outlier` — its % of parent is a cohort outlier (`_cohort_outliers`) against the
  images where the parent has cells.
`metrics` = `nPops`, `nEmptyPops`, `nMissingColumns`; `flags` = `code => [pops]` for the set summary.
"""
function gating_qc_findings(states::AbstractDict; threshold::Real = _COHORT_MODZ_THRESHOLD)
    out = Dict{String,Any}()
    paths = unique(String[p for st in values(states) for p in keys(st.counts)])
    # the pops each image's missing columns empty — already explained, so kept out of the comparisons
    emptied_by = Dict{String,Set{String}}(u => Set{String}(p for r in st.missing for p in r.pops)
                                          for (u, st) in states)
    # pct of parent per population, over the images where its parent has cells — the cohort test
    pct_out = Dict{String,Any}()
    for p in paths
        vals = Dict{String,Float64}(u => 100 * st.counts[p][1] / st.counts[p][2]
                                    for (u, st) in states
                                    if haskey(st.counts, p) && st.counts[p][2] > 0 && !(p in emptied_by[u]))
        pct_out[p] = _cohort_outliers(vals, threshold)
    end
    for (uid, st) in states
        findings = Any[]; flags = Dict{String,Vector{String}}()
        emptied = emptied_by[uid]
        for r in st.missing
            push!(findings, qc_finding("warn", "gating.missing_column"; column = r.column,
                  n = length(r.pops), detail = Dict{String,Any}("column" => r.column, "pops" => r.pops)))
            append!(get!(flags, "missingColumn", String[]), r.pops)
        end
        for p in sort!(collect(keys(st.counts)))
            p in emptied && continue
            cnt, parent = st.counts[p]
            if cnt == 0 && parent > 0
                elsewhere = [u for (u, o) in states if u != uid && haskey(o.counts, p) && o.counts[p][1] > 0 && !(p in emptied_by[u])]
                isempty(elsewhere) && continue
                push!(findings, qc_finding("warn", "gating.empty_pop"; pop = p,
                      detail = Dict{String,Any}("pop" => p, "parentCount" => parent,
                                                "nImagesWithCells" => length(elsewhere))))
                push!(get!(flags, "emptyPop", String[]), p)
                continue
            end
            o = get(pct_out[p].outliers, uid, nothing)
            o === nothing && continue
            med = pct_out[p].median
            val = round(o["value"]; digits = 1)
            push!(findings, qc_finding("warn", "gating.pct_outlier"; pop = p, value = val,
                  median = round(med; digits = 1), dir = val < med ? "below" : "above",
                  detail = merge(Dict{String,Any}("pop" => p, "count" => cnt, "parentCount" => parent,
                                                  "median" => med), o)))
            push!(get!(flags, "pctOutlier", String[]), p)
        end
        metrics = Dict{String,Any}("nPops" => length(st.counts),
                                   "nEmptyPops" => count(c -> c[1] == 0, values(st.counts)),
                                   "nMissingColumns" => length(st.missing))
        out[String(uid)] = (; findings, metrics, flags)
    end
    out
end

"""
    gating_qc!(set, pop_type; value_name="", threshold, write=true) -> Dict

Evaluate every included image's `pop_type` gating per segmentation (all value_names with a stored map,
or just `value_name`), run `gating_qc_findings`, and — when `write` — bank each image's findings +
metrics as `qc/gating.{pop_type}/{vn}.json`. An image whose map is gone but which still has a doc gets
an empty one, so a stale warning clears (also when the map is gone from every image); an image whose
map fails to evaluate keeps its last doc. Returns the cohort reply shape
`{funName, valueNames, byValueName: vn => {funName, valueName, nIncluded, metrics: {code => {outliers:
{uid => {pops}}}}}}`, so the check button, `cohort_has_outliers` and the lab log read it unchanged. An
image whose map fails to evaluate is skipped with a warning, never failing the set.
"""
function gating_qc!(set::CciaSet, pop_type::AbstractString; value_name::AbstractString = "",
                    threshold::Real = _COHORT_MODZ_THRESHOLD, write::Bool = true)::Dict{String,Any}
    pt = String(pop_type)
    fun = gating_qc_fun(pt)
    imgs = filter(image_included, images(set))
    # every segmentation with a map — or with a doc from an earlier check, so a map deleted on every
    # image still gets its old warnings cleared
    vns = isempty(value_name) ?
        sort!(unique(vcat(String[v for img in imgs for v in gating_value_names(img, pt)],
                          cohort_value_names(set, fun)))) :
        [String(value_name)]
    byvn = Dict{String,Any}()
    for vn in vns
        states = Dict{String,Any}()
        failed = Set{String}()
        for img in imgs
            vn in gating_value_names(img, pt) || continue
            st = try
                _gating_image_state(img, vn, pt)
            catch e
                @warn "gating QC: could not evaluate $(pt) gating '$vn' on $(img.uid)" exception = e
                push!(failed, img.uid); nothing
            end
            st === nothing || (states[img.uid] = st)
        end
        res = gating_qc_findings(states; threshold)
        if write
            for img in imgs
                # not evaluated this time: its last doc stands — "couldn't check" is not "clean"
                img.uid in failed && continue
                r = get(res, img.uid, nothing)
                if r === nothing
                    isfile(qc_path(img, fun, vn)) && write_qc(img, fun, vn, Any[])
                else
                    write_qc(img, fun, vn, r.findings; metrics = r.metrics,
                             source = Dict{String,Any}("gating" => basename(gating_path(img._dir, vn; pop_type = pt))))
                end
            end
        end
        metrics = Dict{String,Any}()
        for (uid, r) in res, (code, pops) in r.flags
            m = get!(metrics, code, Dict{String,Any}("outliers" => Dict{String,Any}()))
            m["outliers"][uid] = Dict{String,Any}("pops" => pops)
        end
        byvn[vn] = Dict{String,Any}("funName" => fun, "valueName" => vn, "nIncluded" => length(states),
                                    "metrics" => metrics)
    end
    Dict{String,Any}("funName" => fun, "valueNames" => sort(collect(keys(byvn))), "byValueName" => byvn)
end

# Lab-log lines for one gating doc (PURE): a headline, then one line per flagged image and kind.
# Images by uid, as `cohort_qc_summary_lines` does.
const _GATING_FLAG_LABEL = Dict("missingColumn" => "gate column missing",
                                "emptyPop" => "empty here, not on other images",
                                "pctOutlier" => "% of parent far from the set")
function gating_qc_summary_lines(doc::AbstractDict)::Vector{String}
    fun = string(get(doc, "funName", "gating")); vn = string(get(doc, "valueName", ""))
    head = isempty(vn) ? fun : "$fun ($vn)"
    n = get(doc, "nIncluded", 0)
    lines = String[]
    for (code, m) in get(doc, "metrics", Dict())
        (m isa AbstractDict) || continue
        for (uid, e) in get(m, "outliers", Dict())
            pops = e isa AbstractDict ? get(e, "pops", String[]) : String[]
            push!(lines, "  $(uid) — $(get(_GATING_FLAG_LABEL, string(code), string(code))): $(join(pops, ", "))")
        end
    end
    isempty(lines) && return [severity_symbol("ok") * " $(head): gates fit all $(n) image(s)"]
    nimg = length(unique(first(split(strip(l), " — ")) for l in lines))
    vcat([severity_symbol("warn") * " $(head): $(nimg) image(s) flagged across $(n)"], sort(lines))
end
