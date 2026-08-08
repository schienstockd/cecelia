# Analysis lineage — the synthesized pipeline story for the read-only observer (Slice A of
# docs/todo/OBSERVER_DATA_ACCESS_PLAN.md). The observer could see QC/run-logs/meta but not HOW an
# image's data was produced (denoise → segment → gate → track → cluster → plotted), so every session
# the user re-explained the workflow. This assembles that story from data we already have — the run
# log, the value_name/suffix naming links, gating sidecars, chain templates, and board tabs — as a
# compact, summary-level payload (names + counts + order; never raw cell/track rows).
#
# READ-ONLY: reads existing sidecars, writes nothing. Served via GET /api/analysis/lineage and the MCP
# `get_analysis_lineage` tool. Richer per-stage numbers (populations, measures, HMM/clusters) are the
# later slices B–E.

# fun_name ("category.task") → a coarse pipeline stage. Gating is interactive (not a task fun), so it
# has no run-log step — it surfaces via `gatedPops`, not here.
function _stage_of(fun::AbstractString)::String
    cat = String(first(split(fun, "."; limit = 2)))
    cat == "importImages"  && return "import"
    cat == "cleanupImages" && return "cleanup"
    cat == "editImages"    && return "edit"
    cat == "segment"       && return "segment"
    cat == "tracking"      && return "track"
    cat == "behaviour"     && return "behaviour"
    (cat == "clustPops" || cat == "clustTracks") && return "cluster"
    "other"
end

# Canonical pipeline order for rolling stages up; unknown stages sort after, alphabetically.
const _LINEAGE_STAGE_ORDER = ["import", "cleanup", "edit", "segment", "track", "gate", "cluster", "behaviour", "other"]
_stage_order(stages) = vcat([s for s in _LINEAGE_STAGE_ORDER if s in stages],
                            sort([s for s in stages if !(s in _LINEAGE_STAGE_ORDER)]))

# Ordered run-log steps for an image → the task order + outcome + which value_name each wrote. Legacy
# entries lack `status` (treated "done").
_run_log_steps(img::CciaImage) =
    [(; stage     = _stage_of(string(get(e, "fun", ""))),
        fun       = string(get(e, "fun", "")),
        valueName = string(get(e, "valueName", "")),
        status    = string(get(e, "status", "done")),
        at        = string(get(e, "at", "")))
     for e in read_run_log(img)]

# Clustering runs banked on an image → suffix ⇒ the value_names (label sets) it clustered, read from
# the per-cell and per-track `.clustfeatures.json` sidecars (the same source the cluster pages use).
function _image_cluster_runs(img::CciaImage, segs::AbstractVector)
    acc = Dict{String,Set{String}}()
    for v in segs, p in (img_label_props_path(img, v), img_track_props_path(img, v))
        isfile(p) || continue
        # `clusters` family only — region-clustering runs are reported by the spatial summary
        # (`ai/spatial.jl` regionRuns), so they must not double-report here as cluster runs.
        for suf in _clustfeatures_suffixes(p; family="clusters")
            push!(get!(acc, suf, Set{String}()), v)
        end
    end
    [(; suffix = s, valueNames = sort(collect(vs))) for (s, vs) in sort(collect(acc); by = first)]
end

# Gate-defined populations per segmentation (the "gated into CD3/CD8" story). Names + counts only —
# full gate geometry is Slice B (`get_populations`). Flow + track pop maps only; cluster pops
# (clust/trackclust) are the clustering story, covered by `clusterRuns`.
# Cap the pop-name list per (value_name, pop_type) — the plan mandates hard caps on every list. `n`
# carries the TRUE count, so a truncated `pops` is never silent.
const _LINEAGE_POP_CAP = 100
function _image_gated_pops(img::CciaImage, segs::AbstractVector)
    out = Vector{Any}()
    for v in segs, pt in ("flow", "track")
        isfile(gating_path(img._dir, v; pop_type = pt)) || continue
        ps = pop_paths(load_pop_map(img; value_name = v, pop_type = pt))
        isempty(ps) && continue
        push!(out, (; valueName = v, popType = pt, n = length(ps), pops = ps[1:min(end, _LINEAGE_POP_CAP)]))
    end
    out
end

function _image_lineage(img::CciaImage)
    segs    = sort(img_value_names(img))                                      # segmentations (label_props keys), stable order
    tracked = String[v for v in segs if isfile(img_track_props_path(img, v))]  # those with a __tracks table
    (; _observer_image_header(img)...,
       steps         = _run_log_steps(img),
       segmentations = segs,
       tracked       = tracked,
       clusterRuns   = _image_cluster_runs(img, segs),
       gatedPops     = _image_gated_pops(img, segs))
end

# Whiteboard chain templates wired for the project → name + the distinct task funs each wires (so the
# observer can tell "these steps were pipelined" from ad-hoc runs). Non-.json entries (the runs/ and
# .cache/ subdirs) are skipped.
function _chain_summaries(proj::CciaProject)
    dir = _chains_dir(proj); isdir(dir) || return Any[]
    out = Any[]
    for f in sort(readdir(dir))
        endswith(f, ".json") || continue
        t = try load_chain_template(proj, f[1:end-5]) catch; continue end
        push!(out, (; name = t.name, tasks = unique(String[n.fn for n in t.nodes])))
    end
    out
end

# Analysis-board tab names (best-effort). `board_summaries` below reads the plot semantics too — this
# stays the cheap name-only view lineage embeds. Any missing/renamed field just yields fewer names.
function _board_tabs(proj::CciaProject)
    p = joinpath(proj.root, "settings", "analysisBoards.json")
    isfile(p) || return String[]
    b = try JSON3.read(read(p, String)) catch; return String[] end
    tabs = get(b, :tabs, nothing); tabs isa AbstractVector || return String[]
    names = String[]
    for t in tabs
        t isa AbstractDict || continue
        nm = string(get(t, :name, get(t, :label, get(t, :title, get(t, :id, "")))))
        isempty(nm) || push!(names, nm)
    end
    names
end


# ── Analysis-board READ-BACK ────────────────────────────────────────────────────────────────────────
# What each board actually SHOWS, so the observer can see the boards a project already has instead of
# proposing a duplicate. Deliberately a SUMMARY, not the stored layout: `analysisBoards.json` holds
# `slotAreas`, `gridArea` strings, `vis` bags and `tkey`-encoded selections, which say nothing readable
# about the figure. Read side and write side share one vocabulary on purpose — this returns roughly what
# the add-a-board tool will accept (docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Decision 2).
#
# The file is written by the frontend and read here, so EVERY field is optional by construction: a board
# from an older schema, or one mid-migration, must degrade to fewer fields rather than throw.

# `tkey` (frontend plots/series.ts): "popType::valueName/pop" — split on the first "::" then the first
# "/". pop_types and value_names contain no "::", and a pop path always starts with "/".
function _parse_tkey(k::AbstractString)
    c = findfirst("::", k)
    pop_type, rest = c === nothing ? ("live", String(k)) : (String(k[1:first(c)-1]), String(k[last(c)+1:end]))
    i = findfirst('/', rest)
    i === nothing ? (; popType = pop_type, valueName = rest, pop = "") :
                    (; popType = pop_type, valueName = String(rest[1:i-1]), pop = String(rest[i:end]))
end

_nonempty(x) = x !== nothing && !(x isa AbstractString && isempty(x))

# One filled slot → what it plots. `kind` "summary" carries a plot-spec id in `ref` and its choices in
# `state` (measure / chartType / popType / the eye-selected series); "interactive" carries a view key.
function _board_slot(i::Int, c)
    c isa AbstractDict || return nothing
    st = get(c, :state, Dict{Symbol,Any}())
    st isa AbstractDict || (st = Dict{Symbol,Any}())
    sel = get(st, :sel, nothing)
    pops = sel isa AbstractVector ?
        [let t = _parse_tkey(string(k)); "$(t.valueName)$(t.pop)" end for k in sel] : String[]
    out = Dict{String,Any}("slot" => i, "kind" => string(get(c, :kind, "")), "ref" => string(get(c, :ref, "")))
    for (key, field) in ("title" => :title, "measure" => :measure, "chart" => :chartType,
                         "popType" => :popType, "groupBy" => :groupBy)
        v = get(st, field, nothing)
        _nonempty(v) && (out[key] = string(v))
    end
    isempty(pops) || (out["pops"] = pops)
    out
end

"""
    board_summaries(proj) -> Vector

Every Analysis board in the project and what it shows: `[{name, cols, rows, plots: [{slot, kind, ref,
measure?, chart?, popType?, pops?, title?}]}]`. Empty slots are omitted. Read-only, summary-level —
never the stored layout geometry. Returns `[]` when the project has no boards.
"""
function board_summaries(proj::CciaProject)
    p = joinpath(proj.root, "settings", "analysisBoards.json")
    isfile(p) || return Any[]
    b = try JSON3.read(read(p, String)) catch; return Any[] end
    tabs = get(b, :tabs, nothing)
    layouts = get(b, :layouts, nothing)
    layouts isa AbstractDict || (layouts = Dict{Symbol,Any}())
    # tab order comes from `tabs.tabs`; the layout for tab <id> is keyed "tab:<id>" (project-relative,
    # see frontend utils/boardKeys). A tab with no layout yet is a real state — report it with no plots.
    tab_list = tabs isa AbstractDict ? get(tabs, :tabs, nothing) : nothing
    entries = tab_list isa AbstractVector ? tab_list : Any[]
    out = Any[]
    for t in entries
        t isa AbstractDict || continue
        id = get(t, :id, nothing)
        lay = id === nothing ? nothing : get(layouts, Symbol("tab:$(id)"), nothing)
        plots = Any[]
        cols = rows = 0
        if lay isa AbstractDict
            cols = something(tryparse(Int, string(get(lay, :cols, 0))), 0)
            rows = something(tryparse(Int, string(get(lay, :rows, 0))), 0)
            contents = get(lay, :contents, nothing)
            if contents isa AbstractVector
                for (i, c) in enumerate(contents)
                    s = _board_slot(i - 1, c)
                    s === nothing || push!(plots, s)
                end
            end
        end
        push!(out, Dict{String,Any}("name" => string(get(t, :name, get(t, :id, "?"))),
                                    "cols" => cols, "rows" => rows, "plots" => plots))
    end
    out
end

# The stages an image reached — from run-log steps UNION artifact evidence. A stage counts as present
# if a dated step OR a produced artifact shows it (segmentations→segment, tracked→track, clusterRuns→
# cluster, gatedPops→gate). This matters because the run log is a recent, capped window: a segmentation
# or tracking that predates it leaves no step but its artifacts persist, so a step-only rollup would
# false-flag "missing segment/track" on images that plainly have them.
function _image_stages(e)
    st = String[s.stage for s in e.steps]
    isempty(e.segmentations) || push!(st, "segment")
    isempty(e.tracked)       || push!(st, "track")
    isempty(e.clusterRuns)   || push!(st, "cluster")
    isempty(e.gatedPops)     || push!(st, "gate")
    unique(st)
end

# Set-level roll-up: the common pipeline (every stage anyone reached, in canonical order) and where
# images diverge from it — an image missing a stage the others reached, or one that's excluded. Ties to #9.
function _lineage_rollup(entries::AbstractVector)
    isempty(entries) && return (; pipeline = String[], divergences = Any[])
    seqs = Dict(e.uid => _image_stages(e) for e in entries)
    pipeline = _stage_order(unique(vcat(values(seqs)...)))
    divergences = Any[]
    for e in entries
        miss = [st for st in pipeline if !(st in seqs[e.uid])]
        (!isempty(miss) || e.included == false) &&
            push!(divergences, (; uid = e.uid, name = e.name, included = e.included, missingStages = miss))
    end
    (; pipeline = pipeline, divergences = divergences)
end

"""
    analysis_lineage(proj; image_uid="", set_uid="") -> NamedTuple

Synthesize the analysis lineage for a project (optionally scoped to one `image_uid` or `set_uid`): per
image the ordered pipeline `steps`, its `segmentations`/`tracked`/`clusterRuns`/`gatedPops` links, plus
project-level `chains` (wired templates) and `boards` (tab names) and a `rollup` (common pipeline +
divergences). Summary-level only (names/counts/order), read-only. See OBSERVER_DATA_ACCESS_PLAN.md.
"""
function analysis_lineage(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "")
    base = observer_image_summary(proj, _image_lineage; image_uid = image_uid, set_uid = set_uid)
    (; base...,                                # projectUid, images
       chains = _chain_summaries(proj),
       boards = _board_tabs(proj),
       rollup = _lineage_rollup(base.images))
end
