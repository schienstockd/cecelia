# Phase 2 of docs/todo/MCP_BOARD_AUTHORING_PLAN.md — the SEMANTIC board spec and its expander.
#
# Claude sends what a board should SHOW; this turns it into the `LayoutEntry` the /analysis canvas
# renders, and refuses anything the project cannot actually plot. One call, server-side, exactly as
# `create_chain` takes nodes/edges and the server fills `start_targets` and validates before writing.
#
#     {name: "B vs T motility", template: "2x2",
#      plots: [{plot: "track_measures", measure: "live.track.speed", chart: "boxplot",
#               pops: ["B/qc/_tracked", "T/qc/_tracked"], statUnit: "image"}, …]}
#
# ── What is deliberately NOT here ────────────────────────────────────────────────────────────────────
# * `vis` (palette, jitter, font size, axis labels…). SummaryPanel already resolves `props.vis ??
#   defaultVis()`, so emitting a vis bag here would copy ~25 frontend defaults into Julia where they
#   would drift. An expanded slot carries only semantics; the panel supplies its own look.
# * Grid AREAS beyond uniform grids. `template` is "<cols>x<rows>"; the named comic plates are a
#   frontend catalogue (plots/layoutTemplates.ts) and stay GUI-only rather than being duplicated here.
# * Everything in Decision 4: dragging, resizing, captions. Claude picks which plots and in what order.
#
# ── What validation can and cannot do ────────────────────────────────────────────────────────────────
# It closes the failure the plan names: a bad `tkey` renders an EMPTY plot with no error, so a board can
# look authored and show nothing. Unknown spec id, a chart that spec doesn't offer, a measure it doesn't
# carry, a population that doesn't exist — all rejected before anything is written. It cannot check
# INTENT: nothing here will stop a well-formed board built on a junk clustering run. That is the
# checkpoint's job, not the validator's (see Risks in the plan).

struct BoardSpecError <: Exception
    msg::String
end
Base.showerror(io::IO, e::BoardSpecError) = print(io, "BoardSpecError: ", e.msg)

# `&amp;` LAST, so an escaped entity ("&amp;lt;") decodes one level to "&lt;" and not all the way to "<".
const _NAME_ENTITIES = ("&lt;" => "<", "&gt;" => ">", "&quot;" => "\"", "&#39;" => "'",
                        "&apos;" => "'", "&#38;" => "&", "&amp;" => "&")

"""
    board_display_name(name) -> String

A board name as it should be STORED: stripped, with HTML entities decoded.

Vue renders text escaped, so a stored `&amp;` displays as `&amp;` — the entity itself, forever, on a
tab the user cannot rename from the tool that created it. An LLM authoring a board called
"Behaviour & tracking" escapes the ampersand roughly as often as not; `add_analysis_board` is add-only,
so the cost of not repairing it is a permanently mis-titled board and a manual delete. Repair here
rather than reject: the intent is unambiguous, and an error would spend a round-trip on punctuation.

Idempotent, so normalising at more than one layer is safe.
"""
board_display_name(name::AbstractString) = String(strip(replace(String(name), _NAME_ENTITIES...)))

# ── The plot-spec registry (one reader) ──────────────────────────────────────────────────────────────
# The specs are package JSON; `api/src/plotting_api.jl` serves them to Vue and reads them through here,
# so the expander validates against exactly what the board can render.
plot_specs_dir() = joinpath(@__DIR__, "plotDefinitions")

"""
    plot_specs() -> Vector{Dict{String,Any}}

Every plot spec in the registry. A malformed file is skipped with a warning rather than taking the
whole registry down — the same tolerance the HTTP route had.
"""
function plot_specs()
    out = Dict{String,Any}[]
    isdir(plot_specs_dir()) || return out
    for f in sort(readdir(plot_specs_dir(); join = true))
        endswith(f, ".json") || continue
        try
            push!(out, JSON3.read(read(f, String), Dict{String,Any}))
        catch e
            @warn "Skipping malformed plot spec" path = f exception = e
        end
    end
    out
end

plot_spec_index() = Dict{String,Any}(String(get(s, "id", "")) => s for s in plot_specs())

_bs_str(v, default = "") = v === nothing ? default : string(v)
_bs_strs(v) = v isa AbstractVector ? String[string(x) for x in v] : String[]

"""
    board_spec_populations(proj; pop_types = …) -> Dict{String,String}

The populations a board may reference: `"valueName/pop"` (the vocabulary `board_summaries` reports and
the spec accepts) → the `pop_type` to fetch it under, unioned over every image in the project.

Built with `plot_population_groups` — **the same enumerator that fills the board's own series picker**
— so the validator accepts exactly what the GUI offers and nothing else. Walking the persisted
populations instead is wrong and was: DERIVED pops are not stored (`docs/POPULATION.md`), so a walk of
the gating sidecars misses `/_tracked` and would have rejected `B/qc/_tracked` — the population the
project's real boards actually plot.
"""
function board_spec_populations(proj::CciaProject;
                                pop_types::Vector{String} = ["live", "track", "flow", "trackclust",
                                                             "clust", "region", "labels"])
    imgs = CciaImage[]
    for s in proj._sets, img in images(s); push!(imgs, img); end
    isempty(imgs) && return Dict{String,String}()
    names_for = img -> versioned_keys(img.label_props)
    load_map  = (img, vn, pt) -> load_pop_map(img; value_name = vn, pop_type = pt)
    # Root `/_tracked` only where tracking was UNGATED — the same rule the picker applies, so we never
    # accept a population the user cannot select (plotting_api.jl `root_ok`).
    root_ok = (vn, _pt, dpath) -> dpath != "/_tracked" ? true :
        any(im -> (vn in String.(versioned_keys(im.label_props))) &&
                  has_ungated_tracks(im; value_name = vn), imgs)
    pops = Dict{String,String}()
    for pt in pop_types
        groups = try
            plot_population_groups(imgs, names_for, load_map, String[pt]; root_derived_ok = root_ok)
        catch e
            @warn "Could not enumerate populations for a board spec" pop_type = pt exception = e
            continue
        end
        for g in groups, p in g.populations
            get!(pops, "$(g.value_name)$(p.path)", string(p.pop_type))
        end
    end
    pops
end

_TEMPLATE_RE = r"^(\d+)\s*[x×]\s*(\d+)$"

# `template` → (cols, rows). Empty picks the smallest near-square grid that holds the plots, which is
# what a generated board wants; anything else must be an explicit "<cols>x<rows>".
function board_template_grid(template::AbstractString, n::Int)
    t = strip(template)
    if isempty(t)
        cols = max(1, ceil(Int, sqrt(max(n, 1))))
        return cols, max(1, ceil(Int, max(n, 1) / cols))
    end
    m = match(_TEMPLATE_RE, lowercase(t))
    m === nothing && throw(BoardSpecError(
        "template must be \"<cols>x<rows>\" (e.g. \"2x2\"); named comic plates are only available in " *
        "the GUI. Got \"$template\""))
    cols, rows = parse(Int, m.captures[1]), parse(Int, m.captures[2])
    (cols in 1:6 && rows in 1:6) ||
        throw(BoardSpecError("template \"$template\" is out of range — 1..6 columns and rows"))
    cols, rows
end

# One CSS grid-area per slot, row-major — the uniform case of plots/layoutTemplates.ts `uniform()`.
board_slot_areas(cols::Int, rows::Int) =
    String["$(r) / $(c) / $(r + 1) / $(c + 1)" for r in 1:rows for c in 1:cols]

const _STAT_UNITS = ("individual", "image")
const _IMAGE_AGGS = ("mean", "median")

# One entry of `plots` → a `SlotContent`. Every rejection names the offending value AND what was
# available, because the caller is an agent that can correct itself if told what the options are.
function _expand_plot(specs::AbstractDict, pops::AbstractDict, raw, i::Int)
    raw isa AbstractDict || throw(BoardSpecError("plots[$i] must be an object"))
    d = Dict{String,Any}(string(k) => v for (k, v) in pairs(raw))

    id = _bs_str(get(d, "plot", get(d, "specId", nothing)))
    isempty(id) && throw(BoardSpecError("plots[$i] needs a `plot` (a plot-spec id)"))
    sp = get(specs, id, nothing)
    sp === nothing && throw(BoardSpecError(
        "plots[$i]: unknown plot \"$id\". Available: $(join(sort(collect(keys(specs))), ", "))"))

    charts = _bs_strs(get(sp, "chartTypes", nothing))
    chart  = _bs_str(get(d, "chart", nothing))
    if isempty(chart)
        isempty(charts) && throw(BoardSpecError("plots[$i]: \"$id\" declares no chart types"))
        chart = first(charts)
    elseif !(chart in charts)
        throw(BoardSpecError("plots[$i]: \"$id\" does not offer chart \"$chart\". Offers: $(join(charts, ", "))"))
    end

    ds = get(sp, "dataSource", nothing)
    ds = ds isa AbstractDict ? ds : Dict{String,Any}()
    opts    = _bs_strs(get(ds, "measureOptions", nothing))
    measure = _bs_str(get(d, "measure", nothing))
    if isempty(measure)
        measure = _bs_str(get(ds, "measure", nothing))
    elseif !isempty(opts) && !(measure in opts)
        throw(BoardSpecError(
            "plots[$i]: \"$id\" does not carry measure \"$measure\". Offers: $(join(opts, ", "))"))
    end

    # ── popType: which FAMILIES the panel will load, not a label ────────────────────────────────────
    #
    # Traced, because guessing this wrote a board where all four plots said "Select one or more
    # populations". The panel fetches its population list from GET /api/plots/populations with the
    # panel's `popType`, which the route expands via `plot_pop_types(popType, granularity)` — a track
    # plot unions `[popType, "track"]`, a cell plot is just `[popType]` — and offers each population
    # TAGGED with the family it was found under. The frontend builds every `tkey` from that tag. So a
    # tkey whose family is outside the expansion of the panel's popType is one the picker never offers:
    # it matches nothing, the panel renders empty, and NOTHING errors.
    #
    # That is exactly what `popType: "track"` on `T/qc/_tracked` did. `plot_pop_types("track","track")`
    # is just `["track"]` — track-family pops are gates drawn on per-track measures (`{vn}__tracks.json`)
    # — and that project has none at all (its families are live, labels, region, trackclust). Zero
    # populations, four blank plots. Note "track" IS in track_measures' offered popTypes, so a
    # membership check would not have caught it: what matters is whether the NAMED POPULATIONS are
    # reachable under the popType, not whether the spec lists it.
    #
    # `pops` (board_spec_populations) is the same enumeration the picker uses and tags each population
    # with its family, so it can answer exactly that question.
    offered = get(ds, "popTypes", nothing)
    offered = offered isa AbstractVector ? offered : Any[]
    _pt_of  = e -> _bs_str(get(e, "popType", nothing))
    _gran_of = e -> _bs_str(get(e, "granularity", nothing))
    # families a given popType would make available on THIS spec (its own granularity entry)
    _families = pt -> begin
        e = findfirst(x -> _pt_of(x) == pt, offered)
        Set(plot_pop_types(pt, e === nothing ? "" : _gran_of(offered[e])))
    end
    first_pt = isempty(offered) ? "" : _pt_of(first(offered))
    asked_pt = _bs_str(get(d, "popType", nothing))

    wanted = _bs_strs(get(d, "pops", nothing))
    for p in wanted
        haskey(pops, p) || throw(BoardSpecError(
            "plots[$i]: no population \"$p\" in this project. " *
            "Use get_populations to see what exists (as valueName/pop)."))
    end
    needed = unique(String[pops[p] for p in wanted])          # the families the named pops live in

    if isempty(asked_pt)
        # Derive: the spec's first offered popType that actually reaches every named population. With no
        # pops named there is nothing to reach, so the spec's own default stands.
        pop_type = first_pt
        if !isempty(needed)
            idx = findfirst(e -> issubset(needed, _families(_pt_of(e))), offered)
            idx === nothing && throw(BoardSpecError(
                "plots[$i]: \"$id\" cannot plot $(join(("\"$p\" ($(pops[p]))" for p in wanted), ", ")) " *
                "— it offers $(join((_pt_of(e) for e in offered), ", ")), and none of those reach " *
                "$(join(needed, ", ")) populations. Pick a different plot or different populations."))
            pop_type = _pt_of(offered[idx])
        end
    else
        # Explicit: must be one the spec offers AND must reach every named population.
        any(e -> _pt_of(e) == asked_pt, offered) || isempty(offered) || throw(BoardSpecError(
            "plots[$i]: \"$id\" does not offer popType \"$asked_pt\". " *
            "Offers: $(join((_pt_of(e) for e in offered), ", ")) — and it is DERIVED from the " *
            "populations anyway, so omit it."))
        fam = _families(asked_pt)
        issubset(needed, fam) || throw(BoardSpecError(
            "plots[$i]: popType \"$asked_pt\" cannot reach " *
            "$(join(("\"$p\" (a $(pops[p]) population)" for p in wanted if !(pops[p] in fam)), ", ")). " *
            "With popType \"$asked_pt\" this panel only lists $(join(sort(collect(fam)), ", ")) " *
            "populations, so the plot would render blank with no error. popType is DERIVED from the " *
            "populations — omit it."))
        pop_type = asked_pt
    end

    # Populations → `tkey`s ("popType::valueName/pop"), each tagged with ITS OWN family — which is how
    # the picker tags them, and why a track plot can legitimately overlay a `live` pop and a `track` pop
    # in one panel (`plot_pop_types` unions them).
    sel = String["$(pops[p])::$(p)" for p in wanted]

    state = Dict{String,Any}("specId" => id, "chartType" => chart)
    isempty(measure) || (state["measure"] = measure)
    isempty(sel)     || (state["sel"] = sel)
    isempty(pop_type) || (state["popType"] = pop_type)
    gb = _bs_str(get(d, "groupBy", nothing))
    isempty(gb) || (state["groupBy"] = gb)

    # statUnit/imageAgg travel together (utils/statUnitState.ts). Omitted entirely when the caller says
    # nothing, so the panel fills its own default on first open rather than us guessing one here.
    su = _bs_str(get(d, "statUnit", nothing))
    if !isempty(su)
        su in _STAT_UNITS || throw(BoardSpecError(
            "plots[$i]: statUnit must be one of $(join(_STAT_UNITS, ", ")), got \"$su\""))
        agg = _bs_str(get(d, "imageAgg", nothing), "mean")
        agg in _IMAGE_AGGS || throw(BoardSpecError(
            "plots[$i]: imageAgg must be one of $(join(_IMAGE_AGGS, ", ")), got \"$agg\""))
        state["statUnit"] = su
        state["imageAgg"] = agg
    end

    Dict{String,Any}("kind" => "summary", "ref" => id, "state" => state)
end

# What the board compares across images → the `shared` keys `useSummaryData` reads
# (`compareMode`/`compareAttr`/`compareAttr2`). Board-level, not per-slot: the composable destructures
# all three out of the shared bag, so one comparison governs every panel.
#
# This is the difference between a board and a FIGURE. Without it an authored board is stuck on the
# frontend default (single image), which is why the first board authored for a 4-mouse experiment
# compared by image and could not answer "does this differ between mice". `groupAttr` reaches the data
# route generically (api_plot_data builds `attr_map` from `im.attr` for any plot), so this is not
# limited to the two specs whose `scopeModes` happen to list `by_attr` — that field is declarative and
# nothing reads it.
function _compare_state(proj::CciaProject, compare_by::AbstractString;
                        attrs_available::Union{Vector{String},Nothing} = nothing)::Dict{String,Any}
    want = strip(String(compare_by))
    isempty(want) && return Dict{String,Any}()
    want in ("image", "per_image", "summarised") && return Dict{String,Any}("compareMode" => want)

    # anything else names image attributes ("Mouse", or "Treatment,Mouse" to combine two)
    attrs = String[strip(a) for a in split(want, ","; keepempty = false)]
    isempty(attrs) && return Dict{String,Any}()
    length(attrs) <= 2 || throw(BoardSpecError(
        "compareBy takes at most two attributes (the board combines them); got $(length(attrs))"))
    # injectable for the same reason `pops` is: the accept/reject rules are testable without a project
    # on disk. Production passes nothing and gets the project's own images.
    have = attrs_available
    if have === nothing
        imgs = CciaImage[]
        for s in proj._sets, img in images(s); push!(imgs, img); end
        have = String[String(p.first) for p in attr_value_counts(imgs)]
    end
    for a in attrs
        a in have || throw(BoardSpecError(
            "compareBy \"$a\" is not an image attribute in this project. " *
            (isempty(have) ?
             "These images carry no attributes at all, so they can only be compared per image — " *
             "use compareBy \"per_image\" (or annotate the set first)." :
             "Available: $(join(have, ", ")). Use get_image_attributes to see their values.")))
    end
    out = Dict{String,Any}("compareMode" => "by_attr", "compareAttr" => attrs[1])
    length(attrs) == 2 && (out["compareAttr2"] = attrs[2])
    out
end

"""
    expand_board(proj, name, plots; template = "", compare_by = "") -> Dict

Validate a board spec against the project and expand it into a `LayoutEntry` (the value stored under
`layouts["tab:<id>"]`). Throws `BoardSpecError` — with a message naming the bad value and the available
ones — rather than writing a board that would render blank.

`compare_by` sets what the board compares ACROSS IMAGES — board-level, because that is where
`useSummaryData` keeps it (the `shared` bag, not per-slot):

  - `""`            leave the frontend default (single-image)
  - `"per_image"`   one series per image
  - `"summarised"`  pool the whole set into one series
  - any other value is an image ATTRIBUTE name (e.g. `"Mouse"`) → `by_attr`, so images sharing a value
    pool into one series labelled by it. Two attributes may be combined as `"Treatment,Mouse"`.

The attribute names are validated against the project's own images, because an attribute that does not
exist groups nothing and the board silently falls back to per-image.
"""
function expand_board(proj::CciaProject, name::AbstractString, plots; template::AbstractString = "",
                      pops::Union{AbstractDict,Nothing} = nothing,
                      compare_by::AbstractString = "",
                      attrs::Union{Vector{String},Nothing} = nothing)
    isempty(strip(String(name))) && throw(BoardSpecError("the board needs a name"))
    plots isa AbstractVector || throw(BoardSpecError("`plots` must be a list"))
    isempty(plots) && throw(BoardSpecError("a board needs at least one plot"))

    cols, rows = board_template_grid(template, length(plots))
    slots = cols * rows
    length(plots) <= slots || throw(BoardSpecError(
        "$(length(plots)) plots do not fit a $(cols)x$(rows) board ($slots slots) — " *
        "use a bigger template or fewer plots"))

    specs = plot_spec_index()
    # `pops` is injectable so the accept/reject rules can be tested without a gated+tracked project on
    # disk; production always passes nothing and gets the picker's own list.
    available = pops === nothing ? board_spec_populations(proj) : pops
    contents = Any[nothing for _ in 1:slots]
    for (i, p) in enumerate(plots)
        contents[i] = _expand_plot(specs, available, p, i)
    end

    # `shared.scope` decides WHERE the board reads each panel's population selection: "global" (the
    # frontend's default) makes every slot use the board-level `shared.sel` and IGNORE its own `sel`;
    # "local" makes each slot use the one it carries. We write per-slot `sel` — one plot may want
    # different populations from the next — so the board must be told to read them, or an authored board
    # renders with no series until the user picks populations by hand. That is exactly what happened the
    # first time this shipped with an empty `shared`.
    #
    # Only `scope` and the requested comparison are set. Everything else in the bag (poolGroups, vis,
    # the clustering picks) is a frontend default we would only be copying — same rule as omitted `vis`.
    shared = any(c -> c !== nothing && haskey(c["state"], "sel"), contents) ?
        Dict{String,Any}("scope" => "local") : Dict{String,Any}()
    merge!(shared, _compare_state(proj, compare_by; attrs_available = attrs))

    Dict{String,Any}(
        "cols" => cols, "rows" => rows,
        "slotAreas" => board_slot_areas(cols, rows),
        "contents" => contents,
        "activeIndex" => 0,
        "shared" => shared)
end

"""
    append_board(doc, name, layout) -> (BoardsDoc, Int)

Add ONE board to a document — the add-only half of Decision 1. Never touches an existing tab, so the
id bookkeeping lives here rather than in the route. Throws `BoardSpecError` if the name is taken (the
route turns that into a 409), because two boards with one name is a GUI the user cannot navigate.

The name is stored as `board_display_name` gives it (stripped, entities decoded) — normalised HERE, the
invariant's owner, because the REPL reaches this without the route.
"""
function append_board(doc::BoardsDoc, name::AbstractString, layout::AbstractDict)
    nm = board_display_name(name)
    for t in doc.tabs
        t isa AbstractDict || continue
        strip(_bs_str(get(t, :name, get(t, "name", nothing)))) == nm &&
            throw(BoardSpecError("a board named \"$nm\" already exists — pick another name"))
    end
    id = max(doc.next_id, length(doc.tabs)) + 1
    tabs = vcat(doc.tabs, Any[Dict{String,Any}("id" => id, "name" => nm)])
    layouts = merge(doc.layouts, Dict{String,Any}("tab:$(id)" => layout))
    # `activeId` is deliberately NOT moved to the new board: this is an additive write into a project
    # the user may have open, and stealing their active tab is a modification of their view.
    BoardsDoc(doc.version, tabs, doc.active_id, id, layouts, doc.present, doc.readable), id
end
