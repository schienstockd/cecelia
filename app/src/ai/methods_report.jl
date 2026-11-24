# ── Methods report — a draft Methods section from what the project already records ────────────────
#
# docs/ANALYSIS.md → *Methods report*. ONE generator, three scopes, three front doors (the route
# `GET /api/analysis/methods`, the board's Export menu + the chain Live view, the MCP read tool
# `get_methods_report`). Assembled by code from records that already exist — the parameter table
# (`history_params_table`), the run log (author stamps, timing, versions), chain run records, cohort QC
# sidecars and population / board / chain stamps — so the same record always gives the same text and
# nothing is invented. Read-only: writes nothing, the lab log included.
#
# Rules the text keeps (docs/todo/METHODS_REPORT_PLAN.md, locked decisions):
#   - it opens with a draft banner: the author checks it before use;
#   - a step started by Claude or Kiwi names the RECORDED model, or says "model not recorded";
#   - a field the record lacks is said to be missing, never filled in.

const METHODS_DRAFT_BANNER = "**Draft.** Generated from the project record by Cecelia. " *
                             "Check every statement before you use it."
const METHODS_MODEL_UNKNOWN = "model not recorded"
# Cecelia's own paper, cited once beside the version (CITATION.cff)
const CECELIA_DOI = "10.1038/s41467-025-57193-y"
# A step Claude suggested and the user ran is stamped as the user's: the report can't see it.
const METHODS_CEILING = "A step Claude suggested and a person then ran is stamped as that person's, " *
                        "so this list can miss suggestions."

# `via` values that mean Claude did it, and how the text names them (`author_stamp`, kiwi_api.jl)
const _METHODS_AI_VIAS = Dict("claude" => "Claude", "kiwi" => "Kiwi (Claude in the app)")

_mr_str(d, k) = d isa AbstractDict ? string(something(get(d, k, get(d, Symbol(k), nothing)), "")) : ""
_mr_md(s) = replace(string(s), "|" => "\\|", "\n" => " ")    # one Markdown table cell
_mr_join(xs) = length(xs) <= 1 ? join(xs) : join(xs[1:end-1], ", ") * " and " * xs[end]

"""
    methods_ai_phrase(stamps) -> String

How a set of author stamps (`{via, model?}`, any shape `author_stamp` writes) reads in the report: ""
when none is Claude's; else "Claude (claude-x)", one per AI `via`, each with its recorded models and
"model not recorded" when any of its stamps carries none.
"""
function methods_ai_phrase(stamps)::String
    by_via = Dict{String,Set{String}}()
    for s in stamps
        via = _mr_str(s, "via")
        haskey(_METHODS_AI_VIAS, via) || continue
        m = _mr_str(s, "model")
        push!(get!(Set{String}, by_via, via), isempty(m) ? METHODS_MODEL_UNKNOWN : m)
    end
    parts = String[]
    for via in sort!(collect(keys(by_via)))
        ms = sort!(collect(by_via[via]); by = m -> (m == METHODS_MODEL_UNKNOWN, m))   # unknown last
        push!(parts, "$(_METHODS_AI_VIAS[via]) ($(join(ms, ", ")))")
    end
    _mr_join(parts)
end

# "2026-11-15T14:30:35" → DateTime; nothing when absent or unreadable
_mr_time(s) = isempty(s) ? nothing : (try DateTime(first(s, 19)) catch; nothing end)
_mr_fmt(t) = t === nothing ? "" : Dates.format(t, "yyyy-mm-dd HH:MM")
function _mr_took(a, b)
    (a === nothing || b === nothing || b < a) && return ""
    s = round(Int, Dates.value(b - a) / 1000)
    s < 60 ? "$s s" : s < 3600 ? "$(div(s, 60)) min $(lpad(rem(s, 60), 2, '0')) s" :
                      "$(div(s, 3600)) h $(lpad(div(rem(s, 3600), 60), 2, '0')) min"
end

# Every run-log entry (archive included) on `uids`, by task id: `taskId => [(uid, entry)]`.
function _mr_entries(proj::CciaProject, uids)
    out = Dict{String,Vector{Tuple{String,Any}}}()
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage || continue
        for e in read_run_log_history(img)
            id = _rl_str(e, "taskId")
            isempty(id) || push!(get!(Vector{Tuple{String,Any}}, out, id), (uid, e))
        end
    end
    out
end
_mr_row_entries(row, ents) = [e for id in get(row, "taskIds", String[]) for (_, e) in get(ents, id, Tuple{String,Any}[])]

# A row's `cite` (task citation DOIs) as text — a list or a string
_mr_cite(row) = (c = get(row, "cite", ""); c isa AbstractVector ? join(string.(c), "; ") : string(something(c, "")))

# A step's notes worth a reader's time: what the record lacks. "never replayed" / "per-image geometry"
# are about replaying a chain, not about the method.
_mr_note_shown(note) = startswith(note, "parameters unknown") || startswith(note, "no recorded run")

# Populations a step read that are no longer defined on the images (renamed or removed since), by name
function _mr_missing_pops(fn::AbstractString, entries, defined::Set{String})::Vector{String}
    isempty(defined) && return String[]
    idx = _mr_param_index(fn)
    vals = first(_mr_values(fn, entries, idx))
    own = string(something(get(vals, "valueName", nothing), VERSIONED_DEFAULT_VAL))
    out = String[]
    for (k, p) in idx
        (p isa AbstractDict && get(p, "type", "") == "popSelection") || continue
        for ref in _mr_list(get(vals, k, nothing))
            r = string(ref)
            (_whole_pop(r) || any(seg -> startswith(seg, "_"), split(r, '/'))) && continue
            ls, path = _split_pop_ref(r, own)
            "$ls$path" in defined || push!(out, r)
        end
    end
    sort!(unique!(methods_pop_phrases(out)))          # named together, so two OTIs stay two
end

"""
    _mr_step_sentence(row, entries, n, refs, defined, checks) -> String

One analysis step as prose: its `methodsText` (else the plain sentence) — opened with "In k of N
images," when it did not run on all — then who started it when Claude did, then its citations as `[i]`
into `refs` (DOIs in first-cited order, appended to). A step that read populations no longer defined on
the images adds a line to `checks` (the report's *Check before use*), not to the prose.
"""
function _mr_step_sentence(row, entries, n::Int, refs::Vector{String}, defined::Set{String},
                           checks::Vector{String} = String[])::String
    fn = string(get(row, "task", ""))
    text = isempty(fn) ? nothing : _mr_methods_text(fn, entries, something(get(row, "spread", nothing), Dict{String,Any}()))
    text === nothing && (text = _mr_plain_text(row))
    extras = String[]
    m = match(r"^(\d+)/(\d+)$", string(get(row, "images", "")))
    if m !== nothing && m.captures[1] != m.captures[2]
        first_word = first(split(text, r"[\s,]"; limit = 2))
        rest = any(isuppercase, first_word[nextind(first_word, 1):end]) ? text : lowercase(text[1:1]) * text[nextind(text, 1):end]
        text = "In $(m.captures[1]) of $(m.captures[2]) images, $rest"
    end
    ai = methods_ai_phrase(_rl_get(e, "author", nothing) for e in entries)
    isempty(ai) || push!(extras, "started by $ai")
    nums = Int[]
    for d in split(_mr_cite(row), "; ")
        d = strip(d); isempty(d) && continue
        i = findfirst(==(d), refs)
        i === nothing && (push!(refs, String(d)); i = length(refs))
        push!(nums, i)
    end
    body = endswith(text, ".") ? text[1:prevind(text, lastindex(text))] : text
    s = body * (isempty(extras) ? "" : " ($(join(extras, "; ")))") * (isempty(nums) ? "" : " [$(join(nums, ", "))]") * "."
    note = string(get(row, "note", ""))
    _mr_note_shown(note) && (s *= " Note: $note.")
    gone = isempty(fn) ? String[] : _mr_missing_pops(fn, entries, defined)
    isempty(gone) || push!(checks, "*$(get(row, "step", fn))* ($(get(row, "images", ""))) read " *
        "population$(length(gone) == 1 ? "" : "s") no longer defined on these images — $(_mr_join(gone)): " *
        "renamed or removed since, so it cannot be rerun as recorded.")
    s
end

# Which paragraph a step belongs in, by its task category
const _MR_SECTIONS = [
    "Image processing and segmentation" => ("importImages", "cleanupImages", "editImages", "segment", "gate"),
    "Tracking and behaviour"            => ("tracking", "behaviour", "clustTracks"),
    "Clustering"                        => ("clustPops",),
    "Spatial analysis"                  => ("spatialAnalysis", "clustRegions"),
]
const _MR_OTHER_SECTION = "Other analysis"
function _mr_section(fn::AbstractString)::String
    cat = String(first(split(fn, "."; limit = 2)))
    for (title, cats) in _MR_SECTIONS
        cat in cats && return title
    end
    _MR_OTHER_SECTION
end

# The steps a set's Methods left out, as one sentence: label, how many, on how many images
function _mr_passed_over(rows)::String
    groups = Dict{Tuple{String,String},Vector{Int}}()
    order = Tuple{String,String}[]
    for r in rows
        k = (string(get(r, "step", get(r, "task", ""))), string(get(r, "usage", "")))
        haskey(groups, k) || push!(order, k)
        m = match(r"^(\d+)/", string(get(r, "images", "")))
        push!(get!(Vector{Int}, groups, k), m === nothing ? 0 : parse(Int, m.captures[1]))
    end
    parts = map(order) do (label, use)
        ns = groups[(label, use)]
        lo, hi = extrema(ns)
        imgs = lo == hi ? "$lo image$(lo == 1 ? "" : "s")" : "$(lo)–$(hi) images"
        "$label$(length(ns) > 1 ? " ×$(length(ns))" : "")$(use == "redone" ? ", later redone" : "") " *
        "($imgs$(length(ns) > 1 ? " each" : ""))"
    end
    "$(join(parts, "; ")). Their parameters are in the supplementary table."
end

# The images a scope covers, and how the scope reads. A list of images that is exactly one set's is
# that set (the board's Methods sends its images).
function _mr_scope(proj::CciaProject, image_uids, set_uid, run)
    if run !== nothing
        return run.image_uids, "Chain run `$(run.id)` (\"$(run.chain_name)\")"
    elseif !isempty(set_uid)
        s = findfirst(st -> st.uid == set_uid, sets(proj))
        s === nothing && error("set not found: $set_uid")
        st = sets(proj)[s]
        return String[i.uid for i in images(st)], "Set \"$(st.name)\""
    end
    uids = String.(image_uids)
    for st in sets(proj)
        Set(i.uid for i in images(st)) == Set(uids) && return uids, "Set \"$(st.name)\""
    end
    uids, "Selected images"
end

# ── the parameter table as CSV ──
# Columns in a fixed order, `relation` only when a row says one — the same columns and quoting as the
# frontend's `historyParamsCsv` + `rowsToCsv` (utils/chainFromHistory.ts, plots/export.ts), which the
# history dialog and the board export still use. The Methods modal and the RO-Crate take this one, so
# the CSV a user downloads and the one in the crate are the same bytes.
const _MR_CSV_COLS = [("step", "step"), ("task", "task"), ("output", "output"), ("relation", "relation"),
                      ("images", "images"), ("last_run", "lastRun"), ("changed_from_default", "changedFromDefault"),
                      ("differs_between_images", "conflicts"), ("note", "note"), ("in_methods", "usage")]
_mr_csv_cell(v) = (s = v === nothing ? "" : string(v);
                   occursin(r"[\",\n]", s) ? "\"" * replace(s, "\"" => "\"\"") * "\"" : s)

# a row's `usage` as the CSV says it: in the Methods text, or why not
_mr_usage_cell(u) = (u = string(something(u, "")); isempty(u) ? "yes" : u == "redone" ? "no (redone later)" : "no (trial)")

"""
    history_params_csv(table) -> String

The Methods parameter table (`history_params_table` rows) as CSV, "" for no rows. `relation` appears
only when some row carries one (a board-scoped table).
"""
function history_params_csv(table::AbstractVector)::String
    isempty(table) && return ""
    scoped = any(r -> !isempty(string(something(get(r, "relation", ""), ""))), table)
    has(k) = any(r -> !isempty(string(something(get(r, k, ""), ""))), table)
    cols = [c for c in _MR_CSV_COLS if (c[1] != "relation" || scoped) && (c[1] != "in_methods" || has("usage"))]
    lines = [join(first.(cols), ",")]
    for r in table
        push!(lines, join((_mr_csv_cell(k == "usage" ? _mr_usage_cell(get(r, k, "")) : get(r, k, "")) for (_, k) in cols), ","))
    end
    join(lines, "\n")
end

"""
    _mr_data_lines(proj, uids) -> Vector{String}

The "Data" section: each distinct source of the images in scope (`data_source(proj, img)`, the image's
own else its set's) with its DOI, licence and image count, an import suggestion marked unconfirmed, and
the count with none. Empty when no image in scope records a source (the lab's own data).
"""
function _mr_data_lines(proj::CciaProject, uids)::Vector{String}
    groups = Dict{String,Tuple{DataSource,Int}}()
    unknown = 0
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        d = img isa CciaImage ? data_source(proj, img) : nothing
        if d === nothing
            unknown += 1
        else
            k = string(hash(data_source_dict(d)))
            groups[k] = (d, get(groups, k, (d, 0))[2] + 1)
        end
    end
    isempty(groups) && return String[]
    lines = String["### Data", ""]
    for (d, n) in sort!(collect(values(groups)); by = g -> (g[1].citation, g[1].doi, g[1].url))
        label = isempty(d.citation) ? (isempty(d.doi) ? d.url : "doi:$(d.doi)") : d.citation
        extra = String[]
        isempty(d.doi) || isempty(d.citation) || push!(extra, "doi:$(d.doi)")
        isempty(d.url) || !isempty(d.doi) || push!(extra, d.url)
        isempty(d.licence) || push!(extra, d.licence)
        line = "- $label" * (isempty(extra) ? "" : " ($(join(extra, ", ")))") *
               ": $n image$(n == 1 ? "" : "s")."
        d.confirmed || (line *= " Suggested at import from the file path; not confirmed.")
        push!(lines, line)
    end
    unknown > 0 && push!(lines, "- $unknown image$(unknown == 1 ? "" : "s"): data source not recorded.")
    push!(lines, "")
    lines
end

"""
    methods_report(proj; image_uids, set_uid, run_id) -> Dict

A draft Methods section, `{markdown, table, csv, scope}`. Two scopes, one text:

- **set** — `set_uid` (or a list of `image_uids`, which reads as its set when it is one): the analysis
  on its images, in prose. Steps the analysis moved past — redone later, or trials (`_history_usage`)
  — are one sentence and stay in the table (`usage`);
- **chain run** — `run_id`: the run's images, the runs its nodes made (`history_params_table` cut to
  their task ids), each node's status and timing (the run log's `at`/`finishedAt`, joined by task id),
  and the cohort QC last checked on its sets.

Both: each step is its task's `methodsText` (`ai/methods_text.jl`), grouped into paragraphs by
category, with `[n]` citations into one reference list (`ai/references.toml`); how the populations
were defined (`ai/methods_populations.jl`); what Claude drew or started, by recorded model.
`table` is the parameter table the text is written from, `csv` that table as CSV
(`history_params_csv`). Deterministic: no clock, no randomness, every list sorted. Read-only.
"""
function methods_report(proj::CciaProject; image_uids::AbstractVector = String[],
                        set_uid::AbstractString = "", run_id::AbstractString = "")::Dict{String,Any}
    run = isempty(run_id) ? nothing : load_chain_run(proj, String(run_id))
    uids, scope = _mr_scope(proj, image_uids, set_uid, run)
    isempty(uids) && error("no images in scope")
    n = length(uids)
    pops = _mr_population_index(proj, uids)
    task_ids = run === nothing ? nothing :
        sort!(unique(String[st.task_id for m in values(run.image_states) for st in values(m) if st.task_id !== nothing]))
    table = run !== nothing ? history_params_table(proj, uids; task_ids) :
            history_params_table(proj, uids; usage_runs = _mr_pop_runs(proj, uids, pops))
    ents = _mr_entries(proj, uids)
    names = Dict{String,String}()
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        names[uid] = img isa CciaImage ? img.name : uid
    end
    analysis = [r for r in table if isempty(string(get(r, "usage", "")))]
    passed   = [r for r in table if !isempty(string(get(r, "usage", "")))]

    used = [e for row in table for e in _mr_row_entries(row, ents)]
    versions = sort!(unique(String[_rl_str(e, "ceceliaVersion") for e in used if !isempty(_rl_str(e, "ceceliaVersion"))]))
    commits  = sort!(unique(String[_rl_str(e, "ceceliaCommit") for e in used if !isempty(_rl_str(e, "ceceliaCommit"))]))
    sw = isempty(versions) ? "Cecelia (version not recorded on these runs)" :
         "Cecelia $(join(versions, ", "))" * (isempty(commits) ? "" : " (commit $(join(commits, ", ")))")

    md = String["> " * METHODS_DRAFT_BANNER, "", "## Methods (draft)", "",
                "*$scope: $n image$(n == 1 ? "" : "s"), project \"$(proj.name)\". $sw [1].*", ""]

    # ── the analysis, one paragraph per section; the populations after processing ──
    refs = String[CECELIA_DOI]          # the software itself, cited once in the line above
    checks = String[]
    head = length(md)
    append!(md, _mr_data_lines(proj, uids))          # where the images came from, when recorded
    defined = Set("$(e.value_name)$(e.path)" for e in pops)
    pop_lines, ai_lines = _mr_populations(pops, n)
    isempty(analysis) && push!(md, "No recorded step ran on these images.", "")
    titles = vcat(first(_MR_SECTIONS[1]), "Populations", [first(s) for s in _MR_SECTIONS[2:end]], _MR_OTHER_SECTION)
    # sentences in reading order, so the references number as the text cites them
    for title in titles
        body = title == "Populations" ? pop_lines :
               [_mr_step_sentence(row, _mr_row_entries(row, ents), n, refs, defined, checks)
                for row in analysis if _mr_section(string(get(row, "task", ""))) == title]
        isempty(body) && continue
        push!(md, "### $title", "", join(body, title == "Populations" ? "\n\n" : " "), "")
    end

    # what the record says the reader must look at before using the text — under the banner, once each
    isempty(checks) || splice!(md, head+1:head, ["**Check before use:**", "", ("- " .* unique(checks))..., ""])

    for row in analysis
        ai = methods_ai_phrase(_rl_get(e, "author", nothing) for e in _mr_row_entries(row, ents))
        isempty(ai) || push!(ai_lines, "- Step *$(get(row, "step", ""))*: started by $ai.")
    end

    # ── chain run: nodes, status, timing, cohort QC ──
    run === nothing || append!(md, _mr_chain(proj, run, ents, names, ai_lines))

    isempty(passed) || push!(md, "### Also run, not part of this analysis", "",
                             "Left out: runs a later run redid, and runs on part of the set that nothing in the " *
                             "analysis read. " * _mr_passed_over(passed), "")

    # ── AI assistance ──
    push!(md, "### AI assistance", "")
    isempty(ai_lines) ? push!(md, "No step or population in this record is stamped as made by Claude.") :
                        append!(md, ai_lines)
    unstamped = count(e -> _rl_get(e, "author", nothing) === nothing, used)
    unstamped > 0 && push!(md, "", "$unstamped run$(unstamped == 1 ? "" : "s") predate author stamps: who started them is not recorded.")
    push!(md, "", METHODS_CEILING, "")

    # ── references + table ──
    if !isempty(refs)
        push!(md, "### References", "")
        append!(md, ["$i. $(methods_reference_line(d))" for (i, d) in enumerate(refs)])
        push!(md, "")
    end
    push!(md, "*Supplementary table (CSV): all $(length(table)) recorded step$(length(table) == 1 ? "" : "s") " *
              "with every parameter changed from its default$(isempty(passed) ? "" : ", including the runs under *Also run*").*")

    Dict{String,Any}("markdown" => join(md, "\n") * "\n", "table" => table, "csv" => history_params_csv(table),
                     "scope" => Dict{String,Any}("kind" => run !== nothing ? "chainRun" : "set",
                                                 "label" => scope, "imageUids" => uids))
end

# A node's params as the run recorded them (`node_params`, before the per-image steps), only what
# differs from the spec default — the same comparison as the parameter table's `changedFromDefault`.
function _mr_node_params(run, n)::String
    np = run.node_params === nothing ? nothing : get(run.node_params, n.id, nothing)
    np isa AbstractDict || return "not recorded"
    flat = _hp_flat(Dict{String,Any}(string(k) => v for (k, v) in np), _history_param_defaults(n.fn), _history_leaves(n.fn))
    changed = String[]
    for k in sort!(collect(keys(flat)))
        (startswith(k, "_") || is_bookkeeping_param(k)) && continue
        v, d = flat[k]
        (d === missing ? _hp_empty(v) : _hp_key(v) == _hp_key(d)) || push!(changed, "$k=$(_hp_show(v))")
    end
    isempty(changed) ? "defaults" : join(changed, "; ")
end

# The chain-run sections: the run, one row per node (status across images, timing), cohort QC.
function _mr_chain(proj::CciaProject, run, ents, names, ai_lines)::Vector{String}
    md = String["### Chain run", ""]
    # local wall time at that instant (the zone's rules then, not now), as the run log writes its `at`
    started = Libc.strftime("%Y-%m-%d %H:%M", run.created_at)
    line = "Chain \"$(run.chain_name)\" (run `$(run.id)`) was started $started" *
           (isempty(run.by) ? "" : " by profile \"$(run.by)\"") *
           (isempty(run.resumed_by) ? "" : ", last resumed by \"$(run.resumed_by)\"") *
           (isempty(run.from_run) ? "" : ", replaying run `$(run.from_run)`") *
           " on $(length(run.image_uids)) images."
    push!(md, line)
    if !isempty(run.excluded_images)
        nm(u) = (img = try init_object(proj.uid, u) catch; nothing end; img isa CciaImage ? img.name : u)
        ex = ["$(nm(u)) ($r)" for (u, r) in sort!(collect(run.excluded_images); by = first)]
        push!(md, "Left out before the run: $(join(ex, "; ")).")
    end
    tpl_path = _template_path(proj, run.chain_name)
    if isfile(tpl_path)
        raw = try JSON3.read(read(tpl_path, String)) catch; nothing end
        ai = raw === nothing ? "" : methods_ai_phrase([get(raw, :createdBy, nothing), get(raw, :updatedBy, nothing)])
        isempty(ai) || push!(ai_lines, "- Chain template \"$(run.chain_name)\" (as saved now): written by $ai.")
    end
    push!(md, "", "| # | Step | Task | Scope | Parameters changed from default | Status | Started | Finished | Took |",
                  "|---|---|---|---|---|---|---|---|---|")
    N = length(run.image_uids)
    nodes = _topo_sort(run.template_snapshot)
    label_of(fn) = (spec = try _task_spec(_task_from_fun_name(fn)) catch; nothing end;
                    spec isa AbstractDict ? string(get(spec, "label", fn)) : fn)
    dup = Set(l for l in (label_of(n.fn) for n in nodes) if count(==(l), label_of.(getfield.(nodes, :fn))) > 1)
    for (i, n) in enumerate(nodes)
        counts = Dict{String,Int}(); reasons = Set{String}(); ids = Set{String}()
        for uid in run.image_uids
            st = get(get(run.image_states, uid, Dict{String,Any}()), n.id, nothing)
            s = st === nothing ? "not reached" : string(st.status)
            counts[s] = get(counts, s, 0) + 1
            st !== nothing && st.reason !== nothing && push!(reasons, st.reason)
            st !== nothing && st.task_id !== nothing && push!(ids, st.task_id)
        end
        status = join(("$s $c/$N" for (s, c) in sort!(collect(counts); by = first)), ", ")
        isempty(reasons) || (status *= " ($(join(sort!(collect(reasons)), "; ")))")
        es = [e for id in ids for (_, e) in get(ents, id, Tuple{String,Any}[])]
        ats = filter(!isnothing, [_mr_time(_rl_str(e, "at")) for e in es])
        fins = filter(!isnothing, [_mr_time(_rl_str(e, "finishedAt")) for e in es])
        a = isempty(ats) ? nothing : minimum(ats); b = isempty(fins) ? nothing : maximum(fins)
        label = label_of(n.fn)
        label in dup && (label *= " (node $(n.id))")
        push!(md, "| $i | $(_mr_md(label)) | `$(n.fn)` | $(string(n.scope)) | $(_mr_md(_mr_node_params(run, n))) | $(_mr_md(status)) | " *
                  "$(_mr_fmt(a)) | $(_mr_fmt(b)) | $(_mr_took(a, b)) |")
    end
    push!(md, "", "Times are the run log's local wall-clock times; a blank time was not recorded.", "")

    push!(md, "### Cohort QC", "")
    # the set's stored cohort sidecars for the run's steps. A check is per set and the record keeps no
    # check time per sidecar (`_checks.json` is one stamp per fun), so the text never dates it
    funs = Set{String}(n.fn for n in run.template_snapshot.nodes)
    want = Set(run.image_uids); any_doc = false
    for set in sort(sets(proj); by = s -> s.uid)
        any(in(want), set.image_uids) || continue
        docs = read_all_cohort_qc(set)
        for k in sort!(collect(keys(docs)))
            d = JSON3.read(JSON3.write(docs[k]), Dict{String,Any})
            fun = string(get(d, "funName", ""))
            fun in funs || continue
            any_doc = true
            push!(md, "Set \"$(set.name)\", its latest stored check of `$k` (not necessarily from this run):")
            lines = cohort_qc_summary_lines(d; name_of = u -> get(names, u, u))
            for l in lines
                push!(md, "- " * strip(replace(l, r"^\s+" => "")))
            end
        end
    end
    any_doc || push!(md, "No cohort check is recorded for these steps.")
    push!(md, "")
    md
end
