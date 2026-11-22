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

# The spec's optional `methodsText` with `{key}` slots filled from the run's params over the spec
# defaults; `nothing` (the generic sentence is used) when the spec has none, a slot has no value, or the
# images disagree on a slot (`conflicts` — a sentence must not state one image's value as the method).
function _mr_methods_text(fn::AbstractString, entries, conflicts::AbstractString = "")
    spec = try _task_spec(_task_from_fun_name(String(fn))) catch; nothing end
    spec isa AbstractDict || return nothing
    tpl = get(spec, "methodsText", nothing)
    tpl isa AbstractString && !isempty(tpl) || return nothing
    vals = Dict{String,Any}(k => v for (k, v) in _history_param_defaults(String(fn)) if !(v isa NamedTuple))
    done = sort([e for e in entries if run_entry_status(e) == "done"]; by = e -> _rl_str(e, "at"))
    isempty(done) || merge!(vals, run_log_row(last(done)).params)
    disputed = Set(String(strip(first(split(c, ":")))) for c in split(conflicts, "; ") if occursin(":", c))
    out = tpl
    for m in eachmatch(r"\{([A-Za-z0-9_]+)\}", tpl)
        k = String(m.captures[1])
        k in disputed && return nothing
        haskey(vals, k) && !_hp_empty(vals[k]) || return nothing
        out = replace(out, "{$k}" => _hp_show(vals[k]))
    end
    out
end

# One numbered step: the spec's `methodsText` when it has one, else the generic sentence.
function _mr_step(i::Int, row, entries)::Vector{String}
    task = string(get(row, "task", "")); step = string(get(row, "step", task))
    out, imgs = string(get(row, "output", "")), string(get(row, "images", ""))
    changed, note = string(get(row, "changedFromDefault", "")), string(get(row, "note", ""))
    head = isempty(task) ? "**$step**" : "**$step** (`$task`)"
    custom = isempty(task) ? nothing : _mr_methods_text(task, entries, string(get(row, "conflicts", "")))
    s = if custom !== nothing
        "$head: $custom" * (isempty(imgs) ? "" : " ($imgs images.)")
    else
        body = isempty(imgs) ? head : "$head was run on $imgs images"
        isempty(out) || (body *= ", writing `$out`")
        if !isempty(changed)
            body *= ", with these parameters changed from their defaults: $changed"
        elseif isempty(note) && !isempty(task)
            body *= ", with default parameters"
        end
        body * "."
    end
    isempty(note) || (s *= " Note: $note.")
    c = string(get(row, "conflicts", ""))
    isempty(c) || (s *= " The images differ in: $c.")
    lines = ["$i. $s"]
    ai = methods_ai_phrase(_rl_get(e, "author", nothing) for e in entries)
    isempty(ai) || push!(lines, "   - Started by $ai.")
    cite = _mr_cite(row)
    isempty(cite) || push!(lines, "   - Cite: $cite")
    lines
end

# A row's `cite` (task citation DOIs, once the table carries them) as text — a list or a string
_mr_cite(row) = (c = get(row, "cite", ""); c isa AbstractVector ? join(string.(c), "; ") : string(something(c, "")))

# A population's Claude stamp on the board's images, as a phrase ("" when nobody stamped it)
function _mr_pop_ai(proj::CciaProject, uids, key::AbstractString)
    t = _parse_tkey(key)
    info = pop_type_info(t.popType)
    (info === nothing || info.tree_type === nothing || isempty(t.pop)) && return ""
    stamps = Any[]; edited = false
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage || continue
        m = try stored_pop_tree(img, t.valueName, string(info.tree_type)) catch; nothing end
        (m === nothing || !has_pop(m, t.pop)) && continue
        p = pop_at(m, t.pop)
        p.created_by === nothing && continue
        push!(stamps, p.created_by)
        u = p.updated_by
        edited |= u !== nothing && !haskey(_METHODS_AI_VIAS, _mr_str(u, "via"))
    end
    ai = methods_ai_phrase(stamps)
    isempty(ai) ? "" : "drawn by $ai" * (edited ? ", then edited by hand" : "")
end

# The images a scope covers, and how the scope reads
function _mr_scope(proj::CciaProject, image_uids, set_uid, pops, run)
    if run !== nothing
        return run.image_uids, "chain run `$(run.id)` (\"$(run.chain_name)\")"
    elseif !isempty(set_uid)
        s = findfirst(st -> st.uid == set_uid, sets(proj))
        s === nothing && error("set not found: $set_uid")
        st = sets(proj)[s]
        return String[i.uid for i in images(st)], "set \"$(st.name)\""
    end
    String.(image_uids), pops === nothing ? "selected images" : "analysis board"
end

# ── the parameter table as CSV ──
# Columns in a fixed order, `relation` only when a row says one — the same columns and quoting as the
# frontend's `historyParamsCsv` + `rowsToCsv` (utils/chainFromHistory.ts, plots/export.ts), which the
# history dialog and the board export still use. The Methods modal and the RO-Crate take this one, so
# the CSV a user downloads and the one in the crate are the same bytes.
const _MR_CSV_COLS = [("step", "step"), ("task", "task"), ("output", "output"), ("relation", "relation"),
                      ("images", "images"), ("last_run", "lastRun"), ("changed_from_default", "changedFromDefault"),
                      ("differs_between_images", "conflicts"), ("note", "note")]
_mr_csv_cell(v) = (s = v === nothing ? "" : string(v);
                   occursin(r"[\",\n]", s) ? "\"" * replace(s, "\"" => "\"\"") * "\"" : s)

"""
    history_params_csv(table) -> String

The Methods parameter table (`history_params_table` rows) as CSV, "" for no rows. `relation` appears
only when some row carries one (a board-scoped table).
"""
function history_params_csv(table::AbstractVector)::String
    isempty(table) && return ""
    scoped = any(r -> !isempty(string(something(get(r, "relation", ""), ""))), table)
    cols = [c for c in _MR_CSV_COLS if scoped || c[1] != "relation"]
    lines = [join(first.(cols), ",")]
    for r in table
        push!(lines, join((_mr_csv_cell(get(r, k, "")) for (_, k) in cols), ","))
    end
    join(lines, "\n")
end

"""
    methods_report(proj; image_uids, set_uid, pops, cluster_runs, run_id, board) -> Dict

A draft Methods section, `{markdown, table, csv, scope}`. Three scopes, one text:

- **set** — `set_uid` (or a list of `image_uids`): every step that ran on its images;
- **board** — `image_uids` + `pops` (board population keys) + `cluster_runs` (`"popType:suffix"`),
  as the board's Parameters CSV takes them; `board` names the tab, for its creator stamp;
- **chain run** — `run_id`: the run's images, the runs its nodes made (`history_params_table` cut to
  their task ids), each node's status and timing (the run log's `at`/`finishedAt`, joined by task id),
  and the cohort QC last checked on its sets.

`table` is the parameter table the text is written from (`history_params_table` rows, `cite` included
when the rows carry it), `csv` that table as CSV (`history_params_csv`). Deterministic: no clock, no randomness, every list sorted. Read-only.
"""
function methods_report(proj::CciaProject; image_uids::AbstractVector = String[],
                        set_uid::AbstractString = "", pops::Union{Nothing,AbstractVector} = nothing,
                        cluster_runs::AbstractVector = String[], run_id::AbstractString = "",
                        board::AbstractString = "")::Dict{String,Any}
    run = isempty(run_id) ? nothing : load_chain_run(proj, String(run_id))
    uids, scope = _mr_scope(proj, image_uids, set_uid, pops, run)
    run === nothing && pops !== nothing && !isempty(board) && (scope = "analysis board \"$board\"")
    isempty(uids) && error("no images in scope")
    task_ids = run === nothing ? nothing :
        sort!(unique(String[st.task_id for m in values(run.image_states) for st in values(m) if st.task_id !== nothing]))
    table = run !== nothing ? history_params_table(proj, uids; task_ids) :
            pops !== nothing ? history_params_table(proj, uids; pops = String.(pops), cluster_runs = String.(cluster_runs)) :
            history_params_table(proj, uids)
    ents = _mr_entries(proj, uids)
    names = Dict{String,String}()
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        names[uid] = img isa CciaImage ? img.name : uid
    end

    md = String["> " * METHODS_DRAFT_BANNER, "", "## Methods (draft)", ""]
    push!(md, "Scope: $scope, $(length(uids)) image$(length(uids) == 1 ? "" : "s") in project \"$(proj.name)\".", "")

    # ── steps ──
    push!(md, "### Image analysis", "")
    used = [e for row in table for e in _mr_row_entries(row, ents)]
    versions = sort!(unique(String[_rl_str(e, "ceceliaVersion") for e in used if !isempty(_rl_str(e, "ceceliaVersion"))]))
    commits  = sort!(unique(String[_rl_str(e, "ceceliaCommit") for e in used if !isempty(_rl_str(e, "ceceliaCommit"))]))
    sw = isempty(versions) ? "Cecelia (version not recorded on these runs)" :
         "Cecelia $(join(versions, ", "))" * (isempty(commits) ? "" : " (commit $(join(commits, ", ")))")
    if isempty(table)
        push!(md, "No recorded step ran on these images.", "")
    else
        push!(md, "Images were processed in $sw, in this order:", "")
        for (i, row) in enumerate(table)
            append!(md, _mr_step(i, row, _mr_row_entries(row, ents)))
        end
        push!(md, "")
    end

    ai_lines = String[]
    for (i, row) in enumerate(table)
        ai = methods_ai_phrase(_rl_get(e, "author", nothing) for e in _mr_row_entries(row, ents))
        isempty(ai) || push!(ai_lines, "- Step $i ($(get(row, "step", ""))): started by $ai.")
    end

    # ── board: what it shows ──
    if pops !== nothing
        push!(md, "### Populations", "")
        isempty(pops) && push!(md, "The board shows no population.")
        for k in sort!(unique(String.(pops)))
            t = _parse_tkey(k)
            ai = _mr_pop_ai(proj, uids, k)
            push!(md, "- `$(t.valueName)$(t.pop)` ($(t.popType))" * (isempty(ai) ? "" : ": $ai"))
            isempty(ai) || push!(ai_lines, "- Population `$(t.valueName)$(t.pop)`: $ai.")
        end
        for r in sort!(unique(String.(cluster_runs)))
            push!(md, "- Cluster run `$r`")
        end
        push!(md, "")
        if !isempty(board)
            doc = read_boards_doc(boards_doc_path(proj))
            for t in doc.tabs
                (t isa AbstractDict && _mr_str(t, "name") == board) || continue
                ai = methods_ai_phrase([get(t, :createdBy, get(t, "createdBy", nothing))])
                isempty(ai) || push!(ai_lines, "- Board \"$board\": created by $ai.")
            end
        end
    end

    # ── chain run: nodes, status, timing, cohort QC ──
    run === nothing || append!(md, _mr_chain(proj, run, ents, names, ai_lines))

    # ── AI assistance ──
    push!(md, "### AI assistance", "")
    isempty(ai_lines) ? push!(md, "No step, population or board in this record is stamped as made by Claude.") :
                        append!(md, ai_lines)
    unstamped = count(e -> _rl_get(e, "author", nothing) === nothing, used)
    unstamped > 0 && push!(md, "", "$unstamped run$(unstamped == 1 ? "" : "s") predate author stamps: who started them is not recorded.")
    push!(md, "", METHODS_CEILING, "")

    # ── software + table ──
    cites = sort!(unique(String[_mr_cite(r) for r in table if !isempty(_mr_cite(r))]))
    push!(md, "### Software", "", "- $sw.")
    isempty(cites) || push!(md, "- Cite: $(join(cites, "; ")).")
    push!(md, "", "The supplementary parameter table (CSV) lists the same $(length(table)) step$(length(table) == 1 ? "" : "s").")

    Dict{String,Any}("markdown" => join(md, "\n") * "\n", "table" => table, "csv" => history_params_csv(table),
                     "scope" => Dict{String,Any}("kind" => run !== nothing ? "chainRun" : pops !== nothing ? "board" : "set",
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
        startswith(k, "_") && continue
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
