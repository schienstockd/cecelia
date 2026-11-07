# Claim recital routes — the mechanical checks (S0 record, S1 consistency, S2 conventions) for a claim's
# scope over a set. Read-only: nothing is written. Plan: docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md.

# The scope from query params, or a (400, body) tuple. `pops` is comma-separated pop paths
# ("B/qc,T/qc"); `measure` a measure column ("live.track.speed"); `unitAttrs` the image attributes
# whose values together name one replicate ("Mouse" or "Mouse,Location"), optional.
function _claim_scope_from_query(q::AbstractDict)
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    pops = _query_list(q, "pops")
    measure = strip(get(q, "measure", ""))
    (isempty(project_uid) || isempty(set_uid) || isempty(pops) || isempty(measure)) &&
        return (400, JSON3.write((; error = "projectUid, setUid, pops and measure required")))
    set = _cohort_set(project_uid, set_uid)
    set isa Tuple && return set
    ClaimScope(set, pops, measure; unit_attrs = _query_list(q, "unitAttrs"))
end

# GET /api/claim/checks?projectUid&setUid&pops&measure[&unitAttrs][&rows=1]
# → {findings, checks, ran, skipped, notCompared, nRows, nUsed[, rows]}. `rows` (the per-(image, population)
# record table the rules read) only on request: it carries every flattened param and is large.
function api_claim_checks(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    scope = _claim_scope_from_query(q)
    scope isa Tuple && return scope
    out = try
        claim_checks(scope)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    get(q, "rows", "") in ("1", "true") || delete!(out, "rows")
    200, JSON3.write(out)
end

# ── Claim entries ────────────────────────────────────────────────────────────────────────────────
# A stated claim is a Blackboard entry carrying a `claim` meta key (a passthrough field, like
# `agentRun`): its scope, its rounds and the last check. The checks' results are plain lines grouped
# by category (mechanical, so nothing to mark); only the model review's points are `### sNN · …`
# sections the analyst marks Good / Bad / Unsure. Every warning still keeps an id across re-checks
# (`findingIds`) — that is how `closed` and the briefing's open claims track it; a finding the record
# no longer shows drops out of the text, which is how it closes — nobody closes one by hand. Rewording the claim or changing its scope starts a new round; every round stays on
# the entry.
#
#   claim = {setUid, populations, measure, unitAttrs, text, round, rounds: [{round, at, by, text,
#            setUid, populations, measure, unitAttrs}], findingIds: {key: "s01"},
#            lastCheck: {at, ran, skipped, open: ["s01", …]}, source?}

# What identifies a finding across re-checks: its code and what it is about, not its counts.
const _CLAIM_KEY_SUBS = ("pop", "kind", "attr", "variable", "measure")
function _claim_finding_key(f)::String
    subs = get(f, "subs", Dict{String,Any}())
    f["code"] * "|" * join((string(k, "=", get(subs, k, "")) for k in _CLAIM_KEY_SUBS), "|")
end


# The section id for `key`: the one it had, else the next number after every id the claim has used.
function _claim_section_id!(ids::AbstractDict, key::AbstractString)::String
    haskey(ids, key) && return String(ids[key])
    n = maximum((parse(Int, String(v)[2:end]) for v in values(ids)); init = 0)
    ids[key] = "s" * lpad(n + 1, 2, '0')
end

# Give every warning a section id, reusing the one its key had.
function _claim_assign_ids!(ids::AbstractDict, findings)
    for f in findings
        f["level"] == "warn" && (f["sectionId"] = _claim_section_id!(ids, _claim_finding_key(f)))
    end
    ids
end

# A claim's replicate attributes, from an entry or a request body: `unitAttrs` (a list), or the older
# single `unitAttr` string; normalised by `claim_unit_attrs`.
function _claim_unit_attrs(c)::Vector{String}
    field(k) = something(get(c, k, nothing), get(c, Symbol(k), nothing), Some(nothing))   # String- or Symbol-keyed
    claim_unit_attrs(something(field("unitAttrs"), field("unitAttr"), Some(nothing)))
end

function _claim_scope_text(c)
    ua = _claim_unit_attrs(c)
    string(join(c["populations"], " vs "), " · `", c["measure"], "` · ",
           isempty(ua) ? "no replicate named" : "per " * join(ua, " × "), " · set `", c["setUid"], "`")
end

# A review point's category label ("The data"); an unknown id reads as Other.
_claim_category_label(id) = Cecelia.qc_text("claim.cat." * (id in Cecelia.CLAIM_CATEGORIES ? id : "other")).short

# The entry's text for a claim and its latest check: a list of facts grouped by what they are about
# (`result["categories"]`). A problem is one line — the fact, then → what to do. The checks are
# mechanical, so they are plain lines; only the model review's points are `### sNN` sections, for the
# analyst to mark Good / Bad / Unsure. No timestamp in it: a re-check that finds the same thing leaves
# the text byte-identical, so the revise is a no-op and no snapshot is spent.
function _claim_entry_md(c::AbstractDict, result::AbstractDict)::String
    io = IOBuffer()
    r = Int(get(c, "round", 1))
    println(io, "# ", c["text"], "\n")
    println(io, _claim_scope_text(c), r > 1 ? " · round $r" : "", "\n")
    mark = Dict("warn" => "⚠", "ok" => "✓", "skipped" => "–")
    lines = get(result, "checks", Any[])
    for cat in get(result, "categories", Any[])
        println(io, "## ", cat["label"], " ", mark[cat["state"]], "\n")
        for l in filter(l -> l["category"] == cat["id"], lines)
            fs = filter(f -> get(f["detail"], "check", "") == l["id"], result["findings"])
            for f in filter(f -> f["level"] == "warn", fs)
                println(io, "- ⚠ ", f["short"], " → ", f["long"])
            end
            l["state"] == "ok"      && println(io, "- ✓ ", l["text"])
            l["state"] == "skipped" && println(io, "- – Not checked: ", l["text"])
            for f in filter(f -> f["level"] != "warn" && f["code"] != "claim.unit_count", fs)
                println(io, "- ", f["short"])
            end
        end
        println(io)
    end
    rv = get(c, "review", nothing)
    println(io, "## Model review\n")
    if !(rv isa AbstractDict)
        println(io, "Not run.\n")
    else
        err = get(rv, "lastError", "")
        isempty(err) || println(io, "⚠ The last run failed: ", err, "\n")
        done = get(rv, "status", "") == "done"
        done && println(io, "One ", isempty(get(rv, "model", "")) ? "" : rv["model"] * " ",
                        "turn that read the claim and the record but not your notes. Mark each point Good, Bad or Unsure.\n")
        rs = filter(x -> x["kind"] != "interpretation", rv["claims"])
        done && isempty(rs) && println(io, get(rv, "abstain", false) ? "Nothing raised. " * get(rv, "note", "") : "Nothing beyond the checks.", "\n")
        # grouped like the checks: a `### <Category>` heading (not a section — it has no id) closes the
        # previous point's section, then that category's points follow
        cat_of(x) = (k = get(x, "category", "other"); k in Cecelia.CLAIM_CATEGORIES ? k : "other")
        for cat in Cecelia.CLAIM_CATEGORIES
            xs = filter(x -> cat_of(x) == cat, rs)
            isempty(xs) && continue
            println(io, "### ", _claim_category_label(cat), "\n")
            for x in xs
                act = get(x, "action", "")
                println(io, "### ", x["sectionId"], " · ", x["text"], isempty(act) ? "" : " → " * act, "\n")
            end
        end
        for x in filter(x -> x["kind"] == "interpretation", rv["claims"])
            println(io, "## What the record would support\n\n", x["text"], "\n")
        end
    end
    if r > 1
        println(io, "## Earlier rounds\n")
        for x in c["rounds"][1:end-1]
            println(io, "- round ", x["round"], ": “", x["text"], "” (", _claim_scope_text(x), ")")
        end
        println(io)
    end
    String(take!(io))
end

# Run the checks for a claim meta dict; the scope's set is looked up fresh.
function _claim_run(uid::AbstractString, c::AbstractDict)
    set = _cohort_set(uid, c["setUid"])
    set isa Tuple && return set
    claim_checks(ClaimScope(set, String.(c["populations"]), c["measure"]; unit_attrs = _claim_unit_attrs(c)))
end

function _claim_last_check(result)
    Dict{String,Any}("at" => string(Dates.now()), "ran" => result["ran"], "skipped" => result["skipped"],
        "open" => sort!(String[f["sectionId"] for f in result["findings"] if haskey(f, "sectionId")]))
end

# Re-check, rewrite the entry text (a no-op revise when nothing changed) and record the check.
function _claim_write!(uid::AbstractString, id::AbstractString, c::AbstractDict, result)
    st, body = api_blackboard_revise(Vector{UInt8}(JSON3.write(Dict(
        "projectUid" => uid, "entryId" => id, "content" => _claim_entry_md(c, result)))))
    st == 200 || return st, body
    c["lastCheck"] = _claim_last_check(result)
    _set_bb_meta_field!(uid, id, "claim", c)
    200, body
end

# The reply to a state or a check: the findings, the checks as facts, and the model review if it ran.
_claim_reply(id, c, result; closed = String[]) = JSON3.write((; ok = true, entryId = id, round = c["round"],
    text = c["text"], findings = result["findings"], checks = result["checks"], categories = result["categories"],
    categoryLabels = Dict(id => _claim_category_label(id) for id in Cecelia.CLAIM_CATEGORIES),
    reviewModel = observer_default_model(),   # the model a review run would use
    ran = result["ran"],
    skipped = result["skipped"], review = get(c, "review", nothing), closed))

function _claim_from_body(body)
    pops = get(body, :populations, nothing)
    pops isa AbstractVector || return nothing
    # trimmed like `_claim_scope_from_query`, so a scope means the same thing from either route
    Dict{String,Any}("setUid" => String(strip(_wstr(body, :setUid))),
                     "populations" => String[String(strip(string(p))) for p in pops if !isempty(strip(string(p)))],
                     "measure" => String(strip(_wstr(body, :measure))), "unitAttrs" => _claim_unit_attrs(body),
                     "text" => String(strip(String(get(body, :text, "")))))
end
const _CLAIM_SCOPE_KEYS = ("text", "setUid", "populations", "measure", "unitAttrs")
_claim_same_round(a, b) = all(get(a, k, nothing) == get(b, k, nothing) for k in _CLAIM_SCOPE_KEYS)

"""
    POST /api/claim/state

Body: `{ projectUid, setUid, populations: [pop, …], measure, unitAttrs?: [attr, …], text, entryId?, source? }`
Reply: `{ ok, entryId, round, text, findings, checks, categories, categoryLabels, reviewModel, ran, skipped, review, closed }`

States a claim: runs the checks and writes them as a Blackboard entry (one line per check, grouped
by category). With `entryId` it restates an existing claim: a changed text or scope starts a new round,
the same one just re-checks. `source` (e.g. `{boardId, panelId}`) records where it was stated.
"""
function api_claim_state(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid); id = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    new = _claim_from_body(body)
    (new === nothing || isempty(new["setUid"]) || isempty(new["populations"]) || isempty(new["measure"]) ||
     isempty(new["text"])) && return 400, JSON3.write((; error = "setUid, populations, measure and text required"))
    src = get(body, :source, nothing)
    src isa AbstractDict && (new["source"] = JSON3.read(JSON3.write(src), Dict{String,Any}))
    by = author_stamp(); at = string(Dates.now())
    round_rec(c, r) = merge(Dict{String,Any}(k => c[k] for k in _CLAIM_SCOPE_KEYS),
                            Dict{String,Any}("round" => r, "at" => at, "by" => by))
    # a restate of a claim whose entry was deleted states it afresh — the plot still holds the old id
    !isempty(id) && _valid_bb_entry_id(id) && _read_bb_meta(uid, id) === nothing && (id = "")
    if isempty(id)
        c = new
        c["round"] = 1; c["rounds"] = Any[round_rec(c, 1)]; c["findingIds"] = Dict{String,Any}()
        result = _claim_run(uid, c)
        result isa Tuple && return result
        _claim_assign_ids!(c["findingIds"], result["findings"])
        st, rb = api_blackboard_create(Vector{UInt8}(JSON3.write(Dict(
            "projectUid" => uid, "title" => c["text"], "content" => _claim_entry_md(c, result)))))
        st == 200 || return st, rb
        id = String(JSON3.read(rb).entryId)
        c["lastCheck"] = _claim_last_check(result)
        _set_bb_meta_field!(uid, id, "claim", c)
    else
        _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
        meta = _read_bb_meta(uid, id)
        meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
        old = get(meta, "claim", nothing)
        old isa AbstractDict || return 400, JSON3.write((; error = "Not a claim entry"))
        c = _to_str_dict(old)
        # the single-attribute form, from before a replicate could combine several
        c["unitAttrs"] = _claim_unit_attrs(c); delete!(c, "unitAttr")
        if !_claim_same_round(c, new)
            for k in _CLAIM_SCOPE_KEYS; c[k] = new[k]; end
            c["round"] = Int(get(c, "round", 1)) + 1
            delete!(c, "review")   # it answered the earlier round's question
            push!(c["rounds"], round_rec(c, c["round"]))
        end
        result = _claim_run(uid, c)
        result isa Tuple && return result
        _claim_assign_ids!(c["findingIds"], result["findings"])
        st, rb = _claim_write!(uid, id, c, result)
        st == 200 || return st, rb
    end
    200, _claim_reply(id, c, result)
end

"""
    POST /api/claim/check

Body: `{ projectUid, entryId }` — Reply: as `/api/claim/state`, with `closed` (section ids the record no longer shows)

Re-runs a claim's checks against the current record and rewrites the entry (a no-op when nothing
changed). `closed` lists the section ids open at the last check that the record no longer shows.
"""
function api_claim_check(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid); id = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
    old = get(meta, "claim", nothing)
    old isa AbstractDict || return 400, JSON3.write((; error = "Not a claim entry"))
    c = _to_str_dict(old)
    was = String.(get(get(c, "lastCheck", Dict()), "open", String[]))
    result = _claim_run(uid, c)
    result isa Tuple && return result
    _claim_assign_ids!(c["findingIds"], result["findings"])
    st, rb = _claim_write!(uid, id, c, result)
    st == 200 || return st, rb
    closed = setdiff(was, c["lastCheck"]["open"])
    200, _claim_reply(id, c, result; closed)
end

"""
    GET /api/claim/open?projectUid

The project's open claim entries, each re-checked against the current record WITHOUT writing:
`{claims: [{entryId, title, round, open: [{sectionId, short}], closed: [sectionId], new: [short]}]}`.
`closed` were open at the last check and are gone now; `new` are findings the last check didn't have.
For the session briefing: what still applies, and what fixed itself since.
"""
function api_claim_open(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(q, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    out = Any[]
    for (id, e) in _read_bb_registry(uid)
        get(e, "status", "open") == "open" || continue
        meta = _read_bb_meta(uid, id)
        (meta === nothing || !(get(meta, "claim", nothing) isa AbstractDict)) && continue
        c = _to_str_dict(meta["claim"])
        ids = _to_str_dict(get(c, "findingIds", nothing))
        result = try _claim_run(uid, c) catch; nothing end
        (result === nothing || result isa Tuple) && continue
        _claim_assign_ids!(ids, result["findings"])
        warns = filter(f -> haskey(f, "sectionId"), result["findings"])
        was = String.(get(get(c, "lastCheck", Dict()), "open", String[]))
        now_ids = String[f["sectionId"] for f in warns]
        push!(out, Dict{String,Any}("entryId" => id, "title" => get(meta, "title", ""), "round" => get(c, "round", 1),
            "open" => [Dict("sectionId" => f["sectionId"], "short" => f["short"]) for f in warns],
            "closed" => sort!(setdiff(was, now_ids)),
            "new" => String[f["short"] for f in warns if !(f["sectionId"] in was)]))
    end
    sort!(out; by = r -> r["entryId"], rev = true)
    200, JSON3.write((; claims = out))
end
