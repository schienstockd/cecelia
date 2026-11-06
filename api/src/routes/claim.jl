# Claim recital routes — the mechanical checks (S0 record, S1 consistency, S2 conventions) for a claim's
# scope over a set. Read-only: nothing is written. Plan: docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md.

# The scope from query params, or a (400, body) tuple. `pops` is comma-separated pop paths
# ("B/qc,T/qc"); `measure` a measure column ("live.track.speed"); `unitAttr` the image attribute that
# names the statistical unit ("Mouse"), optional.
function _claim_scope_from_query(q::AbstractDict)
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    pops = _query_list(q, "pops")
    measure = strip(get(q, "measure", ""))
    (isempty(project_uid) || isempty(set_uid) || isempty(pops) || isempty(measure)) &&
        return (400, JSON3.write((; error = "projectUid, setUid, pops and measure required")))
    set = _cohort_set(project_uid, set_uid)
    set isa Tuple && return set
    ClaimScope(set, pops, measure; unit_attr = strip(get(q, "unitAttr", "")))
end

# GET /api/claim/checks?projectUid&setUid&pops&measure[&unitAttr][&rows=1]
# → {findings, ran, skipped, notCompared, nRows, nUsed[, rows]}. `rows` (the per-(image, population)
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
# `agentRun`): its scope, its rounds and the last check. The check's warnings are the entry's
# `### sNN · …` sections, so the analyst marks each one Good / Bad / Unsure with the existing section
# verdicts. A finding keeps its section id across re-checks (`findingIds`), so a verdict stays on it;
# a finding the record no longer shows drops out of the text, which is how it closes — nobody closes
# one by hand. Rewording the claim or changing its scope starts a new round; every round stays on
# the entry.
#
#   claim = {setUid, populations, measure, unitAttr, text, round, rounds: [{round, at, by, text,
#            setUid, populations, measure, unitAttr}], findingIds: {key: "s01"},
#            lastCheck: {at, ran, skipped, open: ["s01", …]}, source?}

# What identifies a finding across re-checks: its code and what it is about, not its counts.
const _CLAIM_KEY_SUBS = ("pop", "kind", "attr", "variable", "measure")
function _claim_finding_key(f)::String
    subs = get(f, "subs", Dict{String,Any}())
    f["code"] * "|" * join((string(k, "=", get(subs, k, "")) for k in _CLAIM_KEY_SUBS), "|")
end

const _CLAIM_STAGE_LABEL = Dict("S0" => "Record", "S1" => "Consistency", "S2" => "Conventions")

# Give every warning a section id, reusing the one its key had; new keys take the next number.
function _claim_assign_ids!(ids::AbstractDict, findings)
    n = maximum((parse(Int, String(v)[2:end]) for v in values(ids)); init = 0)
    for f in findings
        f["level"] == "warn" || continue
        k = _claim_finding_key(f)
        haskey(ids, k) || (n += 1; ids[k] = "s" * lpad(n, 2, '0'))
        f["sectionId"] = String(ids[k])
    end
    ids
end

function _claim_evidence_line(ev)::String
    ev isa AbstractDict || return ""
    imgs = get(ev, "images", nothing)
    imgs isa AbstractVector && !isempty(imgs) && return "Images: " * join(string.(imgs), ", ")
    imgs isa AbstractDict && !isempty(imgs) && return "Images: " * join(sort!(string.(collect(keys(imgs)))), ", ")
    d = get(ev, "imagesDiffering", nothing)
    d isa AbstractVector && !isempty(d) && return "Images: " * join(string.(d), ", ")
    ""
end

_claim_scope_text(c) = string("set `", c["setUid"], "` · ", join(c["populations"], " vs "), " · `", c["measure"], "`",
                              isempty(get(c, "unitAttr", "")) ? "" : " · unit: " * c["unitAttr"])

# The entry's text for a claim and its latest check. No timestamp in it: a re-check that finds the
# same thing leaves the text byte-identical, so the revise is a no-op and no snapshot is spent.
function _claim_entry_md(c::AbstractDict, result::AbstractDict)::String
    io = IOBuffer()
    println(io, "# ", c["text"], "\n")
    println(io, "Scope: ", _claim_scope_text(c), "  ")
    r = Int(get(c, "round", 1))
    if r > 1
        println(io, "Round ", r, ". Earlier:  ")
        for x in c["rounds"][1:end-1]
            println(io, "- round ", x["round"], ": “", x["text"], "” (", _claim_scope_text(x), ")")
        end
    end
    println(io)
    println(io, "Checked: ", join(result["ran"], ", "), "  ")
    sk = result["skipped"]
    println(io, "Not checked: ", join(vcat([string(k, " (", sk[k], ")") for k in sort!(collect(keys(sk)))],
                                           ["review (not run)"]), ", "), "\n")
    warns = filter(f -> f["level"] == "warn", result["findings"])
    infos = filter(f -> f["level"] != "warn", result["findings"])
    println(io, "## Findings\n")
    isempty(warns) && println(io, "None from the checks that ran.\n")
    for f in sort(warns; by = f -> f["sectionId"])
        st = get(get(f, "detail", Dict()), "stage", "")
        println(io, "### ", f["sectionId"], " · ", get(_CLAIM_STAGE_LABEL, st, st), " · ", f["short"], "\n")
        println(io, f["long"])
        ev = _claim_evidence_line(get(get(f, "detail", Dict()), "evidence", nothing))
        prov = get(get(f, "detail", Dict()), "provenance", "")
        line = join(filter(!isempty, [ev, isempty(prov) ? "" : prov * " from the record"]), " · ")
        isempty(line) || println(io, "\n", line)
        println(io)
    end
    if !isempty(infos)
        println(io, "## Notes\n")
        for f in infos
            println(io, "- ", f["short"])
        end
    end
    String(take!(io))
end

# Run the checks for a claim meta dict; the scope's set is looked up fresh.
function _claim_run(uid::AbstractString, c::AbstractDict)
    set = _cohort_set(uid, c["setUid"])
    set isa Tuple && return set
    claim_checks(ClaimScope(set, String.(c["populations"]), c["measure"]; unit_attr = get(c, "unitAttr", "")))
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

function _claim_from_body(body)
    pops = get(body, :populations, nothing)
    pops isa AbstractVector || return nothing
    # trimmed like `_claim_scope_from_query`, so a scope means the same thing from either route
    Dict{String,Any}("setUid" => String(strip(_wstr(body, :setUid))),
                     "populations" => String[String(strip(string(p))) for p in pops if !isempty(strip(string(p)))],
                     "measure" => String(strip(_wstr(body, :measure))), "unitAttr" => String(strip(_wstr(body, :unitAttr))),
                     "text" => String(strip(String(get(body, :text, "")))))
end
_claim_same_round(a, b) = all(a[k] == b[k] for k in ("text", "setUid", "populations", "measure", "unitAttr"))

"""
    POST /api/claim/state

Body: `{ projectUid, setUid, populations: [pop, …], measure, unitAttr?, text, entryId?, source? }`
Reply: `{ ok, entryId, round, findings, ran, skipped }`

States a claim: runs the checks and writes them as a Blackboard entry (S0–S2 warnings as `### sNN`
sections). With `entryId` it restates an existing claim: a changed text or scope starts a new round,
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
    round_rec(c, r) = merge(Dict{String,Any}(k => c[k] for k in ("text", "setUid", "populations", "measure", "unitAttr")),
                            Dict{String,Any}("round" => r, "at" => at, "by" => by))
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
        if !_claim_same_round(c, new)
            for k in ("text", "setUid", "populations", "measure", "unitAttr"); c[k] = new[k]; end
            c["round"] = Int(get(c, "round", 1)) + 1
            push!(c["rounds"], round_rec(c, c["round"]))
        end
        result = _claim_run(uid, c)
        result isa Tuple && return result
        _claim_assign_ids!(c["findingIds"], result["findings"])
        st, rb = _claim_write!(uid, id, c, result)
        st == 200 || return st, rb
    end
    200, JSON3.write((; ok = true, entryId = id, round = c["round"], findings = result["findings"],
                      ran = result["ran"], skipped = result["skipped"]))
end

"""
    POST /api/claim/check

Body: `{ projectUid, entryId }` — Reply: `{ ok, entryId, round, findings, ran, skipped, closed }`

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
    200, JSON3.write((; ok = true, entryId = id, round = c["round"], findings = result["findings"],
                      ran = result["ran"], skipped = result["skipped"], closed = closed))
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
