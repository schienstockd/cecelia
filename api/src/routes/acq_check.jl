# Test-acquisition check routes — the set-level check (`acq_check`, app/src/acq_check.jl) and its plan.
# Plan: docs/todo/ACQUISITION_TEST_CHECK_PLAN.md.
#
# The plan for a set is a Blackboard entry carrying an `acqCheck` meta key (a passthrough, like
# `claim`): the set, the plan, its rounds and the last check. One entry per set, found by its key.
# The check itself reads image meta only, so GET runs it live and writes nothing; saving a plan writes
# the entry (a new round when the plan changed), and its text is the latest check as plain lines.
#
#   acqCheck = {setUid, plan, round, rounds: [{round, at, by, plan}], lastCheck: {at, open: [code|scope]},
#               review?, reviewIds?: {key: "s01"}}
#
# `review` is the model review (api/src/acq_review.jl, shape in review_turn.jl): its points are the
# entry's only `### sNN` sections, marked Good / Bad / Unsure; the rule findings stay plain lines.
# A new round drops it — it answered the earlier plan.

# The set's plan entry id, or "" — the newest open-or-not entry whose `acqCheck.setUid` is the set.
function _acq_entry_for(uid::AbstractString, set_uid::AbstractString)::String
    best = ""; best_at = ""
    for (id, e) in _read_bb_registry(uid)
        meta = _read_bb_meta(uid, id)
        (meta === nothing || !(get(meta, "acqCheck", nothing) isa AbstractDict)) && continue
        string(get(meta["acqCheck"], "setUid", "")) == set_uid || continue
        at = string(get(e, "updatedAt", ""))
        (isempty(best) || at > best_at) && (best = id; best_at = at)
    end
    best
end

_acq_finding_key(f) = string(f["code"], "|", f["scope"], "|", get(get(f, "subs", Dict()), "what", ""), "|",
                             get(get(f, "subs", Dict()), "variable", ""))

# The entry's text for a plan and its check: the plan, then the findings as plain lines — across the
# set, then per movie. No timestamp, so a re-check that finds the same leaves the text unchanged.
function _acq_entry_md(set_name, a::AbstractDict, result::AbstractDict)::String
    io = IOBuffer()
    println(io, "# Acquisition check · ", set_name, "\n")
    p = a["plan"]
    objs = join([string(get(o, "name", ""), get(o, "kind", "cells") == "points" ? " (points)" : "") for o in get(p, "objects", Any[])], ", ")
    println(io, "Plan: ", isempty(strip(string(get(p, "text", "")))) ? "compare $(objs)" : p["text"],
            Int(get(a, "round", 1)) > 1 ? " · round $(a["round"])" : "", "\n")
    mark = Dict("warn" => "⚠", "info" => "–", "ok" => "✓")
    line(f) = string("- ", mark[f["level"]], " ", f["short"], " → ", f["long"],
                     haskey(f, "thisData") ? " For these movies: " * f["thisData"] : "")
    println(io, "## Across the set\n")
    for f in filter(f -> f["scope"] == "set", result["findings"]); println(io, line(f)); end
    names = Dict(r["uid"] => r["name"] for r in result["rows"])
    per = filter(f -> f["scope"] != "set", result["findings"])
    if !isempty(per)
        println(io, "\n## Per movie\n")
        for uid in unique(f["scope"] for f in per)
            println(io, "**", get(names, uid, uid), "**\n")
            for f in filter(f -> f["scope"] == uid, per); println(io, line(f)); end
            println(io)
        end
    end
    _review_md!(io, get(a, "review", nothing); categories = Cecelia.ACQ_CATEGORIES, label = _acq_category_label,
                intro = "read the plan, the movies' record and the acquisition knowledge.",
                nothing_more = "Nothing beyond the checks.", supports_heading = "What these movies would support")
    String(take!(io))
end

# A finding or review category's label ("Shared structure"); an unknown id reads as Other.
_acq_category_label(id) = Cecelia.qc_text("acq.cat." * (id in Cecelia.ACQ_CATEGORIES ? id : "other")).short

# The reply: the plan (or none), the set's label designs (shared channel-name lists), and the live check.
function _acq_reply(uid, set, id)
    facts = Cecelia.acq_set_facts(set)
    meta = isempty(id) ? nothing : _read_bb_meta(uid, id)
    a = meta === nothing ? nothing : _to_str_dict(meta["acqCheck"])
    plan = a === nothing ? nothing : a["plan"]
    result = Cecelia.acq_check(plan, facts)
    JSON3.write((; ok = true, entryId = id, round = a === nothing ? 0 : get(a, "round", 1), plan,
                 suggestedDesigns = Cecelia.acq_suggest_designs(facts), findings = result["findings"],
                 rows = result["rows"], ran = result["ran"], skipped = result["skipped"],
                 nImages = result["nImages"], nMeasured = result["nMeasured"], nOpen = length(Cecelia.acq_open(result)),
                 review = a === nothing ? nothing : get(a, "review", nothing),
                 categoryLabels = Dict(c => _acq_category_label(c) for c in Cecelia.ACQ_CATEGORIES)))
end

"""
    GET /api/acqcheck?projectUid&setUid

The set's acquisition check, run now against the current image meta; nothing is written.
Reply: `{ok, entryId ("" = no plan yet), round, plan, suggestedDesigns, findings, rows, ran, skipped,
nImages, nMeasured, nOpen}` — without a plan only the checks that need none run (`skipped` says which).
"""
function api_acqcheck(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    set = _cohort_set(uid, set_uid)
    set isa Tuple && return set
    200, _acq_reply(uid, set, _acq_entry_for(uid, set_uid))
end

# A plan from a request body, normalised: objects/references keep name, kind and channel (by name) per design.
function _acq_plan_from(p)::Union{Dict{String,Any},Nothing}
    p isa AbstractDict || return nothing
    pd = JSON3.read(JSON3.write(p), Dict{String,Any})
    norm(o) = Dict{String,Any}("name" => strip(string(get(o, "name", ""))), "kind" => string(get(o, "kind", "cells")),
                               "channels" => Dict{String,Any}(string(k) => (v isa Real ? Int(v) : string(v))
                                                              for (k, v) in get(o, "channels", Dict()) if v isa Real || (v isa AbstractString && !isempty(v))))
    objs = [norm(o) for o in get(pd, "objects", Any[]) if o isa AbstractDict && !isempty(strip(string(get(o, "name", ""))))]
    isempty(objs) && return nothing
    Dict{String,Any}(
        "designs" => [Dict{String,Any}("id" => string(get(d, "id", "")), "key" => string(get(d, "key", "")),
                                       "label" => string(get(d, "label", get(d, "key", "")))) for d in get(pd, "designs", Any[]) if d isa AbstractDict],
        "objects" => objs,
        "references" => [norm(o) for o in get(pd, "references", Any[]) if o isa AbstractDict && !isempty(strip(string(get(o, "name", ""))))],
        "measure" => String[string(m) for m in get(pd, "measure", Any[])],
        "compare" => String[string(m) for m in get(pd, "compare", Any[])],
        "expectation" => strip(string(get(pd, "expectation", ""))),
        "text" => strip(string(get(pd, "text", ""))))
end

"""
    POST /api/acqcheck/plan

Body: `{ projectUid, setUid, plan: {designs, objects: [{name, kind, channels: {designId: channelName}}],
references, measure, compare, expectation, text} }` — Reply: as `GET /api/acqcheck`.

Saves the set's plan as its Blackboard entry (created on first save) and writes the check into it. A
changed plan starts a new round on the same entry; the same plan only re-records the check.
"""
function api_acqcheck_plan(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid); set_uid = _wstr(body, :setUid)
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    plan = _acq_plan_from(get(body, :plan, nothing))
    plan === nothing && return 400, JSON3.write((; error = "plan with at least one named object required"))
    set = _cohort_set(uid, set_uid)
    set isa Tuple && return set
    result = Cecelia.acq_check(plan, Cecelia.acq_set_facts(set))
    by = author_stamp(); at = string(Dates.now())
    id = _acq_entry_for(uid, set_uid)
    if isempty(id)
        a = Dict{String,Any}("setUid" => set_uid, "plan" => plan, "round" => 1,
                             "rounds" => Any[Dict{String,Any}("round" => 1, "at" => at, "by" => by, "plan" => plan)])
        st, rb = api_blackboard_create(Vector{UInt8}(JSON3.write(Dict(
            "projectUid" => uid, "title" => "Acquisition check · " * set.name, "content" => _acq_entry_md(set.name, a, result)))))
        st == 200 || return st, rb
        id = String(JSON3.read(rb).entryId)
    else
        a = _to_str_dict(_read_bb_meta(uid, id)["acqCheck"])
        if JSON3.write(a["plan"]) != JSON3.write(plan)
            a["plan"] = plan; a["round"] = Int(get(a, "round", 1)) + 1
            push!(a["rounds"], Dict{String,Any}("round" => a["round"], "at" => at, "by" => by, "plan" => plan))
            delete!(a, "review")   # it answered the earlier plan
        end
    end
    st, rb = _acq_write!(uid, id, set.name, a, result)   # a no-op revise right after a create
    st == 200 || return st, rb
    200, _acq_reply(uid, set, id)
end

# Rewrite the entry text for `a` and its check (a no-op revise when nothing changed), record the check,
# and carry the review's refs as the entry's `kiwiRefs`.
function _acq_write!(uid::AbstractString, id::AbstractString, set_name, a::AbstractDict, result::AbstractDict)
    st, rb = api_blackboard_revise(Vector{UInt8}(JSON3.write(Dict(
        "projectUid" => uid, "entryId" => id, "content" => _acq_entry_md(set_name, a, result)))))
    st == 200 || return st, rb
    a["lastCheck"] = Dict{String,Any}("at" => string(Dates.now()),
                                      "open" => sort!([_acq_finding_key(f) for f in Cecelia.acq_open(result)]))
    _set_bb_meta_field!(uid, id, "acqCheck", a)
    _set_review_kiwi_refs!(uid, id, get(a, "review", nothing))
    200, rb
end
