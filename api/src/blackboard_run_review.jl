# ── Agent run review on the Blackboard ─────────────────────────────────────────
# `docs/todo/AGENT_RUN_REVIEW_PLAN.md` P2. An unattended agent run's record is one Blackboard entry
# in the SOURCE project (`scripts/agent_eval/run_record.py`), one `### dNN · step · image · …`
# section per decision. Three meta fields live here, all carried unchanged through every other write
# (`_BB_PASSTHROUGH_KEYS` in `_write_bb_meta!`):
#
#   agentRun         — set once at create by the harness: `{copyProjectUid, startedAt, images,
#                      sectionIds, …}`. Marks the entry as a run record (the list's filter) and says
#                      which sections there are without reading entry.md.
#   sectionOutcomes  — `{sectionId: {verdict, note, by, at}}`. A verdict on ONE decision (Decision 7):
#                      `good | bad | unsure`, note required for `bad`. `by` is `author_stamp()`, so
#                      `via: "claude"` marks a proposal from a chat session; the score counts
#                      only `via: "app"` (a person). A proposal never replaces a person's verdict.
#   knowledge        — `{by, at, from?: {entryId, sectionId}}`. Marks the entry as lab knowledge for
#                      this project (P4, Decision 11): the run harness carries these entries, and only
#                      these, into a run's copy. `from` names the run section it was promoted from.
#
# Misses (Decision 8) are `### mNN · …` sections the reviewer appends through the normal revise
# route, then marks `bad` here.

const _BB_PASSTHROUGH_KEYS = ("agentRun", "sectionOutcomes", "knowledge")
const _BB_SECTION_VERDICTS = ("good", "bad", "unsure")
const _BB_SECTION_ID_RE = r"^[dm][0-9]{2,3}$"
const _BB_AGENT_RUN_MAX_BYTES = 8 * 1024

# Write one field into an entry's meta.json as it is on disk (no other field touched).
function _set_bb_meta_field!(uid::AbstractString, id::AbstractString, key::String, value)
    meta = _read_bb_meta(uid, id)
    meta === nothing && return nothing
    if value === nothing
        delete!(meta, key)
    else
        meta[key] = value
    end
    write_json_atomic(joinpath(_bb_entry_dir(uid, id), "meta.json"), meta)
end

# The create body's `agentRun`: `nothing` when absent, the dict when valid, a 400 reply otherwise.
function _agent_run_from_body(v)
    v === nothing && return nothing
    v isa AbstractDict || return 400, JSON3.write((; error = "agentRun must be an object"))
    length(codeunits(JSON3.write(v))) > _BB_AGENT_RUN_MAX_BYTES &&
        return 400, JSON3.write((; error = "agentRun exceeds $_BB_AGENT_RUN_MAX_BYTES bytes"))
    JSON3.read(JSON3.write(v), Dict{String,Any})
end

_section_outcomes(meta)::Dict{String,Any} =
    (so = get(meta, "sectionOutcomes", nothing); so isa AbstractDict ? Dict{String,Any}(so) : Dict{String,Any}())

_by_person(o) = o isa AbstractDict && get(something(get(o, "by", nothing), Dict()), "via", "") == "app"

# Put the run-review fields on a reply. A list row gets the marker and a count of the sections a
# person has marked (the "3 / 15" in the list); the entry read gets both fields whole. Both get
# the knowledge marker.
function _bb_put_run_review!(row::AbstractDict, meta; list_row::Bool = false)
    ar = get(meta, "agentRun", nothing)
    ar isa AbstractDict && (row["agentRun"] = ar)
    kn = get(meta, "knowledge", nothing)
    kn isa AbstractDict && (row["knowledge"] = kn)
    so = _section_outcomes(meta)
    if list_row
        ar isa AbstractDict && (row["sectionsMarked"] = count(_by_person, values(so)))
    else
        isempty(so) || (row["sectionOutcomes"] = so)
    end
    row
end

# Does entry.md have a `### <sid> ·` heading?
function _bb_has_section(uid::AbstractString, id::AbstractString, sid::AbstractString)::Bool
    p = joinpath(_bb_entry_dir(uid, id), "entry.md")
    isfile(p) || return false
    head = "### " * sid * " "
    # read whole: a lazy `eachline` that stops early leaves the file open, and Windows then
    # refuses the next revise's atomic replace of entry.md (EBUSY)
    any(l -> startswith(l, head), split(read(p, String), '\n'))
end

"""
    POST /api/blackboard/section-outcome

Body: `{ projectUid, entryId, sectionId, verdict: "good"|"bad"|"unsure"|"", note? }`
Reply: `{ ok:true, sectionId, outcome: {verdict, note, by, at} | null }`

A verdict on one section (`### dNN ·` / `### mNN ·`) of an entry — AGENT_RUN_REVIEW_PLAN Decision 7.
The note is required for `bad`. `verdict: ""` clears the section's verdict. `by` is the caller
(`author_stamp()`): a call from Claude (`X-Cecelia-Client: claude`) is a PROPOSAL and may neither
replace nor clear a person's verdict (409). No snapshot — metadata only, like `/outcome`.
"""
function api_blackboard_section_outcome(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid, id, sid = _wstr(body, :projectUid), _wstr(body, :entryId), _wstr(body, :sectionId)
    verdict = _wstr(body, :verdict)
    note = String(strip(String(get(body, :note, ""))))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isnothing(match(_BB_SECTION_ID_RE, sid)) &&
        return 400, JSON3.write((; error = "sectionId must look like d07 or m02"))
    isempty(verdict) || verdict in _BB_SECTION_VERDICTS ||
        return 400, JSON3.write((; error = "verdict must be one of $(_BB_SECTION_VERDICTS), or \"\" to clear"))
    verdict == "bad" && isempty(note) &&
        return 400, JSON3.write((; error = "note required for a bad verdict"))
    length(codeunits(note)) > _BB_OUTCOME_NOTE_MAX_BYTES &&
        return 400, JSON3.write((; error = "note exceeds $_BB_OUTCOME_NOTE_MAX_BYTES bytes"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
    _bb_has_section(uid, id, sid) || return 404, JSON3.write((; error = "No section $sid in this entry"))

    so = _section_outcomes(meta)
    by = author_stamp()
    by["via"] == "claude" && _by_person(get(so, sid, nothing)) &&
        return 409, JSON3.write((; error = "$sid already has a person's verdict; a proposal does not replace it"))
    out = if isempty(verdict)
        delete!(so, sid)
        nothing
    else
        so[sid] = Dict{String,Any}("verdict" => verdict, "note" => note, "by" => by,
                                   "at" => string(Dates.now()))
    end
    _set_bb_meta_field!(uid, id, "sectionOutcomes", isempty(so) ? nothing : so)
    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, sectionId = sid, outcome = out))
end

"""
    POST /api/blackboard/knowledge

Body: `{ projectUid, entryId, knowledge: true|false, from?: {entryId, sectionId} }`
Reply: `{ ok:true, knowledge: {by, at, from?} | null }`

Marks an entry as lab knowledge for this project, or unmarks it — AGENT_RUN_REVIEW_PLAN P4. The run
harness copies the source project's knowledge entries into each run's copy (Decision 11), so this
is what a run gets to read. `from` records the run section the lesson was promoted from. Not on a
run record or the profile. Metadata only; no snapshot.
"""
function api_blackboard_knowledge(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid, id = _wstr(body, :projectUid), _wstr(body, :entryId)
    on = get(body, :knowledge, nothing)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    on isa Bool || return 400, JSON3.write((; error = "knowledge must be true or false"))
    id == "profile" && return 400, JSON3.write((; error = "The profile is not a knowledge entry"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
    haskey(meta, "agentRun") && return 400, JSON3.write((; error = "A run record is not a knowledge entry"))

    kn = nothing
    if on
        kn = Dict{String,Any}("by" => author_stamp(), "at" => string(Dates.now()))
        from = get(body, :from, nothing)
        if from isa AbstractDict
            fe, fs = _wstr(from, :entryId), _wstr(from, :sectionId)
            _valid_bb_entry_id(fe) && !isnothing(match(_BB_SECTION_ID_RE, fs)) ||
                return 400, JSON3.write((; error = "from must be {entryId, sectionId}"))
            kn["from"] = Dict{String,Any}("entryId" => fe, "sectionId" => fs)
        end
    end
    _set_bb_meta_field!(uid, id, "knowledge", kn)
    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, knowledge = kn))
end
