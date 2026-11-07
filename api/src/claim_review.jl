# ── Claim review — the one model turn of the claim recital ───────────────────────────────────────────
#
# docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → S4 / B5. A plain "does the record support this claim?"
# turn, run as a Kiwi turn (`kiwi_start_turn`: background job, `kiwi:step` / `kiwi:done`, ref
# validation, one re-ask, kept in the Kiwi feed) with its own system prompt and a narrower tool list:
# the analyst's reasoning is WITHHELD — no Blackboard, no captures, no image notes, no briefing, no
# ELN context — so the review reads the claim and the record only. Advisory: its findings are
# `inferred`, go on the claim entry as `### sNN · Review · …` sections the analyst marks, and never
# change what the checks found. It reads through the observer tools only, so it can't recompute.

# What the review must not see: where the analyst's own reasoning lives.
const CLAIM_REVIEW_WITHHELD = ("list_blackboard_entries", "read_blackboard_entry", "search_blackboard",
    "get_session_briefing", "get_recent_captures", "get_capture", "get_capture_landscape_tiles",
    "get_image_notes", "get_labarchives_context")
claim_review_tools() = String["mcp__" * OBSERVER_MCP_NAME * "__" * t for t in KIWI_READ_TOOLS
                              if !(t in CLAIM_REVIEW_WITHHELD)]

claim_review_system_prompt(project_uid::AbstractString) = """
You review one claim an analyst wants to make from an image-analysis project in Cecelia. You are a
second pair of eyes on the RECORD, not on the biology: what was imaged, how it was segmented, tracked,
gated and measured, and whether that lets the claim be made as stated.

The project is `$(project_uid)` — pass exactly that as `project_uid` in every tool call. Read the record
with the tools before you answer. You can only read; you cannot recompute.

Look for what would change the analysis: images in scope missing data or processed differently;
populations measured differently from each other (2D vs 3D tracks, other parameters, other gates); the
statistical unit (cells or tracks pooled vs animals); the same test run many times; images that left
the set; anything in the task logs that was flagged and not followed up.

Reply ONLY through the structured output. Each claim is ONE finding about the record, with refs to the
images, populations or plots it concerns: `observation` for a problem you saw in a tool result,
`question` for something the analyst must answer that the record can't. At most one `interpretation`,
last: the claim as the record WOULD support it. If the record raises nothing, abstain and say in
`note` what you checked. Never say the claim is true or false; say what the record can't support yet.
"""

function claim_review_prompt(c::AbstractDict)::String
    unit = isempty(get(c, "unitAttr", "")) ? "none named" : c["unitAttr"]
    """
    Claim: “$(c["text"])”
    Set: $(c["setUid"])
    Populations compared: $(join(c["populations"], ", "))
    Measure: $(c["measure"])
    Statistical unit (image attribute): $(unit)

    Does the record support this claim as stated?"""
end

"""
    claim_review_start(project_uid, entry_id; model) -> (status, record)

Start the review turn for a claim entry. The finished turn writes its findings onto the entry
(`_claim_review_done!`). One Kiwi turn per project at a time, so a busy Kiwi is a 409.
"""
function claim_review_start(uid::AbstractString, id::AbstractString; model::AbstractString = observer_default_model())
    _valid_bb_entry_id(id) || return 400, Dict{String,Any}("error" => "Invalid entryId")
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, Dict{String,Any}("error" => "Entry not found")
    c = get(meta, "claim", nothing)
    c isa AbstractDict || return 400, Dict{String,Any}("error" => "Not a claim entry")
    kiwi_start_turn(uid, claim_review_prompt(c); model,
                    system_prompt = claim_review_system_prompt(uid), allowed_tools = claim_review_tools(),
                    extra = Dict{String,Any}("claimReview" => Dict{String,Any}("entryId" => String(id),
                                                                              "round" => get(c, "round", 1))),
                    on_done = _claim_review_done!)
end

# The finished review onto the claim entry: its claims become review sections; the round it was asked
# for must still be the claim's round (a reworded claim's review would answer the wrong question).
function _claim_review_done!(rec::AbstractDict)
    cr = get(rec, "claimReview", nothing)
    cr isa AbstractDict || return nothing
    uid, id = String(rec["projectUid"]), String(cr["entryId"])
    meta = _read_bb_meta(uid, id)
    (meta === nothing || !(get(meta, "claim", nothing) isa AbstractDict)) && return nothing
    c = Dict{String,Any}(String(k) => v for (k, v) in meta["claim"])
    get(c, "round", 1) == get(cr, "round", 1) || return nothing
    reply = get(rec, "reply", nothing)
    claims = reply isa AbstractDict ? collect(get(reply, "claims", Any[])) : Any[]
    c["review"] = Dict{String,Any}(
        "turnId" => rec["turnId"], "at" => get(rec, "finishedAt", string(Dates.now())),
        "status" => rec["status"], "model" => get(rec, "model", ""),
        "abstain" => reply isa AbstractDict && get(reply, "abstain", false) == true,
        "note" => reply isa AbstractDict ? string(get(reply, "note", "")) : "",
        # one line each: a finding's text becomes a `### sNN` heading
        "claims" => [Dict{String,Any}("kind" => string(get(x, "kind", "")),
                                      "text" => replace(strip(string(get(x, "text", ""))), r"\s*\n\s*" => " "))
                     for x in claims])
    result = _claim_run(uid, c)
    result isa Tuple && return nothing
    _claim_assign_ids!(c["findingIds"], result["findings"])
    _claim_assign_review_ids!(c)
    _claim_write!(uid, id, c, result)
    nothing
end

# Review findings get section ids from the same counter, keyed by their text: a re-run that finds the
# same thing keeps the id (and its verdict); a new wording is a new section.
function _claim_assign_review_ids!(c::AbstractDict)
    for x in get(get(c, "review", Dict()), "claims", Any[])
        x["kind"] == "interpretation" && continue
        x["sectionId"] = _claim_section_id!(c["findingIds"], "review|" * x["text"])
    end
    c
end

"""
    POST /api/claim/review

Body: `{ projectUid, entryId, model? }` → the Kiwi turn record (`{turnId, status: "running", …}`).
Runs the claim review (one model turn, about a minute and ~\$1 of the user's quota) in the background;
`kiwi:step` / `kiwi:done` report it, and the claim entry gets its findings when it finishes.
"""
function api_claim_review(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid); id = _wstr(body, :entryId)
    _kiwi_project_ok(uid) || return 404, JSON3.write((; error = "Project not found"))
    m = _wstr(body, :model)
    st, rec = claim_review_start(uid, id; model = isempty(m) ? observer_default_model() : m)
    st, JSON3.write(rec)
end
