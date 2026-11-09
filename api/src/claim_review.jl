# ── Claim review — the one model turn of the claim recital ───────────────────────────────────────────
#
# docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → S4 / B5. A plain "does the record support this claim?"
# turn, run as a Kiwi turn (`kiwi_start_turn`: background job, `kiwi:step` / `kiwi:done`, ref
# validation, one re-ask, kept in the Kiwi feed) with its own system prompt and a narrower tool list:
# the analyst's reasoning is WITHHELD — no Blackboard, no captures, no image notes, no briefing, no
# ELN context — so the review reads the claim and the record only. Advisory: its findings are
# `inferred`, go on the claim entry as `### sNN · …` sections under a `### <Category>` heading, which the analyst marks, and never
# change what the checks found. It reads through the observer tools only, so it can't recompute.
# The parsing, write-back and rendering it shares with the acquisition review live in review_turn.jl.

# What the review may read: the RECORD — what was imaged, run, measured and gated. An allow-list, so a
# read tool added to Kiwi later reaches the review only once someone puts it here; every Kiwi read tool
# is in exactly one of these two lists (test-enforced).
const CLAIM_REVIEW_TOOLS = (
    "get_project_info", "find_object", "list_images", "get_image_info", "get_image_attributes",
    "get_populations", "get_measure_summary", "get_behaviour_summary", "get_cluster_summary",
    "get_region_clusters", "get_contact_stats", "get_spatial_patterns", "get_analysis_lineage",
    "get_qc_metrics", "get_cohort_qc", "check_claim", "get_task_log", "get_task_history",
    "get_module_params", "get_chains", "get_object_ids", "get_task_catalogue", "get_guide",
    "get_acquisition_knowledge", "gate_plot", "gate_cells_view", "gate_histogram", "gate_stats",
    "gate_examples", "get_available_plots")
# What the review must not see, and why: where the analyst's own reasoning lives, and their other
# plots — a review that reads those critiques the plots, not the claim.
const CLAIM_REVIEW_WITHHELD = Dict(
    "list_blackboard_entries" => "the analyst's entries", "read_blackboard_entry" => "the analyst's entries",
    "search_blackboard" => "the analyst's entries", "get_session_briefing" => "summarises the entries and notes",
    "get_recent_captures" => "what the analyst pointed at", "get_capture" => "what the analyst pointed at",
    "get_capture_landscape_tiles" => "what the analyst pointed at", "get_image_notes" => "the analyst's notes",
    "get_labarchives_context" => "the analyst's ELN", "read_lab_log" => "session notes, the analyst's and the assistant's",
    "list_notebooks" => "the analyst's notebooks", "get_notebook" => "the analyst's notebooks",
    "get_analysis_boards" => "the analyst's other plots", "list_plots" => "the analyst's open plots",
    "list_figures" => "the analyst's figures and captions", "get_landscape" => "the analyst's live viewer")
claim_review_tools() = String["mcp__" * OBSERVER_MCP_NAME * "__" * t for t in CLAIM_REVIEW_TOOLS]

claim_review_system_prompt(project_uid::AbstractString) = """
You review one claim an analyst wants to make from an image-analysis project in Cecelia. You are a
second pair of eyes on the RECORD, not on the biology: what was imaged, how it was segmented, tracked,
gated and measured, and whether that lets the claim be made as stated.

The project is `$(project_uid)` — pass exactly that as `project_uid` in every tool call. Read the record
with the tools before you answer. You can only read; you cannot recompute.

Look for what would change the analysis: images in scope missing data or processed differently;
populations measured differently from each other (2D vs 3D tracks, other parameters, other gates); the
statistical unit (cells or tracks pooled vs animals); images that left the set; anything in the task
logs that was flagged and not followed up.

Reply ONLY through the structured output. Each claim is ONE finding about the record, with refs to the
images, populations or plots it concerns: `observation` for a problem you saw in a tool result,
`question` for something the analyst must answer that the record can't. At most one `interpretation`,
last: the claim as the record WOULD support it. If the record raises nothing, abstain and say in
`note` what you checked. Never say the claim is true or false; say what the record can't support yet.
"""

function claim_review_prompt(c::AbstractDict, found::AbstractVector = String[])::String
    ua = _claim_unit_attrs(c)
    unit = isempty(ua) ? "none named" : join(ua, " × ")
    known = isempty(found) ? "The rule checks found nothing." :
        "The rule checks already found these — the analyst sees them, so do NOT repeat or rephrase them:\n" *
        join(("- " * f for f in found), "\n")
    """
    Claim: “$(c["text"])”
    Set: $(c["setUid"])
    Populations compared: $(join(c["populations"], ", "))
    Measure: $(c["measure"])
    Replicate (image attributes; one value combination = one replicate): $(unit)

    $(known)

    What else in the record bears on whether this claim can be made as stated? Report only what the
    checks above don't already say; if that is nothing, abstain.

    Write each observation or question as `<Category>: <what is wrong, and where — name the images or
    populations> → <what the analyst should do about it>`, with Category one of: $(_claim_review_category_list()).
    Each side of the arrow is ONE sentence of at most $(KIWI_CLAIM_MAX_CHARS) characters, with no semicolon:
    two problems are two claims. The `interpretation` is one such sentence too.
    """
end

# The category labels the review files its points under — the checks' own (qc/text.jl `claim.cat.*`).
_claim_review_category_list() = join((_claim_category_label(c) for c in Cecelia.CLAIM_CATEGORIES), ", ")

# A review point's text → `(category, text, action)`, filed under the claim categories (`_review_point`).
_claim_review_point(text::AbstractString) = _review_point(text, Cecelia.CLAIM_CATEGORIES, _claim_category_label)

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
    # what the rule checks found, so the review spends its turn on what they can't see
    result = _claim_run(uid, c)
    found = result isa Tuple ? String[] : String[f["short"] for f in result["findings"]]
    kiwi_start_turn(uid, claim_review_prompt(c, found); model,
                    system_prompt = claim_review_system_prompt(uid), allowed_tools = claim_review_tools(),
                    extra = Dict{String,Any}("claimReview" => Dict{String,Any}("entryId" => String(id),
                                                                              "round" => get(c, "round", 1))),
                    on_done = _claim_review_done!)
end

# The finished review onto the claim entry: its claims become review sections; the round it was asked
# for must still be the claim's round (`_review_target`). A turn that didn't finish keeps the last review
# that did and says why (`_review_from_turn`).
function _claim_review_done!(rec::AbstractDict)
    t = _review_target(rec, "claimReview", "claim")
    t === nothing && return nothing
    uid, id, c, _ = t
    c["review"] = _review_from_turn(rec, get(c, "review", nothing);
                                    categories = Cecelia.CLAIM_CATEGORIES, label = _claim_category_label)
    result = _claim_run(uid, c)
    result isa Tuple && return nothing
    _claim_assign_ids!(c["findingIds"], result["findings"])
    _review_assign_ids!(c["review"], c["findingIds"])
    _claim_write!(uid, id, c, result)
    nothing
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
