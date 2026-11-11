# ── Acquisition review — the one model turn of the test-acquisition check ────────────────────────────
#
# docs/todo/ACQUISITION_TEST_CHECK_PLAN.md → the review row of the recital table. The claim review's
# mechanism (claim_review.jl; shared pieces in review_turn.jl) with a plan instead of a claim: one Kiwi
# turn reads the set's plan, the movies' record and the acquisition knowledge, and says what to change
# before the rest of the experiment is acquired — or, on final data, what the analysis must watch for. The user's own reasoning is WITHHELD (notes, Blackboard,
# captures, briefing, ELN, lab log), and so is the analysis record — this is before analysis. Advisory:
# its points are `inferred`, go on the set's acqCheck entry as `### sNN · …` sections under a
# `### <Category>` heading, which the user marks, and never change what the rule checks found.

# What the review may read: the movies as acquired and imported, and why an image looks off at
# acquisition. An allow-list, so a read tool added to Kiwi later reaches the review only once someone
# puts it here; every Kiwi read tool is in exactly one of these two lists (test-enforced).
const ACQ_REVIEW_TOOLS = (
    "get_project_info", "find_object", "list_images", "get_image_info", "get_image_attributes",
    "get_qc_metrics", "get_cohort_qc", "get_task_log", "get_task_history", "get_acquisition_knowledge",
    "get_guide", "get_task_catalogue")
# What the review must not see, and why.
const _ACQ_ANALYSIS = "the analysis record — this review is before analysis"
const ACQ_REVIEW_WITHHELD = Dict(
    "list_blackboard_entries" => "the user's entries", "read_blackboard_entry" => "the user's entries",
    "search_blackboard" => "the user's entries", "get_session_briefing" => "summarises the entries and notes",
    "get_recent_captures" => "what the user pointed at", "get_capture" => "what the user pointed at",
    "get_capture_landscape_tiles" => "what the user pointed at", "get_image_notes" => "the user's notes",
    "get_labarchives_context" => "the user's ELN", "read_lab_log" => "session notes, the user's and the assistant's",
    "list_notebooks" => "the user's notebooks", "get_notebook" => "the user's notebooks",
    "get_analysis_boards" => "the user's plots", "list_plots" => "the user's open plots",
    "list_figures" => "the user's figures and captions", "get_landscape" => "the user's live viewer",
    "get_populations" => _ACQ_ANALYSIS, "get_measure_summary" => _ACQ_ANALYSIS,
    "get_behaviour_summary" => _ACQ_ANALYSIS, "get_cluster_summary" => _ACQ_ANALYSIS,
    "get_region_clusters" => _ACQ_ANALYSIS, "get_contact_stats" => _ACQ_ANALYSIS,
    "get_spatial_patterns" => _ACQ_ANALYSIS, "get_analysis_lineage" => _ACQ_ANALYSIS,
    "check_claim" => _ACQ_ANALYSIS, "get_module_params" => _ACQ_ANALYSIS, "get_chains" => _ACQ_ANALYSIS,
    "get_object_ids" => _ACQ_ANALYSIS, "get_available_plots" => _ACQ_ANALYSIS,
    "gate_plot" => _ACQ_ANALYSIS, "gate_cells_view" => _ACQ_ANALYSIS, "gate_histogram" => _ACQ_ANALYSIS,
    "gate_stats" => _ACQ_ANALYSIS, "gate_examples" => _ACQ_ANALYSIS)
acq_review_tools() = String["mcp__" * OBSERVER_MCP_NAME * "__" * t for t in ACQ_REVIEW_TOOLS]

# The two stages a plan's movies can be at (`Cecelia.acq_stage`): a test, the rest still to acquire, or
# the final data, all there is. The stage picks the review's question, its verdict and the verdict's heading.
const _ACQ_STAGE_INTRO = Dict(
    "test" => """
You review a TEST acquisition in Cecelia before the rest of the experiment is acquired. The user has
imaged a few movies and written a plan: which objects they will segment or track, in which channel,
what they will measure and compare, and what they expect. You judge whether that plan can work on data
acquired like this, and what to change at the microscope (or in the plan) before acquiring the rest.""",
    "final" => """
You review FINAL data in Cecelia before it is analysed: the user has acquired every movie there will be
and cannot image more. They wrote a plan: which objects they will segment or track, in which channel,
what they will measure and compare, and what they expect. You judge whether that plan can work on these
movies, and what to watch for, correct or leave out in the analysis. Changing the microscope is no
longer an option — never suggest it.""")
const _ACQ_STAGE_LOOK = Dict(
    "test" => "anything the knowledge says to change at the scope.",
    "final" => "anything the knowledge says to correct for in the analysis.")
const _ACQ_STAGE_ASK = Dict(
    "test" => "What else in these movies would get in the way of the plan, and what should change before the rest is\nacquired?",
    "final" => "What else in these movies would get in the way of the plan, and what should the analysis do about it?")
const _ACQ_STAGE_ARROW = Dict("test" => "what to change at the scope or in the plan",
                              "final" => "what to do in the analysis, or how to narrow the plan")
# The verdict's rule part is the app's (`Cecelia.acq_verdict`); the model adds at most one step to it.
const _ACQ_STAGE_STEP = Dict("test" => "change at the scope or in the plan", "final" => "do in the analysis")
# finished data that can't answer the plan: the narrower question it can (a test can still change)
const _ACQ_STAGE_NARROW = Dict("test" => "",
    "final" => " If these movies cannot answer the plan at all, write instead \"These movies can answer: <the narrower question they can>.\"")

"""The prompt's TEMPLATE text in the set's noun (`acq_check`'s `item`/`items`: "movie" only when every
image is a time-lapse). Applied to the template pieces only — never to the user's names, plan or the
rule findings (already worded) — so an image called "movie_3" stays as it is."""
_acq_noun(s::AbstractString, item::AbstractString, items::AbstractString) =
    replace(s, "Movies" => titlecase(items), "movies" => items, "movie" => item)

"""The heading over the review's verdict on the plan's entry and in the dialog, by the plan's stage."""
acq_verdict_heading(plan) = Cecelia.acq_stage(plan) == "final" ? "Before analysing" : "Before acquiring the rest"

acq_review_system_prompt(project_uid::AbstractString, stage::AbstractString = "test";
                         item::AbstractString = "movie", items::AbstractString = "movies") = _acq_noun("""
$(_ACQ_STAGE_INTRO[stage])

The project is `$(project_uid)` — pass exactly that as `project_uid` in every tool call. Read the movies'
record (image info: acquisition settings, the import probe's photons, shared structure, fading;
QC metrics; task logs) and the acquisition knowledge before you answer. You can only read.

Look for what would stop the plan: an object too dim or too noisy to segment; a channel that carries
another's signal where the plan needs them apart; settings that differ between movies the plan compares;
signal fading that would break tracking; $(_ACQ_STAGE_LOOK[stage])
Say "consistent with" until the cause is separated — a channel dimming over time can be bleaching or
cells leaving.

Reply ONLY through the structured output. Each claim is ONE point, with refs to the movies it concerns:
`observation` for a problem you saw in a tool result, `question` for something the user must answer
that the record can't. Then one `interpretation`, last: the one step you add to the verdict on the plan.
If nothing would change, abstain and say in `note` what you checked.
""", item, items)

# The plan as the prompt states it: each object and reference with its channel per design.
function _acq_plan_lines(plan::AbstractDict)::String
    label(did) = (ds = get(plan, "designs", Any[]); i = findfirst(d -> string(get(d, "id", "")) == did, ds);
                  i === nothing ? did : string(get(ds[i], "label", get(ds[i], "key", did))))
    chans(o) = join((string(label(string(d)), ": ", c) for (d, c) in get(o, "channels", Dict())), "; ")
    line(o) = string("- ", o["name"], get(o, "kind", "cells") == "points" ? " (points, counted)" : "", " — ", chans(o))
    io = IOBuffer()
    println(io, "Objects (segmented or tracked):")
    for o in get(plan, "objects", Any[]); println(io, line(o)); end
    refs = get(plan, "references", Any[])
    if !isempty(refs)
        println(io, "Reference channels (recorded, not analysed):")
        for o in refs; println(io, line(o)); end
    end
    ms = get(plan, "measure", Any[]); cs = get(plan, "compare", Any[])
    println(io, "Measures: ", isempty(ms) ? "not said" : join(ms, ", "))
    println(io, "Compared: ", isempty(cs) ? "all objects" : join(cs, ", "))
    ex = string(get(plan, "expectation", ""))
    isempty(ex) || println(io, "Expectation: ", ex)
    String(take!(io))
end

# `found` are the rule findings as lines; `noted` the ones that are expected, not a problem (an info
# finding — "different laser between designs, compare within each").
function acq_review_prompt(plan::AbstractDict, movies::AbstractVector, found::AbstractVector = String[],
                           noted::AbstractVector = String[]; verdict::AbstractString = "",
                           item::AbstractString = "movie", items::AbstractString = "movies")::String
    stage = Cecelia.acq_stage(plan)
    N(s) = _acq_noun(s, item, items)
    known = isempty(found) ? "The rule checks found nothing." :
        "The rule checks already found these — the user sees them, so do NOT repeat or rephrase them:\n" *
        join(("- " * f for f in found), "\n")
    isempty(noted) || (known *= "\n\nThey also noted these as expected, NOT a problem — don't act on them:\n" *
                                 join(("- " * f for f in noted), "\n"))
    text = strip(string(get(plan, "text", "")))
    """
    Plan: “$(isempty(text) ? "(no sentence)" : text)”
    $(_acq_plan_lines(plan))
    $(N("Movies")) (name — image uid):
    $(join(("- " * m for m in movies), "\n"))

    $(known)

    $(N(_ACQ_STAGE_ASK[stage])) Report only what the checks above don't already say; if that is nothing, abstain.

    Write each observation or question as `<Category>: <what is wrong, and where — name the $(items) and
    channels> → <$(_ACQ_STAGE_ARROW[stage])>`, with Category one of: $(_acq_review_category_list()).
    Each side of the arrow is ONE sentence of at most $(KIWI_CLAIM_MAX_CHARS) characters, with no semicolon:
    two problems are two claims.

    End with the `interpretation`: the app already closes with these steps from the rule checks —
    “$(isempty(verdict) ? "(none)" : verdict)” — so add only the ONE most important thing to $(_ACQ_STAGE_STEP[stage])
    that they don't cover, as one imperative sentence with no lead-in, naming the plan's objects and
    channels. A step that needs something the record lacks (a Mouse attribute, a control) says to add it
    first. If nothing matters beyond those steps, write exactly "Nothing to add."$(N(_ACQ_STAGE_NARROW[stage])) At most
    $(KIWI_CLAIM_MAX_CHARS) characters, no arrow.

    Name a$(item == "image" ? "n" : "") $(item) in your text by its full name exactly as listed above — never shortened, never by its uid:
    the app shows names or uids as the reader chooses.
    """
end

_acq_review_category_list() = join((_acq_category_label(c) for c in Cecelia.ACQ_CATEGORIES), ", ")

# A review point's text → `(category, text, action)`, filed under the acquisition check's categories.
_acq_review_point(text::AbstractString) = _review_point(text, Cecelia.ACQ_CATEGORIES, _acq_category_label)

# The rule findings as the prompt lists them, `(found, noted)`: a movie's finding names the movie; an
# info finding is `noted` (expected, nothing to do).
function _acq_found_lines(result, names)
    line(f) = f["scope"] == "set" ? f["short"] : string(f["short"], " (", get(names, f["scope"], f["scope"]), ")")
    String[line(f) for f in result["findings"] if f["level"] != "info"], String[line(f) for f in result["findings"] if f["level"] == "info"]
end

"""
    acq_review_start(project_uid, set_uid; model) -> (status, record)

Start the review turn for a set's saved plan. The finished turn writes its points onto the set's
acqCheck entry (`_acq_review_done!`). One Kiwi turn per project at a time, so a busy Kiwi is a 409.
"""
function acq_review_start(uid::AbstractString, set_uid::AbstractString; model::AbstractString = observer_default_model())
    set = _cohort_set(uid, set_uid)
    set isa Tuple && return set[1], Dict{String,Any}("error" => string(get(JSON3.read(set[2]), :error, "Set not found")))
    id = _acq_entry_for(uid, set_uid)
    isempty(id) && return 400, Dict{String,Any}("error" => "Save a plan first")
    a = _read_bb_meta(uid, id)["acqCheck"]
    plan = get(a, "plan", nothing)
    plan isa AbstractDict || return 400, Dict{String,Any}("error" => "Save a plan first")
    facts = Cecelia.acq_set_facts(set)
    result = Cecelia.acq_check(plan, facts)
    names = Dict(f["uid"] => f["name"] for f in facts)
    movies = [string(f["name"], " — ", f["uid"]) for f in facts]
    found, noted = _acq_found_lines(result, names)
    noun = (item = result["item"], items = result["items"])
    kiwi_start_turn(uid, acq_review_prompt(plan, movies, found, noted; verdict = result["verdict"], noun...); model,
                    system_prompt = acq_review_system_prompt(uid, Cecelia.acq_stage(plan); noun...), allowed_tools = acq_review_tools(),
                    extra = Dict{String,Any}("acqReview" => Dict{String,Any}("entryId" => id, "setUid" => String(set_uid),
                                                                            "round" => get(a, "round", 1))),
                    on_done = _acq_review_done!)
end

# The finished review onto the set's entry, if the plan is still on the round it was asked for. A turn
# that didn't finish keeps the last review that did and records why (`_review_from_turn`).
function _acq_review_done!(rec::AbstractDict)
    t = _review_target(rec, "acqReview", "acqCheck")
    t === nothing && return nothing
    uid, id, a, _ = t
    set = _cohort_set(uid, string(a["setUid"]))
    set isa Tuple && return nothing
    a["review"] = _review_from_turn(rec, get(a, "review", nothing);
                                    categories = Cecelia.ACQ_CATEGORIES, label = _acq_category_label)
    ids = get(a, "reviewIds", nothing)
    ids = ids isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in ids) : Dict{String,Any}()
    _review_assign_ids!(a["review"], ids)
    a["reviewIds"] = ids
    _acq_write!(uid, id, set.name, a, Cecelia.acq_check(a["plan"], Cecelia.acq_set_facts(set)))
    nothing
end

"""
    POST /api/acqcheck/review

Body: `{ projectUid, setUid, model? }` → the Kiwi turn record (`{turnId, status: "running", …}`).
Runs the acquisition review (one model turn, about a minute of the user's quota) in the background on
the set's saved plan; `kiwi:step` / `kiwi:done` report it, and the set's acqCheck entry gets its points
when it finishes. 400 without a saved plan, 409 while Kiwi is busy on the project, 403 with AI off
(refused in `kiwi_start_turn`, like every in-app model turn).
"""
function api_acqcheck_review(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid); set_uid = _wstr(body, :setUid)
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    _kiwi_project_ok(uid) || return 404, JSON3.write((; error = "Project not found"))
    m = _wstr(body, :model)
    st, rec = acq_review_start(uid, set_uid; model = isempty(m) ? observer_default_model() : m)
    st, JSON3.write(rec)
end
