# ── Kiwi turn — one structured, validated assistant reply ────────────────────────────────────────────
#
# docs/todo/KIWI_ASSISTANT_PLAN.md Phase 3. One call = one Kiwi turn:
#
#   prompt + attached KiwiRefs
#     → context pack (the attached refs, resolved)               Decision 11 (v1: labels only)
#     → engine turn: Kiwi system prompt, reply schema, read-only observer tools, streamed
#     → validate every claim's refs: resolves (kiwi_refs.jl)     Decision 6
#                                   AND was seen this turn        Decision 7
#       and every claim: one fact, citing what its text names      (claim shape, below)
#     → any failure → ONE re-ask on the same session naming what failed, then validate again
#
# REPL-first (no route yet — Phase 4 adds the UI): `run_kiwi_turn("zolIMa", "…"; refs = [...])`.
# Engine-independent: it drives any `AgentBackend` through `run_agent_turn` (app/src/ai/agent_runner.jl);
# only `ClaudeAgent` exists. Read-only: the tool allow-list below has no write tool, and the MCP config
# is the headless one, so a turn never pairs the project to a throwaway session.

# Observer tools a Kiwi turn may call — reads only. Deliberately excludes every write (lab log,
# Blackboard, notebooks, chains), every point-out mark (Kiwi points through its reply's refs, rendered
# by the app), and pairing. Adding one here is a decision: it widens what a turn can touch. Every
# observer tool is either here or in `KIWI_EXCLUDED_TOOLS` with its reason — test-enforced, because a
# model that calls an unlisted tool gets a permission error, not the data (a 2026-09-23 eval turn
# abstained after calling three reads missing from this list).
const KIWI_READ_TOOLS = [
    "get_project_info", "get_session_briefing", "find_object", "list_images", "get_image_info",
    "get_image_notes", "get_image_attributes", "get_populations", "get_measure_summary",
    "get_behaviour_summary", "get_cluster_summary", "get_region_clusters", "get_contact_stats",
    "get_analysis_lineage", "get_qc_metrics", "get_cohort_qc", "get_task_log", "get_task_history",
    "get_module_params", "get_chains", "list_plots", "get_available_plots", "get_analysis_boards",
    "get_landscape", "get_recent_captures", "get_capture", "get_capture_landscape_tiles",
    "get_object_ids", "list_blackboard_entries", "read_blackboard_entry", "search_blackboard",
    "read_lab_log", "list_notebooks", "get_notebook",
]
const KIWI_EXCLUDED_TOOLS = Dict(
    "list_projects" => "other projects — a turn is scoped to one",
    "get_labarchives_context" => "an external notebook, not app state", "set_labarchives_context" => "write",
    "poll_observations" => "drains the observation queue into the lab log — a write",
    "get_observer_stats" => "runtime, not project data", "get_recent_logs" => "runtime, not project data",
    "get_repl_api" => "developer API reference, not project data",
    "register_push_target" => "pairing", "set_observer_active" => "write",
    "append_lab_log" => "write", "create_blackboard_entry" => "write", "revise_blackboard_entry" => "write",
    "set_blackboard_status" => "write", "set_blackboard_outcome" => "write", "create_chain" => "write",
    "create_notebook" => "write", "revise_notebook" => "write", "set_notebook_description" => "write",
    "add_analysis_board" => "write", "open_analysis_board_plot" => "drives the user's UI",
    "seek_viewer" => "drives the user's UI", "point_at_ui" => "point-out — Kiwi points through refs",
    "mark_cells" => "point-out", "mark_tracks" => "point-out", "mark_plot" => "point-out",
    "mark_tile" => "point-out", "mark_freeform" => "point-out", "select_on_plot" => "point-out",
)
_kiwi_allowed_tools() = String["mcp__" * OBSERVER_MCP_NAME * "__" * t for t in KIWI_READ_TOOLS]

const KIWI_CLAIM_KINDS = ("observation", "interpretation", "question")

"""
    kiwi_reply_schema(; reasoning = false) -> String

The JSON Schema every Kiwi reply is generated under: `{abstain, claims:[{kind, text, refs:[KiwiRef…]}]}`,
the refs being the SAME `KiwiRef` definition the resolver checks (embedded from
`frontend/src/lib/kiwiRef.schema.json`, not restated). `reasoning = true` adds a free-text `reasoning`
field ordered first — Open decision 8 (does a strict schema degrade reasoning?) is settled by comparing
the two on Phase 3's fixed prompt set, which is why it's a switch and not a choice made here.
"""
function kiwi_reply_schema(; reasoning::Bool = false)::String
    defs = Dict{String,Any}(_KIWI_REF_DEFS)
    defs["kiwiRef"] = Dict{String,Any}("oneOf" => _KIWI_REF_SCHEMA["oneOf"])
    claim = Dict{String,Any}(
        "type" => "object", "additionalProperties" => false, "required" => ["kind", "text", "refs"],
        "properties" => Dict{String,Any}(
            "kind" => Dict("enum" => collect(KIWI_CLAIM_KINDS)),
            "text" => Dict("type" => "string", "minLength" => 1),
            "refs" => Dict("type" => "array", "minItems" => 1, "items" => Dict("\$ref" => "#/definitions/kiwiRef"))))
    props = Dict{String,Any}("abstain" => Dict("type" => "boolean"),
                             "claims"  => Dict("type" => "array", "items" => claim))
    req = ["abstain", "claims"]
    reasoning && (props["reasoning"] = Dict("type" => "string"); pushfirst!(req, "reasoning"))
    JSON3.write(Dict{String,Any}("type" => "object", "additionalProperties" => false,
                                 "required" => req, "properties" => props, "definitions" => defs))
end

# The duck line (docs/archive/kiwi-duck-not-oracle.md → the plan's Decisions 5, 7, 9, 10), as the
# engine's WHOLE system prompt (`replace_system_prompt`), not appended to a coding assistant's.
kiwi_system_prompt(project_uid::AbstractString) = """
You are Kiwi, a structured rubber duck inside Cecelia, an image-analysis app for immunology. You help a
scientist see what is in their project. You point at things; you do not judge them.

The project is `$(project_uid)`. Read it with the tools before you answer. You can only read.

Reply ONLY through the structured output. Each claim is ONE fact — never two joined by "and", never a
fact plus what it suggests. Each claim carries at least one ref to an app object, and every ref must be
an object you saw THIS turn — in a tool result or in the attached context — with its exact ids copied
from there. Never cite from memory or from earlier in the conversation. Never invent an id.

Claim kinds:
- observation: what is shown. Numbers, counts, names, differences — no reasons.
- interpretation: what you think it means. Start with "I think". Use sparingly.
- question: a checkable next look ("Is track 12 in the same population at t=40?"), never an instruction.

Never recommend including, excluding or trusting data. Never reassure ("that's normal", "nothing to
worry about"). Never explain why two things differ — say that they do, and ask a question.
If you cannot say anything from what you saw, set abstain to true and give no claims.
"""

"""
    kiwi_context_pack(project_uid, refs) -> String

The attached refs, resolved, as text for the prompt — so the engine starts from what the user pointed
at instead of browsing for it (Decision 11). v1 carries each ref and its resolved label; the fuller
pack (a text rendering of each object's content, which Open decision 5's support checker also needs) is
later work. An unresolvable attached ref is listed with its error rather than dropped, so the engine
can say it couldn't find it.
"""
function kiwi_context_pack(project_uid::AbstractString, refs)::String
    isempty(refs) && return ""
    lines = String["Attached by the user (you may cite these as-is):"]
    for r in refs
        res = resolve_kiwi_ref(project_uid, r)
        push!(lines, string("- ", JSON3.write(r), res["ok"] ? "  → $(res["label"])" : "  → NOT FOUND: $(res["error"])"))
    end
    join(lines, "\n")
end

# ── "Seen this turn" (Decision 7) ────────────────────────────────────────────────────────────────────
#
# A cited ref counts as seen when it was attached by the user, or when its identifying values all
# appear in the text the engine received this turn (tool results + the context pack). Textual on
# purpose — deterministic and engine-independent — so it is a presence check on the ids, not proof the
# engine read them carefully. Integer ids match as whole numbers (label 12 is not seen in "123").

_kiwi_id_seen(id::Integer, text) = occursin(Regex("(?<![0-9])$(id)(?![0-9])"), text)
_kiwi_id_seen(id::AbstractString, text) = !isempty(id) && occursin(id, text)

# The values that identify a ref — what must have appeared for it to count as seen.
function _kiwi_ref_anchors(ref)::Vector{Any}
    g(k) = _kiwi_get(ref, k)
    kind = string(g("kind"))
    kind == "project"    && return Any[]                       # the turn's own project: always in view
    kind == "set"        && return Any[String(g("setUid"))]
    kind in ("image", "viewer") && return Any[String(g("imageUid"))]
    if kind == "population"                                     # the pop's leaf name, as tools print it
        parts = split(String(g("popPath")), '/'; keepempty = false)
        return Any[String(g("imageUid")), isempty(parts) ? String(g("popPath")) : String(last(parts))]
    end
    kind == "cells"      && return Any[String(g("imageUid")), Int.(g("labelIds"))...]
    kind == "tracks"     && return Any[String(g("imageUid")), Int.(g("trackIds"))...]
    kind == "tile"       && return Any[String(g("imageUid")), String(g("cellId"))]
    kind == "plot"       && return Any[String(g("plotId"))]
    kind == "capture"    && return Any[String(g("captureId"))]
    kind == "task"       && return Any[String(g("funName"))]
    kind == "ui"         && return Any[String(g("anchor"))]
    kind == "blackboard" && return Any[String(g("entryId"))]
    Any[]
end

# Key-order-independent identity of a ref (Symbol- or String-keyed), for "was this one attached?".
_kiwi_canon(d) = JSON3.write(sort!([String(k) => v for (k, v) in pairs(d)]; by = first))

"""
    kiwi_ref_seen(ref, seen_text, attached) -> Bool

Decision 7: was this ref attached by the user, or do all its identifying values appear in what the
engine saw this turn? PURE → tested.
"""
function kiwi_ref_seen(ref, seen_text::AbstractString, attached)::Bool
    c = _kiwi_canon(ref)
    any(a -> _kiwi_canon(a) == c, attached) && return true
    all(x -> _kiwi_id_seen(x, seen_text), _kiwi_ref_anchors(ref))
end

# ── Claim shape: one fact, pointed at precisely ──────────────────────────────────────────────────────
#
# The partial Phase 3 gate run (29 live turns, 2026-09-23) showed the two faults no schema variant
# fixes: most claims bundled several facts, and most cited the whole image for a population or track
# they named. Both are checked here, textually and deterministically (Decision 6 — no LLM judge), and a
# failure goes into the same one re-ask. Heuristics: they will misfire on some phrasing — the eval
# harness reports how often they fire (`reaskErrors`), which is the number to watch.

# Longer than this is almost always two facts: the live run's claims were p50 102, p90 185 characters.
const KIWI_CLAIM_MAX_CHARS = 160

"""
    kiwi_claim_bundling(text) -> String

Why `text` reads as more than one fact, or "" if it doesn't: over `KIWI_CLAIM_MAX_CHARS`, a
semicolon or em-dash join, a second sentence, or a list of three or more facts (inline or in
parentheses — a list of names is one fact, see `_kiwi_clause_list`). Thousands separators ("1,449") don't count as list commas. PURE → tested.
"""
function kiwi_claim_bundling(text::AbstractString)::String
    length(text) > KIWI_CLAIM_MAX_CHARS && return "longer than $KIWI_CLAIM_MAX_CHARS characters"
    t = replace(text, r"(?<=\d),(?=\d{3}\b)" => "", r"\b(e\.g|i\.e|vs|approx)\." => s"\1")
    occursin(';', t) && return "two facts joined by a semicolon"
    occursin(r"\s[—–]\s|\s--\s", t) && return "two facts joined by a dash"
    occursin(r"[.!?]\s+[A-Z]", t) && return "more than one sentence"
    any(m -> _kiwi_clause_list(m.captures[1]), eachmatch(r"\(([^)]*)\)", t)) &&
        return "a list of three or more facts in parentheses"
    _kiwi_clause_list(replace(t, r"\([^)]*\)" => "")) && return "a list of three or more facts"
    ""
end

# Three or more comma-separated items where an inner item is a clause (3+ words: "2 pops on default")
# — several facts. A list of names ("coastalFg, coastalSm15, cpSAM2", "(movement, test, here)",
# "/qc 0.63, /Scanning 0.66") is one fact about a set, not a bundle: the 2026-09-23 sanity run's only
# post-re-ask failures were three such enumerations flagged by a plain comma count.
function _kiwi_clause_list(s::AbstractString)::Bool
    parts = split(last(split(s, r":\s")), ',')      # "6 are tracked: a, b, c" — the list is after the colon
    length(parts) >= 3 || return false
    any(p -> length(split(replace(strip(p), r"^(and|or)\s+" => ""))) >= 3, parts[2:end-1])
end

# Objects a claim's TEXT names more precisely than an image: population paths ("/qc/CD169-", as the
# tools print them) and numbered tracks / cells ("track 12", "cell #40"). A path must start the token
# (so "µm/min", "1/3" and "n/a" are not paths) and not be a file path — a filesystem root or a file
# extension ("/home/…/crop.tif", which images' oriPath carries — live run 2026-09-23); an id must end
# the token (so "3w4IY5" is not track 3).
const _KIWI_TEXT_POP_RE   = r"(?<![\w/.])(?>/[A-Za-z0-9_+\-]+(?:/[A-Za-z0-9_+\-]+)*)(?!\.\w)"   # atomic: no backtracking into a file name
const _KIWI_FS_ROOT_RE    = r"^/(home|Users|tmp|mnt|media|Volumes|data|opt|var|usr|srv)(/|$)"
const _KIWI_TEXT_TRACK_RE = r"\btracks?\s+(?:id\s+)?#?(\d+)(?![\w.])"i
const _KIWI_TEXT_CELL_RE  = r"\b(?:cells?|labels?)\s+(?:id\s+)?#?(\d+)(?![\w.])"i

"""
    kiwi_claim_underspecified(text, refs) -> Vector{String}

What the claim's text names that none of its refs points at: every population path in the text needs
a `population` ref with that path (or one ending in it — "/Directed" for "/tracked/Directed"), every
"track N" a `tracks` ref holding N, every "cell N" / "label N" a `cells` ref holding N. PURE → tested.
"""
function kiwi_claim_underspecified(text::AbstractString, refs)::Vector{String}
    kinds_of(k) = [r for r in refs if string(_kiwi_get(r, "kind")) == k]
    pops   = [String(_kiwi_get(r, "popPath", "")) for r in kinds_of("population")]
    tracks = Set{Int}(Int(x) for r in kinds_of("tracks") for x in _kiwi_get(r, "trackIds", Int[]))
    cells  = Set{Int}(Int(x) for r in kinds_of("cells")  for x in _kiwi_get(r, "labelIds", Int[]))
    missing_ = String[]
    for m in eachmatch(_KIWI_TEXT_POP_RE, text)
        p = String(m.match)
        occursin(_KIWI_FS_ROOT_RE, p) && continue
        any(q -> q == p || endswith(q, p), pops) || push!(missing_, "population $p (cite it as a population ref)")
    end
    for m in eachmatch(_KIWI_TEXT_TRACK_RE, text)
        parse(Int, m.captures[1]) in tracks || push!(missing_, "track $(m.captures[1]) (cite it as a tracks ref)")
    end
    for m in eachmatch(_KIWI_TEXT_CELL_RE, text)
        parse(Int, m.captures[1]) in cells || push!(missing_, "cell $(m.captures[1]) (cite it as a cells ref)")
    end
    unique(missing_)
end

"""
    kiwi_validate_reply(project_uid, reply, seen_text, attached) -> (claims, errors)

Check a structured reply. Every ref of every claim must resolve (`resolve_kiwi_ref`) AND be seen this
turn (`kiwi_ref_seen`); every claim must be one fact (`kiwi_claim_bundling`) and cite what its text
names (`kiwi_claim_underspecified`). Returns the claims annotated per ref (`{ref, result, seen}`) and
the list of problems, each naming the claim — the text of the re-ask. An abstaining reply with no
claims is valid (Decision 9); a non-abstaining reply with no claims is not.
"""
function kiwi_validate_reply(project_uid::AbstractString, reply, seen_text::AbstractString, attached)
    errors = String[]
    claims = Dict{String,Any}[]
    reply isa AbstractDict || return (claims, ["the reply was not an object"])
    abstain = _kiwi_get(reply, "abstain", false) == true
    raw = _kiwi_get(reply, "claims", Any[])
    (!abstain && isempty(raw)) && push!(errors, "no claims and abstain is false — give claims or abstain")
    for (i, c) in enumerate(raw)
        kind = string(_kiwi_get(c, "kind", ""))
        kind in KIWI_CLAIM_KINDS || push!(errors, "claim $i: kind \"$kind\" is not one of $(join(KIWI_CLAIM_KINDS, ", "))")
        refs = _kiwi_get(c, "refs", Any[])
        isempty(refs) && push!(errors, "claim $i has no refs")
        annotated = Dict{String,Any}[]
        for r in refs
            res  = resolve_kiwi_ref(project_uid, r)
            seen = res["ok"] && kiwi_ref_seen(r, seen_text, attached)
            push!(annotated, Dict{String,Any}("ref" => r, "result" => res, "seen" => seen))
            desc = JSON3.write(r)
            if !res["ok"]
                push!(errors, "claim $i: ref $desc — $(res["error"])")
            elseif !seen
                push!(errors, "claim $i: ref $desc was not in any tool result or attachment this turn")
            end
        end
        text = string(_kiwi_get(c, "text", ""))
        why = kiwi_claim_bundling(text)
        isempty(why) || push!(errors, "claim $i is more than one fact ($why) — split it, one fact per claim")
        for u in kiwi_claim_underspecified(text, refs)
            push!(errors, "claim $i names $u")
        end
        push!(claims, Dict{String,Any}("kind" => kind, "text" => text, "refs" => annotated))
    end
    (claims, errors)
end

_kiwi_reask_prompt(errors) = """
Your reply failed validation:
$(join(("- " * e for e in errors), "\n"))

Reply again under the same schema. Split a claim that is more than one fact into separate short
claims, each with its own refs. Point at the most specific object a claim names — the population or
tracks, not only their image. Cite only objects you saw in a tool result or the attachments this turn,
copying their exact ids — call a tool first if you need to see one. Drop any claim you cannot support
that way. If nothing is left, abstain."""

"""
    run_kiwi_turn(project_uid, prompt; refs = [], agent = ClaudeAgent(), reasoning = false,
                  session_id = "", mcp_config_path = …, timeout_s = 300) -> Dict

One Kiwi turn (see the top of this file). Returns
`{ok, abstain, claims, reasoning, errors, reasked, reaskErrors, sessionId, usage:{input,output}, toolCalls,
seconds}` where `ok` means every claim passed validation, possibly after the one re-ask; `errors` lists
what still failed and `reaskErrors` what the first attempt failed on (empty if no re-ask). Never
throws for an engine failure — it's reported in `errors`.
"""
function run_kiwi_turn(project_uid::AbstractString, prompt::AbstractString;
                       refs = Any[], agent::Cecelia.AgentBackend = ClaudeAgent(),
                       reasoning::Bool = false, session_id::AbstractString = "",
                       mcp_config_path::AbstractString = _write_observer_mcp_config(; headless = true),
                       timeout_s::Real = 300)::Dict{String,Any}
    t0 = time()
    pack = kiwi_context_pack(project_uid, refs)
    full_prompt = isempty(pack) ? String(prompt) : string(prompt, "\n\n", pack)
    schema = kiwi_reply_schema(; reasoning)
    opts = (; system_prompt = kiwi_system_prompt(project_uid), replace_system_prompt = true,
              json_schema = schema, allowed_tools = _kiwi_allowed_tools(), strict_mcp = true,
              builtin_tools = "", stream = true, timeout_s)
    usage = [0, 0]; tool_calls = 0; reask_errors = String[]
    turn(p, sid) = (r = Cecelia.run_agent_turn(agent, p, mcp_config_path; session_id = sid, opts...);
                    usage[1] += r.input_tokens; usage[2] += r.output_tokens; tool_calls += length(r.tool_results); r)

    res = turn(full_prompt, String(session_id))
    seen = string(pack, "\n", join(res.tool_results, "\n"))
    out(ok, claims, errors, reasked, r) = Dict{String,Any}(
        "ok" => ok, "abstain" => (r.structured isa AbstractDict && _kiwi_get(r.structured, "abstain", false) == true),
        "claims" => claims, "errors" => errors, "reasked" => reasked,
        "reasoning" => r.structured isa AbstractDict ? string(_kiwi_get(r.structured, "reasoning", "")) : "",
        "sessionId" => r.session_id, "usage" => Dict("input" => usage[1], "output" => usage[2]),
        "toolCalls" => tool_calls, "reaskErrors" => reask_errors, "seconds" => round(time() - t0; digits = 1))
    res.ok || return out(false, Dict{String,Any}[], [res.error], false, res)

    claims, errors = kiwi_validate_reply(project_uid, res.structured, seen, refs)
    isempty(errors) && return out(true, claims, errors, false, res)

    # ONE re-ask on the same session, naming what failed. Refs seen in the first attempt stay seen.
    append!(reask_errors, errors)
    res2 = turn(_kiwi_reask_prompt(errors), res.session_id)
    res2.ok || return out(false, claims, vcat(errors, [res2.error]), true, res)
    seen2 = string(seen, "\n", join(res2.tool_results, "\n"))
    claims2, errors2 = kiwi_validate_reply(project_uid, res2.structured, seen2, refs)
    out(isempty(errors2), claims2, errors2, true, res2)
end
