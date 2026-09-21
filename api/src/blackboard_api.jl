# ── Bidirectional context — Part 4 (Blackboard) ────────────────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` Part 4. A BLACKBOARD ENTRY is a piece of shared thinking a user
# and Claude iterate on across sessions — Markdown notes (Mermaid diagrams via triple-backtick
# `mermaid` fences on the frontend), with attached captureIds when the visual is load-bearing.
# Distinct from a CHAIN (executable) or a NOTEBOOK (analysis code) — this is the working history.
#
# Storage. Per-project `<proj>/blackboard/<entryId>/{entry.md, meta.json, .snapshots/entry@v<N>.md}`;
# per-project registry at `<proj>/settings/blackboard.json`. Same shape as the notebook registry
# (Decision 21 — reimplement locally rather than extracting a versioning helper before rule-of-
# three; the two systems will drift on details and a premature shared abstraction locks the wrong
# invariants). Additive-only for MCP writes (create + revise); delete + restore + prune are
# user-driven (Kiwi / a future /blackboard page).
using Base64
using Dates

_blackboard_dir_for_project(uid::AbstractString) = joinpath(projects_dir(), uid, "blackboard")
_bb_entry_dir(uid::AbstractString, id::AbstractString) = joinpath(_blackboard_dir_for_project(uid), id)
_bb_registry_path(uid::AbstractString) = joinpath(_settings_dir_for_project(uid), "blackboard.json")

# Same id shape as captures: `bb-<yyyymmddThhmmss>-<6 hex>`. Sortable by name → sortable by time.
# The prefix + regex keeps a `..` payload from ever escaping the project dir when a client sends a
# malformed entryId (rejected before joinpath).
const _BB_ID_RE = r"^bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$"
# Reserved id for the project profile entry — Decision 2 in `docs/todo/PROJECT_MEMORY_PLAN.md`. One
# per project, auto-created on first list. Literal `profile` (not a bb-<ts> id) so the profile is
# recognisable in a path listing without a meta lookup, and so a Claude session can address it
# without first calling list_blackboard_entries. In DESC name sort `profile` > `bb-…` (p > b), so it
# naturally lands at the top of the list without a separate pin.
const _BB_PROFILE_ID = "profile"
function _new_bb_entry_id()::String
    ts = Dates.format(Dates.now(), dateformat"yyyymmddTHHMMSS")
    tail = bytes2hex(rand(UInt8, 3))
    string("bb-", ts, "-", tail)
end
_valid_bb_entry_id(id::AbstractString)::Bool =
    String(id) == _BB_PROFILE_ID || !isnothing(match(_BB_ID_RE, String(id)))

# Entry status — Decision 3 in `docs/todo/PROJECT_MEMORY_PLAN.md`. Additive metadata that answers
# "is this thread still on the table" without touching the entry's content or version history.
# `open` = actively being worked on; `resolved` = the topic settled, entry kept as record;
# `parked` = deliberately set aside, do not surface in the "what's open" briefing. Missing on read
# (any pre-status entry) backfills as `open` — the read-safe default that keeps a legacy entry
# visible in the briefing until someone explicitly retires it.
const _BB_STATUS_VALS = ("open", "resolved", "parked")
_valid_bb_status(s::AbstractString)::Bool = String(s) in _BB_STATUS_VALS
_status_from_meta(meta)::String =
    (s = String(get(meta, "status", "open")); _valid_bb_status(s) ? s : "open")

# Entry outcome — Decision 11 in `docs/todo/PROJECT_MEMORY_PLAN.md`. Additive metadata that answers
# "did this thread turn out to be right or wrong" for future Claude sessions. `good` = the finding
# / suggestion held up; `bad` = it didn't (with the required note explaining WHY it was wrong, since
# a verdict without a reason is useless in a future briefing). Missing ⇒ untagged (no signal); the
# distinction between "untagged" and "neutral" is intentional (see Decision 11 D4 — no neutral state).
# Notes are capped so a paste can't blow the meta.json; 2 KiB is enough for a couple of sentences
# with references, not a whole essay.
const _BB_OUTCOME_VERDICTS = ("good", "bad")
const _BB_OUTCOME_NOTE_MAX_BYTES = 2 * 1024
_valid_bb_outcome_verdict(v::AbstractString)::Bool = String(v) in _BB_OUTCOME_VERDICTS

"""
    _outcome_from_meta(meta) -> Union{Dict{String,Any},Nothing}

Return the outcome dict recorded on `meta` (`{verdict, note, taggedAt}`), or `nothing` when the
entry is untagged or the on-disk shape is malformed. Kept absent-on-untagged so a briefing / list
row can distinguish "no signal" from "explicitly neutral" (Decision 11 D4).
"""
function _outcome_from_meta(meta)::Union{Dict{String,Any},Nothing}
    o = get(meta, "outcome", nothing)
    o isa AbstractDict || return nothing
    v = String(get(o, "verdict", ""))
    _valid_bb_outcome_verdict(v) || return nothing
    note = String(get(o, "note", ""))
    isempty(note) && return nothing
    Dict{String,Any}(
        "verdict"  => v,
        "note"     => note,
        "taggedAt" => String(get(o, "taggedAt", "")),
    )
end

# Entry fingerprint — PROJECT_MEMORY_PLAN Phase 5.1/5.2. Small, structured, set-once at create so
# a retrieval pass can score a new entry's context against the failure fingerprints banked on
# `bad`-tagged entries in the corpus. INTERNAL key, not a public interface — the version field
# lets the schema move forward without a migration: retrieval reads `v` and dispatches, an older-v
# entry with an unrecognised field is treated as "no signal for that dimension" rather than
# rejected.
#   v1 (2026-09-21): channel_count + stain_classes + pipeline_stage
#   v2 (2026-09-21): + modality + tissue_context (MCP-side extractors — see docs/inventory/fingerprint_extractors.md)
# Kept ≤ 2 KiB so meta.json stays a cheap read; a fingerprint that runs long is a bug
# (fingerprints are keys, not payloads — pointers back to the entry carry the detail).
const _BB_FINGERPRINT_VERSION      = 2
const _BB_FINGERPRINT_MAX_BYTES    = 2 * 1024
_valid_bb_fingerprint(fp) = (fp isa AbstractDict) &&
    (haskey(fp, "v") || haskey(fp, :v)) &&
    (try Int(get(fp, "v", get(fp, :v, 0))) > 0 catch; false end)

"""
    _fingerprint_from_meta(meta) -> Union{Dict{String,Any},Nothing}

Return the fingerprint recorded on `meta`, or `nothing` when the entry has none / the on-disk shape
is malformed. Kept absent-on-missing (same discipline as `outcome`) so a reader can distinguish "no
fingerprint at all" from "fingerprint present but no signal on this dimension". The stored dict is
shallow-copied under `String` keys so a downstream consumer can add fields without mutating meta.
"""
function _fingerprint_from_meta(meta)::Union{Dict{String,Any},Nothing}
    fp = get(meta, "fingerprint", nothing)
    _valid_bb_fingerprint(fp) || return nothing
    out = Dict{String,Any}()
    for (k, v) in fp
        out[String(k)] = v
    end
    out
end

# Blackboard search (PROJECT_MEMORY_PLAN Decision 4). Substring, case-insensitive, per project.
# Cheap at the sizes this store reaches — a title match beats a body match; snippet is ±40 chars
# around the first hit. `limit` bounded so a runaway caller can't blow the response.
const _BB_SEARCH_LIMIT_DEFAULT = 10
const _BB_SEARCH_LIMIT_MAX     = 50
const _BB_SEARCH_SNIPPET_HALF  = 40

# Outcome tiebreak (PROJECT_MEMORY_PLAN Decision 12). Applied within a search bucket (title/body)
# and inside the briefing's open-entries slice: bad beats good beats untagged on equal match
# strength. Rationale: a `bad` verdict is a known trap a future session must SEE before proposing
# on the same topic; leading with it is the whole point of the tag. Untagged means "no signal", so
# it sits below any explicit verdict. Numeric so `sort` / `sortperm` can key on it directly.
_bb_outcome_rank(verdict::AbstractString)::Int =
    verdict == "bad"  ? 0 :
    verdict == "good" ? 1 : 2
_bb_outcome_rank(::Nothing)::Int = 2
_bb_outcome_rank(o::AbstractDict)::Int = _bb_outcome_rank(String(get(o, "verdict", "")))

"""
    _bb_search_snippet(text, pos, needle_len)

Return a substring of `text` centred on the byte position `pos` (1-indexed as returned by
`findfirst`), extending `_BB_SEARCH_SNIPPET_HALF` chars on each side and adding "…" markers when
truncated. Kept as a helper so the title-match and body-match branches share one snippet shape.
"""
function _bb_search_snippet(text::AbstractString, pos::Int, needle_len::Int)::String
    n = ncodeunits(text)
    lo = max(1, pos - _BB_SEARCH_SNIPPET_HALF)
    hi = min(n, pos + needle_len - 1 + _BB_SEARCH_SNIPPET_HALF)
    # Nudge lo/hi to valid character boundaries so a substring slice never lands mid-codepoint.
    while lo > 1 && !isvalid(text, lo); lo -= 1; end
    while hi < n && !isvalid(text, hi + 1); hi += 1; end
    core = text[lo:hi]
    lead  = lo > 1 ? "…" : ""
    trail = hi < n ? "…" : ""
    string(lead, core, trail)
end

# Titles are a short label shown in the entries table — cap so a verbose caller can't bloat the
# row. Same rule as notebooks' `_NB_DESC_MAX`.
const _BB_TITLE_MAX = 200
_cap_bb_title(s::AbstractString)::String =
    (t = strip(String(s)); length(t) > _BB_TITLE_MAX ? String(first(t, _BB_TITLE_MAX)) : String(t))

# Blackboard entries are notes, not novels. 100 KiB is enough for a rich Markdown page with a few
# Mermaid diagrams; a 1 MiB paste is a bug or an abuse, not a legit entry. Applies to create AND
# revise so the invariant holds across the entry's lifetime.
const _BB_CONTENT_MAX_BYTES = 100 * 1024
function _valid_bb_content(s::AbstractString)::Bool
    length(codeunits(String(s))) <= _BB_CONTENT_MAX_BYTES
end

# Attachments = a list of captureIds already on disk in `<proj>/captures/`. We don't COPY them;
# a blackboard entry cites them by id, and the frontend follows the pointer. Ignore unknown /
# malformed ids so a caller with a stale reference doesn't get an opaque 400 — the stored list
# reflects what actually exists at the moment of write.
function _clean_attachments(uid::AbstractString, raw)::Vector{String}
    raw isa AbstractVector || return String[]
    cap_root = joinpath(projects_dir(), uid, "captures")
    out = String[]
    for v in raw
        v isa AbstractString || continue
        s = String(v)
        _valid_capture_id(s) || continue
        isdir(joinpath(cap_root, s)) || continue
        s in out || push!(out, s)
    end
    out
end

# ── Registry ──────────────────────────────────────────────────────────────────
# Keyed by entryId → `{title, current, updatedAt}`. Same rules as `_read_registry` in
# notebooks_api.jl: unreadable ⇒ empty (a corrupt registry doesn't block work).
function _read_bb_registry(uid::AbstractString)::Dict{String,Any}
    p = _bb_registry_path(uid)
    isfile(p) || return Dict{String,Any}()
    try
        Dict{String,Any}(String(k) => Dict{String,Any}(v)
                         for (k, v) in JSON3.read(read(p, String), Dict{String,Any}))
    catch
        Dict{String,Any}()
    end
end
function _write_bb_registry!(uid::AbstractString, reg::AbstractDict)
    mkpath(_settings_dir_for_project(uid))
    write_json_atomic(_bb_registry_path(uid), reg)
end

# Snapshot versions on disk for one entry (`.snapshots/entry@v<N>.md`). Same shape as notebooks'
# `_snapshot_versions` — the byte-slice between the "entry@v" prefix and ".md" is safe because the
# filenames are ASCII and we generate them ourselves.
function _bb_snapshot_versions(uid::AbstractString, id::AbstractString)::Vector{Int}
    snapdir = joinpath(_bb_entry_dir(uid, id), ".snapshots")
    vs = Int[]
    isdir(snapdir) || return vs
    for f in readdir(snapdir)
        (startswith(f, "entry@v") && endswith(f, ".md")) || continue
        v = tryparse(Int, f[(length("entry@v") + 1):(length(f) - 3)])
        v === nothing || push!(vs, v)
    end
    vs
end
_next_bb_snapshot_version(uid, id) =
    (vs = _bb_snapshot_versions(uid, id); isempty(vs) ? 1 : maximum(vs) + 1)

# One meta.json write, called from every mutation so the field order + defaults stay in one place.
# `outcome`: pass a `Dict{String,Any}("verdict"=>…, "note"=>…, "taggedAt"=>…)` to record a Decision-11
# outcome; pass `nothing` to omit the field entirely (untagged state). Every mutation site reads the
# prior outcome from meta and passes it back so a revise/status flip preserves the tag.
# `fingerprint`: pass a `{"v"=>N, ...}` dict to record a P5.1 fingerprint; `nothing` to omit. Set
# once at create; every mutation site reads the prior fingerprint from meta and passes it back so
# no downstream write can accidentally drop it.
function _write_bb_meta!(uid::AbstractString, id::AbstractString;
    title, createdAt, updatedAt, current, attachments,
    snapshots = Any[], status::AbstractString = "open",
    outcome::Union{Nothing,AbstractDict} = nothing,
    fingerprint::Union{Nothing,AbstractDict} = nothing)
    dir = _bb_entry_dir(uid, id)
    meta = Dict{String,Any}(
        "entryId"     => id,
        "title"       => title,
        "createdAt"   => createdAt,
        "updatedAt"   => updatedAt,
        "current"     => current,          # snapshot version the LIVE entry.md reflects (0 = never snapshotted)
        "attachments" => attachments,
        # Per-version attachment record. Every element = `{version, attachments, updatedAt}` for the
        # state that was CURRENT at the moment the snapshot fired. Attachments aren't stored in the
        # snapshot .md (they aren't part of the markdown), so `read_at_version` looks here to answer
        # "what was attached when v<N> was live". Old meta.json files without this key ⇒ empty list;
        # `read_at_version` falls back to current attachments (best it can do for a legacy entry).
        "snapshots"   => snapshots,
        # Status is entry-level metadata, not versioned per snapshot — flipping "open" → "resolved"
        # is a state transition on the WHOLE entry, not a content revision, so it doesn't fire a
        # snapshot (see api_blackboard_status). PROJECT_MEMORY_PLAN Decision 3.
        "status"      => _valid_bb_status(status) ? String(status) : "open",
    )
    # Outcome — Decision 11. Absent-on-untagged: writing a null `outcome` would produce a shape a
    # reader has to distinguish from "untagged", which defeats the point of Decision 4 (absent =
    # no signal). Set only when the caller passes a real dict validated at the handler layer.
    if outcome !== nothing
        meta["outcome"] = Dict{String,Any}(
            "verdict"  => String(get(outcome, "verdict", "")),
            "note"     => String(get(outcome, "note", "")),
            "taggedAt" => String(get(outcome, "taggedAt", "")),
        )
    end
    # Fingerprint — P5.1. Same absent-on-missing discipline as outcome. The dict is shallow-copied
    # under String keys so the on-disk shape stays stable regardless of whether the caller passed
    # Symbol- or String-keyed data. `v` is coerced to Int; other fields pass through as-is (the
    # schema is defined by the caller — see MCP `_infer_fingerprint`).
    if _valid_bb_fingerprint(fingerprint)
        fp_out = Dict{String,Any}()
        for (k, v) in fingerprint
            fp_out[String(k)] = v
        end
        fp_out["v"] = Int(get(fp_out, "v", _BB_FINGERPRINT_VERSION))
        meta["fingerprint"] = fp_out
    end
    write_json_atomic(joinpath(dir, "meta.json"), meta)
end
function _read_bb_meta(uid::AbstractString, id::AbstractString)::Union{Dict{String,Any},Nothing}
    p = joinpath(_bb_entry_dir(uid, id), "meta.json")
    isfile(p) || return nothing
    try
        JSON3.read(read(p, String), Dict{String,Any})
    catch
        nothing
    end
end

# Snapshot the current entry.md as v<N> and bump current in meta + registry. Called by revise (so
# the pre-revision state is always restorable) and restore (so the pre-restore state is likewise
# restorable — the "restore loses un-snapshotted edits" papercut the plan flagged in notebooks).
function _snapshot_current!(uid::AbstractString, id::AbstractString)::Int
    dir = _bb_entry_dir(uid, id)
    live = joinpath(dir, "entry.md")
    isfile(live) || error("Entry has no live content to snapshot: $id")
    v = _next_bb_snapshot_version(uid, id)
    snapdir = joinpath(dir, ".snapshots"); mkpath(snapdir)
    cp(live, joinpath(snapdir, "entry@v$(v).md"); force = false)
    v
end

# Append a per-version attachment record to a snapshots list. Kept as a helper so revise/restore
# read one consistent shape rather than each hand-building the dict. The list may not exist yet
# (first snapshot on an entry created before this field existed) — treat missing / wrong-typed
# as empty and grow from there.
function _append_snapshot_record(snapshots_in, version::Int, attachments, updatedAt::AbstractString)
    snapshots = (snapshots_in isa AbstractVector) ? Any[copy(x) for x in snapshots_in] : Any[]
    push!(snapshots, Dict{String,Any}(
        "version"     => version,
        "attachments" => attachments,
        "updatedAt"   => String(updatedAt),
    ))
    snapshots
end

# Return the attachments recorded for `version` (from meta["snapshots"]). Nothing recorded ⇒
# `nothing` so the caller can decide the fallback (typically: use current attachments).
function _attachments_for_version(meta::AbstractDict, version::Int)
    snapshots = get(meta, "snapshots", Any[])
    snapshots isa AbstractVector || return nothing
    for s in snapshots
        s isa AbstractDict || continue
        (Int(get(s, "version", -1)) == version) || continue
        atts = get(s, "attachments", nothing)
        atts isa AbstractVector || return nothing
        return atts
    end
    nothing
end

# ── Handlers ──────────────────────────────────────────────────────────────────

# Placeholder body written on first ensure — five suggested headings from PROJECT_MEMORY_PLAN
# Decision 9 (subject + goal required; modality/cohort/channels suggested). The italic parenthetical
# lines are the "placeholder" marker the briefing's newProject check strips before deciding whether
# a section has been filled in — see `_profile_missing_required` in `mcp/cecelia_mcp/server.py`.
# Keep the placeholder markers on their own line and in the `_(…)_` form so that check stays cheap.
const _BB_PROFILE_PLACEHOLDER_BODY = """# Project profile

## Subject
_(a short description of what this data is — whose project, what tissue, what preparation)_

## Goal
_(what you're trying to answer with this project — the science question)_

## Modality
_(e.g. resonant intravital, spinning-disk fixed, light-sheet organoid)_

## Cohort / groups
_(experimental groups + how they're identified in image names or attributes)_

## Key channels
_(what each channel labels — e.g. c1 = CD169, c2 = MerTK)_
"""

"""
    _ensure_profile_entry!(uid)

Auto-create the reserved `profile` entry for this project if it doesn't already exist. Called at
the top of `api_blackboard_list` so a project that never asks for its blackboard doesn't get an
empty entry it never wanted; a session that opens the Blackboard page (or a Claude session that
lists entries) always finds a profile to write into.

Body seeded with `_BB_PROFILE_PLACEHOLDER_BODY` — the five suggested headings from
PROJECT_MEMORY_PLAN Decision 9. Filling in Subject and Goal is what flips `newProject` off in the
session briefing. Same on-disk shape as a normal entry — one place in Julia has to know the
reserved id, and this is it. PROJECT_MEMORY_PLAN Decision 2.
"""
function _ensure_profile_entry!(uid::AbstractString)
    dir = _bb_entry_dir(uid, _BB_PROFILE_ID)
    isfile(joinpath(dir, "meta.json")) && return
    mkpath(dir)
    write_atomic(joinpath(dir, "entry.md")) do io
        write(io, _BB_PROFILE_PLACEHOLDER_BODY)
    end
    ts = string(Dates.now())
    _write_bb_meta!(uid, _BB_PROFILE_ID;
        title = "Project profile", createdAt = ts, updatedAt = ts,
        current = 0, attachments = Any[], status = "open")
    reg = _read_bb_registry(uid)
    reg[_BB_PROFILE_ID] = Dict{String,Any}(
        "title" => "Project profile", "current" => 0, "updatedAt" => ts, "status" => "open")
    _write_bb_registry!(uid, reg)
    nothing
end

"""
    GET /api/blackboard?projectUid=…

Reply: `{ entries: [{ entryId, title, current, updatedAt, attachmentsCount, status }, …] }`,
newest-first by id (which encodes a sortable timestamp). The reserved `profile` entry sorts before
`bb-…` ids (p > b in DESC), so it naturally lands at the top of the list. Skips an entry whose
meta.json is unreadable rather than taking the list down — matches the captures/notebooks list
convention. `status` backfills to "open" for pre-Decision-3 entries.
"""
function api_blackboard_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    _ensure_profile_entry!(uid)

    dir = _blackboard_dir_for_project(uid)
    isdir(dir) || return 200, JSON3.write((; entries = Any[]))
    ids = sort!(String[e for e in readdir(dir) if _valid_bb_entry_id(e)], rev = true)
    entries = Any[]
    for id in ids
        meta = _read_bb_meta(uid, id)
        meta === nothing && continue
        atts = get(meta, "attachments", Any[])
        row = Dict{String,Any}(
            "entryId"          => id,
            "title"            => String(get(meta, "title", "")),
            "current"          => Int(get(meta, "current", 0)),
            "updatedAt"        => String(get(meta, "updatedAt", "")),
            "attachmentsCount" => atts isa AbstractVector ? length(atts) : 0,
            "status"           => _status_from_meta(meta),
        )
        # Only carry `outcome` on rows that are actually tagged — absent means "no signal" (Decision
        # 11 D4). Rows for the entry-list UI can then render nothing, "good", or "bad" without a
        # tri-state check against a placeholder.
        o = _outcome_from_meta(meta)
        o !== nothing && (row["outcome"] = o)
        # Fingerprint on the list row too — P5.1. Absent on legacy entries (created before v1) and
        # on entries whose caller didn't infer one. Costs a few bytes per row but lets a retrieval
        # pass score the whole project without a per-entry fetch.
        fp = _fingerprint_from_meta(meta)
        fp !== nothing && (row["fingerprint"] = fp)
        push!(entries, row)
    end
    200, JSON3.write((; entries = entries))
end

"""
    GET /api/blackboard/entry?projectUid=…&entryId=…[&version=N]

Reply: `{ entry: { entryId, title, content, current, updatedAt, versions, attachments } }`.
When `version` is given, `content` reads back the snapshot at that version (unchanged current /
updatedAt / attachments in the response body — those describe the LIVE state). Unknown version ⇒
404 rather than falling back to current, so a stale caller doesn't silently read fresher content.
"""
function api_blackboard_entry_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    id  = get(query, "entryId", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))

    dir = _bb_entry_dir(uid, id)
    versions = _bb_snapshot_versions(uid, id)
    content = ""
    # Attachments in the response describe the STATE requested — the live set for a live read, the
    # snapshot's own set for a `?version=` read (from meta["snapshots"]). Pre-A' entries have no
    # snapshot record ⇒ fall back to current attachments (best we can do; the alternative is
    # returning [] which reads as "attachments were removed", which is wrong).
    attachments_live = get(meta, "attachments", Any[])
    attachments_out = attachments_live
    v_asked = tryparse(Int, get(query, "version", ""))
    if v_asked !== nothing
        v_asked in versions ||
            return 404, JSON3.write((; error = "Snapshot version not found: $v_asked"))
        content = read(joinpath(dir, ".snapshots", "entry@v$(v_asked).md"), String)
        recorded = _attachments_for_version(meta, v_asked)
        recorded === nothing || (attachments_out = recorded)
    else
        live = joinpath(dir, "entry.md")
        content = isfile(live) ? read(live, String) : ""
    end
    entry_out = Dict{String,Any}(
        "entryId"     => id,
        "title"       => String(get(meta, "title", "")),
        "content"     => content,
        "current"     => Int(get(meta, "current", 0)),
        "updatedAt"   => String(get(meta, "updatedAt", "")),
        "versions"    => sort(versions),
        "attachments" => attachments_out,
        "status"      => _status_from_meta(meta),  # describes the LIVE entry; not versioned per snapshot
    )
    # Outcome describes the LIVE entry too (like status — not per-snapshot); absent when untagged.
    o = _outcome_from_meta(meta)
    o !== nothing && (entry_out["outcome"] = o)
    # Fingerprint describes the entry's ORIGINAL context (set at create), not the live state — but
    # it's a property of the entry as a whole, so the read exposes it alongside status/outcome.
    fp = _fingerprint_from_meta(meta)
    fp !== nothing && (entry_out["fingerprint"] = fp)
    200, JSON3.write((; entry = entry_out))
end

"""
    POST /api/blackboard/search

Body: `{ projectUid, query, status?, limit? }` where `status ∈ ("open","resolved","parked")` and
`limit` ≤ `_BB_SEARCH_LIMIT_MAX` (default `_BB_SEARCH_LIMIT_DEFAULT`).
Reply: `{ results: [{ entryId, title, snippet, status, updatedAt, matchType }, …] }`, capped at
`limit`. `matchType ∈ ("title", "body")` — a title match orders before a body match; within each
group, newer entries come first (id encodes a sortable timestamp; `profile` sorts before any
`bb-…`, which is fine — an entry that matches the query is what the caller wants regardless of id).

Case-insensitive substring over title + entry body. Not semantic search — a v1 that's cheap at
the sizes this store reaches (Blackboard bodies are capped at 100 KiB per entry, and there are on
the order of tens per project). PROJECT_MEMORY_PLAN Decision 4. If substring stops surfacing what
sessions actually need, semantic is the FUTURE.md follow-up; the endpoint shape stays.

`snippet` is ±_BB_SEARCH_SNIPPET_HALF chars around the first match in whichever field matched;
for a title-only match the snippet is the (possibly truncated) title itself. Unreadable meta.json
skipped, same convention as list.
"""
function api_blackboard_search(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    q   = _wstr(body, :query)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(q)   && return 400, JSON3.write((; error = "query required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    status_filter = String(get(body, :status, ""))
    isempty(status_filter) || _valid_bb_status(status_filter) ||
        return 400, JSON3.write((; error = "status must be one of $(_BB_STATUS_VALS)"))

    limit_raw = get(body, :limit, _BB_SEARCH_LIMIT_DEFAULT)
    limit = try
        clamp(Int(limit_raw), 1, _BB_SEARCH_LIMIT_MAX)
    catch
        _BB_SEARCH_LIMIT_DEFAULT
    end

    dir = _blackboard_dir_for_project(uid)
    isdir(dir) || return 200, JSON3.write((; results = Any[]))
    needle = lowercase(q)
    # Two-bucket collection: title matches beat body matches so a caller reading top-of-list gets
    # the intent-shaped hits first. Newest-first within each bucket comes from the reverse-sorted
    # ids we read the dir with — `profile` sorts to the top per the DESC id ordering, which is
    # fine: a search hit is what the caller asked for; id-precedence is only a tiebreaker.
    # Decision 12: within each bucket, `bad` > `good` > untagged (outcome tiebreak). We scan ALL
    # matching entries first — no early stop — so a `bad`-tagged hit at a lower ID still surfaces
    # ahead of an untagged one at a higher ID. Full scan is cheap at this store's sizes.
    ids = sort!(String[e for e in readdir(dir) if _valid_bb_entry_id(e)], rev = true)
    title_hits = Any[]; body_hits = Any[]
    for id in ids
        meta = _read_bb_meta(uid, id)
        meta === nothing && continue
        entry_status = _status_from_meta(meta)
        isempty(status_filter) || entry_status == status_filter || continue
        title = String(get(meta, "title", ""))
        title_lc = lowercase(title)
        entry_outcome = _outcome_from_meta(meta)
        title_pos = findfirst(needle, title_lc)
        if title_pos !== nothing
            row = Dict{String,Any}(
                "entryId"   => id,
                "title"     => title,
                "snippet"   => _bb_search_snippet(title, first(title_pos), length(needle)),
                "status"    => entry_status,
                "updatedAt" => String(get(meta, "updatedAt", "")),
                "matchType" => "title",
            )
            entry_outcome !== nothing && (row["outcome"] = entry_outcome)
            push!(title_hits, row)
            continue
        end
        # Body scan — read entry.md and look for the needle. Skip missing/unreadable body files
        # (a directory without an entry.md is an in-flight state; not fatal).
        live = joinpath(_bb_entry_dir(uid, id), "entry.md")
        isfile(live) || continue
        body_txt = try
            read(live, String)
        catch
            continue
        end
        body_lc = lowercase(body_txt)
        body_pos = findfirst(needle, body_lc)
        body_pos === nothing && continue
        row = Dict{String,Any}(
            "entryId"   => id,
            "title"     => title,
            "snippet"   => _bb_search_snippet(body_txt, first(body_pos), length(needle)),
            "status"    => entry_status,
            "updatedAt" => String(get(meta, "updatedAt", "")),
            "matchType" => "body",
        )
        entry_outcome !== nothing && (row["outcome"] = entry_outcome)
        push!(body_hits, row)
    end
    # Sort each bucket by outcome rank (bad<good<untagged), stable so newer-id-first survives from
    # the reverse-sorted ids we scanned in. `sort` in Julia is stable by default. Then vcat (titles
    # still beat bodies as the primary key) and cap.
    sort!(title_hits, by = r -> _bb_outcome_rank(get(r, "outcome", nothing)))
    sort!(body_hits,  by = r -> _bb_outcome_rank(get(r, "outcome", nothing)))
    out = vcat(title_hits, body_hits)
    length(out) > limit && (out = out[1:limit])
    200, JSON3.write((; results = out))
end

"""
    POST /api/blackboard/status

Body: `{ projectUid, entryId, status }` where `status ∈ ("open","resolved","parked")`
Reply: `{ ok:true, status, unchanged? }`

Flips an entry's status field in place. Does NOT touch entry.md or create a snapshot — status is
entry-level metadata (not content), so a status change and a content revision travel separately.
A no-op (same status as current) is idempotent: response carries `unchanged:true` and nothing is
rewritten. Broadcasts `blackboard:changed` on a real transition so a Kiwi / Blackboard list
refresh picks it up. PROJECT_MEMORY_PLAN Decision 3.

Additive-write from Claude's side: allow-listed in the MCP client so a session can retire a thread
it has finished with, but never delete an entry (delete stays user-driven, per Part 4's discipline).
"""
function api_blackboard_status(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid    = _wstr(body, :projectUid)
    id     = _wstr(body, :entryId)
    status = _wstr(body, :status)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    _valid_bb_status(status) ||
        return 400, JSON3.write((; error = "status must be one of $(_BB_STATUS_VALS)"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))

    prev = _status_from_meta(meta)
    if prev == status
        return 200, JSON3.write((; ok = true, status = status, unchanged = true))
    end

    ts = string(Dates.now())
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts,                                      # status change is a state transition — bumps updatedAt
        current = Int(get(meta, "current", 0)),
        attachments = get(meta, "attachments", Any[]),
        snapshots = get(meta, "snapshots", Any[]),
        status = status,
        outcome = _outcome_from_meta(meta),
        fingerprint = _fingerprint_from_meta(meta))

    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    entry["status"]    = status
    entry["updatedAt"] = ts
    _write_bb_registry!(uid, reg)

    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, status = status))
end

"""
    POST /api/blackboard/outcome

Body: `{ projectUid, entryId, verdict: "good"|"bad", note: string }`
Reply: `{ ok:true, outcome: {verdict, note, taggedAt}, unchanged?: true }`

Tag a Blackboard entry as good (the finding / decision held up) or bad (it didn't). The note is
required — a verdict without an explanation is useless to a future Claude session, which is the
whole point of Decision 11 D2. Same discipline as `/status`: additive, no snapshot fired, preserves
the tagged entry's content + version history. A no-op (same verdict AND same note) is idempotent.
Broadcasts `blackboard:changed` on a real update so a Kiwi / Blackboard list refresh picks it up.
PROJECT_MEMORY_PLAN Decision 11.

Additive-write from Claude's side: allow-listed in the MCP client so a session can retire what
turned out wrong (or confirm what worked) for future sessions to lean on.
"""
function api_blackboard_outcome(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid     = _wstr(body, :projectUid)
    id      = _wstr(body, :entryId)
    verdict = _wstr(body, :verdict)
    note    = String(get(body, :note, ""))
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    _valid_bb_outcome_verdict(verdict) ||
        return 400, JSON3.write((; error = "verdict must be one of $(_BB_OUTCOME_VERDICTS)"))
    # Decision 11 D2 — note required. A verdict without a note is refused rather than silently
    # stored; the note is the part a future session actually reads.
    stripped_note = strip(note)
    isempty(stripped_note) && return 400, JSON3.write((; error = "note required (must be non-empty)"))
    length(codeunits(stripped_note)) > _BB_OUTCOME_NOTE_MAX_BYTES &&
        return 400, JSON3.write((; error = "note exceeds $_BB_OUTCOME_NOTE_MAX_BYTES bytes"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))

    note_str = String(stripped_note)
    prev = _outcome_from_meta(meta)
    if prev !== nothing && prev["verdict"] == verdict && prev["note"] == note_str
        return 200, JSON3.write((; ok = true, outcome = prev, unchanged = true))
    end

    ts = string(Dates.now())
    new_outcome = Dict{String,Any}(
        "verdict"  => verdict,
        "note"     => note_str,
        "taggedAt" => ts,
    )
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts,                                      # outcome change bumps updatedAt (a state transition on the whole entry)
        current = Int(get(meta, "current", 0)),
        attachments = get(meta, "attachments", Any[]),
        snapshots = get(meta, "snapshots", Any[]),
        status = _status_from_meta(meta),
        outcome = new_outcome,
        fingerprint = _fingerprint_from_meta(meta))

    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    # Registry mirrors just the verdict (for cheap filter without loading meta); the note lives in
    # meta. Keeps the registry row small and doesn't duplicate the 2 KiB note per project-wide list.
    entry["outcome"]   = verdict
    entry["updatedAt"] = ts
    _write_bb_registry!(uid, reg)

    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, outcome = new_outcome))
end

"""
    POST /api/blackboard/create

Body: `{ projectUid, title, content, attachments?: [captureId, ...] }`
Reply: `{ ok:true, entryId }`

Creates `<proj>/blackboard/<entryId>/{entry.md, meta.json}` — no snapshot on create (current = 0
mirrors notebooks; first revision produces v1). Additive-only from the MCP surface; this is one of
the two write routes exposed to Claude.
"""
function api_blackboard_create(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    title   = _cap_bb_title(_wstr(body, :title))
    isempty(title) && return 400, JSON3.write((; error = "title required"))
    content = String(get(body, :content, ""))
    _valid_bb_content(content) ||
        return 400, JSON3.write((; error = "content exceeds $_BB_CONTENT_MAX_BYTES bytes"))
    attachments = _clean_attachments(uid, get(body, :attachments, nothing))

    id  = _new_bb_entry_id()
    ts  = string(Dates.now())
    # Optional status on create — defaults to "open" so a caller that doesn't care lands on the
    # right default. Invalid values are rejected rather than silently coerced (a mismatch here is
    # a caller bug, not a data-migration case).
    status = String(get(body, :status, "open"))
    _valid_bb_status(status) ||
        return 400, JSON3.write((; error = "status must be one of $(_BB_STATUS_VALS)"))
    # Optional fingerprint on create — PROJECT_MEMORY_PLAN P5.1. Set once at create; no PATCH
    # endpoint (the schema is a snapshot of the entry's context at the moment it was opened; a
    # future edit to the image doesn't retroactively change what the entry was about). Must carry
    # a positive int `v` — an unrecognised or absent version reads as "no fingerprint" downstream.
    # Byte-capped so a runaway caller can't blow meta.json.
    fp_in = get(body, :fingerprint, nothing)
    fingerprint = nothing
    if fp_in !== nothing
        _valid_bb_fingerprint(fp_in) ||
            return 400, JSON3.write((; error = "fingerprint must be an object with an integer 'v' >= 1"))
        length(codeunits(JSON3.write(fp_in))) > _BB_FINGERPRINT_MAX_BYTES &&
            return 400, JSON3.write((; error = "fingerprint exceeds $_BB_FINGERPRINT_MAX_BYTES bytes"))
        fingerprint = fp_in
    end

    dir = _bb_entry_dir(uid, id); mkpath(dir)
    write_atomic(joinpath(dir, "entry.md")) do io
        write(io, content)
    end
    _write_bb_meta!(uid, id;
        title = title, createdAt = ts, updatedAt = ts, current = 0,
        attachments = attachments, status = status, fingerprint = fingerprint)

    reg = _read_bb_registry(uid)
    reg[id] = Dict{String,Any}("title" => title, "current" => 0, "updatedAt" => ts, "status" => status)
    _write_bb_registry!(uid, reg)

    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, entryId = id))
end

"""
    POST /api/blackboard/revise

Body: `{ projectUid, entryId, content, note?: string, attachments?: [captureId, ...] }`
Reply: `{ ok:true, version }` — or `{ ok:true, version:<current>, unchanged:true }` on a no-op.

Snapshots the CURRENT entry.md as v<N> (so nothing is lost), then overwrites with `content`. The
optional `attachments` REPLACES the previous list (a revision is a self-contained write) — omit to
keep the existing set. Attachments are versioned per-snapshot via `meta.snapshots[]` — a later
`read_at_version` returns the attachment set that was live when v<N> was captured.

**No-op skip.** If both `content` AND the resolved `attachments` list are byte-for-byte identical
to the current live state, no snapshot is taken and the response carries `unchanged:true`. This
keeps a duplicate v<N> from appearing in the history when a caller resends the same payload.

`note` is currently accepted for future changelog rendering but not stored until a Vue-side
changelog view exists to display it (avoids writing a field nobody reads).
"""
function api_blackboard_revise(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    id  = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
    content = String(get(body, :content, ""))
    _valid_bb_content(content) ||
        return 400, JSON3.write((; error = "content exceeds $_BB_CONTENT_MAX_BYTES bytes"))

    dir = _bb_entry_dir(uid, id)
    live = joinpath(dir, "entry.md")
    old_content = isfile(live) ? read(live, String) : ""
    old_atts = get(meta, "attachments", Any[])
    old_atts isa AbstractVector || (old_atts = Any[])
    atts = haskey(body, :attachments) ?
           _clean_attachments(uid, get(body, :attachments, nothing)) :
           old_atts
    # No-op skip: a revise with the same content AND the same attachments as the current live
    # state is a redundant call — don't spend a snapshot on it. The response still resolves
    # (`ok:true`) with the caller's expected shape so both a UI form and Claude get the same
    # signal ("nothing needed changing"). Kept to normalized list equality: `_clean_attachments`
    # already dedups + orders by first occurrence in the input, and `old_atts` came off disk in
    # write order, so a compare-by-value works.
    if content == old_content && Vector{Any}(atts) == Vector{Any}(old_atts)
        return 200, JSON3.write((; ok = true, version = Int(get(meta, "current", 0)), unchanged = true))
    end

    v = _snapshot_current!(uid, id)
    write_atomic(live) do io
        write(io, content)
    end
    ts  = string(Dates.now())
    snapshots = _append_snapshot_record(
        get(meta, "snapshots", Any[]), v, old_atts, String(get(meta, "updatedAt", ts)))
    # Preserve the entry's status across a content revision — a revise is a content-diff, not a
    # state transition; the two travel separately. Missing on legacy entries backfills to "open".
    prev_status = _status_from_meta(meta)
    # Preserve the outcome tag across a revise too — a content edit doesn't retract a good/bad
    # verdict any more than it retracts the status. If the user changes their mind about the
    # outcome, they hit the outcome endpoint separately (Decision 11).
    prev_outcome = _outcome_from_meta(meta)
    prev_fingerprint = _fingerprint_from_meta(meta)
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts, current = v, attachments = atts,
        snapshots = snapshots, status = prev_status,
        outcome = prev_outcome, fingerprint = prev_fingerprint)

    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    entry["current"]   = v
    entry["updatedAt"] = ts
    entry["title"]     = String(get(meta, "title", get(entry, "title", "")))
    entry["status"]    = prev_status
    _write_bb_registry!(uid, reg)

    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, version = v))
end

"""
    POST /api/blackboard/restore

Body: `{ projectUid, entryId, version }`
Reply: `{ ok:true, version }` (the version the LIVE entry now reflects, i.e. `version`)

Snapshots CURRENT live content first (so the user's un-snapshotted edits are still restorable)
then copies snapshot `version` over `entry.md` and bumps `current` to `version`. Attachments are
restored to the set recorded for `version` (from `meta.snapshots[]`), so both halves of the
entry's state — the markdown AND the attached captures — travel together across a restore.
User-driven, not on the MCP surface — a Claude session must ask the user to restore.
"""
function api_blackboard_restore(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    id  = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    meta = _read_bb_meta(uid, id)
    meta === nothing && return 404, JSON3.write((; error = "Entry not found"))
    v_asked = try
        parse(Int, _wstr(body, :version))
    catch
        return 400, JSON3.write((; error = "version required (integer)"))
    end
    versions = _bb_snapshot_versions(uid, id)
    v_asked in versions ||
        return 404, JSON3.write((; error = "Snapshot version not found: $v_asked"))

    # Snapshot the un-snapshotted live edits BEFORE restoring, so the user can undo the restore.
    # This is the "restore loses un-snapshotted edits" fix (Decision 21 in the plan).
    old_atts_live = get(meta, "attachments", Any[])
    old_atts_live isa AbstractVector || (old_atts_live = Any[])
    v_new = _snapshot_current!(uid, id)
    dir = _bb_entry_dir(uid, id)
    cp(joinpath(dir, ".snapshots", "entry@v$(v_asked).md"),
       joinpath(dir, "entry.md"); force = true)
    ts = string(Dates.now())
    # Restore should also restore the attachment set that was recorded for v_asked (that's the
    # WHOLE state of that version — not just the markdown). Falls back to the current attachments
    # only for legacy entries with no per-version record; a modern entry always has one because
    # revise/restore both call _append_snapshot_record.
    restored_atts = _attachments_for_version(meta, v_asked)
    restored_atts === nothing && (restored_atts = old_atts_live)
    snapshots = _append_snapshot_record(
        get(meta, "snapshots", Any[]), v_new, old_atts_live, String(get(meta, "updatedAt", ts)))
    prev_status  = _status_from_meta(meta)
    prev_outcome = _outcome_from_meta(meta)
    prev_fingerprint = _fingerprint_from_meta(meta)
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts, current = v_asked,
        attachments = restored_atts, snapshots = snapshots,
        status = prev_status, outcome = prev_outcome,
        fingerprint = prev_fingerprint)
    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    entry["current"]   = v_asked
    entry["updatedAt"] = ts
    entry["status"]    = prev_status
    _write_bb_registry!(uid, reg)
    broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, version = v_asked))
end

"""
    POST /api/blackboard/prune

Body: `{ projectUid, entryId, keep: N }`
Reply: `{ ok:true, pruned: k }`

Keep the N MOST RECENT snapshots for this entry; remove older ones from `.snapshots/`. Idempotent
(pruning again returns pruned:0). User-driven, not on the MCP surface. `current` in meta is left
alone even if the snapshot it points at was pruned — the LIVE entry.md is the source of truth for
what the user sees, `current` is a UI label.
"""
function api_blackboard_prune(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    id  = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    isfile(joinpath(_bb_entry_dir(uid, id), "meta.json")) ||
        return 404, JSON3.write((; error = "Entry not found"))
    keep = try
        max(0, parse(Int, _wstr(body, :keep)))
    catch
        return 400, JSON3.write((; error = "keep required (non-negative integer)"))
    end
    versions = sort(_bb_snapshot_versions(uid, id), rev = true)   # newest first
    prune_ids = length(versions) > keep ? versions[(keep + 1):end] : Int[]
    snapdir = joinpath(_bb_entry_dir(uid, id), ".snapshots")
    pruned = 0
    for v in prune_ids
        p = joinpath(snapdir, "entry@v$(v).md")
        try; rm(p; force = true); pruned += 1; catch e
            @warn "blackboard/prune: rm failed" path = p exception = e
        end
    end
    # Drop the meta snapshot records for the pruned versions too — a dangling record with no .md
    # file behind it would show up as attachments-for-a-version that can't actually be read.
    if pruned > 0
        meta = _read_bb_meta(uid, id)
        if meta !== nothing
            snapshots_prev = get(meta, "snapshots", Any[])
            if snapshots_prev isa AbstractVector
                pruned_set = Set(prune_ids)
                snapshots = Any[s for s in snapshots_prev
                                if !(s isa AbstractDict) || !(Int(get(s, "version", -1)) in pruned_set)]
                _write_bb_meta!(uid, id;
                    title = String(get(meta, "title", "")),
                    createdAt = String(get(meta, "createdAt", "")),
                    updatedAt = String(get(meta, "updatedAt", "")),
                    current = Int(get(meta, "current", 0)),
                    attachments = get(meta, "attachments", Any[]),
                    snapshots = snapshots,
                    status = _status_from_meta(meta),
                    outcome = _outcome_from_meta(meta),
                    fingerprint = _fingerprint_from_meta(meta))
            end
        end
    end
    pruned > 0 && broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    200, JSON3.write((; ok = true, pruned = pruned))
end

"""
    POST /api/blackboard/delete

Body: `{ projectUid, entryId }`
Reply: `{ ok:true, deleted: bool }`

User-driven single-entry delete. Removes `<proj>/blackboard/<entryId>/` recursively and drops the
registry key. Idempotent: deleting a missing entry returns 200 with `deleted:false`. NOT on the
MCP surface (additive-write discipline for Claude-authored writes; a user pruning their own board
is a different concern).
"""
function api_blackboard_delete(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    id  = _wstr(body, :entryId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "entryId required"))
    _valid_bb_entry_id(id) || return 400, JSON3.write((; error = "Invalid entryId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    dir = _bb_entry_dir(uid, id)
    existed = isdir(dir)
    existed && rm(dir; recursive = true, force = true)
    if existed
        reg = _read_bb_registry(uid)
        if haskey(reg, id)
            delete!(reg, id)
            _write_bb_registry!(uid, reg)
        end
        broadcast_ws(Dict{String,Any}("type" => "blackboard:changed", "projectUid" => uid))
    end
    200, JSON3.write((; ok = true, deleted = existed))
end
