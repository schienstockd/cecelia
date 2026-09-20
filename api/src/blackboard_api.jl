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
function _new_bb_entry_id()::String
    ts = Dates.format(Dates.now(), dateformat"yyyymmddTHHMMSS")
    tail = bytes2hex(rand(UInt8, 3))
    string("bb-", ts, "-", tail)
end
_valid_bb_entry_id(id::AbstractString)::Bool = !isnothing(match(_BB_ID_RE, String(id)))

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
function _write_bb_meta!(uid::AbstractString, id::AbstractString; title, createdAt, updatedAt, current, attachments, snapshots = Any[])
    dir = _bb_entry_dir(uid, id)
    write_json_atomic(joinpath(dir, "meta.json"), Dict{String,Any}(
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
    ))
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

"""
    GET /api/blackboard?projectUid=…

Reply: `{ entries: [{ entryId, title, current, updatedAt, attachmentsCount }, …] }`, newest-first
by id (which encodes a sortable timestamp). Skips an entry whose meta.json is unreadable rather
than taking the list down — matches the captures/notebooks list convention.
"""
function api_blackboard_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    dir = _blackboard_dir_for_project(uid)
    isdir(dir) || return 200, JSON3.write((; entries = Any[]))
    ids = sort!(String[e for e in readdir(dir) if _valid_bb_entry_id(e)], rev = true)
    entries = Any[]
    for id in ids
        meta = _read_bb_meta(uid, id)
        meta === nothing && continue
        atts = get(meta, "attachments", Any[])
        push!(entries, Dict{String,Any}(
            "entryId"          => id,
            "title"            => String(get(meta, "title", "")),
            "current"          => Int(get(meta, "current", 0)),
            "updatedAt"        => String(get(meta, "updatedAt", "")),
            "attachmentsCount" => atts isa AbstractVector ? length(atts) : 0,
        ))
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
    200, JSON3.write((; entry = Dict{String,Any}(
        "entryId"     => id,
        "title"       => String(get(meta, "title", "")),
        "content"     => content,
        "current"     => Int(get(meta, "current", 0)),
        "updatedAt"   => String(get(meta, "updatedAt", "")),
        "versions"    => sort(versions),
        "attachments" => attachments_out,
    )))
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
    dir = _bb_entry_dir(uid, id); mkpath(dir)
    write_atomic(joinpath(dir, "entry.md")) do io
        write(io, content)
    end
    _write_bb_meta!(uid, id;
        title = title, createdAt = ts, updatedAt = ts, current = 0, attachments = attachments)

    reg = _read_bb_registry(uid)
    reg[id] = Dict{String,Any}("title" => title, "current" => 0, "updatedAt" => ts)
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
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts, current = v, attachments = atts, snapshots = snapshots)

    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    entry["current"]   = v
    entry["updatedAt"] = ts
    entry["title"]     = String(get(meta, "title", get(entry, "title", "")))
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
    _write_bb_meta!(uid, id;
        title = String(get(meta, "title", "")),
        createdAt = String(get(meta, "createdAt", ts)),
        updatedAt = ts, current = v_asked,
        attachments = restored_atts, snapshots = snapshots)
    reg = _read_bb_registry(uid)
    entry = get!(reg, id, Dict{String,Any}())
    entry["current"]   = v_asked
    entry["updatedAt"] = ts
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
                    snapshots = snapshots)
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
