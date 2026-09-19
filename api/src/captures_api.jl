# ── Bidirectional context — Part 2 (share-in) captures ─────────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` Part 2. A CAPTURE is what the user shows Claude: a frozen
# viewer frame (this PR — `surface: "viewer_frame"`) with an annotation overlay drawn on top,
# addressed by project / image / t / z / extent so the MCP side never has to ask "which image".
# UI, plot and slab surfaces (`"ui"`, `"plot"`, `"viewer_slab"`) share the same envelope and are
# added by PR #5 / PR #6.
#
# Storage. Per-project `<proj>/captures/<captureId>/{meta.json, frame.png}`, one dir per capture.
# ADDITIVE-only: no delete, no mutate. Matches the notebook-registry / movies-registry pattern —
# a directory listing IS the truth (Decision 6 in the movies plan), and a corrupt `meta.json`
# is skipped rather than taking the list down.
#
# MCP surface (`mcp/cecelia_mcp/server.py`): `get_capture(captureId)` reads one full envelope;
# `get_recent_captures(projectUid, limit)` lists newest-first. The write route is called from
# the FRONTEND only (Share button in the pop-out viewer) — the MCP client never authors a
# capture, so `POST /api/viewer/capture` is deliberately NOT on `ALLOWED_ROUTES`.
using Base64
using Dates

_captures_dir_for_project(uid::AbstractString) = joinpath(projects_dir(), uid, "captures")

# ── Capture id ────────────────────────────────────────────────────────────────
# Short, sortable, no filesystem-hostile chars. Format: `cap-<yyyymmddThhmmss>-<6 hex>`. The
# timestamp gives newest-first ordering by name alone (no sidecar sort key), and the hex tail
# makes a race between two captures in the same second impossible in practice. `T` separator +
# no colons so it's Windows-safe (same rule as viewer screenshots).
const _CAPTURE_ID_RE = r"^cap-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$"
function _new_capture_id()::String
    ts = Dates.format(Dates.now(), dateformat"yyyymmddTHHMMSS")
    tail = bytes2hex(rand(UInt8, 3))
    string("cap-", ts, "-", tail)
end
_valid_capture_id(id::AbstractString)::Bool = !isnothing(match(_CAPTURE_ID_RE, String(id)))

# ── Payload shaping ───────────────────────────────────────────────────────────
# The envelope the plan defines (Part 2 → "Captured payload shape"). We validate the small set of
# structural fields we can and pass the rest through — the caller (frontend) is the one place
# that assembles a shape from its own state, and a strict schema here would need updating
# every time the frontend adds a field. Anything unknown at read time is ignored by Claude.
const _CAPTURE_SURFACES = Set(["viewer_frame", "viewer_slab", "ui", "plot"])
const _CAPTURE_OVERLAY_KINDS = Set(["rect", "poly", "stroke", "circle", "arrow"])
# CVD-safe palette locked 2026-09-19 (see `frontend/src/utils/overlayCompose.ts`). Safelisted so
# a tampered payload can't smuggle arbitrary CSS through; unknown names are DROPPED (the frontend
# resolver then falls back to `white`), not stored — a value that would render is a value the
# canvas already treats as legitimate.
const _CAPTURE_OVERLAY_COLORS = Set(["magenta", "cyan", "yellow", "white"])

# The PNG can be a base64 data URL (`data:image/png;base64,...`) or bare base64 bytes. We accept
# both and store the raw bytes on disk so downstream readers don't repeat the prefix strip. Cap
# at 8 MiB — a viewer frame is well under that (a 2048×2048 8-bit-RGBA compresses to <2 MiB),
# and a runaway browser upload should be rejected, not silently written to the project.
const _CAPTURE_PNG_MAX_BYTES = 8 * 1024 * 1024

# Strip an optional data-URL prefix, decode base64. Returns `nothing` on any failure — the caller
# turns that into a 400 rather than surfacing decode internals to the client.
function _decode_capture_png(raw)::Union{Vector{UInt8},Nothing}
    raw isa AbstractString || return nothing
    s = String(raw)
    isempty(s) && return nothing
    payload = startswith(s, "data:") ? (i = findfirst(',', s); isnothing(i) ? "" : String(SubString(s, i+1))) : s
    isempty(payload) && return nothing
    try
        bytes = Base64.base64decode(payload)
        length(bytes) > _CAPTURE_PNG_MAX_BYTES && return nothing
        # PNG magic: 89 50 4E 47 0D 0A 1A 0A — reject anything that isn't a PNG so a stray JPEG
        # or empty buffer surfaces as 400 rather than sitting broken in the project.
        length(bytes) >= 8 &&
            bytes[1] == 0x89 && bytes[2] == 0x50 && bytes[3] == 0x4E && bytes[4] == 0x47 || return nothing
        bytes
    catch
        nothing
    end
end

# Coerce a `Union{Nothing, JSON3.Object, Dict}` field into a plain `Dict{String,Any}`.
_capture_dict(v)::Dict{String,Any} =
    v isa AbstractDict ? Dict{String,Any}(String(k) => x for (k, x) in v) : Dict{String,Any}()
_capture_vec(v)::Vector{Any} =
    v isa AbstractVector ? Any[x for x in v] : Any[]

# One overlay mark. `kind` is validated; `geom` is a bag of numbers/strings the caller assembled
# in the same coord system as the frame (payload-relative 0..1, `viewerScreenshot.ts` shape) —
# we don't second-guess it because the renderer on the far side is what actually consumes it.
function _clean_overlay_mark(m)::Union{Dict{String,Any},Nothing}
    m isa AbstractDict || return nothing
    kind = String(get(m, "kind", get(m, :kind, "")))
    kind in _CAPTURE_OVERLAY_KINDS || return nothing
    out = Dict{String,Any}("kind" => kind)
    for k in ("geom", "label")
        v = get(m, k, get(m, Symbol(k), nothing))
        v === nothing || (out[k] = v)
    end
    # Colour name — safelisted against `_CAPTURE_OVERLAY_COLORS`. An unknown name is silently
    # dropped so the mark still stores + renders (frontend resolves the absent field to `white`);
    # a nil-payload capture never gains a stray CSS value it didn't send. Symbol lookup first
    # because the body arrives as a `JSON3.Object` (native `Symbol` keys); the String fallback
    # covers a caller that hands over a plain `Dict{String,Any}`.
    color_v = get(m, :color, get(m, "color", nothing))
    if color_v isa AbstractString
        c = String(color_v)
        c in _CAPTURE_OVERLAY_COLORS && (out["color"] = c)
    end
    out
end
_clean_overlay(raw)::Vector{Dict{String,Any}} =
    filter(!isnothing, [_clean_overlay_mark(m) for m in _capture_vec(raw)])

# The envelope written to `meta.json`. Everything except `captureId` / `createdAt` / `frames` comes
# from the request; the frame PNG is written separately as `frame.png` and the meta records the
# byte count only (so a diff shows a real change, not a re-encoded but equivalent blob).
function _build_capture_envelope(body::AbstractDict, id::String, ts::String,
                                  png_bytes::Vector{UInt8})::Dict{String,Any}
    surface = String(get(body, :surface, "viewer_frame"))
    surface in _CAPTURE_SURFACES || (surface = "viewer_frame")
    Dict{String,Any}(
        "captureId"       => id,
        "createdAt"       => ts,
        "surface"         => surface,
        "address"         => _capture_dict(get(body, :address, nothing)),
        # `frames` is the plan's shape ({png, overlayLayers?}) — this PR emits ONE frame; slabs
        # (PR #6) push additional entries in the same array. The `png` field is the RELATIVE
        # filename inside the capture dir (`frame.png`), not the bytes — bytes live on disk.
        "frames"          => Any[Dict{String,Any}("png" => "frame.png", "bytes" => length(png_bytes))],
        "overlay"          => _clean_overlay(get(body, :overlay, nothing)),
        "viewStateSnapshot" => get(body, :viewStateSnapshot, nothing),
        "viewerPropsRef"    => get(body, :viewerPropsRef, nothing),
    )
end

# ── Handlers ──────────────────────────────────────────────────────────────────

"""
    POST /api/viewer/capture

Body: `{ projectUid, surface?, address, frames:[{png:<dataURL|base64>}], overlay?, viewStateSnapshot?, viewerPropsRef? }`
Reply: `{ ok:true, captureId, path }`

Writes `<proj>/captures/<captureId>/{meta.json, frame.png}` atomically (via `write_json_atomic` +
`write_atomic`). Rejects a missing/unknown project (404), a missing project uid (400), an
unreadable PNG (400).
"""
function api_viewer_capture(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    frames = get(body, :frames, nothing)
    frames isa AbstractVector && !isempty(frames) ||
        return 400, JSON3.write((; error = "frames required (a non-empty list; frame 1 must carry a PNG)"))
    first_frame = first(frames)
    first_frame isa AbstractDict ||
        return 400, JSON3.write((; error = "frames[0] must be an object"))
    png_bytes = _decode_capture_png(get(first_frame, :png, nothing))
    isnothing(png_bytes) &&
        return 400, JSON3.write((; error = "frames[0].png must be a PNG (data URL or base64, ≤ 8 MiB)"))

    id  = _new_capture_id()
    ts  = string(Dates.now())
    dir = joinpath(_captures_dir_for_project(uid), id)
    mkpath(dir)

    envelope = _build_capture_envelope(body, id, ts, png_bytes)
    write_atomic(joinpath(dir, "frame.png")) do io
        write(io, png_bytes)
    end
    write_json_atomic(joinpath(dir, "meta.json"), envelope)

    # BIDIR PR #2: try to push a plain-text notification to the paired Claude Code session
    # over its inbox socket. Any failure ⇒ `:fallback`, and the frontend keeps the existing
    # clipboard/toast path — a broken push never blocks a share. `:not_paired` is the normal
    # unpaired case; the frontend renders it the same as `:fallback` today (both mean "no
    # push happened"). The address here is the same envelope dict the meta.json carries; the
    # writer picks the fields it needs (surface / imageUid / t / z).
    push_outcome, _msg = push_capture_notification(uid, id, get(envelope, "address", nothing))

    200, JSON3.write((; ok = true, captureId = id, path = dir, push = String(push_outcome)))
end

"""
    GET /api/viewer/captures?projectUid=…&limit=N

Reply: `{ items: [{ captureId, createdAt, surface, address }, …] }`, newest-first, capped.
Skips a corrupt `meta.json` (never takes the list down for it — captures are additive decoration
on top of a project, not core data).
"""
const _CAPTURES_LIST_DEFAULT_LIMIT = 10
const _CAPTURES_LIST_MAX_LIMIT     = 100
function api_viewer_captures_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    limit = try
        clamp(parse(Int, get(query, "limit", string(_CAPTURES_LIST_DEFAULT_LIMIT))), 1, _CAPTURES_LIST_MAX_LIMIT)
    catch
        _CAPTURES_LIST_DEFAULT_LIMIT
    end

    dir = _captures_dir_for_project(uid)
    isdir(dir) || return 200, JSON3.write((; items = Any[]))

    # Newest-first by name — the id encodes a sortable timestamp so a name sort is a time sort
    # (see `_new_capture_id`). Cheaper than opening each `meta.json` just to read `createdAt`.
    entries = sort!(String[e for e in readdir(dir) if _valid_capture_id(e)], rev = true)
    items = Any[]
    for id in entries
        length(items) >= limit && break
        meta_path = joinpath(dir, id, "meta.json")
        isfile(meta_path) || continue
        try
            meta = JSON3.read(read(meta_path, String))
            push!(items, Dict{String,Any}(
                "captureId" => String(get(meta, :captureId, id)),
                "createdAt" => String(get(meta, :createdAt, "")),
                "surface"   => String(get(meta, :surface, "")),
                "address"   => _capture_dict(get(meta, :address, nothing)),
            ))
        catch e
            @warn "Skipping unreadable capture meta" path = meta_path exception = e
        end
    end
    200, JSON3.write((; items = items))
end

"""
    GET /api/viewer/capture?projectUid=…&captureId=…

Reply: `{ capture: <envelope>, frame: "<data URL>" }`. The frame is inlined as a data URL because
the MCP client returns it as a single-shot image content block; a separate binary fetch would
double the round-trip for no gain.
"""
function api_viewer_capture_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    id  = get(query, "captureId", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "captureId required"))
    _valid_capture_id(id) || return 400, JSON3.write((; error = "Invalid captureId"))
    dir = joinpath(_captures_dir_for_project(uid), id)
    meta_path = joinpath(dir, "meta.json")
    isfile(meta_path) || return 404, JSON3.write((; error = "Capture not found"))

    envelope = try
        JSON3.read(read(meta_path, String), Dict{String,Any})
    catch
        return 500, JSON3.write((; error = "Capture meta unreadable"))
    end
    png_path = joinpath(dir, "frame.png")
    frame = if isfile(png_path)
        string("data:image/png;base64,", Base64.base64encode(read(png_path)))
    else
        ""
    end
    200, JSON3.write((; capture = envelope, frame = frame))
end

"""
    POST /api/viewer/capture/delete

Body: `{projectUid, captureId}`. User-driven single-capture delete from Kiwi
(docs/todo/KIWI_PLAN.md — post-PR #3 follow-up). Removes `<proj>/captures/<captureId>/`
recursively. Idempotent: missing captureId returns 200 `deleted:false`.

Why this endpoint exists at all: BIDIR_CONTEXT_PLAN.md ruled out a delete tool on the
*MCP surface* (additive-write discipline for assistant-authored writes). This is different —
a user driving the frontend can prune their own capture directory the way they can prune a
notebook or a movie. Same shape as `POST /api/notebooks/delete`.

The captureId regex + directory containment check keep this from turning into a "delete an
arbitrary path" surface: `_valid_capture_id` rejects anything that isn't `cap-…`, and the
joinpath is anchored under `_captures_dir_for_project(uid)`, so a `..` in a payload gets
regex-rejected before it can escape the tree.
"""
function api_viewer_capture_delete(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    id  = _wstr(body, :captureId)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(id)  && return 400, JSON3.write((; error = "captureId required"))
    _valid_capture_id(id) || return 400, JSON3.write((; error = "Invalid captureId"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    dir = joinpath(_captures_dir_for_project(uid), id)
    existed = isdir(dir)
    existed && rm(dir; recursive = true, force = true)
    # Broadcast so any open Kiwi in another window (or a second Cecelia session on this
    # project) drops the row from its recent-captures list without a manual refresh.
    broadcast_ws(Dict{String,Any}(
        "type" => "captures:changed", "projectUid" => uid,
    ))
    200, JSON3.write((; ok = true, deleted = existed))
end

"""
    POST /api/viewer/captures/clear

Body: `{projectUid}`. User-driven bulk delete — the "Clear all captures" button in Kiwi.
Removes every `cap-…` subdir under `<proj>/captures/`. Skips non-matching entries so a stray
file or a future format lives on. Returns the count so the UI can report "cleared 42".
"""
function api_viewer_captures_clear(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))

    dir = _captures_dir_for_project(uid)
    cleared = 0
    if isdir(dir)
        for name in readdir(dir)
            _valid_capture_id(name) || continue
            entry = joinpath(dir, name)
            isdir(entry) || continue
            try
                rm(entry; recursive = true, force = true)
                cleared += 1
            catch e
                @warn "captures/clear: rm failed" path = entry exception = e
            end
        end
    end
    broadcast_ws(Dict{String,Any}(
        "type" => "captures:changed", "projectUid" => uid,
    ))
    200, JSON3.write((; ok = true, cleared = cleared))
end
