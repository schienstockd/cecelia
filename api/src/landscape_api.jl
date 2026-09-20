# ── Bidirectional context — Part 3 (point-out) landscape ─────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` PR #6 (rescoped 2026-09-20, Decision 14). The landscape is a
# CHEAP tile-level semantic heatmap over the shown frame — categorical labels per grid tile
# (`dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`), NOT a segmentation and NOT
# SAM / Cellpose. Computed in the browser (`frontend/src/utils/landscape.ts`) so the labels
# reflect exactly what the user is looking at (channels, contrast); this file just STORES the
# last-published copy in-memory so an MCP `get_landscape` call has something to read.
#
# EPHEMERAL BY DESIGN. Same shape as `marks_api.jl` — one last-published landscape per
# (project, image, valueName, t, z). No history, no persistence. If the user closes and reopens
# the app before Claude asks, the landscape is gone — the frontend recomputes on demand. Keeps
# the file cheap to reason about and avoids a durable-file design for something that's cheaper
# to recompute than to hydrate.
#
# NO WRITE FROM MCP. The publish route (`POST /api/viewer/landscape`) is browser-only — Claude
# READS the landscape (`get_landscape`), never writes one. Same principle as captures: the
# assistant reads pixels the human framed, doesn't invent them.

using Dates

# One published landscape. Keys mirror the frontend's LandscapeResult shape verbatim so the
# response JSON deserialises cleanly without a per-field pass. `payload` is the frontend's
# {grid, tiles, legend} dict; we don't second-guess its shape because the frontend is the
# authoritative computer and a schema drift is a bug better caught in one place (the util's
# tests) than in a per-field guard here.
struct Landscape
    projectUid::String
    imageUid::String
    valueName::String
    t::Int
    z::Int
    createdAt::Float64
    payload::Dict{String,Any}
end

const _LANDSCAPE_LOCK = ReentrantLock()
# Keyed by (projectUid, imageUid, valueName, t, z) → the latest landscape for that address.
# A repeat POST for the same address just overwrites — matches the "always the freshest view"
# semantics; a landscape is a snapshot, no reason to keep old copies around.
const _LANDSCAPE_BY_KEY = Dict{NTuple{5,Any}, Landscape}()
const _LANDSCAPE_TTL_SECONDS = 3600     # 1 h — a stale landscape is worse than none; recompute is cheap

_now_epoch() = time()
_landscape_alive(l::Landscape, now::Float64 = _now_epoch()) = (now - l.createdAt) < _LANDSCAPE_TTL_SECONDS

_landscape_key(l::Landscape) = (l.projectUid, l.imageUid, l.valueName, l.t, l.z)

# Sanitise `t` / `z` from HTTP: an unspecified z is legitimate (2D image), which we key as -1
# rather than nothing so the tuple has a fixed shape.
_clean_int(v, default::Int)::Int = begin
    v isa Number ? Int(v) : (v isa AbstractString ? (try; parse(Int, v); catch; default; end) : default)
end

# ── Handlers ──────────────────────────────────────────────────────────────────

"""
    POST /api/viewer/landscape

Body: `{ projectUid, imageUid, valueName, t, z?, landscape: {grid, tiles, legend} }`
Reply: `{ ok:true }`

Called by the browser when the user toggles the landscape overlay on (or bumps the grid
density). Overwrites the last-published landscape for this address. `landscape` is the
frontend `LandscapeResult` shape passed through verbatim.
"""
function api_viewer_landscape_publish(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = _wstr(body, :imageUid)
    value_name = _wstr(body, :valueName)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name) && return 400, JSON3.write((; error = "valueName required"))
    t = _clean_int(get(body, :t, nothing), -1)
    z = _clean_int(get(body, :z, nothing), -1)
    payload_raw = get(body, :landscape, nothing)
    payload_raw isa AbstractDict || return 400, JSON3.write((; error = "landscape body required (object)"))
    # JSON3 gives us a symbolic-keyed dict; normalise to String keys so the response uses the same
    # shape the frontend published without an accidental key-type mismatch.
    payload = Dict{String,Any}(String(k) => v for (k, v) in payload_raw)
    l = Landscape(project_uid, image_uid, value_name, t, z, _now_epoch(), payload)
    lock(_LANDSCAPE_LOCK) do
        _LANDSCAPE_BY_KEY[_landscape_key(l)] = l
    end
    200, JSON3.write((; ok = true))
end

"""
    GET /api/viewer/landscape?projectUid=…&imageUid=…&valueName=…&t=…&z=…

Reply: `{ landscape: {grid, tiles, legend, createdAt, t, z} }` on hit, `{ landscape: null }` on
miss. z is optional (omit for 2D images — matches how the publisher sends -1).

Read by the MCP `get_landscape` tool. LIVE ONLY — a landscape past its TTL is treated as absent
(the frontend recomputes on demand; a stale hit would be worse than a miss).
"""
function api_viewer_landscape_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = get(query, "imageUid", "")
    value_name = get(query, "valueName", "")
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name) && return 400, JSON3.write((; error = "valueName required"))
    t = _clean_int(get(query, "t", "-1"), -1)
    z = _clean_int(get(query, "z", "-1"), -1)
    now = _now_epoch()
    hit = lock(_LANDSCAPE_LOCK) do
        l = get(_LANDSCAPE_BY_KEY, (uid, image_uid, value_name, t, z), nothing)
        (isnothing(l) || !_landscape_alive(l, now)) ? nothing : l
    end
    if isnothing(hit)
        return 200, JSON3.write((; landscape = nothing))
    end
    out = copy(hit.payload)
    out["createdAt"] = hit.createdAt
    out["t"] = hit.t
    out["z"] = hit.z
    out["imageUid"] = hit.imageUid
    out["valueName"] = hit.valueName
    200, JSON3.write((; landscape = out))
end


# ── Complementary compute (LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 1) ──────────
# The frontend `utils/landscape.ts` layer reads the already-decoded RGB composite,
# which is enough for `category` but restates what the eye already sees. To be
# genuinely complementary (Decision 14 reframe + LANDSCAPE_COMPLEMENTARY_PLAN.md
# design principle), we UNTANGLE the colour-blend: per visible channel, per tile,
# a `mean` and a rough SNR from the raw multichannel plane. Computed on demand
# (typically at Share) so the augmented tiles travel with the capture.
#
# Read path uses `read_slab` from `viewer_api.jl` (already the sanctioned Zarr
# entry, per CLAUDE.md rule that only the display layer uses `using Zarr`).
# Down-sampling: pick the smallest pyramid level with a long side ≥ 512 px so
# the compute stays inside a ~500 ms budget even on 4K frames.

# ~500 px on the long side is the smallest scale where tile stats over 32×32 still
# average > 250 px per tile — enough to be a meaningful "signal-worthy" answer, not
# so big we pay for full-res reads on frames where the eye couldn't tell the
# difference anyway. Same downsample floor the frontend uses (`readCanvasImageData`).
const _LANDSCAPE_MIN_LONG_SIDE = 512

# Pick the smallest level with `max(nx, ny) >= _LANDSCAPE_MIN_LONG_SIDE`. Falls back
# to the last (coarsest) level if every level is smaller than the floor — cheap
# frames don't need finer.
function _pick_landscape_level(zp::AbstractString)
    lvls = try store_pyramid_levels(String(zp)) catch; nothing end
    (lvls === nothing || isempty(lvls)) && return 0
    # `store_pyramid_levels` returns levels ordered coarsest→finest OR finest→coarsest
    # depending on the store; scan for the smallest level meeting the floor by iterating
    # both directions and picking the smaller index. shape is (…, y, x) or (…, x, y);
    # take the last two dims and read `max` of them.
    best = length(lvls) - 1
    for (i, lvl) in enumerate(lvls)
        s = lvl.shape
        n = length(s)
        n >= 2 || continue
        long = max(s[n], s[n - 1])
        long >= _LANDSCAPE_MIN_LONG_SIDE && (best = min(best, i - 1))
    end
    best
end

# Tile stats for a single plane (nx, ny Float32). Mean + SNR (mean / max(σ, floor)).
# The floor guards against divide-by-tiny-σ in tiles that are literally uniform;
# 1e-3 in normalised [0,1] terms → SNR maxes at ~1000 rather than exploding to Inf,
# which is what the JSON serialisation needs.
const _SNR_SIGMA_FLOOR = 1e-3
function _tile_channel_stats(plane::AbstractMatrix{<:Real}, ncols::Int, nrows::Int)
    W, H = size(plane, 1), size(plane, 2)
    out = Vector{Tuple{Float64,Float64}}(undef, ncols * nrows)
    # Normalise the plane to [0, 1] using its own dynamic range — mean/SNR are unit-free
    # once we do this, so a caller doesn't need to send channel LUT contrasts. Uses
    # min/max rather than a percentile to keep it O(N) and predictable; the tile stats
    # inherit whatever headroom the raw plane had.
    lo, hi = extrema(plane)
    span = hi > lo ? (Float64(hi) - Float64(lo)) : 1.0
    for r in 0:(nrows - 1), c in 0:(ncols - 1)
        x0 = floor(Int, (c / ncols) * W) + 1
        x1 = floor(Int, ((c + 1) / ncols) * W)
        y0 = floor(Int, (r / nrows) * H) + 1
        y1 = floor(Int, ((r + 1) / nrows) * H)
        x0 = clamp(x0, 1, W); x1 = clamp(x1, x0, W)
        y0 = clamp(y0, 1, H); y1 = clamp(y1, y0, H)
        n = 0
        sum = 0.0
        sumSq = 0.0
        @inbounds for y in y0:y1, x in x0:x1
            v = (Float64(plane[x, y]) - Float64(lo)) / span
            sum += v
            sumSq += v * v
            n += 1
        end
        mean = n > 0 ? sum / n : 0.0
        varv = n > 0 ? max(0.0, sumSq / n - mean * mean) : 0.0
        std  = sqrt(varv)
        snr  = mean / max(std, _SNR_SIGMA_FLOOR)
        out[r * ncols + c + 1] = (mean, snr)
    end
    out
end

# Spreadsheet-style cell label, mirroring the frontend `gridOverlay.ts::cellLabel`.
# Duplicated on purpose — Julia and TypeScript don't share code, and the alternative
# is a JSON manifest that both sides read at boot, which is heavier than 15 lines of
# arithmetic. Row-major, columns A..Z, then AA..ZZ, rows 1..N.
function _tile_cell_label(row::Int, col::Int)
    letters = String[]
    n = col
    while true
        push!(letters, string(Char(UInt8('A') + (n % 26))))
        n = div(n, 26) - 1
        n < 0 && break
    end
    string(reverse(letters)...) * string(row + 1)
end

"""
    POST /api/viewer/landscape/compute

Body: `{ projectUid, imageUid, valueName, t, z?, cols, rows, channels: [{index, name}] }`
Reply: `{ tiles: [{tileId, channels: {name: {mean, snr}}}, ...] }`

Called by the browser at Share time (or on explicit augmented-recompute) so the
capture envelope carries per-channel per-tile stats alongside the frontend's
category — the complementary payload from LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 1.

`channels` is the VISIBILITY snapshot (Decision 3): only currently-visible channels
appear here, and only those appear in the response. `index` is the 0-based channel
index in the store; `name` is the display name the frontend uses. Unknown indices
are silently dropped — a stale visibility snapshot doesn't break the write.

Cost: one plane read per visible channel at a pyramid level chosen to keep the long
side ≥ 512 px, plus O(ncols*nrows) per channel for the tile aggregations. Typical
call for a 3-channel visible tile at 8×8 is < 200 ms; a 32×32 request on a 4-channel
image lands in the 500 ms – 2 s budget the plan allocates for the augmented layer.
"""
function api_viewer_landscape_compute(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = _wstr(body, :imageUid)
    value_name = _wstr(body, :valueName)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    t_raw = _clean_int(get(body, :t, nothing), -1)
    z_raw = _clean_int(get(body, :z, nothing), -1)
    ncols = clamp(_clean_int(get(body, :cols, nothing), 8), 2, 64)
    nrows = clamp(_clean_int(get(body, :rows, nothing), ncols), 2, 64)
    channels_raw = get(body, :channels, nothing)
    channels_raw isa AbstractVector || return 400, JSON3.write((; error = "channels required (list)"))
    isempty(channels_raw) && return 200, JSON3.write((; tiles = []))

    vnn = isempty(value_name) ? nothing : String(value_name)
    zp, _td, err = resolve_image_version(project_uid, image_uid, vnn; version = nothing)
    err === nothing || return 404, JSON3.write((; error = err))

    level = _pick_landscape_level(zp)
    # Open once for many channel reads — same pattern as `_sampled_specs`; per-open cost
    # is a metadata round-trip that would dominate if we redid it per channel.
    arr, caxes = try
        open_level(zp, level)
    catch e
        return 500, JSON3.write((; error = "landscape compute: could not open store — $(sprint(showerror, e))"))
    end
    t = t_raw < 0 ? 0 : t_raw
    z = z_raw < 0 ? nothing : z_raw

    # Parse channel spec: {index, name}. Silently drop entries that are the wrong shape
    # or point past the store's channel count — a stale visibility snapshot from the
    # frontend shouldn't 500 the whole compute.
    dims = axis_dims(caxes, ndims(arr))
    nc_total = haskey(dims, "c") ? size(arr, dims["c"]) : 1
    channels = Tuple{Int,String}[]
    for ch in channels_raw
        ch isa AbstractDict || continue
        idx = get(ch, :index, get(ch, "index", nothing))
        name = get(ch, :name, get(ch, "name", nothing))
        idx isa Number && name isa AbstractString || continue
        i = Int(idx)
        (0 <= i < nc_total) || continue
        push!(channels, (i, String(name)))
    end
    isempty(channels) && return 200, JSON3.write((; tiles = []))

    # Compute per channel: read the plane, run tile stats. `read_slab` gives us a
    # `(nx, ny, 1, 1)` volume when both z and c are scalar Ints.
    tiles = [Dict{String,Any}("tileId" => _tile_cell_label(r, c),
                              "channels" => Dict{String,Dict{String,Float64}}())
             for r in 0:(nrows - 1), c in 0:(ncols - 1)]
    tiles = vec(permutedims(tiles, (2, 1)))   # row-major flat list matching the frontend
    for (ci, cname) in channels
        vol, nx, ny, _nz, _nc = try
            read_slab(arr, caxes, t, ci; z = z)
        catch
            continue
        end
        (nx == 0 || ny == 0) && continue
        # `vol` is (x, y, [z], [c]) — with scalar z and scalar c, it's a 2D (x, y) plane.
        plane = ndims(vol) == 2 ? vol : reshape(vol, nx, ny)
        stats = _tile_channel_stats(plane, ncols, nrows)
        for r in 0:(nrows - 1), c in 0:(ncols - 1)
            (mean, snr) = stats[r * ncols + c + 1]
            tiles[r * ncols + c + 1]["channels"][cname] = Dict("mean" => round(mean, digits = 4),
                                                                "snr"  => round(snr,  digits = 3))
        end
    end

    200, JSON3.write((; tiles = tiles))
end

# Test-only reset. Not registered as a route — tests import the module and call it
# directly to get a hermetic state between assertions. Deliberately private.
function _reset_landscape!()
    lock(_LANDSCAPE_LOCK) do
        empty!(_LANDSCAPE_BY_KEY)
    end
    nothing
end
