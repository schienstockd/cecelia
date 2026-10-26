# ── Bidirectional context — Part 3 (point-out) landscape ─────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` PR #6 (rescoped 2026-09-20, Decision 14). The landscape is a
# CHEAP tile-level semantic heatmap over the shown frame — categorical labels per grid tile
# (`dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`), NOT a segmentation and NOT
# SAM / Cellpose. Computed in the browser (`frontend/src/utils/landscape.ts`) so the labels
# reflect exactly what the user is looking at (channels, contrast); this file just STORES the
# last-published copy in-memory (Kiwi landscape refs) and hands an MCP `get_landscape` request the
# viewer's answer.
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
#
# ON DEMAND. `get_landscape` does not wait for the overlay to be on: it ASKS the open viewer
# (`POST /api/viewer/landscape/request` → WS `viewer:landscape-request`), which computes the same
# registered landscape Share attaches and publishes it tagged with the request id. Every publish —
# the overlay's scrub-time ones included — also lands in `_LANDSCAPE_BY_KEY`, which Kiwi's
# landscape refs read (`kiwi_refs.jl`).

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

# On-demand requests in flight: requestId → the viewer's answer (`nothing` until it lands). Only ids
# registered here are accepted, so a late answer after the wait gave up is dropped, not leaked.
const _LANDSCAPE_REQUESTS = Dict{String,Union{Nothing,Landscape}}()
const _LANDSCAPE_REQ_SEQ = Threads.Atomic{Int}(0)
# Long enough for a 4K canvas readback + the backend compute on a heavy image (both measured in
# the 0.2–2 s range); short enough that "no viewer open" answers inside one tool call.
const _LANDSCAPE_REQUEST_TIMEOUT_S = 8.0

function _open_landscape_request!()::String
    rid = string("lr-", time_ns(), "-", Threads.atomic_add!(_LANDSCAPE_REQ_SEQ, 1))
    lock(_LANDSCAPE_LOCK) do; _LANDSCAPE_REQUESTS[rid] = nothing; end
    rid
end

# The viewer's answer to `rid`. False when nobody is waiting for it (timed out, or a stray id).
function _deliver_landscape_request!(rid::AbstractString, l::Landscape)::Bool
    lock(_LANDSCAPE_LOCK) do
        haskey(_LANDSCAPE_REQUESTS, rid) || return false
        _LANDSCAPE_REQUESTS[rid] === nothing || return false     # first viewer to answer wins
        _LANDSCAPE_REQUESTS[rid] = l
        true
    end
end

# Wait for `rid`'s answer, then forget the request either way.
function _await_landscape_request(rid::AbstractString, timeout_s::Real; poll_s::Real = 0.05)
    deadline = time() + timeout_s
    try
        while true
            l = lock(_LANDSCAPE_LOCK) do; get(_LANDSCAPE_REQUESTS, rid, nothing); end
            l === nothing || return l
            time() >= deadline && return nothing
            sleep(poll_s)
        end
    finally
        lock(_LANDSCAPE_LOCK) do; delete!(_LANDSCAPE_REQUESTS, rid); end
    end
end

_landscape_reply(l::Landscape) = begin
    out = copy(l.payload)
    out["createdAt"] = l.createdAt
    out["t"] = l.t
    out["z"] = l.z
    out["imageUid"] = l.imageUid
    out["valueName"] = l.valueName
    out
end

# Sanitise `t` / `z` from HTTP: a missing one is keyed as -1 rather than nothing so the tuple has a
# fixed shape. The viewer always sends both (z=0 on a 2D image).
_clean_int(v, default::Int)::Int = begin
    v isa Number ? Int(v) : (v isa AbstractString ? (try; parse(Int, v); catch; default; end) : default)
end

# ── Handlers ──────────────────────────────────────────────────────────────────

"""
    POST /api/viewer/landscape

Body: `{ projectUid, imageUid, valueName, t, z?, landscape: {grid, tiles, legend}, requestId? }`
Reply: `{ ok:true }`

Called by the browser when the user toggles the landscape overlay on (or bumps the grid
density), and in answer to a `viewer:landscape-request` (then with its `requestId`). Overwrites
the last-published landscape for this address. `landscape` is the frontend `LandscapeResult`
shape passed through verbatim.
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
    rid = _wstr(body, :requestId)
    isempty(rid) || _deliver_landscape_request!(rid, l)
    200, JSON3.write((; ok = true))
end

"""
    POST /api/viewer/landscape/request

Body: `{ projectUid, imageUid }`
Reply: `{ landscape: {…} }` — what the open viewer shows NOW, computed for this call — or
`{ landscape: null, reason: "no_viewer" }` when no viewer of that image answered in time.

Read by the MCP `get_landscape` tool. Broadcasts `viewer:landscape-request`; a viewer window
showing `imageUid` runs the same snapshot Share attaches (category + registered per-tile
channels / counts / pops / tracks) and publishes it with the request id. The Landscape overlay
toggle plays no part. Nothing on disk changes.
"""
function api_viewer_landscape_request(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = _wstr(body, :imageUid)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    rid = _open_landscape_request!()
    broadcast_ws(Dict{String,Any}("type" => "viewer:landscape-request", "projectUid" => project_uid,
                                  "imageUid" => image_uid, "requestId" => rid))
    l = _await_landscape_request(rid, _LANDSCAPE_REQUEST_TIMEOUT_S)
    l === nothing && return 200, JSON3.write((; landscape = nothing, reason = "no_viewer"))
    200, JSON3.write((; landscape = _landscape_reply(l)))
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

# `(nx, ny)` per pyramid level in store order — NGFF puts x last, y second-last.
function _landscape_level_xy(zp::AbstractString)
    lvls = try store_pyramid_levels(String(zp)) catch; nothing end
    lvls === nothing && return Tuple{Int,Int}[]
    [(l.shape[end], l.shape[end - 1]) for l in lvls if length(l.shape) >= 2]
end

# Tile stats for a single plane (nx, ny Float32). Mean + SNR (mean / max(σ, floor)).
# The floor guards against divide-by-tiny-σ in tiles that are literally uniform;
# 1e-3 in normalised [0,1] terms → SNR maxes at ~1000 rather than exploding to Inf,
# which is what the JSON serialisation needs.
const _SNR_SIGMA_FLOOR = 1e-3
# `frame` = the tiled rectangle `(x0, y0, x1, y1)` in this plane's 0-based pixel coords — the viewer's
# view when the caller read a crop of it (can hang past the plane where the view shows no image).
# `nothing` = the whole plane. A tile with no pixel of the plane under it is `nothing`; a tile thinner
# than a pixel reads the pixel under its centre, so a deep zoom on a small image still answers.
function _tile_channel_stats(plane::AbstractMatrix{<:Real}, ncols::Int, nrows::Int;
                             frame::Union{Nothing,NTuple{4,Float64}} = nothing)
    W, H = size(plane, 1), size(plane, 2)
    fx0, fy0, fx1, fy1 = frame === nothing ? (0.0, 0.0, Float64(W), Float64(H)) : frame
    out = Vector{Union{Nothing,Tuple{Float64,Float64}}}(nothing, ncols * nrows)
    # Normalise the plane to [0, 1] using its own dynamic range — mean/SNR are unit-free
    # once we do this, so a caller doesn't need to send channel LUT contrasts. Uses
    # min/max rather than a percentile to keep it O(N) and predictable; the tile stats
    # inherit whatever headroom the raw plane had.
    lo, hi = extrema(plane)
    span = hi > lo ? (Float64(hi) - Float64(lo)) : 1.0
    for r in 0:(nrows - 1), c in 0:(ncols - 1)
        xr = _tile_px_range(fx0, fx1, c, ncols, W)
        yr = _tile_px_range(fy0, fy1, r, nrows, H)
        (xr === nothing || yr === nothing) && continue
        x0, x1 = xr; y0, y1 = yr
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

# 1-based inclusive pixel range of tile `i` of `n` along one axis, the tiled span `[f0, f1)` in
# 0-based plane px clipped to `[0, len)`. `nothing` when the tile is off the plane.
function _tile_px_range(f0::Float64, f1::Float64, i::Int, n::Int, len::Int)
    a = f0 + (i / n) * (f1 - f0)
    b = f0 + ((i + 1) / n) * (f1 - f0)
    lo = max(floor(Int, a), 0)
    hi = min(floor(Int, b), len)          # exclusive
    if hi <= lo                           # thinner than a pixel: the pixel under the centre
        m = floor(Int, (a + b) / 2)
        (0 <= m < len) || return nothing
        lo, hi = m, m + 1
    end
    (lo + 1, hi)
end

# Which tile a level-0 centroid falls in — 1-based, row-major; 0 = outside the view.
# `view` = the image rectangle the viewer shows, `(x0, y0, x1, y1)` in level-0 px — the SAME frame the
# browser tiles for the category layer, so tile "C4" names one region in both halves of a landscape.
# `nothing` = the whole image, edges clamped in (an older client, or a caller with no viewport).
function _tile_of(px::Real, py::Real, ncols::Int, nrows::Int, sizeX::Int, sizeY::Int,
                  view::Union{Nothing,NTuple{4,Float64}})::Int
    if view === nothing
        c = clamp(floor(Int, (Float64(px) / sizeX) * ncols), 0, ncols - 1)
        r = clamp(floor(Int, (Float64(py) / sizeY) * nrows), 0, nrows - 1)
        return r * ncols + c + 1
    end
    x0, y0, x1, y1 = view
    u = (Float64(px) - x0) / (x1 - x0)
    v = (Float64(py) - y0) / (y1 - y0)
    (0.0 <= u < 1.0 && 0.0 <= v < 1.0) || return 0
    min(floor(Int, v * nrows), nrows - 1) * ncols + min(floor(Int, u * ncols), ncols - 1) + 1
end

# `view` off the wire: `{x0, y0, x1, y1}` finite with positive width and height, else `nothing`.
function _landscape_view(raw)::Union{Nothing,NTuple{4,Float64}}
    raw isa AbstractDict || return nothing
    vals = Float64[]
    for k in (:x0, :y0, :x1, :y1)
        v = get(raw, k, get(raw, String(k), nothing))
        (v isa Real && isfinite(Float64(v))) || return nothing
        push!(vals, Float64(v))
    end
    (vals[3] > vals[1] && vals[4] > vals[2]) || return nothing
    (vals[1], vals[2], vals[3], vals[4])
end

# Pyramid level for a viewport read: the COARSEST level at which the visible part of the image is
# still ≥ `_LANDSCAPE_MIN_LONG_SIDE` px on its long side, else the finest. Zoomed in, that is a finer
# level than the whole-plane pick — a crop of the 512-px level would leave a few pixels per tile.
# `level_xy` = `(nx, ny)` per level in store order (0-based index returned); picked by size, so the
# order the store lists its levels in does not matter.
function _pick_view_level(level_xy::AbstractVector, sizeX::Int, sizeY::Int,
                          view::NTuple{4,Float64})::Int
    x0, y0, x1, y1 = view
    vw = min(x1, sizeX) - max(x0, 0.0)
    vh = min(y1, sizeY) - max(y0, 0.0)
    (vw > 0 && vh > 0 && sizeX > 0 && sizeY > 0 && !isempty(level_xy)) || return 0
    longs = [max(vw * nx / sizeX, vh * ny / sizeY) for (nx, ny) in level_xy]
    ok = findall(>=(_LANDSCAPE_MIN_LONG_SIDE), longs)
    isempty(ok) ? argmax(longs) - 1 : ok[argmin(longs[ok])] - 1
end

# Count segmented objects per tile from `label_props` centroids for the current t.
# LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2a (`segCount`) — the "how many objects
# is Claude looking at in each tile" answer the composite alone doesn't give (a
# tile that reads bright can be five fused cells or one wash, and the eye can't
# reliably tell 5 vs 12 in a downsampled frame).
#
# Coordinate frame: centroids come out of `label_props` in LEVEL-0 pixel units and
# are tiled by `_tile_of` over the same rectangle the channel stats tile — the
# viewer's `view` when sent, else the whole image (`image_geometry(zp).sizeX/.sizeY`).
# Independent of the pyramid level the channels read (the fractional bin math is the
# same at every level).
#
# Non-timecourse images (no `centroid_t`): every row counts against t=0 — matches
# how the frontend treats a still (`shownT` sits at 0). A row with a non-finite
# centroid is silently dropped rather than 500'ing the whole compute — a stale
# label_props can carry a NaN centroid from a failed measure run.
function _tile_seg_counts(img::CciaImage, vn::AbstractString, t::Int,
                          ncols::Int, nrows::Int, sizeX::Int, sizeY::Int;
                          z_lo::Union{Int,Nothing} = nothing,
                          z_hi::Union{Int,Nothing} = nothing,
                          view::Union{Nothing,NTuple{4,Float64}} = nothing)::Union{Vector{Int},Nothing}
    (sizeX > 0 && sizeY > 0) || return nothing
    df = try
        label_props(img; value_name = vn) |> view_centroid_cols |> as_df
    catch
        return nothing
    end
    nm = names(df)
    ("centroid_x" in nm && "centroid_y" in nm) || return nothing
    xs = df[!, :centroid_x]
    ys = df[!, :centroid_y]
    ts = ("centroid_t" in nm) ? df[!, :centroid_t] : nothing
    zs = ("centroid_z" in nm) ? df[!, :centroid_z] : nothing
    _bin_centroids_to_tiles(xs, ys, ts, t, ncols, nrows, sizeX, sizeY;
                            zs = zs, z_lo = z_lo, z_hi = z_hi, view = view)
end

# Per-tile per-visible-pop counts (LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2b — `pops`).
# For each pop the user has toggled visible in the manager, count how many of its labels
# have centroids inside each tile at the shown t. Returns a Vector{Vector{NamedTuple}} —
# one entry per tile, each holding the (path, name, count) triples for pops with count > 0
# in that tile. Empty tile ⇒ empty inner vector (caller decides whether to emit a `pops`
# key or drop it, matching Decision 3 sparsity for zero-count tiles too).
#
# One label_props read shared across all visible pops; per-pop cost is O(|pop.labels|) once
# the label→tile map is built. Uses `resolve_pops` — the same authoritative population
# resolver `overlay_author` uses, so a Claude reader and a viewer see the SAME membership.
#
# `nothing` return means "can't compute pops for this (image, vn, popType)" — either the
# resolver failed or centroids are missing. Frontend degrades: no `pops` key on any tile.
#
# `paths`: the pops the viewer is drawing (its point eyes), which win over the manager's `.show` —
# `nothing` = `.show` (an older caller with no per-pop snapshot).
function _tile_pop_counts(img::CciaImage, pop_vn::AbstractString, pop_type::AbstractString,
                          t::Int, ncols::Int, nrows::Int, sizeX::Int, sizeY::Int;
                          z_lo::Union{Int,Nothing} = nothing,
                          z_hi::Union{Int,Nothing} = nothing,
                          paths::Union{Nothing,AbstractVector} = nothing,
                          view::Union{Nothing,NTuple{4,Float64}} = nothing)::Union{Vector{Vector{NamedTuple}},Nothing}
    (sizeX > 0 && sizeY > 0) || return nothing
    # Centroids: one label_props read, keyed by :label so we can bin per label id.
    df = try
        label_props(img; value_name = pop_vn) |> view_centroid_cols |> as_df
    catch
        return nothing
    end
    nm = names(df)
    (("centroid_x" in nm) && ("centroid_y" in nm) && ("label" in nm)) || return nothing
    has_t = "centroid_t" in nm
    has_z = "centroid_z" in nm && z_lo !== nothing && z_hi !== nothing
    # Build label → tile_index, filtered to this t (and to the Z slab when the viewer sent one
    # — see `_bin_centroids_to_tiles`, same round-then-in-range rule). A label absent from the
    # map (wrong t, wrong z, NaN centroid) is silently skipped when a pop asks about it.
    label_to_tile = Dict{Int,Int}()
    @inbounds for i in 1:size(df, 1)
        lab = df[i, :label]
        (lab isa Real && isfinite(Float64(lab))) || continue
        px = df[i, :centroid_x]; py = df[i, :centroid_y]
        (px isa Real && py isa Real && isfinite(Float64(px)) && isfinite(Float64(py))) || continue
        if has_t
            tv = df[i, :centroid_t]
            (tv isa Real && isfinite(Float64(tv))) || continue
            Int(round(Float64(tv))) == t || continue
        end
        if has_z
            zv = df[i, :centroid_z]
            (zv isa Real && isfinite(Float64(zv))) || continue
            zr = Int(round(Float64(zv)))
            (z_lo <= zr <= z_hi) || continue
        end
        ti = _tile_of(px, py, ncols, nrows, sizeX, sizeY, view)
        ti == 0 && continue
        label_to_tile[Int(round(Float64(lab)))] = ti
    end
    # Resolve the pops the manager knows about — SHOW-filtered per Decision 3 (only the ones
    # the user has ticked visible on the panel). `resolve_pops` gives us (path, name, labels,
    # show, …) NamedTuples.
    pops = try
        resolve_pops(img, pop_type; value_name = pop_vn)
    catch
        return nothing
    end
    if paths !== nothing
        want = Set(String.(paths))
        pops = [merge(p, (; show = true)) for p in pops if String(p.path) in want]
    end
    _pop_counts_from_label_map(pops, label_to_tile, ncols * nrows)
end

# Pure helper: given the visible-pop list (as `resolve_pops` returns) and a label→tile map,
# produce the per-tile `(path, name, count)` bag. Skips pops with `show == false` and pops
# whose labels don't land in any tile, and skips zero-count tiles per Decision 3 (an empty
# inner vector means "no visible pops occupy this tile" — the outer caller decides whether
# to emit a `pops` key on the response tile).
function _pop_counts_from_label_map(pops, label_to_tile::AbstractDict{Int,Int},
                                    n_tiles::Int)::Vector{Vector{NamedTuple}}
    per_tile = [NamedTuple[] for _ in 1:n_tiles]
    for p in pops
        Bool(get(p, :show, true)) || continue
        labs = get(p, :labels, Int[])
        isempty(labs) && continue
        counts = Dict{Int,Int}()
        for L in labs
            ti = get(label_to_tile, Int(L), 0)
            ti == 0 && continue
            counts[ti] = get(counts, ti, 0) + 1
        end
        name = String(get(p, :name, String(get(p, :path, ""))))
        path = String(get(p, :path, ""))
        for (ti, n) in counts
            (1 <= ti <= n_tiles) || continue
            push!(per_tile[ti], (path = path, name = name, count = n))
        end
    end
    per_tile
end

# Pure helper — no I/O, no DataFrame dependency, hermetically testable. `xs`/`ys` are
# centroid coordinates in level-0 pixel units; `ts` is the temporal column when the image
# is a timecourse (else `nothing` — every centroid counts against the queried `t`); `zs`
# is the vertical centroid column when the image is 3D (else `nothing` — no z filter).
# When `zs` AND both `z_lo`/`z_hi` are supplied, a centroid contributes only if
# `z_lo ≤ round(centroid_z) ≤ z_hi` — the "which Z slab does the viewer show" filter
# (plane mode sends z±1, volume mode sends the slab-slider range).
# `view` (level-0 px rect) tiles the viewer's view instead of the whole image; a centroid outside it
# drops (`_tile_of`).
# Returns the flat row-major count vector; NaN centroids and off-frame timepoints drop.
function _bin_centroids_to_tiles(xs::AbstractVector, ys::AbstractVector,
                                 ts::Union{AbstractVector,Nothing}, t::Int,
                                 ncols::Int, nrows::Int,
                                 sizeX::Int, sizeY::Int;
                                 zs::Union{AbstractVector,Nothing} = nothing,
                                 z_lo::Union{Int,Nothing} = nothing,
                                 z_hi::Union{Int,Nothing} = nothing,
                                 view::Union{Nothing,NTuple{4,Float64}} = nothing)::Vector{Int}
    n = length(xs)
    length(ys) == n || throw(ArgumentError("_bin_centroids_to_tiles: xs / ys length mismatch"))
    (ts === nothing || length(ts) == n) ||
        throw(ArgumentError("_bin_centroids_to_tiles: ts length mismatch"))
    (zs === nothing || length(zs) == n) ||
        throw(ArgumentError("_bin_centroids_to_tiles: zs length mismatch"))
    counts = zeros(Int, ncols * nrows)
    @inbounds for i in 1:n
        px = xs[i]; py = ys[i]
        (px isa Real && py isa Real && isfinite(Float64(px)) && isfinite(Float64(py))) || continue
        if ts !== nothing
            tv = ts[i]
            (tv isa Real && isfinite(Float64(tv))) || continue
            Int(round(Float64(tv))) == t || continue
        end
        if zs !== nothing && z_lo !== nothing && z_hi !== nothing
            zv = zs[i]
            (zv isa Real && isfinite(Float64(zv))) || continue
            zr = Int(round(Float64(zv)))
            (z_lo <= zr <= z_hi) || continue
        end
        ti = _tile_of(px, py, ncols, nrows, sizeX, sizeY, view)
        ti == 0 && continue
        counts[ti] += 1
    end
    counts
end

# Per-tile track summary (LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 3 — `tracks`). For each
# tile with any tracked cell at the shown t, emit `{count, meanDuration, meanSpeed}`:
#   count        = distinct track_ids with a cell in this tile at t
#   meanDuration = mean of per-track num_cells (full lifetime in frames) across those tracks
#   meanSpeed    = mean of per-cell `live.cell.speed` across cells in this tile at t
#                  — an INSTANTANEOUS spatial measure ("how fast are cells moving here right
#                  now"), not the per-track average speed over its whole lifetime. Present
#                  only when `live.cell.speed` is on the segmentation's obs.
#
# Returns Vector{Union{Nothing,NamedTuple}} — nothing for tiles with no tracked cells at t.
# Caller decides whether to emit a `tracks` key on the response tile; sparsity carries.
#
# `nothing` return (outer): the segmentation is unmeasured / untracked / label_props missing —
# the compute handler drops the tracks pass entirely, no `tracks` key on any tile.
function _tile_track_summary(img::CciaImage, vn::AbstractString, t::Int,
                             ncols::Int, nrows::Int, sizeX::Int, sizeY::Int;
                             z_lo::Union{Int,Nothing} = nothing,
                             z_hi::Union{Int,Nothing} = nothing,
                             view::Union{Nothing,NTuple{4,Float64}} = nothing)::Union{Vector{Union{Nothing,NamedTuple}},Nothing}
    (sizeX > 0 && sizeY > 0) || return nothing
    # Ask for the columns we need. `select_cols` @warns for absent columns and drops them —
    # `live.cell.speed` is optional so a segmentation without it is not an error. `track_id`
    # is required; a missing one degrades to "no tracks summary" via the guard below.
    df = try
        label_props(img; value_name = vn) |>
            (lp -> select_cols(lp, ["track_id", "live.cell.speed"])) |>
            view_centroid_cols |> as_df
    catch
        return nothing
    end
    nm = names(df)
    (("centroid_x" in nm) && ("centroid_y" in nm) && ("track_id" in nm)) || return nothing
    has_t = "centroid_t" in nm
    has_z = "centroid_z" in nm && z_lo !== nothing && z_hi !== nothing
    has_speed = "live.cell.speed" in nm
    # First pass: per-track lifetime (num_cells across ALL t AND all z) so a per-tile mean of
    # durations reflects the whole track, not just visible frames or slices. Applying the Z
    # filter here would count a track's duration as ONLY the frames its centroid is inside the
    # viewer slab — that misrepresents track lifetime for a summary that means "how long has
    # this cell been alive." The Z filter belongs on the second pass (tile membership) only.
    duration_by_track = Dict{Int,Int}()
    @inbounds for i in 1:size(df, 1)
        tid = df[i, :track_id]
        (tid isa Real && isfinite(Float64(tid)) && Float64(tid) > 0) || continue
        k = Int(round(Float64(tid)))
        duration_by_track[k] = get(duration_by_track, k, 0) + 1
    end
    # Second pass: bin cells at the shown t (and inside the viewer's Z slab when the frontend
    # sent one — see `_bin_centroids_to_tiles` for the round-then-in-range rule) into tiles,
    # tracking track_ids + accumulating per-cell speeds for the instantaneous meanSpeed.
    n_tiles = ncols * nrows
    tile_track_ids = [Set{Int}() for _ in 1:n_tiles]
    tile_speed_sum = zeros(Float64, n_tiles)
    tile_speed_n   = zeros(Int, n_tiles)
    @inbounds for i in 1:size(df, 1)
        tid = df[i, :track_id]
        (tid isa Real && isfinite(Float64(tid)) && Float64(tid) > 0) || continue
        px = df[i, :centroid_x]; py = df[i, :centroid_y]
        (px isa Real && py isa Real && isfinite(Float64(px)) && isfinite(Float64(py))) || continue
        if has_t
            tv = df[i, :centroid_t]
            (tv isa Real && isfinite(Float64(tv))) || continue
            Int(round(Float64(tv))) == t || continue
        end
        if has_z
            zv = df[i, :centroid_z]
            (zv isa Real && isfinite(Float64(zv))) || continue
            zr = Int(round(Float64(zv)))
            (z_lo <= zr <= z_hi) || continue
        end
        ti = _tile_of(px, py, ncols, nrows, sizeX, sizeY, view)
        ti == 0 && continue
        push!(tile_track_ids[ti], Int(round(Float64(tid))))
        if has_speed
            sp = df[i, Symbol("live.cell.speed")]
            if sp isa Real && isfinite(Float64(sp))
                tile_speed_sum[ti] += Float64(sp)
                tile_speed_n[ti]   += 1
            end
        end
    end
    _track_summary_from_binned(tile_track_ids, tile_speed_sum, tile_speed_n, duration_by_track)
end

# Pure aggregator — no I/O, hermetically testable. Given per-tile track_id sets + speed
# accumulators + a global track→duration map, produce the (count, meanDuration, meanSpeed)
# summary per tile (nothing for empty tiles).
function _track_summary_from_binned(tile_track_ids::AbstractVector{<:AbstractSet{Int}},
                                    tile_speed_sum::AbstractVector{<:Real},
                                    tile_speed_n::AbstractVector{<:Integer},
                                    duration_by_track::AbstractDict{Int,<:Integer})::Vector{Union{Nothing,NamedTuple}}
    n = length(tile_track_ids)
    (length(tile_speed_sum) == n && length(tile_speed_n) == n) ||
        throw(ArgumentError("_track_summary_from_binned: length mismatch across tile inputs"))
    out = Vector{Union{Nothing,NamedTuple}}(nothing, n)
    for i in 1:n
        s = tile_track_ids[i]
        isempty(s) && continue
        count = length(s)
        dur_sum = 0.0; dur_n = 0
        for k in s
            d = get(duration_by_track, k, 0)
            d > 0 || continue
            dur_sum += Float64(d); dur_n += 1
        end
        mean_duration = dur_n > 0 ? dur_sum / dur_n : NaN
        mean_speed = tile_speed_n[i] > 0 ? tile_speed_sum[i] / tile_speed_n[i] : NaN
        out[i] = (count = count, meanDuration = mean_duration, meanSpeed = mean_speed)
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

Body: `{ projectUid, imageUid, valueName, t, z?, cols, rows, view?: {x0, y0, x1, y1},
         channels: [{index, name}], labelsValueName?,
         popLayers?: [{valueName, popType, paths?}], popValueName?, popType?, tracksValueName? }`
Reply: `{ tiles: [{tileId, channels: {name: {mean, snr}}, segCount?, pops?, tracks?}, ...],
           sourceRun?: {segCount?, pops?, tracks?, channels?} }`

Called by the browser at Share time (or on explicit augmented-recompute) so the
capture envelope carries per-channel per-tile stats alongside the frontend's
category — the complementary payload from LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 1+.

`channels` is the VISIBILITY snapshot (Decision 3): only currently-visible channels
appear here, and only those appear in the response. `index` is the 0-based channel
index in the store; `name` is the display name the frontend uses. Unknown indices
are silently dropped — a stale visibility snapshot doesn't break the write.

`labelsValueName` is the (Phase 2a) visibility snapshot for the segmentation layer:
the vn of the labels the user has toggled on in `ViewerPanel`. Non-empty ⇒ each
tile gets a `segCount` (int) — how many segmented objects have centroids inside
that tile at the shown t. Empty / omitted ⇒ tiles have no `segCount` key (sparse
by visibility per Decision 3). A vn that has no `label_props` on disk is treated
as absent (a fresh unmeasured image doesn't 500 the whole compute).

`popLayers` is the (Phase 2b) visibility snapshot for the population overlay: every
(segmentation, pop type) the viewer draws, `paths` the pops in it whose eye is on (absent
⇒ the pop manager's `.show`). The older `popValueName` + `popType` pair (the pop manager's
one set) is read when `popLayers` is absent. Each tile with any visible-pop members gets
`pops: [{path, name, count, layer}]` for pops with count > 0 in THAT tile — `layer` indexes
`sourceRun.pops` (its `valueName` / `popType`), since paths collide across segmentations (sparse per Decision 3 — zero-count pops are simply absent, and a
tile with no member pops has no `pops` key at all). Same coordinate frame as
segCount: level-0 pixel bins.

`tracksValueName` is the (Phase 3) visibility snapshot for the tracks overlay: the
first vn the frontend has ticked visible in `getTrackVisibility`. Non-empty ⇒
each tile with any tracked cell at the shown t gets
`tracks: {count, meanDuration, meanSpeed}`:
  count        — distinct track_ids with a cell in this tile at t
  meanDuration — mean of per-track num_cells (full lifetime) across those tracks
  meanSpeed    — mean of per-cell `live.cell.speed` in this tile at t (instantaneous
                 local spatial measure; NaN when the segmentation has no speed obs).
An untracked vn (no `track_id` obs) drops the pass; no `tracks` key on any tile.

`sourceRun` is the (Phase 4) per-field provenance bag at the LANDSCAPE level
(Decision 4 — not sprinkled into each tile). One key per field that was actually
computed; the value names what the field came from:
  `segCount = {valueName, labelsVersion}`
  `pops     = [{valueName, popType, gatingMtime}]` one per pop layer (gating file mtime)
  `tracks   = {valueName, labelsVersion}`
  `channels = {valueName, imageVersion, level}`    (pyramid level actually read)
Sparse — a field absent from the response has no `sourceRun` key. Enables
"which run produced this number" reads on the capture envelope; also lets Kiwi /
MCP compare two envelopes and say "the newer one saw a re-tracked h5ad".

Cost: one plane read per visible channel at a pyramid level chosen to keep the long
side ≥ 512 px, plus O(ncols*nrows) per channel for the tile aggregations. Typical
call for a 3-channel visible tile at 8×8 is < 200 ms; a 32×32 request on a 4-channel
image lands in the 500 ms – 2 s budget the plan allocates for the augmented layer.
segCount / pops each add one `.h5ad` read of centroid columns + `O(nCells + nPops)`
binning; `resolve_pops` is cached per (vn, popType) on the CciaImage so a re-run for
the same visibility snapshot pays only the first-call cost. `sourceRun` is metadata
lookups only (`resolve_version`, `mtime`) — sub-millisecond.
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
    # Phase 6 (Z-awareness): the frontend snapshots the viewer's `mode` + Z scope so the compute
    # matches what the user is looking at. `renderMode` is either 'plane' (single-slice viewer)
    # or 'volume' (MIP through a slab). `zLo`/`zHi` are the inclusive Z bounds for that mode —
    # plane sends z±1 (matches the gating page's pick-rect z-scope pattern), volume sends the
    # slab-slider range. When both bounds are absent or `nZ == 1`, the filter is a no-op and
    # behaviour matches the pre-Phase-6 whole-stack collapse.
    render_mode_raw = get(body, :renderMode, "plane")
    render_mode = render_mode_raw isa AbstractString && String(render_mode_raw) == "volume" ?
                  "volume" : "plane"
    z_lo_raw = _clean_int(get(body, :zLo, nothing), -1)
    z_hi_raw = _clean_int(get(body, :zHi, nothing), -1)
    z_lo = z_lo_raw < 0 ? nothing : z_lo_raw
    z_hi = z_hi_raw < 0 ? nothing : z_hi_raw
    # Guard: `zLo > zHi` came off the wire backwards — swap rather than reject so the frontend's
    # min/max order isn't a load-bearing invariant.
    if z_lo !== nothing && z_hi !== nothing && z_lo > z_hi
        z_lo, z_hi = z_hi, z_lo
    end
    # The image rectangle the viewer shows (level-0 px). Present ⇒ tiles cut THAT rectangle — the frame
    # the browser's category layer tiles — instead of the whole image. The browser omits it when no
    # flat rectangle matches the screen (a rotated or perspective 3D view) and skips this call.
    view = _landscape_view(get(body, :view, nothing))
    ncols = clamp(_clean_int(get(body, :cols, nothing), 8), 2, 64)
    nrows = clamp(_clean_int(get(body, :rows, nothing), ncols), 2, 64)
    channels_raw = get(body, :channels, nothing)
    channels_raw isa AbstractVector || return 400, JSON3.write((; error = "channels required (list)"))
    labels_vn = _wstr(body, :labelsValueName)   # empty ⇒ segCount off (Decision 3 sparsity)
    pop_layers = _landscape_pop_layers(body)
    pops_on   = !isempty(pop_layers)
    tracks_vn = _wstr(body, :tracksValueName)   # empty ⇒ tracks off
    # Early-out: nothing to compute (no channels AND no labels/pops/tracks layer visible).
    # Return an empty tiles list rather than 400 — the frontend calls this optimistically at
    # Share time and a bare landscape (grid-density picked, no layers on) is a legitimate shape.
    isempty(channels_raw) && isempty(labels_vn) && !pops_on && isempty(tracks_vn) &&
        return 200, JSON3.write((; tiles = []))

    vnn = isempty(value_name) ? nothing : String(value_name)
    zp, _td, err = resolve_image_version(project_uid, image_uid, vnn; version = nothing)
    err === nothing || return 404, JSON3.write((; error = err))

    t = t_raw < 0 ? 0 : t_raw
    z = z_raw < 0 ? nothing : z_raw

    # Row-major flat tile bag, `channels` empty by default (Decision 3 sparsity — never
    # emit `channels: {}` on a tile that saw no visible-channel augmentation).
    tiles = [Dict{String,Any}("tileId" => _tile_cell_label(r, c),
                              "channels" => Dict{String,Dict{String,Float64}}())
             for r in 0:(nrows - 1), c in 0:(ncols - 1)]
    tiles = vec(permutedims(tiles, (2, 1)))

    # Phase 4 sourceRun bag — populated as each field lands. Only keys for fields that
    # were actually computed appear (sparse mirrors the tile-level sparsity).
    source_run = Dict{String,Any}()

    # Level-0 size — the frame `view` and the centroids are in.
    sizeX = 0; sizeY = 0
    try
        geo = image_geometry(zp)
        sizeX = Int(geo.sizeX); sizeY = Int(geo.sizeY)
    catch
    end

    # ── Per-channel mean + SNR (Phase 1) — only when the frontend sent channels.
    channels_emitted = false
    channels_level = 0
    if !isempty(channels_raw) && !(view !== nothing && sizeX <= 0)
        level = view === nothing ? _pick_landscape_level(zp) :
                _pick_view_level(_landscape_level_xy(zp), sizeX, sizeY, view)
        channels_level = level
        # Open once for many channel reads — same pattern as `_sampled_specs`; per-open cost
        # is a metadata round-trip that would dominate if we redid it per channel.
        arr, caxes = try
            open_level(zp, level)
        catch e
            return 500, JSON3.write((; error = "landscape compute: could not open store — $(sprint(showerror, e))"))
        end
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
        # Channels read shape depends on renderMode:
        #   plane  → single plane at `z` (matches what the viewer draws)
        #   volume → slab [zLo, zHi] followed by per-pixel MIP (matches the viewer's MIP);
        #            falls back to single plane when the frontend didn't send zLo/zHi
        #            (2D image, or an older client), so this is safe to add without a
        #            client-version handshake.
        z_read = if render_mode == "volume" && z_lo !== nothing && z_hi !== nothing
            z_lo:z_hi
        else
            z
        end
        # With a view: read only its crop at this level, and tile the view's rectangle in the crop's
        # pixel coords. A view wholly off the image reads nothing.
        xr = nothing; yr = nothing; frame = nothing
        if view !== nothing
            lw = haskey(dims, "x") ? size(arr, dims["x"]) : sizeX
            lh = haskey(dims, "y") ? size(arr, dims["y"]) : sizeY
            fx = lw / sizeX; fy = lh / sizeY
            vx0, vy0, vx1, vy1 = view[1] * fx, view[2] * fy, view[3] * fx, view[4] * fy
            xr = max(floor(Int, vx0), 0):(min(ceil(Int, vx1), lw) - 1)
            yr = max(floor(Int, vy0), 0):(min(ceil(Int, vy1), lh) - 1)
            (isempty(xr) || isempty(yr)) && (channels = Tuple{Int,String}[])
            frame = (vx0 - first(xr), vy0 - first(yr), vx1 - first(xr), vy1 - first(yr))
        end
        for (ci, cname) in channels
            vol, nx, ny, nz_read, _nc = try
                read_slab(arr, caxes, t, ci; z = z_read, x = xr, y = yr)
            catch
                continue
            end
            (nx == 0 || ny == 0) && continue
            # `vol` is (x, y, [z]) with scalar c dropped. Scalar z drops the z dim → 2D (x, y);
            # range z keeps it → 3D (x, y, nz) where we MIP over z to a plane.
            plane = if ndims(vol) == 2
                vol
            elseif ndims(vol) == 3 && nz_read > 0
                dropdims(maximum(vol, dims = 3); dims = 3)
            else
                reshape(vol, nx, ny)
            end
            stats = _tile_channel_stats(plane, ncols, nrows; frame = frame)
            for r in 0:(nrows - 1), c in 0:(ncols - 1)
                st = stats[r * ncols + c + 1]
                st === nothing && continue
                (mean, snr) = st
                tiles[r * ncols + c + 1]["channels"][cname] = Dict("mean" => round(mean, digits = 4),
                                                                    "snr"  => round(snr,  digits = 3))
            end
            channels_emitted = true
        end
    end

    # ── Image-object + geometry for the label-props-driven augmentations (Phase 2a/2b/3).
    # Resolved once and shared: `init_object` reads ccid.json, and `image_geometry` reads
    # `.zarray` metadata — either is cheap on its own, but paying twice when segCount + pops
    # + tracks all run in one compute is wasteful. Channels-only computes skip this block.
    img_obj = nothing
    if (!isempty(labels_vn) || pops_on || !isempty(tracks_vn)) && sizeX > 0 && sizeY > 0
        try
            img_obj = init_object(project_uid, image_uid)
            img_obj isa CciaImage || (img_obj = nothing)
        catch
            img_obj = nothing
        end
    end

    # ── Per-tile segCount (Phase 2a) — only when the frontend has a labels layer on.
    # A missing/unmeasured vn returns nothing and tiles just don't get a `segCount` key —
    # the sparsity rule handles it.
    if !isempty(labels_vn) && img_obj !== nothing
        seg_counts = try
            _tile_seg_counts(img_obj, labels_vn, t, ncols, nrows, sizeX, sizeY;
                             z_lo = z_lo, z_hi = z_hi, view = view)
        catch
            nothing
        end
        if seg_counts !== nothing
            for i in eachindex(tiles)
                tiles[i]["segCount"] = seg_counts[i]
            end
            source_run["segCount"] = _source_run_for_labels(img_obj, labels_vn)
        end
    end

    # ── Per-tile pops (Phase 2b) — every pop layer the viewer draws, each filtered to the pops
    # whose eye is on (`paths`; else `resolve_pops`' `.show`), so we get exactly what the viewer
    # is currently painting. Empty tile ⇒ no `pops` key (Decision 3 sparsity carries all
    # the way down: an off pop is absent, a zero-count tile is absent, no false zeros).
    if pops_on && img_obj !== nothing
        runs = Dict{String,Any}[]
        for L in pop_layers
            pop_tiles = try
                _tile_pop_counts(img_obj, L.valueName, L.popType, t, ncols, nrows, sizeX, sizeY;
                                 z_lo = z_lo, z_hi = z_hi, paths = L.paths, view = view)
            catch
                nothing
            end
            pop_tiles === nothing && continue
            # A tile pop names its layer by index into `sourceRun.pops` (`layer`, 0-based): `/qc` on
            # two segmentations is two pops (VIEWER_OVERLAY_PARITY_PLAN.md P6).
            # Record provenance even if all tiles were empty — the compute ran, and a reader
            # seeing "pops in sourceRun but no `pops` on any tile" learns that the visible-pops
            # snapshot HAD nothing landing on-screen, not that pops weren't asked for.
            li = length(runs)
            push!(runs, _source_run_for_pops(img_obj, L.valueName, L.popType))
            for i in eachindex(tiles)
                bag = pop_tiles[i]
                isempty(bag) && continue
                append!(get!(tiles[i], "pops", Dict{String,Any}[]),
                        [Dict{String,Any}("path" => p.path, "name" => p.name, "count" => p.count,
                                          "layer" => li)
                         for p in bag])
            end
        end
        isempty(runs) || (source_run["pops"] = runs)
    end

    # ── Per-tile tracks summary (Phase 3) — only when the frontend has a tracks layer on
    # AND the vn actually carries tracks. Empty tiles (no tracked cells at t) get no
    # `tracks` key — sparsity discipline all the way down.
    if !isempty(tracks_vn) && img_obj !== nothing
        track_tiles = try
            _tile_track_summary(img_obj, tracks_vn, t, ncols, nrows, sizeX, sizeY;
                                z_lo = z_lo, z_hi = z_hi, view = view)
        catch
            nothing
        end
        if track_tiles !== nothing
            for i in eachindex(tiles)
                s = track_tiles[i]
                s === nothing && continue
                # Round the floats to keep the envelope tight; NaN → JSON null so the frontend
                # sees "unknown" (typically meanSpeed when the segmentation has no speed obs)
                # rather than a raw NaN which some JSON parsers refuse.
                md = isfinite(s.meanDuration) ? round(Float64(s.meanDuration), digits = 2) : nothing
                ms = isfinite(s.meanSpeed)    ? round(Float64(s.meanSpeed),    digits = 3) : nothing
                bag = Dict{String,Any}("count" => s.count)
                md === nothing || (bag["meanDuration"] = md)
                ms === nothing || (bag["meanSpeed"]    = ms)
                tiles[i]["tracks"] = bag
            end
            source_run["tracks"] = _source_run_for_labels(img_obj, tracks_vn)
        end
    end

    # ── Phase 4 sourceRun for channels — identity is (valueName, imageVersion, pyramid
    # level). imageVersion is the resolved vN of the shown filepath (what `_latest` points
    # at). Load img_obj on demand for a channels-only compute (labels/pops/tracks would
    # already have loaded it above).
    if channels_emitted
        if img_obj === nothing
            img_obj = try
                obj = init_object(project_uid, image_uid)
                obj isa CciaImage ? obj : nothing
            catch; nothing end
        end
        img_version = try
            iv = img_obj === nothing ? nothing : resolve_version(img_obj, :filepath, vnn)
            iv isa AbstractString ? String(iv) : LATEST_DEFAULT_VAL
        catch
            LATEST_DEFAULT_VAL
        end
        source_run["channels"] = Dict("valueName" => isempty(value_name) ? "default" : String(value_name),
                                       "imageVersion" => img_version,
                                       "level" => channels_level)
    end

    # Drop the empty `channels` bag when this compute didn't populate any (Decision 3
    # sparsity — a v2 tile with no visible-channel data has NO `channels` key at all,
    # matching the frontend `augmentLandscape` merge rule).
    for t_dict in tiles
        ch = get(t_dict, "channels", nothing)
        (ch isa AbstractDict && isempty(ch)) && delete!(t_dict, "channels")
    end

    # Phase 6 viewport bag — records the Z reduction the compute used, so a reader (Kiwi, MCP,
    # a Claude session comparing two captures) can distinguish `segCount: 8` on a plane-mode
    # z=5±1 slab from `segCount: 8` on a volume-mode MIP through all 30 slices. Emitted
    # unconditionally whenever we computed anything at all, since the render mode determines
    # what a `segCount` / `pops.count` / channel mean/snr NUMBER means.
    include_viewport = !isempty(source_run) || channels_emitted
    viewport = if include_viewport
        vp = Dict{String,Any}("renderMode" => render_mode)
        z_lo === nothing || (vp["zLo"] = z_lo)
        z_hi === nothing || (vp["zHi"] = z_hi)
        # The level-0 image rectangle the tiles cut — a reader converts tile ↔ image px with it.
        view === nothing || (vp["view"] = Dict("x0" => round(view[1], digits = 1), "y0" => round(view[2], digits = 1),
                                               "x1" => round(view[3], digits = 1), "y1" => round(view[4], digits = 1)))
        vp
    else
        nothing
    end

    # Only include `sourceRun` / `viewport` in the response when at least one field was actually
    # computed — matches the sparse-by-visibility discipline the tiles carry.
    if isempty(source_run) && viewport === nothing
        200, JSON3.write((; tiles = tiles))
    elseif isempty(source_run)
        200, JSON3.write((; tiles = tiles, viewport = viewport))
    elseif viewport === nothing
        200, JSON3.write((; tiles = tiles, sourceRun = source_run))
    else
        200, JSON3.write((; tiles = tiles, sourceRun = source_run, viewport = viewport))
    end
end

# Provenance helpers (LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 4, Decision 4). Each returns
# a small Dict identifying the run that produced the corresponding tile field. Kept
# separate from the compute so a test can call them without a full request round-trip.

function _source_run_for_labels(img::CciaImage, vn::AbstractString)::Dict{String,Any}
    lv = try
        String(resolve_version(img, :label_props, vn))
    catch
        LATEST_DEFAULT_VAL
    end
    Dict("valueName" => String(vn), "labelsVersion" => lv)
end

# The population layers to count: `popLayers = [{valueName, popType, paths?}]` — every
# (segmentation, pop type) the viewer draws, `paths` its shown pops — else the older single
# `popValueName` + `popType` (the pop manager's). Layers missing either name are dropped.
function _landscape_pop_layers(body)
    raw = get(body, :popLayers, nothing)
    out = NamedTuple{(:valueName, :popType, :paths),Tuple{String,String,Union{Nothing,Vector{String}}}}[]
    if raw isa AbstractVector
        for L in raw
            L isa AbstractDict || continue
            vn = _wstr(L, :valueName); pt = _wstr(L, :popType)
            (isempty(vn) || isempty(pt)) && continue
            ps = get(L, :paths, nothing)
            push!(out, (; valueName = vn, popType = pt,
                          paths = ps isa AbstractVector ? String[string(p) for p in ps] : nothing))
        end
        return out
    end
    vn = _wstr(body, :popValueName); pt = _wstr(body, :popType)
    (isempty(vn) || isempty(pt)) || push!(out, (; valueName = vn, popType = pt, paths = nothing))
    out
end

function _source_run_for_pops(img::CciaImage, vn::AbstractString,
                              pop_type::AbstractString)::Dict{String,Any}
    # Fingerprint the gating map by its on-disk mtime — a reader comparing two envelopes'
    # `sourceRun.pops.gatingMtime` knows whether the underlying map moved between shares.
    # String, not Float — JSON round-trips exact seconds with no precision loss, and matches
    # the shape `Cecelia.gating`'s internal `_pop_df_mtime` cache key uses ("∅" when absent).
    mtime_s = try
        path = gating_path(img._dir, String(vn); pop_type = String(pop_type))
        isfile(path) ? string(mtime(path)) : "∅"
    catch
        "∅"
    end
    Dict("valueName" => String(vn), "popType" => String(pop_type), "gatingMtime" => mtime_s)
end

# Test-only reset. Not registered as a route — tests import the module and call it
# directly to get a hermetic state between assertions. Deliberately private.
function _reset_landscape!()
    lock(_LANDSCAPE_LOCK) do
        empty!(_LANDSCAPE_BY_KEY)
    end
    nothing
end
