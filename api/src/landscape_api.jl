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

# Count segmented objects per tile from `label_props` centroids for the current t.
# LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2a (`segCount`) — the "how many objects
# is Claude looking at in each tile" answer the composite alone doesn't give (a
# tile that reads bright can be five fused cells or one wash, and the eye can't
# reliably tell 5 vs 12 in a downsampled frame).
#
# Coordinate frame: centroids come out of `label_props` in LEVEL-0 pixel units,
# and Phase 1's `_tile_channel_stats` bins the full frame — so we do the same,
# using `image_geometry(zp).sizeX / .sizeY` as the divisor. Independent of the
# pyramid level Phase 1 read (the fractional bin math is the same at every level).
#
# Non-timecourse images (no `centroid_t`): every row counts against t=0 — matches
# how the frontend treats a still (`shownT` sits at 0). A row with a non-finite
# centroid is silently dropped rather than 500'ing the whole compute — a stale
# label_props can carry a NaN centroid from a failed measure run.
function _tile_seg_counts(img::CciaImage, vn::AbstractString, t::Int,
                          ncols::Int, nrows::Int, sizeX::Int, sizeY::Int)::Union{Vector{Int},Nothing}
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
    _bin_centroids_to_tiles(xs, ys, ts, t, ncols, nrows, sizeX, sizeY)
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
function _tile_pop_counts(img::CciaImage, pop_vn::AbstractString, pop_type::AbstractString,
                          t::Int, ncols::Int, nrows::Int, sizeX::Int, sizeY::Int)::Union{Vector{Vector{NamedTuple}},Nothing}
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
    # Build label → tile_index, filtered to this t. A label absent from the map (wrong t,
    # NaN centroid) is silently skipped when a pop asks about it — same discipline as
    # `_bin_centroids_to_tiles`, just with a lookup instead of an accumulator.
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
        c = clamp(floor(Int, (Float64(px) / sizeX) * ncols), 0, ncols - 1)
        r = clamp(floor(Int, (Float64(py) / sizeY) * nrows), 0, nrows - 1)
        label_to_tile[Int(round(Float64(lab)))] = r * ncols + c + 1
    end
    # Resolve the pops the manager knows about — SHOW-filtered per Decision 3 (only the ones
    # the user has ticked visible on the panel). `resolve_pops` gives us (path, name, labels,
    # show, …) NamedTuples.
    pops = try
        resolve_pops(img, pop_type; value_name = pop_vn)
    catch
        return nothing
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
# is a timecourse (else `nothing` — every centroid counts against the queried `t`).
# Returns the flat row-major count vector; NaN centroids and off-frame timepoints drop.
function _bin_centroids_to_tiles(xs::AbstractVector, ys::AbstractVector,
                                 ts::Union{AbstractVector,Nothing}, t::Int,
                                 ncols::Int, nrows::Int,
                                 sizeX::Int, sizeY::Int)::Vector{Int}
    n = length(xs)
    length(ys) == n || throw(ArgumentError("_bin_centroids_to_tiles: xs / ys length mismatch"))
    (ts === nothing || length(ts) == n) ||
        throw(ArgumentError("_bin_centroids_to_tiles: ts length mismatch"))
    counts = zeros(Int, ncols * nrows)
    @inbounds for i in 1:n
        px = xs[i]; py = ys[i]
        (px isa Real && py isa Real && isfinite(Float64(px)) && isfinite(Float64(py))) || continue
        if ts !== nothing
            tv = ts[i]
            (tv isa Real && isfinite(Float64(tv))) || continue
            Int(round(Float64(tv))) == t || continue
        end
        c = clamp(floor(Int, (Float64(px) / sizeX) * ncols), 0, ncols - 1)
        r = clamp(floor(Int, (Float64(py) / sizeY) * nrows), 0, nrows - 1)
        counts[r * ncols + c + 1] += 1
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
                             ncols::Int, nrows::Int, sizeX::Int, sizeY::Int)::Union{Vector{Union{Nothing,NamedTuple}},Nothing}
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
    has_speed = "live.cell.speed" in nm
    # First pass: per-track lifetime (num_cells across ALL t) so a per-tile mean of durations
    # reflects the whole track, not just visible frames.
    duration_by_track = Dict{Int,Int}()
    @inbounds for i in 1:size(df, 1)
        tid = df[i, :track_id]
        (tid isa Real && isfinite(Float64(tid)) && Float64(tid) > 0) || continue
        k = Int(round(Float64(tid)))
        duration_by_track[k] = get(duration_by_track, k, 0) + 1
    end
    # Second pass: bin cells at the shown t into tiles, tracking track_ids + accumulating
    # per-cell speeds for the instantaneous meanSpeed aggregate.
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
        c = clamp(floor(Int, (Float64(px) / sizeX) * ncols), 0, ncols - 1)
        r = clamp(floor(Int, (Float64(py) / sizeY) * nrows), 0, nrows - 1)
        ti = r * ncols + c + 1
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

Body: `{ projectUid, imageUid, valueName, t, z?, cols, rows,
         channels: [{index, name}], labelsValueName?,
         popValueName?, popType?, tracksValueName? }`
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

`popValueName` + `popType` are the (Phase 2b) visibility snapshot for the population
overlay: the pop manager's currently-active (vn, popType) — matches what
`overlay_author` reads via `resolve_pops`. Both non-empty ⇒ each tile with any
visible-pop members gets `pops: [{path, name, count}]` for pops with count > 0
in THAT tile (sparse per Decision 3 — zero-count pops are simply absent, and a
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
  `pops     = {valueName, popType, gatingMtime}`   (gating file mtime, cache-key parity)
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
    ncols = clamp(_clean_int(get(body, :cols, nothing), 8), 2, 64)
    nrows = clamp(_clean_int(get(body, :rows, nothing), ncols), 2, 64)
    channels_raw = get(body, :channels, nothing)
    channels_raw isa AbstractVector || return 400, JSON3.write((; error = "channels required (list)"))
    labels_vn = _wstr(body, :labelsValueName)   # empty ⇒ segCount off (Decision 3 sparsity)
    pop_vn    = _wstr(body, :popValueName)      # both required for pops — the pop manager's
    pop_type  = _wstr(body, :popType)           # (vn, popType), from `cc.gatingCurrent`
    pops_on   = !isempty(pop_vn) && !isempty(pop_type)
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

    # ── Per-channel mean + SNR (Phase 1) — only when the frontend sent channels.
    channels_emitted = false
    channels_level = 0
    if !isempty(channels_raw)
        level = _pick_landscape_level(zp)
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
            channels_emitted = true
        end
    end

    # ── Image-object + geometry for the label-props-driven augmentations (Phase 2a/2b/3).
    # Resolved once and shared: `init_object` reads ccid.json, and `image_geometry` reads
    # `.zarray` metadata — either is cheap on its own, but paying twice when segCount + pops
    # + tracks all run in one compute is wasteful. Channels-only computes skip this block.
    img_obj = nothing
    sizeX = 0; sizeY = 0
    if !isempty(labels_vn) || pops_on || !isempty(tracks_vn)
        try
            geo = image_geometry(zp)
            sizeX = Int(geo.sizeX); sizeY = Int(geo.sizeY)
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
            _tile_seg_counts(img_obj, labels_vn, t, ncols, nrows, sizeX, sizeY)
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

    # ── Per-tile pops (Phase 2b) — only when the pop manager's (vn, popType) is set AND
    # the layer is on. `resolve_pops` filters by `.show`, so we get exactly what the viewer
    # is currently painting. Empty tile ⇒ no `pops` key (Decision 3 sparsity carries all
    # the way down: an off pop is absent, a zero-count tile is absent, no false zeros).
    if pops_on && img_obj !== nothing
        pop_tiles = try
            _tile_pop_counts(img_obj, pop_vn, pop_type, t, ncols, nrows, sizeX, sizeY)
        catch
            nothing
        end
        if pop_tiles !== nothing
            for i in eachindex(tiles)
                bag = pop_tiles[i]
                isempty(bag) && continue
                tiles[i]["pops"] = [Dict("path" => p.path, "name" => p.name, "count" => p.count)
                                    for p in bag]
            end
            # Record provenance even if all tiles were empty — the compute ran, and a reader
            # seeing "pops in sourceRun but no `pops` on any tile" learns that the visible-pops
            # snapshot HAD nothing landing on-screen, not that pops weren't asked for.
            source_run["pops"] = _source_run_for_pops(img_obj, pop_vn, pop_type)
        end
    end

    # ── Per-tile tracks summary (Phase 3) — only when the frontend has a tracks layer on
    # AND the vn actually carries tracks. Empty tiles (no tracked cells at t) get no
    # `tracks` key — sparsity discipline all the way down.
    if !isempty(tracks_vn) && img_obj !== nothing
        track_tiles = try
            _tile_track_summary(img_obj, tracks_vn, t, ncols, nrows, sizeX, sizeY)
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

    # Only include `sourceRun` in the response when at least one field was actually
    # computed — matches the sparse-by-visibility discipline the tiles carry.
    if isempty(source_run)
        200, JSON3.write((; tiles = tiles))
    else
        200, JSON3.write((; tiles = tiles, sourceRun = source_run))
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
