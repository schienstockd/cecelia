# ── Decoded-chunk cache ───────────────────────────────────────────────────────────
# `read_native` (image_geometry.jl) reads through `cached_read`, which assembles the requested block
# from DECODED zarr chunks kept in memory. The viewer's bricks are 128² through all z, and stores are
# one z-plane per chunk at 512² or 1024², so one brick decodes 16–64x the bytes it keeps — and the 64
# bricks of one timepoint all decode the SAME chunks. With the chunks resident, the second and later
# bricks of a timepoint are a slice and a copy. Design + measurements: docs/todo/SLAB_READ_PERF_PLAN.md
# (Decisions 5–8, 11).
#
# Keyed on the level array's directory (path, inode, mtime) plus the chunk index. Every writer stages
# a new store and promotes it (`test_store_staging_convention`), so a rewritten store arrives as a new
# directory and can never be served from a stale entry. The one store read WHILE it is
# being written — a running segmentation's `.partial` staging store (`live=1`) — is never cached.
#
# Single-flight: concurrent misses on one chunk wait on ONE decode instead of each decoding it — the
# viewer asks for a timepoint's bricks 16 at a time, and they all miss the same chunks at once.
#
# OFF until `chunk_cache_budget!` gives it bytes. The server does that in `start` from the user's
# setting (`Cecelia.viewer_chunk_cache_bytes`); scripts and tests that include this file decode
# every read unless they turn it on, so a benchmark measures the decode, not the cache.

mutable struct _CachedChunk
    data::Array
    bytes::Int
    tick::Int
end

const _CC_LOCK   = ReentrantLock()
const _CC        = Dict{Any,_CachedChunk}()
const _CC_FLIGHT = Dict{Any,Base.Event}()
const _CC_BUDGET = Ref(0)                    # bytes; 0 = off
const _CC_BYTES  = Ref(0)
const _CC_TICK   = Ref(0)
const _CC_HITS   = Ref(0)
const _CC_MISSES = Ref(0)
const _CC_WAITS  = Ref(0)
const _CC_EVICTS = Ref(0)

"""Set the cache's byte budget (0 turns it off and empties it). Evicts down to the new budget."""
function chunk_cache_budget!(bytes::Integer)
    lock(_CC_LOCK) do
        _CC_BUDGET[] = max(0, Int(bytes))
        _CC_BUDGET[] == 0 ? (empty!(_CC); _CC_BYTES[] = 0) : _cc_evict_locked!()
    end
    _CC_BUDGET[]
end

"""Counters for `/api/diagnostics`: what the cache holds and how it has been doing since start."""
chunk_cache_stats() = lock(_CC_LOCK) do
    (; budgetBytes = _CC_BUDGET[], bytes = _CC_BYTES[], chunks = length(_CC), hits = _CC_HITS[],
       misses = _CC_MISSES[], waits = _CC_WAITS[], evictions = _CC_EVICTS[])
end

"""
    cached_read(arr, idx...) -> Array

`arr[idx...]` for a Zarr.jl array, assembled from cached decoded chunks. Same result as `arr[idx...]`
— shape, dropped integer dims, BoundsError — and falls back to exactly that for anything it does not
cache: cache off, a non-directory store, a `.partial` store, an index that is not an Int, a unit
range or a Colon, or a read that covers every chunk it touches in full. Byte order is NOT applied
here; `read_native` does that on the result.
"""
function cached_read(arr, idx...)
    _CC_BUDGET[] > 0 || return arr[idx...]
    N = ndims(arr)
    length(idx) == N || return arr[idx...]
    all(i -> i isa Integer, idx) && return arr[idx...]
    ranges = ntuple(d -> _cc_range(idx[d], size(arr, d)), N)
    # anything out of bounds or empty goes to Zarr.jl, so it raises (or returns) exactly what it would
    any(r -> r === nothing || isempty(r) || first(r) < 1, ranges) && return arr[idx...]
    any(d -> last(ranges[d]) > size(arr, d), 1:N) && return arr[idx...]
    ident = _cc_identity(arr)
    ident === nothing && return arr[idx...]

    cs  = Tuple(arr.metadata.chunks)
    # A read that uses EVERY chunk it touches in full — a whole volume, a 2D plane, a chunk-aligned
    # tile — has no amplification to save: caching it only adds a copy. Measured on 32 whole (t, c)
    # volumes: 1.4–2.7x slower cold and no faster on a revisit. Only partial-chunk reads (bricks) pay.
    covers(d) = (first(ranges[d]) - 1) % cs[d] == 0 &&
                (last(ranges[d]) % cs[d] == 0 || last(ranges[d]) == size(arr, d))
    all(covers, 1:N) && return arr[idx...]
    out = Array{eltype(arr)}(undef, map(length, ranges))
    cir = ntuple(d -> ((first(ranges[d]) - 1) ÷ cs[d]):((last(ranges[d]) - 1) ÷ cs[d]), N)
    place!(ci, chunk) = begin
        g0  = ntuple(d -> ci[d] * cs[d] + 1, N)                       # chunk's first global index
        lo  = ntuple(d -> max(first(ranges[d]), g0[d]), N)
        hi  = ntuple(d -> min(last(ranges[d]), g0[d] + size(chunk, d) - 1), N)
        src = ntuple(d -> (lo[d] - g0[d] + 1):(hi[d] - g0[d] + 1), N)
        dst = ntuple(d -> (lo[d] - first(ranges[d]) + 1):(hi[d] - first(ranges[d]) + 1), N)
        @views out[dst...] .= chunk[src...]
    end
    # Claim-first, from a random start: copy what is cached, DECODE what nobody else is decoding, and
    # only then wait for the chunks another read already claimed. Concurrent bricks of one timepoint
    # need mostly the same chunks; walking them in the same order would leave every brick but one
    # waiting while that one decodes them in turn. Measured on a 512²-chunked store: same-order
    # single-flight made a cold scrub SLOWER than no cache at all.
    cis = vec(collect(Iterators.product(cir...)))
    cis = circshift(cis, -rand(0:length(cis) - 1))
    deferred = eltype(cis)[]
    for ci in cis
        key = (ident, ci)
        state, x = _cc_claim(key)
        if state === :hit
            place!(ci, x)
        elseif state === :mine
            place!(ci, _cc_decode!(arr, key, ci, cs, x))
        else
            push!(deferred, ci)
        end
    end
    for ci in deferred
        place!(ci, _cc_chunk(arr, ident, ci, cs))
    end
    # drop the integer-indexed dims, as `arr[idx...]` does
    keep = Tuple(d for d in 1:N if !(idx[d] isa Integer))
    length(keep) == N ? out : reshape(out, map(d -> size(out, d), keep))
end

_cc_range(i::Integer, n) = Int(i):Int(i)
_cc_range(::Colon, n)    = 1:n
_cc_range(r::AbstractUnitRange{<:Integer}, n) = Int(first(r)):Int(last(r))
_cc_range(_, n)          = nothing

# (directory, inode, mtime) of the level array's directory, or nothing when it must not be cached.
# A promoted store brings a NEW directory (new inode, later mtime), and a chunk file added in place
# would bump the mtime — either way the key changes and old entries are never matched again.
function _cc_identity(arr)
    (arr isa Zarr.ZArray && arr.storage isa Zarr.DirectoryStore) || return nothing
    dir = joinpath(arr.storage.folder, arr.path)
    # a store being filled right now (live=1). `Cecelia.STORE_STAGING_SUFFIX`, spelled out so this file
    # loads without the package (the single-flight test includes it alone)
    occursin(".partial", dir) && return nothing
    s = stat(dir)
    isdir(s) ? (dir, s.inode, s.mtime) : nothing
end

# One look under the lock: `(:hit, data)`, `(:mine, event)` — the caller now owns the decode and must
# `_cc_decode!` — or `(:theirs, event)`, another read is decoding it.
function _cc_claim(key)
    lock(_CC_LOCK) do
        e = get(_CC, key, nothing)
        if e !== nothing
            e.tick = (_CC_TICK[] += 1)
            _CC_HITS[] += 1
            return (:hit, e.data)
        end
        theirs = get(_CC_FLIGHT, key, nothing)
        theirs === nothing || (_CC_WAITS[] += 1; return (:theirs, theirs))
        mine = Base.Event()
        _CC_FLIGHT[key] = mine
        _CC_MISSES[] += 1
        (:mine, mine)
    end
end

# Decode one chunk we claimed, publish it (if it fits the budget) and release the waiters.
function _cc_decode!(arr, key, ci, cs, mine)
    data = try
        arr[ntuple(d -> (ci[d] * cs[d] + 1):min((ci[d] + 1) * cs[d], size(arr, d)), length(cs))...]
    catch
        lock(() -> delete!(_CC_FLIGHT, key), _CC_LOCK)
        notify(mine)
        rethrow()
    end
    lock(_CC_LOCK) do
        b = sizeof(data)
        if b <= _CC_BUDGET[]
            _CC[key] = _CachedChunk(data, b, (_CC_TICK[] += 1))
            _CC_BYTES[] += b
            _cc_evict_locked!()
        end
        delete!(_CC_FLIGHT, key)
    end
    notify(mine)
    data
end

# A chunk, blocking: a hit, our own decode, or — when another read holds it — wait and look again (a
# hit, or, if that decode failed or the chunk was too big to keep, decode it ourselves).
function _cc_chunk(arr, ident, ci, cs)
    key = (ident, ci)
    while true
        state, x = _cc_claim(key)
        state === :hit  && return x
        state === :mine && return _cc_decode!(arr, key, ci, cs, x)
        wait(x)
    end
end

# Over budget → drop least-recently-used entries down to 90% of it, so eviction runs in batches rather
# than on every insert. Caller holds `_CC_LOCK`.
function _cc_evict_locked!()
    _CC_BYTES[] <= _CC_BUDGET[] && return
    target = floor(Int, 0.9 * _CC_BUDGET[])
    for (k, e) in sort!(collect(_CC); by = kv -> kv[2].tick)
        _CC_BYTES[] <= target && break
        delete!(_CC, k)
        _CC_BYTES[] -= e.bytes
        _CC_EVICTS[] += 1
    end
end
