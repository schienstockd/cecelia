# Child process for the "decoded-chunk cache: single-flight" testset (suite/viewer_slab_and_meta.jl).
# Run with real threads (`-t 4`) — the API suite runs on one, where concurrent misses cannot happen.
#
#   julia -t 4 --project=api chunk_cache_child.jl <path/to/api/src/image_render.jl> <empty dir>
#
# 32 tasks read the SAME region of one timepoint at once with the cache on — 2:255, so it touches all
# 48 chunks but none in full (a read covering whole chunks bypasses the cache). Single-flight means each chunk is
# decoded once (misses == chunks in the region) and every task gets the bytes an uncached read gives.
include(ARGS[1])          # image_render.jl — brings chunk_cache.jl with it
using Zarr, Random
g = zgroup(Zarr.DirectoryStore(ARGS[2]))
a = zcreate(UInt16, g, "0", 256, 256, 6, 2, 3; chunks = (128, 128, 1, 1, 1),
            compressor = Zarr.BloscCompressor(cname = "zstd", clevel = 3, shuffle = 1))
a[:, :, :, :, :] = rand(MersenneTwister(1), UInt16(0):UInt16(4095), size(a))
ref = a[2:255, 2:255, :, :, 2]
chunk_cache_budget!(256 * 2^20)
got = fetch.([Threads.@spawn cached_read(a, 2:255, 2:255, :, :, 2) for _ in 1:32])
s = chunk_cache_stats()
print("misses=", s.misses, " chunks=", 2 * 2 * 6 * 2, " mismatches=", count(x -> x != ref, got))
