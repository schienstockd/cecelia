# Child process for the "c-blosc without its global lock" testset (suite/viewer_slab_and_meta.jl). Run
# with real threads (`-t 4`) — the API suite itself runs on one, where `Threads.@spawn` is cooperative
# and a race check cannot bite.
#
#   julia -t 4 --project=api blosc_parallel_child.jl <path/to/api/src/image_render.jl> <empty dir>
#
# Loads image_render.jl (its load runs `enable_blosc_nolock!`), writes a blosc/zstd store with one
# plane per chunk like a real import, and reads 16 bricks 16-way parallel for 5 rounds, comparing each
# brick's SHA-256 against a serial read. Prints one line the testset matches.
include(ARGS[1])
using Zarr, SHA, Random
blosc_nolock() || (print("nolock=false"); exit())
g = zgroup(Zarr.DirectoryStore(ARGS[2]))
a = zcreate(UInt16, g, "0", 256, 256, 8, 2, 4; chunks = (256, 256, 1, 1, 1),
            compressor = Zarr.BloscCompressor(cname = "zstd", clevel = 3, shuffle = 1))
a[:, :, :, :, :] = rand(MersenneTwister(1), UInt16(0):UInt16(4095), size(a))
bricks = [(i % 4 + 1, 64 * (i % 4) + 1, 64 * ((3i) % 4) + 1) for i in 0:15]
rd((t, x, y)) = sha256(reinterpret(UInt8, vec(a[x:x+63, y:y+63, :, :, t])))
ref = rd.(bricks)
bad = sum(_ -> count(fetch.([Threads.@spawn rd(b) for b in bricks]) .!= ref), 1:5)
print("nolock=true mismatches=", bad)
