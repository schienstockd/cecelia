# Bytes from 16-way parallel BLOSC_NOLOCK reads must equal serial reads (repeated 5 rounds).
# Run from the repo root, passing the store:
#   BLOSC_NOLOCK=1 julia --project=api -t 16 docs/todo/spike/webgpu/slab_nolock_check.jl <store.ome.zarr>
using Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers, SHA
length(ARGS) == 1 || error("usage: slab_nolock_check.jl <store.ome.zarr>")
const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    include_string(Main, src[1:first(findfirst("function api_image_stores", src)) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))
arr, _ = open_level(expanduser(ARGS[1]), 0)
nx, ny, nz, nc, nt = size(arr)              # Julia dims are (x,y,z,c,t)
bricks = [((10i) % nt, (3i) % (nx ÷ 128), (5i) % (ny ÷ 128)) for i in 0:15]
rd((t, bx, by)) = sha256(reinterpret(UInt8, vec(read_native(arr, bx*128+1:bx*128+128, by*128+1:by*128+128, 1:nz, 1:nc, t+1))))
ref = [rd(b) for b in bricks]
bad = 0
for round in 1:5
    got = fetch.([Threads.@spawn rd(b) for b in bricks])
    global bad += count(got .!= ref)
end
println("NOLOCK=", get(ENV, "BLOSC_NOLOCK", "unset"), "  parallel-vs-serial mismatches over 5x16 bricks: ", bad)
