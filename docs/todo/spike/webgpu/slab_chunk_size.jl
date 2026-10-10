# What does chunk XY size cost and buy, on the SAME data? Copies the first 10 timepoints of a store
# into three stores chunked (1,1,1,N,N) for N = 1024, 512, 256, same codec (blosc/zstd 3, shuffle),
# and reports file count, apparent and on-disk size, a viewer brick read (128² x all z x all c) and a
# whole-plane read. Decides the import's chunk default (`bf2raw_chunk_flags`).
#
# Run from the repo root:
#   julia --project=api docs/todo/spike/webgpu/slab_chunk_size.jl <store.ome.zarr> <scratch dir>
# Writes slab_cache_results/chunk_size_<image>.json; the scratch copies are deleted afterwards.

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    include_string(Main, src[1:first(findfirst("function api_image_stores", src)) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))
Blosc.set_num_threads(1)

length(ARGS) == 2 || error("usage: slab_chunk_size.jl <store.ome.zarr> <scratch dir>")
const ZP = String(expanduser(rstrip(ARGS[1], '/')))
const IMAGE = basename(dirname(ZP))
const NT = 10

arr, _ = open_level(ZP, 0)
data = read_native(arr, :, :, :, :, 1:NT)            # (x,y,z,c,t)
nxb, nyb = size(data, 1) ÷ 128, size(data, 2) ÷ 128
rows = Any[]
for cs in (1024, 512, 256)
    d = joinpath(ARGS[2], "chunk$cs"); rm(d; recursive = true, force = true)
    a = zcreate(UInt16, zgroup(Zarr.DirectoryStore(d)), "0", size(data)...; chunks = (cs, cs, 1, 1, 1),
                compressor = Zarr.BloscCompressor(cname = "zstd", clevel = 3, shuffle = 1))
    a[:, :, :, :, :] = data
    files = sum(length(f) for (_, _, f) in walkdir(d))
    app   = sum(filesize(joinpath(r, x)) for (r, _, f) in walkdir(d) for x in f)
    disk  = parse(Int, split(read(`du -sk $d`, String))[1]) * 1024
    rd(t, bx, by) = a[bx*128+1:bx*128+128, by*128+1:by*128+128, :, :, t]
    foreach(t -> rd(t, 0, 0), 1:NT)                  # warm
    brick = median([(t0 = time_ns(); rd(t, (3t) % nxb, (5t) % nyb); (time_ns() - t0) / 1e6) for t in 1:NT, _ in 1:3])
    plane = median([(t0 = time_ns(); a[:, :, cld(size(a, 3), 2), 1, t]; (time_ns() - t0) / 1e6) for t in 1:NT])
    @printf("chunk %4d²: %6d files  apparent %6.1f MB  on disk %6.1f MB | brick %6.1f ms | plane %5.2f ms\n",
            cs, files, app / 1e6, disk / 1e6, brick, plane)
    push!(rows, Dict("chunk_xy" => cs, "files" => files, "apparent_mb" => round(app / 1e6; digits = 1),
                     "disk_mb" => round(disk / 1e6; digits = 1), "brick_ms" => round(brick; digits = 1),
                     "plane_ms" => round(plane; digits = 2)))
    rm(d; recursive = true, force = true)
end
out = joinpath(@__DIR__, "slab_cache_results", "chunk_size_$(IMAGE).json")
write(out, JSON3.write(Dict("version" => 1, "image" => IMAGE, "timepoints" => NT,
                            "codec" => "blosc/zstd clevel 3 shuffle", "sizes" => rows)))
println("wrote ", out)
