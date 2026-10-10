# Do concurrent slab reads scale across threads once HTTP is out of the picture — and does
# `BLOSC_NOLOCK` change the answer?
#
# slab_cache_findings.md → Open questions 1 suspects two serialising layers: HTTP.jl running
# `try_serve_slab` on the single interactive thread, and behind it c-blosc 1.x's global decompress
# mutex (taken by plain `blosc_decompress` unless `BLOSC_NOLOCK` is set). This script removes layer 1
# (reads run on the default pool via `Threads.@spawn`) and measures layer 2 directly: run once as-is,
# once with BLOSC_NOLOCK=1. Not scaling as-is + scaling with NOLOCK ⇒ the mutex is the cap.
#
# Reads are the viewer's brick (128x128 x all z x all c, level 0) through the same `open_level` +
# `read_native` the route uses. The page cache is warmed first, so the disk is out of the answer.
#
# Run from the repo root (twice), passing the store (geometry is read from it):
#   S=~/cecelia-projects/e1Mn6X/0/8eapy6/ccidImage.ome.zarr
#   julia --project=api -t 16 docs/todo/spike/webgpu/slab_parallel_bench.jl $S
#   BLOSC_NOLOCK=1 julia --project=api -t 16 docs/todo/spike/webgpu/slab_parallel_bench.jl $S
# Writes slab_cache_results/parallel_{default,nolock}_<image>.json (<image> = the store's parent dir).

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

length(ARGS) == 1 || error("usage: slab_parallel_bench.jl <store.ome.zarr>")
const ZP = expanduser(ARGS[1])
const IMAGE = basename(dirname(rstrip(ZP, '/')))
const B = 128
arr, caxes = open_level(ZP, 0)
const NX, NY, NZ, NC, NT = size(arr)      # Julia dims are (x,y,z,c,t)
const NXB, NYB = NX ÷ B, NY ÷ B
println("store ", ZP, "  (x,y,z,c,t) = ", size(arr))

# 16 bricks, distinct timepoints and positions — same idea as slab_cache_bench.sh's list
const BRICKS = [(t = (10i) % NT, bx = (3i) % NXB, by = (5i) % NYB) for i in 0:15]
brick(b) = read_native(arr, b.bx*B+1:b.bx*B+B, b.by*B+1:b.by*B+B, 1:NZ, 1:NC, b.t + 1)

# Run all 16 bricks with at most n in flight; return wall ms for the whole batch.
function batch(n)
    ch = Channel{eltype(BRICKS)}(length(BRICKS)); foreach(b -> put!(ch, b), BRICKS); close(ch)
    t0 = time_ns()
    @sync for _ in 1:n
        Threads.@spawn for b in ch
            v = brick(b)
            size(v) == (B, B, NZ, NC) || error("bad brick shape $(size(v))")
        end
    end
    (time_ns() - t0) / 1e6
end

nolock = haskey(ENV, "BLOSC_NOLOCK")
@printf("julia threads = %d (+%d interactive), BLOSC_NOLOCK = %s, blosc threads = 1\n",
        Threads.nthreads(:default), Threads.nthreads(:interactive), nolock ? ENV["BLOSC_NOLOCK"] : "unset")
Blosc.set_num_threads(1)
batch(16); batch(1)                       # warm page cache + compile

rows = Any[]
base = 0.0
for n in (1, 2, 4, 8, 16)
    ms = median([batch(n) for _ in 1:3])
    n == 1 && (global base = ms)
    mbs = length(BRICKS) * B * B * NZ * NC * 2 / 1e6 / (ms / 1000)
    @printf("n=%-2d  16 bricks in %7.1f ms  (%.1f ms/brick, %6.1f MB/s, speedup %.2fx)\n",
            n, ms, ms / 16, mbs, base / ms)
    push!(rows, Dict("concurrency" => n, "batch_ms" => round(ms; digits = 1),
                     "mb_per_s" => round(mbs; digits = 1), "speedup" => round(base / ms; digits = 2)))
end

out = joinpath(@__DIR__, "slab_cache_results", "parallel_$(nolock ? "nolock" : "default")_$(IMAGE).json")
mkpath(dirname(out))
write(out, JSON3.write(Dict("version" => 1, "image" => IMAGE, "brick" => [B, B, NZ, NC],
                            "blosc_nolock" => nolock, "julia_threads" => Threads.nthreads(:default),
                            "scaling" => rows)))
println("wrote ", out)
