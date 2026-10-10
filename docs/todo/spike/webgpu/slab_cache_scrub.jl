# What does the decoded-chunk cache (api/src/chunk_cache.jl) buy on a viewer-shaped scrub, with HTTP
# out of the picture? Reads EVERY brick of 4 timepoints (8x8 bricks of 128² x all z x all c, level 0)
# 16 at a time, through `read_native` exactly as the slab route does — once with the cache off (the
# Phase 1 state: NOLOCK, threads, no cache) and once on, then repeats the on-pass (a revisit: all hits).
#
# Run from the repo root:
#   julia --project=api -t 16 docs/todo/spike/webgpu/slab_cache_scrub.jl <store.ome.zarr> [budget MB]
# Writes slab_cache_results/scrub_<image>_<store>.json.

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    include_string(Main, src[1:first(findfirst("function api_image_stores", src)) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))
Blosc.set_num_threads(1)

const ZP     = String(expanduser(rstrip(ARGS[1], '/')))
const BUDGET = (length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 2048) * 2^20
const B = 128
arr, _ = open_level(ZP, 0)
nx, ny, nz, nc, nt = size(arr)
const TS = [1 + (k * (nt - 1)) ÷ 3 for k in 0:3]
bricks = [(t, bx, by) for t in TS for by in 0:(ny ÷ B - 1) for bx in 0:(nx ÷ B - 1)]

function scrub()
    sem = Base.Semaphore(16)
    lat = zeros(length(bricks))
    g0 = Base.gc_num()
    t0 = time_ns()
    @sync for (i, (t, bx, by)) in enumerate(bricks)
        Threads.@spawn Base.acquire(sem) do
            s = time_ns()
            read_native(arr, bx*B+1:bx*B+B, by*B+1:by*B+B, 1:nz, 1:nc, t)
            lat[i] = (time_ns() - s) / 1e6
        end
    end
    (wall = (time_ns() - t0) / 1e9, median = median(lat), p95 = quantile(lat, 0.95),
     gc_ms = (Base.gc_num().total_time - g0.total_time) / 1e6)
end

read_native(arr, 1:B, 1:B, 1:nz, 1:nc, TS[1])           # compile + warm the page cache
foreach(t -> read_native(arr, :, :, :, :, t), TS)
# compile the CACHED path too, on a timepoint outside the scrub, then empty the cache — otherwise the
# first cold bricks pay JIT time (~1 s) and it lands in the cold numbers
chunk_cache_budget!(BUDGET)
fetch.([Threads.@spawn read_native(arr, 1:B, 1:B, 1:nz, 1:nc, 2) for _ in 1:4])
chunk_cache_budget!(0)
rows = Dict{String,Any}()
for (label, budget) in (("off", 0), ("on_cold", BUDGET), ("on_revisit", -1))
    budget >= 0 && (chunk_cache_budget!(0); chunk_cache_budget!(budget))
    r = scrub()
    st = chunk_cache_stats()
    @printf("%-10s %d bricks: wall %6.2f s | per-brick median %7.1f ms p95 %7.1f ms | GC %6.0f ms | decodes %d, waits %d, cache %.0f MB\n",
            label, length(bricks), r.wall, r.median, r.p95, r.gc_ms, st.misses, st.waits, st.bytes / 1e6)
    rows[label] = Dict("wall_s" => round(r.wall; digits = 2), "median_ms" => round(r.median; digits = 1),
                       "p95_ms" => round(r.p95; digits = 1), "gc_ms" => round(r.gc_ms; digits = 0), "decodes" => st.misses, "waits" => st.waits,
                       "cache_mb" => round(st.bytes / 1e6; digits = 0))
end
image, store = basename(dirname(ZP)), replace(basename(ZP), ".ome.zarr" => "")
out = joinpath(@__DIR__, "slab_cache_results", "scrub_$(image)_$(store).json")
write(out, JSON3.write(Dict("version" => 1, "image" => image, "store" => store, "threads" => Threads.nthreads(),
                            "budget_mb" => BUDGET ÷ 2^20, "timepoints" => TS, "bricks" => length(bricks), "runs" => rows)))
println("wrote ", out)
