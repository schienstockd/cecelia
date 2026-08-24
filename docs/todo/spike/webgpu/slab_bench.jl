# P1 gate - what `read_slab` costs, and whether it beats the prototype's Python reader.
#
# WEB_VIEWER_PLAN.md P1 fails if "the Julia slab read is much slower than the 533 ms measured outside
# a request handler". That 533 ms is Julia's whole-timepoint read from the audit's G3; the Python
# prototype's warm per-timepoint read was ~330 ms across 4 channels (~82 ms/channel) once the page
# cache was hot. This measures the real function the route calls, per channel, on both stores - the
# flat one (VJy1Nx, the real target) and the nested one (fXgbTl, the format everything is written in
# from now on).
#
# Decomposed, because "slow" without a split is unattributable (a total says nothing about its
# components): zarr read+decode, then the permute-to-(x,y,z) and byte view the route adds on top.
#
# Run from the repo root:
#   pixi run julia --project=api docs/todo/spike/webgpu/slab_bench.jl

using Statistics, JSON3, Printf, HTTP, Zarr, PNGFiles, ColorTypes, FixedPointNumbers
using ChunkCodecLibZstd: ZstdEncodeOptions
using ChunkCodecCore: encode

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
# Reader half only - the two HTTP route functions at the bottom reach into the app's config/model
# layer, which a bench has no business loading. Same cut as julia_render_bench.jl.
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, cut === nothing ? src : src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))
# viewer_api.jl the same way: `read_slab` / `slab_bytes` / `_sampled_specs` are the reader half, the
# two route functions below them need HTTP.Stream and the config layer.
let src = read(joinpath(REPO, "api", "src", "viewer_api.jl"), String)
    cut = findfirst("# ── GET /api/viewer/meta", src)
    include_string(Main, cut === nothing ? src : src[1:first(cut) - 1], "viewer_api.jl")
end

const PROJ = expanduser("~/cecelia-feijoa/projects/zolIMa/0")
const VERSION = "ccidSmoothed.ome.zarr"

med(v) = sort(v)[max(1, cld(length(v), 2))]

function timeit(f, reps)
    f()                                     # compile + warm the page cache
    ts = Float64[]
    for _ in 1:reps
        t0 = time_ns(); f(); push!(ts, (time_ns() - t0) / 1e6)
    end
    (n = reps, ms_median = round(med(ts); digits = 1),
     ms_min = round(minimum(ts); digits = 1), ms_max = round(maximum(ts); digits = 1))
end

results = Dict{String,Any}()
for uid in ("VJy1Nx", "fXgbTl")
    zp = joinpath(PROJ, uid, VERSION)
    isdir(zp) || (@warn "skip $uid (absent)"; continue)

    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    dim(n) = haskey(d, n) ? size(arr, d[n]) : 1
    nx, ny, nz, nc, nt = dim("x"), dim("y"), dim("z"), dim("c"), dim("t")
    zmeta = JSON3.read(read(joinpath(zp, "0", ".zarray"), String))
    sep = String(get(zmeta, :dimension_separator, "."))

    # one channel of one timepoint - the exact unit the route serves
    slab   = timeit(() -> read_slab(zp, 5, 0), 5)
    # the permute + byte view on top of the read, so neither is assumed free
    vol, sx, sy, sz = read_slab(zp, 5, 0)
    bytes  = timeit(() -> slab_bytes(vol), 5)
    zst    = timeit(() -> encode(ZstdEncodeOptions(; compression_level = 1), slab_bytes(vol)), 3)
    zsize  = length(encode(ZstdEncodeOptions(; compression_level = 1), slab_bytes(vol)))
    nbytes = sx * sy * sz * sizeof(eltype(vol))
    specs  = timeit(() -> _sampled_specs(zp, nc), 3)

    results[uid] = Dict(
        "shape" => [nt, nc, nz, ny, nx], "separator" => sep,
        "slab_shape" => [sz, sy, sx], "slab_bytes" => nbytes,
        "eltype" => string(eltype(vol)),
        "read_slab_ms" => slab, "slab_bytes_ms" => bytes,
        "zstd_ms" => zst, "zstd_bytes" => zsize,
        "zstd_ratio" => round(nbytes / zsize; digits = 2),
        "sampled_contrast_ms" => specs,
        "timepoint_ms_est" => round(nc * slab.ms_median; digits = 1),
    )
    @printf("%s  %s sep  slab %d x %d x %d = %.1f MB\n", uid, sep, sz, sy, sx, nbytes / 1e6)
    @printf("   read_slab  %.1f ms (min %.1f max %.1f)   x%d ch = %.1f ms/timepoint\n",
            slab.ms_median, slab.ms_min, slab.ms_max, nc, nc * slab.ms_median)
    @printf("   bytes view %.1f ms   zstd-1 %.1f ms -> %.2fx   sampled contrast %.1f ms\n",
            bytes.ms_median, zst.ms_median, nbytes / zsize, specs.ms_median)
end

out = joinpath(@__DIR__, "p1_slab_bench.json")
write(out, JSON3.write(Dict("version" => 1, "results" => results)))
println("\nwrote ", out)
