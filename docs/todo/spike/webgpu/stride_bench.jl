# P2 follow-up - why looping through a whole timecourse takes minutes, and what a stride buys.
#
# Dml3RG is the case that hurts: 181 timepoints x 4 ch x 37 x 1039 x 1060 = 326 MB per timepoint,
# 59 GB in total. No VRAM budget caches that, so a full loop pays the cold cost 181 times over and the
# only levers are (a) read less, (b) send less, (c) overlap the stages.
#
# The lever worth measuring is the STRIDE, because the two axes behave completely differently and
# guessing which one helps is how this gets attributed wrong:
#
#   z-stride  cuts the READ. z chunks are size 1, so every Nth plane is literally 1/N of the chunk
#             files touched. This is what image_render.jl's `z_keep` already exploits for previews.
#   xy-stride cuts the WIRE and the GPU UPLOAD but NOT the read - a strided xy read still has to
#             decompress the whole 512x512 chunk it lands in.
#
# So the prediction is that (1,2,2) is nearly free on the server and 4x smaller to move, while (2,1,1)
# halves the server and moves the same bytes. Measured rather than asserted.
#
# Run from the repo root:
#   pixi run julia --project=api docs/todo/spike/webgpu/stride_bench.jl

using Statistics, JSON3, Printf, HTTP, Zarr, PNGFiles, ColorTypes, FixedPointNumbers
using ChunkCodecLibZstd: ZstdEncodeOptions
using ChunkCodecCore: encode

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

const ZP = expanduser("~/cecelia-feijoa/projects/zolIMa/0/Dml3RG/ccidSmoothed.ome.zarr")

med(v) = sort(v)[max(1, cld(length(v), 2))]
function timeit(f, reps)
    f()
    ts = Float64[]
    for _ in 1:reps
        t0 = time_ns(); f(); push!(ts, (time_ns() - t0) / 1e6)
    end
    round(med(ts); digits = 1)
end

# One (t, c) volume at a stride, mirroring `read_slab` but with steps on z/y/x.
function read_strided(arr, dims, t, c, sz, sy, sx)
    nd = ndims(arr)
    idx = Any[Colon() for _ in 1:nd]
    haskey(dims, "t") && (idx[dims["t"]] = t + 1)
    haskey(dims, "c") && (idx[dims["c"]] = c + 1)
    haskey(dims, "z") && (idx[dims["z"]] = 1:sz:size(arr, dims["z"]))
    idx[dims["y"]] = 1:sy:size(arr, dims["y"])
    idx[dims["x"]] = 1:sx:size(arr, dims["x"])
    read_native(arr, idx...)
end

arr, caxes = open_level0(ZP)
d = axis_dims(caxes, ndims(arr))
dim(n) = haskey(d, n) ? size(arr, d[n]) : 1
nx, ny, nz, nc, nt = dim("x"), dim("y"), dim("z"), dim("c"), dim("t")
@printf("Dml3RG  %d x %d x %d, %d ch, %d t   %.0f MB/timepoint   %.1f GB total\n",
        nx, ny, nz, nc, nt, nx*ny*nz*nc*2/1e6, nx*ny*nz*nc*nt*2/1e9)
println()

results = Any[]
@printf("%-12s %10s %10s %10s %10s %10s\n",
        "stride zyx", "read ms", "MB", "zstd ms", "wire MB", "loop est")
for (sz, sy, sx) in ((1,1,1), (2,1,1), (1,2,2), (2,2,2), (3,2,2), (2,4,4))
    # a different t each time would confound the page cache; hold t and vary only the stride
    ms = timeit(() -> read_strided(arr, d, 5, 0, sz, sy, sx), 3)
    blk = read_strided(arr, d, 5, 0, sz, sy, sx)
    mb  = length(blk) * 2 / 1e6
    zms = timeit(() -> encode(ZstdEncodeOptions(; compression_level = 1),
                              reinterpret(UInt8, vec(blk))), 2)
    zmb = length(encode(ZstdEncodeOptions(; compression_level = 1),
                        reinterpret(UInt8, vec(blk)))) / 1e6
    # a full loop, all channels, read only (upload is the client's half and measured separately)
    loop_s = nc * ms * nt / 1000
    @printf("%-12s %10.1f %10.1f %10.1f %10.2f %8.0f s\n",
            "$sz,$sy,$sx", ms, mb, zms, zmb, loop_s)
    push!(results, Dict("stride" => [sz, sy, sx], "read_ms" => ms, "mb" => round(mb; digits = 1),
                        "zstd_ms" => zms, "wire_mb" => round(zmb; digits = 2),
                        "loop_read_s" => round(loop_s; digits = 0),
                        "shape" => collect(size(blk))))
end

out = joinpath(@__DIR__, "p2_stride_bench.json")
write(out, JSON3.write(Dict("version" => 1, "image" => "Dml3RG",
    "geometry" => Dict("nx" => nx, "ny" => ny, "nz" => nz, "nc" => nc, "nt" => nt),
    "results" => results)))
println("\nwrote ", out)
