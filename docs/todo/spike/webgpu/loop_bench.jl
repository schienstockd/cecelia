# What a FORWARD LOOP through Dml3RG actually costs per timepoint, server-side.
#
# The first stride bench measured one timepoint repeatedly, which is the wrong shape for this question
# twice over: it read the same bytes every rep (so the page cache answered, and 59 GB of a real loop
# cannot be cached) and it read channels serially (the server runs `-t auto`, and the browser asks for
# all four at once). Its tail was also non-monotonic - (3,2,2) beat (2,4,4) - which is the tell that
# reps were too few to trust. So: distinct timepoints walked FORWARD, channels in parallel, and a
# DIFFERENT t-range per stride so no config is served bytes a previous one warmed.
#
# Run from the repo root:
#   pixi run julia --project=api -t auto docs/todo/spike/webgpu/loop_bench.jl

using Statistics, JSON3, Printf, HTTP, Zarr, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

const ZP = expanduser("~/cecelia-feijoa/projects/zolIMa/0/Dml3RG/ccidSmoothed.ome.zarr")

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
@printf("threads = %d   (the server runs -t auto)\n\n", Threads.nthreads())

# All channels of one timepoint, in parallel, as the route does under -t auto.
function timepoint(t, sz, sy, sx)
    tasks = [Threads.@spawn read_strided(arr, d, t, c, sz, sy, sx) for c in 0:(nc - 1)]
    blks = fetch.(tasks)
    sum(length, blks) * 2
end

const REPS = 10
timepoint(0, 2, 2, 2)                                    # compile

results = Any[]
@printf("%-12s %12s %12s %12s %14s\n", "stride zyx", "ms/tp med", "ms/tp p90", "MB/tp", "181-t loop")
# a different t-range per config, so nothing is handed bytes a previous config warmed
for (i, (sz, sy, sx)) in enumerate(((1,1,1), (2,1,1), (1,2,2), (2,2,2), (2,4,4), (4,4,4)))
    base = 10 + (i - 1) * REPS * 2                        # spread across the movie, no overlap
    ms = Float64[]; bytes = 0
    for k in 0:(REPS - 1)
        t = base + k
        t0 = time_ns()
        bytes = timepoint(t, sz, sy, sx)
        push!(ms, (time_ns() - t0) / 1e6)
    end
    s = sort(ms)
    med = s[cld(length(s), 2)]
    p90 = s[clamp(ceil(Int, 0.9 * length(s)), 1, length(s))]
    @printf("%-12s %12.1f %12.1f %12.1f %12.0f s\n",
            "$sz,$sy,$sx", med, p90, bytes / 1e6, med * nt / 1000)
    push!(results, Dict("stride" => [sz, sy, sx], "ms_median" => round(med; digits = 1),
                        "ms_p90" => round(p90; digits = 1), "mb_per_timepoint" => round(bytes/1e6; digits = 1),
                        "loop_s" => round(med * nt / 1000; digits = 0),
                        "t_range" => [base, base + REPS - 1]))
end

out = joinpath(@__DIR__, "p2_loop_bench.json")
write(out, JSON3.write(Dict("version" => 1, "image" => "Dml3RG", "reps" => REPS,
    "threads" => Threads.nthreads(),
    "geometry" => Dict("nx" => nx, "ny" => ny, "nz" => nz, "nc" => nc, "nt" => nt),
    "results" => results)))
println("\nwrote ", out)
