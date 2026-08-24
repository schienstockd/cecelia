# Is `Blosc.set_num_threads(n > 1)` a real 2x, and is it SAFE?
#
# Zarr.jl calls `Blosc.decompress!` with no lock of its own. blosc1's own header says
# `blosc_decompress` uses the global context and that `blosc_decompress_ctx` is the form for
# multithreaded callers - so raising the thread count is either a 2x or a data race, and a stress test
# that finds no corruption does not distinguish "safe" from "latent".
#
# The scaling curve is what distinguishes them without reading the C. If concurrent decompresses cost
# N x one decompress, they are SERIALISING on a global lock (safe, and the concurrency is fake). If they
# cost roughly one, they are genuinely parallel (and then the absence of a lock is the worry).
#
# Run from the repo root:
#   pixi run julia --project=api -t auto docs/todo/spike/webgpu/blosc_threads.jl

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

const ZP = expanduser("~/cecelia-feijoa/projects/zolIMa/0/Dml3RG/ccidSmoothed.ome.zarr")
arr, caxes = open_level0(ZP)
d = axis_dims(caxes, ndims(arr))
nc = size(arr, d["c"])

med(v) = sort(v)[cld(length(v), 2)]
# One channel read per task. Same (t, c) for every task on purpose: the question is the DECOMPRESS
# path's concurrency, and reusing the bytes takes the disk out of the answer entirely.
function conc(n, reps)
    f() = fetch.([Threads.@spawn read_native(arr, :, :, :, 1, 41) for _ in 1:n])
    f()
    ms = Float64[]
    for _ in 1:reps; t0 = time_ns(); f(); push!(ms, (time_ns() - t0) / 1e6); end
    round(med(ms); digits = 1)
end

@printf("julia threads = %d\n\n", Threads.nthreads())
out = Dict{String,Any}()
for nthr in (1, 4, 8)
    Blosc.set_num_threads(nthr)
    one = conc(1, 5)
    row = Any[]
    @printf("blosc=%-2d  ", nthr)
    for n in (1, 2, 4, 8)
        ms = conc(n, 4)
        # 1.0 => perfectly parallel; n => fully serialised on a global lock
        @printf("n=%d %6.1f ms (%.2fx) ", n, ms, ms / one)
        push!(row, Dict("concurrency" => n, "ms" => ms, "vs_single" => round(ms / one; digits = 2)))
    end
    println()
    out["blosc_$nthr"] = row
end
Blosc.set_num_threads(1)

write(joinpath(@__DIR__, "p2_blosc_threads.json"),
      JSON3.write(Dict("version" => 1, "image" => "Dml3RG", "channel_mb" => 81.5,
                       "julia_threads" => Threads.nthreads(), "scaling" => out)))
println("\nwrote ", joinpath(@__DIR__, "p2_blosc_threads.json"))
