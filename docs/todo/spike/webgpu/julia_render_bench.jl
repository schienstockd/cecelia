# G2, option C - what the EXISTING server-side Julia renderer costs on the real image.
#
# `api/src/image_render.jl` already composites a coloured z-MIP of one timepoint and returns PNG
# bytes; `/api/crop/frame` already serves it to the browser. So option C's starting point is not
# zero. This measures what it costs today, decomposed, at the audit's real dimensions - and at the
# full-resolution / full-z settings a 3D viewer would need rather than the crop panel's 512 px,
# 12-plane preview defaults.
#
# What this does NOT measure: a rotatable raycast. That does not exist yet. What it establishes is
# the per-frame budget of the code that DOES exist, so the gap to a camera can be costed honestly.
#
# Run from the repo root, with the api project's deps available:
#   pixi run julia --project=api docs/todo/spike/webgpu/julia_render_bench.jl

using Statistics, JSON3, Printf, HTTP, Zarr, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
# `resolve_image_version` reaches into the app's config/model layer; the two HTTP route functions at
# the bottom of image_geometry.jl need it. This bench only needs the READER half, so the file is
# included up to the routes rather than dragging the whole app in.
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, cut === nothing ? src : src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

const ZP = expanduser("~/cecelia-feijoa/projects/zolIMa/0/VJy1Nx/ccidSmoothed.ome.zarr")
const PROPS = ""   # no viewer props -> percentile contrast fallback, same as a cold image

med(v) = sort(v)[max(1, cld(length(v), 2))]

function timeit(f, reps)
    f()                                    # compile
    ts = Float64[]
    for i in 1:reps
        t0 = time_ns()
        f()
        push!(ts, (time_ns() - t0) / 1e6)
    end
    Dict("n" => reps, "ms_median" => round(med(ts); digits = 2),
         "ms_min" => round(minimum(ts); digits = 2), "ms_max" => round(maximum(ts); digits = 2))
end

R = Dict{String,Any}("zarr" => ZP)
arr, caxes = open_level0(ZP)
R["shape"] = collect(size(arr))
R["axes"] = caxes

# NOTE ON `z_keep`: it is NOT "number of planes to keep". The stride is `cld(hi-lo+1, z_keep)`, so
# z_keep=1 reads ONE plane and z_keep>=nz reads ALL of them. Getting this backwards first time round
# produced a table where "full z" was 2x faster than "subsampled z" - the tell that the knob was
# inverted, not that the renderer was strange.
const NZ = 38

# 1. one z-plane, 512 px: the floor - almost no reading, just composite + PNG
R["z1_512px"] = timeit(() -> render_preview_frame(ZP, PROPS, 0; max_px = 512, z_keep = 1), 5)

# 2. the crop panel's shipped default (z_keep=12 -> stride 4 -> 10 planes), 512 px
R["shipped_10z_512px"] = timeit(() -> render_preview_frame(ZP, PROPS, 0; max_px = 512, z_keep = 12), 5)

# 3. ALL 38 planes, 512 px: full data read, small output
R["full_z_512px"] = timeit(() -> render_preview_frame(ZP, PROPS, 0; max_px = 512, z_keep = NZ), 3)

# 4. what a real viewer needs: all planes, full resolution
R["full_z_full_res"] = timeit(() -> render_preview_frame(ZP, PROPS, 0; max_px = 4096, z_keep = NZ), 3)

# 5. all planes at napari's measured canvas size (G1: 1566x1003), the like-for-like comparison
R["full_z_1566px"] = timeit(() -> render_preview_frame(ZP, PROPS, 0; max_px = 1566, z_keep = NZ), 3)

# 6. THE NUMBER THAT ACTUALLY MATTERS FOR OPTION C. Everything above re-reads the volume every
# frame, which no real viewer would do - a rotate keeps the timepoint resident. So: read once, then
# time only the per-frame half (project + composite + encode). This is C's warm frame cost, and the
# read above becomes its per-TIMEPOINT cost instead.
let
    arr2, caxes2 = open_level0(ZP)
    nd = ndims(arr2)
    dims = axis_dims(caxes_or_fallback(caxes2, nd), nd)
    idx = Any[Colon() for _ in 1:nd]
    idx[dims["t"]] = 1
    t0 = time_ns()
    sub = read_native(arr2, idx...)
    R["read_one_timepoint"] = Dict("ms" => round((time_ns() - t0) / 1e6; digits = 2))

    names = [caxes_or_fallback(caxes2, nd)[nd - j + 1] for j in 1:nd]
    rem_names = String[names[j] for j in 1:nd if j != dims["t"]]
    kz = findfirst(==("z"), rem_names)
    R["mip_over_z"] = timeit(() -> dropdims(maximum(sub; dims = kz); dims = kz), 5)

    m = dropdims(maximum(sub; dims = kz); dims = kz)
    mnames = [rem_names[k] for k in eachindex(rem_names) if k != kz]
    kc = findfirst(==("c"), mnames); ky = findfirst(==("y"), mnames); kx = findfirst(==("x"), mnames)
    chw = permutedims(m, (kc, ky, kx))
    specs = [percentile_spec(view(chw, c, :, :), DEFAULT_CMAPS[mod1(c, 4)]) for c in 1:size(chw, 1)]
    R["composite_rgb_full_res"] = timeit(() -> composite_rgb(chw, specs), 5)
    img = composite_rgb(chw, specs)
    R["png_encode_full_res"] = timeit(() -> (io = IOBuffer(); PNGFiles.save(io, img); take!(io)), 5)
    R["frame_px"] = [size(img, 2), size(img, 1)]
end

println("RESULT " * JSON3.write(R))
open(joinpath(@__DIR__, "g2_julia_render.json"), "w") do io
    JSON3.pretty(io, R)
end
