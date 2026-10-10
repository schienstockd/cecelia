# Is the brick cost really "decode the whole chunk, keep a sliver" — and how does it change per level?
#
# For each pyramid level: time one whole chunk (the middle z plane, one channel) against a 128² sub-read of the
# same chunk, then a full viewer brick (128² x all z x all c). If the sub-read costs about the same as
# the whole chunk, the decode is the cost and the amplification is real. Reads go through the route's
# own `open_level` + `read_native`, single thread, page cache warmed first.
#
# Run from the repo root:
#   julia --project=api docs/todo/spike/webgpu/slab_amplification.jl <store.ome.zarr>
# Writes slab_cache_results/amplification_<image>_<store>.json.

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    cut = findfirst("function api_image_stores", src)
    include_string(Main, src[1:first(cut) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))

length(ARGS) == 1 || error("usage: slab_amplification.jl <store.ome.zarr>")
const ZP = String(expanduser(rstrip(ARGS[1], '/')))
const IMAGE = basename(dirname(ZP))
const STORE = replace(basename(ZP), ".ome.zarr" => "")
const B = 128
Blosc.set_num_threads(1)

ms(f) = (t0 = time_ns(); f(); (time_ns() - t0) / 1e6)
med(f, ts) = median([ms(() -> f(t)) for t in ts])

rows = Any[]
for level in 0:3
    arr = try
        first(open_level(ZP, level))
    catch e
        e isa KeyError || rethrow()                     # past the last level
        break
    end
    nx, ny, nz, nc, nt = size(arr)                     # Julia dims are (x,y,z,c,t)
    cx, cy, cz, cc, ct = arr.metadata.chunks
    bx, by = min(B, nx), min(B, ny)
    ts = [1 + (k * 17) % nt for k in 0:4]               # 5 distinct timepoints
    foreach(t -> read_native(arr, :, :, :, :, t), ts)  # warm page cache + compile
    chunk = med(t -> read_native(arr, 1:min(cx, nx), 1:min(cy, ny), cld(nz, 2), 1, t), ts)
    sub   = med(t -> read_native(arr, 1:bx, 1:by, cld(nz, 2), 1, t), ts)
    brick = med(t -> read_native(arr, 1:bx, 1:by, 1:nz, 1:nc, t), ts)
    # chunks one brick touches x bytes per chunk, over the brick's own bytes
    touched = cld(bx, cx) * cld(by, cy) * cld(nz, cz) * cld(nc, cc)
    decoded = touched * min(cx, nx) * min(cy, ny) * min(cz, nz) * min(cc, nc) * sizeof(eltype(arr))
    kept = bx * by * nz * nc * sizeof(eltype(arr))
    @printf("L%d  shape %s  chunks %s | chunk %6.2f ms  128² sub-read %6.2f ms (%.2fx of chunk) | brick %7.1f ms  decodes %6.1f MB for %5.2f MB (%.1fx)\n",
            level, size(arr), (cx, cy, cz, cc, ct), chunk, sub, sub / chunk, brick, decoded / 1e6, kept / 1e6, decoded / kept)
    push!(rows, Dict("level" => level, "shape_xyzct" => collect(size(arr)), "chunks_xyzct" => [cx, cy, cz, cc, ct],
                     "chunk_ms" => round(chunk; digits = 2), "sub128_ms" => round(sub; digits = 2),
                     "brick_ms" => round(brick; digits = 1), "brick_decoded_mb" => round(decoded / 1e6; digits = 1),
                     "brick_kept_mb" => round(kept / 1e6; digits = 2), "amplification" => round(decoded / kept; digits = 1)))
end

out = joinpath(@__DIR__, "slab_cache_results", "amplification_$(IMAGE)_$(STORE).json")
write(out, JSON3.write(Dict("version" => 1, "image" => IMAGE, "store" => STORE, "levels" => rows)))
println("wrote ", out)
