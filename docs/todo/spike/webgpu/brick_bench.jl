# P0 gate for KILN_BRICK_PLAN — what a brick-shaped slab COSTS over HTTP on the shipping route.
#
# The brick port streams sub-volumes out of `/api/viewer/slab` at (t, c, z, zTo, x, xTo, y, yTo,
# level). Kiln's atlas expects all channels for a brick stacked along Z in one texture, but the
# slab route takes a SINGLE `c`, so one visible brick is `nC` separate requests. On SispLk (nC=38)
# that ratio is not free — the audit already found per-chunk HTTP was 4.2x slower than per-slab.
#
# This bench measures both:
#   1) Per-brick fetch, one channel: what a single Kiln-style upload costs.
#   2) Per-brick fetch, ALL channels of one brick (nC serial requests): what a viewport-visible
#      brick actually costs today, so we can decide whether the route needs a `cTo` param before P3.
#
# Grid: brick sizes 64/128/256, pyramid levels 0/1/2. On SispLk (7848x7293x4, uint8, 6 levels) and
# 35uedD (12977x6543x4, uint8, 6 levels).
#
# Everything reported in ms (median across N reps). Warm reads only — the first request per level
# is dropped so the OS page cache is populated.
#
# Assumes `pixi run dev` is running on :8080. Never starts a server (memory:
# feedback_never_start_servers).
#
# Run from the repo root:
#   pixi run julia --project=api docs/todo/spike/webgpu/brick_bench.jl

using Statistics, JSON3, Printf, HTTP

const BASE = "http://localhost:8080"
const PROJECT = "4rNbMp"
const IMAGES = [
    (uid = "SispLk", name = "Human_Lymph_Node_Manual_IBEX"),
    (uid = "35uedD", name = "Human_Spleen_Manual_IBEX"),
]
const BRICK_SIZES = [64, 128, 256]
const LEVELS = [0, 1, 2]
const REPS = 5
const OUT_PATH = normpath(joinpath(@__DIR__, "p0_brick_bench.json"))

med(v) = isempty(v) ? NaN : sort(v)[max(1, cld(length(v), 2))]

"""Fetch one brick-shaped slab; return (ms, bytes)."""
function fetch_brick_ms(image_uid, level, t, c, xlo, xhi, ylo, yhi, zlo, zhi)
    url = "$BASE/api/viewer/slab?projectUid=$PROJECT&imageUid=$image_uid" *
          "&t=$t&c=$c&level=$level" *
          "&x=$xlo&xTo=$xhi&y=$ylo&yTo=$yhi&z=$zlo&zTo=$zhi"
    t0 = time_ns()
    r = HTTP.get(url; status_exception = false)
    dt = (time_ns() - t0) / 1e6
    r.status == 200 || error("HTTP $(r.status) for $url")
    (dt, length(r.body))
end

function image_meta(image_uid)
    r = HTTP.get("$BASE/api/viewer/meta?projectUid=$PROJECT&imageUid=$image_uid")
    JSON3.read(r.body, Dict{String,Any})
end

function level_dims(meta, level)
    # `levels` is per-store per-level {nX, nY, nZ, ...}. Use the top-level nX/nY/nZ for level 0
    # and downsample by 2^level for the deeper ones — matches the store's clean-2x pyramid
    # convention (see WEB_VIEWER_PLAN.md → Decision 2 for the 2D atlas which relies on the same
    # assumption).
    scale = 2 ^ level
    nx = max(1, div(Int(meta["nX"]), scale))
    ny = max(1, div(Int(meta["nY"]), scale))
    nz = Int(meta["nZ"])       # z is not downsampled in these stores (nZ=4, single z-slab)
    (nx, ny, nz)
end

function bench_one_channel(image_uid, level, brick, meta)
    (nx, ny, nz) = level_dims(meta, level)
    # Centre-of-image brick — avoids the edge-clamped case that reports smaller reads.
    xlo = clamp(div(nx, 2) - div(brick, 2), 0, nx - brick)
    ylo = clamp(div(ny, 2) - div(brick, 2), 0, ny - brick)
    xhi = xlo + brick - 1
    yhi = ylo + brick - 1
    zlo = 0; zhi = nz - 1
    ts = Float64[]; bytes = 0
    # warmup then N
    fetch_brick_ms(image_uid, level, 0, 0, xlo, xhi, ylo, yhi, zlo, zhi)
    for _ in 1:REPS
        (dt, nb) = fetch_brick_ms(image_uid, level, 0, 0, xlo, xhi, ylo, yhi, zlo, zhi)
        push!(ts, dt); bytes = nb
    end
    (ms_median = round(med(ts); digits = 1),
     ms_min    = round(minimum(ts); digits = 1),
     ms_max    = round(maximum(ts); digits = 1),
     bytes     = bytes)
end

function bench_all_channels(image_uid, level, brick, meta)
    nc = Int(meta["nC"])
    (nx, ny, nz) = level_dims(meta, level)
    xlo = clamp(div(nx, 2) - div(brick, 2), 0, nx - brick)
    ylo = clamp(div(ny, 2) - div(brick, 2), 0, ny - brick)
    xhi = xlo + brick - 1
    yhi = ylo + brick - 1
    zlo = 0; zhi = nz - 1
    # warmup
    for c in 0:min(3, nc - 1)
        fetch_brick_ms(image_uid, level, 0, c, xlo, xhi, ylo, yhi, zlo, zhi)
    end
    ts = Float64[]; total_bytes = 0
    for _ in 1:REPS
        t0 = time_ns()
        nb = 0
        for c in 0:(nc - 1)
            (_, b) = fetch_brick_ms(image_uid, level, 0, c, xlo, xhi, ylo, yhi, zlo, zhi)
            nb += b
        end
        push!(ts, (time_ns() - t0) / 1e6)
        total_bytes = nb
    end
    (ms_median = round(med(ts); digits = 1),
     ms_min    = round(minimum(ts); digits = 1),
     ms_max    = round(maximum(ts); digits = 1),
     bytes     = total_bytes,
     nc        = nc)
end

results = Dict{String,Any}(
    "notes" => "Server-side HTTP bench for KILN_BRICK_PLAN P0. Warm reads only.",
    "generated_via" => "docs/todo/spike/webgpu/brick_bench.jl",
    "endpoint" => "/api/viewer/slab",
    "project" => PROJECT,
    "reps" => REPS,
    "images" => Dict{String,Any}(),
)

for img in IMAGES
    println("\n== ", img.uid, " (", img.name, ") ==")
    meta = image_meta(img.uid)
    rows = Dict{String,Any}(
        "shape"        => Dict("nT" => meta["nT"], "nC" => meta["nC"], "nZ" => meta["nZ"],
                               "nY" => meta["nY"], "nX" => meta["nX"],
                               "bytesPerVoxel" => meta["bytesPerVoxel"]),
        "one_channel"  => Dict{String,Any}(),
        "all_channels" => Dict{String,Any}(),
    )
    for level in LEVELS
        (nx, ny, nz) = level_dims(meta, level)
        for brick in BRICK_SIZES
            brick > nx || brick > ny && continue
            key = "L$(level)_B$(brick)"
            r1 = bench_one_channel(img.uid, level, brick, meta)
            r2 = bench_all_channels(img.uid, level, brick, meta)
            rows["one_channel"][key]  = r1
            rows["all_channels"][key] = r2
            @printf("  L%d B%d  1ch: %6.1f ms  %8d B    %dch: %6.1f ms  %8d B  (nC=%d)\n",
                    level, brick, r1.ms_median, r1.bytes,
                    r2.nc, r2.ms_median, r2.bytes, r2.nc)
        end
    end
    results["images"][img.uid] = rows
end

open(OUT_PATH, "w") do io
    JSON3.pretty(io, results)
end
println("\nWrote ", OUT_PATH)
