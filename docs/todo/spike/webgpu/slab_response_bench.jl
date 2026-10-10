# Where does a slab response spend its time once the read is cheap? Serves the slab route's read
# (`read_native` through the decoded-chunk cache, on a pool thread) and its response in an ISOLATED
# process on an ephemeral port — the app server is never touched — with the response written each of
# the ways Phase 5 compared (SLAB_READ_PERF_PLAN.md). Drive it with `slab_response_bench.sh`.
#
#   julia --project=api -t auto docs/todo/spike/webgpu/slab_response_bench.jl <store.ome.zarr> [budget MB]
#
# Prints PORT=<n>. Query: /s?t=&bx=&by=&m=<mode>, bricks are 128² x all z x all c at level 0.
#   m=view    `reinterpret` body, written on the connection task (the route before Phase 5)
#   m=chunked same, no Content-Length (chunked framing)
#   m=vec     `Vector` body over the volume's memory, connection task
#   m=pool    `Vector` body, written from the pool task that read it (the route after Phase 5)
#   m=flatview / m=flatpool   whole (t, c) volume, bx = channel (bypasses the cache)
# /s?gc=1 → cumulative GC time, read time and cache counters.

using Statistics, Printf, JSON3, Blosc, Zarr, HTTP, PNGFiles, ColorTypes, FixedPointNumbers

const REPO = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))
let src = read(joinpath(REPO, "api", "src", "image_geometry.jl"), String)
    include_string(Main, src[1:first(findfirst("function api_image_stores", src)) - 1], "image_geometry.jl")
end
include(joinpath(REPO, "api", "src", "image_render.jl"))
enable_blosc_nolock!()
Blosc.set_num_threads(1)

const ZP = String(expanduser(rstrip(ARGS[1], '/')))
arr, _ = open_level(ZP, 0)
nx, ny, nz, nc, nt = size(arr)
chunk_cache_budget!((length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 2048) * 2^20)
const B = 128
const READ_NS = Threads.Atomic{Int}(0)

wrap(v) = unsafe_wrap(Array, Ptr{UInt8}(pointer(v)), sizeof(v))   # caller preserves `v`

function respond(stream, body; fixed = true)
    HTTP.setheader(stream, "Content-Type" => "application/octet-stream")
    fixed && HTTP.setheader(stream, "Content-Length" => string(length(body)))
    HTTP.setstatus(stream, 200)
    HTTP.startwrite(stream)
    write(stream, body)
end

function brick(t, bx, by)
    s = time_ns()
    v = read_native(arr, bx*B+1:bx*B+B, by*B+1:by*B+B, 1:nz, 1:nc, t)
    Threads.atomic_add!(READ_NS, Int(time_ns() - s))
    v
end

function handler(stream::HTTP.Stream)
    q = HTTP.queryparams(HTTP.URI(stream.message.target))
    if haskey(q, "gc")
        g = Base.gc_num()
        HTTP.setstatus(stream, 200); HTTP.startwrite(stream)
        write(stream, "gc_ms=$(g.total_time / 1e6) full=$(g.full_sweep) read_ms=$(READ_NS[] / 1e6) cache=$(chunk_cache_stats())\n")
        return
    end
    m = get(q, "m", "pool")
    t = parse(Int, q["t"]); bx = parse(Int, q["bx"]); by = parse(Int, get(q, "by", "0"))
    if m == "flatview" || m == "flatpool"
        v = fetch(Threads.@spawn read_native(arr, :, :, :, bx + 1, t))
        m == "flatview" && return respond(stream, reinterpret(UInt8, vec(v)))
        return fetch(Threads.@spawn GC.@preserve v respond(stream, wrap(v)))
    end
    m == "pool" && return fetch(Threads.@spawn (v = brick(t, bx, by); GC.@preserve v respond(stream, wrap(v))))
    v = fetch(Threads.@spawn brick(t, bx, by))
    m == "vec"     && return GC.@preserve v respond(stream, wrap(v))
    m == "chunked" && return respond(stream, reinterpret(UInt8, vec(v)); fixed = false)
    respond(stream, reinterpret(UInt8, vec(v)))
end

srv = HTTP.listen!(handler, "127.0.0.1", 0)
println("PORT=", HTTP.port(srv), " nt=", nt, " threads=", Threads.nthreads(), "+", Threads.nthreads(:interactive))
flush(stdout)
wait(srv)
