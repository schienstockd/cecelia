# `[viewer].chunkCache` — how much RAM the server's decoded-chunk cache may hold (`api/src/chunk_cache.jl`).
# The viewer's bricks decode whole zarr chunks and keep a sliver of each; the cache keeps the decoded
# chunks so the other bricks of the same timepoint are a copy. Design: docs/todo/SLAB_READ_PERF_PLAN.md
# (Decision 8).

"""Selectable budgets: `auto`, `off`, or a size in MB."""
const VIEWER_CACHE_CHOICES = ["auto", "off", "512", "1024", "2048", "4096", "8192", "16384", "32768"]
const VIEWER_CACHE_DEFAULT = "auto"

"""
    viewer_chunk_cache_auto_bytes() -> Int

`auto`: 1/16 of physical RAM, clamped to 256 MiB–4 GiB. Sized from the machine it runs on, not from the
workstation the numbers were measured on: a 16 GB laptop gets 1 GiB — four timepoints of a 1024² x 31z
x 4c raw store (248 MB each), or sixteen of a 512²-chunked one — which leaves the rest of its RAM to
the browser and the tasks. A big workstation is capped at 4 GiB by default and raises it in Settings.
"""
viewer_chunk_cache_auto_bytes() =
    clamp(floor(Int, Sys.total_memory() / 16), 256 * 2^20, 4 * 2^30)

"""The configured choice — one of `VIEWER_CACHE_CHOICES`; anything else reads as the default."""
function viewer_chunk_cache_setting()::String
    v = string(get(get(cecelia_conf(), "viewer", Dict{String,Any}()), "chunkCache", VIEWER_CACHE_DEFAULT))
    v in VIEWER_CACHE_CHOICES ? v : VIEWER_CACHE_DEFAULT
end

"""The budget in bytes for a choice (default: the configured one). `off` is 0."""
function viewer_chunk_cache_bytes(choice::AbstractString = viewer_chunk_cache_setting())::Int
    choice == "auto" && return viewer_chunk_cache_auto_bytes()
    choice == "off"  && return 0
    n = tryparse(Int, choice)
    n === nothing ? viewer_chunk_cache_auto_bytes() : n * 2^20
end

"""Persist the choice to `custom.toml` and hot-reload config. Throws on a value not in the choices."""
function set_viewer_chunk_cache!(choice::AbstractString)::String
    choice in VIEWER_CACHE_CHOICES || throw(ArgumentError("chunk cache must be one of $(VIEWER_CACHE_CHOICES)"))
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    v = get(cfg, "viewer", Dict{String,Any}())
    v["chunkCache"] = String(choice)
    cfg["viewer"] = v
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    String(choice)
end
