# Image store compression + layout + bioformats2raw CLI flag helpers. Split out of config.jl.
# Names live in the top-level `Cecelia` module; included from config.jl after the bootstrap.

# ── Image store compression ──────────────────────────────────────────────────────────────────────

"""
Compressor choices for an IMAGE store, mirroring `zarr_utils.IMAGE_COMPRESSOR_CHOICES`.

Julia needs its own copy for two reasons: bioformats2raw is handed flags on a command line (it cannot
read a Python constant), and the API serves the list to Settings. Kept in the SAME order as the Python
dict, and asserted equal by `app/test/suite.jl` → *"import metrics"* — same arrangement as the
calibration writers (docs/OBJECTMODEL.md → *Calibration — three copies, one stamp*). If you change one, that
test is the contract.

Each entry carries its MEASURED numbers as display strings (`size`/`write`/`read`), because Settings
renders them as a comparison table rather than a bare dropdown — the trade-off is the whole reason
there is a choice, so hiding it behind a name would make the control decorative. Serving them
pre-formatted keeps the numbers stated in exactly one place; the frontend never computes or restates
them.

Measured identically for every row: `4kS67f/LUkCpP` (64x4x13x512x512, 12-bit data in 16-bit words,
1.745 GB raw), whole store rewritten. `read` is a warm per-plane read, the only read difference that
survived three repeat runs — cold and sequential reads were inside run-to-run noise, because a smaller
store offsets the extra CPU. `url` is the codec's own site so a user can check what they're choosing.

**Held fixed while measuring:** the store LAYOUT — zarr v2, nested chunk keys (bioformats2raw's own
default, which is what wrote that store), 1x1x1x512x512 chunks, unsharded. It has to be stated because
the other table on that Settings page varies exactly those, and a size in one table is only comparable
to a size in the other if you know which layout each was taken in. Chunk shape is the part that could
actually move these numbers: the codec compresses one chunk at a time, so a different chunk size gives
it a different amount of context. The two variables are not independent, and neither table's rows were
re-measured across the other's — each varies one thing with the other pinned.
"""
const IMAGE_COMPRESSOR_CHOICES = [
    (name = "zstd-shuffle", cname = "zstd", clevel = 3, shuffle = true,
     label = "zstd + shuffle", size = "0.64 GB", ratio = "2.74x", write = "5.1 s", read = "1.7 ms",
     url = "https://facebook.github.io/zstd/"),
    (name = "zstd",         cname = "zstd", clevel = 3, shuffle = false,
     label = "zstd",          size = "0.77 GB", ratio = "2.27x", write = "5.0 s", read = "1.7 ms",
     url = "https://facebook.github.io/zstd/"),
    (name = "lz4-shuffle",  cname = "lz4",  clevel = 5, shuffle = true,
     label = "lz4 + shuffle", size = "0.94 GB", ratio = "1.85x", write = "4.6 s", read = "1.35 ms",
     url = "https://lz4.org/"),
    (name = "zstd-max",     cname = "zstd", clevel = 9, shuffle = true,
     label = "zstd level 9",  size = "0.60 GB", ratio = "2.90x", write = "36.9 s", read = "1.6 ms",
     url = "https://facebook.github.io/zstd/"),
]

#: What every row was measured on, shown as the table's caption. One line, no prose. Names the LAYOUT
#: it was measured in (see the docstring) — the layout table below states its codec, symmetrically, so
#: neither set of numbers is readable as "the" size without knowing what the other variable was.
const IMAGE_COMPRESSOR_MEASURED_ON =
    "1.7 GB 16-bit timecourse, whole store · zarr v2, nested keys, 512×512 chunks"

const IMAGE_COMPRESSOR_DEFAULT = "zstd-shuffle"

"""
    image_compressor() -> String

The configured image-store compressor choice (`[zarr].imageCompressor`), or the default when unset or
unrecognised. Falls back rather than erroring: a typo in `custom.toml` must not fail every write.

Label stores are deliberately not configurable — see `zarr_utils.LABEL_COMPRESSOR`.
"""
function image_compressor()::String
    name = string(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "imageCompressor",
                      IMAGE_COMPRESSOR_DEFAULT))
    any(c -> c.name == name, IMAGE_COMPRESSOR_CHOICES) ? name : IMAGE_COMPRESSOR_DEFAULT
end

"""
    set_image_compressor!(name) -> String

Persist `name` as `[zarr].imageCompressor` in the user's `custom.toml` and hot-reload config, so the
next task writes with it — no restart. Rejects an unknown name (the caller validates for the API's
400). Existing stores are untouched; `python/cecelia/utils/rechunk_zarr.py` re-lands them.
"""
function set_image_compressor!(name::AbstractString)::String
    nm = strip(String(name))
    any(c -> c.name == nm, IMAGE_COMPRESSOR_CHOICES) ||
        throw(ArgumentError("unknown compressor '$nm'"))
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    z   = get(cfg, "zarr", Dict{String,Any}())
    z["imageCompressor"] = nm
    cfg["zarr"] = z
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    nm
end

# ── Store LAYOUT defaults (Settings → Storage; the import pre-fills from these) ────────────────────
#
# The format and the chunk-key separator are decided PER IMAGE, at import — an existing v2 image cannot
# become v3 (no converter, ZARR_V3_PLAN D7) and derived stores inherit from their source (D9/D11). So
# unlike the compressor, which applies to whatever is written next, these are **defaults** that the
# import form pre-fills, not a switch that changes existing behaviour. `docs/todo/ZARR_V3_PLAN.md` D10.

#: Store LAYOUTS a new import can be written in, as the three VIABLE combinations of NGFF version and
#: chunk-key separator — not two independent controls. Flat keys and NGFF 0.5 cannot be combined
#: (bioformats2raw silently writes zarr v2 for that pair, verified in both flag orders), so presenting
#: them as one choice makes the impossible state unrepresentable instead of warning about it.
#:
#: Rendered as a TABLE in Settings, like the compressor, because the trade-off is the only reason there
#: is a choice. Every number is MEASURED on one source (`M3c-CD8-GFP-CD20-Tom_MAX.tif`, 2 pyramid
#: levels), with `read` the median of 9 interleaved full-level reads so drift hits each store equally.
#: All three decode to identical pixels.
#:
#: **Held fixed while measuring:** the CODEC — blosc/zstd level 3 + byte shuffle (`zstd-shuffle`, the
#: default of the compressor table above) with 1x1x1x512x512 chunks, verified identical in all three
#: stores' array metadata. Stated for the same reason that table names its layout: these are two
#: settings on one page and each table pins the other's variable, so `size` here is "this layout at the
#: default codec", not the size of the layout as such. A different codec shifts all three rows together
#: — the DIFFERENCE between them is a directory-inode cost and does not depend on the codec at all.
#:
#: **`flat` was removed.** It was the default, on 10 MB of 81 MB (~14%) saved
#: directory inodes at identical read time. Two things undid it. First, re-measured on a real 3.5 GB
#: movie instead of this 81 MB fixture, the saving is **~171 MB, ~5%** — the fixture flattered it
#: because its data was small relative to its directory count. Second, and decisive: a flat store
#: **conforms to no published NGFF version.** Nested storage is what 0.2 introduced (ome-zarr-py
#: `FormatV02`, "Changelog: move to nested storage") and 0.3/0.4/0.5 inherit it, so flat keys are 0.1
#: storage carrying 0.4-shaped metadata that 0.1 does not define. The two writers each labelled a
#: different half and disagreed on disk — bioformats2raw stamped `0.1` for `--no-nested`, our own
#: writers stamped `0.4` for the identical layout. 5% of a movie does not buy a store no conforming
#: reader can name. Existing flat stores still READ (the separator is self-describing); nothing needs
#: re-importing, and derived stores of a legacy flat source now come out nested.
#:
#: `v3` is offered and NOT default: same size, ~40% slower to read (263 vs 188 ms, its whole range above
#: every other median). bioformats2raw ALWAYS shards v3, so this measures "v3 as we can actually produce
#: it"; the shard-index indirection is the likely cause. See docs/todo/ZARR_V3_PLAN.md.
const STORE_LAYOUT_CHOICES = [
    (name = "nested", ngffVersion = "0.4", chunkSeparator = "nested",
     label = "zarr v2 · nested keys", keys = "0/0/36/0/8", dirs = "2,474",
     size = "81.2 MB", read = "188 ms",
     detail = "NGFF 0.4, dimension_separator '/' — bioformats2raw's own default"),
    (name = "v3", ngffVersion = "0.5", chunkSeparator = "nested",
     label = "zarr v3 · sharded", keys = "c/0/0/36/0/8", dirs = "2,476",
     size = "81.2 MB", read = "263 ms",
     detail = "NGFF 0.5, zarr.json + sharding_indexed (--ngff-version 0.5)"),
]
const STORE_LAYOUT_DEFAULT = "nested"

#: What every row was measured on — shown as the table's caption, one line, like the compressor's, and
#: naming the codec for the same reason that one names the layout.
const STORE_LAYOUT_MEASURED_ON =
    "0.5 GB 16-bit stack, 2 levels, zstd + shuffle · read = median of 9 interleaved"

"""The configured store layout, as a `STORE_LAYOUT_CHOICES` entry.

Resolved from the two persisted `[zarr]` keys rather than a layout name, so a config written before
this table existed still resolves, and so the two remain independently settable from the API. An
unrecognised pair falls back to the default rather than erroring — this is read on the write path of a
long import."""
function store_layout()
    v, sep = ngff_version(), chunk_separator()
    i = findfirst(c -> c.ngffVersion == v && c.chunkSeparator == sep, STORE_LAYOUT_CHOICES)
    STORE_LAYOUT_CHOICES[isnothing(i) ? findfirst(c -> c.name == STORE_LAYOUT_DEFAULT, STORE_LAYOUT_CHOICES) : i]
end

ngff_version()::String =
    (v = String(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "ngffVersion", NGFF_VERSION_DEFAULT));
     v in ("0.4", "0.5") ? v : NGFF_VERSION_DEFAULT)

chunk_separator()::String =
    (v = String(get(get(cecelia_conf(), "zarr", Dict{String,Any}()), "chunkSeparator", CHUNK_SEPARATOR_DEFAULT));
     v == "nested" ? v : CHUNK_SEPARATOR_DEFAULT)   # "flat" is no longer offered; see CHUNK_SEPARATORS

#: Defaults for the two underlying keys — derived from the default LAYOUT so they cannot drift apart.
const NGFF_VERSION_DEFAULT = "0.4"
const CHUNK_SEPARATOR_DEFAULT = "nested"

"""Persist a whole LAYOUT by name (both keys at once) and hot-reload. Setting the pair together is what
keeps the impossible combination unreachable from the UI."""
function set_store_layout!(name::AbstractString)::String
    nm = strip(String(name))
    i  = findfirst(c -> c.name == nm, STORE_LAYOUT_CHOICES)
    isnothing(i) && throw(ArgumentError("unknown store layout '$nm'"))
    c = STORE_LAYOUT_CHOICES[i]
    ensure_config_dir()
    cfg_path = custom_toml_path()
    cfg = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    z   = get(cfg, "zarr", Dict{String,Any}())
    z["ngffVersion"]    = c.ngffVersion
    z["chunkSeparator"] = c.chunkSeparator
    cfg["zarr"] = z
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    init_cecelia!()
    c.name
end

"""
    bf2raw_shuffle_values(lib_dir) -> (on, off)

How THIS bioformats2raw spells the blosc `shuffle` property. Returns the value for byte-shuffle-on and
for shuffle-off.

bioformats2raw 0.12.0 swapped its zarr library (jzarr → zarr-java, upstream PR #302) and with it the
spelling of this one property. The two are mutually exclusive — each version **hard-fails** on the
other's, so the flag has to follow the binary:

| passed              | 0.11.x (jzarr) | 0.12.x (zarr-java)   |
|---------------------|----------------|----------------------|
| `shuffle=1`         | byte shuffle   | NullPointerException |
| `shuffle=0`         | no shuffle     | NullPointerException |
| `shuffle=shuffle`   | invalid option | byte shuffle         |
| `shuffle=noshuffle` | invalid option | no shuffle           |

**`byteshuffle` — the alias 0.12's README documents for byte shuffle — is BROKEN upstream.** It
reaches blosc-java as a null enum (`NullPointerException: Cannot read field "shuffle" because "x0" is
null`, in `Blosc.Shuffle.access\$000`) and every chunk write throws `ZarrException: Error in encoding
blosc`. `shuffle` is the spelling that works; measured against 0.12.1 for cname ∈
{lz4, zstd, zlib, blosclz} and both NGFF 0.4 and 0.5.

Detected from the BUNDLED ZARR LIBRARY, not by parsing `--version`: the library *is* the cause, and a
directory listing costs nothing where `--version` pays a JVM start on every import. An unrecognised
install gets the current spelling — a wrong guess fails loudly (non-zero exit, which `_run_task`
already checks), it cannot silently write the wrong codec.
"""
function bf2raw_shuffle_values(lib_dir::AbstractString)
    legacy = isdir(lib_dir) && any(startswith(f, "jzarr-") for f in readdir(lib_dir))
    legacy ? ("1", "0") : ("shuffle", "noshuffle")
end

# lib/ sits next to bin/ in every bioformats2raw distribution: <install>/bin/bioformats2raw + <install>/lib
_bf2raw_lib_dir(bin::AbstractString = bioformats2raw_bin()) = joinpath(dirname(dirname(bin)), "lib")

"""
    bf2raw_chunk_flags(value) -> Vector{String}

bioformats2raw `--tile-width`/`--tile-height` flags for the configured chunk size, or **empty for
`"auto"`**.

Auto deliberately passes NOTHING and lets bioformats2raw apply its own default of 1024 — because that
default is already *capped to the frame*: a 512×512 acquisition gets 512×512 chunks, a 1024×1024 one
gets 1024×1024. That is exactly the rule we want (one chunk per plane, up to 1024) and it needs no
knowledge of the source dimensions, which we do not have at this point anyway — the image has not been
converted yet, so there is no store to measure.

Why one chunk per plane is the target rather than something smaller: napari slices per (t,c,z) and
draws whole planes, so a plane that is one chunk is one read. The same reasoning is written down in
`zarr_utils.plane_chunks`, which chunks our OWN writes that way. Smaller chunks only pay off for
routine sub-region reads, which nothing in the app does — segmentation reads tiles that are at least
its own block size.

A 1024×1024 `uint16` chunk is 2 MB. 2048 is 8 MB, which is a lot to fetch for a viewport showing far
less; it is offered for the rare very large frame, not as an upgrade.

Anything unparseable falls back to auto rather than raising — the same call as
`bf2raw_compression_flags`: a typo must not fail an hour-long import.
"""
function bf2raw_chunk_flags(value)::Vector{String}
    s = lowercase(strip(string(value)))
    (isempty(s) || s == "auto") && return String[]
    n = tryparse(Int, s)
    (isnothing(n) || n < 32) && return String[]
    ["--tile-width", string(n), "--tile-height", string(n)]
end

"""
    bf2raw_format_flags(ngff_version, shard_size) -> Vector{String}

bioformats2raw `--ngff-version` / `--shard-*` flags. Empty for the default (`"0.4"`, no shard), so an
unchanged import produces the exact command it always did.

The import is the ONLY place the store format is chosen; every derived store inherits it from its
source (`docs/todo/ZARR_V3_PLAN.md` D9). That is what keeps this answerable — the user decides once
per image, not again on every correction, crop and label set.

Returns `(flags, conflict)` — see the conflict note below.

**Sharding applies only to NGFF 0.5** and is silently dropped for 0.4 rather than raising: the two are
separate controls in the UI, and a user who sets a shard size and then switches back to 0.4 should get
a working import, not an error.

**`--no-nested` and `--ngff-version 0.5` are mutually exclusive** — together, bioformats2raw silently
writes zarr **v2** (verified in both flag orders: the root carries `.zgroup`, not `zarr.json`). Flat
wins, because flat has a measured benefit (56x fewer directories) and 0.5 on its own currently buys
nothing. `conflict` is returned so the caller can say so instead of leaving the user to notice in the
metadata modal.

**There is no "off".** `--shard-width` defaults to 1024 and cannot be disabled, so bioformats2raw shards
EVERY v3 store — verified against 0.12.1: a 0.5 import with no shard flag still produces a
`sharding_indexed` codec. The control therefore sets the shard SIZE, and `"auto"` means "pass nothing,
take the 1024 default" (itself capped to the frame). An option claiming to turn sharding off would be a
lie, which is why there is not one.

Which size is *best* is unmeasured — Phase 4's job. A shard is one file holding many chunks, so writing
one chunk rewrites the whole shard: safe for an import (written once, sequentially) and potentially
expensive for anything filling a store incrementally (D8). `"auto"` defers to upstream rather than
naming a number chosen to look decisive.
"""
function bf2raw_format_flags(ngff_version, shard_size;
                            shard_depth = "1", z_planes::Int = 0)
    v     = strip(string(ngff_version))
    flags = String[]

    # `--no-nested` is never emitted: we write NESTED keys in every format (see CHUNK_SEPARATORS in
    # zarr_utils.py for why flat was dropped — it conforms to no NGFF version). That also retires the
    # old flat-vs-0.5 conflict this function used to report: `--no-nested` combined with
    # `--ngff-version 0.5` silently produced a zarr v2 store, which was the reason a `conflict` flag
    # existed at all. With flat gone the impossible state is unrepresentable rather than warned about.
    if !(isempty(v) || v == "0.4")
        append!(flags, ["--ngff-version", v])
    end
    # sharding is NGFF 0.5 only
    v == "0.5" || return flags

    sh = lowercase(strip(string(shard_size)))
    if !(isempty(sh) || sh == "auto")
        n = tryparse(Int, sh)
        (isnothing(n) || n < 32) || append!(flags, ["--shard-width", string(n), "--shard-height", string(n)])
    end
    # Shard DEPTH is the only axis that actually reduces the file count on a 512x512 frame — width and
    # height cap to the frame, so the shard equals the chunk and packs nothing. Measured: depth 13 gave
    # 13 files where the default gave 109. It is also the axis D8 warns about: a shard spanning z is
    # rewritten in full for every plane write, so this is opt-in and warned, never a default.
    d = lowercase(strip(string(shard_depth)))
    if d == "all"
        z_planes > 1 && append!(flags, ["--shard-depth", string(z_planes)])
    else
        n = tryparse(Int, d)
        (isnothing(n) || n <= 1) || append!(flags, ["--shard-depth", string(n)])
    end
    flags
end

"""
    bf2raw_compression_flags([name]) -> Vector{String}

bioformats2raw CLI flags that make it write the SAME compressor our own Python writers use.

bioformats2raw defaults to `blosc/lz4-5`, which on real 16-bit acquisition data is 33% larger than the
default choice for no read-speed benefit that survives measurement. The import is the one store we do
NOT write through `zarr_utils`, so it is the one that has to be told explicitly — otherwise an
imported original and every correction derived from it are encoded differently.

The `shuffle` value's spelling depends on the installed version — see `bf2raw_shuffle_values`.
"""
function bf2raw_compression_flags(name::AbstractString = image_compressor())::Vector{String}
    i = findfirst(c -> c.name == name, IMAGE_COMPRESSOR_CHOICES)
    c = IMAGE_COMPRESSOR_CHOICES[isnothing(i) ? 1 : i]
    shuf_on, shuf_off = bf2raw_shuffle_values(_bf2raw_lib_dir())
    ["--compression", "blosc",
     "--compression-properties", "cname=$(c.cname)",
     "--compression-properties", "clevel=$(c.clevel)",
     "--compression-properties", "shuffle=$(c.shuffle ? shuf_on : shuf_off)"]
end

"""
    bf2raw_worker_flags(value) -> Vector{String}

bioformats2raw `--max-workers=N` flag, or **empty for `"auto"`**.

Empty defers to bioformats2raw's own default of 4. That is fine for TIFF/CZI/ND2 sources whose native
chunks decompress cheaply, but wrong for **Imaris HDF5 (`.ims`)**: each worker holds a decompressed
HDF5 chunk of the source (`H5tiledLayoutBB\$DataChunk.getByteBuffer` → `Deflate.decode`), and on a
big 3D volume that chunk can be several hundred MB. Four workers × chunk + reader overhead ran the
JVM out of heap on a 3.2 GB manual-IBEX Imaris file — 105 OOMs against the default `-Xmx` in one run,
0-3 chunks written before the task terminated. Two workers dropped it to 2 OOMs. `--max-workers=1` on
Imaris sources is the safe floor.

The auto-picker lives in `bf2raw_default_workers` — this helper just packages an explicit choice into
the CLI shape. Anything unparseable falls back to auto rather than raising, same as the chunk /
compression helpers: a typo must not fail an hour-long import.
"""
function bf2raw_worker_flags(value)::Vector{String}
    s = lowercase(strip(string(value)))
    (isempty(s) || s == "auto") && return String[]
    n = tryparse(Int, s)
    (isnothing(n) || n < 1) && return String[]
    ["--max-workers=$n"]
end

"""
    bf2raw_default_workers(src_path::AbstractString) -> String

Reader-aware default for `--max-workers`. Returns `"1"` for Imaris (`.ims`) — the reader whose native
HDF5 chunks are large enough that even 2 workers touch the JVM heap ceiling — and `"auto"` for
everything else (which resolves to bioformats2raw's own default of 4). See `bf2raw_worker_flags` for
the measurement that named this.
"""
function bf2raw_default_workers(src_path::AbstractString)::String
    endswith(lowercase(src_path), ".ims") ? "1" : "auto"
end

"""
    bf2raw_java_heap_gib(value) -> Int

Parse the `jvmHeapGiB` param into an integer GiB, or `0` for auto (defer to the JVM's own default of
~25% of RAM). Same forgiving-fallback rule as the sibling flag helpers.
"""
function bf2raw_java_heap_gib(value)::Int
    s = lowercase(strip(string(value)))
    (isempty(s) || s == "auto") && return 0
    n = tryparse(Int, s)
    (isnothing(n) || n < 1) && return 0
    n
end

"""
    bf2raw_default_heap_gib(src_path::AbstractString) -> Int

Reader-aware default for the JVM heap ceiling, in GiB. Returns `0` (leave the JVM default alone) for
readers where the default has always been enough, and a headroom number for Imaris — capped at half
the system RAM so the launcher can't wedge the box into swap. The cap is measured off `Sys.total_memory`
at task-start time, not a fixed literal (per `feedback_reservation_is_not_management` — don't ship a
diagnosis without the guard).
"""
function bf2raw_default_heap_gib(src_path::AbstractString)::Int
    endswith(lowercase(src_path), ".ims") || return 0
    ram_gib = max(1, floor(Int, Sys.total_memory() / (1024^3)))
    min(16, floor(Int, ram_gib / 2))
end

"""
    bf2raw_java_env(heap_gib::Int) -> Dict{String,String}

Build the env-var dict that the bioformats2raw launcher script honours (line 168 of the script:
"DEFAULT_JVM_OPTS, JAVA_OPTS, and BIOFORMATS2RAW_OPTS environment variables"). `heap_gib > 0` sets
`BIOFORMATS2RAW_OPTS = "-Xmx\${n}g"`, prepended to anything the caller already has in that env var so
we don't clobber a manually-set flag. `heap_gib <= 0` returns an empty dict — the caller then adds
nothing, and the JVM picks its own default.

Feed the result to `addenv(cmd, env_dict)` before `run`.
"""
function bf2raw_java_env(heap_gib::Integer)::Dict{String,String}
    heap_gib > 0 || return Dict{String,String}()
    prev = get(ENV, "BIOFORMATS2RAW_OPTS", "")
    opt  = "-Xmx$(heap_gib)g"
    Dict{String,String}("BIOFORMATS2RAW_OPTS" => isempty(prev) ? opt : "$opt $prev")
end
