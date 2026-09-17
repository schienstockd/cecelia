# ── OME-ZARR + bioformats2raw + OME-TIFF testsets ─────────────────────
# Eight sections covering the OME-ZARR / import boundary: chipSelect validation, flow
# boundary weight requires the metrics it is built from, intensity loss is an offered
# dial at the measured default, OME-ZARR metadata reads v2 and v3 alike, bioformats2raw
# chunk/worker+heap/format flags, and OME-TIFF export carries the calibration. Extracted
# from suite.jl to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).
#
# Three `joinpath(_app_src, "tasks", "importImages", "omezarr.json")` scans
# in the bioformats2raw testsets are rerouted through pathof(Cecelia) via _app_src so
# they resolve identically whether the file sits at app/test/ or app/test/suite/.

_app_src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "app", "src")

@testset "chipSelect validation" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    scales = only(p for p in spec["params"] if get(p, "key", "") == "temporalScales")
    @test scales["type"] == "chipSelect"
    @test [string(o["value"]) for o in scales["options"]] == ["1", "2", "3", "4", "6", "8", "12", "16"]

    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "2", "8"])) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "5"]))       # 5 is not offered
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => "1,2,4,8"))        # a string is no longer the shape

    # …and the runner-side parser still takes what the chips produce
    @test parse_temporal_scales(["1", "2", "8"]) == [1, 2, 8]
end


# ── the intensity loss is a dial, not a switch ───────────────────────────────
# This testset previously asserted the opposite: that `intensityWeight` defaulted to 0 and was no
# longer offered. That change was made on coastal's docstring ("prefer ConfettiForegroundLoss", with a
# measurement of the intensity target ALONE: 2535 components, median 3 px) plus the observation that
# recent models all used 0. An audit refuted the inference:
#
#   • The controlled pair was already on disk. memTom (intensity 1.0) and flow.small (intensity 0.0)
#     share image, channel, seed, epochs and frame count — and memTom reaches a LOWER foreground loss
#     (0.32507 vs 0.32747). The retained objective is fit no better without the term, so "it degrades
#     the head it supervises" does not hold.
#   • A sweep put peak IoU at 0.25 (0.622) with 0.0 at 0.606 and 1.0 at 0.523: 1.0 over-claims
#     (precision 54%) and 0.0 under-claims (recall 70%, circularity 16% above the raw signal).
#   • cecelia pins `confetti: 0.0`, so `foreground` is coastal's `ForegroundLoss` — brightness-only,
#     explicitly "the colour-blind form". The single-channel worry that shipped with the change named
#     `ConfettiForegroundLoss` and did not apply.
#
# So the default is the measured middle and the control is back. Pinned because "the form offers it"
# is the part that had no measurement behind it either way, and removing it also made
# `paramsFromManifest` drop it silently — a model trained at 1.0 handed back a form that would not
# reproduce it.
# ── the flow-boundary term and the metrics it is built from ──────────────────
# `foregroundBoundaryWeight` reached coastal as its default 0.0 because nothing passed it, and per
# `ForegroundLoss.target` it is "the ONLY path by which optical flow reaches the labels" — everywhere
# else flow enters as an input channel or through the contrastive term. Now plumbed.
#
# The trap it ships with: `coastal.loss.flow_discontinuity` builds the signal from |strain| +
# |vorticity| + |divergence|, and cecelia's DEFAULT metric set drops two of those three
# (`FLAT_FLOW_METRICS` — they are flat as input channels, which is a different question from whether
# their gradient marks a boundary). `flow_discontinuity` sums whichever are present and normalises, so
# a partial set trains against a weaker signal and says nothing. Refused rather than warned about,
# because a warning is read after the hour of training.
@testset "flow boundary weight requires the metrics it is built from" begin
    @test Set(Cecelia.FLOW_BOUNDARY_METRICS) == Set(["strain", "vorticity", "divergence"])
    # the collision is real and worth asserting, so a future change to either set surfaces it here
    @test !isempty(intersect(Set(Cecelia.FLOW_BOUNDARY_METRICS), Set(Cecelia.FLAT_FLOW_METRICS)))

    # off → nothing is required, whatever the metric set
    @test isempty(Cecelia.flow_boundary_missing(nothing, 0.0))
    @test isempty(Cecelia.flow_boundary_missing(["strain"], 0.0))

    # on with the shipped default → the two dropped ones are named
    @test Set(Cecelia.flow_boundary_missing(nothing, 0.5)) == Set(["vorticity", "divergence"])
    # on with all three ticked → nothing missing
    all_three = ["strain", "vorticity", "divergence", "acceleration"]
    @test isempty(Cecelia.flow_boundary_missing(all_three, 0.5))

    # …and the validator turns that into a submit-time error rather than a weak model
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5,
                         "flowMetrics" => all_three)) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))

    # the message has to NAME them — "check your metrics" costs a round trip to the docs
    err = try
        validate_params(TrainFlowModel(),
            Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))
        nothing
    catch e; e end
    @test occursin("vorticity", err.msg) && occursin("divergence", err.msg)

    # the control is offered, and OFF by default: switching it on also requires re-ticking metrics,
    # so it cannot be a silent default
    spec = Cecelia._task_spec(TrainFlowModel())
    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    fb = only(p for p in flat(spec["params"]) if get(p, "key", "") == "foregroundBoundaryWeight")
    @test fb["default"] == 0.0
end

@testset "intensity loss is an offered dial at the measured default" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    keys_of(ps) = reduce(vcat, [haskey(p, "params") ? keys_of(p["params"]) : [get(p, "key", "")]
                                for p in ps]; init = String[])
    @test "intensityWeight" ∈ keys_of(spec["params"])
    @test "foregroundWeight" ∈ keys_of(spec["params"])

    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    iw = only(p for p in flat(spec["params"]) if get(p, "key", "") == "intensityWeight")
    # NOT 0.0 (the refuted default) and NOT 1.0 (over-claims); the sweep's peak.
    @test iw["default"] == 0.25
    @test iw["min"] == 0.0        # 0 stays REACHABLE — the experiment must remain runnable
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 1.0)) === nothing
end

@testset "OME-ZARR metadata reads v2 and v3 alike" begin
    # `read_ome_metadata` feeds ccid.json `meta`, which docs/OBJECTMODEL.md → *Calibration* makes authoritative
    # for every physical number in the app. NGFF 0.5 nests attributes under `ome`; a reader that misses
    # that returns an EMPTY Dict, and the caller then has no PhysicalSize/TimeIncrement at all — which
    # downstream becomes 1.0 rather than an error. So the two formats are asserted to agree, against two
    # committed stores of the same real pixels. See test-data/README.md, docs/todo/ZARR_V3_PLAN.md.
    v2 = fixture_path("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = fixture_path("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(have_fixture(v2) && have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        # the series wrapper is found structurally in BOTH formats (v2 `.zattrs`, v3 `zarr.json`)
        @test Cecelia.series_base(v2) == joinpath(v2, "0")
        @test Cecelia.series_base(v3) == joinpath(v3, "0")

        # the one resolver: attributes come back unwrapped regardless of the `ome` nesting
        for p in (v2, v3)
            attrs = ngff_attrs(joinpath(p, "0"))
            @test !isnothing(attrs)
            @test haskey(attrs, :multiscales)          # NOT nested under :ome by the time we see it
            ms = ngff_multiscales(joinpath(p, "0"))
            @test !isnothing(ms) && !isempty(ms)
        end
        # a directory with no zarr metadata answers nothing rather than throwing
        @test isnothing(ngff_attrs(joinpath(v2, "does-not-exist")))
        # array metadata resolves for both; a GROUP dir must NOT be mistaken for an array (v3 shares
        # the filename `zarr.json` between the two)
        @test !isnothing(zarr_array_meta(joinpath(v3, "0", "0")))
        @test isnothing(zarr_array_meta(joinpath(v3, "0")))

        m2 = read_ome_metadata(v2)
        m3 = read_ome_metadata(v3)
        @test !isempty(m2) && !isempty(m3)
        for k in ("SizeC", "SizeT", "SizeZ")
            @test m2[k] == m3[k]
        end
        @test (m2["SizeC"], m2["SizeT"], m2["SizeZ"]) == (4, 3, 3)

        # Calibration — the whole reason these fixtures are real. Deliberately not 1.0, so a correct
        # read is distinguishable from the "unknown" fallback.
        for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "TimeIncrement")
            @test haskey(m2, k) && haskey(m3, k)
            @test isapprox(m2[k], m3[k]; rtol = 1e-9)
        end
        @test isapprox(m2["PhysicalSizeX"], 0.5964274525755702; rtol = 1e-6)
        @test !isapprox(m2["PhysicalSizeX"], 1.0; atol = 1e-6)    # not the silent fallback
        @test isapprox(m2["PhysicalSizeZ"], 3.0; rtol = 1e-6)
        @test isapprox(m2["TimeIncrement"], 30.0; rtol = 1e-6)
    end
end

@testset "bioformats2raw chunk flags" begin
    # These flags were the bug: `chunkSizeX`/`chunkSizeY` existed in omezarr.json and were read by
    # NOTHING — no tile flag ever reached the CLI, so a user who chose 512 still got bioformats2raw's
    # 1024. One `chunkSize` param now, and it is passed.
    @test Cecelia.bf2raw_chunk_flags("512") == ["--tile-width", "512", "--tile-height", "512"]
    @test Cecelia.bf2raw_chunk_flags(1024)  == ["--tile-width", "1024", "--tile-height", "1024"]

    # "auto" passes NOTHING on purpose: bioformats2raw's own default is 1024 ALREADY CAPPED to the
    # frame, which is exactly the rule we want (one chunk per plane, up to 1024) and needs no source
    # dimensions — which we do not have, since the image is not converted yet.
    @test isempty(Cecelia.bf2raw_chunk_flags("auto"))
    @test isempty(Cecelia.bf2raw_chunk_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_chunk_flags(""))

    # unparseable / absurd falls back to auto rather than raising — same call as the compression
    # flags: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_chunk_flags("banana"))
    @test isempty(Cecelia.bf2raw_chunk_flags(0))
    @test isempty(Cecelia.bf2raw_chunk_flags(-8))
    @test isempty(Cecelia.bf2raw_chunk_flags(16))       # below 32: not a sane chunk

    # every option the task spec offers must actually resolve (a spec/handler drift here is silent —
    # the import would just ignore the choice, which is the bug this whole testset exists for)
    spec = JSON3.read(read(joinpath(_app_src, "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    cs   = only(filter(p -> get(p, :key, "") == "chunkSize", collect(adv.params)))
    vals = [string(get(o, :value, o)) for o in cs.options]
    @test "auto" in vals
    @test string(cs.default) in vals
    for v in vals
        @test v == "auto" ? isempty(Cecelia.bf2raw_chunk_flags(v)) :
                            Cecelia.bf2raw_chunk_flags(v) == ["--tile-width", v, "--tile-height", v]
    end

    # and the tips must not merely restate the label — that is what made these params guesswork
    for p in vcat(collect(spec.params), collect(adv.params))
        get(p, :type, "") == "section" && continue
        tip = String(get(p, :tip, ""))
        @test !isempty(tip)
        @test lowercase(tip) != lowercase(String(get(p, :label, "")))
    end
end

@testset "bioformats2raw worker + heap flags" begin
    # Same spec/handler drift shape as bf2raw_chunk_flags — an option surfaced in the JSON that no
    # code translates is silent (import runs at the wrong worker count). Measured 2026-08-27 on
    # `Human_Lymph_Node_Manual_IBEX.ims`: workers=4 (bf2raw default) → 105 OOMs and 0-3 chunks;
    # workers=2 + -Xmx16g → 2 OOMs and 3820 chunks; workers=1 → zero OOMs. That's why Imaris auto = 1.
    @test Cecelia.bf2raw_worker_flags("1") == ["--max-workers=1"]
    @test Cecelia.bf2raw_worker_flags(2)   == ["--max-workers=2"]
    @test Cecelia.bf2raw_worker_flags(8)   == ["--max-workers=8"]

    # "auto" and unparseable both defer to bioformats2raw's own default (4) — same forgiving-fallback
    # rule as chunk/compression: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_worker_flags("auto"))
    @test isempty(Cecelia.bf2raw_worker_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_worker_flags(""))
    @test isempty(Cecelia.bf2raw_worker_flags("banana"))
    @test isempty(Cecelia.bf2raw_worker_flags(0))
    @test isempty(Cecelia.bf2raw_worker_flags(-2))

    # Extension-keyed defaults — Imaris (`.ims`) is the reader we know decompresses fat HDF5 chunks.
    # Everything else stays on "auto" (bf2raw picks 4).
    @test Cecelia.bf2raw_default_workers("/some/path/thing.ims") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/THING.IMS") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.tif") == "auto"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.czi") == "auto"
    @test Cecelia.bf2raw_default_workers("")                       == "auto"

    # JVM heap parsing mirrors the workers pattern
    @test Cecelia.bf2raw_java_heap_gib("16") == 16
    @test Cecelia.bf2raw_java_heap_gib(24)   == 24
    @test Cecelia.bf2raw_java_heap_gib("auto") == 0
    @test Cecelia.bf2raw_java_heap_gib("") == 0
    @test Cecelia.bf2raw_java_heap_gib("banana") == 0
    @test Cecelia.bf2raw_java_heap_gib(0) == 0
    @test Cecelia.bf2raw_java_heap_gib(-4) == 0

    # Extension-keyed heap default — Imaris gets headroom, everything else defers to the JVM. Cap at
    # half the box RAM so a fixed literal can't wedge a small machine into swap (measured off
    # Sys.total_memory at the call site, not a hardcoded number).
    ram_gib = max(1, floor(Int, Sys.total_memory() / (1024^3)))
    exp_ims = min(16, floor(Int, ram_gib / 2))
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.ims") == exp_ims
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.tif") == 0
    @test Cecelia.bf2raw_default_heap_gib("")                       == 0

    # Env dict shape — `heap_gib > 0` sets BIOFORMATS2RAW_OPTS with -Xmx, otherwise empty (JVM default).
    # A pre-existing value in ENV is preserved (prepended to keep our flag winning) — feedback: never
    # clobber a user-set env var. This side-tests that path without mutating the real ENV.
    empty_env = Cecelia.bf2raw_java_env(0)
    @test isempty(empty_env)
    heap_env = Cecelia.bf2raw_java_env(16)
    @test heap_env["BIOFORMATS2RAW_OPTS"] == "-Xmx16g" ||
          startswith(heap_env["BIOFORMATS2RAW_OPTS"], "-Xmx16g ")

    # every worker option in the task spec must actually resolve
    spec2 = JSON3.read(read(joinpath(_app_src, "tasks", "importImages", "omezarr.json"), String))
    adv2  = only(filter(p -> get(p, :type, "") == "section", collect(spec2.params)))
    mw    = only(filter(p -> get(p, :key, "") == "maxWorkers", collect(adv2.params)))
    for o in mw.options
        v = string(get(o, :value, o))
        @test v == "auto" ? isempty(Cecelia.bf2raw_worker_flags(v)) :
                            Cecelia.bf2raw_worker_flags(v) == ["--max-workers=$v"]
    end
    hp = only(filter(p -> get(p, :key, "") == "jvmHeapGiB", collect(adv2.params)))
    for o in hp.options
        v = string(get(o, :value, o))
        n = Cecelia.bf2raw_java_heap_gib(v)
        @test v == "auto" ? n == 0 : n == parse(Int, v)
    end
end

@testset "bioformats2raw format flags" begin
    # The import is the ONLY place the store format is chosen; derived stores inherit it
    # (docs/todo/ZARR_V3_PLAN.md D9).
    ff(args...; kw...) = Cecelia.bf2raw_format_flags(args...; kw...)

    @test isempty(ff("0.4", "auto"))                       # default = the command we always ran
    @test ff("0.5", "auto") == ["--ngff-version", "0.5"]
    @test ff("0.5", "1024") ==
          ["--ngff-version", "0.5", "--shard-width", "1024", "--shard-height", "1024"]

    # Sharding is NGFF 0.5 only, and is dropped for 0.4 rather than raising: they are separate controls
    # and switching the version back must still produce a working import.
    @test isempty(ff("0.4", "1024"))

    # unparseable / absurd falls back to upstream's default rather than raising
    for bad in ("banana", "0", "-8", "16", "")
        @test ff("0.5", bad) == ["--ngff-version", "0.5"]
    end

    # ── chunk-key separator ──────────────────────────────────────────────────────
    # `--no-nested` IS NEVER EMITTED (2026-08-14). Flat keys saved ~5% on a real movie at identical read
    # time, but produce a store that conforms to no published NGFF version — nested storage is what 0.2
    # introduced, so flat keys are 0.1 storage under the 0.4-shaped metadata written beside them. The
    # separator is therefore no longer a parameter of this function at all, which also retires the old
    # flat+0.5 conflict (that pair silently wrote zarr v2) by making it unrepresentable.
    for v in ("0.4", "0.5"), sh in ("auto", "1024")
        @test !("--no-nested" in ff(v, sh))
    end
    @test_throws MethodError Cecelia.bf2raw_format_flags("0.4", "auto"; separator = "flat")

    # ── shard depth ──────────────────────────────────────────────────────────────
    # The ONLY axis that reduces the file count on a 512x512 frame — width/height cap to the frame, so
    # the shard equals the chunk and packs nothing (measured: depth 13 -> 13 files vs 109).
    @test ff("0.5", "auto"; shard_depth = "13") == ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 13) ==
          ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "1") == ["--ngff-version", "0.5"]      # the default: no flag
    # "all" with no usable z count drops the flag rather than guessing a depth
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 0) == ["--ngff-version", "0.5"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 1) == ["--ngff-version", "0.5"]
    # depth is NGFF 0.5 only, like the rest of sharding
    @test isempty(ff("0.4", "auto"; shard_depth = "13"))

    # Every option the spec offers must resolve, and there must be NO option claiming to disable
    # sharding: --shard-width cannot be turned off, so bioformats2raw shards every v3 store (verified
    # against 0.12.1 — a 0.5 import with no shard flag still produces a sharding_indexed codec), and an
    # "off" option would be a lie.
    spec = JSON3.read(read(joinpath(_app_src, "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    # `chunkSeparator` is NOT in this list any more: it was a declared param that the importer never
    # read — no `--no-nested`, no `dimension_separator`, nothing — and its default `"flat"`
    # contradicted `CHUNK_SEPARATOR_DEFAULT = "nested"`, which `config.jl` notes is the only separator
    # still offered. A control that reaches nothing is worse than an absent one: it reads as a choice.
    for key in ("ngffVersion", "shardSize", "shardDepth")
        prm  = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        vals = [string(get(o, :value, o)) for o in prm.options]
        # `ngffVersion` takes its default from the Settings store layout (`defaultFrom`), so this
        # reads the RESOLVED spec — a raw file read would see the pre-resolution literal.
        @test string(prm.default) in vals
        @test !isempty(String(get(prm, :tip, "")))
    end
    shard = only(filter(p -> get(p, :key, "") == "shardSize", collect(adv.params)))
    @test !any(lowercase(string(get(o, :value, o))) in ("none", "off", "0") for o in shard.options)

    # Transparency: someone who knows zarr must be able to map each control onto what lands on disk, so
    # every one of these tips names its bioformats2raw flag or the metadata key it sets.
    for key in ("chunkSize", "ngffVersion", "shardSize", "shardDepth")
        prm = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        tip = String(get(prm, :tip, ""))
        @test occursin("--", tip) || occursin("_", tip)   # a CLI flag or a zarr metadata key
    end
end

@testset "OME-TIFF export carries the calibration" begin
    # The task exists because the OLD route (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter)
    # lost the pixel sizes: a plain TIFF has nowhere to record Z spacing, so the converter guessed the
    # voxel size. Every assertion below is about the calibration surviving — that IS the feature.

    meta = Dict{String,Any}("PhysicalSizeX" => 0.325, "PhysicalSizeY" => 0.325,
                            "PhysicalSizeZ" => 2.0,   "PhysicalSizeUnit" => "µm",
                            "TimeIncrement" => 10.0,  "TimeIncrementUnit" => "s")

    cal = Cecelia._export_calibration(meta)
    @test cal["PhysicalSizeZ"] == 2.0                     # the field the old workflow dropped
    @test cal["PhysicalSizeZUnit"] == "µm"

    # UNITS MUST BE THE OME SYMBOL, not the NGFF/UDUNITS name ccid.json stores. OME's UnitsLength and
    # UnitsTime are ENUMERATIONS; "micrometer" is not a member, so one such attribute makes <Pixels>
    # schema-invalid and Bio-Formats discards the ENTIRE OME block and falls back to counting IFDs —
    # a 31x4x32 movie then opens as 3968 timepoints, one channel, no names, no voxel size. Verified
    # against real Bio-Formats (bioformats2raw): "µm" reads back in full, "micrometer" reads nothing.
    ngff = Dict{String,Any}("PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33,
                            "PhysicalSizeZ" => 2.0,  "PhysicalSizeUnit" => "micrometer",
                            "TimeIncrement" => 15.0, "TimeIncrementUnit" => "second")
    ncal = Cecelia._export_calibration(ngff)
    for k in ("PhysicalSizeXUnit", "PhysicalSizeYUnit", "PhysicalSizeZUnit")
        @test ncal[k] == "µm"
    end
    @test ncal["TimeIncrementUnit"] == "s"
    # An unknown unit passes through rather than being guessed at — same rule as the converter.
    @test Cecelia._export_calibration(
        Dict{String,Any}("PhysicalSizeX" => 1.0, "PhysicalSizeUnit" => "furlong")
    )["PhysicalSizeXUnit"] == "furlong"
    @test cal["PhysicalSizeX"] == 0.325 && cal["PhysicalSizeXUnit"] == "µm"
    @test cal["TimeIncrement"] == 10.0 && cal["TimeIncrementUnit"] == "s"

    # A Z-MIP has no z extent left and a single frame has no interval — writing either would state a
    # geometry the file doesn't have.
    @test !haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeZ")
    @test haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeX")
    @test !haskey(Cecelia._export_calibration(meta; one_frame = true), "TimeIncrement")

    # Unknown must stay unknown. Defaulting an absent/zero/garbage size to 1.0 would tell Imaris the
    # pixel is one micron, which is a claim, not a fallback.
    for bad in (Dict{String,Any}(), Dict{String,Any}("PhysicalSizeX" => ""),
                Dict{String,Any}("PhysicalSizeX" => 0.0), Dict{String,Any}("PhysicalSizeX" => "abc"))
        @test !haskey(Cecelia._export_calibration(bad), "PhysicalSizeX")
    end

    # …and that absence is exactly what QC flags, since the write itself always "succeeds".
    codes(f) = [x["code"] for x in f]
    @test isempty(Cecelia._export_qc_findings(cal, 21))     # fully calibrated → nothing to say
    @test isempty(Cecelia._export_qc_findings(cal, 1))
    # A 2D image legitimately has no Z spacing — don't cry wolf on SizeZ == 1.
    @test isempty(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 1))
    @test "export.no_z_calibration" in
          codes(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 21))
    @test "export.no_xy_calibration" in codes(Cecelia._export_qc_findings(Dict{String,Any}(), 1))

    # channelSelection submits channel NAMES, not indices. Converting them by hand threw
    # `Int("DAPI")` straight out of the task; `channel_indices` is the resolver, and it is 0-based —
    # which is what the runner slices with, so an off-by-one here exports the wrong channel.
    names = ["DAPI", "GFP", "mem-Tom"]
    @test channel_indices(["GFP"], names; what = "channels") == [1]
    @test channel_indices(["mem-Tom", "DAPI"], names; what = "channels") == [2, 0]
    @test channel_indices(nothing, names; what = "channels") == Int[]
    @test channel_indices(String[], names; what = "channels") == Int[]
    # A name this version doesn't have must say so rather than silently pick something.
    @test_throws ErrorException channel_indices(["nope"], names; what = "channels")

    # …and the NAMES must come from `channel_names`, which falls back to the active version. Channel
    # names are typically registered only under `default` while a processed version carries none, so
    # reading the requested version's raw field returns nothing and the task reports "(none
    # registered)" for an image whose channels the picker is listing — the picker is fed by
    # `channel_names(img)`, so any other source disagrees with what the user just clicked.
    proj = create_project!(name = "chan-fallback-$(rand(1000:9999))")
    st   = add_set!(proj; name = "s")
    im   = add_image!(st; name = "chan-fallback")
    set_channel_names!(im, ["DAPI", "SHG"]; value_name = VERSIONED_DEFAULT_VAL, check_length = false)
    save!(im)

    @test channel_names(im) == ["DAPI", "SHG"]
    # An explicit version with no entry of its own still resolves — this is the bug.
    @test channel_names(im; value_name = "corrected") == ["DAPI", "SHG"]
    @test channel_indices(["SHG"],
                          something(channel_names(im; value_name = "corrected"), String[]);
                          what = "channels") == [1]

    # Dispatch + spec wiring
    @test Cecelia._task_from_fun_name("exportImages.ome_tiff") isa ExportOmeTiff
    spec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    @test String(get(spec, :fun_name, "")) == "exportImages.ome_tiff"
    @test String(get(spec, :resource_pool, "")) == "io"
    # The output is an ARTEFACT, not a version — nothing may register an image version from it.
    @test !any(String(get(p, :key, "")) == "outputValueName" for p in get(spec, :params, []))

    # One filename rule, shared with the movie recorders — an image called "… (cropped)" must not
    # produce a name that ends in a separator (that bug shipped once already).
    @test safe_name_part("A B (cropped)") == "A_B_cropped"
    @test safe_name_part("  ") == ""
    @test safe_name_part(nothing) == ""
end
