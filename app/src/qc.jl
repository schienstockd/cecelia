# QC framework — general "we processed this, but the output looks off" findings.
#
# Convention (docs/todo/QC_PLAN.md): one JSON per (task, output) at
#   1/{uid}/qc/{funName}/{valueName}.json
# with a generic `findings` list the GUI renders verbatim (badge + tooltip on the image, the
# MetadataPanel, and the chain whiteboard node). QC is ADVISORY — it never fails or gates a task.
# The backend (this layer + each task) computes findings so thresholds live in one place; the GUI
# only renders. This file is image-owned (like img_physical_sizes / pop_df) — any task emits QC the
# same way via `write_qc`, rather than each task hand-rolling its own sidecar.

const QC_DIRNAME = "qc"

# Colour-blind-safe severity symbols for lab-log entries — SHAPE-DISTINCT glyphs (✅ check / ⚠️
# triangle / ❌ cross), NOT the same-shape circles 🟢🟡🔴 (which differ only in hue and are unreadable
# under red-green colour blindness). Mirrors the frontend severity model (frontend/src/lib/severity.ts):
# ok / warn / fail. See docs/todo/QC_OBSERVER_PLAN.md.
const SEVERITY_SYMBOLS = Dict("ok" => "✅", "warn" => "⚠️", "fail" => "❌")
severity_symbol(sev::AbstractString)::String = get(SEVERITY_SYMBOLS, string(sev), "")

qc_root(img::CciaImage) = joinpath(img._dir, QC_DIRNAME)
qc_fun_dir(img::CciaImage, fun_name::AbstractString) = joinpath(qc_root(img), string(fun_name))
# A task with no output value_name falls back to the default versioned key so there's always a key.
_qc_vn(value_name::AbstractString) = isempty(value_name) ? VERSIONED_DEFAULT_VAL : string(value_name)
qc_path(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    joinpath(qc_fun_dir(img, fun_name), _qc_vn(value_name) * ".json")

# One finding. `level ∈ ("info","warn")` — "error" is reserved (QC never blocks). `code` is a stable
# kebab/dotted slug (e.g. "drift.canvas_expansion") so the GUI can key styling/help off it.
function qc_finding(level::AbstractString, code::AbstractString,
                    short::AbstractString, long::AbstractString; detail = nothing)
    f = Dict{String,Any}("level" => string(level), "code" => string(code),
                         "short" => string(short), "long" => string(long))
    isnothing(detail) || (f["detail"] = detail)
    f
end

# ── QC copy catalog ───────────────────────────────────────────────────────────────────────────────
#
# Every user-facing QC string, in one table. This is the worst-placed copy in the app — prose buried
# inside analysis functions, where nobody reviews it and `pixi run ui-copy` was the first thing that
# could even see it. Keeping it here means the wording can be read and revised as a set, and the
# analysis code below reads as logic rather than as logic-plus-writing.
#
# Text rules (`docs/UI.md` → *UI copy*, `docs/MODULES.md` → *QC*): `short` = the problem, terse and
# with no trailing period; `long` = what to DO about it, one imperative sentence. Numbers belong in
# the finding's `detail`, not in the prose — a `{}` placeholder is only for the cases where the
# number IS the message (a percentage, a channel index). The set of allowed placeholder names is
# ASSERTED in runtests, so adding one is a deliberate act, not a typo.
#
# KEYED SEPARATELY FROM `code`. Usually the key is the code, but they are deliberately not the same
# field: `metadata.pixel_size_no_unit` is emitted for the x, y AND z axes with different wording for
# z, and `output.canvas_expansion` is emitted by the drift task under `drift.canvas_expansion`. Codes
# are a stored contract — they sit in every banked `qc/*.json` on disk and the frontend filters on
# them (`isMetadataCode`) — so the catalog bends around them rather than the other way round.
const QC_TEXT = Dict{String,@NamedTuple{short::String, long::String}}(
    # calibration (metadata_qc_findings)
    "metadata.z_spacing_unknown" => (
        short = "Z spacing unknown",
        long  = "No Z step found — set the voxel depth (acquisition software, or Fiji ▸ Image ▸ Properties)."),
    "metadata.z_spacing_corrected" => (
        short = "Z spacing auto-corrected",
        long  = "Auto-corrected from the source ImageJ tag — confirm it in Fiji ▸ Image ▸ Properties before trusting it."),
    "metadata.z_spacing_unusual" => (
        short = "Z spacing looks unusual",
        long  = "Z step is far from the XY pixel size — likely a wrong calibration unit; check the original in Fiji and correct it."),
    "metadata.frame_interval_unknown" => (
        short = "Frame interval unknown",
        long  = "No frame interval found — enter it from your acquisition settings."),
    "metadata.frame_interval_no_unit" => (
        short = "Frame interval has no unit",
        long  = "A frame interval is recorded without a unit — re-enter it with seconds/minutes."),
    "metadata.pixel_size_no_unit" => (
        short = "Pixel size has no unit",
        long  = "A pixel size is recorded without a unit — re-enter it with a unit."),
    "metadata.voxel_depth_no_unit" => (      # emitted under code `metadata.pixel_size_no_unit`
        short = "Voxel depth has no unit",
        long  = "A Z step is recorded without a unit — re-enter it with a unit."),

    # clipping at acquisition (import.channel_saturated)
    "import.channel_saturated" => (
        short = "Channel {channel} clipped at the detector",
        long  = "Lower the gain or exposure when acquiring — clipped values cannot be recovered."),


    # HMM (hmm_states_qc_findings / hmm_transitions_qc_findings)
    "hmm.no_states_decoded" => (
        short = "No cells decoded into a state",
        long  = "Tracks may be too short or measurements incomplete — check segmentation/tracking and re-run."),
    "hmm.single_state" => (
        short = "All cells sat in one state",
        long  = "This image didn't switch states — check it's the same acquisition and measurements, or reduce the state count."),
    "hmm.dominant_state" => (
        short = "One state holds {pct}% of cells",
        long  = "Check the behaviour is really this uniform, or the model may have too many states."),
    "hmm.no_transitions" => (
        short = "No state transitions found",
        long  = "Tracks may be too short or the model produced one state — check HMM states and track lengths."),

    # tracking (track_measures_qc_findings)
    "tracking.motion_dims_uncertain" => (
        short = "Motion dimensionality uncertain ({dims}D)",
        long  = "z couldn't be classified as migration vs jitter — review whether tracking should be 2D or 3D and re-run with dims set."),

    # clustering (cluster_qc_findings)
    "clustering.single_cluster" => (
        short = "Only one cluster found",
        long  = "Resolution too low or features don't separate populations — raise resolution or add features and re-run."),
    "clustering.image_one_cluster" => (
        short = "All {unit} fell into one cluster",
        long  = "This image separated from the cohort — check it's the same acquisition and normalisation, then re-run."),
    "clustering.dominant_cluster" => (
        short = "One cluster holds {pct}% of {unit}",
        long  = "Check the population is really this uniform, or raise resolution to split it."),

    # skeleton branching (segment/branching.jl). These predated the catalog and were inlined at the
    # call site with the four-argument form; moved here so the wording is reviewable with the rest.
    "branching.no_branches" => (
        short = "No branches found",
        long  = "Lower the pre-dilation or check the segmentation, then re-run."),
    "branching.aniso_grid_large" => (
        short = "Orientation grid is large",
        long  = "Raise the grid spacing — the stored field costs 1/box², so doubling it saves about 4x."),
    "branching.uncalibrated" => (
        short = "Image has no pixel size",
        long  = "Set the pixel size, or read the µm scale settings as pixels — 1 µm/px was assumed."),

    # output geometry (qc_canvas_expansion)
    "output.canvas_expansion" => (
        short = "Output canvas grew +{pct}% in XY",
        long  = "Larger than a clean correction — check the output and re-run this step if it looks wrong."),

    # cohort comparison (qc_cohort.jl `_cohort_finding`)
    "cohort.outlier" => (
        short = "{metric} is a cohort outlier",
        long  = "This image's {metric} ({value}) is far {dir} the set median ({median}) — check this image before trusting the run."),
)

"""
    qc_text(key; subs...) -> (short, long)

The catalog entry for `key`, with every `{name}` placeholder replaced by the matching keyword.

Throws on an unknown key, and on a placeholder with no substitution — a QC finding that renders
`"Channel {channel} is flat"` to a user is worse than one that fails loudly in the task's test.
"""
function qc_text(key::AbstractString; subs...)
    haskey(QC_TEXT, key) || error("Unknown QC text key: \"$key\". Add it to QC_TEXT in app/src/qc.jl.")
    entry = QC_TEXT[key]
    fill(s) = replace(s, r"\{(\w+)\}" => m -> begin
        name = Symbol(match(r"\{(\w+)\}", m)[1])
        haskey(subs, name) || error("QC text \"$key\" needs a `$name` substitution.")
        string(subs[name])
    end)
    (short = fill(entry.short), long = fill(entry.long))
end

"""
    qc_finding(level, code; key = code, detail = nothing, subs...) -> Dict

A finding whose text comes from [`QC_TEXT`](@ref) rather than the call site. `key` defaults to
`code` and is only passed separately where one code carries more than one wording (see the note on
the catalog). This is the form new code should use; the four-argument method above stays for
custom modules, which have no entry in the catalog.
"""
function qc_finding(level::AbstractString, code::AbstractString; key::AbstractString = code,
                    detail = nothing, subs...)
    t = qc_text(key; subs...)
    f = qc_finding(level, code, t.short, t.long; detail = detail)
    # Carry the INPUTS, not just the output. `short`/`long` are still written — as a snapshot, so a
    # sidecar stays readable and survives a catalog entry being renamed away — but `key` + `subs` are
    # what the read path re-renders from. See `_qc_hydrate`.
    f["key"] = String(key)
    isempty(subs) || (f["subs"] = Dict{String,Any}(String(k) => v for (k, v) in pairs(subs)))
    f
end

# ── Read-time rendering ───────────────────────────────────────────────────────────────────────────
#
# QC findings are PERSISTED, so text rendered at emit time is frozen: fixing a clumsy warning used to
# mean re-running the analysis that produced it, and a language switch could never reach anything
# already banked. So the catalog is applied on the way OUT instead — `key` + `subs` are the stored
# truth and the prose is rebuilt per read.
#
# Constraints this has to respect:
#  - NEVER throw. This is a data path; a bad or missing catalog entry must degrade to the stored
#    snapshot, not break the image payload for every image in the set.
#  - Preserve JSON3 access semantics. Callers read these docs with Symbol keys (`get(f, :short)` in
#    lab_log_context/qc_cohort) AND String keys (`doc["findings"]` in the tests). A plain Dict
#    supports only one, so the rebuild is re-parsed back into a JSON3 object.
#  - Cost nothing for old data. A pre-catalog sidecar carries no `key`, so it short-circuits before
#    any allocation — which is also why this is safe to leave in the per-image payload path.
function _qc_hydrate_finding(f)
    f isa AbstractDict || return f
    k = get(f, :key, nothing)
    (k isa AbstractString && haskey(QC_TEXT, String(k))) || return f
    subs = get(f, :subs, nothing)
    kw = subs isa AbstractDict ? [Symbol(sk) => sv for (sk, sv) in pairs(subs)] : Pair{Symbol,Any}[]
    t = try
        qc_text(String(k); kw...)
    catch
        return f          # entry changed shape / a sub is missing → keep the snapshot
    end
    d = Dict{String,Any}(String(kk) => vv for (kk, vv) in pairs(f))
    d["short"] = t.short; d["long"] = t.long
    d
end

function _qc_hydrate(doc)
    doc === nothing && return nothing
    fs = get(doc, :findings, nothing)
    fs isa AbstractVector || return doc
    any(f -> f isa AbstractDict && haskey(f, :key), fs) || return doc   # old sidecar → untouched
    d = Dict{String,Any}(String(k) => v for (k, v) in pairs(doc))
    d["findings"] = [_qc_hydrate_finding(f) for f in fs]
    JSON3.read(JSON3.write(d))                                          # keep Symbol+String access
end

# Write (or clear) an image's QC for one (task, output). `findings` empty ⇒ still writes the file with
# an empty list, so a clean re-run overwrites a previous warning rather than leaving it stale.
function write_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString,
                  findings::AbstractVector; source = nothing, output = nothing, extras...)
    dir = qc_fun_dir(img, fun_name); mkpath(dir)
    doc = Dict{String,Any}("funName" => string(fun_name), "valueName" => _qc_vn(value_name),
                           "findings" => collect(findings))
    isnothing(source) || (doc["source"] = source)
    isnothing(output) || (doc["output"] = output)
    for (k, v) in extras; doc[string(k)] = v; end
    path = qc_path(img, fun_name, value_name)
    write_json_atomic(path, doc)
    path
end

# The one QC file read — parse + apply the catalog (`_qc_hydrate`). Both readers below go through it
# so finding text can never be stale on one path and current on another.
_read_qc_file(path::AbstractString) = _qc_hydrate(JSON3.read(read(path, String)))

read_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (p = qc_path(img, fun_name, value_name); isfile(p) ? _read_qc_file(p) : nothing)

# All QC docs for an image, keyed "funName/valueName" → parsed doc. Powers the API image payload.
function read_all_qc(img::CciaImage)
    root = qc_root(img); out = Dict{String,Any}()
    isdir(root) || return out
    for fun in readdir(root)
        fdir = joinpath(root, fun); isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") || continue
            out[string(fun, "/", f[1:end-5])] = _read_qc_file(joinpath(fdir, f))
        end
    end
    out
end

# All QC docs for an image AS THE USER SEES THEM: the persisted sidecars (read_all_qc) PLUS a computed
# calibration fallback for images imported before metadata QC was banked — so the flag here matches the
# image table's indicator. Persisted wins when present. ONE source for the API image payload, the
# session briefing, and any severity roll-up (don't re-merge these two elsewhere).
function all_qc_docs(img::CciaImage)
    docs = read_all_qc(img)
    key  = "importImages.omezarr/" * VERSIONED_DEFAULT_VAL
    if !haskey(docs, key)
        docs[key] = Dict{String,Any}("funName" => "importImages.omezarr",
            "valueName" => VERSIONED_DEFAULT_VAL, "findings" => metadata_qc_findings(img.meta))
    end
    docs
end

# ── Image calibration QC (metadata warnings) ─────────────────────────────────────
# The image's physical-size/timing calibration is a QC concern like any other: missing or
# untrustworthy values get advisory `warn` findings under `importImages.omezarr`, so ONE source
# (qc.jl) drives the image-table indicator, the whiteboard, the lab log and MCP — instead of the
# frontend re-deriving them from the payload (the old `imageMetadataWarnings.ts fieldIssues`, now
# retired). Each finding carries `detail.field ∈ {x,y,z,t}` so the physical-size fix dialog keeps its
# per-field highlight. Findings are RECOMPUTED wherever calibration changes (import / resync /
# metadata edit) via `write_metadata_qc!`, so a fixed image clears its warning. See QC_OBSERVER_PLAN.

# Z-step vs XY pixel-size sanity band — a ratio outside this is far more likely a unit-conversion bug
# (e.g. an ImageJ TIFF calibrated in inch) than a real voxel geometry. Mirrors imageMetadataWarnings.ts.
const _Z_RATIO_MIN = 0.02
const _Z_RATIO_MAX = 50

# Materiality floor for the clipping FINDING: the fraction of a channel's voxels sitting at the
# detector ceiling above which it is worth interrupting someone.
#
# **This is a chosen number, not a fitted one, and it should be re-set by the first real case.**
# `intensity_utils.is_saturated` decides whether a channel clipped AT ALL — structural, measured, and
# it stays the gate on detection. But detection is not the same as materiality: measured over all 36
# channels of the nine `kSUFux` movies (one session), the four it flags hold 415-534 voxels of ~377 M
# at the ceiling — 1.1-1.4e-6. That is real clipping of trivial extent, and "lower the gain" is not
# an actionable thing to tell someone about 500 voxels, on 4 of every 9 imports.
#
# 1e-4 is ~37 000 voxels on an image that size: unambiguously truncated structure rather than a few
# hot cells, and ~70x above the worst trace case observed. Nothing in that session would warn. It is
# set from what would damage a MEASUREMENT, because there is no material case in hand to fit to.
#
# The metric (`saturation_metrics`) is banked regardless of this floor: a trace-level count is still
# worth having, because the cohort comparison is relative and will surface an image clipping far more
# than its session peers without anyone choosing an absolute level.
const _SATURATION_WARN_FRAC = 1e-4

_cal_num(v) = v === nothing ? nothing : (v isa Real ? Float64(v) : tryparse(Float64, string(v)))
_cal_int(v, default::Int) = (n = _cal_num(v); n === nothing ? default : round(Int, n))
_cal_txt(v) = (v === nothing || (v isa AbstractString && isempty(v))) ? nothing : string(v)

"""
    metadata_qc_findings(meta) -> Vector

Advisory calibration findings for one image, from its `meta` dict (String keys: `SizeZ`/`SizeT`,
`PhysicalSizeX/Y/Z`, `PhysicalSizeUnit`, `PhysicalSizeZ_raw` presence = auto-corrected,
`TimeIncrement`/`TimeIncrementUnit`). PURE → unit-tested. Faithful port of the old frontend
`fieldIssues`: missing Z spacing / auto-corrected Z / unusual Z ratio / missing frame interval /
unit-less interval / unit-less pixel size. All `warn`; each carries `detail.field`.
"""
function metadata_qc_findings(meta::AbstractDict)
    size_z    = _cal_int(get(meta, "SizeZ", nothing), 1)
    size_t    = _cal_int(get(meta, "SizeT", nothing), 1)
    phys_x    = _cal_num(get(meta, "PhysicalSizeX", nothing))
    phys_z    = _cal_num(get(meta, "PhysicalSizeZ", nothing))
    phys_y    = _cal_num(get(meta, "PhysicalSizeY", nothing))
    phys_unit = _cal_txt(get(meta, "PhysicalSizeUnit", nothing))
    t_incr    = _cal_num(get(meta, "TimeIncrement", nothing))
    t_unit    = _cal_txt(get(meta, "TimeIncrementUnit", nothing))
    z_corr    = haskey(meta, "PhysicalSizeZ_raw")

    fs = Dict{String,Any}[]
    # `key` defaults to the code; the z-axis unit case is the one place they differ.
    mf(code, field; key = code) =
        push!(fs, qc_finding("warn", code; key = key, detail = Dict{String,Any}("field" => field)))

    # Z spacing — the first applicable case only (mirrors the frontend if/elseif chain)
    if size_z > 1 && phys_z === nothing
        mf("metadata.z_spacing_unknown", "z")
    elseif z_corr
        mf("metadata.z_spacing_corrected", "z")
    elseif phys_z !== nothing && phys_x !== nothing && phys_x > 0
        ratio = phys_z / phys_x
        (ratio < _Z_RATIO_MIN || ratio > _Z_RATIO_MAX) && mf("metadata.z_spacing_unusual", "z")
    end

    # frame interval
    if size_t > 1 && t_incr === nothing
        mf("metadata.frame_interval_unknown", "t")
    elseif t_incr !== nothing && t_unit === nothing
        mf("metadata.frame_interval_no_unit", "t")
    end

    # spatial unit — one PhysicalSizeUnit covers x/y/z; flag whichever axes carry a value
    if phys_unit === nothing
        phys_x !== nothing && mf("metadata.pixel_size_no_unit", "x")
        phys_y !== nothing && mf("metadata.pixel_size_no_unit", "y")
        (phys_z !== nothing && !any(f -> f["detail"]["field"] == "z", fs)) &&
            mf("metadata.pixel_size_no_unit", "z"; key = "metadata.voxel_depth_no_unit")
    end
    fs
end

"""
    import_metrics(meta) -> Union{Dict,Nothing}

Objective, cohort-comparable metrics EVERY import produces: channel/Z/T counts (`nChannels`/`nZ`/`nT`)
from the OME metadata (`SizeC`/`SizeZ`/`SizeT`). An image whose channel count or dimensionality
differs from its cohort peers is an outlier (wrong file imported / acquisition misconfigured).
`nothing` only for pre-metadata images carrying none of these keys. This is the base import metric
the QC convention requires of every result-producing task (MODULES.md → QC).
"""
function import_metrics(meta::AbstractDict)
    d = Dict{String,Any}()
    for (key, name) in (("SizeC", "nChannels"), ("SizeZ", "nZ"), ("SizeT", "nT"))
        v = _cal_int(get(meta, key, nothing), -1)
        v >= 0 && (d[name] = v)
    end
    isempty(d) ? nothing : d
end

"""
    saturation_qc_findings(meta) -> Vector

One `warn` per channel that clipped at acquisition **materially** — detected by
`intensity_utils.is_saturated` AND with at least `_SATURATION_WARN_FRAC` of its voxels at the ceiling.
From the persisted `meta["saturation"]` (written by `ImportOmezarr`, which checks every import).
PURE → unit-tested. Empty when the check didn't run (an image imported before it existed, or a
non-integer store), and empty for trace-level clipping — which the METRICS still record.

Advisory, like all QC — but this is the one import finding a user can only act on *before* the
experiment: clipped values are gone, so no correction, threshold or rescale recovers them. Hence the
imperative long text points at the acquisition, not at a parameter.

Detection lives in `intensity_utils.is_saturated` and is structural (a pile-up in the brightest
occupied bin), so it holds for a 12-bit sensor stored in 16-bit words — where the obvious
"fraction at the dtype maximum" test reads 0 on visibly clipped data.
"""
function saturation_qc_findings(meta::AbstractDict)
    fs = Dict{String,Any}[]
    for ch in _saturation_channels(meta)
        get(ch, "saturated", false) === true || continue
        # detected AND material — see _SATURATION_WARN_FRAC. A trace pile-up is recorded in the
        # metrics but does not raise a finding.
        frac = _cal_num(get(ch, "topFrac", nothing))
        (isnothing(frac) || frac < _SATURATION_WARN_FRAC) && continue
        i = _cal_int(get(ch, "index", nothing), 0)
        push!(fs, qc_finding("warn", "import.channel_saturated"; channel = i,
            # the COUNT, not a percentage: measured on a real session the clipped fraction is ~1e-6,
            # which rounds to "0.0001%" and tells the user nothing. "534 voxels at 4095" is judgeable.
            detail = Dict{String,Any}("channel"        => i,
                                      "topValue"       => _cal_num(get(ch, "topValue", nothing)),
                                      "clippedVoxels"  => _cal_num(get(ch, "topCount", nothing)))))
    end
    fs
end

# `meta["saturation"]["channels"]`, normalised to String-keyed Dicts. JSON3 hands back Symbol keys
# (CLAUDE.md → JSON3 gotcha), and this is read from the persisted ccid on every QC recompute.
function _saturation_channels(meta::AbstractDict)::Vector{Dict{String,Any}}
    s = get(meta, "saturation", nothing)
    s isa AbstractDict || return Dict{String,Any}[]
    chans = get(Dict{String,Any}(String(k) => v for (k, v) in s), "channels", nothing)
    chans isa AbstractVector || return Dict{String,Any}[]
    out = Dict{String,Any}[]
    for ch in chans
        ch isa AbstractDict && push!(out, Dict{String,Any}(String(k) => v for (k, v) in ch))
    end
    out
end

"""
    saturation_metrics(meta) -> Union{Dict,Nothing}

Cohort-comparable saturation counts: `nChannelsSaturated` and `maxClippedFrac` (the worst channel's
clipped fraction). `nothing` when the check didn't run.

Cohort-comparable because both describe the ACQUISITION, not a parameter anyone typed — so an image
clipping far more than its session peers is a real gain/expression difference. Measured across nine
movies from one session, 4 had a clipped channel and 5 did not, which is exactly the spread an
outlier flag should surface.
"""
function saturation_metrics(meta::AbstractDict)
    chans = _saturation_channels(meta)
    isempty(chans) && return nothing
    n = 0; worst = 0.0
    for ch in chans
        get(ch, "saturated", false) === true && (n += 1)
        v = _cal_num(get(ch, "topFrac", nothing))
        isnothing(v) || (worst = max(worst, v))
    end
    Dict{String,Any}("nChannelsSaturated" => n, "maxClippedFrac" => worst)
end

# Compute + persist an image's import QC. Re-reads the PERSISTED ccid meta (not the possibly stale
# in-memory `img.meta`) so it's correct from any call site — import, resync, metadata edit. Writes
# even when clean (empty findings), so a fixed image overwrites its stale warning. Recomputed from
# persisted meta, so findings and metrics never diverge.
function write_metadata_qc!(img::CciaImage)
    ccid = state_file(img)
    isfile(ccid) || return
    raw      = read_ccid_raw(ccid)
    meta     = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    findings = vcat(metadata_qc_findings(meta), saturation_qc_findings(meta))

    metrics = import_metrics(meta)
    sm      = saturation_metrics(meta)
    isnothing(sm) || (metrics = merge(isnothing(metrics) ? Dict{String,Any}() : metrics, sm))

    if isnothing(metrics)
        write_qc(img, "importImages.omezarr", VERSIONED_DEFAULT_VAL, findings)
    else
        write_qc(img, "importImages.omezarr", VERSIONED_DEFAULT_VAL, findings; metrics = metrics)
    end
end

# ── Objective count metrics (QC banking) ─────────────────────────────────────────
# Tasks bank objective counts (cells measured, tracks + mean length) into the qc/ sidecar so future
# cohort stats can flag anomalies (a run that produced 10× fewer cells/tracks than usual). There is
# no cohort threshold yet, so a count is recorded as a METRIC (the doc's `metrics` field) — the
# bankable datum — with an advisory `finding` only for the unambiguous "produced nothing" case.

"""
    track_count_metrics(track_ids) -> (n_tracks, mean_length, n_tracked_cells)

From a per-cell `track_id` vector, count distinct tracks, mean cells-per-track, and total tracked
cells. Untracked cells (`missing`/`nothing`/`NaN`/`≤ 0`) are ignored — matching the `track_id > 0`
"tracked" convention used across gating/pop_df. Pure (no I/O) so it's unit-tested directly.
"""
function track_count_metrics(track_ids)
    counts = Dict{Int,Int}()
    for t in track_ids
        (ismissing(t) || t === nothing) && continue
        (t isa Real && isnan(t)) && continue
        ti = t isa Integer ? Int(t) : Int(round(t))
        ti > 0 || continue
        counts[ti] = get(counts, ti, 0) + 1
    end
    n_tracks = length(counts)
    n_cells  = sum(values(counts); init = 0)
    mean_len = n_tracks == 0 ? 0.0 : n_cells / n_tracks
    (n_tracks, mean_len, n_cells)
end

# One HMM state holding ≥ this fraction of an image's cells is flagged as dominant. (Clustering uses
# its own _CLUSTER_DOMINANT_FRAC — a single cluster is a weaker signal than a single behavioural
# state, so the thresholds differ.) See hmm_states_qc_findings.
const _DOMINANT_FRAC = 0.95

"""
    category_dist_metrics(vals) -> (; n, n_distinct, dominant_frac)

Distribution stats for one image's categorical/state column (PURE → unit-tested). Counts the valid
entries (skips `nothing`/`missing`/`NaN`), the number of distinct values, and the fraction in the
single most common value (dominance). Shared by HMM states (numeric state codes) + HMM transitions
(string labels) — an image whose cells collapse into one state/transition is a QC signal.
"""
function category_dist_metrics(vals)
    counts = Dict{Any,Int}(); n = 0
    for v in vals
        (v === nothing || ismissing(v)) && continue
        (v isa Real && isnan(v)) && continue
        counts[v] = get(counts, v, 0) + 1; n += 1
    end
    n == 0 && return (; n = 0, n_distinct = 0, dominant_frac = 0.0)
    (; n = n, n_distinct = length(counts), dominant_frac = maximum(values(counts)) / n)
end

"""
    hmm_states_qc_findings(m) -> Vector

Advisory findings for one image's HMM state assignment, from `category_dist_metrics` `m`
(PURE → unit-tested). Most-severe first:
  • no cells decoded (`n == 0`) ⇒ warn (tracks too short / measurements incomplete).
  • all decoded cells in one state (`n_distinct ≤ 1`) ⇒ warn (image didn't switch states — check
    it's the same acquisition/measurements, or the model has too few states).
  • one state holds ≥ `_DOMINANT_FRAC` of cells ⇒ info (check it's really this uniform).
"""
function hmm_states_qc_findings(m)
    findings = Dict{String,Any}[]
    if m.n == 0
        push!(findings, qc_finding("warn", "hmm.no_states_decoded"))
    elseif m.n_distinct <= 1
        push!(findings, qc_finding("warn", "hmm.single_state"))
    elseif m.dominant_frac >= _DOMINANT_FRAC
        push!(findings, qc_finding("info", "hmm.dominant_state";
            pct = round(Int, 100 * m.dominant_frac),
            detail = Dict{String,Any}("dominantStateFrac" => round(m.dominant_frac; digits = 3))))
    end
    findings
end

"""
    hmm_transitions_qc_findings(m) -> Vector

Advisory finding for one image's HMM transitions, from `category_dist_metrics` `m` (PURE). Only the
unambiguous "no transitions" case flags (warn) — transition dominance isn't clearly actionable.
"""
function hmm_transitions_qc_findings(m)
    m.n == 0 ? [qc_finding("warn", "hmm.no_transitions")] : Dict{String,Any}[]
end

"""
    track_measures_qc_findings(n_tracks, dims_param, resolved_dims, auto_dims, confidence, reason) -> Vector

Advisory findings for a track-measures run (PURE → unit-tested). Surfaces the motion-dimensionality
call the task already computes: when `dims_param == "auto"` and the detector's `confidence` is "low",
the z axis couldn't be classified as migration vs jitter — feeding an ambiguous z corrupts turning
angle + speed (see track_measures.jl `_detect_motion_dims`), so it's flagged for review. A confident
auto call (or a user-set dims) flags nothing. `reason` is carried into the finding detail.
"""
function track_measures_qc_findings(n_tracks::Integer, dims_param::AbstractString,
                                    resolved_dims::Integer, auto_dims::Integer,
                                    confidence::AbstractString, reason::AbstractString = "")
    (lowercase(strip(String(dims_param))) == "auto" && String(confidence) == "low") ?
        [qc_finding("warn", "tracking.motion_dims_uncertain"; dims = resolved_dims,
            detail = Dict{String,Any}("resolvedDims" => Int(resolved_dims),
                                      "autoDims" => Int(auto_dims), "reason" => String(reason)))] :
        Dict{String,Any}[]
end

# QC thresholds for a clustering run (clustPops/clustTracks). A single dominant cluster holding this
# fraction of an image's points suggests under-clustering (resolution too low / features don't
# separate). See cluster_qc_findings.
const _CLUSTER_DOMINANT_FRAC = 0.9

"""
    cluster_qc_findings(n_clusters_total, n_here, n_clusters_here, largest_frac; unit="cells")

Advisory findings for ONE image's slice of a set-wide clustering run (PURE → unit-tested). The run
clusters all images together, so `n_clusters_total` is the run's cluster count; `n_here` /
`n_clusters_here` / `largest_frac` describe how THIS image's points landed. Degenerate patterns,
most-severe first:
  • run collapsed to ≤ 1 cluster ⇒ warn (resolution too low / features don't separate).
  • this image's points all fell into one cluster while the run found several ⇒ warn (the image
    separated from the cohort — a batch/normalisation outlier).
  • one cluster holds ≥ `_CLUSTER_DOMINANT_FRAC` of this image's points ⇒ info (check it's really
    that uniform, or raise resolution).
`unit` is "cells" (clustPops) or "tracks" (clustTracks) for the message. Returns a findings vector.
"""
function cluster_qc_findings(n_clusters_total::Integer, n_here::Integer, n_clusters_here::Integer,
                             largest_frac::Real; unit::AbstractString = "cells")
    findings = Dict{String,Any}[]
    if n_clusters_total <= 1
        push!(findings, qc_finding("warn", "clustering.single_cluster"))
    elseif n_here > 0 && n_clusters_here <= 1
        push!(findings, qc_finding("warn", "clustering.image_one_cluster"; unit = unit))
    elseif n_here > 0 && largest_frac >= _CLUSTER_DOMINANT_FRAC
        push!(findings, qc_finding("info", "clustering.dominant_cluster";
            pct = round(Int, 100 * largest_frac), unit = unit,
            detail = Dict{String,Any}("largestClusterFrac" => round(largest_frac; digits = 3))))
    end
    findings
end

# Bank a clustering run's QC from the per-segment JSON its Python runner wrote (see
# clustering_utils.split_back_and_write → qcOutPath). Shared by clustPops + clustTracks (both are
# set-scope, cluster all images at once, and write one QC doc per (image, segmentation) keyed by the
# segmentation value_name). `unit` = "cells" (clustPops) or "tracks" (clustTracks) — drives the
# banked count key (nCells/nTracks) and the finding wording. Best-effort; never fails the task.
function write_cluster_qc!(imgs::AbstractVector, fun_name::AbstractString, qc_out_path::AbstractString;
                           unit::AbstractString = "cells", suffix::AbstractString = "",
                           on_log::Function = _ -> nothing)
    isfile(qc_out_path) || return
    img_by_uid = Dict(img.uid => img for img in imgs)
    count_key = unit == "tracks" ? "nTracks" : "nCells"
    try
        qc    = JSON3.read(read(qc_out_path, String))
        total = Int(get(qc, :nClusters, 0))
        segs  = get(qc, :perSegment, ())
        for seg in segs
            uid = string(get(seg, :uID, "")); vn = string(get(seg, :valueName, ""))
            img = get(img_by_uid, uid, nothing); img === nothing && continue
            n    = Int(get(seg, :n, 0))
            nc   = Int(get(seg, :nClusters, 0))
            frac = Float64(get(seg, :largestClusterFrac, 0.0))
            findings = cluster_qc_findings(total, n, nc, frac; unit = unit)
            # Bank per (LABEL SET × RUN): a clustering run is identified by its suffix (e.g. "movement"
            # /"test"), and the same label set (vn, e.g. "T"/"B") can be clustered by many runs. Keying
            # QC under the label set alone made a later run OVERWRITE an earlier one — the cohort could
            # then only ever judge the newest. Bank under the composite `{labelSet}.{suffix}` so every
            # run's QC is retained and cohort-checkable independently (the button's run selector picks
            # one). `runSuffix`/`labelSet` are stored explicitly so callers group by run without parsing
            # the composite key (a label set could itself contain a dot). See project_cluster_pop_scoping.
            qc_vn = isempty(suffix) ? vn : string(vn, ".", suffix)
            write_qc(img, fun_name, qc_vn, findings;
                     runSuffix = string(suffix), labelSet = vn,
                     metrics = Dict{String,Any}(count_key => n, "nClusters" => nc,
                         "largestClusterFrac" => round(frac; digits = 4), "nClustersTotal" => total))
        end
        on_log("[QC] $total cluster(s) over $(length(segs)) segment(s).")
    catch e
        on_log("[QC] could not compute cluster QC: $e")
    end
end

# Reusable spatial check — flag an output whose XY canvas grew abnormally vs its source. Shapes are in
# `dim_order` (e.g. "TCZYX"). Generic across any spatially-transforming task (drift/AF correction, …);
# returns a finding or `nothing`. Default threshold 25% (normal drift expands XY ≤~15%).
function qc_canvas_expansion(source_shape, output_shape, dim_order::AbstractString;
                             threshold_pct::Real = 25, code::AbstractString = "output.canvas_expansion")
    order = collect(dim_order)
    yi = findfirst(==('Y'), order); xi = findfirst(==('X'), order)
    (isnothing(yi) || isnothing(xi)) && return nothing
    pct(i) = source_shape[i] > 0 ? 100 * (output_shape[i] - source_shape[i]) / source_shape[i] : 0.0
    ye, xe = pct(yi), pct(xi); m = max(ye, xe)
    m > threshold_pct || return nothing
    # The `code` varies by caller (drift.canvas_expansion), so the catalog key is pinned.
    qc_finding("warn", code; key = "output.canvas_expansion", pct = round(Int, m),
        detail = Dict{String,Any}("yExpansionPct" => round(ye, digits = 1),
                                  "xExpansionPct" => round(xe, digits = 1)))
end
