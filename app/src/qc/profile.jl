# Depth and time findings from the import probe's per-plane profile (ACQUISITION_ADVISOR_PLAN P1).
#
# `meta["saturation"]["channels"][i]["profile"]` holds, per depth (`byZ`) and per frame (`byT`), the
# median p50 / p99, zero fraction and voxels at the channel ceiling (`intensity_utils.PlaneProfile`).
# These helpers turn it into `info` findings that STATE A MEASUREMENT ("brightest signal falls 46 %
# over the movie"), never a cause: bleaching, drift and cells leaving all look alike in these numbers,
# and the reader — or the acquisition advisor — separates them.
#
# **Every level below is a placeholder smoke alarm, set between quiet and clear channels of the only
# profiled movies so far** (one two-photon session, 3 movies, 12 channels): brightest-signal change over
# the movie −19…+46 %, background change 0.81…1.42×, brightest-signal change with depth +4…+58 %
# (brighter deeper in every channel: Bright Z), and 71–93 % of clipped voxels in the deeper half.
# Replace them once the plan's regression set (P7) exists. One finding per kind per image, listing the
# channels — the photon-limited shape: a setting shared across channels shows up in several at once.

using Statistics: median

const _PROFILE_ENDS          = 3      # planes / frames averaged (median) at each end
const _PROFILE_MIN_STEPS     = 6      # fewer planes / frames than this: no trend to read
const _FADE_OVER_TIME        = 0.35   # brightest signal (p99) falls by this fraction over the movie
const _BACKGROUND_RISE       = 1.3    # median (p50) at the end over the start
const _DEPTH_CHANGE          = 0.35   # p99 deepest over shallowest, either way, by this fraction
const _CLIPPED_DEEP_SHARE    = 2 / 3  # share of a channel's clipped voxels in the deeper half

_profile_series(prof, axis, key) = begin
    a = prof isa AbstractDict ? get(prof, axis, nothing) : nothing
    a isa AbstractDict || return Float64[]
    v = get(Dict{String,Any}(String(k) => x for (k, x) in a), key, nothing)
    v isa AbstractVector ? Float64[something(_cal_num(x), NaN) for x in v] : Float64[]
end

# median of the first / last `_PROFILE_ENDS` entries; nothing when too short or not finite
function _profile_ends(v::Vector{Float64})
    length(v) < _PROFILE_MIN_STEPS && return nothing
    a = median(v[1:_PROFILE_ENDS]); b = median(v[end-_PROFILE_ENDS+1:end])
    (isfinite(a) && isfinite(b) && a > 0) ? (a, b) : nothing
end

"""
    profile_trends(meta) -> Vector{Dict}

Per channel with a profile: `fade` (1 − p99 end/start over frames), `backgroundRise` (p50 end/start
over frames), `depthChange` (p99 deepest/shallowest − 1) and `clippedDeepShare` (share of clipped
voxels in the deeper half of the stack). A key is absent when its series is too short or empty. PURE.
"""
function profile_trends(meta::AbstractDict)
    out = Dict{String,Any}[]
    for ch in _saturation_channels(meta)
        prof = get(ch, "profile", nothing)
        prof isa AbstractDict || continue
        prof = Dict{String,Any}(String(k) => v for (k, v) in prof)
        r = Dict{String,Any}("channel" => _cal_int(get(ch, "index", nothing), 0))
        t99 = _profile_ends(_profile_series(prof, "byT", "p99"))
        isnothing(t99) || (r["fade"] = round(1 - t99[2] / t99[1], digits = 3))
        t50 = _profile_ends(_profile_series(prof, "byT", "p50"))
        isnothing(t50) || (r["backgroundRise"] = round(t50[2] / t50[1], digits = 3))
        z99 = _profile_ends(_profile_series(prof, "byZ", "p99"))
        isnothing(z99) || (r["depthChange"] = round(z99[2] / z99[1] - 1, digits = 3))
        cl = _profile_series(prof, "byZ", "clipped")
        if length(cl) >= 2 && all(isfinite, cl) && sum(cl) > 0
            h = length(cl) ÷ 2
            r["clippedDeepShare"] = round(sum(cl[h+1:end]) / sum(cl), digits = 3)
        end
        length(r) > 1 && push!(out, r)
    end
    out
end

# `meta` given: a depth finding, whose catalog key `_depth_text_key` picks
function _profile_finding(code, trends, key, hit, pct; meta = nothing)
    hits = [t for t in trends if haskey(t, key) && hit(t[key])]
    isempty(hits) && return nothing
    chans = [t["channel"] for t in hits]
    text_key = isnothing(meta) ? code : _depth_text_key(code, meta, chans)
    worst = maximum(pct(t[key]) for t in hits)
    n = length(hits)
    qc_finding("info", code; key = text_key, n = n, s = (n == 1 ? "" : "s"), pct = worst,
               channels = join(chans, ", "),
               detail = Dict{String,Any}("channels" => chans, "worstPct" => worst,
                                         "perChannel" => [Dict{String,Any}("channel" => t["channel"],
                                                                           key => t[key]) for t in hits]))
end

"""
    _depth_text_key(code, meta, chans) -> String

The catalog key for a depth finding over the flagged channels `chans`: `import.clipped_deep_export`
when every one of them clips at an 8-bit export's ceiling (`export_ceiling_channels` — not the
detector's, so no HV or ramp advice), else `_depth_ramp_key` in the image's technology's wording
(`qc_technology_key`).
"""
function _depth_text_key(code::AbstractString, meta::AbstractDict, chans)::String
    code == "import.clipped_deep" && all(in(export_ceiling_channels(meta)), chans) &&
        return "import.clipped_deep_export"
    qc_technology_key(_depth_ramp_key(code, meta), acquisition_technology(meta))
end

"""
    _depth_ramp_key(code, meta) -> String

The catalog key for a depth finding: `code * "_bright_z"` when the source's settings were read
(`meta["acquisition"]`) from an Evident `.oir` and record Bright Z switched ON, else `code` — whose text
names no vendor setting, because without that record the finding can't assume a ramp was used. Only
`.oir` gets the name: it is Evident's on every system the acquisition knowledge's `scopes` map lists
(FV4000, FVMPE-RS); Zeiss's ramp ("Auto Z brightness", `utils/acquisitionSettings.ts` →
`depthRampName`) has not been seen ON in a file, so it reads the generic text.
"""
function _depth_ramp_key(code::AbstractString, meta::AbstractDict)::String
    acq = get(meta, "acquisition", nothing)
    acq isa AbstractDict || return String(code)
    acq = Dict{String,Any}(String(k) => v for (k, v) in acq)
    bz = get(acq, "brightZ", nothing)
    on = bz isa AbstractDict && any(k -> String(k) == "enabled" && bz[k] === true, keys(bz))
    # no `source` = settings from before `.czi` was read, i.e. the `.oir` reader (`_acquisition_outdated`)
    (on && string(get(acq, "source", "oir")) == "oir") ? string(code, "_bright_z") : String(code)
end

"""
    profile_qc_findings(meta) -> Vector

`info` findings from `profile_trends`, one per kind that any channel crosses: signal fading over the
movie, background rising over it, brighter or darker with depth, and clipping concentrated deep. Empty
when no channel has a profile (an image probed before it existed — Refresh metadata re-probes). PURE.
"""
function profile_qc_findings(meta::AbstractDict)
    tr = profile_trends(meta)
    pct(x) = round(abs(x) * 100, digits = 0)
    fs = [
        _profile_finding("import.signal_fades", tr, "fade", ≥(_FADE_OVER_TIME), pct),
        _profile_finding("import.background_rises", tr, "backgroundRise", ≥(_BACKGROUND_RISE), x -> pct(x - 1)),
        _profile_finding("import.brighter_with_depth", tr, "depthChange", ≥(_DEPTH_CHANGE), pct; meta),
        _profile_finding("import.darker_with_depth", tr, "depthChange", ≤(-_DEPTH_CHANGE), pct; meta),
        _profile_finding("import.clipped_deep", tr, "clippedDeepShare", ≥(_CLIPPED_DEEP_SHARE), pct; meta),
    ]
    Dict{String,Any}[f for f in fs if !isnothing(f)]
end

"""
    profile_metrics(meta) -> Union{Dict,Nothing}

Cohort numbers from `profile_trends`: the largest fade over time and the largest depth change (signed
by the channel it came from). `nothing` when no channel has a profile.
"""
function profile_metrics(meta::AbstractDict)
    tr = profile_trends(meta)
    isempty(tr) && return nothing
    m = Dict{String,Any}()
    fades = [t["fade"] for t in tr if haskey(t, "fade")]
    isempty(fades) || (m["maxSignalFade"] = maximum(fades))
    dz = [t["depthChange"] for t in tr if haskey(t, "depthChange")]
    isempty(dz) || (m["maxDepthChange"] = dz[argmax(abs.(dz))])
    isempty(m) ? nothing : m
end
