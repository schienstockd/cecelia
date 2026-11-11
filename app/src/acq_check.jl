# Test-acquisition check over a set (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the import probe's numbers
# (`meta["saturation"]` — photons, shared structure, drift) and the acquisition
# settings (`meta["acquisition"]`), read against the user's PLAN for the set — which objects they will
# segment or track, in which channel per label design, and what they will compare. Pure over facts:
# `acq_image_facts` reads one image's meta, `acq_check(plan, facts)` gives the findings. The route and
# the Blackboard entry live in api/src/routes/acq_check.jl.

# ── Bands, not cut-offs ─────────────────────────────────────────────────────────────────────────
# Each value sorts into fine / borderline (an info note) / a problem (a warning — the verdict's steps).
# Bands are set by shot noise or against the set itself, not read off one set by eye; the value is
# always shown. docs/todo/ACQUISITION_TEST_CHECK_PLAN.md → "Thresholds: relative to the set, not fixed".
#: shot-noise SNR per voxel — √(median photons on an object), the best a photon count allows (Poisson:
#: the noise on N photons is √N) — below which an object is a problem / borderline
const ACQ_SNR_BANDS = (problem = 1.5, borderline = 2.5)
#: headroom — the detector range ÷ the objects' p99 photons — below which bright cells clip / may clip
const ACQ_HEADROOM_BANDS = (problem = 2.0, borderline = 4.0)
#: how far a channel pair's shared-structure r stands above its movie's baseline (the median r over
#: the movie's plan channel pairs) to be a problem / borderline. Needs three pairs for a baseline.
const ACQ_SHARED_EXCESS = (problem = 0.25, borderline = 0.1)
#: a baseline this high: every channel shares structure, so a leak can't be told from shared tissue
#: or excitation (one line exciting all channels) — a note asking for a control
const ACQ_SHARED_BASELINE_HIGH = 0.4
#: relative difference in pixel size treated as a different setting (float tolerance)
const ACQ_REL_TOLERANCE = 0.01

# a value against bands where low is bad (`b.problem < b.borderline`): :problem, :borderline or :fine
_acq_band_low(v, b) = v < b.problem ? :problem : v < b.borderline ? :borderline : :fine
# a value against bands where high is bad (`b.problem > b.borderline`)
_acq_band_high(v, b) = v >= b.problem ? :problem : v >= b.borderline ? :borderline : :fine
const _ACQ_BAND_LEVEL = Dict(:problem => "warn", :borderline => "info")
_acq_snr(p50) = sqrt(max(p50, 0.0))
_acq_r2(x) = round(x; digits = 2)

# What each finding is about, for the reader and the model review (labels in qc/text.jl, `acq.cat.<id>`).
# The review uses the same ones, plus `fading` (signal lost over the movie — no rule reads it yet) and `other`.
const ACQ_CATEGORIES = ("movies", "settings", "depth", "photons", "balance", "shared", "drift", "fading", "other")
const ACQ_CATEGORY_OF = Dict(
    "acq.not_measured" => "movies", "acq.channels_unnamed" => "movies", "acq.channel_missing" => "movies",
    "acq.not_in_plan" => "movies",
    "acq.settings_differ" => "settings", "acq.settings_within_design" => "settings",
    "acq.settings_by_design" => "settings", "acq.settings_by_design_cross" => "settings",
    "acq.brightz_ramp_unused" => "depth", "acq.brightz_ramp_unused_image" => "depth", "acq.brightz_off" => "depth",
    "acq.gain_high" => "photons", "acq.few_photons" => "photons", "acq.balance" => "balance",
    "acq.shares_object" => "shared", "acq.shares_object_image" => "shared",
    "acq.shares_reference" => "shared", "acq.shares_reference_image" => "shared",
    "acq.shares_all" => "shared", "acq.shares_no_baseline" => "shared", "acq.dim_movie" => "photons",
    "acq.drift_unreliable" => "drift", "acq.drift_ok" => "drift", "acq.drift_far" => "drift")

# a field of a String- or Symbol-keyed dict (meta read back through JSON3 can be either)
_ag(d, k, default = nothing) = d isa AbstractDict ? something(get(d, k, nothing), get(d, Symbol(k), nothing), Some(default)) : default
_anum(v) = v isa Real ? Float64(v) : nothing

# A default channel name — what an image has before anyone names its channels ("CH1", "C2", "Channel 3")
const _ACQ_DEFAULT_CHANNEL = r"^(?i)(ch|c|channel)[ _-]?\d+$"

"""
    acq_design_key(channel_names) -> String

The label design of an image: its channel names in channel order (`"P14-CTDR · empty-570 · …"`). Movies
with the same list share a design. Unnamed or default names (`CH1`…) give `""` — every such movie falls
in one design, and the check asks for names rather than guessing.
"""
function acq_design_key(names::AbstractVector)::String
    (isempty(names) || all(n -> occursin(_ACQ_DEFAULT_CHANNEL, strip(string(n))) || isempty(strip(string(n))), names)) && return ""
    join((strip(string(n)) for n in names), " · ")
end

"""
    acq_bright_z_ramp(bz) -> Union{String,Nothing}

The Bright Z ramp saved with a movie (`acquisition.brightZ.points`: transmission and HV per depth), as
text — `"12→15 % over 35 µm"` (lasers joined by `/`, HV added when it changes) — or `nothing` when no
point differs from the first (no ramp). Whether Bright Z was ON is `brightZ.enabled`, read separately.
"""
function acq_bright_z_ramp(bz)::Union{String,Nothing}
    pts = [p for p in _ag(bz, "points", Any[]) if p isa AbstractDict]
    length(pts) < 2 && return nothing
    sort!(pts; by = p -> something(_anum(_ag(p, "zFromStartUm")), 0.0))
    # per laser (or detector), the values that were READ, in depth order — a value the reader couldn't
    # read is unknown, not a change, so it is skipped (not a NaN that differs from everything). A laser
    # read at fewer than two points says nothing about a ramp.
    series(k) = begin
        ds = [_ag(p, k) for p in pts]
        ks = unique!(sort!([string(x) for d in ds if d isa AbstractDict for x in keys(d)]))
        vs = [Float64[v for v in (_anum(_ag(d, x)) for d in ds) if v !== nothing] for x in ks]
        filter(v -> length(v) >= 2, vs)
    end
    num(v) = replace(string(round(v; digits = 1)), r"\.0$" => "")
    tx = series("transmissionPct"); hv = series("hv")
    ramps(xs) = any(v -> any(!=(v[1]), v), xs)
    tx_ramps = ramps(tx); hv_ramps = ramps(hv)
    (tx_ramps || hv_ramps) || return nothing
    dz = something(_anum(_ag(pts[end], "zFromStartUm")), 0.0) - something(_anum(_ag(pts[1], "zFromStartUm")), 0.0)
    parts = String[]
    tx_ramps && push!(parts, join((num(v[1]) for v in tx), "/") * "→" * join((num(v[end]) for v in tx), "/") * " %")
    hv_ramps && push!(parts, "HV " * join(unique(num(v[1]) for v in hv), "/") * "→" * join(unique(num(v[end]) for v in hv), "/"))
    join(parts, ", ") * " over " * num(dz) * " µm"
end

# Whether any Bright Z point has a transmission or HV value the reader couldn't read.
_acq_ramp_has_gaps(bz) = any(p -> p isa AbstractDict && any(k -> (d = _ag(p, k); d isa AbstractDict &&
                                                                  any(v -> !(v isa Real), values(d))), ("transmissionPct", "hv")),
                             _ag(bz, "points", Any[]))

"""
    acq_image_facts(uid, name, meta; channel_names = String[]) -> Dict

What the check reads from one image: settings (`lasers`, `brightZ`, `objective`, `pixelSize`), per
channel the photons (`channels[i]`), the Bright Z ramp saved with the movie (`brightZRamp`, text or `nothing`), the shared-structure r per channel pair (`shared`,
"a-b" keys), drift (`drift`) and whether the signal pass ran (`probed`). Absent fields stay `nothing`.
"""
function acq_image_facts(uid::AbstractString, name::AbstractString, meta::AbstractDict;
                         channel_names::AbstractVector = String[])::Dict{String,Any}
    acq = _ag(meta, "acquisition")
    sat = _ag(meta, "saturation")
    lasers = acq isa AbstractDict ? sort!([(something(_anum(_ag(l, "wavelengthNm")), 0.0), _anum(_ag(l, "transmissionPct")))
                                          for l in _ag(acq, "lasers", Any[]) if l isa AbstractDict]; by = first) : nothing
    bz = acq isa AbstractDict ? _ag(_ag(acq, "brightZ"), "enabled") : nothing
    obj = acq isa AbstractDict ? _ag(_ag(acq, "objective"), "name") : nothing
    # A camera's per-channel settings (`lif_meta`): cube, exposure, gain, lamp step — what scales a widefield
    # image's intensities the way laser % and HV scale a scanned one. `nothing` without a camera.
    camera = acq isa AbstractDict && _ag(acq, "camera") isa AbstractDict ?
        [(_ag(c, "filterCube"), _anum(_ag(c, "exposureMs")), _anum(_ag(c, "gain")), _anum(_ag(c, "illuminationIntensity")))
         for c in _ag(acq, "channels", Any[]) if c isa AbstractDict] : nothing
    filters = acq isa AbstractDict ? Dict(i - 1 => _ag(c, "emissionFilter") for (i, c) in enumerate(_ag(acq, "channels", Any[]))) : Dict()
    chans = Dict{Int,Dict{String,Any}}()
    probed = false
    for (i, ch) in enumerate(sat isa AbstractDict ? _ag(sat, "channels", Any[]) : Any[])
        idx = something(_anum(_ag(ch, "index")), Float64(i - 1)) |> Int
        ph = _ag(ch, "photons")
        objs = _ag(ph, "objects")
        probed |= ph isa AbstractDict
        chans[idx] = Dict{String,Any}(
            "name" => idx + 1 <= length(channel_names) ? String(channel_names[idx + 1]) : "Channel $(idx + 1)",
            "filter" => get(filters, idx, nothing),
            "greyPerPhoton" => _anum(_ag(_ag(ph, "fit"), "greyPerPhoton")),
            "p50Photons" => _anum(_ag(objs, "p50Photons")), "p99Photons" => _anum(_ag(objs, "p99Photons")),
            "ceilingPhotons" => _anum(_ag(objs, "ceilingPhotons")))
    end
    shared = Dict{String,Float64}()
    for s in (sat isa AbstractDict ? something(_ag(sat, "sharedStructure"), Any[]) : Any[])
        a, b, r = _anum(_ag(s, "a")), _anum(_ag(s, "b")), _anum(_ag(s, "r"))
        (a === nothing || b === nothing || r === nothing) && continue
        shared["$(Int(min(a, b)))-$(Int(max(a, b)))"] = r
    end
    px = _anum(_ag(meta, "PhysicalSizeX"))
    d = sat isa AbstractDict ? _ag(sat, "drift") : nothing
    drift = nothing
    if d isa AbstractDict && _ag(d, "untestable") === nothing
        ex = _ag(d, "excursionPx", Any[]); ax = _ag(d, "axes", Any[])
        xy = [Float64(e) for (a, e) in zip(ax, ex) if string(a) in ("Y", "X")]
        xypx = isempty(xy) ? nothing : sqrt(sum(abs2, xy))
        drift = Dict{String,Any}("residualRmsPx" => _anum(_ag(d, "residualRmsPx")), "maxDriftPx" => xypx,
                                 "maxDriftUm" => (xypx === nothing || px === nothing) ? nothing : xypx * px)
    end
    nch = max(length(channel_names), isempty(chans) ? 0 : maximum(keys(chans)) + 1)
    Dict{String,Any}("uid" => String(uid), "name" => String(name), "designKey" => acq_design_key(channel_names),
        "channelNames" => String[i <= length(channel_names) ? String(channel_names[i]) : "Channel $i" for i in 1:nch],
        "lasers" => lasers, "camera" => camera, "brightZ" => bz isa Bool ? bz : nothing,
        "brightZRamp" => acq isa AbstractDict ? acq_bright_z_ramp(_ag(acq, "brightZ")) : nothing,
        # no ramp found while some ramp value went unread: whether one was saved is unknown
        "brightZRampKnown" => !(acq isa AbstractDict) || acq_bright_z_ramp(_ag(acq, "brightZ")) !== nothing ||
                              !_acq_ramp_has_gaps(_ag(acq, "brightZ")), "objective" => obj, "pixelSize" => px,
        "channels" => chans, "shared" => shared, "drift" => drift, "probed" => probed)
end

"""Facts for every image of a set, in set order."""
acq_set_facts(set) = [acq_image_facts(img.uid, img.name, img.meta;
                                      channel_names = something(channel_names(img), String[])) for img in images(set)]

"""
    acq_suggest_designs(facts) -> Vector{Dict}

One design per distinct channel-name list (`acq_design_key`), in order of first appearance: `{id, label,
key, imageUids, channels: [{index, name, filter}]}` (channels from its first image). The plan form's designs.
"""
function acq_suggest_designs(facts)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    for f in facts
        d = findfirst(x -> x["key"] == f["designKey"], out)
        if d === nothing
            chans = [Dict{String,Any}("index" => i, "name" => c["name"], "filter" => c["filter"])
                     for (i, c) in sort!(collect(f["channels"]); by = first)]
            push!(out, Dict{String,Any}("id" => "d$(length(out) + 1)", "label" => isempty(f["designKey"]) ? "Channels not named" : f["designKey"],
                                        "key" => f["designKey"], "imageUids" => String[f["uid"]], "channels" => chans))
        else
            push!(out[d]["imageUids"], f["uid"])
        end
    end
    out
end

# The plan's design an image belongs to: by key, else the only design, else nothing.
function _acq_design_of(plan, f)
    ds = _ag(plan, "designs", Any[])
    i = findfirst(d -> string(_ag(d, "key", "")) == f["designKey"], ds)
    i !== nothing && return string(_ag(ds[i], "id"))
    length(ds) == 1 ? string(_ag(ds[1], "id")) : nothing
end

# The plan's objects (or references) with their channel in design `did`, resolved on image `f` through
# `channel_index` (a name, or an index as is): [(name, index)]. A channel the image doesn't have goes to
# `missing` as (object, channel) — reported, never dropped silently.
function _acq_channels(plan, field, did, f, missing)
    out = Tuple{String,Int}[]
    did === nothing && return out
    for o in _ag(plan, field, Any[])
        c = _ag(_ag(o, "channels"), did)
        c === nothing && continue
        name = string(_ag(o, "name", ""))
        try
            i = channel_index(c isa Real ? Int(c) : string(c), f["channelNames"]; what = name)
            0 <= i < length(f["channelNames"]) || error("channel $i out of range")   # an index passes through unchecked
            push!(out, (name, i))
        catch
            push!(missing, (name, string(c)))
        end
    end
    out
end

"""A camera's per-channel settings as one line: "A4 122.5 ms gain 2 lamp 3 / Y5 …"."""
_acq_camera_text(c) = c === nothing || isempty(c) ? "—" :
    join((join(filter(!isempty, [something(cube, "?") |> string,
                                 ms === nothing ? "" : "$(round(ms; digits = 1)) ms",
                                 g === nothing ? "" : "gain $(round(g; digits = 2))",
                                 il === nothing ? "" : "lamp $(round(il; digits = 1))"]), " ")
          for (cube, ms, g, il) in c), " / ")

_acq_lasers_text(l) = l === nothing || isempty(l) ? "—" :
    join((string(round(Int, w)) for (w, _) in l), "/") * " nm · " *
    join((p === nothing ? "?" : string(round(p; digits = 1)) |> s -> replace(s, r"\.0$" => "") for (_, p) in l), "/") * " %"

# The verdict's steps, from the rule warnings: per finding code, what to do at a `test` acquisition and on
# `final` data (`nothing` = no step at that stage). `{whats}` is every finding of the code's `what`,
# listed ("OTI, P14 and gBT"); `{variable}` its setting. In the order the verdict lists them.
const ACQ_VERDICT_STEPS = [
    "acq.not_measured"             => (test = "measure the remaining movies", final = "measure the remaining movies"),
    "acq.channels_unnamed"         => (test = "name the channels", final = "name the channels"),
    "acq.few_photons"              => (test = "collect more light for {whats}", final = "denoise {whats}"),
    "acq.gain_high"                => (test = "lower the detector gain", final = "check bright cells for clipping"),
    "acq.shares_object"            => (test = "separate the {whats} channels", final = "correct bleed-through between {whats}"),
    "acq.balance"                  => (test = "brighten {whats}", final = "check detection per object"),
    "acq.settings_within_design"   => (test = "use one {variable} per design", final = nothing),
    "acq.settings_differ"          => (test = "use one {variable} for every compared movie", final = nothing),
    "acq.settings_by_design_cross" => (test = "put every compared object in each design", final = "compare within each design"),
    "acq.brightz_ramp_unused"      => (test = "turn Bright Z on", final = "check whether cells dim with depth"),
    "acq.drift_unreliable"         => (test = "stabilise the preparation", final = "check the movies that don't register for jumps"),
    "acq.drift_ok"                 => (test = nothing, final = "drift-correct before tracking")]

"""
    acq_verdict(findings, stage) -> (steps, text)

The rule part of the verdict on a plan: one step per warning code with a step at this stage (see
`ACQ_VERDICT_STEPS`; drift correction on final data from the ok finding too), and the sentence they make —
"Denoise OTI, P14 and gBT; drift-correct before tracking." — or, with none, "Ready to acquire as planned." /
"Analyse as planned.". The model review adds at most one step of its own after it.
"""
function acq_verdict(findings::AbstractVector, stage::AbstractString)
    steps = String[]
    for (code, st) in ACQ_VERDICT_STEPS
        t = getproperty(st, Symbol(stage))
        t === nothing && continue
        fs = filter(f -> f["code"] == code && f["scope"] == "set" && (f["level"] == "warn" || code == "acq.drift_ok"), findings)
        isempty(fs) && continue
        sub(k) = unique(string(get(get(f, "subs", Dict()), k, "")) for f in fs)
        for v in (occursin("{variable}", t) ? sub("variable") : [""])
            push!(steps, replace(t, "{whats}" => _acq_list(filter(!isempty, sub("what"))), "{variable}" => v))
        end
    end
    text = isempty(steps) ? (stage == "final" ? "Analyse as planned." : "Ready to acquire as planned.") :
                            uppercasefirst(join(steps, "; ")) * "."
    steps, text
end

_acq_list(xs) = length(xs) <= 1 ? join(xs) : join(xs[1:end-1], ", ") * " and " * xs[end]

"""
    acq_stage(plan) -> "test" | "final"

Where the plan's movies stand: a `test` acquisition, the rest still to be acquired (the default), or the
`final` data, all there is to analyse. It picks which action a finding leads with.
"""
acq_stage(plan) = plan isa AbstractDict && string(_ag(plan, "stage", "test")) == "final" ? "final" : "test"

"""
    acq_check(plan, facts) -> Dict

The check. `plan` may be `nothing` (no plan yet): then only what needs none runs — settings that differ,
Bright Z, detector range, drift, unmeasured movies. Returns
`{stage, steps, verdict, findings, rows, ran, skipped}` (`steps`/`verdict`: `acq_verdict`): `findings` are `qc_finding`s with `scope` ("set" or an image uid),
`thisData` (what it means for the analysis — the `.data` catalog entry's `long`, when there is one; on
`final` data that IS the finding's `long`, and `thisData` is dropped),
`where` (which movies or designs hold which value, for a setting that differs; for movies also
`whereIds`, the same with image uids) and
`detail`; `rows` one per image for the per-movie table.
"""
function acq_check(plan, facts::AbstractVector)::Dict{String,Any}
    has_plan = plan isa AbstractDict && !isempty(_ag(plan, "objects", Any[]))
    plan_objs = has_plan ? _ag(plan, "objects", Any[]) : Any[]
    findings = Dict{String,Any}[]
    add!(level, code, scope; data = nothing, where = nothing, detail = nothing, subs...) = begin
        f = qc_finding(level, code; detail = detail, subs...)
        f["scope"] = scope
        f["category"] = get(ACQ_CATEGORY_OF, code, "other")
        # which movies (or designs) hold which value; the line's label says which it lists
        # `where` is the text, or for movies a function of the field naming them: the names as `where`,
        # the uids as `whereIds` (the dialog shows either — a set of long names reads as a wall)
        where === nothing || (f["where"] = where isa Function ? where("name") : where;
                              where isa Function && (f["whereIds"] = where("uid"));
                              f["whereLabel"] = startswith(code, "acq.settings_by_design") ? "Designs" : "Movies")
        # what it means for analysing these movies: the code's `.data` entry (a movie's finding reads its
        # set finding's), unless a call names one ("" = none)
        dk = something(data, replace(code, r"_image$" => "") * ".data")
        haskey(QC_TEXT, dk) && (f["thisData"] = qc_text(dk; subs...).long)
        push!(findings, f)
    end
    probed = filter(f -> f["probed"], facts)
    unprobed = length(facts) - length(probed)
    unprobed > 0 && add!("info", "acq.not_measured", "set"; n = unprobed, s = unprobed == 1 ? "" : "s",
                         detail = Dict("imageUids" => [f["uid"] for f in facts if !f["probed"]]))

    design = Dict(f["uid"] => (has_plan ? _acq_design_of(plan, f) : f["designKey"]) for f in facts)
    unnamed = [f["uid"] for f in facts if isempty(f["designKey"])]
    isempty(unnamed) || add!("warn", "acq.channels_unnamed", "set"; detail = Dict("imageUids" => unnamed))

    # settings that differ — within a design (movies compared directly were imaged differently), and
    # between designs (matters only to a comparison that crosses designs). Bright Z is read on its own,
    # below: its one fix is the same whichever way it splits.
    dlabel = Dict{Any,String}()
    for f in facts
        id = design[f["uid"]]
        haskey(dlabel, id) && continue
        pd = has_plan ? findfirst(d -> string(_ag(d, "id", "")) == string(id), _ag(plan, "designs", Any[])) : nothing
        lab = pd === nothing ? (id === nothing ? "" : string(id)) : string(something(_ag(_ag(plan, "designs")[pd], "label"), _ag(_ag(plan, "designs")[pd], "key"), ""))
        dlabel[id] = has_plan && id === nothing ? "Not in the plan" : isempty(lab) ? "Channels not named" : lab
    end
    # a movie whose channel names match none of the plan's designs: say so, and keep it out of every
    # design's comparison (its channels can't be read as the plan's objects)
    outside = has_plan ? filter(f -> design[f["uid"]] === nothing, facts) : Dict{String,Any}[]
    isempty(outside) || add!("warn", "acq.not_in_plan", "set"; n = length(outside), s = length(outside) == 1 ? "" : "s",
                             where = k -> join((f[k] for f in outside), ", "), detail = Dict("imageUids" => [f["uid"] for f in outside]))
    # does the plan's comparison stay inside one design? Every compared object has a channel in every design
    cmp_names = has_plan ? (isempty(_ag(plan, "compare", Any[])) ? [string(_ag(o, "name", "")) for o in plan_objs] :
                            string.(_ag(plan, "compare", Any[]))) : String[]
    pdesigns = has_plan ? [string(_ag(d, "id", "")) for d in _ag(plan, "designs", Any[])] : String[]
    within_only = has_plan && all(cmp_names) do n
        o = findfirst(x -> string(_ag(x, "name", "")) == n, plan_objs)
        o !== nothing && all(d -> _ag(_ag(plan_objs[o], "channels"), d) !== nothing, pdesigns)
    end
    group_by(fs, get_v, same) = begin
        gs = Vector{Vector{Dict{String,Any}}}()
        for f in fs
            g = findfirst(g -> same(get_v(g[1]), get_v(f)), gs)
            g === nothing ? push!(gs, [f]) : push!(gs[g], f)
        end
        gs
    end
    where_of(gs, value_of) = k -> join((value_of(g[1]) * ": " * join((f[k] for f in g), ", ") for g in gs), " · ")
    for (variable, get_v, same) in (
            ("laser setting", f -> f["lasers"], ==),
            ("camera setting", f -> get(f, "camera", nothing), ==),
                # on/off, and whether a ramp is saved — not the ramp's text, which an unread value changes;
                # a movie whose ramp is unknown (values unread) sits out
            ("Bright Z setting", f -> (f["brightZ"] === nothing || !f["brightZRampKnown"]) ? nothing :
                                    (f["brightZ"], f["brightZRamp"] !== nothing), ==),
            ("objective", f -> f["objective"], ==),
            ("pixel size", f -> f["pixelSize"], (a, b) -> a !== nothing && b !== nothing && abs(a - b) <= ACQ_REL_TOLERANCE * max(abs(a), abs(b))))
        known = filter(f -> get_v(f) !== nothing && !(has_plan && design[f["uid"]] === nothing), facts)
        length(known) < 2 && continue
        value_of(f) = (v = get_v(f); variable == "laser setting" ? _acq_lasers_text(v) :
                       variable == "camera setting" ? _acq_camera_text(v) :
                       variable == "Bright Z setting" ? (v[1] ? "on" : "off") * (v[2] ? " · ramp saved" : "") : string(v))
        groups_detail(gs) = Dict("groups" => [Dict("value" => value_of(g[1]), "imageUids" => [f["uid"] for f in g]) for g in gs])
        gs = group_by(known, get_v, same)
        length(gs) < 2 && continue
        ids = unique(design[f["uid"]] for f in known)
        # within each design
        for id in ids
            dg = group_by(filter(f -> design[f["uid"]] == id, known), get_v, same)
            length(dg) < 2 && continue
            # one design in the set (or channels not named): every movie is compared with every other
            length(ids) == 1 ?
                add!("warn", "acq.settings_differ", "set"; variable = variable, where = where_of(dg, value_of), detail = groups_detail(dg)) :
                add!("warn", "acq.settings_within_design", "set"; variable = variable, what = dlabel[id],
                     where = where_of(dg, value_of), detail = merge(groups_detail(dg), Dict("design" => something(id, ""))))
        end
        # between designs: the designs' sets of values differ
        length(ids) < 2 && continue
        vals(id) = Set(value_of(f) for f in known if design[f["uid"]] == id)
        allequal(vals(id) for id in ids) && continue
        per_design = join((dlabel[id] * ": " * join(sort!(collect(vals(id))), ", ") for id in ids), " · ")
        if within_only || !has_plan
            add!("info", "acq.settings_by_design", "set"; variable = variable, where = per_design,
                 data = has_plan ? "acq.settings_by_design.data" : "", detail = groups_detail(gs))
        else
            add!("warn", "acq.settings_by_design_cross", "set"; variable = variable, where = per_design,
                 detail = groups_detail(gs))
        end
    end

    # which channels count: the plan's objects, else every channel
    # the plan's objects; without a plan none — photons, balance and shared structure need to know which
    # channel is what. Depth and detector range read the objects' channels, else every channel.
    obj_ch(f, m) = has_plan ? _acq_channels(plan, "objects", design[f["uid"]], f, m) : Tuple{String,Int}[]
    all_ch(f) = sort!(collect(keys(f["channels"])))
    ref_ch(f, m) = has_plan ? _acq_channels(plan, "references", design[f["uid"]], f, m) : Tuple{String,Int}[]
    kind_of(name) = (i = findfirst(o -> string(_ag(o, "name", "")) == name, plan_objs);
                     i === nothing ? "cells" : string(_ag(plan_objs[i], "kind", "cells")))

    rows = Dict{String,Any}[]
    few = Dict{String,Vector{Float64}}(); p50_by = Dict{Tuple{String,Any},Dict{String,Float64}}()
    head_hits = Dict{String,Any}[]
    shared_hits = Dict{String,Any}[]; ref_hits = Dict{String,Any}[]
    high_base = Dict{String,Any}[]; no_base = String[]
    drift_ok = Float64[]; drift_by = Dict{String,Float64}()
    for f in facts
        missing = Tuple{String,String}[]
        objs = obj_ch(f, missing); refs = ref_ch(f, missing)
        for (o, c) in missing
            add!("warn", "acq.channel_missing", f["uid"]; what = o, variable = c)
        end
        row = Dict{String,Any}("uid" => f["uid"], "name" => f["name"], "design" => design[f["uid"]],
                               "lasers" => _acq_lasers_text(f["lasers"]), "brightZ" => f["brightZ"],
                               "probed" => f["probed"], "objects" => Dict{String,Any}())
        chs = has_plan ? [c for (_, c) in objs if haskey(f["channels"], c)] : all_ch(f)
        row["brightZRamp"] = f["brightZRamp"]
        f["brightZ"] === false && f["brightZRamp"] !== nothing &&
            add!("warn", "acq.brightz_ramp_unused_image", f["uid"]; what = f["brightZRamp"])
        p99 = Tuple{String,Float64}[]
        for (name, c) in objs
            ch = get(f["channels"], c, nothing)
            ch === nothing && continue
            row["objects"][name] = Dict("channel" => c, "p50" => ch["p50Photons"], "p99" => ch["p99Photons"])
            ch["p99Photons"] === nothing || push!(p99, (name, ch["p99Photons"]))
            if ch["p50Photons"] !== nothing
                push!(get!(few, name, Float64[]), ch["p50Photons"])
                get!(p50_by, (name, design[f["uid"]]), Dict{String,Float64}())[f["uid"]] = ch["p50Photons"]
            end
        end
        # headroom: the detector range against the brightest object voxels in the same channel
        for c in chs
            ch = f["channels"][c]; cp = ch["ceilingPhotons"]; hi = ch["p99Photons"]
            (cp === nothing || hi === nothing || hi <= 0) && continue
            band = _acq_band_low(cp / hi, ACQ_HEADROOM_BANDS)
            band == :fine || push!(head_hits, Dict("uid" => f["uid"], "channel" => c, "photons" => cp,
                                                   "headroom" => cp / hi, "band" => string(band)))
        end
        # brightest ÷ dimmest object, for the row; whether it matters is read from their photons, below
        if length(p99) >= 2 && minimum(last.(p99)) > 0
            lo = argmin(last.(p99))
            row["ratio"] = maximum(last.(p99)) / p99[lo][2]; row["dimmest"] = p99[lo][1]
        else
            row["ratio"] = nothing; row["dimmest"] = nothing
        end
        # shared structure: an object's channel against another object's (identity at risk) or a
        # reference's (autofluorescence / leak to correct for), read against the movie's own baseline —
        # the median r over its plan channel pairs, what tissue and excitation make every pair share
        pairs = Tuple{String,String,Float64,Bool}[]
        for (i, (a, ca)) in enumerate(objs), (b, cb) in vcat(objs[i+1:end], refs)
            r = get(f["shared"], "$(min(ca, cb))-$(max(ca, cb))", nothing)
            r === nothing || push!(pairs, (a, b, r, any(x -> x[1] == b, refs)))
        end
        row["sharedR"] = isempty(pairs) ? nothing : maximum(p[3] for p in pairs)
        base = length(pairs) >= 3 ? median([p[3] for p in pairs]) : nothing
        row["sharedBase"] = base
        if base === nothing
            isempty(pairs) || push!(no_base, f["uid"])
        else
            base >= ACQ_SHARED_BASELINE_HIGH && push!(high_base, Dict("uid" => f["uid"], "base" => base))
            for (a, b, r, is_ref) in pairs
                band = _acq_band_high(r - base, ACQ_SHARED_EXCESS)
                # a reference is there to show what leaks or glows: only a pair that stands well out
                (band == :fine || (is_ref && band != :problem)) && continue
                push!(is_ref ? ref_hits : shared_hits, Dict("uid" => f["uid"], "a" => a, "b" => b, "r" => r,
                                                            "base" => base, "band" => string(band)))
                add!(is_ref ? "info" : _ACQ_BAND_LEVEL[band], is_ref ? "acq.shares_reference_image" : "acq.shares_object_image", f["uid"];
                     what = "$a ~ $b", value = _acq_r2(r), base = _acq_r2(base),
                     data = is_ref ? "acq.shares_reference.data" : "acq.shares_object.data")
            end
        end
        dr = f["drift"]
        row["driftUm"] = dr === nothing ? nothing : dr["maxDriftUm"]
        if dr !== nothing && dr["residualRmsPx"] !== nothing
            if dr["residualRmsPx"] > DRIFT_RESIDUAL_WARN_PX
                add!("warn", "acq.drift_unreliable", f["uid"]; value = round(dr["residualRmsPx"]; digits = 1))
                row["registers"] = false
            else
                row["registers"] = true
                dr["maxDriftUm"] === nothing || push!(drift_ok, dr["maxDriftUm"])
            end
            dr["maxDriftUm"] === nothing || (drift_by[f["uid"]] = dr["maxDriftUm"])
        else
            row["registers"] = nothing
        end
        push!(rows, row)
    end

    # Bright Z off: with a ramp saved it was meant to be on (a warning, naming the movies per ramp);
    # with none, nothing says the depth was compensated — a note. Whether cells really dim with depth is
    # read after segmentation, on the cells (whole-plane intensity mixes cell count with brightness).
    unused = filter(f -> f["brightZ"] === false && f["brightZRamp"] !== nothing, facts)
    if !isempty(unused)
        rg = Vector{Vector{Dict{String,Any}}}()
        for f in unused
            g = findfirst(g -> g[1]["brightZRamp"] == f["brightZRamp"], rg)
            g === nothing ? push!(rg, [f]) : push!(rg[g], f)
        end
        add!("warn", "acq.brightz_ramp_unused", "set"; n = length(unused), s = length(unused) == 1 ? "" : "s",
             where = k -> join((g[1]["brightZRamp"] * ": " * join((f[k] for f in g), ", ") for g in rg), " · "),
             detail = Dict("imageUids" => [f["uid"] for f in unused]))
    end
    noramp = filter(f -> f["brightZ"] === false && f["brightZRamp"] === nothing, facts)
    isempty(noramp) || add!("info", "acq.brightz_off", "set"; n = length(noramp), s = length(noramp) == 1 ? "" : "s",
                            where = k -> join((f[k] for f in noramp), ", "), detail = Dict("imageUids" => [f["uid"] for f in noramp]))
    if !isempty(head_hits)
        worst = argmin(h -> h["headroom"], head_hits)
        add!(any(h -> h["band"] == "problem", head_hits) ? "warn" : "info", "acq.gain_high", "set";
             n = round(Int, worst["photons"]), value = round(worst["headroom"]; digits = 1), detail = Dict("images" => head_hits))
    end
    # photons: the shot-noise SNR of each object's median voxel, across the set
    snr = Dict{String,Float64}()
    for (name, xs) in sort!(collect(few); by = first)
        kind_of(name) == "points" && continue          # a point object is counted, not read for its noise
        med = median(xs); snr[name] = _acq_snr(med)
        band = _acq_band_low(snr[name], ACQ_SNR_BANDS)
        band == :fine || add!(_ACQ_BAND_LEVEL[band], "acq.few_photons", "set"; what = name, value = round(med; digits = 1),
                              snr = round(snr[name]; digits = 2), data = "acq.few_photons.data",
                              detail = Dict("medianP50Photons" => med, "snr" => snr[name], "n" => length(xs)))
    end
    # balance: compared objects that differ in whether they have photons enough — the dimmer one is then
    # detected less well. A brightness ratio between objects that all have enough (or all too few) isn't one.
    cmp_snr = [(n, snr[n]) for n in cmp_names if haskey(snr, n)]
    if length(cmp_snr) >= 2
        rank(s) = (b = _acq_band_low(s, ACQ_SNR_BANDS); b == :problem ? 2 : b == :borderline ? 1 : 0)
        dim = argmin(last, cmp_snr); bright = argmax(last, cmp_snr)
        rank(dim[2]) > rank(bright[2]) &&
            add!(rank(dim[2]) == 2 ? "warn" : "info", "acq.balance", "set"; what = dim[1], variable = bright[1],
                 value = round(dim[2]; digits = 2), snr = round(bright[2]; digits = 2), data = "acq.balance.data",
                 detail = Dict("snr" => Dict(cmp_snr)))
    end
    by_uid = Dict(f["uid"] => f for f in facts)
    for (hits, code) in ((shared_hits, "acq.shares_object"), (ref_hits, "acq.shares_reference"))
        for pair in unique((h["a"], h["b"]) for h in hits)
            hs = filter(h -> (h["a"], h["b"]) == pair, hits)
            lvl = code == "acq.shares_object" && any(h -> h["band"] == "problem", hs) ? "warn" : "info"
            add!(lvl, code, "set"; what = "$(pair[1]) ~ $(pair[2])", n = length(hs), s = length(hs) == 1 ? "" : "s",
                 value = _acq_r2(maximum(h["r"] for h in hs)), base = _acq_r2(median([h["base"] for h in hs])),
                 data = code * ".data", detail = Dict("images" => hs))
        end
    end
    isempty(high_base) || add!("info", "acq.shares_all", "set"; n = length(high_base), s = length(high_base) == 1 ? "" : "s",
                               value = _acq_r2(median([h["base"] for h in high_base])),
                               where = k -> join((by_uid[h["uid"]][k] for h in high_base), ", "),
                               detail = Dict("images" => high_base))
    isempty(no_base) || add!("info", "acq.shares_no_baseline", "set"; n = length(no_base), s = length(no_base) == 1 ? "" : "s",
                             detail = Dict("imageUids" => no_base))
    # a movie out of line with the set's others (`_cohort_outliers`, robust z): an object dimmer than in
    # the other movies of its design (another design is another dye), more drift than the set's
    for ((name, _), vals) in sort!(collect(p50_by); by = x -> string(x[1]))
        kind_of(name) == "points" && continue
        o = _cohort_outliers(vals)
        for (uid, x) in sort!(collect(o.outliers); by = first)
            x["value"] < o.median && add!("info", "acq.dim_movie", uid; what = name, value = round(x["value"]; digits = 1),
                                          typical = round(o.median; digits = 1))
        end
    end
    let o = _cohort_outliers(drift_by)
        for (uid, x) in sort!(collect(o.outliers); by = first)
            x["value"] > o.median && add!("info", "acq.drift_far", uid; value = round(Int, x["value"]), typical = round(Int, o.median))
        end
    end
    isempty(drift_ok) || add!("ok", "acq.drift_ok", "set"; n = length(drift_ok), value = round(Int, maximum(drift_ok)))

    # final data: the movies are all there is, so a finding's action is what to do in the analysis
    stage = acq_stage(plan)
    if stage == "final"
        for f in findings
            haskey(f, "thisData") && (f["long"] = pop!(f, "thisData"))
        end
    end

    ran = String["settings", "Bright Z", "detector range", "drift"]
    skipped = String[]
    has_plan ? append!(ran, ["photons", "balance", "shared structure"]) :
               append!(skipped, ["photons", "balance", "shared structure"])
    steps, verdict = acq_verdict(findings, stage)
    Dict{String,Any}("stage" => stage, "steps" => steps, "verdict" => verdict, "findings" => findings, "rows" => rows, "ran" => ran, "skipped" => skipped,
                     "nImages" => length(facts), "nMeasured" => length(probed))
end

"""The warnings a check result still has open — the badge count and the Blackboard entry's sections."""
acq_open(result) = [f for f in result["findings"] if f["level"] == "warn"]
