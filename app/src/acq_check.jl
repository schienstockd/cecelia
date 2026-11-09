# Test-acquisition check over a set (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the import probe's numbers
# (`meta["saturation"]` — photons, shared structure, drift, per-depth profile) and the acquisition
# settings (`meta["acquisition"]`), read against the user's PLAN for the set — which objects they will
# segment or track, in which channel per label design, and what they will compare. Pure over facts:
# `acq_image_facts` reads one image's meta, `acq_check(plan, facts)` gives the findings. The route and
# the Blackboard entry live in api/src/routes/acq_check.jl.

# ── Placeholder thresholds — read off two stand-in sets, to be set on more ─────────────────────────
#: brightest ÷ dimmest plan object (p99 photons on its objects) that flags an imbalance
const ACQ_BALANCE_RATIO = 2.0
#: shared-structure r between an object's channel and another plan channel that flags it
const ACQ_SHARED_R = 0.5
#: p99 change from the top plane to the bottom one (%) that flags dimming with depth, Bright Z off
const ACQ_DEPTH_FALL_PCT = -15.0
#: median photons per voxel on an object's channel below which it is photon-starved
const ACQ_FEW_PHOTONS = 3.0
#: photons the detector range holds below which the gain is too high for the light collected
const ACQ_CEILING_PHOTONS = 20.0
#: relative difference in pixel size treated as a different setting
const ACQ_REL_TOLERANCE = 0.01

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
    acq_image_facts(uid, name, meta; channel_names = String[]) -> Dict

What the check reads from one image: settings (`lasers`, `brightZ`, `objective`, `pixelSize`), per
channel the depth change and photons (`channels[i]`), the shared-structure r per channel pair (`shared`,
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
    filters = acq isa AbstractDict ? Dict(i - 1 => _ag(c, "emissionFilter") for (i, c) in enumerate(_ag(acq, "channels", Any[]))) : Dict()
    chans = Dict{Int,Dict{String,Any}}()
    probed = false
    for (i, ch) in enumerate(sat isa AbstractDict ? _ag(sat, "channels", Any[]) : Any[])
        idx = something(_anum(_ag(ch, "index")), Float64(i - 1)) |> Int
        p99 = _ag(_ag(_ag(ch, "profile"), "byZ"), "p99")
        depth = (p99 isa AbstractVector && length(p99) > 1 && _anum(p99[1]) !== nothing && p99[1] > 0) ?
                100 * (Float64(p99[end]) / Float64(p99[1]) - 1) : nothing
        ph = _ag(ch, "photons")
        objs = _ag(ph, "objects")
        probed |= ph isa AbstractDict
        chans[idx] = Dict{String,Any}(
            "name" => idx + 1 <= length(channel_names) ? String(channel_names[idx + 1]) : "Channel $(idx + 1)",
            "filter" => get(filters, idx, nothing), "depthChangePct" => depth,
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
        "lasers" => lasers, "brightZ" => bz isa Bool ? bz : nothing, "objective" => obj, "pixelSize" => px,
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

_acq_lasers_text(l) = l === nothing || isempty(l) ? "—" :
    join((string(round(Int, w)) for (w, _) in l), "/") * " nm · " *
    join((p === nothing ? "?" : string(round(p; digits = 1)) |> s -> replace(s, r"\.0$" => "") for (_, p) in l), "/") * " %"

"""
    acq_check(plan, facts) -> Dict

The check. `plan` may be `nothing` (no plan yet): then only what needs none runs — settings that differ,
dimming with depth, detector range, drift, unmeasured movies. Returns
`{findings, rows, ran, skipped}`: `findings` are `qc_finding`s with `scope` ("set" or an image uid),
`thisData` (what it means for analysing these movies — the `.data` catalog entry's `long`, when there is one) and
`detail`; `rows` one per image for the per-movie table.
"""
function acq_check(plan, facts::AbstractVector)::Dict{String,Any}
    has_plan = plan isa AbstractDict && !isempty(_ag(plan, "objects", Any[]))
    findings = Dict{String,Any}[]
    add!(level, code, scope; data = nothing, detail = nothing, subs...) = begin
        f = qc_finding(level, code; detail = detail, subs...)
        f["scope"] = scope
        data === nothing || (f["thisData"] = qc_text(data; subs...).long)
        push!(findings, f)
    end
    probed = filter(f -> f["probed"], facts)
    unprobed = length(facts) - length(probed)
    unprobed > 0 && add!("info", "acq.not_measured", "set"; n = unprobed, s = unprobed == 1 ? "" : "s",
                         detail = Dict("imageUids" => [f["uid"] for f in facts if !f["probed"]]))

    design = Dict(f["uid"] => (has_plan ? _acq_design_of(plan, f) : f["designKey"]) for f in facts)
    unnamed = [f["uid"] for f in facts if isempty(f["designKey"])]
    isempty(unnamed) || add!("warn", "acq.channels_unnamed", "set"; detail = Dict("imageUids" => unnamed))

    # settings that differ between movies — and whether they split along the designs
    for (variable, get_v, same) in (
            ("laser setting", f -> f["lasers"], ==),
            ("Bright Z setting", f -> f["brightZ"], ==),
            ("objective", f -> f["objective"], ==),
            ("pixel size", f -> f["pixelSize"], (a, b) -> a !== nothing && b !== nothing && abs(a - b) <= ACQ_REL_TOLERANCE * max(abs(a), abs(b))))
        known = filter(f -> get_v(f) !== nothing, facts)
        length(known) < 2 && continue
        groups = Vector{Vector{Dict{String,Any}}}()
        for f in known
            g = findfirst(g -> same(get_v(g[1]), get_v(f)), groups)
            g === nothing ? push!(groups, [f]) : push!(groups[g], f)
        end
        length(groups) < 2 && continue
        by_design = all(g -> length(unique(design[f["uid"]] for f in g)) == 1, groups) &&
                    length(unique(design[f["uid"]] for f in known)) > 1
        value_of(v) = variable == "laser setting" ? _acq_lasers_text(v) : variable == "Bright Z setting" ? (v ? "on" : "off") : string(v)
        add!("warn", by_design ? "acq.settings_by_design" : "acq.settings_differ", "set"; variable = variable,
             data = by_design ? "acq.settings_by_design.data" : nothing,
             detail = Dict("groups" => [Dict("value" => value_of(get_v(g[1])), "imageUids" => [f["uid"] for f in g],
                                             "designs" => unique(something(design[f["uid"]], "") for f in g)) for g in groups]))
    end

    # which channels count: the plan's objects, else every channel
    # the plan's objects; without a plan none — photons, balance and shared structure need to know which
    # channel is what. Depth and detector range read the objects' channels, else every channel.
    obj_ch(f, m) = has_plan ? _acq_channels(plan, "objects", design[f["uid"]], f, m) : Tuple{String,Int}[]
    all_ch(f) = sort!(collect(keys(f["channels"])))
    ref_ch(f, m) = has_plan ? _acq_channels(plan, "references", design[f["uid"]], f, m) : Tuple{String,Int}[]
    plan_objs = has_plan ? _ag(plan, "objects", Any[]) : Any[]
    kind_of(name) = (i = findfirst(o -> string(_ag(o, "name", "")) == name, plan_objs);
                     i === nothing ? "cells" : string(_ag(plan_objs[i], "kind", "cells")))

    rows = Dict{String,Any}[]
    depth_hits = Dict{String,Any}[]; few = Dict{String,Vector{Float64}}(); ceil_hits = Dict{String,Any}[]
    balance = Dict{String,Any}[]; shared_hits = Dict{String,Any}[]; ref_hits = Dict{String,Any}[]
    drift_ok = Float64[]
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
        dz = [f["channels"][c]["depthChangePct"] for c in chs if f["channels"][c]["depthChangePct"] !== nothing]
        row["depthPct"] = isempty(dz) ? nothing : [minimum(dz), maximum(dz)]
        if f["brightZ"] === false && !isempty(dz) && minimum(dz) <= ACQ_DEPTH_FALL_PCT
            push!(depth_hits, Dict("uid" => f["uid"], "pct" => minimum(dz)))
            add!("warn", "acq.depth_falls_image", f["uid"]; pct = round(Int, minimum(dz)))
        end
        p99 = Tuple{String,Float64}[]
        for (name, c) in objs
            ch = get(f["channels"], c, nothing)
            ch === nothing && continue
            row["objects"][name] = Dict("channel" => c, "p50" => ch["p50Photons"], "p99" => ch["p99Photons"])
            ch["p99Photons"] === nothing || push!(p99, (name, ch["p99Photons"]))
            ch["p50Photons"] === nothing || push!(get!(few, name, Float64[]), ch["p50Photons"])
        end
        for c in chs
            cp = f["channels"][c]["ceilingPhotons"]
            cp !== nothing && cp < ACQ_CEILING_PHOTONS && push!(ceil_hits, Dict("uid" => f["uid"], "channel" => c, "photons" => cp))
        end
        if length(p99) >= 2 && minimum(last.(p99)) > 0
            lo = argmin(last.(p99)); ratio = maximum(last.(p99)) / p99[lo][2]
            row["ratio"] = ratio; row["dimmest"] = p99[lo][1]
            ratio >= ACQ_BALANCE_RATIO && push!(balance, Dict("uid" => f["uid"], "ratio" => ratio, "dimmest" => p99[lo][1]))
        else
            row["ratio"] = nothing; row["dimmest"] = nothing
        end
        # an object's channel showing in another plan channel: another object's (identity at risk) or a
        # reference's (autofluorescence / leak to correct for)
        rmax = nothing
        for (i, (a, ca)) in enumerate(objs), (b, cb) in vcat(objs[i+1:end], refs)
            r = get(f["shared"], "$(min(ca, cb))-$(max(ca, cb))", nothing)
            r === nothing && continue
            rmax = rmax === nothing ? r : max(rmax, r)
            r >= ACQ_SHARED_R || continue
            is_ref = any(x -> x[1] == b, refs)
            hit = Dict("uid" => f["uid"], "a" => a, "b" => b, "r" => r)
            push!(is_ref ? ref_hits : shared_hits, hit)
            add!(is_ref ? "info" : "warn", is_ref ? "acq.shares_reference_image" : "acq.shares_object_image", f["uid"];
                 what = "$a ~ $b", value = round(r; digits = 2), data = is_ref ? "acq.shares_reference.data" : "acq.shares_object.data")
        end
        row["sharedR"] = rmax
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
        else
            row["registers"] = nothing
        end
        push!(rows, row)
    end

    isempty(depth_hits) || add!("warn", "acq.depth_falls", "set"; n = length(depth_hits), s = length(depth_hits) == 1 ? "" : "s",
                                 pct = round(Int, minimum(h["pct"] for h in depth_hits)), detail = Dict("images" => depth_hits))
    isempty(ceil_hits) || add!("warn", "acq.gain_high", "set"; n = round(Int, minimum(h["photons"] for h in ceil_hits)),
                                detail = Dict("images" => ceil_hits))
    for (name, xs) in sort!(collect(few); by = first)
        med = sort(xs)[cld(length(xs), 2)]
        kind_of(name) == "points" && continue          # a point object is counted, not read for its noise
        med < ACQ_FEW_PHOTONS && add!("warn", "acq.few_photons", "set"; what = name, value = round(med; digits = 1),
                                       data = "acq.few_photons.data", detail = Dict("medianP50Photons" => med, "n" => length(xs)))
    end
    if !isempty(balance)
        dims = [b["dimmest"] for b in balance]
        top = argmax(n -> count(==(n), dims), unique(dims))
        add!("warn", "acq.balance", "set"; what = top, n = count(==(top), dims), count = length(probed),
             value = round(maximum(b["ratio"] for b in balance); digits = 1), data = "acq.balance.data",
             detail = Dict("images" => balance))
    end
    for (hits, code, data) in ((shared_hits, "acq.shares_object", "acq.shares_object.data"),
                               (ref_hits, "acq.shares_reference", "acq.shares_reference.data"))
        isempty(hits) && continue
        for pair in unique((h["a"], h["b"]) for h in hits)
            hs = filter(h -> (h["a"], h["b"]) == pair, hits)
            add!(code == "acq.shares_object" ? "warn" : "info", code, "set"; what = "$(pair[1]) ~ $(pair[2])",
                 n = length(hs), s = length(hs) == 1 ? "" : "s", value = round(maximum(h["r"] for h in hs); digits = 2), data = data,
                 detail = Dict("images" => hs))
        end
    end
    isempty(drift_ok) || add!("ok", "acq.drift_ok", "set"; n = length(drift_ok), value = round(Int, maximum(drift_ok)))

    ran = String["settings", "depth", "detector range", "drift"]
    skipped = String[]
    has_plan ? append!(ran, ["photons", "balance", "shared structure"]) :
               append!(skipped, ["photons", "balance", "shared structure"])
    Dict{String,Any}("findings" => findings, "rows" => rows, "ran" => ran, "skipped" => skipped,
                     "nImages" => length(facts), "nMeasured" => length(probed))
end

"""The warnings a check result still has open — the badge count and the Blackboard entry's sections."""
acq_open(result) = [f for f in result["findings"] if f["level"] == "warn"]
