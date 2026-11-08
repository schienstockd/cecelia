# Import from Immunemap — the two routes in front of the `importImages.immunemap` task. Same shape as
# legacy migration (`scan-legacy` / `register-legacy` in image.jl): a read-only preview, then a
# register step that creates one placeholder image per video carrying everything the download needs,
# then the per-image task does the work. Client + metadata mapping: `python/cecelia/utils/immunemap.py`.
# Design: `docs/todo/IMMUNEMAP_IMPORT_PLAN.md`.

# POST /api/import/scan-immunemap {experiment} → the experiment's manifest: conditions, and per video
# its size, calibration, channel labels, curated-track count, and whether it can be imported (a
# private video in a public experiment can't). Read-only; one request per video to Immunemap.
function api_import_scan_immunemap(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    exp_raw = get(body, :experiment, nothing)          # an id (number) or a URL (string)
    exp_ref = exp_raw === nothing ? "" : strip(string(exp_raw))
    isempty(exp_ref) && return 400, JSON3.write((; error="experiment required (id or Immunemap URL)"))

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "immunemap_scan.result.json")
    logs = String[]
    ok = try
        Cecelia.run_py("tasks/importImages/scan_immunemap_run.py",
                       (; experiment = exp_ref, resultPath = result_file), run_dir;
                       on_log = l -> push!(logs, l))
    catch e
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="scan failed: $(sprint(showerror, e))"))
    end
    if !(ok && isfile(result_file))
        tail = isempty(logs) ? "no output" : join(last(logs, 4), " | ")
        rm(run_dir; recursive=true, force=true)
        return 502, JSON3.write((; error="Could not read the experiment from Immunemap: $tail"))
    end
    manifest = read(result_file, String)
    rm(run_dir; recursive=true, force=true)
    200, manifest   # already JSON
end

# The per-image provenance kept under `meta["immunemap"]` — what the download step reads and what a
# reader needs to cite or re-fetch the video. Pure, so the mapping is tested without a project.
function immunemap_image_meta(video::AbstractDict, experiment::AbstractDict)::Dict{String,Any}
    g(d, k, default=nothing) = get(d, k, get(d, Symbol(k), default))
    Dict{String,Any}(
        "acquisitionId"  => g(video, "acquisitionId"),
        "videoUid"       => string(g(video, "videoUid", "")),
        "fileName"       => string(g(video, "fileName", "")),
        "sizeBytes"      => g(video, "sizeBytes"),
        "md5"            => g(video, "md5"),
        "sha1"           => g(video, "sha1"),
        "spacingXYZ"     => collect(Any, g(video, "spacingXYZ", Any[])),
        "intervalSec"    => g(video, "intervalSec"),
        "groupId"        => g(video, "groupId"),
        "condition"      => string(g(video, "condition", "")),
        "recorded"       => string(g(video, "recorded", "")),
        "notes"          => string(g(video, "notes", "")),
        "nCuratedTracks" => g(video, "nCuratedTracks", 0),
        "dois"           => collect(Any, g(video, "dois", Any[])),
        "experimentId"   => g(experiment, "id"),
        "experimentName" => string(g(experiment, "name", "")),
        "experimentUrl"  => string(g(experiment, "url", "")),
        "experimentDois" => collect(Any, g(experiment, "dois", Any[])),
    )
end

# The frame interval to declare for a video, for `omezarr` to use when the file has none. The measured
# timestamps win over the nominal frame rate; `nothing` when Immunemap has neither, or when its
# timestamps are irregular (a paused acquisition has no single interval, and the nominal rate would
# hide that). Pure.
function immunemap_declared_interval(video::AbstractDict)::Union{Dict{String,Any},Nothing}
    g(k) = get(video, k, get(video, Symbol(k), nothing))
    g("timeStepsIrregular") === true && return nothing
    acq = g("acquisitionId")
    for (k, what) in (("intervalFromTimeStepsSec", "timestamps"), ("intervalSec", "frame rate"))
        v = g(k)
        (v isa Real && v > 0) && return Dict{String,Any}(
            "value" => round(Float64(v); digits = 6), "unit" => "second",
            "source" => "Immunemap acquisition $acq ($what)")
    end
    nothing
end

# A video's image attributes: its group's raw name as `condition`, plus whatever the user split that
# group into in the dialog's group × attribute table (`group_attrs[groupId]`, e.g. time / antigen).
# Blank names and values are dropped and `condition` can't be overwritten. Pure.
function immunemap_image_attrs(video::AbstractDict, group_attrs)::Dict{String,String}
    g(k) = get(video, k, get(video, Symbol(k), nothing))
    attr = Dict{String,String}("condition" => string(something(g("condition"), "")))
    group_attrs isa AbstractDict || return attr
    gid  = string(something(g("groupId"), ""))
    vals = get(group_attrs, gid, get(group_attrs, Symbol(gid), nothing))
    vals isa AbstractDict || return attr
    for (k, v) in pairs(vals)
        name, val = strip(string(k)), strip(string(something(v, "")))
        (isempty(name) || isempty(val) || name == "condition") && continue
        attr[name] = val
    end
    attr
end

# POST /api/import/register-immunemap {projectUid, setUid, experiment: {...}, videos: [...],
# groupAttrs?: {groupId: {name: value}}} — the `experiment` block and `videos` rows exactly as the
# scan returned them. Creates one image per video with its provenance in `meta.immunemap`, its
# attributes (`condition` + the group's split, `immunemap_image_attrs` — what Compare by attribute
# groups on), and its channel labels from the staining table. A video already in the set
# (same acquisition id) is skipped, not duplicated. Nothing is downloaded here.
function api_import_register_immunemap(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
    experiment  = get(body, :experiment, nothing)
    videos      = get(body, :videos, [])
    group_attrs = get(body, :groupAttrs, nothing)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    experiment isa AbstractDict || return 400, JSON3.write((; error="experiment required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    proj = load_project(project_uid)
    si   = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(si) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[si]

    have = Set{Any}()
    for im in images(s)
        m = get(im.meta, "immunemap", nothing)
        m isa AbstractDict && push!(have, get(m, "acquisitionId", nothing))
    end

    registered = Dict{String,Any}[]
    for v in videos
        acq_id = get(v, :acquisitionId, nothing)
        name   = String(get(v, :name, string(acq_id)))
        if !Bool(get(v, :importable, false))
            push!(registered, Dict{String,Any}("name" => name, "status" => "skipped",
                                               "reason" => String(get(v, :reason, "not importable"))))
            continue
        end
        if acq_id in have
            push!(registered, Dict{String,Any}("name" => name, "status" => "exists"))
            continue
        end
        meta = Dict{String,Any}("immunemap" => immunemap_image_meta(v, experiment))
        dt   = immunemap_declared_interval(v)
        isnothing(dt) || (meta["declaredTimeIncrement"] = dt)
        attr = immunemap_image_attrs(v, group_attrs)
        img  = add_image!(s; name = name, meta = meta, attr = attr)
        names = String[String(x) for x in get(v, :channelNames, String[])]
        # SizeC isn't known until the conversion; it keeps these names when the count matches
        # (`_merge_channel_names!`), so a staining table that disagrees with the file loses, not wins.
        isempty(names) || (set_channel_names!(img, names; check_length = false); save!(img))
        push!(have, acq_id)
        push!(registered, Dict{String,Any}("uid" => img.uid, "name" => img.name, "status" => "pending"))
    end
    @info "Registered Immunemap videos" count=count(r -> r["status"] == "pending", registered) set=set_uid
    200, JSON3.write((; images = registered))
end
