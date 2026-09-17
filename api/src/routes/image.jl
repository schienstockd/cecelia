# ── Image management ──────────────────────────────────────────────────────────

function api_images_register(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
    # `filepaths` is either a list of strings (each path = one image, no series pick — the classic
    # single-series case) OR a list of objects `{path, series?, name?}` (series-picker output, one
    # entry per (file, series)). Both shapes go through the same loop; a `series` field lands in
    # `meta.ori_series` and the importImages.omezarr task threads it into `bioformats2raw --series N`.
    raw_records = get(body, :filepaths, [])

    isempty(project_uid)   && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)       && return 400, JSON3.write((; error="setUid required"))
    isempty(raw_records)   && return 400, JSON3.write((; error="filepaths required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    set_ = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(set_) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[set_]

    registered = Dict{String,Any}[]
    for rec in raw_records
        filepath, series, name_override = if rec isa AbstractString || rec isa AbstractDict
            if rec isa AbstractString
                (String(rec), nothing, nothing)
            else
                p  = String(get(rec, :path, get(rec, "path", "")))
                si = get(rec, :series, get(rec, "series", nothing))
                nm = get(rec, :name,   get(rec, "name",   nothing))
                (p,
                 isnothing(si) ? nothing : Int(si),
                 (isnothing(nm) || isempty(String(nm))) ? nothing : String(nm))
            end
        else
            @warn "Skipping unrecognised filepath record" record=rec
            continue
        end
        isempty(filepath) && continue

        abs_path = isabspath(filepath) ? filepath : joinpath(FS_ROOT, filepath)
        isfile(abs_path) || begin; @warn "Skipping missing file" path=abs_path; continue; end

        meta = Dict{String,Any}("ori_path" => abs_path)
        isnothing(series) || (meta["ori_series"] = series)
        base_name = isnothing(name_override) ? splitext(basename(abs_path))[1] : name_override

        # No task subdirs are created here — each one is made by whoever writes into it (see
        # docs/OBJECTMODEL.md → Disk layout), so an image folder holds only what has actually run.
        img = add_image!(s; name=base_name, meta=meta)

        push!(registered, Dict{String,Any}(
            "uid"       => img.uid,
            "name"      => img.name,
            "status"    => "pending",
            "filepath"  => abs_path,            # SOURCE path, for display only (not the converted zarr)
            "oriPath"   => abs_path,            # kept in `meta.ori_path`; surfaced here for the
                                                 # pre-import pyramid advisor (paramAdvisors.ts) so
                                                 # the freshly-registered rows carry it without a
                                                 # separate refresh trip to the image listing route.
            "oriSeries" => series,               # nothing or an Int — `something(x, nothing)` throws
                                                 # when x is also nothing ("No value arguments"),
                                                 # so pass the value through directly.
            # No versioned `filepaths` yet — the OME-ZARR doesn't exist until the import task converts it.
            # (Faking `{default: …}` here made a pending row look "imported" — see isImported / the crop
            # + open gates. The conversion task writes the real versioned filepath.)
            "filepaths" => Dict{String,Any}(),
        ))
    end

    @info "Registered images" count=length(registered) set=set_uid
    200, JSON3.write((; images=registered))
end

# POST /api/import/series/probe {filepath, maxPx?} → enumerate the series of a multi-series microscopy
# file (currently .lif via readlif; other formats fall through with format="unsupported"). Runs the
# Python probe via run_py; no image is registered here — the wizard shows the picker and only then
# calls /api/images/register with the chosen (path, series) pairs. See probe_series_run.py.
function api_import_series_probe(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    fp = _wstr(body, :filepath)
    isempty(fp) && return 400, JSON3.write((; error="filepath required"))
    abs_path = isabspath(fp) ? fp : joinpath(FS_ROOT, fp)
    isfile(abs_path) || return 404, JSON3.write((; error="File not found: $abs_path"))
    max_px = Int(get(body, :maxPx, 128))

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "probe.result.json")
    params = Dict{String,Any}("imPath" => abs_path, "resultPath" => result_file, "maxPx" => max_px)
    logs = String[]
    ok = try
        Cecelia.run_py("tasks/importImages/probe_series_run.py", params, run_dir; on_log = l -> push!(logs, l))
    catch e
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="probe failed: $(sprint(showerror, e))"))
    end
    if !(ok && isfile(result_file))
        tail = isempty(logs) ? "no output" : join(last(logs, 8), " | ")
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="Series probe failed: $tail"))
    end
    payload = read(result_file, String)
    rm(run_dir; recursive=true, force=true)
    200, payload
end

# POST /api/import/peek-pyramid {paths:[...], chunk?} → per-path {reader, nX,nY,nZ,nT,nC,
# recommendedPyramidLevels, targetChunk}. Metadata-only, no pixels. The wizard uses it to pre-fill
# the omezarr import form's `pyramidLevels`. Fast-path readers (tifffile/readlif/h5py) come back
# instantly; JVM-only formats (CZI/ND2/OIR/LSM/OIB/...) route through Bio-Formats' `showinf` when
# bftools is installed (~2 s cold-start — the FRONTEND fires those lazily, one path per wizard
# open, not on set-add). Missing bftools ⇒ JVM formats come back `reader: "unsupported"` and the
# caller keeps the current default. See peek_pyramid_run.py for the tiering rule.
function api_import_peek_pyramid(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    raw_paths = get(body, :paths, nothing)
    (raw_paths === nothing || isempty(raw_paths)) &&
        return 400, JSON3.write((; error="paths required (non-empty list)"))
    paths = String[]
    for p in raw_paths
        s = String(p)
        isempty(s) && continue
        push!(paths, isabspath(s) ? s : joinpath(FS_ROOT, s))
    end
    isempty(paths) && return 400, JSON3.write((; error="no usable paths in request"))
    params = Dict{String,Any}("paths" => paths, "resultPath" => "")
    haskey(body, :chunk) && (params["chunk"] = Int(body.chunk))
    # Opt the JVM (`showinf`) fallback in whenever bftools is present, so a wizard peek on a
    # .czi/.nd2/.oir/... resolves to real dims instead of `unsupported`. Empty ⇒ Python skips it.
    let sh = Cecelia.showinf_bin(); isempty(sh) || (params["showinfBin"] = sh) end

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "peek.result.json")
    params["resultPath"] = result_file
    logs = String[]
    ok = try
        Cecelia.run_py("tasks/importImages/peek_pyramid_run.py", params, run_dir; on_log = l -> push!(logs, l))
    catch e
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="peek failed: $(sprint(showerror, e))"))
    end
    if !(ok && isfile(result_file))
        tail = isempty(logs) ? "no output" : join(last(logs, 8), " | ")
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="Pyramid peek failed: $tail"))
    end
    payload = read(result_file, String)
    rm(run_dir; recursive=true, force=true)
    200, payload
end

# POST /api/import/scan-legacy {sourceProjectDir, rscript?, imageUids?} → read-only preview manifest
# of a legacy R/Shiny cecelia project (what will/won't transfer per image). See
# app/src/tasks/importImages/scan_legacy_run.py and docs/todo/LEGACY_MIGRATION_PLAN.md.
function api_import_scan_legacy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    src = _wstr(body, :sourceProjectDir)
    isempty(src) && return 400, JSON3.write((; error="sourceProjectDir required"))
    abs_src = isabspath(src) ? src : joinpath(FS_ROOT, src)
    isdir(joinpath(abs_src, "ANALYSIS")) ||
        return 400, JSON3.write((; error="Not a legacy cecelia project (no ANALYSIS/ dir): $abs_src"))

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "scan.result.json")
    params = Dict{String,Any}("sourceProjectDir" => abs_src, "resultPath" => result_file,
                              "rscript" => Cecelia.rscript_bin_path(_wstr(body, :rscript)))
    haskey(body, :imageUids) && (params["imageUids"] = [String(u) for u in body.imageUids])
    logs = String[]
    ok = try
        Cecelia.run_py("tasks/importImages/scan_legacy_run.py", params, run_dir; on_log = l -> push!(logs, l))
    catch e
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="scan failed: $(sprint(showerror, e))"))
    end
    if !(ok && isfile(result_file))
        tail = isempty(logs) ? "no output (is Rscript available? try the Rscript path option)" :
               join(last(logs, 8), " | ")
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="Scan failed: $tail"))
    end
    manifest = read(result_file, String)
    rm(run_dir; recursive=true, force=true)
    200, manifest   # already JSON
end

# POST /api/import/register-legacy {projectUid, setUid, sourceProjectDir, images:[{uid,name,kind}]}
# Registers a placeholder image per legacy image, PRESERVING its UID and stashing the source in meta,
# so the per-image importImages.migrateLegacy task can run. Mirrors api_images_register.
function api_import_register_legacy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
    src         = _wstr(body, :sourceProjectDir)
    rsc         = _wstr(body, :rscript)
    imgs_in     = get(body, :images, [])
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(src)         && return 400, JSON3.write((; error="sourceProjectDir required"))
    abs_src = isabspath(src) ? src : joinpath(FS_ROOT, src)

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    proj = load_project(project_uid)
    si   = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(si) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[si]

    registered = Dict{String,Any}[]
    for im in imgs_in
        uid  = String(get(im, :uid, ""))
        isempty(uid) && continue
        name = String(get(im, :name, uid))
        # Legacy `kind` on the R side (static/live/flow) is intentionally dropped — the new app
        # gates per-image on axes (Cecelia.task_applies), not project-wide.
        meta = Dict{String,Any}("legacySourceDir" => abs_src, "legacySourceUid" => uid)
        isempty(rsc) || (meta["legacyRscript"] = rsc)
        img = add_image!(s; name=name, uid=uid, meta=meta)
        push!(registered, Dict{String,Any}(
            "uid" => img.uid, "name" => img.name, "status" => "pending"))
    end
    @info "Registered legacy images" count=length(registered) set=set_uid
    200, JSON3.write((; images=registered))
end

function api_images_meta(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(state_file(proj_dir, image_uid)) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    obj = init_object(project_uid, image_uid)
    obj isa CciaImage || return 404, JSON3.write((; error="Not an image: $image_uid"))
    200, JSON3.write((; image=_image_payload(obj)))
end

# GET /api/images?projectUid → a read-only listing of the project's sets + images (uid, name,
# per-image status). Unlike POST /api/projects/load this has NO side effects (load bumps
# lastOpenedAt), so the MCP observer can enumerate images while keeping its no-mutation guarantee.
# Backs the observer's get_project_info + list_images tools.
function api_images_list(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    sets = [(; uid=s.uid, name=s.name, imageCount=length(s.image_uids)) for s in proj._sets]
    imgs = Vector{Any}()
    # `attr` is the per-image ASSIGNMENT (Mouse => "3"), distinct from GET /api/plots/attrs, which is
    # the set's attribute AXES (name + distinct values) and stays the one discovery route. Both are
    # needed to choose a cross-image plot: the axes say what you may group by, the assignment says how
    # many images land in each group — see docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
    for s in proj._sets, img in images(s)
        push!(imgs, (; uid=img.uid, name=img.name, status=string(img.status),
                       included=image_included(img), setUid=s.uid, setName=s.name,
                       attr=Dict(string(k) => string(v) for (k, v) in img.attr)))
    end
    200, JSON3.write((; projectUid=project_uid, name=proj.name,
                        count=length(imgs), sets, images=imgs))
end

# GET /api/objects/find?q=[&limit=] → WHICH PROJECT an object lives in, across every project.
#
# Every other read route needs a `projectUid` the caller does not have: a uid quoted in a chat, a note
# or a filename ("what happened to image p6t4mC?") says nothing about its project, so the only way to
# use it was to call /api/images for each project in turn until one matched — N round trips to answer
# a lookup, and the MCP observer did exactly that. This is the one route that starts from the object.
#
# `q` is a uid OR a name fragment, in that order of preference:
#   - UID (exact, case-sensitive) — the metadata dir is `{proj}/1/{uid}`, so existence is one `isfile`
#     per project and only the OWNING project is then loaded. Matches a project uid too.
#   - NAME (case-insensitive substring) — only when nothing matched the uid pass, and it loads every
#     project (each `ccid.json`, no pixel data). Names are not unique, so this can return several.
# Read-only: no `lastOpenedAt` bump, the same non-mutating guarantee as GET /api/images.
function api_objects_find(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    q     = strip(get(query, "q", ""))
    isempty(q) && return 400, JSON3.write((; error="q required (a uid or a name fragment)"))
    limit = something(tryparse(Int, get(query, "limit", "")), 50)
    limit = clamp(limit, 1, 500)

    _proj_match(p) = (; kind="project", uid=string(get(p, "uid", "")), name=string(get(p, "name", "")),
                        projectUid=string(get(p, "uid", "")), projectName=string(get(p, "name", "")))
    _set_match(proj, s) = (; kind="set", uid=s.uid, name=s.name,
                             projectUid=proj.uid, projectName=proj.name,
                             imageCount=length(s.image_uids))
    _img_match(proj, s, img) = (; kind="image", uid=img.uid, name=img.name,
                                  projectUid=proj.uid, projectName=proj.name,
                                  setUid=s.uid, setName=s.name,
                                  status=string(img.status), included=image_included(img))

    projects = _scan_projects_raw()          # most-recently-opened first; carries uid/name/path
    matches  = Vector{Any}()

    # ── uid pass ──────────────────────────────────────────────────────────────────────────────────
    for p in projects
        root = string(get(p, "path", ""))
        string(get(p, "uid", "")) == q && push!(matches, _proj_match(p))
        isfile(state_file(root, String(q))) || continue
        proj = try load_project(string(get(p, "uid", ""))) catch e
            @warn "Skipping project that would not load" dir=root exception=e
            continue
        end
        found = false
        for s in proj._sets
            s.uid == q && (push!(matches, _set_match(proj, s)); found = true)
            for img in images(s)
                img.uid == q && (push!(matches, _img_match(proj, s, img)); found = true)
            end
        end
        # The dir exists but no set claims it — a set-less leftover. Report what it IS rather than
        # "not found": the caller asked where a uid lives, and "in this project, unattached" is the
        # answer. `init_object` dispatches on the ccid.json `class`, so this needs no guessing.
        #
        # `obj.uid == q` is NOT redundant, and dropping it is a macOS/Windows-only bug. Those
        # filesystems are case-INSENSITIVE, so the `isfile` above says yes for a wrong-case uid and
        # `init_object` then happily reads the real object through that path — which would answer
        # "p6T4MC" with p6t4mC, listed as unattached because the string comparisons in the set loop
        # (correctly) all missed. The uid match is the STRING comparison; the stat is only a cheap
        # pre-filter. Linux passed this; CI on the other two did not.
        if !found
            obj = try init_object(proj.uid, String(q)) catch; nothing end
            isnothing(obj) && continue
            obj.uid == q || continue
            push!(matches, (; kind = obj isa CciaSet ? "set" : "image",
                              uid=obj.uid, name=obj.name,
                              projectUid=proj.uid, projectName=proj.name,
                              setUid="", setName=""))
        end
    end
    if !isempty(matches)
        return 200, JSON3.write((; query=String(q), matchedBy="uid",
                                   count=length(matches), truncated=false, matches))
    end

    # ── name pass ─────────────────────────────────────────────────────────────────────────────────
    needle = lowercase(String(q))
    _hit(name) = occursin(needle, lowercase(String(name)))
    for p in projects
        _hit(get(p, "name", "")) && push!(matches, _proj_match(p))
        proj = try load_project(string(get(p, "uid", ""))) catch e
            @warn "Skipping project that would not load" dir=get(p, "path", "") exception=e
            continue
        end
        for s in proj._sets
            _hit(s.name) && push!(matches, _set_match(proj, s))
            for img in images(s)
                _hit(img.name) && push!(matches, _img_match(proj, s, img))
            end
        end
    end
    # A name fragment can match a whole set; cap it, but SAY so — a silently trimmed list reads as
    # "these are all of them" and the caller would name the wrong image with full confidence.
    truncated = length(matches) > limit
    truncated && (matches = matches[1:limit])
    200, JSON3.write((; query=String(q), matchedBy="name",
                        count=length(matches), truncated, matches))
end

# GET /api/images/tasklog?projectUid&imageUid&fun → the raw task log for one fun on one image.
# Reads {img._dir}/logs/{fun}.log (written by _wrap_log_with_file in the scheduler). Read-only;
# backs the MCP observer's get_task_log tool. Returns exists=false + "" when no log exists yet.
# The per-image task log is CUMULATIVE: one file per (image, fun_name), appended by every run, with each
# line stamped in LOCAL time by `_wrap_log_with_file`. A caller that wants ONE run's output — the GUI
# backfilling the log of a task that was already running when the tab connected, or a history row for a
# past run — passes that task's `started_at` as `since`, and (for a past run) the NEXT same-fun run's
# start as `until`. Both bounds are ISO-8601 UTC (`TASK_TS_FORMAT`). Without `until` the slice runs to
# EOF, which reads correctly for a run with no successor (the live case, and any fun-on-image only ever
# run once), but drags every later run's output into an older history row when there is one. The slice
# happens HERE: this is the process whose clock wrote the stamps, so it is the only place where the
# local/UTC comparison has a single answer.
#
# Julia's stdlib carries no timezone database (and TimeZones.jl is not a dependency), so the offset is
# taken as `now() - now(UTC)`, rounded to the minute. That is the CURRENT offset, so a run that straddled a
# DST change could be sliced up to an hour off — acceptable for a live task's log, which is what this is for.
_tasklog_local_offset() = round(Dates.now() - Dates.now(UTC), Dates.Minute)

const _TASKLOG_STAMP = r"^\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\]"

# Parse an ISO-8601 UTC bound into the log file's LOCAL clock; nothing when absent/unparseable.
_tasklog_bound(s::AbstractString) = try
    isempty(s) ? nothing : DateTime(String(s), TASK_TS_FORMAT) + _tasklog_local_offset()
catch
    nothing
end

"""
    _tasklog_slice(content, since, until) -> String

Keep only the lines between two ISO-8601 UTC bounds. Half-open: `since <= ts < until` — a line stamped
exactly at `until` belongs to the NEXT run, not this one. Either bound may be empty.

An UNSTAMPED line inherits the previous line's fate rather than being dropped on its own, so a log line
that itself contained a newline survives intact. An unparseable bound is treated as absent — a backfill
showing too much beats one showing nothing.
"""
function _tasklog_slice(content::AbstractString, since::AbstractString, until::AbstractString = "")
    t0 = _tasklog_bound(since)
    t1 = _tasklog_bound(until)
    # No bounds usable — return the content untouched rather than re-emitting it (`println` per line
    # would add a stray trailing newline). A caller sending garbage bounds sees the whole file, which
    # beats seeing nothing.
    (isnothing(t0) && isnothing(t1)) && return String(content)
    out  = IOBuffer()
    keep = isnothing(t0)   # no lower bound → keep unstamped preamble too
    for line in split(String(content), '\n'; keepempty=true)
        m = match(_TASKLOG_STAMP, line)
        if !isnothing(m)
            ts = try DateTime(m.captures[1], dateformat"yyyy-mm-dd HH:MM:SS") catch; nothing end
            if !isnothing(ts)
                lo_ok = isnothing(t0) || ts >= t0
                hi_ok = isnothing(t1) || ts <  t1
                keep = lo_ok && hi_ok
            end
        end
        keep && println(out, line)
    end
    String(take!(out))
end

function api_images_tasklog(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    fun         = get(query, "fun", "")
    since       = get(query, "since", "")
    until       = get(query, "until", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(fun)         && return 400, JSON3.write((; error="fun required"))
    # fun becomes a filename ({fun}.log) — reject separators / traversal so it can't escape logs/
    (occursin('/', fun) || occursin('\\', fun) || occursin("..", fun)) &&
        return 400, JSON3.write((; error="invalid fun"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    img_dir = joinpath(proj_dir, "1", image_uid)
    isfile(state_file(img_dir)) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    logfile = joinpath(img_dir, "logs", fun * ".log")
    isfile(logfile) || return 200, JSON3.write((; projectUid=project_uid, imageUid=image_uid,
                                                  fun, exists=false, content=""))
    content = read(logfile, String)
    (isempty(since) && isempty(until)) || (content = _tasklog_slice(content, since, until))
    200, JSON3.write((; projectUid=project_uid, imageUid=image_uid, fun,
                        exists=true, content, bytes=sizeof(content)))
end

# GET /api/tasks/history?projectUid[&limit] → recent task runs across all images, newest first.
# Aggregates each image's runlog.json (fun, valueName, timestamp) plus the image's current status.
# Read-only; backs the MCP observer's get_task_history tool. (Attempt counts arrive with the
# per-node counter in a later slice.) limit caps the returned rows (default 100).
function api_tasks_history(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    parsed = tryparse(Int, get(query, "limit", ""))
    limit  = (isnothing(parsed) || parsed <= 0) ? 100 : parsed
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    # run-log entries may deserialise with String or Symbol keys depending on the JSON3 path — try both
    _rl(e, k) = (v = get(e, k, get(e, Symbol(k), nothing)); v === nothing ? "" : String(v))
    rows = Vector{Any}()
    for img in images(proj), e in read_run_log(img)
        # per-RUN outcome; legacy entries have none → "done". An OPEN SET, not just done/failed:
        # "cancelled" and "interrupted" (a run whose process died — see run_log.jl) both appear here,
        # and "running" means the run is live right now. Don't assume a terminal value.
        rs = _rl(e, "status")
        push!(rows, Dict{String,Any}(
            "imageUid" => img.uid, "imageName" => img.name, "status" => string(img.status),  # image's status
            "runStatus" => (isempty(rs) ? "done" : rs),                              # this run's outcome
            "fun" => _rl(e, "fun"), "valueName" => _rl(e, "valueName"), "at" => _rl(e, "at"),
            # the tuning trail: the params this run used (run_log.jl; {} on legacy entries). Lets the
            # observer suggest a param adjustment on an outlier without a second per-image call.
            "params" => get(e, :params, get(e, "params", Dict{String,Any}()))))
    end
    # newest first — the run-log timestamp is yyyy-mm-ddTHH:MM:SS, so lexicographic == chronological
    sort!(rows, by = r -> r["at"], rev = true)
    length(rows) > limit && (rows = rows[1:limit])
    200, JSON3.write((; projectUid=project_uid, count=length(rows), history=rows))
end

# GET /api/qc/cohort?projectUid&setUid&funName[&valueName][&threshold]
# Recompute the cohort QC summary for one (task, output) across a set's included images and return
# it (also writes the sidecar). `threshold` is the robust modified-z cutoff (default 3.5). Feeds the
# MCP get_cohort_qc tool + the morning summary.
function api_qc_cohort(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    fun_name    = get(q, "funName", "")
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    haskey(COHORT_METRICS, fun_name) ||
        return 400, JSON3.write((; error = "No cohort metrics for fun '$fun_name'",
                                   known = sort(collect(keys(COHORT_METRICS)))))
    vn_param = get(q, "valueName", "")
    run_param = get(q, "run", "")   # clustering: restrict to one run's value_names (see cohort_runs)
    thr = something(tryparse(Float64, get(q, "threshold", "")), Cecelia._COHORT_MODZ_THRESHOLD)
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    # READ-ONLY: compute + return, write nothing (a GET must be safe). The write path — set sidecar +
    # per-image cohort findings — is the explicit POST /api/qc/cohort/check below.
    # No valueName → discover every value_name this fun banked and return per-value_name cohorts (a
    # `byValueName` map): clustering banks per label set (T/B), segment/tracking under "default", so a
    # caller that doesn't know the suffix still gets all cohorts. An explicit valueName returns just that
    # one cohort (single doc, backward-compatible).
    if isempty(vn_param)
        byval = try
            cohort_qc_for_all(set, fun_name; threshold = thr, run = run_param)
        catch e
            return 500, JSON3.write((; error = sprint(showerror, e)))
        end
        return 200, JSON3.write((; funName = fun_name, valueNames = sort(collect(keys(byval))), byValueName = byval))
    end
    doc = try
        cohort_qc_for(set, fun_name, vn_param; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(doc)
end

# GET /api/qc/cohort/runs?projectUid&setUid&funName — the distinct clustering RUNS a fun banked across
# the set (cheap: scans QC filenames + reads each doc's runSuffix, no cohort math). Powers the Check-
# cohort button's run selector: cluster QC is banked per run, so the user picks WHICH run to check
# rather than the button re-checking every past iteration. `[]` for funs that keep no runs (segment/
# tracking/HMM) — the button then shows no selector and checks as before. Newest run first.
function api_qc_cohort_runs(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    fun_name    = get(q, "funName", "")
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    runs = try
        [(; run = r.run, valueNames = r.valueNames) for r in cohort_runs(set, fun_name)]
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write((; funName = fun_name, runs = runs))
end

# Shared GET handler for the observer's project-scoped summary routes (analysis/*): parse projectUid +
# optional image/set scope, load the project (404), run `build(proj, image_uid, set_uid)` (500), return
# JSON. Each route is then a one-liner over its builder — the same consolidation as the Julia
# `observer_image_summary` scaffold and the MCP `_analysis_summary` client helper.
function _observer_summary_route(req::HTTP.Request, build::Function)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    out = try
        build(proj, get(q, "imageUid", ""), get(q, "setUid", ""))
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# GET /api/analysis/lineage — synthesized pipeline (steps + seg/track/cluster/gating links, chains,
# boards, rollup). GET /api/analysis/populations — the gate/filter DEFINITIONS behind lineage's
# gatedPops. Both READ-ONLY, summary-level. See analysis_lineage / populations_summary and Slices A/B
# of OBSERVER_DATA_ACCESS_PLAN.
api_analysis_lineage(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> analysis_lineage(p; image_uid = i, set_uid = s))
api_analysis_populations(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> populations_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/measures — per-population phenotype + motility summaries (gated pops, else the base
# tracked/all-cells pop). Heavier (touches cell data via pop_df); prefer image/set scope. Slice C.
api_analysis_measures(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> measure_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/behaviour — HMM state distribution + transition counts. GET /api/analysis/clusters —
# per clustering run: n clusters, sizes, largest fraction, features. Both read obs via pop_df. Slice D.
api_analysis_behaviour(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> behaviour_summary(p; image_uid = i, set_uid = s))
api_analysis_clusters(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> cluster_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/spatial — per image, region-clustering runs (regions.{suffix}) + pairwise cell-type
# contact log-odds (neighbourStats sidecars). Flat + interpretable for MCP.
api_analysis_spatial(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> spatial_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/chains — the project's whiteboard chains: wired templates (node DAG) + recent runs.
# Project-level (ignores image/set scope). Slice E.
api_analysis_chains(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> chains_summary(p))
# GET /api/analysis/boards — the Analysis boards a project already has and WHAT THEY SHOW (a summary,
# never the stored layout geometry). Lineage's `boards` is tab names only; this is the plot detail, so
# the observer can see an existing board instead of proposing a duplicate. Project-level, read-only.
# See board_summaries + docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
api_analysis_boards(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> board_summaries(p))

# GET /api/observer/briefing?projectUid — the observer SESSION BRIEFING (Observer Phase 2 §2): a small
# startup context (project name + image count, flagged images, recent lab log) a fresh Chat-to-Claude
# session pulls first so the user need not re-explain. Project-level, READ-ONLY. Backs get_session_briefing.
api_observer_briefing(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> session_briefing(p))

# GET /api/mcp/connections — every MCP server registered in the user's Claude config, for the Settings
# "MCP connections" panel. Machine-level (no project), READ-ONLY. Generic: it enumerates what's there,
# so a connector added later needs no change here. It cannot see claude.ai ACCOUNT connectors (e.g.
# LabArchives) — absence from this list is NOT evidence of "disconnected"; see `mcp_connections`.
function api_mcp_connections(::HTTP.Request)
    out = try
        (; configPath = Cecelia.claude_config_path(),
           present = isfile(Cecelia.claude_config_path()),
           connections = mcp_connections())
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# GET /api/observer/labarchives?projectUid — the FULL LabArchives context sidecar + derived gaps (the
# briefing carries only headings). Read-only; see app/src/ai/labarchives.jl.
api_observer_labarchives(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> merge(read_la_doc(p),
                                                      Dict{String,Any}("gaps" => la_gaps(p))))

# PUT /api/observer/labarchives — REPLACE the sidecar. Body {projectUid, source, sections, cohort,
# syncedBy}. The one write; cecelia never fetches from LabArchives itself (no credentials, by design —
# the connector lives in the user's Claude session), so this is how the context arrives.
function api_observer_labarchives_set(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    doc = try
        write_la_doc!(proj;
                      source     = json_native(get(body, :source, Dict{String,Any}())),
                      sections   = json_native(get(body, :sections, Any[])),
                      cohort     = json_native(get(body, :cohort, Any[])),
                      synced_by  = _wstr(body, :syncedBy, "claude"))
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    # Same panel-reload signal the lab log uses — the context card sits in that panel, and an external
    # Chat-to-Claude session writes here with no frontend action at all.
    broadcast_ws(Dict{String,Any}("type" => "lab_log_updated", "projectUid" => project_uid))
    200, JSON3.write(merge(doc, Dict{String,Any}("gaps" => la_gaps(proj, doc))))
end

# GET /api/repl/api — the notebook/REPL data-access surface (Observer Phase 2 foundation): the
# NOTEBOOK_API accessors with their live docstrings, plus the docs/REPL.md cookbook when present. Backs
# the MCP get_repl_api tool so Claude can generate correct `using Cecelia` notebooks without guessing
# the interface. Project-independent, read-only. `doc` is "" if REPL.md isn't shipped (installed app).
function api_repl_api(::HTTP.Request)
    out = try
        api = [(; name = e.name, exported = e.exported, documented = e.documented, doc = e.doc)
               for e in repl_api_reference()]
        p = Cecelia.repl_doc_path()
        (; api = api, doc = isfile(p) ? read(p, String) : "")
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# POST /api/qc/cohort/check — the explicit "Check cohort consistency" action: recompute AND persist
# (set sidecar + per-image `cohort.{fun}` findings so outliers surface on the image). Body:
# {projectUid, setUid, funName, valueName?, threshold?}. This is the ONLY cohort write path.
function api_qc_cohort_check(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid); set_uid = _wstr(body, :setUid)
    fun_name    = _wstr(body, :funName)
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    haskey(COHORT_METRICS, fun_name) ||
        return 400, JSON3.write((; error = "No cohort metrics for fun '$fun_name'",
                                   known = sort(collect(keys(COHORT_METRICS)))))
    vn_param = _wstr(body, :valueName)
    run_param = _wstr(body, :run)   # clustering: check only this run's value_names (see cohort_runs)
    tv  = get(body, :threshold, nothing)
    thr = tv isa Real ? Float64(tv) : Cecelia._COHORT_MODZ_THRESHOLD
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    # Cecelia authors a "Cohort check" lab-log entry ONLY for docs that flagged (an all-clear would be
    # noise). This is the cross-image analysis — image UIDs (stable; the panel resolves uid→name on
    # demand), the metric, its value vs the cohort median — the durable record the amber button points
    # at (no toast). Best-effort; a lab-log hiccup never fails the check. Author "Cecelia — …" so the
    # append route treats it as a Cecelia entry.
    log_flagged(docs) = begin
        flagged = [d for d in docs if d isa AbstractDict && Cecelia.cohort_has_outliers(d)]
        isempty(flagged) && return
        try
            proj = load_project(project_uid)
            for d in flagged
                Cecelia.append_lab_log!(proj, "Cecelia — Cohort check",
                                        Cecelia.cohort_qc_summary_lines(d))
            end
        catch e
            @warn "cohort check: lab-log append failed" exception = e
        end
    end
    # No valueName → check EVERY value_name the fun banked (per label set); else just the one.
    if isempty(vn_param)
        byval = try
            cohort_qc_for_all!(set, fun_name; threshold = thr, run = run_param)
        catch e
            return 500, JSON3.write((; error = sprint(showerror, e)))
        end
        log_flagged(collect(values(byval)))
        return 200, JSON3.write((; funName = fun_name, valueNames = sort(collect(keys(byval))), byValueName = byval))
    end
    doc = try
        cohort_qc_for!(set, fun_name, vn_param; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    log_flagged([doc])
    200, JSON3.write(doc)
end

function api_images_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
    image_uid   = _wstr(body, :imageUid)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    s = init_object(project_uid, set_uid)
    s isa CciaSet || return 404, JSON3.write((; error="Not a set: $set_uid"))
    delete_image!(s, image_uid)

    @info "Deleted image" uid=image_uid set=set_uid project=project_uid
    200, JSON3.write((; ok=true))
end

# POST /api/images/move {projectUid, imageUid, fromSetUid, toSetUid?|newSetName?}
# Move an image to another set in the same project. Provide EITHER an existing `toSetUid` OR a
# `newSetName` to create the destination set on the fly. Manifest-only — no image data moves on
# disk (see move_image!). Returns the resolved destination {toSetUid, toSetName, createdSet}.
function api_images_move(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid  = _wstr(body, :projectUid)
    image_uid    = _wstr(body, :imageUid)
    from_set_uid = _wstr(body, :fromSetUid)
    to_set_uid   = _wstr(body, :toSetUid)
    new_set_name = strip(_wstr(body, :newSetName))
    isempty(project_uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)    && return 400, JSON3.write((; error="imageUid required"))
    isempty(from_set_uid) && return 400, JSON3.write((; error="fromSetUid required"))
    (isempty(to_set_uid) && isempty(new_set_name)) &&
        return 400, JSON3.write((; error="toSetUid or newSetName required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))

    proj = load_project(project_uid)

    # resolve (or create) the destination set
    created = false
    to_name = ""
    if isempty(to_set_uid)
        existing = findfirst(s -> s.name == new_set_name, proj._sets)
        if isnothing(existing)
            s = add_set!(proj; name=String(new_set_name))
            to_set_uid = s.uid; to_name = s.name; created = true
        else
            s = proj._sets[existing]
            to_set_uid = s.uid; to_name = s.name
        end
    else
        ti = findfirst(s -> s.uid == to_set_uid, proj._sets)
        isnothing(ti) && return 404, JSON3.write((; error="Destination set not found: $to_set_uid"))
        to_name = proj._sets[ti].name
    end

    try
        move_image!(proj, image_uid, from_set_uid, to_set_uid)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end

    @info "Moved image" uid=image_uid from=from_set_uid to=to_set_uid project=project_uid createdSet=created
    200, JSON3.write((; ok=true, toSetUid=to_set_uid, toSetName=to_name, createdSet=created))
end

# POST /api/images/version/remove {projectUid, imageUid, valueName, newDefault}
# Delete ONE image version's store and clear its ccid.json entry, re-pointing `_active` at
# `newDefault`. A thin adapter over `remove_image_version!` (app/src/storage.jl) — the same core the
# `importImages.remove` task and the storage reclaim use, so there is one deletion path, not three.
# The caller loops for several versions and must order `default` LAST (docs/todo/IMAGE_DELETE_PLAN.md
# Decision 11), so the safe-primary un-import lands at the end rather than mid-loop.
function api_images_version_remove(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    image_uid   = _wstr(body, :imageUid)
    value_name  = _wstr(body, :valueName)
    new_default = _wstr(body, :newDefault, VERSIONED_DEFAULT_VAL)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error="valueName required"))

    isdir(joinpath(projects_dir(), project_uid)) ||
        return 404, JSON3.write((; error="Project not found: $project_uid"))
    img = init_object(project_uid, image_uid)
    img isa CciaImage || return 404, JSON3.write((; error="Image not found: $image_uid"))

    res = remove_image_version!(img, value_name, new_default)
    isnothing(res) && return 404, JSON3.write((; error="No version '$value_name' on this image"))
    freed, cleared = res

    fresh = init_object(project_uid, image_uid)
    @info "Removed image version" value_name new_default image=image_uid project=project_uid freed
    200, JSON3.write((; ok=true, freedBytes=freed, cleared=cleared,
                        image = fresh isa CciaImage ? _image_payload(fresh) : nothing))
end

# POST /api/images/analysis/reset {projectUid, imageUids: [...]}
# Drop everything DERIVED from each image, keeping the image itself: every child of `1/{uid}` except
# the keep-list, plus the `labels`/`label_props`/`branch_labels` registrations. Touches no image store
# — shedding a version is /api/images/version/remove's job (IMAGE_DELETE_PLAN Decision 9). Core:
# `reset_image_analysis!` (app/src/storage.jl).
function api_images_analysis_reset(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    image_uids = get(body, :imageUids, nothing)
    (image_uids isa AbstractVector && !isempty(image_uids)) ||
        return 400, JSON3.write((; error="imageUids (non-empty) required"))
    isdir(joinpath(projects_dir(), project_uid)) ||
        return 404, JSON3.write((; error="Project not found: $project_uid"))

    freed  = 0
    images = Dict{String,Any}()
    for uid in image_uids
        img = init_object(project_uid, string(uid))
        img isa CciaImage || continue
        f, _ = reset_image_analysis!(img)
        freed += f
        fresh = init_object(project_uid, string(uid))
        fresh isa CciaImage && (images[string(uid)] = _image_payload(fresh))
    end

    @info "Reset image analysis" n=length(images) project=project_uid freed
    200, JSON3.write((; ok=true, freedBytes=freed, images=images))
end

