using Dates

# ── Chain template CRUD ───────────────────────────────────────────────────────

# Per-project persisted UI config lives under `<proj>/settings/` (chains, analysis-canvas boards, …).
_settings_dir_for_project(project_uid::String) = joinpath(projects_dir(), project_uid, "settings")

function _chains_dir_for_project(project_uid::String)
    newdir = joinpath(_settings_dir_for_project(project_uid), "chains")
    olddir = joinpath(projects_dir(), project_uid, "chains")   # legacy location (pre-settings/)
    if isdir(olddir) && !isdir(newdir)
        try; mkpath(_settings_dir_for_project(project_uid)); mv(olddir, newdir)
        catch e; @warn "Could not migrate chains into settings/" project=project_uid exception=e; end
    end
    newdir
end

function api_chains_list(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    dir   = _chains_dir_for_project(uid)
    names = isdir(dir) ?
        sort([splitext(f)[1] for f in readdir(dir) if endswith(f, ".json") && !startswith(f, ".")]) :
        String[]
    200, JSON3.write((; chains=names))
end

function api_chains_get(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    name  = get(query, "name", "")
    isempty(uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    path  = joinpath(_chains_dir_for_project(uid), "$(name).json")
    isfile(path) || return 404, JSON3.write((; error="Chain not found: $name"))
    # Return raw JSON — the template may contain extra fields (e.g. whiteboard positions)
    # that the frontend added; preserve them verbatim.
    200, read(path, String)
end

function api_chains_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    name = String(get(body, :name, ""))
    isempty(uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    path = joinpath(_chains_dir_for_project(uid), "$(name).json")
    isfile(path) || return 404, JSON3.write((; error="Chain not found: $name"))
    rm(path)
    @info "Deleted chain" name project=uid
    200, JSON3.write((; ok=true))
end

# GET /api/chains/runs?projectUid — list persisted run records (newest first) for the Live view.
# Reads each run.json's header fields directly (cheap; no template-cache resolution). Runs live under
# settings/chains/runs/<id>/run.json.
function api_chains_runs(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    runs_dir = joinpath(_chains_dir_for_project(uid), "runs")
    isdir(runs_dir) || return 200, JSON3.write((; runs=Any[]))
    out = Any[]
    for d in readdir(runs_dir; join=true)
        isdir(d) || continue
        rj = joinpath(d, "run.json")
        isfile(rj) || continue
        raw = try JSON3.read(read(rj, String)) catch; continue end
        push!(out, (; runId     = string(get(raw, :id, basename(d))),
                      chainName  = string(get(raw, :chain_name, "")),
                      createdAt  = Float64(get(raw, :created_at, 0.0)),
                      imageCount = length(get(raw, :image_uids, []))))
    end
    sort!(out; by = r -> r.createdAt, rev = true)
    200, JSON3.write((; runs = out))
end

# GET /api/chains/run?projectUid&runId — a single persisted run's frozen template (nodes/edges for
# the layered layout) + per-image per-node status, so the Live view can render a past run from disk.
function api_chains_run(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    rid   = get(query, "runId", "")
    (isempty(uid) || isempty(rid)) && return 400, JSON3.write((; error="projectUid and runId required"))
    proj = try load_project(uid) catch e; return 404, JSON3.write((; error=sprint(showerror, e))) end
    run  = try load_chain_run(proj, rid) catch; return 404, JSON3.write((; error="run not found: $rid")) end
    nodes = [(; id=n.id, fn=n.fn, params=n.params) for n in run.template_snapshot.nodes]
    edges = [(; from=e.from, to=e.to) for e in run.template_snapshot.edges]
    states = Dict{String,Any}()
    for (u, nm) in run.image_states
        states[u] = Dict{String,Any}(nid => string(st.status) for (nid, st) in nm)
    end
    200, JSON3.write((; runId=run.id, chainName=run.chain_name, createdAt=run.created_at,
                        imageUids=run.image_uids, nodes=nodes, edges=edges, imageStates=states))
end

function api_chains_save(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    tmpl = get(body, :template, nothing)
    isempty(uid)      && return 400, JSON3.write((; error="projectUid required"))
    isnothing(tmpl)   && return 400, JSON3.write((; error="template required"))
    name = String(get(tmpl, :name, ""))
    isempty(name)     && return 400, JSON3.write((; error="template.name required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    dir  = _chains_dir_for_project(uid)
    mkpath(dir)
    # Write verbatim — preserves any extra fields (positions, etc.) the whiteboard added.
    open(joinpath(dir, "$(name).json"), "w") do io
        JSON3.write(io, tmpl)
    end
    200, JSON3.write((; ok=true))
end

# ── Task definitions (single-source from package JSON specs) ──────────────────

const _TASK_SPECS_ROOT = joinpath(@__DIR__, "..", "..", "app", "src", "tasks")

function api_task_definitions(req::HTTP.Request)
    uri    = HTTP.URI(req.target)
    query  = HTTP.queryparams(uri)
    cat    = get(query, "category", "")   # optional filter

    raw = Dict{String, Vector{Any}}()
    isdir(_TASK_SPECS_ROOT) || return 200, JSON3.write(raw)

    frag_dir = joinpath(_TASK_SPECS_ROOT, "fragments")

    for entry in readdir(_TASK_SPECS_ROOT; join=true)
        isdir(entry) || continue
        entry == frag_dir && continue          # skip shared fragments
        basename(entry) == "testTasks" && continue   # dev-only stubs for the test suite — never user-facing
        category = basename(entry)
        (!isempty(cat) && category != cat) && continue
        specs = Any[]
        for f in readdir(entry; join=true)
            endswith(f, ".json") || continue
            try
                parsed = JSON3.read(read(f, String), Dict{String,Any})
                resolved = Cecelia._resolve_spec_includes(parsed, frag_dir)
                push!(specs, resolved)
            catch e
                @warn "Skipping malformed task spec" path=f exception=e
            end
        end
        isempty(specs) || (raw[category] = specs)
    end

    # Build fun_name → spec lookup so composite tasks can pull params from their steps.
    by_fun = Dict{String, Any}()
    for specs in values(raw)
        for spec in specs
            fn = string(get(spec, "fun_name", ""))
            isempty(fn) || (by_fun[fn] = spec)
        end
    end

    # For composite specs: merge params from sub-task specs (dedup by key, first wins).
    result = Dict{String, Vector{Any}}()
    for (category, specs) in raw
        out = Any[]
        for spec in specs
            composite = get(spec, "composite", nothing)
            if !isnothing(composite) && !isempty(composite)
                merged = Any[]
                seen   = Set{String}()
                for fn_ref in composite
                    sub = get(by_fun, string(fn_ref), nothing)
                    isnothing(sub) && continue
                    for p in get(sub, "params", [])
                        k = string(get(p, "key", ""))
                        k ∈ seen && continue
                        # Skip params derived inside the composite (e.g. hmm_transitions.hmmStates,
                        # set automatically from the states step) — they shouldn't appear in the form.
                        (p isa AbstractDict && get(p, "hideInComposite", false) == true) && continue
                        push!(seen, k)
                        push!(merged, p)
                    end
                end
                spec["params"] = merged
                push!(out, spec)
            else
                push!(out, spec)
            end
        end
        result[category] = out
    end

    200, JSON3.write(result)
end

# ── Task param memory (funParams) ─────────────────────────────────────────────
# GET /api/tasks/funparams?projectUid=&fun=&imageUid=&setUid=
# Returns the last-used params for `fun`, resolved image → set → none (R parity). The frontend
# passes imageUid only when exactly one image is selected (else the shared set-level default).
function api_task_fun_params(req::HTTP.Request)
    q     = HTTP.queryparams(HTTP.URI(req.target))
    proj  = get(q, "projectUid", "")
    fun   = get(q, "fun", "")
    imgu  = get(q, "imageUid", "")
    setu  = get(q, "setUid", "")
    (isempty(proj) || isempty(fun)) &&
        return 400, JSON3.write((; error = "projectUid and fun are required"))

    proj_root = joinpath(projects_dir(), proj)
    params = isempty(imgu) ? nothing :
             Cecelia.read_module_fun_params(joinpath(proj_root, "1", imgu), fun)
    if isnothing(params) && !isempty(setu)
        params = Cecelia.read_module_fun_params(joinpath(proj_root, "1", setu), fun)
    end
    200, JSON3.write((; params = params))
end

# ── Resource pools ───────────────────────────────────────────────────────────

function api_pools_list(_req)
    pools = sort(list_pools(), by=p->p.name)
    200, JSON3.write(pools)
end

# Point-in-time snapshot of queued/running tasks (reporting only — no control).
# The WS `task:*` / `chain:node:*` stream is the live feed; this fills in what is
# already in-flight when a console first connects.
function api_tasks_list(_req)
    200, JSON3.write(list_tasks())
end

# ── Filesystem browser ────────────────────────────────────────────────────────

const FS_ROOT = get(ENV, "CECELIA_FS_ROOT", homedir())

const IMAGE_EXTS = Set([
    # TIFF family
    ".tif", ".tiff", ".btf", ".tf8", ".tf2",
    # OME-ZARR
    ".zarr",
    # Zeiss
    ".czi", ".lsm",
    # Leica
    ".lif", ".lei", ".xlef", ".scn",
    # Nikon
    ".nd2",
    # Olympus / Evident
    ".oir", ".oib", ".oif", ".vsi",
    # MetaMorph
    ".nd", ".stk",
    # 3i Slidebook
    ".sld",
    # Imaris / HDF5
    ".ims", ".h5", ".hdf5",
    # PerkinElmer
    ".flex",
    # Whole slide imaging
    ".svs", ".ndpi", ".mrxs", ".qptiff",
    # DICOM
    ".dcm", ".dicom",
    # Common raster (useful for testing / simple imports)
    ".png", ".jpg", ".jpeg",
])

function api_fs_list(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    rel   = get(query, "path", "")
    base  = isempty(rel) ? FS_ROOT : normpath(joinpath(FS_ROOT, rel))
    startswith(base, FS_ROOT) || return 400, JSON3.write((; error="Path outside allowed root"))
    isdir(base) || return 400, JSON3.write((; error="Not a directory: $base"))

    entries = map(readdir(base; join=false)) do name
        full   = joinpath(base, name)
        ext    = lowercase(splitext(name)[2])
        isdir_ = isdir(full)
        (; name, path=relpath(full, FS_ROOT), isdir=isdir_,
           isimage=!isdir_ && ext ∈ IMAGE_EXTS, ext,
           size=isdir_ ? nothing : filesize(full))
    end
    visible = filter(e -> !startswith(e.name, "."), entries)
    sorted  = sort(visible; by=e -> (!e.isdir, lowercase(e.name)))
    200, JSON3.write((; root=FS_ROOT, current=relpath(base, FS_ROOT),
                       parent=base == FS_ROOT ? nothing : relpath(dirname(base), FS_ROOT),
                       entries=sorted))
end

# ── Project management ────────────────────────────────────────────────────────

function api_projects_list(req::HTTP.Request)
    projects = _scan_projects_raw()
    200, JSON3.write((; projects, projectsDir=projects_dir()))
end

function api_projects_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    name = String(strip(String(get(body, :name, ""))))
    type = String(get(body, :type, "static"))
    isempty(name) && return 400, JSON3.write((; error="Project name is required"))
    type ∉ ("static", "live", "flow") && return 400, JSON3.write((; error="Invalid project type: $type"))

    existing = _scan_projects_raw()
    any(p -> get(p, "name", "") == name, existing) &&
        return 400, JSON3.write((; error="A project named \"$name\" already exists"))

    proj = create_project!(name=name, kind=type)
    meta = Dict{String,Any}("uid"=>proj.uid, "name"=>proj.name, "kind"=>proj.kind,
                             "type"=>proj.kind, "path"=>proj.root,
                             "meta"=>proj.meta, "set_uids"=>proj.set_uids,
                             "createdAt"=>string(now()), "lastOpenedAt"=>string(now()))
    @info "Created project" name type uid=proj.uid
    200, JSON3.write((; project=meta))
end

function api_projects_load(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :uid, ""))
    isempty(uid) && return 400, JSON3.write((; error="Project UID is required"))

    projects = _scan_projects_raw()
    idx = findfirst(p -> get(p, "uid", "") == uid, projects)
    isnothing(idx) && return 404, JSON3.write((; error="Project not found: $uid"))

    project = projects[idx]
    proj_dir = string(project["path"])

    # Update lastOpenedAt
    meta_file = joinpath(proj_dir, "project.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(meta_file, String)))
        raw["lastOpenedAt"] = string(now())
        open(meta_file, "w") do io; JSON3.write(io, raw); end
        project["lastOpenedAt"] = raw["lastOpenedAt"]
    catch e
        @warn "Could not update lastOpenedAt" uid exception=e
    end

    proj_obj = load_project(uid)
    sets     = [_set_payload(s) for s in proj_obj._sets]

    # Analysis-canvas boards saved with the project (settings/); null when none saved yet.
    boards = nothing
    boards_file = joinpath(_settings_dir_for_project(uid), "analysisBoards.json")
    if isfile(boards_file)
        try; boards = JSON3.read(read(boards_file, String)); catch e
            @warn "Could not read analysis boards" uid exception=e
        end
    end

    @info "Opened project" name=get(project, "name", "?") uid sets=length(sets)
    200, JSON3.write((; project, sets, boards))
end

function api_projects_save(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :uid, ""))
    isempty(uid) && return 400, JSON3.write((; error="uid required"))
    proj_dir = joinpath(projects_dir(), uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $uid"))

    meta_file = joinpath(proj_dir, "project.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(meta_file, String)))
        raw["lastOpenedAt"] = string(now())
        open(meta_file, "w") do io; JSON3.write(io, raw); end
    catch e
        @warn "Could not update project metadata" uid exception=e
    end

    # persist the Analysis-canvas boards (tabs + grid layouts + captured screenshots) under settings/ so
    # they survive reopen. Opaque JSON owned by the frontend — we just store/return it verbatim.
    boards = get(body, :boards, nothing)
    if boards !== nothing
        try
            settings = _settings_dir_for_project(uid); mkpath(settings)
            open(joinpath(settings, "analysisBoards.json"), "w") do io; JSON3.write(io, boards); end
        catch e
            @warn "Could not save analysis boards" uid exception=e
        end
    end
    200, JSON3.write((; ok=true))
end

function api_projects_rename(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :uid,  ""))
    name = String(strip(String(get(body, :name, ""))))
    isempty(uid)  && return 400, JSON3.write((; error="uid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    proj_dir = joinpath(projects_dir(), uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))

    meta_file = joinpath(proj_dir, "project.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(meta_file, String)))
        raw["name"] = name
        open(meta_file, "w") do io; JSON3.write(io, raw); end
    catch
        return 500, JSON3.write((; error="Failed to write project metadata"))
    end
    200, JSON3.write((; ok=true, name))
end

# ── Set management ────────────────────────────────────────────────────────────

function api_sets_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    name        = String(get(body, :name, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(name)        && return 400, JSON3.write((; error="name required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))

    proj = load_project(project_uid)
    s    = add_set!(proj; name=name)
    @info "Created set" name uid=s.uid project=project_uid
    200, JSON3.write((; uid=s.uid, name))
end

function api_sets_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = joinpath(proj_dir, "1", set_uid, "ccid.json")
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    idx  = findfirst(s -> s.uid == set_uid, proj._sets)
    n    = isnothing(idx) ? 0 : length(proj._sets[idx].image_uids)
    delete_set!(proj, set_uid)

    @info "Deleted set" uid=set_uid project=project_uid images=n
    200, JSON3.write((; ok=true))
end

# ── Image management ──────────────────────────────────────────────────────────

function api_images_register(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    filepaths   = [String(p) for p in get(body, :filepaths, [])]
    kind        = String(get(body, :kind, "static"))

    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(filepaths)   && return 400, JSON3.write((; error="filepaths required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = joinpath(proj_dir, "1", set_uid, "ccid.json")
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    set_ = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(set_) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[set_]

    registered = Dict{String,Any}[]
    for filepath in filepaths
        abs_path = isabspath(filepath) ? filepath : joinpath(FS_ROOT, filepath)
        isfile(abs_path) || begin; @warn "Skipping missing file" path=abs_path; continue; end

        task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
        img = add_image!(s; name=splitext(basename(abs_path))[1], kind=kind,
                         meta=Dict{String,Any}("ori_path" => abs_path))
        for subdir in values(task_dirs)
            mkpath(joinpath(proj_dir, "1", img.uid, string(subdir)))
        end

        push!(registered, Dict{String,Any}(
            "uid"       => img.uid,
            "name"      => img.name,
            "kind"      => img.kind,
            "status"    => "pending",
            "filepath"  => abs_path,
            "filepaths" => Dict{String,Any}("default" => "ccidImage.ome.zarr"),
        ))
    end

    @info "Registered images" count=length(registered) set=set_uid
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
    isfile(joinpath(proj_dir, "1", image_uid, "ccid.json")) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    obj = init_object(project_uid, image_uid)
    obj isa CciaImage || return 404, JSON3.write((; error="Not an image: $image_uid"))
    200, JSON3.write((; image=_image_payload(obj)))
end

function api_images_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    image_uid   = String(get(body, :imageUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = joinpath(proj_dir, "1", set_uid, "ccid.json")
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    s = init_object(project_uid, set_uid)
    s isa CciaSet || return 404, JSON3.write((; error="Not a set: $set_uid"))
    delete_image!(s, image_uid)

    @info "Deleted image" uid=image_uid set=set_uid project=project_uid
    200, JSON3.write((; ok=true))
end

# ── Metadata management ───────────────────────────────────────────────────────

function _parse_meta_request(body_bytes)
    data = try JSON3.read(String(body_bytes)) catch
        return nothing, nothing, "Invalid JSON body"
    end
    project_uid = String(get(data, :projectUid, ""))
    isempty(project_uid) && return nothing, nothing, "projectUid required"
    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return nothing, nothing, "Project not found: $project_uid"
    proj_dir, data, nothing
end

# Load each listed image as a CciaImage, apply f!(img), and persist via save!.
# Delegating to the model keeps every ccid.json field intact (status, attr,
# channel names, filepath versions) — see the CciaImage round-trip contract.
function _mutate_images!(f!::Function, project_uid::String, image_uids)
    for uid in image_uids
        img = init_object(project_uid, uid)
        img isa CciaImage || continue
        f!(img)
        save!(img)
    end
end

function api_images_attr_create(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = String(get(data, :attrName, ""))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))

    _mutate_images!(project_uid, image_uids) do img
        haskey(img.attr, attr_name) || (img.attr[attr_name] = "")
    end
    200, JSON3.write((; ok=true))
end

function api_images_attr_delete(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = String(get(data, :attrName, ""))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))

    _mutate_images!(project_uid, image_uids) do img
        delete!(img.attr, attr_name)
    end
    200, JSON3.write((; ok=true))
end

function api_images_attr_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = String(get(data, :attrName, ""))
    values_raw  = get(data, :values, nothing)
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    values = Dict{String,String}(String(k) => string(v) for (k, v) in values_raw)
    for (image_uid, val) in values
        _mutate_images!(project_uid, [image_uid]) do img
            img.attr[attr_name] = val
        end
    end
    200, JSON3.write((; ok=true))
end

function api_images_delete_labels(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    image_uid   = String(get(body, :imageUid,   ""))
    value_name  = String(get(body, :valueName,  ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error="valueName required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    task_dir = joinpath(proj_dir, "1", image_uid)
    ccid     = joinpath(task_dir, "ccid.json")
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))
    isfile(ccid)    || return 404, JSON3.write((; error="Image not found"))

    raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    # Delete zarr files registered under labels[valueName]
    labels_dict = get(raw, "labels", Dict{String,Any}())
    label_entry = get(labels_dict, value_name, get(labels_dict, Symbol(value_name), nothing))
    label_dir   = joinpath(task_dir, "labels")
    if !isnothing(label_entry)
        for fn in (label_entry isa AbstractVector ? label_entry : [string(label_entry)])
            p = joinpath(label_dir, string(fn))
            ispath(p) && rm(p; recursive = true)
        end
    end

    # Delete h5ad registered under label_props[valueName]
    label_props = get(raw, "label_props", Dict{String,Any}())
    h5ad_fn = get(label_props, value_name, get(label_props, Symbol(value_name), nothing))
    if !isnothing(h5ad_fn)
        h5ad_path = joinpath(task_dir, "labelProps", string(h5ad_fn))
        isfile(h5ad_path) && rm(h5ad_path)
    end

    # Update ccid.json
    raw["labels"]      = Dict{String,Any}(String(k) => v for (k, v) in labels_dict
                                          if string(k) != value_name)
    raw["label_props"] = Dict{String,Any}(String(k) => v for (k, v) in label_props
                                          if string(k) != value_name)
    open(ccid, "w") do io; JSON3.write(io, raw); end

    img = init_object(project_uid, image_uid)
    img isa CciaImage || return 200, JSON3.write((; ok = true))
    @info "Deleted label set" value_name image=image_uid project=project_uid
    200, JSON3.write((; ok = true, image = _image_payload(img)))
end

function api_images_channelnames(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    ch_names    = [String(n) for n in get(data, :channelNames, [])]
    isempty(image_uids) && return 400, JSON3.write((; error="imageUids required"))
    isempty(ch_names)   && return 400, JSON3.write((; error="channelNames required"))

    _mutate_images!(project_uid, image_uids) do img
        set_channel_names!(img, ch_names; check_length=false)
    end
    200, JSON3.write((; ok=true))
end

# Generic bulk merge into an image's `meta` dict — one endpoint for any meta field (physical
# size/unit, time interval, …) rather than a one-off route per field. `values` maps
# uid → partial dict of meta keys to merge in (same shape idea as api_images_attr_set, but the
# per-uid value is itself a dict instead of a scalar).
function api_images_meta_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    values_raw  = get(data, :values, nothing)
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    for (image_uid, fields_raw) in values_raw
        fields = Dict{String,Any}(String(k) => v for (k, v) in fields_raw)
        _mutate_images!(project_uid, [String(image_uid)]) do img
            for (k, v) in fields
                # a JSON `null` deletes the key (e.g. clearing a stale PhysicalSizeZ_raw marker
                # once a trusted value replaces an auto-corrected one) rather than setting it
                isnothing(v) ? delete!(img.meta, k) : (img.meta[k] = v)
            end
        end

        # Copy any physical-size/timing edit INTO the zarr's own calibration (`.zattrs` scale/units
        # + OME-XML `<Pixels>`), so napari renders what ccid.json/analysis use — without it a fix
        # only changes ccid's display copy and napari keeps showing the old value / "t = N". Same
        # translator the importer uses (`sync_zarr_calibration!`), so the field→zarr mapping lives in
        # ONE place. Targets the "default" (bioformats2raw) zarr, NOT the active version: processed
        # variants (drift/cellpose-correct) carry a flat NGFF layout with no unit/OME-XML, and
        # `resync_ome_meta!` re-reads the default anyway. See CLAUDE.md → OME-ZARR dual-format.
        if Cecelia.has_calibration_meta(fields)
            img = init_object(project_uid, String(image_uid))
            if img isa CciaImage
                zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
                (isnothing(zarr_path) || !isdir(zarr_path)) ||
                    Cecelia.sync_zarr_calibration!(zarr_path, fields)
            end
        end
    end
    200, JSON3.write((; ok=true))
end

# Backfill physical-size/timing meta for images imported before this metadata was tracked (or
# whose ccid.json lost these fields) — re-derives them from the already-converted OME-ZARR (same
# reader ImportOmezarr uses) without touching the original source file or re-running
# bioformats2raw. Returns the refreshed payload per uid so the frontend can drop the warning icon
# immediately, no page reload needed.
function api_images_meta_resync(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(image_uids) && return 400, JSON3.write((; error="imageUids required"))

    images = Dict{String,Any}()
    for uid in image_uids
        img = init_object(project_uid, uid)
        img isa CciaImage || continue
        resync_ome_meta!(img)
        reloaded = init_object(project_uid, uid)
        reloaded isa CciaImage && (images[uid] = _image_payload(reloaded))
    end
    200, JSON3.write((; ok=true, images=images))
end

# ── Internal helpers ──────────────────────────────────────────────────────────
# Project listing reads project.json directly (lightweight discovery, no object
# graph). Image/set payloads are sourced from the model (CciaImage/CciaSet) so
# ccid.json parsing lives in one place; the API only shapes the response.

function _scan_projects_raw()::Vector{Dict{String,Any}}
    isdir(projects_dir()) || return Dict{String,Any}[]
    projects = Dict{String,Any}[]
    for entry in readdir(projects_dir(); join=true)
        isdir(entry) || continue
        meta_file = joinpath(entry, "project.json")
        isfile(meta_file) || continue
        try
            raw  = JSON3.read(read(meta_file, String))
            proj = Dict{String,Any}(String(k) => v for (k, v) in raw)
            proj["path"] = entry
            haskey(proj, "type") || (proj["type"] = get(proj, "kind", "static"))
            push!(projects, proj)
        catch e
            @warn "Skipping malformed project" dir=entry exception=e
        end
    end
    sort!(projects; by=p -> string(get(p, "lastOpenedAt", get(p, "createdAt", ""))), rev=true)
    projects
end

function _meta_int(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) && return nothing
    v isa Integer ? v : tryparse(Int, string(v))
end

function _meta_float(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) && return nothing
    v isa Real ? Float64(v) : tryparse(Float64, string(v))
end

function _meta_str(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) ? nothing : string(v)
end

# Frontend-shaped payload for one image, sourced from the model. Response shaping
# (camelCase, field selection) is the API's job; data access goes through CciaImage
# so ccid.json parsing has a single home.
function _image_payload(img::CciaImage)
    fps = Dict{String,String}(k => v for (k, v) in img.filepath if k != VERSIONED_ACTIVE_KEY)
    # Lenient (no write-back): surface the default zarr if present but unregistered (legacy data).
    if isempty(fps) && isdir(joinpath(img_zero_dir(img), "ccidImage.ome.zarr"))
        fps["default"] = "ccidImage.ome.zarr"
    end
    active_vn = versioned_active(img.filepath)
    active_fn = something(active(img.filepath), get(fps, VERSIONED_DEFAULT_VAL, ""))
    ch        = channel_names(img)
    (;
        uid             = img.uid,
        name            = img.name,
        kind            = img.kind,
        status          = img.status,
        sizeC           = _meta_int(img.meta, "SizeC"),
        sizeT           = _meta_int(img.meta, "SizeT"),
        sizeZ           = _meta_int(img.meta, "SizeZ"),
        # Raw/nullable — NOT img_physical_sizes' 1.0-default-for-computation fallback. The UI
        # needs to tell "genuinely missing" apart from "explicitly confirmed 1.0".
        physicalSizeX     = _meta_float(img.meta, "PhysicalSizeX"),
        physicalSizeY     = _meta_float(img.meta, "PhysicalSizeY"),
        physicalSizeZ     = _meta_float(img.meta, "PhysicalSizeZ"),
        physicalSizeUnit  = _meta_str(img.meta, "PhysicalSizeUnit"),
        # set when the ImageJ-TIFF Z-spacing auto-fix overrode bioformats2raw's value at import
        # (see omezarr.jl) — the corrected number is still only as good as the source file's own
        # ImageJ tag, so the frontend keeps flagging it for the user to confirm, not just silently
        # trusting it because the ratio now looks plausible.
        physicalSizeZCorrected = haskey(img.meta, "PhysicalSizeZ_raw"),
        timeIncrement     = _meta_float(img.meta, "TimeIncrement"),
        timeIncrementUnit = _meta_str(img.meta, "TimeIncrementUnit"),
        channelNames    = isnothing(ch) ? String[] : ch,
        filepath        = active_fn,
        activeValueName = active_vn,
        filepaths       = fps,
        labels          = img.labels,
        attr            = img.attr,
    )
end

_set_payload(s::CciaSet) = (; uid=s.uid, name=s.name, images=[_image_payload(i) for i in s._images])
