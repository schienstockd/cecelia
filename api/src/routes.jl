using Dates
import Base64   # decode base64 PNGs when migrating legacy inline board images to sidecar files

# ── Chain template CRUD ───────────────────────────────────────────────────────

# Per-project persisted UI config lives under `<proj>/settings/` (chains, analysis-canvas boards, …).
_settings_dir_for_project(project_uid::String) = joinpath(projects_dir(), project_uid, "settings")

# Analysis-board image assets (napari screenshots) live as SIDECAR PNGs under settings/board-assets/,
# NOT base64 inside analysisBoards.json — so the board JSON stays small (autosave-friendly) and the
# images are transparent on disk. See docs/todo/ANIMATION_PLAN.md.
_board_assets_dir(project_uid::String) = joinpath(_settings_dir_for_project(project_uid), "board-assets")
_valid_asset_id(id::AbstractString) = occursin(r"^[A-Za-z0-9_-]+$", id)   # guard against path traversal

# Copy a captured PNG (temp file) into settings/board-assets/<id>.png; returns the new asset id.
function _save_board_asset_file(project_uid::String, src_png::String)::String
    dir = _board_assets_dir(project_uid); mkpath(dir)
    id = gen_uid()
    cp(src_png, joinpath(dir, id * ".png"); force = true)
    id
end

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

    # ── User drop-in modules (custom tasks) ────────────────────────────────────
    # Same directory-driven contract as the built-ins, but rooted at the per-user config dir
    # (<config_dir>/modules/inputDefinitions/<category>/<name>.json). Category = subdir name, so a
    # custom task in an existing category (e.g. behaviour/) appears in that module page automatically.
    # Built-ins win on a fun_name clash. See docs/CUSTOM_MODULES.md and Cecelia.load_custom_modules!.
    builtin_funs = Set{String}()
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        isempty(fn) || push!(builtin_funs, fn)
    end
    user_defs_root = joinpath(Cecelia.config_dir(), "modules", "inputDefinitions")
    if isdir(user_defs_root)
        for entry in readdir(user_defs_root; join=true)
            isdir(entry) || continue
            category = basename(entry)
            (!isempty(cat) && category != cat) && continue
            for f in readdir(entry; join=true)
                endswith(f, ".json") || continue
                try
                    parsed = JSON3.read(read(f, String), Dict{String,Any})
                    fn     = string(get(parsed, "fun_name", ""))
                    (isempty(fn) || fn ∈ builtin_funs) && continue   # need a fun_name; built-ins win
                    resolved = Cecelia._resolve_spec_includes(parsed, frag_dir)
                    push!(get!(raw, category, Any[]), resolved)
                catch e
                    @warn "Skipping malformed custom task spec" path=f exception=e
                end
            end
        end
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

# ── Custom (user drop-in) modules ─────────────────────────────────────────────
# GET  /api/tasks/custom-modules         → load report + category list (see below)
# POST /api/tasks/custom-modules/reload  → rescan <config_dir>/modules for NEWLY dropped .jl, then
#                                          return the same report. (Edits to already-loaded modules
#                                          need a server restart — same as any app/ struct change.)
# See docs/CUSTOM_MODULES.md and Cecelia.load_custom_modules!.

# Categories present among the user's custom specs, each flagged whether a built-in page already owns
# that category (a matching dir under app/src/tasks). The frontend renders a generic page + nav entry
# only for the NEW categories (builtin == false); tasks in an existing category already show there.
function _custom_module_categories()
    user_defs_root = joinpath(Cecelia.config_dir(), "modules", "inputDefinitions")
    isdir(user_defs_root) || return Any[]
    builtin = Set(basename(e) for e in readdir(_TASK_SPECS_ROOT; join=true) if isdir(e))
    cats = Any[]
    for entry in readdir(user_defs_root; join=true)
        isdir(entry) || continue
        category = basename(entry)
        funs = String[]
        for f in readdir(entry; join=true)
            endswith(f, ".json") || continue
            try
                parsed = JSON3.read(read(f, String), Dict{String,Any})
                fn = string(get(parsed, "fun_name", ""))
                isempty(fn) || push!(funs, fn)
            catch
            end
        end
        isempty(funs) && continue
        # cohortFuns = the category's funs that bank cohort-comparable metrics (Cecelia.COHORT_METRICS,
        # populated at load incl. custom modules' register_cohort_metrics!). Drives the "Check cohort"
        # button on the generic custom page WITHOUT any hardcoded per-page list — a custom module that
        # declares its metrics gets the button automatically.
        cohort_funs = String[f for f in funs if haskey(Cecelia.COHORT_METRICS, f)]
        push!(cats, (; name = category, builtin = category ∈ builtin, funNames = funs, cohortFuns = cohort_funs))
    end
    cats
end

function api_custom_modules_status(::HTTP.Request)
    200, JSON3.write((; dir        = Cecelia.custom_modules_dir(),
                        modules    = Cecelia.custom_modules_report(),
                        categories = _custom_module_categories()))
end

function api_custom_modules_reload(::Vector{UInt8})
    res = Cecelia.load_custom_modules!()
    200, JSON3.write((; dir        = Cecelia.custom_modules_dir(),
                        loaded     = res.loaded,
                        skipped    = res.skipped,
                        removed    = res.removed,
                        failed     = [(; path = p, error = m) for (p, m) in res.failed],
                        modules    = Cecelia.custom_modules_report(),
                        categories = _custom_module_categories()))
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
        raw = read_ccid_raw(meta_file)
        raw["lastOpenedAt"] = string(now())
        open(meta_file, "w") do io; JSON3.write(io, raw); end
        project["lastOpenedAt"] = raw["lastOpenedAt"]
    catch e
        @warn "Could not update lastOpenedAt" uid exception=e
    end

    proj_obj = load_project(uid)
    sets     = [_set_payload(s) for s in proj_obj._sets]

    # Analysis-canvas boards saved with the project (settings/); null when none saved yet.
    # Animation page: captured view snapshots (settings/animations.json). Sidecar PNGs live in the same
    # board-assets/ store as the board strip (shared capture path), so this JSON stays small.
    animations = nothing
    anim_file = joinpath(_settings_dir_for_project(uid), "animations.json")
    if isfile(anim_file)
        try; animations = JSON3.read(read(anim_file, String)); catch e
            @warn "Could not read animations" uid exception=e
        end
    end

    boards = nothing
    boards_file = joinpath(_settings_dir_for_project(uid), "analysisBoards.json")
    if isfile(boards_file)
        try; boards = JSON3.read(read(boards_file, String)); catch e
            @warn "Could not read analysis boards" uid exception=e
        end
    end

    # Per-object module-page canvas layouts, stored WITH each object at 1/{uid}/moduleCanvases.json
    # (like ccid.json / labelProps — locality + auto-cleanup on delete). Reassemble the per-canvas-key
    # map by merging every object's file; null when none saved.
    moduleCanvases = nothing
    onedir = joinpath(proj_dir, "1")
    if isdir(onedir)
        merged_entries = Dict{String,Any}(); merged_geom = Dict{String,Any}()
        for obj in readdir(onedir)
            f = joinpath(onedir, obj, "moduleCanvases.json")
            isfile(f) || continue
            try
                d = JSON3.read(read(f, String))
                for (k, v) in pairs(get(d, :entries, Dict{Symbol,Any}())); merged_entries[String(k)] = v; end
                for (k, v) in pairs(get(d, :geom, Dict{Symbol,Any}()));    merged_geom[String(k)] = v;    end
            catch e
                @warn "Could not read module canvases" obj exception=e
            end
        end
        (isempty(merged_entries) && isempty(merged_geom)) ||
            (moduleCanvases = Dict("entries" => merged_entries, "geom" => merged_geom))
    end

    @info "Opened project" name=get(project, "name", "?") uid sets=length(sets)
    200, JSON3.write((; project, sets, boards, moduleCanvases, animations))
end

# POST /api/projects/boards  { projectUid, boards: { tabs, layouts } }
# Debounced AUTOSAVE of the /analysis boards (tabs + grid layouts + slot state incl. strip snapshots) →
# settings/analysisBoards.json. Board IMAGES are sidecar files (board-assets/, see below), NOT base64
# in this JSON, so it stays small and cheap to rewrite on every edit. Mirrors api_projects_canvases (the
# module-page autosave). Opaque frontend JSON, stored verbatim. `lastOpenedAt` is stamped on project
# OPEN (api_projects_load), so there's nothing to touch in project.json here. Replaces the old
# api_projects_save + the manual save button.
function api_projects_boards(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    boards = get(body, :boards, nothing)
    if boards !== nothing
        try
            settings = _settings_dir_for_project(uid); mkpath(settings)
            open(joinpath(settings, "analysisBoards.json"), "w") do io; JSON3.write(io, boards); end
        catch e
            return 500, JSON3.write((; error=sprint(showerror, e)))
        end
    end
    200, JSON3.write((; ok=true))
end

# POST /api/projects/animations  { projectUid, animations }
# Debounced AUTOSAVE of the Animation page's captured view snapshots → settings/animations.json. The
# frame PNGs are sidecar files (board-assets/, shared with the board strip), so this JSON stays small.
# Mirrors api_projects_boards. Opaque frontend JSON, stored verbatim.
function api_projects_animations(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    animations = get(body, :animations, nothing)
    if animations !== nothing
        try
            settings = _settings_dir_for_project(uid); mkpath(settings)
            open(joinpath(settings, "animations.json"), "w") do io; JSON3.write(io, animations); end
        catch e
            return 500, JSON3.write((; error=sprint(showerror, e)))
        end
    end
    200, JSON3.write((; ok=true))
end

# POST /api/board-assets/save  { projectUid, png(base64) }  → { assetId }
# Write a board image to a sidecar PNG (settings/board-assets/<id>.png) and return its id. Used to
# MIGRATE legacy boards that still carry inline base64 in a cell's `src` into a sidecar on first load.
# (Fresh captures are saved directly by the screenshot endpoint — no base64 round-trip.)
function api_board_asset_save(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); png = String(get(body, :png, ""))
    (isempty(uid) || isempty(png)) && return 400, JSON3.write((; error="projectUid and png required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    b64 = replace(png, r"^data:image/[^;]+;base64," => "")   # tolerate a data-URL prefix
    bytes = try Base64.base64decode(b64) catch
        return 400, JSON3.write((; error="Invalid base64 png"))
    end
    try
        dir = _board_assets_dir(uid); mkpath(dir); id = gen_uid()
        write(joinpath(dir, id * ".png"), bytes)
        return 200, JSON3.write((; assetId = id))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/board-assets/delete  { projectUid, assetId }  → { ok }
# Best-effort removal of a sidecar board image (when a frame/board is deleted). Missing file is fine.
function api_board_asset_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); aid = String(get(body, :assetId, ""))
    (isempty(uid) || isempty(aid)) && return 400, JSON3.write((; error="projectUid and assetId required"))
    _valid_asset_id(aid) || return 400, JSON3.write((; error="Invalid assetId"))
    f = joinpath(_board_assets_dir(uid), aid * ".png")
    isfile(f) && rm(f; force=true)
    200, JSON3.write((; ok=true))
end

# POST /api/board-assets/copy  { projectUid, assetId }  → { assetId }
# Duplicate a sidecar board image to a NEW id — so a duplicated board owns independent asset files
# (deleting a frame in one board must not orphan the copy that shares it). Missing source → 404.
function api_board_asset_copy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); aid = String(get(body, :assetId, ""))
    (isempty(uid) || isempty(aid)) && return 400, JSON3.write((; error="projectUid and assetId required"))
    _valid_asset_id(aid) || return 400, JSON3.write((; error="Invalid assetId"))
    src = joinpath(_board_assets_dir(uid), aid * ".png")
    isfile(src) || return 404, JSON3.write((; error="Asset not found: $aid"))
    try
        dir = _board_assets_dir(uid); mkpath(dir); id = gen_uid()
        cp(src, joinpath(dir, id * ".png"); force=true)
        return 200, JSON3.write((; assetId = id))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/projects/canvases  { projectUid, objects: { <objUid>: {entries, geom} } }
# Autosaved module-page canvas layouts, written PER OBJECT to 1/{objUid}/moduleCanvases.json (the
# object = the image or set the canvas is scoped to; frontend groups by canvas key). Stored with the
# object → survives with it and is removed when it's deleted; the debounced autosave rewrites only the
# object(s) that changed, never a global blob. Opaque frontend JSON, stored verbatim.
function api_projects_canvases(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    objects = get(body, :objects, nothing)
    if objects !== nothing
        for (objUid, data) in pairs(objects)
            objdir = joinpath(projects_dir(), uid, "1", String(objUid))
            isdir(objdir) || continue   # object deleted/unknown → skip (no stray files)
            try
                open(joinpath(objdir, "moduleCanvases.json"), "w") do io; JSON3.write(io, data); end
            catch e
                @warn "Could not save module canvases" uid obj=String(objUid) exception=e
            end
        end
    end
    200, JSON3.write((; ok=true))
end

# POST /api/images/value-name-check  { projectUid, valueName, imageUids: [...] }
# Partition images by whether they carry the labelProps value_name (segmentation) — a generic
# building block for any feature that must skip images lacking a value_name (e.g. copy gating across
# images). Just a value_name-presence check per image (img_has_value_name); returns {available, missing}.
function api_images_value_name_check(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    proj = String(get(body, :projectUid, ""))
    vn   = String(get(body, :valueName, ""))
    uids = get(body, :imageUids, nothing)
    (uids isa AbstractVector) || return 400, JSON3.write((; error="imageUids required"))
    isdir(joinpath(projects_dir(), proj)) || return 404, JSON3.write((; error="Project not found: $proj"))
    available = String[]; missing = String[]
    for u in uids
        uid = String(u)
        ok = try
            img = init_object(proj, uid)
            img isa CciaImage && img_has_value_name(img, vn)
        catch; false end
        ok ? push!(available, uid) : push!(missing, uid)
    end
    200, JSON3.write((; available, missing))
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
        raw = read_ccid_raw(meta_file)
        raw["name"] = name
        open(meta_file, "w") do io; JSON3.write(io, raw); end
    catch
        return 500, JSON3.write((; error="Failed to write project metadata"))
    end
    200, JSON3.write((; ok=true, name))
end

# delete → permanently remove a project directory from disk. Body {uid}. The frontend guards against
# deleting the currently-open project; this is the raw removal (the recent list is a scan of
# projects_dir, so it refreshes automatically). Destructive + irreversible.
function api_projects_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :uid, ""))
    isempty(uid) && return 400, JSON3.write((; error="uid required"))
    proj_dir = joinpath(projects_dir(), uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))
    try
        rm(proj_dir; recursive=true, force=true)
    catch e
        return 500, JSON3.write((; error="Failed to delete project: " * sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, uid))
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

# POST /api/import/scan-legacy {sourceProjectDir, rscript?, imageUids?} → read-only preview manifest
# of a legacy R/Shiny cecelia project (what will/won't transfer per image). See
# python/cecelia/tasks/importImages/scan_legacy_run.py and docs/todo/LEGACY_MIGRATION_PLAN.md.
function api_import_scan_legacy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    src = String(get(body, :sourceProjectDir, ""))
    isempty(src) && return 400, JSON3.write((; error="sourceProjectDir required"))
    abs_src = isabspath(src) ? src : joinpath(FS_ROOT, src)
    isdir(joinpath(abs_src, "ANALYSIS")) ||
        return 400, JSON3.write((; error="Not a legacy cecelia project (no ANALYSIS/ dir): $abs_src"))

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "scan.result.json")
    params = Dict{String,Any}("sourceProjectDir" => abs_src, "resultPath" => result_file,
                              "rscript" => String(get(body, :rscript, "Rscript")))
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
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    src         = String(get(body, :sourceProjectDir, ""))
    rsc         = String(get(body, :rscript, ""))
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

    task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    registered = Dict{String,Any}[]
    for im in imgs_in
        uid  = String(get(im, :uid, ""))
        isempty(uid) && continue
        name = String(get(im, :name, uid))
        kind = String(get(im, :kind, "static"))
        meta = Dict{String,Any}("legacySourceDir" => abs_src, "legacySourceUid" => uid)
        isempty(rsc) || (meta["legacyRscript"] = rsc)
        img = add_image!(s; name=name, kind=kind, uid=uid, meta=meta)
        for subdir in values(task_dirs)
            mkpath(joinpath(proj_dir, "1", img.uid, string(subdir)))
        end
        push!(registered, Dict{String,Any}(
            "uid" => img.uid, "name" => img.name, "kind" => img.kind, "status" => "pending"))
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
    isfile(joinpath(proj_dir, "1", image_uid, "ccid.json")) ||
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
    for s in proj._sets, img in images(s)
        push!(imgs, (; uid=img.uid, name=img.name, status=img.status, setUid=s.uid, setName=s.name))
    end
    200, JSON3.write((; projectUid=project_uid, name=proj.name, kind=proj.kind,
                        count=length(imgs), sets, images=imgs))
end

# GET /api/images/tasklog?projectUid&imageUid&fun → the raw task log for one fun on one image.
# Reads {img._dir}/logs/{fun}.log (written by _wrap_log_with_file in the scheduler). Read-only;
# backs the MCP observer's get_task_log tool. Returns exists=false + "" when no log exists yet.
function api_images_tasklog(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    fun         = get(query, "fun", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(fun)         && return 400, JSON3.write((; error="fun required"))
    # fun becomes a filename ({fun}.log) — reject separators / traversal so it can't escape logs/
    (occursin('/', fun) || occursin('\\', fun) || occursin("..", fun)) &&
        return 400, JSON3.write((; error="invalid fun"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    img_dir = joinpath(proj_dir, "1", image_uid)
    isfile(joinpath(img_dir, "ccid.json")) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    logfile = joinpath(img_dir, "logs", fun * ".log")
    isfile(logfile) || return 200, JSON3.write((; projectUid=project_uid, imageUid=image_uid,
                                                  fun, exists=false, content=""))
    content = read(logfile, String)
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
        rs = _rl(e, "status")                        # per-RUN outcome; legacy entries have none → "done"
        push!(rows, Dict{String,Any}(
            "imageUid" => img.uid, "imageName" => img.name, "status" => img.status,  # image's status
            "runStatus" => (isempty(rs) ? "done" : rs),                              # this run's outcome
            "fun" => _rl(e, "fun"), "valueName" => _rl(e, "valueName"), "at" => _rl(e, "at")))
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
    value_name = get(q, "valueName", VERSIONED_DEFAULT_VAL)
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
    doc = try
        cohort_qc_for(set, fun_name, value_name; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(doc)
end

# POST /api/qc/cohort/check — the explicit "Check cohort consistency" action: recompute AND persist
# (set sidecar + per-image `cohort.{fun}` findings so outliers surface on the image). Body:
# {projectUid, setUid, funName, valueName?, threshold?}. This is the ONLY cohort write path.
function api_qc_cohort_check(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, "")); set_uid = String(get(body, :setUid, ""))
    fun_name    = String(get(body, :funName, ""))
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    haskey(COHORT_METRICS, fun_name) ||
        return 400, JSON3.write((; error = "No cohort metrics for fun '$fun_name'",
                                   known = sort(collect(keys(COHORT_METRICS)))))
    value_name = String(get(body, :valueName, VERSIONED_DEFAULT_VAL))
    tv  = get(body, :threshold, nothing)
    thr = tv isa Real ? Float64(tv) : Cecelia._COHORT_MODZ_THRESHOLD
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    doc = try
        cohort_qc_for!(set, fun_name, value_name; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    # Cecelia authors a lab-log summary ONLY when something flagged (an all-clear would be noise; the
    # toast covers that). Best-effort — a lab-log hiccup never fails the check. Author "Cecelia — …"
    # so the append route treats it as a Cecelia digest (no observer re-broadcast).
    if Cecelia.cohort_has_outliers(doc)
        try
            proj = load_project(project_uid)
            Cecelia.append_lab_log!(proj, "Cecelia — Cohort QC", Cecelia.cohort_qc_summary_lines(doc))
        catch e
            @warn "cohort check: lab-log append failed" exception = e
        end
    end
    200, JSON3.write(doc)
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

    raw = read_ccid_raw(ccid)

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
                # recompute calibration QC from the saved meta so a fixed image clears its warning
                # (or a bad edit re-flags it) — the image-table indicator reads this, not the payload.
                Cecelia.write_metadata_qc!(img)
            end
        end
    end
    200, JSON3.write((; ok=true))
end

# Set include/exclude (+ optional note) for one or more images. `values` maps uid → a partial
# dict {included?, note?}; only the keys present are changed (toggle inclusion without clobbering a
# note, or edit a note without touching inclusion). First-class CciaImage fields, so this rounds
# through the model (save! preserves every other field) rather than the meta bag.
function api_images_inclusion_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = String(get(data, :projectUid, ""))
    values_raw  = get(data, :values, nothing)
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    for (image_uid, fields_raw) in values_raw
        fields = Dict{String,Any}(String(k) => v for (k, v) in fields_raw)
        _mutate_images!(project_uid, [String(image_uid)]) do img
            haskey(fields, "included") && (img.included = Bool(fields["included"]))
            haskey(fields, "note")     && (img.note     = string(fields["note"]))
        end
        # Notify observers (mcp/) that a note was set — first-class user context (OBSERVER.md §4).
        if haskey(fields, "note")
            broadcast_ws(Dict{String,Any}(
                "type" => "image_note_added", "projectUid" => project_uid,
                "imageUid" => String(image_uid), "note" => string(fields["note"])))
        end
    end
    200, JSON3.write((; ok=true))
end

# ── Lab log (per-project append-only markdown; see docs/ai-assist/LAB-LOG.md) ─────────────────────
# read → raw content + parsed entries (newest-first) + file mtime (unix seconds, nothing if absent).
function api_lablog_read(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    content = read_lab_log(proj)
    p = lab_log_path(proj)
    200, JSON3.write((; content, entries=parse_lab_log(content),
                        tuning=read_tuning(proj), mutes=read_mutes(proj),
                        pageCategories=lab_log_page_categories(), operationCategories=lab_log_operation_categories(),
                        dismissed=read_dismissed(proj),
                        mtime=(isfile(p) ? mtime(p) : nothing)))
end

# tune → set/clear a "Tuning"-mode rating (useful/noise) for an entry id. Config sidecar, not the
# log. Body {projectUid, id, vote} where vote ∈ "up"|"down"|"" (clear). Returns the updated map.
function api_lablog_tune(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    entry_id    = String(get(body, :id, ""))
    vote        = String(get(body, :vote, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(entry_id)    && return 400, JSON3.write((; error="id required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    tuning = try
        set_tuning!(proj, entry_id, vote)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, tuning))
end

# mute → mute/unmute a whole digest CATEGORY (tasks|populations|exclusions) from future captures.
# Config sidecar (settings/lab-log-mutes.json), not the log. Body {projectUid, category, muted}.
function api_lablog_mute(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    category    = String(get(body, :category, ""))
    muted       = Bool(get(body, :muted, false))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(category)    && return 400, JSON3.write((; error="category required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    mutes = try
        set_mute!(proj, category, muted)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, mutes))
end

# dismiss → hide/un-hide a single entry from the PANEL (config sidecar; the log file is never edited —
# append-only). Body {projectUid, id, dismissed}. Returns the updated dismissed-id list.
function api_lablog_dismiss(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    entry_id    = String(get(body, :id, ""))
    dismissed   = Bool(get(body, :dismissed, false))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(entry_id)    && return 400, JSON3.write((; error="id required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    ids = try
        set_dismissed!(proj, entry_id, dismissed)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, dismissed=ids))
end

# append → one dated, author-tagged block. Server injects date + author tag (append-only, lock-guarded
# in append_lab_log!); body is {projectUid, author, lines: string | [string]}.
function api_lablog_append(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    author      = String(get(body, :author, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(author)      && return 400, JSON3.write((; error="author required"))
    lines_raw = get(body, :lines, nothing)
    lines = if lines_raw isa AbstractString
        [String(lines_raw)]
    elseif lines_raw isa AbstractVector
        String[String(l) for l in lines_raw]
    else
        return 400, JSON3.write((; error="lines required (string or array of strings)"))
    end
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    block = try
        append_lab_log!(proj, author, lines)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    # Notify observers (mcp/) of USER-written entries only — not the observer's own [Claude] writes
    # (would loop) nor [Cecelia] auto-digests (not a user decision). See OBSERVER.md §4.
    let a = lowercase(strip(author))
        if !startswith(a, "claude") && !startswith(a, "cecelia")
            broadcast_ws(Dict{String,Any}(
                "type" => "lab_log_entry_added", "projectUid" => project_uid,
                "summary" => join(lines, " ")))
        end
    end
    200, JSON3.write((; ok=true, block, entries=parse_lab_log(read_lab_log(proj))))
end

# capture → append an auto-generated [Cecelia] digest of run-log activity since the last capture.
# Returns captured=false (and appends nothing) when there's no new activity. Backs the panel's
# "Capture" button and the auto-on-open toggle.
function api_lablog_capture(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    block = try
        capture_context!(proj)
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, captured=(block !== nothing), block,
                        entries=parse_lab_log(read_lab_log(proj))))
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

# QC docs for the payload. Persisted sidecars (read_all_qc) PLUS a fallback calibration doc computed
# live from the current meta when none is persisted yet — so images imported before metadata QC was
# banked still show their calibration warnings in the table (and a `write_metadata_qc!` on the next
# import/resync/edit persists the same thing for MCP/lab-log/whiteboard). Persisted wins when present
# (it's kept fresh by the wired edit paths), so a future richer importImages.omezarr doc isn't clobbered.
function _image_qc_payload(img::CciaImage)
    docs = Cecelia.read_all_qc(img)
    key  = "importImages.omezarr/" * Cecelia.VERSIONED_DEFAULT_VAL
    if !haskey(docs, key)
        docs[key] = Dict{String,Any}("funName" => "importImages.omezarr",
            "valueName" => Cecelia.VERSIONED_DEFAULT_VAL,
            "findings" => Cecelia.metadata_qc_findings(img.meta))
    end
    docs
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
    active_fn = something(versioned_get(img.filepath), get(fps, VERSIONED_DEFAULT_VAL, ""))
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
        # Include/exclude in further processing (default true). Excluded images are greyed in the
        # GUI, unselectable for runs, and hard-skipped by the runners; `note` is the optional reason.
        included        = img.included,
        note            = img.note,
        # QC findings per "funName/valueName" (docs/todo/QC_PLAN.md) — advisory "output looks off"
        # flags the GUI renders as a badge + tooltip. Includes the live calibration fallback so
        # pre-migration images still surface metadata warnings (see _image_qc_payload).
        qc              = _image_qc_payload(img),
        # automatic provenance: which task functions ran on this image + when ({fun, valueName, at});
        # the image table shows it in a cog popover after the uid. Appended by the scheduler on success.
        runLog          = read_run_log(img),
    )
end

_set_payload(s::CciaSet) = (; uid=s.uid, name=s.name, images=[_image_payload(i) for i in s._images])
