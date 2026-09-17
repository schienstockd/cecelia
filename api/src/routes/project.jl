# ── Project management ────────────────────────────────────────────────────────

function api_projects_list(req::HTTP.Request)
    projects = _scan_projects_raw()
    200, JSON3.write((; projects, projectsDir=projects_dir()))
end

# GET /api/projects/bundles — .ccbundle files in the default export dir, for the import picker.
function api_projects_bundles(::HTTP.Request)
    try
        200, JSON3.write((; bundles = list_bundles(), exportDir = default_export_dir()))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# GET /api/projects/bundle-info?path= — peek a bundle's uid/name + whether that uid already exists,
# so the import UI can prompt (replace / copy / cancel) BEFORE unpacking.
function api_projects_bundle_info(req::HTTP.Request)
    path = get(HTTP.queryparams(HTTP.URI(req.target)), "path", "")
    isempty(path) && return 400, JSON3.write((; error = "path required"))
    info = bundle_info(path)
    isnothing(info) && return 400, JSON3.write((; error = "Not a cecelia bundle: $path"))
    200, JSON3.write(info)
end

function api_projects_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    name = String(strip(String(get(body, :name, ""))))
    isempty(name) && return 400, JSON3.write((; error="Project name is required"))

    existing = _scan_projects_raw()
    any(p -> get(p, "name", "") == name, existing) &&
        return 400, JSON3.write((; error="A project named \"$name\" already exists"))

    # Project-wide static/live/flow distinction was dropped — applicability is per-image, derived
    # from axes (see Cecelia.img_axes / task_applies). `kind` is retained on the struct as a
    # vestigial no-op so pre-existing project.json files still round-trip.
    proj = create_project!(name=name)
    meta = Dict{String,Any}("uid"=>proj.uid, "name"=>proj.name, "path"=>proj.root,
                             "meta"=>proj.meta, "set_uids"=>proj.set_uids,
                             "createdAt"=>string(now()), "lastOpenedAt"=>string(now()))
    @info "Created project" name uid=proj.uid
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
        write_json_atomic(meta_file, raw)
        project["lastOpenedAt"] = raw["lastOpenedAt"]
    catch e
        @warn "Could not update lastOpenedAt" uid exception=e
    end

    proj_obj = load_project(uid)

    # Close out runs abandoned by a process that died (a runner Ctrl-C or crash) — they are still
    # marked "running" in each image's run log because the code that would have closed them went with
    # the process. Doing it here means the provenance is already correct by the time the user looks at
    # it, which is the moment they ask. Safe no-op when nothing was interrupted; see runner_api.jl.
    reap_run_log_for_project!(proj_obj)

    sets = [_set_payload(s) for s in proj_obj._sets]

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

    # Normalised through the one reader (app/src/analysis_boards.jl) so the client always sees the
    # current shape and, crucially, the `version` its next autosave has to echo back. `null` when the
    # project has never saved a board.
    boards_doc = read_boards_doc(boards_doc_path(proj_dir))
    boards = boards_doc.present ? boards_doc_payload(boards_doc) : nothing

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
    boards === nothing && return 200, JSON3.write((; ok=true))
    path = boards_doc_path(joinpath(projects_dir(), uid))
    try
        current  = read_boards_doc(path)
        incoming = normalise_boards(boards)
        # OPTIMISTIC CONCURRENCY. The client echoes the version it last read; if the document has moved
        # on since (another browser tab, or the MCP add-a-board route later), reject rather than let the
        # later writer win silently — which is what two tabs open on one project used to do to each
        # other. The client reloads from the returned document and retries. A client that sends no
        # version at all is an OLD frontend against a new server: let it through rather than wedge the
        # autosave, since that is exactly the pairing a mid-session reload produces.
        want = get(body, :version, nothing)
        sent = want isa Integer ? Int(want) : want isa Real ? Int(round(want)) : -1
        if want !== nothing && current.present && current.readable && sent != current.version
            return 409, JSON3.write((; error="Boards changed since you loaded them",
                                       code="stale_version", boards=boards_doc_payload(current)))
        end
        version = write_boards_doc(path, incoming; version = current.version + 1)
        # Tell every OTHER open client to pick this up. The writer identifies its own echo by `clientId`,
        # NOT by version: this broadcast goes out before the response does, so the writer still holds the
        # pre-write version when its own frame arrives — a version test made every autosave reload and
        # re-render the board that had just been saved. Absent for a non-browser writer (the MCP
        # add-a-board route), which is correct: every browser should pick that one up.
        broadcast_ws(Dict{String,Any}("type" => "boards:changed", "projectUid" => uid,
                                      "version" => version,
                                      "clientId" => String(get(body, :clientId, ""))))
        return 200, JSON3.write((; ok=true, version))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/boards/add  { projectUid, name, plots:[…], template? }
# CREATE-ONLY: adds ONE board and can never touch an existing one — the write surface behind the MCP
# `add_analysis_board` tool (docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 3). Deliberately distinct from
# the autosave above, exactly as /api/chains/create is distinct from /api/chains/save: allow-listing the
# autosave would have let a caller replace every board in the project with one request, and the server
# could not have validated a single field of it.
#
# 409 on a duplicate name, 422 on a spec the project cannot plot (unknown plot id, a chart that spec
# doesn't offer, a population that doesn't exist) — rejected BEFORE writing, because a bad `tkey`
# renders an empty panel with no error at all. The expansion and every check live in the package
# (`expand_board`), so they are headless-testable and identical from the REPL.
function api_boards_add(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    proj = try load_project(uid) catch e
        return 404, JSON3.write((; error="Could not load project: $(sprint(showerror, e))"))
    end
    # Normalised on the way IN, so the duplicate check below, the stored tab and the response all talk
    # about the same string. `append_board` normalises again (it owns the invariant, and the REPL reaches
    # it without this route); `board_display_name` is idempotent, so doing both costs nothing. Without
    # this, an agent that HTML-escaped an ampersand got a tab titled "Behaviour &amp; tracking" that it
    # could not rename — add_analysis_board is add-only.
    name = board_display_name(String(get(body, :name, "")))
    plots = get(body, :plots, nothing)
    template = String(get(body, :template, ""))
    path = boards_doc_path(joinpath(projects_dir(), uid))
    try
        doc = read_boards_doc(path)
        doc.present && !doc.readable &&
            return 409, JSON3.write((; error="The project's boards file could not be read; not adding to it"))
        # Name collision is 409 (a conflict with existing state) rather than 422 (a bad spec), and is
        # checked before expanding so the caller is told the cheap thing first. `append_board` asserts
        # it again — it is the invariant's owner, and the REPL reaches it without this route.
        if any(t -> t isa AbstractDict &&
                    strip(string(get(t, :name, get(t, "name", "")))) == strip(name), doc.tabs)
            return 409, JSON3.write((; error="A board named \"$(strip(name))\" already exists in this project",
                                       code="duplicate_board_name"))
        end
        layout = expand_board(proj, name, plots; template = template,
                              compare_by = String(get(body, :compareBy, "")))
        updated, id = append_board(doc, name, layout)
        version = write_boards_doc(path, updated; version = doc.version + 1)
        broadcast_ws(Dict{String,Any}("type" => "boards:changed", "projectUid" => uid, "version" => version))
        return 200, JSON3.write((; ok=true, tabId=id, name=strip(name), version,
                                   slots=length(layout["contents"])))
    catch e
        e isa BoardSpecError && return 422, JSON3.write((; error=e.msg, code="invalid_board_spec"))
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# GET /api/projects/boards?projectUid — the boards document on its own, with its `version`.
# The cheap read behind both recovery paths: a 409'd autosave reloading before it retries, and a client
# reacting to the `boards:changed` broadcast. Project OPEN still gets boards inline in
# api_projects_load — this exists so neither of those has to re-run a whole project load.
function api_projects_boards_get(req::HTTP.Request)
    uid = String(get(HTTP.queryparams(HTTP.URI(req.target)), "projectUid", ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    200, JSON3.write((; boards=boards_doc_payload(read_boards_doc(boards_doc_path(joinpath(projects_dir(), uid))))))
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
            write_json_atomic(joinpath(settings, "animations.json"), animations)
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
                write_json_atomic(joinpath(objdir, "moduleCanvases.json"), data)
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
        write_json_atomic(meta_file, raw)
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
        # Julia's recursive rm walks with readdir; on huge OME-ZARR trees (dimension_separator="/")
        # a mid-walk readdir can hit ENOENT on a stale subpath and throw even though the top-level
        # got torn down. If the target is gone the delete effectively succeeded — surface success
        # so the UI refreshes instead of showing the raw error.
        isdir(proj_dir) &&
            return 500, JSON3.write((; error="Failed to delete project: " * sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, uid))
end

