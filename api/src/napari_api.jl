# ── Napari bridge (API layer) ──────────────────────────────────────────────────
# One global NapariViewer per server process. Lifecycle: lazy-create on first
# open request. The package's launch!() handles the bridge process internally.

const _viewer_ref      = Ref{Union{NapariViewer,Nothing}}(nothing)
const _viewer_lock     = ReentrantLock()
const _pending_open    = Ref{Any}(nothing)
const _viewer_starting = Ref(false)

# Runtime toggle for launching the bridge on the discrete GPU (see app/src/napari.jl → launch!).
# Authoritative at launch time. `nothing` = not yet resolved → seed lazily from the
# CECELIA_NAPARI_DISCRETE_GPU env var, then the `[napari].discreteGpu` config default. The Settings
# switch flips it via POST /api/napari/gpu; the frontend re-asserts its saved choice each session.
const _napari_discrete_gpu = Ref{Union{Bool,Nothing}}(nothing)

function _napari_gpu()::Bool
    isnothing(_napari_discrete_gpu[]) || return _napari_discrete_gpu[]
    envv = lowercase(strip(get(ENV, "CECELIA_NAPARI_DISCRETE_GPU", "")))
    seed = envv in ("1", "true", "yes", "on") ? true :
           envv in ("0", "false", "no", "off") ? false :
           napari_discrete_gpu()
    _napari_discrete_gpu[] = seed
    seed
end

# Track what's currently open so we can auto-save before switching images
const _current_zarr_path = Ref{Union{String,Nothing}}(nothing)
const _current_task_dir  = Ref{Union{String,Nothing}}(nothing)
# which image uid is currently shown — stamped into screenshot provenance (zoom-to-source)
const _current_image_uid = Ref{Union{String,Nothing}}(nothing)

# Serialise all interaction with the single bridge process. Under `-t auto` two concurrent napari
# requests would otherwise interleave command sequences on the one bridge (e.g. a screenshot mid-open,
# or two opens racing the `_current_*` refs → a stale auto-save target). Hold this around each
# handler's full send-sequence so bridge interaction is single-flighted. `_viewer_lock` is reentrant,
# so `_ensure_viewer!`'s own locking nests fine.
_with_viewer(f) = lock(f, _viewer_lock)

# coerce a JSON value (Int/Float/String/null) to a non-negative Int; blank/garbage → 0.
# Used for the z-window dial, which can arrive as null (empty input) or a float.
function _to_int(x)::Int
    x === nothing && return 0
    x isa Integer && return max(0, Int(x))
    x isa Real    && return max(0, round(Int, x))
    x isa AbstractString && return (n = tryparse(Int, x); n === nothing ? 0 : max(0, n))
    0
end

function _props_path(task_dir::String, zarr_path::String)::String
    joinpath(task_dir, "data", basename(zarr_path) * ".pkl")
end

function _try_save_layer_props!(v::NapariViewer, task_dir::String, zarr_path::String)
    try
        props_file = _props_path(task_dir, zarr_path)
        mkpath(dirname(props_file))
        send(v, Dict{String,Any}("type" => "save_layer_props", "path" => props_file))
        @info "Auto-saved layer props" props_file
    catch e
        @warn "Auto-save layer props failed" exception = e
    end
end

function _try_load_layer_props!(v::NapariViewer, task_dir::String, zarr_path::String)
    try
        props_file = _props_path(task_dir, zarr_path)
        isfile(props_file) || return
        send(v, Dict{String,Any}("type" => "load_layer_props", "path" => props_file))
        @info "Auto-loaded layer props" props_file
    catch e
        @warn "Auto-load layer props failed" exception = e
    end
end

# Point the bridge's LIVE (debounced) autosave at the current image's props file and enable/disable it.
# Sent after each open (layers are recreated per open → the bridge must reconnect to the fresh layers)
# and on a live toggle. The bridge saves the moment the user changes contrast/colormap/T-Z, so the view
# survives a crash — not just an image switch. Call after any load so it doesn't echo the load back.
function _configure_autosave!(v::NapariViewer, task_dir::String, zarr_path::String, enabled::Bool)
    try
        props_file = _props_path(task_dir, zarr_path)
        mkpath(dirname(props_file))
        send(v, Dict{String,Any}("type" => "configure_autosave",
                                 "path" => props_file, "enabled" => enabled))
    catch e
        @warn "Configure autosave failed" exception = e
    end
end

function _viewer()::Union{NapariViewer,Nothing}
    _viewer_ref[]
end

function _viewer_alive()::Bool
    v = _viewer_ref[]
    isnothing(v) && return false
    try; send(v, Dict("type" => "ping")); true; catch; false; end
end

function _ensure_viewer!()::Bool
    lock(_viewer_lock) do
        _viewer_alive() && return true
        _viewer_starting[] && return false
        # Adopt a bridge already listening on the port (e.g. one that survived a server
        # restart) instead of spawning a duplicate — two bridges would fight over port 7655,
        # sending commands to one window while the user looks at the other.
        if _viewer_ref[] === nothing
            probe = NapariViewer()
            try
                send(probe, Dict("type" => "ping"))
                _viewer_ref[] = probe
                @info "Adopted existing Napari bridge on port $(probe.port)"
                return true
            catch
                # none running — fall through to launch a fresh one
            end
        end
        @info "Launching Napari bridge..."
        v = NapariViewer()
        _viewer_ref[] = v
        _viewer_starting[] = true
        gpu = _napari_gpu()
        @async begin
            try
                launch!(v; discrete_gpu = gpu)   # blocks until bridge is up
                _execute_pending_open()
            catch e
                @warn "Napari launch failed" exception = e
            finally
                lock(_viewer_lock) do; _viewer_starting[] = false; end
            end
        end
        false
    end
end

function _execute_pending_open()
    pending = lock(_viewer_lock) do
        p = _pending_open[]
        _pending_open[] = nothing
        p
    end
    isnothing(pending) && return
    v = _viewer()
    isnothing(v) && return
    _with_viewer() do
    try
        # Re-resolve _active at fire time — a task may have completed between the
        # eye-button click and Napari becoming ready.
        meta_file = joinpath(pending.proj_dir, "1", pending.image_uid, "ccid.json")
        raw       = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(meta_file, String)))
        filename  = versioned_get_field(raw, "filepath", nothing)   # nothing → _active
        zarr_path = joinpath(pending.proj_dir, "0", pending.image_uid, string(something(filename, "")))
        ch_raw    = versioned_get_field(raw, "imChannelNames", nothing)
        ch_names  = (ch_raw isa AbstractVector) ? collect(String, ch_raw) : nothing
        task_dir  = joinpath(pending.proj_dir, "1", pending.image_uid)

        p_show_3d       = hasproperty(pending, :show_3d)          ? pending.show_3d          : false
        p_as_dask       = hasproperty(pending, :as_dask)          ? pending.as_dask          : true
        p_show_labels = hasproperty(pending, :show_labels) ? pending.show_labels : false
        p_all_labels  = hasproperty(pending, :all_labels)  ? pending.all_labels  : Dict{String,Vector{String}}()
        _do_open!(v, zarr_path, task_dir, ch_names; show_3d = p_show_3d, as_dask = p_as_dask)
        _current_zarr_path[] = zarr_path
        _current_task_dir[]  = task_dir
        _current_image_uid[] = pending.image_uid

        if p_show_labels && !isempty(p_all_labels)
            _show_all_labels!(v, p_all_labels, true)
        end

        if hasproperty(pending, :auto_load_props) && pending.auto_load_props
            _try_load_layer_props!(v, task_dir, zarr_path)
        end
        _configure_autosave!(v, task_dir, zarr_path,
                             hasproperty(pending, :auto_save_props) ? pending.auto_save_props : false)

        @info "Napari opened image" image_uid=pending.image_uid zarr_path
        broadcast_ws(Dict{String,Any}("type" => "napari:opened", "imageUid" => pending.image_uid))
    catch e
        @warn "Failed to open pending image in Napari" exception = e
    end
    end  # _with_viewer
end

function _do_open!(v::NapariViewer, zarr_path::String, task_dir::String,
                   ch_names::Union{Vector{String},Nothing};
                   show_3d::Bool = false, as_dask::Bool = true)
    send(v, Dict{String,Any}("type" => "set_task_dir", "path" => task_dir))
    cmd = Dict{String,Any}("type"=>"open_image", "path"=>zarr_path,
                           "show_3d"=>show_3d, "as_dask"=>as_dask, "visible"=>true)
    isnothing(ch_names) || (cmd["channel_names"] = ch_names)
    send(v, cmd)
end

# ── Label helpers ────────────────────────────────────────────────────────────

# Parse allLabels dict from a request body: {valueName → [file, ...]}
function _parse_all_labels(data)::Dict{String,Vector{String}}
    raw = get(data, :allLabels, nothing)
    raw isa AbstractDict || return Dict{String,Vector{String}}()
    Dict{String,Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : String[string(v)])
        for (k, v) in raw
    )
end

# Show or hide all label sets in napari. A failure on one set is logged and collected so it
# doesn't prevent the others from loading, but errors are NOT swallowed: any failures are
# re-raised as an aggregate so the caller surfaces them (→ 500 + server log) instead of the
# toggle silently doing nothing. (A genuinely missing zarr is skipped bridge-side without
# raising — see napari_bridge.show_labels — so this only fires on real load errors.)
function _show_all_labels!(v::NapariViewer, all_labels::Dict{String,Vector{String}}, show::Bool)
    errs = String[]
    for (vn, files) in all_labels
        try
            show_labels!(v; value_name = vn, label_files = files, show_labels = show)
        catch e
            @warn "show_labels failed" value_name=vn files=files exception=(e, catch_backtrace())
            push!(errs, "$vn: $(sprint(showerror, e))")
        end
    end
    isempty(errs) || error("show_labels failed for: " * join(errs, "; "))
end

# ── REST: POST /api/napari/open ───────────────────────────────────────────────

function api_napari_open(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid,   ""))
    value_name_raw = get(data, :valueName, nothing)
    value_name     = isnothing(value_name_raw) ? nothing : String(value_name_raw)
    auto_save       = Bool(get(data, :autoSaveProps,   false))
    auto_load       = Bool(get(data, :autoLoadProps,   false))
    show_3d         = Bool(get(data, :show3D,          false))
    as_dask         = Bool(get(data, :asDask,          true))
    show_labels_req = Bool(get(data, :showLabels, false))
    all_labels      = _parse_all_labels(data)

    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error = "imageUid required"))

    proj_dir  = joinpath(projects_dir(), project_uid)
    meta_file = joinpath(proj_dir, "1", image_uid, "ccid.json")
    isdir(proj_dir)   || return 404, JSON3.write((; error = "Project not found: $project_uid"))
    isfile(meta_file) || return 404, JSON3.write((; error = "Image metadata not found: $image_uid"))

    raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(meta_file, String)))

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        vn_label = isnothing(value_name) ? "active" : value_name
        return 404, JSON3.write((; error = "No filepath registered (valueName=$vn_label). Run a conversion task first."))
    end

    zarr_path = joinpath(proj_dir, "0", image_uid, string(filename))
    isdir(zarr_path) || return 404, JSON3.write((; error = "Zarr not found on disk: $zarr_path"))

    ch_raw = versioned_get_field(raw, "imChannelNames", value_name)
    # Fall back to default channel names when the correction has no dedicated entry
    if isnothing(ch_raw) && !isnothing(value_name)
        ch_raw = versioned_get_field(raw, "imChannelNames", nothing)
    end
    ch_names = (ch_raw isa AbstractVector) ? collect(String, ch_raw) : nothing
    task_dir = joinpath(proj_dir, "1", image_uid)

    alive = _ensure_viewer!()
    if !alive
        lock(_viewer_lock) do
            _pending_open[] = (; proj_dir, image_uid,
                                 auto_load_props = auto_load,
                                 auto_save_props = auto_save,
                                 show_3d, as_dask,
                                 show_labels     = show_labels_req,
                                 all_labels)
        end
        return 202, JSON3.write((; starting = true,
            message = "Napari is starting — the image will open automatically."))
    end

    v = _viewer()
    isnothing(v) && return 500, JSON3.write((; error = "Viewer not initialised"))
    _with_viewer() do
    try
        # Auto-save layer props for the currently open image before switching
        if auto_save && !isnothing(_current_zarr_path[]) && !isnothing(_current_task_dir[])
            _try_save_layer_props!(v, _current_task_dir[], _current_zarr_path[])
        end

        _do_open!(v, zarr_path, task_dir, ch_names; show_3d, as_dask)
        _current_zarr_path[] = zarr_path
        _current_task_dir[]  = task_dir
        _current_image_uid[] = image_uid

        if show_labels_req && !isempty(all_labels)
            _show_all_labels!(v, all_labels, true)
        end

        # Auto-load layer props for the newly opened image
        if auto_load
            _try_load_layer_props!(v, task_dir, zarr_path)
        end
        # Wire live autosave for this image (after the load, so the load isn't echoed back).
        _configure_autosave!(v, task_dir, zarr_path, auto_save)

        @info "Opened image in Napari" image_uid zarr_path
        broadcast_ws(Dict{String,Any}("type" => "napari:opened", "imageUid" => image_uid))
        200, JSON3.write((; ok = true, imageUid = image_uid))
    catch e
        @warn "Failed to open image in Napari" image_uid exception = e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
    end  # _with_viewer
end

# ── REST: POST /api/napari/close ──────────────────────────────────────────────

function api_napari_close(body_bytes::Vector{UInt8})
    v = _viewer()
    isnothing(v) && return 200, JSON3.write((; ok = true, message = "Napari was not running"))
    _with_viewer() do
        try
            close!(v)
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# POST /api/napari/configure-autosave  { enabled }  → toggle live layer-props autosave for the image
# currently open in napari, without re-opening it. Lets the viewer-panel toggle take effect immediately.
function api_napari_configure_autosave(body_bytes::Vector{UInt8})
    data    = JSON3.read(String(body_bytes))
    enabled = Bool(get(data, :enabled, false))
    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 200, JSON3.write((; ok = false, message = "Napari not running"))
    (isnothing(_current_zarr_path[]) || isnothing(_current_task_dir[])) &&
        return 200, JSON3.write((; ok = false, message = "No image open"))
    _with_viewer() do
        _configure_autosave!(v, _current_task_dir[], _current_zarr_path[], enabled)
        200, JSON3.write((; ok = true))
    end
end

# ── REST: POST /api/napari/screenshot ─────────────────────────────────────────
# Capture the current napari CANVAS and return JSON `{ png(base64), viewState, imageUid }`. The view
# snapshot (camera + dims + per-layer display props) is captured ATOMICALLY with the shot (folded into
# the bridge's save_screenshot reply) so the strip frame carries its exact provenance for zoom-to-source
# (docs/todo/ANIMATION_PLAN.md). Base64 (not octet-stream) so one response carries image + snapshot; the
# frontend already turned the PNG into a data URL anyway. `send` is request-reply, so the file is written
# by the time `save_screenshot!` returns — read then delete.
function api_napari_screenshot(body_bytes::Vector{UInt8})
    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))
    data = try JSON3.read(String(body_bytes)) catch; nothing end
    project_uid = data === nothing ? "" : String(get(data, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    path = tempname() * ".png"
    _with_viewer() do
    try
        # fit_data → tight-fit to the data extent at native resolution: no black margins, and the figure
        # matches the viewer (image fills the frame) instead of a tiny image in a big black canvas.
        reply    = save_screenshot!(v, path; fit_data = true)
        # store the PNG as a SIDECAR file (settings/board-assets/<id>.png), not base64 in the board JSON,
        # so analysisBoards.json stays small (autosave-friendly). Return only the id + snapshot.
        asset_id = _save_board_asset_file(project_uid, path)
        return 200, JSON3.write((;
            assetId   = asset_id,
            viewState = get(reply, "view_state", Dict{String,Any}()),
            imageUid  = _current_image_uid[],
        ))
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    finally
        isfile(path) && rm(path; force = true)
    end
    end  # _with_viewer
end

# ── REST: POST /api/napari/apply-view-state ───────────────────────────────────
# Re-apply a saved view snapshot to the running viewer (the zoom-to-source restore). Body:
# `{ viewState }`. The image must already be open (the caller opens it first, then applies); the bridge
# skips missing layers / unsettable attrs, so a snapshot degrades gracefully.
function api_napari_apply_view_state(body_bytes::Vector{UInt8})
    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))
    data = JSON3.read(String(body_bytes))
    snap = get(data, :viewState, nothing)
    isnothing(snap) && return 400, JSON3.write((; error = "viewState required"))
    _with_viewer() do
        try
            apply_view_state!(v, snap)
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/view-state ─────────────────────────────────────────
# Return the CURRENT view snapshot (camera/dims/per-layer colormap+visibility) of the open image, plus
# which image it is. Lightweight (no screenshot / PNG side-effect — bridge `capture_view_state`); the
# Batch movies page uses it to seed the config from the first selected image's live colours + overlays.
function api_napari_view_state(body_bytes::Vector{UInt8})
    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))
    _with_viewer() do
        try
            resp = send(v, Dict{String,Any}("type" => "capture_view_state"))
            200, JSON3.write((; ok = true,
                viewState = get(resp, "view_state", Dict{String,Any}()),
                imageUid  = something(_current_image_uid[], "")))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/record-timelapse ───────────────────────────────────
# Record the open image's timelapse (T-sweep) to an mp4 under the project's `movies/` dir. F1.1 of the
# batch-movie work (docs/todo/ANIMATION_PLAN.md): records the CURRENT view (whatever channels/pops/
# colour-by are shown); F1.2/F1.3 add an authored config + batch over images. Returns the saved path.
function api_napari_record_timelapse(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    fps         = Int(get(data, :fps, 15))
    scale       = get(data, :scale, 1)
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err

    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))

    # save under {project}/movies/{imageName}.mp4 (img._dir = {proj}/1/{uid}). Named by the IMAGE, not a
    # segmentation — the recording captures the whole current view (which can show several segmentations'
    # tracks/labels at once), so tagging it with one value_name would be misleading. Fall back to the uid
    # if the name is blank / unsafe. (F1.3 will switch to attr-based names for batch runs.)
    movies_dir = joinpath(dirname(dirname(img._dir)), "movies")
    mkpath(movies_dir)
    safe = replace(strip(img.name), r"[^A-Za-z0-9._-]+" => "_")
    path = joinpath(movies_dir, (isempty(safe) ? image_uid : safe) * ".mp4")

    _with_viewer() do
        try
            resp = record_timelapse!(v, path; fps = fps, scale = scale)
            200, JSON3.write((; ok = true, path = path,
                frames = get(resp, "frames", 0), nTimepoints = get(resp, "n_timepoints", 0)))
        catch e
            @warn "record_timelapse failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/record-animation ───────────────────────────────────
# Render an interpolated keyframe animation (the timeline's ordered view snapshots) to an mp4 under the
# project's movies/ dir. `keyframes` = [{viewState, steps}] in play order; each tweened `steps` frames
# from the previous (camera/contrast/colour/T interpolation). F2 of the batch-movie work.
function api_napari_record_animation(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    fps         = Int(get(data, :fps, 15))
    keyframes   = get(data, :keyframes, nothing)
    (keyframes === nothing || length(keyframes) < 2) &&
        return 400, JSON3.write((; error = "need at least 2 keyframes"))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err

    v = _viewer()
    (isnothing(v) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))

    movies_dir = joinpath(dirname(dirname(img._dir)), "movies")
    mkpath(movies_dir)
    safe = replace(strip(img.name), r"[^A-Za-z0-9._-]+" => "_")
    path = joinpath(movies_dir, (isempty(safe) ? image_uid : safe) * "_animation.mp4")

    _with_viewer() do
        try
            resp = record_keyframes!(v, path, keyframes; fps = fps)
            200, JSON3.write((; ok = true, path = path,
                frames = get(resp, "frames", 0), keyframes = get(resp, "keyframes", 0)))
        catch e
            @warn "record_keyframes failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── Batch movies (F1.2 config-apply + F1.3 batch) ─────────────────────────────
# The "make a movie for all images" workflow (ports the R `generateMovies`, docs/todo/ANIMATION_PLAN.md
# → F1). Instead of re-implementing open/show-tracks/show-populations/colour-labels, we build the SAME
# request bodies and call the existing handlers — one code path per overlay, no divergent variant. The
# batch drives the ONE shared viewer sequentially under `_viewer_lock` (napari can't render offscreen —
# GL frames come out black — so it must use the live window; the frontend warns the user it's busy).

# Call a same-module API handler with a Julia payload (NamedTuple/Dict) as its JSON body. Returns
# (ok, parsed-response). Lets the batch reuse the exact endpoint logic (pop resolution, colour
# overrides, legend) with zero re-implementation.
function _call_napari_api(f::Function, payload)::Tuple{Bool,Any}
    status, body = f(Vector{UInt8}(JSON3.write(payload)))
    parsed = try; JSON3.read(body); catch; nothing; end
    (status == 200, parsed)
end

# Attr-named output filename: <attr1>_<attr2>_..._<uid>.mp4 (mirrors the R `paste(fileAttrs...) _ uid`).
# Blank/missing attrs are dropped; the uid always terminates the name so batch outputs never collide.
# Falls back to just the uid when no fileAttrs are given. Pure (attr dict + uid) → testable.
function _movie_basename(attr::AbstractDict, uid::AbstractString, file_attrs::Vector{String})::String
    parts = String[]
    for a in file_attrs
        val = strip(String(get(attr, a, "")))
        isempty(val) || push!(parts, val)
    end
    push!(parts, String(uid))
    replace(join(parts, "_"), r"[^A-Za-z0-9._-]+" => "_") * ".mp4"
end

# Full attr-named output path under {proj}/movies/ (img._dir = {proj}/1/{uid}).
function _movie_out_path(img, file_attrs::Vector{String})::String
    movies_dir = joinpath(dirname(dirname(img._dir)), "movies")
    mkpath(movies_dir)
    joinpath(movies_dir, _movie_basename(img.attr, img.uid, file_attrs))
end

# Apply an authored movie config to ONE image already resolvable by uid (F1.2). Opens the image (contrast
# from its saved layer props), sets each channel's colormap + visibility (only `channels` are shown, the
# rest hidden), then overlays tracks / populations / colour-by exactly as the ViewerPanel does — by
# calling the existing handlers. Caller holds `_with_viewer` so the whole sequence is atomic on the bridge.
function _apply_movie_config!(project_uid::String, image_uid::String, img, config; do_open::Bool = true)::Nothing
    vn_raw = strip(String(get(config, :valueName, "")))
    vn     = isempty(vn_raw) ? nothing : vn_raw

    # 1. open (auto-load saved props → per-image contrast, Decision 4; no auto-save — we're driving it).
    #    SKIP the open when this exact image (active version) is already shown: re-opening re-samples the
    #    channel contrast (add_image contrast=True), which would wipe the contrast the user set live if it
    #    was never saved to props. Preview passes do_open=false (it applies to the open image only), and a
    #    batch skips re-opening its first image when that's the one already open. Both preserve live contrast.
    already_open = (_current_image_uid[] == image_uid) && isempty(vn_raw)
    if do_open && !already_open
        ok, _ = _call_napari_api(api_napari_open, (; projectUid = project_uid, imageUid = image_uid,
            valueName = vn, autoLoadProps = true, autoSaveProps = false))
        ok || error("could not open image in napari")
    end

    # 2. channel colormaps + visibility. `channels` = {name → colormap} for the channels to SHOW; every
    #    other channel is hidden. Applied via a partial view-state (colormap/visible are whitelisted).
    chans = get(config, :channels, nothing)
    if chans isa AbstractDict && !isempty(chans)
        wanted  = Dict(String(k) => String(v) for (k, v) in pairs(chans))
        ch_all  = channel_names(img; value_name = vn)
        layers  = Dict{String,Any}()
        for ch in (ch_all === nothing ? collect(keys(wanted)) : ch_all)
            layers[ch] = haskey(wanted, ch) ?
                Dict{String,Any}("colormap" => wanted[ch], "visible" => true) :
                Dict{String,Any}("visible" => false)
        end
        _call_napari_api(api_napari_apply_view_state, (; viewState = Dict{String,Any}("layers" => layers)))
    end

    color_by  = String(get(config, :colourBy, ""))
    overrides = get(config, :colourOverrides, Dict{String,Any}())

    # 3. tracks (coloured by the measure; user pops supply their colour where they cover a value)
    if Bool(get(config, :showTracks, false))
        _call_napari_api(api_napari_show_tracks, (; projectUid = project_uid, imageUid = image_uid,
            valueNames      = collect(String, get(config, :trackValueNames, String[])),
            colorBy         = color_by,
            tailWidth       = get(config, :tailWidth, 4),
            showGatedTracks = Bool(get(config, :showGatedTracks, false)),
            showTrackclust  = Bool(get(config, :showTrackclust, false)),
            colourOverrides = overrides))
    end

    # 4. populations as points
    if Bool(get(config, :showPopulations, false))
        _call_napari_api(api_napari_show_populations, (; projectUid = project_uid, imageUid = image_uid,
            popType = String(get(config, :popType, "flow")),
            pointsSize = get(config, :pointsSize, 6), show = true))
    end

    # 5. colour the Labels layer by the measure (optional; tracks/points already coloured above)
    if Bool(get(config, :colourLabels, false)) && !isempty(color_by)
        _call_napari_api(api_napari_colour_labels, (; projectUid = project_uid, imageUid = image_uid,
            valueName = something(vn, ""), column = color_by, colourOverrides = overrides))
    end
    nothing
end

# ── REST: POST /api/napari/apply-movie-config ─────────────────────────────────
# F1.2 preview: apply an authored movie config to the CURRENTLY open image (no recording). Lets the user
# eyeball the look the batch will record before kicking off the whole run.
function api_napari_apply_movie_config(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    config      = get(data, :config, nothing)
    config === nothing && return 400, JSON3.write((; error = "config required"))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    (isnothing(_viewer()) || !_viewer_alive()) && return 400, JSON3.write((; error = "Napari not running"))
    _with_viewer() do
        try
            # preview applies to the CURRENTLY open image — never re-open (that would re-sample contrast)
            _apply_movie_config!(project_uid, image_uid, img, config; do_open = false)
            200, JSON3.write((; ok = true))
        catch e
            @warn "apply_movie_config failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── Batch cancel registry ─────────────────────────────────────────────────────
# The batch is NOT a scheduler task (napari is a single UI-serial viewer in api/, not a pooled headless
# job), so `cancel_task!` doesn't reach it. This lightweight flag, keyed by the client's taskId, is set
# by the `task:cancel` WS handler and checked between images. A cancel can't interrupt an in-progress
# record (the bridge blocks inside `animate`) — it takes effect after the current image finishes.
const _batch_cancel      = Dict{String,Bool}()
const _batch_cancel_lock = ReentrantLock()
_batch_register!(id)      = lock(() -> (_batch_cancel[id] = false),                       _batch_cancel_lock)
_batch_cancelled(id)      = lock(() -> get(_batch_cancel, id, false),                     _batch_cancel_lock)
_batch_clear!(id)         = lock(() -> delete!(_batch_cancel, id),                        _batch_cancel_lock)
request_batch_cancel!(id) = lock(() -> (haskey(_batch_cancel, id) && (_batch_cancel[id] = true); nothing), _batch_cancel_lock)

# F1.3 batch runner — invoked async from the WS layer (`movie:batch`). For each image: apply the config,
# record the T-sweep to an attr-named mp4, emit task:progress/log so it drives the existing task UI. `rep`
# = representative uid for status/result. Errors on one image are logged and the batch continues.
function run_batch_movies(task_id::String, project_uid::String, image_uids::Vector{String},
                          config, file_attrs::Vector{String}, fps::Int, scale)
    n   = length(image_uids)
    rep = isempty(image_uids) ? "" : first(image_uids)
    done = 0; errors = String[]
    # fail fast (one clear message, not N per-image errors) if the viewer isn't up — the batch drives
    # the live window, so napari must already be running.
    if isnothing(_viewer()) || !_viewer_alive()
        ws_log(nothing, task_id, "[ERROR] Napari is not running — open an image first, then generate")
        ws_status(nothing, task_id, "failed", rep)
        _batch_clear!(task_id)
        return nothing
    end
    ws_status(nothing, task_id, "running", rep)
    ws_progress(nothing, task_id, 0, n)
    t_start = Int(get(config, :tStart, 0))
    t_end_v = get(config, :tEnd, nothing)
    t_end   = t_end_v === nothing ? nothing : Int(t_end_v)
    for (i, uid) in enumerate(image_uids)
        if _batch_cancelled(task_id)
            ws_log(nothing, task_id, "[CANCELLED] stopped after $done/$n image(s)")
            break
        end
        img, err = _gating_image(project_uid, uid)
        if err !== nothing
            push!(errors, uid)
            ws_log(nothing, task_id, "[WARN] skip $uid — not a loadable image")
            ws_progress(nothing, task_id, i, n); continue
        end
        try
            path = _movie_out_path(img, file_attrs)
            ws_log(nothing, task_id, "[$i/$n] $(img.name) → $(basename(path))")
            _with_viewer() do
                _apply_movie_config!(project_uid, uid, img, config)
                v = _viewer()
                v === nothing && error("Napari not running")
                record_timelapse!(v, path; fps = fps, scale = scale, t_start = t_start, t_end = t_end)
            end
            done += 1
            ws_log(nothing, task_id, "[$i/$n] done → $(basename(path))")
        catch e
            push!(errors, uid)
            ws_log(nothing, task_id, "[ERROR] $uid: $(sprint(showerror, e))")
        end
        ws_progress(nothing, task_id, i, n)
    end
    cancelled = _batch_cancelled(task_id)
    status    = cancelled ? "cancelled" : (isempty(errors) ? "done" : "failed")
    ws_result(nothing, task_id, rep,
        Dict{String,Any}("done" => done, "total" => n, "errors" => errors, "cancelled" => cancelled))
    ws_status(nothing, task_id, status, rep; image_uids = image_uids)
    _batch_clear!(task_id)
    nothing
end

# ── REST: POST /api/napari/restart ────────────────────────────────────────────

function api_napari_restart(body_bytes::Vector{UInt8})
    v = _viewer()
    isnothing(v) && return api_napari_open(body_bytes)
    _with_viewer() do
        try
            restart!(v; discrete_gpu = _napari_gpu())
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/show-labels ───────────────────────────────────────

function api_napari_show_labels(body_bytes::Vector{UInt8})
    data       = JSON3.read(String(body_bytes))
    show       = Bool(get(data, :showLabels, true))
    all_labels = _parse_all_labels(data)

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))

    _with_viewer() do
        try
            _show_all_labels!(v, all_labels, show)
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/show-populations ───────────────────────────────────
# Consumer direction: colour each population's cells as a points layer in napari (ports the
# old napari_utils.show_pop_mapping). Julia owns membership; the bridge reads centroids from
# the H5AD locally and only receives label IDs + display attrs. The transient napari-selection
# pop is deliberately EXCLUDED: it is the *source* of a selection, and re-rendering it as a
# points layer (on every popmap broadcast) added a new layer that stole napari's active layer,
# so the user couldn't keep editing the selection shape. It still shows on the flow plots.
function api_napari_show_populations(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    pop_type    = String(get(data, :popType, "flow"))
    points_size = get(data, :pointsSize, 6)
    show        = Bool(get(data, :show, true))     # false → clear all pop layers in napari

    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err

    # Scope: an explicit `valueNames` list (or a single non-blank `valueName`) → refresh ONLY those
    # segmentations; blank → ALL real segmentations. Live gate edits pass the edited segmentation so we
    # don't recompute every segmentation's membership on each edit (open / the master toggle pass blank
    # → full refresh). `scoped` tells the bridge to prune stale layers only within `segs`, leaving the
    # other segmentations' layers intact — so a scoped push is as cheap as the pre-multi-seg behaviour.
    all_segs = String[v for v in versioned_keys(img.label_props) if !is_reserved_value_name(v)]
    raw_vns  = get(data, :valueNames, nothing)
    one_vn   = String(get(data, :valueName, ""))
    want = raw_vns !== nothing ? String[v for v in String.(raw_vns) if haskey(img.label_props, v)] :
           (!isempty(one_vn) && haskey(img.label_props, one_vn)) ? String[one_vn] : String[]
    segs   = isempty(want) ? all_segs : want
    scoped = !isempty(want)                    # a real subset was requested → bridge prunes within `segs`

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))

    pops = Vector{Dict{String,Any}}()
    # Show the gated populations of the in-scope segmentation(s) — each pop its own layer, tagged and
    # named by its value_name (e.g. "(flow) (T) /qc" AND "(flow) (B) /qc"). Multi-segmentation like
    # show-tracks, so the overlay is independent of which single segmentation is "active": opening the
    # image (blank scope) shows every segmentation's pops, not just the active one. `resolve_pops` is
    # CACHED per (segmentation, mtimes), so an unchanged segmentation on a full push returns instantly.
    #
    # an image not segmented yet has no labelProps → no populations; fall through with empty `pops`.
    if show && _has_label_props(img)
        for wn in segs
            try
                for L in resolve_pops(img, pop_type; value_name = wn)
                    push!(pops, Dict{String,Any}(
                        "value_name" => wn, "path" => L.path, "name" => L.name, "colour" => L.colour,
                        "show" => L.show, "is_track" => L.is_track, "label_ids" => L.labels))
                end
            catch e
                @warn "populations unavailable" value_name = wn exception = e
            end
        end
    end   # show=false → empty pops → bridge removes the in-scope pop layers
    _with_viewer() do
        try
            send(v, Dict{String,Any}("type" => "show_populations", "pop_type" => pop_type,
                "value_name" => (isempty(segs) ? "" : first(segs)), "value_names" => segs,
                "scoped" => scoped, "points_size" => points_size, "pops" => pops))
            200, JSON3.write((; ok = true, n = length(pops)))
        catch e
            @warn "show_populations failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/show-tracks ────────────────────────────────────────
# Consumer direction: show each track population as a napari Tracks layer (ports the old
# napari_utils.show_tracks). Julia owns membership — for a `track` map `cells_in_pop` returns the
# pop's `track_id`s — and the bridge reads the per-cell centroids + t + track_id locally, bin-masks
# to those track_ids, and calls viewer.add_tracks. Mirrors show-populations (per-pop layers).
# Show the TRACKS of one or more SEGMENTATIONS (value names), each as its own napari Tracks layer
# named by its value_name. A segmentation's tracks = its `_tracked` cells (track_id > 0), read
# directly from the cell h5ad — no gating map needed. `valueNames` lists which segmentations to show
# (the per-segmentation "directions" toggles in the ViewerPanel); empty → clear all track layers.
# `colorBy` shades vertices by an obs column (per-segmentation, applied in the bridge).
function api_napari_show_tracks(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    tail_width  = get(data, :tailWidth, 4)
    color_by    = String(get(data, :colorBy, ""))  # obs column to shade vertices by ("" → track_id)

    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err

    # which segmentations' whole-track overlay (_tracked) to show — the per-segmentation "directions"
    # toggles. Resolve each against the image's keys.
    want_raw = get(data, :valueNames, nothing)
    want = want_raw === nothing ? String[] :
           String[v for v in String.(want_raw) if haskey(img.label_props, v)]
    # global toggles: overlay the gated TRACK pops (track-measure gates) and/or the TRACKCLUST pops
    # (cluster-membership pops on the per-track table) across all segmentations, each as ribbons.
    show_gated      = Bool(get(data, :showGatedTracks, false))
    show_trackclust = Bool(get(data, :showTrackclust, false))

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))

    pops = Vector{Dict{String,Any}}()
    # 1. whole-segmentation tracks (_tracked = all track_id>0), per per-segmentation toggle. Read
    #    directly from the cell table — no gating map needed.
    for vn in want
        is_reserved_value_name(vn) && continue          # skip __tracks companion tables
        tdf = try
            _fetch(img, vn)(["track_id"])
        catch
            continue                                     # no track_id column → not tracked
        end
        ("track_id" in names(tdf)) || continue
        tids = unique(Int[Int(t) for t in tdf.track_id if t isa Real && isfinite(t) && t > 0])
        isempty(tids) && continue
        push!(pops, Dict{String,Any}(
            "value_name" => vn, "path" => "/_tracked", "name" => "_tracked", "pop_type" => "track",
            "colour" => "#9ca3af", "show" => true, "track_ids" => tids))
    end
    # 2. gated TRACK populations (track-measure gates in `{vn}__tracks.json`, e.g. TEST/SDGF) under
    #    the global toggle — across ALL segmentations. `cells_in_pop` on a track map → the pop's
    #    `track_id`s. Shown alongside the per-segmentation `_tracked` layers.
    if show_gated
        for vn in versioned_keys(img.label_props)
            is_reserved_value_name(vn) && continue
            try
                tm = _live_map(img, vn, "track")
                for path in pop_paths(tm)
                    p = pop_at(tm, path)
                    p.transient && continue
                    gtids = unique(Int[Int(t) for t in cells_in_pop(tm, path)])
                    isempty(gtids) && continue
                    push!(pops, Dict{String,Any}(
                        "value_name" => vn, "path" => p.path, "name" => p.name, "pop_type" => "track",
                        "colour" => p.colour, "show" => p.show, "track_ids" => gtids))
                end
            catch e
                @warn "track gates unavailable" value_name = vn exception = e
            end
        end
    end
    # 3. TRACKCLUST populations (cluster pops on the per-track table, `{vn}__trackclust.json`) under
    #    their global toggle — one ribbon layer per pop, namespaced by pop_type so they coexist with
    #    the gated `track` ribbons. `cells_in_pop` on a trackclust map → the pop's track_ids.
    if show_trackclust
        for vn in versioned_keys(img.label_props)
            is_reserved_value_name(vn) && continue
            try
                cm = _live_map(img, vn, "trackclust")
                for path in pop_paths(cm)
                    p = pop_at(cm, path)
                    p.transient && continue
                    ctids = unique(Int[Int(t) for t in cells_in_pop(cm, path)])
                    isempty(ctids) && continue
                    push!(pops, Dict{String,Any}(
                        "value_name" => vn, "path" => p.path, "name" => p.name, "pop_type" => "trackclust",
                        "colour" => p.colour, "show" => p.show, "track_ids" => ctids))
                end
            catch e
                @warn "trackclust pops unavailable" value_name = vn exception = e
            end
        end
    end   # empty want + no gated + no trackclust → empty pops → bridge removes existing track layers
    # colour-by overrides: where a user pop FILTERS on the `color_by` column, use its colour (the
    # canonical "use the population's colour" rule); the bridge fills the rest with defaults. Scan ALL
    # pop types — a track can be coloured by a cell-level column (flow/clust pop) or a track-level one
    # (track/trackclust pop); `pop_colour_overrides` only matches pops that filter this exact column.
    overrides = _merge_user_overrides!(
        _colour_overrides_for(img, color_by, ("trackclust", "track", "clust", "flow")), data)
    _with_viewer() do
        try
            resp = send(v, Dict{String,Any}("type" => "show_tracks",
                "tail_width" => tail_width, "color_by" => color_by, "pops" => pops,
                "colour_overrides" => overrides))
            200, JSON3.write((; ok = true, n = length(pops),
                legend = get(resp, "legend", Dict{String,Any}()),
                legendLabels = _pop_labels_for(img, color_by, ("trackclust", "track", "clust", "flow"))))
        catch e
            @warn "show_tracks failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# Gather a {value(str) => X} map for colouring by `column`, pooling every user population (across
# segmentations, in the given pop_types) that FILTERS on `column` via `getter` (a `PopulationMap →
# Dict` — `pop_colour_overrides` for hex, `pop_label_overrides` for the pop name). First pop (by
# segmentation/type order) wins a shared value. Empty when `column` is blank / a special key.
function _gather_pop_overrides(img, column::AbstractString, pop_types, getter)::Dict{String,String}
    out = Dict{String,String}()
    (isempty(column) || column == "track_id") && return out
    for vn in versioned_keys(img.label_props)
        is_reserved_value_name(vn) && continue
        for pt in pop_types
            try
                for (k, val) in getter(_live_map(img, vn, pt), column)
                    get!(out, k, val)
                end
            catch
                # pop map for this (vn, pop_type) unavailable → nothing to contribute
            end
        end
    end
    out
end
# {value(str) => hex} — the population colour a value takes on `column` (bridge fills the rest).
_colour_overrides_for(img, column::AbstractString, pop_types) =
    _gather_pop_overrides(img, column, pop_types, pop_colour_overrides)
# {value(str) => population name} — so the colour-by legend reads the pop name where one defines a value.
_pop_labels_for(img, column::AbstractString, pop_types) =
    _gather_pop_overrides(img, column, pop_types, pop_label_overrides)

# Merge the client's user colour overrides ({value(str) => hex}, from recolouring a legend swatch) on
# TOP of the pop-derived overrides — the user's explicit choice wins (categories with no population have
# no colour defined anywhere, so this is the only source; for pop-backed values it's a display override).
function _merge_user_overrides!(overrides::Dict{String,String}, data)::Dict{String,String}
    user = get(data, :colourOverrides, nothing)
    user === nothing && return overrides
    for (k, v) in pairs(user)
        (v === nothing || isempty(String(v))) && continue
        overrides[String(k)] = String(v)
    end
    overrides
end

# ── REST: POST /api/napari/colour-labels ──────────────────────────────────────
# Recolour the open image's Labels layer by an obs column (continuous → viridis, categorical →
# palette per level), via a DirectLabelColormap in the bridge. `column=""` resets to the default
# colormap. Ports the old `napari_utils.show_channel_intensity`. Bridge reads the column locally.
function api_napari_colour_labels(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    column      = String(get(data, :column, ""))   # "" → reset to default labels colormap

    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    vn = _resolve_vn(img, String(get(data, :valueName, "")))

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))
    # colour overrides: any user pop that FILTERS on `column` supplies its colour. Scan ALL pop types —
    # labels can be coloured by a cell-level column (flow/clust) OR a track-level one (track/trackclust,
    # e.g. clusters.* broadcast to cells); `pop_colour_overrides` only matches pops filtering this column.
    overrides = _merge_user_overrides!(
        _colour_overrides_for(img, column, ("clust", "flow", "trackclust", "track")), data)
    _with_viewer() do
        try
            resp = send(v, Dict{String,Any}("type" => "colour_labels", "value_name" => vn,
                "column" => column, "colour_overrides" => overrides))
            200, JSON3.write((; ok = true,
                legend = get(resp, "legend", Dict{String,Any}()),
                legendLabels = _pop_labels_for(img, column, ("clust", "flow", "trackclust", "track"))))
        catch e
            @warn "colour_labels failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/start-selection ────────────────────────────────────
# Producer direction: tell the bridge to add a "Cell selection" Shapes layer. When the user
# draws on it, the bridge resolves which cell centroids fall inside and POSTs them back to
# /api/napari/event. `apiUrl` is where the bridge reaches this server (default localhost:8080).
function api_napari_start_selection(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    vn = _resolve_vn(img, String(get(data, :valueName, "")))
    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))
    api_url = String(get(data, :apiUrl, "http://localhost:8080"))
    # z scope: "slice" restricts the selection to ±zWindow slices around the live z; "stack"
    # (default) ignores z and selects across the whole stack (docs/NAPARI.md).
    z_mode   = String(get(data, :zMode, "stack"))
    z_window = _to_int(get(data, :zWindow, 0))
    _with_viewer() do
        try
            send(v, Dict{String,Any}("type" => "start_cell_selection",
                "project_uid" => project_uid, "image_uid" => image_uid,
                "value_name" => vn, "api_url" => api_url,
                "z_mode" => z_mode, "z_window" => z_window))
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/selection-scope ────────────────────────────────────
# Change the z scope of the ACTIVE cell selection and re-evaluate the drawn polygon immediately
# (the bridge re-runs point-in-polygon + z filter and POSTs the new label set back). Lets the
# gating-bar Z toggle / ± window update the highlighted cells live, without redrawing. No-op in
# the bridge when no selection is active.
function api_napari_selection_scope(body_bytes::Vector{UInt8})
    data     = JSON3.read(String(body_bytes))
    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))
    z_mode   = String(get(data, :zMode, "stack"))
    z_window = _to_int(get(data, :zWindow, 0))
    _with_viewer() do
        try
            send(v, Dict{String,Any}("type" => "update_selection_scope",
                "z_mode" => z_mode, "z_window" => z_window))
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/stop-selection ─────────────────────────────────────
# Clear the cell-selection entirely: drop the transient "Napari selection" pop (empty registry +
# re-broadcast so it leaves the manager/plots) AND remove the "Cell selection" Shapes layer from
# napari. Used by the manager's trash button — deleting the selection should also take its draw
# layer with it. Works whether or not napari is alive (layer removal is best-effort).
function api_napari_stop_selection(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    pop_type    = String(get(data, :popType, "flow"))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    vn = _resolve_vn(img, String(get(data, :valueName, "")))

    _set_napari_selection!(img._dir, vn, Int[])               # clear the registry
    m = load_pop_map(img; value_name = vn, pop_type = pop_type)
    _inject_napari_pop!(m, img)                               # no-op now (selection gone)
    _broadcast_popmap(project_uid, image_uid, vn, pop_type, m)

    v = _viewer()
    if v !== nothing
        # "Cell selection" mirrors SELECTION_LAYER in napari_bridge.py
        _with_viewer() do
            try; send(v, Dict{String,Any}("type" => "remove_layer", "name" => "Cell selection")); catch; end
        end
    end
    200, JSON3.write((; ok = true))
end

# ── REST: POST /api/napari/event ──────────────────────────────────────────────
# Ingest a napari interaction. Currently `cellSelection`: store the label IDs as the transient
# selection (keyed by task_dir+value_name) and broadcast the updated tree so the flow plots
# light up those cells (linked brushing — docs/POPULATION.md). An empty list clears it.
function api_napari_event(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    evt         = String(get(data, :type, "cellSelection"))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    pop_type    = String(get(data, :popType, "flow"))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    vn = _resolve_vn(img, String(get(data, :valueName, "")))

    if evt == "cellSelection"
        raw    = get(data, :labels, Int[])
        labels = raw isa AbstractVector ? Int[Int(x) for x in raw] : Int[]
        _set_napari_selection!(img._dir, vn, labels)
        m = load_pop_map(img; value_name = vn, pop_type = pop_type)
        _inject_napari_pop!(m, img)
        _broadcast_popmap(project_uid, image_uid, vn, pop_type, m)
        return 200, JSON3.write((; ok = true, n = length(labels)))
    end
    400, JSON3.write((; error = "Unknown napari event: $evt"))
end

# ── REST: GET /api/napari/status ──────────────────────────────────────────────

function api_napari_status(req::HTTP.Request)
    200, JSON3.write((; alive = _viewer_alive(), starting = _viewer_starting[]))
end

# ── REST: discrete-GPU toggle ─────────────────────────────────────────────────
# GET  /api/napari/gpu             → { discreteGpu, supported }
# POST /api/napari/gpu { enabled } → set the runtime flag; effective at the NEXT bridge launch, so
#   the caller restarts napari (needsRestart) to apply it now. `supported` is false off Linux, where
#   the flag is a no-op (GPU choice is an OS/driver setting there).
function api_napari_gpu_get(req::HTTP.Request)
    200, JSON3.write((; discreteGpu = _napari_gpu(), supported = Sys.islinux()))
end

function api_napari_gpu_set(body_bytes::Vector{UInt8})
    data = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch
        return 400, JSON3.write((; error = "invalid JSON body")); end
    _napari_discrete_gpu[] = Bool(get(data, "enabled", false))
    200, JSON3.write((; discreteGpu = _napari_discrete_gpu[], supported = Sys.islinux(),
                        needsRestart = _viewer_alive()))
end
