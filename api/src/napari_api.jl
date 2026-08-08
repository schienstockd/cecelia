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

"""
    current_napari_image() -> (; imageUid, zarrPath, taskDir)

What the viewer has open, for out-of-band callers (the task preview). These refs were tracked but
never exposed, so anything acting on "the image on screen" had to be TOLD which one that was — and a
caller that guesses wrong acts on an image the user isn't looking at. Read-only; `nothing` in each
field until an image is opened.
"""
current_napari_image() = (; imageUid = _current_image_uid[],
                            zarrPath = _current_zarr_path[],
                            taskDir  = _current_task_dir[])

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

# Layer props are stored as JSON (the single canonical format — see docs/todo/CROP_PANEL_PLAN.md Phase 0).
# The bridge reads/writes this; the in-app crop render (Julia) reads it too. A legacy `.pkl` from before
# the switch is migrated to `.json` by the bridge on first load (see `_legacy_props_path`).
function _props_path(task_dir::String, zarr_path::String)::String
    joinpath(task_dir, "data", basename(zarr_path) * ".json")
end
# Pre-JSON pickle path — kept only so an existing file still triggers a load (the bridge migrates it).
_legacy_props_path(task_dir::String, zarr_path::String)::String =
    joinpath(task_dir, "data", basename(zarr_path) * ".pkl")

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
        # Load if the JSON exists OR a legacy .pkl does (the bridge migrates the .pkl → .json on load).
        (isfile(props_file) || isfile(_legacy_props_path(task_dir, zarr_path))) || return
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
            adopted = try
                reply = send(probe, Dict("type" => "ping"))
                # Adopt only a bridge whose command surface MATCHES. One running older code answers a
                # ping perfectly and then misreads a command — which is how a stale bridge has twice
                # surfaced as something else entirely (`unexpected keyword argument 'mask'`, a bare
                # "Preview failed"). Treating a mismatch as not-adoptable is the same rule the preview
                # worker follows; see NAPARI_PROTOCOL.
                protocol = get(reply, "protocol", nothing)
                if protocol == NAPARI_PROTOCOL
                    _viewer_ref[] = probe
                    @info "Adopted existing Napari bridge on port $(probe.port)"
                    true
                else
                    @warn "Napari bridge on port $(probe.port) speaks protocol " *
                          "$(isnothing(protocol) ? "<pre-protocol>" : protocol), not $NAPARI_PROTOCOL " *
                          "— its code predates a change to the command surface. Replacing it; the " *
                          "viewer reopens where you left it (layer props are autosaved)."
                    Cecelia._kill_listeners_on_port(NAPARI_PORT)
                    false
                end
            catch
                false   # none running — fall through to launch a fresh one
            end
            adopted && return true
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
        meta_file = state_file(pending.proj_dir, pending.image_uid)
        raw       = read_ccid_raw(meta_file)
        filename  = versioned_get_field(raw, "filepath", nothing)   # nothing → _active
        zarr_path = joinpath(pending.proj_dir, "0", pending.image_uid, string(something(filename, "")))
        ch_raw    = versioned_get_field(raw, "imChannelNames", nothing)
        ch_names  = (ch_raw isa AbstractVector) ? collect(String, ch_raw) : nothing
        task_dir  = joinpath(pending.proj_dir, "1", pending.image_uid)

        p_show_3d       = hasproperty(pending, :show_3d)          ? pending.show_3d          : false
        p_as_dask       = hasproperty(pending, :as_dask)          ? pending.as_dask          : true
        p_show_labels = hasproperty(pending, :show_labels) ? pending.show_labels : false
        p_all_labels  = hasproperty(pending, :all_labels)  ? pending.all_labels  : Dict{String,Vector{String}}()
        p_show_branch_labels = hasproperty(pending, :show_branch_labels) ? pending.show_branch_labels : false
        p_all_branch_labels  = hasproperty(pending, :all_branch_labels)  ? pending.all_branch_labels  : Dict{String,Vector{String}}()
        p_labels_cache = hasproperty(pending, :labels_cache) ? pending.labels_cache : false
        _do_open!(v, zarr_path, task_dir, ch_names; show_3d = p_show_3d, as_dask = p_as_dask)
        _current_zarr_path[] = zarr_path
        _current_task_dir[]  = task_dir
        _current_image_uid[] = pending.image_uid

        if p_show_labels && !isempty(p_all_labels)
            _show_all_labels!(v, p_all_labels, true; cache = p_labels_cache)
        end
        if p_show_branch_labels && !isempty(p_all_branch_labels)
            _show_all_branch_labels!(v, p_all_branch_labels, true; cache = p_labels_cache)
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

# Skeleton-labels equivalent (branchLabels/ store, `({vn}) Branches` layer). Parallel to
# _parse_all_labels; kept separate so the branch payload never mixes into the generic labels
# picker (BRANCHING_PLAN Decision 6).
function _parse_all_branch_labels(data)::Dict{String,Vector{String}}
    raw = get(data, :allBranchLabels, nothing)
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
function _show_all_labels!(v::NapariViewer, all_labels::Dict{String,Vector{String}}, show::Bool;
                           cache::Bool=false, preview::Bool=false, contour::Int=0)
    errs = String[]
    for (vn, files) in all_labels
        try
            show_labels!(v; value_name = vn, label_files = files, show_labels = show, cache = cache,
                         preview = preview, contour = contour)
        catch e
            @warn "show_labels failed" value_name=vn files=files preview=preview exception=(e, catch_backtrace())
            push!(errs, "$vn: $(sprint(showerror, e))")
        end
    end
    isempty(errs) || error("show_labels failed for: " * join(errs, "; "))
end

# Re-read live-preview layers in place — same fail-open-then-aggregate contract as _show_all_labels!,
# because this runs off progress ticks and one broken value_name must not stop the others refreshing.
function _refresh_all_labels!(v::NapariViewer, all_labels::Dict{String,Vector{String}})
    errs = String[]
    for (vn, files) in all_labels
        try
            refresh_labels!(v; value_name = vn, label_files = files)
        catch e
            @warn "refresh_labels failed" value_name=vn files=files exception=(e, catch_backtrace())
            push!(errs, "$vn: $(sprint(showerror, e))")
        end
    end
    isempty(errs) || error("refresh_labels failed for: " * join(errs, "; "))
end

# Skeleton-labels equivalent — same fail-open-then-aggregate contract as _show_all_labels!.
function _show_all_branch_labels!(v::NapariViewer,
                                  all_branch_labels::Dict{String,Vector{String}}, show::Bool;
                                  cache::Bool=false)
    errs = String[]
    for (vn, files) in all_branch_labels
        try
            show_branch_labels!(v; value_name = vn, label_files = files, show_labels = show,
                                cache = cache)
        catch e
            @warn "show_branch_labels failed" value_name=vn files=files exception=(e, catch_backtrace())
            push!(errs, "$vn: $(sprint(showerror, e))")
        end
    end
    isempty(errs) || error("show_branch_labels failed for: " * join(errs, "; "))
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
    show_branch_labels_req = Bool(get(data, :showBranchLabels, false))
    all_branch_labels      = _parse_all_branch_labels(data)
    # Opt-in cache: napari's global dask cache serves stale bytes across seg re-runs because
    # da.from_zarr gives the same task name for the same path (napari_utils.add_labels docstring).
    # Default false (correct); users can flip on for faster slice-scrubbing when they're not
    # iterating on the segmentation.
    labels_cache    = Bool(get(data, :labelsCache, false))
    # 0 = filled (napari's default); N draws each label as an N-px outline instead
    labels_contour  = _label_contour(data)
    # Whether this open is worth telling the app about. A RECORDING opens an image per cell, applies a
    # config, records and moves on; those are not the user arriving at an image, and announcing them as
    # such is actively harmful — the app-level auto-show (`useNapariAutoShow`) answers `napari:opened`
    # with a full overlay re-push, which then BLOCKS on `_viewer_lock` (held for the whole sequence) and
    # lands in a burst once the movie is finished, one per cell per image. Harmless to the file, but it
    # is why the masks LOOKED present while you watched the window: they came back the moment the
    # render ended, long after every frame had been captured. Default true — only the recorder opts out.
    announce        = Bool(get(data, :announce, true))

    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error = "imageUid required"))

    proj_dir  = joinpath(projects_dir(), project_uid)
    meta_file = state_file(proj_dir, image_uid)
    isdir(proj_dir)   || return 404, JSON3.write((; error = "Project not found: $project_uid"))
    isfile(meta_file) || return 404, JSON3.write((; error = "Image metadata not found: $image_uid"))

    raw = read_ccid_raw(meta_file)

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
                                 all_labels,
                                 show_branch_labels = show_branch_labels_req,
                                 all_branch_labels,
                                 labels_cache)
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
            _show_all_labels!(v, all_labels, true; cache = labels_cache, contour = labels_contour)
        end
        if show_branch_labels_req && !isempty(all_branch_labels)
            _show_all_branch_labels!(v, all_branch_labels, true; cache = labels_cache)
        end

        # Auto-load layer props for the newly opened image
        if auto_load
            _try_load_layer_props!(v, task_dir, zarr_path)
        end
        # Wire live autosave for this image (after the load, so the load isn't echoed back).
        _configure_autosave!(v, task_dir, zarr_path, auto_save)

        @info "Opened image in Napari" image_uid zarr_path
        announce && broadcast_ws(Dict{String,Any}("type" => "napari:opened", "imageUid" => image_uid))
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
    clean = data === nothing ? false : Bool(get(data, :clean, false))   # E1: hide baked scale bar/timestamp
    path = tempname() * ".png"
    _with_viewer() do
    try
        # fit_data → tight-fit to the data extent at native resolution: no black margins, and the figure
        # matches the viewer (image fills the frame) instead of a tiny image in a big black canvas.
        reply    = save_screenshot!(v, path; fit_data = true, clean = clean)
        # store the PNG as a SIDECAR file (settings/board-assets/<id>.png), not base64 in the board JSON,
        # so analysisBoards.json stays small (autosave-friendly). Return only the id + snapshot.
        asset_id = _save_board_asset_file(project_uid, path)
        return 200, JSON3.write((;
            assetId   = asset_id,
            viewState = get(reply, "view_state", Dict{String,Any}()),
            imageUid  = _current_image_uid[],
            extentUm  = get(reply, "extent_um", nothing),   # captured frame physical size → still scale bar (E2)
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

# Requested movie output size from a request body: `(size_x, size_y)`, each `nothing` when absent, blank
# or non-positive — which means "record at the napari canvas size" (the default, and what every movie was
# before the size fields existed). ONE reader for all three surfaces (single record, animation, batch) so
# "blank = canvas" is defined once; the pixel-level validation (clamp, even axes) lives in Python's
# `movie_io.coerce_movie_size`. See docs/NAPARI.md.
function _movie_size_params(data)
    read_axis(key) = begin
        raw = get(data, key, nothing)
        v = raw === nothing ? nothing : tryparse(Int, string(raw))
        (v === nothing || v <= 0) ? nothing : v
    end
    read_axis(:sizeX), read_axis(:sizeY)
end

# Recording is NOT a REST route. `POST /api/napari/record-timelapse` and `record-animation` used to
# block for the whole render and return the finished path, which meant no progress and no way to stop a
# 4K render started by mistake. Both are now WS messages (`movie:record`, api/src/sockets.jl →
# `run_single_movie` below), on the same task rail as the batch. See docs/NAPARI.md → *Movie output size*
# and docs/API.md.

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

# {proj}/movies/ for an image (img._dir = {proj}/1/{uid}); created if missing. One place the movies
# dir is derived — the single-image recorders and the batch path all go through here.
function _movies_dir(img)::String
    d = joinpath(dirname(dirname(img._dir)), "movies")
    mkpath(d)
    d
end

# Movie output path named by the IMAGE (not attrs) — used by the single-image recorders. Sanitises
# img.name, falls back to the uid when blank/unsafe. `suffix` distinguishes timelapse ("") from
# animation ("_animation").
function _movie_named_path(img, uid::AbstractString; suffix::AbstractString = "")::String
    safe = replace(strip(img.name), r"[^A-Za-z0-9._-]+" => "_")
    joinpath(_movies_dir(img), (isempty(safe) ? String(uid) : safe) * suffix * ".mp4")
end

# A user-supplied filename addition → a safe `_suffix` fragment, or "" for none. Same character rule as
# the image name above, so one movie name can't be sanitised two ways.
#
# It exists because a movie is named after the IMAGE: record the AF-corrected version and then the raw
# import and the second overwrites the first, with nothing in the name to say which is which. The
# frontend prefills it with the open image VERSION (the usual reason two movies of one image differ),
# but it is free text — the comparison someone wants to label is not always a version.
const MOVIE_SUFFIX_MAX = 40
function _movie_suffix(raw)::String
    s = strip(String(raw === nothing ? "" : raw))
    isempty(s) && return ""
    safe = replace(s, r"[^A-Za-z0-9._-]+" => "_")
    safe = strip(safe, ['_', '.'])                      # no leading/trailing separators in a filename
    isempty(safe) && return ""
    "_" * first(safe, MOVIE_SUFFIX_MAX)
end

# Sentinel token in `file_attrs` meaning "the shown channel names joined by '-'" — mirrors the
# frontend MOVIE_CHANNELS_TOKEN (utils/batchMovie.ts); keep the two in sync.
const MOVIE_CHANNELS_TOKEN = "__channels__"

# Attr-named output filename: <attr1>_<attr2>_..._<uid>[_suffix].mp4 (mirrors the R `paste(fileAttrs...) _ uid`).
# `file_attrs` is the ordered list of attribute keys and/or the channels token; `channel_names` are the
# channels shown in the movie (used only where the token appears, joined by '-'). Blank/missing attrs
# are dropped; the uid always terminates the name so batch outputs never collide. Falls back to just
# the uid when no file_attrs are given. Pure (attr dict + uid + channels) → testable.
function _movie_basename(attr::AbstractDict, uid::AbstractString, file_attrs::Vector{String},
                         channel_names::Vector{String} = String[];
                         suffix::AbstractString = "")::String
    parts = String[]
    for a in file_attrs
        if a == MOVIE_CHANNELS_TOKEN
            chans = join(filter(!isempty, strip.(channel_names)), "-")
            isempty(chans) || push!(parts, chans)
        else
            val = strip(String(get(attr, a, "")))
            isempty(val) || push!(parts, val)
        end
    end
    push!(parts, String(uid))
    # the user's filename addition goes BEFORE the extension (it arrives already sanitised + `_`-led
    # from `_movie_suffix`), so the file is still an .mp4 to every listing that filters on the suffix
    replace(join(parts, "_"), r"[^A-Za-z0-9._-]+" => "_") * suffix * ".mp4"
end

# Channels shown in the movie for `img` = the `config.channels` keys (the ones given a colormap),
# ordered by the image's channel list so the filename is stable. `config` may be missing/empty.
function _shown_channel_names(img, config, vn)::Vector{String}
    chans = get(config, :channels, nothing)
    (chans isa AbstractDict && !isempty(chans)) || return String[]
    wanted = Set(String(k) for k in keys(chans))
    ch_all = channel_names(img; value_name = vn)
    ch_all === nothing ? collect(wanted) : String[c for c in ch_all if c in wanted]
end

# ── Label layers in a movie ───────────────────────────────────────────────────
# TWO registries, one contract. Cell segmentation masks live in `img.labels` and render as
# `({vn}) Labels`; skeletons from `segment.branching` live in `img.branch_labels` and render as
# `({vn}) Branches` — deliberately separate stores and a separate picker (BRANCHING_PLAN Decision 6).
# They are NOT unified here either; what is shared is the machinery, because they had the identical
# bug for the identical reason: `open_image` clears the canvas and the movie path never asked for
# them back.
#
# What a movie config asks for is THREE-valued, for both:
#
#   * `nothing` — the config says nothing (it predates the setting, or the caller records the live
#     view). Leave the canvas alone.
#   * `String[]` — an explicit "none". Not the same as `nothing`: a user who cleared the picker must
#     get a movie without them, even though the viewer still shows them.
#   * a list — show exactly these, hide every other registered set.
#
# Unregistered names are dropped rather than passed on to the bridge, which would log a skip per
# frameless store; the frontend's `normaliseItems` drops them too, so both ends agree on a deleted set.

# The registries, read defensively: `img.labels`/`img.branch_labels` exist on a CciaImage, but these
# helpers are also exercised against light NamedTuple stand-ins in the tests.
_label_registry(img)::Dict{String,Vector{String}} =
    hasproperty(img, :labels) && img.labels isa AbstractDict ?
        Dict{String,Vector{String}}(String(k) => collect(String, v) for (k, v) in img.labels) :
        Dict{String,Vector{String}}()
_branch_registry(img)::Dict{String,Vector{String}} =
    hasproperty(img, :branch_labels) && img.branch_labels isa AbstractDict ?
        Dict{String,Vector{String}}(String(k) => collect(String, v) for (k, v) in img.branch_labels) :
        Dict{String,Vector{String}}()

# One reader for both keys — `:labelValueNames` against the mask registry, `:branchValueNames`
# against the skeleton one.
function _config_set_names(config, key::Symbol, known::AbstractDict)::Union{Nothing,Vector{String}}
    raw = get(config, key, nothing)
    raw === nothing && return nothing
    seen = Set{String}()
    String[v for v in (String(x) for x in raw)
           if haskey(known, v) && !(v in seen) && (push!(seen, v); true)]
end

_config_label_value_names(config, img)  = _config_set_names(config, :labelValueNames,  _label_registry(img))
_config_branch_value_names(config, img) = _config_set_names(config, :branchValueNames, _branch_registry(img))

# Label OUTLINE width in pixels: 0 = filled (napari's default), N = an N-px contour, which is what lets
# the channel signal under a mask stay readable. Read from a request body or a movie config through the
# one accessor so the routes and the recorder cannot disagree about the key or its floor. Clamped
# rather than validated — a negative contour is meaningless to napari, and failing a whole batch over a
# display nicety would be the wrong trade.
const LABEL_CONTOUR_MAX = 10
_label_contour(src)::Int = clamp(_to_int(get(src, :labelContour, 0)), 0, LABEL_CONTOUR_MAX)

# How much of the z stack a movie shows: the whole thing as a 3D render, or one slice in 2D.
# `show3D` wins — a z index alongside it is a leftover from the last time 2D was chosen, and dropping
# it silently would be worse than ignoring it. `nothing` for the slice means "whatever is showing",
# which is what every recording did before the setting existed.
_show_3d(src)::Bool = Bool(get(src, :show3D, false))
function _z_slice(src)::Union{Int,Nothing}
    _show_3d(src) && return nothing
    raw = get(src, :zSlice, nothing)
    raw === nothing ? nothing : max(0, _to_int(raw))
end

# {valueName => [store files]} for `vns`, the shape `_parse_all_labels`/`_show_all_labels!` consume.
# `nothing` (an agnostic config) and an empty list both give an empty map — the caller pairs it with a
# `showLabels` flag, which is what distinguishes them on the wire.
_files_for(known::AbstractDict, vns::Union{Nothing,AbstractVector})::Dict{String,Vector{String}} =
    vns === nothing ? Dict{String,Vector{String}}() :
        Dict{String,Vector{String}}(String(v) => known[String(v)] for v in vns
                                    if haskey(known, String(v)))

_label_files_for(img, vns)  = _files_for(_label_registry(img), vns)
_branch_files_for(img, vns) = _files_for(_branch_registry(img), vns)

# Show exactly what is wanted and hide every other registered set, on a canvas that was NOT re-opened.
# TWO calls, not four: `show-labels` carries one `show` flag but BOTH payloads (`allLabels` +
# `allBranchLabels`), and the handler is explicit that sending them together keeps them atomic against
# the bridge's layer reconciliation. Both go through the ordinary handler, so the movie path shows
# these the same way the viewer does — no second variant.
#
# `nothing` for either list means "leave that family alone", so a caller that drives masks but not
# skeletons (the batch, which has no branch picker) does not silently clear the skeletons.
function _apply_label_layers!(img; labels::Union{Nothing,AbstractVector} = nothing,
                                   branches::Union{Nothing,AbstractVector} = nothing,
                                   contour::Int = 0)::Nothing
    function split(known, wanted)
        wanted === nothing && return (Dict{String,Vector{String}}(), Dict{String,Vector{String}}())
        keep = Set(String(v) for v in wanted)
        (_files_for(known, collect(keep)),
         Dict{String,Vector{String}}(k => v for (k, v) in known if !(k in keep)))
    end
    show_l, hide_l = split(_label_registry(img), labels)
    show_b, hide_b = split(_branch_registry(img), branches)

    (isempty(hide_l) && isempty(hide_b)) || _call_napari_api(api_napari_show_labels,
        (; allLabels = hide_l, allBranchLabels = hide_b, showLabels = false))
    (isempty(show_l) && isempty(show_b)) || _call_napari_api(api_napari_show_labels,
        (; allLabels = show_l, allBranchLabels = show_b, showLabels = true, labelContour = contour))
    nothing
end

# Full attr-named output path under {proj}/movies/.
function _movie_out_path(img, file_attrs::Vector{String}, channel_names::Vector{String} = String[];
                         suffix::AbstractString = "")::String
    joinpath(_movies_dir(img),
             _movie_basename(img.attr, img.uid, file_attrs, channel_names; suffix = suffix))
end

# Apply an authored movie config to ONE image already resolvable by uid (F1.2). Opens the image (contrast
# from its saved layer props), sets each channel's colormap + visibility (only `channels` are shown, the
# rest hidden), then overlays tracks / populations / colour-by exactly as the ViewerPanel does — by
# calling the existing handlers. Caller holds `_with_viewer` so the whole sequence is atomic on the bridge.
function _apply_movie_config!(project_uid::String, image_uid::String, img, config; do_open::Bool = true)::Nothing
    vn_raw = strip(String(get(config, :valueName, "")))
    vn     = isempty(vn_raw) ? nothing : vn_raw
    # Which label layers this column shows — cell masks and skeletons, each `nothing` when the config
    # says nothing about that family (leave whatever is on screen alone) and a LIST otherwise, the
    # empty one included. See the `Label layers in a movie` block.
    label_vns  = _config_label_value_names(config, img)
    branch_vns = _config_branch_value_names(config, img)
    contour    = _label_contour(config)      # 0 = filled masks, N = an N-px outline

    # 1. open (auto-load saved props → per-image contrast, Decision 4; no auto-save — we're driving it).
    #    SKIP the open when this exact image (active version) is already shown: re-opening re-samples the
    #    channel contrast (add_image contrast=True), which would wipe the contrast the user set live if it
    #    was never saved to props. Preview passes do_open=false (it applies to the open image only), and a
    #    batch skips re-opening its first image when that's the one already open. Both preserve live contrast.
    already_open = (_current_image_uid[] == image_uid) && isempty(vn_raw)
    opened       = do_open && !already_open
    if opened
        ok, _ = _call_napari_api(api_napari_open, (; projectUid = project_uid, imageUid = image_uid,
            valueName = vn, autoLoadProps = true, autoSaveProps = false,
            # a recorder's opens are transient — see `announce` in api_napari_open
            announce = false,
            # The label layers ride the OPEN rather than a show-labels after it, so the saved layer
            # props (opacity above all — an opaque mask hides the channel under it) land on them,
            # exactly as the interactive viewer does it. `open_image` clears the canvas, so without
            # this the movie path silently drops every mask AND every skeleton the user had.
            showLabels = label_vns !== nothing, allLabels = _label_files_for(img, label_vns),
            showBranchLabels = branch_vns !== nothing,
            allBranchLabels = _branch_files_for(img, branch_vns),
            labelContour = contour))
        ok || error("could not open image in napari")
    end

    # 1b. …and when there was no open, nothing was cleared: the layers of the PREVIOUS column (or of
    #     the live viewer) are still on screen, so show/hide explicitly. This is what makes a
    #     segmentation comparison cheap — the columns differ only by which mask is up, no re-open.
    if !opened && (label_vns !== nothing || branch_vns !== nothing)
        _apply_label_layers!(img; labels = label_vns, branches = branch_vns, contour = contour)
    end

    # 1c. how much of the z stack to show — the whole thing in 3D, or one slice in 2D. AFTER the open,
    #     which resets the dims, and unconditional so a 2D recording pins its slice rather than
    #     inheriting whatever the previous cell left behind.
    _call_napari_api(api_napari_set_z_view,
                     (; show3D = _show_3d(config), zSlice = _z_slice(config)))

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

# ── Viewer-task cancel registry ───────────────────────────────────────────────
# Recordings (single + batch) are NOT scheduler tasks — napari is a single UI-serial viewer in api/, not
# a pooled headless job — so `cancel_task!` doesn't reach them. This lightweight flag, keyed by the
# client's taskId, is set by the `task:cancel` WS handler.
#
# The flag alone only stops a batch BETWEEN images, because a record occupies the bridge's command loop
# for its whole run. So cancelling also sends `record_cancel` to the bridge, which answers it on its WS
# thread WITHOUT queueing (see `request_record_cancel` there) and lets the frame loop see it mid-render.
# One registry for both surfaces: the Cancel button in the task list must mean the same thing whether
# the user recorded one movie or forty.
const _batch_cancel      = Dict{String,Bool}()
const _batch_cancel_lock = ReentrantLock()
_batch_register!(id)      = lock(() -> (_batch_cancel[id] = false),                       _batch_cancel_lock)
_batch_cancelled(id)      = lock(() -> get(_batch_cancel, id, false),                     _batch_cancel_lock)
_batch_clear!(id)         = lock(() -> delete!(_batch_cancel, id),                        _batch_cancel_lock)
function request_batch_cancel!(id)
    known = lock(() -> (haskey(_batch_cancel, id) && (_batch_cancel[id] = true)), _batch_cancel_lock)
    # Reach the frame loop too. Best-effort and unconditional on `known`: the bridge keys the flag by
    # task id and clears it when that recording ends, so a cancel for anything else is inert.
    known === true && try
        v = _viewer()
        v === nothing || send(v, Dict{String,Any}("type" => "record_cancel", "task_id" => id))
    catch e
        @warn "could not reach napari to cancel the recording" exception = e
    end
    nothing
end

# Append every non-transient, shown pop of `(vn, pt)` to `out` as {valueName, popType, path} — the shape
# overlay_legend_content consumes. Uses the SAME pop-map primitives the show-tracks handler does, so the
# card can't drift from what's actually rendered.
function _append_config_pop_paths!(out::Vector{Dict{String,Any}}, img, vn::AbstractString, pt::AbstractString)
    try
        m = _live_map(img, vn, pt)
        for path in pop_paths(m)
            p = pop_at(m, path)
            (p.transient || !p.show) && continue
            push!(out, Dict{String,Any}("valueName" => vn, "popType" => pt, "path" => path))
        end
    catch
        # pop map for this (vn, pt) unavailable → nothing to contribute
    end
end

# The overlay pops a movie config would SHOW, as one list of {valueName, popType, path} (the shape
# overlay_legend_content turns into {name, colour}) — point pops AND tracks together, matching the ONE
# "Populations" section the analysis-board strip / single-record card use. Reuses the CANONICAL
# resolvers the show-handlers use — `resolve_pops` for point pops, the pop maps for track gates &
# clusters, "/_tracked" for a segmentation's whole-track overlay — so it can't drift from what renders.
function _config_overlay_pops(img, config)
    out = Vector{Dict{String,Any}}()
    _has_label_props(img) || return out
    segs = String[v for v in versioned_keys(img.label_props) if !is_reserved_value_name(v)]

    if Bool(get(config, :showPopulations, false))
        pt = String(get(config, :popType, "flow"))
        for vn in segs
            try
                for L in resolve_pops(img, pt; value_name = vn)
                    (L.show && !L.is_track) || continue
                    push!(out, Dict{String,Any}("valueName" => vn, "popType" => pt, "path" => L.path))
                end
            catch
            end
        end
    end

    if Bool(get(config, :showTracks, false))
        for vn in collect(String, get(config, :trackValueNames, String[]))   # whole-seg → generic "tracks" row
            haskey(img.label_props, vn) && push!(out, Dict{String,Any}("valueName" => vn, "popType" => "track", "path" => "/_tracked"))
        end
        for vn in segs
            Bool(get(config, :showGatedTracks, false)) && _append_config_pop_paths!(out, img, vn, "track")
            Bool(get(config, :showTrackclust, false))  && _append_config_pop_paths!(out, img, vn, "trackclust")
        end
    end
    out
end

# Assemble the Julia-side title-card content for an image under a movie config (Phase H). Title = image
# name + its attribute values ("MERTK — mouse 1 — location B"); the Populations / Tracks / Colour-by
# sections all come from the CANONICAL `overlay_legend_content` helper. CHANNELS are NOT added here — the
# recorder adds them from the live viewer (the only place channel colour lives). Returns `nothing` when
# the card is disabled/absent so the recorder skips it.
function _title_card_content(img, config)
    tc = get(config, :titleCard, nothing)
    (tc === nothing || !(tc isa AbstractDict) || !Bool(get(tc, :enabled, false))) && return nothing

    attrs = String[]
    if img.attr isa AbstractDict
        for k in sort(collect(keys(img.attr)))
            v = strip(String(img.attr[k])); isempty(v) || push!(attrs, v)
        end
    end
    title = join(vcat([img.name], attrs), " — ")

    sections = Vector{Dict{String,Any}}()
    # Populations (incl. track pops) — the shown pops through the canonical legend helper → {name, colour}
    # rows. ONE section, matching the strip / single-record card (overlay_legend_content dedups by name).
    plist = _config_overlay_pops(img, config)
    if !isempty(plist)
        rows  = overlay_legend_content(img, "", plist, nothing).populations
        items = [Dict{String,Any}("label" => p["name"], "colour" => p["colour"]) for p in rows]
        isempty(items) || push!(sections, Dict{String,Any}("heading" => "Populations", "items" => items))
    end
    # Colour-by — value → pop colour + name for the colour-by measure.
    column = String(get(config, :colourBy, ""))
    if !isempty(column)
        legend = overlay_legend_content(img, column, nothing, get(config, :colourOverrides, nothing))
        items  = [Dict{String,Any}("label" => it["label"], "colour" => it["colour"]) for it in legend.colourBy["items"]]
        isempty(items) || push!(sections, Dict{String,Any}("heading" => "Colour by", "items" => items))
    end

    Dict{String,Any}(
        "enabled"     => true,
        "durationSec" => Float64(get(tc, :durationSec, 3.0)),
        "title"       => title,
        "note"        => String(get(tc, :note, "")),
        "sections"    => sections,   # the recorder prepends a "Channels" section from the live viewer
    )
end

# ── Side-by-side comparison ───────────────────────────────────────────────────
# docs/todo/MOVIE_COMPARE_PLAN.md. A comparison is N recordings plus one compose, NOT one clever
# render: each column goes through the SAME path a single movie uses, so overlays, staging, cancel and
# the size policy keep working untouched, and the finished files are composed frame-by-frame in the
# bridge (D1; D2 records why the alternative — several versions as layers of one canvas — was rejected).
#
# What differs between cells is TWO dimensions, and a movie is a GRID of them:
#
#   * image VERSIONS run across the columns (raw next to AF-corrected),
#   * segmentation MASKS run down the rows (model A above model B).
#
# Pick two of one and one of the other and the grid degenerates to a single row — a plain side-by-side
# comparison, whichever list it came from. Pick two of BOTH and you get the cross-product, which is the
# only layout that answers "does this correction change what each model segments?" in one file.
#
# Everything downstream of the grid is blind to what made two cells differ, which is what D9 bought:
# the pass loop, the per-frame progress arithmetic, cancel, staging, the caption band and the compose
# all key off `Vector{MovieRow}` and never ask. Adding a third dimension would be a builder here plus a
# picker in the UI — though note the cost is multiplicative, not additive (see `_grid_frame_total`).

# One column: what its caption says, and the movie config that produces it. Config keys are Symbols
# because that is how every reader here (`_apply_movie_config!`, `_title_card_content`,
# `_shown_channel_names`) addresses them.
const MovieColumn = NamedTuple{(:label, :config),Tuple{String,Dict{Symbol,Any}}}
# One row of the grid: the cells across it, and what the caption under the whole strip says. A
# single-row grid captions nothing — there is no outer compose to hang a label on.
const MovieRow = NamedTuple{(:label, :columns),Tuple{String,Vector{MovieColumn}}}

# The config's whole base, as the Symbol-keyed Dict a column carries. Split out so both builders pin
# their own key onto the SAME starting point — a column differs from its config by one entry, never by
# which keys survived being copied.
_column_base(config)::Dict{Symbol,Any} = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in pairs(config))

# The columns of a VERSION comparison: the authored config once per selected version, in the order the
# user put the chips in. `""` means the active version — what the config already meant on its own.
function _version_columns(config, value_names::AbstractVector)::Vector{MovieColumn}
    base = _column_base(config)
    MovieColumn[(; label  = (n = strip(String(vn)); isempty(n) ? "active" : n),
                   config = merge(base, Dict{Symbol,Any}(:valueName => strip(String(vn)))))
                for vn in value_names]
end

# The columns of a SEGMENTATION comparison: one label set per column, every column on the SAME image
# version. Pinning the version is what keeps the two dimensions independent — a mask row varies only
# the mask, so the row reads as one model's answer to one image.
#
# Cheap by construction: with the version fixed, no column after the first re-opens the image
# (`_version_is_open`), so the passes differ only by which mask is on screen — no re-sampled contrast,
# no reloaded pyramid.
function _segmentation_columns(config, label_value_names::AbstractVector,
                               value_name::AbstractString)::Vector{MovieColumn}
    base = _column_base(config)
    MovieColumn[(; label  = (n = strip(String(sn)); isempty(n) ? "none" : n),
                   config = merge(base, Dict{Symbol,Any}(
                       :valueName       => value_name,
                       :labelValueNames => String[strip(String(sn))])))
                for sn in label_value_names]
end

# The ONE grid builder every recorder calls. Versions across, masks down:
#
#   * 2+ of BOTH        → the cross-product. One row per mask; that row's cells are the versions.
#   * 2+ of one only    → a single row of that list, side by side — the plain comparison.
#   * neither           → one cell, which is an ordinary single recording.
#
# The degenerate cases are not special-cased anywhere below: a plain movie is a 1x1 grid, and a
# comparison is a 1xN one, so `_record_grid!` has exactly one shape to reason about.
function _compare_grid(config)::Vector{MovieRow}
    versions = _config_value_names(config)              # never empty ("" = the active version)
    masks    = _config_compare_segmentations(config)
    if length(versions) > 1 && length(masks) > 1
        # A cell draws ONE mask on ONE version; the row is captioned with the mask, so the caption
        # under each cell can stay the version and read as a column header repeated per row.
        return MovieRow[(; label   = m,
                           columns = _version_columns(
                               merge(_column_base(config), Dict{Symbol,Any}(:labelValueNames => String[m])),
                               versions))
                        for m in masks]
    end
    length(masks) > 1 &&
        return MovieRow[(; label = "", columns = _segmentation_columns(config, masks, first(versions)))]
    MovieRow[(; label = "", columns = _version_columns(config, versions))]
end

# The versions a config records, in column order. `valueNames` is the comparison list; a config from
# before it existed (or one the user never touched) carries a single `valueName`, and "" — the active
# version — is a perfectly good single column. Never empty, so callers always have one column.
function _config_value_names(config)::Vector{String}
    raw = get(config, :valueNames, nothing)
    names = raw === nothing ? String[] : [String(v) for v in raw]
    isempty(names) ? [String(get(config, :valueName, ""))] : names
end

# The segmentations a config splits into columns. Deliberately NOT `_config_label_value_names`: that
# one is image-scoped (it drops names this image doesn't have) and three-valued, while the column list
# is authored once for a whole batch and must not vary per image — an image missing one of the sets
# would otherwise get a different number of columns than its neighbours.
function _config_compare_segmentations(config)::Vector{String}
    raw = get(config, :labelValueNames, nothing)
    raw === nothing && return String[]
    seen = Set{String}()
    String[v for v in (strip(String(x)) for x in raw)
           if !isempty(v) && !(v in seen) && (push!(seen, v); true)]
end

# D4: how the columns are contrasted. "reference" (the default) applies column 1's intensity mapping to
# the others, so a correction is judged on one ruler; "version" leaves each column with the saved napari
# settings of its own version. Anything else reads as the default rather than failing a whole batch.
_share_contrast(mode)::Bool = String(mode) != "version"

# Frames one T-sweep writes for `img` over `[t_start, t_end]` — 0 when the image has no usable T axis.
# Mirrors the bridge's own range arithmetic (`napari_utils.record_timelapse`): one frame per timepoint,
# both ends inclusive.
function _t_sweep_frames(img, t_start::Int, t_end)::Int
    n = _to_int(get(img.meta, "SizeT", nothing))
    n <= 1 && return 0
    t0 = max(0, t_start)
    t1 = t_end === nothing ? n - 1 : min(_to_int(t_end), n - 1)
    t1 <= t0 ? 0 : (t1 - t0 + 1)
end

# Frames a whole grid renders, so the passes and the composes drive ONE progress bar instead of
# restarting it per cell: a pass per CELL, plus a compose per row (only when a row has something to
# compose) and one more to stack the rows. An estimate made before anything runs — the bridge clamps it
# if a pass comes out longer.
#
# Note what this makes visible: a grid is rows x cols RENDERS. 2 versions x 2 masks is four full
# recordings, not two — which is why the UI states the pass count on the button before you commit.
function _grid_frame_total(n_rows::Int, n_cols::Int, per_pass::Int)::Int
    cells = n_rows * n_cols
    (cells <= 1 || per_pass <= 0) && return 0
    composes = (n_cols > 1 ? n_rows : 0) + (n_rows > 1 ? 1 : 0)
    (cells + composes) * per_pass
end
# …and the same count read off the ACTUAL grid, which is what `_record_grid!` uses. It mirrors the loop
# unit for unit — a pass per cell, a compose per row that has something to compose (a one-cell row IS
# its own strip), and a stack when there is more than one row. The rectangular form above assumes every
# row is the same width; this one cannot be wrong about a grid it was handed, and the two are asserted
# to agree on rectangular input.
function _grid_frame_total(rows::Vector{MovieRow}, per_pass::Int)::Int
    cells = sum(length(r.columns) for r in rows; init = 0)
    (cells <= 1 || per_pass <= 0) && return 0
    (cells + count(r -> length(r.columns) > 1, rows) + (length(rows) > 1 ? 1 : 0)) * per_pass
end
# The single-row case by its old name — a 1xN grid, which is what every comparison was until masks
# became a second dimension.
_comparison_frame_total(n_columns::Int, per_pass::Int)::Int = _grid_frame_total(1, n_columns, per_pass)

# A captured view WITHOUT its per-layer props — the camera and the timepoint, nothing about intensity.
# What "each version keeps its own saved napari settings" (D4) applies to the later columns.
_camera_only(snapshot) =
    Dict{String,Any}(String(k) => v for (k, v) in pairs(snapshot) if String(k) != "layers")

# Is `value_name` the image version the viewer already has open? A column must not re-open one that is:
# re-opening re-samples the channel contrast (`add_image contrast=True`), which would throw away a look
# the user set live and never saved — and "record what is on screen" is the whole promise of the
# viewer's Record button. Blank already means "the active version", which `_apply_movie_config!` treats
# as already-open; this extends the same skip to naming that version explicitly.
#
# Checked for EVERY column, not just the first. On the version axis only the first can match (each later
# column names a different version, so the open happens as it must); on the segmentation axis every
# column names the same version, so after column 1 opens it none of the others re-open at all.
function _version_is_open(img, image_uid::AbstractString, value_name::AbstractString)::Bool
    _current_image_uid[] == image_uid || return false
    vn = strip(value_name)
    isempty(vn) && return true
    path = img_filepath(img, String(vn))
    path !== nothing && _current_zarr_path[] == path
end

# Record every cell of `rows` and compose them into `out_path`. Returns the bridge-shaped reply
# (`frames`/`path`/`cancelled`/…), so a caller treats a grid exactly like a single record.
#
# ONE cell is not a special case: it records straight to `out_path`, which is what a plain movie has
# always been — so both callers can route everything through here.
#
# The compose is NESTED, and deliberately so: each row's cells are stitched side by side into a strip,
# then the strips are stacked. `movie_io.stitch_movies` already does one dimension at a time, correctly
# and with a working cancel, so a grid is two passes of it rather than a second compositor that would
# have to re-derive padding, caption bands and staging. A single-row grid skips the outer stitch
# entirely and is byte-for-byte the comparison it always was — including honouring `layout`, which is
# the user's row-vs-column choice and only means anything when there is one row to point it at.
#
# Contrast (D4): the FIRST cell establishes the look and every later cell inherits it (or keeps its own
# version's saved settings). Across a grid that matters more than across a row — a mask row is the same
# pixels twice, and the eye reads a contrast difference as a segmentation difference.
#
# Holds `_with_viewer` across the WHOLE sequence — between two passes the viewer must not be opened on
# something else, or the next cell would record a different image.
function _record_grid!(task_id::String, project_uid::String, image_uid::String, img,
                       rows::Vector{MovieRow}, out_path::String;
                       fps::Int = 15, size_x = nothing, size_y = nothing, title_card = nothing,
                       share_contrast::Bool = true, layout::String = "row",
                       t_start::Int = 0, t_end = nothing, api_url = nothing,
                       show_timestamp::Bool = true, show_scale_bar::Bool = true)::Dict{String,Any}
    isempty(rows) && error("no rows to record")
    n_rows = length(rows)
    cells  = sum(length(r.columns) for r in rows)
    cells == 0 && error("no columns to record")
    _with_viewer() do
        v = _viewer()
        (isnothing(v) || !_viewer_alive()) && error("Napari not running")
        if cells == 1
            _apply_movie_config!(project_uid, image_uid, img, rows[1].columns[1].config)
            return record_timelapse!(v, out_path; fps = fps, size_x = size_x, size_y = size_y,
                                     t_start = t_start, t_end = t_end, title_card = title_card,
                                     task_id = task_id, api_url = api_url,
                                     show_timestamp = show_timestamp, show_scale_bar = show_scale_bar)
        end

        per_pass = _t_sweep_frames(img, t_start, t_end)
        total    = _grid_frame_total(rows, per_pass)
        strips   = String[]
        temps    = String[]          # everything staged, so `finally` can sweep it in one place
        shared   = nothing
        done     = 0                 # cells rendered so far — for the "[n/cells]" log line
        # ONE progress bar spans every render AND every compose, so the bar is a running COUNTER of
        # work units rather than an offset computed per stage. Computed offsets are what went wrong
        # first: they have to account for the row composes interleaved between the rows, and get it
        # wrong differently again when a row has a single cell and is not composed at all. A counter
        # cannot drift from `_grid_frame_total`, which counts the same units.
        slot     = 0
        next_offset() = (o = slot * per_pass; slot += 1; o)
        try
            for (ri, row) in enumerate(rows)
                cell_paths = String[]
                for (ci, col) in enumerate(row.columns)
                    cfg = col.config
                    if _version_is_open(img, image_uid, String(get(cfg, :valueName, "")))
                        cfg = merge(cfg, Dict{Symbol,Any}(:valueName => ""))   # don't re-open it
                    end
                    _apply_movie_config!(project_uid, image_uid, img, cfg)
                    if shared === nothing
                        shared = capture_view_state(v)
                    elseif !isempty(shared)
                        apply_view_state!(v, share_contrast ? shared : _camera_only(shared))
                    end
                    path = string(out_path, ".r", ri, "c", ci, ".tmp.mp4")
                    push!(temps, path); push!(cell_paths, path)
                    where = n_rows > 1 ? "$(row.label) · $(col.label)" : col.label
                    ws_log(nothing, task_id, "[$(done + 1)/$cells] recording $where")
                    # No title card per pass — it goes on the fully composed file, once (D6).
                    resp = record_timelapse!(v, path; fps = fps, size_x = size_x, size_y = size_y,
                                             t_start = t_start, t_end = t_end, title_card = nothing,
                                             task_id = task_id, api_url = api_url,
                                             frame_offset = next_offset(), frame_total = total,
                                             show_timestamp = show_timestamp,
                                             show_scale_bar = show_scale_bar)
                    # A cancelled pass ends the whole grid: nothing is composed, and `out_path` still
                    # holds whatever movie was there before.
                    get(resp, "cancelled", false) === true && return resp
                    done += 1
                end

                # One row → this IS the final compose (labels + title card + the user's layout).
                if n_rows == 1
                    ws_log(nothing, task_id, "composing $(length(cell_paths)) columns → $(basename(out_path))")
                    return stitch_movies!(v, out_path, cell_paths;
                                          labels = [c.label for c in row.columns],
                                          layout = layout, fps = fps, title_card = title_card,
                                          task_id = task_id, api_url = api_url,
                                          frame_offset = next_offset(), frame_total = total)
                end

                if length(cell_paths) == 1
                    # nothing to stitch across — the cell IS the strip, and consumes no compose slot
                    # (which is exactly what `_grid_frame_total` assumes when n_cols == 1)
                    push!(strips, cell_paths[1])
                else
                    strip = string(out_path, ".row", ri, ".tmp.mp4")
                    push!(temps, strip)
                    ws_log(nothing, task_id, "composing row $ri/$n_rows ($(row.label))")
                    resp = stitch_movies!(v, strip, cell_paths;
                                          labels = [c.label for c in row.columns],
                                          layout = "row", fps = fps, title_card = nothing,
                                          task_id = task_id, api_url = api_url,
                                          frame_offset = next_offset(), frame_total = total)
                    get(resp, "cancelled", false) === true && return resp
                    push!(strips, strip)
                end
            end

            ws_log(nothing, task_id, "stacking $n_rows rows → $(basename(out_path))")
            return stitch_movies!(v, out_path, strips; labels = [r.label for r in rows],
                                  layout = "column", fps = fps, title_card = title_card,
                                  task_id = task_id, api_url = api_url,
                                  frame_offset = next_offset(), frame_total = total)
        finally
            # The per-cell and per-row recordings are scratch. They are named `*.tmp.mp4`, so anything
            # left by a hard kill is already hidden from `/api/movies` and swept by
            # `_clear_stale_staging`.
            for p in temps
                isfile(p) && (try; rm(p); catch; end)
            end
        end
    end
end

# F1.3 batch runner — invoked async from the WS layer (`movie:batch`). For each image: apply the config,
# record the T-sweep to an attr-named mp4, emit task:progress/log so it drives the existing task UI. `rep`
# = representative uid for status/result. Errors on one image are logged and the batch continues.
function run_batch_movies(task_id::String, project_uid::String, image_uids::Vector{String},
                          config, file_attrs::Vector{String}, fps::Int;
                          size_x::Union{Int,Nothing}=nothing, size_y::Union{Int,Nothing}=nothing,
                          suffix::AbstractString="")
    n   = length(image_uids)
    rep = isempty(image_uids) ? "" : first(image_uids)
    done = 0; errors = String[]
    # fail fast (one clear message, not N per-image errors) if the viewer isn't up — the batch drives
    # the live window, so napari must already be running.
    if isnothing(_viewer()) || !_viewer_alive()
        ws_log(nothing, task_id, "[ERROR] Napari is not running — open an image first, then generate")
        ws_status(nothing, task_id, "failed", rep; fun="movie:batch", pool="viewer")
        _batch_clear!(task_id)
        return nothing
    end
    ws_status(nothing, task_id, "running", rep; fun="movie:batch", pool="viewer")
    ws_progress(nothing, task_id, 0, n)
    t_start = Int(get(config, :tStart, 0))
    t_end_v = get(config, :tEnd, nothing)
    t_end   = t_end_v === nothing ? nothing : Int(t_end_v)
    # What each movie shows, as a grid (versions across, masks down). More than one cell makes every
    # movie a comparison; one cell is the ordinary batch — the same path, so the batch keeps no second
    # recording loop of its own. Authored once for the whole batch, so build it once.
    grid           = _compare_grid(config)
    share_contrast = _share_contrast(get(config, :compareContrast, ""))
    layout         = String(get(config, :compareLayout, "row"))
    # napari's baked overlays. Default true = what every movie was; the batch RE-OPENS each image, which
    # turns the scale bar back on, so this is the only way to keep them out of a batch.
    show_ts        = Bool(get(config, :showTimestamp, true))
    show_sb        = Bool(get(config, :showScaleBar, true))
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
            vn_raw = strip(String(get(config, :valueName, "")))
            chan_names = _shown_channel_names(img, config, isempty(vn_raw) ? nothing : vn_raw)
            # same filename addition as a single record — a corrected batch and a raw batch would
            # otherwise write the same attr-named files over each other
            path = _movie_out_path(img, file_attrs, chan_names; suffix = _movie_suffix(suffix))
            ws_log(nothing, task_id, "[$i/$n] $(img.name) → $(basename(path))")
            # One cell per (mask, version) pair, or the one authored config. Both go through
            # `_record_grid!`, so the batch has no second recording path of its own.
            # task_id: the bridge polls the SAME cancel flag per frame, so Cancel stops the image being
            # recorded rather than only the ones after it. Per-frame progress is deliberately NOT
            # relayed here — the batch's bar counts images.
            resp = _record_grid!(task_id, project_uid, uid, img, grid, path;
                                    fps = fps, size_x = size_x, size_y = size_y,
                                    t_start = t_start, t_end = t_end,
                                    title_card = _title_card_content(img, config),
                                    share_contrast = share_contrast, layout = layout,
                                    show_timestamp = show_ts, show_scale_bar = show_sb,
                                    api_url = nothing)
            if get(resp, "cancelled", false) === true
                # cancelled mid-image: nothing was written (the staged file is removed), so this image
                # is neither done nor an error — the loop's cancel check ends the run on the next pass
                ws_log(nothing, task_id, "[$i/$n] cancelled — $(basename(path)) not written")
            else
                done += 1
                ws_log(nothing, task_id, "[$i/$n] done → $(basename(path))")
            end
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
    ws_status(nothing, task_id, status, rep; image_uids = image_uids, fun="movie:batch", pool="viewer")
    _batch_clear!(task_id)
    nothing
end

# Single recording (timelapse or keyframe animation) on the SAME task rail as the batch: registered for
# cancel, `task:progress` per frame, `task:status`/`task:result` at the end. Invoked async from the WS
# layer (`movie:record`).
#
# It used to be a blocking POST that returned when the movie was finished, which gave the user a frozen
# button and no way out of a 4K render they started by accident. The batch already had progress + Cancel
# purely because it loops over images in Julia and can report between them; a single record has no
# "between", so the events come from inside the bridge's frame loop instead (`_record_hooks` /
# `record_cancel` there, relayed by the `recordProgress` branch of `api_napari_event`).
#
# `keyframes === nothing` records the open image's T-sweep; otherwise it renders the keyframe animation.
#
# Two or more cells makes it a COMPARISON instead — versions across the columns, masks down the rows,
# one pass per cell, composed into one file (`_record_grid!`). A single cell leaves the plain single
# record exactly as it was: it records what is on screen without touching the viewer at all, and that
# contract is worth more than routing one movie through the comparison path for symmetry.
#
# `label_value_names` is also what carries the masks INTO a comparison: each pass re-applies the config
# to a canvas the open cleared, so a version comparison keeps its segmentations only because the caller
# names them here.
function run_single_movie(task_id::String, project_uid::String, image_uid::String;
                          fps::Int = 15, size_x::Union{Int,Nothing} = nothing,
                          size_y::Union{Int,Nothing} = nothing, suffix::AbstractString = "",
                          title_card = nothing, keyframes = nothing,
                          value_names::Vector{String} = String[],
                          label_value_names::Union{Vector{String},Nothing} = nothing,
                          branch_value_names::Union{Vector{String},Nothing} = nothing,
                          label_contour::Int = 0,
                          show_3d::Bool = false, z_slice::Union{Int,Nothing} = nothing,
                          share_contrast::Bool = true, layout::String = "row",
                          show_timestamp::Bool = true, show_scale_bar::Bool = true,
                          api_url::AbstractString = "http://localhost:8080")
    animation = keyframes !== nothing
    # The viewer's recorder authors no channels/overlays of its own (it records the live view), so its
    # config is just the two lists — but it goes through the SAME builder the batch does.
    column_config = Dict{Symbol,Any}(:valueNames => value_names, :labelContour => label_contour,
                                     :show3D => show_3d)
    z_slice === nothing || (column_config[:zSlice] = z_slice)
    label_value_names === nothing  || (column_config[:labelValueNames]  = label_value_names)
    branch_value_names === nothing || (column_config[:branchValueNames] = branch_value_names)
    grid      = _compare_grid(column_config)
    comparing = !animation && sum(length(r.columns) for r in grid) > 1
    fun       = animation ? "movie:animation" : "movie:record"
    img, err  = _gating_image(project_uid, image_uid)
    if err !== nothing
        ws_log(nothing, task_id, "[ERROR] image not found")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "viewer")
        _batch_clear!(task_id)
        return nothing
    end
    v = _viewer()
    if isnothing(v) || !_viewer_alive()
        ws_log(nothing, task_id, "[ERROR] Napari is not running — open an image first, then record")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "viewer")
        _batch_clear!(task_id)
        return nothing
    end

    # `_movie_named_path` names by the IMAGE; the user's suffix is what distinguishes two movies of the
    # same image (the corrected version vs the raw import). The animation marker stays last so the two
    # kinds still sort together.
    path = _movie_named_path(img, image_uid;
                             suffix = _movie_suffix(suffix) * (animation ? "_animation" : ""))
    ws_status(nothing, task_id, "running", image_uid; fun = fun, pool = "viewer")
    ws_progress(nothing, task_id, 0, 1)          # a real total arrives with the first frame report
    ws_log(nothing, task_id, comparing ?
           "Comparing $(join((isempty(r.label) ? join((c.label for c in r.columns), " · ") :
                              string(r.label, ": ", join((c.label for c in r.columns), " · "))
                              for r in grid), " / ")) → $(basename(path))" :
           "Recording → $(basename(path))")

    status = "done"
    result = Dict{String,Any}("path" => path)
    try
        resp = comparing ?
            _record_grid!(task_id, project_uid, image_uid, img, grid, path;
                             fps = fps, size_x = size_x, size_y = size_y, title_card = title_card,
                             share_contrast = share_contrast, layout = layout, api_url = api_url,
                             show_timestamp = show_timestamp, show_scale_bar = show_scale_bar) :
            _with_viewer() do
                animation ?
                    record_keyframes!(v, path, keyframes; fps = fps, size_x = size_x, size_y = size_y,
                                      title_card = title_card, task_id = task_id, api_url = api_url,
                                      show_timestamp = show_timestamp, show_scale_bar = show_scale_bar) :
                    record_timelapse!(v, path; fps = fps, size_x = size_x, size_y = size_y,
                                      title_card = title_card, task_id = task_id, api_url = api_url,
                                      show_timestamp = show_timestamp, show_scale_bar = show_scale_bar)
            end
        frames = Int(get(resp, "frames", 0))
        if get(resp, "cancelled", false) === true
            status = "cancelled"
            # nothing to clean up here: the bridge stages the file and removes it on cancel, so any
            # previous movie at this path is still the one on disk
            ws_log(nothing, task_id, "[CANCELLED] stopped after $frames frame(s) — nothing written")
        else
            merge!(result, Dict{String,Any}("frames" => frames,
                                            "sizeX" => get(resp, "sizeX", nothing),
                                            "sizeY" => get(resp, "sizeY", nothing)))
            sz = get(resp, "sizeX", nothing) === nothing ? "" :
                 " at $(get(resp, "sizeX", 0))x$(get(resp, "sizeY", 0))"
            ws_log(nothing, task_id, "Recorded $frames frames$sz → $(basename(path))")
        end
    catch e
        status = "failed"
        @warn "record failed" exception = e
        ws_log(nothing, task_id, "[ERROR] $(sprint(showerror, e))")
    end
    result["cancelled"] = status == "cancelled"
    ws_result(nothing, task_id, image_uid, result)
    ws_status(nothing, task_id, status, image_uid; fun = fun, pool = "viewer")
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
    # A single request can carry cell labels + branch (skeleton) labels. `showLabels` governs both;
    # the two payloads are independent and either may be empty.
    all_branch_labels = _parse_all_branch_labels(data)
    labels_cache = Bool(get(data, :labelsCache, false))
    # `preview` applies to `allLabels` only: it shows a store a task is still writing, in its own
    # layer (see show_labels! / the bridge). Branch labels are written once at the end of
    # segment.branching, so there is never a partial branch store to preview.
    preview = Bool(get(data, :preview, false))
    contour = _label_contour(data)

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))

    _with_viewer() do
        try
            _show_all_labels!(v, all_labels, show; cache = labels_cache, preview = preview,
                              contour = contour)
            _show_all_branch_labels!(v, all_branch_labels, show; cache = labels_cache)
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/refresh-labels ─────────────────────────────────────
# Re-read the live-preview layers of a running task's label stores, in place. Separate from
# show-labels because it must stay cheap: it is called on progress ticks and does no layer teardown,
# so the preview keeps its position and display settings while the data underneath it advances.

function api_napari_refresh_labels(body_bytes::Vector{UInt8})
    data       = JSON3.read(String(body_bytes))
    all_labels = _parse_all_labels(data)
    isempty(all_labels) && return 200, JSON3.write((; ok = true))

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))

    _with_viewer() do
        try
            _refresh_all_labels!(v, all_labels)
            200, JSON3.write((; ok = true))
        catch e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/set-z-view ─────────────────────────────────────────
# Whole z stack (3D) or a single z slice (2D). Drives the live viewer AND is what a recording applies,
# so a movie of a 3D view is not silently flattened back to one plane by its re-open.
function api_napari_set_z_view(body_bytes::Vector{UInt8})
    data    = JSON3.read(String(body_bytes))
    show_3d = Bool(get(data, :show3D, false))
    z_raw   = get(data, :zSlice, nothing)
    z       = z_raw === nothing ? nothing : _to_int(z_raw)

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))
    _with_viewer() do
        try
            resp = set_z_view!(v; show_3d = show_3d, z = z)
            200, JSON3.write((; ok = true, ndisplay = get(resp, "ndisplay", nothing),
                                z = get(resp, "z", nothing)))
        catch e
            @warn "set_z_view failed" exception = e
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
function _apply_user_overrides!(overrides::Dict{String,String}, user)::Dict{String,String}
    (user === nothing || !(user isa AbstractDict)) && return overrides
    for (k, v) in pairs(user)
        (v === nothing || isempty(String(v))) && continue
        overrides[String(k)] = String(v)
    end
    overrides
end
_merge_user_overrides!(overrides::Dict{String,String}, data)::Dict{String,String} =
    _apply_user_overrides!(overrides, get(data, :colourOverrides, nothing))

# CANONICAL legend content for an image's overlays — the single source of truth shared by the
# overlay-legend endpoint (strip legend / napari strip, Phase C) AND the movie title card (Phase H),
# so all three read identical rows. Pure (no viewer). Given a `column` (colour-by measure), the
# `overlay_pops` list ({valueName, popType, path} or nothing) and user recolours, returns:
#  • colourBy: {column, items:[{value, colour(pop hex), label(pop name)}]} for each value on `column`
#  • populations: [{name, colour}] for each requested point/track pop (deduped by name).
function overlay_legend_content(img, column::AbstractString, overlay_pops, user_overrides)
    pt_all  = ("trackclust", "track", "clust", "flow")
    colours = _apply_user_overrides!(_colour_overrides_for(img, column, pt_all), user_overrides)
    labels  = _pop_labels_for(img, column, pt_all)
    cby = [Dict{String,Any}("value" => k, "colour" => colours[k], "label" => get(labels, k, k))
           for k in sort(collect(keys(colours)))]

    pops = Vector{Dict{String,Any}}()
    if overlay_pops !== nothing
        seen = Set{String}()   # dedupe by pop NAME — one pop spans segmentations (one layer each)
        for pp in overlay_pops
            vn   = String(get(pp, :valueName, ""))
            pt   = String(get(pp, :popType, ""))
            path = String(get(pp, :path, ""))
            (isempty(vn) || isempty(pt)) && continue
            if endswith(path, "_tracked")   # whole-segmentation "all tracks" → one generic grey row
                if !("tracks" in seen); push!(seen, "tracks"); push!(pops, Dict{String,Any}("name" => "tracks", "colour" => "#9ca3af")); end
                continue
            end
            try
                p = pop_at(_live_map(img, vn, pt), path)
                p.name in seen && continue
                push!(seen, p.name)
                push!(pops, Dict{String,Any}("name" => p.name, "colour" => p.colour))
            catch
                # pop map / path unavailable → skip
            end
        end
    end
    (; colourBy = Dict{String,Any}("column" => column, "items" => cby), populations = pops)
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

# ── REST: POST /api/napari/colour-branch-labels ───────────────────────────────
# Recolour the open image's Branches layer by a per-branch obs column read from
# `labelProps/{valueName}__branch.h5ad` (`branch-type` → categorical palette; `branch-distance`,
# `tortuosity`, etc. → continuous viridis). `column=""` resets. Parallel to colour-labels, but
# scoped to the branch sidecar; branch pops (from `ensure_filter_pop!` per branch-type) can supply
# colour overrides so the colouring matches what the gating plots show. See BRANCHING_PLAN.md
# Decisions 1–3 and the old `napari_utils.show_branching` (viridis DirectLabelColormap).
function api_napari_colour_branch_labels(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    column      = String(get(data, :column, ""))
    vn_req      = String(get(data, :valueName, "default"))

    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    vn = isempty(vn_req) ? "default" : vn_req

    v = _viewer()
    isnothing(v) && return 400, JSON3.write((; error = "Napari not running"))
    # Branch pops filtering on `column` supply their own colour (typical: the four ensure_filter_pop!
    # branch-types) — same rule as colour-labels, restricted to the branch pop_type.
    overrides = _merge_user_overrides!(
        _colour_overrides_for(img, column, ("branch",)), data)
    _with_viewer() do
        try
            resp = send(v, Dict{String,Any}("type" => "colour_branch_labels", "value_name" => vn,
                "column" => column, "colour_overrides" => overrides))
            200, JSON3.write((; ok = true,
                legend = get(resp, "legend", Dict{String,Any}()),
                legendLabels = _pop_labels_for(img, column, ("branch",))))
        catch e
            @warn "colour_branch_labels failed" exception = e
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end
end

# ── REST: POST /api/napari/overlay-legend ─────────────────────────────────────
# Read-only legend for a captured still's overlays (Phase C) — pure Julia, no viewer touched, so it can't
# disturb the live overlays. Returns, for the strip frame's populations + colour-by legend sections:
#  • colourBy: for a categorical `colourBy` column, {value → pop colour + pop name} (a value a population
#    FILTERS FOR on that column takes that pop's colour — the same rule as colour-labels/show-tracks; the
#    common case is track/cell clusters, which ARE populations, so the legend reads the cluster names).
#  • populations: for the requested point-pop layers ({valueName, popType, path}, parsed from the frame's
#    overlay layer names), each pop's name + colour from its population map.
function api_napari_overlay_legend(body_bytes::Vector{UInt8})
    data        = JSON3.read(String(body_bytes))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    column      = String(get(data, :colourBy, ""))
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err
    content = overlay_legend_content(img, column, get(data, :overlayPops, nothing),
                                     get(data, :colourOverrides, nothing))
    200, JSON3.write((; ok = true, colourBy = content.colourBy, populations = content.populations))
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

    # The viewer moved (pan/zoom/slider/2D-3D). Relayed to the frontend, which re-previews the region
    # now on screen. Handled BEFORE the image resolution below because it carries no image — the
    # viewer is reporting about itself, and requiring a projectUid it doesn't have would 404 it.
    if evt == "viewChanged"
        broadcast_ws(Dict{String,Any}("type" => "napari:view-changed"))
        return 200, JSON3.write((; ok = true))
    end

    # A recording reporting its frame count. Like viewChanged this carries no image — it is the bridge
    # talking about work the backend started — and it is what gives a single record a progress bar
    # (see run_single_movie). Throttled on the bridge side, not here.
    if evt == "recordProgress"
        task_id = String(get(data, :taskId, ""))
        isempty(task_id) || ws_progress(nothing, task_id,
                                        Int(get(data, :frame, 0)), max(Int(get(data, :total, 1)), 1))
        return 200, JSON3.write((; ok = true))
    end

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

# Newest mtime among the sources the napari bridge loads at startup — its own file plus the cecelia
# Python helpers it imports (napari/zarr/ome/dim/label-props utils). The bridge is a SEPARATE process,
# NOT Revise-tracked, so ANY edit to these after it started means it's running old code — the "stale
# bridge" that silently breaks new viewer features until you restart napari + reopen. mtime (not git)
# so it also catches UNCOMMITTED edits (the common dev case: edit napari code, restart the backend but
# not the bridge). Same machine → mtime and the bridge start time share a clock.
function _napari_src_mtime()
    root = dirname(dirname(@__DIR__))   # api/src → api → repo root
    newest = 0.0
    for d in (joinpath(root, "napari"), joinpath(root, "python", "cecelia", "utils"))
        isdir(d) || continue
        for (r, _, fs) in walkdir(d), f in fs
            endswith(f, ".py") || continue
            m = try; mtime(joinpath(r, f)); catch; 0.0; end
            m > newest && (newest = m)
        end
    end
    newest
end

function api_napari_status(req::HTTP.Request)
    # ping once (unlocked, like _viewer_alive — never blocks on a long op) and read the bridge's start
    # time from the reply, so the Settings panel can show bridge uptime and spot a STALE bridge (it
    # survives a backend restart). Same machine → same clock, so uptime is computed server-side.
    v = _viewer()
    alive = false
    bridge_started = nothing
    canvas_x = nothing
    canvas_y = nothing
    if v !== nothing
        try
            resp = send(v, Dict("type" => "ping"))
            alive = true
            bridge_started = get(resp, "started_at", nothing)
            # the size a movie records at when none is requested — the movie controls show it as their
            # placeholder, so the honest default is visible (docs/NAPARI.md)
            canvas_x = get(resp, "canvas_size_x", nothing)
            canvas_y = get(resp, "canvas_size_y", nothing)
        catch
        end
    end
    bridge_uptime = bridge_started === nothing ? nothing : round(Int, time() - Float64(bridge_started))
    # stale = a napari source was edited AFTER the bridge started (1s guard against same-second noise)
    bridge_stale = bridge_started !== nothing &&
                   (try; _napari_src_mtime() > Float64(bridge_started) + 1.0; catch; false; end)
    200, JSON3.write((; alive = alive, starting = _viewer_starting[],
                        bridgeStartedAt = bridge_started, bridgeUptimeSeconds = bridge_uptime,
                        bridgeStale = bridge_stale,
                        canvasSizeX = canvas_x, canvasSizeY = canvas_y))
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
