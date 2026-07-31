# ── Task preview — the API layer ───────────────────────────────────────────────
#
# Routes for the resident preview worker (`preview/preview_worker.py`, :7656). Deliberately shaped
# like `napari_api.jl`'s viewer lifecycle — one adopted-or-launched process behind a reentrant lock,
# an async launch with a `starting` flag, a status route — because "a resident Python process we talk
# to over WS" already has one way to be done here.
#
# The one rule this file exists to enforce: **a preview never guesses which image it is looking at.**
# `napari_api` tracks the open image but used to expose nothing, so an out-of-band caller had to be
# told — and during this feature's development, guessing wrong three times wrote scratch stores into
# images the user was not looking at. `api_preview_run` reads the open image from
# `current_napari_image()` and REFUSES a request that names a different one, rather than trusting the
# client. See docs/todo/TASK_PREVIEW_PLAN.md.

const _preview_ref      = Ref{Union{PreviewWorker,Nothing}}(nothing)
const _preview_starting = Ref(false)
const _preview_lock     = ReentrantLock()

# Serialise all interaction with the single worker, for the same reason as `_with_viewer`: under
# `-t auto` two concurrent previews would interleave on one process, and the debounced re-preview
# (#19) makes overlapping requests the normal case rather than an edge case.
_with_preview(f) = lock(f, _preview_lock)

_preview()::Union{PreviewWorker,Nothing} = _preview_ref[]

function _preview_worker_alive()::Bool
    w = _preview_ref[]
    w === nothing && return false
    try
        send(w, Dict("type" => "ping"))
        true
    catch
        false
    end
end

"""
Launch the worker if it isn't up. Returns true when it is ready NOW, false when a launch is in
flight — the caller reports `starting` rather than blocking, because the worker pays 17.7 s of torch
and cellpose imports before it can answer (that cost is the whole reason it is resident).

Adopts a worker already listening on the port, like `_ensure_viewer!` — one that survived a backend
restart is still perfectly good, and a second process on the port would just fail to bind.
"""
function _ensure_preview!()::Bool
    lock(_preview_lock) do
        _preview_worker_alive() && return true
        _preview_starting[] && return false
        if _preview_ref[] === nothing
            probe = PreviewWorker()
            try
                send(probe, Dict("type" => "ping"))
                _preview_ref[] = probe
                @info "Adopted existing preview worker on port $(probe.port)"
                return true
            catch
                # none running — launch one
            end
        end
        @info "Launching preview worker..."
        w = PreviewWorker()
        _preview_ref[] = w
        _preview_starting[] = true
        @async begin
            try
                launch!(w)
            catch e
                @error "Preview worker failed to start" exception = e
                lock(_preview_lock) do; _preview_ref[] = nothing; end
            finally
                lock(_preview_lock) do; _preview_starting[] = false; end
            end
        end
        false
    end
end

# ── GET /api/preview/status ───────────────────────────────────────────────────
# The route that answers "what is the viewer looking at?" — previously knowable only by guessing.
# Also reports the worker's state so the toggle can show starting/ready without a second call.
function api_preview_status(req::HTTP.Request)
    open_image = current_napari_image()
    200, JSON3.write((;
        alive    = _preview_worker_alive(),
        starting = _preview_starting[],
        port     = PREVIEW_PORT,
        # what the VIEWER has open. null everywhere until an image is opened — a client seeing nulls
        # must prompt the user to open an image, never fall back to a guess.
        imageUid = open_image.imageUid,
        zarrPath = open_image.zarrPath,
        taskDir  = open_image.taskDir,
    ))
end

# ── POST /api/preview/start ───────────────────────────────────────────────────
# Warm the worker without previewing anything, so the import cost is paid at toggle-on rather than on
# the user's first parameter change.
function api_preview_start(body_bytes::Vector{UInt8})
    ready = _ensure_preview!()
    200, JSON3.write((; alive = ready, starting = _preview_starting[], port = PREVIEW_PORT))
end

# ── POST /api/preview/stop ────────────────────────────────────────────────────
# Toggle-off: remove the layer, then stop the worker. Stopping is the ONLY thing that releases the
# VRAM a warm cellpose model holds, which is why this is a real user action and not just cleanup.
function api_preview_stop(body_bytes::Vector{UInt8})
    data = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch; Dict{String,Any}(); end
    value_name = String(get(data, "valueName", VERSIONED_DEFAULT_VAL))

    # hide first: if stopping the worker throws, the user is not left looking at a stale mask
    _with_viewer() do
        v = _viewer()
        v === nothing || try
            hide_task_preview!(v; value_name = value_name)
        catch e
            @warn "Could not remove preview layer" exception = e
        end
    end
    lock(_preview_lock) do
        w = _preview_ref[]
        w === nothing || try; close!(w); catch e
            @warn "Could not stop preview worker" exception = e
        end
        _preview_ref[]      = nothing
        _preview_starting[] = false
    end
    200, JSON3.write((; alive = false, stopped = true))
end

# ── POST /api/preview/run ─────────────────────────────────────────────────────
# Body: `{ projectUid, imageUid, valueName, params }`. Runs the task's real compute over the region
# the viewer is looking at and shows the result as an in-memory labels layer.
#
# `imageUid`/`valueName` are CHECKED against the open image, not used to pick one. A mismatch is a 409
# with the version to open, because the alternatives are both silently wrong: previewing the client's
# choice acts on an image the user isn't looking at, and previewing the open one shows a result the
# configured run would not produce. The region is read from the same open layer, so pixels and region
# can never come from differently-shaped versions of the image.
function api_preview_run(body_bytes::Vector{UInt8})
    data = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch
        return 400, JSON3.write((; error = "invalid JSON body")); end

    project_uid = String(get(data, "projectUid", ""))
    image_uid   = String(get(data, "imageUid", ""))
    value_name  = String(get(data, "valueName", VERSIONED_DEFAULT_VAL))
    params      = get(data, "params", nothing)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error = "imageUid required"))
    params isa AbstractDict || return 400, JSON3.write((; error = "params required"))

    open_image = current_napari_image()
    isnothing(open_image.imageUid) &&
        return 409, JSON3.write((; error = "No image open in the viewer. Open the image to preview it.",
                                   code = "no-image-open"))
    open_image.imageUid == image_uid ||
        return 409, JSON3.write((; error = "The viewer has a different image open. Open this image to preview it.",
                                   code = "image-mismatch", openImageUid = open_image.imageUid))
    (isnothing(open_image.zarrPath) || isnothing(open_image.taskDir)) &&
        return 409, JSON3.write((; error = "The viewer has no image version resolved yet.",
                                   code = "no-image-open"))

    # Which version would the RUN read? If that isn't what's on screen, previewing either the wrong
    # pixels or the wrong region — refuse and say which version to open.
    in_value_name = String(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    proj_dir  = joinpath(projects_dir(), project_uid)
    meta_file = state_file(proj_dir, image_uid)
    isfile(meta_file) || return 404, JSON3.write((; error = "Image metadata not found: $image_uid"))
    filename = versioned_get_field(read_ccid_raw(meta_file), "filepath", in_value_name)
    isnothing(filename) &&
        return 404, JSON3.write((; error = "No filepath registered (valueName=$in_value_name). Run a conversion task first."))
    wanted = joinpath(proj_dir, "0", image_uid, string(filename))
    _same_store(wanted, open_image.zarrPath) ||
        return 409, JSON3.write((;
            error = "The viewer is showing a different image version than this task reads. " *
                    "Open '$in_value_name' to preview it.",
            code = "version-mismatch",
            wantedValueName = in_value_name,
            openZarr = basename(String(open_image.zarrPath))))

    ready = _ensure_preview!()
    ready || return 202, JSON3.write((; starting = true, alive = false,
                                        message = "Preview worker is starting."))

    region = try
        _with_viewer() do
            v = _viewer()
            v === nothing && error("napari is not running")
            preview_region(v)
        end
    catch e
        return 409, JSON3.write((; error = sprint(showerror, e), code = "no-region"))
    end

    # Params as the TASK's Python side needs them — cellpose resolves channel names to indices and a
    # custom model to its checkpoint path. Skipping this sent "CH3" where an int was expected. The
    # translation dispatches on the task, so it cannot be guessed at here; a missing custom checkpoint
    # raises with a message worth showing.
    task = try
        Cecelia._task_from_fun_name(String(get(data, "funName", "")))
    catch
        nothing
    end
    if task !== nothing
        img_for_params = try
            init_object(project_uid, image_uid)      # already validated against the open image above
        catch e
            return 500, JSON3.write((; error = "could not load image metadata: " * sprint(showerror, e)))
        end
        params = try
            preview_params(task, params, img_for_params)
        catch e
            return 400, JSON3.write((;
                error = e isa ErrorException ? e.msg : sprint(showerror, e),
                code = "params-not-previewable"))
        end
    end

    reply = try
        _with_preview() do
            w = _preview()
            w === nothing && error("preview worker is not running")
            send(w, preview_request(open_image.zarrPath, open_image.taskDir, params, region;
                                    value_name = value_name))
        end
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end

    try
        _with_viewer() do
            # api_url makes the viewer report view changes back (→ `viewChanged` → WS → re-preview),
            # which is what stops a preview going stale the moment you scroll
            show_task_preview!(_viewer(), reply; value_name = value_name,
                               api_url = String(get(data, "apiUrl", "http://localhost:8080")))
        end
    catch e
        return 500, JSON3.write((; error = "preview computed but could not be shown: " *
                                           sprint(showerror, e)))
    end

    200, JSON3.write((;
        counts     = get(reply, "counts", Dict{String,Any}()),
        region     = get(reply, "region", Dict{String,Any}()),
        fallback2d = Bool(get(reply, "fallback2d", false)),
        # lets the UI tell "your parameters found nothing" from "there is nothing here" — a padded
        # plane returns 0 cells and otherwise looks exactly like a bad diameter
        hasSignal   = Bool(get(reply, "hasSignal", true)),
        noSignalWhy = String(get(reply, "noSignalWhy", "")),
        # tile seams the RUN would place inside this region; the preview segments it as one tile
        runSeams    = get(reply, "runSeams", Dict{String,Any}()),
        blockSize   = get(reply, "blockSize", 0),
        valueName  = value_name,
    ))
end

# Two paths naming the same store. Compared with the staging suffix stripped, because a RUNNING
# segmentation has the viewer on `X.ome.zarr.partial` while `ccid.json` still resolves `X.ome.zarr` —
# refusing then would be wrong, it is the same store mid-write.
function _same_store(a::AbstractString, b::AbstractString)::Bool
    suffix = Cecelia.STORE_STAGING_SUFFIX      # not exported; the one spelling of `.partial`
    strip_suffix(p) = endswith(p, suffix) ? p[1:end-length(suffix)] : String(p)
    normpath(strip_suffix(String(a))) == normpath(strip_suffix(String(b)))
end
