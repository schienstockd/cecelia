# ── WebSocket helpers ─────────────────────────────────────────────────────────

# Task events (log / status / progress / result) are keyed by taskId and BROADCAST to every connected
# client — not just the socket that launched the task. So a second GUI tab AND the read-only task
# console (api/task_console.jl) both get live progress; previously these went point-to-point to the
# launching socket, which is why the console (a separate client) showed chain runs — those already
# broadcast — but never a module task's progress. The `ws` arg is kept for call-site compatibility.
_broadcast_task(msg::NamedTuple) = broadcast_ws(Dict{String,Any}(String(k) => v for (k, v) in pairs(msg)))

ws_log(_ws, task_id, line)             = _broadcast_task((; type="task:log",      taskId=task_id, line=line))
# `image_uids` carries ALL images a task touched — for a set/combined task, `uid` is just the
# representative (first) member, so the frontend needs the full list to invalidate every member's plots
# (task-refresh; see docs/todo/TASK_DATA_REFRESH_PLAN.md). Defaults empty → single-image tasks fall back
# to `imageUid` on the frontend.
# `fun` carries the task fun_name so a WS observer (mcp/) can attribute a module-page run to a
# function for the 10-attempts pattern (chain nodes already carry `fn`; module tasks didn't).
# `pool` ATTRIBUTES the work to what governs it — scheduler tasks get theirs from the /api/tasks
# snapshot (cpu/gpu/io/network), but non-scheduler producers (batch movies, background jobs) never
# hit that snapshot, so they pass it here ("viewer" for the napari viewer, "job" for jobs.jl work).
# Without it those show a BLANK pool in the task console. NOT a real scheduler pool (no slot budget) —
# purely a label so they read as intentional instead of floating unattributed. The frontend ignores
# both extra fields.
#
# THE rail's status sink — and therefore the one place a terminal outcome is banked for replay
# (`record_task_outcome!`, `app/src/tasks/task_outcomes.jl`; a no-op for queued/running). This frame is
# the only carrier of "how did it end" and it is droppable, so without the bank a client that missed it
# could never find out: the task console reported a whole successful batch as "outcome unseen", and the
# browser left the task pinned at `running`. Banking HERE rather than in the scheduler is what makes that
# true for every producer — background jobs and batch movies never enter the scheduler's registry at all.
# Emit a terminal frame from anywhere and it is recoverable; don't add a second bank.
# Every status frame carries the task's TIMES, so a client shows a real elapsed instead of counting from
# when it happened to see the task. `running` also NOTES the start on the rail — that's what covers the
# producers with no `TaskRecord` (background jobs, batch movies): their only announcement comes through
# here, and `note_task_started!` is first-write-wins, so a scheduler task that already stamped it in
# `_set_status!` reads back its own (earlier, exact) value rather than being overwritten by this one.
# `finishedAt` comes from the banked outcome row so the live frame and the replayable row cannot disagree
# about when the task ended.
# `started_at`/`finished_at` are for a frame this process did NOT produce — the detached task runner
# relaying one it stamped itself. Empty (the normal case) means "derive them here", exactly as before.
# They exist because re-deriving a relayed task's times is the elapsed-timer bug one process boundary
# out: a task the runner has been running for twenty minutes would be stamped as starting when the
# relay first saw it, and a restarted API server would restart every timer on reconnect. Seeding
# `note_task_started!` with the runner's value is safe because it is first-write-wins.
function ws_status(_ws, task_id, status, uid=""; image_uids=String[], fun="", pool="",
                   started_at="", finished_at="")
    if string(status) == "running"
        seed = parse_iso_utc(started_at)
        isnothing(seed) ? note_task_started!(task_id) : note_task_started!(task_id, seed)
    end
    row = record_task_outcome!(task_id, status;
                               image_uid=uid, image_uids=image_uids, fun=fun, pool=pool,
                               started_at=started_at, finished_at=finished_at)
    _broadcast_task((; type="task:status", taskId=task_id, status=status, imageUid=uid,
                       imageUids=image_uids, fun=fun, pool=pool,
                       startedAt  = isnothing(row) ? iso_utc(task_started_at(task_id)) : row.started_at,
                       finishedAt = isnothing(row) ? "" : row.finished_at))
end
ws_result(_ws, task_id, uid, meta)     = _broadcast_task((; type="task:result",    taskId=task_id, imageUid=uid, meta=meta))

ws_progress(_ws, task_id, fraction::Float64) =
    _broadcast_task((; type="task:progress", taskId=task_id, progress=clamp(fraction, 0.0, 1.0)))
ws_progress(ws, task_id, n::Int, total::Int) =
    ws_progress(ws, task_id, total > 0 ? n / total : 0.0)

# ── WS message dispatch ───────────────────────────────────────────────────────

function _to_str_dict(params)::Dict{String,Any}
    isnothing(params) && return Dict{String,Any}()
    params isa AbstractDict && return Dict{String,Any}(String(k) => v for (k, v) in params)
    Dict{String,Any}()
end

function handle_message(ws, raw::AbstractString)
    data = JSON3.read(raw)
    type = get(data, :type, "")

    if type == "ping"
        HTTP.WebSockets.send(ws, JSON3.write((; type="pong")))
    elseif type == "task:run" || type == "task:restart"
        handle_task_run(ws, data)
    elseif type == "task:cancel"
        task_id = String(get(data, :taskId, ""))
        # Also reach the non-scheduler producers that emit task:* frames under this id but aren't in the
        # scheduler's _TASKS: recordings, single and batch (request_batch_cancel!, which flags the run AND
        # tells the bridge to stop the frame loop it is in), and background jobs (cancel_job!, kills the
        # subprocess(es) — data patches + project export/import).
        # So the Task-Manager Cancel button works on all of them, not just scheduler tasks.
        # …and the detached runner, where a scheduler task most likely actually IS. All of these are
        # no-ops for an unknown id, so asking all four is free; asking fewer is a Cancel button that
        # silently does nothing depending on where the task happens to be running.
        isempty(task_id) || (cancel_task!(task_id); request_batch_cancel!(task_id);
                             cancel_job!(task_id); _cancel_on_runner(task_id))
    elseif type == "movie:batch"
        handle_movie_batch(ws, data)
    elseif type == "movie:record"
        handle_movie_record(ws, data)
    elseif type == "chain:run"
        handle_chain_run(ws, data)
    elseif type == "chain:cancel"
        run_id = String(get(data, :runId, ""))
        # Both processes: the run may be executing here (fallback) or on the runner. Each is a no-op
        # for an id it does not know, so asking both is free — asking one is a Cancel that silently
        # does nothing depending on where the run happens to be.
        isempty(run_id) || (cancel_chain_run!(run_id); _cancel_chain_on_runner(run_id))
    elseif type == "maintenance:run"
        handle_maintenance_run(ws, data)
    elseif type == "maintenance:cancel"
        task_id = String(get(data, :taskId, ""))
        isempty(task_id) || cancel_maintenance!(task_id)
    elseif type == "project:export"
        handle_project_export(ws, data)
    elseif type == "project:import"
        handle_project_import(ws, data)
    else
        @warn "Unknown WS message type" type
    end
end

# Project Manager Export / Import — background jobs (jobs.jl / project_io.jl) that stream over the task
# rail like a data patch, cancellable via task:cancel → cancel_job!. NEITHER needs an open project:
# export reads a project dir off disk by uid; import creates a new one. See docs/JOBS.md.
function handle_project_export(ws, data)
    task_id     = String(get(data, :taskId, ""))
    project_uid = String(get(data, :projectUid, ""))
    out_dir     = String(get(data, :outDir, ""))
    isempty(task_id) && return
    if isempty(project_uid) || !isdir(joinpath(projects_dir(), project_uid))
        ws_log(ws, task_id, "[ERROR] Project not found: $project_uid")
        ws_status(ws, task_id, "failed"; fun="project:export", pool="job"); return
    end
    Threads.@spawn begin
        ws_status(ws, task_id, "running"; fun="project:export", pool="job")
        bundle = try
            export_project(project_uid;
                out_dir     = isempty(out_dir) ? default_export_dir() : out_dir,
                task_id     = task_id,
                on_log      = line -> ws_log(ws, task_id, line),
                on_progress = (n, t) -> ws_progress(ws, task_id, n, t))
        catch ex
            ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex)); ""
        end
        ws_status(ws, task_id, isempty(bundle) ? "failed" : "done"; fun="project:export", pool="job")
    end
end

function handle_project_import(ws, data)
    task_id = String(get(data, :taskId, ""))
    bundle  = String(get(data, :bundle, ""))
    mode    = String(get(data, :mode, "error"))   # error (default) | replace | copy — on uid collision
    isempty(task_id) && return
    if isempty(bundle) || !isdir(bundle)
        ws_log(ws, task_id, "[ERROR] Bundle not found: $bundle")
        ws_status(ws, task_id, "failed"; fun="project:import", pool="job"); return
    end
    Threads.@spawn begin
        ws_status(ws, task_id, "running"; fun="project:import", pool="job")
        uid = try
            import_project(bundle;
                mode        = mode,
                task_id     = task_id,
                on_log      = line -> ws_log(ws, task_id, line),
                on_progress = (n, t) -> ws_progress(ws, task_id, n, t))
        catch ex
            ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex)); ""
        end
        ws_status(ws, task_id, isempty(uid) ? "failed" : "done"; fun="project:import", pool="job")
    end
end

# Data patches (project-scoped maintenance, e.g. the centroid-axis converter). Runs the patch's Python
# via `run_maintenance_patch` and streams over the task rail (task:log/progress/status keyed by taskId)
# so it shows live progress + a working Cancel (maintenance:cancel → cancel_maintenance!), like an HPC
# task spin-off. Confined to the ONE project the payload names. See docs/DEV.md → "Data patches".
function handle_maintenance_run(ws, data)
    task_id     = String(get(data, :taskId, ""))
    patch_id    = String(get(data, :patchId, ""))
    project_uid = String(get(data, :projectUid, ""))
    apply       = Bool(get(data, :apply, false))
    fun_lbl     = isempty(patch_id) ? "maintenance" : "maintenance:$patch_id"   # task-console attribution

    patch = maintenance_patch(patch_id)
    if isnothing(patch)
        ws_log(ws, task_id, "[ERROR] Unknown data patch: $patch_id")
        ws_status(ws, task_id, "failed"; fun=fun_lbl, pool="job"); return
    end
    if !isdir(joinpath(projects_dir(), project_uid))
        ws_log(ws, task_id, "[ERROR] Project not found: $project_uid")
        ws_status(ws, task_id, "failed"; fun=fun_lbl, pool="job"); return
    end

    Threads.@spawn begin
        ws_status(ws, task_id, "running"; fun=fun_lbl, pool="job")
        ok = try
            proj = load_project(project_uid)
            run_maintenance_patch(patch, proj; apply = apply, task_id = task_id,
                                  on_log      = line -> ws_log(ws, task_id, line),
                                  on_progress = (n, t) -> ws_progress(ws, task_id, n, t))
        catch ex
            ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex)); false
        end
        ws_status(ws, task_id, ok ? "done" : "failed"; fun=fun_lbl, pool="job")
    end
end

# A SINGLE recording (the viewer's Record button, or the animation page's render) on the same rail as
# the batch below: async, `task:progress` per frame, `task:status`/`task:result`, and a Cancel that works.
# It was a blocking POST until the bridge learned to report progress and take a cancel mid-render — see
# `run_single_movie`. `keyframes` present ⇒ the interpolated animation, absent ⇒ the open image's T-sweep.
function handle_movie_record(ws, data)
    task_id     = String(get(data, :taskId, ""))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    isempty(task_id) && return
    fps         = Int(get(data, :fps, 15))
    size_x, size_y = _movie_size_params(data)   # blank = the napari canvas size (napari_api.jl)
    suffix      = String(get(data, :suffix, ""))
    api_url     = String(get(data, :apiUrl, "http://localhost:8080"))
    kf_raw      = get(data, :keyframes, nothing)
    keyframes   = (kf_raw === nothing || length(kf_raw) == 0) ? nothing : kf_raw
    fun         = keyframes === nothing ? "movie:record" : "movie:animation"
    tc          = get(data, :titleCard, nothing)
    card        = (tc isa AbstractDict && Bool(get(tc, :enabled, false))) ? tc : nothing
    # Side-by-side comparison (docs/todo/MOVIE_COMPARE_PLAN.md): versions across the columns, masks
    # down the rows, one pass per cell into a single file. A single cell is the plain record it was.
    vns_raw     = get(data, :valueNames, nothing)
    value_names = vns_raw === nothing ? String[] : collect(String, vns_raw)
    # The segmentation masks to show. ABSENT and EMPTY differ: absent leaves the canvas alone (what a
    # plain "record what's on screen" wants), empty is an explicit no-masks. 2+ also makes them the
    # grid's ROWS. See `_config_label_value_names`.
    lvns_raw    = get(data, :labelValueNames, nothing)
    label_vns   = lvns_raw === nothing ? nothing : collect(String, lvns_raw)
    # skeletons (`segment.branching`) — a separate registry and a separate picker, same three-valued
    # contract; the batch sends neither, so its skeletons stay untouched
    bvns_raw    = get(data, :branchValueNames, nothing)
    branch_vns  = bvns_raw === nothing ? nothing : collect(String, bvns_raw)
    contour     = _label_contour(data)          # mask outline width, 0 = filled
    # which stretch of the timelapse to sweep (frame indices; end `nothing` = the last frame). An
    # animation ignores it — its keyframes carry their own dims, so the timeline IS the range.
    t_start, t_end = _t_range(data)
    show_3d     = _show_3d(data)                # whole z stack as a 3D render…
    z_slice     = _z_slice(data)                # …or one slice in 2D (nothing = whatever is showing)
    share_ctr   = _share_contrast(get(data, :compareContrast, ""))
    layout      = String(get(data, :compareLayout, "row"))
    # napari's baked overlays, burnt into every frame. Default true = what every movie was.
    show_ts     = Bool(get(data, :showTimestamp, true))
    show_sb     = Bool(get(data, :showScaleBar, true))
    if isempty(image_uid)
        ws_log(ws, task_id, "[ERROR] no image to record")
        ws_status(ws, task_id, "failed", ""; fun=fun, pool="viewer")
        return
    end
    if keyframes !== nothing && length(keyframes) < 2
        ws_log(ws, task_id, "[ERROR] an animation needs at least 2 keyframes")
        ws_status(ws, task_id, "failed", image_uid; fun=fun, pool="viewer")
        return
    end
    # What produced this movie, banked into settings/movies.json once the bytes land (Phase 4 of
    # docs/todo/MOVIE_MANAGEMENT_PLAN.md). Assembled HERE rather than inside the recorder, so the
    # recorder keeps knowing nothing about the request shape. `look` is the viewer's live channels +
    # overlays, seeded frontend-side from the view state it already reads for the title card; the
    # keyframes ARE the config for an animation.
    #
    # `imageUid` and `keyframeMeta` are here for the EDIT side (Phase 6, utils/movieRestore.ts). A movie
    # is named after its image but nothing could turn that name back into a uid, so which image a look
    # was recorded on was simply unrecoverable. And `keyframes` is the RENDER payload — `{viewState,
    # steps}` — which is everything the recorder needs and none of what the timeline editor needs, so
    # the thumbnail/title/seconds ride along in a parallel array rather than doubling every view state.
    movie_config = Dict{String,Any}(
        "imageUid" => image_uid, "keyframeMeta" => get(data, :keyframeMeta, nothing),
        "fps" => fps, "sizeX" => size_x, "sizeY" => size_y, "suffix" => suffix,
        "titleCard" => tc, "valueNames" => value_names, "labelValueNames" => label_vns,
        "branchValueNames" => branch_vns, "labelContour" => contour,
        "show3D" => show_3d, "zSlice" => z_slice, "tStart" => t_start, "tEnd" => t_end,
        "compareLayout" => layout, "compareContrast" => get(data, :compareContrast, ""),
        "showTimestamp" => show_ts, "showScaleBar" => show_sb,
        "look" => get(data, :look, nothing), "keyframes" => keyframes)
    _batch_register!(task_id)
    @async try
        run_single_movie(task_id, project_uid, image_uid; fps = fps, size_x = size_x, size_y = size_y,
                         suffix = suffix, title_card = card, keyframes = keyframes,
                         movie_config = movie_config,
                         value_names = value_names, label_value_names = label_vns,
                         branch_value_names = branch_vns,
                         label_contour = contour, show_3d = show_3d, z_slice = z_slice,
                         t_start = t_start, t_end = t_end,
                         share_contrast = share_ctr, layout = layout,
                         show_timestamp = show_ts, show_scale_bar = show_sb, api_url = api_url)
    catch e
        @warn "movie record crashed" exception = e
        ws_log(ws, task_id, "[ERROR] record crashed: $(sprint(showerror, e))")
        ws_status(ws, task_id, "failed", image_uid; fun=fun, pool="viewer")
        _batch_clear!(task_id)
    end
end

# F1.3 batch movies: apply one authored config across the selected images → one attr-named mp4 each,
# recorded on the single shared napari viewer. Runs async (recording is minutes-long) and reports over
# the normal task events (task:progress/log/status/result) keyed by the client's taskId, so it appears
# in the task list with a progress bar + a working Cancel (see request_batch_cancel!). Orchestrated in
# api/ (napari_api.jl) because the viewer + its lock live there; not a scheduler task (it's UI-serial,
# not pooled headless compute). See docs/todo/ANIMATION_PLAN.md → F1.3.
function handle_movie_batch(ws, data)
    task_id     = String(get(data, :taskId, ""))
    project_uid = String(get(data, :projectUid, ""))
    isempty(task_id) && return
    uids_raw    = get(data, :imageUids, nothing)
    image_uids  = uids_raw === nothing ? String[] : collect(String, uids_raw)
    config      = get(data, :config, Dict{String,Any}())
    attrs_raw   = get(data, :fileAttrs, nothing)
    file_attrs  = attrs_raw === nothing ? String[] : collect(String, attrs_raw)
    fps         = Int(get(data, :fps, 15))
    size_x, size_y = _movie_size_params(data)   # blank = the napari canvas size (napari_api.jl)
    suffix      = String(get(data, :suffix, ""))
    if isempty(image_uids)
        ws_log(ws, task_id, "[ERROR] no images selected for batch movies")
        ws_status(ws, task_id, "failed", ""; fun="movie:batch", pool="viewer")
        return
    end
    # The authored config IS the batch's provenance — one config, one movie per image (Phase 4).
    # `imageUids` is the whole selection, banked on EVERY movie in the batch: the edit side reopens the
    # authoring page, and the run it is reproducing was over all of them, not just the one row clicked.
    movie_config = Dict{String,Any}("config" => config, "fileAttrs" => file_attrs, "fps" => fps,
                                    "sizeX" => size_x, "sizeY" => size_y, "suffix" => suffix,
                                    "imageUids" => image_uids)
    _batch_register!(task_id)
    @async try
        run_batch_movies(task_id, project_uid, image_uids, config, file_attrs, fps;
                         size_x = size_x, size_y = size_y, suffix = suffix,
                         movie_config = movie_config)
    catch e
        @warn "batch movies crashed" exception = e
        ws_log(ws, task_id, "[ERROR] batch crashed: $(sprint(showerror, e))")
        ws_status(ws, task_id, "failed", first(image_uids); fun="movie:batch", pool="viewer")
        _batch_clear!(task_id)
    end
end

# Drop images the user has excluded from further processing (CciaImage.included == false) before a
# run. This is the backend half of the include/exclude feature — the GUI already makes excluded
# images unselectable, so this is belt-and-suspenders for run paths that bypass the checkboxes (chain
# resume from disk, the REPL, a stale selection). Returns the included uids; `on_skip(uid)` is called
# for each dropped one so the caller can log it (nothing is ever silently dropped). Images that fail
# to load are kept — let the downstream loader report the real error.
function _drop_excluded(project_uid::String, uids::Vector{String}, on_skip::Function)
    keep = String[]
    for u in uids
        obj = try init_object(project_uid, u) catch; nothing end
        if obj isa CciaImage && !image_included(obj)
            on_skip(u)
        else
            push!(keep, u)
        end
    end
    keep
end

function handle_chain_run(ws, data)
    project_uid = String(get(data, :projectUid, ""))
    chain_name  = String(get(data, :chain, ""))
    image_uids  = String[String(u) for u in get(data, :imageUids, [])]
    # Resume: a `runId` re-runs a persisted run (restore from disk, re-do failed/incomplete/changed
    # nodes). An optional `startNode` force-restarts that node + everything downstream ("resume from
    # here"). When resuming, `chain`/`imageUids` are read from the run, so they're not required.
    run_id      = String(get(data, :runId, ""))
    start_node  = String(get(data, :startNode, ""))
    resuming    = !isempty(run_id)

    if isempty(project_uid) || (!resuming && isempty(chain_name))
        HTTP.WebSockets.send(ws, JSON3.write((; type="chain:run:failed",
                                               error="projectUid and chain are required")))
        return
    end
    if !resuming && isempty(image_uids)
        HTTP.WebSockets.send(ws, JSON3.write((; type="chain:run:failed",
                                               error="No images selected")))
        return
    end
    proj_dir = joinpath(projects_dir(), project_uid)
    if !isdir(proj_dir)
        HTTP.WebSockets.send(ws, JSON3.write((; type="chain:run:failed",
                                               error="Project not found: $project_uid")))
        return
    end

    # NOTE: no `load_project` here any more. The EXECUTING process loads it (`execute_chain`), which is
    # the whole point — the runner resolves the project against its own `projects_dir()`. Loading it
    # here too was a wasted read of every ccid.json in the project on the dispatch path.

    # Hard-skip excluded images (belt-and-suspenders — the GUI already blocks selecting them).
    if !resuming
        image_uids = _drop_excluded(project_uid, image_uids, u ->
            broadcast_ws(Dict{String,Any}("type" => "chain:log",
                                          "line" => "[INFO] Skipping excluded image $u")))
        if isempty(image_uids)
            HTTP.WebSockets.send(ws, JSON3.write((; type="chain:run:failed",
                                                   error="All selected images are excluded")))
            return
        end
    end

    HTTP.WebSockets.send(ws, JSON3.write((; type="chain:run:started",
                                           chain=chain_name,
                                           runId=(resuming ? run_id : nothing),
                                           imageCount=length(image_uids))))

    # Hand it to the detached runner if there is one. A REFUSAL is not a fallback: it means that run id
    # is already executing there, and starting a second execution would have two processes writing the
    # same `run.json`. That is the corruption the runner's claim exists to prevent, so we stop instead.
    creq = ChainRequest(; project_uid, chain_name, image_uids, run_id, start_node)
    outcome = _submit_chain_to_runner(creq)
    outcome === :accepted && return
    if outcome === :refused
        broadcast_ws(Dict{String,Any}("type" => "chain:run:failed", "chain" => chain_name,
                                      "error" => "This run is already executing on the task runner."))
        return
    end

    Threads.@spawn execute_chain(creq;
        on_log = line -> begin
            println(line)
            broadcast_ws(Dict{String,Any}("type" => "chain:log", "line" => line))
        end,
        on_finished = (ok, err) -> broadcast_ws(Dict{String,Any}(
            "type"  => ok ? "chain:run:done" : "chain:run:failed",
            "chain" => chain_name, "error" => err)))
end

# Persist last-used params to each image dir + the set dir ({proj}/1/{uid}/ccid.json → meta.funParams).
# Dir-based (no object load) — see write_module_fun_params! in app/src/model/image.jl.
#
# Also banked under the OUTPUT NAME this run wrote (`Cecelia.task_output_name`, which reads the spec's
# `namespace` — the name lives under six different keys depending on the task). That is what lets the
# form restore Tcell's parameters when you pick Tcell, instead of showing whatever ran last. `""` for a
# task that names no output, and then only the flat blob is written, exactly as before.
function _remember_fun_params(proj_root::String, fun::String, params::Dict{String,Any},
                              image_uid::String, image_uids::Vector{String}, set_uid::String)
    uids = !isempty(image_uids) ? image_uids : (isempty(image_uid) ? String[] : [image_uid])
    try
        vn = Cecelia.task_output_name(fun, params)
        for u in uids
            write_module_fun_params!(joinpath(proj_root, "1", u), fun, params; value_name = vn)
        end
        isempty(set_uid) ||
            write_module_fun_params!(joinpath(proj_root, "1", set_uid), fun, params; value_name = vn)
    catch ex
        @warn "Could not persist funParams" fun exception=ex   # best-effort; never block the run
    end
end

function handle_task_run(ws, data)
    task_id     = String(get(data, :taskId, ""))
    fun_name    = String(get(data, :funName, ""))
    project_uid = String(get(data, :projectUid, ""))
    image_uid   = String(get(data, :imageUid, ""))
    image_uids  = String[String(u) for u in get(data, :imageUids, [])]
    set_uid     = String(get(data, :setUid, ""))
    pool_name   = String(get(data, :poolName, ""))
    params      = _to_str_dict(get(data, :params, nothing))

    proj_root = joinpath(projects_dir(), project_uid)
    if !isdir(proj_root)
        ws_log(ws, task_id, "[ERROR] Project not found: $project_uid")
        ws_status(ws, task_id, "failed"; fun=fun_name)
        return
    end

    # Hard-skip excluded images before dispatch (belt-and-suspenders; the GUI already blocks them).
    _skip(u) = ws_log(ws, task_id, "[INFO] Skipping excluded image $u")
    isempty(image_uids) || (image_uids = _drop_excluded(project_uid, image_uids, _skip))
    if !isempty(image_uid) && isempty(_drop_excluded(project_uid, [image_uid], _skip))
        image_uid = ""
    end
    if isempty(image_uids) && isempty(image_uid)
        ws_log(ws, task_id, "[ERROR] No images to run (all selected images are excluded).")
        ws_status(ws, task_id, "failed"; fun=fun_name)
        return
    end

    # Remember the params for this run (R parity: saveModuleFunParams). Persist to each processed
    # image (a record of what params produced it) and to the set (the shared last-used default) so
    # the module-page form is pre-populated next time (image → set → task-defaults). Done here, at
    # dispatch, so it sticks regardless of run outcome — like the old taskManager did at launch.
    _remember_fun_params(proj_root, fun_name, params, image_uid, image_uids, set_uid)

    # Everything above is the ASKING side — guards and project-state edits this process owns. The run
    # itself is `execute_task` (app/src/runner/execute.jl), which knows nothing about sockets: scope
    # dispatch, the pre-job throw guard, and the result→terminal-status ordering all live there, so the
    # detached runner executes the identical code rather than a second copy of it.
    # See docs/todo/TASK_RUNNER_PLAN.md (Decision 1).
    req = TaskRequest(; task_id, fun_name, project_uid, image_uid, image_uids, pool_name, params)

    # Run it in THIS process — the fallback whenever no runner takes it. Dies with this server.
    run_in_process() = execute_task(req;
        on_log      = line -> ws_log(ws, task_id, line),
        on_progress = (n, t) -> ws_progress(ws, task_id, n, t),
        on_status   = (status, uid, uids) ->
                          ws_status(ws, task_id, status, uid; image_uids=uids, fun=fun_name),
        on_result   = (uid, meta) -> ws_result(ws, task_id, uid, meta))

    # Hand it to the detached runner if there is one — then this server can restart without taking the
    # run with it, and its frames arrive back through the relay into these same sinks.
    #
    # `:unavailable` — the runner is ENABLED but nothing answered — is the interesting case, and it used
    # to fall straight through to in-process. Silently: a runner that died mid-session then turned every
    # later run into one that dies with the next restart, which is the single thing enabling it was
    # meant to prevent, and the only tell was a 20-second-polled label on the Run panel. So we bring it
    # back instead, and only fall back if we cannot.
    #
    # That relaunch is a COLD START (~45 s), and `handle_message` runs INLINE on the WS receive loop —
    # blocking here would stall this client's entire stream, pings included, and on a set it would do so
    # once per image. Hence the spawn. The task's row stays `queued` meanwhile, which is exactly true.
    st = _submit_to_runner(req)
    st === :accepted && return
    if st === :unavailable && _runner_enabled()
        Threads.@spawn begin
            (_ensure_runner!() && _submit_to_runner(req) === :accepted) || run_in_process()
        end
        return
    end
    # `:refused` (a live runner said no) or the runner is switched off — neither is worth a relaunch.
    Threads.@spawn run_in_process()
end

