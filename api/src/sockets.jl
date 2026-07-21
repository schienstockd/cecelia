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
# function for the 10-attempts pattern (chain nodes already carry `fn`; module tasks didn't). Empty
# for non-task status frames (batch movies). The frontend ignores the extra field.
ws_status(_ws, task_id, status, uid=""; image_uids=String[], fun="") = _broadcast_task((; type="task:status", taskId=task_id, status=status, imageUid=uid, imageUids=image_uids, fun=fun))
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
        # scheduler's _TASKS: batch movies (request_batch_cancel!, stops after the current image) and
        # data patches (cancel_maintenance!, kills the subprocess). So the Task-Manager Cancel button
        # works on all three, not just scheduler tasks.
        isempty(task_id) || (cancel_task!(task_id); request_batch_cancel!(task_id); cancel_maintenance!(task_id))
    elseif type == "movie:batch"
        handle_movie_batch(ws, data)
    elseif type == "chain:run"
        handle_chain_run(ws, data)
    elseif type == "chain:cancel"
        run_id = String(get(data, :runId, ""))
        isempty(run_id) || cancel_chain_run!(run_id)
    elseif type == "maintenance:run"
        handle_maintenance_run(ws, data)
    elseif type == "maintenance:cancel"
        task_id = String(get(data, :taskId, ""))
        isempty(task_id) || cancel_maintenance!(task_id)
    else
        @warn "Unknown WS message type" type
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

    patch = maintenance_patch(patch_id)
    if isnothing(patch)
        ws_log(ws, task_id, "[ERROR] Unknown data patch: $patch_id")
        ws_status(ws, task_id, "failed"); return
    end
    if !isdir(joinpath(projects_dir(), project_uid))
        ws_log(ws, task_id, "[ERROR] Project not found: $project_uid")
        ws_status(ws, task_id, "failed"); return
    end

    Threads.@spawn begin
        ws_status(ws, task_id, "running")
        ok = try
            proj = load_project(project_uid)
            run_maintenance_patch(patch, proj; apply = apply, task_id = task_id,
                                  on_log      = line -> ws_log(ws, task_id, line),
                                  on_progress = (n, t) -> ws_progress(ws, task_id, n, t))
        catch ex
            ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex)); false
        end
        ws_status(ws, task_id, ok ? "done" : "failed")
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
    scale       = get(data, :scale, 1)
    if isempty(image_uids)
        ws_log(ws, task_id, "[ERROR] no images selected for batch movies")
        ws_status(ws, task_id, "failed", "")
        return
    end
    _batch_register!(task_id)
    @async try
        run_batch_movies(task_id, project_uid, image_uids, config, file_attrs, fps, scale)
    catch e
        @warn "batch movies crashed" exception = e
        ws_log(ws, task_id, "[ERROR] batch crashed: $(sprint(showerror, e))")
        ws_status(ws, task_id, "failed", first(image_uids))
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

    proj = load_project(project_uid)

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

    Threads.@spawn try
        if resuming
            run_chain(proj, String[]; run_id=run_id,
                      start_node = isempty(start_node) ? nothing : start_node,
                      on_cancel_check = is_chain_cancelled,
                      on_log = line -> begin
                          println(line)
                          broadcast_ws(Dict{String,Any}("type" => "chain:log", "line" => line))
                      end)
        else
            run_chain(proj, image_uids; chain=chain_name,
                      on_cancel_check = is_chain_cancelled,
                      on_log = line -> begin
                          println(line)
                          broadcast_ws(Dict{String,Any}("type" => "chain:log", "line" => line))
                      end)
        end
        broadcast_ws(Dict{String,Any}("type" => "chain:run:done", "chain" => chain_name))
    catch e
        @warn "chain:run error" chain=chain_name run_id=run_id exception=e
        broadcast_ws(Dict{String,Any}("type"  => "chain:run:failed",
                                      "chain" => chain_name,
                                      "error" => string(e)))
    end
end

# Persist last-used params to each image dir + the set dir ({proj}/1/{uid}/ccid.json → meta.funParams).
# Dir-based (no object load) — see write_module_fun_params! in app/src/model/image.jl.
function _remember_fun_params(proj_root::String, fun::String, params::Dict{String,Any},
                              image_uid::String, image_uids::Vector{String}, set_uid::String)
    uids = !isempty(image_uids) ? image_uids : (isempty(image_uid) ? String[] : [image_uid])
    try
        for u in uids
            write_module_fun_params!(joinpath(proj_root, "1", u), fun, params)
        end
        isempty(set_uid) || write_module_fun_params!(joinpath(proj_root, "1", set_uid), fun, params)
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

    Threads.@spawn begin
        task_struct = try
            _task_from_fun_name(fun_name)
        catch
            ws_log(ws, task_id, "[ERROR] Unknown task: $fun_name")
            ws_status(ws, task_id, "failed", image_uid; fun=fun_name)
            return
        end

        # Set-scope tasks (e.g. behaviour.hmm) run once over the whole selected image vector;
        # image-scope tasks run on the single image. The frontend sends `imageUids` for set tasks.
        if task_scope(task_struct) == "set"
            uids = isempty(image_uids) ? (isempty(image_uid) ? String[] : [image_uid]) : image_uids
            imgs = CciaImage[]
            for u in uids
                try
                    obj = init_object(project_uid, u)
                    obj isa CciaImage ? push!(imgs, obj) :
                        ws_log(ws, task_id, "[WARN] not an image: $u")
                catch ex
                    ws_log(ws, task_id, "[WARN] could not load image $u: $ex")
                end
            end
            if isempty(imgs)
                ws_log(ws, task_id, "[ERROR] Set task '$fun_name' has no images")
                ws_status(ws, task_id, "failed", image_uid; fun=fun_name)
                return
            end
            rep = first(imgs).uid
            final_status = Ref{Symbol}(:failed)
            # run_task validates params FIRST (throws ParamValidationError before any job runs), so a
            # bad-param launch never reaches on_status_change. Catch it here so the failure is logged
            # AND a terminal task:status frame still goes out — otherwise the throw dies in the
            # @spawn silently (no [ERROR], no frame → the observer's "Watch" auto-trigger, which keys
            # off the terminal frame, never fires). Same guarantee for any pre-job throw.
            try
                result = run_task(task_struct, imgs, params;
                                  task_id          = task_id,
                                  pool_name        = pool_name,
                                  on_log           = line -> ws_log(ws, task_id, line),
                                  on_progress      = (n, t) -> ws_progress(ws, task_id, n, t),
                                  on_status_change = rec -> begin
                                      # queued/running forwarded live; also forward :cancelled at once
                                      # (it has no result to order before it) so a cancelled task —
                                      # especially a still-QUEUED one — reflects immediately, not only
                                      # when a worker later dequeues and skips it. :done/:failed still
                                      # wait for the final send.
                                      if rec.status in (:queued, :running, :cancelled)
                                          ws_status(ws, task_id, string(rec.status), rep; fun=fun_name)
                                      end
                                      final_status[] = rec.status
                                  end)
                isnothing(result) || ws_result(ws, task_id, rep, result)
            catch ex
                ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex))
                final_status[] = :failed
            end
            # a set task touched EVERY member — send the full list so the frontend invalidates all of
            # their plots, not just the representative's (closes the non-rep-member gap).
            ws_status(ws, task_id, string(final_status[]), rep; image_uids=[i.uid for i in imgs], fun=fun_name)
            return
        end

        # ── image-scope (single image) ──────────────────────────────────────────
        img = try
            obj = init_object(project_uid, image_uid)
            obj isa CciaImage || error("Not a CciaImage")
            obj
        catch ex
            ws_log(ws, task_id, "[ERROR] Could not load image: $ex")
            ws_status(ws, task_id, "failed", image_uid; fun=fun_name)
            return
        end

        # Capture the final status so we can send ws_result before ws_status("done"),
        # preserving the result→status ordering the frontend expects.
        final_status = Ref{Symbol}(:failed)
        # See the set-scope branch: run_task validates params first (throws before any job runs), so
        # a pre-job throw is caught here to guarantee an [ERROR] log + a terminal task:status frame.
        try
            result = run_task(task_struct, img, params;
                              task_id          = task_id,
                              pool_name        = pool_name,
                              on_log           = line -> ws_log(ws, task_id, line),
                              on_progress      = (n, t) -> ws_progress(ws, task_id, n, t),
                              on_status_change = rec -> begin
                                  # Forward queued/running immediately, and :cancelled too (no result
                                  # to order before it) so a cancelled — especially still-QUEUED —
                                  # task reflects at once. Hold :done/:failed until after the result.
                                  if rec.status in (:queued, :running, :cancelled)
                                      ws_status(ws, task_id, string(rec.status), image_uid; fun=fun_name)
                                  end
                                  final_status[] = rec.status
                              end)
            isnothing(result) || ws_result(ws, task_id, image_uid, result)
        catch ex
            ws_log(ws, task_id, "[ERROR] " * sprint(showerror, ex))
            final_status[] = :failed
        end
        ws_status(ws, task_id, string(final_status[]), image_uid; fun=fun_name)
    end
end

