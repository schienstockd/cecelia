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
ws_status(_ws, task_id, status, uid=""; image_uids=String[]) = _broadcast_task((; type="task:status", taskId=task_id, status=status, imageUid=uid, imageUids=image_uids))
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
        isempty(task_id) || cancel_task!(task_id)
    elseif type == "chain:run"
        handle_chain_run(ws, data)
    elseif type == "chain:cancel"
        run_id = String(get(data, :runId, ""))
        isempty(run_id) || cancel_chain_run!(run_id)
    else
        @warn "Unknown WS message type" type
    end
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
        ws_status(ws, task_id, "failed")
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
            ws_status(ws, task_id, "failed", image_uid)
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
                ws_status(ws, task_id, "failed", image_uid)
                return
            end
            rep = first(imgs).uid
            final_status = Ref{Symbol}(:failed)
            result = run_task(task_struct, imgs, params;
                              task_id          = task_id,
                              pool_name        = pool_name,
                              on_log           = line -> ws_log(ws, task_id, line),
                              on_progress      = (n, t) -> ws_progress(ws, task_id, n, t),
                              on_status_change = rec -> begin
                                  # queued/running forwarded live; also forward :cancelled at once (it
                                  # has no result to order before it) so a cancelled task — especially a
                                  # still-QUEUED one — reflects immediately, not only when a worker later
                                  # dequeues and skips it. :done/:failed still wait for the final send.
                                  if rec.status in (:queued, :running, :cancelled)
                                      ws_status(ws, task_id, string(rec.status), rep)
                                  end
                                  final_status[] = rec.status
                              end)
            isnothing(result) || ws_result(ws, task_id, rep, result)
            # a set task touched EVERY member — send the full list so the frontend invalidates all of
            # their plots, not just the representative's (closes the non-rep-member gap).
            ws_status(ws, task_id, string(final_status[]), rep; image_uids=[i.uid for i in imgs])
            return
        end

        # ── image-scope (single image) ──────────────────────────────────────────
        img = try
            obj = init_object(project_uid, image_uid)
            obj isa CciaImage || error("Not a CciaImage")
            obj
        catch ex
            ws_log(ws, task_id, "[ERROR] Could not load image: $ex")
            ws_status(ws, task_id, "failed", image_uid)
            return
        end

        # Capture the final status so we can send ws_result before ws_status("done"),
        # preserving the result→status ordering the frontend expects.
        final_status = Ref{Symbol}(:failed)
        result = run_task(task_struct, img, params;
                          task_id          = task_id,
                          pool_name        = pool_name,
                          on_log           = line -> ws_log(ws, task_id, line),
                          on_progress      = (n, t) -> ws_progress(ws, task_id, n, t),
                          on_status_change = rec -> begin
                              # Forward queued/running immediately, and :cancelled too (no result to
                              # order before it) so a cancelled — especially still-QUEUED — task
                              # reflects at once. Hold :done/:failed until after the result is sent.
                              if rec.status in (:queued, :running, :cancelled)
                                  ws_status(ws, task_id, string(rec.status), image_uid)
                              end
                              final_status[] = rec.status
                          end)

        isnothing(result) || ws_result(ws, task_id, image_uid, result)
        ws_status(ws, task_id, string(final_status[]), image_uid)
    end
end

