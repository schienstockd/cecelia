using Cecelia
using HTTP
using JSON3
using Logging

# ── Bootstrap ─────────────────────────────────────────────────────────────────

init_cecelia!()

# Load user drop-in task modules from <config_dir>/modules (no rebuild needed). Safe/never-throws;
# a broken module is logged, not fatal. See docs/CUSTOM_MODULES.md.
Cecelia.load_custom_modules!()

# ── Sub-modules ───────────────────────────────────────────────────────────────

include("sockets.jl")
include("routes.jl")
include("napari_api.jl")
include("preview_api.jl")   # task preview; reads the open image from napari_api
include("observer_api.jl")
include("gating_api.jl")
include("plotting_api.jl")
include("tracking_api.jl")
include("update_api.jl")
include("maintenance_api.jl")
include("repl_api.jl")
include("notebooks_api.jl")
include("image_geometry.jl")
include("image_render.jl")   # builds on image_geometry.jl
include("crop_api.jl")       # routes only; builds on both
include("app_api.jl")
include("storage_api.jl")
include("setup_api.jl")

# ── WS broadcast ──────────────────────────────────────────────────────────────

# Outbound broadcast is DECOUPLED from the caller, with a PER-CLIENT queue. Task events fan out on
# every log/progress/status line from many worker threads; writing sockets inline would (a) let
# concurrent worker threads write the SAME socket at once (frame corruption) and (b) let one
# slow/half-open client BLOCK a worker — which stalls a pool slot and, cascaded, leaves every task
# stuck at :queued. Instead each client gets its own bounded queue drained by its own background
# task: callers enqueue a pre-serialised frame per client (non-blocking, DROP on overflow), so a
# stuck client backs up ONLY its own queue and is dropped alone — it can neither block a worker nor,
# via head-of-line blocking on a shared sender, freeze telemetry for the other clients. WS telemetry
# is lossy-safe (the console reconciles from GET /api/tasks); a worker thread must never block on WS I/O.
const _WS_OUT_CAP      = 4096
const _ws_clients      = Dict{Any,Channel{String}}()   # ws => its private outbound queue
const _ws_clients_lock = ReentrantLock()

# One sender per client: drains that client's queue in order and writes frames. Exits when the queue
# is closed (on disconnect) or a send fails; either way the client is removed.
function _ws_client_sender(ws, q::Channel{String})
    try
        for json in q
            HTTP.WebSockets.send(ws, json)
        end
    catch
        # send failed / client gone — drop it (handle_ws's finally also cleans up on the receive side)
        lock(_ws_clients_lock) do; delete!(_ws_clients, ws); end
        try; close(ws); catch; end
    end
end

function broadcast_ws(msg::Dict)
    json   = JSON3.write(msg)
    queues = lock(_ws_clients_lock) do; collect(values(_ws_clients)); end
    for q in queues
        # non-blocking, per-client: skip this frame for a client whose queue is full (it's stuck),
        # never block the caller (a worker thread). The check-then-put race can only ever cost a
        # microsecond wait behind a HEALTHY client's drain, never a block behind a stuck one.
        isopen(q) && Base.n_avail(q) < _WS_OUT_CAP && try; put!(q, json); catch; end
    end
    nothing
end

# ── Server-log tee → WS (the "pixi console" in the browser) ─────────────────────
# The Settings console window streams the backend's OWN @info/@warn/@error (startup banner, napari
# warnings, …), not just task logs. A global AbstractLogger forwards each record to the real console
# logger AND broadcasts it as {type:"server:log"}, keeping a small ring buffer so a freshly-opened
# console backfills recent lines via GET /api/logs/recent. Installed in `start()` only (never under
# CECELIA_NO_SERVE) so the test harness keeps the plain logger.
const _LOG_RING_CAP  = 500
const _log_ring      = Vector{Dict{String,Any}}()
const _log_ring_lock = ReentrantLock()

function _push_log_ring(rec::Dict{String,Any})
    lock(_log_ring_lock) do
        push!(_log_ring, rec)
        length(_log_ring) > _LOG_RING_CAP && popfirst!(_log_ring)
    end
end

struct BroadcastLogger <: Logging.AbstractLogger
    inner::Logging.AbstractLogger
end
Logging.min_enabled_level(l::BroadcastLogger) = Logging.min_enabled_level(l.inner)
Logging.shouldlog(l::BroadcastLogger, level, _module, group, id) =
    Logging.shouldlog(l.inner, level, _module, group, id)
Logging.catch_exceptions(l::BroadcastLogger) = Logging.catch_exceptions(l.inner)
function Logging.handle_message(l::BroadcastLogger, level, message, _module, group, id, file, line; kwargs...)
    Logging.handle_message(l.inner, level, message, _module, group, id, file, line; kwargs...)
    try
        lvl = level >= Logging.Error ? "error" : level >= Logging.Warn ? "warn" : "info"
        msg = string(message)
        isempty(kwargs) || (msg *= "  " * join(("$k = $v" for (k, v) in kwargs), "  "))
        rec = Dict{String,Any}("level" => lvl, "message" => msg)
        _push_log_ring(rec)
        broadcast_ws(Dict{String,Any}("type" => "server:log", "level" => lvl, "message" => msg))
    catch
        # a logging failure must never escape the logger
    end
    nothing
end

_install_log_tee!() = global_logger(BroadcastLogger(global_logger()))

# GET /api/logs/recent → { logs: [{level,message}, …] } — backfill a freshly-opened console window
function api_logs_recent()
    logs = lock(_log_ring_lock) do; copy(_log_ring); end
    200, JSON3.write((; logs = logs))
end

# ── Chain event → WS bridge ───────────────────────────────────────────────────
# `taskId` is the scheduler task the node ran as — the task console correlates it with its
# `GET /api/tasks` row to attribute the node's real outcome (a chain run emits no `task:status`
# frames, so without it a finished node can only be reported as "outcome unseen"). Read with
# `_ev_task_id` rather than `p.task_id`: a hand-fired event from the REPL/tests may omit the field,
# and a node with no task id yet (skipped before submission, set-scope) carries `nothing` — both
# must degrade to "" and never take the bridge down.
_ev_task_id(p)::String = something(get(p, :task_id, ""), "")


subscribe_chain_events!("node:queued", function(p)
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:queued",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "params"     => p.params,
        "taskId"     => _ev_task_id(p),
    ))
end)

# `startedAt` rides along for the same reason `taskId` does: a chain run emits no `task:status`, so these
# frames are the ONLY live carrier of a node's timing, and a client that times the row itself is timing
# from when it saw the frame. The per-image path fires this from the scheduler's own `:running` transition,
# so the start is already on record and exact; a **set-scope** node bypasses `run_task` entirely (no
# `TaskRecord`), which is what `note_task_started!` here covers — first-write-wins, so it can only ever
# fill in a start that nothing more precise has recorded.
subscribe_chain_events!("node:running", function(p)
    tid = _ev_task_id(p)
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:running",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "params"     => p.params,
        "taskId"     => tid,
        "startedAt"  => isempty(tid) ? "" : iso_utc(note_task_started!(tid)),
    ))
end)

# A terminal outcome reaches a client through exactly TWO carriers, and BOTH bank it for replay
# (`record_task_outcome!`) — a chain run emits no `task:status` at all (`handle_chain_run` passes no
# `on_status_change`), so `ws_status` never sees a chain node and banking only there would leave every
# chain node unrecoverable. Keyed by the node's scheduler task id, which is what a consumer correlates a
# chain row against. `status` on node:failed may be "skipped" (never ran, no task id) — not a terminal
# task status, so `record_task_outcome!` ignores it. See `app/src/tasks/task_outcomes.jl`.
#
# The banked row is also where the frame's timestamps come from — it is written FIRST and it is what
# drops the task's start from the in-flight timing map, so re-deriving them here would publish `""` for
# every finished node. Same rule as `ws_status`: one derivation, two carriers, no disagreement about when
# a task ran. `_ev_times` degrades to empty strings for a node with no banked row (a `skipped` status,
# no task id) — the frame still goes out, just without timing.
_ev_times(row) = isnothing(row) ? ("", "") : (row.started_at, row.finished_at)

subscribe_chain_events!("node:done", function(p)
    started, finished = _ev_times(record_task_outcome!(_ev_task_id(p), "done";
                                                       image_uid=p.image_uid, fun=p.fn))
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:done",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "params"     => p.params,
        "result"     => p.result,
        "taskId"     => _ev_task_id(p),
        "startedAt"  => started,
        "finishedAt" => finished,
    ))
end)

subscribe_chain_events!("node:failed", function(p)
    started, finished = _ev_times(record_task_outcome!(_ev_task_id(p), p.status;
                                                       image_uid=p.image_uid, fun=p.fn))
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:failed",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "status"     => p.status,
        "taskId"     => _ev_task_id(p),
        "startedAt"  => started,
        "finishedAt" => finished,
    ))
end)

# ── HTTP router ───────────────────────────────────────────────────────────────

# Routing is a LOOKUP TABLE, not an if/elseif chain. It used to be one — 156 branches — and that
# cost 42s of a 53s server boot, on every restart (measured with --trace-compile-timing; it showed
# up as ONE method, the handle_stream closure that inlines the chain). Compiling the chain forces
# the compiler to infer EVERY handler call even though exactly one runs. With a table only the
# invoked handler compiles: boot 54s -> 11s, compile 50s -> 6s.
#
# Splitting the chain into per-method functions does NOT fix it — measured, no change — because it
# is still one compilation request over the same call tree. The win comes from never putting all
# the handler calls in one inferable unit. (Same shape as the 8k-line @testset that cost ~90s of a
# 200s test run: one oversized unit dominating compilation.)
#
# Add a route by adding a table entry. Do NOT merge these back into a chain — the
# "HTTP router — the full route table still dispatches" testset pins every path.
function handle_http(req::HTTP.Request, body_bytes::Vector{UInt8})
    target = String(req.target)
    path   = split(HTTP.URI(target).path, '?')[1]
    method = req.method

    table = method == "GET"  ? _GET_ROUTES :
            method == "POST" ? _POST_ROUTES : nothing
    table === nothing && return 405, JSON3.write((; error="Method not allowed: $method"))

    handler = get(table, path, nothing)   # SubString hashes as its String — no copy per request
    handler === nothing && return 404, JSON3.write((; error="Not found: $path"))
    handler(req, body_bytes)
end

# ── GET ─────────────────────────────────────────────────────────────────────
const _GET_ROUTES = Dict{String, Function}(
    "/api/health" => (req, body_bytes) -> (200, JSON3.write((; ok=true, version="CeceliaAPI"))),
    "/api/diagnostics" => (req, body_bytes) -> (api_diagnostics(req)),
    "/api/app/worktrees" => (req, body_bytes) -> (api_app_worktrees(req)),
    "/api/diagnostics/packages" => (req, body_bytes) -> (api_packages(req)),
    "/api/version" => (req, body_bytes) -> (api_version(req)),
    "/api/update/check" => (req, body_bytes) -> (api_update_check(req)),
    "/api/setup/defaults" => (req, body_bytes) -> (api_setup_defaults(req)),
    "/api/setup/validate" => (req, body_bytes) -> (api_setup_validate(req)),
    "/api/projects" => (req, body_bytes) -> (api_projects_list(req)),
    "/api/projects/bundles" => (req, body_bytes) -> (api_projects_bundles(req)),
    "/api/projects/bundle-info" => (req, body_bytes) -> (api_projects_bundle_info(req)),
    "/api/fs/list" => (req, body_bytes) -> (api_fs_list(req)),
    "/api/images" => (req, body_bytes) -> (api_images_list(req)),
    "/api/images/meta" => (req, body_bytes) -> (api_images_meta(req)),
    "/api/images/tasklog" => (req, body_bytes) -> (api_images_tasklog(req)),
    "/api/tasks/history" => (req, body_bytes) -> (api_tasks_history(req)),
    "/api/tasks/recent" => (req, body_bytes) -> (api_tasks_recent(req)),
    "/api/qc/cohort" => (req, body_bytes) -> (api_qc_cohort(req)),
    "/api/qc/cohort/runs" => (req, body_bytes) -> (api_qc_cohort_runs(req)),
    "/api/analysis/lineage" => (req, body_bytes) -> (api_analysis_lineage(req)),
    "/api/analysis/populations" => (req, body_bytes) -> (api_analysis_populations(req)),
    "/api/analysis/measures" => (req, body_bytes) -> (api_analysis_measures(req)),
    "/api/analysis/behaviour" => (req, body_bytes) -> (api_analysis_behaviour(req)),
    "/api/analysis/clusters" => (req, body_bytes) -> (api_analysis_clusters(req)),
    "/api/analysis/spatial" => (req, body_bytes) -> (api_analysis_spatial(req)),
    "/api/analysis/chains" => (req, body_bytes) -> (api_analysis_chains(req)),
    "/api/repl/api" => (req, body_bytes) -> (api_repl_api(req)),
    "/api/observer/briefing" => (req, body_bytes) -> (api_observer_briefing(req)),
    "/api/lablog" => (req, body_bytes) -> (api_lablog_read(req)),
    "/api/tasks/definitions" => (req, body_bytes) -> (api_task_definitions(req)),
    "/api/maintenance/patches" => (req, body_bytes) -> (api_maintenance_patches(req)),
    "/api/tasks/custom-modules" => (req, body_bytes) -> (api_custom_modules_status(req)),
    "/api/tasks/funparams" => (req, body_bytes) -> (api_task_fun_params(req)),
    "/api/pools" => (req, body_bytes) -> (api_pools_list(req)),
    "/api/storage/compressor" => (req, body_bytes) -> (api_compressor_get(req)),
    "/api/tasks" => (req, body_bytes) -> (api_tasks_list(req)),
    "/api/chains" => (req, body_bytes) -> (api_chains_list(req)),
    "/api/chains/get" => (req, body_bytes) -> (api_chains_get(req)),
    "/api/chains/runs" => (req, body_bytes) -> (api_chains_runs(req)),
    "/api/chains/run" => (req, body_bytes) -> (api_chains_run(req)),
    "/api/logs/recent" => (req, body_bytes) -> (api_logs_recent()),
    "/api/observer/status" => (req, body_bytes) -> (api_observer_status(req)),
    "/api/napari/status" => (req, body_bytes) -> (api_napari_status(req)),
    "/api/napari/gpu" => (req, body_bytes) -> (api_napari_gpu_get(req)),
    "/api/preview/status" => (req, body_bytes) -> (api_preview_status(req)),
    "/api/notebooks" => (req, body_bytes) -> (api_notebooks_list(req)),
    "/api/notebooks/content" => (req, body_bytes) -> (api_notebooks_content(req)),
    "/api/notebooks/status" => (req, body_bytes) -> (api_notebooks_status(req)),
    "/api/notebooks/snapshots" => (req, body_bytes) -> (api_notebooks_snapshots(req)),
    "/api/gating/channels" => (req, body_bytes) -> (api_gating_channels(req)),
    "/api/gating/popmap" => (req, body_bytes) -> (api_gating_popmap(req)),
    "/api/gating/stats" => (req, body_bytes) -> (api_gating_stats(req)),
    "/api/gating/membership" => (req, body_bytes) -> (api_gating_membership(req)),
    "/api/gating/plotmeta" => (req, body_bytes) -> (api_gating_plotmeta(req)),
    "/api/gating/plotdata" => (req, body_bytes) -> (api_gating_plotdata(req)),
    "/api/gating/density" => (req, body_bytes) -> (api_gating_density(req)),
    "/api/images/geometry" => (req, body_bytes) -> (api_image_geometry(req)),
    "/api/images/stores" => (req, body_bytes) -> (api_image_stores(req)),
    "/api/crop/info" => (req, body_bytes) -> (api_crop_info(req)),
    "/api/crop/frame" => (req, body_bytes) -> (api_crop_frame(req)),
    "/api/plots/umap" => (req, body_bytes) -> (api_plots_umap(req)),
    "/api/plots/definitions" => (req, body_bytes) -> (api_plot_definitions(req)),
    "/api/plots/populations" => (req, body_bytes) -> (api_plot_populations(req)),
    "/api/plots/attrs" => (req, body_bytes) -> (api_plot_attrs(req)),
    "/api/tracking/motion-dims" => (req, body_bytes) -> (api_motion_dims(req)),
    "/api/storage/summary" => (req, body_bytes) -> (api_storage_summary(req)),
    "/api/movies" => (req, body_bytes) -> (api_movies_list(req)),
)

# ── POST ─────────────────────────────────────────────────────────────────────
const _POST_ROUTES = Dict{String, Function}(
    "/api/projects/list" => (req, body_bytes) -> (api_projects_list(req)),
    "/api/pools/set" => (req, body_bytes) -> (api_pool_set(body_bytes)),
    "/api/storage/compressor/set" => (req, body_bytes) -> (api_compressor_set(body_bytes)),
    "/api/tasks/custom-modules/reload" => (req, body_bytes) -> (api_custom_modules_reload(body_bytes)),
    "/api/projects/create" => (req, body_bytes) -> (api_projects_create(body_bytes)),
    "/api/projects/load" => (req, body_bytes) -> (api_projects_load(body_bytes)),
    "/api/projects/boards" => (req, body_bytes) -> (api_projects_boards(body_bytes)),
    "/api/projects/animations" => (req, body_bytes) -> (api_projects_animations(body_bytes)),
    "/api/projects/canvases" => (req, body_bytes) -> (api_projects_canvases(body_bytes)),
    "/api/board-assets/save" => (req, body_bytes) -> (api_board_asset_save(body_bytes)),
    "/api/board-assets/delete" => (req, body_bytes) -> (api_board_asset_delete(body_bytes)),
    "/api/board-assets/copy" => (req, body_bytes) -> (api_board_asset_copy(body_bytes)),
    "/api/projects/rename" => (req, body_bytes) -> (api_projects_rename(body_bytes)),
    "/api/projects/delete" => (req, body_bytes) -> (api_projects_delete(body_bytes)),
    "/api/sets/create" => (req, body_bytes) -> (api_sets_create(body_bytes)),
    "/api/sets/delete" => (req, body_bytes) -> (api_sets_delete(body_bytes)),
    "/api/images/register" => (req, body_bytes) -> (api_images_register(body_bytes)),
    "/api/import/scan-legacy" => (req, body_bytes) -> (api_import_scan_legacy(body_bytes)),
    "/api/import/register-legacy" => (req, body_bytes) -> (api_import_register_legacy(body_bytes)),
    "/api/images/delete" => (req, body_bytes) -> (api_images_delete(body_bytes)),
    "/api/images/move" => (req, body_bytes) -> (api_images_move(body_bytes)),
    "/api/images/version/remove" => (req, body_bytes) -> (api_images_version_remove(body_bytes)),
    "/api/images/analysis/reset" => (req, body_bytes) -> (api_images_analysis_reset(body_bytes)),
    "/api/images/attr/create" => (req, body_bytes) -> (api_images_attr_create(body_bytes)),
    "/api/images/attr/delete" => (req, body_bytes) -> (api_images_attr_delete(body_bytes)),
    "/api/images/attr/set" => (req, body_bytes) -> (api_images_attr_set(body_bytes)),
    "/api/images/channelnames" => (req, body_bytes) -> (api_images_channelnames(body_bytes)),
    "/api/images/meta/set" => (req, body_bytes) -> (api_images_meta_set(body_bytes)),
    "/api/images/inclusion/set" => (req, body_bytes) -> (api_images_inclusion_set(body_bytes)),
    "/api/qc/cohort/check" => (req, body_bytes) -> (api_qc_cohort_check(body_bytes)),
    "/api/lablog/append" => (req, body_bytes) -> (api_lablog_append(body_bytes)),
    "/api/lablog/capture" => (req, body_bytes) -> (api_lablog_capture(body_bytes)),
    "/api/observer/feedback" => (req, body_bytes) -> (api_observer_feedback(body_bytes)),
    "/api/observer/clear" => (req, body_bytes) -> (api_observer_clear(body_bytes)),
    "/api/observer/register" => (req, body_bytes) -> (api_observer_register(body_bytes)),
    "/api/lablog/dismiss" => (req, body_bytes) -> (api_lablog_dismiss(body_bytes)),
    "/api/images/meta/resync" => (req, body_bytes) -> (api_images_meta_resync(body_bytes)),
    "/api/images/labels/delete" => (req, body_bytes) -> (api_images_delete_labels(body_bytes)),
    "/api/chains/save" => (req, body_bytes) -> (api_chains_save(body_bytes)),
    "/api/chains/delete" => (req, body_bytes) -> (api_chains_delete(body_bytes)),
    "/api/notebooks/launch" => (req, body_bytes) -> (api_notebooks_launch(body_bytes)),
    "/api/notebooks/write" => (req, body_bytes) -> (api_notebooks_write(body_bytes)),
    "/api/notebooks/create" => (req, body_bytes) -> (api_notebooks_create(body_bytes)),
    "/api/notebooks/describe" => (req, body_bytes) -> (api_notebooks_describe(body_bytes)),
    "/api/notebooks/delete" => (req, body_bytes) -> (api_notebooks_delete(body_bytes)),
    "/api/notebooks/duplicate" => (req, body_bytes) -> (api_notebooks_duplicate(body_bytes)),
    "/api/notebooks/revise" => (req, body_bytes) -> (api_notebooks_revise(body_bytes)),
    "/api/notebooks/snapshot" => (req, body_bytes) -> (api_notebooks_snapshot(body_bytes)),
    "/api/notebooks/restore" => (req, body_bytes) -> (api_notebooks_restore(body_bytes)),
    "/api/notebooks/prune" => (req, body_bytes) -> (api_notebooks_prune(body_bytes)),
    "/api/notebooks/shutdown" => (req, body_bytes) -> (api_notebooks_shutdown(body_bytes)),
    "/api/notebooks/restart" => (req, body_bytes) -> (api_notebooks_restart(body_bytes)),
    "/api/notebooks/build-sysimage" => (req, body_bytes) -> (api_notebooks_build_sysimage(body_bytes)),
    "/api/setup/init" => (req, body_bytes) -> (api_setup_init(body_bytes)),
    "/api/app/shutdown" => (req, body_bytes) -> (api_app_shutdown(body_bytes)),
    "/api/app/restart" => (req, body_bytes) -> (api_app_restart(body_bytes)),
    "/api/app/switch-worktree" => (req, body_bytes) -> (api_app_switch_worktree(body_bytes)),
    "/api/napari/open" => (req, body_bytes) -> (api_napari_open(body_bytes)),
    "/api/napari/close" => (req, body_bytes) -> (api_napari_close(body_bytes)),
    "/api/napari/screenshot" => (req, body_bytes) -> (api_napari_screenshot(body_bytes)),
    "/api/napari/apply-view-state" => (req, body_bytes) -> (api_napari_apply_view_state(body_bytes)),
    "/api/napari/view-state" => (req, body_bytes) -> (api_napari_view_state(body_bytes)),
    "/api/napari/overlay-legend" => (req, body_bytes) -> (api_napari_overlay_legend(body_bytes)),
    "/api/napari/apply-movie-config" => (req, body_bytes) -> (api_napari_apply_movie_config(body_bytes)),
    "/api/napari/restart" => (req, body_bytes) -> (api_napari_restart(body_bytes)),
    "/api/napari/gpu" => (req, body_bytes) -> (api_napari_gpu_set(body_bytes)),
    "/api/napari/configure-autosave" => (req, body_bytes) -> (api_napari_configure_autosave(body_bytes)),
    "/api/napari/show-labels" => (req, body_bytes) -> (api_napari_show_labels(body_bytes)),
    "/api/napari/refresh-labels" => (req, body_bytes) -> (api_napari_refresh_labels(body_bytes)),
    "/api/napari/show-populations" => (req, body_bytes) -> (api_napari_show_populations(body_bytes)),
    "/api/napari/show-tracks" => (req, body_bytes) -> (api_napari_show_tracks(body_bytes)),
    "/api/napari/colour-labels" => (req, body_bytes) -> (api_napari_colour_labels(body_bytes)),
    "/api/napari/colour-branch-labels" => (req, body_bytes) -> (api_napari_colour_branch_labels(body_bytes)),
    "/api/napari/start-selection" => (req, body_bytes) -> (api_napari_start_selection(body_bytes)),
    "/api/napari/selection-scope" => (req, body_bytes) -> (api_napari_selection_scope(body_bytes)),
    "/api/napari/stop-selection" => (req, body_bytes) -> (api_napari_stop_selection(body_bytes)),
    "/api/napari/event" => (req, body_bytes) -> (api_napari_event(body_bytes)),
    "/api/preview/start" => (req, body_bytes) -> (api_preview_start(body_bytes)),
    "/api/preview/stop" => (req, body_bytes) -> (api_preview_stop(body_bytes)),
    "/api/preview/run" => (req, body_bytes) -> (api_preview_run(body_bytes)),
    "/api/gating/pop/add" => (req, body_bytes) -> (api_gating_pop_add(body_bytes)),
    "/api/gating/pop/set-gate" => (req, body_bytes) -> (api_gating_pop_set_gate(body_bytes)),
    "/api/gating/pop/delete" => (req, body_bytes) -> (api_gating_pop_delete(body_bytes)),
    "/api/gating/pop/update" => (req, body_bytes) -> (api_gating_pop_update(body_bytes)),
    "/api/gating/pop/rename" => (req, body_bytes) -> (api_gating_pop_rename(body_bytes)),
    "/api/gating/copy" => (req, body_bytes) -> (api_gating_copy(body_bytes)),
    "/api/images/value-name-check" => (req, body_bytes) -> (api_images_value_name_check(body_bytes)),
    "/api/plot_data" => (req, body_bytes) -> (api_plot_data(body_bytes)),
    "/api/repl" => (req, body_bytes) -> (api_repl(body_bytes)),
    "/api/repl/config" => (req, body_bytes) -> (api_repl_config(body_bytes)),
    "/api/update/apply" => (req, body_bytes) -> (api_update_apply(body_bytes)),
    "/api/storage/reclaim" => (req, body_bytes) -> (api_storage_reclaim(body_bytes)),
)


# ── WebSocket handler ─────────────────────────────────────────────────────────

function handle_ws(ws)
    q = Channel{String}(_WS_OUT_CAP)
    lock(_ws_clients_lock) do; _ws_clients[ws] = q; end
    Threads.@spawn _ws_client_sender(ws, q)   # per-client drain (see broadcast_ws)
    try
        while true
            raw = HTTP.WebSockets.receive(ws)
            raw isa String || continue
            try
                handle_message(ws, raw)
            catch e
                @warn "WS message error" exception = e
            end
        end
    catch e
        e isa HTTP.WebSockets.WebSocketError || @warn "WS error" exception = e
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, ws); end
        close(q)   # signal this client's sender task to exit
    end
end

# ── Static frontend serving ───────────────────────────────────────────────────
# In production the Julia server serves the built Vue app (frontend/dist) at the same origin, so
# the whole app is reachable at http://localhost:8080 — no CORS, no dev proxy. In dev you still use
# the Vite server (:5173), which proxies /api + /ws here. If dist/ is absent (dev), these no-op and
# requests fall through to the API router. See docs/SHIPPING.md.

const _DIST_DIR = abspath(joinpath(@__DIR__, "..", "..", "frontend", "dist"))

const _STATIC_MIME = Dict(
    ".html" => "text/html; charset=utf-8", ".js"  => "text/javascript; charset=utf-8",
    ".mjs"  => "text/javascript; charset=utf-8", ".css" => "text/css; charset=utf-8",
    ".json" => "application/json", ".svg" => "image/svg+xml", ".png" => "image/png",
    ".jpg"  => "image/jpeg", ".jpeg" => "image/jpeg", ".gif" => "image/gif",
    ".webp" => "image/webp", ".ico" => "image/x-icon", ".woff" => "font/woff",
    ".woff2" => "font/woff2", ".ttf" => "font/ttf", ".wasm" => "application/wasm",
    ".map"  => "application/json", ".txt" => "text/plain; charset=utf-8",
)

_static_content_type(p) = get(_STATIC_MIME, lowercase(splitext(p)[2]), "application/octet-stream")

# Resolve a request path to an existing file under dist/, guarding against path traversal.
function _resolve_static(reqpath::AbstractString)::Union{String,Nothing}
    isdir(_DIST_DIR) || return nothing
    rel = lstrip(reqpath, '/')
    isempty(rel) && (rel = "index.html")
    full = abspath(joinpath(_DIST_DIR, rel))
    (startswith(full, _DIST_DIR) && isfile(full)) || return nothing
    full
end

# Serve a built-frontend file for a non-API GET; SPA-fallback to index.html for extensionless
# (client-route) paths so Vue Router handles them. Returns true if it wrote a response.
function try_serve_static(stream::HTTP.Stream, reqpath::AbstractString)::Bool
    isdir(_DIST_DIR) || return false
    full = _resolve_static(reqpath)
    if full === nothing
        isempty(splitext(reqpath)[2]) || return false      # genuine missing asset → let it 404
        idx = joinpath(_DIST_DIR, "index.html")
        isfile(idx) || return false
        full = idx
    end
    data = read(full)
    HTTP.setstatus(stream, 200)
    HTTP.setheader(stream, "Content-Type" => _static_content_type(full))
    HTTP.startwrite(stream)
    write(stream, data)
    true
end

# Serve a board-image sidecar (settings/board-assets/<assetId>.png) as image/png for an <img> src.
# GET /api/board-assets?projectUid=…&assetId=…  — set the mime explicitly (the generic API response
# path only does octet-stream/JSON). Returns true if it wrote a response.
function try_serve_board_asset(stream::HTTP.Stream, target::AbstractString)::Bool
    q = HTTP.queryparams(HTTP.URI(target))
    uid = get(q, "projectUid", ""); aid = get(q, "assetId", "")
    (isempty(uid) || isempty(aid) || !_valid_asset_id(aid)) && return false
    f = joinpath(_board_assets_dir(String(uid)), String(aid) * ".png")
    isfile(f) || return false
    data = read(f)
    HTTP.setstatus(stream, 200)
    HTTP.setheader(stream, "Content-Type"                => "image/png")
    HTTP.setheader(stream, "Access-Control-Allow-Origin" => "*")
    HTTP.startwrite(stream)
    write(stream, data)
    true
end

# Copy `n` bytes from `io` to the response stream in bounded chunks — never slurp a whole movie (or a
# large range slice) into memory just to hand it to the socket.
function _stream_file!(stream::HTTP.Stream, io::IO, n::Integer)
    remaining = Int(n)
    buf = Vector{UInt8}(undef, 64 * 1024)
    while remaining > 0 && !eof(io)
        nread = readbytes!(io, buf, min(length(buf), remaining))
        nread == 0 && break
        write(stream, view(buf, 1:nread))
        remaining -= nread
    end
end

# Parse an HTTP `Range` header value into inclusive (start, stop) byte offsets clamped to the file, or
# `nothing` if absent/unsatisfiable. Handles the forms a <video> element sends: "bytes=START-" (open
# ended), "bytes=START-END", and "bytes=-SUFFIX" (last N bytes). Single range only (all browsers do).
function _parse_range(header::AbstractString, total::Integer)
    m = match(r"^bytes=(\d*)-(\d*)$", strip(header))
    m === nothing && return nothing
    s, e = m.captures[1], m.captures[2]
    if isempty(s)                          # suffix form: last `e` bytes
        (isempty(e) || total == 0) && return nothing
        n = parse(Int, e)
        n <= 0 && return nothing
        return (max(0, total - n), total - 1)
    end
    start = parse(Int, s)
    start >= total && return nothing       # unsatisfiable → caller falls back to 200
    stop = isempty(e) ? total - 1 : min(parse(Int, e), total - 1)
    stop < start && return nothing
    (start, stop)
end

# Serve a rendered project movie as video/mp4 for the /movies player, honouring HTTP Range so seeking
# works. GET /api/movies/file?projectUid=…&name=….mp4 — no Range → 200 full body; a Range → 206 with
# Content-Range and only that slice. This is the server's ONLY range-capable route, and a <video>
# element issues Range requests in every browser, so this is what makes scrubbing work at all. Streamed
# in chunks (never buffers the whole file). Returns true iff it wrote a response.
function try_serve_movie(stream::HTTP.Stream, target::AbstractString)::Bool
    q = HTTP.queryparams(HTTP.URI(target))
    uid = get(q, "projectUid", ""); name = get(q, "name", "")
    (isempty(uid) || !_valid_movie_name(name)) && return false
    f = joinpath(_movies_dir_for_project(String(uid)), String(name))
    isfile(f) || return false
    total = filesize(f)
    rng = _parse_range(HTTP.header(stream.message, "Range", ""), total)

    HTTP.setheader(stream, "Content-Type"                => "video/mp4")
    HTTP.setheader(stream, "Accept-Ranges"               => "bytes")
    HTTP.setheader(stream, "Access-Control-Allow-Origin" => "*")
    if rng === nothing
        HTTP.setheader(stream, "Content-Length" => string(total))
        HTTP.setstatus(stream, 200)
        HTTP.startwrite(stream)
        open(io -> _stream_file!(stream, io, total), f)
    else
        start, stop = rng
        HTTP.setheader(stream, "Content-Range"  => "bytes $start-$stop/$total")
        HTTP.setheader(stream, "Content-Length" => string(stop - start + 1))
        HTTP.setstatus(stream, 206)
        HTTP.startwrite(stream)
        open(f) do io
            seek(io, start)
            _stream_file!(stream, io, stop - start + 1)
        end
    end
    true
end

# ── Mixed HTTP + WebSocket stream handler ─────────────────────────────────────

function handle_stream(stream::HTTP.Stream)
    req = stream.message

    if HTTP.WebSockets.isupgrade(req)
        HTTP.WebSockets.upgrade(handle_ws, stream; check_origin=(req, origin) -> true)
        return
    end

    if req.method == "OPTIONS"
        HTTP.setstatus(stream, 204)
        HTTP.setheader(stream, "Access-Control-Allow-Origin"  => "*")
        HTTP.setheader(stream, "Access-Control-Allow-Methods" => "GET, POST, OPTIONS")
        HTTP.setheader(stream, "Access-Control-Allow-Headers" => "Content-Type")
        HTTP.startwrite(stream)
        return
    end

    # Serve the built frontend (same-origin) for non-API GETs; falls through to the API router
    # when dist/ is absent (dev) or the path is /api/*.
    if req.method == "GET"
        spath = split(HTTP.URI(req.target).path, '?')[1]
        if spath == "/api/board-assets"
            try_serve_board_asset(stream, req.target) && return   # else falls through → 404 below
        elseif spath == "/api/movies/file"
            try_serve_movie(stream, req.target) && return         # range-served mp4; else → 404 below
        elseif !startswith(spath, "/api/") && spath != "/ws"
            try_serve_static(stream, spath) && return
        end
    end

    body_bytes = read(stream)
    # Run the handler on the thread POOL (not this connection's task), so a CPU/IO-bound handler — e.g.
    # a big HDF5 label-table read, a blocking C call that never yields — doesn't stall the accept loop
    # or other in-flight requests (a napari open would otherwise queue behind it). Under `-t 1` this is
    # just a cooperative task (no behaviour change); under `-t auto` it's real parallelism. Shared state
    # is already lock-guarded (WS clients, napari, chain runs) and Julia HDF5 is serialised via
    # `_with_h5`. Error handling lives INSIDE the spawned task, so `fetch` always yields a (status, body)
    # tuple and never rethrows a TaskFailedException.
    status, body = fetch(Threads.@spawn begin
        try
            handle_http(req, body_bytes)
        catch e
            @error "Unhandled error in $(req.method) $(req.target)" exception = (e, catch_backtrace())
            500, JSON3.write((; error = sprint(showerror, e)))
        end
    end)

    # Binary handlers (gating plotdata/density/membership) return a byte vector → octet-stream;
    # everything else returns a JSON string.
    content_type = body isa AbstractVector{UInt8} ? "application/octet-stream" : "application/json"

    HTTP.setstatus(stream, status)
    HTTP.setheader(stream, "Content-Type"                 => content_type)
    HTTP.setheader(stream, "Access-Control-Allow-Origin"  => "*")
    HTTP.setheader(stream, "Access-Control-Allow-Methods" => "GET, POST, OPTIONS")
    HTTP.setheader(stream, "Access-Control-Allow-Headers" => "Content-Type")
    HTTP.startwrite(stream)
    write(stream, body)
end

# ── Entry point ───────────────────────────────────────────────────────────────

# Cecelia is a LOCAL app, so bind loopback by default — not reachable from the network, which is both
# the safer default and what lets the debug console run (its hard gate is a loopback bind). Set
# CECELIA_HOST=0.0.0.0 to deliberately expose it (the console then refuses to run).
const HOST = get(ENV, "CECELIA_HOST", "127.0.0.1")
const PORT = parse(Int, get(ENV, "CECELIA_PORT", "8080"))
# The address the server is ACTUALLY bound to (set in `start`). The debug REPL keys off this: it only
# runs when the bind is loopback, so a loopback bind — not a spoofable header — is the network control.
const _BOUND_HOST = Ref{String}("")

function start(; host=HOST, port=PORT)
    _BOUND_HOST[] = string(host)
    _install_log_tee!()   # tee server logs to the WS console (only when actually serving)
    @info "CeceliaAPI starting" host port threads=Threads.nthreads() projects_dir=projects_dir()
    HTTP.listen(handle_stream, host, port)
end

# Auto-start on load — EXCEPT when `CECELIA_NO_SERVE` is set, so `api/test/runtests.jl` can `include`
# this file to get the handlers (and shared state like `_BOUND_HOST`) without binding a socket.
haskey(ENV, "CECELIA_NO_SERVE") || start()
