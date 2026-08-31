using Cecelia
using HTTP
using JSON3

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
include("plugins_api.jl")   # plugin install/remove; uses update_api.jl's Downloads + routes.jl's payload
include("maintenance_api.jl")
include("repl_api.jl")
include("runner_api.jl")     # detached task runner; uses repl_api.jl's _GIT_COMMIT for staleness
include("notebooks_api.jl")
include("movies_api.jl")     # movie registry; builds on routes.jl's movies-dir + name guard
include("optical_flow_api.jl")
include("image_geometry.jl")
include("image_render.jl")   # builds on image_geometry.jl
include("frame_overlays.jl") # CPU-side overlay drawing primitives — points/tracks for the offline renderer
include("overlay_author.jl") # resolves populations/tracks into the primitives' columnar shape
include("crop_api.jl")       # routes only; builds on both
include("viewer_api.jl")     # browser WebGPU renderer: volume slabs + display metadata
include("movie_render.jl")   # the offline renderer's timelapse sweep — builds on image_render.jl + read_slab
include("movie_rail.jl")     # movie rail (record button + batch) routed through the offline renderer, off napari
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
# The console streams the backend's OWN @info/@warn/@error (startup banner, handler faults, napari
# warnings, …), not just task logs — plus, since the log rail landed, every line the napari bridge,
# the preview worker and the notebook server print, and everything the detached runner says over its
# own stream. Installed in `start()` only (never under CECELIA_NO_SERVE) so the test harness keeps
# the plain logger.
#
# The logger, the record shape and the ring all live in the PACKAGE (`app/src/log_stream.jl`), because
# the runner needs the identical three and a second copy is how the two consoles would drift. This
# file supplies only the SINK: stamp into the ring, broadcast the stamped record.
const _log_ring = Cecelia.LogRing()

# One record → the ring (which stamps `seq` + `ts`) → the wire. The BROADCAST carries the stamped copy,
# so a client can spot a gap in `seq` and ask for it; see `LogRing`'s docstring for why log lines need
# that when task frames do not.
function _log_sink(rec::Dict{String,Any})
    stamped = Cecelia.log_ring_push!(_log_ring, rec)
    broadcast_ws(merge(stamped, Dict{String,Any}("type" => "server:log")))
end

_install_log_tee!() = Cecelia.install_log_tee!(_log_sink)

# GET /api/logs/recent[?since=<seq>] → { logs: [{seq,ts,level,source,message,detail?}, …], seq, ringId }
# `since` omitted → the whole ring (a cold console's backfill); `since=n` → only what came after, which
# is how a client repairs a gap it detected in the live stream without re-adding what it already has.
# `ringId` tells a client whether its cursor still refers to THIS ring — see `LogRing`.
function api_logs_recent(req::HTTP.Request)
    since = tryparse(Int, get(HTTP.queryparams(HTTP.URI(req.target)), "since", "0"))
    logs  = Cecelia.log_ring_since(_log_ring, since === nothing ? 0 : max(since, 0))
    200, JSON3.write((; logs = logs, seq = Cecelia.log_ring_seq(_log_ring),
                        ringId = Cecelia.log_ring_id(_log_ring)))
end

# ── Chain event → WS bridge ───────────────────────────────────────────────────
# The frames themselves — and the terminal-outcome banking that goes with them — are built by
# `subscribe_chain_frames!` in the package, so the detached runner emits byte-identical frames from its
# own process rather than carrying a second copy of this. That copy is the thing to avoid: the builder
# BANKS (`record_task_outcome!`), and a chain run emits no `task:status` at all, so this is one of only
# two carriers that can make a chain node's outcome recoverable. See app/src/runner/chain_frames.jl.
#
# Registered unconditionally, for the in-process path (the runner disabled, or a chain that fell back).
# When a chain runs on the runner these never fire here — the events happen in that process — and the
# frames arrive over the relay instead.
subscribe_chain_frames!(broadcast_ws)

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
    # Not under /api/images: it answers for sets and projects too, and it is the only route that
    # starts from a uid with no project in hand (see api_objects_find).
    "/api/objects/find" => (req, body_bytes) -> (api_objects_find(req)),
    "/api/images/tasklog" => (req, body_bytes) -> (api_images_tasklog(req)),
    "/api/tasks/history" => (req, body_bytes) -> (api_tasks_history(req)),
    "/api/tasks/recent" => (req, body_bytes) -> (api_tasks_recent(req)),
    "/api/qc/cohort" => (req, body_bytes) -> (api_qc_cohort(req)),
    "/api/qc/cohort/runs" => (req, body_bytes) -> (api_qc_cohort_runs(req)),
    "/api/analysis/lineage" => (req, body_bytes) -> (api_analysis_lineage(req)),
    "/api/analysis/populations" => (req, body_bytes) -> (api_analysis_populations(req)),
    "/api/analysis/boards" => (req, body_bytes) -> (api_analysis_boards(req)),
    # NB same path as the POST autosave, different method — the cheap reload behind a 409'd write and
    # the boards:changed broadcast (see api_projects_boards_get).
    "/api/projects/boards" => (req, body_bytes) -> (api_projects_boards_get(req)),
    "/api/analysis/measures" => (req, body_bytes) -> (api_analysis_measures(req)),
    "/api/analysis/behaviour" => (req, body_bytes) -> (api_analysis_behaviour(req)),
    "/api/analysis/clusters" => (req, body_bytes) -> (api_analysis_clusters(req)),
    "/api/analysis/spatial" => (req, body_bytes) -> (api_analysis_spatial(req)),
    "/api/analysis/chains" => (req, body_bytes) -> (api_analysis_chains(req)),
    "/api/repl/api" => (req, body_bytes) -> (api_repl_api(req)),
    "/api/observer/briefing" => (req, body_bytes) -> (api_observer_briefing(req)),
    "/api/observer/labarchives" => (req, body_bytes) -> (api_observer_labarchives(req)),
    "/api/mcp/connections" => (req, body_bytes) -> (api_mcp_connections(req)),
    "/api/lablog" => (req, body_bytes) -> (api_lablog_read(req)),
    "/api/tasks/definitions" => (req, body_bytes) -> (api_task_definitions(req)),
    "/api/maintenance/patches" => (req, body_bytes) -> (api_maintenance_patches(req)),
    "/api/tasks/custom-modules" => (req, body_bytes) -> (api_custom_modules_status(req)),
    "/api/profiles" => (req, body_bytes) -> (api_view_profiles(req)),
    "/api/tasks/funparams" => (req, body_bytes) -> (api_task_fun_params(req)),
    "/api/pools" => (req, body_bytes) -> (api_pools_list(req)),
    "/api/tasks/threads" => (req, body_bytes) -> (api_task_threads_get(req)),
    "/api/runner/status" => (req, body_bytes) -> (api_runner_status(req)),
    "/api/storage/compressor" => (req, body_bytes) -> (api_compressor_get(req)),
    "/api/storage/layout" => (req, body_bytes) -> (api_store_layout_get(req)),
    "/api/tasks" => (req, body_bytes) -> (api_tasks_list(req)),
    "/api/chains" => (req, body_bytes) -> (api_chains_list(req)),
    "/api/chains/get" => (req, body_bytes) -> (api_chains_get(req)),
    "/api/chains/runs" => (req, body_bytes) -> (api_chains_runs(req)),
    "/api/chains/run" => (req, body_bytes) -> (api_chains_run(req)),
    "/api/logs/recent" => (req, body_bytes) -> (api_logs_recent(req)),
    "/api/observer/status" => (req, body_bytes) -> (api_observer_status(req)),
    "/api/napari/status" => (req, body_bytes) -> (api_napari_status(req)),
    "/api/napari/gpu" => (req, body_bytes) -> (api_napari_gpu_get(req)),
    "/api/preview/status" => (req, body_bytes) -> (api_preview_status(req)),
    "/api/optical-flow/models" => (req, body_bytes) -> (api_optical_flow_models(req)),
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
    "/api/viewer/meta" => (req, body_bytes) -> (api_viewer_meta(req)),
    "/api/viewer/overlays" => (req, body_bytes) -> (api_viewer_overlays(req)),
    "/api/viewer/props" => (req, body_bytes) -> (api_viewer_props_get(req)),
    "/api/plots/umap" => (req, body_bytes) -> (api_plots_umap(req)),
    "/api/plots/definitions" => (req, body_bytes) -> (api_plot_definitions(req)),
    "/api/plots/populations" => (req, body_bytes) -> (api_plot_populations(req)),
    "/api/plots/attrs" => (req, body_bytes) -> (api_plot_attrs(req)),
    "/api/tracking/motion-dims" => (req, body_bytes) -> (api_motion_dims(req)),
    "/api/tracking/issues" => (req, body_bytes) -> (api_track_issues(req)),
    "/api/tracking/paths" => (req, body_bytes) -> (api_track_paths(req)),
    "/api/tracking/diagnostics" => (req, body_bytes) -> (api_track_diagnostics(req)),
    "/api/tracking/selection" => (req, body_bytes) -> (api_track_selection(req)),
    "/api/storage/summary" => (req, body_bytes) -> (api_storage_summary(req)),
    "/api/movies" => (req, body_bytes) -> (api_movies_list(req)),
    "/api/movies/meta" => (req, body_bytes) -> (api_movies_meta_get(req)),
)

# ── POST ─────────────────────────────────────────────────────────────────────
const _POST_ROUTES = Dict{String, Function}(
    "/api/projects/list" => (req, body_bytes) -> (api_projects_list(req)),
    "/api/pools/set" => (req, body_bytes) -> (api_pool_set(body_bytes)),
    "/api/tasks/threads/set" => (req, body_bytes) -> (api_task_threads_set(body_bytes)),
    "/api/runner/restart" => (req, body_bytes) -> (api_runner_restart(body_bytes)),
    "/api/runner/enabled" => (req, body_bytes) -> (api_runner_set_enabled(body_bytes)),
    "/api/storage/compressor/set" => (req, body_bytes) -> (api_compressor_set(body_bytes)),
    "/api/storage/layout/set" => (req, body_bytes) -> (api_store_layout_set(body_bytes)),
    "/api/tasks/custom-modules/reload" => (req, body_bytes) -> (api_custom_modules_reload(body_bytes)),
    "/api/plugins/install" => (req, body_bytes) -> (api_plugins_install(body_bytes)),
    "/api/plugins/install-local" => (req, body_bytes) -> (api_plugins_install_local(body_bytes)),
    "/api/plugins/remove"  => (req, body_bytes) -> (api_plugins_remove(body_bytes)),
    "/api/profiles/save" => (req, body_bytes) -> (api_view_profile_save(body_bytes)),
    "/api/profiles/delete" => (req, body_bytes) -> (api_view_profile_delete(body_bytes)),
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
    "/api/sets/rename" => (req, body_bytes) -> (api_sets_rename(body_bytes)),
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
    "/api/observer/labarchives/set" => (req, body_bytes) -> (api_observer_labarchives_set(body_bytes)),
    "/api/lablog/dismiss" => (req, body_bytes) -> (api_lablog_dismiss(body_bytes)),
    "/api/images/meta/resync" => (req, body_bytes) -> (api_images_meta_resync(body_bytes)),
    "/api/images/labels/delete" => (req, body_bytes) -> (api_images_delete_labels(body_bytes)),
    "/api/chains/save" => (req, body_bytes) -> (api_chains_save(body_bytes)),
    "/api/chains/create" => (req, body_bytes) -> (api_chains_create(body_bytes)),
    # create-only: adds ONE board, never edits one. NOT /api/projects/boards, which overwrites the
    # whole document (see api_boards_add).
    "/api/boards/add" => (req, body_bytes) -> (api_boards_add(body_bytes)),
    "/api/chains/rename" => (req, body_bytes) -> (api_chains_rename(body_bytes)),
    "/api/chains/delete" => (req, body_bytes) -> (api_chains_delete(body_bytes)),
    "/api/optical-flow/inspect" => (req, body_bytes) -> (api_optical_flow_inspect(body_bytes)),
    "/api/optical-flow/rename" => (req, body_bytes) -> (api_optical_flow_rename(body_bytes)),
    "/api/optical-flow/delete" => (req, body_bytes) -> (api_optical_flow_delete(body_bytes)),
    "/api/notebooks/launch" => (req, body_bytes) -> (api_notebooks_launch(body_bytes)),
    "/api/notebooks/write" => (req, body_bytes) -> (api_notebooks_write(body_bytes)),
    "/api/notebooks/create" => (req, body_bytes) -> (api_notebooks_create(body_bytes)),
    "/api/notebooks/describe" => (req, body_bytes) -> (api_notebooks_describe(body_bytes)),
    "/api/notebooks/delete" => (req, body_bytes) -> (api_notebooks_delete(body_bytes)),
    "/api/movies/meta" => (req, body_bytes) -> (api_movies_meta_set(body_bytes)),
    "/api/movies/delete" => (req, body_bytes) -> (api_movies_delete(body_bytes)),
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
    "/api/viewer/props" => (req, body_bytes) -> (api_viewer_props_post(body_bytes)),
    "/api/viewer/pick-cell" => (req, body_bytes) -> (api_viewer_pick_cell(body_bytes)),
    "/api/viewer/pick-rect" => (req, body_bytes) -> (api_viewer_pick_rect(body_bytes)),
    "/api/viewer/pick-clear" => (req, body_bytes) -> (api_viewer_pick_clear(body_bytes)),
    "/api/viewer/record-test" => (req, body_bytes) -> (api_viewer_record_test(body_bytes)),
    "/api/viewer/thumbnail" => (req, body_bytes) -> (api_viewer_thumbnail(body_bytes)),
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
    "/api/napari/set-z-view" => (req, body_bytes) -> (api_napari_set_z_view(body_bytes)),
    "/api/napari/set-3d-level" => (req, body_bytes) -> (api_napari_set_3d_level(body_bytes)),
    "/api/napari/centre" => (req, body_bytes) -> (api_napari_centre(body_bytes)),
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
    "/api/gating/pop/move" => (req, body_bytes) -> (api_gating_pop_move(body_bytes)),
    "/api/gating/copy" => (req, body_bytes) -> (api_gating_copy(body_bytes)),
    "/api/gating/undo" => (req, body_bytes) -> (api_gating_undo(body_bytes)),
    "/api/gating/redo" => (req, body_bytes) -> (api_gating_redo(body_bytes)),
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
    write_http_body!(stream, data)
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
    write_http_body!(stream, data)
    true
end

# Copy `n` bytes from `io` to the response stream in bounded chunks — never slurp a whole movie (or a
# large range slice) into memory just to hand it to the socket.
#
# 64 KB writes are only HALF of what makes that true, and for a long time the other half was missing:
# HTTP.jl BUFFERS the entire body of any response that carries a `Content-Length`
# (`_server_stream_buffered_fixed_h1` — the head is only written at `closewrite`, so the body has
# nowhere to go until then), and it streams straight to the socket only when the response is CHUNKED.
# So this loop handed its bounded chunks to a buffer that grew to the whole file. Measured, serving one
# file through both framings: `Content-Length` peaked at +390 MB for a 210 MB file and +1022 MB for a
# 420 MB one (~2.4x the file, buffer plus copies), against a FLAT ~30 MB either chunked or with the
# range clamped. Confirmed through the ROUTE itself (`/api/movies/file`, warmed up so JIT is not being
# measured): +506 MB before, +24 MB after, on the same 210 MB file. `_movie_plan` is what keeps it flat.
function _stream_file!(stream::HTTP.Stream, io::IO, n::Integer)
    remaining = Int(n)
    buf = Vector{UInt8}(undef, 64 * 1024)
    while remaining > 0 && !eof(io)
        nread = readbytes!(io, buf, min(length(buf), remaining))
        nread == 0 && break
        write_http_body!(stream, view(buf, 1:nread))
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

# How much of a movie ONE response may carry. A `Content-Length` response is buffered whole by HTTP.jl
# (see `_stream_file!`), so this is the ceiling on that buffer — the reason memory no longer tracks the
# file size. 8 MB is ~7 responses for the largest movie in a real project (57 MB) and a handful of
# extra `open`s; a player that seeks was going to issue several range requests anyway.
const MOVIE_RANGE_MAX = 8 * 1024 * 1024

"""
    _movie_plan(range_header, total) -> (status, start, stop, framing)

The response plan for a movie GET: which bytes, and how they are FRAMED. Pure, because framing is what
bounds memory and `api/test` has no live server to measure through — so the rules are asserted here
instead of on a socket.

  * a Range request → `206` for at most `MOVIE_RANGE_MAX` bytes, `:length` (a `Content-Length`, which a
    `<video>` needs to seek). Short is legal and ordinary: `Content-Range` tells the player what it
    got, and it asks for the next slice. This is the clamp that bounds the buffer.
  * no Range → `200` for the whole file, `:chunked`. A 200 must carry the WHOLE body, so it cannot be
    clamped — but omitting `Content-Length` moves it to chunked framing, which HTTP.jl streams instead
    of buffering. `Accept-Ranges` still rides on the response, so a player can switch to ranges to seek.
"""
function _movie_plan(range_header::AbstractString, total::Integer)
    rng = _parse_range(range_header, total)
    rng === nothing && return (200, 0, Int(total) - 1, :chunked)
    start, stop = rng
    (206, start, min(stop, start + MOVIE_RANGE_MAX - 1), :length)
end

# Serve a rendered project movie as video/mp4 for the /movies player, honouring HTTP Range so seeking
# works. GET /api/movies/file?projectUid=…&name=….mp4 — a Range → 206 with Content-Range for at most
# `MOVIE_RANGE_MAX`; no Range → 200, chunked, whole file. `_movie_plan` owns both rules and why.
# This is the server's ONLY range-capable route, and a <video> element issues Range requests in every
# browser, so this is what makes scrubbing work at all. Returns true iff it wrote a response.
function try_serve_movie(stream::HTTP.Stream, target::AbstractString)::Bool
    q = HTTP.queryparams(HTTP.URI(target))
    uid = get(q, "projectUid", ""); name = get(q, "name", "")
    (isempty(uid) || !_valid_movie_name(name)) && return false
    f = joinpath(_movies_dir_for_project(String(uid)), String(name))
    isfile(f) || return false
    total = filesize(f)
    status, start, stop, framing = _movie_plan(HTTP.header(stream.message, "Range", ""), total)
    n = stop - start + 1

    HTTP.setheader(stream, "Content-Type"                => "video/mp4")
    HTTP.setheader(stream, "Accept-Ranges"               => "bytes")
    HTTP.setheader(stream, "Access-Control-Allow-Origin" => "*")
    if status == 206
        HTTP.setheader(stream, "Content-Range" => "bytes $start-$stop/$total")
    end
    # `Content-Length` ONLY on the clamped 206 — setting it is what makes HTTP.jl buffer the body, so
    # the unclamped 200 deliberately goes out chunked instead (see `_movie_plan`/`_stream_file!`).
    framing == :length && HTTP.setheader(stream, "Content-Length" => string(n))
    HTTP.setstatus(stream, status)
    HTTP.startwrite(stream)
    open(f) do io
        start > 0 && seek(io, start)
        _stream_file!(stream, io, n)
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
        elseif spath == "/api/viewer/slab"
            try_serve_slab(stream, req.target) && return          # raw voxels + Content-Encoding
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
    write_http_body!(stream, body)
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

# Stop when our SUPERVISOR goes away without stopping us.
#
# `dev.jl` runs the backend in its own process group (so a terminal Ctrl-C reaches only the
# supervisor, which then stops us in an orderly way — see its `detach = true` comment). The price of
# that isolation is that a supervisor killed OUTRIGHT — `kill -9`, or the terminal window closed —
# can no longer take us with it, and we would sit on :8080 with no UI attached and nothing able to
# reach us. Watching for it is the cheap half of the trade.
#
# Reparenting is the signal: when our parent dies we are re-parented (to init/systemd), so `getppid`
# changes. Polled, because there is no portable "parent died" notification. Unix only — Windows has no
# `getppid`; there `pixi run stop` remains the recovery, which is what it was everywhere before.
function _watch_supervisor!()
    (haskey(ENV, "CECELIA_SUPERVISED") && !Sys.iswindows()) || return
    parent = ccall(:getppid, Cint, ())
    parent > 1 || return                      # already orphaned, or no supervisor to speak of
    # EVERY step here is wrapped, because of WHERE this runs: the supervisor owned our stdout/stderr,
    # so by the time this fires those streams are usually a closed pipe or a dead PTY (the terminal
    # window was closed — that is the case this exists for). An unguarded `@warn` then throws EIO,
    # kills this task, and the shutdown it was about to do never happens. Measured exactly that: the
    # watchdog fired, died on its own log line, and left the backend holding :8080.
    errormonitor(Threads.@spawn while true
        sleep(2)
        ccall(:getppid, Cint, ()) == parent && continue
        try; @warn "Supervisor exited without stopping us — shutting down" was_parent = parent; catch; end
        try; _stop_children_for_exit(); catch; end
        _exit_now(0)                                   # flushes defensively; never throws
    end)
    nothing
end

# Ctrl-C must stop this server's CHILDREN too, not just this process. Nothing else will: napari
# (:7655), the preview worker (:7656), Pluto (:7660) and the task runner (:7657) are grandchildren in
# their own process groups, and an in-flight task's Python child is only reparented. Before this hook,
# a Ctrl-C left every one of them running — verified live: the task runner survived on :7657 with
# nothing able to reach it again.
#
# **It has to be an `atexit` hook, not a caught `InterruptException`.** Julia's default for a
# non-interactive process does not unwind to us, and the obvious fix — `exit_on_sigint(false)` so it
# throws instead — is WRONG here and was measured to be: under `-t auto` the InterruptException is
# delivered to whichever task is at a safepoint, routinely an idle worker inside the scheduler's own
# `poptask`/`task_done_hook`. Nothing there handles it, so the process dies on the spot with
# `fatal: error thrown and no exception handler available` — skipping the teardown entirely, which is
# worse than the bug. `api/test` asserts this file contains no `exit_on_sigint`, to keep it out.
#
# An `atexit` hook has none of that fragility: `jl_exit` runs hooks, and it was verified to run on
# SIGINT *and* SIGTERM, with worker threads idle *and* busy. So the children are stopped however we
# are asked to go.
#
# The Quit/Restart routes are unaffected: they call `_stop_children_for_exit` themselves and leave via
# `_exit_now`, which skips atexit — so the teardown runs exactly once, in the order those routes want
# (`stop_runner = false` for a restart), and never twice.
#
# Registered INSIDE `start`, so `CECELIA_NO_SERVE=1` (the test suite, the REPL) never installs it.
function start(; host=HOST, port=PORT)
    _BOUND_HOST[] = string(host)
    _install_log_tee!()   # tee server logs to the WS console (only when actually serving)
    _start_runner!()      # launch or ADOPT the detached task runner (no-op unless CECELIA_RUNNER=1)
    # Guarded for the same reason as `_watch_supervisor!`: on a Ctrl-C or a closed window our streams
    # may already be gone, and a throwing log line here would take the teardown with it.
    atexit() do
        try; @info "Server exiting — stopping children"; catch; end
        try; _stop_children_for_exit(); catch; end
    end
    _watch_supervisor!()
    @info "CeceliaAPI starting" host port threads=Threads.nthreads() projects_dir=projects_dir()
    HTTP.listen(handle_stream, host, port)
end

# Auto-start on load — EXCEPT when `CECELIA_NO_SERVE` is set, so `api/test/runtests.jl` can `include`
# this file to get the handlers (and shared state like `_BOUND_HOST`) without binding a socket.
haskey(ENV, "CECELIA_NO_SERVE") || start()
