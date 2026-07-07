using Cecelia
using HTTP
using JSON3

# ── Bootstrap ─────────────────────────────────────────────────────────────────

init_cecelia!()

# ── Sub-modules ───────────────────────────────────────────────────────────────

include("sockets.jl")
include("routes.jl")
include("napari_api.jl")
include("gating_api.jl")
include("plotting_api.jl")
include("tracking_api.jl")
include("update_api.jl")
include("repl_api.jl")

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

# ── Chain event → WS bridge ───────────────────────────────────────────────────

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
    ))
end)

subscribe_chain_events!("node:running", function(p)
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:running",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "params"     => p.params,
    ))
end)

subscribe_chain_events!("node:done", function(p)
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
    ))
end)

subscribe_chain_events!("node:failed", function(p)
    broadcast_ws(Dict{String,Any}(
        "type"       => "chain:node:failed",
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "status"     => p.status,
    ))
end)

# ── HTTP router ───────────────────────────────────────────────────────────────

function handle_http(req::HTTP.Request, body_bytes::Vector{UInt8})
    target = String(req.target)
    path   = split(HTTP.URI(target).path, '?')[1]
    method = req.method

    # ── GET ─────────────────────────────────────────────────────────────────────
    if method == "GET"
        status, body = if path == "/api/health"
            200, JSON3.write((; ok=true, version="CeceliaAPI"))
        elseif path == "/api/diagnostics"
            api_diagnostics(req)
        elseif path == "/api/diagnostics/packages"
            api_packages(req)
        elseif path == "/api/version"
            api_version(req)
        elseif path == "/api/update/check"
            api_update_check(req)
        elseif path == "/api/projects"
            api_projects_list(req)
        elseif path == "/api/fs/list"
            api_fs_list(req)
        elseif path == "/api/images/meta"
            api_images_meta(req)
        elseif path == "/api/tasks/definitions"
            api_task_definitions(req)
        elseif path == "/api/tasks/funparams"
            api_task_fun_params(req)
        elseif path == "/api/pools"
            api_pools_list(req)
        elseif path == "/api/tasks"
            api_tasks_list(req)
        elseif path == "/api/chains"
            api_chains_list(req)
        elseif path == "/api/chains/get"
            api_chains_get(req)
        elseif path == "/api/chains/runs"
            api_chains_runs(req)
        elseif path == "/api/chains/run"
            api_chains_run(req)
        elseif path == "/api/napari/status"
            api_napari_status(req)
        elseif path == "/api/gating/channels"
            api_gating_channels(req)
        elseif path == "/api/gating/popmap"
            api_gating_popmap(req)
        elseif path == "/api/gating/stats"
            api_gating_stats(req)
        elseif path == "/api/gating/membership"
            api_gating_membership(req)
        elseif path == "/api/gating/plotmeta"
            api_gating_plotmeta(req)
        elseif path == "/api/gating/plotdata"
            api_gating_plotdata(req)
        elseif path == "/api/gating/density"
            api_gating_density(req)
        elseif path == "/api/plots/umap"
            api_plots_umap(req)
        elseif path == "/api/plots/definitions"
            api_plot_definitions(req)
        elseif path == "/api/plots/populations"
            api_plot_populations(req)
        elseif path == "/api/plots/attrs"
            api_plot_attrs(req)
        elseif path == "/api/tracking/motion-dims"
            api_motion_dims(req)
        else
            404, JSON3.write((; error="Not found: $path"))
        end
        return status, body
    end

    # ── POST ────────────────────────────────────────────────────────────────────
    if method == "POST"
        return if path == "/api/projects/list"
            api_projects_list(req)
        elseif path == "/api/projects/create"
            api_projects_create(body_bytes)
        elseif path == "/api/projects/load"
            api_projects_load(body_bytes)
        elseif path == "/api/projects/save"
            api_projects_save(body_bytes)
        elseif path == "/api/projects/rename"
            api_projects_rename(body_bytes)
        elseif path == "/api/sets/create"
            api_sets_create(body_bytes)
        elseif path == "/api/sets/delete"
            api_sets_delete(body_bytes)
        elseif path == "/api/images/register"
            api_images_register(body_bytes)
        elseif path == "/api/images/delete"
            api_images_delete(body_bytes)
        elseif path == "/api/images/attr/create"
            api_images_attr_create(body_bytes)
        elseif path == "/api/images/attr/delete"
            api_images_attr_delete(body_bytes)
        elseif path == "/api/images/attr/set"
            api_images_attr_set(body_bytes)
        elseif path == "/api/images/channelnames"
            api_images_channelnames(body_bytes)
        elseif path == "/api/images/meta/set"
            api_images_meta_set(body_bytes)
        elseif path == "/api/images/inclusion/set"
            api_images_inclusion_set(body_bytes)
        elseif path == "/api/images/meta/resync"
            api_images_meta_resync(body_bytes)
        elseif path == "/api/images/labels/delete"
            api_images_delete_labels(body_bytes)
        elseif path == "/api/chains/save"
            api_chains_save(body_bytes)
        elseif path == "/api/chains/delete"
            api_chains_delete(body_bytes)
        elseif path == "/api/napari/open"
            api_napari_open(body_bytes)
        elseif path == "/api/napari/close"
            api_napari_close(body_bytes)
        elseif path == "/api/napari/screenshot"
            api_napari_screenshot(body_bytes)
        elseif path == "/api/napari/restart"
            api_napari_restart(body_bytes)
        elseif path == "/api/napari/show-labels"
            api_napari_show_labels(body_bytes)
        elseif path == "/api/napari/show-populations"
            api_napari_show_populations(body_bytes)
        elseif path == "/api/napari/show-tracks"
            api_napari_show_tracks(body_bytes)
        elseif path == "/api/napari/colour-labels"
            api_napari_colour_labels(body_bytes)
        elseif path == "/api/napari/start-selection"
            api_napari_start_selection(body_bytes)
        elseif path == "/api/napari/selection-scope"
            api_napari_selection_scope(body_bytes)
        elseif path == "/api/napari/stop-selection"
            api_napari_stop_selection(body_bytes)
        elseif path == "/api/napari/event"
            api_napari_event(body_bytes)
        elseif path == "/api/gating/pop/add"
            api_gating_pop_add(body_bytes)
        elseif path == "/api/gating/pop/set-gate"
            api_gating_pop_set_gate(body_bytes)
        elseif path == "/api/gating/pop/delete"
            api_gating_pop_delete(body_bytes)
        elseif path == "/api/gating/pop/update"
            api_gating_pop_update(body_bytes)
        elseif path == "/api/gating/pop/rename"
            api_gating_pop_rename(body_bytes)
        elseif path == "/api/plot_data"
            api_plot_data(body_bytes)
        elseif path == "/api/repl"
            api_repl(body_bytes)
        elseif path == "/api/repl/config"
            api_repl_config(body_bytes)
        elseif path == "/api/update/apply"
            api_update_apply(body_bytes)
        else
            404, JSON3.write((; error="Not found: $path"))
        end
    end

    405, JSON3.write((; error="Method not allowed: $method"))
end

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
        if !startswith(spath, "/api/") && spath != "/ws"
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
    @info "CeceliaAPI starting" host port threads=Threads.nthreads() projects_dir=projects_dir()
    HTTP.listen(handle_stream, host, port)
end

# Auto-start on load — EXCEPT when `CECELIA_NO_SERVE` is set, so `api/test/runtests.jl` can `include`
# this file to get the handlers (and shared state like `_BOUND_HOST`) without binding a socket.
haskey(ENV, "CECELIA_NO_SERVE") || start()
