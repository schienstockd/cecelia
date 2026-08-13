# ── The detached task runner — server half ────────────────────────────────────
#
# A second Julia process that owns the resource pools and the task registry, so that restarting the
# API server does not kill work in flight. `api/runner.jl` is its launch script; `client.jl` is the
# API server's side of this conversation. Design + locked decisions:
# docs/todo/TASK_RUNNER_PLAN.md.
#
# **Deliberately shaped like the resident children we already have** (`preview.jl`, `napari.jl`):
# fixed port, launch-then-ping, adopt-if-already-listening, a protocol version that can refuse a
# process we did not start. A third way to run a sidecar would be the bug.
#
# Two transports, because the traffic is two different shapes:
#   • HTTP  — control. Submit, cancel, snapshot, pool limits. Request/reply, no session.
#   • WS    — the event stream. One long-lived subscriber (the API server) reading frames as work
#             progresses. The frames are the SAME shapes the API server already broadcasts to
#             browsers (`task:log` / `task:progress` / `task:status` / `task:result`), so the server
#             relays them verbatim rather than translating: no new client-facing protocol, and
#             `ws.ts` / the task store / the task console are untouched.
#
# What does NOT live here: chains (Phase 2 — the executor moves with them), background jobs (D8 — they
# stay in the app), and the task preview (D7 — it must never queue behind a full run).

const RUNNER_PORT = 7657

# Bumped whenever an ADOPTED older runner would answer differently — a changed reply shape, a changed
# route set, OR a bug fixed inside the runner. Same behavioural rule as `PREVIEW_PROTOCOL`, and for the
# same reason: the version is the only thing that can refuse a process we did not start, so anything we
# would not want served from old code has to move it.
#
# 1: initial — tasks only, no chains, no on-disk spool.
const RUNNER_PROTOCOL = 1

# ── Subscriber fan-out ────────────────────────────────────────────────────────
# Per-subscriber bounded queue drained by its own task, exactly as `api/src/server.jl` does. A task
# emitting log lines runs on a pool worker thread; writing the socket inline would let one stuck
# subscriber block that worker and, cascaded, wedge a pool slot. Frames are lossy-safe by design — a
# subscriber that misses one reconciles from `/tasks` + `/tasks/recent`.
const _RUNNER_OUT_CAP = 4096
const _runner_subs      = Dict{Any,Channel{String}}()
const _runner_subs_lock = ReentrantLock()

function _runner_sub_sender(ws, q::Channel{String})
    try
        for json in q
            HTTP.WebSockets.send(ws, json)
        end
    catch
        lock(_runner_subs_lock) do; delete!(_runner_subs, ws); end
        try; close(ws); catch; end
    end
end

_runner_subscriber_count()::Int = lock(_runner_subs_lock) do; length(_runner_subs); end

"""Broadcast one event frame to every subscriber. Never blocks the caller, never throws."""
function runner_emit(msg::Dict{String,Any})
    json = JSON3.write(msg)
    qs   = lock(_runner_subs_lock) do; collect(values(_runner_subs)); end
    for q in qs
        isopen(q) && Base.n_avail(q) < _RUNNER_OUT_CAP && try; put!(q, json); catch; end
    end
    nothing
end

# The frame shapes. Identical to the API server's `ws_*` helpers on purpose — the server relays these
# straight into its own sinks, so a field added on one side is a field the other already carries.
_emit_log(id, line)      = runner_emit(Dict{String,Any}("type" => "task:log", "taskId" => id, "line" => line))
_emit_progress(id, n, t) = runner_emit(Dict{String,Any}("type" => "task:progress", "taskId" => id,
                                                        "progress" => clamp(t > 0 ? n / t : 0.0, 0.0, 1.0)))
_emit_result(id, uid, m) = runner_emit(Dict{String,Any}("type" => "task:result", "taskId" => id,
                                                        "imageUid" => uid, "meta" => m))
# THIS IS A STATUS SINK, and therefore banks — exactly as `ws_status` does on the API server. The rule
# in docs/SCHEDULER.md is "bank at the rail's status SINKS, not at the producers", and the runner is a
# NEW carrier: its terminal frame is the only announcement that a task ended, and it is droppable. Left
# unbanked, `/tasks/recent` came back empty and an API server that restarted mid-run could never learn
# how the task finished — it would pin the row at `running` forever. That is the exact failure the
# outcome bank was built for, one process boundary further out.
#
# `startedAt`/`finishedAt` ride along for the same reason they do on the API server: this frame is the
# only live carrier of the task's timing, and a client that times the row from when it SAW the frame is
# timing from its own reconnect. `record_task_outcome!` returns the row it banked, so the live frame and
# the replayed one cannot disagree about when the task ran.
function _emit_status(id, status, uid, uids, fun)
    string(status) == "running" && note_task_started!(id)
    row = record_task_outcome!(id, status; image_uid = uid, image_uids = uids, fun = fun)
    runner_emit(Dict{String,Any}(
        "type" => "task:status", "taskId" => id, "status" => status,
        "imageUid" => uid, "imageUids" => uids, "fun" => fun,
        "startedAt"  => isnothing(row) ? iso_utc(task_started_at(id)) : row.started_at,
        "finishedAt" => isnothing(row) ? "" : row.finished_at))
end

# ── Chain runs ────────────────────────────────────────────────────────────────
#
# A chain run MUTATES `run.json` as it goes — per-node status, `params_hash`, the resume bookkeeping —
# so two processes executing the same run id would corrupt each other's state, silently and in a way
# that only shows up as a resume doing the wrong thing later. Today the enabled flag makes that
# impossible (exactly one path is live), but relying on that means the guard is a configuration
# accident rather than a property of the code.
#
# So the runner CLAIMS a run id for the duration and refuses a second submission of it. Only resumes
# need it: a fresh run's id does not exist until `run_chain` mints it, so it cannot collide with
# anything.
const _CHAIN_CLAIMS      = Set{String}()
const _CHAIN_CLAIMS_LOCK = ReentrantLock()

# Returns false if already held. Claim and check are one atomic step on purpose — a check-then-claim
# is precisely the race this exists to close.
function _claim_chain_run!(run_id::AbstractString)::Bool
    isempty(run_id) && return true                 # fresh run: nothing to collide with
    lock(_CHAIN_CLAIMS_LOCK) do
        String(run_id) in _CHAIN_CLAIMS ? false : (push!(_CHAIN_CLAIMS, String(run_id)); true)
    end
end

_release_chain_run!(run_id::AbstractString) =
    (isempty(run_id) || lock(_CHAIN_CLAIMS_LOCK) do; delete!(_CHAIN_CLAIMS, String(run_id)); end; nothing)

runner_chain_claims()::Vector{String} =
    lock(_CHAIN_CLAIMS_LOCK) do; sort(collect(_CHAIN_CLAIMS)); end

_emit_chain_log(line) = runner_emit(Dict{String,Any}("type" => "chain:log", "line" => line))

function _runner_submit_chain(body_bytes::Vector{UInt8})
    creq = try
        chain_request(_body_dict(body_bytes))
    catch e
        return _json(400, (; error = "bad request: " * sprint(showerror, e)))
    end
    isempty(creq.project_uid) && return _json(400, (; error = "projectUid required"))
    (isempty(creq.chain_name) && isempty(creq.run_id)) &&
        return _json(400, (; error = "chain or runId required"))
    _claim_chain_run!(creq.run_id) ||
        return _json(409, (; error = "run $(creq.run_id) is already executing on this runner"))

    Threads.@spawn begin
        try
            execute_chain(creq;
                on_log      = line -> (println(line); _emit_chain_log(line)),
                on_finished = (ok, err) -> runner_emit(Dict{String,Any}(
                    "type"  => ok ? "chain:run:done" : "chain:run:failed",
                    "chain" => creq.chain_name, "runId" => creq.run_id,
                    "error" => err)))
        catch e
            @error "Runner: chain execution escaped" chain = creq.chain_name exception = (e, catch_backtrace())
            runner_emit(Dict{String,Any}("type" => "chain:run:failed", "chain" => creq.chain_name,
                                         "runId" => creq.run_id, "error" => sprint(showerror, e)))
        finally
            _release_chain_run!(creq.run_id)
        end
    end
    _json(200, (; ok = true, chain = creq.chain_name, runId = creq.run_id))
end

function _runner_cancel_chain(body_bytes::Vector{UInt8})
    run_id = String(get(_body_dict(body_bytes), "runId", ""))
    isempty(run_id) && return _json(400, (; error = "runId required"))
    cancel_chain_run!(run_id)
    _json(200, (; ok = true))
end

# ── Process identity ──────────────────────────────────────────────────────────
# What `/ping` answers, so the API server can tell "the runner I started" from "a runner that outlived
# something", and — the point of D5 — WHICH CODE it is running. A runner that does not restart when you
# edit will keep running the bug you just fixed; the only defence is that it says so.
const _RUNNER_STARTED_AT = Ref(0.0)
const _RUNNER_COMMIT     = Ref("")
# The port ACTUALLY bound, not the constant. `api/runner.jl` honours CECELIA_RUNNER_PORT, so reporting
# `RUNNER_PORT` was a lie whenever it was overridden — /ping answered on 7697 and said 7657, which is
# precisely the field a client would use to find it.
const _RUNNER_BOUND_PORT = Ref(RUNNER_PORT)

# Through `git_probe`, not a hand-rolled shell-out: a `git` call gets ONE spelling here, with the
# stderr redirect in it by construction. The four inline copies this replaces were what printed
# "fatal: not a git repository" into a packaged app's launch console on every start (#540) — and a
# runner is spawned by that same packaged app, so a fifth copy would have reintroduced it.
_runner_repo_root() = dirname(dirname(dirname(@__DIR__)))   # app/src/runner → repo root
_runner_git_short() = git_probe("rev-parse", "--short", "HEAD"; dir = _runner_repo_root())

runner_identity()::Dict{String,Any} = Dict{String,Any}(
    "protocol"      => RUNNER_PROTOCOL,
    "pid"           => getpid(),
    "port"          => _RUNNER_BOUND_PORT[],
    "commit"        => _RUNNER_COMMIT[],
    "startedAt"     => _RUNNER_STARTED_AT[],
    "uptimeSeconds" => round(Int, time() - _RUNNER_STARTED_AT[]),
    "threads"       => Threads.nthreads(),
    "chainRuns"     => runner_chain_claims(),
    "projectsDir"   => projects_dir())

# ── Life span: the runner must not outlive its REASON ─────────────────────────
#
# It is launched detached so it survives a backend restart — which means nothing stops it. Left alone
# it is a Julia process holding GPU memory with no window, no cancel and no way for anyone to find it,
# forever. The work finishing is fine; the idle process afterwards is the problem.
#
# So it exits when it has neither WORK nor an AUDIENCE. Both conditions matter:
#   • work only        → it would exit between two tasks of a batch;
#   • subscriber only  → a backend restart drops the connection for ~45 s and would kill it, which is
#                        the exact thing it exists to survive.
#
# The window is therefore generous, and being generous costs only a cold start (~45 s of Julia +
# Cecelia) if you come back after a long gap.
const RUNNER_IDLE_EXIT_SECONDS = Ref(600.0)

_runner_has_work()::Bool = !isempty(list_tasks()) || !isempty(runner_chain_claims())

function _runner_idle_watchdog!()
    idle_since = Ref(time())
    Threads.@spawn while true
        sleep(15)
        try
            if _runner_has_work() || _runner_subscriber_count() > 0
                idle_since[] = time()
            elseif time() - idle_since[] > RUNNER_IDLE_EXIT_SECONDS[]
                @info "Task runner idle with nothing connected — exiting" after_seconds = round(Int, time() - idle_since[])
                _runner_remove_state_file()
                exit(0)
            end
        catch e
            @warn "Runner idle watchdog" exception = e   # never let this kill the runner
        end
    end
end

# A findable process. Without this a stray runner is folklore — you know the port, and that is all.
_runner_state_path() = joinpath(config_dir(), "runner.json")

function _runner_write_state_file(port::Integer)
    try
        ensure_config_dir()
        write_json_atomic(_runner_state_path(),
                          Dict{String,Any}("pid" => getpid(), "port" => Int(port),
                                           "commit" => _RUNNER_COMMIT[],
                                           "startedAt" => _RUNNER_STARTED_AT[]))
    catch e
        @warn "Could not write the runner state file" exception = e
    end
end

_runner_remove_state_file() = (try; rm(_runner_state_path(); force = true); catch; end; nothing)

# ── Routes ────────────────────────────────────────────────────────────────────

# Handlers return a plain `(status, json_string)` pair — never an `HTTP.Response` — so the stream
# handler below owns all of the wire mechanics in one place, exactly as `handle_stream` does in
# `api/src/server.jl`. Mirroring that shape is not style: writing to the stream from inside a handler
# is how this first version bound its port and then answered nothing at all.
_json(status::Int, body) = (status, JSON3.write(body))
_body_dict(body_bytes::Vector{UInt8}) = try
    JSON3.read(String(body_bytes), Dict{String,Any})
catch
    Dict{String,Any}()
end

"""
Accept a task and return immediately — submission is not execution.

The pool is what decides when the work actually starts, and a caller must not be held for the length
of a segmentation just to learn its id. So the run happens on its own task and everything after this
point is announced on the event stream.
"""
function _runner_submit(body_bytes::Vector{UInt8})
    d = _body_dict(body_bytes)
    treq = try
        task_request(d)
    catch e
        return _json(400, (; error = "bad request: " * sprint(showerror, e)))
    end
    isempty(treq.task_id)  && return _json(400, (; error = "taskId required"))
    isempty(treq.fun_name) && return _json(400, (; error = "funName required"))

    Threads.@spawn begin
        try
            execute_task(treq;
                on_log      = line -> _emit_log(treq.task_id, line),
                on_progress = (n, t) -> _emit_progress(treq.task_id, n, t),
                on_status   = (st, uid, uids) -> _emit_status(treq.task_id, st, uid, uids, treq.fun_name),
                on_result   = (uid, meta) -> _emit_result(treq.task_id, uid, meta))
        catch e
            # execute_task already guarantees a terminal frame on every path it knows about. This is
            # the backstop for a throw it cannot know about (an emit failing, a callback bug) — without
            # it the throw dies in this fire-and-forget spawn and the submitter's row pins at running
            # forever, which is the exact failure `_execute_job!`'s `finally` exists to prevent.
            @error "Runner: task execution escaped" task_id = treq.task_id exception = (e, catch_backtrace())
            _emit_log(treq.task_id, "[ERROR] Runner failure: " * sprint(showerror, e))
            _emit_status(treq.task_id, "failed", treq.image_uid, String[], treq.fun_name)
        end
    end
    _json(200, (; ok = true, taskId = treq.task_id))
end

function _runner_cancel(body_bytes::Vector{UInt8})
    task_id = String(get(_body_dict(body_bytes), "taskId", ""))
    isempty(task_id) && return _json(400, (; error = "taskId required"))
    cancel_task!(task_id)
    _json(200, (; ok = true))
end

function _runner_pools_set(body_bytes::Vector{UInt8})
    d     = _body_dict(body_bytes)
    name  = String(get(d, "name", ""))
    limit = try; Int(get(d, "limit", 0)); catch; 0; end
    (isempty(name) || limit < 1) && return _json(400, (; error = "name and limit (>=1) required"))
    # Rejects an unknown pool, so a typo can't accumulate in the runner's custom.toml.
    any(p -> p.name == name, list_pools()) || return _json(400, (; error = "unknown pool: $name"))
    set_pool_limit!(name, limit)
    _json(200, (; ok = true, pools = pool_status()))
end

function _runner_events(ws)
    q = Channel{String}(_RUNNER_OUT_CAP)
    lock(_runner_subs_lock) do; _runner_subs[ws] = q; end
    sender = Threads.@spawn _runner_sub_sender(ws, q)
    try
        # The subscriber sends nothing; this loop exists to notice the socket closing.
        for _ in ws; end
    catch
    finally
        lock(_runner_subs_lock) do; delete!(_runner_subs, ws); end
        try; close(q); catch; end
        try; wait(sender); catch; end
    end
end

function _runner_handler(req::HTTP.Request, body_bytes::Vector{UInt8})
    try
        uri   = HTTP.URI(req.target)
        route = uri.path
        if req.method == "GET"
            route == "/ping"  && return _json(200, runner_identity())
            # THE TASK-RAIL API, spelled exactly as the API server spells it — bare arrays, `/api/…`
            # paths. The runner IS a task-rail server, so it answers the questions a task-rail client
            # asks, and every existing reader points at it unchanged:
            #
            #     CECELIA_PORT=7657 pixi run console
            #
            # which is the only way to see what a runner is doing with no backend attached. The first
            # version invented `/tasks` + a `{tasks: …}` envelope for no reason; those stay as aliases
            # rather than being a second dialect anyone has to know about.
            route in ("/api/tasks", "/tasks")  && return _json(200, list_tasks())
            route in ("/api/pools", "/pools")  && return _json(200, sort(pool_status(), by = p -> p.name))
            route == "/chains" && return _json(200, (; runs = runner_chain_claims()))
            route in ("/api/tasks/recent", "/tasks/recent") &&
                return _json(200, recent_tasks(; since = get(HTTP.queryparams(uri), "since", "")))
            route == "/api/health" && return _json(200, (; ok = true, version = "CeceliaRunner"))
        elseif req.method == "POST"
            route == "/submit"       && return _runner_submit(body_bytes)
            route == "/cancel"       && return _runner_cancel(body_bytes)
            route == "/submit-chain" && return _runner_submit_chain(body_bytes)
            route == "/cancel-chain" && return _runner_cancel_chain(body_bytes)
            route == "/pools/set" && return _runner_pools_set(body_bytes)
        end
        _json(404, (; error = "no route: $(req.method) $route"))
    catch e
        @error "Runner: unhandled error" route = req.target exception = (e, catch_backtrace())
        _json(500, (; error = sprint(showerror, e)))
    end
end

# One request, start to finish. Same shape as `handle_stream` (api/src/server.jl): read the body, run
# the handler on the thread POOL so a slow one cannot stall the accept loop, then write. `list_tasks`
# and `pool_status` take locks a running task also takes, so "cannot stall the accept loop" is a live
# concern here and not a copied precaution.
function _runner_stream(stream::HTTP.Stream)
    req = stream.message
    if HTTP.WebSockets.isupgrade(req)
        HTTP.WebSockets.upgrade(_runner_events, stream; check_origin = (_, _) -> true)
        return
    end
    body_bytes = read(stream)
    status, body = fetch(Threads.@spawn begin
        try
            _runner_handler(req, body_bytes)
        catch e
            @error "Runner: unhandled error" exception = (e, catch_backtrace())
            _json(500, (; error = sprint(showerror, e)))
        end
    end)
    HTTP.setstatus(stream, status)
    HTTP.setheader(stream, "Content-Type" => "application/json")
    HTTP.startwrite(stream)
    write(stream, body)
end

"""
    runner_serve(; port = RUNNER_PORT, host = "127.0.0.1")

Bind and serve until killed. **Blocks** — this is the runner process's whole job.

Loopback by default for the same reason the API server is: this is a local app, and the control
surface runs arbitrary registered tasks with the user's project data. `host` is a parameter rather
than a constant so a remote target has somewhere to land (Phase 4) — but a non-loopback bind must not
ship without authentication. See docs/todo/TASK_RUNNER_PLAN.md → *HPC*, constraint 2.
"""
function runner_serve(; port::Int = RUNNER_PORT, host::AbstractString = "127.0.0.1")
    _RUNNER_STARTED_AT[]  = time()
    _RUNNER_COMMIT[]      = _runner_git_short()
    _RUNNER_BOUND_PORT[]  = port
    # The chain event bus is in-process, so a chain executing here fires its events here. Same builder
    # the API server uses, so the frames are byte-identical and it can relay them untranslated.
    subscribe_chain_frames!(runner_emit)
    _runner_write_state_file(port)
    atexit(_runner_remove_state_file)
    _runner_idle_watchdog!()
    @info "Cecelia task runner starting" host port pid=getpid() threads=Threads.nthreads() commit=_RUNNER_COMMIT[] projects_dir=projects_dir()
    HTTP.listen(_runner_stream, host, port)
end
