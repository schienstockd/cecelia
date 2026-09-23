# ── Kiwi API — run a Kiwi turn from the app, stream its steps, keep its replies ──────────────────────
#
# docs/todo/KIWI_ASSISTANT_PLAN.md Phase 4. The cockpit's prompt box posts here; the turn itself is
# `run_kiwi_turn` (kiwi_turn.jl), run on a worker thread because it takes 30 s – 2 min:
#
#   POST /api/kiwi/turn          {projectUid, prompt, refs, reasoning?, model?, followUp?} → {turnId}
#                                followUp = an earlier turnId: continue ITS engine session (a follow-up
#                                question), with what it attached or validly cited counting as seen
#   WS   kiwi:step               {projectUid, turnId, step}      each tool call, "checking refs", "re-asking…"
#   WS   kiwi:done               {projectUid, turnId, turn}      the finished turn record (below)
#   POST /api/kiwi/turn/cancel   {turnId}                         kills the engine process
#   GET  /api/kiwi/turns         ?projectUid=                     the kept turns, newest last, + the running one
#   POST /api/kiwi/turns/clear   {projectUid}
#
# Open decision 3 (where replies persist) → `<project>/kiwi/turns.json`, the last `KIWI_TURNS_KEEP`
# turns: a turn costs minutes of quota, so a reload must not lose it, and the file is the record the
# eval harness can read back. A past claim is NOT a `KiwiRef` kind (yet) — it's a record, not an object.
# One turn per project at a time: a second POST while one runs is refused (409), because each turn
# spends the user's seat window and two in flight would race for the same tools.

const KIWI_TURNS_KEEP = 50
const _KIWI_RUNNING = Dict{String,Dict{String,Any}}()      # projectUid → the running turn's record
const _KIWI_LOCK = ReentrantLock()

# The engine for a turn. A Ref so the route tests can swap in a scripted fake (the Phase 1 contract)
# without spawning a model — the same seam `run_kiwi_turn(; agent)` already has.
const _KIWI_AGENT = Ref{Function}(model -> ClaudeAgent(; model = model))

_kiwi_turns_path(puid::AbstractString) = joinpath(projects_dir(), puid, "kiwi", "turns.json")

function _kiwi_read_turns(puid::AbstractString)::Vector{Any}
    p = _kiwi_turns_path(puid)
    isfile(p) || return Any[]
    v = try JSON3.read(read(p, String)) catch; return Any[] end
    v isa AbstractVector ? collect(v) : Any[]
end

function _kiwi_append_turn!(puid::AbstractString, rec::Dict{String,Any})
    lock(_KIWI_LOCK) do
        turns = _kiwi_read_turns(puid)
        push!(turns, rec)
        write_json_atomic(_kiwi_turns_path(puid), turns[max(1, end - KIWI_TURNS_KEEP + 1):end])
    end
end

_kiwi_now() = string(Dates.now())

# The conversation a follow-up continues: the earlier turn's engine session, and every ref that
# conversation has attached or validly cited (resolved + seen) — which a follow-up may cite without
# re-reading, since the engine saw them earlier in the same session. `nothing` when `tid` isn't a kept
# turn with a session (it failed before the engine answered, or it was cleared).
function _kiwi_conversation(puid::AbstractString, tid::AbstractString)
    turns = _kiwi_read_turns(puid)
    i = findlast(t -> string(get(t, "turnId", "")) == tid, turns)
    i === nothing && return nothing
    t = JSON3.read(JSON3.write(turns[i]), Dict{String,Any})
    reply = get(t, "reply", nothing)
    sid = reply isa AbstractDict ? string(get(reply, "sessionId", "")) : ""
    isempty(sid) && return nothing
    refs = Any[get(t, "priorRefs", Any[])...]
    append!(refs, [r["ref"] for r in get(t, "refs", Any[])])
    for c in get(reply, "claims", Any[]), r in get(c, "refs", Any[])
        get(r, "seen", false) == true && get(get(r, "result", Dict()), "ok", false) == true && push!(refs, r["ref"])
    end
    seen = Set{String}()
    (; sessionId = sid, refs = [r for r in refs if !(_kiwi_canon(r) in seen) && (push!(seen, _kiwi_canon(r)); true)])
end

_kiwi_project_ok(puid) = !isempty(puid) && _valid_asset_id(puid) && isfile(joinpath(projects_dir(), puid, "project.json"))

"""
    kiwi_start_turn(puid, prompt; refs, reasoning, model) -> (status, record)

Validate and launch one turn in the background; the pieces of `api_kiwi_turn` that don't parse HTTP.
Returns the record immediately (status `running`); `kiwi:step` / `kiwi:done` carry the rest.
"""
function kiwi_start_turn(puid::AbstractString, prompt::AbstractString; refs = Any[], reasoning::Bool = false,
                         model::AbstractString = observer_default_model(), follow_up::AbstractString = "")
    _kiwi_project_ok(puid) || return 404, Dict{String,Any}("error" => "no project $puid")
    isempty(strip(prompt)) && isempty(refs) && return 400, Dict{String,Any}("error" => "ask something or attach a ref")
    conv = isempty(follow_up) ? nothing : _kiwi_conversation(puid, follow_up)
    (!isempty(follow_up) && conv === nothing) &&
        return 400, Dict{String,Any}("error" => "that reply can’t be followed up — ask it fresh")
    agent = _KIWI_AGENT[](observer_valid_model(model))
    Cecelia.agent_available(agent) || return 503, Dict{String,Any}("error" => "No $(Cecelia.agent_label(agent)) CLI found — install it to ask Kiwi")
    rec = Dict{String,Any}(
        "turnId" => "kt-" * gen_uid(), "projectUid" => String(puid), "prompt" => String(prompt),
        "refs" => [Dict{String,Any}("ref" => r, "result" => resolve_kiwi_ref(puid, r)) for r in refs],
        "reasoning" => reasoning, "model" => observer_valid_model(model), "engine" => Cecelia.agent_label(agent),
        "status" => "running", "startedAt" => _kiwi_now(), "steps" => String[])
    if conv !== nothing
        rec["followUp"] = String(follow_up)
        rec["priorRefs"] = conv.refs
    end
    started = lock(_KIWI_LOCK) do
        haskey(_KIWI_RUNNING, puid) && return false
        _KIWI_RUNNING[String(puid)] = rec
        true
    end
    started || return 409, Dict{String,Any}("error" => "Kiwi is already working on this project")
    tid = rec["turnId"]
    start_job!(tid)
    Threads.@spawn _kiwi_run_turn!(rec, agent, refs; session_id = conv === nothing ? "" : conv.sessionId)
    200, rec
end

# The worker: run, broadcast steps, record, broadcast done. Never throws — a crash is a failed turn.
function _kiwi_run_turn!(rec::Dict{String,Any}, agent, refs; session_id::AbstractString = "")
    puid, tid = rec["projectUid"], rec["turnId"]
    step(s) = (push!(rec["steps"], s);
               broadcast_ws(Dict{String,Any}("type" => "kiwi:step", "projectUid" => puid, "turnId" => tid, "step" => s)))
    try
        out = run_kiwi_turn(puid, rec["prompt"]; refs, agent, reasoning = rec["reasoning"], session_id,
                            prior_refs = get(rec, "priorRefs", Any[]),
                            on_progress = step, on_process = p -> track_job!(tid, p))
        rec["reply"] = out
        rec["status"] = job_cancelled(tid) ? "cancelled" : (out["ok"] ? "done" : "failed")
    catch e
        rec["reply"] = nothing
        rec["status"] = job_cancelled(tid) ? "cancelled" : "failed"
        rec["error"] = sprint(showerror, e)
    finally
        rec["finishedAt"] = _kiwi_now()
        finish_job!(tid)
        try _kiwi_append_turn!(puid, rec) catch e; @warn "Kiwi: could not save turn $tid" exception = e end
        lock(_KIWI_LOCK) do; delete!(_KIWI_RUNNING, puid); end
        broadcast_ws(Dict{String,Any}("type" => "kiwi:done", "projectUid" => puid, "turnId" => tid, "turn" => rec))
    end
    nothing
end

function api_kiwi_turn(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    refs = get(body, :refs, Any[])
    refs isa AbstractVector || return 400, JSON3.write((; error = "refs must be a list"))
    status, out = kiwi_start_turn(_wstr(body, :projectUid), _wstr(body, :prompt); refs = collect(refs),
                                  reasoning = get(body, :reasoning, false) == true,
                                  model = _wstr(body, :model, observer_default_model()),
                                  follow_up = _wstr(body, :followUp, ""))
    status, JSON3.write(out)
end

function api_kiwi_turn_cancel(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    tid = _wstr(body, :turnId)
    running = lock(_KIWI_LOCK) do; any(r -> r["turnId"] == tid, values(_KIWI_RUNNING)); end
    running || return 404, JSON3.write((; error = "no running turn $tid"))
    cancel_job!(tid)
    200, JSON3.write((; ok = true))
end

function api_kiwi_turns(req::HTTP.Request)
    puid = get(HTTP.queryparams(HTTP.URI(req.target)), "projectUid", "")
    _kiwi_project_ok(puid) || return 404, JSON3.write((; error = "no project $puid"))
    running = lock(_KIWI_LOCK) do; get(_KIWI_RUNNING, puid, nothing); end
    200, JSON3.write((; turns = _kiwi_read_turns(puid), running))
end

function api_kiwi_turns_clear(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    puid = _wstr(body, :projectUid)
    _kiwi_project_ok(puid) || return 404, JSON3.write((; error = "no project $puid"))
    lock(_KIWI_LOCK) do; rm(_kiwi_turns_path(puid); force = true); end
    200, JSON3.write((; ok = true))
end
