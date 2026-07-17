# ── Observer session sidecar — per-project assistant session + cumulative token usage ─────────────
# Persists the in-app observer's session id (for `--resume` continuity) and running token totals, so
# the UI can show REAL usage (from the agent's own output) and "clear context" can reset it. One
# sidecar per project, alongside the lab-log mutes/tuning configs. See
# docs/todo/OBSERVER_INTEGRATION_PLAN.md (Decisions 3 + 4).

observer_session_path(proj::CciaProject)::String =
    joinpath(proj.root, "settings", "observer-session.json")

_observer_session_base() = Dict{String,Any}(
    "sessionId" => "", "inputTokens" => 0, "outputTokens" => 0, "turns" => 0)

function read_observer_session(proj::CciaProject)::Dict{String,Any}
    p = observer_session_path(proj)
    isfile(p) || return _observer_session_base()
    try
        merge(_observer_session_base(),
              Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(p, String))))
    catch
        _observer_session_base()   # corrupt/legacy → treat as fresh rather than throwing on load
    end
end

function _write_observer_session(proj::CciaProject, s::Dict{String,Any})::Dict{String,Any}
    p = observer_session_path(proj)
    mkpath(dirname(p))
    open(p, "w") do io; JSON3.write(io, s); end
    s
end

# Fold one completed turn into the session: adopt its session id (so the next run `--resume`s it) and
# accumulate tokens. An empty `session_id` (agent returned none) keeps the existing one.
function record_observer_turn!(proj::CciaProject, session_id::AbstractString,
                               input_tokens::Integer, output_tokens::Integer)::Dict{String,Any}
    s = read_observer_session(proj)
    isempty(session_id) || (s["sessionId"] = String(session_id))
    s["inputTokens"]  = Int(s["inputTokens"])  + Int(input_tokens)
    s["outputTokens"] = Int(s["outputTokens"]) + Int(output_tokens)
    s["turns"]        = Int(s["turns"]) + 1
    _write_observer_session(proj, s)
end

# Clear context: drop the session id (the next run starts a FRESH session, so no stale assumptions
# carry over) and zero the token totals.
clear_observer_session!(proj::CciaProject)::Dict{String,Any} =
    _write_observer_session(proj, _observer_session_base())
