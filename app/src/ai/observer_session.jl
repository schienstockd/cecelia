# ── Observer session sidecar — per-project assistant session + cumulative token usage ─────────────
# Persists the in-app observer's session id (for `--resume` continuity) and running token totals, so
# the UI can show REAL usage (from the agent's own output) and "clear context" can reset it. One
# sidecar per project, alongside the lab-log mutes/tuning configs. See
# docs/todo/OBSERVER_INTEGRATION_PLAN.md (Decisions 3 + 4).

import Dates

observer_session_path(proj::CciaProject)::String =
    joinpath(proj.root, "settings", "observer-session.json")

# `passes` is the activity log: one entry per observer run (auto OR manual), whether or not it wrote
# to the lab log — so the user can see "it looked and had nothing to say" + the token cost, not just
# silence. Bounded, newest-first.
_observer_session_base() = Dict{String,Any}(
    "sessionId" => "", "inputTokens" => 0, "outputTokens" => 0, "turns" => 0, "passes" => [])

const _OBSERVER_MAX_PASSES = 30
const _OBSERVER_NOTE_CAP   = 800

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

# Log one observer PASS for the activity readout — regardless of whether it appended to the lab log,
# so a silent "looked, nothing to flag" run is still visible (with its token cost). Newest-first,
# bounded. Separate from record_observer_turn! (which folds only a successful turn's usage/session).
function log_observer_pass!(proj::CciaProject; trigger::AbstractString, model::AbstractString,
                            ok::Bool, appended::Bool, input_tokens::Integer, output_tokens::Integer,
                            note::AbstractString)::Dict{String,Any}
    s = read_observer_session(proj)
    passes = s["passes"] isa AbstractVector ? collect(Any, s["passes"]) : Any[]
    pushfirst!(passes, Dict{String,Any}(
        "at"           => Dates.format(Dates.now(), Dates.dateformat"yyyy-mm-dd HH:MM:SS"),
        "trigger"      => String(trigger),
        "model"        => String(model),
        "ok"           => ok,
        "appended"     => appended,
        "inputTokens"  => Int(input_tokens),
        "outputTokens" => Int(output_tokens),
        "note"         => first(strip(String(note)), _OBSERVER_NOTE_CAP)))
    length(passes) > _OBSERVER_MAX_PASSES && (passes = passes[1:_OBSERVER_MAX_PASSES])
    s["passes"] = passes
    _write_observer_session(proj, s)
end

# Clear context: drop the session id (the next run starts a FRESH session, so no stale assumptions
# carry over), zero the token totals, and clear the activity log.
clear_observer_session!(proj::CciaProject)::Dict{String,Any} =
    _write_observer_session(proj, _observer_session_base())
