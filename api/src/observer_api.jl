# ── Observer (in-app AI assistant) API ────────────────────────────────────────────────────────────
# The in-app driver for the MCP observer: spawn a headless assistant that reads project state and
# appends to the lab log via the cecelia-observer MCP. Phase 1 = the one-shot "give feedback" button.
# See docs/todo/OBSERVER_INTEGRATION_PLAN.md and app/src/ai/agent_runner.jl.
#
#   GET  /api/observer/status    → { available } — is an assistant CLI present (drives the UI gate:
#                                   controls render disabled-with-why when false, not hidden).
#   POST /api/observer/feedback  → run ONE assistant turn on {projectUid}; returns usage + its text.
#                                   The lab-log write is a side effect the agent performs through the
#                                   MCP append tool, so the frontend just refreshes the lab log after.

_observer_repo_root() = dirname(dirname(@__DIR__))              # api/src → api → repo root
_observer_mcp_dir()   = joinpath(_observer_repo_root(), "mcp")
_observer_api_url()   = "http://127.0.0.1:$(PORT)"

# (Re)write the MCP config the spawned agent loads — cheap, keeps the resolved paths current.
function _write_observer_mcp_config()::String
    cfg  = observer_mcp_config(_observer_mcp_dir(), python_bin_path(), _observer_api_url())
    path = joinpath(config_dir(), "observer-mcp.json")
    open(path, "w") do io; JSON3.write(io, cfg); end
    path
end

# status doubles as the per-project session/usage readout when given ?projectUid — one call drives
# both the availability gate and the token readout on the panel.
function api_observer_status(req::HTTP.Request)
    resp = Dict{String,Any}("available"    => agent_available(ClaudeAgent()),
                            "models"        => OBSERVER_MODELS,          # the picker's choices
                            "defaultModel"  => observer_default_model()) # config default (Sonnet)
    puid = get(HTTP.queryparams(HTTP.URI(req.target)), "projectUid", "")
    if !isempty(puid)
        proj = try load_project(puid) catch; nothing end
        proj === nothing || (resp["session"] = read_observer_session(proj))
    end
    200, JSON3.write(resp)
end

function api_observer_feedback(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    proj = try
        load_project(project_uid)                                # the agent will read this project
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    # allow-listed model (default Sonnet); the panel sends the user's pick, auto-Watch included.
    model = observer_valid_model(get(body, :model, ""))
    agent = ClaudeAgent(; model = model)
    if !agent_available(agent)
        return 200, JSON3.write((; ok = false, available = false,
            error = "No assistant CLI found. Install Claude Code (or set [ai] agent_bin) to enable this."))
    end
    sess = read_observer_session(proj)
    cfg_path = _write_observer_mcp_config()
    res = run_observer_turn(agent, observer_feedback_prompt(project_uid), cfg_path;
                            session_id = String(sess["sessionId"]))     # resume the project's session
    # accumulate real usage + adopt the session id for the next --resume (only on a clean turn)
    updated = res.ok ? record_observer_turn!(proj, res.session_id, res.input_tokens, res.output_tokens) : sess
    200, JSON3.write((; ok = res.ok, available = true, model = model,
                        message = res.text, error = res.error,
                        inputTokens = res.input_tokens, outputTokens = res.output_tokens,
                        session = updated))
end

# Clear context: reset the project's assistant session + token totals (next run starts fresh).
function api_observer_clear(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    puid = String(get(body, :projectUid, ""))
    isempty(puid) && return 400, JSON3.write((; error = "projectUid required"))
    proj = try load_project(puid) catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write((; ok = true, session = clear_observer_session!(proj)))
end
