# ── Observer (in-app AI assistant) API ────────────────────────────────────────────────────────────
# The in-app driver for the MCP observer: spawn a headless assistant that reads project state and
# appends to the lab log via the cecelia-observer MCP. Phase 1 = the one-shot "give feedback" button.
# See docs/todo/OBSERVER_INTEGRATION_PLAN.md and app/src/ai/agent_runner.jl.
#
#   GET  /api/observer/status    → { available } — is an assistant CLI present (drives the UI gate:
#                                   controls render disabled-with-why when false, not hidden). Also
#                                   returns `mcpConfigPath`: the generated observer MCP config, so the
#                                   info panel can hand the user a ready `claude --mcp-config <path>`
#                                   line for their OWN terminal — no hand-registering an MCP server.
#   POST /api/observer/feedback  → run ONE assistant turn on {projectUid}; returns usage + its text.
#                                   The lab-log write is a side effect the agent performs through the
#                                   MCP append tool, so the frontend just refreshes the lab log after.
#   POST /api/observer/register  → one-click terminal setup: register (or re-sync) the observer MCP in
#                                   the user's own Claude Code config, so plain `claude` has the tools.
#                                   Idempotent (remove-then-add). The ONLY route that touches the
#                                   user's Claude config, and only on an explicit click.

_observer_repo_root() = dirname(dirname(@__DIR__))              # api/src → api → repo root
_observer_mcp_dir()   = joinpath(_observer_repo_root(), "mcp")
_observer_api_url()   = "http://127.0.0.1:$(PORT)"

# Detect whether an observer run actually appended a [Claude] entry (its MCP append is a side effect
# the API can't see directly) — diff the count of [Claude] lab-log entries before/after the run.
_claude_entry_count(proj) =
    count(e -> String(get(e, "author", "")) == "Claude", parse_lab_log(read_lab_log(proj)))
# Newest [Claude] entry's first line — the one-line badge preview for a fresh append.
function _newest_claude_line(proj)::String
    for e in parse_lab_log(read_lab_log(proj))                 # already newest-first
        String(get(e, "author", "")) == "Claude" || continue
        ls = get(e, "lines", String[])
        return isempty(ls) ? "" : String(first(ls))
    end
    ""
end

# (Re)write the MCP config the spawned agent loads — cheap, keeps the resolved paths current.
function _write_observer_mcp_config()::String
    cfg  = observer_mcp_config(_observer_mcp_dir(), python_bin_path(), _observer_api_url())
    path = joinpath(config_dir(), "observer-mcp.json")
    open(path, "w") do io; JSON3.write(io, cfg); end
    path
end

# The spec THIS install needs registered — same resolution as the --mcp-config file, so detection and
# registration can never disagree about what "set up" means.
_observer_want_spec() = observer_mcp_spec(_observer_mcp_dir(), python_bin_path(), _observer_api_url())

# `{state, ready}` for the user's own terminal. `state` ∈ missing/stale/current; `ready` is the single
# thing the UI branches on. A STALE registration counts as not-ready: it points at another checkout's
# interpreter or a different port, so the tools would silently fail to connect in the user's session —
# they need the same one-click re-sync, not a Chat button that appears to work.
function _observer_terminal_state()
    st = observer_registration_state(_observer_want_spec())
    Dict{String,Any}("state" => String(st), "ready" => st === :current)
end

# status doubles as the per-project session/usage readout when given ?projectUid — one call drives
# both the availability gate and the token readout on the panel.
function api_observer_status(req::HTTP.Request)
    resp = Dict{String,Any}("available"    => agent_available(ClaudeAgent()),
                            "models"        => OBSERVER_MODELS,          # the picker's choices
                            "defaultModel"  => observer_default_model(), # config default (Sonnet)
                            "prompt"        => observer_prompt_display(), # transparency: what it runs under
                            # written here (not only on a feedback run) so the info panel can always show
                            # the terminal one-liner, even before the user has ever run Ask Claude
                            "mcpConfigPath" => _write_observer_mcp_config(),
                            # is the user's OWN terminal set up? drives which button the lab-log toolbar
                            # shows (Set up my terminal vs Chat to Claude) — see _observer_terminal_state
                            "terminal"      => _observer_terminal_state())
    puid = get(HTTP.queryparams(HTTP.URI(req.target)), "projectUid", "")
    if !isempty(puid)
        proj = try load_project(puid) catch; nothing end
        proj === nothing || (resp["session"] = read_observer_session(proj))
    end
    200, JSON3.write(resp)
end

# One-click terminal setup. Registers the SAME server spec the in-app agent uses at Claude Code's
# `user` scope, so the biologist's next plain `claude` has the observer tools — nothing to copy, no
# path to mistype. Safe to click repeatedly: it removes first, so it doubles as a re-sync after the
# app moves or the port changes. Body is ignored (no inputs — the spec is entirely server-derived);
# `scope` is deliberately NOT client-settable.
function api_observer_register(::Vector{UInt8})
    agent = ClaudeAgent()
    if !agent_available(agent)
        return 200, JSON3.write((; ok = false, available = false,
            error = "No assistant CLI found. Install Claude Code to enable this."))
    end
    spec = _observer_want_spec()
    ok, message = register_observer_mcp(agent, JSON3.write(spec))
    # Report the state read back from the config, not the CLI's exit code alone — the UI flips its
    # button on `terminal.ready`, so it must reflect what's actually on disk.
    200, JSON3.write((; ok = ok, available = true, name = OBSERVER_MCP_NAME,
                        message = message, error = ok ? "" : message,
                        terminal = _observer_terminal_state()))
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
    # trigger: "manual" (Ask Claude button) or "auto" (Watch) — recorded in the activity log.
    trigger = String(get(body, :trigger, "manual"))
    agent = ClaudeAgent(; model = model)
    if !agent_available(agent)
        return 200, JSON3.write((; ok = false, available = false,
            error = "No assistant CLI found. Install Claude Code (or set [ai] agent_bin) to enable this."))
    end
    sess = read_observer_session(proj)
    cfg_path = _write_observer_mcp_config()
    before = _claude_entry_count(proj)                                  # to detect an actual append
    res = run_observer_turn(agent, observer_feedback_prompt(project_uid), cfg_path;
                            session_id = String(sess["sessionId"]))     # resume the project's session
    appended_line = _newest_claude_line(proj)
    appended = _claude_entry_count(proj) > before
    # accumulate real usage + adopt the session id for the next --resume (only on a clean turn)
    res.ok && (sess = record_observer_turn!(proj, res.session_id, res.input_tokens, res.output_tokens))
    # log EVERY pass (even a silent/failed one) so the activity readout shows what it did + cost
    updated = log_observer_pass!(proj; trigger = trigger, model = model, ok = res.ok,
                                 appended = appended, input_tokens = res.input_tokens,
                                 output_tokens = res.output_tokens,
                                 note = res.ok ? res.text : res.error)
    200, JSON3.write((; ok = res.ok, available = true, model = model, trigger = trigger,
                        message = res.text, error = res.error, appended = appended,
                        appendedLine = (appended ? appended_line : ""),
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
