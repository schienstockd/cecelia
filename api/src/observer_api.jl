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
#                                   Idempotent (remove-then-add). Also clears `local`-scope entries that
#                                   would OVERRIDE ours (Claude resolves local before user — see
#                                   _observer_terminal_state). The ONLY route that touches the user's
#                                   Claude config, and only on an explicit click.

_observer_repo_root() = dirname(dirname(@__DIR__))              # api/src → api → repo root
_observer_mcp_dir()   = joinpath(_observer_repo_root(), "mcp")
_observer_api_url()   = "http://127.0.0.1:$(PORT)"

# (Re)write an MCP config — cheap, keeps the resolved paths current. Two files, two jobs:
#   observer-mcp.json          the user's OWN terminal (`claude --mcp-config <path>`, shown by status):
#                              pairs with their session, as before.
#   observer-mcp-headless.json the turns THIS app spawns (feedback/Watch): carries
#                              CECELIA_OBSERVER_NO_PAIR, so a throwaway `claude -p` can't overwrite the
#                              user's real pairing (see observer_mcp_config in app/src/ai/agent_runner.jl).
function _write_observer_mcp_config(; headless::Bool = false)::String
    cfg  = observer_mcp_config(_observer_mcp_dir(), python_bin_path(), _observer_api_url(); headless)
    name = headless ? "observer-mcp-headless.json" : "observer-mcp.json"
    path = joinpath(ensure_config_dir(), name)                  # may be a machine's first ever write
    write_json_atomic(path, cfg)
    path
end

# The spec THIS install needs registered — same resolution as the --mcp-config file, so detection and
# registration can never disagree about what "set up" means.
_observer_want_spec() = observer_mcp_spec(_observer_mcp_dir(), python_bin_path(), _observer_api_url())

# `{state, ready}` for the user's own terminal. `state` ∈ missing/stale/shadowed/current; `ready` is the
# single thing the UI branches on. A STALE registration counts as not-ready: it points at another
# checkout's interpreter or a different port, so the tools would silently fail to connect in the user's
# session — they need the same one-click re-sync, not a Chat button that appears to work.
#
# SHADOWED outranks a good user-scope entry: a leftover `local`-scope entry wins over ours in the
# directory it names, so the registration is correct and the user's session still has no tools. That
# looked like "the setup button does nothing". `shadowedDirs` names the folders so the UI can say which.
function _observer_terminal_state()
    want    = _observer_want_spec()
    st      = observer_registration_state(want)
    shadows = shadowing_observer_dirs(want)
    st === :current && !isempty(shadows) && (st = :shadowed)
    Dict{String,Any}("state" => String(st), "ready" => st === :current,
                     "shadowedDirs" => shadows)
end

# status: is an assistant CLI here, which models it offers, and is the user's own terminal set up.
function api_observer_status(req::HTTP.Request)
    resp = Dict{String,Any}("available"    => agent_available(ClaudeAgent()),
                            "models"        => OBSERVER_MODELS,          # the picker's choices
                            "defaultModel"  => observer_default_model(), # config default (Sonnet)
                            # written here (not only on a feedback run) so the info panel can always show
                            # the terminal one-liner, even before the user has ever run Ask Claude
                            "mcpConfigPath" => _write_observer_mcp_config(),
                            # is the user's OWN terminal set up? drives which button the lab-log toolbar
                            # shows (Set up my terminal vs Chat to Claude) — see _observer_terminal_state
                            "terminal"      => _observer_terminal_state())
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
    prior = read_registered_observer_spec()
    # `local`-scope leftovers override our `-s user` entry in the folders they name, so setup is not
    # done until they're gone — checked BEFORE the early return below, which otherwise reports
    # "Already set up" for a terminal that has no working tools.
    shadows = shadowing_observer_dirs(spec)
    ok, message = if observer_registration_state(prior, spec) === :current
        # Already correct → touch nothing. The user's main Claude config shouldn't be rewritten just
        # because someone clicked a button twice.
        (true, isempty(shadows) ? "Already set up" : "Registration was already correct")
    else
        # Otherwise pass the entry that's there now so a failed re-sync can put it back (see
        # register_observer_mcp) — and so a first-time setup never runs a `remove` at all.
        register_observer_mcp(agent, JSON3.write(spec);
                              prior_json = prior === nothing ? "" : JSON3.write(prior))
    end
    # Clear the shadowing entries only once the user-scope one is good — removing them after a failed
    # add would leave the user with no working registration at all. Reported per folder: this deletes
    # something the user (or an older install) put there, so it must never be a silent side effect.
    if ok && !isempty(shadows)
        removed, failed = remove_shadowing_observer_mcps(agent, shadows)
        isempty(removed) || (message = string(message, "\nCleared a conflicting per-folder entry in: ",
                                              join(removed, ", ")))
        isempty(failed)  || (message = string(message, "\nCould not clear the entry in: ",
                                              join(failed, ", "),
                                              " — run `claude mcp remove ", OBSERVER_MCP_NAME,
                                              " -s local` there."))
    end
    # Report the state read back from the config, not the CLI's exit code alone — the UI flips its
    # button on `terminal.ready`, so it must reflect what's actually on disk.
    200, JSON3.write((; ok = ok, available = true, name = OBSERVER_MCP_NAME,
                        message = message, error = ok ? "" : message,
                        terminal = _observer_terminal_state()))
end

