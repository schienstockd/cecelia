# ── Observer (in-app AI assistant) API ────────────────────────────────────────────────────────────
# The in-app driver for the MCP observer: spawn a headless assistant that reads project state and
# appends to the lab log via the cecelia-observer MCP. Phase 1 = the one-shot "give feedback" button.
# See docs/todo/OBSERVER_INTEGRATION_PLAN.md and app/src/ai/agent_runner.jl.
#
#   GET  /api/observer/status    → { available, cliPath } — is an assistant CLI present, and where
#                                   (PATH or a known install dir — see agent_bin_path; drives the UI
#                                   gate: controls render disabled-with-why when false, not hidden). Also
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
_observer_api_url()   = observer_api_url(_PROTOCOL[], PORT)   # the scheme the server started with

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

# The `--settings` file the app's own turns load: the caller hook, so a Kiwi turn's writes record the
# model that made them (`observer_hook_settings`). Rewritten each time, like the MCP config beside it.
function _write_observer_hook_settings()::String
    path = joinpath(ensure_config_dir(), "observer-hook-settings.json")
    write_json_atomic(path, observer_hook_settings(_observer_mcp_dir(), python_bin_path()))
    path
end

# The spec THIS install needs registered — same resolution as the --mcp-config file, so detection and
# registration can never disagree about what "set up" means.
_observer_want_spec() = observer_mcp_spec(_observer_mcp_dir(), python_bin_path(), _observer_api_url())

# The plugin options THIS install needs — same resolution as the --mcp-config file and the spec.
_observer_want_options() = observer_plugin_options(_observer_mcp_dir(), python_bin_path(), _observer_api_url())

# `{state, ready}` for the user's own terminal. `state` ∈ missing/stale/shadowed/current; `ready` is the
# single thing the UI branches on. The terminal gets the observer through the Cecelia Claude Code PLUGIN
# (app/src/ai/observer_plugin.jl): STALE covers a plugin pointing at another install, an older plugin
# version, changed options (port, interpreter), and a leftover `claude mcp add-json` registration —
# that one still gives tools, but without the caller hook, so writes can't record the model.
#
# SHADOWED outranks a good install: a leftover `local`-scope entry wins in the directory it names, so
# the user's session there talks to the wrong server. `shadowedDirs` names the folders so the UI can say
# which.
function _observer_terminal_state()
    mcp_dir = _observer_mcp_dir()
    st = observer_plugin_state(read_observer_plugin(), _observer_want_options(),
                               observer_marketplace_dir(mcp_dir), shipped_plugin_version(mcp_dir);
                               legacy = read_registered_observer_spec() !== nothing)
    shadows = shadowing_observer_dirs(_observer_want_spec())
    st === :current && !isempty(shadows) && (st = :shadowed)
    Dict{String,Any}("state" => String(st), "ready" => st === :current,
                     "shadowedDirs" => shadows)
end

# status: is an assistant CLI here, which models it offers, and is the user's own terminal set up.
function api_observer_status(req::HTTP.Request)
    resp = Dict{String,Any}("available"    => agent_available(ClaudeAgent()),
                            # where it was found ("" = not found) — shown in the Settings row's tooltip,
                            # so "which claude is Cecelia using?" never needs a terminal
                            "cliPath"       => something(agent_bin_path(observer_agent_bin()), ""),
                            "models"        => OBSERVER_MODELS,          # the picker's choices
                            "defaultModel"  => observer_default_model(), # config default (Sonnet)
                            # written here (not only on a feedback run) so the info panel can always show
                            # the terminal one-liner, even before the user has ever run Ask Claude
                            "mcpConfigPath" => _write_observer_mcp_config(),
                            # is the user's OWN terminal set up? drives which button Kiwi's Terminal row
                            # shows (Set up / Fix vs Chat to Claude) — see _observer_terminal_state
                            "terminal"      => _observer_terminal_state())
    200, JSON3.write(resp)
end

# One-click terminal setup: install the Cecelia plugin (observer MCP + caller hook) through Claude
# Code's own CLI, so the biologist's next plain `claude` has the tools and its writes record the model.
# Safe to click repeatedly: an already-current install is left alone, anything else is re-synced (see
# install_observer_plugin). Body is ignored — every value is server-derived; scope is NOT client-settable.
function api_observer_register(::Vector{UInt8})
    agent = ClaudeAgent()
    if !agent_available(agent)
        return 200, JSON3.write((; ok = false, available = false,
            error = "No assistant CLI found. Install Claude Code to enable this."))
    end
    # `local`-scope leftovers override the plugin's server in the folders they name, so setup is not
    # done until they're gone.
    shadows = shadowing_observer_dirs(_observer_want_spec())
    ok, message = install_observer_plugin(agent, _observer_mcp_dir(), _observer_want_options();
                                          legacy = read_registered_observer_spec() !== nothing)
    # Clear the shadowing entries only once the plugin is in — removing them after a failed install
    # would leave the user with no working registration at all. Reported per folder: this deletes
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

