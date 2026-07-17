# ── Agent runner — spawn a headless AI assistant to observe + comment in the lab log ──────────────
#
# The app runs an assistant (Claude Code today) as a subprocess: it reads project state and appends to
# the lab log via the cecelia-observer MCP, then exits. Mirrors the napari-bridge spawn pattern
# (napari.jl `_bridge_cmd` / `Base.Process`). The MCP server is model-agnostic, so the assistant is a
# swappable adapter — `AgentBackend`, with `ClaudeAgent` the first implementation (Gemini/ChatGPT can
# slot in later without touching the server or the prompt). See docs/todo/OBSERVER_INTEGRATION_PLAN.md.

abstract type AgentBackend end

# The CLI binary for the agent — default "claude", overridable via config.toml [ai] agent_bin.
observer_agent_bin()::String = string(get(get(cecelia_conf(), "ai", Dict()), "agent_bin", "claude"))

# The models the observer offers (Claude CLI `--model` aliases). Opus is deliberately NOT the
# default: the observer's work — spot a repeat pattern, read a task log to diagnose a failure, write
# a brief lab-log line — fits Sonnet, and Haiku is enough for the frequent auto-Watch passes; Opus
# just costs more and runs slower. Default overridable via config.toml [ai] model, picked per-run by
# the panel. A second backend (Gemini/…) would define its own list.
const OBSERVER_MODELS = ["haiku", "sonnet", "opus"]
observer_default_model()::String =
    (m = string(get(get(cecelia_conf(), "ai", Dict()), "model", "sonnet"));
     m in OBSERVER_MODELS ? m : "sonnet")
# Coerce a requested model to a safe allow-listed value (never pass an arbitrary string to --model).
observer_valid_model(m)::String =
    (s = string(m); s in OBSERVER_MODELS ? s : observer_default_model())

struct ClaudeAgent <: AgentBackend
    bin::String
    model::String     # "" = the CLI's default model
end
ClaudeAgent(; bin::AbstractString = observer_agent_bin(), model::AbstractString = observer_default_model()) =
    ClaudeAgent(String(bin), String(model))

# One turn's outcome — backend-agnostic. The lab-log write itself is a side effect the agent performs
# through the MCP append tool; this carries the usage/session data for the in-app readouts.
struct AgentResult
    ok::Bool
    text::String
    input_tokens::Int
    output_tokens::Int
    session_id::String
    error::String
end

# Is the agent CLI available on PATH? Drives the UI availability gate (feature hidden if absent).
agent_available(a::AgentBackend)::Bool = !isnothing(Sys.which(_agent_bin(a)))
_agent_bin(a::ClaudeAgent)::String = a.bin

# The MCP config the spawned agent loads — points at the SAME `cecelia_mcp.server`, talking back to
# this API. `mcp_dir` is repo-root/mcp (on PYTHONPATH); `api_url` is this server. Reuses mcp/ unchanged.
function observer_mcp_config(mcp_dir::AbstractString, python_bin::AbstractString,
                             api_url::AbstractString)::Dict{String,Any}
    Dict{String,Any}("mcpServers" => Dict{String,Any}(
        "cecelia-observer" => Dict{String,Any}(
            "command" => String(python_bin),
            "args"    => ["-m", "cecelia_mcp.server"],
            "env"     => Dict{String,Any}("PYTHONPATH" => String(mcp_dir),
                                          "CECELIA_API_URL" => String(api_url)))))
end

# Build the `claude -p` command. PURE given its inputs → unit-tested without spawning anything.
# --allowedTools mcp__cecelia-observer lets the agent call the observer tools non-interactively; the
# MCP allow-list (read routes + lablog/append only) remains the hard no-mutation guarantee.
function _build_claude_cmd(a::ClaudeAgent, prompt::AbstractString, mcp_config_path::AbstractString;
                           session_id::AbstractString = "", system_prompt::AbstractString = "")::Cmd
    args = String[a.bin, "-p", String(prompt),
                  "--output-format", "json",
                  "--mcp-config", String(mcp_config_path),
                  "--allowedTools", "mcp__cecelia-observer"]
    isempty(system_prompt) || append!(args, ["--append-system-prompt", String(system_prompt)])
    isempty(session_id)    || append!(args, ["--resume", String(session_id)])
    isempty(a.model)       || append!(args, ["--model", a.model])
    Cmd(args)
end

# Parse `claude --output-format json` output. PURE → unit-tested. Claude prints one JSON object with
# `result` (final text), `session_id`, `is_error`, and `usage {input_tokens, output_tokens}`. Tolerant
# of missing keys (the exact schema is an adapter detail — confirm against a live run, see the PLAN).
function _parse_claude_result(json_str::AbstractString)::AgentResult
    j = try
        JSON3.read(json_str)
    catch
        return AgentResult(false, "", 0, 0, "", "could not parse agent output")
    end
    j isa AbstractDict || return AgentResult(false, "", 0, 0, "", "unexpected agent output")
    usage = get(j, :usage, nothing)
    intok = usage isa AbstractDict ? Int(get(usage, :input_tokens, 0)) : 0
    outok = usage isa AbstractDict ? Int(get(usage, :output_tokens, 0)) : 0
    is_err = get(j, :is_error, false) == true
    text   = string(get(j, :result, ""))
    AgentResult(!is_err, text, intok, outok, string(get(j, :session_id, "")),
                is_err ? (isempty(text) ? "agent reported an error" : text) : "")
end

# Run one observer turn: spawn the agent, let it read + append through the MCP, return usage/session.
# Bounded by a timeout so a hung agent can't wedge the request. LIVE path (needs the agent CLI + a
# running API) — not exercised in CI; the pure builders/parsers above are the tested surface.
function run_observer_turn(a::ClaudeAgent, prompt::AbstractString, mcp_config_path::AbstractString;
                           system_prompt::AbstractString = "", session_id::AbstractString = "",
                           timeout_s::Real = 180, on_process::Function = _ -> nothing)::AgentResult
    agent_available(a) || return AgentResult(false, "", 0, 0, "", "assistant CLI not found: $(a.bin)")
    cmd = _build_claude_cmd(a, prompt, mcp_config_path; session_id, system_prompt)
    out = Pipe()
    proc = run(pipeline(cmd; stdout = out, stderr = out); wait = false)
    close(out.in)
    on_process(proc)
    timer = Timer(_ -> (try; _kill_proc_tree(proc); catch; end), timeout_s)
    output = read(out, String)
    wait(proc)
    close(timer)
    if proc.exitcode != 0 && !occursin("\"result\"", output)
        return AgentResult(false, "", 0, 0, "",
                           isempty(strip(output)) ? "agent exited $(proc.exitcode)" : output)
    end
    _parse_claude_result(output)
end
