# ── Agent runner — spawn a headless AI assistant to observe + comment in the lab log ──────────────
#
# The app runs an assistant (Claude Code today) as a subprocess: it reads project state and appends to
# the lab log via the cecelia-observer MCP, then exits. Mirrors the napari-bridge spawn pattern
# (napari.jl `_bridge_cmd` / `Base.Process`). The MCP server is model-agnostic, so the assistant is a
# swappable adapter — `AgentBackend`, with `ClaudeAgent` the first implementation (Gemini/ChatGPT can
# slot in later without touching the server or the prompt). See docs/todo/OBSERVER_INTEGRATION_PLAN.md.
#
# ── The engine contract (docs/todo/KIWI_ASSISTANT_PLAN.md → *The engine seam*) ─────────────────────
#
# Every backend implements these four; everything else in this file is Claude-specific plumbing.
#
#   agent_available(a)     → Bool       is the engine usable here (drives the UI gate). The default
#                                       below assumes a CLI on PATH; a non-CLI engine overrides it
#   agent_label(a)         → String     display name for the UI, so components never hard-code one
#   agent_capabilities(a)  → NamedTuple (; native_schema, native_mcp, resumable)
#   _run_agent_once(a, prompt, mcp_config_path; opts...) → AgentResult   one spawn, no retries
#
# Optional: `on_progress(step::String)` — called, best-effort, with each tool the engine calls WHILE the
# turn runs (Kiwi shows it instead of a bare spinner — plan, Open decision 4). An engine that can't
# report steps just never calls it; a throwing callback never breaks the turn.
#
# `run_agent_turn` (below) is the engine-independent driver on top: availability gate, stale-session
# self-heal (only if `resumable`), and the "asked for a schema, got none" failure. Designed to
# transfer to a second engine but NOT proven to — only Claude implements it (plan, Decision 3).

abstract type AgentBackend end

agent_label(a::AgentBackend)::String = string(nameof(typeof(a)))
agent_capabilities(::AgentBackend) = (; native_schema = false, native_mcp = false, resumable = false)
_run_agent_once(a::AgentBackend, args...; kw...) =
    error("$(typeof(a)) does not implement _run_agent_once — see the engine contract in agent_runner.jl")
# Does this error mean the stored session is gone (drop it and retry fresh)? Engine-specific wording.
_is_stale_session_error(::AgentBackend, ::AbstractString)::Bool = false

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

agent_label(::ClaudeAgent)::String = "Claude"
# `--json-schema` validates + re-prompts inside the CLI; `--mcp-config`; `--resume <session_id>`.
agent_capabilities(::ClaudeAgent) = (; native_schema = true, native_mcp = true, resumable = true)

# One turn's outcome — backend-agnostic. The lab-log write itself is a side effect the agent performs
# through the MCP append tool; this carries the usage/session data for the in-app readouts.
#
# `structured` is the schema-validated reply when the turn asked for one (`json_schema`), else `nothing`.
# `tool_results` is the text of every tool result the engine received this turn, in order — only filled
# on a streamed turn (`stream = true`); Kiwi uses it to check a cited ref was actually SEEN this turn
# (KIWI_ASSISTANT_PLAN Decision 7).
struct AgentResult
    ok::Bool
    text::String
    input_tokens::Int
    output_tokens::Int
    session_id::String
    error::String
    structured::Any
    tool_results::Vector{String}
end
AgentResult(ok, text, intok, outok, sid, err) = AgentResult(ok, text, intok, outok, sid, err, nothing, String[])
AgentResult(ok, text, intok, outok, sid, err, so) = AgentResult(ok, text, intok, outok, sid, err, so, String[])

# ── Finding and spawning the CLI (Windows) ────────────────────────────────────────────────────────
#
# Two Windows facts that made this feature invisible-and-broken there, both handled HERE so no caller
# re-derives the platform branch (same rule as `bioformats2raw_bin`/`_kill_tree` — see CLAUDE.md):
#
#  1. `Sys.which` does NOT find `.cmd`/`.bat`. Its candidate list (base/sysinfo.jl) is the bare name
#     plus `.exe` and `.com` only. Claude Code installed via npm ships `claude.cmd` (+ `.ps1` + an
#     extensionless shell script), so `Sys.which("claude")` returned `nothing`, `agent_available` was
#     `false`, and the whole observer was gated off — while "Set up my terminal" told a user who HAD
#     Claude Code installed to go install it. Only the native installer (`claude.exe`) worked.
#
#  2. A `.cmd`/`.bat` cannot be executed directly. Windows `CreateProcess` (what Julia's `run` uses)
#     refuses batch files; they have to go through `cmd /c`.
#
# Extensions to try, in PATHEXT-ish order. `.ps1` is deliberately excluded: running it needs a
# PowerShell host plus an execution policy that may forbid it, and npm always installs the `.cmd`
# alongside, so there is nothing to gain.
const _AGENT_WIN_EXTS = (".cmd", ".bat")

# The names to look for, in order. PURE and explicitly parameterised on `iswin` (rather than reading
# `Sys.iswindows()` inside) so the WINDOWS behaviour is unit-testable from any host — the whole reason
# this bug shipped is that nobody could exercise the Windows path.
_agent_bin_candidates(name::AbstractString, iswin::Bool)::Vector{String} =
    (iswin && isempty(splitext(String(name))[2])) ?
        String[String(name), (String(name) * e for e in _AGENT_WIN_EXTS)...] :
        String[String(name)]

# Does this resolved path have to be started via `cmd /c`? PURE, same reasoning as above.
_needs_cmd_shell(path::AbstractString, iswin::Bool)::Bool =
    iswin && lowercase(splitext(String(path))[2]) in _AGENT_WIN_EXTS

# The argv to actually spawn, given the logical argv and the resolved argv[1]. PURE.
function _agent_spawn_argv(argv::Vector{String}, resolved::Union{String,Nothing},
                           iswin::Bool)::Vector{String}
    isempty(argv) && return argv
    isnothing(resolved) && return argv         # not found: let the spawn fail with the original name
    out = copy(argv); out[1] = String(resolved)
    _needs_cmd_shell(out[1], iswin) ? vcat(String["cmd", "/c"], out) : out
end

"""
    agent_bin_path(bin) -> String | Nothing

Absolute path to the agent CLI, or `nothing` if it isn't on `PATH`. Extension-aware on Windows (see
the comment above): falls back to `<bin>.cmd` / `<bin>.bat` when the bare name isn't found.
"""
function agent_bin_path(bin::AbstractString)::Union{String,Nothing}
    isempty(String(bin)) && return nothing
    for cand in _agent_bin_candidates(bin, Sys.iswindows())
        p = Sys.which(cand)
        isnothing(p) || return String(p)
    end
    nothing
end

# Turn a logical argv (as the pure builders below produce it) into something the OS will actually
# start: resolve argv[1] to its absolute path, and route batch files through `cmd /c`. Keeping this
# separate from the builders is deliberate — the builders stay pure and platform-independent, so
# their unit tests assert one argv shape on every OS, and only this function knows about Windows.
#
# NOTE on `cmd /c`: cmd.exe applies its OWN parsing to the rest of the line, so
# `claude mcp add-json <name> <spec>` — whose spec argument is quoted JSON — is the part of this to
# distrust on Windows until it has actually been run there. Everything else passes plain arguments.
#
# `dir` is carried over deliberately: rebuilding the argv into a fresh `Cmd` would drop it, and the
# local-scope cleanup below is the one caller that MUST run in a specific directory (`claude mcp
# remove -s local` acts on its own cwd). Losing it there would silently edit the wrong scope.
_agent_spawn_cmd(cmd::Cmd)::Cmd =
    (argv = collect(String, cmd.exec);
     isempty(argv) ? cmd :
     Cmd(Cmd(_agent_spawn_argv(argv, agent_bin_path(argv[1]), Sys.iswindows())); dir = cmd.dir))

# Is the agent CLI available on PATH? Drives the UI availability gate (feature hidden if absent).
agent_available(a::AgentBackend)::Bool = !isnothing(agent_bin_path(_agent_bin(a)))
_agent_bin(::AgentBackend)::String = ""         # non-CLI engines: no binary to find
_agent_bin(a::ClaudeAgent)::String = a.bin

# The MCP server's name — one literal, used by the config, the `--allowedTools` filter, and the
# register/remove commands. Renaming it in one place must rename it everywhere.
const OBSERVER_MCP_NAME = "cecelia-observer"

# The server SPEC (one entry) — points at the SAME `cecelia_mcp.server`, talking back to this API.
# `mcp_dir` is repo-root/mcp (on PYTHONPATH); `api_url` is this server. Reuses mcp/ unchanged. Split
# out from the wrapper below because `claude mcp add-json <name> <json>` takes exactly this object.
function observer_mcp_spec(mcp_dir::AbstractString, python_bin::AbstractString,
                           api_url::AbstractString)::Dict{String,Any}
    Dict{String,Any}("command" => String(python_bin),
                     "args"    => ["-m", "cecelia_mcp.server"],
                     "env"     => Dict{String,Any}("PYTHONPATH" => String(mcp_dir),
                                                   "CECELIA_API_URL" => String(api_url)))
end

# The `--mcp-config` file shape (`{mcpServers: {<name>: <spec>}}`) — what the spawned agent loads.
#
# `headless = true` for the turns the APP spawns (`claude -p`): it sets CECELIA_OBSERVER_NO_PAIR so the
# observer never auto-pairs. A headless `claude -p` hands its MCP children its own messaging socket, so
# without this each in-app turn re-paired the project to a session that exits seconds later — the
# user's real pairing overwritten, pushes sent to a dead socket (seen live 2026-09-23, KIWI_ASSISTANT_PLAN
# Phase 0). The terminal one-liner and `claude mcp add-json` registration stay pairing-enabled.
function observer_mcp_config(mcp_dir::AbstractString, python_bin::AbstractString,
                             api_url::AbstractString; headless::Bool = false)::Dict{String,Any}
    spec = observer_mcp_spec(mcp_dir, python_bin, api_url)
    headless && (spec["env"]["CECELIA_OBSERVER_NO_PAIR"] = "1")
    Dict{String,Any}("mcpServers" => Dict{String,Any}(OBSERVER_MCP_NAME => spec))
end

# Build the `claude -p` command. PURE given its inputs → unit-tested without spawning anything.
# --allowedTools mcp__cecelia-observer lets the agent call the observer tools non-interactively; the
# MCP allow-list (read routes + lablog/append only) remains the hard no-mutation guarantee.
#
# The defaults reproduce the observer's turn exactly. A Kiwi turn (KIWI_ASSISTANT_PLAN Decision 8) also
# passes: `replace_system_prompt` (its own prompt, not appended to Claude Code's), `json_schema` (the
# reply schema), `allowed_tools` (a read-only allow-list, not the whole server), `strict_mcp` (ignore
# the user's other MCP servers) and `builtin_tools = ""` (no Bash/Read/Edit — observer tools only).
function _build_claude_cmd(a::ClaudeAgent, prompt::AbstractString, mcp_config_path::AbstractString;
                           session_id::AbstractString = "", system_prompt::AbstractString = "",
                           replace_system_prompt::Bool = false, json_schema::AbstractString = "",
                           allowed_tools::Union{Nothing,Vector{String}} = nothing,
                           strict_mcp::Bool = false,
                           builtin_tools::Union{Nothing,AbstractString} = nothing,
                           stream::Bool = false)::Cmd
    allowed = allowed_tools === nothing ? ["mcp__" * OBSERVER_MCP_NAME] : allowed_tools
    # `stream`: one JSON event per line, tool results included (`--verbose` is required for it)
    args = String[a.bin, "-p", String(prompt),
                  "--output-format", stream ? "stream-json" : "json",
                  "--mcp-config", String(mcp_config_path),
                  "--allowedTools", join(allowed, ",")]
    stream                  && push!(args, "--verbose")
    strict_mcp              && push!(args, "--strict-mcp-config")
    builtin_tools === nothing || append!(args, ["--tools", String(builtin_tools)])
    isempty(json_schema)    || append!(args, ["--json-schema", String(json_schema)])
    isempty(system_prompt)  ||
        append!(args, [replace_system_prompt ? "--system-prompt" : "--append-system-prompt", String(system_prompt)])
    isempty(session_id)     || append!(args, ["--resume", String(session_id)])
    isempty(a.model)        || append!(args, ["--model", a.model])
    Cmd(args)
end

# ── One-click terminal setup ──────────────────────────────────────────────────────────────────────
#
# The in-app buttons need no setup (Cecelia passes `--mcp-config` to the spawned agent). A session the
# USER starts in their own terminal does — and asking a biologist to paste a path-bearing command is
# how support tickets happen. So Cecelia registers the server for them: `claude mcp add-json <name>
# <spec> -s user`, after which plain `claude` has the observer tools in every session.
#
# `add-json` is NOT idempotent — a second add prints "already exists" and fails — so registering is
# always remove-then-add. That also makes it a RE-SYNC: the paths/port in the spec are re-resolved
# every time, so clicking it again after a move/reinstall fixes a stale entry.
# Both builders are PURE → unit-tested without spawning anything.
_build_mcp_register_cmd(a::ClaudeAgent, spec_json::AbstractString; scope::AbstractString = "user")::Cmd =
    Cmd(String[a.bin, "mcp", "add-json", OBSERVER_MCP_NAME, String(spec_json), "-s", String(scope)])
_build_mcp_remove_cmd(a::ClaudeAgent; scope::AbstractString = "user", dir::AbstractString = "")::Cmd =
    (c = Cmd(String[a.bin, "mcp", "remove", OBSERVER_MCP_NAME, "-s", String(scope)]);
     isempty(dir) ? c : Cmd(c; dir = String(dir)))

# ── Is the user's terminal already set up? ─────────────────────────────────────────────────────────
#
# The lab-log panel offers "Set up my terminal" INSTEAD of "Chat to Claude" until this says yes, so the
# setup isn't buried in an info dialog. Detection reads Claude Code's config file rather than shelling
# out: `claude mcp get/list` health-check every server (spawning our own Python MCP process) which would
# make opening the panel slow for a question we ask on every refresh.
#
# User-scope servers live at the top level of `~/.claude.json` as `mcpServers[<name>]`. Our button
# writes `-s user`, which works from any directory — but `projects[<dir>].mcpServers` (the per-directory
# `local` scope) takes PRECEDENCE over it, so it can't be ignored: see `shadowing_observer_dirs`.
# ── Per-profile credential isolation (LOGIN_CREDENTIAL_ISOLATION_PLAN D2, D6, phase P2) ──────────
#
# `~/.claude/{.credentials.json, .claude.json}` is one credential home shared by every process on
# an OS login. Kiwi spawns `claude` here — so on a shared lab login without isolation, the last
# person to `claude login` is who every Kiwi turn silently authenticates as. This block routes
# every `claude` spawn through the active Kiwi profile's CLAUDE_CONFIG_DIR (D2), and scrubs the
# ambient credential env vars that would otherwise silently override it (D6 — `ANTHROPIC_API_KEY`
# and friends outrank `CLAUDE_CONFIG_DIR` on every platform; measured 2026-09-23 on CLI 2.1.280).
#
# The `default` profile is special: `kiwi_profile_dir("default") == ""` — meaning "do not set
# `CLAUDE_CONFIG_DIR`, let the CLI use its own paths (`~/.claude.json` + `~/.claude/.credentials.json`)".
# This makes P2 a strict behavioural no-op for a single-seat setup — no re-login, no credential
# copy. The picker (P3) introduces named profiles that DO get their own dir. Ambient scrubbing
# runs for every profile including default, so a stray `.bashrc` line can never cross the isolation
# even for the shared-credential default case.

const _AMBIENT_CLAUDE_ENV = ("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN",
                             "CLAUDE_CODE_OAUTH_TOKEN",
                             "CLAUDE_CODE_USE_BEDROCK", "CLAUDE_CODE_USE_VERTEX",
                             "CLAUDE_CODE_USE_FOUNDRY",
                             "AWS_BEARER_TOKEN_BEDROCK")

const _DEFAULT_KIWI_PROFILE = "default"

"""
    kiwi_profile_name() -> String

Active Kiwi profile name from `custom.toml [ai].profile`, defaulting to `"default"`. The picker
(phase P3) will write this key; today it is always the default.
"""
kiwi_profile_name()::String =
    string(get(get(cecelia_conf(), "ai", Dict{String,Any}()), "profile", _DEFAULT_KIWI_PROFILE))

"""
    kiwi_profile_dir(name = kiwi_profile_name(); config_root = config_dir()) -> String

Resolve a profile name to its `CLAUDE_CONFIG_DIR`. Returns `""` for the `default` profile
(meaning "let the CLI use `~/.claude*` as before") and `<config_root>/kiwi-profiles/<name>/`
otherwise. PURE — does NOT create the directory (see `_active_claude_profile_dir!` for the live
resolver that also mkpaths).
"""
kiwi_profile_dir(name::AbstractString = kiwi_profile_name();
                 config_root::AbstractString = config_dir())::String =
    String(name) == _DEFAULT_KIWI_PROFILE ? "" :
        joinpath(String(config_root), "kiwi-profiles", String(name))

"""
    _claude_env_pairs(profile_dir) -> Vector{Pair{String,Any}}

Env overrides for `addenv`: every ambient credential var mapped to `nothing` (unset), and
`CLAUDE_CONFIG_DIR` set to `profile_dir` unless it is empty (default profile — keep the CLI's own
resolution). PURE → unit-tested.
"""
function _claude_env_pairs(profile_dir::AbstractString)::Vector{Pair{String,Any}}
    pairs = Pair{String,Any}[k => nothing for k in _AMBIENT_CLAUDE_ENV]
    isempty(profile_dir) || pushfirst!(pairs, "CLAUDE_CONFIG_DIR" => String(profile_dir))
    pairs
end

"""
    _apply_claude_env(cmd, [profile_dir]) -> Cmd

Add the profile's env overrides to a `Cmd`. Used at every `claude` spawn site so `claude -p`,
`claude mcp add-json`, and `claude mcp remove` all land in the same profile dir.
"""
_apply_claude_env(cmd::Cmd, profile_dir::AbstractString)::Cmd =
    addenv(cmd, _claude_env_pairs(profile_dir)...)
_apply_claude_env(cmd::Cmd)::Cmd = _apply_claude_env(cmd, _active_claude_profile_dir!())

# Live resolver — ensures the named-profile dir exists so the CLI can write on first login.
# The default profile ("") is a no-op here; the CLI's own paths already exist.
function _active_claude_profile_dir!()::String
    d = kiwi_profile_dir()
    isempty(d) || isdir(d) || mkpath(d)
    d
end

"""
    claude_config_path([profile_dir = kiwi_profile_dir()]) -> String

Path to the active profile's `.claude.json` — the file Claude Code writes for its top-level user
config (project history, MCP registrations). Falls back to `~/.claude.json` when `profile_dir` is
empty (default profile). Used by `read_registered_observer_spec` for the "is the terminal already
set up?" UI, so the check reflects the profile the app is actually spawning under.
"""
claude_config_path(profile_dir::AbstractString = kiwi_profile_dir())::String =
    isempty(profile_dir) ? joinpath(homedir(), ".claude.json") :
        joinpath(String(profile_dir), ".claude.json")

# The registered spec, or `nothing`. Tolerant: an unreadable/!JSON config just means "not set up"
# (it's another tool's file — never error the status route over its shape).
function read_registered_observer_spec(path::AbstractString = claude_config_path())
    isfile(path) || return nothing
    cfg = try
        JSON3.read(read(path, String))
    catch
        return nothing
    end
    cfg isa AbstractDict || return nothing
    servers = get(cfg, :mcpServers, nothing)
    servers isa AbstractDict || return nothing
    get(servers, Symbol(OBSERVER_MCP_NAME), nothing)
end

# Compare what's registered against what this install needs. PURE → unit-tested.
#   :missing — nothing registered; offer setup
#   :stale   — registered, but pointing at a different interpreter / mcp dir / API port. It would
#              connect to the wrong place or fail outright, so it needs the same one-click re-sync as
#              :missing (a stale entry fails SILENTLY in the user's terminal — the worst outcome).
#   :current — good to go
function observer_registration_state(registered, want::AbstractDict)::Symbol
    registered === nothing && return :missing
    registered isa AbstractDict || return :stale
    _s(x) = x === nothing ? "" : string(x)
    _s(get(registered, :command, nothing)) == _s(want["command"]) || return :stale
    args = get(registered, :args, nothing)
    (args !== nothing && collect(String.(args)) == String.(want["args"])) || return :stale
    renv = get(registered, :env, nothing)
    renv isa AbstractDict || return :stale
    for (k, v) in want["env"]
        _s(get(renv, Symbol(k), nothing)) == _s(v) || return :stale
    end
    :current
end

# Convenience for the API layer: the state of the live config against the spec we'd register.
observer_registration_state(want::AbstractDict) =
    observer_registration_state(read_registered_observer_spec(), want)

# ── Local-scope shadowing ─────────────────────────────────────────────────────────────────────────
#
# The failure this exists to catch: our button writes `-s user`, but Claude Code resolves `local`
# scope (`projects[<dir>].mcpServers`) FIRST. A leftover local entry — e.g. written by an earlier
# install, or by hand — therefore overrides a perfectly good user-scope registration for every session
# started in that directory, and when it points at a checkout that no longer exists the server dies
# with ENOENT and the tools simply aren't there. Reading only the top level made this invisible: the
# app reported `:current` and offered "Chat to Claude" while every session was in fact broken.
#
# Read every local-scope observer entry as `dir => spec`. Tolerant for the same reason as the
# user-scope reader — it's another tool's file, and its shape must never error the status route.
function read_local_observer_specs(path::AbstractString = claude_config_path())::Vector{Pair{String,Any}}
    out = Pair{String,Any}[]
    isfile(path) || return out
    cfg = try
        JSON3.read(read(path, String))
    catch
        return out
    end
    cfg isa AbstractDict || return out
    projects = get(cfg, :projects, nothing)
    projects isa AbstractDict || return out
    for (dir, entry) in projects
        entry isa AbstractDict || continue
        servers = get(entry, :mcpServers, nothing)
        servers isa AbstractDict || continue
        spec = get(servers, Symbol(OBSERVER_MCP_NAME), nothing)
        spec === nothing || push!(out, String(dir) => spec)
    end
    out
end

"""
    mcp_connections([path]) -> Vector{Dict{String,Any}}

EVERY MCP server registered in the user's Claude config — `[{name, scope, dir, transport, ours}]`,
sorted by (name, scope). Generic on purpose: it enumerates whatever is there rather than looking for
names we know, so a connector added later shows up with no code change here.

`scope` is `"user"` (top-level `mcpServers`) or `"local"` (`projects[<dir>].mcpServers`, which takes
PRECEDENCE — see `read_local_observer_specs`). `ours` marks Cecelia's own observer entry.

**What this can NOT see**: connectors managed by the user's claude.ai ACCOUNT rather than this
machine (LabArchives is one — it authenticates through `/mcp` and never touches `~/.claude.json`).
They are invisible here by construction, so a UI built on this must not render their absence as
"disconnected" — it would read as broken for every user who is, in fact, connected.

Tolerant like the readers above: it is another tool's file, and its shape must never error a route.
"""
function mcp_connections(path::AbstractString = claude_config_path())::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    isfile(path) || return out
    cfg = try
        JSON3.read(read(path, String))
    catch
        return out
    end
    cfg isa AbstractDict || return out

    _transport(spec) = spec isa AbstractDict ?
        String(something(get(spec, :type, nothing), get(spec, :transport, nothing),
                         haskey(spec, :url) ? "http" : "stdio")) : ""
    # PYTHONPATH is the load-bearing part of the observer spec — the checkout it points at. Showing
    # it in the row is what makes "out of date" self-explanatory. Tolerant: not every server carries
    # one, and another tool's file must never error a route.
    _install_path(spec) = spec isa AbstractDict ? begin
        env = get(spec, :env, nothing)
        env isa AbstractDict ? String(something(get(env, :PYTHONPATH, nothing), "")) : ""
    end : ""
    _row(name, scope, dir, spec) = Dict{String,Any}(
        "name" => String(name), "scope" => scope, "dir" => dir,
        "transport" => _transport(spec), "ours" => String(name) == OBSERVER_MCP_NAME,
        "installPath" => _install_path(spec))

    servers = get(cfg, :mcpServers, nothing)
    if servers isa AbstractDict
        for (name, spec) in servers; push!(out, _row(name, "user", "", spec)); end
    end
    projects = get(cfg, :projects, nothing)
    if projects isa AbstractDict
        for (dir, entry) in projects
            entry isa AbstractDict || continue
            local_servers = get(entry, :mcpServers, nothing)
            local_servers isa AbstractDict || continue
            for (name, spec) in local_servers
                push!(out, _row(name, "local", String(dir), spec))
            end
        end
    end
    sort!(out; by = r -> (r["name"], r["scope"], r["dir"]))
end

# Which of those local entries would actually MISLEAD a session, i.e. shadow the user-scope entry with
# a different endpoint. A local entry that matches `want` resolves to the same server, so it is not a
# problem and is deliberately left alone — we only ever clear entries that would break or misdirect.
# PURE → unit-tested. Sorted so the reported list (and the removal order) is deterministic.
observer_shadow_dirs(locals::Vector{Pair{String,Any}}, want::AbstractDict)::Vector{String} =
    sort!(String[d for (d, spec) in locals
                 if observer_registration_state(spec, want) !== :current])

# Convenience for the API layer: shadowing dirs in the live config.
shadowing_observer_dirs(want::AbstractDict)::Vector{String} =
    observer_shadow_dirs(read_local_observer_specs(), want)

# Clear the shadowing local-scope entries. `claude mcp remove -s local` acts on the process's cwd, so
# each removal is spawned IN the offending directory — which is why `_agent_spawn_cmd` preserves `dir`.
# Best-effort and fully reported: returns `(removed, failed)` so the UI can say which folders were
# cleaned rather than claiming a blanket success. Skips a dir that no longer exists (spawning there
# would just fail) — Claude Code ignores its config entry too, so there is nothing to fix.
function remove_shadowing_observer_mcps(a::ClaudeAgent, dirs::Vector{String}; timeout_s::Int = 30)
    removed, failed = String[], String[]
    for d in dirs
        isdir(d) || continue
        ok = try
            proc = run(pipeline(_apply_claude_env(_agent_spawn_cmd(_build_mcp_remove_cmd(a; scope = "local", dir = d)));
                                stdout = devnull, stderr = devnull); wait = false)
            timer = Timer(_ -> (try; _kill_proc_tree(proc); catch; end), timeout_s)
            wait(proc)
            close(timer)
            proc.exitcode == 0 && proc.termsignal == 0
        catch
            false
        end
        push!(ok ? removed : failed, d)
    end
    (removed, failed)
end

# Register (or re-sync) the observer MCP in the user's Claude Code config. Returns
# `(ok, message)` — `message` is the CLI's own output, shown verbatim in the UI on failure so a
# problem is never silent.
#
# We never edit `~/.claude.json` ourselves — every write goes through Claude Code's own CLI, which owns
# that file's format and does its own read-modify-write. It is the user's MAIN config (project history,
# caches, auth state), so the only safe way to touch one key in it is to let its owner do it.
#
# `prior_json` is the entry currently registered (or ""), and it exists for one reason: `add-json`
# refuses an existing name, so re-syncing a stale entry means remove-then-add — two commands, and a
# failure between them would leave the user with NO registration, i.e. worse off than before they
# clicked. So: remove only when something is actually there, and restore it if the add then fails.
# LIVE path (spawns the CLI) — the pure builders above are the tested surface.
function register_observer_mcp(a::ClaudeAgent, spec_json::AbstractString;
                               prior_json::AbstractString = "",
                               scope::AbstractString = "user", timeout_s::Int = 30)
    agent_available(a) || return (false, "No assistant CLI found. Install Claude Code to enable this.")
    _run_quiet(cmd) = try
        run(pipeline(_apply_claude_env(_agent_spawn_cmd(cmd)), stdout = devnull, stderr = devnull); wait = true); true
    catch
        false
    end
    # Nothing registered → nothing to remove, so a failed add can't lose anything.
    isempty(prior_json) || _run_quiet(_build_mcp_remove_cmd(a; scope))
    # Same spawn idiom as _run_observer_once: pipe + timeout timer, and check termsignal too (libuv
    # reports exitcode 0 for signal-kills).
    out = Pipe()
    local output = ""
    ok = try
        proc = run(pipeline(_apply_claude_env(_agent_spawn_cmd(_build_mcp_register_cmd(a, spec_json; scope)));
                            stdout = out, stderr = out); wait = false)
        close(out.in)
        timer = Timer(_ -> (try; _kill_proc_tree(proc); catch; end), timeout_s)
        output = read(out, String)
        wait(proc)
        close(timer)
        proc.exitcode == 0 && proc.termsignal == 0
    catch e
        output = sprint(showerror, e); false
    end
    # Put the user's previous entry back if we removed it and the replacement didn't land. Best-effort
    # and reported either way — never silently leave them with less than they started with.
    if !ok && !isempty(prior_json)
        restored = _run_quiet(_build_mcp_register_cmd(a, prior_json; scope))
        output = string(output, restored ? "\n(your previous cecelia-observer entry was restored)" :
                                          "\n(could not restore your previous cecelia-observer entry)")
    end
    (ok, strip(output))
end

# A stored session id goes stale when Claude Code prunes/expires it (its own log rotation, or a
# different working dir). `--resume <gone-sid>` then makes the CLI exit non-zero with
# "No conversation found with session ID: …" — which, before the self-heal below, made EVERY
# subsequent Watch pass fail permanently until the user hit Clear. Detected here (PURE → unit-tested)
# so `run_agent_turn` can drop the dead id and retry fresh. Matched loosely (message wording is a
# CLI detail): the "no conversation found" phrase plus a session-id mention.
function _is_stale_session_error(msg::AbstractString)::Bool
    m = lowercase(String(msg))
    occursin("no conversation found", m) && occursin("session id", m)
end
_is_stale_session_error(::ClaudeAgent, msg::AbstractString)::Bool = _is_stale_session_error(msg)

# Parse `claude --output-format json` output. PURE → unit-tested. Claude prints one JSON object with
# `result` (final text), `session_id`, `is_error`, and `usage {input_tokens, output_tokens}`. Tolerant
# of missing keys (the exact schema is an adapter detail — confirm against a live run, see the PLAN).
# With `--json-schema` it also carries `structured_output`; when the CLI's own validate-and-re-prompt
# loop gives up, `subtype` is `error_max_structured_output_retries` — an error even if `is_error` isn't.
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
    subtype = string(get(j, :subtype, ""))
    schema_gave_up = subtype == "error_max_structured_output_retries"
    is_err = get(j, :is_error, false) == true || schema_gave_up
    text   = string(get(j, :result, ""))
    so     = get(j, :structured_output, nothing)
    err    = !is_err ? "" :
             schema_gave_up ? "the reply never matched the requested schema" :
             isempty(text) ? "agent reported an error" : text
    AgentResult(!is_err, text, intok, outok, string(get(j, :session_id, "")), err, so)
end

# Parse `claude --output-format stream-json --verbose` output. PURE → unit-tested. One JSON event per
# line; the final `{"type":"result",…}` event carries the same fields `_parse_claude_result` reads, and
# every `user` event's `tool_result` blocks are the tool outputs the engine saw, collected in order.
# The CLI's own `StructuredOutput` acknowledgement is dropped — it's not data the engine looked at.
function _parse_claude_stream(output::AbstractString)::AgentResult
    seen = String[]
    final = ""
    for line in eachline(IOBuffer(String(output)))
        ev = try JSON3.read(line) catch; continue end
        ev isa AbstractDict || continue
        t = string(get(ev, :type, ""))
        if t == "result"
            final = line
        elseif t == "user"
            msg = get(ev, :message, nothing)
            content = msg isa AbstractDict ? get(msg, :content, nothing) : nothing
            content isa AbstractVector || continue
            for c in content
                (c isa AbstractDict && get(c, :type, "") == "tool_result") || continue
                body = get(c, :content, "")
                txt = body isa AbstractString ? String(body) :
                      join((string(get(b, :text, "")) for b in body if b isa AbstractDict), "\n")
                startswith(txt, "Structured output provided") || push!(seen, txt)
            end
        end
    end
    isempty(final) && return AgentResult(false, "", 0, 0, "", "no result event in the agent's stream", nothing, seen)
    r = _parse_claude_result(final)
    AgentResult(r.ok, r.text, r.input_tokens, r.output_tokens, r.session_id, r.error, r.structured, seen)
end

# The tools one stream-json line reports the engine calling — an `assistant` event's `tool_use` blocks,
# named without the MCP prefix ("get_populations"). The CLI's own StructuredOutput call is the reply,
# not a step. PURE → tested.
function _claude_stream_steps(line::AbstractString)::Vector{String}
    ev = try JSON3.read(line) catch; return String[] end
    (ev isa AbstractDict && get(ev, :type, "") == "assistant") || return String[]
    msg = get(ev, :message, nothing)
    content = msg isa AbstractDict ? get(msg, :content, nothing) : nothing
    content isa AbstractVector || return String[]
    steps = String[]
    for c in content
        (c isa AbstractDict && get(c, :type, "") == "tool_use") || continue
        name = string(get(c, :name, ""))
        (isempty(name) || name == "StructuredOutput") && continue
        push!(steps, replace(name, r"^mcp__.+?__" => ""))
    end
    steps
end

# Spawn the agent once and parse its result. Bounded by a timeout so a hung agent can't wedge the
# request. LIVE path (needs the agent CLI) — not exercised in CI; the pure builders/parsers above are
# the tested surface. `cmd_opts` are `_build_claude_cmd`'s Kiwi options, passed through untouched.
function _run_agent_once(a::ClaudeAgent, prompt::AbstractString, mcp_config_path::AbstractString;
                         system_prompt::AbstractString, session_id::AbstractString,
                         timeout_s::Real, on_process::Function,
                         on_progress::Function = _ -> nothing, cmd_opts...)::AgentResult
    cmd = _apply_claude_env(_agent_spawn_cmd(_build_claude_cmd(a, prompt, mcp_config_path;
                                              session_id, system_prompt, cmd_opts...)))
    out = Pipe()
    proc = run(pipeline(cmd; stdout = out, stderr = out); wait = false)
    close(out.in)
    on_process(proc)
    timer = Timer(_ -> (try; _kill_proc_tree(proc); catch; end), timeout_s)
    output = if get(cmd_opts, :stream, false)
        buf = IOBuffer()                          # one event per line — report tool calls as they land
        for line in eachline(out)
            println(buf, line)
            for step in _claude_stream_steps(line)
                try on_progress(step) catch end
            end
        end
        String(take!(buf))
    else
        read(out, String)
    end
    wait(proc)
    close(timer)
    # `exitcode != 0` alone misses the case this function most needs to catch: the timeout above kills
    # the process, and libuv reports exitcode 0 for a signal-killed child — so a timed-out agent looked
    # like a clean exit and its TRUNCATED output was handed to the parser. Check the signal too (the
    # same rule as `run_py`; see app/CLAUDE.md → *Task system*).
    killed = proc.termsignal != 0
    if (proc.exitcode != 0 || killed) && !occursin("\"result\"", output)
        # Name the signal rather than the exit code when it was killed — "agent exited 0" for a
        # timeout is the same misreport in the error string that the check above just fixed.
        why = killed ? "agent killed (signal $(proc.termsignal); timeout was $(timeout_s)s)" :
                       "agent exited $(proc.exitcode)"
        return AgentResult(false, "", 0, 0, "", isempty(strip(output)) ? why : output)
    end
    get(cmd_opts, :stream, false) ? _parse_claude_stream(output) : _parse_claude_result(output)
end

# Run one turn on any backend — the engine-independent driver of the contract above.
# Self-heals a stale session: if we passed a session id and the engine reports the conversation is
# gone, drop the dead id and retry ONCE fresh — otherwise a pruned session would fail every Watch
# pass forever (see _is_stale_session_error). A fresh turn just loses the prior context, not the pass.
# A turn that asked for a schema (`json_schema`) and came back without a structured reply is a
# failure: the CLI can report `success` with no `structured_output` (KIWI_ASSISTANT_PLAN Decision 6).
function run_agent_turn(a::AgentBackend, prompt::AbstractString, mcp_config_path::AbstractString;
                        system_prompt::AbstractString = "", session_id::AbstractString = "",
                        timeout_s::Real = 180, on_process::Function = _ -> nothing,
                        on_progress::Function = _ -> nothing,
                        json_schema::AbstractString = "", cmd_opts...)::AgentResult
    agent_available(a) ||
        return AgentResult(false, "", 0, 0, "", "assistant CLI not found: $(agent_label(a)) ($(_agent_bin(a)))")
    once(sid) = _run_agent_once(a, prompt, mcp_config_path; system_prompt, session_id = sid,
                                timeout_s, on_process, on_progress, json_schema, cmd_opts...)
    res = once(String(session_id))
    if !res.ok && !isempty(session_id) && agent_capabilities(a).resumable &&
       _is_stale_session_error(a, res.error)
        res = once("")
    end
    if res.ok && !isempty(json_schema) && res.structured === nothing
        res = AgentResult(false, res.text, res.input_tokens, res.output_tokens, res.session_id,
                          "the reply had no structured output", nothing, res.tool_results)
    end
    res
end

