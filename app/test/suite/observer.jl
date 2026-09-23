# ── Observer / MCP / LabArchives testsets ─────────────────────────────
# Five sections covering: pure command+result builders for the in-app AI observer runner
# (Claude CLI argv, MCP config, spawn wrapping, registration state, shadow scope cleanup);
# the in-app observer prompt-as-role contract (asserts the DIVISION — the loop's own tools are
# named here, the shared MCP catalogue is not restated); MCP connections enumeration (Settings
# → MCP connections row source); LabArchives context sidecar (round-trip / gaps / briefing);
# and the AI observer session sidecar (tokens + turns + clear). Extracted from suite.jl to
# keep it small enough to merge without EOF conflicts on every append. The extracted file
# loads inside this file's aggregating testset scope, so any helpers defined earlier in
# suite.jl are still in scope (lexical include).
#
# The one `normpath(joinpath(@__DIR__, "..", ".."))` repo-root walk (in the prompt-as-role
# testset — needs to read mcp/cecelia_mcp/server.py) is rerouted through pathof(Cecelia) so
# it resolves identically whether the file sits at app/test/ or app/test/suite/.

_repo = dirname(dirname(dirname(pathof(Cecelia))))

# ── AI observer (in-app assistant) — pure command/result pieces ─────────────
# The live spawn (needs the agent CLI + a running API) isn't tested here; these pin the pure
# builders/parsers that the runner + api route depend on. See docs/todo/OBSERVER_INTEGRATION_PLAN.md.
@testset "AI observer agent runner (pure pieces)" begin
    a   = Cecelia.ClaudeAgent(bin = "claude", model = "")               # explicit empty → no flag
    cmd = Cecelia._build_claude_cmd(a, "hello", "/tmp/mcp.json"; system_prompt = "be brief")
    argv = cmd.exec
    @test argv[1] == "claude"
    @test "-p" in argv && "hello" in argv
    @test "--output-format" in argv && "json" in argv
    @test "--mcp-config" in argv && "/tmp/mcp.json" in argv
    @test "--allowedTools" in argv                                    # observer tools allowed
    @test "--append-system-prompt" in argv && "be brief" in argv
    @test !("--resume" in argv)                                       # no session → no resume
    @test !("--model" in argv)                                        # empty model → no flag

    cmd2 = Cecelia._build_claude_cmd(Cecelia.ClaudeAgent(bin = "claude", model = "claude-opus-4-8"),
                                     "hi", "/tmp/m.json"; session_id = "sess123")
    @test "--resume" in cmd2.exec && "sess123" in cmd2.exec
    @test "--model" in cmd2.exec && "claude-opus-4-8" in cmd2.exec

    # model choice: shipped default is Sonnet (Opus not needed for observer work); the request
    # model is allow-listed — an arbitrary string never reaches --model. (default_model reads
    # config [ai] model, so assert it stays within the allow-list rather than a hard "sonnet".)
    @test Set(Cecelia.OBSERVER_MODELS) == Set(["haiku", "sonnet", "opus"])
    @test Cecelia.observer_default_model() in Cecelia.OBSERVER_MODELS
    @test Cecelia.observer_valid_model("haiku") == "haiku"
    @test Cecelia.observer_valid_model("gpt-4") == Cecelia.observer_default_model()   # unknown → default
    @test Cecelia.observer_valid_model("")     == Cecelia.observer_default_model()
    @test Cecelia.ClaudeAgent(bin = "claude").model == Cecelia.observer_default_model()

    # result parsing — success carries text + usage + session
    r = Cecelia._parse_claude_result(
        """{"is_error":false,"result":"noted a stuck task","session_id":"s1","usage":{"input_tokens":1200,"output_tokens":40}}""")
    @test r.ok && r.text == "noted a stuck task"
    @test r.input_tokens == 1200 && r.output_tokens == 40 && r.session_id == "s1"
    # error result surfaces the message; garbage is a clean failure, not a throw
    e = Cecelia._parse_claude_result("""{"is_error":true,"result":"tool failed"}""")
    @test !e.ok && occursin("tool failed", e.error)
    g = Cecelia._parse_claude_result("not json")
    @test !g.ok && g.input_tokens == 0

    # stale-session detection: a pruned/expired --resume id makes the CLI say "No conversation
    # found with session ID: …" → run_observer_turn drops the id and retries fresh (self-heal).
    @test Cecelia._is_stale_session_error(
        "No conversation found with session ID: 0df65af8-ae13-4ec5-964a-7231cd8bf005")
    @test Cecelia._is_stale_session_error("no conversation found with session id: x")  # case-insensitive
    @test !Cecelia._is_stale_session_error("agent exited 1")                            # other failures don't retry
    @test !Cecelia._is_stale_session_error("tool failed")

    # MCP config points the spawned agent at the same cecelia_mcp server + this API
    cfg = Cecelia.observer_mcp_config("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    srv = cfg["mcpServers"]["cecelia-observer"]
    @test srv["command"] == "/env/python"
    @test srv["args"] == ["-m", "cecelia_mcp.server"]
    @test srv["env"]["PYTHONPATH"] == "/repo/mcp"
    @test srv["env"]["CECELIA_API_URL"] == "http://127.0.0.1:8080"
    # the wrapper is built FROM the spec — one source of truth for both the --mcp-config file and
    # `claude mcp add-json` (which takes the bare spec)
    @test srv == Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    @test !haskey(srv["env"], "CECELIA_OBSERVER_NO_PAIR")          # the user's terminal still pairs
    # app-spawned turns: the headless config switches auto-pairing off (else a throwaway `claude -p`
    # re-pairs the project to a session that is about to exit — seen live, KIWI_ASSISTANT_PLAN Phase 0)
    hcfg = Cecelia.observer_mcp_config("/repo/mcp", "/env/python", "http://127.0.0.1:8080"; headless = true)
    @test hcfg["mcpServers"]["cecelia-observer"]["env"]["CECELIA_OBSERVER_NO_PAIR"] == "1"
    # …without leaking into the spec `claude mcp add-json` registers for the user's own terminal
    @test !haskey(Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")["env"],
                  "CECELIA_OBSERVER_NO_PAIR")

    # one-click terminal setup: add-json is not idempotent, so registering is remove-then-add at
    # user scope, and both commands must name the SAME server as the config/--allowedTools filter
    reg = Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/env/python\"}")
    @test reg.exec == [a.bin, "mcp", "add-json", Cecelia.OBSERVER_MCP_NAME,
                       "{\"command\":\"/env/python\"}", "-s", "user"]
    rm_cmd = Cecelia._build_mcp_remove_cmd(a)
    @test rm_cmd.exec == [a.bin, "mcp", "remove", Cecelia.OBSERVER_MCP_NAME, "-s", "user"]
    @test Cecelia._build_mcp_register_cmd(a, "{}"; scope = "local").exec[end] == "local"
    # the restore path uses the SAME add command, so a prior entry can be put back verbatim
    @test Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/old/python\"}").exec[5] ==
          "{\"command\":\"/old/python\"}"
    @test Cecelia.OBSERVER_MCP_NAME == "cecelia-observer"   # the name users see in `claude mcp list`

    # ── Windows CLI resolution + spawn wrapping ──────────────────────────────────────────
    # The observer was invisible on Windows: `Sys.which` only tries the bare name plus `.exe`/
    # `.com` (base/sysinfo.jl), never `.cmd`/`.bat`, so an npm-installed `claude.cmd` was never
    # found → available:false → "Set up my terminal" told users WITH Claude Code to install it.
    # And a `.cmd` cannot be spawned directly — CreateProcess refuses batch files.
    # These helpers take `iswin` explicitly so BOTH platforms' behaviour is asserted from any
    # host — the reason this shipped broken is that nobody could exercise the Windows path.
    @test Cecelia._agent_bin_candidates("claude", false) == ["claude"]        # unix: as given
    @test Cecelia._agent_bin_candidates("claude", true) ==
          ["claude", "claude.cmd", "claude.bat"]                             # windows: + batch shims
    # an explicit extension is Sys.which's job already — don't append to it
    @test Cecelia._agent_bin_candidates("claude.exe", true) == ["claude.exe"]
    @test Cecelia._agent_bin_candidates("claude.cmd", true) == ["claude.cmd"]

    # only batch files need the shell, and only on Windows
    @test Cecelia._needs_cmd_shell("C:/n/claude.cmd", true)
    @test Cecelia._needs_cmd_shell("C:/n/claude.BAT", true)                  # extension case-insensitive
    @test !Cecelia._needs_cmd_shell("C:/n/claude.exe", true)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude", false)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude.cmd", false)            # never on unix

    # argv rewriting: argv[1] becomes the resolved path, and a batch file gains `cmd /c`
    logical = ["claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "/usr/bin/claude", false) ==
          ["/usr/bin/claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true) ==
          ["cmd", "/c", "C:/npm/claude.cmd", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/p/claude.exe", true) ==
          ["C:/p/claude.exe", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    # unresolvable → argv untouched, so the spawn fails naming what the user configured
    @test Cecelia._agent_spawn_argv(logical, nothing, true) == logical
    @test Cecelia._agent_spawn_argv(String[], nothing, true) == String[]
    # the spec argument must survive rewriting verbatim — it is the whole payload
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true)[7] == "{}"

    # live resolver: an absolute path to a real executable resolves to itself; nonsense is nothing.
    # (Uses this Julia's own binary — no assumption about what is on PATH.)
    let jl = joinpath(Sys.BINDIR, Sys.iswindows() ? "julia.exe" : "julia")
        isfile(jl) && @test Cecelia.agent_bin_path(jl) == jl
    end
    @test Cecelia.agent_bin_path("") === nothing
    @test Cecelia.agent_bin_path("cecelia-definitely-no-such-binary-42") === nothing

    # Is the user's own terminal set up? Drives which button the lab-log toolbar shows, so the
    # three states must be exact. A stale entry (another checkout's python, or no/!matching
    # CECELIA_API_URL) is NOT "set up" — it fails silently in the user's session.
    want = Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    @test Cecelia.observer_registration_state(nothing, want) === :missing
    registered = JSON3.read(JSON3.write(merge(want, Dict("type" => "stdio"))))  # as Claude stores it
    @test Cecelia.observer_registration_state(registered, want) === :current    # extra keys are fine
    other_py = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/OTHER/python",
                                                               "http://127.0.0.1:8080")))
    @test Cecelia.observer_registration_state(other_py, want) === :stale        # moved checkout
    other_port = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/env/python",
                                                                 "http://127.0.0.1:9999")))
    @test Cecelia.observer_registration_state(other_port, want) === :stale      # different port
    # the real-world case that bit an early manual registration: PYTHONPATH set, CECELIA_API_URL absent
    no_url = JSON3.read(JSON3.write(Dict("command" => "/env/python", "args" => ["-m", "cecelia_mcp.server"],
                                         "env" => Dict("PYTHONPATH" => "/repo/mcp"))))
    @test Cecelia.observer_registration_state(no_url, want) === :stale
    @test Cecelia.observer_registration_state(Dict{String,Any}("command" => "/env/python"), want) === :stale

    # config path honours Claude Code's own env override; missing file → not set up, never an error
    withenv("CLAUDE_CONFIG_DIR" => "/tmp/cc-cfg") do
        @test Cecelia.claude_config_path() == joinpath("/tmp/cc-cfg", ".claude.json")
    end
    withenv("CLAUDE_CONFIG_DIR" => nothing) do
        @test Cecelia.claude_config_path() == joinpath(homedir(), ".claude.json")
    end
    @test Cecelia.read_registered_observer_spec(joinpath(mktempdir(), "nope.json")) === nothing
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test Cecelia.read_registered_observer_spec(bad) === nothing     # another tool's file — tolerate
    end
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
        # this reader is user-scope ONLY — a local-scope entry is not a registration for it to find
        write(cfgf, JSON3.write(Dict("projects" => Dict("/somewhere" =>
            Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))))
        @test Cecelia.read_registered_observer_spec(cfgf) === nothing
    end

    # ── Local-scope shadowing ────────────────────────────────────────────────────────────
    # The bug: our button writes `-s user`, but Claude Code resolves `local` scope
    # (projects[<dir>].mcpServers) FIRST. A leftover local entry pointing at a DELETED checkout
    # therefore killed the server with ENOENT for every session started in that dir — while the
    # status route, reading only the top level, reported :current and offered "Chat to Claude".
    stale_local = Cecelia.observer_mcp_spec("/gone/mcp", "/gone/python", "http://127.0.0.1:8080")
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict(
            "mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want),
            "projects"   => Dict(
                "/home/u"       => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => stale_local)),
                "/home/u/right" => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want)),
                "/home/u/none"  => Dict("mcpServers" => Dict{String,Any}()),
                "/home/u/other" => Dict("mcpServers" => Dict("something-else" => want))))))
        locals = Cecelia.read_local_observer_specs(cfgf)
        @test sort(String[d for (d, _) in locals]) == ["/home/u", "/home/u/right"]
        # only the MISMATCHED one is a problem: a local entry equal to `want` resolves to the same
        # server, so it is left alone — we never delete config that isn't breaking anything
        @test Cecelia.observer_shadow_dirs(locals, want) == ["/home/u"]
        # user scope is still read independently of any of this
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
    end
    # tolerant like the user-scope reader — a missing/garbage/odd-shaped config is "no shadows",
    # never an exception (it is another tool's file and this runs on every status poll)
    @test isempty(Cecelia.read_local_observer_specs(joinpath(mktempdir(), "nope.json")))
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test isempty(Cecelia.read_local_observer_specs(bad))
    end
    let odd = joinpath(mktempdir(), "odd.json")
        write(odd, JSON3.write(Dict("projects" => "a string, not an object")))
        @test isempty(Cecelia.read_local_observer_specs(odd))
    end
    @test Cecelia.observer_shadow_dirs(Pair{String,Any}[], want) == String[]
    # sorted → the folder list the UI reports (and the removal order) is deterministic
    @test Cecelia.observer_shadow_dirs(
        Pair{String,Any}["/b" => stale_local, "/a" => stale_local], want) == ["/a", "/b"]

    # `claude mcp remove -s local` acts on the process's CWD, so the cleanup must be able to say
    # WHERE it runs — and the spawn wrapper must not drop that when it rewrites argv (it rebuilds
    # the Cmd, which is exactly how a `dir` gets silently lost and the wrong scope edited)
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").exec[end] == "local"
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").dir == "/home/u"
    @test isempty(Cecelia._build_mcp_remove_cmd(a).dir)                  # unchanged default
    @test Cecelia._agent_spawn_cmd(
        Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u")).dir == "/home/u"
    # a dir that no longer exists is skipped, not attempted (Claude ignores its entry too)
    @test Cecelia.remove_shadowing_observer_mcps(a, ["/no/such/dir/at/all"]) == (String[], String[])

    # the prompt carries the project + the discipline rules THIS ROLE adds
    fp = Cecelia.observer_feedback_prompt("NRUBxU")
    @test occursin("NRUBxU", fp) && occursin("append_lab_log", fp) && occursin("[Claude]", fp)
    # §1 param-suggestion guidance is present: on an outlier, use get_module_params + the trail to
    # suggest a param direction — framed as a suggestion, current-state only (not a prediction).
    @test occursin("get_module_params", fp) && occursin("suggest", fp)
    # the watch loop's own tools — nobody else polls a running project
    @test occursin("poll_observations", fp) && occursin("get_cohort_qc", fp)
    # the lab-log discipline, which is the whole point of the role: most of the time, write nothing
    @test occursin("write NOTHING", fp)
    # …and it is pointed at the server's briefing for everything shared, rather than restating it
    @test occursin("get_session_briefing", fp)
    # It must NOT restate what the MCP server already tells every client (mcp/cecelia_mcp/guidance.py):
    # the read catalogue, the grouping discipline, the boards/chains rules, the never-starts boundary.
    # A second copy is what went stale twice. Match on unwrapped text — the prompt is hard-wrapped, so
    # a phrase can straddle a newline.
    flat = replace(fp, r"\s+" => " ")
    for shared in ("not four replicates", "statUnit", "press Run", "get_image_attributes",
                   "get_analysis_boards", "get_available_plots", "get_repl_api")
        @test !occursin(shared, flat)
    end
end


# ── Engine contract (KIWI_ASSISTANT_PLAN Phase 1) ─────────────────────────────────────────────
# The driver `run_agent_turn` is engine-independent; a fake backend pins its behaviour without
# spawning anything. The Kiwi command options are pinned on the real Claude builder.
struct _FakeAgent <: Cecelia.AgentBackend
    results::Vector{Cecelia.AgentResult}       # returned in order, one per spawn
    seen::Vector{String}                       # session ids each spawn was given
end
Cecelia.agent_available(::_FakeAgent) = true
Cecelia.agent_capabilities(::_FakeAgent) = (; native_schema = true, native_mcp = true, resumable = true)
Cecelia._is_stale_session_error(::_FakeAgent, msg::AbstractString) = msg == "gone"
function Cecelia._run_agent_once(a::_FakeAgent, prompt, cfg; session_id, kw...)
    push!(a.seen, String(session_id))
    popfirst!(a.results)
end

struct _EmptyAgent <: Cecelia.AgentBackend end      # implements nothing — the fallbacks' target

@testset "AI engine contract" begin
    ok(; so = nothing) = Cecelia.AgentResult(true, "hi", 1, 2, "s2", "", so)
    bad(err)           = Cecelia.AgentResult(false, "", 0, 0, "", err)

    # Claude declares the contract
    c = Cecelia.ClaudeAgent(bin = "claude", model = "")
    @test Cecelia.agent_label(c) == "Claude"
    @test Cecelia.agent_capabilities(c) == (; native_schema = true, native_mcp = true, resumable = true)
    @test Cecelia._is_stale_session_error(c, "No conversation found with session ID: x")

    # stale session → retried ONCE with no session id
    f = _FakeAgent([bad("gone"), ok()], String[])
    r = Cecelia.run_agent_turn(f, "q", "/tmp/m.json"; session_id = "old")
    @test r.ok && f.seen == ["old", ""]
    # any other failure is not retried
    f = _FakeAgent([bad("boom")], String[])
    @test !Cecelia.run_agent_turn(f, "q", "/tmp/m.json"; session_id = "old").ok && f.seen == ["old"]
    # a schema was asked for but the reply has none → failure, even though the engine said success
    f = _FakeAgent([ok()], String[])
    r = Cecelia.run_agent_turn(f, "q", "/tmp/m.json"; json_schema = "{}")
    @test !r.ok && occursin("no structured output", r.error)
    f = _FakeAgent([ok(so = Dict("claims" => []))], String[])
    @test Cecelia.run_agent_turn(f, "q", "/tmp/m.json"; json_schema = "{}").ok
    # no schema asked → plain text replies are fine (the observer's turn)
    @test Cecelia.run_agent_turn(_FakeAgent([ok()], String[]), "q", "/tmp/m.json").ok

    # an engine that doesn't implement the spawn fails loudly, not silently
    @test_throws ErrorException Cecelia._run_agent_once(_EmptyAgent(), "q", "/tmp/m.json")
    @test Cecelia.agent_capabilities(_EmptyAgent()).native_schema == false

    # Kiwi turn options on the Claude builder (the Phase 0 isolation flags)
    argv = Cecelia._build_claude_cmd(c, "q", "/tmp/m.json"; system_prompt = "you are kiwi",
        replace_system_prompt = true, json_schema = "{\"type\":\"object\"}",
        allowed_tools = ["mcp__cecelia-observer__list_images", "mcp__cecelia-observer__list_plots"],
        strict_mcp = true, builtin_tools = "").exec
    @test "--system-prompt" in argv && !("--append-system-prompt" in argv)
    @test argv[findfirst(==("--json-schema"), argv) + 1] == "{\"type\":\"object\"}"
    @test argv[findfirst(==("--allowedTools"), argv) + 1] ==
          "mcp__cecelia-observer__list_images,mcp__cecelia-observer__list_plots"
    @test "--strict-mcp-config" in argv
    @test argv[findfirst(==("--tools"), argv) + 1] == ""       # built-in tools off
    # the observer's defaults are untouched by the new options
    obs = Cecelia._build_claude_cmd(c, "q", "/tmp/m.json").exec
    @test !any(in(obs), ["--strict-mcp-config", "--tools", "--json-schema", "--system-prompt"])
    @test obs[findfirst(==("--allowedTools"), obs) + 1] == "mcp__cecelia-observer"

    # structured output parsing
    p = Cecelia._parse_claude_result(
        """{"is_error":false,"subtype":"success","result":"","session_id":"s","structured_output":{"claims":[]}}""")
    @test p.ok && p.structured !== nothing && haskey(p.structured, :claims)
    # the CLI's own re-prompt loop gave up → error even though is_error is false
    g = Cecelia._parse_claude_result(
        """{"is_error":false,"subtype":"error_max_structured_output_retries","result":""}""")
    @test !g.ok && occursin("schema", g.error)
    @test Cecelia._parse_claude_result("""{"is_error":false,"result":"x"}""").structured === nothing

    # streamed turns (Kiwi): tool results are collected in order, the CLI's own StructuredOutput
    # acknowledgement is not, and the final result event is parsed as usual
    stream = join([
        """{"type":"system","subtype":"init"}""",
        """{"type":"assistant","message":{"content":[{"type":"tool_use","name":"mcp__cecelia-observer__list_images","input":{}}]}}""",
        """{"type":"user","message":{"content":[{"type":"tool_result","content":[{"type":"text","text":"{\\"uid\\":\\"KDIeEm\\"}"}]}]}}""",
        """{"type":"user","message":{"content":[{"type":"tool_result","content":"plain text result"}]}}""",
        """{"type":"user","message":{"content":[{"type":"tool_result","content":"Structured output provided successfully"}]}}""",
        """not json at all""",
        """{"type":"result","subtype":"success","is_error":false,"result":"","session_id":"s9","usage":{"input_tokens":5,"output_tokens":7},"structured_output":{"abstain":true,"claims":[]}}""",
    ], "\n")
    sr = Cecelia._parse_claude_stream(stream)
    @test sr.ok && sr.session_id == "s9" && sr.input_tokens == 5
    @test sr.tool_results == ["{\"uid\":\"KDIeEm\"}", "plain text result"]
    @test sr.structured !== nothing && sr.structured[:abstain] == true
    nr = Cecelia._parse_claude_stream("""{"type":"user","message":{"content":[]}}""")
    @test !nr.ok && occursin("no result event", nr.error)
    # the stream flag reaches the argv (and --verbose, which stream-json requires)
    sv = Cecelia._build_claude_cmd(c, "q", "/tmp/m.json"; stream = true).exec
    @test sv[findfirst(==("--output-format"), sv) + 1] == "stream-json" && "--verbose" in sv
end

@testset "the in-app observer prompt is a role, not a second tool manual" begin
    # THE recurring bug in this area, twice: an MCP tool is added, one of the prompts describing the
    # toolset is updated and another silently goes stale — an unmentioned tool is an unused one, so the
    # capability just never gets offered (create_chain the first time, then get_analysis_boards /
    # get_image_attributes). Both surfaced only because reading a prompt and noticed a gap.
    #
    # The fix was to stop having copies. The MCP server describes its own toolset
    # (mcp/cecelia_mcp/guidance.py: SERVER_INSTRUCTIONS on connect, BRIEFING_GUIDANCE with
    # get_session_briefing), and the in-app agent is spawned with `--mcp-config` pointing at that same
    # server, so it gets both. What is left here is the ROLE — the watch loop, the QC pass, the
    # lab-log discipline — which no chat session has.
    #
    # So this no longer checks "does the prompt name every tool". It checks the DIVISION: the loop's
    # own tools are named here, and the shared catalogue is NOT restated here. Each side is guarded in
    # its own language, since neither can import the other — mcp/tests/test_server.py → GuidanceTest
    # holds the other half (every registered tool is named in guidance.py).
    root = _repo
    server = read(joinpath(root, "mcp", "cecelia_mcp", "server.py"), String)
    # The PROMPT, not the file that holds it: a tool name in a source comment (this file's own header
    # names several) would otherwise count as "mentioned" and mask a real omission. That is not
    # hypothetical — rewording the header alone flipped this assertion once.
    jl = Cecelia.observer_feedback_prompt("NRUBxU")

    tools = Set(String[m.captures[1] for m in eachmatch(r"@mcp\.tool\(\)\s*\ndef (\w+)", server)])
    @test length(tools) >= 30                          # anti-vacuity: a bad regex must not pass
    named_jl = Set(t for t in tools if occursin(t, jl))

    # The watch loop's OWN tools — the reason this role exists. Nothing else polls a running project
    # or decides whether a finished-but-degenerate run is worth a line in the lab log.
    for own in ("poll_observations",        # the 10-attempts pattern, from the session monitor
                "get_task_history", "get_task_log", "get_recent_logs",   # what ran / what broke
                "get_cohort_qc",            # a "done" run that produced far too few cells
                "get_module_params",        # the param-suggestion range
                "read_lab_log", "append_lab_log",                        # prior context + its output
                "get_session_briefing")     # …and where the shared rules come from
        @test own in named_jl
    end
    # The SHARED catalogue must not be restated here — that is the second copy, and the second copy is
    # the bug. These are all named by guidance.py, which reaches this agent through the same MCP.
    for shared in ("get_image_attributes", "get_analysis_boards", "get_available_plots",
                   "get_populations", "get_measure_summary", "get_analysis_lineage", "get_repl_api",
                   "add_analysis_board", "list_projects")
        @test !(shared in named_jl)
    end
    # A tool this role does not use is fine; a tool NOBODY names is not. Every tool the in-app prompt
    # leaves out must be covered by the server's guidance — asserted in full by GuidanceTest, and
    # pinned here as the reason this set is allowed to be small.
    @test length(named_jl) < length(tools)
end

@testset "MCP connections — enumerate whatever is registered" begin
    # Generic on purpose: it lists what's in the config rather than looking for names we know, so a
    # connector added later needs no change here. Backs Settings → MCP connections.
    dir = mktempdir()
    cfg = joinpath(dir, ".claude.json")

    @test isempty(mcp_connections(joinpath(dir, "nope.json")))     # no config → no rows, never throws
    write(cfg, "{not json")
    @test isempty(mcp_connections(cfg))                            # another tool's file: tolerate junk

    write(cfg, """
    {"mcpServers": {"cecelia-observer": {"command": "py", "env": {"PYTHONPATH": "/opt/here/mcp"}},
                    "other-tool": {"url": "https://x/mcp"}},
     "projects": {"/tmp/p1": {"mcpServers": {"cecelia-observer": {"command": "old"}}},
                  "/tmp/p2": {}}}
    """)
    rows = mcp_connections(cfg)
    @test length(rows) == 3                                        # 2 user-scope + 1 local-scope
    names = [r["name"] for r in rows]
    @test "other-tool" in names                                    # a server we know nothing about still lists
    obs = [r for r in rows if r["name"] == "cecelia-observer"]
    @test length(obs) == 2 && Set(r["scope"] for r in obs) == Set(["user", "local"])
    @test all(r["ours"] for r in obs)                              # ours is flagged, others are not
    @test !first(r["ours"] for r in rows if r["name"] == "other-tool")
    # transport is inferred so an http connector doesn't render as a stdio one
    @test first(r["transport"] for r in rows if r["name"] == "other-tool") == "http"
    @test first(r["dir"] for r in rows if r["scope"] == "local") == "/tmp/p1"
    @test rows == sort(rows; by = r -> (r["name"], r["scope"], r["dir"]))   # deterministic order
    # installPath: exposed so the Settings row can name the checkout an "out of date" entry points at.
    # Tolerant: a spec with no env yields "", never errors.
    @test first(r["installPath"] for r in rows if r["scope"] == "user" && r["name"] == "cecelia-observer") == "/opt/here/mcp"
    @test first(r["installPath"] for r in rows if r["scope"] == "local") == ""
end

@testset "LabArchives context sidecar (round-trip, gaps, briefing)" begin
    proj = create_project!(name = "la-$(rand(1000:9999))")
    s = add_set!(proj; name = "s1")

    # no sidecar → present=false, no gaps, and the briefing OMITS the key entirely (so "not linked"
    # and "linked but empty" stay distinguishable).
    d0 = read_la_doc(proj)
    @test d0["present"] == false && d0["readable"] == true
    @test isempty(la_gaps(proj))
    @test la_briefing(proj) === nothing
    @test !haskey(session_briefing(proj), :labarchives)

    # two images, both Treatment=MERTK — the WT arm the ELN declares has NO images.
    for (nm, mouse) in (("m1", "1"), ("m2", "2"))
        img = add_image!(s; name = nm)
        img.attr = Dict("Treatment" => "MERTK", "Mouse" => mouse)
        save!(img)
    end

    doc = write_la_doc!(proj;
        source   = Dict("notebookId" => "nb1", "notebookName" => "Ailsa",
                        "pageIds" => ["p1"], "url" => "https://example/nb"),
        sections = [Dict("heading" => "Setup", "lines" => ["LHS immunised only", "2 sites/mouse"],
                         "sourceDate" => "2026-02-24")],
        cohort   = [Dict("attr" => "Treatment", "value" => "WT", "n" => 6),
                    Dict("attr" => "Treatment", "value" => "MERTK", "n" => 5)])
    @test doc["source"]["notebookName"] == "Ailsa" && doc["source"]["pageIds"] == ["p1"]
    @test !isempty(doc["syncedAt"])

    # round-trips through disk with String keys intact (JSON3 hands back Symbols — json_native)
    r = read_la_doc(proj)
    @test r["present"] == true && r["readable"] == true
    @test r["sections"][1]["heading"] == "Setup"
    @test r["sections"][1]["lines"] == ["LHS immunised only", "2 sites/mouse"]

    # THE case this feature exists for: the declared WT arm has no images, and nothing in the project
    # would otherwise show it — attribute levels are derived from the images PRESENT.
    @test [v for (v, _) in Dict(attr_value_counts(images(proj)))["Treatment"]] == ["MERTK"]
    g = la_gaps(proj)
    @test length(g) == 1
    @test g[1]["attr"] == "Treatment" && g[1]["value"] == "WT"
    @test g[1]["declared"] == 6 && g[1]["present"] == 0

    # briefing carries HEADINGS + gaps, never the section text
    b = la_briefing(proj)
    @test b.sections == ["Setup"] && length(b.gaps) == 1 && b.notebookName == "Ailsa"
    sb = session_briefing(proj)
    @test haskey(sb, :labarchives) && sb.labarchives.sections == ["Setup"]

    # a full REPLACE, not a merge — a section deleted in the ELN must not linger
    write_la_doc!(proj; source = Dict("notebookName" => "Ailsa"),
                  sections = [Dict("heading" => "Question", "lines" => ["nuclear vs cytoplasmic"])],
                  cohort = [])
    r2 = read_la_doc(proj)
    @test [s["heading"] for s in r2["sections"]] == ["Question"]
    @test isempty(la_gaps(proj))            # cohort cleared → nothing to be missing

    # bounds are the WRITER's call, not the caller's
    r3 = write_la_doc!(proj; sections = [Dict("heading" => "H", "lines" => ["x" for _ in 1:50])])
    @test length(r3["sections"][1]["lines"]) == Cecelia.LA_MAX_LINES
    r4 = write_la_doc!(proj; sections = [Dict("heading" => "H$i", "lines" => ["l"]) for i in 1:40])
    @test length(r4["sections"]) == Cecelia.LA_MAX_SECTIONS

    # a corrupt sidecar reads as present-but-UNREADABLE, never as "no context"
    write(la_doc_path(proj), "{not json")
    rbad = read_la_doc(proj)
    @test rbad["present"] == true && rbad["readable"] == false
    @test la_briefing(proj).readable == false

    # the sidecar must never have touched the lab log — that stays append-only
    @test !isfile(lab_log_path(proj)) || isempty(read_lab_log(proj))
end

@testset "AI observer session sidecar (tokens + clear)" begin
    proj = create_project!(name = "obs-sess-$(rand(1000:9999))")
    # fresh project → zeroed session
    s0 = read_observer_session(proj)
    @test s0["sessionId"] == "" && s0["inputTokens"] == 0 && s0["turns"] == 0

    # a turn adopts the session id + accumulates tokens
    record_observer_turn!(proj, "sessABC", 1000, 40)
    s1 = read_observer_session(proj)                       # re-read from disk (persisted)
    @test s1["sessionId"] == "sessABC" && s1["inputTokens"] == 1000 && s1["outputTokens"] == 40
    @test s1["turns"] == 1
    # a second turn accumulates; an EMPTY session id keeps the existing one
    record_observer_turn!(proj, "", 500, 10)
    s2 = read_observer_session(proj)
    @test s2["sessionId"] == "sessABC"                     # unchanged (empty id kept prior)
    @test s2["inputTokens"] == 1500 && s2["outputTokens"] == 50 && s2["turns"] == 2

    # activity log: every pass is recorded (newest-first), even a silent/failed one
    log_observer_pass!(proj; trigger = "manual", model = "sonnet", ok = true, appended = false,
                       input_tokens = 900, output_tokens = 20, note = "reviewed — nothing to flag")
    log_observer_pass!(proj; trigger = "auto", model = "haiku", ok = true, appended = true,
                       input_tokens = 700, output_tokens = 30, note = "flagged clustTracks failed 4×")
    ps = read_observer_session(proj)["passes"]
    @test length(ps) == 2
    @test ps[1]["trigger"] == "auto" && ps[1]["appended"] == true          # newest-first
    @test ps[1]["model"] == "haiku" && ps[1]["inputTokens"] == 700
    @test ps[2]["trigger"] == "manual" && ps[2]["appended"] == false
    @test occursin("nothing to flag", ps[2]["note"])

    # clear resets everything (next run forks a fresh session), incl. the activity log
    cleared = clear_observer_session!(proj)
    @test cleared["sessionId"] == "" && cleared["inputTokens"] == 0 && cleared["turns"] == 0
    @test isempty(cleared["passes"])
    @test read_observer_session(proj)["inputTokens"] == 0
    rm(proj.root; recursive = true)
end
