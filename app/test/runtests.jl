using Cecelia
using Test
using JSON3
import DataFrames: DataFrame, nrow   # only the symbols the tests construct/measure with

# ── Smoke tests — no API, no WebSocket, no Vue ────────────────────────────────
# Run from the app/ directory:  julia --project test/runtests.jl
# Requires CECELIA_DEV_DIR to point at a config with a valid projects_dir.

init_cecelia!()

# ── Test data fixtures ────────────────────────────────────────────────────────
# Version-controlled fixtures live at <workspace-root>/test-data/projects, independent
# of the deletable dev projects dir (`projects_dir()`). Override with CECELIA_TEST_DATA.
# (@__DIR__ = app/test → ../../.. = workspace root.)  See test-data/README.md.
test_projects_dir() = get(ENV, "CECELIA_TEST_DATA",
    normpath(joinpath(@__DIR__, "..", "..", "..", "test-data", "projects")))

"""Absolute path to a fixture under test-data/projects (no existence check)."""
fixture_path(relparts...) = joinpath(test_projects_dir(), relparts...)

const _WARNED_FIXTURES = Set{String}()

"""
    have_fixture(path) -> Bool

True if the fixture exists. If not, emit a single strong warning (once per path) and
return false so the caller can `@test_skip`. Generic — works for any fixture file.
"""
function have_fixture(path::AbstractString)::Bool
    isfile(path) && return true
    if !(path in _WARNED_FIXTURES)
        push!(_WARNED_FIXTURES, path)
        @warn """
        ╔══════════════════════════════════════════════════════════════════════════╗
        ║  TEST FIXTURE MISSING — dependent tests will be SKIPPED.                   ║
        ╚══════════════════════════════════════════════════════════════════════════╝
        Expected: $path
        Tests that assert against real data are skipped without it, leaving that path
        unverified. Restore test-data/ (see test-data/README.md) or set CECELIA_TEST_DATA
        to a projects dir containing the file above."""
    end
    false
end

# Custom drop-in task fixture — structs must live at module top level, not inside a @testset block.
struct _TestCustomTask <: CciaTask end

# A task whose _run_task always throws — used to assert the scheduler tees a crash into the task log.
struct _CrashTask <: CciaTask end
Cecelia._run_task(::_CrashTask, ::CciaImage, ::Dict{String,Any};
                  on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing,
                  on_process::Function = _ -> nothing) = error("boom in _run_task (test)")

@testset "Cecelia package smoke tests" begin

    # ── Config ────────────────────────────────────────────────────────────────
    @testset "Config" begin
        @test !isempty(projects_dir())
        @test !isempty(python_bin_path())
        @test tasks_concurrent_limit() >= 1
        @test napari_discrete_gpu() isa Bool   # [napari].discreteGpu, default false
    end

    # ── Config resolver (dev↔prod coordination) ─────────────────────────────────
    # The single resolver both init_cecelia! (reader) and set_projects_dir! (writer) share.
    # Order: explicit arg → CECELIA_DEV_DIR env → .env → ~/.cecelia. See docs/todo/ONBOARDING_PLAN.md.
    @testset "Config resolver" begin
        # pure resolution order, no env/file reads
        @test Cecelia._resolve_config_dir("/x", "/y", "/z") == "/x"       # explicit wins
        @test Cecelia._resolve_config_dir(nothing, "/y", "/z") == "/y"    # env beats .env
        @test Cecelia._resolve_config_dir(nothing, nothing, "/z") == "/z" # .env beats default
        @test Cecelia._resolve_config_dir(nothing, nothing, nothing) ==   # installed-app default
              expanduser("~/.cecelia")
        @test Cecelia._resolve_config_dir("~/foo", nothing, nothing) == expanduser("~/foo")
        # public composition
        @test config_dir("/tmp/ceceliatest") == "/tmp/ceceliatest"
        @test custom_toml_path("/tmp/ceceliatest") == joinpath("/tmp/ceceliatest", "custom.toml")
    end

    @testset "run_py custom-modules PYTHONPATH (config_dir not shadowed)" begin
        # Regression: run_py's task-dir parameter was named `config_dir`, which shadowed the
        # config_dir() function, so the custom-modules PYTHONPATH line `joinpath(config_dir(), …)`
        # called the task-dir STRING as a function → every Python task died with
        # `MethodError(<task dir>, (), …)` before Python was ever spawned. The call now lives in a
        # standalone helper with no shadowing param in scope. This asserts it resolves via the real
        # config_dir() function (equality would fail if it ever called anything else).
        @test Cecelia._custom_modules_pydir() == joinpath(config_dir(), "modules", "python")
        @test endswith(Cecelia._custom_modules_pydir(), joinpath("modules", "python"))
    end

    # ── First-launch setup wizard (isolated temp config dir) ────────────────────
    # Uses its own CECELIA_DEV_DIR tempdir so it never touches the real dev/prod config; restores
    # global config afterwards. Exercises setup_required + set_projects_dir! (merge + reload).
    @testset "Config setup wizard" begin
        prev_env = get(ENV, "CECELIA_DEV_DIR", nothing)
        mktempdir() do tmp
            ENV["CECELIA_DEV_DIR"] = tmp
            try
                init_cecelia!()                            # load the empty temp config
                @test custom_toml_path() == joinpath(tmp, "custom.toml")
                @test setup_required() == true             # no custom.toml yet

                # a pre-existing key must survive the merge
                write(custom_toml_path(), "[dirs]\npython = \"/opt/py\"\n")
                @test setup_required() == true             # projects still unset → placeholder

                proj = joinpath(tmp, "projects"); mkpath(proj)
                stored = set_projects_dir!(proj)
                @test stored == proj
                @test isfile(custom_toml_path())
                @test projects_dir() == proj               # hot-reloaded, no restart
                @test setup_required() == false            # configured + dir exists
                @test python_bin_path() == "/opt/py"       # merge preserved the other key

                # a configured-but-missing dir re-triggers setup
                rm(proj; recursive = true)
                @test setup_required() == true
            finally
                prev_env === nothing ? delete!(ENV, "CECELIA_DEV_DIR") :
                                       (ENV["CECELIA_DEV_DIR"] = prev_env)
                init_cecelia!()                            # restore real dev/prod config
            end
        end
    end

    # ── Napari discrete-GPU launch env ──────────────────────────────────────────
    # The bridge command gains the offload env only when discrete_gpu is on (Linux). DRI_PRIME is
    # always applied (safe on single-GPU); the NVIDIA GLX vendor var only when NVIDIA is present.
    @testset "Napari discrete-GPU command" begin
        plain = Cecelia._bridge_cmd(false)
        @test plain.env === nothing                          # no env override → inherits parent
        gpu = Cecelia._bridge_cmd(true)
        if Sys.islinux()
            @test gpu.env !== nothing
            @test any(==("DRI_PRIME=1"), gpu.env)             # always safe → always applied
            # nvidia GLX vendor var is gated on detection (forcing it without NVIDIA breaks GL)
            has_nvidia = any(startswith("__GLX_VENDOR_LIBRARY_NAME=nvidia"), gpu.env)
            @test has_nvidia == Cecelia._nvidia_present()
        else
            @test gpu.env === nothing                         # no-op off Linux
        end
    end

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

        # the prompt carries the project + the discipline rules
        fp = Cecelia.observer_feedback_prompt("NRUBxU")
        @test occursin("NRUBxU", fp) && occursin("append_lab_log", fp) && occursin("[Claude]", fp)
        # §1 param-suggestion guidance is present: on an outlier, use get_module_params + the trail to
        # suggest a param direction — framed as a suggestion, current-state only (not a prediction).
        @test occursin("get_module_params", fp) && occursin("suggest", fp)
    end

    @testset "AI observer session sidecar (tokens + clear)" begin
        proj = create_project!(name = "obs-sess-$(rand(1000:9999))", kind = "static")
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

    # ── Model: create project and image ───────────────────────────────────────
    @testset "Model round-trip" begin
        proj = create_project!(name="smoke-test-$(rand(1000:9999))", kind="static")
        @test isdir(proj.root)
        @test isfile(joinpath(proj.root, "project.json"))

        s = add_set!(proj; name="set-A")
        @test isdir(s._dir)

        img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
        @test isdir(img._dir)
        @test isfile(joinpath(img._dir, "ccid.json"))

        # Round-trip via init_object
        loaded = init_object(proj.uid, img.uid)
        @test loaded isa CciaImage
        @test loaded.name == "img-1"
        @test get(loaded.meta, "ori_path", "") == "/tmp/fake.tif"

        # image_by_uid(...; uid) convenience accessor (REPL/notebook lookup)
        @test image_by_uid(proj; uid = img.uid) === img
        @test image_by_uid(s; uid = img.uid) === img
        @test image_by_uid(proj; uid = "nope") === nothing
        @test image_by_uid(s; uid = "nope") === nothing

        # Cleanup
        rm(proj.root; recursive=true)
    end

    # ── REPL / notebook data-access surface (Observer Phase 2 foundation) ─────────
    @testset "REPL API surface + generated doc" begin
        # every allow-listed accessor is defined, exported, and documented — the notebook-facing
        # surface must be complete (a rename/removal or a missing docstring fails here).
        ref = repl_api_reference()
        @test !isempty(ref)
        for e in ref
            @test isdefined(Cecelia, Symbol(e.name))
            @test e.exported
            @test e.documented          # has a real docstring (undocumented accessors are a bug)
            @test !occursin("value*name", e.doc)   # raw docstring, not the mangled re-render
        end

        # GOLDEN: docs/REPL.md's generated section is in sync with the live docstrings. If this fails,
        # someone changed a listed function's docstring (or the list) without regenerating — run
        # `Cecelia.write_repl_doc()`. This is the drift-guard that keeps REPL.md honest.
        p = Cecelia.repl_doc_path()
        if isfile(p)
            committed = read(p, String)
            @test committed == Cecelia.render_repl_doc(committed)
            @test occursin(Cecelia.REPL_DOC_BEGIN, committed)
        else
            @test_skip "docs/REPL.md not found at $p"
        end
    end

    # ── Run log (automatic per-image provenance) ─────────────────────────────────
    @testset "Run log" begin
        proj = create_project!(name="runlog-test-$(rand(1000:9999))", kind="static")
        s = add_set!(proj; name="set-A")
        img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

        @test read_run_log(img) == Any[]                       # empty before any run
        append_run_log!(img, "segment.cellpose", "default")
        append_run_log!(img, "behaviour.hmm")
        log = read_run_log(img)
        @test length(log) == 2
        @test log[1]["fun"] == "segment.cellpose"
        @test log[1]["valueName"] == "default"
        @test haskey(log[1], "at") && !isempty(log[1]["at"])
        @test log[2]["fun"] == "behaviour.hmm"

        # persists across reload (init_object)
        loaded = init_object(proj.uid, img.uid)
        @test length(read_run_log(loaded)) == 2

        # capped to the most recent RUN_LOG_CAP entries
        for i in 1:(Cecelia.RUN_LOG_CAP + 10); append_run_log!(img, "x.$i"); end
        capped = read_run_log(img)
        @test length(capped) == Cecelia.RUN_LOG_CAP
        @test capped[end]["fun"] == "x.$(Cecelia.RUN_LOG_CAP + 10)"   # newest kept

        # params trail: entry carries the sanitised task params; internal `_…` keys and the redundant
        # `valueName` are dropped, real tuning knobs kept (Observer Phase 2 §1 — see docs/ai-assist/OBSERVER.md).
        img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path" => "/tmp/fake2.tif"))
        append_run_log!(img2, "tracking.bayesian_tracking", "default", "done",
                        Dict{String,Any}("search_radius" => 5.0, "max_lost" => 3,
                                         "valueName" => "default", "_task_id" => "abc123"))
        e = read_run_log(img2)[end]
        @test e["params"]["search_radius"] == 5.0
        @test e["params"]["max_lost"] == 3
        @test !haskey(e["params"], "valueName")    # redundant with its own field
        @test !haskey(e["params"], "_task_id")      # internal, dropped
        # default (no params) → shape-stable empty dict, and it survives reload
        append_run_log!(img2, "behaviour.hmm")
        @test read_run_log(img2)[end]["params"] == Dict{String,Any}()
        @test read_run_log(init_object(proj.uid, img2.uid))[end-1]["params"]["search_radius"] == 5.0
        # sanitiser handles nothing/empty directly
        @test Cecelia._run_log_params(nothing) == Dict{String,Any}()
        @test Cecelia._run_log_params(Dict("_x" => 1, "keep" => 2)) == Dict{String,Any}("keep" => 2)

        rm(proj.root; recursive=true)
    end

    # ── Session briefing + all_qc_docs (Observer Phase 2 §2) ─────────────────────
    @testset "Session briefing + all_qc_docs" begin
        proj = create_project!(name="brief-$(rand(1000:9999))", kind="static")
        s = add_set!(proj; name="set-A")
        img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
        img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
        # suppress the calibration fallback (these fixtures have no PhysicalSize) by persisting an
        # empty omezarr QC doc — so the only flag is the one we add explicitly. Also tests "persisted wins".
        for im in (img1, img2)
            write_qc(im, "importImages.omezarr", "default", Dict{String,Any}[])
        end
        write_qc(img1, "tracking.bayesian_tracking", "default",
                 [qc_finding("warn", "few_tracks", "Few tracks", "Only 5 tracks")])
        append_lab_log!(proj, "User", ["started tracking run"])

        b = session_briefing(proj)
        @test b.projectUid == proj.uid && b.projectName == proj.name && b.imageCount == 2
        uids = [f.uid for f in b.flagged]
        @test img1.uid in uids && !(img2.uid in uids)     # only the warn image flags; clean stays clean
        f1 = b.flagged[findfirst(f -> f.uid == img1.uid, b.flagged)]
        @test f1.worst == "warn" && f1.findings[1].short == "Few tracks"
        @test length(b.recentLabLog) == 1 && b.recentLabLog[1].author == "User"
        @test occursin("tracking", b.recentLabLog[1].summary)

        # all_qc_docs: a fresh image (no persisted omezarr) gets the computed calibration fallback
        img3 = add_image!(s; name="img-3", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
        @test haskey(all_qc_docs(img3), "importImages.omezarr/default")
        @test haskey(all_qc_docs(img1), "importImages.omezarr/default")   # persisted present too

        rm(proj.root; recursive=true)
    end

    # ── Lockfile (naive guard) ──────────────────────────────────────────────────
    @testset "with_transaction" begin
        proj     = create_project!(name="lock-test-$(rand(1000:9999))", kind="static")
        lockfile = joinpath(proj.root, ".cecelia.lock")

        # happy path: returns the body value and releases the lock
        @test (with_transaction(proj) do; 42; end) == 42
        @test !isfile(lockfile)

        # lock is released even when the body throws (no leaked lock)
        @test_throws ErrorException with_transaction(proj) do; error("boom"); end
        @test !isfile(lockfile)

        rm(proj.root; recursive=true)
    end

    # ── Lab log (per-project append-only markdown) ──────────────────────────────
    @testset "Lab log" begin
        proj = create_project!(name="lablog-test-$(rand(1000:9999))", kind="static")

        @test read_lab_log(proj) == ""                     # empty before any entry
        @test parse_lab_log("") == Dict{String,Any}[]

        append_lab_log!(proj, "User", "CD4 gate lower bound ~0.25 for this tissue prep")
        append_lab_log!(proj, "Claude", ["Image 7 gated to 23 cells (cohort mean 187)",
                                         "User excluded image"])
        content = read_lab_log(proj)
        @test occursin("[User]", content) && occursin("[Claude]", content)

        # parsed NEWEST-FIRST, date+author injected, bullets captured
        entries = parse_lab_log(content)
        @test length(entries) == 2
        @test entries[1]["author"] == "Claude"             # newest first
        @test entries[1]["lines"] == ["Image 7 gated to 23 cells (cohort mean 187)",
                                      "User excluded image"]
        @test entries[2]["author"] == "User"
        @test occursin(r"^\d{4}-\d{2}-\d{2}$", entries[1]["date"])

        # APPEND-ONLY: a later write never rewrites earlier bytes
        before = read_lab_log(proj)
        append_lab_log!(proj, "User — correction",
                        "Corrects above: image 7 low count is real biology — keep it")
        after = read_lab_log(proj)
        @test startswith(after, before)
        after_entries = parse_lab_log(after)
        @test length(after_entries) == 3
        @test after_entries[1]["author"] == "User — correction"

        # persists across reload
        @test length(parse_lab_log(read_lab_log(load_project(proj.uid)))) == 3

        # a non-entry `## ` header (version boundary) is not parsed as an entry, and doesn't
        # swallow the following real entry
        marked = after * "\n## [Version boundary: v1 → v2, 2026-07-15]\n---\n"
        append_lab_log!(proj, "User", "post-boundary note")
        # (the boundary line lives in the file only if a user adds it; here we assert the parser)
        @test length(parse_lab_log(marked)) == 3           # boundary line adds no entry

        # empty / whitespace-only / no-author entries are rejected
        @test_throws ErrorException append_lab_log!(proj, "User", ["   "])
        @test_throws ErrorException append_lab_log!(proj, "Claude", String[])
        @test_throws ErrorException append_lab_log!(proj, "   ", ["x"])

        # returned block is a well-formed, header-injected markdown block
        blk = append_lab_log!(proj, "Claude", "another note")
        @test startswith(blk, "## ") && occursin("[Claude]", blk) && occursin("- another note", blk)

        rm(proj.root; recursive=true)
    end

    # ── Lab log context (auto [Cecelia] activity digest) ────────────────────────
    @testset "Lab log context" begin
        proj = create_project!(name="labctx-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="set-A")
        img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
        img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))

        # nothing run yet → no digest
        @test capture_context!(proj) === nothing
        @test isempty(parse_lab_log(read_lab_log(proj)))

        # simulate task activity (what the run logs record)
        append_run_log!(img1, "segment.cellpose", "default")
        append_run_log!(img2, "segment.cellpose", "default")
        append_run_log!(img1, "tracking.bayesian_tracking")

        block = capture_context!(proj)
        @test block !== nothing
        @test occursin("[Cecelia]", block)
        # grouped by module category (task-manager tags), fun prefix dropped
        @test occursin("Segment — cellpose on 2 images", block)
        @test occursin("Tracking — bayesian_tracking on 1 image", block)   # singular
        @test occursin("img-1", block) && occursin("img-2", block)
        # all runs succeeded, no QC → each module line leads with ✅
        @test occursin("✅ Segment", block) && occursin("✅ Tracking", block)

        entries = parse_lab_log(read_lab_log(proj))
        @test length(entries) == 1 && entries[1]["author"] == "Cecelia"

        # idempotent: no new activity → no second digest
        @test capture_context!(proj) === nothing
        @test length(parse_lab_log(read_lab_log(proj))) == 1

        # severity symbols (fresh project so the cutoff is empty — no same-second edge):
        # a failed run → ❌ on its module; a warn QC finding → ⚠️
        projS = create_project!(name="labctx-sev-$(rand(1000:9999))", kind="static")
        sS    = add_set!(projS; name="set-S")
        iS1   = add_image!(sS; name="s-1", meta=Dict{String,Any}("ori_path"=>"/tmp/s1.tif"))
        iS2   = add_image!(sS; name="s-2", meta=Dict{String,Any}("ori_path"=>"/tmp/s2.tif"))
        append_run_log!(iS1, "segment.measureLabels", "default", "failed")
        write_qc(iS2, "tracking.track_measures", "default",
                 [Dict{String,Any}("level"=>"warn","code"=>"c","short"=>"s","long"=>"l")])
        append_run_log!(iS2, "tracking.track_measures", "default")
        # a fun run on 3 images with a warn banked on 2 of them → the digest reports "— 2 flagged"
        iS3 = add_image!(sS; name="s-3", meta=Dict{String,Any}("ori_path"=>"/tmp/s3.tif"))
        for im in (iS1, iS2, iS3); append_run_log!(im, "behaviour.hmm_states", "default"); end
        for im in (iS1, iS2)      # only 2 of the 3 get a warn
            write_qc(im, "behaviour.hmm_states", "default",
                     [Dict{String,Any}("level"=>"warn","code"=>"c","short"=>"Collapsed to one state","long"=>"l")])
        end
        sev = capture_context!(projS)
        @test sev !== nothing
        @test occursin("❌ Segment", sev)      # measureLabels failed → worst outcome for the module
        @test occursin("⚠️ Tracking", sev)     # track_measures produced a warn finding
        @test occursin("hmm_states on 3 images — 2 flagged", sev)   # count of flagged images, not just "≥1"
        @test !occursin("(3 images)", sev)     # redundant parenthetical dropped for >2 images
        # the ACTUAL finding text is spelled out per flagged image (↳ image — what's wrong), not just a count
        @test occursin("↳ s-1 — Collapsed to one state", sev)
        @test occursin("↳ s-2 — Collapsed to one state", sev)
        @test !occursin("↳ s-3", sev)          # the un-flagged image has no detail line

        # new activity strictly after the cutoff → a fresh digest that doesn't repeat old activity
        sleep(1)   # run-log timestamps are second-granular; ensure a strictly-later `at`
        append_run_log!(img2, "behaviour.hmm")
        block2 = capture_context!(proj)
        @test block2 !== nothing
        @test occursin("Behaviour — hmm on 1 image", block2)
        @test !occursin("cellpose", block2)                    # already reported, not repeated
        @test length(parse_lab_log(read_lab_log(proj))) == 2

        # ── gating: net change by population (snapshot-diff, not per-edit) ──
        m = PopulationMap(; pop_type="flow", value_name="default")
        cd3 = add_pop!(m, "CD3"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(m, img1)
        bg = capture_context!(proj)
        @test bg !== nothing && occursin("Gating — ", bg) && occursin("added: CD3", bg)

        # changing a gate → "gate changed: CD3" (net), never a re-add
        set_gate!(m, cd3, RectangleGate("c1", "c2", 0.2, 1.0, 0.0, 1.0))
        save_pop_map!(m, img1)
        bg2 = capture_context!(proj)
        @test bg2 !== nothing && occursin("gate changed: CD3", bg2) && !occursin("added: CD3", bg2)

        @test capture_context!(proj) === nothing            # no gating change → nothing

        # ── filter/membership pop (e.g. cluster tracks): a DEFINITION change is captured too —
        # generically, not just gates (this is the cluster-tracks bug fix). ──
        mt = PopulationMap(; pop_type="trackclust", value_name="default")
        tc = add_pop!(mt, "clust_a"; filter_measure="clusters.x", filter_values=[0, 1])
        save_pop_map!(mt, img1)
        @test capture_context!(proj) !== nothing            # baseline: added clust_a
        mt.pops[tc].filter_values = [0, 1, 2]               # change WHICH clusters define it (no gate)
        save_pop_map!(mt, img1)
        bd = capture_context!(proj)
        @test bd !== nothing && occursin("Clustering — redefined: clust_a", bd)
        @test capture_context!(proj) === nothing            # net change reverts to none

        # ── exclusions: net change ──
        img2.included = false; save!(img2)
        be = capture_context!(proj)
        @test be !== nothing && occursin("Manage images — excluded img-2", be)
        @test capture_context!(proj) === nothing            # no further change

        rm(proj.root; recursive=true)
        rm(projS.root; recursive=true)                      # severity sub-project — also clean up (was leaking)
    end

    @testset "Lab log context — first capture seeds silently" begin
        proj = create_project!(name="labctx-seed-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="imgB", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
        m    = PopulationMap(; pop_type="flow", value_name="default")
        add_pop!(m, "preexisting"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(m, img)

        # gating already present at first capture → baseline seeded, NOT reported (no retro dump)
        @test capture_context!(proj) === nothing
        @test isempty(parse_lab_log(read_lab_log(proj)))

        # a subsequent addition IS reported
        m2 = load_pop_map(img; value_name="default", pop_type="flow")
        add_pop!(m2, "newpop"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(m2, img)
        b = capture_context!(proj)
        @test b !== nothing && occursin("added: newpop", b)

        rm(proj.root; recursive=true)
    end

    # ── Lab log tuning ratings (entry-type feedback → config sidecar) ────────────
    @testset "Lab log tuning" begin
        proj = create_project!(name="tuning-test-$(rand(1000:9999))", kind="static")
        @test read_tuning(proj) == Dict{String,String}()
        set_tuning!(proj, "e1", "up")
        set_tuning!(proj, "e2", "down")
        @test read_tuning(proj) == Dict("e1" => "up", "e2" => "down")
        set_tuning!(proj, "e1", "")                       # clear/toggle-off
        @test read_tuning(proj) == Dict("e2" => "down")
        @test read_tuning(load_project(proj.uid)) == Dict("e2" => "down")   # persists
        @test_throws ErrorException set_tuning!(proj, "e3", "sideways")     # invalid vote
        rm(proj.root; recursive=true)
    end

    # ── Lab log mutes (suppress a digest category) ───────────────────────────────
    @testset "Lab log mutes" begin
        proj = create_project!(name="mutes-test-$(rand(1000:9999))", kind="static")
        @test read_mutes(proj) == String[]
        set_mute!(proj, "Gating", true)                                    # categories = task-manager tags
        set_mute!(proj, "Segment", true)
        @test Set(read_mutes(proj)) == Set(["Gating", "Segment"])
        set_mute!(proj, "Gating", false)
        @test read_mutes(proj) == ["Segment"]
        @test read_mutes(load_project(proj.uid)) == ["Segment"]           # persists
        @test_throws ErrorException set_mute!(proj, "  ", true)            # empty rejected (else lenient)
        rm(proj.root; recursive=true)
    end

    # ── Lab log dismiss (hide a single entry — config sidecar, log stays append-only) ──
    @testset "Lab log dismiss" begin
        proj = create_project!(name="dismiss-test-$(rand(1000:9999))", kind="static")
        @test read_dismissed(proj) == String[]
        set_dismissed!(proj, "e1a2", true)
        set_dismissed!(proj, "b3c4", true)
        @test Set(read_dismissed(proj)) == Set(["e1a2", "b3c4"])
        set_dismissed!(proj, "e1a2", false)                                # un-hide
        @test read_dismissed(proj) == ["b3c4"]
        @test read_dismissed(load_project(proj.uid)) == ["b3c4"]           # persists
        @test_throws ErrorException set_dismissed!(proj, "  ", true)       # empty id rejected

        # hiding NEVER edits the log file (append-only): the entry text is still on disk
        append_lab_log!(proj, "Cecelia", ["a digest line to hide"])
        before = read_lab_log(proj)
        set_dismissed!(proj, "deadbeef", true)
        @test read_lab_log(proj) == before                                 # file untouched
        rm(proj.root; recursive=true)
    end

    @testset "Lab log mutes — capture filtering" begin
        proj = create_project!(name="muteflt-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))

        set_mute!(proj, "Segment", true)                    # category of segment.cellpose
        append_run_log!(img, "segment.cellpose", "default")
        @test capture_context!(proj) === nothing            # Segment muted → not reported

        # unmute: the cutoff already advanced while muted, so the muted-while activity does NOT resurface
        set_mute!(proj, "Segment", false)
        @test capture_context!(proj) === nothing

        # new activity in a non-muted category after unmute IS reported
        sleep(1)                                            # strictly-later ISO timestamp
        append_run_log!(img, "behaviour.hmm")
        b = capture_context!(proj)
        @test b !== nothing && occursin("Behaviour — hmm", b)
        rm(proj.root; recursive=true)
    end

    # ── Mute-bar taxonomy: module pages vs operations (two separate groups) ──
    @testset "Lab log mute categories — pages vs operations" begin
        pages = lab_log_page_categories()
        ops   = lab_log_operation_categories()
        # the pipeline module pages are all present, in _CATEGORY_ORDER, and carry NO operation tags
        for p in ("Segment", "Gating", "Tracking", "Clustering", "Behaviour")
            @test p in pages
        end
        @test !("Edit" in pages) && !("Manage images" in pages)
        # operations are the non-page actions, shown as one general group
        @test Set(ops) == Set(["Edit", "Manage images"])
        # the two groups partition the full universe (no overlap, union == all)
        @test isempty(intersect(Set(pages), Set(ops)))
        @test Set(vcat(pages, ops)) == Set(lab_log_categories())
        # ordering follows _CATEGORY_ORDER (Segment before Tracking before Clustering)
        @test findfirst(==("Segment"), pages) < findfirst(==("Tracking"), pages) < findfirst(==("Clustering"), pages)
    end

    # ── Param validation ──────────────────────────────────────────────────────
    @testset "Param validation" begin
        task = ImportOmezarr()

        # Valid params — should not throw
        @test begin
            validate_params(task, Dict{String,Any}("pyramidScale" => 2))
            true
        end

        # Out of range
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("pyramidScale" => 99))

        # Wrong type (string where int expected)
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("pyramidScale" => "not-a-number"))

        # Validation enforced by run_task itself — not just validate_params.
        # Use TestImageTask to confirm _run_task dispatch works.
        # Then confirm ImportOmezarr's run_task rejects bad params before reaching _run_task.
        proj2 = create_project!(name="val-test-$(rand(1000:9999))", kind="static")
        s2 = add_set!(proj2; name="s")
        img2 = add_image!(s2; name="img", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
        @test_throws ParamValidationError run_task(
            ImportOmezarr(), img2, Dict{String,Any}("pyramidScale" => 99))
        rm(proj2.root; recursive=true)
    end

    # ── Param validation — CellposeCorrect (constraints live inside a `group`) ───
    @testset "Param validation — CellposeCorrect" begin
        # modelDiameter is int min=1/max=100, nested in the `models` group
        out_of_range = Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model"=>"denoise_cyto3", "modelChannels"=>["DAPI"], "modelDiameter"=>500)))
        @test_throws ParamValidationError validate_params(CellposeCorrect(), out_of_range)

        # model is a select — an unknown value must be rejected
        bad_select = Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model"=>"not_a_model", "modelChannels"=>["DAPI"], "modelDiameter"=>30)))
        @test_throws ParamValidationError validate_params(CellposeCorrect(), bad_select)

        # NOTE: RemoveImage has only valueNameSelection params (no scalar constraints),
        # so there is nothing for validate_params to reject — no test is meaningful there.
    end

    # ── Dispatch + param validation — ClustPops (clustPops.cluster, set-scope) ───
    @testset "Param validation — ClustPops" begin
        @test _task_from_fun_name("clustPops.cluster") isa ClustPops
        @test task_scope(ClustPops()) == "set"
        # resolution is float min=0/max=5 — out of range must be rejected
        @test_throws ParamValidationError validate_params(
            ClustPops(), Dict{String,Any}("resolution" => 99))
        # wrong type where float expected
        @test_throws ParamValidationError validate_params(
            ClustPops(), Dict{String,Any}("resolution" => "not-a-number"))
    end

    @testset "Param validation — CellNeighbours" begin
        @test _task_from_fun_name("spatialAnalysis.cellNeighbours") isa CellNeighbours
        @test task_scope(CellNeighbours()) == "image"          # per-image graph (no "scope" in spec)
        # neighbourRadius is float min=0/max=1000 — out of range must be rejected
        @test_throws ParamValidationError validate_params(
            CellNeighbours(), Dict{String,Any}("neighbourRadius" => 5000))
        # nNeighbours is int min=1 — below the floor must be rejected
        @test_throws ParamValidationError validate_params(
            CellNeighbours(), Dict{String,Any}("nNeighbours" => 0))
        # a valid param set passes
        @test validate_params(
            CellNeighbours(), Dict{String,Any}("neighbourRadius" => 30, "nNeighbours" => 6,
                                               "neighbourMethod" => "knn")) === nothing
    end

    @testset "cellNeighbours QC findings (pure helper)" begin
        # objective graph metrics → advisory findings; only the unambiguous problems flag
        @test isempty(Cecelia._neighbours_qc_findings(100, 500, 0.1))        # healthy graph → no finding
        @test only(Cecelia._neighbours_qc_findings(0, 0, 0.0))["code"]   == "spatial.no_cells"
        @test only(Cecelia._neighbours_qc_findings(100, 0, 0.0))["code"] == "spatial.no_edges"
        @test only(Cecelia._neighbours_qc_findings(100, 40, 0.7))["code"] == "spatial.many_isolated"
        @test isempty(Cecelia._neighbours_qc_findings(100, 40, 0.3))         # some isolated, under half → fine
    end

    @testset "Param validation — DetectAggregates" begin
        @test _task_from_fun_name("spatialAnalysis.detectAggregates") isa DetectAggregates
        @test task_scope(DetectAggregates()) == "image"
        @test_throws ParamValidationError validate_params(
            DetectAggregates(), Dict{String,Any}("minCells" => 1))       # min=2
        @test_throws ParamValidationError validate_params(
            DetectAggregates(), Dict{String,Any}("clustDiameter" => -3))  # min=0
    end

    @testset "aggregate DBSCAN ids (Clustering.jl)" begin
        # two dense blobs + one far noise point → two aggregates, noise = id 0
        coords = [0.0 0.0; 0.1 0.1; 0.2 0.0; 5.0 5.0; 5.1 5.1; 5.2 5.0; 50.0 50.0]
        ids = Cecelia._aggregate_ids(coords, 0.5, 2)
        @test length(unique(ids[ids .> 0])) == 2                          # two aggregates
        @test ids[end] == 0                                               # far point is noise
        @test count(==(0), ids) == 1                                      # exactly one noise point
        # too-few points → all noise
        @test all(Cecelia._aggregate_ids([0.0 0.0; 0.1 0.1], 0.5, 5) .== 0)
    end

    @testset "Param validation — ContactsMeshes" begin
        @test _task_from_fun_name("spatialAnalysis.contactsMeshes") isa ContactsMeshes
        @test task_scope(ContactsMeshes()) == "image"
        @test_throws ParamValidationError validate_params(
            ContactsMeshes(), Dict{String,Any}("maxContactDist" => -1))
    end

    @testset "Param validation — AggregatesMeshes" begin
        @test _task_from_fun_name("spatialAnalysis.aggregatesMeshes") isa AggregatesMeshes
        @test task_scope(AggregatesMeshes()) == "image"
        @test_throws ParamValidationError validate_params(
            AggregatesMeshes(), Dict{String,Any}("minCells" => 1))
    end

    @testset "Param validation — CellContacts" begin
        @test _task_from_fun_name("spatialAnalysis.cellContacts") isa CellContacts
        @test task_scope(CellContacts()) == "image"
        @test_throws ParamValidationError validate_params(
            CellContacts(), Dict{String,Any}("maxContactDist" => -1))
        # target-name sanitisation (used for the obs column suffix)
        @test Cecelia._contact_target("flow", ["T/qc"]) == "flow.T_qc"
        @test Cecelia._contact_target("flow", ["B/qc", "T/qc"]) == "flow.B_qc+T_qc"
    end

    @testset "Param validation — NeighbourStats" begin
        @test _task_from_fun_name("spatialAnalysis.neighbourStats") isa NeighbourStats
        @test task_scope(NeighbourStats()) == "image"
        @test_throws ParamValidationError validate_params(
            NeighbourStats(), Dict{String,Any}("neighbourRadius" => 5000))
        @test_throws ParamValidationError validate_params(
            NeighbourStats(), Dict{String,Any}("nNeighbours" => 0))
    end

    @testset "Param validation — ClustRegions" begin
        @test _task_from_fun_name("clustRegions.cluster") isa ClustRegions
        @test task_scope(ClustRegions()) == "set"              # set-scope (regions comparable across set)
        # numClusters is int min=1 — below the floor rejected
        @test_throws ParamValidationError validate_params(
            ClustRegions(), Dict{String,Any}("numClusters" => 0))
        # resolution is float min=0/max=5 — out of range rejected
        @test_throws ParamValidationError validate_params(
            ClustRegions(), Dict{String,Any}("resolution" => 99))
        @test validate_params(
            ClustRegions(), Dict{String,Any}("numClusters" => 5, "resolution" => 1.0,
                                             "clusterMethod" => "leiden")) === nothing
    end

    @testset "Param validation — CropImage" begin
        @test _task_from_fun_name("editImages.cropImage") isa CropImage
        # x0/x1/y0/y1 are int min=0 — negative must be rejected
        @test_throws ParamValidationError validate_params(
            CropImage(), Dict{String,Any}("x0" => -5, "x1" => 10, "y0" => 0, "y1" => 10))
        # wrong type where int expected
        @test_throws ParamValidationError validate_params(
            CropImage(), Dict{String,Any}("x0" => "nope", "x1" => 10, "y0" => 0, "y1" => 10))
        # a valid box (z/t bounds are extra params, not spec-declared — they pass through untouched)
        @test validate_params(
            CropImage(), Dict{String,Any}("x0" => 0, "x1" => 100, "y0" => 0, "y1" => 100,
                                          "z0" => 2, "z1" => 8, "t0" => -1, "t1" => -1)) === nothing
    end

    @testset "Custom module registry (drop-in tasks)" begin
        # A user drops a task by calling register_task! with an instance + a spec path; it must then
        # resolve through _task_from_fun_name / _spec_path / validate_params exactly like a built-in.
        spec_dir = mktempdir()
        spec = joinpath(spec_dir, "exampleTest.json")
        write(spec, JSON3.write(Dict(
            "fun_name"      => "customTest.exampleTest",
            "label"         => "Example test",
            "resource_pool" => "default",
            "scope"         => "image",
            "params"        => [Dict("key" => "n", "label" => "N", "type" => "int",
                                     "min" => 0, "max" => 10)],
        )))

        register_task!("customTest.exampleTest", _TestCustomTask(); spec = spec)

        @test _task_from_fun_name("customTest.exampleTest") isa _TestCustomTask
        @test Cecelia._spec_path(_TestCustomTask()) == spec       # default _spec_path → custom registry
        @test task_scope(_TestCustomTask()) == "image"           # spec read via the registered path
        # param validation uses the dropped spec (n has min=0/max=10)
        @test_throws ParamValidationError validate_params(
            _TestCustomTask(), Dict{String,Any}("n" => 99))
        @test validate_params(_TestCustomTask(), Dict{String,Any}("n" => 3)) === nothing

        # built-ins win on a fun_name clash — registering under an existing name must NOT shadow it
        register_task!("importImages.remove", _TestCustomTask(); spec = spec)
        @test _task_from_fun_name("importImages.remove") isa RemoveImage

        # a missing spec file is rejected up front
        @test_throws ArgumentError register_task!(
            "customTest.bad", _TestCustomTask(); spec = joinpath(spec_dir, "nope.json"))

        # unknown fun_name still errors
        @test_throws Exception _task_from_fun_name("customTest.doesNotExist")
    end

    @testset "Custom module reload prunes deleted files" begin
        # load a module from a temp config dir, then delete its .jl and reload → the task must be
        # unregistered (no longer dispatchable) and dropped from the load report. Regression: the
        # report used to only accumulate, so a deleted module stayed "loaded"/green forever.
        tmp = mktempdir()
        srcdir = joinpath(tmp, "modules", "sources", "tmpcat");            mkpath(srcdir)
        defdir = joinpath(tmp, "modules", "inputDefinitions", "tmpcat");   mkpath(defdir)
        spec = joinpath(defdir, "pruneMe.json")
        write(spec, JSON3.write(Dict(
            "fun_name" => "tmpcat.pruneMe", "label" => "Prune me",
            "resource_pool" => "default", "scope" => "image", "params" => Any[])))
        jl = joinpath(srcdir, "pruneMe.jl")
        write(jl, """
        struct _PruneMeTask <: Cecelia.CciaTask end
        Cecelia.register_task!("tmpcat.pruneMe", _PruneMeTask(); spec = $(repr(spec)))
        """)

        load_custom_modules!(; dev_dir = tmp)
        @test _task_from_fun_name("tmpcat.pruneMe") isa CciaTask
        @test any(m -> m.path == jl, custom_modules_report())

        rm(jl)                                   # user deletes the module file
        res = load_custom_modules!(; dev_dir = tmp)
        @test jl in res.removed                  # reported as unloaded
        @test_throws Exception _task_from_fun_name("tmpcat.pruneMe")   # no longer dispatchable
        @test !any(m -> m.path == jl, custom_modules_report())         # gone from the report
    end

    # ── Section-param flattening (whiteboard/chain stores section params NESTED) ───
    # Regression: a chain node saves `section` params under the section key (e.g.
    # measureOptions => {extendedMeasures: true}), but tasks read them flat. run_task must lift them.
    @testset "Section params flatten (chain nesting)" begin
        ml = _task_from_fun_name("segment.measureLabels")
        # measureLabels declares `measureOptions` + `imageTiling` sections
        @test "measureOptions" in Cecelia._section_keys(ml)
        @test "imageTiling"    in Cecelia._section_keys(ml)
        nested = Dict{String,Any}(
            "outputValueName" => "T",
            "measureOptions"  => Dict{String,Any}("extendedMeasures" => true),
            "imageTiling"     => Dict{String,Any}("blockSize" => 4096, "overlap" => 0))
        flat = Cecelia._flatten_sections(ml, nested)
        @test flat["extendedMeasures"] == true          # was buried under measureOptions
        @test flat["blockSize"] == 4096                  # was buried under imageTiling
        @test !haskey(flat, "measureOptions")            # section container dropped
        @test flat["outputValueName"] == "T"             # top-level survives
        # composite pulls section keys from its sub-tasks (cellpose + measureLabels)
        comp = _task_from_fun_name("segment.cellposeMeasure")
        @test "measureOptions" in Cecelia._section_keys(comp)
        @test Cecelia._flatten_sections(comp,
            Dict{String,Any}("measureOptions" => Dict{String,Any}("extendedMeasures" => true)))["extendedMeasures"] == true
        # already-flat params are unchanged (idempotent)
        @test Cecelia._flatten_sections(ml, Dict{String,Any}("extendedMeasures" => true))["extendedMeasures"] == true
    end

    # ── Dispatch + param validation — ClustTracks (clustTracks.cluster, set-scope) ───
    @testset "Param validation — ClustTracks" begin
        @test _task_from_fun_name("clustTracks.cluster") isa ClustTracks
        @test task_scope(ClustTracks()) == "set"
        # resolution is float min=0/max=5 — out of range must be rejected
        @test_throws ParamValidationError validate_params(
            ClustTracks(), Dict{String,Any}("resolution" => 99))
        # wrong type where float expected
        @test_throws ParamValidationError validate_params(
            ClustTracks(), Dict{String,Any}("resolution" => "not-a-number"))
    end

    # ── Image round-trip (status + attr) ────────────────────────────────────────
    # Regression guard: save!(img) must persist status and attr, not silently drop them.
    @testset "Image status/attr round-trip" begin
        proj = create_project!(name="rt-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        img.status = "done"
        img.attr["condition"] = "treated"
        save!(img)
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test r.status == "done"
        @test get(r.attr, "condition", "") == "treated"
        rm(proj.root; recursive=true)
    end

    # ── Include/exclude (+ note) round-trip ──────────────────────────────────────
    # Guards: new images default to included; excluded flag + note survive save!/init_object; and a
    # legacy ccid.json with neither field loads as included (the accessor never sees a missing field).
    @testset "Image included/note round-trip" begin
        proj = create_project!(name="incl-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        @test image_included(img)                 # default: included
        @test img.note == ""

        img.included = false
        img.note = "bad drift reference channel"
        save!(img)
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test !image_included(r)
        @test r.note == "bad drift reference channel"

        # legacy file (no included/note keys) → defaults to included, empty note
        ccid = joinpath(r._dir, "ccid.json")
        raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
        delete!(raw, "included"); delete!(raw, "note")
        open(ccid, "w") do io; JSON3.write(io, raw); end
        legacy = init_object(proj.uid, img.uid)
        @test image_included(legacy)
        @test legacy.note == ""
        rm(proj.root; recursive=true)
    end

    # ── Per-task param memory (funParams) — R moduleFunParams parity ─────────────
    # Last-used params are remembered in ccid.json under meta["funParams"][fun], per image and per
    # set. Guards: round-trips through save!/init_object, per-fun keys don't clobber, set-level too.
    @testset "funParams per-object memory" begin
        proj = create_project!(name="fp-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        save!(img)

        @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect") === nothing  # absent

        p = Dict{String,Any}("valueName" => "cpCorrected", "driftChannel" => ["DAPI"])
        write_module_fun_params!(img._dir, "cleanupImages.driftCorrect", p)
        got = read_module_fun_params(img._dir, "cleanupImages.driftCorrect")
        @test !isnothing(got)
        @test got["valueName"] == "cpCorrected"
        @test got["driftChannel"] == ["DAPI"]

        # init_object loads funParams into the object's meta; a load-modify-save then preserves them
        # (the loaded object carries funParams, so save! doesn't drop them — unlike a stale object).
        r = init_object(proj.uid, img.uid)
        @test haskey(r.meta, "funParams")
        r.status = "done"; save!(r)
        r2 = init_object(proj.uid, img.uid)
        @test r2.status == "done"
        @test read_module_fun_params(r2._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"

        # a second task's params coexist under its own key (no clobber)
        write_module_fun_params!(img._dir, "cleanupImages.cellposeCorrect",
                                 Dict{String,Any}("valueName" => "default"))
        @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"
        @test read_module_fun_params(img._dir, "cleanupImages.cellposeCorrect")["valueName"] == "default"

        # set-level memory uses the same dir-based mechanism on the set's ccid.json
        write_module_fun_params!(s._dir, "cleanupImages.driftCorrect",
                                 Dict{String,Any}("valueName" => "setDefault"))
        @test read_module_fun_params(s._dir, "cleanupImages.driftCorrect")["valueName"] == "setDefault"

        rm(proj.root; recursive=true)
    end

    # ── Channel names use the versioned convention ──────────────────────────────
    # Regression guard: channel names were stored unversioned under meta, where the
    # task/API readers (which use top-level versioned imChannelNames) never saw them.
    @testset "Channel names versioned round-trip" begin
        proj = create_project!(name="cn-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        set_channel_names!(img, ["DAPI", "GFP"]; check_length=false)
        save!(img)

        # model accessor round-trips through save!/load
        r = init_object(proj.uid, img.uid)
        @test channel_names(r) == ["DAPI", "GFP"]

        # on-disk: top-level versioned imChannelNames (the shape tasks/API use), not under meta
        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "imChannelNames")
        @test !haskey(Dict{String,Any}(get(raw, "meta", Dict())), "imChannelNames")
        @test versioned_active(raw["imChannelNames"]) == "default"
        # readable via the exact helper tasks/API use
        @test collect(String, versioned_get_field(raw, "imChannelNames")) == ["DAPI", "GFP"]

        rm(proj.root; recursive=true)
    end

    # read_ccid_raw is the one ccid.json read+Symbol-key-normalize helper (used by the api layer).
    # versioned_get is the single active-value accessor for both String→String path dicts and the
    # Any/JSON3 raw dicts (replaced the removed image.jl `active`).
    @testset "read_ccid_raw + versioned_get on path dicts" begin
        mktempdir() do d
            p = joinpath(d, "ccid.json")
            write(p, """{"filepath":{"default":"x.ome.zarr","_active":"default"},"class":"CciaImage"}""")
            raw = read_ccid_raw(p)
            @test raw isa Dict{String,Any}
            @test all(k -> k isa String, keys(raw))
            @test raw["class"] == "CciaImage"
            # readable via the exact helper the api/tasks use (nothing → active entry)
            @test versioned_get_field(raw, "filepath") == "x.ome.zarr"
        end
        # versioned_get on a concrete String→String versioned dict (the img.filepath / img.label_props shape)
        d = Dict{String,String}("default" => "a.zarr", "v2" => "b.zarr", "_active" => "v2")
        @test versioned_get(d) == "b.zarr"                 # active entry
        @test sort(versioned_keys(d)) == ["default", "v2"] # excludes _active
    end

    # ── Destructive ops ──────────────────────────────────────────────────────────
    @testset "delete_image! / delete_set!" begin
        proj = create_project!(name="del-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        a    = add_image!(s; name="a")
        b    = add_image!(s; name="b")

        delete_image!(s, b.uid)
        @test !(b.uid in s.image_uids)
        @test !isdir(joinpath(proj.root, "0", b.uid))
        @test !isdir(joinpath(proj.root, "1", b.uid))
        @test !(b.uid in init_object(proj.uid, s.uid).image_uids)   # persisted

        set_uid = s.uid
        delete_set!(proj, set_uid)
        @test !(set_uid in proj.set_uids)
        @test !isdir(joinpath(proj.root, "1", set_uid))
        @test !isdir(joinpath(proj.root, "1", a.uid))               # member removed too
        @test !(set_uid in load_project(proj.uid).set_uids)         # persisted

        rm(proj.root; recursive=true)
    end

    # ── Boundary contract: run a REAL module function end-to-end, no api/ ───────
    # The whole suite loads only `using Cecelia` (api/ is not on the path). This runs
    # an actual task (RemoveImage — real ccid.json + disk work, no external binary) to
    # completion through the public `run_task` entrypoint. Catches coupling creeping back.
    @testset "Boundary contract — real module fn end-to-end" begin
        proj = create_project!(name="bc-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        # register a real on-disk image version
        zarr = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = "done"
        save!(img)

        logs = String[]
        result = run_task(RemoveImage(), img,
            Dict{String,Any}("valueName"=>"default", "newDefault"=>"default");
            on_log = l -> push!(logs, l))

        @test result isa Dict
        @test result["removedValue"] == "default"
        @test result["cleared"] == true          # primary removal clears dims/status
        @test !isdir(zarr)                        # file actually deleted from disk
        @test !isempty(logs)                      # on_log callback fired (no WS needed)

        reloaded = init_object(proj.uid, img.uid)
        @test reloaded.status == "pending"        # primary removal reset status
        @test !haskey(reloaded.filepath, "default")  # version entry gone
        rm(proj.root; recursive=true)
    end

    # ── Storage reclaim — free every non-active image version, keep the active one ───────
    @testset "Storage reclaim" begin
        # pure policy: everything except the active version
        @test Set(Cecelia.reclaimable_versions(Dict{String,Any}(
            "default"=>"a", "afCorrected"=>"b", "cpCorrected"=>"c", "_active"=>"cpCorrected"))) ==
            Set(["default", "afCorrected"])
        @test isempty(Cecelia.reclaimable_versions(Dict{String,Any}(  # only the active version present
            "default"=>"a", "_active"=>"default")))
        # active is the original, but a leftover corrected variant is still freeable (NEW vs default-only)
        @test Cecelia.reclaimable_versions(Dict{String,Any}(
            "default"=>"a", "afCorrected"=>"b", "_active"=>"default")) == ["afCorrected"]

        _mk_ver!(img, fn) = (d = joinpath(img_zero_dir(img), fn); mkpath(d);
                             write(joinpath(d, "chunk"), rand(UInt8, 2048)); fn)

        proj = create_project!(name="stor-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")

        # imgA: original + af + cp, cp active → reclaim frees default AND af, keeps cp
        a = add_image!(s; name="a")
        _mk_ver!(a, "import.ome.zarr"); _mk_ver!(a, "af.ome.zarr"); _mk_ver!(a, "cp.ome.zarr")
        a.filepath = Dict("default"=>"import.ome.zarr", "afCorrected"=>"af.ome.zarr",
                          "cpCorrected"=>"cp.ome.zarr", "_active"=>"cpCorrected")
        a.im_channel_names = Dict{String,Any}("default"=>["ch0","ch1"], "_active"=>"default")
        a.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
        a.status = "done"; save!(a)

        # imgB: original only, active default → nothing to reclaim
        b = add_image!(s; name="b")
        _mk_ver!(b, "import.ome.zarr")
        b.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
        b.status = "done"; save!(b)

        # safe-primary unit: removing default while other versions remain must NOT un-import
        freed, cleared = remove_image_version!(a, "default", "cpCorrected")
        @test freed > 0 && cleared == false
        # restore default for the batch reclaim below
        _mk_ver!(a, "import.ome.zarr")
        ra0 = init_object(proj.uid, a.uid); ra0.filepath["default"] = "import.ome.zarr"; save!(ra0)

        # reclaim_inactive! frees ALL non-active (default + af), keeps cp; imgB skipped
        tot, reclaimed = reclaim_inactive!(proj.uid, [a.uid, b.uid])
        @test reclaimed == [a.uid]
        @test tot > 0
        @test !isdir(joinpath(img_zero_dir(a), "import.ome.zarr"))    # original gone
        @test !isdir(joinpath(img_zero_dir(a), "af.ome.zarr"))        # intermediate gone
        @test  isdir(joinpath(img_zero_dir(a), "cp.ome.zarr"))        # active kept
        @test  isdir(joinpath(img_zero_dir(b), "import.ome.zarr"))    # b untouched

        ra = init_object(proj.uid, a.uid)
        @test ra.status == "done"                                     # NOT un-imported
        @test ra.filepath["_active"] == "cpCorrected"
        @test collect(keys(filter(kv -> kv.first != "_active", ra.filepath))) == ["cpCorrected"]
        @test ra.meta["SizeC"] == 2                                   # dims kept
        @test Cecelia.versioned_get(ra.im_channel_names, "default") == ["ch0","ch1"]  # channel names kept
        rm(proj.root; recursive=true)
    end

    # ── A task crash is recorded in the per-image log, not just the console ──────
    # Regression: a Julia-side failure (caught in _execute_job!) used to only @warn to the console —
    # it never reached {img._dir}/logs/{fun}.log, so a crashed task looked like it just stopped
    # mid-run with no error (invisible to get_task_log + on-disk debugging).
    @testset "Task crash is teed into the per-image log" begin
        proj = create_project!(name="crash-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        logs = String[]
        result = run_task(_CrashTask(), img, Dict{String,Any}(); on_log = l -> push!(logs, l))

        @test result === nothing                                       # crash → nil result
        @test any(l -> occursin("Task crashed", l) && occursin("boom", l), logs)  # reached on_log

        fun_name = Cecelia._fun_name_from_task(_CrashTask())
        logfile  = joinpath(img._dir, "logs", fun_name * ".log")
        @test isfile(logfile)                                          # ...and the on-disk log
        @test occursin("boom", read(logfile, String))

        # ...AND the run log records the FAILED run (so history / the observer can see repeats, not
        # just successes). This is the fix for tasks that silently failed invisibly (broken run_py, HMM).
        rlog = read_run_log(img)
        @test length(rlog) == 1
        @test String(rlog[end]["fun"]) == fun_name && String(rlog[end]["status"]) == "failed"
        rm(proj.root; recursive=true)
    end

    @testset "Run log records status (done + failed)" begin
        proj = create_project!(name="rl-status-$(rand(1000:9999))", kind="static")
        img  = add_image!(add_set!(proj; name="s"); name="img")
        append_run_log!(img, "segment.cellpose", "default")              # default status = done
        append_run_log!(img, "behaviour.hmm", "", "failed")
        rl = read_run_log(img)
        @test String(rl[1]["status"]) == "done"
        @test String(rl[2]["fun"]) == "behaviour.hmm" && String(rl[2]["status"]) == "failed"
        rm(proj.root; recursive=true)
    end

    # ── Set expansion — a set resolves to its correct member UIDs ───────────────
    # run_tasks and the batch accessors depend on this everywhere.
    @testset "Set expansion" begin
        proj = create_project!(name="se-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        a = add_image!(s; name="a"); b = add_image!(s; name="b"); c = add_image!(s; name="c")
        expected = Set([a.uid, b.uid, c.uid])

        @test Set(i.uid for i in images(s))    == expected   # in-memory
        @test Set(i.uid for i in images(proj)) == expected
        reloaded = init_object(proj.uid, s.uid)              # reload from disk
        @test reloaded isa CciaSet
        @test Set(reloaded.image_uids)            == expected
        @test Set(i.uid for i in images(reloaded)) == expected
        rm(proj.root; recursive=true)
    end

    # ── Chain template round-trip ─────────────────────────────────────────────
    # ── Chain node scope defaults from the task spec (single source of truth) ────
    # A node built without an explicit scope inherits the task JSON's "scope": set-scope
    # tasks (behaviour.hmm, clustTracks.cluster) become picnic nodes automatically, image
    # tasks stay image-scope. An explicit scope always overrides.
    @testset "Chain node scope inherits from task spec" begin
        @test Cecelia._task_default_scope("clustTracks.cluster") == "set"
        @test Cecelia._task_default_scope("behaviour.hmm")        == "set"
        @test Cecelia._task_default_scope("importImages.remove")  == "image"
        @test Cecelia._task_default_scope("nonexistent.task")     == "image"   # unknown fn → image

        # chain_node / ChainNode with no scope kwarg resolve from the spec …
        @test chain_node("clustTracks.cluster").scope == "set"
        @test chain_node("importImages.remove").scope == "image"
        @test ChainNode(id="x", fn="behaviour.hmm").scope == "set"
        # … and an explicit scope still wins (force a set task to run per-image)
        @test chain_node("clustTracks.cluster"; scope="image").scope == "image"

        # Deserialisation: a node dict with no "scope" key also inherits from the spec
        @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster")).scope == "set"
        # …while a stored scope (frozen template) is honoured verbatim
        @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster",
                                           "scope"=>"image")).scope == "image"
    end

    # ── Producer output value_name is declared in the JSON spec (introspectable) ──
    # The whiteboard reads this to prefill a downstream node's input `valueName`.
    @testset "Output value_name from spec" begin
        @test Cecelia._spec_output_value_name(CellposeCorrect(), "fallback") == "cpCorrected"
        @test Cecelia._spec_output_value_name(DriftCorrect(),    "fallback") == "driftCorrected"
        @test Cecelia._spec_output_value_name(AfCorrect(),       "fallback") == "afCorrected"
        # A task that declares no top-level outputValueName falls back to the caller's default
        @test Cecelia._spec_output_value_name(RemoveImage(), "fallback") == "fallback"
    end

    @testset "Chain template round-trip" begin
        proj = create_project!(name="chain-tpl-$(rand(1000:9999))", kind="static")

        tpl = ChainTemplate(
            "test-chain",
            [ChainNode(id="n1", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
             ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
            [ChainEdge("n1", "n2")],
        )
        save_chain_template!(proj, tpl)

        # Templates must land under settings/chains — the SAME dir the API reads/writes
        # (api/src/routes.jl _chains_dir_for_project). A divergence here made every whiteboard
        # chain run fail with "template not found" (saved via API, loaded via package).
        @test isfile(joinpath(proj.root, "settings", "chains", "test-chain.json"))
        @test !isfile(joinpath(proj.root, "chains", "test-chain.json"))

        loaded = load_chain_template(proj, "test-chain")
        @test loaded.name == "test-chain"
        @test length(loaded.nodes) == 2
        @test loaded.nodes[1].id == "n1"
        @test loaded.nodes[2].id == "n2"
        @test length(loaded.edges) == 1
        @test loaded.edges[1].from == "n1"
        @test loaded.edges[1].to   == "n2"

        rm(proj.root; recursive=true)
    end

    # ── Chain run — template frozen, per-image state, pipelining ─────────────
    @testset "Chain run — end-to-end with RemoveImage" begin
        proj = create_project!(name="chain-run-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")

        # Two images, each with a registered filepath for RemoveImage to remove
        imgs = map(("img-a", "img-b")) do nm
            img = add_image!(s; name=nm)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
            save!(img)
            img
        end

        tpl = ChainTemplate(
            "remove-chain",
            [ChainNode(id="n1", fn="importImages.remove",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="remove-chain",
                        on_log = line -> push!(logs, line))

        # Run record persisted
        @test isfile(joinpath(run._dir, "run.json"))

        # Template frozen in-memory
        @test run.template_snapshot.name == "remove-chain"
        @test length(run.template_snapshot.nodes) == 1

        # Both images completed node n1
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
        end

        # on_log fired for both images
        @test !isempty(logs)

        # run.json stores hash reference, not embedded template
        raw = JSON3.read(read(joinpath(run._dir, "run.json"), String), Dict{String,Any})
        @test length(raw["image_uids"]) == 2
        @test haskey(raw["image_states"], imgs[1].uid)
        @test haskey(raw["image_states"], imgs[2].uid)
        @test haskey(raw, "template_hash")
        @test !isempty(raw["template_hash"])
        @test !haskey(raw, "template_snapshot")

        # Cache entry exists and round-trips back to the original template
        cached = load_template_from_cache(proj, run.template_hash)
        @test cached.name == "remove-chain"
        @test length(cached.nodes) == 1

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — explicit start node (re-run from here) ─────────────────
    @testset "Chain resume — start node force-restart" begin
        # descendants: pure graph reachability over n1→n2→n3
        tpl = ChainTemplate(
            "restart-chain",
            [ChainNode(id="n1", fn="importImages.remove",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
             ChainNode(id="n2", fn="importImages.remove",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
             ChainNode(id="n3", fn="importImages.remove",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        @test Cecelia._descendants(tpl, "n1") == Set(["n2","n3"])
        @test Cecelia._descendants(tpl, "n2") == Set(["n3"])
        @test isempty(Cecelia._descendants(tpl, "n3"))

        # force-restart from n2 on a run whose nodes are all :done → n2,n3 reset to :pending; n1 kept
        proj = create_project!(name="chain-restart-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        img.status = "done"; save!(img)
        states = Dict(img.uid => Dict(
            "n1" => Cecelia.ImageNodeState(), "n2" => Cecelia.ImageNodeState(),
            "n3" => Cecelia.ImageNodeState()))
        for nid in ("n1","n2","n3")
            states[img.uid][nid].status      = :done
            states[img.uid][nid].params_hash = "h"
        end
        run = Cecelia.ChainRun("rid", "restart-chain", proj.uid, [img.uid], tpl,
                               "hash", states, time(), joinpath(Cecelia._runs_dir(proj), "rid"),
                               ReentrantLock(), Dict{String,Channel{Nothing}}(),
                               Dict{String,Channel{Nothing}}())
        mkpath(run._dir)
        Cecelia._force_restart_from!(run, "n2")
        @test run.image_states[img.uid]["n1"].status == :done       # upstream untouched
        @test run.image_states[img.uid]["n2"].status == :pending    # start node reset
        @test run.image_states[img.uid]["n3"].status == :pending    # downstream reset
        @test run.image_states[img.uid]["n2"].params_hash === nothing

        rm(proj.root; recursive=true)
    end

    # ── Chain start dot — prune to the reachable subgraph ─────────────────────
    @testset "Chain start dot — prune to reachable subgraph" begin
        tpl = ChainTemplate(
            "start-chain",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="d", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="x", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="y", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d"), ChainEdge("x","y")],
            ["c"],                                          # start dot → c (mid-chain)
        )
        pruned = Cecelia._prune_to_start(tpl)
        @test Set(n.id for n in pruned.nodes) == Set(["c","d"])        # c + downstream only
        @test Set((e.from, e.to) for e in pruned.edges) == Set([("c","d")])
        @test isempty(pruned.start_targets)                            # consumed into the node set
        # disconnected draft branch (x→y) dropped; a→b upstream of the start dot dropped too
        @test !any(n.id in ("a","b","x","y") for n in pruned.nodes)
        # no start targets ⇒ unchanged (run the whole chain)
        @test length(Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges)).nodes) == 6
        # start dot pointing ONLY at since-deleted nodes ⇒ fall back to run-all, not an empty run
        stale = Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges, ["ghost"]))
        @test length(stale.nodes) == 6
        @test isempty(stale.start_targets)
    end

    # ── Chain run — set-scope (picnic) node ──────────────────────────────────
    @testset "Chain run — picnic node" begin
        proj = create_project!(name="chain-picnic-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm
            add_image!(s; name=nm)
        end

        # n1 (image) → n2 (set-scope) → n3 (image)
        tpl = ChainTemplate(
            "picnic-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="picnic-chain",
                        on_log = line -> push!(logs, line))

        # All per-image nodes completed for every image
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
        end

        # Set-scope node: all images show :done, result contains the full image count
        for img in imgs
            @test run.image_states[img.uid]["n2"].status == :done
            @test run.image_states[img.uid]["n2"].result["image_count"] == 3
        end

        # Set-scope task log appeared exactly once (ran once, not once per image)
        set_logs = filter(l -> contains(l, "setTask ran"), logs)
        @test length(set_logs) == 1

        rm(proj.root; recursive=true)
    end

    # ── Chain start dot — end-to-end run prunes to the reachable subgraph ─────
    # Reservation guard: pruning to a start dot must still work when the reachable subgraph contains a
    # picnic (set-scope barrier) node. The target becomes a root — its dropped upstream doesn't block
    # it — and the barrier still fires once across all images. Also covers the save/load round-trip of
    # `startTargets` and that pruned-out nodes never enter the run.
    @testset "Chain start dot — run prunes to subgraph (set-scope)" begin
        proj = create_project!(name="chain-startrun-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm); end

        # n1(image) → n2(set) → n3(image); start dot → n2, so n1 is an upstream draft (excluded)
        tpl = ChainTemplate(
            "startrun-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
            ["n2"],                                        # start dot → n2 (a set-scope node)
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs]; chain="startrun-chain",
                        on_log = line -> push!(logs, line))

        # only the reachable subgraph exists in the run — n1 pruned out entirely
        @test Set(keys(run.image_states[imgs[1].uid])) == Set(["n2", "n3"])
        for img in imgs
            @test run.image_states[img.uid]["n2"].status == :done   # set-scope barrier fired as root
            @test run.image_states[img.uid]["n3"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
        @test length(filter(l -> contains(l, "setTask ran"), logs)) == 1   # ran once, not per image

        rm(proj.root; recursive=true)
    end

    # ── Fault isolation is per-predecessor, not global (DAG fan-out) ─────────────
    # A failed branch must not skip a SIBLING branch that shares only an upstream ancestor
    # (e.g. afDriftCorrect → two independent segmentations). Regression guard for the
    # over-broad "any node failed → skip" check that skipped independent branches.
    @testset "Chain fault isolation — independent fan-out" begin
        proj = create_project!(name="chain-fanout-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        # a → { b (fails), c (ok) } — c is independent of b, must still run
        save_chain_template!(proj, ChainTemplate("fanout",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("a","c")]))
        st = run_chain(proj, [img.uid]; chain="fanout").image_states[img.uid]
        @test st["a"].status == :done
        @test st["b"].status == :failed
        @test st["c"].status == :done       # independent sibling — NOT skipped by b's failure

        # a (fails) → { b, c } — the shared ancestor failing skips BOTH branches
        save_chain_template!(proj, ChainTemplate("fanout-root-fail",
            [ChainNode(id="a", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("a","c")]))
        st2 = run_chain(proj, [img.uid]; chain="fanout-root-fail").image_states[img.uid]
        @test st2["a"].status == :failed
        @test st2["b"].status == :skipped
        @test st2["c"].status == :skipped

        # transitive: a → b(fail) → c → d — skip propagates down the branch via :skipped
        save_chain_template!(proj, ChainTemplate("chain-transitive",
            [ChainNode(id="a", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
             ChainNode(id="c", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="d", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d")]))
        st3 = run_chain(proj, [img.uid]; chain="chain-transitive").image_states[img.uid]
        @test st3["a"].status == :done
        @test st3["b"].status == :failed
        @test st3["c"].status == :skipped   # pred b failed
        @test st3["d"].status == :skipped   # pred c skipped → propagates

        rm(proj.root; recursive=true)
    end

    # ── Picnic node — require_all aborts if any image failed upstream ────────
    @testset "Picnic node — require_all policy" begin
        proj = create_project!(name="picnic-req-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1 is a bad fn — both images will fail there
        # n2 is require_all — should abort since n1 failed
        tpl = ChainTemplate(
            "req-chain",
            [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="require_all")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="req-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :failed
            @test run.image_states[img.uid]["n2"].status == :failed
        end
        rm(proj.root; recursive=true)
    end

    # ── Picnic node — successful_only aborts when no images eligible ─────────
    @testset "Picnic node — successful_only policy" begin
        proj = create_project!(name="picnic-ok-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1: bad fn — both images fail upstream
        # n2 (successful_only): zero eligible images → should abort, both :failed
        tpl = ChainTemplate(
            "ok-chain",
            [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="successful_only")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="ok-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :failed
            # no eligible images → set-scope node also fails
            @test run.image_states[img.uid]["n2"].status == :failed
        end
        rm(proj.root; recursive=true)
    end

    # ── Picnic node — successful_only runs with eligible subset ──────────────
    @testset "Picnic node — successful_only with all passing" begin
        proj = create_project!(name="picnic-pass-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1: always succeeds → both images eligible → task runs with all 2
        tpl = ChainTemplate(
            "pass-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask", scope="set",
                       params=Dict{String,Any}(), barrier_policy="successful_only")],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="pass-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
        rm(proj.root; recursive=true)
    end

    # ── Chain run — overrides applied, bad fn isolated ────────────────────────
    @testset "Chain run — overrides + fault isolation" begin
        proj = create_project!(name="chain-iso-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")

        imgs = map(("img-a", "img-b")) do nm
            img = add_image!(s; name=nm)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
            save!(img)
            img
        end

        # n1: bad fn (will fail for both images)
        # n2: valid fn — should be skipped because n1 failed
        tpl = ChainTemplate(
            "fault-chain",
            [ChainNode(id="n1", fn="nonexistent.task",  params=Dict{String,Any}()),
             ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
            [ChainEdge("n1", "n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="fault-chain",
                        on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :failed
            @test run.image_states[img.uid]["n2"].status == :skipped
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — load_chain_run round-trips state ──────────────────────
    @testset "Chain resume — load_chain_run round-trip" begin
        proj = create_project!(name="chain-resume-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "resume-rt-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="resume-rt-chain", on_log=_->nothing)

        # Verify states are :done with a params_hash stored
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test !isnothing(run.image_states[img.uid]["n1"].params_hash)
        end

        # Load the run from disk and verify state is preserved
        loaded = load_chain_run(proj, run.id)
        @test loaded.id == run.id
        @test loaded.chain_name == "resume-rt-chain"
        @test length(loaded.image_uids) == 2
        for img in imgs
            @test loaded.image_states[img.uid]["n1"].status == :done
            @test loaded.image_states[img.uid]["n1"].params_hash == run.image_states[img.uid]["n1"].params_hash
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — already-done nodes are skipped ────────────────────────
    @testset "Chain resume — skip unchanged done nodes" begin
        proj = create_project!(name="chain-resume-skip-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "skip-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        # First run — collects logs
        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs];
                         chain="skip-chain", on_log=line->push!(logs1, line))
        @test !isempty(logs1)

        # Second run via run_id — same params, no work to do
        logs2 = String[]
        run2 = run_chain(proj, String[];
                         run_id=run1.id, on_log=line->push!(logs2, line))

        # No new log lines because all nodes were skipped
        @test isempty(logs2)
        # States still :done
        for img in imgs
            @test run2.image_states[img.uid]["n1"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Chain resume — params change triggers re-run ──────────────────────────
    @testset "Chain resume — params change re-runs node" begin
        proj = create_project!(name="chain-resume-rerun-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "rerun-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("message" => "first"))],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        run1 = run_chain(proj, [i.uid for i in imgs]; chain="rerun-chain", on_log=_->nothing)
        @test run1.image_states[imgs[1].uid]["n1"].result["image"] == imgs[1].name

        # Resume with overridden message param — node must re-run
        logs2 = String[]
        run2 = run_chain(proj, String[];
                         run_id=run1.id,
                         overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "second")),
                         on_log=line->push!(logs2, line))

        @test !isempty(logs2)   # node re-ran and produced logs
        @test run2.image_states[imgs[1].uid]["n1"].status == :done

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Resume from mid-chain failure — only failed/downstream nodes rerun ─
    @testset "Resume — failure at node 3 does not redo nodes 1-2" begin
        proj = create_project!(name="resume-fail-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        # n1 → n2 → n3(bad) → n4(skipped)
        tpl = ChainTemplate(
            "fail-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n3", fn="nonexistent.task",   scope="image", params=Dict{String,Any}()),
             ChainNode(id="n4", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="fail-chain",
                         on_log=line->push!(logs1, line))

        uid = imgs[1].uid
        @test run1.image_states[uid]["n1"].status == :done
        @test run1.image_states[uid]["n2"].status == :done
        @test run1.image_states[uid]["n3"].status == :failed
        @test run1.image_states[uid]["n4"].status == :skipped

        # Resume — n1/n2 are :done with unchanged params → must be skipped
        logs2 = String[]
        run2 = run_chain(proj, String[]; run_id=run1.id,
                         on_log=line->push!(logs2, line))

        # n1 and n2 produced no new log lines — they were skipped
        n1_logs_run1 = count(l -> contains(l, uid*"/n1"), logs1)
        n2_logs_run1 = count(l -> contains(l, uid*"/n2"), logs1)
        n1_logs_run2 = count(l -> contains(l, uid*"/n1"), logs2)
        n2_logs_run2 = count(l -> contains(l, uid*"/n2"), logs2)
        @test n1_logs_run1 > 0    # ran in first pass
        @test n2_logs_run1 > 0    # ran in first pass
        @test n1_logs_run2 == 0   # skipped on resume
        @test n2_logs_run2 == 0   # skipped on resume
        # n3 still fails (fn still missing), n4 still skipped
        @test run2.image_states[uid]["n1"].status == :done
        @test run2.image_states[uid]["n2"].status == :done
        @test run2.image_states[uid]["n3"].status == :failed
        @test run2.image_states[uid]["n4"].status == :skipped

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Params change on node 4 — only n4 and downstream rerun ──────
    @testset "Resume — params change on n4 reruns only n4 and downstream" begin
        proj = create_project!(name="resume-p4-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "p4-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n4", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n5", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4"), ChainEdge("n4","n5")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="p4-chain",
                         on_log=line->push!(logs1, line))
        uid = imgs[1].uid
        for n in ("n1","n2","n3","n4","n5")
            @test run1.image_states[uid][n].status == :done
        end

        # Resume with n4 params changed via override → n1,n2,n3 skip; n4,n5 rerun
        logs2 = String[]
        run2 = run_chain(proj, String[]; run_id=run1.id,
                         overrides=Dict{String,Any}("n4" => Dict{String,Any}("message" => "changed")),
                         on_log=line->push!(logs2, line))

        for n in ("n1","n2","n3") # not re-run
            @test count(l -> contains(l, uid*"/$n"), logs2) == 0
        end
        for n in ("n4","n5") # re-run
            @test count(l -> contains(l, uid*"/$n"), logs2) > 0
        end
        for n in ("n1","n2","n3","n4","n5")
            @test run2.image_states[uid][n].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 5: Picnic node restarts when per-image upstream input changes ───
    @testset "Resume — picnic node restarts when upstream stale" begin
        proj = create_project!(name="resume-picnic-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(set) → n3(image)
        tpl = ChainTemplate(
            "picnic-resume-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        logs1 = String[]
        run1 = run_chain(proj, [i.uid for i in imgs]; chain="picnic-resume-chain",
                         on_log=line->push!(logs1, line))
        for img in imgs
            @test run1.image_states[img.uid]["n1"].status == :done
            @test run1.image_states[img.uid]["n2"].status == :done
            @test run1.image_states[img.uid]["n3"].status == :done
        end

        # setTask log appeared once in run 1
        set_logs1 = filter(l -> contains(l, "setTask ran"), logs1)
        @test length(set_logs1) == 1

        # Resume with n1 params changed → n1 stale → n2 (picnic) stale → n3 stale
        logs2 = String[]
        run2 = run_chain(proj, String[]; run_id=run1.id,
                         overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "new")),
                         on_log=line->push!(logs2, line))

        # Picnic re-ran (set log appeared again)
        set_logs2 = filter(l -> contains(l, "setTask ran"), logs2)
        @test length(set_logs2) == 1   # ran exactly once in this resume run
        # All nodes redone
        for img in imgs
            @test run2.image_states[img.uid]["n1"].status == :done
            @test run2.image_states[img.uid]["n2"].status == :done
            @test run2.image_states[img.uid]["n3"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Incremental plot node fires as images complete ───────────────
    @testset "Incremental plot node — fires and sets :done state" begin
        proj = create_project!(name="incr-plot-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(incremental plot)
        # debounce_ms=10 so it fires quickly in the test
        tpl = ChainTemplate(
            "incr-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.incrementalPlotTask", scope="incremental",
                       params=Dict{String,Any}("debounce_ms" => 10))],
            [ChainEdge("n1", "n2")],
        )
        save_chain_template!(proj, tpl)

        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="incr-chain", on_log=line->push!(logs, line))

        # All per-image n1 nodes succeeded
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
        end

        # Incremental plot ran — all images' n2 state is :done
        for img in imgs
            @test run.image_states[img.uid]["n2"].status == :done
        end

        # Plot log appeared at least once
        plot_logs = filter(l -> contains(l, "incr/n2"), logs)
        @test !isempty(plot_logs)

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Incremental node does not block per-image progression ────────
    @testset "Incremental plot node — image threads not blocked" begin
        proj = create_project!(name="incr-nob-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        # n1(image) → n2(incremental) → n3(image) — n3 should still run
        # (incremental nodes don't gate downstream per-image nodes)
        tpl = ChainTemplate(
            "incr-pass-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask",          scope="image",
                       params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.incrementalPlotTask", scope="incremental",
                       params=Dict{String,Any}("debounce_ms" => 10))],
            [ChainEdge("n1", "n2"), ChainEdge("n1", "n3")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="incr-pass-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 6: Event bus subscribe/unsubscribe ───────────────────────────────
    @testset "Event bus — subscribe and receive node:done events" begin
        proj = create_project!(name="evbus-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "evbus-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}())],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        received = String[]
        handler  = payload -> push!(received, payload.image_uid)
        subscribe_chain_events!("node:done", handler)

        run = run_chain(proj, [i.uid for i in imgs];
                        chain="evbus-chain", on_log=_->nothing)

        unsubscribe_chain_events!("node:done", handler)

        # Both images fired node:done events
        @test Set(received) ⊇ Set(i.uid for i in imgs)

        # After unsubscribe, new events don't reach the handler
        n_before = length(received)
        run2 = run_chain(proj, [imgs[1].uid]; chain="evbus-chain", on_log=_->nothing)
        @test length(received) == n_before  # unchanged

        rm(proj.root; recursive=true)
    end

    # ── Step 7: Resource pool — concurrency limit respected ──────────────────
    # Pool limit = 1 on n1. Three images each sleep 40ms in n1. With one worker the
    # node executions serialise, so total wall time ≥ 3×40ms (parallel would be ~40ms).
    # (Wall-clock, not event counting: node:running now fires from the pool worker and
    # node:done from the image thread, so a size-1 pool has a benign running/done
    # handoff overlap — execution is still serial, which the timing assertion proves.)
    @testset "Resource pool — concurrency limit respected" begin
        proj = create_project!(name="pool-limit-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pool-limit-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 40),
                       resource_pool="slow_pool"),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}())],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        # Pools are global (scheduler.jl _POOLS); register the test pool at limit 1.
        resize_pool!("slow_pool", 1)
        t0  = time()
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="pool-limit-chain",
                        on_log=_->nothing)
        elapsed = time() - t0

        # Serialised: ≥ 3×40ms of n1 work. Parallel would finish in ~40-60ms.
        @test elapsed >= 0.10
        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 7: Resource pool — higher limit allows parallel execution ────────
    @testset "Resource pool — limit=3 allows all concurrent" begin
        proj = create_project!(name="pool-par-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pool-par-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 40),
                       resource_pool="par_pool")],
            ChainEdge[],
        )
        save_chain_template!(proj, tpl)

        max_concurrent = Threads.Atomic{Int}(0)
        current        = Threads.Atomic{Int}(0)

        sh = payload -> begin
            payload.node_id == "n1" || return
            n = Threads.atomic_add!(current, 1) + 1
            Threads.atomic_max!(max_concurrent, n)
            nothing
        end
        dh = payload -> (payload.node_id == "n1" && Threads.atomic_sub!(current, 1); nothing)

        subscribe_chain_events!("node:running", sh)
        subscribe_chain_events!("node:done",    dh)

        resize_pool!("par_pool", 3)
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="pool-par-chain",
                        on_log=_->nothing)

        unsubscribe_chain_events!("node:running", sh)
        unsubscribe_chain_events!("node:done",    dh)

        # With limit 3 and 3 images all able to run simultaneously, max should be 3
        @test max_concurrent[] == 3

        rm(proj.root; recursive=true)
    end

    # ── Step 7: Pipelining — image A reaches n2 before image B finishes n1 ───
    # Pool limit=1 on n1 (each image sleeps 80ms there). Image A exits n1 first
    # and immediately enters n2 (instant). B and C are still queuing for n1.
    # Verify: A's n2 completion timestamp < B's n1 completion timestamp.
    @testset "Pipelining — n2 of first image before n1 of second image" begin
        proj = create_project!(name="pipeline-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "pipeline-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 80),
                       resource_pool="serial_pool"),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}("waitMs" => 0))],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        done_times = Dict{String,Float64}()  # "uid/nid" => time()
        th = payload -> (done_times["$(payload.image_uid)/$(payload.node_id)"] = time(); nothing)
        subscribe_chain_events!("node:done", th)

        resize_pool!("serial_pool", 1)
        run = run_chain(proj, [i.uid for i in imgs];
                        chain="pipeline-chain",
                        on_log=_->nothing)

        unsubscribe_chain_events!("node:done", th)

        # Find the image that finished n1 first (earliest n1 completion)
        first_uid  = argmin(uid -> done_times["$(uid)/n1"], [i.uid for i in imgs])
        other_uids = filter(i -> i.uid != first_uid, imgs)

        # The first image's n2 must have finished before any other image's n1 did
        t_first_n2 = done_times["$(first_uid)/n2"]
        for other in other_uids
            @test t_first_n2 < done_times["$(other.uid)/n1"]
        end

        rm(proj.root; recursive=true)
    end

    # ── Step 7: Cross-image fault isolation ──────────────────────────────────
    # Image A fails at n1. Images B and C proceed through n1→n2 unaffected.
    # (Different from Step 5's test which checks downstream skips on the SAME image.)
    #
    # We make n1 = RemoveImage, which requires a registered zarr to succeed.
    # img_b and img_c have a real zarr; img_a does not → img_a fails at n1.
    # n2 = testTasks.imageTask (always succeeds).
    # Expected: img_a fails n1, skips n2. img_b and img_c succeed both nodes.
    @testset "Cross-image fault isolation" begin
        proj = create_project!(name="xiso-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img_a = add_image!(s; name="img-a")   # no zarr → RemoveImage will fail
        img_b = add_image!(s; name="img-b")
        img_c = add_image!(s; name="img-c")

        for img in (img_b, img_c)
            zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
            mkpath(zarr)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.filepath["_active"] = "default"
            img.status = "done"
            save!(img)
        end

        tpl = ChainTemplate(
            "xiso-chain",
            [ChainNode(id="n1", fn="importImages.remove", scope="image",
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
             ChainNode(id="n2", fn="testTasks.imageTask", scope="image",
                       params=Dict{String,Any}())],
            [ChainEdge("n1","n2")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [img_a.uid, img_b.uid, img_c.uid];
                        chain="xiso-chain", on_log=_->nothing)

        # img_a: n1 failed (no zarr), n2 skipped
        @test run.image_states[img_a.uid]["n1"].status == :failed
        @test run.image_states[img_a.uid]["n2"].status == :skipped

        # img_b and img_c: both nodes succeeded — not affected by img_a's failure
        @test run.image_states[img_b.uid]["n1"].status == :done
        @test run.image_states[img_b.uid]["n2"].status == :done
        @test run.image_states[img_c.uid]["n1"].status == :done
        @test run.image_states[img_c.uid]["n2"].status == :done

        rm(proj.root; recursive=true)
    end

    # ── Step 7: run_chain headless — no api/ loaded ───────────────────────────
    # All tests in this file run without `using` api/. This testset makes the
    # contract explicit: run_chain on a picnic chain produces correct results
    # with nothing but `using Cecelia`.
    @testset "run_chain headless (no api/ dependency)" begin
        proj = create_project!(name="headless-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

        tpl = ChainTemplate(
            "headless-chain",
            [ChainNode(id="n1", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}()),
             ChainNode(id="n2", fn="testTasks.setTask",   scope="set",   params=Dict{String,Any}()),
             ChainNode(id="n3", fn="testTasks.imageTask", scope="image", params=Dict{String,Any}())],
            [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        )
        save_chain_template!(proj, tpl)

        run = run_chain(proj, [i.uid for i in imgs]; chain="headless-chain", on_log=_->nothing)

        for img in imgs
            @test run.image_states[img.uid]["n1"].status == :done
            @test run.image_states[img.uid]["n2"].status == :done
            @test run.image_states[img.uid]["n3"].status == :done
        end
        @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2

        rm(proj.root; recursive=true)
    end

    # ── fun_name dispatch ─────────────────────────────────────────────────────
    @testset "fun_name dispatch" begin
        @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
        @test _task_from_fun_name("importImages.remove")  isa RemoveImage
        @test _task_from_fun_name("cleanupImages.cellposeCorrect") isa CellposeCorrect
        @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
        @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
        @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
        @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
        composite = _task_from_fun_name("cleanupImages.afDriftCorrect")
        @test composite isa CompositeTask
        @test composite.fun_name == "cleanupImages.afDriftCorrect"
        cp_measure = _task_from_fun_name("segment.cellposeMeasure")
        @test cp_measure isa CompositeTask
        @test cp_measure.fun_name == "segment.cellposeMeasure"
        @test_throws ErrorException _task_from_fun_name("nonexistent.task")
        @test _task_from_fun_name("tracking.bayesian_tracking")    isa BayesianTracking
        @test _task_from_fun_name("tracking.track_measures")       isa TrackMeasures
        bt_measures = _task_from_fun_name("tracking.bayesian_track_measures")
        @test bt_measures isa CompositeTask
        @test bt_measures.fun_name == "tracking.bayesian_track_measures"
    end

    # ── BayesianTracking param validation ─────────────────────────────────────
    @testset "BayesianTracking params" begin
        task = BayesianTracking()
        good = Dict{String,Any}(
            "valueName" => "default", "popsToTrack" => "NONE",
            "maxSearchRadius" => 20, "maxLost" => 3, "trackBranching" => false,
            "minTimepoints" => 5, "accuracy" => 0.8, "probToAssign" => 0.8,
            # advanced section params are flattened by the frontend before submit
            "noiseInital" => 300, "distThresh" => 10.0, "segmentationMissRate" => 0.1,
        )
        @test begin validate_params(task, good); true end
        # maxSearchRadius max is 200
        @test_throws ParamValidationError validate_params(
            task, merge(good, Dict{String,Any}("maxSearchRadius" => 500)))
        # segmentationMissRate min is 0.001
        @test_throws ParamValidationError validate_params(
            task, merge(good, Dict{String,Any}("segmentationMissRate" => 0.0)))
    end

    # ── TrackMeasures param validation ────────────────────────────────────────
    @testset "TrackMeasures params" begin
        task = TrackMeasures()
        @test begin validate_params(task,
            Dict{String,Any}("valueName" => "B", "forceRecompute" => false)); true end
        # forceRecompute is bool — a non-bool must be rejected. Also a guard that params use
        # "key" (not "id"): with "id" the spec key resolves empty and validation silently skips.
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("valueName" => "B", "forceRecompute" => "yes"))
    end

    # ── Labels field round-trip ───────────────────────────────────────────────
    # Regression guard: the `labels` Dict written by cellposeSegment must survive
    # save!/init_object and land at the agreed location in ccid.json.
    @testset "Labels field round-trip" begin
        proj = create_project!(name="labels-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        img.labels["default"] = ["default.zarr", "default_nuc.zarr"]
        save!(img)

        # Reloaded value is correct in memory
        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test haskey(r.labels, "default")
        @test r.labels["default"] == ["default.zarr", "default_nuc.zarr"]

        # On-disk shape: top-level "labels" dict with string-vector values
        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "labels")
        label_val = collect(String, raw["labels"]["default"])
        @test label_val == ["default.zarr", "default_nuc.zarr"]

        rm(proj.root; recursive=true)
    end

    # ── CompositeTask — spec loads and composite array is correct ─────────────
    @testset "CompositeTask spec" begin
        task = CompositeTask("cleanupImages.afDriftCorrect")
        spec = Cecelia._task_spec(task)
        @test !isnothing(spec)
        @test haskey(spec, "composite")
        steps = [string(s) for s in spec["composite"]]
        @test steps == ["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]
        @test get(spec, "fun_name", "") == "cleanupImages.afDriftCorrect"
    end

    # ── $include fragment resolution ──────────────────────────────────────────
    # Verifies that {"$include": "imageTiling"} in cellpose.json is expanded
    # to the 4 shared tiling params (blockSize, overlap, blockSizeZ, overlapZ).
    @testset "\$include fragment resolution" begin
        task = CellposeSegment()
        spec = Cecelia._task_spec(task)
        @test !isnothing(spec)
        # Find the imageTiling section
        tiling_sec = nothing
        for p in spec["params"]
            p isa AbstractDict && string(get(p, "key", "")) == "imageTiling" &&
                (tiling_sec = p; break)
        end
        @test !isnothing(tiling_sec)
        tiling_params = tiling_sec["params"]
        keys_in_tiling = [string(get(p, "key", "")) for p in tiling_params if p isa AbstractDict]
        # Fragment contributes these 4; cellpose.json adds labelOverlap
        @test "blockSize"  ∈ keys_in_tiling
        @test "overlap"    ∈ keys_in_tiling
        @test "blockSizeZ" ∈ keys_in_tiling
        @test "overlapZ"   ∈ keys_in_tiling
        @test "labelOverlap" ∈ keys_in_tiling
        @test length(keys_in_tiling) == 5   # 4 from fragment + 1 inline
        # No raw $include entries should survive
        @test !any(p isa AbstractDict && haskey(p, "\$include") for p in tiling_params)
    end

    # ── label_props field round-trip ──────────────────────────────────────────
    @testset "label_props field round-trip" begin
        proj = create_project!(name="lp-rt-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")

        img.label_props["default"] = "default.h5ad"
        save!(img)

        r = init_object(proj.uid, img.uid)
        @test r isa CciaImage
        @test get(r.label_props, "default", nothing) == "default.h5ad"

        raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
        @test haskey(raw, "label_props")
        @test string(raw["label_props"]["default"]) == "default.h5ad"

        rm(proj.root; recursive=true)
    end

    # ── Param validation — AfCorrect (group with flat sub-params) ─────────────
    @testset "Param validation — AfCorrect" begin
        task = AfCorrect()

        # Valid group entry — should not throw
        good = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("correctionMode" => "divide", "channelPercentile" => 60.0)))
        @test begin validate_params(task, good); true end

        # correctionMode is a select — unknown value must be rejected
        bad_select = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("correctionMode" => "unknown_mode")))
        @test_throws ParamValidationError validate_params(task, bad_select)

        # channelPercentile is float min=0/max=100 — out of range must be rejected
        bad_float = Dict{String,Any}("afCombinations" => Dict{String,Any}(
            "0" => Dict{String,Any}("channelPercentile" => 150.0)))
        @test_throws ParamValidationError validate_params(task, bad_float)
    end

    # ── Param validation — DriftCorrect ───────────────────────────────────────
    @testset "Param validation — DriftCorrect" begin
        task = DriftCorrect()

        # Valid select value
        @test begin validate_params(task, Dict{String,Any}("driftNormalisation" => "none")); true end
        @test begin validate_params(task, Dict{String,Any}("driftNormalisation" => "phase")); true end

        # Invalid select value
        @test_throws ParamValidationError validate_params(
            task, Dict{String,Any}("driftNormalisation" => "invalid"))
    end

    # ── Versioned helpers ─────────────────────────────────────────────────────
    @testset "Versioned dict helpers" begin
        d = Dict{String,Any}()
        versioned_set_field!(d, "filepath", "ccidImage.ome.zarr")
        @test versioned_get_field(d, "filepath") == "ccidImage.ome.zarr"
        @test versioned_active(d["filepath"]) == "default"

        versioned_set_field!(d, "filepath", "ccidCpCorrected.ome.zarr", "cpCorrected")
        @test versioned_get_field(d, "filepath", "cpCorrected") == "ccidCpCorrected.ome.zarr"
        @test versioned_get_field(d, "filepath") == "ccidCpCorrected.ome.zarr"  # active = cpCorrected

        versioned_set_field!(d, "filepath", nothing, "cpCorrected")
        @test isnothing(get(d["filepath"], "cpCorrected", nothing))
    end

    # ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────
    @testset "LabelProps reader" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "LabelProps reader (fixture missing)"
        else
            # metadata (cheap reads)
            @test length(col_names(label_props(h5); data_type=:vars)) == 27
            @test channel_columns(label_props(h5)) ==
                  ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2", "mean_intensity_3"]
            @test centroid_columns(label_props(h5)) == ["centroid-0", "centroid-1", "centroid-2"]
            @test temporal_columns(label_props(h5)) == ["t"]

            # full frame: label + 27 vars + 3 spatial + 1 temporal + 8 obs (track lineage +
            # live.cell.* from tracking.track_measures) = 40 cols, 1377 rows
            df = label_props(h5) |> as_df
            @test size(df) == (1377, 40)
            # n_obs is the cheap dims-only count — must agree with the materialised row count
            @test n_obs(label_props(h5)) == 1377
            @test "label" in names(df)
            @test eltype(df.label) == Int64
            @test df.label[1:5] == [0, 1, 2, 3, 4]

            # X orientation correctness (audited values).
            # NOTE: intentional coupling to the committed fixture state of KDIeEm/B.h5ad — these
            # are the actual bbox values in that file, asserting /X is read with correct row/col
            # orientation (not transposed). If this breaks, it's either (a) the reader regressed,
            # or (b) the fixture was deliberately regenerated (e.g. segmentation rerun) — in which
            # case re-audit and update these constants. A failure here is NOT "the test is wrong".
            @test [df[1, "bbox-$j"] for j in 0:4] == Float32[0, 0, 71, 2, 29]
            @test [df[2, "bbox-$j"] for j in 0:4] == Float32[0, 7, 368, 4, 38]

            # is_tracked's signal: a tracked segmentation carries a track_id obs column (KDIeEm/B is
            # tracked). track_props / the track-grained gating plots key off this to say "track first"
            # (empty) instead of erroring when it's absent.
            obs = col_names(label_props(h5); data_type=:obs)
            @test "track_id" in obs
            @test !("not_a_column" in obs)

            # lazy column selection — only requested columns (+ label) are returned
            @test names(label_props(h5) |> select_cols(["area"]) |> as_df) == ["label", "area"]

            # centroid + intensity selection
            @test Set(names(label_props(h5) |> select_cols(["mean_intensity_0"]) |> view_centroid_cols |> as_df)) ==
                  Set(["label", "mean_intensity_0", "centroid-0", "centroid-1", "centroid-2", "t"])

            # row filter by label
            d4 = label_props(h5) |> filter_rows([0, 1, 2]; by=:label) |> as_df
            @test sort(d4.label) == [0, 1, 2]

            # filter is intersection: nonexistent IDs are silently skipped (≤ requested, no NaN/error)
            d4b = label_props(h5) |> filter_rows([0, 1, 999_999]; by=:label) |> as_df
            @test sort(d4b.label) == [0, 1]

            # sort by area, descending
            d5 = label_props(h5) |> select_cols(["area"]) |> sort_by("area"; rev=true) |> as_df
            @test d5.area[1] == maximum(d5.area)
        end
    end

    # ── LabelProps writer (add_obs / save! — the chain write path) ─────────────
    @testset "LabelProps writer" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "LabelProps writer (fixture missing)"
        else
            tmp = joinpath(mktempdir(), "B.h5ad")
            cp(h5, tmp)

            existing = label_props(tmp) |> as_df
            some = existing.label[1:5]                       # write to a subset of labels
            df = DataFrame("label" => some,
                           "test.measure" => Float64.(1:5),
                           "test.other"   => [10.0, 20.0, 30.0, 40.0, 50.0])
            # the documented chain write idiom
            label_props(tmp) |> add_obs(df) |> save!

            # new columns appear in obs column-order
            obs_cols = col_names(label_props(tmp); data_type=:obs)
            @test "test.measure" in obs_cols
            @test "test.other" in obs_cols

            # read back via the reader, aligned by label; unset labels are NaN
            back = label_props(tmp) |> select_cols(["test.measure", "test.other"]) |> as_df
            byrow = Dict(l => i for (i, l) in enumerate(back.label))
            for (k, lab) in enumerate(some)
                @test back[byrow[lab], "test.measure"] == Float64(k)
            end
            # a label not in df → NaN
            other = first(setdiff(back.label, some))
            @test isnan(back[byrow[other], "test.measure"])

            # original data preserved (var count unchanged — obs append only, no X rewrite)
            @test length(col_names(label_props(tmp); data_type=:vars)) ==
                  length(col_names(label_props(h5);  data_type=:vars))

            # idempotent overwrite: re-writing the same column updates, doesn't duplicate
            df2 = DataFrame("label" => some, "test.measure" => fill(99.0, 5))
            label_props(tmp) |> add_obs(df2) |> save!
            @test count(==("test.measure"), col_names(label_props(tmp); data_type=:obs)) == 1
            back2 = label_props(tmp) |> select_cols(["test.measure"]) |> as_df
            @test back2[Dict(l => i for (i, l) in enumerate(back2.label))[some[1]], "test.measure"] == 99.0

            # drop_obs: remove a column; gone from column-order and from as_df
            label_props(tmp) |> drop_obs(["test.measure"]) |> save!
            @test "test.measure" ∉ col_names(label_props(tmp); data_type=:obs)
            @test "test.other"   ∈ col_names(label_props(tmp); data_type=:obs)   # sibling untouched
            @test "test.measure" ∉ names(label_props(tmp) |> as_df)
            # dropping a nonexistent column is a no-op (idempotent)
            @test begin label_props(tmp) |> drop_obs(["never.existed"]) |> save!; true end

            # combined drop + add in one chain (invalidate-then-write, e.g. btrack rerun)
            df3 = DataFrame("label" => some, "test.fresh" => Float64.(1:5))
            label_props(tmp) |> drop_obs(["test.other"]) |>
                                add_obs(df3) |> save!
            cols3 = col_names(label_props(tmp); data_type=:obs)
            @test "test.other" ∉ cols3
            @test "test.fresh" ∈ cols3

            # drop + re-add the SAME column in one chain → the add wins (column survives with new
            # values). Regression: the drop used to de-list and delete the just-written dataset, so
            # e.g. overwriting a categorical hmm.state with a numeric one in one chain lost it.
            df4 = DataFrame("label" => some, "test.fresh" => Float64.(101:105))
            label_props(tmp) |> drop_obs(["test.fresh"]) |>
                                add_obs(df4) |> save!
            @test "test.fresh" ∈ col_names(label_props(tmp); data_type=:obs)
            back4 = label_props(tmp) |> select_cols(["test.fresh"]) |> as_df
            row4  = Dict(l => i for (i, l) in enumerate(back4.label))
            for (k, lab) in enumerate(some)
                @test back4[row4[lab], "test.fresh"] == Float64(100 + k)
            end
        end
    end

    # ── Julia ↔ Python reader parity (the duplication safety net) ──────────────
    # The Julia LabelProps reader and the Python LabelPropsView are two implementations of
    # ONE spec (docs/DATAMODEL.md). They can drift — a new encoding type added to one and not
    # the other. This runs BOTH against the same fixture and compares. Gated on the napari
    # venv + anndata being importable, so headless CI without Python skips rather than fails.
    @testset "LabelProps Julia/Python parity" begin
        h5    = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        pybin = python_bin_path()
        py_ok = !isempty(pybin) && isfile(pybin) &&
                success(setenv(`$pybin -c "import anndata, numpy, pandas"`, dir=@__DIR__))
        if !have_fixture(h5) || !py_ok
            @test_skip "LabelProps parity (fixture or python+anndata unavailable)"
        else
            # Python side dumps a comparable summary as JSON via the LabelPropsView reader.
            pyscript = """
            import sys, json
            import cecelia.utils.label_props_utils as lpu
            v = lpu.LabelPropsView(sys.argv[1])
            df  = v.view_cols(["mean_intensity_0"]).as_df().sort_values("label")
            cv  = lpu.LabelPropsView(sys.argv[1]).only_centroid_cols().as_df().sort_values("label")
            print(json.dumps({
                "var_names":      list(v.var_names()),
                "obs_cols":       list(v.adata.obs.columns),
                "centroid_cols":  list(v.centroid_columns()),
                "temporal_cols":  list(v.temporal_columns()),
                "n_obs":          int(len(v.labels())),
                "labels5":        [int(x) for x in df["label"].to_numpy()[:5]],
                "mean_int0_5":    [float(x) for x in df["mean_intensity_0"].to_numpy()[:5]],
                "centroid0_5":    [float(x) for x in cv["centroid-0"].to_numpy()[:5]],
            }))
            """
            # python/ on PYTHONPATH so `import cecelia.utils...` resolves (matches run_py's PYTHONPATH).
            py_dir = joinpath(dirname(dirname(@__DIR__)), "python")   # app/test → app → repo-root/python
            penv = copy(ENV); penv["PYTHONPATH"] = py_dir
            out = read(setenv(`$pybin -c $pyscript $h5`, penv; dir=py_dir), String)
            py = JSON3.read(out)

            lp = label_props(h5)
            @test Set(col_names(lp; data_type=:vars)) == Set(String.(py.var_names))
            @test Set(col_names(lp; data_type=:obs))  == Set(String.(py.obs_cols))
            @test Set(centroid_columns(lp))           == Set(String.(py.centroid_cols))
            @test Set(temporal_columns(lp))           == Set(String.(py.temporal_cols))

            # value parity on the same labels (sorted), one var col + one centroid col
            dj = label_props(h5) |> select_cols(["mean_intensity_0"]) |> sort_by("label") |> as_df
            @test dj.label[1:5] == collect(Int, py.labels5)
            @test dj[1:5, "mean_intensity_0"] ≈ Float64.(py.mean_int0_5)
            cj = label_props(h5) |> view_centroid_cols |> sort_by("label") |> as_df
            @test cj[1:5, "centroid-0"] ≈ Float64.(py.centroid0_5)
        end
    end

    # ── Track measures: numeric cross-check vs celltrackR ─────────────────────
    # Golden values — provenance:
    #   Generated from celltrackR 1.2.2 (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006)
    #   on the track below, via the R package's own functions (trackLength/speed/displacement/
    #   straightness/asphericity/overallAngle/meanTurningAngle, degrees=TRUE; per-step via
    #   subtracks()). celltrackR is the reference Cecelia ported these from; it is NOT a runtime
    #   dependency — these constants pin the port to the original. If a measure here changes,
    #   either the port regressed or it was deliberately changed (then re-derive from celltrackR).
    @testset "Track measures (celltrackR golden)" begin
        Track = Cecelia.Track
        t  = [0.0, 10, 20, 30, 40]
        c3 = [0.0 0 0; 3 4 0; 7 4 2; 7 8 2; 10 12 5]    # (x,y,z) per position
        tr = Track(1, t, c3)

        @test Cecelia.track_length(tr)            ≈ 19.3030878498 atol=1e-6
        @test Cecelia.track_duration(tr)          == 40.0
        @test Cecelia.track_speed(tr)             ≈ 0.4825771962  atol=1e-6
        @test Cecelia.track_displacement(tr)      ≈ 16.4012194669 atol=1e-6
        @test Cecelia.max_displacement(tr)        ≈ 16.4012194669 atol=1e-6
        @test Cecelia.track_straightness(tr)      ≈ 0.8496681772  atol=1e-6
        @test Cecelia.track_displacement_ratio(tr) ≈ 1.0          atol=1e-6
        @test Cecelia.track_outreach_ratio(tr)    ≈ 0.8496681772  atol=1e-6
        @test Cecelia.track_asphericity(tr)       ≈ 0.8469835416  atol=1e-6
        @test Cecelia.track_overall_angle(tr)     ≈ 30.9637565321 atol=1e-6
        @test Cecelia.track_mean_turning_angle(tr) ≈ 64.7432782933 atol=1e-6

        # per-cell subtracks (celltrackR subtracks(·,1) speed; subtracks(·,2) overallAngle)
        ss = Cecelia.step_speeds(tr)              # cell_id 1 → NaN; i>1 = step speed to endpoint
        @test isnan(ss[1])
        @test ss[2:5] ≈ [0.5, 0.4472135955, 0.4, 0.5830951895] atol=1e-6
        sa = Cecelia.step_turning_angles(tr)      # cell_id 1,2 → NaN; i≥3 = turn angle (deg)
        @test all(isnan, sa[1:2])
        @test sa[3:5] ≈ [57.5436915381, 90.0, 46.6861433417] atol=1e-6

        # 2D path (drop z) — same functions, no call-site branching
        tr2 = Track(1, t, c3[:, 1:2])
        @test Cecelia.track_straightness(tr2)  ≈ 0.8678055195 atol=1e-6
        @test Cecelia.track_asphericity(tr2)   ≈ 0.8287305960 atol=1e-6
        @test Cecelia.track_overall_angle(tr2) ≈ 0.0          atol=1e-6  # first ∥ last step in xy

        # ── edge cases (Step 4 mandates these) ────────────────────────────────
        # single-step track (2 positions): measures needing ≥3 steps → NaN; no crash
        one = Track(1, [0.0, 10], [0.0 0 0; 3 4 0])
        @test Cecelia.track_length(one)            ≈ 5.0
        @test Cecelia.track_speed(one)             ≈ 0.5
        @test Cecelia.track_straightness(one)      ≈ 1.0          # straight by definition
        @test isnan(Cecelia.track_overall_angle(one))            # n<3
        @test isnan(Cecelia.track_mean_turning_angle(one))
        @test Cecelia.track_asphericity(one)       == 1.0         # celltrackR convention for <3

        # single-position track: no div-by-zero, sane fallbacks
        pt = Track(1, [0.0], reshape([0.0, 0, 0], 1, 3))
        @test Cecelia.track_length(pt)        == 0.0
        @test isnan(Cecelia.track_speed(pt))                     # duration 0
        @test Cecelia.track_straightness(pt)  == 1.0             # length 0 → 1
        @test isnan(Cecelia.track_displacement_ratio(pt))        # maxDisplacement 0

        # zero net displacement (returns to origin): straightness 0, no div-by-zero
        loop = Track(1, [0.0, 1, 2], [0.0 0; 1 0; 0 0])
        @test Cecelia.track_displacement(loop)       ≈ 0.0
        @test Cecelia.track_length(loop)             ≈ 2.0
        @test Cecelia.track_straightness(loop)       ≈ 0.0
        @test Cecelia.track_displacement_ratio(loop) ≈ 0.0       # disp 0 / maxDisp 1
        @test Cecelia.track_outreach_ratio(loop)     ≈ 0.5       # maxDisp 1 / length 2
    end

    # ── Gating engine: transforms ─────────────────────────────────────────────
    @testset "Transforms" begin
        # linear is identity
        @test apply_transform(LinearTransform(), 42.0) == 42.0
        @test invert_transform(LinearTransform(), 42.0) == 42.0

        # log / asinh round-trip
        lg = LogTransform()
        @test invert_transform(lg, apply_transform(lg, 1234.0)) ≈ 1234.0
        ah = AsinhTransform(cofactor=150.0)
        for x in (-500.0, 0.0, 37.0, 9000.0)
            @test invert_transform(ah, apply_transform(ah, x)) ≈ x atol=1e-6
        end

        # Logicle golden values — provenance:
        #   Generated once from FlowUtils' reference C implementation (`logicle_c`, the C port
        #   of Moore & Parks 2012), params T=262144 W=0.5 M=4.5 A=0, via:
        #     flowutils.transforms.logicle(x, channel_indices=[0], t=262144, m=4.5, w=0.5, a=0)
        #   flowutils was used transiently only to produce these numbers; it is NOT a runtime
        #   dependency. The values are baked in below so this test needs no Python at runtime.
        #   See app/src/gating/transforms.jl for the full citation.
        lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        golden = [(-1000.0, -0.2321153540), (-100.0, 0.0090411347), (0.0, 0.1111111111),
                  (1.0, 0.1122315321), (10.0, 0.1223042757), (100.0, 0.2131810875),
                  (1000.0, 0.4543375762), (10000.0, 0.6838326572),
                  (100000.0, 0.9069275915), (262144.0, 1.0)]
        for (x, gy) in golden
            @test apply_transform(lc, x) ≈ gy atol=1e-6
        end
        # round-trip
        for x in (-1000.0, 0.0, 1.0, 1000.0, 262144.0)
            @test invert_transform(lc, apply_transform(lc, x)) ≈ x atol=1e-4
        end
        # vectorised
        @test apply_transform(lc, [0.0, 262144.0]) ≈ [0.1111111111, 1.0] atol=1e-6

        # Range-based auto-linearisation: logicle collapses a bounded 0–1 measure (morphology) but
        # spreads a real intensity range → effective_transform swaps to linear only for the former.
        @test transform_kind(lc) == "logicle"
        @test transform_collapses(lc, 0.02, 1.0)              # solidity ∈ [0,1] → collapses
        @test !transform_collapses(lc, 0.0, 262144.0)         # full intensity range → fine
        @test !transform_collapses(lc, 0.0, 5000.0)           # large-range morphology (area) → keep logicle
        @test effective_transform(lc, 0.02, 1.0) isa LinearTransform
        @test effective_transform(lc, 0.0, 262144.0) === lc   # untouched
        @test effective_transform(LinearTransform(), 0.02, 1.0) isa LinearTransform  # linear never coerces
        @test !transform_collapses(LinearTransform(), 0.02, 1.0)
        # log needs ≥1 decade above the floor; asinh coerces when all data is inside its ~linear core
        @test transform_collapses(LogTransform(floor=1.0), 0.5, 5.0)
        @test !transform_collapses(LogTransform(floor=1.0), 1.0, 1e5)
        @test transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 1.0)
        @test !transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 5000.0)
        # degenerate extent (single value / non-finite) → no coercion, keep requested
        @test effective_transform(lc, 1.0, 1.0) === lc
        @test effective_transform(lc, NaN, 1.0) === lc
    end

    # ── Gating engine: gates ──────────────────────────────────────────────────
    @testset "Gates" begin
        # rectangle (linear): inclusive bounds
        rg = RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0)
        xin = inside(rg, [5.0, 11.0, -1.0, 0.0], [5.0, 5.0, 5.0, 0.0])
        @test xin == BitVector([true, false, false, true])

        # polygon point-in-polygon (unit square)
        sq = [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
        @test point_in_polygon(0.5, 0.5, sq)
        @test !point_in_polygon(1.5, 0.5, sq)
        @test !point_in_polygon(-0.1, 0.5, sq)
        pg = PolygonGate("x", "y", sq)
        @test inside(pg, [0.5, 2.0], [0.5, 2.0]) == BitVector([true, false])

        # transformed-space gate: a rectangle in logicle coords selects by raw value
        lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        # logicle(1000)=0.4543, logicle(100000)=0.9069 → gate [0.45,0.95] keeps 1000 & 100000, not 10
        rgl = RectangleGate("x", "y", 0.45, 0.95, 0.45, 0.95;
                            x_transform=lc, y_transform=lc)
        keep = inside(rgl, [10.0, 1000.0, 100000.0], [1000.0, 1000.0, 100000.0])
        @test keep == BitVector([false, true, true])

        # JSON round-trip preserves membership
        for g in (rg, pg, rgl)
            g2 = gate_from_spec(gate_spec(g))
            @test inside(g2, [5.0, 1000.0], [5.0, 1000.0]) == inside(g, [5.0, 1000.0], [5.0, 1000.0])
        end

        # project_gate: re-express a gate's stored geometry into a DISPLAY transform so its outline
        # aligns with points drawn in that transform (the client has no transform math).
        lcp = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
        # a rectangle stored in logicle coords, projected onto a LINEAR display → raw bounds
        rgp = RectangleGate("cd4", "cd8", 0.4543375762, 0.9069275915, 0.4543375762, 0.9069275915;
                            x_transform=lcp, y_transform=lcp)
        lin = LinearTransform()
        pj = project_gate(rgp, "cd4", "cd8", lin, lin)
        @test pj["kind"] == "rectangle"
        @test pj["x_min"] ≈ 1000.0 atol=1e-2      # invert(logicle, 0.4543) ≈ 1000
        @test pj["x_max"] ≈ 100000.0 atol=1e-1    # invert(logicle, 0.9069) ≈ 100000
        # projecting onto the SAME transform is (near) identity
        same = project_gate(rgp, "cd4", "cd8", lcp, lcp)
        @test same["x_min"] ≈ 0.4543375762 atol=1e-4
        # swapped channel order → x/y transposed
        sw = project_gate(rgp, "cd8", "cd4", lin, lin)
        @test sw !== nothing
        @test sw["y_min"] ≈ 1000.0 atol=1e-2
        # not on this channel pair → nothing
        @test project_gate(rgp, "cd4", "cd19", lin, lin) === nothing
        # polygon vertices map pointwise
        pgp = PolygonGate("cd4", "cd8", [(0.4543375762, 0.4543375762), (0.9069275915, 0.4543375762),
                                         (0.9069275915, 0.9069275915)]; x_transform=lcp, y_transform=lcp)
        # onto a DIFFERENT display transform (logicle→linear) the edges curve → sampled 12 pts/edge
        pjp = project_gate(pgp, "cd4", "cd8", lin, lin)
        @test pjp["kind"] == "polygon"
        @test length(pjp["vertices"]) == 36                 # 3 edges × 12 samples
        @test pjp["vertices"][1][1] ≈ 1000.0 atol=1e-2       # first sample = corner 1
        # edge 1 is horizontal (y const ≈1000); its midpoint (idx 7 = t=0.5) bows far below the straight
        # chord's linear midpoint (~50500) — that bow is the curve the sampling captures.
        @test pjp["vertices"][7][2] ≈ 1000.0 atol=1e-2
        @test pjp["vertices"][7][1] < 50000
        # onto the SAME transform edges are already straight → corners kept (clean edit round-trip)
        @test length(project_gate(pgp, "cd4", "cd8", lcp, lcp)["vertices"]) == 3
    end

    # ── Gating engine: density ────────────────────────────────────────────────
    @testset "Density" begin
        x = collect(0.0:0.01:1.0)
        d = density_2d(x, x; bins=10)
        @test sum(d.counts) == length(x)          # every point counted once
        @test size(d.counts) == (10, 10)
        @test d.counts[1, 10] == 0                 # off-diagonal empty (x==y data)
        @test all(d.counts[i, i] >= 1 for i in 1:10)

        # NaN/Inf (object/morphology measures on degenerate objects) must be skipped, not throw —
        # extents come from the finite values, and a non-finite point contributes to no bin.
        xn = [0.0, 0.5, 1.0, NaN, Inf, -Inf]
        dn = density_2d(xn, xn; bins=10)
        @test sum(dn.counts) == 3                  # only the 3 finite points counted
        @test (dn.x_min, dn.x_max) == (0.0, 1.0)   # extents ignore NaN/Inf
        # all-non-finite → falls back to the default extent and empty counts (no throw)
        dz = density_2d([NaN, Inf], [NaN, Inf]; bins=4)
        @test sum(dz.counts) == 0
    end

    # ── Population manager: paths, tree, persistence ──────────────────────────
    @testset "Population manager" begin
        @test pop_parent("/a/b") == "/a"
        @test pop_parent("/a") == ROOT
        @test pop_name("/a/b") == "b"
        @test pop_path("/a", "b") == "/a/b"
        @test pop_path(ROOT, "a") == "/a"
        @test is_root(ROOT) && is_root("/") && !is_root("/a")

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10), colour="#f00")
        add_pop!(m, "cd8"; parent="/cd4", gate=PolygonGate("x", "y", [(0.,0.),(0.,5.),(5.,5.),(5.,0.)]))
        @test pop_paths(m) == ["/cd4", "/cd4/cd8"]
        @test direct_children(m, ROOT) == ["/cd4"]
        @test descendants(m, "/cd4") == ["/cd4/cd8"]

        # save/load round-trip (tree + gate)
        td = mktempdir()
        save_pop_map!(m, td)
        @test isfile(gating_path(td, "B"))
        # atomic write: the temp file is renamed into place, never left behind
        @test !isfile(gating_path(td, "B") * ".tmp")
        m2 = load_pop_map(td, "B")
        @test pop_paths(m2) == pop_paths(m)
        @test pop_at(m2, "/cd4").colour == "#f00"
        @test pop_at(m2, "/cd4").gate isa RectangleGate
        @test pop_at(m2, "/cd4/cd8").gate isa PolygonGate

        # cascade rename
        rename_pop!(m, "/cd4", "tcell")
        @test Set(pop_paths(m)) == Set(["/tcell", "/tcell/cd8"])
        @test pop_at(m, "/tcell/cd8").parent == "/tcell"
        # cascade delete
        del_pop!(m, "/tcell")
        @test isempty(pop_paths(m))
    end

    # ── clust / trackclust pop types (cluster-membership populations) ─────────────
    # A cluster pop is a filter on the `clusters.{suffix}` column (clustPops/clustTracks output):
    # filter_fun="in", filter_values=[ticked cluster ids]. Stored in its own sidecar so it never
    # collides with flow gates. Headless — membership via a recompute! closure (no fixture).
    # gating pop types = the hand-drawn ones (flow=cells, track=tracks); clust/trackclust are filters.
    # Drives copy-to-images + the defining-plot view (one abstraction over both, no flow special-casing).
    @testset "GATING_POP_TYPES" begin
        @test GATING_POP_TYPES == ("flow", "track")
        @test is_gating_pop_type("flow") && is_gating_pop_type("track")
        @test !is_gating_pop_type("clust") && !is_gating_pop_type("trackclust") && !is_gating_pop_type("live")
    end

    # generic value_name presence check on an image (drives copy-to-images target filtering).
    # `_active` is a bookkeeping key, not a value_name → excluded by versioned_keys.
    @testset "img_has_value_name" begin
        proj = create_project!(name="vn-test-$(rand(1000:9999))", kind="static")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="i")
        img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "_active" => "A")
        @test Set(img_value_names(img)) == Set(["A", "B"])
        @test img_has_value_name(img, "A") && img_has_value_name(img, "B")
        @test !img_has_value_name(img, "C") && !img_has_value_name(img, "_active")
        rm(proj.root; recursive=true)
    end

    @testset "clust / trackclust pop types" begin
        td = mktempdir()
        # each clustering pop_type routes to its OWN gating sidecar (no collision with flow's {vn}.json)
        @test endswith(gating_path(td, "B"; pop_type="flow"),       joinpath("gating", "B.json"))
        @test endswith(gating_path(td, "B"; pop_type="clust"),      joinpath("gating", "B__clust.json"))
        @test endswith(gating_path(td, "B"; pop_type="trackclust"), joinpath("gating", "B__trackclust.json"))
        @test endswith(gating_path(td, "B"; pop_type="track"),      joinpath("gating", "B__tracks.json"))

        # cluster pop membership = filter "in" over the cluster code column
        m = PopulationMap(pop_type="clust", value_name="B")
        add_pop!(m, "myeloid"; filter_measure="clusters.default", filter_fun="in",
                 filter_values=[1, 3], colour="#10b981")
        fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                               "clusters.default" => [0, 1, 2, 3, 1])
        recompute!(m, fetch)
        @test Set(cells_in_pop(m, "/myeloid")) == Set([11, 13, 14])   # codes ∈ {1,3}

        # save/load round-trip → own file, filter fields preserved, flow file untouched
        save_pop_map!(m, td)
        @test isfile(gating_path(td, "B"; pop_type="clust"))
        @test !isfile(gating_path(td, "B"; pop_type="flow"))
        m2 = load_pop_map(td, "B"; pop_type="clust")
        @test pop_at(m2, "/myeloid").filter_measure == "clusters.default"
        @test pop_at(m2, "/myeloid").filter_fun == "in"
        @test Set(pop_at(m2, "/myeloid").filter_values) == Set([1, 3])
    end

    @testset "region pop type (spatial regions)" begin
        # region reuses the cluster-pop machinery with its OWN `regions.{suffix}` column prefix
        # (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5) — no duplicated logic, one generalisation.
        td = mktempdir()
        @test endswith(gating_path(td, "B"; pop_type="region"), joinpath("gating", "B__region.json"))
        @test Cecelia._is_cluster_pop_type("region")
        @test Cecelia._cluster_measure_prefix("region") == "regions."
        @test Cecelia._cluster_measure_prefix("clust") == "clusters."
        @test is_track_pop("region", "/tumour_zone") == false          # regions are per-cell, not per-track
        @test !is_gating_pop_type("region")                            # filter/membership pop, not a gate

        # region membership = filter "in" over the region code column (same engine path as clust)
        m = PopulationMap(pop_type="region", value_name="B")
        add_pop!(m, "tumour_zone"; filter_measure="regions.niches", filter_fun="in",
                 filter_values=[1, 3], colour="#10b981")
        fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                               "regions.niches" => [0, 1, 2, 3, 1])
        recompute!(m, fetch)
        @test Set(cells_in_pop(m, "/tumour_zone")) == Set([11, 13, 14])   # region codes ∈ {1,3}

        # referenced-suffixes generalisation reads the region prefix from the map's own pop_type
        @test Cecelia._referenced_cluster_suffixes(m) == Set(["niches"])

        # save/load round-trip → own __region file, flow file untouched
        save_pop_map!(m, td)
        @test isfile(gating_path(td, "B"; pop_type="region"))
        @test !isfile(gating_path(td, "B"; pop_type="flow"))
        m2 = load_pop_map(td, "B"; pop_type="region")
        @test pop_at(m2, "/tumour_zone").filter_measure == "regions.niches"
        @test Set(pop_at(m2, "/tumour_zone").filter_values) == Set([1, 3])

        # categorical name-rule: `regions`/`regions.{suffix}` are always a code set, even past the level cap
        @test Cecelia._is_categorical_col(collect(0:50), "regions.niches")   # 51 int levels, name-rule wins
        @test Cecelia._is_categorical_col(collect(0:50), "regions")
        @test Cecelia._is_categorical_col([0.0, 1.5, 2.7], "regions.niches") # decimals irrelevant under name-rule

        # per-region heatmap matrix detection routes through the shared suffix extractor (regions. prefix)
        @test Cecelia._cluster_matrix_suffix("matrix", "regions.niches") == "niches"
        @test Cecelia._cluster_matrix_suffix("matrix", "clusters.default") == "default"
    end

    @testset "contact_matrix — CODEX log-odds heatmap matrix" begin
        # sidecar spatialStats/{suffix}.json → symmetric pop×pop log-odds matrix for the plot renderer
        td = mktempdir(); mkpath(joinpath(td, "spatialStats"))
        open(joinpath(td, "spatialStats", "default.json"), "w") do f
            write(f, """{"basis":["B/qc","T/qc"],"nCells":100,"nEdges":200,"records":[""" *
                     """{"popA":"B/qc","popB":"B/qc","observed":10,"expected":5,"logOdds":0.7,"association":"associated"},""" *
                     """{"popA":"B/qc","popB":"T/qc","observed":1,"expected":5,"logOdds":-1.1,"association":"avoided"},""" *
                     """{"popA":"T/qc","popB":"T/qc","observed":8,"expected":4,"logOdds":0.6,"association":"associated"}]}""")
        end
        m = contact_matrix(CciaImage(; dir=td))
        @test m.suffixes == ["default"] && m.suffix == "default"
        @test Set(m.basis) == Set(["B/qc", "T/qc"]) && m.nCells == 100 && m.nEdges == 200
        val(x, y) = only(c.value for c in m.cells if c.x == x && c.y == y)
        @test val("B/qc", "T/qc") ≈ -1.1 && val("T/qc", "B/qc") ≈ -1.1   # symmetric fill
        @test val("B/qc", "B/qc") ≈ 0.7 && val("T/qc", "T/qc") ≈ 0.6
        @test length(m.cells) == 4                                       # 2×2 fully filled
        # no sidecar → empty (route returns empty, UI shows "run contact stats first")
        m0 = contact_matrix(CciaImage(; dir=mktempdir()))
        @test isempty(m0.cells) && isempty(m0.suffixes)
    end

    @testset "region pop auto-share (co-clustered value_names, cell granularity)" begin
        # regions are a per-run column shared across co-clustered segmentations — the identical
        # auto-share/expand machinery as clust, exercised via the `regions.` prefix + cell granularity.
        td = mktempdir()
        lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
        # A & B were region-clustered together (both CELL sidecars carry suffix "niches"); C was not.
        for vn in ("A", "B")
            open(joinpath(lpdir, "$(vn).clustfeatures.json"), "w") do f
                JSON3.write(f, Dict("niches" => Dict("features" => ["flow.region.cd8"], "partOf" => ["u1"])))
            end
        end
        am = PopulationMap(pop_type="region", value_name="A")
        add_pop!(am, "TumourZone"; filter_measure="regions.niches", filter_fun="in", filter_values=[2], colour="#c061cb")
        save_pop_map!(am, td)

        img = CciaImage(; dir=td)
        img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "C" => "C.h5ad", "_active" => "A")

        @test Set(Cecelia.co_clustered_value_names(img, "niches"; granularity=:cell)) == Set(["A", "B"])

        # B has no sidecar but IS co-clustered → borrows A's region pops, relabeled to B
        mb = load_pop_map(img; value_name="B", pop_type="region")
        @test Set(keys(mb.pops)) == Set(["/TumourZone"]) && mb.value_name == "B"
        @test all(p.value_name == "B" for p in values(mb.pops))
        # C was not in the run → no borrow
        @test isempty(load_pop_map(img; value_name="C", pop_type="region").pops)

        # bare region-pop ref expands across all co-clustered segmentations
        @test Set(Cecelia._expand_cluster_pops(img, ["/TumourZone"], "region", "A")) ==
              Set(["A/TumourZone", "B/TumourZone"])
    end

    @testset "recompute! — a missing filter/gate column degrades to empty (no crash)" begin
        # A cluster pop whose `clusters.{suffix}` column isn't in the fetched frame — e.g. evaluated
        # against a segmentation that didn't take part in that run, so `fetch_cols` silently dropped it
        # — must resolve to NO members, not raise `ArgumentError: column name … not found` and 500 the
        # whole plot. Regression: the trackclust heatmap crash (clusters.default not found).
        m = PopulationMap(pop_type="trackclust", value_name="C")
        add_pop!(m, "present"; filter_measure="clusters.movement", filter_fun="in", filter_values=[1, 2], colour="#10b981")
        add_pop!(m, "absent";  filter_measure="clusters.default",  filter_fun="in", filter_values=[0, 1], colour="#ef4444")
        # frame HAS clusters.movement but NOT clusters.default
        fetch = _ -> DataFrame("label" => [10, 11, 12, 13], "clusters.movement" => [0, 1, 2, 1])
        @test_logs (:warn, r"clusters\.default") match_mode=:any recompute!(m, fetch)  # warns, doesn't throw
        @test Set(cells_in_pop(m, "/present")) == Set([11, 12, 13])   # present column resolves normally
        @test isempty(cells_in_pop(m, "/absent"))                     # missing column → empty membership

        # same guard for a GATE whose axis column is absent from the frame
        mg = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(mg, "g"; gate=RectangleGate("x", "missingY", 0.0, 10.0, 0.0, 10.0), colour="#abcdef")
        fetchg = _ -> DataFrame("label" => [1, 2], "x" => [1.0, 2.0])   # no "missingY"
        @test_logs (:warn, r"missingY") match_mode=:any recompute!(mg, fetchg)
        @test isempty(cells_in_pop(mg, "/g"))
    end

    @testset "colour_by_palette — pop colour else default" begin
        # a value a user pop FILTERS for on the column → that pop's colour; the rest → OKABE_ITO by
        # sorted position. Generalises "use the population's colour where one exists" (a cluster pop is
        # just a filter on clusters.{suffix}).
        m = PopulationMap(pop_type="clust", value_name="B")
        add_pop!(m, "directed";   filter_measure="clusters.mov", filter_fun="in", filter_values=[2],       colour="#ff1493")
        add_pop!(m, "crawling";   filter_measure="clusters.mov", filter_fun="in", filter_values=[0, 3, 4], colour="#ffd700")
        add_pop!(m, "unrelated";  filter_measure="clusters.other", filter_fun="in", filter_values=[1],     colour="#000001")

        pal = colour_by_palette(m, "clusters.mov", [0, 1, 2, 3, 4])
        @test pal[2] == "#ff1493"                 # user pop colour
        @test pal[0] == "#ffd700" && pal[3] == "#ffd700" && pal[4] == "#ffd700"
        @test pal[1] == OKABE_ITO[1]              # uncovered value 1 → first default
        # a pop filtering a DIFFERENT column never leaks its colour in
        @test pal[1] != "#000001"

        # numeric tolerance: a filter value stored as 2.0 still matches integer column value 2
        m3 = PopulationMap(pop_type="clust", value_name="B")
        add_pop!(m3, "d"; filter_measure="clusters.mov", filter_fun="in", filter_values=[2.0], colour="#abcdef")
        @test colour_by_palette(m3, "clusters.mov", [2])[2] == "#abcdef"

        # no matching pop → all default, by sorted position (stable)
        empty = PopulationMap(pop_type="clust", value_name="B")
        p2 = colour_by_palette(empty, "clusters.mov", [5, 3, 3, 1])
        @test p2[1] == OKABE_ITO[1] && p2[3] == OKABE_ITO[2] && p2[5] == OKABE_ITO[3]

        # pop_colour_overrides: string-keyed {value => hex} for the wire (2.0/2 → "2"); only pops on
        # the column contribute; no default fill (the bridge does that).
        ov = pop_colour_overrides(m, "clusters.mov")
        @test ov == Dict("2" => "#ff1493", "0" => "#ffd700", "3" => "#ffd700", "4" => "#ffd700")
        @test pop_colour_overrides(m3, "clusters.mov") == Dict("2" => "#abcdef")   # 2.0 → "2"
        @test isempty(pop_colour_overrides(m, "clusters.absent"))

        # pop_label_overrides: same keying, value → the POP NAME (so the legend reads "directed", not "2")
        lbl = pop_label_overrides(m, "clusters.mov")
        @test lbl == Dict("2" => "directed", "0" => "crawling", "3" => "crawling", "4" => "crawling")
        @test isempty(pop_label_overrides(m, "clusters.absent"))
    end

    # ── Summary-canvas population picker (plot_pop_types / plot_population_groups) ──
    # The logic the /api/plots/populations route delegates to — pure, so tested here (the route is a
    # thin wrapper). Covers granularity→pop_type selection, cross-image + cross-pop_type union/dedup,
    # derived-pop injection, and pop_type tagging (the track-pops-in-the-picker fix, docs/TODO #00043).
    @testset "plot population picker" begin
        # pop_type selection by granularity
        @test plot_pop_types("live", "cell") == ["live"]
        @test plot_pop_types("live", "")     == ["live"]
        @test plot_pop_types("live", "track") == ["live", "track"]
        @test plot_pop_types("track", "track") == ["track"]          # no duplicate

        # flatten_pop_tree: pre-order paths + colours
        fm = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
        add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
        flat = flatten_pop_tree(to_tree(fm))
        @test [p for (p, _, _) in flat] == ["/qc", "/qc/sub"]
        @test flat[1][3] == "#ef4444"

        # a track-gated map (pop_type "track") with one pop
        tm = PopulationMap(pop_type="track", value_name="C")
        add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")

        # loaders injected (as the API passes versioned_keys/load_pop_map closures)
        names_for = _ -> ["C"]
        load = (_, vn, pt) -> vn == "C" ? (pt == "track" ? tm : (pt == "live" ? fm : nothing)) : nothing

        # CELL granularity → live pops only, with derived `_tracked` at root AND under each stored pop
        # (so /qc/_tracked is a selectable, indented child); a derived child directly follows its parent.
        cell = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell"))
        @test length(cell) == 1 && cell[1].value_name == "C"
        cpops = cell[1].populations
        @test [p.path for p in cpops] ==
              ["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked"]
        @test all(p.pop_type == "live" for p in cpops)
        @test !any(p.path == "/TEST" for p in cpops)
        # the nested derived pop is named by its leaf (indents under its parent in the UI)
        @test only(p for p in cpops if p.path == "/qc/_tracked").name == "_tracked"
        # a derived child inherits its parent pop's colour (so /qc/_tracked pairs with /qc visually —
        # the derived colour is read-only on the behaviour page, the parent's is editable on gating)
        @test only(p for p in cpops if p.path == "/qc/_tracked").colour == "#ef4444"      # = /qc
        @test only(p for p in cpops if p.path == "/qc/sub/_tracked").colour == "#abc"      # = /qc/sub
        @test only(p for p in cpops if p.path == "/_tracked").colour == "#7c93b8"          # root: no parent → grey

        # TRACK granularity → unions live (incl. nested /qc/_tracked) AND track (/TEST), each tagged
        trk = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "track"))
        tp = trk[1].populations
        @test Set(p.path for p in tp) ==
              Set(["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked", "/TEST"])
        test_pop = only(p for p in tp if p.path == "/TEST")
        @test test_pop.pop_type == "track" && test_pop.colour == "#f59e0b"
        @test only(p for p in tp if p.path == "/qc").pop_type == "live"
        @test !any(p.path == "/TEST/_tracked" for p in tp)          # no track-derived pop registered

        # root_derived_ok predicate: hide the root-level /_tracked (the API passes false when tracking
        # was gated → root is a redundant duplicate of /qc/_tracked) while KEEPING the per-pop derived
        # children. Default (no predicate) still offers root /_tracked — asserted by `cell` above.
        gated = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell");
                                       root_derived_ok = (_v, _pt, dpath) -> dpath != "/_tracked")
        gpaths = [p.path for p in gated[1].populations]
        @test !("/_tracked" in gpaths)                              # root hidden
        @test "/qc/_tracked" in gpaths && "/qc/sub/_tracked" in gpaths   # per-gate derived kept

        # cross-image UNION + dedup: two images both expose "C" → each (pop_type, path) appears once
        dedup = plot_population_groups([:img1, :img2], names_for, load, ["live"])
        @test length(dedup) == 1
        @test length(dedup[1].populations) == length(cpops)         # no duplicates across images

        # LABELS (gateless): no gating map — one selectable pop per segmentation value_name, named by
        # the value_name, tagged pop_type "labels" (segmentation QC: B/T plot side by side).
        names2 = _ -> ["B", "T"]
        lab = plot_population_groups([:img1], names2, (args...) -> error("must not load a map for labels"),
                                     ["labels"])
        @test [g.value_name for g in lab] == ["B", "T"]
        @test all(g -> length(g.populations) == 1, lab)
        bp = only(lab[1].populations)
        @test bp.path == "/labels" && bp.name == "B" && bp.pop_type == "labels"
        @test only(lab[2].populations).name == "T"
    end

    @testset "popScope population picker" begin
        # is_track_pop: the sole cell-vs-track test (Julia parity of the R `isTrack` attribute)
        @test is_track_pop("live", "/qc") == false                  # plain cell gate
        @test is_track_pop("flow", "/qc/sub") == false
        @test is_track_pop("clust", "/myeloid") == false            # cell cluster
        @test is_track_pop("live", "/_tracked") == true             # derived tracked set (root)
        @test is_track_pop("live", "/qc/_tracked") == true          # derived tracked subset of a gate
        @test is_track_pop("track", "/TEST") == true                # per-track gate
        @test is_track_pop("trackclust", "/clusterA") == true       # track cluster

        # scope_pop_types: sources loaded per scope; clusters toggleable; unknown scope throws.
        # `cells` also loads `region` (spatial regions) alongside `clust` — both cluster-family.
        @test scope_pop_types("cells", true)  == ["live", "clust", "region"]
        @test scope_pop_types("cells", false) == ["live"]
        @test scope_pop_types("tracks", true)  == ["live", "track", "trackclust"]
        @test scope_pop_types("tracks", false) == ["live", "track"]
        @test_throws ErrorException scope_pop_types("bogus", true)

        # maps: flow gates (/qc, /qc/sub), a per-track gate (/TEST), a cell cluster (/myeloid),
        # a track cluster (/clusterA)
        fm = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
        add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
        tm = PopulationMap(pop_type="track", value_name="C")
        add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")
        cm = PopulationMap(pop_type="clust", value_name="C")
        add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
        tcm = PopulationMap(pop_type="trackclust", value_name="C")
        add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
        names_for = _ -> ["C"]
        load = (_, vn, pt) -> vn != "C" ? nothing :
            pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm : pt == "trackclust" ? tcm : nothing

        # CELLS scope: all-cells root ("/") + plain gates + cell clusters; NO derived _tracked sets
        cells = population_scope_groups([:img1], names_for, load, "cells")
        @test length(cells) == 1 && cells[1].value_name == "C"
        cpaths = [p.path for p in cells[1].populations]
        @test cpaths == ["/", "/qc", "/qc/sub", "/myeloid"]
        @test cells[1].populations[1].name == "all"                 # backend all-cells root
        @test !any(occursin("_tracked", p) for p in cpaths)         # cells never show tracked sets
        @test all(!is_track_pop(p.pop_type, p.path) for p in cells[1].populations if p.path != "/")

        # CELLS, clusters excluded → drops /myeloid
        cells_nc = population_scope_groups([:img1], names_for, load, "cells"; include_clusters=false)
        @test [p.path for p in cells_nc[1].populations] == ["/", "/qc", "/qc/sub"]

        # TRACKS scope: derived _tracked sets (root + per-gate) + per-track gate + track cluster;
        # NO plain cell gates (/qc, /qc/sub) and NO all-cells root ("/")
        trk = population_scope_groups([:img1], names_for, load, "tracks")
        tpaths = Set(p.path for p in trk[1].populations)
        @test tpaths == Set(["/_tracked", "/qc/_tracked", "/qc/sub/_tracked", "/TEST", "/clusterA"])
        @test !("/qc" in tpaths) && !("/qc/sub" in tpaths) && !("/" in tpaths)
        @test all(is_track_pop(p.pop_type, p.path) for p in trk[1].populations)
        # a derived tracked child keeps its parent gate's colour (visual pairing, read-only)
        @test only(p for p in trk[1].populations if p.path == "/qc/_tracked").colour == "#ef4444"

        # TRACKS, gated tracking → root /_tracked hidden (redundant with /qc/_tracked); children kept
        trk_g = population_scope_groups([:img1], names_for, load, "tracks";
                                        root_derived_ok=(_v, _pt, d) -> d != "/_tracked")
        gpaths = Set(p.path for p in trk_g[1].populations)
        @test !("/_tracked" in gpaths) && "/qc/_tracked" in gpaths

        # TRACKS, clusters excluded → drops /clusterA, keeps the per-track gate /TEST
        trk_nc = population_scope_groups([:img1], names_for, load, "tracks"; include_clusters=false)
        tncpaths = Set(p.path for p in trk_nc[1].populations)
        @test !("/clusterA" in tncpaths) && "/TEST" in tncpaths
    end

    # ── pop_category + population_accept_groups (Decision 14, accepts allow-list) ────────────────
    @testset "population accepts allow-list + category tags" begin
        # pop_category: gated / clustered / region / tracked / aggregated from (pop_type, leaf).
        @test pop_category("live", "/qc")               == "gated"
        @test pop_category("track", "/TEST")             == "gated"
        @test pop_category("clust", "/myeloid")          == "clustered"
        @test pop_category("trackclust", "/clusterA")    == "clustered"
        @test pop_category("region", "/r0")              == "region"
        @test pop_category("live", "/qc/_tracked")       == "tracked"
        @test pop_category("live", "/qc/" * Cecelia.AGGREGATED_POP_NAME) == "aggregated"

        # same fixtures as the popScope testset above, plus a region map and an aggregated cell pop.
        fm = PopulationMap(pop_type="flow", value_name="C")
        add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
        add_pop!(fm, Cecelia.AGGREGATED_POP_NAME; parent="/qc", filter_measure="live.cell.is.aggregate",
                 filter_fun="gt", filter_values=0, reserved_ok=true)   # auto-created aggregate pop
        tm = PopulationMap(pop_type="track", value_name="C")
        add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5)
        cm = PopulationMap(pop_type="clust", value_name="C")
        add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
        tcm = PopulationMap(pop_type="trackclust", value_name="C")
        add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
        rm_ = PopulationMap(pop_type="region", value_name="C")
        add_pop!(rm_, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0])
        names_for = _ -> ["C"]
        load = (_, vn, pt) -> vn != "C" ? nothing :
            pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm :
            pt == "trackclust" ? tcm : pt == "region" ? rm_ : nothing

        # accepts=["live"] → all-cells root + cell gate + the aggregated cell pop; NO tracked sets,
        # NO clusters/regions. Each population carries granularity/category tags.
        g = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
        @test [p.path for p in g] == ["/", "/qc", "/qc/" * Cecelia.AGGREGATED_POP_NAME]
        @test all(p.granularity == "cell" for p in g)
        @test only(p for p in g if p.path == "/qc").category == "gated"
        @test only(p for p in g if endswith(p.path, Cecelia.AGGREGATED_POP_NAME)).category == "aggregated"

        # "flow" is an alias for "live".
        @test [p.path for p in population_accept_groups([:img1], names_for, load, ["flow"])[1].populations] ==
              [p.path for p in g]

        # region basis: cells (gated+clustered+region) AND tracks (gated+clustered). One picker, both
        # granularities — the case popScope could not express.
        basis = population_accept_groups([:img1], names_for, load,
                    ["live", "clust", "region", "track", "trackclust"])[1].populations
        bcats = Set((p.granularity, p.category) for p in basis)
        @test ("cell", "gated") in bcats && ("cell", "clustered") in bcats && ("cell", "region") in bcats
        @test ("track", "tracked") in bcats && ("track", "gated") in bcats && ("track", "clustered") in bcats
        @test "/r0" in [p.path for p in basis] && "/myeloid" in [p.path for p in basis]
        @test "/clusterA" in [p.path for p in basis] && "/TEST" in [p.path for p in basis]

        # accepts=["clust"] alone → only cell clusters, no all-cells root (live not accepted).
        cl = population_accept_groups([:img1], names_for, load, ["clust"])[1].populations
        @test [p.path for p in cl] == ["/myeloid"]

        # popScope shim must still produce identical paths to the direct accept call.
        @test [p.path for p in population_scope_groups([:img1], names_for, load, "cells")[1].populations] ==
              [p.path for p in population_accept_groups([:img1], names_for, load,
                                    ["live", "clust", "region"])[1].populations]

        # unknown token / empty list throw loudly.
        @test_throws ErrorException population_accept_groups([:img1], names_for, load, ["bogus"])
        @test_throws ErrorException population_accept_groups([:img1], names_for, load, String[])
    end

    # ── ensure_filter_pop! — a cutoff materialised as a reusable filter pop (Decision 14) ────────
    @testset "ensure_filter_pop! auto-created population" begin
        td = mktempdir()
        img = CciaImage(; dir=td)
        m = PopulationMap(; pop_type="flow", value_name="B")
        add_pop!(m, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(m, img)

        # a 0/1 flag column → aggregated pop under /qc (the generalisable `> 0`, not a baked TRUE/FALSE)
        created = ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                     filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
        @test created == ["/qc/" * AGGREGATED_POP_NAME]
        p = pop_at(load_pop_map(img; value_name="B", pop_type="flow"), "/qc/" * AGGREGATED_POP_NAME)
        @test p.filter_measure == "flow.cell.is.aggregate" && p.filter_fun == "gt" && p.filter_values == 0
        @test pop_category(p.pop_type, p.path) == "aggregated" && !is_track_pop(p.pop_type, p.path)

        # idempotent: re-running REDEFINES (a probability cutoff — measure-agnostic), never duplicates
        ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                     filter_measure="flow.cell.aggregate.score", filter_fun="gte", filter_values=0.5)
        m3 = load_pop_map(img; value_name="B", pop_type="flow")
        @test count(pp -> endswith(pp, AGGREGATED_POP_NAME), pop_paths(m3)) == 1
        @test pop_at(m3, "/qc/" * AGGREGATED_POP_NAME).filter_fun == "gte"

        # a parent absent from the map is skipped; the all-cells root ("/") maps to ROOT and is created
        created2 = ensure_filter_pop!(img, "flow", "B", ["/nonexistent", "/"], AGGREGATED_POP_NAME;
                     filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
        @test created2 == ["/" * AGGREGATED_POP_NAME]
        rm(td; recursive=true)
    end

    # ── Cluster-pop auto-share across co-clustered segmentations (CLUSTER_POOLING_PLAN.md) ─────
    @testset "cluster pop auto-share (co-clustered value_names)" begin
        td = mktempdir()
        lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
        # B & T were clustered together (both track sidecars carry suffix "movement"); C was not.
        for vn in ("B", "T")
            open(joinpath(lpdir, "$(vn)__tracks.clustfeatures.json"), "w") do f
                JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["u1"])))
            end
        end
        # named trackclust pops authored ONLY under B (filter the shared clusters.movement column)
        bm = PopulationMap(pop_type="trackclust", value_name="B")
        add_pop!(bm, "Directed"; filter_measure="clusters.movement", filter_fun="in", filter_values=[3], colour="#c061cb")
        add_pop!(bm, "Scanning"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0], colour="#62a0ea")
        save_pop_map!(bm, td)

        img = CciaImage(; dir=td)
        img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "C" => "C.h5ad", "_active" => "B")

        # co-clustered segmentations for run "movement" (track granularity) = B and T (not C)
        @test Set(Cecelia.co_clustered_value_names(img, "movement"; granularity=:track)) == Set(["B", "T"])

        # B has its OWN sidecar → loaded verbatim
        mb = load_pop_map(img; value_name="B", pop_type="trackclust")
        @test Set(keys(mb.pops)) == Set(["/Directed", "/Scanning"]) && mb.value_name == "B"

        # T has NO sidecar but IS co-clustered → BORROWS B's named pops, relabeled to T so
        # membership resolves over T's own track table
        mt = load_pop_map(img; value_name="T", pop_type="trackclust")
        @test Set(keys(mt.pops)) == Set(["/Directed", "/Scanning"])
        @test mt.value_name == "T" && all(p.value_name == "T" for p in values(mt.pops))
        @test mt.pops["/Directed"].filter_measure == "clusters.movement"

        # C was NOT part of the run (no clustfeatures suffix) → no borrow (empty map)
        mc = load_pop_map(img; value_name="C", pop_type="trackclust")
        @test isempty(mc.pops)

        # BARE cluster-pop ref expands across ALL co-clustered segmentations (R popDT parity)
        @test Set(Cecelia._expand_cluster_pops(img, ["/Directed"], "trackclust", "B")) ==
              Set(["B/Directed", "T/Directed"])
        # explicit value_name-prefixed ref is untouched (single-segmentation request still works)
        @test Cecelia._expand_cluster_pops(img, ["T/Scanning"], "trackclust", "B") == ["T/Scanning"]
        # unknown pop → left as-is (falls back to default_vn downstream); non-cluster type → no-op
        @test Cecelia._expand_cluster_pops(img, ["/Nope"], "trackclust", "B") == ["/Nope"]
        @test Cecelia._expand_cluster_pops(img, ["/x"], "flow", "B") == ["/x"]

        # per-cluster heatmap detection: a matrix over a clusters.{suffix} column pools co-clustered vns
        @test Cecelia._cluster_matrix_suffix("matrix", "clusters.movement") == "movement"
        @test Cecelia._cluster_matrix_suffix("matrix", "pop") === nothing      # per-population mode
        @test Cecelia._cluster_matrix_suffix("boxplot", "clusters.movement") === nothing
    end

    # ── Gating engine: recompute, membership, filtered (tracked) pops ─────────
    @testset "recompute! + cells_in_pop" begin
        df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9], track_id=[0, 5, 9, 0, 7])

        # flow: parent (x≥0) ∩ child (x≥5)
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", 0.0, 1e9, -1e9, 1e9))
        add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        recompute!(m, _ -> df)
        @test cells_in_pop(m, "/p") == [1, 2, 3, 4, 5]
        @test cells_in_pop(m, "/p/c") == [2, 3, 4, 5]         # x≥5
        @test pop_stats(m, "/p/c").pct_parent == 80.0

        # filtered (tracked) pop: track_id > 0
        mt = PopulationMap(pop_type="live", value_name="T")
        add_pop!(mt, "tracked"; filter_measure="track_id", filter_fun="gt", filter_values=0)
        recompute!(mt, _ -> df)
        @test cells_in_pop(mt, "/tracked") == [2, 3, 5]

        @test_throws ErrorException cells_in_pop(PopulationMap(), "/x")  # not recomputed
    end

    # ── explicit-label membership (napari selection) + transient not persisted ─
    @testset "explicit-label (napari) membership" begin
        df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9])

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # x≥5 → 2,3,4,5
        # transient napari selection of labels {2,4,9} ∩ parent(x≥5) → {2,4}
        add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 4, 9],
                 colour="#22d3ee", transient=true)
        recompute!(m, _ -> df)
        @test cells_in_pop(m, "/p/napari") == [2, 4]              # 9 absent, 3/5 not selected

        # root-level selection (no gate parent): exactly the labels present
        m2 = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m2, "sel"; explicit_labels=[1, 3, 99], transient=true)
        recompute!(m2, _ -> df)
        @test cells_in_pop(m2, "/sel") == [1, 3]

        # transient pops are NOT written to disk, but stay in the in-memory/broadcast tree
        td = mktempdir()
        save_pop_map!(m, td)
        reloaded = load_pop_map(td, "B")
        @test !has_pop(reloaded, "/p/napari")                    # dropped on persist
        @test has_pop(reloaded, "/p")                            # real pop kept
        @test "transient" in keys(Cecelia._node_dict(m, "/p/napari"))  # flagged in broadcast tree

        # explicit-label pops carry a membership signature in the broadcast tree (no gate/filter
        # to diff on) so the client refreshes plots when the selection's cell set changes.
        nd1 = Cecelia._node_dict(m, "/p/napari")
        @test haskey(nd1, "membership_sig")
        del_pop!(m, "/p/napari")
        add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 9], colour="#22d3ee", transient=true)
        @test Cecelia._node_dict(m, "/p/napari")["membership_sig"] != nd1["membership_sig"]
    end

    # ── pop_df: pooling across value_names + dedup to most-specific pop ────────
    @testset "pop_df pooling + dedup" begin
        dfA = DataFrame(label=[1, 2, 3], x=[1.0, 6.0, 9.0])
        dfB = DataFrame(label=[10, 11], x=[7.0, 2.0])
        mA = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(mA, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
        add_pop!(mA, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        mB = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(mB, "hi"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        maps = Dict("A" => mA, "B" => mB)
        load_map = vn -> maps[vn]
        fetch = (vn, _) -> (vn == "A" ? dfA : dfB)

        # request /p and /p/c from A, and hi from B (prefixed)
        res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c", "B/hi"];
                              default_vn="A", pop_cols=["x"], unique_labels=true)
        @test Set(unique(res.value_name)) == Set(["A", "B"])      # pooled across value_names
        # dedup: label 2 & 3 are in both /p and /p/c → assigned the most specific (/p/c)
        getpop(l, vn) = only(res[(res.label .== l) .& (res.value_name .== vn), :pop])
        @test getpop(1, "A") == "/p"
        @test getpop(2, "A") == "/p/c"
        @test getpop(3, "A") == "/p/c"
        @test getpop(10, "B") == "/hi"                            # x=7 ≥ 5
        @test nrow(res[res.value_name .== "B", :]) == 1           # label 11 (x=2) excluded
    end

    # ── pop_df: drop_na drops NA/NaN cells in requested pop_cols ──────────────
    @testset "pop_df drop_na" begin
        # NaN is in a measure column (x), NOT the gate column (g) — a NaN gate value would
        # fail the gate comparison and never enter the result, masking what drop_na does.
        df = DataFrame(label=[1, 2, 3], g=[0.0, 0.0, 0.0], x=[1.0, NaN, 9.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "p"; gate=RectangleGate("g", "g", -1e9, 1e9, -1e9, 1e9))  # all pass
        load_map = _ -> m
        fetch = (_, _) -> df
        keep = Cecelia._pop_df(load_map, fetch, "flow", ["/p"]; default_vn="A", pop_cols=["x"])
        @test nrow(keep) == 3                                     # NaN row kept by default
        dropped = Cecelia._pop_df(load_map, fetch, "flow", ["/p"];
                                  default_vn="A", pop_cols=["x"], drop_na=true)
        @test sort(dropped.label) == [1, 3]                       # label 2 (x=NaN) dropped
    end

    # ── pop_df: track_id joins the dedup key when present (most-specific pop still wins) ──
    @testset "pop_df track_id dedup key" begin
        df = DataFrame(label=[1, 2], x=[9.0, 9.0], track_id=[10.0, 20.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
        add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        load_map = _ -> m
        fetch = (_, _) -> df
        res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c"];
                              default_vn="A", pop_cols=["x"], unique_labels=true)
        @test "track_id" in names(res)                            # track_id carried through
        @test nrow(res) == 2                                      # still one row per cell
        @test Set(res.pop) == Set(["/p/c"])                       # most-specific pop wins
    end

    # ── pop_df: derived live "_tracked" pop (track_id>0 filter on a gated parent) ──
    @testset "pop_df live _tracked (derived filter)" begin
        # label4 fails the qc gate (x=1); label2 is in qc but untracked (track_id=NaN).
        df = DataFrame(label=[1, 2, 3, 4], x=[9.0, 9.0, 9.0, 1.0], track_id=[10.0, NaN, 20.0, 30.0])
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # qc = {1,2,3}
        # "_tracked" is derived, not stored — injecting it adds a filtered child of /qc
        Cecelia._inject_derived_pops!(m, ["/qc/_tracked"], "live")
        @test has_pop(m, "/qc/_tracked")
        @test m.pops["/qc/_tracked"].filter_measure == "track_id"
        load_map = _ -> m
        fetch = (_, _) -> df
        res = Cecelia._pop_df(load_map, fetch, "live", ["/qc/_tracked"];
                              default_vn="A", pop_cols=["track_id"])
        @test sort(res.label) == [1, 3]                # qc ∩ track_id>0 (label2 NaN, label4 not in qc)
        @test unique(res.pop) == ["/qc/_tracked"]
        # a derived pop is only injected under its registered pop_type (foreign type → skip)
        m2 = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m2, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        Cecelia._inject_derived_pops!(m2, ["/qc/_tracked"], "clust")  # _tracked is a `live` spec
        @test !has_pop(m2, "/qc/_tracked")
        # an unknown `_`-name is not derived either
        Cecelia._inject_derived_pops!(m2, ["/qc/_nope"], "live")
        @test !has_pop(m2, "/qc/_nope")
    end

    # ── reserved derived-pop namespace: `_`-prefixed names can't be hand-drawn gates ──
    @testset "reserved pop names (_ prefix)" begin
        @test is_reserved_pop_name("_tracked")
        @test is_reserved_pop_name("_anything")
        @test !is_reserved_pop_name("qc")
        m = PopulationMap(pop_type="flow", value_name="A")
        add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
        # a hand-drawn gate may not take a reserved name
        @test_throws ErrorException add_pop!(m, "_tracked"; parent="/qc",
                                             gate=RectangleGate("x", "x", 0.0, 1.0, 0.0, 1.0))
        @test_throws ErrorException rename_pop!(m, "/qc", "_qc")
        # the derived injection (reserved_ok) is allowed to create it
        add_pop!(m, "_tracked"; parent="/qc", filter_measure="track_id", filter_fun="gt",
                 filter_values=0, transient=true, reserved_ok=true)
        @test has_pop(m, "/qc/_tracked")
        # round-trips through from_tree (reconstruction bypasses the guard)
        m3 = from_tree(to_tree(m; include_transient=true))
        @test has_pop(m3, "/qc/_tracked")
    end

    # ── pop_df: cache key folds in file mtimes → auto-invalidates on gate/h5ad change ──
    @testset "pop_df cache auto-invalidation" begin
        td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
        img = CciaImage(uid="X", dir=td)
        img.label_props["A"] = "A.h5ad"
        write(joinpath(td, "gating", "A.json"), "{}")
        write(joinpath(td, "labelProps", "A.h5ad"), "x")
        key() = Cecelia._pop_df_cache_key(img, "flow", "A", ["/qc"], nothing,
                                          false, true, true, false, false, :cell, String[], String[])
        k1 = key()
        sleep(0.05); touch(joinpath(td, "labelProps", "A.h5ad"))   # re-track rewrites h5ad
        k2 = key()
        @test k1 != k2
        sleep(0.05); touch(joinpath(td, "gating", "A.json"))        # gate edit rewrites map
        @test key() != k2
    end

    # ── pop_df: integration on real KDIeEm (gate eval over real H5AD) ─────────
    @testset "pop_df integration (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "pop_df integration (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
            thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)] # ~median → discriminating
            truth = sum(full.mean_intensity_0 .>= thr)
            @test 0 < truth < nrow(full)                          # genuinely partial selection

            m = PopulationMap(pop_type="flow", value_name="B")
            add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                  thr, 1e12, -1e12, 1e12))
            save_pop_map!(m, img)
            df = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area", "mean_intensity_0"])
            @test nrow(df) == truth
            @test Set(names(df)) ⊇ Set(["label", "area", "mean_intensity_0", "pop", "value_name"])
            @test all(df.mean_intensity_0 .>= thr)                # every returned cell passes the gate
            @test unique(df.pop) == ["/pos"]

            # channel-name resolution: pop_df renames intensity cols to channel names by default,
            # raw_channel_names=true keeps the {measure}_intensity_{i} names (channel names are
            # stored under the default version, so value_name="B" falls back to it)
            set_channel_names!(img, ["CD4", "CD8", "CD3", "CD19"]; check_length=false)
            named = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true)
            @test "CD4" in names(named) && !("mean_intensity_0" in names(named))
            raw = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true, raw_channel_names=true)
            @test "mean_intensity_0" in names(raw) && !("CD4" in names(raw))

            # value_name=nothing resolves to the active segmentation (img.label_props _active="B")
            auto = pop_df(img, "flow", ["/pos"]; pop_cols=["area"])
            @test nrow(auto) == truth

            # cache: a request is stored under its signature key; flush_cache recomputes
            ck = Cecelia._pop_df_cache_key(img, "flow", "B", ["/pos"], ["area"],
                                           false, true, true, false, false, :cell, String[], String[])
            cached = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
            @test haskey(img._pop_df_cache, ck)
            fresh = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"], flush_cache=true)
            @test nrow(cached) == truth && nrow(fresh) == truth
        end
    end

    # ── resolve_pops: cached, display-ready per-pop membership (napari points overlay) ──
    @testset "resolve_pops (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "resolve_pops (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
            thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
            want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

            m = PopulationMap(pop_type="flow", value_name="B")
            add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                                  thr, 1e12, -1e12, 1e12), colour="#ef4444")
            save_pop_map!(m, img)

            layers = resolve_pops(img, "flow"; value_name="B")
            @test length(layers) == 1
            L = layers[1]
            @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
            @test L.show === true && L.is_track === false
            @test sort(L.labels) == want                       # membership == the gate's cells

            # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
            again = resolve_pops(img, "flow"; value_name="B")
            @test again === layers
            @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
        end
    end

    # ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
    # count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
    # See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
    # ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
    @testset "labels pop_type + count (KDIeEm)" begin
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "labels pop_type (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
            all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
            @test nrow(all) > 0
            @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
            @test unique(all.pop) == ["/labels"]
            @test unique(all.value_name) == ["B"]

            # cell count via the summary aggregator over labels — one series, value == total.
            whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
            @test whole["chartType"] == "count"
            @test length(whole["series"]) == 1
            @test whole["series"][1]["value"] == Float64(nrow(all))

            # count per timepoint (group_by the temporal column) → counts partition the total.
            byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="t")
            @test byT["groupBy"] == "t"
            @test length(byT["series"]) > 1
            @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

            # a morphology distribution over labels, per timepoint
            area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                     measure="area", group_by="t")
            @test area["measure"] == "area"
            @test length(area["series"]) == length(byT["series"])

            # targets signature (the path the summary canvas + whiteboard QC row use: series =
            # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
            tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
            @test tg["chartType"] == "count"
            @test length(tg["series"]) == 1
            @test tg["series"][1]["value"] == Float64(nrow(all))
            @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
        end
    end

    # ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
    @testset "track table helpers" begin
        td = mktempdir()
        img = CciaImage(uid="X", dir=td)
        # companion track table sits next to the cell labelProps with the __tracks suffix
        @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
        @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
        @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
        # reserved value-name suffix (a user segmentation may not end in __tracks)
        @test is_reserved_value_name("A__tracks")
        @test is_reserved_value_name("foo__tracks")
        @test !is_reserved_value_name("A")
        @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
        # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
        @test Cecelia._jsonsafe(NaN) === nothing
        @test Cecelia._jsonsafe(1.5) === 1.5
        @test Cecelia._jsonsafe(3)   === 3
        # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
        mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
        img.label_props["B"] = "B.h5ad"
        kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                       false, true, true, false, false, :cell, String[], String[])
        kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                       false, true, true, false, false, :track, String[], String[])
        @test kc != kt
    end

    # ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
    @testset "pop_df :track (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "pop_df :track (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # track table layout: measures in X/var, lineage in obs, one row per track_id
            tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
            @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
            tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
            @test "track_root" in tobs

            # one row per track; carries measures + track_id + value_name
            tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
            @test nrow(tr) > 0
            @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
            @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
            @test unique(tr.value_name) == ["B"]

            # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
            ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
            ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
            @test nrow(tr) == ntracks_cells
            @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

            # pop_cols restriction returns just that measure (+ bookkeeping)
            sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                        pop_cols=["live.track.speed"])
            @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))

            # cell_measures aggregation (the clustTracks path): a per-cell measure is aggregated to
            # per-track feature column(s) via track_props, alongside motility — this is what lets
            # clustTracks cluster `_tracked` pops on HMM/intensity features, not just motility.
            cvars = col_names(label_props(img; value_name="B"); data_type=:vars)
            if !isempty(cvars)
                base = String(first(cvars))                        # a real per-cell measure
                ag = pop_df(img, "live", ["B/_tracked"]; granularity=:track, cell_measures=[base])
                @test any(startswith(c, base * ".") for c in names(ag))   # aggregated → {base}.…
                @test nrow(ag) == nrow(tr)                          # same tracks, extra feature cols
                @test "live.track.speed" in names(ag)               # motility still present
                @test "num_cells" in names(ag)                      # per-track cell count (minTracklength)
            end
        end
    end

    # ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
    @testset "plot_summary_data (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # histogram of per-track speed (granularity=:track): shared edges, every track binned
            h = plot_summary_data(img, "live", ["B/_tracked"], "histogram";
                                  measure="live.track.speed", granularity=:track, nbins=20)
            @test h["chartType"] == "histogram"
            @test length(h["binEdges"]) == 21
            @test length(h["series"]) == 1
            ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
            @test sum(h["series"][1]["counts"]) == ntr

            # mock a categorical per-cell column, then frequency over the tracked cells
            cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
            mock = DataFrame("label" => cells.label,
                             "mock.state" => [Float64((l % 3) + 1) for l in cells.label])
            label_props(img_label_props_path(img, "B")) |> add_obs(mock) |> save!
            f = plot_summary_data(img, "live", ["B/_tracked"], "frequency";
                                  measure="mock.state", granularity=:cell, normalize=:fraction)
            @test f["chartType"] == "frequency"
            @test Set(f["categories"]) ⊆ Set(["1", "2", "3"])
            @test length(f["series"]) == 1
            props = f["series"][1]["values"]
            @test all(0 .<= props .<= 1) && isapprox(sum(props), 1.0; atol=1e-9)

            # measureType auto-detection: continuous speed → numeric; integer code set → categorical
            @test h["measureType"] == "numeric"
            @test f["measureType"] == "categorical"

            # bar: mean + all three error metrics (sd, sem = sd/√n, ci95 ≈ 1.96·sem)
            br = plot_summary_data(img, "live", ["B/_tracked"], "bar";
                                   measure="live.track.speed", granularity=:track)
            s = br["series"][1]
            @test Set(keys(s)) ⊇ Set(["value", "sd", "sem", "ci95", "n"])
            @test s["sem"] ≈ s["sd"] / sqrt(s["n"])
            @test s["ci95"] ≈ 1.96 * s["sem"]

            # raw points: boxplot with raw_points carries downsampled values (≤ cap); "points" chart
            bx = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track,
                                   raw_points=true, max_points=10)
            @test length(bx["series"][1]["points"]) == min(ntr, 10)
            pts = plot_summary_data(img, "live", ["B/_tracked"], "points";
                                    measure="live.track.speed", granularity=:track, max_points=10)
            @test pts["chartType"] == "points" && length(pts["series"][1]["points"]) == min(ntr, 10)
            # without raw_points, boxplot carries no payload of values
            bx0 = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                    measure="live.track.speed", granularity=:track)
            @test isempty(bx0["series"][1]["points"])
        end
    end

    @testset "motion dimensionality detection (2D vs 3D)" begin
        # build a Track by cumulative-summing per-step [dz,dy,dx] vectors (coords are [z,y,x])
        mk(steps) = begin
            P = zeros(length(steps) + 1, 3)
            for k in 1:length(steps); P[k+1, :] = P[k, :] .+ steps[k]; end
            Cecelia.Track(1, Float64.(0:length(steps)), P)
        end
        dy(k) = 2 + 0.5 * cos(k / 3); dx(k) = 2 + 0.5 * sin(k / 3)   # persistent forward heading in xy
        # z either tracks the persistent xy motion (real 3D) or oscillates with large amplitude (jitter)
        real(rng)   = mk([[2 + 0.5*sin(k/3), dy(k), dx(k)] for k in rng])
        jitter(rng) = mk([[(-1.0)^k * 4.0,   dy(k), dx(k)] for k in rng])
        trks_real   = [real(t*20 : t*20+14)   for t in 1:8]
        trks_jitter = [jitter(t*20 : t*20+14) for t in 1:8]

        d3 = Cecelia._detect_motion_dims(trks_real)
        @test d3.dims == 3 && d3.z_used            # persistent z → keep 3D
        d2 = Cecelia._detect_motion_dims(trks_jitter)
        @test d2.dims == 2 && !d2.z_used           # oscillating/anti-persistent z → in-plane 2D
        @test d2.metrics["autocorrZ"] < 0          # jitter signature

        # a 2D-only track set (no z column) is trivially 2D
        P2 = zeros(12, 2); for k in 1:11; P2[k+1, :] = P2[k, :] .+ [dy(k), dx(k)]; end
        @test Cecelia._detect_motion_dims([Cecelia.Track(1, Float64.(0:11), P2)]).dims == 2
    end

    @testset "plot groupBy (generic categorical sub-axis)" begin
        # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
        # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
        df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                       "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                       "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
        r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=false, group_by="st")
        @test r["groupBy"] == "st"
        @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
        @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
        byg = Dict(s["group"] => s for s in r["series"])
        @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
        @test byg["2"]["n"] == 3
        # no group_by → single series, empty group label (back-compat)
        r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=false)
        @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

        # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
        # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
        dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                        "uID" => ["x","x","y","y","x","y"],
                        "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                        "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
        rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, group_by="st", collapse_series=true)
        @test length(rc["series"]) == 2
        @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
        @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
        @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
        # collapse with no group_by → one pooled series over everything
        rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, collapse_series=true)
        @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
    end

    @testset "plot count (raw + proportion normalize) — population summary" begin
        # two pops in two images; count → raw row counts; normalize=:fraction → each pop's share of
        # its image's plotted total (the population-summary plot). Deterministic frame — no fixture.
        df = DataFrame("value_name" => fill("A", 10),
                       "pop" => ["/p","/p","/p","/q","/q", "/p","/q","/q","/q","/p"],
                       "uID" => ["x","x","x","x","x",       "y","y","y","y","y"])
        # image x: /p=3, /q=2 (total 5); image y: /p=2, /q=3 (total 5)
        bykey(r) = Dict((s["uID"], s["pop"]) => s["value"] for s in r["series"])
        raw = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=true)
        @test raw["chartType"] == "count"
        rk = bykey(raw)
        @test rk[("x","A/p")] == 3.0 && rk[("x","A/q")] == 2.0
        @test rk[("y","A/p")] == 2.0 && rk[("y","A/q")] == 3.0
        prop = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                    normalize=:fraction, by_image=true)
        @test prop["normalize"] == "fraction"
        pk = bykey(prop)
        @test pk[("x","A/p")] ≈ 0.6 && pk[("x","A/q")] ≈ 0.4     # 3/5, 2/5
        @test pk[("y","A/p")] ≈ 0.4 && pk[("y","A/q")] ≈ 0.6     # 2/5, 3/5
        @test Dict((s["uID"], s["pop"]) => s["n"] for s in prop["series"])[("x","A/p")] == 3

        # no measure + a DISTRIBUTION chart → each IMAGE is a point (its pop count), grouped by pop:
        # boxplot/beeswarm show within-pop variability and compare pops. A/p counts = [3(x),2(y)],
        # A/q = [2(x),3(y)] → each pop has 2 points (images), median 2.5.
        bx = Cecelia._summary_agg(df, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true)
        @test bx["chartType"] == "boxplot"
        bybp = Dict(s["pop"] => s for s in bx["series"])
        @test Set(keys(bybp)) == Set(["A/p", "A/q"])
        @test bybp["A/p"]["n"] == 2 && bybp["A/p"]["median"] == 2.5
        # bar over the same per-image counts → mean (A/p mean of [3,2] = 2.5)
        br = Cecelia._summary_agg(df, "bar"; measure=nothing, granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true)
        @test Dict(s["pop"] => s["value"] for s in br["series"])["A/p"] == 2.5

        # SPLIT BY POPULATION: two tracked pops (value_names B, T) each with clusters — proportion is
        # normalised WITHIN each value_name per image, not pooled across B+T.
        df2 = DataFrame("value_name" => ["B","B","B","T","T", "B","B","T","T","T"],
                        "pop" => ["/Dir","/Dir","/Mea","/Dir","/Mea", "/Dir","/Mea","/Dir","/Mea","/Mea"],
                        "uID" => ["x","x","x","x","x",              "y","y","y","y","y"])
        pr2 = Cecelia._summary_agg(df2, "count"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:fraction, by_image=true)
        p2 = Dict((s["uID"], s["pop"]) => s["value"] for s in pr2["series"])
        @test p2[("x","B/Dir")] ≈ 2/3 && p2[("x","B/Mea")] ≈ 1/3   # within B (image x: B tot 3)
        @test p2[("x","T/Dir")] ≈ 1/2 && p2[("x","T/Mea")] ≈ 1/2   # within T (image x: T tot 2)
        @test p2[("y","B/Dir")] ≈ 1/2 && p2[("y","T/Mea")] ≈ 2/3   # within B / T (image y)

        # COMPLETE CASES (R tidyr::complete): image y has no /q — it must still contribute a 0 to /q's
        # distribution, not be dropped. Without completion /q would have n=1 (only x) and median 1.
        df3 = DataFrame("value_name" => fill("A", 5),
                        "pop" => ["/p","/p","/q", "/p","/p"],
                        "uID" => ["x","x","x",    "y","y"])          # x: p=2 q=1 ; y: p=2 q=0 (missing)
        bx3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true)
        by3 = Dict(s["pop"] => s for s in bx3["series"])
        @test by3["A/q"]["n"] == 2 && by3["A/q"]["median"] == 0.5    # /q points [1(x), 0(y)]
        @test by3["A/p"]["n"] == 2 && by3["A/p"]["median"] == 2.0
        # proportion completes too: /q in image y = 0 / (y's A total 2) = 0 → points [1/3, 0]
        pr3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                                   normalize=:fraction, by_image=true)
        by3f = Dict(s["pop"] => s for s in pr3["series"])
        @test by3f["A/q"]["n"] == 2 && by3f["A/q"]["median"] ≈ 1/6
    end

    @testset "plot raw (per-datapoint export)" begin
        # raw=true → one tidy row per datapoint (identity + value) for re-plotting externally, instead of
        # collapsing to box stats. Deterministic frame with label + a groupBy column; last row NaN measure.
        df = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                       "uID" => ["x","x","y","y","y"], "label" => [1, 2, 3, 4, 5],
                       "m"  => [1.0, 2.0, 3.0, 4.0, NaN],
                       "st" => [1.0, 1.0, 2.0, 2.0, 2.0])
        r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, group_by="st", raw=true)
        @test r["chartType"] == "raw" && r["measure"] == "m" && r["groupBy"] == "st"
        @test length(r["rows"]) == 4                       # the NaN-measure row is dropped
        row1 = r["rows"][1]
        @test row1["uID"] == "x" && row1["label"] == "1" && row1["value_name"] == "A"
        @test row1["pop"] == "/p" && row1["value"] == 1.0 && row1["group"] == "1"
        @test [rw["value"] for rw in r["rows"]] == [1.0, 2.0, 3.0, 4.0]
        @test [rw["group"] for rw in r["rows"]] == ["1", "1", "2", "2"]

        # measure-less count chart → raw collapses to per-(image, pop) counts (no label column populated)
        dfc = DataFrame("value_name" => fill("A", 5), "pop" => ["/p","/p","/p","/q","/q"],
                        "uID" => ["x","x","x","x","x"])
        rc = Cecelia._summary_agg(dfc, "count"; measure=nothing, granularity=:cell, nbins=0,
                                  normalize=:none, by_image=true, raw=true)
        @test rc["chartType"] == "raw" && rc["measure"] == "count"
        cbyp = Dict(rw["pop"] => rw["value"] for rw in rc["rows"])
        @test cbyp["/p"] == 3.0 && cbyp["/q"] == 2.0 && all(!haskey(rw, "label") for rw in rc["rows"])

        # TRACK granularity: `label` duplicates `track_id` in the track table → drop it, keep track_id.
        dft = DataFrame("value_name" => fill("A", 3), "pop" => fill("/_tracked", 3),
                        "uID" => ["x","x","y"], "label" => [10, 11, 12], "track_id" => [10, 11, 12],
                        "m" => [1.0, 2.0, 3.0])
        rt = Cecelia._summary_agg(dft, "boxplot"; measure="m", granularity=:track, nbins=10,
                                  normalize=:none, by_image=true, raw=true)
        @test all(!haskey(rw, "label") for rw in rt["rows"])            # no meaningless label
        @test [rw["track_id"] for rw in rt["rows"]] == ["10", "11", "12"]

        # groupBy that ISN'T applied (its column isn't in the frame) → groupBy null + no group column,
        # so the export never carries an empty, misleading category column.
        rna = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, group_by="not_a_column", raw=true)
        @test rna["groupBy"] === nothing && all(!haskey(rw, "group") for rw in rna["rows"])
    end

    @testset "plot statUnit=image (per-image mean = each dot an image)" begin
        # collapse each image to its mean, then plot those per-image means (n = #images). Deterministic
        # frame: image x cells [1,3,5] (mean 3), image y cells [10,20,30] (mean 20).
        df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                       "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 3.0, 5.0, 10.0, 20.0, 30.0])
        r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image)
        @test length(r["series"]) == 1                     # images pooled into ONE box
        @test r["series"][1]["n"] == 2                      # two datapoints = two images
        @test r["series"][1]["median"] == 11.5 && r["series"][1]["mean"] == 11.5   # of [3, 20]
        # default (individual) + per-image scope → one box PER image, each over its own cells (n = 3);
        # image-mean instead pools those into a single box whose points are the two image means.
        r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true)
        @test length(r0["series"]) == 2 && all(s["n"] == 3 for s in r0["series"])
        # bar over per-image means → mean of the image means
        rb = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, stat_unit=:image)
        @test rb["series"][1]["value"] == 11.5 && rb["series"][1]["n"] == 2

        # with groupBy: per-image mean WITHIN each level → each level's points are its image means.
        df2 = DataFrame("value_name" => fill("A", 8), "pop" => fill("/p", 8),
                        "uID" => ["x","x","y","y","x","x","y","y"],
                        "m"  => [2.0, 4.0, 6.0, 8.0, 20.0, 20.0, 30.0, 10.0],
                        "st" => [1.0, 1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  2.0])
        r2 = Cecelia._summary_agg(df2, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, group_by="st", stat_unit=:image)
        byg = Dict(s["group"] => s for s in r2["series"])
        @test byg["1"]["n"] == 2 && byg["1"]["median"] == 5.0     # st1: x[2,4]→3, y[6,8]→7 → [3,7]
        @test byg["2"]["n"] == 2 && byg["2"]["median"] == 20.0    # st2: x[20,20]→20, y[30,10]→20

        # with attr_map: one series PER ATTRIBUTE value, points = the images in it.
        dfa = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                        "uID" => ["x","x","y","y","z","z"], "m" => [2.0, 4.0, 6.0, 8.0, 100.0, 100.0])
        am = Dict("x" => "ctrl", "y" => "ctrl", "z" => "treat")
        ra = Cecelia._summary_agg(dfa, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, stat_unit=:image, attr_map=am)
        bya = Dict(s["uID"] => s for s in ra["series"])
        @test Set(keys(bya)) == Set(["ctrl", "treat"])
        @test bya["ctrl"]["n"] == 2 && bya["ctrl"]["median"] == 5.0   # images x(3), y(7) → [3,7]
        @test bya["treat"]["n"] == 1                                  # image z only

        # raw export honours it too: rows are the per-image means (label empty, value = the mean)
        rr = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                  normalize=:none, by_image=true, stat_unit=:image, raw=true)
        @test [rw["value"] for rw in rr["rows"]] == [3.0, 20.0]
        @test all(!haskey(rw, "label") for rw in rr["rows"]) && Set(rw["uID"] for rw in rr["rows"]) == Set(["x","y"])

        # image_agg=:median collapses each image by MEDIAN, not mean — distinguishable on skewed images:
        # x cells [1,2,9] (mean 4, median 2), y [10,20,90] (mean 40, median 20).
        dfs = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                        "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 2.0, 9.0, 10.0, 20.0, 90.0])
        rmean = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                     normalize=:none, by_image=true, stat_unit=:image, image_agg=:mean)
        rmed  = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                     normalize=:none, by_image=true, stat_unit=:image, image_agg=:median)
        @test rmean["series"][1]["value"] == 22.0   # mean of image means [4, 40]
        @test rmed["series"][1]["value"] == 11.0    # mean of image medians [2, 20]
        rmedraw = Cecelia._summary_agg(dfs, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                       normalize=:none, by_image=true, stat_unit=:image, image_agg=:median, raw=true)
        @test [rw["value"] for rw in rmedraw["rows"]] == [2.0, 20.0]   # per-image medians
    end

    @testset "plot matrix (heatmap: profile + crosstab)" begin
        # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
        # frame into one grid (no series). Deterministic synthetic frame — no fixture.
        df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                       "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                       "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                       "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
        pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                  normalize=:none, by_image=false,
                                  matrix_mode="profile", measures=["speed", "angle"], category="st")
        @test pr["matrixMode"] == "profile"
        @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
        cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
        @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
        @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
        @test cell(pr, "2", "speed")["n"] == 2
        @test isempty(pr["series"])

        # z-score standardises each row across its levels (mean 0) — the comparable "signature"
        prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false, zscore=true,
                                   matrix_mode="profile", measures=["speed", "angle"], category="st")
        @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
        zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
        @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

        # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
        # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
        dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                        "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
        ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                  normalize=:none, by_image=false,
                                  matrix_mode="crosstab", category="tr")
        @test ct["matrixMode"] == "crosstab"
        @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
        ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
        @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
        # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
        ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false,
                                   matrix_mode="crosstab", category="tr", matrix_normalize=:row)
        @test ctr["valueLabel"] == "P(to|from)"
        ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
        @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
        @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

        # error cases: unknown mode, missing category, profile with no present measure column
        @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
            nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
        @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
            nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

        # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
        dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                        "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
        prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                   normalize=:none, by_image=false, matrix_mode="profile",
                                   measures=["speed"], category="st")
        c2 = first(c for c in prn["cells"] if c["x"] == "2")
        @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell
    end

    @testset "plot attribute grouping (compare by attr)" begin
        # group cross-image series by an image attribute: images sharing a value pool into one series
        # labelled by the value; an image with no value falls back to its uID. No fixture.
        df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                       "uID" => ["x1","x1","x2","x2","y1","y1"],
                       "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
        amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
        r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                 normalize=:none, by_image=true, attr_map=amap)
        @test length(r["series"]) == 2
        @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
        byu = Dict(s["uID"] => s for s in r["series"])
        @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
        # image missing the attribute → falls back to its uID
        r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                  normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
        @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
        # no attr_map → group by image as before (3 images)
        r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                                  normalize=:none, by_image=true)
        @test length(r3["series"]) == 3
    end

    # ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
    @testset "plot_summary_data cross-image" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data cross-image (fixture missing)"
        else
            # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
            mk = function (uid)
                td = mktempdir(); mkpath(joinpath(td, "labelProps"))
                cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
                cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
                img = CciaImage(uid=uid, dir=td)
                img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
                img
            end
            imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

            # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
            one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
            both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
            @test "uID" in names(both)
            @test Set(unique(both.uID)) == Set(uids)
            @test nrow(both) == 2 * nrow(one)

            # boxplot per_image → one box per image (same data → equal stats), + scope field
            sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
            @test length(sp["series"]) == 2
            @test Set(s["uID"] for s in sp["series"]) == Set(uids)
            @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
            @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
            @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
            @test sp["series"][1]["n"] == nrow(one)

            # summarised → one pooled box across both images
            ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                                   measure="live.track.speed", granularity=:track, scope=:summarised)
            @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

            # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
            br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test br["chartType"] == "bar" && length(br["series"]) == 2
            @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
            @test br["series"][1]["value"] ≈ br["series"][2]["value"]

            # histogram per_image → 2 overlay series sharing bin edges
            hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                                   measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
            @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
            @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
        end
    end

    # ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
    @testset "plot_summary_data multi-segmentation targets" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "plot_summary_data multi-segmentation (fixture missing)"
        else
            # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
            # targets path: a (value_name, pop) per series, vcat across segmentations.
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
            img = CciaImage(uid="uM", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
            img.label_props["_active"] = "B"

            targets = [("B", "/_tracked"), ("B2", "/_tracked")]
            # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
            bp = plot_summary_data(img, "live", targets, "boxplot";
                                   measure="live.track.speed", granularity=:track)
            @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
            @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
            @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
            @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

            # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
            img2 = CciaImage(uid="uN", dir=td)
            img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
            img2.label_props["_active"] = "B"
            xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                                   measure="live.track.speed", granularity=:track, scope=:per_image)
            @test xp["scope"] == "per_image" && length(xp["series"]) == 4
            @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
                  Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
        end
    end

    # ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
    @testset "plot_summary_data helpers" begin
        @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
        @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
        let edges = Cecelia._hist_edges([0.0, 10.0], 10)
            @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
        end
        @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
        @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
        @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
        # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
        @test derived_pop_paths("live") == ["/_tracked"]
        @test isempty(derived_pop_paths("flow"))
    end

    # ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
    @testset "track_props (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "track_props (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # mock a categorical per-cell column to exercise the freq branch
            cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
            label_props(img_label_props_path(img, "B")) |>
                add_obs(DataFrame("label" => cells.label,
                                          "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

            tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
            ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
            @test nrow(tp) == ntr                                  # one row per track
            @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
            @test tp.label == tp.track_id                          # engine membership key
            # numeric aggregates present
            @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
            # categorical → per-category frequency columns
            @test "st.1" in names(tp) && "st.2" in names(tp)
            # motility joined from the track table
            @test "live.track.speed" in names(tp)
            # num_cells totals the tracked cells
            @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

            # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
            # off the decoded type + values: strings and integer code sets → categorical; continuous
            # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
            @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
            @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
            @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
            @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
            @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
            @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
            # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
            @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
            @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
            @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
            # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
            auto = track_props(img; value_name="B", cell_measures=["st"])
            @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
            # `numeric` escape-hatch forces it back to numeric aggregates when desired
            forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
            @test "st.mean" in names(forced) && !("st.1" in names(forced))
        end
    end

    # ── track_cell_measures: derive base cell measures from track-property column names ──
    @testset "track_cell_measures" begin
        mot = ["live.track.speed", "live.track.meanTurningAngle"]
        # motility axes need no cell aggregation
        @test isempty(track_cell_measures(["live.track.speed"], mot))
        # numeric aggregate columns → their base cell measure (suffix stripped)
        @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
              ["mean_intensity_0", "area"]
        # categorical frequency column `{base}.{cat}` → base
        @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
        # bookkeeping + motility skipped; dedup across aggregates of the same base
        @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                                   "area.mean", "area.sd"], mot) == ["area"]
    end

    # ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
    @testset "pop_df pop_type=track (KDIeEm B)" begin
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "pop_df pop_type=track (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid="KDIeEm", dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

            # motility-only track props (the common track-gating case: no cell_measures needed)
            tp = track_props(img; value_name="B")
            @test "live.track.speed" in names(tp)
            spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
            thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
            truth = count(>=(thr), spd)
            @test 0 < truth < length(spd)

            # a TRACK gate (one point per track) on the speed axis, stored under __tracks
            m = PopulationMap(pop_type="track", value_name="B")
            add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                                   thr, 1e12, -1e12, 1e12))
            save_pop_map!(m, img)
            @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
            @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

            # granularity=:track → gated track rows, one point per track, gate genuinely applied
            g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
            @test nrow(g) == truth
            @test length(unique(g.track_id)) == nrow(g)
            @test unique(g.pop) == ["/fast"]
            @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

            # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
            gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
            @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
            @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
            @test nrow(gc) > nrow(g)                                 # many cells per track
            @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))
        end
    end

    @testset "HMM states + transitions" begin
        # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
        # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
        # `missing` (per-cell states are undefined where a measurement can't exist).
        uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
        for img in ("X", "Y"), k in 1:3, t in 1:25
            slow = t <= 12
            s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
            a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
            if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
            push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
            push!(sp, s); push!(an, a)
        end
        df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                       "live.cell.speed" => sp, "live.cell.angle" => an)

        st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
        @test length(st) == nrow(df)

        # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
        # Vector{String}); fit must not MethodError on the normalise/scale step. (#00051)
        let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                                 scale_measures=Union{}[], normalise=Dict{String,String}())
            @test count(!ismissing, stu) == count(!ismissing, st)
        end
        @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
        @test Set(skipmissing(st)) == Set([1, 2])
        df[!, "live.cell.hmm.state.default"] = st

        one = st[(df.uID .== "X") .& (df.track_id .== 1)]
        @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
        decoded = collect(skipmissing(one))
        @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

        tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                             include_start=false, include_self=true)
        @test length(tr) == nrow(df)
        nonmiss = collect(skipmissing(tr))
        @test all(occursin("_", x) for x in nonmiss)
        @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
        @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

        trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                              include_start=false, include_self=false)
        nm2 = Set(skipmissing(trn))
        @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

        # cross-model hybrid: two state columns paste into "a.b" before transitions
        df[!, "live.cell.hmm.state.second"] = st
        trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                              time_col="t", include_start=false, include_self=true)
        @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

        # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
        # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
        # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
        # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
        @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
              ["A/_tracked", "B/_tracked"]
        @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
        @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
              Set(["A", "B", "C"])

        # task registration + set-scope routing
        @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
        @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
        @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
        @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
        @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
        @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
    end

    # ── Physical-size / timing metadata (import review, edit, resync) ──────────────
    # All fixtures are synthetic temp zarrs (no real data) — these functions are pure
    # readers/writers over the bioformats2raw nested layout (`zarr/0/.zattrs`, `zarr/OME`).
    @testset "OME metadata read/edit/resync" begin
        # Build a minimal bioformats2raw-shaped zarr: `0/.zattrs` (multiscales) + optional
        # `0/0/.zarray` (shape → SizeC/T/Z) + optional `OME/METADATA.ome.xml` (planes).
        function make_zarr(dir; axes, level_scales, units = Dict{String,String}(),
                           shape = nothing, planes = nothing)
            mkpath(joinpath(dir, "0"))
            ax_objs = map(axes) do a
                o = Dict{String,Any}("name" => a,
                    "type" => a in ("x", "y", "z") ? "space" : (a == "t" ? "time" : "channel"))
                haskey(units, a) && (o["unit"] = units[a])
                o
            end
            datasets = [Dict{String,Any}("path" => string(i - 1),
                          "coordinateTransformations" =>
                              [Dict{String,Any}("type" => "scale", "scale" => level_scales[i])])
                        for i in eachindex(level_scales)]
            zattrs = Dict{String,Any}("multiscales" =>
                [Dict{String,Any}("axes" => ax_objs, "datasets" => datasets)])
            open(joinpath(dir, "0", ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
            if !isnothing(shape)
                mkpath(joinpath(dir, "0", "0"))
                open(joinpath(dir, "0", "0", ".zarray"), "w") do io
                    JSON3.write(io, Dict{String,Any}("shape" => shape))
                end
            end
            if !isnothing(planes)
                mkpath(joinpath(dir, "OME"))
                body = join([
                    "<Plane TheZ=\"$(p.z)\" TheT=\"$(p.t)\" DeltaT=\"$(p.dt)\"" *
                    (haskey(p, :unit) ? " DeltaTUnit=\"$(p.unit)\"" : "") * "/>"
                    for p in planes], "\n")
                open(joinpath(dir, "OME", "METADATA.ome.xml"), "w") do io
                    write(io, "<OME><Image><Pixels>$body</Pixels></Image></OME>")
                end
            end
            dir
        end

        # ── _delta_t_fallback: per-plane DeltaT (TheZ=0, TheT=1), unit-converted to seconds ──
        @testset "_delta_t_fallback" begin
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 0, dt = 0.0, unit = "ms"),
                                    (z = 0, t = 1, dt = 5000.0, unit = "ms"),
                                    (z = 0, t = 2, dt = 10000.0, unit = "ms")])
                @test Cecelia._delta_t_fallback(d) == 5.0            # 5000 ms → 5 s, from TheT=1
            end
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 1, dt = 2.0, unit = "min")])
                @test Cecelia._delta_t_fallback(d) == 120.0          # 2 min → 120 s
            end
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                          planes = [(z = 0, t = 1, dt = 30.0)])      # no unit → seconds
                @test Cecelia._delta_t_fallback(d) == 30.0
            end
            # non-self-closing <Plane>…</Plane> (some vendors) — DeltaT is on the opening tag
            mktempdir() do d
                mkpath(joinpath(d, "OME"))
                write(joinpath(d, "OME", "METADATA.ome.xml"),
                      "<OME><Image><Pixels>" *
                      "<Plane TheZ=\"0\" TheT=\"1\" DeltaT=\"3\" DeltaTUnit=\"s\"><Annotation/></Plane>" *
                      "</Pixels></Image></OME>")
                @test Cecelia._delta_t_fallback(d) == 3.0
            end
            @test isnothing(Cecelia._delta_t_fallback(joinpath(tempdir(), "nope-$(rand(UInt32))")))
        end

        # ── read_ome_metadata: unit-less-t placeholder is rejected; DeltaT fills the gap ──
        @testset "read_ome_metadata" begin
            # t axis has a scale (1.0) but NO unit → placeholder, must NOT become TimeIncrement=1.0;
            # SizeT>1 so the DeltaT fallback kicks in and supplies the real interval.
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"],
                          level_scales = [[1.0, 0.6, 0.5, 0.5]],
                          units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                          shape = [3, 1, 4, 4],
                          planes = [(z = 0, t = 1, dt = 7.0, unit = "s")])
                m = read_ome_metadata(d)
                @test m["SizeT"] == 3
                @test m["PhysicalSizeX"] == 0.5
                @test m["PhysicalSizeZ"] == 0.6
                @test m["PhysicalSizeUnit"] == "micrometer"
                @test m["TimeIncrement"] == 7.0                       # from DeltaT, not the 1.0 placeholder
                @test m["TimeIncrementUnit"] == "second"
            end
            # t axis WITH a unit → trusted verbatim, no fallback needed.
            mktempdir() do d
                make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                          units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                          shape = [4, 8, 8])
                m = read_ome_metadata(d)
                @test m["TimeIncrement"] == 2.5
                @test m["TimeIncrementUnit"] == "second"
            end
        end

        # ── update_ome_scale!: level-0 value set, other levels keep their downsample ratio; units ──
        @testset "update_ome_scale!" begin
            mktempdir() do d
                # z doesn't downsample (0.6, 0.6); x halves per level (0.5, 1.0)
                make_zarr(d; axes = ["z", "y", "x"],
                          level_scales = [[0.6, 0.5, 0.5], [0.6, 1.0, 1.0]])
                update_ome_scale!(d, Dict("z" => 3.0, "x" => 0.65);
                    units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
                z = JSON3.read(read(joinpath(d, "0", ".zattrs"), String))
                dss = z[:multiscales][1][:datasets]
                s0 = dss[1][:coordinateTransformations][1][:scale]
                s1 = dss[2][:coordinateTransformations][1][:scale]
                @test s0[1] == 3.0 && s1[1] == 3.0                   # z ratio 5× applied to both levels
                @test s0[3] == 0.65 && isapprox(s1[3], 1.3)          # x ratio 1.3× preserves downsample
                m = read_ome_metadata(d)
                @test m["PhysicalSizeUnit"] == "micrometer"          # axis unit now round-trips
                @test m["PhysicalSizeZ"] == 3.0
            end
            # unit-only edit (no numeric change) still writes the axis unit
            mktempdir() do d
                make_zarr(d; axes = ["y", "x"], level_scales = [[0.5, 0.5]])
                @test isnothing(get(read_ome_metadata(d), "PhysicalSizeUnit", nothing))
                update_ome_scale!(d, Dict{String,Float64}();
                                  units = Dict("x" => "nanometer", "y" => "nanometer"))
                @test read_ome_metadata(d)["PhysicalSizeUnit"] == "nanometer"
            end
        end

        # ── update_ome_xml_pixels!: replace an existing attr, insert a missing one ──
        @testset "update_ome_xml_pixels!" begin
            mktempdir() do d
                mkpath(joinpath(d, "OME"))
                xml_file = joinpath(d, "OME", "METADATA.ome.xml")
                write(xml_file, "<OME><Image><Pixels SizeX=\"4\" PhysicalSizeZ=\"0.6\">" *
                                "<Plane/></Pixels></Image></OME>")
                update_ome_xml_pixels!(d, Dict("PhysicalSizeZ" => "3.0", "TimeIncrement" => "5.0"))
                out = read(xml_file, String)
                @test occursin("PhysicalSizeZ=\"3.0\"", out)         # replaced
                @test !occursin("PhysicalSizeZ=\"0.6\"", out)
                @test occursin("TimeIncrement=\"5.0\"", out)         # inserted
                @test occursin("SizeX=\"4\"", out)                   # untouched
            end
        end

        # ── sync_zarr_calibration!: one translator (meta shape → zarr) for import + editor ──
        @testset "sync_zarr_calibration!" begin
            @test !Cecelia.has_calibration_meta(Dict{String,Any}("SizeC" => 2))
            @test !Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => nothing))  # null clear
            @test Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => 3.0))
            mktempdir() do d
                make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 0.6, 0.5, 0.5]],
                          shape = [3, 1, 4, 4],
                          planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])  # OME/ dir + <Pixels>
                # a meta-shaped correction (as ccid.json / the importer / the editor produce it)
                Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                    "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "micrometer",
                    "TimeIncrement" => 5.0, "TimeIncrementUnit" => "second"))
                # .zattrs now round-trips the corrected spatial value + unit
                m = read_ome_metadata(d)
                @test m["PhysicalSizeZ"] == 3.0
                @test m["PhysicalSizeUnit"] == "micrometer"
                # OME-XML <Pixels> carries the time interval napari reads unconditionally
                xml = read(joinpath(d, "OME", "METADATA.ome.xml"), String)
                @test occursin("TimeIncrement=\"5.0\"", xml)
                @test occursin("TimeIncrementUnit=\"s\"", xml)
            end
        end

        # ── _merge_zarr_meta_into_ccid!: overwrite=true is authoritative; false is fill-only ──
        @testset "merge fill-only vs overwrite" begin
            proj = create_project!(name = "meta-merge-$(rand(1000:9999))", kind = "static")
            s    = add_set!(proj; name = "set")
            # Simulate an ImageJ-corrected image: PhysicalSizeZ + the ccid-only PhysicalSizeZ_raw marker
            img  = add_image!(s; name = "img", meta = Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))

            # Fill-only backfill: existing keys survive, genuinely-missing ones get filled.
            Cecelia._merge_zarr_meta_into_ccid!(img,
                Dict{String,Any}("PhysicalSizeZ" => 0.6, "PhysicalSizeX" => 0.5); overwrite = false)
            r = init_object(proj.uid, img.uid)
            @test r.meta["PhysicalSizeZ"] == 3.0                     # NOT reverted to the raw 0.6
            @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker NOT dropped
            @test r.meta["PhysicalSizeX"] == 0.5                     # filled (was absent)

            # Authoritative import merge: clears derived keys, takes the fresh read verbatim.
            Cecelia._merge_zarr_meta_into_ccid!(r,
                Dict{String,Any}("PhysicalSizeZ" => 0.6); overwrite = true)
            r2 = init_object(proj.uid, img.uid)
            @test r2.meta["PhysicalSizeZ"] == 0.6
            @test !haskey(r2.meta, "PhysicalSizeZ_raw")              # zombie marker cleared
            rm(proj.root; recursive = true)
        end

        # ── resync_ome_meta! end-to-end: fill-only backfill never reverts a correction ──
        @testset "resync_ome_meta! fill-only" begin
            proj = create_project!(name = "meta-resync-$(rand(1000:9999))", kind = "static")
            s    = add_set!(proj; name = "set")
            img  = add_image!(s; name = "img", meta = Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))   # ImageJ-corrected, ccid-only

            # Register a "default" zarr on disk carrying the RAW (pre-correction) calibration.
            zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
            make_zarr(zdir; axes = ["z", "y", "x"], level_scales = [[0.6, 0.5, 0.5]],
                      units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                      shape = [1, 8, 8])
            img.filepath["default"]         = "img.ome.zarr"
            img.filepath[VERSIONED_ACTIVE_KEY] = "default"
            save!(img)

            @test resync_ome_meta!(init_object(proj.uid, img.uid))
            r = init_object(proj.uid, img.uid)
            @test r.meta["PhysicalSizeZ"] == 3.0                     # correction survives resync
            @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker survives
            @test r.meta["PhysicalSizeUnit"] == "micrometer"         # genuinely-missing field filled
            @test r.meta["PhysicalSizeX"] == 0.5
            rm(proj.root; recursive = true)
        end
    end

    @testset "QC framework" begin
        @testset "sidecar round-trip" begin
            img = CciaImage(; dir = mktempdir())
            f = qc_finding("warn", "demo.code", "short text", "long text"; detail = Dict("k" => 1))
            @test f["level"] == "warn" && f["code"] == "demo.code"

            p = write_qc(img, "cleanupImages.driftCorrect", "driftCorrected", [f];
                         source = Dict("shape" => [1, 2, 3]))
            @test isfile(p)
            @test occursin(joinpath("qc", "cleanupImages.driftCorrect", "driftCorrected.json"), p)

            doc = read_qc(img, "cleanupImages.driftCorrect", "driftCorrected")
            @test length(doc["findings"]) == 1
            @test doc["findings"][1]["code"] == "demo.code"

            all = read_all_qc(img)
            @test haskey(all, "cleanupImages.driftCorrect/driftCorrected")

            # no-value_name → falls back to the default key
            write_qc(img, "some.task", "", Dict{String,Any}[])
            @test isfile(qc_path(img, "some.task", VERSIONED_DEFAULT_VAL))
        end

        @testset "canvas-expansion check" begin
            order = "TCZYX"
            # fHqhyb: XY +42%/+21% → flagged; Z doubling is ignored
            bad = qc_canvas_expansion([94, 4, 13, 512, 512], [94, 4, 26, 728, 618], order)
            @test bad !== nothing && bad["code"] == "output.canvas_expansion"
            # LUkCpP (normal): XY +6%/+3% → not flagged even though Z grew +46%
            @test qc_canvas_expansion([64, 4, 13, 512, 512], [64, 4, 19, 541, 527], order) === nothing
        end

        @testset "drift findings" begin
            base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"])
            smooth = [[0.0, 1.0, 1.0] for _ in 1:20]
            spiky  = copy(smooth); spiky[16] = [0.0, 120.0, 90.0]   # jump at frame 16

            # bad ref: canvas ballooned AND a spike → both findings
            meta_bad = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                        "outputShape" => [20, 4, 26, 728, 618], "shifts" => spiky))
            fb, _, _ = Cecelia._drift_qc_findings(meta_bad)
            codes = Set(f["code"] for f in fb)
            @test "drift.canvas_expansion" in codes
            @test "drift.jump" in codes
            jump = first(f for f in fb if f["code"] == "drift.jump")
            @test jump["detail"]["atT"] == 15                       # 0-based frame index of the spike

            # good ref: modest canvas, smooth trajectory → no findings
            meta_ok = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                       "outputShape" => [20, 4, 19, 541, 527], "shifts" => smooth))
            fo, _, _ = Cecelia._drift_qc_findings(meta_ok)
            @test isempty(fo)
        end

        @testset "OIR companion-file staging" begin
            # REAL Olympus naming: the registered file already ends in _NNNN.oir and companions are
            # EXTENSIONLESS <mainstem>_00001, _00002, … (this is what shipped broken — only the main
            # matched, so bioformats saw a fraction of the timepoints).
            real = ["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045",
                    "M1a-res_0002.oir",           # a DIFFERENT acquisition — must NOT be grabbed
                    "M1a-res_0001_notes.txt"]     # non-numeric sibling — excluded
            @test Set(Cecelia._companion_files(real, "M1a-res_0001.oir")) ==
                  Set(["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045"])

            # extensioned companions (Img.oir + Img_00001.oir …); sibling Img2 / non-numbered excluded
            names = ["Img.oir", "Img_00001.oir", "Img_00002.oir",
                     "Img2.oir", "Img_processed.oir", "Other.oir", "notes.txt"]
            @test Set(Cecelia._companion_files(names, "Img.oir")) ==
                  Set(["Img.oir", "Img_00001.oir", "Img_00002.oir"])
            # regex metacharacters in the stem (the `basal+NECA` bug): literal match, no injection
            plus = ["basal+NECA.oir", "basal+NECA_00001", "basal+NECB.oir"]
            @test Set(Cecelia._companion_files(plus, "basal+NECA.oir")) ==
                  Set(["basal+NECA.oir", "basal+NECA_00001"])
            # single self-contained file → just itself
            @test Cecelia._companion_files(["a.tif", "b.tif"], "a.tif") == ["a.tif"]

            # chunked yielding copy is byte-identical (incl. a size that isn't a chunk multiple)
            src = tempname(); dst = tempname()
            data = rand(UInt8, 3 * 1024 * 1024 + 777)
            write(src, data)
            copied = Ref(0)
            Cecelia._copy_file_yielding(src, dst; chunk = 1024 * 1024, on_bytes = n -> (copied[] += n))
            @test read(dst) == data
            @test copied[] == length(data)
            rm(src; force = true); rm(dst; force = true)
        end

        @testset "rescale (16→8-bit) findings" begin
            mk_ch(i; span = 100.0, clipHigh = 0.0, trueMax = 100, p999 = 100) =
                Dict{String,Any}("index" => i, "rangeSpan" => span, "clipHighFrac" => clipHigh,
                                 "trueMax" => trueMax, "p999" => p999)
            meta = Dict{String,Any}("rescale8bit" => Dict{String,Any}("channels" => [
                mk_ch(0),                                  # healthy → no finding
                mk_ch(1; span = 0.0),                      # flat
                mk_ch(2; clipHigh = 0.05),                 # clips bright signal
                mk_ch(3; trueMax = 1000, p999 = 100),      # hot pixel (max ≫ p99.9)
            ]))

            fs    = Cecelia.rescale_qc_findings(meta)
            codes = Dict(f["detail"]["channel"] => f["code"] for f in fs)
            @test length(fs) == 3
            @test !haskey(codes, 0)
            @test codes[1] == "rescale.channel_flat"
            @test codes[2] == "rescale.channel_clipped"
            @test codes[3] == "rescale.hot_pixel"

            m = Cecelia.rescale_metrics(meta)
            @test m["nChannelsFlat"] == 1
            @test m["nChannelsClipped"] == 1
            @test !haskey(m, "nChannels")                 # channel count is a base import metric

            # base import metric — present for EVERY import (from SizeC/SizeZ/SizeT)
            bm = Cecelia.import_metrics(Dict{String,Any}("SizeC" => 4, "SizeZ" => 13, "SizeT" => 20))
            @test bm == Dict{String,Any}("nChannels" => 4, "nZ" => 13, "nT" => 20)
            @test Cecelia.import_metrics(Dict{String,Any}()) === nothing

            # unconverted image (no rescale8bit) → no rescale findings, no rescale metric
            @test isempty(Cecelia.rescale_qc_findings(Dict{String,Any}()))
            @test Cecelia.rescale_metrics(Dict{String,Any}()) === nothing

            # JSON3 round-trip — the real path (meta read back from ccid.json has Symbol keys)
            rt     = JSON3.read(JSON3.write(meta))
            rtmeta = Dict{String,Any}(String(k) => v for (k, v) in rt)
            @test length(Cecelia.rescale_qc_findings(rtmeta)) == 3
            @test Cecelia.rescale_metrics(rtmeta)["nChannelsFlat"] == 1
        end

        @testset "count metrics" begin
            # pure: distinct tracks, mean cells/track, tracked-cell total; untracked = missing/NaN/≤0
            nt, ml, ntc = track_count_metrics([1, 1, 1, 2, 2, 0, -1, NaN, missing, 3])
            @test nt == 3                       # tracks 1, 2, 3
            @test ntc == 6                      # 3 + 2 + 1 cells tracked
            @test ml ≈ 2.0                      # 6 cells / 3 tracks

            # no tracks at all → zeros (drives the "No tracks formed" advisory)
            @test track_count_metrics([0, NaN, missing]) == (0, 0.0, 0)
            @test track_count_metrics(Float64[]) == (0, 0.0, 0)

            # floats round to the nearest track id
            n2, _, c2 = track_count_metrics([1.0, 1.0, 2.0])
            @test (n2, c2) == (2, 3)

            # segment counts → findings: 0 base cells warns; any base count is clean
            f0, p0 = Cecelia._segment_qc_findings(Dict("base" => 0))
            @test p0 == 0 && length(f0) == 1 && f0[1]["code"] == "segment.no_cells"
            fN, pN = Cecelia._segment_qc_findings(Dict("base" => 812, "nuc" => 790))
            @test pN == 812 && isempty(fN)
            # no explicit "base" key → primary falls back to the sole type's count
            _, pf = Cecelia._segment_qc_findings(Dict("nuc" => 5))
            @test pf == 5

            # metadata calibration findings (port of the old frontend fieldIssues) — codes + field
            codes(fs) = [f["code"] for f in fs]; fields(fs) = [f["detail"]["field"] for f in fs]
            # clean 3D timelapse with units → nothing
            @test isempty(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"SizeT"=>5,
                "PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0,"PhysicalSizeUnit"=>"micron",
                "TimeIncrement"=>30.0,"TimeIncrementUnit"=>"second")))
            # z stack, no z spacing → z_spacing_unknown
            @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeUnit"=>"micron"))) ==
                  ["metadata.z_spacing_unknown"]
            # auto-corrected z (PhysicalSizeZ_raw marker) → z_spacing_corrected
            @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeZ"=>2.0,
                "PhysicalSizeUnit"=>"micron","PhysicalSizeZ_raw"=>99.0))) == ["metadata.z_spacing_corrected"]
            # unusual z:xy ratio (100:1 > 50) → z_spacing_unusual
            @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>1.0,"PhysicalSizeZ"=>100.0,
                "PhysicalSizeUnit"=>"micron"))) == ["metadata.z_spacing_unusual"]
            # timelapse, no interval → frame_interval_unknown; string values coerce
            @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>"8","PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,
                "PhysicalSizeUnit"=>"micron"))) == ["metadata.frame_interval_unknown"]
            # interval present, no unit → frame_interval_no_unit
            @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>8,"TimeIncrement"=>30.0,
                "PhysicalSizeX"=>0.5,"PhysicalSizeUnit"=>"micron"))) == ["metadata.frame_interval_no_unit"]
            # no spatial unit, x+y+z present (2D-safe: SizeZ=1 so no z-spacing case) → three no-unit (x,y,z)
            fu = Cecelia.metadata_qc_findings(Dict("PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0))
            @test all(==("metadata.pixel_size_no_unit"), codes(fu)) && fields(fu) == ["x","y","z"]
            # z-spacing case suppresses the z no-unit dup (z already flagged)
            fz = Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5))
            @test codes(fz) == ["metadata.z_spacing_unknown","metadata.pixel_size_no_unit","metadata.pixel_size_no_unit"]
            @test fields(fz) == ["z","x","y"]     # no second z entry

            # severity symbols: shape-distinct (✅/⚠️/❌), NOT same-shape circles; unknown → ""
            @test Cecelia.severity_symbol("ok")   == "✅"
            @test Cecelia.severity_symbol("warn") == "⚠️"
            @test Cecelia.severity_symbol("fail") == "❌"
            @test length(Set(values(Cecelia.SEVERITY_SYMBOLS))) == 3   # distinct glyphs
            @test !("🟢" in values(Cecelia.SEVERITY_SYMBOLS))          # not the colour-blind trap
            @test Cecelia.severity_symbol("bogus") == ""

            # clustering findings (set-scope: total = run clusters; the rest = one image's slice)
            # run collapsed to 1 cluster → warn (regardless of the per-image numbers)
            c1 = Cecelia.cluster_qc_findings(1, 500, 1, 1.0)
            @test length(c1) == 1 && c1[1]["code"] == "clustering.single_cluster" && c1[1]["level"] == "warn"
            # image's cells all in one cluster while the run found several → warn (batch outlier)
            c2 = Cecelia.cluster_qc_findings(6, 400, 1, 1.0)
            @test length(c2) == 1 && c2[1]["code"] == "clustering.image_one_cluster" && c2[1]["level"] == "warn"
            # one cluster dominates this image (≥90%) but not all → info
            c3 = Cecelia.cluster_qc_findings(6, 400, 3, 0.95; unit = "tracks")
            @test length(c3) == 1 && c3[1]["code"] == "clustering.dominant_cluster" && c3[1]["level"] == "info"
            @test occursin("tracks", c3[1]["short"])
            # a healthy spread flags nothing; an empty image (n=0) never flags
            @test isempty(Cecelia.cluster_qc_findings(6, 400, 5, 0.4))
            @test isempty(Cecelia.cluster_qc_findings(6, 0, 0, 0.0))

            # category distribution metrics (HMM states/transitions): skip NaN/missing/nothing
            m1 = Cecelia.category_dist_metrics([1.0, 1.0, 2.0, NaN, missing])
            @test m1.n == 3 && m1.n_distinct == 2 && m1.dominant_frac ≈ 2/3
            ms = Cecelia.category_dist_metrics(["1_2", "1_2", nothing, "2_1"])
            @test ms.n == 3 && ms.n_distinct == 2
            @test Cecelia.category_dist_metrics(Any[NaN, missing, nothing]).n == 0

            # HMM state findings: no decode → warn; single state → warn; ≥95% one state → info; else none
            @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(Float64[]))[1]["code"] == "hmm.no_states_decoded"
            @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 1.0]))[1]["code"] == "hmm.single_state"
            fd = Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(vcat(fill(1.0, 96), fill(2.0, 4))))
            @test fd[1]["code"] == "hmm.dominant_state" && fd[1]["level"] == "info"
            @test isempty(Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 2.0, 2.0])))
            # HMM transitions: only the no-transitions case flags
            @test Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(Any[nothing]))[1]["code"] == "hmm.no_transitions"
            @test isempty(Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(["1_2", "2_1"])))

            # track measures: auto + low-confidence motion dims → warn; confident/user-set → none
            tm = Cecelia.track_measures_qc_findings(120, "auto", 2, 2, "low", "z ambiguous")
            @test length(tm) == 1 && tm[1]["code"] == "tracking.motion_dims_uncertain" && tm[1]["level"] == "warn"
            @test isempty(Cecelia.track_measures_qc_findings(120, "auto", 3, 3, "high", "clear"))
            @test isempty(Cecelia.track_measures_qc_findings(120, "3D", 3, 2, "low", "user forced"))  # user-set: no flag

            # against the tracked fixture: metrics agree with an independent count of track_id
            h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
            if !have_fixture(h5)
                @test_skip "track_count_metrics fixture (missing)"
            else
                tids = (label_props(h5) |> select_cols(["track_id"]) |> as_df).track_id
                valid = Int.(filter(t -> !isnan(t) && t > 0, tids))
                fnt, fml, fntc = track_count_metrics(tids)
                @test fnt == length(unique(valid))
                @test fntc == length(valid)
                @test fml ≈ length(valid) / length(unique(valid))
            end
        end

        @testset "cohort outliers (robust median/MAD)" begin
            # MAD>0 regime: a clear outlier flags even at n=3 (mean/SD couldn't — max |z| there = 1.15)
            r3 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0))
            @test r3.n == 3 && haskey(r3.outliers, "c") && !haskey(r3.outliers, "a")
            @test r3.median == 800.0 && r3.mad > 0
            @test r3.outliers["c"]["z"] |> abs > 3.5             # modified-z carried for the flag

            # MAD==0 regime — THE qcProbe case [800,800,100]: two identical baselines → MAD 0 (and the
            # old mean-abs-dev fallback missed it). Relative-departure rule flags the 100, not the 800s.
            r0 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>100.0))
            @test r0.mad == 0.0 && haskey(r0.outliers, "c") && !haskey(r0.outliers, "a")
            @test r0.outliers["c"]["relDev"] >= 0.5             # relative departure carried for the flag
            # …but a near-identical value (801 vs 800) is NOT flagged (no magnitude → no false positive)
            @test isempty(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>801.0)).outliers)

            # too few to judge (no cohort) → no outliers even with a wild value
            @test isempty(Cecelia._cohort_outliers(Dict("a"=>5.0,"b"=>500.0)).outliers)
            # all identical → nothing flagged (no false positive)
            @test isempty(Cecelia._cohort_outliers(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0)).outliers)
            # a lower explicit threshold is honoured in the MAD>0 regime (more sensitive)
            @test haskey(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0), 1.0).outliers, "c")

            # per-image finding from an outlier entry — direction from value vs median; carries detail
            cf = Cecelia._cohort_finding("nCells", Dict{String,Any}("value"=>100.0,"z"=>-5.2), 800.0)
            @test cf["code"] == "cohort.nCells" && cf["level"] == "warn"
            @test cf["detail"]["metric"] == "nCells" && cf["detail"]["value"] == 100.0 && cf["detail"]["z"] == -5.2
            @test occursin("below", cf["long"])
            cf2 = Cecelia._cohort_finding("nTracks", Dict{String,Any}("value"=>900.0,"relDev"=>0.8), 500.0)
            @test occursin("above", cf2["long"]) && cf2["detail"]["relDev"] == 0.8

            # lab-log summary lines + has-outliers predicate (drives whether the check logs at all)
            clean = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
                "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,"outliers"=>Dict{String,Any}())))
            @test !cohort_has_outliers(clean)
            cl = cohort_qc_summary_lines(clean)
            @test length(cl) == 1 && occursin("all 10", cl[1]) && startswith(cl[1], "✅")
            flagged = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
                "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,
                    "outliers"=>Dict{String,Any}("j"=>Dict{String,Any}("value"=>100.0,"z"=>-5.2)))))
            @test cohort_has_outliers(flagged)
            fl = cohort_qc_summary_lines(flagged)
            @test startswith(fl[1], "⚠️") && any(l -> occursin("j", l) && occursin("100", l), fl)
        end

        @testset "cohort round-trip (banked metrics → set sidecar)" begin
            set = CciaSet(; dir = mktempdir())
            counts = Dict("a"=>800,"b"=>810,"c"=>790,"d"=>805,"e"=>795,
                          "f"=>808,"g"=>803,"h"=>797,"i"=>802,"j"=>100)
            for (uid, n) in counts
                img = CciaImage(; uid = uid, dir = mktempdir())
                write_qc(img, "segment.measureLabels", "default", Dict{String,Any}[];
                         metrics = Dict{String,Any}("nCells" => n))
                push!(set._images, img); push!(set.image_uids, uid)
            end
            # READ-ONLY path (GET): computes outliers but writes NOTHING (no sidecar, no per-image)
            ro = cohort_qc_for(set, "segment.measureLabels", "default")
            @test haskey(ro["metrics"]["nCells"]["outliers"], "j")
            @test !isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
            @test read_qc(set._images[findfirst(i -> i.uid == "j", set._images)],
                          "cohort.segment.measureLabels", "default") === nothing
            # PERSIST path (the check action): sidecar + per-image findings
            doc = cohort_qc_for!(set, "segment.measureLabels", "default")
            m = doc["metrics"]["nCells"]
            @test m["n"] == 10 && haskey(m["outliers"], "j") && !haskey(m["outliers"], "a")
            @test doc["nIncluded"] == 10
            # sidecar written + re-readable
            @test isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
            @test read_cohort_qc(set, "segment.measureLabels", "default")["nIncluded"] == 10
            @test haskey(read_all_cohort_qc(set), "segment.measureLabels/default")
            # per-image write-back: the outlier (j) gets a cohort finding ON the image. A normal image
            # (a) is NOT written — no empty placeholder (that would put an empty cohort.* doc on every
            # image on every check). Under the cohort.* namespace, merged by read_all_qc.
            byid = Dict(i.uid => i for i in set._images)
            fj = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
            @test fj !== nothing && !isempty(fj["findings"])
            @test fj["findings"][1]["code"] == "cohort.nCells" && fj["findings"][1]["level"] == "warn"
            @test occursin("below", fj["findings"][1]["long"])         # 100 < median 800
            @test read_qc(byid["a"], "cohort.segment.measureLabels", "default") === nothing
            @test haskey(read_all_qc(byid["j"]), "cohort.segment.measureLabels/default")
            # clear-stale: bump the outlier back into range and re-check → j's prior cohort doc is
            # CLEARED (written empty, un-flags), not left as a stale warning
            byid["j"].included = true
            write_qc(byid["j"], "segment.measureLabels", "default", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nCells" => 801))
            cohort_qc_for!(set, "segment.measureLabels", "default")
            fj2 = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
            @test fj2 !== nothing && isempty(fj2["findings"])          # existing doc cleared, not deleted
            # excluded images drop out of the cohort
            set._images[1].included = false                      # exclude one
            @test cohort_qc_for!(set, "segment.measureLabels", "default")["nIncluded"] == 9
            # unknown fun errors (not a metric producer)
            @test_throws ErrorException cohort_qc_for!(set, "not.aTask", "default")
        end

        @testset "cohort value_name discovery (per label set)" begin
            set = CciaSet(; dir = mktempdir())
            # clustering banks per label set: T-tracks tight, B-tracks with one sparse image (c=9)
            for (uid, nT, nB) in [("a", 40, 22), ("b", 39, 24), ("c", 41, 9)]
                img = CciaImage(; uid = uid, dir = mktempdir())
                write_qc(img, "clustTracks.cluster", "T", Dict{String,Any}[];
                         metrics = Dict{String,Any}("nTracks"=>nT, "nClusters"=>4, "largestClusterFrac"=>0.4))
                write_qc(img, "clustTracks.cluster", "B", Dict{String,Any}[];
                         metrics = Dict{String,Any}("nTracks"=>nB, "nClusters"=>3, "largestClusterFrac"=>0.5))
                push!(set._images, img); push!(set.image_uids, uid)
            end
            # discovers the banked label sets (sorted), empty for a fun that banked nothing
            @test cohort_value_names(set, "clustTracks.cluster") == ["B", "T"]
            @test cohort_value_names(set, "segment.cellpose") == String[]
            # per-value_name cohorts: T and B are SEPARATE cohorts
            allc = cohort_qc_for_all(set, "clustTracks.cluster")
            @test Set(keys(allc)) == Set(["B", "T"])
            @test allc["T"]["valueName"] == "T" && allc["B"]["valueName"] == "B"
            # the sparse B image (c) flags in the B cohort, not the T cohort
            @test haskey(allc["B"]["metrics"]["nTracks"]["outliers"], "c")
            @test !haskey(allc["T"]["metrics"]["nTracks"]["outliers"], "c")
            @test !isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))   # read-only wrote nothing
            # persist variant writes each label set's sidecar
            allw = cohort_qc_for_all!(set, "clustTracks.cluster")
            @test isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))
            @test isfile(cohort_qc_path(set, "clustTracks.cluster", "T"))
            @test occursin("(B)", join(cohort_qc_summary_lines(allw["B"])))   # label set named in the summary
        end

        @testset "cluster QC banked per run (suffix, no collision)" begin
            img = CciaImage(; uid = "a", dir = mktempdir())
            qcdir = mktempdir()
            # a cluster_qc.json fixture (what the Python runner writes): two segments T & B
            mkqc(path) = open(path, "w") do io
                JSON3.write(io, Dict("nClusters" => 4, "perSegment" => [
                    Dict("uID" => "a", "valueName" => "T", "n" => 40, "nClusters" => 4, "largestClusterFrac" => 0.4),
                    Dict("uID" => "a", "valueName" => "B", "n" => 20, "nClusters" => 3, "largestClusterFrac" => 0.5)]))
            end
            p1 = joinpath(qcdir, "run1.json"); mkqc(p1)
            Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p1; unit = "tracks", suffix = "movement")
            p2 = joinpath(qcdir, "run2.json"); mkqc(p2)
            Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p2; unit = "tracks", suffix = "test")
            # BOTH runs retained under composite {labelSet}.{suffix} keys — "test" did NOT overwrite "movement"
            dmov = read_qc(img, "clustTracks.cluster", "T.movement")
            dtst = read_qc(img, "clustTracks.cluster", "T.test")
            @test dmov !== nothing && dtst !== nothing
            @test dmov["metrics"]["nTracks"] == 40 && dmov["runSuffix"] == "movement" && dmov["labelSet"] == "T"
            @test dtst["runSuffix"] == "test"
            # empty suffix ⇒ bank under the bare label set (no trailing dot)
            p3 = joinpath(qcdir, "run3.json"); mkqc(p3)
            Cecelia.write_cluster_qc!([img], "clustPops.cluster", p3; unit = "cells", suffix = "")
            @test read_qc(img, "clustPops.cluster", "T") !== nothing

            set = CciaSet(; dir = mktempdir()); push!(set._images, img); push!(set.image_uids, "a")
            # cohort discovers all (label set × run) value_names
            @test cohort_value_names(set, "clustTracks.cluster") == ["B.movement", "B.test", "T.movement", "T.test"]
            # cohort_runs groups by run (segment/tracking funs bank no run → [])
            crs = cohort_runs(set, "clustTracks.cluster")
            @test Set(r.run for r in crs) == Set(["movement", "test"])
            @test sort(first(r.valueNames for r in crs if r.run == "test")) == ["B.test", "T.test"]  # each run carries its value_names
            @test isempty(cohort_runs(set, "segment.cellpose"))
            # run filter: cohort_qc_for_all!(run="test") persists ONLY the test run's value_names
            allw = cohort_qc_for_all!(set, "clustTracks.cluster"; run = "test")
            @test Set(keys(allw)) == Set(["B.test", "T.test"])
            @test occursin("(T.test)", join(cohort_qc_summary_lines(allw["T.test"])))  # run named in the lab-log line
        end

        @testset "register_cohort_metrics! (custom-module opt-in)" begin
            fun = "customExamples.qcProbeTest"
            @test !haskey(COHORT_METRICS, fun)                       # unknown → cohort errors
            register_cohort_metrics!(fun, ["nCells"])
            @test COHORT_METRICS[fun] == ["nCells"]                  # now a known producer
            register_cohort_metrics!(fun, ["nCells", "nClusters"])   # idempotent overwrite
            @test COHORT_METRICS[fun] == ["nCells", "nClusters"]
            delete!(COHORT_METRICS, fun)                             # don't leak into other testsets
        end

        @testset "analysis lineage (Slice A synthesizer)" begin
            proj = CciaProject(; uid = "linP", name = "lineage"); proj.root = mktempdir()
            s = CciaSet(; uid = "linS", dir = mktempdir())
            push!(proj._sets, s); push!(proj.set_uids, s.uid)

            # i1 — full pipeline: import → segment(A,B) → track(A) → cluster(A/movement); A gated (flow)
            i1 = CciaImage(; uid = "i1", dir = mktempdir())
            i1.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad")
            lp1 = img_label_props_dir(i1); mkpath(lp1)
            touch(img_track_props_path(i1, "A"))                       # A is tracked
            open(joinpath(lp1, "A__tracks.clustfeatures.json"), "w") do f
                JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["i1"])))
            end
            g1 = PopulationMap(; pop_type = "flow", value_name = "A")
            add_pop!(g1, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
            save_pop_map!(g1, i1)
            append_run_log!(i1, "importImages.omezarr", "default", "done")
            append_run_log!(i1, "segment.cellpose", "A", "done")
            append_run_log!(i1, "segment.cellpose", "B", "done")
            append_run_log!(i1, "tracking.bayesian_tracking", "A", "done")
            append_run_log!(i1, "clustTracks.cluster", "movement", "done")
            push!(s._images, i1); push!(s.image_uids, i1.uid)

            # i2 — partial + excluded: import → segment(A, failed) only
            i2 = CciaImage(; uid = "i2", dir = mktempdir()); i2.included = false
            i2.label_props = Dict("A" => "A.h5ad")
            append_run_log!(i2, "importImages.omezarr", "default", "done")
            append_run_log!(i2, "segment.cellpose", "A", "failed")
            push!(s._images, i2); push!(s.image_uids, i2.uid)

            # a wired chain + board tabs (project-level)
            save_chain_template!(proj, ChainTemplate("pipeline",
                [ChainNode(; id = "n1", fn = "segment.cellpose"),
                 ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")], ChainEdge[]))
            mkpath(joinpath(proj.root, "settings"))
            open(joinpath(proj.root, "settings", "analysisBoards.json"), "w") do io
                JSON3.write(io, Dict("tabs" => [Dict("name" => "Behaviour"), Dict("name" => "Counts")]))
            end

            lin = analysis_lineage(proj)
            @test lin.projectUid == "linP" && length(lin.images) == 2
            e1 = lin.images[findfirst(e -> e.uid == "i1", lin.images)]
            e2 = lin.images[findfirst(e -> e.uid == "i2", lin.images)]
            # i1 ordered steps + stage mapping, last step's value_name is the run suffix
            @test [st.stage for st in e1.steps] == ["import", "segment", "segment", "track", "cluster"]
            @test e1.steps[end].fun == "clustTracks.cluster" && e1.steps[end].valueName == "movement"
            @test e1.segmentations == ["A", "B"] && e1.tracked == ["A"]
            @test length(e1.clusterRuns) == 1 && e1.clusterRuns[1].suffix == "movement" &&
                  e1.clusterRuns[1].valueNames == ["A"]
            @test length(e1.gatedPops) == 1 && e1.gatedPops[1].valueName == "A" && "/CD3" in e1.gatedPops[1].pops
            # i2 partial + excluded + a failed step is surfaced
            @test e2.included == false && isempty(e2.tracked) && isempty(e2.clusterRuns)
            @test any(st -> st.fun == "segment.cellpose" && st.status == "failed", e2.steps)
            # project-level chains + boards
            @test length(lin.chains) == 1 && lin.chains[1].name == "pipeline"
            @test Set(lin.chains[1].tasks) == Set(["segment.cellpose", "tracking.bayesian_tracking"])
            @test lin.boards == ["Behaviour", "Counts"]
            # rollup: pipeline unions run-log steps AND artifact evidence, so i1's gated pop adds a
            # "gate" stage even though gating isn't a task step. i2 diverges (excluded + missing the
            # track/gate/cluster stages the others reached).
            @test lin.rollup.pipeline == ["import", "segment", "track", "gate", "cluster"]
            dv = lin.rollup.divergences[findfirst(d -> d.uid == "i2", lin.rollup.divergences)]
            @test dv.included == false && Set(dv.missingStages) == Set(["track", "gate", "cluster"])
            # artifact-aware stages: a segmentation/track with NO run-log step still counts as reached
            # (it predates the capped run-log window) — the fix for false "missing segment" divergences
            noStep = (; uid = "x", name = "X", included = true, steps = NamedTuple[],
                        segmentations = ["A"], tracked = ["A"], clusterRuns = Any[], gatedPops = Any[])
            @test Set(Cecelia._image_stages(noStep)) == Set(["segment", "track"])
            # scoping: one image, one set, unknown → empty
            @test length(analysis_lineage(proj; image_uid = "i1").images) == 1
            @test length(analysis_lineage(proj; set_uid = "linS").images) == 2
            @test isempty(analysis_lineage(proj; image_uid = "nope").images)
        end

        @testset "populations summary (Slice B)" begin
            proj = CciaProject(; uid = "popP", name = "pops"); proj.root = mktempdir()
            s = CciaSet(; uid = "popS", dir = mktempdir())
            push!(proj._sets, s); push!(proj.set_uids, s.uid)
            img = CciaImage(; uid = "i1", dir = mktempdir())
            img.label_props = Dict("A" => "A.h5ad")
            # a flow gate on A (CD3) + a cluster pop filtering clusters.movement
            mf = PopulationMap(; pop_type = "flow", value_name = "A")
            add_pop!(mf, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
            save_pop_map!(mf, img)
            mc = PopulationMap(; pop_type = "trackclust", value_name = "A")
            add_pop!(mc, "Directed"; filter_measure = "clusters.movement", filter_fun = "in", filter_values = [3])
            save_pop_map!(mc, img)
            push!(s._images, img); push!(s.image_uids, img.uid)

            out = populations_summary(proj)
            @test out.projectUid == "popP" && length(out.images) == 1
            pops = out.images[1].populations
            @test out.images[1].truncated == false
            cd3 = pops[findfirst(p -> p.name == "CD3", pops)]
            @test cd3.popType == "flow" && cd3.valueName == "A" && cd3.filter === nothing
            @test cd3.gate !== nothing && cd3.gate["kind"] == "rectangle" &&
                  cd3.gate["x_channel"] == "c1" && cd3.gate["y_channel"] == "c2"
            dir = pops[findfirst(p -> p.name == "Directed", pops)]
            @test dir.popType == "trackclust" && dir.gate === nothing
            @test dir.filter.measure == "clusters.movement" && dir.filter.fun == "in" &&
                  collect(dir.filter.values) == [3]
            # scoping mirrors lineage
            @test length(populations_summary(proj; image_uid = "i1").images) == 1
            @test isempty(populations_summary(proj; image_uid = "nope").images)
        end

        @testset "measure summary (Slice C)" begin
            # pure summary logic (always runs): median/quantiles/mean over finite values, NaN/missing dropped
            s = Cecelia._summarise_measure("x", Any[1.0, 2.0, 3.0, NaN, missing])
            @test s.n == 3 && s.median == 2.0 && s.q25 <= 2.0 <= s.q75    # mean dropped (payload trim)
            @test !hasproperty(s, :mean)
            @test Cecelia._summarise_measure("y", Any[NaN, missing]) === nothing

            # integration over the real KDIeEm B fixture: UNGATED image → the base fallback (all-cells
            # phenotype + tracked motility). The gated path (T/_qc) is validated separately off-suite.
            h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
            trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
            if !have_fixture(h5) || !have_fixture(trk)
                @test_skip "measure summary (fixture missing)"
            else
                td = mktempdir(); mkpath(joinpath(td, "labelProps"))
                cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
                cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
                img = CciaImage(uid = "KDIeEm", dir = td)
                img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
                proj = CciaProject(; uid = "mP", name = "m"); proj.root = mktempdir()
                st = CciaSet(; uid = "mS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
                push!(st._images, img); push!(st.image_uids, img.uid)

                out = measure_summary(proj)
                @test length(out.images) == 1
                summ = out.images[1].summaries
                @test !isempty(summ)
                # motility over the tracked base (no channel-name dependence) — the robust anchor
                mi = findfirst(x -> x.kind == "motility", summ)
                @test mi !== nothing
                moti = summ[mi]
                @test moti.n > 0 && any(m -> m.name == "live.track.speed", moti.measures)
                @test all(m -> isfinite(m.median) && m.n > 0, moti.measures)
                # phenotype over all cells: more rows than tracks (cells collapse to tracks)
                pi = findfirst(x -> x.kind == "phenotype", summ)
                @test pi !== nothing && summ[pi].n > moti.n && !isempty(summ[pi].measures)
            end
        end

        @testset "behaviour + cluster summary (Slice D)" begin
            # pure category-distribution logic (always runs): fractions, distinct count, cap, null-drop
            d = Cecelia._category_distribution(Any[1.0, 1.0, 1.0, 2.0, NaN, missing, nothing])
            @test d.n == 4 && d.nDistinct == 2
            @test d.top[1].value == "1.0" && d.top[1].n == 3 && d.top[1].fraction == 0.75
            @test d.top[2].value == "2.0" && d.top[2].n == 1
            big = Cecelia._category_distribution(collect(1:100); cap = 5)
            @test big.nDistinct == 100 && length(big.top) == 5   # capped, but distinct count is the true total
            @test Cecelia._category_distribution(Any[NaN, missing, nothing]).n == 0

            # integration over the real KDIeEm B fixture: the summaries run and return the right shape
            # (behaviour/cluster entries only if the fixture banked HMM/cluster obs — asserted when present)
            h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
            if !have_fixture(h5)
                @test_skip "behaviour/cluster summary (fixture missing)"
            else
                td = mktempdir(); mkpath(joinpath(td, "labelProps"))
                cp(h5, joinpath(td, "labelProps", "B.h5ad"))
                img = CciaImage(uid = "KDIeEm", dir = td)
                img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
                proj = CciaProject(; uid = "bP", name = "b"); proj.root = mktempdir()
                st = CciaSet(; uid = "bS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
                push!(st._images, img); push!(st.image_uids, img.uid)

                b = behaviour_summary(proj); c = cluster_summary(proj)
                @test length(b.images) == 1 && b.images[1].behaviour isa AbstractVector
                @test length(c.images) == 1 && c.images[1].clusters isa AbstractVector
                @test haskey(c, :featuresByRun)   # feature lists hoisted out of per-image entries
                # every behaviour entry is a well-formed distribution
                for e in b.images[1].behaviour
                    @test e.kind in ("state", "transitions") && e.n > 0 && !isempty(e.distribution)
                    @test all(x -> 0.0 <= x.fraction <= 1.0, e.distribution)
                end
            end
        end

        @testset "chains summary (Slice E)" begin
            proj = CciaProject(; uid = "chP", name = "ch"); proj.root = mktempdir()
            save_chain_template!(proj, ChainTemplate("pipe",
                [ChainNode(; id = "n1", fn = "segment.cellpose"),
                 ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")],
                [ChainEdge("n1", "n2")]))
            c = chains_summary(proj)
            @test c.projectUid == "chP" && length(c.templates) == 1
            t = c.templates[1]
            @test t.name == "pipe" && length(t.nodes) == 2 && length(t.edges) == 1
            @test t.nodes[1].fun == "segment.cellpose" && t.nodes[1].scope == "image"   # per-task default
            @test t.edges[1].from == "n1" && t.edges[1].to == "n2"
            @test c.runs isa AbstractVector && isempty(c.runs)   # no runs recorded on disk
        end
    end

end
