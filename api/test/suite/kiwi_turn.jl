# ── Kiwi turn — schema, "seen this turn", validation, re-ask (KIWI_ASSISTANT_PLAN Phase 3) ──────────
# Pure pieces first, then `run_kiwi_turn` driven by a SCRIPTED fake engine (the Phase 1 contract) over
# the testpr fixture — so the loop's behaviour is pinned without spawning a model. Live turns are the
# eval harness's job, not CI's.

# A fake engine: returns `script` results in order and records each call's prompt + session id.
struct _KiwiFakeEngine <: Cecelia.AgentBackend
    script::Vector{Cecelia.AgentResult}
    calls::Vector{Tuple{String,String}}
end
Cecelia.agent_available(::_KiwiFakeEngine) = true
Cecelia.agent_capabilities(::_KiwiFakeEngine) = (; native_schema = true, native_mcp = true, resumable = true)
function Cecelia._run_agent_once(a::_KiwiFakeEngine, prompt, cfg; session_id, kw...)
    push!(a.calls, (String(prompt), String(session_id)))
    popfirst!(a.script)
end
_kiwi_fake_reply(reply; seen = String[], sid = "s1") =
    Cecelia.AgentResult(true, "", 10, 20, sid, "", JSON3.read(JSON3.write(reply)), seen)

@testset "Kiwi turn — reply schema + seen-this-turn (pure)" begin
    s = JSON3.read(kiwi_reply_schema())
    @test Set(String.(s.required)) == Set(["abstain", "claims"]) && !haskey(s.properties, :reasoning)
    @test s.properties.claims.items.properties.refs.minItems == 1
    @test s.properties.claims.maxItems == KIWI_MAX_CLAIMS
    @test s.properties.note.maxLength == KIWI_NOTE_MAX_CHARS && !("note" in String.(s.required))   # optional
    # the app renders the "I think" flag; a model that writes it too is trimmed, not doubled
    @test kiwi_claim_text("interpretation", "I think no single measure is best") == "No single measure is best"
    @test kiwi_claim_text("interpretation", "i think that, B is slower") == "B is slower"
    @test kiwi_claim_text("observation", " I think is a word ") == "I think is a word"
    @test String(s.properties.claims.items.properties.refs.items[Symbol("\$ref")]) == "#/definitions/kiwiRef"
    # the ref definitions are the shared schema's, embedded — every kind present
    @test Set(split(String(b[Symbol("\$ref")]), '/')[end] for b in s.definitions.kiwiRef.oneOf) == Set(KIWI_REF_KINDS)
    @test Set(String.(s.properties.claims.items.properties.kind.enum)) == Set(KIWI_CLAIM_KINDS)
    r = JSON3.read(kiwi_reply_schema(; reasoning = true))
    @test String(first(r.required)) == "reasoning" && haskey(r.properties, :reasoning)
    # the tool allow-list is read-only: no write, mark or pairing tool
    @test !any(t -> occursin(r"append|create|revise|set_|mark_|point_at|register|delete", t), KIWI_READ_TOOLS)
    @test all(t -> startswith(t, "mcp__" * OBSERVER_MCP_NAME * "__"), _kiwi_allowed_tools())
    # every observer tool is decided: allowed or excluded with a reason — a new tool fails here
    server = read(joinpath(@__DIR__, "..", "..", "..", "mcp", "cecelia_mcp", "server.py"), String)
    tools = Set(m.captures[1] for m in eachmatch(r"@mcp\.tool\([^)]*\)\s*\ndef ([a-z_]+)\(", server))
    @test length(tools) > 40
    @test tools == union(Set(KIWI_READ_TOOLS), keys(KIWI_EXCLUDED_TOOLS))
    @test isempty(intersect(Set(KIWI_READ_TOOLS), keys(KIWI_EXCLUDED_TOOLS)))

    seen = """{"uid":"KDIeEm","trackIds":[12, 40],"pops":["/gated/Directed"],"note":"label 123"}"""
    @test kiwi_ref_seen(Dict("kind" => "image", "imageUid" => "KDIeEm"), seen, [])
    @test !kiwi_ref_seen(Dict("kind" => "image", "imageUid" => "zzzzzz"), seen, [])
    @test kiwi_ref_seen(Dict("kind" => "tracks", "imageUid" => "KDIeEm", "valueName" => "B", "trackIds" => [12, 40]), seen, [])
    # a whole-number match: 12 is seen, 1 and 23 are not (they only occur inside 12 / 123)
    @test !kiwi_ref_seen(Dict("kind" => "cells", "imageUid" => "KDIeEm", "valueName" => "B", "labelIds" => [1]), seen, [])
    @test !kiwi_ref_seen(Dict("kind" => "cells", "imageUid" => "KDIeEm", "valueName" => "B", "labelIds" => [23]), seen, [])
    @test kiwi_ref_seen(Dict("kind" => "population", "imageUid" => "KDIeEm", "valueName" => "B", "popPath" => "/Directed"), seen, [])
    @test kiwi_ref_seen(Dict("kind" => "project"), "", [])
    # attached refs count as seen whatever the text — key order and Symbol keys don't matter
    att = [Dict("imageUid" => "QQQQQQ", "kind" => "image")]
    @test kiwi_ref_seen(JSON3.read("""{"kind":"image","imageUid":"QQQQQQ"}"""), "", att)
end

@testset "Kiwi turn — claim shape: one fact, cited precisely (pure)" begin
    # one fact — real single-fact claims from the 2026-09-23 live run pass
    for t in ("Set obWDNS (\"MERTK crop\") contains 5 images.",
              "Total clustered B tracks differ between the two images: 17 in 3w4IY5 vs 25 in EaMaVq.",
              "/Directed (B) holds 1,449 cells.", "Is track 12 in the same population at t=40?",
              "Tcells-uGFP median speed is 3.2 µm/min, e.g. higher than in /qc.")
        @test kiwi_claim_bundling(t) == ""
    end
    # bundled — real multi-fact claims from the same run each fail, with the reason named
    # a comparison is ONE fact — the first real turns' claims, which the old dash / 160-char rules split
    for t in ("In M1a_005, T tracks move faster than B: median speed 7.00 vs 3.06 µm/min (n=18, n=21) — little overlap.",
              "Speed separates B from T in 5 of 7 images, but reverses in M2b (B 4.70 vs T 2.50 µm/min).")
        @test kiwi_claim_bundling(t) == ""
    end
    @test occursin("parentheses", kiwi_claim_bundling("Pops exist (13 on flowTom, 2 gated on default, one on cpSAM2)."))
    @test occursin("list", kiwi_claim_bundling("It has 13 pops on flowTom, 2 pops on default, and one pop on cpSAM2."))
    # …but a list of NAMES is one fact — the sanity run's three post-re-ask failures, all misfires
    for t in ("fXgbTl has per-track tables for 6 value names: coastalFg, coastalSm15, cpSAM2, default, flowTom, memTom.",
              "The four clustering run suffixes (movement, test, here, there) report identical Directed track counts.",
              "The lineage's segmentations, tracked, and clusterRuns lists are all empty for this image.",
              "Median extent in /Directed is 0.58, the lowest of four (/qc 0.63, /Scanning 0.66, /Meandering 0.63).",
              "Of fXgbTl's segmentations, 6 are tracked: coastalFg, coastalSm15, cpSAM2, default, flowTom, memTom.")
        @test kiwi_claim_bundling(t) == ""
    end
    @test occursin("semicolon", kiwi_claim_bundling("17 tracks in A; 25 in B."))
    @test occursin("sentence", kiwi_claim_bundling("A has 17 tracks. B has 25."))
    @test occursin("longer", kiwi_claim_bundling(repeat("x", KIWI_CLAIM_MAX_CHARS + 1)))

    img = Dict("kind" => "image", "imageUid" => "3w4IY5")
    pop(p) = Dict("kind" => "population", "imageUid" => "3w4IY5", "valueName" => "B", "popPath" => p)
    # a named population needs its population ref — the image alone is too coarse
    @test length(kiwi_claim_underspecified("/Directed holds 205 cells.", [img])) == 1
    @test isempty(kiwi_claim_underspecified("/Directed holds 205 cells.", [pop("/Directed")]))
    @test isempty(kiwi_claim_underspecified("/Directed holds 205 cells.", [pop("/tracked/Directed")]))   # leaf form
    @test length(kiwi_claim_underspecified("/Directed is larger than /qc.", [pop("/Directed")])) == 1
    # tracks / cells by number
    trk = Dict("kind" => "tracks", "imageUid" => "3w4IY5", "valueName" => "B", "trackIds" => [12, 40])
    @test isempty(kiwi_claim_underspecified("Track 12 leaves the field at t=40.", [trk]))
    @test only(kiwi_claim_underspecified("Track 13 leaves the field.", [trk])) == "track 13 (cite it as a tracks ref)"
    @test length(kiwi_claim_underspecified("Cell #7 is dim.", [img])) == 1
    # not objects: file paths (an image's oriPath), units, fractions, counts before the noun, uids starting with a digit
    @test isempty(kiwi_claim_underspecified("Speed is 3.2 µm/min in 1/3 of the 17 tracks of 3w4IY5, n/a for cells 3w4IY5.", [img]))
    @test isempty(kiwi_claim_underspecified("Both have oriPath /home/d/Downloads/x-crop.tif and /q/r/s.ome.tiff.", [img]))

    # both feed the validator's errors — alongside the ref's own (this project doesn't exist, so the
    # project ref fails too; only the shape errors are asserted)
    reply = JSON3.read(JSON3.write(Dict("abstain" => false, "claims" => [Dict("kind" => "observation",
                "text" => "/Directed has 5 tracks; /qc has 12.", "refs" => [Dict("kind" => "project")])])))
    _, errs = kiwi_validate_reply("no-such-project-zz", reply, "", [])
    @test any(e -> occursin("more than one fact", e), errs)
    @test count(e -> occursin("cite it as a population ref", e), errs) == 2
end

@testset "Kiwi turn — validation + re-ask against the testpr fixture" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        img_ref = Dict("kind" => "image", "imageUid" => "KDIeEm")
        tool_saw_image = ["""{"uid":"KDIeEm","name":"fixture"}"""]
        good = Dict("abstain" => false, "claims" => [Dict("kind" => "observation", "text" => "One image.", "refs" => [img_ref])])

        # validation, directly
        claims, errs = kiwi_validate_reply("testpr", JSON3.read(JSON3.write(good)), join(tool_saw_image), [])
        @test isempty(errs) && claims[1]["refs"][1]["seen"] && claims[1]["refs"][1]["result"]["check"] == "exists"
        # real but not seen this turn (Decision 7)
        _, errs = kiwi_validate_reply("testpr", JSON3.read(JSON3.write(good)), "", [])
        @test length(errs) == 1 && occursin("not in any tool result", errs[1])
        # seen but not real (Decision 6) — the resolver's error is carried into the re-ask text
        bad = Dict("abstain" => false, "claims" => [Dict("kind" => "observation", "text" => "x",
                    "refs" => [Dict("kind" => "image", "imageUid" => "zzzzzz")])])
        _, errs = kiwi_validate_reply("testpr", JSON3.read(JSON3.write(bad)), "zzzzzz", [])
        @test length(errs) == 1 && occursin("no image zzzzzz", errs[1])
        # no claims without abstaining is a failure; abstaining with none is a reply (Decision 9)
        @test !isempty(last(kiwi_validate_reply("testpr", JSON3.read("""{"abstain":false,"claims":[]}"""), "", [])))
        @test isempty(last(kiwi_validate_reply("testpr", JSON3.read("""{"abstain":true,"claims":[]}"""), "", [])))

        cfg = "/tmp/unused-mcp.json"
        # 1. clean first reply → no re-ask
        f = _KiwiFakeEngine([_kiwi_fake_reply(good; seen = tool_saw_image)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "what images?"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && !out["reasked"] && length(f.calls) == 1 && out["toolCalls"] == 1
        @test out["claims"][1]["refs"][1]["result"]["label"] != ""

        # 2. first reply cites an unseen ref → ONE re-ask on the same session naming it → fixed
        f = _KiwiFakeEngine([_kiwi_fake_reply(good; seen = String[], sid = "sessA"),
                             _kiwi_fake_reply(good; seen = tool_saw_image, sid = "sessA")], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "what images?"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && out["reasked"] && length(f.calls) == 2
        @test f.calls[2][2] == "sessA"                                  # resumed the first attempt's session
        @test occursin("KDIeEm", f.calls[2][1]) && occursin("failed validation", f.calls[2][1])
        @test out["usage"]["input"] == 20                               # both attempts counted
        @test length(out["reaskErrors"]) == 1 && occursin("KDIeEm", out["reaskErrors"][1])

        # 2b. a bundled claim is re-asked too, with the split instruction; the split reply passes
        bundled = Dict("abstain" => false, "claims" => [Dict("kind" => "observation",
                       "text" => "One image; it has no tracks.", "refs" => [img_ref])])
        f = _KiwiFakeEngine([_kiwi_fake_reply(bundled; seen = tool_saw_image),
                             _kiwi_fake_reply(good; seen = tool_saw_image)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "what images?"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && out["reasked"] && occursin("more than one fact", only(out["reaskErrors"]))
        @test occursin("Split a claim", f.calls[2][1])

        # 2c. a shape problem that survives the re-ask is kept apart and does NOT fail the turn: it is
        #     how Kiwi wrote, not what it cited ("4 claims didn't check out — what does this tell me?")
        f = _KiwiFakeEngine([_kiwi_fake_reply(bundled; seen = tool_saw_image),
                             _kiwi_fake_reply(bundled; seen = tool_saw_image)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "what images?"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && isempty(out["errors"]) && occursin("more than one fact", only(out["shapeErrors"]))

        # 3. still wrong after the re-ask → not ok, errors kept, no third attempt
        f = _KiwiFakeEngine([_kiwi_fake_reply(bad), _kiwi_fake_reply(bad)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "q"; agent = f, mcp_config_path = cfg)
        @test !out["ok"] && out["reasked"] && length(f.calls) == 2 && !isempty(out["errors"])

        # 4. an attached ref is cited as-is without any tool call, and the pack names it in the prompt
        f = _KiwiFakeEngine([_kiwi_fake_reply(good)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "tell me about this"; refs = [img_ref], agent = f, mcp_config_path = cfg)
        @test out["ok"] && occursin("Attached by the user", f.calls[1][1]) && occursin("KDIeEm", f.calls[1][1])

        # 4b. an attached LIVE ref keeps its ask-time result: a plot open when asked, closed before the
        #     reply was checked (4kS67f, 2026-09-24), is not a failed claim
        pref = Dict("kind" => "plot", "plotId" => "kiwi-turn-plot")
        lock(_PLOTS_LOCK) do
            get!(_PLOTS_BY_PROJECT, "testpr", Dict{String,PlotEntry}())["kiwi-turn-plot"] =
                PlotEntry("kiwi-turn-plot", "c1", "summary", "Track measures", "/analysis", String[], nothing,
                          Dict{String,Any}("series" => ["B/qc"]), time(), "testpr",
                          "measure: live.track.speed · chart: boxplot\nB/qc | M1a (LUkCpP) | - | n=21 median=3.06 q1=2.10 q3=4.17")
        end
        on_plot = Dict("abstain" => false, "claims" => [Dict("kind" => "observation", "text" => "The plot has one series.", "refs" => [pref])])
        closing = _KiwiFakeEngine([_kiwi_fake_reply(on_plot)], Tuple{String,String}[])
        close_it(_) = lock(_PLOTS_LOCK) do; delete!(_PLOTS_BY_PROJECT, "testpr") end
        out = run_kiwi_turn("testpr", "what does this show?"; refs = [pref], agent = closing, mcp_config_path = cfg,
                            on_progress = s -> s == "checking refs" && close_it(s))
        @test out["ok"] && out["claims"][1]["refs"][1]["result"]["label"] == "Track measures"
        @test occursin("(B/qc)", closing.calls[1][1])                   # the pack carries what the plot is…
        @test occursin("What this plot shows", closing.calls[1][1])      # …and the numbers it draws
        @test occursin("    B/qc | M1a (LUkCpP) | - | n=21 median=3.06", closing.calls[1][1])

        # 4b'. a follow-up cites a plot attached EARLIER, now closed: its earlier result stands, and the
        #      pack tells the engine it may
        prior_ok = Dict(_kiwi_canon(pref) => Dict{String,Any}("ok" => true, "check" => "live", "label" => "Track measures", "error" => ""))
        f = _KiwiFakeEngine([_kiwi_fake_reply(on_plot)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "can you reference the plots?"; agent = f, mcp_config_path = cfg,
                            prior_refs = [pref], prior_results = prior_ok)
        @test out["ok"] && occursin("Earlier in this conversation", f.calls[1][1]) && occursin("Track measures", f.calls[1][1])

        # 4c. too many claims is a re-ask
        many = Dict("abstain" => false, "claims" => [Dict("kind" => "observation", "text" => "One image.", "refs" => [img_ref])
                                                    for _ in 1:(KIWI_MAX_CLAIMS + 1)])
        _, errs = kiwi_validate_reply("testpr", JSON3.read(JSON3.write(many)), join(tool_saw_image), [])
        @test only(errs) |> e -> occursin("at most $KIWI_MAX_CLAIMS", e)

        # 4d. a note — what Kiwi couldn't do — comes back as its own line, not as a claim
        f = _KiwiFakeEngine([_kiwi_fake_reply(merge(good, Dict("note" => "  I couldn't open the board.  ")); seen = tool_saw_image)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "q"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && out["note"] == "I couldn't open the board." && length(out["claims"]) == 1

        # 5. an abstaining reply is ok
        f = _KiwiFakeEngine([_kiwi_fake_reply(Dict("abstain" => true, "claims" => []))], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "q"; agent = f, mcp_config_path = cfg)
        @test out["ok"] && out["abstain"] && isempty(out["claims"])

        # 6. an engine failure is reported, not thrown
        f = _KiwiFakeEngine([Cecelia.AgentResult(false, "", 0, 0, "", "boom")], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "q"; agent = f, mcp_config_path = cfg)
        @test !out["ok"] && out["errors"] == ["boom"]
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

@testset "Kiwi API — background turn, kept replies, one at a time (fake engine)" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old, old_agent = Cecelia.cecelia_conf()["dirs"]["projects"], _KIWI_AGENT[]
    img_ref = Dict("kind" => "image", "imageUid" => "KDIeEm")
    good = Dict("abstain" => false, "claims" => [Dict("kind" => "observation", "text" => "One image.", "refs" => [img_ref])])
    # every turn gets a fresh one-reply script; the model name it was built with is recorded
    built = String[]
    wait_idle(puid) = (t0 = time(); while haskey(_KIWI_RUNNING, puid) && time() - t0 < 20; sleep(0.05); end)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        _KIWI_AGENT[] = m -> (push!(built, m); _KiwiFakeEngine([_kiwi_fake_reply(good; seen = ["""{"uid":"KDIeEm"}"""])], Tuple{String,String}[]))

        st, rec = kiwi_start_turn("testpr", "what images?"; refs = [img_ref], model = "haiku")
        @test st == 200 && rec["status"] == "running" && startswith(rec["turnId"], "kt-")
        @test rec["refs"][1]["result"]["ok"]                     # attached refs resolved up front, for the chips
        @test built == ["haiku"]
        wait_idle("testpr")
        @test !haskey(_KIWI_RUNNING, "testpr")
        @test rec["status"] == "done" && rec["reply"]["ok"] && "checking refs" in rec["steps"]
        # kept on disk and served back
        st, body = api_kiwi_turns(HTTP.Request("GET", "/api/kiwi/turns?projectUid=testpr"))
        turns = JSON3.read(body).turns
        @test st == 200 && length(turns) == 1 && turns[1].turnId == rec["turnId"] && turns[1].status == "done"

        # a follow-up continues the earlier turn's session, and what it cited counts as seen
        engines = _KiwiFakeEngine[]
        _KIWI_AGENT[] = m -> (e = _KiwiFakeEngine([_kiwi_fake_reply(good; sid = "s2")], Tuple{String,String}[]);
                              push!(engines, e); e)
        st, fu = kiwi_start_turn("testpr", "and then?"; follow_up = rec["turnId"])
        wait_idle("testpr")
        @test st == 200 && fu["followUp"] == rec["turnId"] && length(fu["priorRefs"]) == 1
        @test only(engines).calls[1][2] == "s1"                          # resumed, not fresh
        @test fu["status"] == "done"                                     # cited KDIeEm with no tool call this turn
        @test first(kiwi_start_turn("testpr", "q"; follow_up = "kt-nope")) == 400
        _KIWI_AGENT[] = m -> (push!(built, m); _KiwiFakeEngine([_kiwi_fake_reply(good; seen = ["""{"uid":"KDIeEm"}"""])], Tuple{String,String}[]))

        # validation: unknown project, empty ask, unknown model coerced to the allow-list
        @test first(kiwi_start_turn("nope", "q")) == 404
        @test first(kiwi_start_turn("testpr", "  ")) == 400
        st, rec2 = kiwi_start_turn("testpr", "q"; model = "gpt-9"); wait_idle("testpr")
        @test st == 200 && built[end] in Cecelia.OBSERVER_MODELS

        # one turn per project: a second start while one runs is refused
        _KIWI_RUNNING["testpr"] = Dict{String,Any}("turnId" => "kt-busy")
        @test first(kiwi_start_turn("testpr", "q")) == 409
        @test first(api_kiwi_turn_cancel(Vector{UInt8}("""{"turnId":"kt-other"}"""))) == 404
        @test first(api_kiwi_turn_cancel(Vector{UInt8}("""{"turnId":"kt-busy"}"""))) == 200
        delete!(_KIWI_RUNNING, "testpr")

        # an engine that isn't installed is refused before anything runs
        _KIWI_AGENT[] = m -> Cecelia.ClaudeAgent(; bin = "/no/such/claude-cli")
        st, out = kiwi_start_turn("testpr", "q")
        @test st == 503 && occursin("Claude", out["error"])

        # "plot this": the first click adds ONE board named after the plot, the second opens it
        pp = Dict("kind" => "proposedPlot", "plot" => "track_measures", "measure" => "live.track.duration")
        st, out = kiwi_open_proposed_plot("testpr", pp)
        @test st == 200 && out["created"] && startswith(out["board"], "Kiwi · ") && occursin("live.track.duration", out["board"])
        st, again = kiwi_open_proposed_plot("testpr", pp)
        @test st == 200 && !again["created"] && again["board"] == out["board"]
        @test count(b -> startswith(b["name"], "Kiwi · "), board_summaries(load_project("testpr"))) == 1
        # …and a proposal that is now on a board says where, so it isn't passed off as new
        @test resolve_kiwi_ref("testpr", pp)["detail"] == "already on board “$(out["board"])”"
        # a different measure is a different plot; a bad one is refused before anything is written
        @test kiwi_open_proposed_plot("testpr", merge(pp, Dict("measure" => "live.track.speed")))[2]["created"]
        st, bad = kiwi_open_proposed_plot("testpr", merge(pp, Dict("measure" => "live.cell.nope")))
        @test st == 422 && occursin("does not carry measure", bad["error"])
        @test first(kiwi_open_proposed_plot("testpr", Dict("kind" => "image", "imageUid" => "KDIeEm"))) == 400
        # the slot matcher: the spec's default measure counts; named pops and grouping must be there
        slot = Dict("kind" => "summary", "ref" => "track_measures", "measure" => "live.track.speed",
                    "pops" => ["B/qc/_tracked", "T/qc/_tracked"], "groupBy" => "hmm")
        @test kiwi_slot_holds(slot, Dict("kind" => "proposedPlot", "plot" => "track_measures"))       # default = speed
        @test kiwi_slot_holds(slot, Dict("kind" => "proposedPlot", "plot" => "track_measures", "pops" => ["B/qc/_tracked"]))
        @test !kiwi_slot_holds(slot, Dict("kind" => "proposedPlot", "plot" => "track_measures", "pops" => ["B/other"]))
        @test !kiwi_slot_holds(slot, Dict("kind" => "proposedPlot", "plot" => "track_measures", "groupBy" => "other"))
        @test !kiwi_slot_holds(slot, Dict("kind" => "proposedPlot", "plot" => "cell_measures"))

        # keep only the last KIWI_TURNS_KEEP, then clear
        for i in 1:(KIWI_TURNS_KEEP + 2)
            _kiwi_append_turn!("testpr", Dict{String,Any}("turnId" => "kt-$i"))
        end
        kept = _kiwi_read_turns("testpr")
        @test length(kept) == KIWI_TURNS_KEEP && kept[end]["turnId"] == "kt-$(KIWI_TURNS_KEEP + 2)"
        @test first(api_kiwi_turns_clear(Vector{UInt8}("""{"projectUid":"testpr"}"""))) == 200
        @test isempty(_kiwi_read_turns("testpr"))
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        _KIWI_AGENT[] = old_agent
    end
  end
end

@testset "Kiwi context pack — a set's track measures, compact (pure)" begin
    m(name, med) = (; name, n = 20, median = med, q25 = med - 1, q75 = med + 1)
    s(vn, pop, meds...) = (; population = pop, valueName = vn, kind = "motility", n = 20,
                            measures = [m("num_cells", 8.0), (m("live.track.$k", v) for (k, v) in meds)...])
    im(uid, name, inc, sums...) = (; uid, name, included = inc, summaries = [sums...])
    out = (; projectUid = "P", images = [
        im("LUkCpP", "M1a_005", true, s("B", "/qc", "speed" => 3.5), s("T", "/qc", "speed" => 7.0)),
        im("k3Tx90", "M2b", true, s("B", "/qc", "speed" => 4.7)),
        im("xxEXCL", "excluded", false, s("B", "/qc", "speed" => 99.0))])
    t = kiwi_set_measures_text(out; set_uid = "XcPcu8", value_names = ["B", "T"])
    @test startswith(t, "Track measures for B, T across set XcPcu8")
    @test occursin("  M1a_005 (LUkCpP) | B/qc | n=20 | speed 3.5 [2.5–4.5]", t)
    @test occursin("M2b (k3Tx90) | B/qc", t)
    @test !occursin("num_cells", t) && !occursin("excluded", t)          # counts and excluded images out
    # a plot over named images keeps only those
    @test !occursin("M2b", kiwi_set_measures_text(out; set_uid = "XcPcu8", images = Set(["LUkCpP"])))
    # huge → cut on whole lines, and says so
    big = (; projectUid = "P", images = [im("u$i", "img$i", true, s("B", "/qc", ("m$j" => 1.0 for j in 1:30)...)) for i in 1:200])
    tb = kiwi_set_measures_text(big; set_uid = "S")
    @test length(tb) <= KIWI_SET_MEASURES_MAX_CHARS + 60 && occursin(r"… cut: \d+ more lines$", tb)
    # no set on the plot → nothing, and a set already listed isn't listed twice
    done = Set{String}()
    @test _kiwi_set_measures("testpr", Dict{String,Any}("series" => ["B/qc"]), done) == ""
end
