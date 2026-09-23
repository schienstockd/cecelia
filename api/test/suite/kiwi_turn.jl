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
    @test occursin("dash", kiwi_claim_bundling("nG1jSi's steps list has two entries — driftCorrect and denoise."))
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

        # 3. still wrong after the re-ask → not ok, errors kept, no third attempt
        f = _KiwiFakeEngine([_kiwi_fake_reply(bad), _kiwi_fake_reply(bad)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "q"; agent = f, mcp_config_path = cfg)
        @test !out["ok"] && out["reasked"] && length(f.calls) == 2 && !isempty(out["errors"])

        # 4. an attached ref is cited as-is without any tool call, and the pack names it in the prompt
        f = _KiwiFakeEngine([_kiwi_fake_reply(good)], Tuple{String,String}[])
        out = run_kiwi_turn("testpr", "tell me about this"; refs = [img_ref], agent = f, mcp_config_path = cfg)
        @test out["ok"] && occursin("Attached by the user", f.calls[1][1]) && occursin("KDIeEm", f.calls[1][1])

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
