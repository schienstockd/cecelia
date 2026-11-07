# Claim review (api/src/claim_review.jl): the one model turn of the claim recital, run as a Kiwi turn
# with the analyst's reasoning withheld, its findings written onto the claim entry. A scripted engine
# stands in for the model — no quota spent.

struct _ClaimReviewFake <: Cecelia.AgentBackend
    script::Vector{Cecelia.AgentResult}
    seen_opts::Vector{String}
end
Cecelia.agent_available(::_ClaimReviewFake) = true
Cecelia.agent_capabilities(::_ClaimReviewFake) = (; native_schema = true, native_mcp = true, resumable = true)
function Cecelia._run_agent_once(a::_ClaimReviewFake, prompt, cfg; session_id, kw...)
    push!(a.seen_opts, string(prompt, "\n", cfg, "\n", collect(kw)))
    popfirst!(a.script)
end

@testset "API: claim review" begin
    # the review never sees where the analyst's reasoning lives, and keeps every other read tool
    tools = claim_review_tools()
    @test !any(t -> any(w -> endswith(t, "__" * w), CLAIM_REVIEW_WITHHELD), tools)
    @test any(endswith("__get_task_log"), tools) && any(endswith("__check_claim"), tools)
    @test all(w -> w in KIWI_READ_TOOLS || haskey(KIWI_EXCLUDED_TOOLS, w), CLAIM_REVIEW_WITHHELD)

    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    old_agent = _KIWI_AGENT[]
    post(f, b) = (r = f(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    wait_idle(puid) = (t0 = time(); while haskey(_KIWI_RUNNING, puid) && time() - t0 < 20; sleep(0.05); end)
    try
        proj = create_project!(name = "api-claim-review")
        s    = add_set!(proj; name = "set-A")
        imgs = [add_image!(s; name = nm, meta = Dict{String,Any}("ori_path" => "/tmp/x.tif")) for nm in ("i1", "i2")]
        for (img, m) in zip(imgs, ("1", "2")); img.attr["Mouse"] = m; Cecelia.save!(img); end
        st, r = post(api_claim_state, Dict("projectUid" => proj.uid, "setUid" => s.uid, "populations" => ["B/qc", "T/qc"],
            "measure" => "live.track.speed", "unitAttrs" => ["Mouse"], "text" => "T cells move faster than B cells"))
        id = String(r.entryId)
        md0 = read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String)
        @test occursin("## Model review\n\nNot run.", md0)

        uid1 = imgs[1].uid
        ref = Dict("kind" => "image", "imageUid" => uid1)
        reply = Dict("abstain" => false, "claims" => [
            Dict("kind" => "observation", "text" => "Image i1 has no tracks for\neither population.", "refs" => [ref]),
            Dict("kind" => "interpretation", "text" => "Nothing about speed can be said yet.", "refs" => [ref])])
        fake = _ClaimReviewFake([Cecelia.AgentResult(true, "", 10, 20, "s1", "", JSON3.read(JSON3.write(reply)),
                                                     ["""{"uid":"$uid1"}"""])], String[])
        _KIWI_AGENT[] = m -> fake

        @test post(api_claim_review, Dict("projectUid" => proj.uid, "entryId" => "bb-nope"))[1] == 400
        st, rec = post(api_claim_review, Dict("projectUid" => proj.uid, "entryId" => id))
        @test st == 200 && rec.status == "running" && rec.claimReview.entryId == id
        wait_idle(proj.uid)
        # the prompt names the claim and scope; the withheld tools never reach the engine
        sent = only(fake.seen_opts)
        @test occursin("T cells move faster than B cells", sent) && occursin("Replicate (image attributes; one value combination = one replicate): Mouse", sent)
        # it is told what the rule checks found, so it doesn't spend the turn repeating them
        @test occursin("do NOT repeat", sent) && occursin("- No B and T data on ", sent)
        @test !occursin("read_blackboard_entry", sent) && occursin("get_task_log", sent)

        meta = _read_bb_meta(proj.uid, id)
        rv = meta["claim"]["review"]
        @test rv["status"] == "done" && length(rv["claims"]) == 2
        sid = rv["claims"][1]["sectionId"]
        md = read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String)
        @test occursin("### $sid · Image i1 has no tracks for either population.", md)   # one line
        @test occursin("## What the record would support\n\nNothing about speed can be said yet.", md)
        @test !occursin("Not run.", md) && sid in meta["sectionIds"]
        # the analyst can mark a review finding like any other
        @test api_blackboard_section_outcome(Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid,
            "entryId" => id, "sectionId" => sid, "verdict" => "bad", "note" => "it has tracks"))))[1] == 200
        # a re-check keeps the review; a new round drops it (it answered the earlier question)
        post(api_claim_check, Dict("projectUid" => proj.uid, "entryId" => id))
        @test haskey(_read_bb_meta(proj.uid, id)["claim"], "review")
        post(api_claim_state, Dict("projectUid" => proj.uid, "entryId" => id, "setUid" => s.uid, "populations" => ["B/qc", "T/qc"],
            "measure" => "live.track.speed", "unitAttrs" => ["Mouse"], "text" => "T cells are faster per mouse"))
        @test !haskey(_read_bb_meta(proj.uid, id)["claim"], "review")
        @test occursin("## Model review\n\nNot run.", read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String))
    finally
        _KIWI_AGENT[] = old_agent
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
