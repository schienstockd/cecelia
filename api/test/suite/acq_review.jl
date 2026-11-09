# Acquisition review (api/src/acq_review.jl): the one model turn of the test-acquisition check, run as a
# Kiwi turn with the user's reasoning and the analysis record withheld, its points written onto the set's
# acqCheck entry. A scripted engine stands in for the model — no quota spent.

struct _AcqReviewFake <: Cecelia.AgentBackend
    script::Vector{Cecelia.AgentResult}
    seen_opts::Vector{String}
end
Cecelia.agent_available(::_AcqReviewFake) = true
Cecelia.agent_capabilities(::_AcqReviewFake) = (; native_schema = true, native_mcp = true, resumable = true)
function Cecelia._run_agent_once(a::_AcqReviewFake, prompt, cfg; session_id, kw...)
    push!(a.seen_opts, string(prompt, "\n", cfg, "\n", collect(kw)))
    popfirst!(a.script)
end

@testset "API: acquisition review — allow-list, prompt, points" begin
    # every Kiwi read tool is classified, allowed or withheld with a reason — a read tool added later
    # fails here until someone decides
    @test all(t -> t in KIWI_READ_TOOLS, ACQ_REVIEW_TOOLS)
    @test all(w -> w in KIWI_READ_TOOLS || haskey(KIWI_EXCLUDED_TOOLS, w), keys(ACQ_REVIEW_WITHHELD))
    @test isempty(intersect(Set(ACQ_REVIEW_TOOLS), keys(ACQ_REVIEW_WITHHELD)))
    @test isempty(setdiff(Set(KIWI_READ_TOOLS), union(Set(ACQ_REVIEW_TOOLS), keys(ACQ_REVIEW_WITHHELD))))
    # the record and the acquisition knowledge in; the user's reasoning and the analysis out
    @test "get_acquisition_knowledge" in ACQ_REVIEW_TOOLS && "get_image_info" in ACQ_REVIEW_TOOLS
    @test all(w -> haskey(ACQ_REVIEW_WITHHELD, w), ("read_blackboard_entry", "get_image_notes", "get_session_briefing",
                                                     "get_recent_captures", "read_lab_log", "get_labarchives_context",
                                                     "get_populations", "gate_stats"))

    plan = Dict{String,Any}("designs" => [Dict("id" => "d1", "key" => "Beads · GFP", "label" => "Beads · GFP")],
        "objects" => [Dict("name" => "B cells", "kind" => "cells", "channels" => Dict("d1" => "GFP")),
                      Dict("name" => "Beads", "kind" => "points", "channels" => Dict("d1" => "Beads"))],
        "references" => [Dict("name" => "Collagen", "channels" => Dict("d1" => "SHG"))],
        "measure" => ["uptake"], "compare" => String[], "expectation" => "", "text" => "Measure uptake of B cells")
    p = acq_review_prompt(plan, ["m1 — uidA"], ["Different objective between movies", "Frames don't register (m1)"])
    @test occursin("Plan: “Measure uptake of B cells”", p)
    @test occursin("- B cells — Beads · GFP: GFP", p) && occursin("- Beads (points, counted) — Beads · GFP: Beads", p)
    @test occursin("Reference channels", p) && occursin("Collagen", p) && occursin("Measures: uptake", p)
    @test occursin("- m1 — uidA", p)
    # told what the rules found, so it doesn't spend the turn repeating them
    @test occursin("do NOT repeat", p) && occursin("- Frames don't register (m1)", p)
    @test occursin("ONE sentence of at most $(KIWI_CLAIM_MAX_CHARS) characters, with no semicolon", p)
    @test occursin("Category one of: Movies and channels, Settings, Depth, Photons, Balance, Shared structure, Drift, Fading, Other", p)
    # the closing interpretation is a verdict on the plan as written, weighing the rule findings too
    @test occursin("\"Ready to acquire as planned.\" or the one to three changes", p) && occursin("the rule findings AND your points", p)
    # no lead-in that repeats the heading; the rule warnings first; movies by full name (the app swaps in uids)
    @test occursin("so don't start with a lead-in like \"Change before acquiring:\"", p)
    @test occursin("Start from the rule checks' warnings", p) && occursin("by its full name exactly as listed", p)
    @test occursin("The rule checks found nothing.", acq_review_prompt(plan, String[]))
    @test occursin("before the rest of the experiment is acquired", acq_review_system_prompt("pX")) &&
          occursin("`pX`", acq_review_system_prompt("pX"))
    @test !occursin("expected, NOT a problem", p)
    # an info finding is listed as expected, so the verdict doesn't act on it
    pn = acq_review_prompt(plan, String[], String[], ["Different laser setting between the label designs"])
    @test occursin("expected, NOT a problem — don't act on them:\n- Different laser setting between the label designs", pn)
    # final data: the analysis is the question, the scope is off the table, and the verdict is about analysing
    fin = merge(plan, Dict{String,Any}("stage" => "final"))
    pf = acq_review_prompt(fin, ["m1 — uidA"])
    @test occursin("what should the analysis do about it?", pf) && occursin("→ <what to do in the analysis", pf)
    @test occursin("\"Analyse as planned.\" or the one to three analysis steps", pf) && occursin("These movies can answer:", pf)
    @test occursin("like \"Before comparing:\"", pf)
    @test !occursin("Ready to acquire", pf)
    sf = acq_review_system_prompt("pX", "final")
    @test occursin("FINAL data", sf) && occursin("never suggest it", sf) && occursin("correct for in the analysis", sf)
    @test acq_verdict_heading(fin) == "Before analysing" && acq_verdict_heading(plan) == "Before acquiring the rest"

    # points: the acquisition check's categories, the action after the arrow; unknown → Other, whole
    @test _acq_review_point("Shared structure: GFP shows in the bead channel in m1 → use a narrower filter") ==
          ("shared", "GFP shows in the bead channel in m1", "Use a narrower filter")
    @test _acq_review_point("fading: beads dim over m2 -> check counts per frame") == ("fading", "Beads dim over m2", "Check counts per frame")
    @test _acq_review_point("Lighting: odd") == ("other", "Lighting: odd", "")
    # the claim's parser is the same function with its own categories
    @test _claim_review_point("The data: x → y") == ("data", "X", "Y")
end

@testset "API: acquisition review — route, write-back, rounds" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    old_agent = _KIWI_AGENT[]
    post(f, b) = (r = f(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    wait_idle(puid) = (t0 = time(); while haskey(_KIWI_RUNNING, puid) && time() - t0 < 20; sleep(0.05); end)
    entry_md(puid, id) = read(joinpath(Cecelia.projects_dir(), puid, "blackboard", id, "entry.md"), String)
    meta = Dict{String,Any}("PhysicalSizeX" => 0.5,
        "acquisition" => Dict("lasers" => [Dict("wavelengthNm" => 890, "transmissionPct" => 10)]),
        "saturation" => Dict("channels" => [Dict("index" => 0, "photons" => Dict("fit" => Dict("greyPerPhoton" => 30.0),
                                                   "objects" => Dict("p50Photons" => 1.0, "p99Photons" => 10.0)))],
                             "sharedStructure" => Any[]))
    try
        proj = create_project!(name = "api-acq-review")
        s    = add_set!(proj; name = "uptake test")
        imgs = [add_image!(s; name = nm, meta = deepcopy(meta)) for nm in ("m1", "m2")]
        for img in imgs; Cecelia.set_channel_names!(img, ["GFP"]; check_length = false); Cecelia.save!(img); end
        body = Dict("projectUid" => proj.uid, "setUid" => s.uid)

        # no plan saved yet: nothing to review
        st, r = post(api_acqcheck_review, body)
        @test st == 400 && occursin("plan", r.error)
        @test post(api_acqcheck_review, Dict("projectUid" => proj.uid))[1] == 400
        @test post(api_acqcheck_review, Dict("projectUid" => proj.uid, "setUid" => "nope"))[1] == 404

        plan = Dict("designs" => [Dict("id" => "d1", "key" => "GFP")],
                    "objects" => [Dict("name" => "B cells", "kind" => "cells", "channels" => Dict("d1" => "GFP"))],
                    "text" => "Track B cells")
        st, r1 = post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => plan))
        id = String(r1.entryId)
        @test st == 200 && r1.review === nothing && r1.categoryLabels.shared == "Shared structure"
        @test occursin("## Model review\n\nNot run.", entry_md(proj.uid, id))
        # every rule finding names what it is about
        @test all(f -> f.category in Cecelia.ACQ_CATEGORIES, r1.findings)
        @test any(f -> f.code == "acq.few_photons" && f.category == "photons", r1.findings)

        uid1 = imgs[1].uid
        ref = Dict("kind" => "image", "imageUid" => uid1)
        reply = Dict("abstain" => false, "claims" => [
            Dict("kind" => "observation", "text" => "Drift: m1 shifts 40 µm\nin X over the movie. → Stabilise the preparation.", "refs" => [ref]),
            Dict("kind" => "interpretation", "text" => "Change before acquiring: B cells need more light.", "refs" => [ref])])
        fake = _AcqReviewFake([Cecelia.AgentResult(true, "", 10, 20, "s1", "", JSON3.read(JSON3.write(reply)),
                                                   ["""{"uid":"$uid1"}"""])], String[])
        _KIWI_AGENT[] = m -> fake

        # AI off for the profile: refused (kiwi_start_turn, where every in-app model turn starts), nothing spawned
        was = Cecelia.AI_PROFILE_DEFAULT[]
        Cecelia.AI_PROFILE_DEFAULT[] = false
        try
            st, r = post(api_acqcheck_review, body)
            @test !Cecelia.ai_enabled() && st == 403 && r.error == Cecelia.AI_OFF_MESSAGE
        finally
            Cecelia.AI_PROFILE_DEFAULT[] = was
        end
        @test isempty(fake.seen_opts) && !haskey(_KIWI_RUNNING, proj.uid)

        # Kiwi busy on the project: 409, nothing started
        _KIWI_RUNNING[proj.uid] = Dict{String,Any}("turnId" => "kt-busy")
        try
            @test post(api_acqcheck_review, body)[1] == 409
        finally
            delete!(_KIWI_RUNNING, proj.uid)
        end
        @test isempty(fake.seen_opts)

        st, rec = post(api_acqcheck_review, body)
        @test st == 200 && rec.status == "running" && rec.acqReview.entryId == id && rec.acqReview.round == 1
        wait_idle(proj.uid)
        sent = only(fake.seen_opts)
        @test occursin("Plan: “Track B cells”", sent) && occursin("- m1 — $uid1", sent)
        @test occursin("do NOT repeat", sent) && occursin("carries ~1.0 photons per voxel", sent)
        allowed = match(r":allowed_tools => \[([^\]]*)\]", sent)[1]
        denied  = match(r":disallowed_tools => \[([^\]]*)\]", sent)[1]
        @test occursin("get_acquisition_knowledge", allowed) && !occursin("read_blackboard_entry", allowed)
        @test occursin("read_blackboard_entry", denied) && occursin("get_populations", denied) && !occursin("get_image_info", denied)

        a = _read_bb_meta(proj.uid, id)["acqCheck"]
        rv = a["review"]
        @test rv["status"] == "done" && length(rv["claims"]) == 2
        x = rv["claims"][1]
        @test x["category"] == "drift" && x["text"] == "M1 shifts 40 µm in X over the movie." && x["action"] == "Stabilise the preparation."
        sid = x["sectionId"]
        @test sid == "s01" && a["reviewIds"]["review|" * x["text"]] == sid
        md = entry_md(proj.uid, id)
        @test occursin("### Drift\n\n### s01 · M1 shifts 40 µm in X over the movie. → Stabilise the preparation.", md)
        @test occursin("## Before acquiring the rest\n\nChange before acquiring: B cells need more light.", md)
        @test occursin("## Across the set", md)                          # the rule findings stay
        meta_e = _read_bb_meta(proj.uid, id)
        @test sid in meta_e["sectionIds"] && length(meta_e["kiwiRefs"]) == 1
        @test meta_e["updatedBy"]["via"] == "kiwi"
        # the GET carries the review
        g = JSON3.read(api_acqcheck(HTTP.Request("GET", "/api/acqcheck?projectUid=$(proj.uid)&setUid=$(s.uid)"))[2])
        @test g.review.claims[1].sectionId == sid
        # the user marks a point like any section
        @test api_blackboard_section_outcome(Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid,
            "entryId" => id, "sectionId" => sid, "verdict" => "good"))))[1] == 200

        # a turn that doesn't finish keeps the last review and says why
        push!(fake.script, Cecelia.AgentResult(false, "", 0, 0, "", "You've hit your limit · resets 5pm"))
        @test post(api_acqcheck_review, body)[1] == 200
        wait_idle(proj.uid)
        rvf = _read_bb_meta(proj.uid, id)["acqCheck"]["review"]
        @test occursin("hit your limit", rvf["lastError"]) && length(rvf["claims"]) == 2 && rvf["status"] == "done"
        @test occursin("⚠ The last run failed: ", entry_md(proj.uid, id)) && occursin("### s01", entry_md(proj.uid, id))

        # a review asked for an earlier round is ignored: the plan moved on
        stale = Dict{String,Any}("projectUid" => proj.uid, "turnId" => "kt-old", "status" => "done", "model" => "m",
                                 "acqReview" => Dict("entryId" => id, "round" => 0),
                                 "reply" => Dict("claims" => [Dict("kind" => "observation", "text" => "Other: stale")]))
        _acq_review_done!(stale)
        @test length(_read_bb_meta(proj.uid, id)["acqCheck"]["review"]["claims"]) == 2

        # a changed plan is a new round, and drops the review (it answered the earlier plan)
        plan["text"] = "Track B cells and count beads"
        st, r2 = post(api_acqcheck_plan, Dict("projectUid" => proj.uid, "setUid" => s.uid, "plan" => plan))
        @test r2.round == 2 && r2.review === nothing
        @test !haskey(_read_bb_meta(proj.uid, id)["acqCheck"], "review")
        @test occursin("## Model review\n\nNot run.", entry_md(proj.uid, id))
    finally
        _KIWI_AGENT[] = old_agent
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
