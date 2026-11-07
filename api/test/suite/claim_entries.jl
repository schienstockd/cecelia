# Claim entries (api/src/routes/claim.jl): a stated claim is a Blackboard entry whose `### sNN`
# sections are the checks' warnings. Re-checks keep a finding's section id, close the ones the record
# no longer shows, and spend no snapshot when nothing changed; rewording starts a new round.

@testset "API: claim entries" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    post(f, b) = (r = f(Vector{UInt8}(JSON3.write(b))); (r[1], JSON3.read(r[2])))
    try
        proj = create_project!(name = "api-claim-entries")
        s    = add_set!(proj; name = "set-A")
        imgs = [add_image!(s; name = nm, meta = Dict{String,Any}("ori_path" => "/tmp/x.tif")) for nm in ("i1", "i2")]
        scope = Dict("projectUid" => proj.uid, "setUid" => s.uid, "populations" => ["B/qc", "T/qc"],
                     "measure" => "live.track.speed", "unitAttr" => "Mouse")

        @test post(api_claim_state, Dict("projectUid" => proj.uid))[1] == 400
        st, r = post(api_claim_state, merge(scope, Dict("text" => "T cells move faster than B cells")))
        @test st == 200 && r.round == 1
        id = String(r.entryId)
        meta = _read_bb_meta(proj.uid, id)
        c = meta["claim"]
        @test c["text"] == "T cells move faster than B cells" && length(c["rounds"]) == 1
        # Mouse is blank on both images: a warning with its own section
        ub = only(f for f in r.findings if f.code == "claim.unit_blank")
        sid = String(ub.sectionId)
        md = read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String)
        @test occursin("### $sid · Record · ", md) && occursin("review (not run)", md)
        @test sid in c["lastCheck"]["open"] && sid in meta["sectionIds"]

        # nothing changed: the re-check is a no-op revise, no snapshot
        st, r2 = post(api_claim_check, Dict("projectUid" => proj.uid, "entryId" => id))
        @test st == 200 && isempty(r2.closed)
        @test _read_bb_meta(proj.uid, id)["current"] == 0

        # the analyst marks the finding; then fixes the record
        @test api_blackboard_section_outcome(Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid,
            "entryId" => id, "sectionId" => sid, "verdict" => "good"))))[1] == 200
        st, op = (r = api_claim_open(HTTP.Request("GET", "/api/claim/open?projectUid=$(proj.uid)")); (r[1], JSON3.read(r[2])))
        @test st == 200 && any(o -> o.sectionId == sid, only(op.claims).open)
        for (img, m) in zip(imgs, ("1", "2")); img.attr["Mouse"] = m; Cecelia.save!(img); end
        # the briefing read sees it closed, without writing
        op = JSON3.read(api_claim_open(HTTP.Request("GET", "/api/claim/open?projectUid=$(proj.uid)"))[2])
        @test sid in only(op.claims).closed
        @test sid in _read_bb_meta(proj.uid, id)["claim"]["lastCheck"]["open"]
        # the re-check writes it: the section leaves the text, the verdict stays stored
        st, r3 = post(api_claim_check, Dict("projectUid" => proj.uid, "entryId" => id))
        @test st == 200 && sid in r3.closed
        meta = _read_bb_meta(proj.uid, id)
        @test !(sid in get(meta, "sectionIds", String[])) && haskey(meta["sectionOutcomes"], sid)
        @test meta["current"] == 1

        # the same claim restated: no new round. Reworded: round 2, and the earlier one stays listed
        @test post(api_claim_state, merge(scope, Dict("entryId" => id, "text" => "T cells move faster than B cells")))[2].round == 1
        st, r4 = post(api_claim_state, merge(scope, Dict("entryId" => id, "text" => "T cells move faster per image")))
        @test st == 200 && r4.round == 2
        md = read(joinpath(Cecelia.projects_dir(), proj.uid, "blackboard", id, "entry.md"), String)
        @test occursin("Round 2", md) && occursin("round 1: “T cells move faster than B cells”", md)
        # if Mouse goes blank again the finding comes back under the same section id
        imgs[1].attr["Mouse"] = ""; Cecelia.save!(imgs[1])
        st, r5 = post(api_claim_check, Dict("projectUid" => proj.uid, "entryId" => id))
        @test any(f -> get(f, :sectionId, "") == sid, r5.findings)

        # not a claim entry
        st, bb = api_blackboard_create(Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid, "title" => "note", "content" => "x"))))
        @test post(api_claim_check, Dict("projectUid" => proj.uid, "entryId" => String(JSON3.read(bb).entryId)))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
