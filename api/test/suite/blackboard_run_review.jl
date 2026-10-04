# Agent run review on the Blackboard — AGENT_RUN_REVIEW_PLAN P2 (`api/src/blackboard_run_review.jl`):
# the agentRun marker, per-section verdicts, a proposal from Claude never replacing a person's verdict,
# and both fields surviving the other meta writes.

@testset "API: blackboard section verdicts on an agent run record (AGENT_RUN_REVIEW P2)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTRUNREV"; mkpath(joinpath(tmp, uid))
        post(path, obj; claude = false) = handle_http(
            HTTP.Request("POST", path, claude ? ["X-Cecelia-Client" => "claude"] : Pair{String,String}[]),
            Vector{UInt8}(JSON3.write(obj)))
        content = "intro\n\n## Decisions\n\n### d01 · segment · img1 · cellpose\n- did\n\n### d02 · gate · img1 · add_gate on T\n- did\n"
        run = Dict("copyProjectUid" => "CP", "sectionIds" => ["d01", "d02"])
        @test post("/api/blackboard/create", Dict("projectUid" => uid, "title" => "t", "content" => "x",
                                                   "agentRun" => "nope"))[1] == 400
        st, body = post("/api/blackboard/create", Dict("projectUid" => uid, "title" => "Agent run x",
                                                        "content" => content, "agentRun" => run))
        @test st == 200
        eid = String(JSON3.read(body).entryId)
        set(sid, verdict, note = ""; claude = false) = post("/api/blackboard/section-outcome",
            Dict("projectUid" => uid, "entryId" => eid, "sectionId" => sid, "verdict" => verdict,
                 "note" => note); claude = claude)
        entry() = JSON3.read(api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))[2]).entry
        row() = only(r for r in JSON3.read(api_blackboard_list(HTTP.Request("GET",
            "/api/blackboard?projectUid=$uid"))[2]).entries if r.entryId == eid)

        @test entry().agentRun.copyProjectUid == "CP"
        @test row().sectionsMarked == 0
        # guards
        @test set("x1", "good")[1] == 400                     # not a section id
        @test set("d01", "maybe")[1] == 400
        @test set("d01", "bad")[1] == 400                     # bad needs a note
        @test set("d09", "good")[1] == 404                    # no such heading
        # a proposal, then a person's verdict over it, then a proposal refused
        @test set("d01", "good"; claude = true)[1] == 200
        @test entry().sectionOutcomes.d01.by.via == "claude"
        @test row().sectionsMarked == 0                       # proposals are not counted
        @test set("d01", "bad", "wrong channel")[1] == 200
        @test entry().sectionOutcomes.d01.verdict == "bad"
        @test set("d01", "good"; claude = true)[1] == 409
        @test set("d01", ""; claude = true)[1] == 409         # nor cleared
        @test set("d02", "unsure")[1] == 200
        @test row().sectionsMarked == 2
        # other meta writes keep both fields
        @test _post(api_blackboard_status, Dict("projectUid" => uid, "entryId" => eid, "status" => "resolved"))[1] == 200
        @test _post(api_blackboard_revise, Dict("projectUid" => uid, "entryId" => eid,
                                                 "content" => content * "\n### m01 · gate · img1 · missed\n- QC gate\n"))[1] == 200
        e = entry()
        @test e.agentRun.copyProjectUid == "CP" && e.sectionOutcomes.d01.note == "wrong channel"
        @test set("m01", "bad", "no QC gate")[1] == 200       # a miss added by revise, then marked
        @test set("d02", "")[1] == 200                        # a person clears their own
        @test !haskey(entry().sectionOutcomes, :d02)

        # P4 — a lesson promoted from a section is marked as lab knowledge; the mark rides through
        # a revise; not on the run record itself or the profile
        st, body = post("/api/blackboard/create", Dict("projectUid" => uid, "title" => "Lesson", "content" => "QC gate first"))
        kid = String(JSON3.read(body).entryId)
        mark(id, on; from = nothing) = post("/api/blackboard/knowledge",
            Dict{String,Any}("projectUid" => uid, "entryId" => id, "knowledge" => on,
                             (from === nothing ? () : ("from" => from,))...))
        krow() = only(r for r in JSON3.read(api_blackboard_list(HTTP.Request("GET",
            "/api/blackboard?projectUid=$uid"))[2]).entries if r.entryId == kid)
        @test !haskey(krow(), :knowledge)
        @test mark(eid, true)[1] == 400                       # a run record
        @test mark("profile", true)[1] == 400
        @test mark(kid, "yes")[1] == 400
        @test mark(kid, true; from = Dict("entryId" => eid, "sectionId" => "zz"))[1] == 400
        @test mark(kid, true; from = Dict("entryId" => eid, "sectionId" => "m01"))[1] == 200
        @test krow().knowledge.from.sectionId == "m01" && krow().knowledge.by.via == "app"
        @test _post(api_blackboard_revise, Dict("projectUid" => uid, "entryId" => kid, "content" => "QC gates on volume"))[1] == 200
        @test haskey(krow(), :knowledge)
        @test mark(kid, false)[1] == 200
        @test !haskey(krow(), :knowledge)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
