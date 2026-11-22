# ── Methods report (app/src/ai/methods_report.jl; docs/ANALYSIS.md → *Methods report*) ──
# The set scope runs on a REAL run log (test-data/run-logs/RFtn9f_TKufLV.json: segment → tracking ×4 →
# an HMM sweep → track clustering, every entry stamped `via: app`). Two entries are re-stamped through
# `run_author` — the function the scheduler stamps with — as Claude's, one with a model and one
# without. The chain scope runs a real chain of the in-tree test task, so its record and run log are
# what `run_chain` writes.

function _mr_project(fixture::String, n::Int = 1)
    proj = create_project!(name = "methods-$(rand(1000:9999))")
    s = add_set!(proj; name = "s")
    src = normpath(joinpath(@__DIR__, "..", "..", "..", "test-data", "run-logs", fixture * ".json"))
    imgs = [add_image!(s; name = "img$i") for i in 1:n]
    for img in imgs
        cp(src, Cecelia.run_log_path(img._dir); force = true)
    end
    proj, s, imgs
end

@testset "Methods report — how an author stamp reads" begin
    @test methods_ai_phrase(Any[]) == ""
    @test methods_ai_phrase([Dict("profile" => "p", "via" => "app")]) == ""          # a person's run
    @test methods_ai_phrase([Dict("profile" => "p")]) == ""                          # a REPL run: no via
    @test methods_ai_phrase([Dict("via" => "claude", "model" => "claude-x-1")]) == "Claude (claude-x-1)"
    @test methods_ai_phrase([Dict("via" => "claude")]) == "Claude ($(Cecelia.METHODS_MODEL_UNKNOWN))"
    @test Cecelia.METHODS_MODEL_UNKNOWN == "model not recorded"
    # both: the known model first, then the gap — never one standing in for the other
    @test methods_ai_phrase([Dict("via" => "claude"), Dict("via" => "claude", "model" => "m1")]) ==
          "Claude (m1, model not recorded)"
    @test occursin("Kiwi", methods_ai_phrase([Dict("via" => "kiwi", "model" => "m2")]))
end

@testset "Methods report — set scope on a real run log" begin
    proj, s, imgs = _mr_project("RFtn9f_TKufLV")
    img = only(imgs)
    log = read_run_log(img)
    model = "claude-opus-4-1-20250805"
    for e in log
        e["taskId"] == "VpF1hG" && (e["author"] = Cecelia.run_author("default", Dict("via" => "claude", "model" => model)))
        e["taskId"] == "L3rb9J" && (e["author"] = Cecelia.run_author("default", Dict("via" => "claude")))
    end
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))

    r = methods_report(proj; set_uid = s.uid)
    md = r["markdown"]
    # determinism: the same record gives the same text, byte for byte
    @test methods_report(proj; set_uid = s.uid)["markdown"] == md
    @test methods_report(proj; image_uids = [img.uid])["table"] == r["table"]
    # the draft banner leads
    @test startswith(md, "> **Draft.**")
    # the table is the Methods parameter table for these images, not a second derivation
    @test r["table"] == history_params_table(proj, [img.uid])
    lines = split(md, '\n')
    stepline(fn) = findfirst(l -> occursin("(`$fn`)", l) && occursin(r"^\d+\. ", l), lines)
    # the step Claude started names the recorded model, under that step
    i = stepline("clustTracks.cluster")
    @test i !== nothing && occursin("Started by Claude ($model)", lines[i + 1])
    # the one without a model says so — never a guessed one
    @test any(l -> occursin("Started by Claude (model not recorded)", l), lines)
    # the app-stamped steps are no one's AI step: exactly the two re-stamped runs are named
    @test count(l -> occursin("Started by", l), lines) == 2
    @test occursin(Cecelia.METHODS_CEILING, md)
    # one numbered step per table row, in the table's order
    @test count(l -> occursin(r"^\d+\. \*\*", l), lines) == length(r["table"])
    # the version on the runs is cited; the fixture's entries all say 0.2.12
    @test occursin("Cecelia 0.2.12", md)
    # every row the table gives a citation (the task spec's `citations`) prints it in the text
    cited = [row["cite"] for row in r["table"] if !isempty(get(row, "cite", ""))]
    @test !isempty(cited)
    @test all(c -> occursin("   - Cite: $c", md), cited)
    rm(proj.root; recursive = true)
end

@testset "Methods report — a row's cite and an entry's commit print when present" begin
    proj, s, imgs = _mr_project("RFtn9f_TKufLV")
    img = only(imgs)
    log = read_run_log(img)
    foreach(e -> e["ceceliaCommit"] = "abc1234", log)
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("commit abc1234", md)
    # a row carrying `cite` (task citation DOIs) prints it under its step
    row = Dict{String,Any}("step" => "Cellpose", "task" => "segment.cellposeMeasure", "images" => "1/1",
                           "cite" => "10.1038/s41592-020-01018-x", "taskIds" => String[])
    @test any(l -> occursin("Cite: 10.1038/s41592-020-01018-x", l), Cecelia._mr_step(1, row, Any[]))
    rm(proj.root; recursive = true)
end

@testset "Methods report — chain-run scope lists every node with its status" begin
    proj = create_project!(name = "methods-chain-$(rand(1000:9999))")
    s = add_set!(proj; name = "s")
    imgs = [add_image!(s; name = "img$i") for i in 1:3]
    for img in imgs; img.status = IMAGE_DONE; save!(img); end
    tpl = ChainTemplate("methods-chain",
        [ChainNode(id = "a", fn = "testTasks.image_task", scope = "image", params = Dict{String,Any}()),
         ChainNode(id = "b", fn = "testTasks.image_task", scope = "image",
                   params = Dict{String,Any}("message" => "second"))],
        [ChainEdge("a", "b")])
    save_chain_template!(proj, tpl)
    run = run_chain(proj, [imgs[1].uid, imgs[2].uid]; chain = "methods-chain", on_log = _ -> nothing,
                    excluded = Dict(imgs[3].uid => "excluded"))
    @test all(st -> st.status == NODE_DONE, (st for m in values(run.image_states) for st in values(m)))

    r = methods_report(proj; run_id = run.id)
    md = r["markdown"]
    @test methods_report(proj; run_id = run.id)["markdown"] == md
    @test startswith(md, "> **Draft.**") && r["scope"]["kind"] == "chainRun"
    @test sort(r["scope"]["imageUids"]) == sort([imgs[1].uid, imgs[2].uid])
    lines = split(md, '\n')
    rows = [l for l in lines if occursin("| `testTasks.image_task` |", l)]
    @test length(rows) == length(tpl.nodes)                       # every node, once
    @test all(l -> occursin("done 2/2", l), rows)                 # its status across the run's images
    # timing joined from the run log by task id: a start and an end on every node
    @test all(l -> length(collect(eachmatch(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}", l))) == 2, rows)
    @test occursin("img3 (excluded)", md)                         # the selection was larger
    # per-node params as the run recorded them: node a ran on defaults, node b changed the message
    @test occursin("(node a)", rows[1]) && occursin("| defaults |", rows[1])
    @test occursin("(node b)", rows[2]) && occursin("message=second", rows[2])
    # `overwrite` (replace a label set of the same name) is how a node writes, not a method
    np = Cecelia._mr_node_params((; node_params = Dict("x" => Dict("overwrite" => true, "message" => "second"))),
                                 (; id = "x", fn = "testTasks.image_task"))
    @test occursin("message=second", np) && !occursin("overwrite", np)
    # the table is the runs these nodes made, nothing else on the images
    ids = Set(st.task_id for m in values(run.image_states) for st in values(m))
    @test !isempty(r["table"]) && all(row -> all(in(ids), row["taskIds"]), r["table"])
    # the spec's `methodsText` is honoured, slots filled from what the step ran with
    @test occursin("a test step logged \"second\" after waiting 0 ms.", md)
    @test occursin("No cohort check is recorded", md)
    @test_throws Exception methods_report(proj; run_id = "nope")

    # a run whose entries rotated into the run-log archive still reports the steps it made
    for img in imgs[1:2]
        Cecelia._append_run_log_archive!(img._dir, read_run_log(img))
        write(Cecelia.run_log_path(img._dir), "[]")
    end
    @test methods_report(proj; run_id = run.id)["table"] == r["table"]

    # images that disagree on a `methodsText` slot get the generic sentence and the spread, never one
    # image's value stated as the method
    run_chain(proj, [imgs[1].uid]; chain = "methods-chain", on_log = _ -> nothing,
              overrides = Dict{String,Any}("b" => Dict("message" => "third")))
    set_md = methods_report(proj; image_uids = [imgs[1].uid, imgs[2].uid])["markdown"]
    @test !occursin("a test step logged", set_md)
    @test occursin("The images differ in: message:", set_md)
    rm(proj.root; recursive = true)
end
