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
    # determinism: the same record gives the same text, byte for byte; a list of the set's images is the set
    @test methods_report(proj; set_uid = s.uid)["markdown"] == md
    @test methods_report(proj; image_uids = [img.uid])["markdown"] == md
    @test startswith(md, "> **Draft.**")
    @test occursin("*Set \"s\": 1 image", md)
    # the table is the Methods parameter table for these images (no populations: no cluster runs to keep)
    @test r["table"] == history_params_table(proj, [img.uid]; usage_runs = Set{Tuple{String,String,String}}())
    # the HMM sweep: clustering read movement_3, so 2/4/5/6 were tries — one sentence, still in the table
    hmm = Dict(m.captures[1] => row["usage"] for row in r["table"] if row["task"] == "behaviour.hmm"
               for m in [match(r"colName=(movement_\d)", row["changedFromDefault"])] if m !== nothing)
    @test hmm == Dict("movement_2" => "trial", "movement_3" => "", "movement_4" => "trial",
                      "movement_5" => "trial", "movement_6" => "trial")
    @test occursin("HMM (states + transitions) ×4 (1 image each)", md)
    # one population per tracking run, none tracked again later: each run is the method, none redone
    @test !any(row -> row["usage"] == "redone", r["table"])
    @test occursin("3-state hidden Markov model", md)
    @test !occursin("2-state", md) && !occursin("6-state", md)
    # prose, not a log: the template's words and units, not `key=value`
    @test occursin("maximum search radius 10 µm", md) && !occursin("maxSearchRadius", md)
    # the step Claude started names the recorded model, in its sentence and in the AI section
    @test occursin("(started by Claude ($model))", md)
    @test occursin("- Step *Cluster tracks*: started by Claude ($model).", md)
    # the one without a model says so — never a guessed one
    @test occursin("started by Claude (model not recorded)", md)
    @test count("- Step *", md) == 2                       # exactly the two re-stamped runs
    @test occursin(Cecelia.METHODS_CEILING, md)
    @test occursin("Cecelia 0.2.12", md)
    # every DOI the text's steps cite is in ONE numbered reference list, with its reference line
    cited = unique(strip(d) for row in r["table"] if isempty(row["usage"]) for d in split(row["cite"], "; ") if !isempty(strip(d)))
    @test !isempty(cited)
    refs = [l for l in split(md, '\n') if occursin(r"^\d+\. .* doi:10\.", l)]
    @test length(refs) == length(cited) + 1                       # + Cecelia, cited once beside the version
    @test startswith(refs[1], "1. Schienstock D et al. (2025) Cecelia") && occursin("Cecelia 0.2.12 [1].", md)
    @test all(d -> any(l -> endswith(l, "doi:$d"), refs), cited)
    @test occursin("Stringer C et al. (2020) Cellpose", md)
    @test !occursin("Cite:", md)
    # an input version set the same on every image is a changed setting, like any other
    seg = only(row for row in r["table"] if row["task"] == "segment.cellposeMeasure" && row["usage"] == "")
    @test occursin("intensityValueName=driftCorrected", seg["changedFromDefault"])
    # the CSV says which rows the text left out
    @test occursin("in_methods", first(split(r["csv"], '\n'))) && occursin("no (trial)", r["csv"])
    rm(proj.root; recursive = true)
end

@testset "Methods report — a run redone later on every image is not the method" begin
    # the real log tracks OTI, gBT and P14 one at a time; a later run tracking all three re-makes all
    # of them, so the three single runs are `redone` and the later one is the step
    proj, s, imgs = _mr_project("RFtn9f_TKufLV")
    img = only(imgs)
    log = read_run_log(img)
    redo = deepcopy(only(filter(e -> e["taskId"] == "rCQPDq", log)))
    redo["taskId"] = "REDO01"; redo["at"] = "2026-11-15T15:00:00"
    redo["params"]["popsToTrack"] = ["/qc/OTI", "/qc/non-OTI/gBT", "/qc/non-OTI/P14"]
    push!(log, redo)
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    t = history_params_table(proj, [img.uid]; usage_runs = Set{Tuple{String,String,String}}())
    tr = [row for row in t if row["task"] == "tracking.bayesian_track_measures"]
    @test length(tr) == 4
    @test count(row -> row["usage"] == "redone", tr) == 3
    @test only(row for row in tr if row["usage"] == "")["taskIds"] == ["REDO01"]
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("Bayesian track + measures ×3, later redone (1 image each)", md)
    @test occursin("OTI, gBT and P14 cells were tracked with btrack", md)
    rm(proj.root; recursive = true)

    # redone means LATER on EVERY image the earlier run was on: a redo on one of two images, or one
    # logged the same second, leaves the earlier run the method
    for (imgs_redone, at) in ((1, "2026-11-15T15:00:00"), (2, "2026-11-15T14:18:54"))
        proj, s, imgs = _mr_project("RFtn9f_TKufLV", 2)
        for (k, img) in enumerate(imgs)
            log = read_run_log(img)
            if k <= imgs_redone
                redo = deepcopy(only(filter(e -> e["taskId"] == "rCQPDq", log)))
                redo["taskId"] = "REDO0$k"; redo["at"] = at
                redo["params"]["popsToTrack"] = ["/qc/OTI", "/qc/non-OTI/gBT", "/qc/non-OTI/P14"]
                push!(log, redo)
            end
            write(Cecelia.run_log_path(img._dir), JSON3.write(log))
        end
        t = history_params_table(proj, [i.uid for i in imgs]; usage_runs = Set{Tuple{String,String,String}}())
        @test only(row for row in t if "rCQPDq" in row["taskIds"])["usage"] != "redone"
        rm(proj.root; recursive = true)
    end
end

@testset "Methods report — a row's cite and an entry's commit print when present" begin
    proj, s, imgs = _mr_project("RFtn9f_TKufLV")
    img = only(imgs)
    log = read_run_log(img)
    foreach(e -> e["ceceliaCommit"] = "abc1234", log)
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("commit abc1234", md)
    # a row's citations number into the shared list, first-cited first; a second cite of one DOI reuses it
    row = Dict{String,Any}("step" => "Cellpose", "task" => "segment.cellposeMeasure", "images" => "1/2",
                           "cite" => "10.1038/s41592-020-01018-x", "taskIds" => String[])
    refs = String["10.1083/jcb.201004104"]
    s1 = Cecelia._mr_step_sentence(row, Any[], 2, refs, Set{String}())
    @test startswith(s1, "In 1 of 2 images, ") && endswith(s1, " [2].")
    @test refs == ["10.1083/jcb.201004104", "10.1038/s41592-020-01018-x"]
    @test endswith(Cecelia._mr_step_sentence(row, Any[], 2, refs, Set{String}()), "[2].") && length(refs) == 2
    rm(proj.root; recursive = true)
end

@testset "Methods report — a step that read a population since removed is a check, not prose" begin
    proj, s, imgs = _mr_project("RFtn9f_TKufLV")
    row = Dict{String,Any}("step" => "Cluster regions", "task" => "clustRegions.cluster", "images" => "1/1",
                           "taskIds" => String[], "cite" => "")
    e = Dict{String,Any}("taskId" => "x", "fun" => "clustRegions.cluster", "status" => "done", "at" => "2026-11-19T12:00:00",
                         "params" => Dict{String,Any}("basisPops" => ["Tcells/slow", "Tcells/long meandering"]))
    checks = String[]
    txt = Cecelia._mr_step_sentence(row, Any[e], 1, String[], Set(["Tcells/slow"]), checks)
    @test !occursin("no longer", txt)
    @test only(checks) == "*Cluster regions* (1/1) read population no longer defined on these images — " *
                          "long meandering: renamed or removed since, so it cannot be rerun as recorded."
    rm(proj.root; recursive = true)
end

@testset "Methods report — a population removed is checked on every run, not the newest" begin
    proj, s, imgs = _mr_project("RFtn9f_TKufLV", 2)
    row = Dict{String,Any}("step" => "Cluster regions", "task" => "clustRegions.cluster", "images" => "2/2",
                           "taskIds" => String[], "cite" => "")
    run(id, at, uid, pops) = Dict{String,Any}("taskId" => id, "fun" => "clustRegions.cluster", "status" => "done",
                                              "at" => at, "uID" => uid, "params" => Dict{String,Any}("basisPops" => pops))
    # the older image read a pop since removed; the newer one did not — the record still holds it
    older = run("a", "2026-11-19T12:00:00", imgs[1].uid, ["Tcells/slow", "Tcells/gone"])
    newer = run("b", "2026-11-20T12:00:00", imgs[2].uid, ["Tcells/slow"])
    checks = String[]
    Cecelia._mr_step_sentence(row, Any[older, newer], 2, String[], Set(["Tcells/slow"]), checks)
    @test only(checks) == "*Cluster regions* (2/2) read population no longer defined on these images — " *
                          "gone: renamed or removed since, so it cannot be rerun as recorded."
    rm(proj.root; recursive = true)
end

@testset "Methods report — a combination of two same-named pops names them apart" begin
    pop(path) = (; value_name = "Tcells", path)
    names = Cecelia._mr_pop_names([(nothing, Any[pop("/qc"), pop("/qc/OTI"), pop("/other"), pop("/other/OTI")])])
    @test names["Tcells"]["/qc/OTI"] == "qc/OTI" && names["Tcells"]["/other/OTI"] == "other/OTI"
    p = (; gate = nothing, pop_type = "flow", parent = "root", boolean_op = "and",
           boolean_pops = ["/qc/OTI"], boolean_not = ["/other/OTI"])
    @test Cecelia._mr_pop_detail(p, String[], names["Tcells"]) == (:boolean, "qc/OTI without other/OTI")
    # a "mostly <child>" pop says which sub-population its tracks are mostly in
    mp = (; gate = nothing, pop_type = "flow", parent = "/qc", boolean_op = nothing, track_class_of = "/qc/OTI")
    @test Cecelia._mr_pop_detail(mp, String[], names["Tcells"])[1] == :trackclass
    @test endswith(Cecelia._mr_pop_detail(mp, String[], names["Tcells"])[2], "mostly in qc/OTI")
end

@testset "Methods report — what a population is, and what Claude did to make it" begin
    mk(kind, pt, detail) = Cecelia._MrPop(pt, "Tcells", "/x", "x", "root", kind, Dict(detail => 1), 1,
                                          Any[Dict("via" => "claude", "model" => "m")], false)
    _, ai = Cecelia._mr_populations([mk(:gate, "flow", "volume vs surface-to-volume ratio"),
                                     mk(:cluster, "trackclust", "movement|clusters 4, 5")], 1)
    @test "- *x* — cell gate (volume vs surface-to-volume ratio): gate drawn by Claude (m)." in ai
    @test "- *x* — track-cluster population (clusters 4, 5 of run movement): clusters assigned by Claude (m)." in ai
    @test !any(l -> occursin("drawn", l) && occursin("cluster", l), ai)
end

@testset "Methods report — values read as a reader says them" begin
    @test methods_measure_phrase("live.track.meanTurningAngle") == "mean turning angle"
    @test methods_measure_phrase("live.track.speed") == "speed"
    @test methods_measure_phrase("live.cell.hmm.state.movement_3") == "HMM states (movement_3)"
    @test methods_measure_phrase("surface_to_volume") == "surface-to-volume ratio"
    @test methods_pop_phrase("Tcells/qc/non-OTI/gBT/_tracked") == "gBT"
    @test methods_pop_phrase("Tcells/") == "all"
    @test methods_pop_phrase("/qc/OTI") == "OTI" && methods_pop_phrase("Tcells/_tracked") == "all"
    # two pops of one name under different parents stay two names
    @test Cecelia.methods_pop_phrases(["T/qc/OTI/_tracked", "T/other/OTI/_tracked"]) == ["qc/OTI", "other/OTI"]
    # the unit comes from the label; a bracket that is not a unit is not one
    @test Cecelia._mr_value_text(10, Dict("type" => "int", "label" => "Max search radius (µm)")) == "10 µm"
    @test Cecelia._mr_value_text(3, Dict("type" => "int", "label" => "Number of regions (k-means)")) == "3"
    @test Cecelia._mr_value_text(-2.0, Dict("type" => "float", "label" => "Cell probability threshold")) == "−2"
    @test Cecelia._mr_value_text(5.0, Dict("type" => "float", "label" => "Max rotation per frame (°)")) == "5°"
    sel = Dict("type" => "select", "options" => [Dict("value" => "movie", "label" => "Whole movie (window graphs)")])
    @test Cecelia._mr_value_text("movie", sel) == "whole movie"
    @test Cecelia._mr_value_text("default", Dict("type" => "valueNameSelection")) == "raw"
    @test Cecelia._mr_value_text(["a/qc/OTI/_tracked", "a/qc/P14/_tracked"], Dict("type" => "popSelection")) == "OTI and P14"
    # images that disagree: every value with its count, or the table when the values are lists
    @test Cecelia._mr_spread_text([("gBT-uGFP", 9), ("OTI-uGFP", 7)], Dict("type" => "channelSelection")) ==
          "gBT-uGFP (9 images) or OTI-uGFP (7 images)"
    @test Cecelia._mr_spread_text([(["a", "b"], 9), (["c", "d"], 7)], Dict("type" => "channelSelection")) ==
          "set per image, see the table"
    # a gate axis is named through THIS image's channels: one raw column, two channel orders
    @test Cecelia._mr_axis("mean_intensity_1", ["SHG", "gBT-uGFP", "OTI-CTV"]) == "gBT-uGFP intensity"
    @test Cecelia._mr_axis("mean_intensity_1", ["SHG", "OTI-uGFP", "gBT-CTV"]) == "OTI-uGFP intensity"
    @test Cecelia._mr_axes("A intensity", "B intensity") == "A vs B intensity"
    @test Cecelia._mr_capitalise("gBT cells were tracked") == "gBT cells were tracked"
    @test Cecelia._mr_capitalise("cells were tracked") == "Cells were tracked"
    @test replace("gaps of up to 1 frames, 11 frames", Cecelia._MR_ONE_PLURAL => s"1 \1") == "gaps of up to 1 frame, 11 frames"
end

@testset "Methods report — every built-in methodsText names real params, every cited DOI has a reference" begin
    root = joinpath(pkgdir(Cecelia), "src", "tasks")
    specs = [JSON3.read(read(joinpath(d, f), String)) for (d, _, fs) in walkdir(root) for f in fs if endswith(f, ".json")]
    specs = [JSON3.read(JSON3.write(s), Dict{String,Any}) for s in specs if s isa AbstractDict]
    specs = [s for s in specs if s isa AbstractDict && haskey(s, "fun_name")]
    with_text = [s for s in specs if haskey(s, "methodsText")]
    @test length(with_text) >= 15
    for s in with_text
        fn = s["fun_name"]
        idx = Cecelia._mr_param_index(fn)
        parts = Cecelia._mr_template(s)
        @test parts !== nothing
        for part in parts
            text = part isa AbstractString ? part : string(get(part, "text", ""))
            for m in eachmatch(r"\{([A-Za-z0-9_]+)\}", text)
                @test (fn, m.captures[1], haskey(idx, m.captures[1])) == (fn, m.captures[1], true)
            end
            part isa AbstractDict && for k in keys(get(part, "showIf", Dict()))
                @test (fn, string(k), haskey(idx, string(k))) == (fn, string(k), true)
            end
        end
    end
    refs = Cecelia.methods_references()
    for s in specs, c in something(get(s, "citations", nothing), [])
        d = c isa AbstractDict ? string(get(c, "doi", "")) : string(c)
        @test (s["fun_name"], d, haskey(refs, d)) == (s["fun_name"], d, true)
    end
    @test Cecelia.methods_reference_line("10.0000/unknown") == "doi:10.0000/unknown"
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
    @test occursin("A test step logged \"second\" after waiting 0 ms.", md)
    @test occursin("No cohort check is recorded", md)
    @test_throws Exception methods_report(proj; run_id = "nope")

    # a run whose entries rotated into the run-log archive still reports the steps it made
    for img in imgs[1:2]
        Cecelia._append_run_log_archive!(img._dir, read_run_log(img))
        write(Cecelia.run_log_path(img._dir), "[]")
    end
    @test methods_report(proj; run_id = run.id)["table"] == r["table"]

    # images that disagree on a `methodsText` slot state the spread, never one image's value as the method
    run_chain(proj, [imgs[1].uid]; chain = "methods-chain", on_log = _ -> nothing,
              overrides = Dict{String,Any}("b" => Dict("message" => "third")))
    set_md = methods_report(proj; image_uids = [imgs[1].uid, imgs[2].uid])["markdown"]
    @test occursin("logged \"second (1 image) or third (1 image)\"", set_md)
    @test !occursin("logged \"third\"", set_md)
    rm(proj.root; recursive = true)
end

@testset "Methods report — a sweep version is a trial when any image ran it beside the one read" begin
    # the same case as chain_from_history's: movement_5 last ran on one image's OTI alone, but the other
    # image ran it on the tracks the clustering's movement_3 read — still a try, not the method
    proj, s, imgs = _mr_project("RFtn9f_TKufLV", 2)
    _hmm_m5_on_oti!(proj, imgs[1].uid)
    r = methods_report(proj; set_uid = s.uid)
    usage = [row["usage"] for row in r["table"]
             if row["task"] == "behaviour.hmm" && occursin("colName=movement_5", row["changedFromDefault"])]
    @test !isempty(usage) && all(==("trial"), usage)
    rm(proj.root; recursive = true)
end
