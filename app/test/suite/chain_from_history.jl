# ── Chain from history (tasks/chain/from_history.jl) ──────────────────────
# Fixtures are REAL run-log rows (test-data/run-logs/), copied from two dev projects' histories:
# zolIMa (MERTK; UMq4v3 = the one image run end to end, x4E5HU = drift/denoise trial and error) and
# 4kS67f (B vs T; two images whose spatial steps ran as one set-scope run).

function _history_project(logs::Pair{String,String}...)
    proj = create_project!(name = "history-$(rand(1000:9999))")
    s = add_set!(proj; name = "s")
    uids = Dict{String,String}()
    for (name, fixture) in logs
        img = add_image!(s; name)
        src = normpath(joinpath(@__DIR__, "..", "..", "..", "test-data", "run-logs", fixture * ".json"))
        cp(src, Cecelia.run_log_path(img._dir); force = true)
        uids[name] = img.uid
    end
    proj, uids
end

_hnode(r, fn) = [n for n in r["template"]["nodes"] if n["fn"] == fn]
_hedge(r, from, to) = any(e -> e["from"] == from && e["to"] == to, r["template"]["edges"])

@testset "Chain from history — zolIMa UMq4v3: segment → gate → tracking ×2" begin
    proj, uids = _history_project("u" => "zolIMa_UMq4v3")
    r = chain_from_history(proj, [uids["u"]])
    seg = only(_hnode(r, "segment.coastalMeasure"))
    trk = _hnode(r, "tracking.bayesian_track_measures")
    # three tracking runs on Bcells: /qc/frags twice (a rerun — latest wins) and /qc/cells once
    @test sort([t["params"]["popsToTrack"] for t in trk]) == ["/qc/cells", "/qc/frags"]
    # the failed + cancelled CD169 segmentations of 10-29 are not the step
    @test seg["params"]["outputValueName"] == "Bcells"
    # gates: tracking reads gated populations nothing produces → one manual gate step on Bcells
    @test r["gateSteps"] == ["gate.Bcells"]
    gate = only(_hnode(r, "gate.manual_gate"))
    @test gate["params"]["requiredPops"] == "/qc/cells, /qc/frags"
    @test gate["params"]["copyFrom"] == uids["u"]
    @test _hedge(r, seg["id"], "gate.Bcells")
    @test all(t -> _hedge(r, "gate.Bcells", t["id"]) && _hedge(r, seg["id"], t["id"]), trk)
    # training: the model exists, so it is reused, not retrained
    @test r["reusedModels"] == [Dict{String,Any}("fn" => "opticalFlow.train", "model" => "flow.small")]
    @test isempty(_hnode(r, "opticalFlow.train"))
    @test length(_hnode(chain_from_history(proj, [uids["u"]]; retrain = true), "opticalFlow.train")) == 1
    # tracking per population is a fork (keep both); its versions are told apart by the populations
    tf = [f for f in r["finalOutputs"] if f["fn"] == "tracking.bayesian_track_measures"]
    @test all(f -> f["multi"], tf) && sort([f["detail"] for f in tf]) == ["/qc/cells", "/qc/frags"]
    # the start dot sits on the roots, so the canvas draws it straight away, saved or not
    @test r["template"]["startTargets"] == [seg["id"]]
    # the template only fails validation on the model, which this test install's vault lacks
    @test !r["valid"] && occursin("flow.small.pt", r["error"])
    rm(proj.root; recursive = true)
end

@testset "Chain from history — zolIMa x4E5HU: trial and error resolves to what is on disk" begin
    proj, uids = _history_project("x" => "zolIMa_x4E5HU")
    r = chain_from_history(proj, [uids["x"]])
    # ten drift runs flipping input and channel all wrote `driftCorrected`: the last done one is on disk
    drift = only(_hnode(r, "cleanupImages.driftCorrect"))
    @test drift["params"]["valueName"] == "default"
    @test drift["params"]["driftChannel"] == ["CD169-Kat"]
    # four denoise runs with model `supp` failed; the done one used supp.small
    den = only(_hnode(r, "cleanupImages.denoise"))
    @test den["params"]["model"] == "supp.small"
    seg = only(_hnode(r, "segment.coastalMeasure"))
    @test _hedge(r, drift["id"], den["id"]) && _hedge(r, den["id"], seg["id"])
    # stack-align: nothing read its output — offered as its own final output, and gone when untick'd
    sa = only(f for f in r["finalOutputs"] if f["fn"] == "cleanupImages.stackAlign")
    @test sa["output"] == "stackAligned"
    r2 = chain_from_history(proj, [uids["x"]]; keep = [seg["id"]])
    @test isempty(_hnode(r2, "cleanupImages.stackAlign")) && length(_hnode(r2, "cleanupImages.driftCorrect")) == 1
    # an import is never replayed
    @test isempty(_hnode(r, "importImages.omezarr"))
    rm(proj.root; recursive = true)
end

@testset "Chain from history — 4kS67f: two exemplars, namespaces, gates, legacy rows" begin
    proj, uids = _history_project("a" => "4kS67f_LUkCpP", "b" => "4kS67f_k3Tx90")
    r = chain_from_history(proj, [uids["a"], uids["b"]])
    @test r["valid"]
    # a set-scope run logged on both images is one step, not two, and not a conflict
    @test isempty(r["conflicts"])
    nb = only(_hnode(r, "spatialAnalysis.cellNeighbours"))
    @test nb["params"]["graphSuffix"] == "immune"
    stats = _hnode(r, "spatialAnalysis.neighbourStats")
    @test all(s -> _hedge(r, nb["id"], s["id"]), stats)
    # region clusters are named `immune` too — a different namespace, so no edge to graph readers
    regions_immune = only(n for n in _hnode(r, "clustRegions.cluster") if n["params"]["valueNameSuffix"] == "immune")
    @test !any(e -> e["from"] == regions_immune["id"], r["template"]["edges"])
    # B/qc and T/qc are read everywhere and produced by nothing
    @test r["gateSteps"] == ["gate.B", "gate.T"]
    @test all(g -> only(n for n in r["template"]["nodes"] if n["id"] == g)["params"]["requiredPops"] == "/qc",
              r["gateSteps"])
    # two region clusterings are alternatives, not a fork: keep one
    @test all(f -> !f["multi"], [f for f in r["finalOutputs"] if f["fn"] == "clustRegions.cluster"])
    # clustTracks ran before params were recorded — reported, not replayed with {}
    @test isempty(_hnode(r, "clustTracks.cluster"))
    @test any(n -> n["fn"] == "clustTracks.cluster" && occursin("params", n["why"]), r["notReplayed"])
    rm(proj.root; recursive = true)
end

@testset "Chain from history — exemplars that disagree are a conflict, not a guess" begin
    # the real UMq4v3 log, and a copy of it where the tracking miss rate was tuned differently
    proj, uids = _history_project("u" => "zolIMa_UMq4v3", "v" => "zolIMa_UMq4v3")
    v = init_object(proj.uid, uids["v"])
    log = read_run_log(v)
    for e in log
        e["fun"] == "tracking.bayesian_track_measures" && (e["params"]["segmentationMissRate"] = 0.2)
    end
    write(Cecelia.run_log_path(v._dir), JSON3.write(log))
    r = chain_from_history(proj, [uids["u"], uids["v"]])
    c = [c for c in r["conflicts"] if c["param"] == "segmentationMissRate"]
    @test length(c) == 2                                    # one per tracking step (frags, cells)
    @test Set(values(first(c)["values"])) == Set([0.1, 0.2])
    rm(proj.root; recursive = true)
end

# ── Parameter table from history (tasks/chain/history_params.jl) ──
_hprow(t, fn) = [r for r in t if r["task"] == fn]

@testset "History params table — only what differs from the spec default" begin
    proj, uids = _history_project("x" => "zolIMa_x4E5HU")
    t = history_params_table(proj, [uids["x"]])
    drift = only(_hprow(t, "cleanupImages.driftCorrect"))
    # the last done drift run; driftMaxLag 3 and driftMaxAngle 5 (spec 5.0) are defaults, so absent
    @test drift["changedFromDefault"] ==
          "driftChannel=CD169-Kat; driftPerPlane=true; driftSmoothSigma=0; driftZSmoothness=0.3"
    @test drift["images"] == "1/1" && drift["output"] == "driftCorrected" && isempty(drift["conflicts"])
    # the training step a chain would reuse is still a row: it is part of the method
    @test !isempty(_hprow(t, "opticalFlow.trainSupportDenoise"))
    rm(proj.root; recursive = true)
end

@testset "History params table — repeatable group entries against their own position's defaults" begin
    proj, uids = _history_project("u" => "zolIMa_UMq4v3")
    seg = only(_hprow(history_params_table(proj, [uids["u"]]), "segment.coastalMeasure"))
    ch = split(seg["changedFromDefault"], "; ")
    @test "models[0].seedSize=8" in ch && "models[1].seedSize=4" in ch
    # entry 1's spec seed (entryDefaults) is 0.9 — what ran — so it is not a change
    @test !any(startswith("models[1].mergeAffinityThreshold="), ch)
    @test "models[0].model=flow.small.pt" in ch
    rm(proj.root; recursive = true)
end

@testset "History params table — disagreement is a counted spread; unrecorded params say so" begin
    proj, uids = _history_project("u" => "zolIMa_UMq4v3", "v" => "zolIMa_UMq4v3")
    v = init_object(proj.uid, uids["v"])
    log = read_run_log(v)
    for e in log
        e["fun"] == "tracking.bayesian_track_measures" && (e["params"]["segmentationMissRate"] = 0.2)
    end
    write(Cecelia.run_log_path(v._dir), JSON3.write(log))
    trk = _hprow(history_params_table(proj, [uids["u"], uids["v"]]), "tracking.bayesian_track_measures")
    @test length(trk) == 2 && all(r -> r["images"] == "2/2", trk)
    @test all(r -> occursin("segmentationMissRate: ", r["conflicts"]) && occursin("0.1 (1)", r["conflicts"]) &&
                   occursin("0.2 (1)", r["conflicts"]), trk)
    @test all(r -> !occursin("segmentationMissRate", r["changedFromDefault"]), trk)
    rm(proj.root; recursive = true)

    proj, uids = _history_project("a" => "4kS67f_LUkCpP", "b" => "4kS67f_k3Tx90")
    ct = only(_hprow(history_params_table(proj, [uids["a"], uids["b"]]), "clustTracks.cluster"))
    @test ct["note"] == "parameters unknown — ran before params were recorded"
    rm(proj.root; recursive = true)
end

@testset "History params table — a renamed param reads under its current key (formerKeys)" begin
    # the real 4kS67f region runs were saved as `mergeUmap` (now `createUmap`, formerKeys) = true, the default
    proj, uids = _history_project("a" => "4kS67f_LUkCpP", "b" => "4kS67f_k3Tx90")
    rows = _hprow(history_params_table(proj, [uids["a"], uids["b"]]), "clustRegions.cluster")
    @test !isempty(rows)
    @test all(r -> !occursin("mergeUmap", r["changedFromDefault"]) && !occursin("createUmap", r["changedFromDefault"]), rows)
    # the chain template holds them under the current key, the one its conflicts name
    nodes = _hnode(chain_from_history(proj, [uids["a"], uids["b"]]), "clustRegions.cluster")
    @test !isempty(nodes) && all(n -> haskey(n["params"], "createUmap") && !haskey(n["params"], "mergeUmap"), nodes)
    # one image re-saved under the new key with the same value: still one param, no conflict;
    # the other set to a non-default value: reported under the current name
    a, b = init_object(proj.uid, uids["a"]), init_object(proj.uid, uids["b"])
    la, lb = read_run_log(a), read_run_log(b)
    for e in la
        e["fun"] == "clustRegions.cluster" && haskey(e["params"], "mergeUmap") &&
            (e["params"]["createUmap"] = pop!(e["params"], "mergeUmap"))
    end
    write(Cecelia.run_log_path(a._dir), JSON3.write(la))
    rows = _hprow(history_params_table(proj, [uids["a"], uids["b"]]), "clustRegions.cluster")
    @test all(r -> !occursin("Umap", r["conflicts"]) && !occursin("Umap", r["changedFromDefault"]), rows)
    # the chain built from the same history: no exemplar conflict between the two spellings either
    @test !any(c -> occursin("Umap", c["param"]), chain_from_history(proj, [uids["a"], uids["b"]])["conflicts"])
    for e in lb
        e["fun"] == "clustRegions.cluster" && haskey(e["params"], "mergeUmap") && (e["params"]["mergeUmap"] = false)
    end
    write(Cecelia.run_log_path(b._dir), JSON3.write(lb))
    # the spellings now disagree in value: one conflict, named by a key the template node holds
    r = chain_from_history(proj, [uids["a"], uids["b"]])
    cs = [c for c in r["conflicts"] if occursin("Umap", c["param"])]
    @test !isempty(cs) && all(c -> c["param"] == "createUmap", cs)
    @test all(c -> haskey(only(n for n in r["template"]["nodes"] if n["id"] == c["nodeId"])["params"], "createUmap"), cs)
    rows =_hprow(history_params_table(proj, [uids["b"]]), "clustRegions.cluster")
    @test all(r -> occursin("createUmap=false", r["changedFromDefault"]) && !occursin("mergeUmap", r["changedFromDefault"]), rows)
    rm(proj.root; recursive = true)
end

@testset "Chain from history — a renamed input picker keys one step, not two" begin
    lv = Dict{String,Cecelia._HistoryLeaf}(
        "inputName" => (; type = "valueNameSelection", ns = "", writes = false, accepts = String[], former = ["oldInput"]))
    old = Cecelia._history_inputs(Dict{String,Any}("oldInput" => "denoised"), lv)
    new = Cecelia._history_inputs(Dict{String,Any}("inputName" => "denoised"), lv)
    @test old == new == "inputName=\"denoised\""
    # both spellings saved: the current one wins (`_stored_key`'s precedence)
    @test Cecelia._history_current_params(Dict{String,Any}("oldInput" => "a", "inputName" => "b"), lv) ==
          Dict{String,Any}("inputName" => "b")
    # two former spellings saved: the first listed wins, as `_stored_key` says — not Dict order
    lv2 = Dict{String,Cecelia._HistoryLeaf}(
        "inputName" => (; type = "valueNameSelection", ns = "", writes = false, accepts = String[], former = ["older", "old"]))
    for p in (Dict{String,Any}("old" => "b", "older" => "a"), Dict{String,Any}("older" => "a", "old" => "b"))
        @test Cecelia._history_current_params(p, lv2) == Dict{String,Any}("inputName" => "a")
    end
end

@testset "History params table — cut to what a board shows" begin
    # a /qc/cells plot drew the /qc/cells tracking, the segmentation it sits on and the model that
    # segmented — not the /qc/frags tracking
    proj, uids = _history_project("u" => "zolIMa_UMq4v3")
    t = history_params_table(proj, [uids["u"]]; pops = ["live::Bcells/qc/cells/_tracked"])
    @test sort([r["task"] for r in t]) ==
          ["opticalFlow.train", "segment.coastalMeasure", "tracking.bayesian_track_measures"]
    trk = only(_hprow(t, "tracking.bayesian_track_measures"))
    @test occursin("popsToTrack=/qc/cells", trk["changedFromDefault"])
    # the segmentation (and its model) made the cells; tracking ran on them
    @test trk["relation"] == "ran on" && only(_hprow(t, "segment.coastalMeasure"))["relation"] == "made" &&
          only(_hprow(t, "opticalFlow.train"))["relation"] == "made"
    @test all(r -> r["relation"] == "", history_params_table(proj, [uids["u"]]))
    # a segmentation QC plot draws the objects: what ran on them since is not part of it
    @test sort([r["task"] for r in history_params_table(proj, [uids["u"]]; pops = ["labels::Bcells/labels"])]) ==
          ["opticalFlow.train", "segment.coastalMeasure"]
    # the label set's root population: every tracking on it
    @test length(_hprow(history_params_table(proj, [uids["u"]]; pops = ["live::Bcells/"]),
                        "tracking.bayesian_track_measures")) == 2
    rm(proj.root; recursive = true)

    # the segmentation walks back through denoise (and its training) and drift; stack-align fed nothing
    proj, uids = _history_project("x" => "zolIMa_x4E5HU")
    t = history_params_table(proj, [uids["x"]]; pops = ["labels::flowTom/labels"])
    @test sort([r["task"] for r in t]) == ["cleanupImages.denoise", "cleanupImages.driftCorrect",
                                           "opticalFlow.trainSupportDenoise", "segment.coastalMeasure"]
    @test isempty(history_params_table(proj, [uids["x"]]; pops = String[]))
    # every version on the way was written by a recorded step, so no gap rows
    @test !any(r -> isempty(r["task"]), t)
    # one that wasn't: segmentation read a version whose writer is not in the log
    v = init_object(proj.uid, uids["x"])
    log = [e for e in read_run_log(v) if e["fun"] != "cleanupImages.denoise"]
    write(Cecelia.run_log_path(v._dir), JSON3.write(log))
    t = history_params_table(proj, [uids["x"]]; pops = ["labels::flowTom/labels"])
    @test any(r -> r["step"] == "image version denoised" && startswith(r["note"], "no recorded run"), t)
    rm(proj.root; recursive = true)

    # a region population is a filter on one run's column: that run, not the other region run
    proj, uids = _history_project("a" => "4kS67f_LUkCpP")
    img = init_object(proj.uid, uids["a"])
    mkpath(Cecelia.gating_dir(img._dir))
    cp(normpath(joinpath(@__DIR__, "..", "..", "..", "test-data", "run-logs", "4kS67f_LUkCpP_B__region.json")),
       gating_path(img._dir, "B"; pop_type = "region"))
    reg(t) = [r["output"] for r in _hprow(t, "clustRegions.cluster")]
    @test reg(history_params_table(proj, [uids["a"]]; pops = ["region::B/Population 1"])) == ["default"]
    # a board's cluster panels name their run directly
    @test reg(history_params_table(proj, [uids["a"]]; pops = String[], cluster_runs = ["region:immune"])) == ["immune"]
    # a run the board draws that nothing recorded made is a row saying so, beside that task's
    # unrecorded runs — never silently left out
    t = history_params_table(proj, [uids["a"]]; pops = ["live::B/qc/_tracked"], cluster_runs = ["trackclust:movement"])
    gap = only(r for r in t if r["step"] == "clusters.movement")
    @test startswith(gap["note"], "no recorded run made this")
    @test only(_hprow(t, "clustTracks.cluster"))["note"] == "parameters unknown — ran before params were recorded"
    @test any(r -> r["step"] == "label set B", t)          # this log holds no segmentation of B
    rm(proj.root; recursive = true)
end

# ── Board export provenance (analysis_export_provenance.jl) ──
@testset "Board export provenance — runs carry their task ids, populations their gate files" begin
    proj, uids = _history_project("u" => "zolIMa_UMq4v3")
    img = init_object(proj.uid, uids["u"])
    log = read_run_log(img)
    for (i, e) in enumerate(log)
        e["taskId"] = "task-$i"
    end
    write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    pops = ["live::Bcells/qc/cells/_tracked"]
    # the parameter table names the run behind each row
    trk = only(_hprow(history_params_table(proj, [uids["u"]]; pops), "tracking.bayesian_track_measures"))
    want = [e["taskId"] for e in log if e["fun"] == "tracking.bayesian_track_measures" &&
            get(e["params"], "popsToTrack", "") == "/qc/cells" && get(e, "status", "done") == "done"]
    @test trk["taskIds"] == [want[end]]
    # no gate file yet: the population is named, with nothing to point at
    rec = board_export_provenance(proj, [uids["u"]]; pops)
    @test rec["project"]["uid"] == proj.uid && rec["images"][1]["uid"] == uids["u"]
    @test rec["ceceliaVersion"] == cecelia_version()
    p = only(rec["populations"])
    @test p["valueName"] == "Bcells" && p["pop"] == "/qc/cells/_tracked" && isempty(p["gateFiles"])
    @test only(r for r in rec["runs"] if r["task"] == "tracking.bayesian_track_measures")["taskIds"] == [want[end]]
    # a `live` population is defined by the flow gating sidecar — project-relative, forward slashes
    gp = Cecelia.gating_path(img._dir, "Bcells"; pop_type = "live")
    mkpath(dirname(gp)); write(gp, "{}")
    g = only(only(board_export_provenance(proj, [uids["u"]]; pops)["populations"])["gateFiles"])
    @test !isabspath(g) && !occursin('\\', g) && endswith(g, "gating/Bcells.json")
    @test isfile(joinpath(proj.root, g))
    # a segmentation QC key draws the objects: no gate behind it
    @test isempty(only(board_export_provenance(proj, [uids["u"]]; pops = ["labels::Bcells/labels"])["populations"])["gateFiles"])
    rm(proj.root; recursive = true)
end
