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
    @test all(f -> f["multi"], tf) && sort([f["variant"] for f in tf]) == ["/qc/cells", "/qc/frags"]
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
