# ROI auto-discovery + reviews (docs/todo/ROI_TRIAGE_PLAN.md): the task's params, weights and QC, and
# the review guard rules behind POST /api/rois/review — who may write what, history, undo, orphans.

const _ROI_PERSON = Dict{String,Any}("profile" => "alice", "via" => "app")
const _ROI_CLAUDE = Dict{String,Any}("profile" => "alice", "via" => "claude")
_roi_fixture() = Dict{String,Any}("id" => "r00000001", "t0" => 10, "t1" => 20,
    "objects" => Any[Dict{String,Any}("label" => 5), Dict{String,Any}("label" => 7)])
_roi_claude_label(; kw...) = merge(Dict{String,Any}("type" => "engulfment", "confidence" => 0.8,
    "reason" => "fragment inside the CD169 process", "objectIds" => [5], "frames" => [12]), Dict{String,Any}(String(k) => v for (k, v) in kw))

@testset "roiDiscovery params, weights and QC" begin
    p = Cecelia.parse_roi_discovery_params(Dict{String,Any}())
    @test p.candidateQuantile == 0.95 && p.windowFrames == 12 && p.roisSuffix == "default"
    # a cue whose input is missing takes no share of the score
    w = Cecelia._roi_weights(p; has_mask = false, has_partner = false, has_naked = false)
    @test w["contact"] == 0 && w["partner"] == 0 && w["naked"] == 0 && w["density"] == 1
    @test Cecelia._roi_weights(p; has_mask = true, has_partner = true, has_naked = true)["contact"] == 2

    good = Dict{String,Any}("candidatePops" => ["B/qc/frags"], "roisSuffix" => "frags")
    @test begin validate_params(RoiDiscovery(), good); true end
    @test_throws ParamValidationError validate_params(RoiDiscovery(), merge(good, Dict{String,Any}("candidateQuantile" => 2.0)))

    codes(f) = Set(String(x["code"]) for x in f)
    @test codes(Cecelia._roi_discovery_findings(0, 0, nothing)) == Set(["rois.no_candidates"])
    @test codes(Cecelia._roi_discovery_findings(100, 0, nothing)) == Set(["rois.none_found"])
    @test codes(Cecelia._roi_discovery_findings(100, 5, 0.4)) == Set(["rois.mask_too_large"])
    @test isempty(Cecelia._roi_discovery_findings(100, 5, 0.01))
end

@testset "ROI review: the person decides, Claude proposes" begin
    roi = _roi_fixture()
    rv = Dict{String,Any}()
    # Claude labels and flags; it cannot accept, reject, or write the person's fields
    @test apply_roi_review!(rv, roi, Dict{String,Any}("claudeLabel" => _roi_claude_label()), _ROI_CLAUDE)[1] == 200
    @test roi_status(rv["r00000001"]) == "claude-labelled"
    @test rv["r00000001"]["claudeLabel"]["by"]["via"] == "claude"
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "accepted"), _ROI_CLAUDE)[1] == 409
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "rejected"), _ROI_CLAUDE)[1] == 409
    @test apply_roi_review!(rv, roi, Dict{String,Any}("userLabel" => "artifact"), _ROI_CLAUDE)[1] == 409
    @test apply_roi_review!(rv, roi, Dict{String,Any}("note" => "x"), _ROI_CLAUDE)[1] == 409
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "flagged"), _ROI_CLAUDE)[1] == 200
    # the person accepts; Claude can no longer flag it
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "accepted", "userLabel" => "engulfment"), _ROI_PERSON)[1] == 200
    @test roi_status(rv["r00000001"]) == "accepted"
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "flagged"), _ROI_CLAUDE)[1] == 409
    # every change is attributed history
    hist = rv["r00000001"]["history"]
    @test [h["field"] for h in hist] == ["claudeLabel", "status", "status", "userLabel"]
    @test hist[end]["by"]["via"] == "app"
end

@testset "ROI review: hallucinated ids / frames and bad values are refused" begin
    roi = _roi_fixture(); rv = Dict{String,Any}()
    bad(cl) = apply_roi_review!(rv, roi, Dict{String,Any}("claudeLabel" => cl), _ROI_CLAUDE)[1]
    @test bad(_roi_claude_label(objectIds = [5, 99])) == 400     # 99 is not in this ROI
    @test bad(_roi_claude_label(frames = [25])) == 400           # outside t0..t1
    @test bad(_roi_claude_label(type = "cat")) == 400
    @test bad(_roi_claude_label(confidence = 1.5)) == 400
    @test bad(_roi_claude_label(reason = " ")) == 400
    @test apply_roi_review!(rv, roi, Dict{String,Any}("status" => "done"), _ROI_PERSON)[1] == 400
    @test apply_roi_review!(rv, roi, Dict{String,Any}("userLabel" => "cat"), _ROI_PERSON)[1] == 400
    @test apply_roi_review!(rv, roi, Dict{String,Any}("note" => repeat("x", 3000)), _ROI_PERSON)[1] == 400
    @test !haskey(get(rv, "r00000001", Dict()), "claudeLabel")   # nothing stored by a refused write
end

@testset "ROI review: undo is reversible and Claude can only undo its own latest change" begin
    roi = _roi_fixture(); rv = Dict{String,Any}()
    @test apply_roi_review!(rv, roi, Dict{String,Any}("undo" => true), _ROI_PERSON)[1] == 409   # empty
    apply_roi_review!(rv, roi, Dict{String,Any}("status" => "flagged"), _ROI_CLAUDE)
    apply_roi_review!(rv, roi, Dict{String,Any}("note" => "check z"), _ROI_PERSON)
    # the latest change is the person's → Claude may not reach past it
    @test apply_roi_review!(rv, roi, Dict{String,Any}("undo" => true), _ROI_CLAUDE)[1] == 409
    @test apply_roi_review!(rv, roi, Dict{String,Any}("undo" => true), _ROI_PERSON)[1] == 200
    @test !haskey(rv["r00000001"], "note")
    @test rv["r00000001"]["history"][end]["field"] == "note"     # the undo is itself recorded
    @test rv["r00000001"]["status"] == "flagged"
end

@testset "ROI list payload: reviews merge by id, orphans are kept" begin
    proj = create_project!(name = "rois-test-$(rand(1000:9999))")
    s = add_set!(proj; name = "s"); img = add_image!(s; name = "a"); save!(img)
    try
        mkpath(img_rois_dir(img))
        set = Dict{String,Any}("version" => 1, "rois" => Any[merge(_roi_fixture(), Dict{String,Any}("rank" => 1))])
        write_json_atomic(img_rois_path(img, "default"), set)
        @test roi_review!(img, "default", "r00000001", Dict{String,Any}("status" => "rejected"), _ROI_PERSON)[1] == 200
        @test roi_review!(img, "default", "rNOPE", Dict{String,Any}("status" => "rejected"), _ROI_PERSON)[1] == 404
        @test roi_review!(img, "missing", "r00000001", Dict{String,Any}("status" => "rejected"), _ROI_PERSON)[1] == 404
        out = roi_list_payload(img, "default")
        @test out["rois"][1]["status"] == "rejected"
        @test out["rois"][1]["objectLabels"] == [5, 7]
        @test !haskey(out["rois"][1], "objects")
        @test haskey(roi_list_payload(img, "default"; objects = true)["rois"][1], "objects")
        @test isempty(out["orphaned"])
        # a re-run with other params writes other ids; the old review is reported, not dropped
        write_json_atomic(img_rois_path(img, "default"),
                          Dict{String,Any}("rois" => Any[merge(_roi_fixture(), Dict{String,Any}("id" => "r00000002"))]))
        out2 = roi_list_payload(img, "default")
        @test out2["rois"][1]["status"] == "pending"
        @test haskey(out2["orphaned"], "r00000001")
        @test isfile(img_roi_reviews_path(img, "default"))
    finally
        rm(proj.root; recursive = true, force = true)
    end
end
