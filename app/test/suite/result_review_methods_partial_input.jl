# RESULT-REVIEW: result-52df57f7
# A step whose images read different inputs: one image segmented on its drift-corrected version, the
# other (run later) on raw. The drift correction fed half the set's segmentation, so it is part of
# the analysis, not a trial (`_history_usage` reads every image's params, not only the newest run's).

function _mr_two_image_log(edit!)
    proj, s, imgs = _mr_project("RFtn9f_TKufLV", 2)
    src = read_run_log(imgs[1])
    pick(id) = deepcopy(only(filter(e -> e["taskId"] == id, src)))
    imp, drift, seg = pick("pB1ncZ"), pick("AvycPt"), pick("xuTaCr")
    seg["fun"] = "segment.cellpose"
    seg2 = deepcopy(seg)
    seg2["taskId"] = "SEG002"; seg2["at"] = "2026-11-16T10:00:00"; seg2["valueName"] = "default"
    seg2["params"]["intensityValueName"] = "default"
    seg2["inputs"] = [Dict("name" => "default", "param" => "valueName", "version" => "v1")]
    logs = [[imp, drift, seg], [imp, seg2]]
    edit!(logs)
    for (img, log) in zip(imgs, logs)
        write(Cecelia.run_log_path(img._dir), JSON3.write(log))
    end
    proj, s, imgs
end

@testset "Methods report — a step that fed part of the set is the analysis" begin
    proj, s, imgs = _mr_two_image_log(_ -> nothing)
    t = history_params_table(proj, [i.uid for i in imgs]; usage_runs = Set{Tuple{String,String,String}}())
    drift = only(row for row in t if row["task"] == "cleanupImages.driftCorrect")
    @test drift["images"] == "1/2"
    @test drift["usage"] == ""
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test !occursin(r"Left out:.*Drift correction", md)
    rm(proj.root; recursive = true)
end
