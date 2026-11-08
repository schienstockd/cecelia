# Claim checks — what they compare beyond the segmentation/tracking params: the preprocessing steps
# that made the image a segmentation read (followed through the run log, not task names), every gate on
# the population's path (parents too), and whether the compared populations were measured in 2D or 3D.
# Same pattern as claim_checks.jl: every seeded fault has a clean twin. Uses its `_cc_*` helpers.

# the segmentation of B and T reads image version `input` (the fixture's by-name blob)
function _cc_reads!(img, input)
    for vn in ("B", "T")
        img.meta["funParamsByName"]["segment.cellposeMeasure"][vn]["valueName"] = input
    end
    img
end
_cc_denoise(batch; input = "default") = Dict{String,Any}("valueName" => input, "model" => "m1",
                                                         "channels" => ["Channel 2"], "batchSize" => batch)
_cc_drift(maxlag) = Dict{String,Any}("valueName" => "default", "driftChannel" => "Channel 1", "driftMaxLag" => maxlag)

@testset "claim checks: preprocessing, parent gates, 2D vs 3D" begin
    pre_vars(r) = [f for f in _cc_find(r, "claim.differs_across_images")
                   if startswith(f["detail"]["evidence"]["variable"], "params.cleanupImages.") ||
                      f["detail"]["evidence"]["variable"] == "preprocessing"]

    # ── every image denoised alike: quiet, and the lineage is on the row ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    for img in imgs
        append_run_log!(img, "cleanupImages.denoise", "default", "done", _cc_denoise(2))
    end
    r = _cc_run(imgs)
    @test isempty(_cc_codes(r))
    row = first(claim_record_table(ClaimScope(_cc_set(imgs), ["B/qc"], "live.track.speed")))
    @test row["vars"]["preprocessing"] == "cleanupImages.denoise"
    @test row["vars"]["params.cleanupImages.denoise.batchSize"] == 2
    @test row["paramsSource"]["preprocessing"]["cleanupImages.denoise"] == "runLog"

    # ── one image denoised with another setting: a difference, named by the step ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    for (i, img) in enumerate(imgs)
        append_run_log!(img, "cleanupImages.denoise", "default", "done", _cc_denoise(i == 3 ? 8 : 2))
    end
    f = pre_vars(_cc_run(imgs))
    @test length(f) == 1 && f[1]["detail"]["evidence"]["variable"] == "params.cleanupImages.denoise.batchSize"
    @test f[1]["detail"]["evidence"]["images"] == ["c"] && f[1]["short"] == "denoise: batchSize differs across images for B and T"

    # ── one image segmented on the raw image, the rest on the denoised one ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    for img in imgs
        append_run_log!(img, "cleanupImages.denoise", "default", "done", _cc_denoise(2))
    end
    _cc_reads!(imgs[4], "default")
    f = pre_vars(_cc_run(imgs))
    @test length(f) == 1 && f[1]["detail"]["evidence"]["variable"] == "preprocessing"
    @test f[1]["detail"]["evidence"]["images"] == ["d"] && f[1]["short"] == "Preprocessing steps differs across images for B and T"

    # ── a chain: drift → denoise, followed through what each step read ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    for (i, img) in enumerate(imgs)
        append_run_log!(img, "cleanupImages.driftCorrect", "default", "done", _cc_drift(i == 2 ? 9 : 5))
        append_run_log!(img, "cleanupImages.denoise", "driftCorrected", "done", _cc_denoise(2; input = "driftCorrected"))
    end
    r = _cc_run(imgs)
    f = pre_vars(r)
    @test length(f) == 1 && f[1]["detail"]["evidence"]["variable"] == "params.cleanupImages.driftCorrect.driftMaxLag"
    row = first(claim_record_table(ClaimScope(_cc_set(imgs), ["B/qc"], "live.track.speed")))
    @test row["vars"]["preprocessing"] == "cleanupImages.driftCorrect → cleanupImages.denoise"

    # ── a cleanup re-run AFTER the segmentation is not what the segmentation read ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    segtask = Cecelia._task_from_fun_name("segment.cellposeMeasure")
    for img in imgs
        append_run_log!(img, "cleanupImages.denoise", "default", "done", _cc_denoise(2); at = "2026-01-01T00:00:00")
        for vn in ("B", "T")
            p = img.meta["funParamsByName"]["segment.cellposeMeasure"][vn]
            append_run_log!(img, "segment.cellposeMeasure", "denoised", "done",
                            Cecelia.resolve_run_params(segtask, p, img); at = "2026-01-02T00:00:00")
        end
    end
    append_run_log!(imgs[5], "cleanupImages.denoise", "default", "done", _cc_denoise(8); at = "2026-02-01T00:00:00")
    @test isempty(pre_vars(_cc_run(imgs)))

    # ── a version the segmentation read that no run in the log made: a record gap, not a difference ──
    imgs = [_cc_reads!(img, "denoised") for img in _cc_clean()]
    for img in imgs[1:6]
        append_run_log!(img, "cleanupImages.denoise", "default", "done", _cc_denoise(2))
    end
    r = _cc_run(imgs)
    @test isempty(pre_vars(r))
    nr = only(_cc_find(r, "claim.params_not_recorded"))
    @test Set((x["pop"], x["kind"], x["images"]) for x in nr["detail"]["evidence"]["gaps"]) ==
          Set([("B/qc", "preprocessing", ["g"]), ("T/qc", "preprocessing", ["g"])])
    # …and the raw image is never a gap: the clean fixture reads `default`, with no run log at all
    @test isempty(_cc_find(_cc_run(_cc_clean()), "claim.params_not_recorded"))

    # ── a parent gate drawn differently on one image: the leaf alone would miss it ──
    nest!(img, parent_vertices) = for vn in ("B", "T")
        Cecelia.write_json_atomic(Cecelia.gating_path(img._dir, vn), Dict{String,Any}(
            "value_name" => vn, "pop_type" => "flow", "populations" => Any[Dict{String,Any}(
                "name" => "live", "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "volume",
                    "y_channel" => "area", "vertices" => parent_vertices),
                "children" => Any[Dict{String,Any}("name" => "qc", "children" => Any[],
                    "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "volume", "y_channel" => "area",
                                               "vertices" => [[0, 0], [1, 0], [1, 1]]))])]))
    end
    run_nested(imgs) = claim_checks(ClaimScope(_cc_set(imgs), ["B/live/qc", "T/live/qc"], "live.track.speed"; unit_attrs = ["Mouse"]))
    imgs = _cc_clean()
    foreach(img -> nest!(img, [[0, 0], [5, 0], [5, 5]]), imgs)
    clean = run_nested(imgs)
    @test isempty(_cc_codes(clean))
    row = first(claim_record_table(ClaimScope(_cc_set(imgs), ["B/live/qc"], "live.track.speed")))
    @test haskey(row["vars"], "gate.geometry") && haskey(row["vars"], "gate@/live::geometry")
    nest!(imgs[2], [[0, 0], [9, 0], [9, 9]])
    gd = only(_cc_find(run_nested(imgs), "claim.gate_differs"))
    @test gd["detail"]["evidence"]["variable"] == "gate@/live::geometry" && gd["detail"]["evidence"]["images"] == ["b"]
    @test gd["short"] == "Parent gate /live: shape differs across images for B and T"

    # ── 2D vs 3D between the compared populations ──
    # tracks: B 2D, T 3D on every image — one fact, saying it holds on every image
    r = _cc_run(_cc_clean(; dims = (B = 2, T = 3)))
    @test _cc_codes(r) == ["claim.dims_differ"]
    dd = only(_cc_find(r, "claim.dims_differ"))
    @test dd["detail"]["check"] == "S1.dims" && dd["detail"]["evidence"]["everyImage"] == true
    @test dd["short"] == "Track dimensions (2D/3D) differs between the groups on 7 images"
    @test "S1.dims" in _cc_run(_cc_clean())["ran"]
    # …on one image only
    imgs = _cc_clean(); imgs[2] = _cc_image("b"; mouse = "2", dims = (B = 2, T = 3))
    dd = only(_cc_find(_cc_run(imgs), "claim.dims_differ"))
    @test dd["detail"]["evidence"]["images"] == ["b"] && dd["detail"]["evidence"]["everyImage"] == false
    # cells: the label table's centroid axes say 2D or 3D — read from a real table, then the rules
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    have_fixture(h5) ? (@test Cecelia._claim_measure_dims(h5) == 3) : (@test_skip "measure dims (fixture missing)")
    @test Cecelia._claim_measure_dims(joinpath(mktempdir(), "none.h5ad")) === nothing
    sc = ClaimScope(_cc_set(_cc_clean()), ["B/qc", "T/qc"], "area"; unit_attrs = ["Mouse"])
    rows = claim_record_table(sc)
    for row in rows
        row["hasData"] = true
        row["vars"]["measureDims"] = (row["pop"] == "T/qc" && row["image"] == "c") ? 3 : 2
    end
    r = claim_checks(rows, sc)
    dd = only(_cc_find(r, "claim.dims_differ"))
    @test dd["detail"]["evidence"]["variable"] == "measureDims" && dd["detail"]["evidence"]["images"] == ["c"]
    @test occursin("Measured dimensions (2D/3D)", dd["short"])
    @test isempty(_cc_find(r, "claim.differs_between_groups"))       # one fact, not two
    # …no dims on record: skipped, and says why
    foreach(row -> delete!(row["vars"], "measureDims"), rows)
    @test haskey(claim_checks(rows, sc)["skipped"], "S1.dims")
    # one population: nothing to compare
    @test haskey(claim_checks(ClaimScope(_cc_set(_cc_clean()), ["B/qc"], "live.track.speed"))["skipped"], "S1.dims")
end
