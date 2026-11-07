# RESULT-REVIEW: result-a5ea0622
# Claim checks' same-source rule (`_claim_same_source`, app/src/claim_checks.jl) must be per POPULATION
# across images: B's segmentation having the run-log record must not strip T's params when every T row
# reads the same fallback source. T segmented differently on one image is still a difference.
# Uses the `_cc_*` fixture helpers from claim_checks.jl (included just before this file).

@testset "claim checks: one population's run-log record does not hide another's difference" begin
    segtask = Cecelia._task_from_fun_name("segment.cellposeMeasure")
    seg(vn, d) = Dict{String,Any}("outputValueName" => vn, "valueName" => "default",
        "models" => Dict("0" => Dict("model" => "cyto2", "cellDiameter" => d,
                                     "cellChannels" => [vn == "B" ? "Channel 2" : "Channel 3"])))
    imgs = _cc_clean()
    for img in imgs     # B: the run-log record on every image
        append_run_log!(img, "segment.cellposeMeasure", "default", "done",
                        Cecelia.resolve_run_params(segtask, seg("B", 10), img))
    end
    # T: the fallback blob on every image (one source, one shape) — and image c segmented at 35
    imgs[3].meta["funParamsByName"]["segment.cellposeMeasure"]["T"] = seg("T", 35)
    r = _cc_run(imgs)
    f = filter(x -> x["detail"]["evidence"]["pop"] == "T/qc" &&
                    occursin("cellDiameter", x["detail"]["evidence"]["variable"]),
               _cc_find(r, "claim.differs_across_images"))
    @test length(f) == 1 && f[1]["detail"]["evidence"]["images"] == ["c"]
end
