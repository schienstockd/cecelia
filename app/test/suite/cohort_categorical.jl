# Cohort QC for a CATEGORICAL metric (qc_cohort.jl `_cohort_minority`, `COHORT_CATEGORICAL`):
# a set whose track measures mix 2D and 3D. Own file because ome_qc.jl carries a result-review
# repro, which the commit hook protects from edits.
@testset "cohort categorical metrics" begin
    @testset "cohort categorical metric: a set that mixes motion dims" begin
        # 4 images measured in 2D, 2 in 3D: the 3D pair is flagged, the majority isn't
        r = Cecelia._cohort_minority(Dict("a"=>2.0,"b"=>2.0,"c"=>2.0,"d"=>2.0,"e"=>3.0,"f"=>3.0))
        @test r.median == 2.0 && Set(keys(r.outliers)) == Set(["e","f"])
        @test r.outliers["e"]["value"] == 3.0 && r.outliers["e"]["majority"] == 2.0
        # the reverse majority flags the 2D minority — the relative-departure fallback of
        # `_cohort_outliers` would miss this direction (|2-3|/3 < 0.5), which is why it's routed here
        r2 = Cecelia._cohort_minority(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0,"d"=>3.0,"e"=>2.0))
        @test Set(keys(r2.outliers)) == Set(["e"])
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0,"d"=>3.0,"e"=>2.0)).outliers)
        # a tie has no majority: every image is flagged
        rt = Cecelia._cohort_minority(Dict("a"=>2.0,"b"=>2.0,"c"=>3.0,"d"=>3.0))
        @test rt.median === nothing && length(rt.outliers) == 4
        # agreement, or too few to call a set, flags nothing
        @test isempty(Cecelia._cohort_minority(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0)).outliers)
        @test isempty(Cecelia._cohort_minority(Dict("a"=>2.0,"b"=>3.0)).outliers)

        # the finding names both values as integers and says the set isn't comparable
        f = Cecelia._cohort_finding("motionDims", Dict{String,Any}("value"=>3.0,"majority"=>2.0), 2.0)
        @test f["code"] == "cohort.motionDims" && f["level"] == "warn"
        @test occursin("motionDims 3", f["long"]) && occursin("unlike most of the set", f["long"])
        @test !occursin("3.0", f["long"]) && f["detail"]["majority"] == 2.0
        ft = Cecelia._cohort_finding("motionDims", Dict{String,Any}("value"=>2.0,"majority"=>nothing), nothing)
        @test ft["code"] == "cohort.motionDims" && ft["detail"]["majority"] === nothing
        # every placeholder the new copy uses is one the caller passes (the catalog test's rule)
        @test all(m -> m.captures[1] in ("metric", "value"),
                  eachmatch(r"\{(\w+)\}", Cecelia.QC_TEXT["cohort.mixed"].long))

        # end to end through the banked QC: motionDims is a registered track_measures metric
        @test "motionDims" in COHORT_METRICS["tracking.track_measures"]
        set = CciaSet(; dir = mktempdir())
        for (uid, d) in [("a",2),("b",2),("c",2),("d",3)]
            img = CciaImage(; uid = uid, dir = mktempdir())
            write_qc(img, "tracking.track_measures", "T", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks" => 50, "motionDims" => d))
            push!(set._images, img); push!(set.image_uids, uid)
        end
        doc = cohort_qc_for!(set, "tracking.track_measures", "T")
        @test Set(keys(doc["metrics"]["motionDims"]["outliers"])) == Set(["d"])
        byid = Dict(i.uid => i for i in set._images)
        fd = read_qc(byid["d"], "cohort.tracking.track_measures", "T")
        @test fd !== nothing && any(x -> x["code"] == "cohort.motionDims", fd["findings"])
        lines = cohort_qc_summary_lines(doc)
        @test any(l -> occursin("d — motionDims 3 (most of the set: 2)", l), lines)
    end
end
