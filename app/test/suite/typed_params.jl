# ── Typed params testsets (per task family) ───────────────────────────
# Six sections covering the parse_*_params boundary contract per task family:
# cleanupImages, editImages, tracking, segment, opticalFlow, and remaining families.
# Extracted from suite.jl to keep it small enough to merge without EOF conflicts on every
# append. The extracted file loads inside this file's aggregating testset scope, so any
# helpers defined earlier in suite.jl are still in scope (lexical include).

@testset "typed params — cleanupImages" begin
    # Smooth: full dict → every field carries through; empty dict → defaults.
    let p = Cecelia.parse_smooth_params(Dict{String,Any}(
            "valueName"           => "driftCorrected",
            "channels"            => ["mem-TOM"],
            "spatialMethod"       => "bilateral_vst",
            "spatialSigma"        => 2.5,
            "bilateralColor"      => 12.0,
            "bilateralReach"      => 4.0,
            "bilateralPolish"     => 0.7,
            "temporalFrames"      => 3,
            "temporalStat"        => "mean",
            "farnebackMaxShiftPx" => 6.5,
            "restoreDynamicRange" => false))
        @test p.valueName == "driftCorrected"
        @test p.channels == ["mem-TOM"]
        @test p.spatialMethod == "bilateral_vst"
        @test p.spatialSigma === 2.5
        @test p.temporalFrames === 3
        @test p.restoreDynamicRange === false
    end
    let p = Cecelia.parse_smooth_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.spatialMethod == "gaussian"
        @test p.temporalFrames === 1
        @test p.restoreDynamicRange === true
    end

    # StackAlign
    let p = Cecelia.parse_stack_align_params(Dict{String,Any}(
            "alignChannel" => ["CD169"], "referenceMode" => "first",
            "minConfidence" => 0.5, "maxShiftPx" => 12.0))
        @test p.alignChannel == ["CD169"]
        @test p.referenceMode == "first"
        @test p.minConfidence === 0.5
        @test p.maxShiftPx === 12.0
    end

    # FlowRegister
    let p = Cecelia.parse_flow_register_params(Dict{String,Any}(
            "registerChannel" => ["CD169"], "structuralChannels" => ["SHG"],
            "referenceMode" => "first", "aggressiveness" => "gentle",
            "pyrLevels" => 3, "maxShiftPx" => 20.0))
        @test p.registerChannel == ["CD169"]
        @test p.structuralChannels == ["SHG"]
        @test p.aggressiveness == "gentle"
        @test p.pyrLevels === 3
    end

    # Denoise
    let p = Cecelia.parse_denoise_params(Dict{String,Any}(
            "model" => "supp.MERTK", "channels" => ["mem-TOM"], "batchSize" => 4))
        @test p.model == "supp.MERTK"
        @test p.channels == ["mem-TOM"]
        @test p.batchSize === 4
    end

    # DriftCorrect
    let p = Cecelia.parse_drift_correct_params(Dict{String,Any}(
            "driftChannel" => ["mem-TOM"], "driftEstimator" => "sitkRigid",
            "driftMaxAngle" => 3.0, "driftPerPlane" => true, "driftZSmoothness" => 0.5))
        @test p.driftChannel == ["mem-TOM"]
        @test p.driftEstimator == "sitkRigid"
        @test p.driftMaxAngle === 3.0
        @test p.driftPerPlane === true
        @test p.driftZSmoothness === 0.5
    end

    # AfCorrect + the two typed structs at its edges (task #21 AfChannelStats, task #22
    # AfCombinationSpec — see docs/archive/comment-audit-findings.md → Tier 1 boundary bags).
    let p = Cecelia.parse_af_correct_params(Dict{String,Any}(
            "backgroundMethod" => "otsu"))
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.backgroundMethod == "otsu"
    end
    let specs = Cecelia.parse_af_combinations(Dict{String,Any}(
            "afCombinations" => Dict{String,Any}(
                "CD169-Kat" => Dict{String,Any}(
                    "targetChannel"     => ["CD169-Kat"],
                    "competingChannels" => ["SHG", "CH4"]),
                "malformed" => "not a dict")))
        @test length(specs) == 1
        @test specs[1].key == "CD169-Kat"
        @test specs[1].targetChannel == ["CD169-Kat"]
        @test specs[1].competingChannels == ["SHG", "CH4"]
    end
    # AfChannelStats round-trips the Python-side per-channel block and carries a Dict{String,Float64}
    # bleedthrough. A missing bleedthrough is a legal shape (no leaks detected) → empty Dict.
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}(
            "saturatedFrac" => 0.02, "levelsUsed" => 3200.0, "levelsAvailable" => 4096.0,
            "bleedthrough" => Dict{String,Any}("SHG" => 0.15, "CD169" => 0.03)))
        @test s.saturatedFrac === 0.02
        @test s.levelsUsed === 3200.0
        @test s.levelsAvailable === 4096.0
        @test s.bleedthrough == Dict("SHG" => 0.15, "CD169" => 0.03)
    end
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}())
        @test s.saturatedFrac === 0.0
        @test s.levelsAvailable === 1.0    # min-1 floor so `used / avail` never divides by zero
        @test isempty(s.bleedthrough)
    end
end

# Typed per-task params structs for the editImages family. Each parser is the ONE place its task
# reads the params bag — a spec rename that isn't mirrored here becomes a struct-field error rather
# than a silent default at the read site. Same pattern as `typed params — cleanupImages`.
@testset "typed params — editImages" begin
    # BinImage
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}(
            "valueName" => "corrected", "factorX" => 4, "factorY" => 2, "op" => "sum"))
        @test p.valueName == "corrected"
        @test p.factorX === 4
        @test p.factorY === 2
        @test p.op == "sum"
    end
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}())
        @test p.factorX === 2 && p.factorY === 2 && p.op == "mean"
    end

    # CopyImage — exactly-one-of toSetUid/newSetName is a runtime check (not enforced by the type),
    # so both empty is a legal parse (the handler catches it).
    let p = Cecelia.parse_copy_image_params(Dict{String,Any}(
            "valueName" => "corrected", "newSetName" => "  Day 3  "))
        @test p.valueName == "corrected"
        @test p.toSetUid == ""
        @test p.newSetName == "Day 3"          # whitespace stripped
    end

    # CropImage — nested CropBox with the -1 keep-axis convention.
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}(
            "cropBox" => Dict{String,Any}(
                "x0"=>10, "x1"=>200, "y0"=>0, "y1"=>256, "z0"=>-1, "z1"=>-1, "t0"=>5, "t1"=>20)))
        b = p.cropBox
        @test b.x0 === 10 && b.x1 === 200
        @test b.y0 === 0  && b.y1 === 256
        @test b.z0 === -1 && b.z1 === -1        # 2D image: keep the whole axis
        @test b.t0 === 5  && b.t1 === 20
    end
    # missing/malformed cropBox → defaulted struct (the handler's separate check rejects the run).
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}("valueName" => "x"))
        @test p.cropBox.x0 === 0 && p.cropBox.z0 === -1
    end

    # DtypeConvert — dtype + rescale are LOWERCASED at parse (the handler's whitelist reads the
    # lowered value).
    let p = Cecelia.parse_dtype_convert_params(Dict{String,Any}(
            "dtype" => "UINT16", "rescale" => "NONE"))
        @test p.dtype == "uint16"
        @test p.rescale == "none"
    end

    # Flip — axis UPPERCASED at parse.
    let p = Cecelia.parse_flip_params(Dict{String,Any}("axis" => "z"))
        @test p.axis == "Z"
    end

    # Register — the audit's `regChannel` is a scalar channel NAME (not a `channelSelection`
    # array); parser preserves it verbatim.
    let p = Cecelia.parse_register_params(Dict{String,Any}(
            "regChannel" => "mem-TOM", "doAffine3d" => true, "sigma" => 0.5,
            "samplesPerParameter" => 8000))
        @test p.regChannel == "mem-TOM"
        @test p.doAffine2d === true            # default preserved
        @test p.doAffine3d === true
        @test p.sigma === 0.5
        @test p.samplesPerParameter === 8000
    end

    # ResampleZ — order LOWERCASED at parse.
    let p = Cecelia.parse_resample_z_params(Dict{String,Any}("order" => "CUBIC"))
        @test p.order == "cubic"
    end

    # TProject / ZProject — same shape, different defaults (mean vs max).
    let p = Cecelia.parse_t_project_params(Dict{String,Any}())
        @test p.op == "mean"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}())
        @test p.op == "max"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}("op" => "median"))
        @test p.op == "median"
    end
end

# Typed per-task params structs for the tracking family. Same pattern as
# `typed params — cleanupImages`: each parser is the ONE place its task reads the params bag, so
# a spec rename that isn't mirrored here becomes a struct-field error rather than a silent default.
@testset "typed params — tracking" begin
    # BayesianTracking — full-dict + empty-dict paths. Field spellings match btrack's own names
    # (`noiseInital` is a typo in the spec; preserved verbatim so a saved run stays reproducible).
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}(
            "valueName"      => "corrected",
            "popsToTrack"    => "T/tracked",
            "maxSearchRadius"=> 30,
            "maxLost"        => 5,
            "trackBranching" => true,
            "accuracy"       => 0.9,
            "distThresh"     => 12.5,
            "lambdaBranch"   => 100,
            "thetaDist"      => 7.5))
        @test p.valueName == "corrected"
        @test p.popsToTrack == "T/tracked"
        @test p.maxSearchRadius === 30
        @test p.maxLost === 5
        @test p.trackBranching === true
        @test p.accuracy === 0.9
        @test p.distThresh === 12.5
        @test p.lambdaBranch === 100
        @test p.thetaDist === 7.5
    end
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}())
        @test p.popsToTrack == "NONE"                # default = track whole segmentation
        @test p.maxSearchRadius === 20
        @test p.noiseInital === 300                  # historical spelling preserved
        @test p.thetaDist === 5.0
    end

    # TrackCorrect — the parser delegates `trackOps` to `parse_track_ops` (the same helper that
    # `validate_params` uses); empty/nothing → empty vector, a real op passes through with its
    # own dict entries preserved.
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "valueName" => "corrected",
            "trackOps"  => Any[Dict{String,Any}(
                "op" => "track.join", "trackIds" => Any[78, 92])]))
        @test p.valueName == "corrected"
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.join"
        @test p.trackOps[1]["trackIds"] == Any[78, 92]
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}("trackOps" => nothing))
        @test isempty(p.trackOps)
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test isempty(p.trackOps)
    end
    # JSON string round-trips the same way as a real Vector (the form path).
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "trackOps" => "[{\"op\":\"track.remove\",\"trackIds\":[10]}]"))
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.remove"
    end

    # TrackMeasures — `dims` is lower-cased AND stripped at parse so the handler reads one shape.
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}(
            "valueName" => "corrected", "forceRecompute" => false, "dims" => "  3D  "))
        @test p.valueName == "corrected"
        @test p.forceRecompute === false
        @test p.dims == "3d"
    end
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}())
        @test p.dims == "auto"                       # default: preflight detects
        @test p.forceRecompute === true              # default is force-recompute
    end
end

# Typed per-task params structs for the segment family. Same pattern as
# `typed params — cleanupImages` — each parser is the ONE place its task reads the params bag.
@testset "typed params — segment" begin
    # CellposeSegment — `models` stays a bag (resolved separately by cellpose_models_for_python).
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}(
            "valueName" => "corrected", "outputValueName" => "cells",
            "blockSize" => 1024, "overlap" => 128, "matchThreshold" => 0.5,
            "removeUnmatched" => true, "normaliseToWhole" => false))
        @test p.valueName == "corrected"
        @test p.outputValueName == "cells"
        @test p.blockSize === 1024
        @test p.overlap === 128
        @test p.matchThreshold === 0.5
        @test p.removeUnmatched === true
        @test p.normaliseToWhole === false
    end
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}())
        @test p.blockSize === 512
        @test p.overlap === 64
        @test p.matchThreshold === 0.3
        @test p.normaliseToWhole === true         # default: normalise to whole image
    end

    # CoastalSegment — same shape as cellpose plus temporalScaleMode.
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}(
            "outputValueName" => "flow-cells", "temporalScaleMode" => "seconds",
            "labelSmoothing" => 1.0))
        @test p.outputValueName == "flow-cells"
        @test p.temporalScaleMode == "seconds"
        @test p.labelSmoothing === 1.0
    end
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"    # default: as trained
        @test p.labelSmoothing === 0.5           # coastal's default differs from cellpose (0.0)
    end

    # Branching — fibreChannels stays a bag (resolved via channel_indices in the handler).
    let p = Cecelia.parse_branching_params(Dict{String,Any}(
            "refPops" => "T/tracked", "calcAnisotropy" => true,
            "fibreChannels" => ["SHG"], "anisotropySource" => "channel",
            "structureTensorSigmaUm" => 5.0, "anisotropyBoxUm" => 3.0,
            "integrateTime" => true))
        @test p.refPops == "T/tracked"
        @test p.calcAnisotropy === true
        @test p.fibreChannels == ["SHG"]
        @test p.anisotropySource == "channel"
        @test p.structureTensorSigmaUm === 5.0
        @test p.anisotropyBoxUm === 3.0
        @test p.integrateTime === true
    end
    let p = Cecelia.parse_branching_params(Dict{String,Any}())
        @test p.refPops == "NONE"                # default: skeletonise whole segmentation
        @test p.calcAnisotropy === false
        @test p.anisotropySource == "skeleton"
    end

    # SegmentCorrect — same JSON-string trackOps pattern as tracking.correct.
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "valueName" => "cells",
            "labelOps" => Any[Dict{String,Any}(
                "op" => "label.merge", "t" => 0, "ids" => Any[3, 5], "into" => 3)]))
        @test p.valueName == "cells"
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.merge"
        @test p.labelOps[1]["into"] == 3
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}("labelOps" => nothing))
        @test isempty(p.labelOps)
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "labelOps" => "[{\"op\":\"label.remove\",\"t\":0,\"ids\":[7]}]"))
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.remove"
    end

    # SegmentCorrectCarryOver — a single shared struct for both phases (they read only valueName).
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}(
            "valueName" => "corrected"))
        @test p.valueName == "corrected"
    end
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
    end

    # MeasureLabels
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}(
            "outputValueName" => "cells", "intensityValueName" => "corrected",
            "intensityMeasure" => "median", "gaussianFilter" => 1.0,
            "extendedMeasures" => true))
        @test p.outputValueName == "cells"
        @test p.intensityValueName == "corrected"
        @test p.intensityMeasure == "median"
        @test p.gaussianFilter === 1.0
        @test p.extendedMeasures === true
    end
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}())
        @test p.intensityMeasure == "mean"       # default
        @test p.extendedMeasures === false
    end
end

@testset "typed params — opticalFlow" begin
    # TrainFlowModel — 20+ fields; spot-check the ones that MEAN something.
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}(
            "valueName" => "corrected", "modelName" => "cd8-flow",
            "overwrite" => true, "trainChannels" => ["mem-TOM"],
            "temporalScaleMode" => "seconds", "temporalScales" => "2,4,8",
            "flowMetrics" => ["cell_boundary_likelihood", "cumulative_mag"],
            "epochs" => 60, "foregroundBoundaryWeight" => 0.3,
            "cropSize" => 256))
        @test p.valueName == "corrected"
        @test p.modelName == "cd8-flow"
        @test p.overwrite === true
        @test p.trainChannels == ["mem-TOM"]
        @test p.temporalScaleMode == "seconds"
        @test p.temporalScales == "2,4,8"
        @test p.flowMetrics == ["cell_boundary_likelihood", "cumulative_mag"]
        @test p.epochs === 60
        @test p.foregroundBoundaryWeight === 0.3
        @test p.cropSize === 256
    end
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"
        @test p.temporalScales == "1,2,4,8"
        @test isnothing(p.flowMetrics)                  # nothing → shipped default
        @test p.epochs === 30
        @test p.foregroundBoundaryWeight === 0.0        # OFF by default
        @test p.foregroundBlurSigma === 1.0
    end

    # TrainSupportDenoise
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}(
            "modelName" => "supp.MERTK", "trainChannels" => ["mem-TOM"],
            "unetSize" => "large", "inputFrames" => 31,
            "trainMode" => "perChannel", "epochs" => 40, "earlyStop" => false))
        @test p.modelName == "supp.MERTK"
        @test p.unetSize == "large"
        @test p.inputFrames === 31
        @test p.trainMode == "perChannel"
        @test p.epochs === 40
        @test p.earlyStop === false
    end
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}())
        @test p.unetSize == "medium"
        @test p.inputFrames === 61
        @test p.trainMode == "pooled"
        @test p.earlyStop === true
        @test p.patience === 5
    end
end

# Typed per-task params for the remaining families — closing out the ChainNode.params arc.
# Bundles the small tail families (importImages, exportImages, clust{Pops,Tracks,Regions},
# behaviour, spatialAnalysis) into one testset; each parser is the ONE place its task reads the
# params bag.
@testset "typed params — remaining families" begin
    # exportImages.ExportOmeTiff — `timepoint` accepts strings + -1 sentinel; `outDir` is stripped.
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}(
            "valueName" => "corrected", "channels" => ["mem-TOM"],
            "zMip" => true, "timepoint" => "5", "outDir" => "  /tmp/out  "))
        @test p.valueName == "corrected"
        @test p.channels == ["mem-TOM"]
        @test p.zMip === true
        @test p.timepoint === 5
        @test p.outDir == "/tmp/out"
    end
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}())
        @test p.zMip === false
        @test p.timepoint === -1                    # sentinel: export every frame
        @test p.outDir == ""                        # empty → default_export_dir()
    end

    # clustPops
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}(
            "popsToCluster" => ["A/root", "B/root", "NONE", ""],
            "valueNameSuffix" => "immune",
            "clusterMeasures" => ["mean_intensity_0"],
            "resolution" => 0.5, "usePaga" => true))
        @test p.popsToCluster == ["A/root", "B/root"]     # NONE + blank stripped
        @test p.valueNameSuffix == "immune"
        @test p.clusterMeasures == ["mean_intensity_0"]
        @test p.resolution === 0.5
        @test p.usePaga === true
    end
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}())
        @test isempty(p.popsToCluster)
        @test p.valueNameSuffix == "default"
        @test p.resolution === 1.0
        @test p.transformation == "NONE"
    end

    # clustTracks — same shape as clustPops plus popType + minTracklength.
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}(
            "popsToCluster" => ["A/_tracked"], "popType" => "track",
            "minTracklength" => 10, "resolution" => 2.0))
        @test p.popsToCluster == ["A/_tracked"]
        @test p.popType == "track"
        @test p.minTracklength === 10
    end
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}())
        @test p.popType == "live"                   # default: _tracked cells
        @test p.minTracklength === 5
    end

    # clustRegions
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "graphSuffix" => "delaunay",
            "clusterMethod" => "kmeans", "numClusters" => 8))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.graphSuffix == "delaunay"
        @test p.clusterMethod == "kmeans"
        @test p.numClusters === 8
    end
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}())
        @test p.clusterMethod == "leiden"
        @test p.numClusters === 5
        @test p.includeOther === true
    end

    # behaviour.hmm_states
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "modelMeasurements" => ["live.cell.speed"],
            "numStates" => 3, "normaliseTo" => "1", "normaliseMeasurements" => ["live.cell.speed"]))
        @test p.pops == ["A/_tracked"]
        @test p.colName == "cd8"
        @test p.modelMeasurements == ["live.cell.speed"]
        @test p.numStates === 3
        @test p.normaliseTo == "1"
        @test p.normaliseMeasurements == ["live.cell.speed"]
    end
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}())
        @test p.modelMeasurements == ["live.cell.speed", "live.cell.angle"]
        @test p.numStates === 2
        @test p.normaliseTo == "none"
    end

    # behaviour.hmm_transitions — hmmStates honours nothing → derived from colName
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "hmmStates" => ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"],
            "includeStart" => true))
        @test p.colName == "cd8"
        @test p.hmmStates == ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"]
        @test p.includeStart === true
    end
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}())
        @test isnothing(p.hmmStates)                # nothing → derived from colName in the handler
        @test p.includeSelfTransitions === true
    end

    # behaviour.motif_discovery (P1 POC) — mirror hmm_states: good case + defaults, and a
    # validate_params bad-case (windowSize=0 violates the JSON spec's min=3). The output
    # suffix is derived from the source pops' value_name at run-time (not a param), so any
    # legacy `valueName` key in a saved chain is silently ignored — no field on the struct.
    let p = Cecelia.parse_motif_discovery_params(Dict{String,Any}(
            "pops" => ["/live"], "valueName" => "poc",
            "windowSize" => 8, "topK" => 100, "numClasses" => 3,
            "resolutionLocked" => true))
        @test p.pops == ["/live"]
        @test !hasproperty(p, :valueName)
        @test p.windowSize === 8
        @test p.topK === 100
        @test p.numClasses === 3
        @test p.resolutionLocked === true
    end
    let p = Cecelia.parse_motif_discovery_params(Dict{String,Any}())
        @test p.pops == String[]
        @test p.windowSize === 8
        @test p.topK === 100
        @test p.numClasses === 3
        @test p.resolutionLocked === false
    end
    # Spec-level validation: windowSize=0 is below the JSON spec's min=3, so validate_params
    # rejects it as ParamValidationError before the handler ever runs.
    @test_throws Cecelia.ParamValidationError Cecelia.validate_params(
        Cecelia.MotifDiscovery(),
        Dict{String,Any}("pops" => ["flowTom/qc/CD169-/cells"], "windowSize" => 0))
    # `pops` is required (matches hmm_states) — empty list is rejected by the spec's `required: true`.
    @test_throws Cecelia.ParamValidationError Cecelia.validate_params(
        Cecelia.MotifDiscovery(),
        Dict{String,Any}("pops" => String[]))
    # Column-name contract: unsuffixed (matches HMM convention). Load-bearing for
    # `motif_class_frequency.json` (`match: "motif.class"`) and for B+T co-plot on one axis
    # (MOTIF_DISCOVERY_PLAN Decision 12, revised 2026-09-19). A drift here — someone
    # reintroducing `.{suffix}` in a follow-up — forks column identity across pops and the
    # summary panel silently renders empty on the other vn's series. Full rationale:
    # docs/DATAMODEL.md → *Motif discovery output*.
    @test Cecelia.MOTIF_CLASS_COL       == "motif.class"
    @test Cecelia.MOTIF_DISTANCE_COL    == "motif.distance"
    @test Cecelia.MOTIF_INSTANCE_ID_COL == "motif.instance_id"
    @test Cecelia.MOTIF_SEQUENCE_COL    == "motif.sequence"
    # Legacy-suffix drop list — see the `_legacy_motif_cell_cols` comment in
    # `motif_discovery.jl`. Guards the re-run cleanup path against silent breakage
    # (e.g. if MOTIF_*_COL changed but the legacy helper wasn't updated in step).
    @test Cecelia._legacy_motif_cell_cols("T") ==
        String["motif.class.T", "motif.distance.T", "motif.instance_id.T"]
    @test Cecelia._legacy_motif_track_cols("B") == String["motif.sequence.B"]

    # spatialAnalysis (spot-check the parsers; each is small).
    let p = Cecelia.parse_aggregates_meshes_params(Dict{String,Any}(
            "pops" => ["A/B"], "maxClusterDist" => 7.5, "minCells" => 10))
        @test p.pops == ["A/B"]
        @test p.maxClusterDist === 7.5
        @test p.minCells === 10
    end
    let p = Cecelia.parse_cell_contacts_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"], "maxContactDist" => 15.0))
        @test p.popsA == ["A/T"] && p.popsB == ["A/B"]
        @test p.maxContactDist === 15.0
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}(
            "neighbourMethod" => "knn", "pops" => ["A/"], "graphSuffix" => "knn6",
            "nNeighbours" => 12, "perTimepoint" => true))
        @test p.neighbourMethod == "knn"
        @test p.pops == ["A/"]
        @test p.graphSuffix == "knn6"
        @test p.nNeighbours === 12
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}())
        @test p.neighbourMethod == "delaunay"
        @test p.perTimepoint === false
        @test p.neighbourRadius === 30.0
    end
    let p = Cecelia.parse_contacts_meshes_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"]))
        @test p.maxContactDist === 5.0
    end
    let p = Cecelia.parse_detect_aggregates_params(Dict{String,Any}(
            "pops" => ["A/B"], "clustDiameter" => 20.0, "perTimepoint" => true))
        @test p.clustDiameter === 20.0
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_neighbour_stats_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "nPermutations" => 500))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.nPermutations === 500
    end

    # importImages
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}(
            "src_path" => "/data/foo.czi", "pyramidLevels" => 5,
            "stageLocal" => true, "chunkSize" => 512,
            "maxWorkers" => "2", "jvmHeapGiB" => "16"))
        @test p.src_path == "/data/foo.czi"
        @test p.pyramidLevels == 5
        @test p.stageLocal === true
        @test p.chunkSize == 512
        @test p.maxWorkers == "2"
        @test p.jvmHeapGiB == "16"
    end
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}())
        @test p.src_path == ""                       # empty → fall back to img.meta.ori_path in the handler
        @test isnothing(p.pyramidLevels)             # nothing → fall back to pyramidScale
        @test p.pyramidScale === 2
        @test p.maxWorkers == "auto"                 # sentinel, resolved per-reader in the handler
        @test p.jvmHeapGiB == "auto"
        @test isnothing(p.ngffVersion)               # nothing → Settings default (store_layout())
    end
    let p = Cecelia.parse_migrate_legacy_params(Dict{String,Any}(
            "sourceProjectDir" => "/old/proj", "sourceUid" => "abc123", "rscript" => "Rscript"))
        @test p.sourceProjectDir == "/old/proj"
        @test p.sourceUid == "abc123"
        @test p.rscript == "Rscript"
        @test p.mode == "copy"
    end
    # `_merge_meta_preserving_legacy`: the register-time source pointers survive the meta rewrite
    # applied at the end of a successful migration, so a second run (e.g. copy → symlink) still
    # knows what to migrate. Without this the second run hits the `no legacy source` guard.
    let old = Dict{String,Any}("legacySourceDir" => "/old/proj", "legacySourceUid" => "abc123",
                               "legacyRscript" => "/usr/bin/Rscript", "orifilepath" => "/x")
        new_ = Dict{String,Any}("SizeX" => 512, "SizeY" => 512, "name" => "img1")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/old/proj"
        @test merged["legacySourceUid"] == "abc123"
        @test merged["legacyRscript"]   == "/usr/bin/Rscript"
        @test merged["SizeX"] == 512                    # new-side OME content preserved
        @test !haskey(merged, "orifilepath")            # non-legacy old-side keys NOT carried
    end
    let old = Dict{String,Any}("legacySourceDir" => "/old", "legacySourceUid" => "abc")
        # new side wins if it happens to carry a legacy key (would only happen if the Python side
        # started returning one) — nothing gets silently overwritten by the carry-over.
        new_ = Dict{String,Any}("legacySourceDir" => "/from-python")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/from-python"
        @test merged["legacySourceUid"] == "abc"
    end
    let old = Dict{String,Any}("legacySourceDir" => "", "legacySourceUid" => "abc")
        # An empty string on the old side is treated as absent (guard on read uses `isempty`).
        merged = Cecelia._merge_meta_preserving_legacy(Dict{String,Any}(), old)
        @test !haskey(merged, "legacySourceDir")
        @test merged["legacySourceUid"] == "abc"
    end
    let p = Cecelia.parse_remove_image_params(Dict{String,Any}(
            "valueName" => "old", "newDefault" => "corrected"))
        @test p.valueName == "old"
        @test p.newDefault == "corrected"
    end
end

# label_props boundary types (audit task #26): `add_obs` refuses non-numeric columns at entry
# with a named-column error; `CategoricalObsColumn` names the drift instead of dying deep in the
# Python subprocess. Pure-Julia tests — no h5ad needed.
