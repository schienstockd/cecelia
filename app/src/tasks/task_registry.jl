# Registered after all concrete task types are defined.
# Add new tasks here: _spec_path overload + entry in _fun_name_map().

function _spec_path(::ImportOmezarr)
    joinpath(@__DIR__, "importImages", "omezarr.json")
end

function _spec_path(::RemoveImage)
    joinpath(@__DIR__, "importImages", "remove.json")
end

function _spec_path(::MigrateLegacy)
    joinpath(@__DIR__, "importImages", "migrateLegacy.json")
end

function _spec_path(::CellposeSegment)
    joinpath(@__DIR__, "segment", "cellpose.json")
end

function _spec_path(::CoastalSegment)
    joinpath(@__DIR__, "segment", "coastal.json")
end

function _spec_path(::TrainFlowModel)
    joinpath(@__DIR__, "opticalFlow", "train.json")
end

function _spec_path(::MeasureLabels)
    joinpath(@__DIR__, "segment", "measure_labels.json")
end

function _spec_path(::Branching)
    joinpath(@__DIR__, "segment", "branching.json")
end

function _spec_path(::BayesianTracking)
    joinpath(@__DIR__, "tracking", "bayesian_tracking.json")
end

function _spec_path(::TrackMeasures)
    joinpath(@__DIR__, "tracking", "track_measures.json")
end

function _spec_path(::TrackCorrect)
    joinpath(@__DIR__, "tracking", "correct.json")
end

function _spec_path(::HmmStates)
    joinpath(@__DIR__, "behaviour", "hmm_states.json")
end

function _spec_path(::HmmTransitions)
    joinpath(@__DIR__, "behaviour", "hmm_transitions.json")
end

function _spec_path(::ClustPops)
    joinpath(@__DIR__, "clustPops", "cluster.json")
end

function _spec_path(::ClustTracks)
    joinpath(@__DIR__, "clustTracks", "cluster.json")
end

function _spec_path(::CellNeighbours)
    joinpath(@__DIR__, "spatialAnalysis", "cellNeighbours.json")
end

function _spec_path(::NeighbourStats)
    joinpath(@__DIR__, "spatialAnalysis", "neighbourStats.json")
end

function _spec_path(::DetectAggregates)
    joinpath(@__DIR__, "spatialAnalysis", "detectAggregates.json")
end

function _spec_path(::CellContacts)
    joinpath(@__DIR__, "spatialAnalysis", "cellContacts.json")
end

function _spec_path(::ContactsMeshes)
    joinpath(@__DIR__, "spatialAnalysis", "contactsMeshes.json")
end

function _spec_path(::AggregatesMeshes)
    joinpath(@__DIR__, "spatialAnalysis", "aggregatesMeshes.json")
end

function _spec_path(::ClustRegions)
    joinpath(@__DIR__, "clustRegions", "cluster.json")
end

function _spec_path(::AfCorrect)
    joinpath(@__DIR__, "cleanupImages", "af_correct.json")
end

function _spec_path(::DriftCorrect)
    joinpath(@__DIR__, "cleanupImages", "drift_correct.json")
end

function _spec_path(::Smooth)
    joinpath(@__DIR__, "cleanupImages", "smooth.json")
end

function _spec_path(::Flip)
    joinpath(@__DIR__, "cleanupImages", "flip.json")
end

function _spec_path(::DtypeConvert)
    joinpath(@__DIR__, "cleanupImages", "dtype.json")
end

function _spec_path(::CropImage)
    joinpath(@__DIR__, "editImages", "cropImage.json")
end

function _spec_path(::ExportOmeTiff)
    joinpath(@__DIR__, "exportImages", "ome_tiff.json")
end

function _spec_path(::CopyImage)
    joinpath(@__DIR__, "editImages", "copyImage.json")
end

function _spec_path(::ZProject)
    joinpath(@__DIR__, "editImages", "zProject.json")
end

function _spec_path(::TProject)
    joinpath(@__DIR__, "editImages", "tProject.json")
end

function _spec_path(::BinImage)
    joinpath(@__DIR__, "editImages", "bin.json")
end

function _spec_path(::ResampleZ)
    joinpath(@__DIR__, "editImages", "resampleZ.json")
end

function _spec_path(::Register)
    joinpath(@__DIR__, "editImages", "register.json")
end

function _spec_path(::TestImageTask)
    joinpath(@__DIR__, "testTasks", "image_task.json")
end

function _spec_path(::TestSetTask)
    joinpath(@__DIR__, "testTasks", "set_task.json")
end

function _spec_path(::IncrementalPlotTask)
    joinpath(@__DIR__, "testTasks", "incremental_plot_task.json")
end

_COMPOSITE_SPEC_PATHS["cleanupImages.afDriftCorrect"] =
    joinpath(@__DIR__, "cleanupImages", "af_drift_correct.json")

_COMPOSITE_SPEC_PATHS["segment.cellposeMeasure"] =
    joinpath(@__DIR__, "segment", "cellpose_measure.json")

_COMPOSITE_SPEC_PATHS["segment.coastalMeasure"] =
    joinpath(@__DIR__, "segment", "coastal_measure.json")

_COMPOSITE_SPEC_PATHS["tracking.bayesian_track_measures"] =
    joinpath(@__DIR__, "tracking", "bayesian_track_measures.json")

_COMPOSITE_SPEC_PATHS["tracking.correct_measures"] =
    joinpath(@__DIR__, "tracking", "correct_measures.json")

_COMPOSITE_SPEC_PATHS["behaviour.hmm"] =
    joinpath(@__DIR__, "behaviour", "hmm.json")

function _fun_name_map()::Dict{String, CciaTask}
    Dict{String, CciaTask}(
        "importImages.omezarr"              => ImportOmezarr(),
        "importImages.remove"               => RemoveImage(),
        "importImages.migrateLegacy"        => MigrateLegacy(),
        "segment.cellpose"                  => CellposeSegment(),
        "segment.coastal"                   => CoastalSegment(),
        "opticalFlow.train"                 => TrainFlowModel(),
        "segment.measureLabels"             => MeasureLabels(),
        "segment.branching"                 => Branching(),
        "tracking.bayesian_tracking"        => BayesianTracking(),
        "tracking.track_measures"           => TrackMeasures(),
        "tracking.correct"                  => TrackCorrect(),
        "tracking.bayesian_track_measures"  => CompositeTask("tracking.bayesian_track_measures"),
        "tracking.correct_measures"         => CompositeTask("tracking.correct_measures"),
        "behaviour.hmm_states"              => HmmStates(),
        "behaviour.hmm_transitions"         => HmmTransitions(),
        "behaviour.hmm"                     => CompositeTask("behaviour.hmm"),
        "clustPops.cluster"                 => ClustPops(),
        "clustTracks.cluster"               => ClustTracks(),
        "spatialAnalysis.cellNeighbours"    => CellNeighbours(),
        "spatialAnalysis.neighbourStats"    => NeighbourStats(),
        "spatialAnalysis.detectAggregates"  => DetectAggregates(),
        "spatialAnalysis.cellContacts"      => CellContacts(),
        "spatialAnalysis.contactsMeshes"    => ContactsMeshes(),
        "spatialAnalysis.aggregatesMeshes"  => AggregatesMeshes(),
        "clustRegions.cluster"              => ClustRegions(),
        "segment.cellposeMeasure"           => CompositeTask("segment.cellposeMeasure"),
        "segment.coastalMeasure"            => CompositeTask("segment.coastalMeasure"),
        "cleanupImages.afCorrect"           => AfCorrect(),
        "cleanupImages.driftCorrect"        => DriftCorrect(),
        "cleanupImages.smooth"              => Smooth(),
        "cleanupImages.flip"                => Flip(),
        "cleanupImages.dtype"               => DtypeConvert(),
        "cleanupImages.afDriftCorrect"      => CompositeTask("cleanupImages.afDriftCorrect"),
        "editImages.cropImage"              => CropImage(),
        "editImages.copyImage"              => CopyImage(),
        "editImages.zProject"               => ZProject(),
        "editImages.tProject"               => TProject(),
        "editImages.bin"                    => BinImage(),
        "editImages.resampleZ"              => ResampleZ(),
        "editImages.register"               => Register(),
        "exportImages.ome_tiff"        => ExportOmeTiff(),
        "testTasks.image_task"               => TestImageTask(),
        "testTasks.set_task"                 => TestSetTask(),
        "testTasks.incremental_plot_task"     => IncrementalPlotTask(),
    )
end
