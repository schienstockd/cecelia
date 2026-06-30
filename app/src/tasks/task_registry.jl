# Registered after all concrete task types are defined.
# Add new tasks here: _spec_path overload + entry in _fun_name_map().

function _spec_path(::ImportOmezarr)
    joinpath(@__DIR__, "importImages", "omezarr.json")
end

function _spec_path(::RemoveImage)
    joinpath(@__DIR__, "importImages", "remove.json")
end

function _spec_path(::CellposeCorrect)
    joinpath(@__DIR__, "cleanupImages", "cellpose_correct.json")
end

function _spec_path(::CellposeSegment)
    joinpath(@__DIR__, "segment", "cellpose.json")
end

function _spec_path(::MeasureLabels)
    joinpath(@__DIR__, "segment", "measure_labels.json")
end

function _spec_path(::BayesianTracking)
    joinpath(@__DIR__, "tracking", "bayesian_tracking.json")
end

function _spec_path(::TrackMeasures)
    joinpath(@__DIR__, "tracking", "track_measures.json")
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

function _spec_path(::AfCorrect)
    joinpath(@__DIR__, "cleanupImages", "af_correct.json")
end

function _spec_path(::DriftCorrect)
    joinpath(@__DIR__, "cleanupImages", "drift_correct.json")
end

function _spec_path(::TestImageTask)
    joinpath(@__DIR__, "testTasks", "imageTask.json")
end

function _spec_path(::TestSetTask)
    joinpath(@__DIR__, "testTasks", "setTask.json")
end

function _spec_path(::IncrementalPlotTask)
    joinpath(@__DIR__, "testTasks", "incrementalPlotTask.json")
end

_COMPOSITE_SPEC_PATHS["cleanupImages.afDriftCorrect"] =
    joinpath(@__DIR__, "cleanupImages", "af_drift_correct.json")

_COMPOSITE_SPEC_PATHS["segment.cellposeMeasure"] =
    joinpath(@__DIR__, "segment", "cellpose_measure.json")

_COMPOSITE_SPEC_PATHS["tracking.bayesian_track_measures"] =
    joinpath(@__DIR__, "tracking", "bayesian_track_measures.json")

_COMPOSITE_SPEC_PATHS["behaviour.hmm"] =
    joinpath(@__DIR__, "behaviour", "hmm.json")

function _fun_name_map()::Dict{String, CciaTask}
    Dict{String, CciaTask}(
        "importImages.omezarr"              => ImportOmezarr(),
        "importImages.remove"               => RemoveImage(),
        "cleanupImages.cellposeCorrect"     => CellposeCorrect(),
        "segment.cellpose"                  => CellposeSegment(),
        "segment.measureLabels"             => MeasureLabels(),
        "tracking.bayesian_tracking"        => BayesianTracking(),
        "tracking.track_measures"           => TrackMeasures(),
        "tracking.bayesian_track_measures"  => CompositeTask("tracking.bayesian_track_measures"),
        "behaviour.hmm_states"              => HmmStates(),
        "behaviour.hmm_transitions"         => HmmTransitions(),
        "behaviour.hmm"                     => CompositeTask("behaviour.hmm"),
        "clustPops.cluster"                 => ClustPops(),
        "clustTracks.cluster"               => ClustTracks(),
        "segment.cellposeMeasure"           => CompositeTask("segment.cellposeMeasure"),
        "cleanupImages.afCorrect"           => AfCorrect(),
        "cleanupImages.driftCorrect"        => DriftCorrect(),
        "cleanupImages.afDriftCorrect"      => CompositeTask("cleanupImages.afDriftCorrect"),
        "testTasks.imageTask"               => TestImageTask(),
        "testTasks.setTask"                 => TestSetTask(),
        "testTasks.incrementalPlotTask"     => IncrementalPlotTask(),
    )
end
