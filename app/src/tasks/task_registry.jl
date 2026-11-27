# Built-in COMPOSITE tasks — a JSON spec with a "composite" array and no .jl of their own, so they
# register here. Every other built-in task registers itself beside its struct with
# `register_builtin_task!` (task/registry.jl); a new task needs no line in this file.

register_builtin_task!("importImages.immunemap", CompositeTask("importImages.immunemap");
    spec = joinpath(@__DIR__, "importImages", "immunemap.json"))

register_builtin_task!("segment.cellposeMeasure", CompositeTask("segment.cellposeMeasure");
    spec = joinpath(@__DIR__, "segment", "cellpose_measure.json"))

register_builtin_task!("segment.coastalMeasure", CompositeTask("segment.coastalMeasure");
    spec = joinpath(@__DIR__, "segment", "coastal_measure.json"))

register_builtin_task!("segment.importLabelsMeasure", CompositeTask("segment.importLabelsMeasure");
    spec = joinpath(@__DIR__, "segment", "import_labels_measure.json"))

register_builtin_task!("segment.ridgesMeasure", CompositeTask("segment.ridgesMeasure");
    spec = joinpath(@__DIR__, "segment", "ridges_measure.json"))

register_builtin_task!("tracking.bayesian_track_measures", CompositeTask("tracking.bayesian_track_measures");
    spec = joinpath(@__DIR__, "tracking", "bayesian_track_measures.json"))

register_builtin_task!("tracking.correct_measures", CompositeTask("tracking.correct_measures");
    spec = joinpath(@__DIR__, "tracking", "correct_measures.json"))

register_builtin_task!("segment.correct_measures", CompositeTask("segment.correct_measures");
    spec = joinpath(@__DIR__, "segment", "correct_measures.json"))

register_builtin_task!("behaviour.hmm", CompositeTask("behaviour.hmm");
    spec = joinpath(@__DIR__, "behaviour", "hmm.json"))
