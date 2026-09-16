# AF correct task aggregator. Three responsibility groups — one file each — mirroring the seams
# docs/MAP.md prescribed for this task. The `AfCorrect` singleton lives here (so every downstream
# method dispatch has the type available before the sub-files load); everything else is one of:
#
#   af_correct/translate.jl — the boundary. `AfCombinationSpec`, `parse_af_combinations`, and the
#                             shared `af_combinations_for_python` translator that resolves the
#                             frontend's channel NAMES into 0-based indices for Python. The preview
#                             and the run both call it, so they can't drift on which channels were
#                             actually used.
#   af_correct/qc.jl        — QC ONLY. `AfChannelStats`, `parse_af_channel_stats`, and
#                             `af_qc_findings` — the "saturated input" / "bleedthrough" findings
#                             built from the Python side's per-channel stats file. Kept separate
#                             because the AF audit (docs/todo/AF_CORRECTION_AUDIT.md) points here.
#   af_correct/run.jl       — the task itself: `AfCorrectParams`, `parse_af_correct_params`,
#                             `preview_params`, `task_previewable`, and `_run_task`.
struct AfCorrect <: CciaTask end

include(joinpath(@__DIR__, "af_correct", "translate.jl"))
include(joinpath(@__DIR__, "af_correct", "qc.jl"))
include(joinpath(@__DIR__, "af_correct", "run.jl"))
