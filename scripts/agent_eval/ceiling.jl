# Human ceiling for an agent-run fixture (docs/todo/AGENT_OVERNIGHT_PLAN.md, P2): the pipeline run by
# hand with sensible parameters, into a fresh value_name, through the same `run_task` an agent would use.
# If this can't score well, the fixture is wrong, not the agent. Also the end-to-end check of the
# sandbox audit's "a fresh value_name isolates the run" (docs/audit/agent-sandbox-value-name.md).
#
#   julia --project=app scripts/agent_eval/ceiling.jl --run <root>/run.json [--value-name ceiling]

function _args()
    a = Dict{String,String}("value-name" => "ceiling")
    i = 1
    while i <= length(ARGS)
        a[replace(ARGS[i], "--" => "")] = ARGS[i + 1]; i += 2
    end
    haskey(a, "run") || error("--run is required")
    a
end
const A = _args()

using JSON3
const RUN = JSON3.read(read(A["run"], String))
ENV["CECELIA_DEV_DIR"] = String(RUN.devDir)        # the fixture's isolated dev dir, never the user's
using Cecelia
init_cecelia!()

const VN   = A["value-name"]
const PROJ = String(RUN.projectUid)
const UIDS = [String(im.uid) for im in RUN.images]

const SEGMENT = Dict{String,Any}(
    "valueName" => "default", "outputValueName" => VN,
    "models" => Dict{String,Any}("0" => Dict{String,Any}(
        "model" => "cpsam_v2", "matchAs" => "base", "cellChannels" => ["cells"], "nucChannels" => String[],
        "cellDiameter" => 10, "normalise" => 99.9, "stitchThreshold" => 0.2, "threshold" => 0,
        "medianFilter" => 0, "gaussianFilter" => 0.0)))
const TRACK   = Dict{String,Any}("valueName" => VN, "popsToTrack" => "NONE", "maxSearchRadius" => 15,
                                 "maxLost" => 2, "minTimepoints" => 5)
const MEASURE = Dict{String,Any}("valueName" => VN)
const HMM     = Dict{String,Any}("pops" => ["$VN/_tracked"], "colName" => VN, "numStates" => 2,
                                 "modelMeasurements" => ["live.cell.speed", "live.cell.angle"])

step(name, r) = (isnothing(r) && error("ceiling: $name failed"); println("[CEILING] $name ok"); r)

t0 = time()
for uid in UIDS
    step("segment $uid", run_task(PROJ, uid; fun_name = "segment.cellposeMeasure", params = SEGMENT))
    step("track $uid",   run_task(PROJ, uid; fun_name = "tracking.bayesian_tracking", params = TRACK))
    step("measure $uid", run_task(PROJ, uid; fun_name = "tracking.track_measures", params = MEASURE))
end
imgs = [init_object(PROJ, uid) for uid in UIDS]
step("hmm", run_task(Cecelia._task_from_fun_name("behaviour.hmm_states"), imgs, HMM))
println("[CEILING] wall-clock $(round(time() - t0; digits = 1)) s")
println("[CEILING] state column live.cell.hmm.state.$VN")
