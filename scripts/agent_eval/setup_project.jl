# Build the agent-run fixture project (docs/todo/AGENT_OVERNIGHT_PLAN.md, P1): a project in an ISOLATED
# dev dir (never the user's), one image per fixture source, imported through the real import task, and —
# with `--prior cellpose` — a "prior work" segmentation registered as `default`, the user's earlier
# result an unattended run must leave alone. Driven by `setup.py`, which snapshots the result.
#
#   julia --project=app scripts/agent_eval/setup_project.jl --fixture DIR --dev-dir DIR --out FILE \
#         [--prior cellpose|none] [--bioformats2raw PATH] [--python PATH]

function _args()
    a = Dict{String,String}("prior" => "cellpose")
    i = 1
    while i <= length(ARGS)
        a[replace(ARGS[i], "--" => "")] = ARGS[i + 1]; i += 2
    end
    for k in ("fixture", "dev-dir", "out")
        haskey(a, k) || error("--$k is required")
    end
    a
end
const A = _args()

using Cecelia, JSON3, TOML

# The converter the developer already has configured — READ from their config through the one
# resolver, before ENV is switched; the isolated config points at the same one.
function _configured_bioformats2raw()::Union{String,Nothing}
    toml = Cecelia.custom_toml_path()
    isfile(toml) || return nothing
    d = get(get(TOML.parsefile(toml), "dirs", Dict{String,Any}()), "bioformats2raw", "")
    isempty(string(d)) ? nothing : Cecelia.expand_user(string(d))
end

# Isolation before `init_cecelia!` reads any config: a fresh dev dir whose projects dir lives inside it.
const DEV = abspath(A["dev-dir"])
mkpath(joinpath(DEV, "projects"))
let dirs = Dict{String,Any}("projects" => joinpath(DEV, "projects"))
    b2r = get(A, "bioformats2raw", _configured_bioformats2raw())
    isnothing(b2r) || (dirs["bioformats2raw"] = b2r)
    # an explicit interpreter, so tasks find the analysis env even when the caller isn't inside pixi
    haskey(A, "python") && (dirs["python"] = A["python"])
    write_atomic(joinpath(DEV, "custom.toml")) do io
        TOML.print(io, Dict{String,Any}("dirs" => dirs))
    end
end
ENV["CECELIA_DEV_DIR"] = DEV
init_cecelia!()

const PRIOR_PARAMS = Dict{String,Any}(
    "valueName" => "default", "outputValueName" => "default",
    "models" => Dict{String,Any}("0" => Dict{String,Any}(
        "model" => "cpsam_v2", "matchAs" => "base", "cellChannels" => ["cells"], "nucChannels" => String[],
        "cellDiameter" => 10, "normalise" => 99.9, "stitchThreshold" => 0.2, "threshold" => 0,
        "medianFilter" => 0, "gaussianFilter" => 0.0)))

function main()
    fixture = JSON3.read(read(joinpath(A["fixture"], "fixture.json"), String))
    proj = create_project!(name = "agent-eval")
    s    = add_set!(proj; name = String(fixture.kind))
    images = Dict{String,Any}[]
    for im in fixture.images
        img = add_image!(s; name = String(im.name), meta = Dict{String,Any}("ori_path" => String(im.tif)))
        r = run_task(proj.uid, img.uid; fun_name = "importImages.omezarr")
        isnothing(r) && error("import failed for $(im.name)")
        if A["prior"] == "cellpose"
            r = run_task(proj.uid, img.uid; fun_name = "segment.cellposeMeasure", params = PRIOR_PARAMS)
            isnothing(r) && error("prior segmentation failed for $(im.name)")
        end
        push!(images, Dict{String,Any}("name" => String(im.name), "uid" => img.uid, "gt" => String(im.gt)))
    end
    write_json_atomic(A["out"], Dict{String,Any}(
        "devDir" => DEV, "projectUid" => proj.uid, "projectDir" => proj.root,
        "prior" => A["prior"], "images" => images))
end

main()
