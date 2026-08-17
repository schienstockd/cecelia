# Example PLUGIN task — attach externally-tracked cells to this segmentation.
#
# The driving case (docs/todo/PLUGINS_PLAN.md): a lab tracked cells in some OTHER tool — ImageJ Manual
# Tracking, TrackMate, Imaris — independently of cecelia, and wants those track ids on cecelia's own
# segmentation. That format is one lab's, so it must not ship in the app. This is what a plugin is for.
#
# **Nothing about a format is hard-coded.** The plugin needs exactly four things — a track id, a frame,
# and X/Y (Z if 3D) — so the task takes a COLUMN MAPPING. `templates/*.json` beside this plugin ship
# ready-made mappings; any field left non-empty on the form overrides the template, so an unlisted
# export works by mapping its columns by hand and a *nearly*-matching one works by fixing one field.
# Supporting a new tool is then a new template file, not new code.
#
# There is no label column to join on — an external tracker knows nothing about cecelia's labels — so
# spots are matched to cells SPATIALLY, per frame, within a cutoff. See python/csv_tracks.py.
#
# Category is `tracking`, an EXISTING built-in page, so this appears in the Tracking page's task list.
# Contrast with this plugin's other task (`trackTools/cumulativeChange.jl`), whose category is new and
# therefore gets the plugin's own page. One plugin, both routes.

struct ImportCsvTracks <: Cecelia.CciaTask end

# Where the shipped column mappings live — a sibling of this task's category dir, at the plugin root.
_ict_templates_dir() = normpath(joinpath(@__DIR__, "..", "templates"))

"""
Resolve the column mapping: start from the chosen template, then apply any non-empty override from the
form. Returns a `Dict{String,Any}` or throws with a readable message naming the templates available.

Override-on-top rather than either/or, because the common case is an export that is *almost* one of
the templates — one renamed column — and making the user retype all five fields to fix one is how a
mapping ends up subtly wrong.
"""
function _ict_mapping(params::Dict{String,Any})
    tpl = string(get(params, "template", "imagej_manual"))
    dir = _ict_templates_dir()
    available = sort([splitext(f)[1] for f in readdir(dir) if endswith(f, ".json")])
    map = Dict{String,Any}()
    if tpl != "custom"
        path = joinpath(dir, "$tpl.json")
        isfile(path) || error("Unknown template '$tpl'. Available: $(join(available, ", ")), custom")
        map = Cecelia.JSON3.read(read(path, String), Dict{String,Any})
    end
    for k in ("trackColumn", "frameColumn", "xColumn", "yColumn", "zColumn")
        v = string(get(params, k, ""))
        isempty(v) || (map[k] = v)          # a non-empty field overrides the template
    end
    for k in ("frameBase", "skipRows")
        haskey(params, k) && (map[k] = Int(params[k]))
    end
    u = string(get(params, "spotUnit", ""))
    isempty(u) || (map["spotUnit"] = u)
    map
end

function Cecelia._run_task(::ImportCsvTracks, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    vn     = string(get(params, "valueName", Cecelia.VERSIONED_DEFAULT_VAL))
    csv    = string(get(params, "csvPath", ""))
    outcol = string(get(params, "outColumn", "trackTools.track_id"))
    maxd   = Float64(get(params, "maxDistance", 10.0))

    isempty(csv) && (on_log("[ERROR] No CSV path given"); return nothing)
    isfile(csv)  || (on_log("[ERROR] CSV not found: $csv"); return nothing)

    path = Cecelia.img_label_props_path(img, vn)
    isfile(path) || (on_log("[ERROR] No label props for valueName='$vn'"); return nothing)

    map = try
        _ict_mapping(params)
    catch e
        on_log("[ERROR] " * sprint(showerror, e)); return nothing
    end
    on_log("[INFO] Column mapping: track=$(get(map,"trackColumn","?")) " *
           "frame=$(get(map,"frameColumn","?")) " *
           "x=$(get(map,"xColumn","?")) y=$(get(map,"yColumn","?")) z=$(get(map,"zColumn",""))")

    # Physical sizes let the runner convert calibrated spot coordinates to pixels, so `maxDistance`
    # stays in pixels — the unit a user can judge against a segmentation. NOTE the vector is Z,Y,X
    # (the codebase convention); the runner reverses it, and the name says so to stop it being
    # re-indexed as X,Y,Z on the other side.
    sizes, _ = Cecelia.img_physical_sizes(img)

    on_progress(0, 1)
    script = joinpath(@__DIR__, "importCsvTracks_run.py")
    ok = Cecelia.run_py(script,
            (; labelPropsPath = path, csvPath = csv, outColumn = outcol,
               maxDistance = maxd, physicalSizesZYX = sizes, mapping = map),
            Cecelia.task_run_dir(img._dir);
            on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] Track import failed"); return nothing)
    on_progress(1, 1)

    Dict{String,Any}("outputColumn" => outcol, "csvPath" => csv, "valueName" => vn,
                     "template" => string(get(params, "template", "")))
end

Cecelia.register_task!("tracking.importCsvTracks", ImportCsvTracks();
                       spec = joinpath(@__DIR__, "importCsvTracks.json"))   # co-located
