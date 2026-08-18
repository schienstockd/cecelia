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
# spots are matched to cells SPATIALLY, per frame, within a cutoff. See python/track_readers.py.
#
# Category is `tracking`, an EXISTING built-in page, so this appears in the Tracking page's task list
# with no page of its own. (`ccia-trackMeasures` is the counterpart example: a NEW category, so it gets
# the generic `/custom/<category>` page plus its own plot canvas.)

struct ImportCsvTracks <: Cecelia.CciaTask end

# Where the shipped column mappings live — a sibling of this task's category dir, at the plugin root.
_ict_templates_dir() = normpath(joinpath(@__DIR__, "..", "templates"))

# ── Reading the user's file: ONE delimiter decision, made here ────────────────────────────────────
#
# The form needs the file's column names, and the runner needs its rows. Sniffing separately in Julia
# and again in Python would be two implementations of one decision that can disagree on the same file
# — so Julia sniffs once and PASSES the delimiter to the runner, which never guesses. Same principle as
# the image compressor in CLAUDE.md: the delimiter is a decision, not a default.

"""Sniff the delimiter from the header line: whichever candidate splits it into the most fields."""
function _ict_delimiter(path::AbstractString)::String
    line = try
        open(path) do io
            for l in eachline(io)
                isempty(strip(l)) || return l
            end
            ""
        end
    catch; "" end
    isempty(line) && return ","
    best, n = ",", 0
    for d in (",", "\t", ";", "|")
        c = length(split(line, d))
        c > n && ((best, n) = (d, c))
    end
    best
end

"""
Column names of a delimited file, or `String[]` if it cannot be read.

`skip` drops preamble lines above the header (Imaris writes 3). Never throws: this feeds a form, and a
half-typed or missing path must degrade to "no suggestions", never break the page that draws it.
"""
function _ict_headers(path::AbstractString; skip::Int = 0)::Vector{String}
    isfile(path) || return String[]
    # An XML export has no columns; splitting its first line would offer nonsense as suggestions.
    lowercase(splitext(path)[2]) == ".xml" && return String[]
    try
        d = _ict_delimiter(path)
        open(path) do io
            for _ in 1:skip; readline(io); end
            for l in eachline(io)
                isempty(strip(l)) && continue
                return String[strip(String(t), ['"', ' ']) for t in split(l, d)]
            end
            String[]
        end
    catch; String[] end
end

# The form's column fields offer the columns of the file the user just picked. Suggestions only — the
# fields stay free text, so nothing in validation depends on them (see `_inject_dynamic_options!`).
Cecelia._needs_dynamic_options(::ImportCsvTracks) = true

function Cecelia._inject_dynamic_options!(spec::Dict{String,Any}, ::ImportCsvTracks,
                                          form::AbstractDict)::Dict{String,Any}
    path = string(get(form, "csvPath", ""))
    isempty(path) && return spec
    cols = _ict_headers(path; skip = Int(get(form, "skipRows", 0)))
    isempty(cols) && return spec
    opts = [Dict{String,Any}("label" => c, "value" => c) for c in cols]
    # walk the spec (params + section sub-params) and attach the options to each column field
    keys_wanted = Set(["trackColumn", "frameColumn", "xColumn", "yColumn", "zColumn"])
    function _attach!(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            string(get(p, "key", "")) ∈ keys_wanted && (p["options"] = deepcopy(opts))
            _attach!(get(p, "params", nothing))     # sections nest one level
        end
    end
    _attach!(get(spec, "params", nothing))
    spec
end

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
        # `frameBase` comes from a select, so it arrives as a STRING ("0"/"1"). An int slider for a
        # two-value choice was the wrong control — it read as a range of numbers to tune.
        haskey(params, k) && (map[k] = something(tryparse(Int, string(params[k])), 0))
    end
    u = string(get(params, "spotUnit", ""))
    isempty(u) || (map["spotUnit"] = u)
    map
end

function Cecelia._run_task(::ImportCsvTracks, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    vn   = string(get(params, "valueName", Cecelia.VERSIONED_DEFAULT_VAL))
    csv  = string(get(params, "csvPath", ""))
    maxd = Float64(get(params, "maxDistance", 10.0))

    # `track_id` is THE column, not a choice. Everything downstream reads exactly that name —
    # `track_props`, `tracking.track_measures`, `behaviour.hmm_transitions`, `is_tracked` — so writing
    # anywhere else would produce a column no other task can use, and offering the name as a param
    # would invite several rival track_id columns on one image. Importing tracks is an ALTERNATIVE to
    # running `tracking.bayesian_tracking`, so it writes where that writes.
    outcol = "track_id"

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
               maxDistance = maxd, physicalSizesZYX = sizes, mapping = map,
               delimiter = _ict_delimiter(csv)),
            Cecelia.task_run_dir(img._dir);
            on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] Track import failed"); return nothing)
    on_progress(1, 1)

    Dict{String,Any}("outputColumn" => outcol, "csvPath" => csv, "valueName" => vn,
                     "template" => string(get(params, "template", "")))
end

Cecelia.register_task!("tracking.importCsvTracks", ImportCsvTracks();
                       spec = joinpath(@__DIR__, "importCsvTracks.json"))   # co-located
