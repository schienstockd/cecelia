# Figures — render a spec on an image and bank it with the movies. docs/todo/FIGURES_PLAN.md.
#
# The package does the rendering (`Cecelia.render_figure`, which fills in everything the image knows);
# this file decides where the result lives: `{proj}/figures/<name>.png` — the ONE file kept — registered
# in the movie registry as `configKind: "figure"`, the authored spec, the as-rendered spec and the source
# in its entry. The Movies & figures page lists it; its star / tags / delete apply unchanged.

# Tell open Movies & figures pages (any window) to re-list — a figure is not a task, so the task-finished
# refresh never sees one.
_movies_changed(project_uid::AbstractString) =
    broadcast_ws(Dict{String,Any}("type" => "movies:changed", "projectUid" => String(project_uid)))

"""
    _figure_filename(dir, img, name, replace) -> String

`<safe name>.png` in `dir`: the requested `name`, else `<image name>_figure`. Without `replace` an
existing file gets `_001`, `_002`, … (`_numbered_if_taken`, the movies' rule) rather than being
overwritten. The viewer sends `replace` when its "number if taken" toggle is off.
"""
function _figure_filename(dir::AbstractString, img::CciaImage, name::AbstractString, replace::Bool)::String
    base = _safe_name_part(isempty(strip(name)) ? "$(img.name)_figure" : name)
    isempty(base) && (base = "figure")
    basename(_numbered_if_taken(joinpath(dir, "$base.png"), !replace))
end

"""
    make_figure!(project_uid, img, spec; name = "", replace = false, dry_run = false, tags, on_log)
        -> (; name, png, resolved)   # `png` = the preview (≤ 1600 px a side), not the print figure

Render `spec` on `img`. `dry_run` renders into a temp dir and keeps nothing — `png` is the bytes, for a
preview. Otherwise the figure is moved into the figures dir as `name` (see `_figure_filename`) and
registered (`producedBy = "figure"`, the authored `spec` and the as-rendered `resolved` spec as its
config, `author_stamp()` as `createdBy`/`updatedBy`).
"""
function make_figure!(project_uid::AbstractString, img::CciaImage, spec::AbstractDict;
                      name::AbstractString = "", replace::Bool = false, dry_run::Bool = false,
                      tags = String[],
                      on_log::Function = line -> nothing)
    tmp = mktempdir()
    try
        r = render_figure(img, spec; out_dir = tmp, on_log = on_log)
        png = read(isfile(r.preview) ? r.preview : r.figure)
        resolved = JSON3.read(read(r.spec, String))
        dry_run && return (; name = "", png, resolved)
        dir = _figures_dir_for_project(project_uid)
        mkpath(dir)
        fname = _figure_filename(dir, img, name, replace)
        mv(r.figure, joinpath(dir, fname); force = true)   # the preview and figure.json stay in tmp
        shown = unique!([String(something(get(ch, :name, nothing), "")) for row in get(resolved, :rows, [])
                         for ch in get(row, :channels, [])])
        register_movie!(project_uid, fname; produced_by = "figure", image_uid = img.uid,
                        channels = filter(!isempty, shown),
                        config = FigureConfig(imageUid = img.uid, spec = spec, resolved = resolved,
                                              source = figure_source(img, _spec_value_name(spec);
                                                                     spec_style = get(spec, "style", nothing))),
                        config_kind = "figure", author = author_stamp(), auto_tags = figure_auto_tags(spec, tags))
        _movies_changed(project_uid)
        (; name = fname, png, resolved)
    finally
        isdir(tmp) && rm(tmp; force = true, recursive = true)
    end
end

"""
    figure_auto_tags(spec, extra) -> Vector{String}

The tags a saved figure gets from its spec — `grid` (frames wrapped into a matrix), `split` (the merge
plus a row per channel) — plus what the caller knows that the spec can't say (`roi`).
"""
function figure_auto_tags(spec::AbstractDict, extra = String[])::Vector{String}
    tags = String[string(t) for t in extra]
    get(spec, "layout", nothing) == "grid" && push!(tags, "grid")
    rows = get(spec, "rows", nothing)
    rows isa AbstractVector && length(rows) > 1 && push!(tags, "split")
    tags
end

_spec_value_name(spec::AbstractDict) = (v = get(spec, "valueName", nothing); (v === nothing || v == "") ? nothing : String(v))

"""
    _figure_stale(project_uid, entry, images) -> Union{Nothing,String}

Why a listed figure no longer shows the data (`Cecelia.figure_stale_reason`), or `nothing` — also for a
figure saved before sources were recorded (nothing to compare). `images` caches each image's load
across one listing.
"""
function _figure_stale(project_uid::AbstractString, e::AbstractDict, images::AbstractDict)
    cfg = get(e, "config", nothing)
    src = cfg isa AbstractDict ? get(cfg, "source", nothing) : nothing
    src isa AbstractDict || return nothing
    iu = String(get(cfg, "imageUid", get(e, "imageUid", "")))
    isempty(iu) && return nothing
    img = get!(images, iu) do
        isfile(state_file(joinpath(projects_dir(), project_uid), iu)) ? init_object(project_uid, iu) : nothing
    end
    img isa CciaImage || return "its image is gone"
    spec = get(cfg, "spec", nothing)
    figure_stale_reason(img, Dict{String,Any}(String(k) => v for (k, v) in src);
                        spec_style = spec isa AbstractDict ? get(spec, "style", nothing) : nothing)
end

# Claude made it and nobody has re-made it since — the gate rule (`claude_made`, gating_api.jl).
figure_claude_made(e::AbstractDict) = claude_made(get(e, "createdBy", nothing), get(e, "updatedBy", nothing))

# A request from Claude may replace or delete only a figure Claude made: the 403 that names whose it is.
function _figure_own_check(project_uid::AbstractString, filename::AbstractString)
    REQUEST_VIA[] == "claude" || return nothing
    e = get(_read_movies_registry(project_uid), String(filename), nothing)
    (e === nothing || figure_claude_made(e)) && return nothing
    403, JSON3.write((; ok = false, error = "$(filename) is the user's figure — Claude may change only figures it made; " *
                                            "save a new one instead"))
end

# POST /api/figures/render {projectUid, imageUid, spec, name?, replace?, dryRun?, roiSuffix?}
#   → {ok, name, png (base64), resolved}. A spec error is a 400 whose `error` names the field.
function api_figures_render(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    pu, iu = _wstr(body, :projectUid), _wstr(body, :imageUid)
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    spec = get(body, :spec, nothing)
    spec isa AbstractDict || return 400, JSON3.write((; error = "spec: required, an object"))
    repl = get(body, :replace, false) === true
    dry  = get(body, :dryRun, false) === true
    if repl && !dry
        dir = _figures_dir_for_project(pu)
        target = _figure_filename(dir, img, _wstr(body, :name), true)
        isfile(joinpath(dir, target)) && (denied = _figure_own_check(pu, target); denied === nothing || return denied)
    end
    try
        r = make_figure!(pu, img, Dict{String,Any}(String(k) => v for (k, v) in spec);
                         name = _wstr(body, :name), replace = repl, dry_run = dry,
                         # an ROI figure (the viewer's Save as figure → ROIs) is tagged as its movies are
                         tags = haskey(body, :roiSuffix) ? roi_movie_tags(_wstr(body, :roiSuffix), false) : String[])
        200, JSON3.write((; ok = true, name = r.name, png = base64encode(r.png), resolved = r.resolved))
    catch e
        msg = sprint(showerror, e)
        # The renderer's own spec errors and the channel resolver's both name the field — a caller can
        # fix the spec from the message, so they are the client's 400, not a server fault.
        status = occursin("figure spec:", msg) || occursin("figure channel", msg) ||
                 occursin("no image version", msg) ? 400 : 500
        status, JSON3.write((; ok = false, error = replace(msg, r"^figure: the renderer failed: " => "")))
    end
end

# POST /api/figures/delete-own {projectUid, name, dryRun?} — remove a figure CLAUDE made (its file and
# registry entry). Any other figure is the user's: 403. The general `/api/movies/delete` stays
# the GUI's.
function api_figures_delete_own(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    pu, name = _wstr(body, :projectUid), _wstr(body, :name)
    isempty(pu) && return 400, JSON3.write((; error = "projectUid required"))
    (_valid_movie_name(name) && _movie_kind(name) == "figure") ||
        return 400, JSON3.write((; error = "name: a figure's file name, e.g. cell_figure.png"))
    isfile(_media_path(pu, name)) || return 404, JSON3.write((; error = "No such figure: $name"))
    e = get(_read_movies_registry(pu), name, Dict{String,Any}())
    figure_claude_made(e) || return 403, JSON3.write((; ok = false,
        error = "$name is the user's figure — Claude may delete only figures it made"))
    get(body, :dryRun, false) === true && return 200, JSON3.write((; ok = true, dryRun = true, wouldDelete = name))
    reg = _read_movies_registry(pu)
    _delete_movie_file!(pu, reg, name)
    _write_movies_registry!(pu, reg)
    _movies_changed(pu)
    200, JSON3.write((; ok = true, deleted = name))
end

# GET /api/figures/style?projectUid → {defaults, limits, choices, style} — the house style file and the
# project's overrides of it (`{}` = the house style). POST {projectUid, style} replaces the overrides;
# a bad key is a 400 naming it, and nothing is written. Figures drawn in the old style list as out of
# date (`figure_stale_reason`).
function api_figures_style_get(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(q, "projectUid", "")
    pdir = joinpath(projects_dir(), uid)
    (isempty(uid) || !isdir(pdir)) && return 404, JSON3.write((; error = "Project not found"))
    schema = figure_style_schema()
    200, JSON3.write((; defaults = schema[:defaults], limits = schema[:limits], choices = schema[:choices],
                       style = project_figure_style(pdir)))
end

function api_figures_style_set(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    uid = _wstr(body, :projectUid)
    pdir = joinpath(projects_dir(), uid)
    (isempty(uid) || !isdir(pdir)) && return 404, JSON3.write((; error = "Project not found"))
    raw = get(body, :style, nothing)
    raw isa AbstractDict || return 400, JSON3.write((; error = "style: required, an object"))
    errs = set_project_figure_style!(pdir, raw)
    isempty(errs) || return 400, JSON3.write((; ok = false, error = join(errs, "; ")))
    _movies_changed(uid)                    # the list re-reads which figures are now out of date
    200, JSON3.write((; ok = true, style = project_figure_style(pdir)))
end
