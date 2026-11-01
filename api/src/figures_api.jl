# Figures — render a spec on an image and bank it beside the movies. docs/todo/FIGURES_PLAN.md.
#
# The package does the rendering (`Cecelia.render_figure`, which fills in everything the image knows);
# this file decides where the result lives: `{proj}/movies/<name>.png` registered in the movie registry
# as `configKind: "figure"`, with its tiles and the as-rendered spec in `{proj}/movies/.figures/<stem>/`.
# The Movies page lists it, and its star / tags / delete apply unchanged.

# The per-figure side files: tiles + `figure.json`. Hidden from the listing (a directory, dot-named).
_figure_extras_dir(project_uid::AbstractString, filename::AbstractString)::String =
    joinpath(_movies_dir_for_project(String(project_uid)), ".figures", splitext(filename)[1])

"""
    _figure_filename(dir, img, name, replace) -> String

`<safe name>.png` in `dir`: the requested `name`, else `<image name>_figure`. Without `replace` an
existing file gets `-2`, `-3`, … rather than being overwritten — a figure is a deliberate artefact,
unlike a movie, which is named after its image and re-recorded in place.
"""
function _figure_filename(dir::AbstractString, img::CciaImage, name::AbstractString, replace::Bool)::String
    base = _safe_name_part(isempty(strip(name)) ? "$(img.name)_figure" : name)
    isempty(base) && (base = "figure")
    candidate = "$base.png"
    replace && return candidate
    k = 2
    while isfile(joinpath(dir, candidate))
        candidate = "$base-$k.png"; k += 1
    end
    candidate
end

"""
    make_figure!(project_uid, img, spec; name = "", replace = false, dry_run = false, on_log)
        -> (; name, png, resolved)   # `png` = the preview (≤ 1600 px a side), not the print figure

Render `spec` on `img`. `dry_run` renders into a temp dir and keeps nothing — `png` is the bytes, for a
preview. Otherwise the figure is moved into the movies dir as `name` (see `_figure_filename`) and
registered (`producedBy = "figure"`, the authored `spec` and the as-rendered `resolved` spec as its
config, `author_stamp()` as `createdBy`/`updatedBy`).
"""
function make_figure!(project_uid::AbstractString, img::CciaImage, spec::AbstractDict;
                      name::AbstractString = "", replace::Bool = false, dry_run::Bool = false,
                      on_log::Function = line -> nothing)
    tmp = mktempdir()
    try
        r = render_figure(img, spec; out_dir = tmp, on_log = on_log)
        png = read(isfile(r.preview) ? r.preview : r.figure)
        resolved = JSON3.read(read(r.spec, String))
        dry_run && return (; name = "", png, resolved)
        dir = _movies_dir_for_project(String(project_uid))
        mkpath(dir)
        fname = _figure_filename(dir, img, name, replace)
        extras = _figure_extras_dir(project_uid, fname)
        rm(extras; force = true, recursive = true)
        mkpath(dirname(extras))
        mv(r.figure, joinpath(dir, fname); force = true)
        mv(tmp, extras; force = true)
        shown = unique!([String(something(get(ch, :name, nothing), "")) for row in get(resolved, :rows, [])
                         for ch in get(row, :channels, [])])
        register_movie!(project_uid, fname; produced_by = "figure", image_uid = img.uid,
                        channels = filter(!isempty, shown),
                        config = FigureConfig(imageUid = img.uid, spec = spec, resolved = resolved),
                        config_kind = "figure", author = author_stamp())
        (; name = fname, png, resolved)
    finally
        isdir(tmp) && rm(tmp; force = true, recursive = true)
    end
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

# POST /api/figures/render {projectUid, imageUid, spec, name?, replace?, dryRun?}
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
        dir = _movies_dir_for_project(pu)
        target = _figure_filename(dir, img, _wstr(body, :name), true)
        isfile(joinpath(dir, target)) && (denied = _figure_own_check(pu, target); denied === nothing || return denied)
    end
    try
        r = make_figure!(pu, img, Dict{String,Any}(String(k) => v for (k, v) in spec);
                         name = _wstr(body, :name), replace = repl, dry_run = dry)
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

# POST /api/figures/delete-own {projectUid, name, dryRun?} — remove a figure CLAUDE made (its file, side
# files and registry entry). Any other figure is the user's: 403. The general `/api/movies/delete` stays
# the GUI's.
function api_figures_delete_own(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    pu, name = _wstr(body, :projectUid), _wstr(body, :name)
    isempty(pu) && return 400, JSON3.write((; error = "projectUid required"))
    (_valid_movie_name(name) && _movie_kind(name) == "figure") ||
        return 400, JSON3.write((; error = "name: a figure's file name, e.g. cell_figure.png"))
    dir = _movies_dir_for_project(pu)
    isfile(joinpath(dir, name)) || return 404, JSON3.write((; error = "No such figure: $name"))
    e = get(_read_movies_registry(pu), name, Dict{String,Any}())
    figure_claude_made(e) || return 403, JSON3.write((; ok = false,
        error = "$name is the user's figure — Claude may delete only figures it made"))
    get(body, :dryRun, false) === true && return 200, JSON3.write((; ok = true, dryRun = true, wouldDelete = name))
    reg = _read_movies_registry(pu)
    _delete_movie_file!(pu, reg, name)
    _write_movies_registry!(pu, reg)
    200, JSON3.write((; ok = true, deleted = name))
end
