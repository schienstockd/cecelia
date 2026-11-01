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
        -> (; name, png, resolved)

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
        png = read(r.figure)
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
    try
        r = make_figure!(pu, img, Dict{String,Any}(String(k) => v for (k, v) in spec);
                         name = _wstr(body, :name), replace = get(body, :replace, false) === true,
                         dry_run = get(body, :dryRun, false) === true)
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
