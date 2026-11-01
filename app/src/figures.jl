# Figures — a still, a filmstrip or a z-strip of one region, rendered from the raw image in one
# standard look. docs/todo/FIGURES_PLAN.md.
#
# The renderer is `python/cecelia/writers/render_figure.py` (the viewer's own shader, via the shared
# still renderer). This file owns the half that needs the image model: a spec names WHAT to show
# (crop, frames, rows of channels, contrast); everything the image already knows — its store, channel
# names, pixel size, z step, frame interval — is filled in here from the image's own accessors, so a
# spec can't disagree with its image and nobody hard-codes a pixel size or an interval again.
#
# Registering the result with the Movies page is the API's job (`api/src/figures_api.jl`); from the
# REPL this renders to a directory and returns the paths.

"""
    figure_params(img, spec) -> Dict{String,Any}

The renderer's params for `spec` on `img`: the spec plus `zarrPath` (the spec's `valueName`, default the
active image version), `channelNames`, and — only when the image records them (`img_scale_axes`) —
`pixelSizeUm` / `zStepUm` / `zAniso` and `timeIncrementS`. Without a recorded XY scale there is no
scale bar; without a recorded interval, frames are labelled `t<n>`. Raises when the version doesn't exist.
"""
function figure_params(img::CciaImage, spec::AbstractDict)::Dict{String,Any}
    p = Dict{String,Any}(String(k) => v for (k, v) in spec)
    vn = get(p, "valueName", nothing)
    vn = (vn === nothing || vn == "") ? nothing : String(vn)
    zp = img_filepath(img, vn)
    zp === nothing && error("figure: image $(img.uid) has no image version $(repr(vn)) " *
                            "(has: $(join(sort!(collect(versioned_keys(img.filepath))), ", ")))")
    p["zarrPath"] = zp
    names = something(channel_names(img; value_name = vn), String[])
    haskey(p, "channelNames") || (p["channelNames"] = names)
    # A row's channel may be a NAME (what the user knows) or a 0-based index — `channel_indices` is the
    # one resolver, and it raises by name on a miss.
    p["rows"] = [_figure_row(r, names) for r in get(p, "rows", Any[])]
    # Explicit display ranges may be keyed by name too (the viewer's on-screen contrast is).
    disp = get(p, "display", nothing)
    if disp isa AbstractDict && get(disp, "ranges", nothing) isa AbstractDict
        d = Dict{String,Any}(String(k) => v for (k, v) in disp)
        d["ranges"] = Dict{String,Any}(string(only(channel_indices([_channel_key(k)], names;
                                                                   what = "figure display range"))) => v
                                       for (k, v) in d["ranges"])
        p["display"] = d
    end
    axes = img_scale_axes(img)
    sizes, step_min = img_physical_sizes(img)          # (z, y, x) µm; minutes per frame
    if :XY in axes
        p["pixelSizeUm"] = sizes[end]
        :Z in axes && (p["zStepUm"] = sizes[1]; p["zAniso"] = sizes[1] / sizes[end])
    else
        delete!(p, "scaleBarUm")
    end
    :T in axes && (p["timeIncrementS"] = step_min * 60)
    p
end

function _figure_row(row::AbstractDict, names::Vector{String})::Dict{String,Any}
    r = Dict{String,Any}(String(k) => v for (k, v) in row)
    r["channels"] = map(get(r, "channels", Any[])) do ch
        c = Dict{String,Any}(String(k) => v for (k, v) in ch)
        c["c"] = only(channel_indices([c["c"]], names; what = "figure channel"))
        c
    end
    r
end
_figure_row(row, names) = row          # not a dict: left for the renderer's own validation to name

# A range key arrives as a JSON object key — always a string — so "2" is the index 2, not a name.
_channel_key(k) = (s = String(k); i = tryparse(Int, s); i === nothing ? s : i)

"""
    render_figure(img, spec; out_dir, task_dir = task_run_dir(img._dir), on_log = println) -> (; figure, spec, preview)

Render `spec` on `img` into `out_dir`: `figure.png` (the layout at the style's dpi), `preview.png` (≤ 1600 px
a side) and `figure.json` (the spec as rendered — measured display ranges included). `spec` holds only
what to show; see `render_figure.py` for its fields (`kind`, `center`, `sizePx`, `z`, `zHalf`, `frames` or
`t` + `planes`, `rows` (a channel's `c` is a name or a 0-based index), `display`, `interpolation`, `scaleBarUm`, `style`, `valueName`).

A bad spec fails with the renderer's message, which names the field.

```julia
render_figure(img, Dict("center" => [597, 361], "sizePx" => 90, "z" => 9, "zHalf" => 1,
                        "frames" => [50, 64, 80], "valueName" => "denoised",
                        "rows" => [Dict("channels" => [Dict("c" => "mem-TOM", "colour" => "#ff00ff", "name" => "tdTom")])],
                        "scaleBarUm" => 5); out_dir = "fig")
```
"""
function render_figure(img::CciaImage, spec::AbstractDict; out_dir::AbstractString,
                       task_dir::AbstractString = task_run_dir(img._dir), on_log::Function = println)
    params = figure_params(img, spec)
    params["outDir"] = abspath(out_dir)
    mkpath(out_dir)
    errs = String[]
    log = line -> (startswith(line, "[ERROR]") && push!(errs, line); on_log(line))
    ok = run_py("writers/render_figure.py", params, task_dir; on_log = log)
    ok || error("figure: the renderer failed" * (isempty(errs) ? " — see the log" : ": " * join(errs, "; ")))
    (; figure = joinpath(params["outDir"], "figure.png"), spec = joinpath(params["outDir"], "figure.json"),
       preview = joinpath(params["outDir"], "preview.png"))
end

# ── Staleness (FIGURES_PLAN P4) ───────────────────────────────────────────────────────────────────
# A figure draws image pixels only, so it describes the data while the image version it was rendered
# from is the same store. Recorded at render time; compared when the figure is listed.

"""
    figure_source(img, value_name = nothing) -> Dict

What a figure was rendered from: the image version's name (the active one when `nothing`), its
version (`vN`) and the store's mtime. A re-process replaces the store (staged writes rename a new
directory into place), so its mtime moves even when the version name does not.
"""
function figure_source(img::CciaImage, value_name::Union{AbstractString,Nothing} = nothing)::Dict{String,Any}
    # the IMAGE version (`filepath`), not `resolve_value_name` — that resolves a segmentation
    vn = value_name === nothing ? String(versioned_active(img.filepath)) : String(value_name)
    zp = img_filepath(img, vn)
    Dict{String,Any}("valueName" => vn, "version" => resolve_version(img, :filepath, vn),
                     "storeMtime" => (zp !== nothing && ispath(zp)) ? mtime(zp) : nothing)
end

"""
    figure_stale_reason(img, source) -> Union{Nothing,String}

Why a figure rendered from `source` (`figure_source`) no longer shows the data, or `nothing`.
"""
function figure_stale_reason(img::CciaImage, source::AbstractDict)::Union{Nothing,String}
    vn = String(get(source, "valueName", ""))
    zp = img_filepath(img, isempty(vn) ? nothing : vn)
    (zp === nothing || !ispath(zp)) && return "image version $(repr(vn)) is gone"
    now = figure_source(img, isempty(vn) ? nothing : vn)
    now["version"] == get(source, "version", now["version"]) ||
        return "image version $vn is now $(now["version"]) (was $(source["version"]))"
    was = get(source, "storeMtime", nothing)
    (was isa Real && now["storeMtime"] isa Real && now["storeMtime"] > was + 1) &&
        return "image version $vn was re-processed since"
    nothing
end
