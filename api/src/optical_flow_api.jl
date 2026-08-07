# ── Optical-flow model vault (API layer) ────────────────────────────────────────
# Read/rename/delete for `<config_dir>/models/coastalModels/`. The vault is MACHINE-LOCAL and shared
# across projects — deliberately, so a model trained on one movie can be applied to any set — which
# also means it does not travel with a `.ccbundle` export. That is why the manager shows what each
# model was trained on: the manifest is the only way to tell whether a model fits an image.
#
# A model is a PAIR — `<name>.pt` plus a `<name>.json` manifest — so rename and delete must move or
# remove both. An orphaned manifest is harmless; an orphaned `.pt` is not, because `CoastalUtils`
# would silently fall back to coastal's default metric set and feed the model misaligned channels.
#
# Enumeration itself lives in `Cecelia.list_coastal_models` (config.jl), which the task's picker also
# uses — the manager must not grow a second listing that can disagree with the dropdown.
# See docs/todo/OPTICAL_FLOW_MODULE_PLAN.md.

using Dates

_flow_model_stem(name::AbstractString) = String(first(splitext(String(name))))

"""A vault filename that is provably a leaf inside the vault, or `nothing`."""
function _safe_flow_model(name)::Union{String,Nothing}
    s = String(strip(String(name)))
    (isempty(s) || occursin(r"[/\\]", s) || s in (".", "..")) && return nothing
    endswith(s, ".pt") || return nothing
    s
end

function api_optical_flow_models(::HTTP.Request)
    dir = Cecelia.coastal_models_dir()
    models = map(Cecelia.list_coastal_models()) do m
        path = joinpath(dir, m.name)
        (; name = m.name, label = m.label, stem = _flow_model_stem(m.name),
           bytes = isfile(path) ? filesize(path) : 0,
           modified = isfile(path) ? Dates.format(Dates.unix2datetime(mtime(path)),
                                                  "yyyy-mm-dd") : "",
           hasManifest = !isempty(m.manifest),
           manifest = m.manifest)
    end
    200, JSON3.write((; dir = dir, models = models))
end

function api_optical_flow_rename(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    from = _safe_flow_model(get(body, :name, ""))
    to_stem = String(strip(String(get(body, :newName, ""))))
    isnothing(from) && return 400, JSON3.write((; error = "name required (a .pt in the vault)"))
    (isempty(to_stem) || occursin(r"[/\\]", to_stem) || to_stem in (".", "..")) &&
        return 400, JSON3.write((; error = "New name cannot be empty or contain a path separator"))
    to_stem = _flow_model_stem(to_stem)

    dir = Cecelia.coastal_models_dir()
    src = joinpath(dir, from)
    isfile(src) || return 404, JSON3.write((; error = "Model not found: $from"))
    dest = joinpath(dir, "$(to_stem).pt")
    src == dest && return 200, JSON3.write((; ok = true, name = basename(dest)))
    isfile(dest) && return 409, JSON3.write((; error = "A model named '$to_stem' already exists"))

    mv(src, dest)
    # The manifest moves with the weights or the pair breaks apart silently.
    src_manifest = joinpath(dir, "$(_flow_model_stem(from)).json")
    isfile(src_manifest) && mv(src_manifest, joinpath(dir, "$(to_stem).json"); force = true)
    200, JSON3.write((; ok = true, name = basename(dest)))
end

function api_optical_flow_delete(body_bytes::Vector{UInt8})
    body = JSON3.read(String(body_bytes))
    name = _safe_flow_model(get(body, :name, ""))
    isnothing(name) && return 400, JSON3.write((; error = "name required (a .pt in the vault)"))

    dir = Cecelia.coastal_models_dir()
    path = joinpath(dir, name)
    isfile(path) || return 404, JSON3.write((; error = "Model not found: $name"))
    rm(path)
    manifest = joinpath(dir, "$(_flow_model_stem(name)).json")
    isfile(manifest) && rm(manifest)
    200, JSON3.write((; ok = true))
end

# ── Flow metric planes for the canvas panel ─────────────────────────────────────
# "What goes INTO the model": every flow metric plane for one timepoint, as PNGs the browser can
# show, so the user can see which of them look like cells before choosing what to train on. A model
# is OPTIONAL and only adds the probability map — the metrics are a property of the movie and the
# temporal scales, and the question is asked before any model exists.
#
# No instances: those are segmentation output and the Segment page previews them already.
# These are CANVAS PLOTS — nothing here touches napari.
#
# Deliberately NOT `api_preview_run`. That route exists to keep the viewer honest: it refuses unless
# the image is the one open in napari, because a preview draws INTO the viewer. A canvas plot has no
# such coupling and must work with napari closed, so it resolves the store from `ccid.json` the way
# `api_crop_frame` does instead of from the open layer.
#
# The compute is the resident preview worker (`opticalFlow.inspect` backend) — already loaded with
# coastal, the model and the temporal-window logic, and reached through the same `preview_request`
# client as every other preview, so the planes shown are the planes the run is actually fed.
function api_optical_flow_inspect(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes), Dict{String,Any}) catch
        return 400, JSON3.write((; error = "invalid JSON body")) end

    project_uid = String(get(data, "projectUid", ""))
    image_uid   = String(get(data, "imageUid", ""))
    value_name  = String(get(data, "valueName", VERSIONED_DEFAULT_VAL))
    model_name  = String(get(data, "model", ""))
    (isempty(project_uid) || isempty(image_uid)) &&
        return 400, JSON3.write((; error = "projectUid + imageUid required"))

    zp, task_dir, err = resolve_image_version(project_uid, image_uid, value_name)
    err === nothing || return 404, JSON3.write((; error = err))

    raw = read_ccid_raw(state_file(joinpath(projects_dir(), project_uid), image_uid))
    models = Dict{String,Any}("0" => Dict{String,Any}(
        "model"        => model_name,
        "matchAs"      => "base",
        "cellChannels" => get(data, "cellChannels", Any[])))
    # With no model these ARE the feature set (`CoastalUtils._manifest` falls back to them); with one
    # they are ignored, because a trained model's manifest must win over anything a panel sends.
    haskey(data, "temporalScales") && (models["0"]["temporalScales"] = data["temporalScales"])
    haskey(data, "cumulativeWindow") && (models["0"]["cumulativeWindow"] = data["cumulativeWindow"])
    params = Dict{String,Any}("valueName" => value_name, "models" => models,
                              "normaliseToWhole" => true)
    # merge any inference params the panel passes through, so it shows what the CURRENT settings do
    for (k, v) in get(data, "modelParams", Dict{String,Any}())
        models["0"][String(k)] = v
    end
    prepared = try
        Dict{String,Any}("valueName" => value_name, "normaliseToWhole" => true,
                         "models" => coastal_models_for_python(params, raw; require_model = false))
    catch e
        return 400, JSON3.write((; error = e isa ErrorException ? e.msg : sprint(showerror, e),
                                   code = "params-not-previewable"))
    end

    ready = _ensure_preview!()
    ready || return 202, JSON3.write((; starting = true,
                                        message = "Preview worker is starting."))

    # Whole frame at the requested t/z — a canvas plot is not a zoomed region, and `preview_region_bounds`
    # is the one place that clamps and turns an index into a half-open pair.
    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    xy = Dict{String,Any}("X" => [0, size(arr, d["x"])], "Y" => [0, size(arr, d["y"])])
    region = Dict{String,Any}("xy" => xy,
                              "z"  => get(data, "z", nothing),
                              "t"  => get(data, "t", 0),
                              "ndisplay" => 2)

    # `preview_request` BUILDS the request; `send` is what runs it. Returning the request itself is a
    # 200 full of plausible-looking JSON that the panel then reads no `planes` out of — which is
    # exactly what this route did until it was called for real.
    reply = try
        _with_preview() do
            w = _preview()
            w === nothing && error("preview worker is not running")
            send(w, preview_request(String(zp), String(task_dir), prepared, region;
                                    value_name = value_name, fun_name = "opticalFlow.inspect",
                                    channel_names = ccid_channel_names(raw)))
        end
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end

    haskey(reply, "planes") ||
        return 500, JSON3.write((; error = "preview worker returned no planes: " *
                                           String(get(reply, "error", "unknown"))))
    200, JSON3.write(reply)
end
