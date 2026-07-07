# QC framework — general "we processed this, but the output looks off" findings.
#
# Convention (docs/todo/QC_PLAN.md): one JSON per (task, output) at
#   1/{uid}/qc/{funName}/{valueName}.json
# with a generic `findings` list the GUI renders verbatim (badge + tooltip on the image, the
# MetadataPanel, and the chain whiteboard node). QC is ADVISORY — it never fails or gates a task.
# The backend (this layer + each task) computes findings so thresholds live in one place; the GUI
# only renders. This file is image-owned (like img_physical_sizes / pop_df) — any task emits QC the
# same way via `write_qc`, rather than each task hand-rolling its own sidecar.

const QC_DIRNAME = "qc"

qc_root(img::CciaImage) = joinpath(img._dir, QC_DIRNAME)
qc_fun_dir(img::CciaImage, fun_name::AbstractString) = joinpath(qc_root(img), string(fun_name))
# A task with no output value_name falls back to the default versioned key so there's always a key.
_qc_vn(value_name::AbstractString) = isempty(value_name) ? VERSIONED_DEFAULT_VAL : string(value_name)
qc_path(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    joinpath(qc_fun_dir(img, fun_name), _qc_vn(value_name) * ".json")

# One finding. `level ∈ ("info","warn")` — "error" is reserved (QC never blocks). `code` is a stable
# kebab/dotted slug (e.g. "drift.canvas_expansion") so the GUI can key styling/help off it.
function qc_finding(level::AbstractString, code::AbstractString,
                    short::AbstractString, long::AbstractString; detail = nothing)
    f = Dict{String,Any}("level" => string(level), "code" => string(code),
                         "short" => string(short), "long" => string(long))
    isnothing(detail) || (f["detail"] = detail)
    f
end

# Write (or clear) an image's QC for one (task, output). `findings` empty ⇒ still writes the file with
# an empty list, so a clean re-run overwrites a previous warning rather than leaving it stale.
function write_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString,
                  findings::AbstractVector; source = nothing, output = nothing, extras...)
    dir = qc_fun_dir(img, fun_name); mkpath(dir)
    doc = Dict{String,Any}("funName" => string(fun_name), "valueName" => _qc_vn(value_name),
                           "findings" => collect(findings))
    isnothing(source) || (doc["source"] = source)
    isnothing(output) || (doc["output"] = output)
    for (k, v) in extras; doc[string(k)] = v; end
    path = qc_path(img, fun_name, value_name)
    open(path, "w") do io; JSON3.write(io, doc); end
    path
end

read_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (p = qc_path(img, fun_name, value_name); isfile(p) ? JSON3.read(read(p, String)) : nothing)

# All QC docs for an image, keyed "funName/valueName" → parsed doc. Powers the API image payload.
function read_all_qc(img::CciaImage)
    root = qc_root(img); out = Dict{String,Any}()
    isdir(root) || return out
    for fun in readdir(root)
        fdir = joinpath(root, fun); isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") || continue
            out[string(fun, "/", f[1:end-5])] = JSON3.read(read(joinpath(fdir, f), String))
        end
    end
    out
end

# Reusable spatial check — flag an output whose XY canvas grew abnormally vs its source. Shapes are in
# `dim_order` (e.g. "TCZYX"). Generic across any spatially-transforming task (drift/AF correction, …);
# returns a finding or `nothing`. Default threshold 25% (normal drift expands XY ≤~15%).
function qc_canvas_expansion(source_shape, output_shape, dim_order::AbstractString;
                             threshold_pct::Real = 25, code::AbstractString = "output.canvas_expansion")
    order = collect(dim_order)
    yi = findfirst(==('Y'), order); xi = findfirst(==('X'), order)
    (isnothing(yi) || isnothing(xi)) && return nothing
    pct(i) = source_shape[i] > 0 ? 100 * (output_shape[i] - source_shape[i]) / source_shape[i] : 0.0
    ye, xe = pct(yi), pct(xi); m = max(ye, xe)
    m > threshold_pct || return nothing
    qc_finding("warn", code,
        "Output canvas grew +$(round(Int, m))% in XY",
        "The processed image's XY canvas grew by +$(round(Int, ye))%/$(round(Int, xe))% (Y/X), well " *
        "beyond the ≤~15% typical of a good correction. That usually means the estimation went wrong " *
        "(e.g. drift correction on a reference channel that didn't track) — check the output and " *
        "consider re-running.";
        detail = Dict{String,Any}("yExpansionPct" => round(ye, digits = 1),
                                  "xExpansionPct" => round(xe, digits = 1)))
end
