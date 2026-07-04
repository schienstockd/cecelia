# Segmentation integrity (QC) plot presets — thin wrappers over the summary-plot aggregator
# (`plot_data.jl`). One entry point shared by the REPL, the API, tests, and the whiteboard QC plot
# node, so "what a segmentation QC plot is" lives in exactly one place.
# See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.

# Morphology QC measures produced for every segmentation by `measure_labels` (skimage regionprops).
# Intensity QC (`mean_intensity_{c}`) is per-channel and resolved from the image's channel names at
# call time, so it isn't listed here.
const SEGMENTATION_QC_MEASURES = ["area", "solidity", "aspect_ratio", "eccentricity"]

# Temporal column name (e.g. "t") for an image's segmentation, or `nothing` for a static image.
# Drives the per-timepoint (temporal-consistency) view; absent → static, single per-image series.
function _segmentation_temporal_col(img::CciaImage, vn::AbstractString)::Union{String,Nothing}
    tcols = temporal_columns(label_props(img; value_name=String(vn)))
    isempty(tcols) ? nothing : String(first(tcols))
end

"""
    segmentation_qc_data(img; value_name=nothing, measure="area", chart_type=nothing,
                         per_timepoint=false)

Segmentation integrity plot data for a segmentation's cells — the **root** population (`"/"` = ALL
measured cells, gated or not) — reusing `plot_summary_data`.

- `chart_type` defaults to `"count"` when `measure === nothing` (the cell-count headline), else
  `"boxplot"` (a per-group distribution of the measure).
- `per_timepoint=true` splits series by the temporal column (the temporal-consistency view — cell
  count / morphology per frame, so drops and drift are visible). On a **static** image there is no
  temporal column, so it silently yields the single per-image series.

Returns the same `Dict` shape as `plot_summary_data`, so `PlotChart` renders it unchanged.
"""
function segmentation_qc_data(img::CciaImage;
                              value_name::Union{AbstractString,Nothing}=nothing,
                              measure::Union{AbstractString,Nothing}="area",
                              chart_type::Union{AbstractString,Nothing}=nothing,
                              per_timepoint::Bool=false)::Dict{String,Any}
    vn   = String(something(value_name, get(img.label_props, "_active", "default")))
    ct   = String(something(chart_type, measure === nothing ? "count" : "boxplot"))
    tcol = per_timepoint ? _segmentation_temporal_col(img, vn) : nothing
    plot_summary_data(img, "flow", ["/"], ct;
                      value_name = vn,
                      measure    = (ct == "count" ? nothing : measure),
                      group_by   = tcol)
end

"""
    segmentation_qc_data(imgs, uids; value_name, measure="area", chart_type=nothing,
                         per_timepoint=false)

Cross-image QC (one series per image, `scope=:per_image`) — the set-level integrity view. Same
options as the single-image method; the temporal column is resolved from the first image (the set
shares a segmentation pipeline). Empty `imgs` → empty result.
"""
function segmentation_qc_data(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector;
                              value_name::Union{AbstractString,Nothing}=nothing,
                              measure::Union{AbstractString,Nothing}="area",
                              chart_type::Union{AbstractString,Nothing}=nothing,
                              per_timepoint::Bool=false)::Dict{String,Any}
    isempty(imgs) && return Dict{String,Any}("series" => [], "chartType" => "count")
    vn   = String(something(value_name, get(first(imgs).label_props, "_active", "default")))
    ct   = String(something(chart_type, measure === nothing ? "count" : "boxplot"))
    tcol = per_timepoint ? _segmentation_temporal_col(first(imgs), vn) : nothing
    plot_summary_data(collect(imgs), collect(uids), "flow", ["/"], ct;
                      scope      = :per_image,
                      value_name = vn,
                      measure    = (ct == "count" ? nothing : measure),
                      group_by   = tcol)
end
