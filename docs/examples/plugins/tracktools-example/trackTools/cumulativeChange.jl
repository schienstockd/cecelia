# Example PLUGIN task — a track behaviour measurement variant, in the plugin's OWN category.
#
# This is the Feijoa counterpart of the old R tutorial's `behaviourAnalysis.cumulativeChange`
# (https://cecelia.readthedocs.io/en/latest/create_custom_module.html): the standard per-step speed is
# computed between CONSECUTIVE positions, which is noisy for a cell that jitters in place. Measuring
# the same quantities over a larger temporal gap smooths that out and is often what actually separates
# migrating from searching cells — the reason the R tutorial used it as its worked example.
#
# For each cell it writes, over a gap of `gap` positions along the cell's own track:
#   trackTools.cumulativeDisplacement  — straight-line distance from the position `gap` steps back
#   trackTools.cumulativeSpeed         — that distance divided by `gap`
#   trackTools.cumulativeStraightness  — net displacement / path length over the same window (0..1;
#                                        ~1 = directed migration, ~0 = searching in place)
#
# Category `trackTools` has no built-in page, so this task appears on the generic `/custom/trackTools`
# page — together with the plot canvas fed by this plugin's own
# `plotDefinitions/cumulative_change.json`. THAT pairing is the point of the plugin: a custom task and
# the custom module page that inspects it, shipped as one directory, with no Vue and no rebuild.
#
# The file is `include`d INTO the Cecelia module, so names resolve with the `Cecelia.` prefix.

using Statistics: mean

struct CumulativeChange <: Cecelia.CciaTask end

# Straight-line distance between two rows of a coordinate matrix.
_cc_dist(p, q) = sqrt(sum(abs2, p .- q))

"""
Per-track cumulative measures over `gap` steps. `coords` is one track's positions in time order.
Returns three vectors aligned with `coords`; the first `gap` entries are NaN (no window yet), which is
the honest answer rather than a partial window silently compared against full ones.
"""
function _cc_track(coords::Vector{Vector{Float64}}, gap::Int)
    n = length(coords)
    disp   = fill(NaN, n)
    speed  = fill(NaN, n)
    straig = fill(NaN, n)
    # step lengths once, so the path length over each window is a cheap running sum
    steps = [i == 1 ? 0.0 : _cc_dist(coords[i], coords[i - 1]) for i in 1:n]
    for i in (gap + 1):n
        d = _cc_dist(coords[i], coords[i - gap])
        path = sum(@view steps[(i - gap + 1):i])
        disp[i]   = d
        speed[i]  = d / gap
        straig[i] = path > 0 ? d / path : NaN   # path==0 means the cell never moved — undefined, not 0
    end
    (disp, speed, straig)
end

function Cecelia._run_task(::CumulativeChange, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    vn  = string(get(params, "valueName", Cecelia.VERSIONED_DEFAULT_VAL))
    gap = Int(get(params, "gap", 3))
    gap >= 1 || (on_log("[ERROR] gap must be >= 1"); return nothing)

    # Resolve the file ONCE and read *and* write through that same path. Reading via
    # `label_props(img; value_name=…)` instead would consult the image's registered label_props map,
    # which `img_label_props_path` falls back past — so a value_name missing from ccid.json passes the
    # isfile check here and then errors inside the read. One resolution, no divergence.
    path = Cecelia.img_label_props_path(img, vn)
    isfile(path) || (on_log("[ERROR] No label props for valueName='$vn'"); return nothing)

    # Read through the sanctioned view — never touch the .h5ad directly (CLAUDE.md / DATAMODEL.md).
    view = Cecelia.label_props(path)
    cent = Cecelia.centroid_columns(view)      # centroid_z/y/x as present for this image
    temp = Cecelia.temporal_columns(view)      # the frame/time column, if this segmentation is a timelapse
    isempty(cent) && (on_log("[ERROR] No centroid columns found"); return nothing)

    want = vcat(["track_id"], cent, temp)
    cell = view |> Cecelia.select_cols(want) |> Cecelia.as_df
    cols = Cecelia.DataFrames.names(cell)
    "track_id" in cols ||
        (on_log("[ERROR] Segmentation '$vn' is not tracked (no track_id)"); return nothing)

    on_progress(0, 1)
    # Untracked cells carry track_id <= 0 / missing; they have no window, so drop them up front.
    keep = [r isa Number && isfinite(r) && r > 0 for r in cell[!, "track_id"]]
    cell = cell[keep, :]
    Cecelia.DataFrames.nrow(cell) == 0 &&
        (on_log("[ERROR] No tracked cells in '$vn'"); return nothing)

    # Order within a track matters — these are temporal windows. Sort by the time column when there is
    # one; row order is NOT a time order in general, and silently trusting it would compute distances
    # between unrelated positions.
    tcol = isempty(temp) ? nothing : first(temp)
    isnothing(tcol) && on_log("[WARNING] No temporal column — using row order within each track")

    labels_out = Int[]
    d_out, s_out, st_out = Float64[], Float64[], Float64[]
    for grp in Cecelia.DataFrames.groupby(cell, :track_id)
        g = isnothing(tcol) ? grp : sort(Cecelia.DataFrames.DataFrame(grp), tcol)
        coords = [Float64[Float64(g[i, c]) for c in cent] for i in 1:Cecelia.DataFrames.nrow(g)]
        d, s, st = _cc_track(coords, gap)
        append!(labels_out, Int.(g[!, :label]))
        append!(d_out, d); append!(s_out, s); append!(st_out, st)
    end

    out = Cecelia.DataFrames.DataFrame(
        :label => labels_out,
        Symbol("trackTools.cumulativeDisplacement") => d_out,
        Symbol("trackTools.cumulativeSpeed")        => s_out,
        Symbol("trackTools.cumulativeStraightness") => st_out)
    Cecelia.label_props(path) |> Cecelia.add_obs(out) |> Cecelia.save!

    n_win = count(isfinite, s_out)
    on_progress(1, 1)
    on_log("[INFO] Wrote 3 cumulative measures (gap=$gap) for $n_win of $(length(s_out)) cells")

    Dict{String,Any}("gap" => gap, "cells" => length(s_out), "withWindow" => n_win,
                     "meanCumulativeSpeed" => n_win > 0 ? mean(filter(isfinite, s_out)) : NaN)
end

Cecelia.register_task!("trackTools.cumulativeChange", CumulativeChange();
                       spec = joinpath(@__DIR__, "cumulativeChange.json"))   # co-located
