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
# `plotDefinitions/cumulative_change.json`. THAT pairing is the point of a plugin: a custom task and
# the custom module page that inspects it, shipped as one directory, with no Vue and no rebuild.
#
# It is a MEASURE, deliberately not bundled with the track importer: importing someone else's tracks
# and measuring them are different capabilities, and one repo should do one thing.
#
# **It takes TRACK POPULATIONS, not a segmentation.** That is the house convention for anything
# measured along tracks (`behaviour.hmm_states`, `behaviour.hmm_transitions`, `clustTracks.cluster`
# all do the same) — a `popSelection` with `popScope: "tracks"`, and NO `valueName` dropdown beside
# it, because each picker value already carries its segmentation as a prefix. The first version of
# this file had a `valueNameSelection` labelled "Segmentation", which was both the wrong picker and
# the wrong word for what the task consumes. See docs/MODULES.md → *popScope* and *Derive the
# segmentation from the pops*.
#
# The file is `include`d INTO the Cecelia module, so names resolve with the `Cecelia.` prefix.

using Statistics: mean

struct CumulativeChange <: Cecelia.CciaTask end

# Coerce the pops param (an array from the multi-select; tolerate a bare string) and drop the
# placeholder. Same shape `behaviour.hmm_states` uses — a track-scoped task takes TRACK POPULATIONS.
function _cc_pops(params)::Vector{String}
    raw = get(params, "pops", String[])
    ps  = raw isa AbstractString ? String[raw] : String[string(x) for x in raw]
    filter(p -> !isempty(p) && p != "NONE", ps)
end

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
    pops = _cc_pops(params)
    gap  = Int(get(params, "gap", 3))
    gap >= 1 || (on_log("[ERROR] gap must be >= 1"); return nothing)
    isempty(pops) && (on_log("[ERROR] Select at least one track population"); return nothing)

    # NO `valueName` param, and that is the convention rather than a shortcut (docs/MODULES.md →
    # *Derive the segmentation from the pops*). Each picker value carries its segmentation as a
    # prefix ("B/_tracked"), so a second dropdown would be both redundant and a footgun: pick a pop
    # from B while the dropdown says A and the run silently resolves to zero cells.
    vn = Cecelia.pops_value_name(pops)

    # `pop_df` is THE accessor (docs/POPULATION.md) — it resolves which cells are in the chosen
    # populations AND reads the columns in one narrow read. Reading the whole table through
    # `label_props` and filtering afterwards would be a second, divergent membership implementation.
    path = Cecelia.img_label_props_path(img, vn)
    isfile(path) || (on_log("[ERROR] No label props for '$vn'"); return nothing)
    cent = Cecelia.centroid_columns(Cecelia.label_props(path))   # centroid_z/y/x as present here
    temp = Cecelia.temporal_columns(Cecelia.label_props(path))   # the frame column, if a timelapse
    isempty(cent) && (on_log("[ERROR] No centroid columns found"); return nothing)

    on_progress(0, 1)
    # pop_type "live": a track population's members are resolved to their CELLS, which is what a
    # per-position measure needs. `_tracked` (track_id > 0) is the whole segmentation's tracks.
    #
    # `centroids = :pixel` rather than naming the coordinate columns in `pop_cols`: which axes exist
    # differs per segmentation (no `centroid_z` on a 2D image) and pop_df resolves that per
    # value_name. Pixels, not µm, because `gap` windows are compared within one image.
    cell = Cecelia.pop_df(img, "live", pops;
                          pop_cols = ["track_id"], centroids = :pixel, granularity = :cell)
    cols = Cecelia.DataFrames.names(cell)
    "track_id" in cols ||
        (on_log("[ERROR] '$vn' is not tracked (no track_id)"); return nothing)
    Cecelia.DataFrames.nrow(cell) == 0 &&
        (on_log("[ERROR] No tracked cells in the selected populations"); return nothing)

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
