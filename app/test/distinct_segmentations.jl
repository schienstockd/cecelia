# ── A second segmentation that is genuinely DIFFERENT from the first ───────────────────────────────
# Shared by the package suite (`app/test/runtests.jl`) and the API suite (`api/test/runtests.jl`).
#
# "Which segmentation did this read?" is the biggest wrong-result class in the ledger
# (docs/todo/HARDENING_RESULT_CORRECTNESS.md §6 gap 5). Against a verbatim copy of `B`, a read that
# takes T's rows where it wanted B's gives the same numbers, and a duplicated pool leaves every
# median/min/max unchanged. This derives `T` from `B` and then alters it through the sanctioned writer
# (`label_props |> add_obs |> save!` / `drop_obs`), so the two tables disagree on values a test can assert:
#
#   * `clusters.movement` in `T__tracks.h5ad`: every Directed track (code 1) is relabelled Scanning
#     (code 0). B is 21/20/21 tracks (Scanning/Directed/Meandering, test-data/README.md); T is 41/0/21.
#   * `clustered = false`: T keeps its tracks but has NO clustering run — the column is dropped and
#     the `clustfeatures` sidecar is not copied (a tracked peer with no trackclust run).
#
# Track ids and cell rows stay B's, so per-track/per-cell joins still line up; only the cluster
# assignment — the thing every trackclust read selects on — differs.

"""
    distinct_segmentation!(lpdir; from = "B", to = "T", clustered = true) -> NamedTuple

Copy `from`'s cell table, track table (and clustfeatures sidecar when `clustered`) in the labelProps
dir `lpdir` to `to`, then alter `to`'s track clusters as described above. Returns the per-cluster
track counts read back from both tables, `(; from = Dict(code => n), to = Dict(code => n))`, so a
test asserts against what is on disk rather than a number copied from the README.
"""
function distinct_segmentation!(lpdir::AbstractString; from::AbstractString = "B",
                                to::AbstractString = "T", clustered::Bool = true)
    files = ["$from.h5ad", "$(from)__tracks.h5ad"]
    clustered && push!(files, "$(from)__tracks.clustfeatures.json")
    for f in files
        cp(joinpath(lpdir, f), joinpath(lpdir, to * f[ncodeunits(from)+1:end]); force = true)
    end
    col = "clusters.movement"
    tpath = joinpath(lpdir, "$(to)__tracks.h5ad")
    if clustered
        tt = label_props(tpath) |> select_cols([col]) |> as_df
        moved = Float64[c == 1 ? 0.0 : Float64(c) for c in tt[!, col]]
        label_props(tpath) |> add_obs(Cecelia.DataFrames.DataFrame("label" => tt.label, col => moved)) |> save!
    else
        label_props(tpath) |> drop_obs([col]) |> save!
    end
    counts(p) = (col in col_names(label_props(p); data_type = :obs)) ?
        (d = label_props(p) |> select_cols([col]) |> as_df;
         Dict(Int(c) => count(==(c), d[!, col]) for c in unique(d[!, col]))) : Dict{Int,Int}()
    (; from = counts(joinpath(lpdir, "$(from)__tracks.h5ad")), to = counts(tpath))
end

# ── A second IMAGE (or segmentation) whose rows differ: test-data KDIeEx ─────────────────────────────
# KDIeEx (python/cecelia/tests/make_distinct_fixture.py) is KDIeEm's B with whole tracks whose id is
# divisible by 3, and untracked cells whose label is, removed; `area` and `live.track.speed` doubled.
# The rule is restated here so a test derives what to expect from B itself, never from KDIeEx.

const DISTINCT_SCALE = 2.0

"""Whether KDIeEx keeps a B cell (`track_id` missing/NaN = untracked)."""
distinct_keeps_cell(label::Integer, track_id) =
    (track_id === nothing || ismissing(track_id) || (track_id isa Real && isnan(track_id))) ?
        label % 3 != 0 : Int(track_id) % 3 != 0

"""Whether KDIeEx keeps a B track."""
distinct_keeps_track(track_id) = Int(track_id) % 3 != 0

"""
    distinct_image!(dir; value_name = "B", tracks = true) -> dir

Copy KDIeEx's cell table (and track table) into `dir/labelProps` as `value_name` — a second image dir,
or a second segmentation beside B in the same dir, that disagrees with KDIeEm's B by the rule above.
"""
function distinct_image!(dir::AbstractString; value_name::AbstractString = "B", tracks::Bool = true)
    src = only(filter(fx -> fx.uid == "KDIeEx", registered_fixtures()))
    lp = joinpath(dir, "labelProps"); mkpath(lp)
    cp(fixture_table(src), joinpath(lp, "$value_name.h5ad"); force = true)
    tracks && cp(joinpath(src.dir, "labelProps", "B__tracks.h5ad"), joinpath(lp, "$(value_name)__tracks.h5ad"); force = true)
    dir
end

"""Median by its definition (middle value, or the mean of the two), for expectations independent of the code."""
function distinct_median(v)
    s = sort(collect(Float64, v)); n = length(s)
    isodd(n) ? s[(n + 1) ÷ 2] : (s[n ÷ 2] + s[n ÷ 2 + 1]) / 2
end

"""Per-track `live.track.speed` of a track table, as `track id => speed`."""
distinct_track_speeds(path::AbstractString) =
    (d = label_props(path) |> select_cols(["live.track.speed"]) |> as_df;
     Dict(Int(l) => Float64(s) for (l, s) in zip(d.label, d[!, "live.track.speed"])))

"""Cell count of B, and how many of them KDIeEx keeps — from B's own labels and track ids."""
function distinct_cell_counts(b_h5::AbstractString)
    d = label_props(b_h5) |> select_cols(["track_id"]) |> as_df
    # `length(d.label)`, not `nrow`: this file is shared with the API suite, which does not import DataFrames
    (; b = length(d.label), x = count(i -> distinct_keeps_cell(Int(d.label[i]), d.track_id[i]), eachindex(d.label)))
end
