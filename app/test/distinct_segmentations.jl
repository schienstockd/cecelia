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
