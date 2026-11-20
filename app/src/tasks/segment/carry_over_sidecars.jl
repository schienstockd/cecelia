# carry_over_sidecars.jl — the correction composite carries the per-run sidecars of the columns it
# carries over (`carry_over.jl`).
#
# The per-run manifests that describe obs columns the restore brings back. `segment.measureLabels`
# prunes every record whose column its fresh table lacks — always, composite or not: whether a
# correction run is in progress is not something it can tell from disk (a snapshot an aborted run
# left behind looks the same as a live one). So the snapshot phase keeps a copy of each sidecar, and
# the restore puts it back once the columns are back, then prunes against the restored table.
const _CARRYOVER_SIDECARS = ("hmmfit", "clustfeatures")

_carryover_sidecar_path(task_dir::AbstractString, kind::AbstractString) =
    joinpath(task_run_dir(task_dir), "correct_carryover_$(kind).json")

function _carryover_save_sidecars!(task_dir::AbstractString, labelprops::AbstractString)
    for kind in _CARRYOVER_SIDECARS
        raw = read_props_sidecar(props_sidecar_path(labelprops, kind))
        isempty(raw) || write_json_atomic(_carryover_sidecar_path(task_dir, kind), raw)
    end
end

# Put the saved records back (a key measureLabels kept stays as it is), then drop every record whose
# column the restored table still lacks — "a record never outlives its column".
function _carryover_restore_sidecars!(task_dir::AbstractString, labelprops::AbstractString; on_log = _ -> nothing)
    for kind in _CARRYOVER_SIDECARS
        saved = read_props_sidecar(_carryover_sidecar_path(task_dir, kind))
        isempty(saved) && continue
        sidecar = props_sidecar_path(labelprops, kind)
        kept = read_props_sidecar(sidecar)
        for (k, v) in saved
            haskey(kept, k) || put_props_sidecar_entry!(sidecar, k, v)
        end
    end
    prune_stale_clustfeatures!(labelprops; on_log = on_log)
    prune_stale_hmmfit!(labelprops; on_log = on_log)
end

_carryover_clear_sidecars!(task_dir::AbstractString) =
    foreach(k -> rm(_carryover_sidecar_path(task_dir, k); force = true), _CARRYOVER_SIDECARS)
