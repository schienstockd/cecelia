# Rename a segmentation's value_name across every artifact keyed by it (`rename_value_name!`).
# It reaches up into gating, ROI sets, the run log and the scheduler's param-leaf rewrite, so it sits
# in the jobs tier (scripts/architecture/julia_areas.toml).

# ── Rename a value_name across every artifact keyed by it ─────────────────────

# Every `{from}*` sidecar file lives beside its siblings under the image's `1/{uid}/...` subdirs. This
# is the enumerated list — one place, so a new sidecar family added later is added here too rather than
# drifting between rename and delete. Symmetric with `api_images_delete_labels`, which walks the same
# families (labels/branchLabels + prefix scan of labelProps + registered ccid.json entries).
#
# JSON sidecars in `gating/` need a load→mutate→save, not a plain `mv`: each `PopulationTree` and every
# `Population` inside it embeds its `value_name`; the on-disk `value_name` field must match the filename
# or `load_pop_tree` returns a map whose `m.value_name` disagrees with what every reader assumes.

_rename_move(src, dst) = (ispath(src) && (mkpath(dirname(dst)); mv(src, dst; force = false)); nothing)

function _rename_labels_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_labels_dir(img)
    isdir(dir) || return String[]
    moved = String[]
    for f in readdir(dir)
        # `{from}.` or `{from}_` — the `.` / `_` boundary is what stops value_name "B" from eating
        # "B2.zarr" (matches api_images_delete_labels' prefix rule).
        (startswith(f, from * ".") || startswith(f, from * "_")) || continue
        # rewrite the leading value_name segment; a filename registered as `B_nuc.zarr` becomes
        # `T_nuc.zarr`, not `TB_nuc.zarr`.
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
        push!(moved, new_name)
    end
    moved
end

function _rename_branch_labels_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_branch_labels_dir(img)
    isdir(dir) || return String[]
    moved = String[]
    for f in readdir(dir)
        (startswith(f, from * ".") || startswith(f, from * "_")) || continue
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
        push!(moved, new_name)
    end
    moved
end

# label_props + clustfeatures sidecars — same prefix rule as the delete route walks. Every companion
# derived from `{vn}.h5ad` (`__tracks`, `__branch`, `.clustfeatures.json`, `__tracks.clustfeatures.json`)
# is picked up by the `.`/`__` boundary; a companion added later is renamed too rather than orphaned.
function _rename_label_props_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_label_props_dir(img)
    isdir(dir) || return nothing
    for f in readdir(dir)
        (startswith(f, from * ".") || startswith(f, from * "__")) || continue
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
    end
    nothing
end

# gating/{from}[__…].json — load, rewrite the map's own value_name plus every population's, save under
# the new path, remove the old file. `save_pop_tree!(m, task_dir)` uses `m.pop_type` to pick the suffix,
# so the load pop_type must match the file's suffix; we walk every stored type (`STORED_POP_TYPES`).

function _rename_gating_sidecars!(img::CciaImage, from::String, to::String, on_log)
    dir = joinpath(img._dir, "gating")
    isdir(dir) || return nothing
    # under the gating lock (gating/popmanager/copy.jl): an edit landing mid-rename would otherwise be
    # overwritten, or rewrite the old path after it was removed
    with_pop_tree_lock() do; for pt in STORED_POP_TYPES
        old_path = gating_path(img._dir, from; pop_type = pt)
        isfile(old_path) || continue
        m = load_pop_tree(img._dir, from; pop_type = pt)
        m.value_name = to
        for p in values(m.pops)
            p.value_name = to
        end
        # save_pop_tree!(m, task_dir) writes to gating_path(dir, m.value_name; pop_type=m.pop_type) —
        # i.e. under the new name. The image form would stamp authored_labels_version off the loaded
        # image, but the sidecar's existing breadcrumb already reflects when the pops were authored;
        # a rename must not pretend the pops were re-authored just now. So use the task_dir form.
        # The gate history moves with its sidecar; the rename itself is one more entry in it.
        old_hist = gating_history_path(img._dir, from; pop_type = pt)
        new_hist = gating_history_path(img._dir, to; pop_type = pt)
        (isfile(old_hist) && old_hist != new_hist) && mv(old_hist, new_hist; force = true)
        save_pop_tree!(m, img._dir; history_kind = "rename")
        on_log("[INFO] Rewrote sidecar: $(old_path) → $(gating_path(img._dir, to; pop_type = pt))")
        old_path == gating_path(img._dir, to; pop_type = pt) || rm(old_path; force = true)
    end; end
    nothing
end

# ROI sets (`rois/{suffix}.json`) are keyed by run suffix, not value_name, but each records the
# segmentation it scored as `valueName` — what the viewer highlights against and what correction
# staleness matches on. Rewrite that field; the suffix and the reviews (keyed by ROI id) are untouched.
function _rename_roi_sets!(img::CciaImage, from::String, to::String, on_log)
    for suffix in roi_suffixes_on(img, from)
        set = load_roi_set(img, suffix)
        set["valueName"] = to
        write_json_atomic(img_rois_path(img, suffix), set)
        on_log("[INFO] Rewrote ROI set: rois/$(suffix).json → valueName $(to)")
    end
    nothing
end

# The ccid.json registrations that key by value_name. Every write goes into ONE `commit_state!` block
# so a concurrent task's writes are re-read fresh, not clobbered. `funParamsByName[fun][from]` is the
# per-output-name recall bank (`FUN_PARAMS_BY_NAME_META_KEY`); if we skip it, the form for the renamed
# name silently forgets what was tuned and falls back to the flat blob.
function _rename_ccid_fields!(raw::AbstractDict, from::String, to::String)
    for field in ("labels", "label_props", "branch_labels")
        entries = get(raw, field, nothing)
        entries isa AbstractDict || continue
        # JSON3.Object values are immutable — normalise to Dict{String,Any} first
        mutable = Dict{String,Any}(String(k) => v for (k, v) in entries)
        haskey(mutable, from) || continue
        value = mutable[from]
        # rewrite the leaf filename(s) too — `img_*_path` falls back to `{vn}.zarr`/`{vn}.h5ad` when a
        # value_name isn't registered, but the on-disk file IS `{to}.…` now, so registered filenames
        # must match. Handles: legacy scalar (`String`), vector of filenames, and versioned inner dict.
        mutable[to] = _rewrite_registered_filenames(value, from, to)
        delete!(mutable, from)
        # swing `_active` too, if it pointed at the renamed vn (label_props only carries it — see
        # `resolve_value_name`'s docstring for why the other two don't).
        if field == "label_props" && get(mutable, VERSIONED_ACTIVE_KEY, nothing) == from
            mutable[VERSIONED_ACTIVE_KEY] = to
        end
        raw[field] = mutable
    end
    # funParamsByName rename — per-fun keys, nested by value_name. The saved params name it too
    # (`from/qc` in a population picker), in the by-name bank and in the flat `funParams` blob a new
    # name falls back to — same rewrite as the run log.
    meta = get(raw, "meta", nothing)
    meta isa AbstractDict || return nothing
    mmeta = Dict{String,Any}(String(k) => v for (k, v) in meta)
    in_params(fun, p) = (lv = _history_leaves(fun);
                         (lv === nothing || !(p isa AbstractDict)) ? p : rename_label_set_in_params(p, lv, from, to))
    fpbn = get(mmeta, FUN_PARAMS_BY_NAME_META_KEY, nothing)
    if fpbn isa AbstractDict
        mfpbn = Dict{String,Any}(String(k) => v for (k, v) in fpbn)
        for fun in collect(keys(mfpbn))
            per_vn = mfpbn[fun]
            per_vn isa AbstractDict || continue
            mvn = Dict{String,Any}(String(k) => v for (k, v) in per_vn)
            if haskey(mvn, from) && !haskey(mvn, to)
                mvn[to] = mvn[from]
                delete!(mvn, from)
            end
            mfpbn[fun] = Dict{String,Any}(k => in_params(fun, p) for (k, p) in mvn)
        end
        mmeta[FUN_PARAMS_BY_NAME_META_KEY] = mfpbn
    end
    fp = get(mmeta, FUN_PARAMS_META_KEY, nothing)
    fp isa AbstractDict &&
        (mmeta[FUN_PARAMS_META_KEY] = Dict{String,Any}(String(f) => in_params(String(f), p) for (f, p) in fp))
    raw["meta"] = mmeta
    nothing
end

# Rewrite the leaf filename(s) inside a versioned/legacy `labels`/`label_props`/`branch_labels` entry:
# swap `{from}` for `{to}` at the START of each registered filename, leaving suffixes (`_nuc.zarr`,
# `.h5ad`, `_something.zarr`) alone. Handles all three on-disk shapes.
function _rewrite_registered_filenames(value, from::String, to::String)
    if value isa AbstractDict
        # versioned inner dict (`v1 => filename[…], _latest => …`) — rewrite each version's value
        out = Dict{String,Any}()
        for (k, v) in value
            ks = String(k)
            out[ks] = ks == LATEST_ACTIVE_KEY ? v : _rewrite_registered_filenames(v, from, to)
        end
        return out
    elseif value isa AbstractVector
        return [_rewrite_one_filename(String(x), from, to) for x in value]
    elseif value isa AbstractString
        return _rewrite_one_filename(String(value), from, to)
    else
        return value
    end
end

function _rewrite_one_filename(fn::String, from::String, to::String)::String
    # match the same `.`/`_` boundary the on-disk move uses; leave a filename that doesn't start with
    # `{from}` unchanged (e.g. a user-imported label file registered under a value_name that doesn't
    # equal the filename's basename).
    if startswith(fn, from * ".") || startswith(fn, from * "_")
        return to * fn[ncodeunits(from)+1:end]
    end
    fn
end

# Rewrite `runlog.json` for the rename: an entry's `valueName == from`, and `from` inside any entry's
# params — a population picker's `from/…` prefix or a label-set picker naming `from`
# (`rename_label_set_in_params`). Without the params half, the task-history dialog, chain-from-history
# and the own-gate routes' `used_by_runs` check (`run_pop_reads`) lose every run that read a population
# of the renamed segmentation. Bulk update through the same `_update_run_log!` lock the writers use.
#
# The archive (`runlog.archive.jsonl`) is renamed the same way, under the same lock: it is the older
# half of the same record, and `run_log_params_for_output` falls back to it — left unrenamed it would
# answer for a name that no longer exists, or for a new set that reuses the old name.
function _rename_run_log!(img::CciaImage, from::String, to::String)
    leaves = Dict{String,Any}()
    # an `inputs` / `replaces` record naming the label set (`ns: "labels"`)
    ren_ref(r) = r isa AbstractDict && _rl_str(r, "ns") == "labels" && _rl_str(r, "name") == from ?
        merge(Dict{String,Any}(String(k) => v for (k, v) in pairs(r)), Dict{String,Any}("name" => to)) : r
    function ren(e)
        e isa AbstractDict || return e
        d = Dict{String,Any}(String(k) => v for (k, v) in pairs(e))
        _rl_str(e, "valueName") == from && (d["valueName"] = to)
        fn = _rl_str(e, "fun")
        lv = get!(() -> _history_leaves(fn), leaves, fn)
        p = get(d, "params", nothing)
        (lv !== nothing && p isa AbstractDict) && (d["params"] = rename_label_set_in_params(p, lv, from, to))
        ins = get(d, "inputs", nothing)
        ins isa AbstractVector && (d["inputs"] = Any[ren_ref(x) for x in ins])
        haskey(d, "replaces") && (d["replaces"] = ren_ref(d["replaces"]))
        # the gate hashes a run read (`gates`) name their gating sidecar's value_name
        gs = get(d, "gates", nothing)
        gs isa AbstractVector && (d["gates"] = Any[g isa AbstractDict && _rl_str(g, "valueName") == from ?
            merge(Dict{String,Any}(String(k) => v for (k, v) in pairs(g)), Dict{String,Any}("valueName" => to)) : g
            for g in gs])
        d
    end
    _update_run_log!(img) do entries
        for (i, e) in pairs(entries)
            entries[i] = ren(e)
        end
        entries
    end
    lock(_RUN_LOG_LOCK) do
        archived = read_run_log_archive(img)
        isempty(archived) && return
        write_atomic(run_log_archive_path(img)) do io
            for e in archived
                JSON3.write(io, ren(e)); write(io, '\n')
            end
        end
    end
    nothing
end

# Guard: does any clustering run on this image co-cluster {from} with images OTHER than this one? If
# so, renaming here would leave the sibling images spelling it as `{from}` while this image now says
# `{to}` — the shared `clusters.{suffix}` column no longer resolves for downstream borrow logic. See
# `co_clustered_value_names` + `_borrow_cluster_pop_tree` for the read side that would go inconsistent.
function _rename_would_break_cross_image_clustering(img::CciaImage, from::String)::Bool
    for (granularity, family) in ((:cell, "clusters"), (:track, "clusters"), (:cell, "regions"))
        props = granularity === :track ? img_track_props_path(img, from) : img_label_props_path(img, from)
        isfile(props) || continue
        for sfx in _clustfeatures_suffixes(props; family = family)
            for uid in _clustfeatures_part_of(_clustfeatures_entry(props, sfx; family = family))
                uid != img.uid && return true
            end
        end
    end
    false
end

# Guard: is a task currently running on this image whose `valueName == from`? Rename mid-run would
# leave the running task holding a stale `img_*_path(img, from)` for a file that no longer exists.
function _rename_active_task_on_vn(img::CciaImage, from::String)::Bool
    for e in read_run_log(img)
        e isa AbstractDict || continue
        _rl_str(e, "status") == RUN_LOG_RUNNING || continue
        _rl_str(e, "valueName") == from && return true
    end
    false
end

"""
    rename_value_name!(img, from, to; on_log) -> Dict{String,Any}

Rename a segmentation's value_name (`labels[from]` → `labels[to]`) across every artifact keyed by it:
`labels/{from}*.zarr`, `branchLabels/{from}*.zarr`, `labelProps/{from}{,__tracks,__branch}.h5ad` and
`.clustfeatures.json` companions, `gating/{from}{,__tracks,__clust,__trackclust,__region,__branch}.json`
sidecars (loaded, `value_name` fields rewritten, saved under the new path), plus `ccid.json`'s
`labels`/`label_props`/`branch_labels` keys, `label_props._active` when it named `from`, and
`meta.funParamsByName[fun][from]`. Also rewrites every `runlog.json` entry with `valueName == from`, and
`from` inside saved and logged params (a population picker's `from/…`, a label-set picker naming it).

Returns `Dict("moved" => [filenames], "renamed" => true)` on success. Errors on: `from` absent, `to`
empty or invalid (reserved suffix, contains `/` or `.` or leading whitespace), `to` already registered
on this image (no `force` — two segmentations of the same vn would collide on disk and every
downstream key), a running task on this image against `from`, or a clustering run on this image that
also spans another image (would leave the sibling out of sync).

Same lock discipline as `remove_image_version!`: the disk moves run OUTSIDE the image lock, then ONE
`commit_state!` rewrites every registered key together; the run-log bulk update takes its own lock.
"""
function rename_value_name!(img::CciaImage, from::AbstractString, to::AbstractString;
                             on_log::Function = _ -> nothing)::Dict{String,Any}
    from_s = String(from); to_s = String(to)
    from_s == to_s && error("rename_value_name!: from == to — nothing to do")
    isempty(to_s) && error("rename_value_name!: 'to' must be non-empty")
    strip(to_s) == to_s || error("rename_value_name!: 'to' must not have leading/trailing whitespace")
    (occursin('/', to_s) || occursin('\\', to_s) || startswith(to_s, ".")) &&
        error("rename_value_name!: 'to' must not contain path separators or start with '.'")
    is_reserved_value_name(to_s) &&
        error("rename_value_name!: 'to' uses a reserved suffix ($(TRACK_PROPS_SUFFIX)/$(BRANCH_PROPS_SUFFIX))")
    is_reserved_value_name(from_s) &&
        error("rename_value_name!: 'from' is a reserved-suffix companion, not a user segmentation")
    # `from` must be a real registered label set (matches how the picker enumerates on the frontend)
    haskey(img.labels, from_s) ||
        error("rename_value_name!: no labels registered for value_name '$(from_s)'")
    # collision on the target — safest to refuse without a force (see docstring)
    haskey(img.labels, to_s) &&
        error("rename_value_name!: value_name '$(to_s)' already exists on this image")
    haskey(img.label_props, to_s) &&
        error("rename_value_name!: labelProps entry for '$(to_s)' already exists on this image")
    haskey(img.branch_labels, to_s) &&
        error("rename_value_name!: branchLabels entry for '$(to_s)' already exists on this image")
    # …and on DISK, through the label writers' canonical check: an unregistered `{to}.zarr` /
    # `{to}_nuc.zarr` / `branchLabels/{to}.zarr` / `{to}__branch.h5ad` (a cancelled run's leftover)
    # would otherwise pass the registry checks above and fail the rename halfway, inside `mv`.
    let raw = read_ccid_raw(state_file(img))
        for field in keys(LABEL_REGISTRIES)
            _label_name_taken(img, raw, to_s, field) &&
                error("rename_value_name!: '$(to_s)' is already taken on this image ($(field) " *
                      "store on disk) — pick another name, e.g. '$(free_label_name(img, raw, to_s; field = field))'")
        end
    end
    _rename_active_task_on_vn(img, from_s) &&
        error("rename_value_name!: a task is currently running on '$(from_s)' — wait for it to finish")
    _rename_would_break_cross_image_clustering(img, from_s) &&
        error("rename_value_name!: '$(from_s)' takes part in a clustering run that spans other images " *
              "— rename would leave the siblings out of sync; drop the clustering first")

    # IO outside the lock — the label store can be many GB.
    on_log("[INFO] Renaming value_name '$(from_s)' → '$(to_s)' on image $(img.uid)")
    _rename_labels_dir!(img, from_s, to_s, on_log)
    _rename_branch_labels_dir!(img, from_s, to_s, on_log)
    _rename_label_props_dir!(img, from_s, to_s, on_log)
    _rename_gating_sidecars!(img, from_s, to_s, on_log)
    _rename_roi_sets!(img, from_s, to_s, on_log)

    # ONE commit — re-read fresh so a concurrent write isn't clobbered.
    commit_state!(img) do raw
        _rename_ccid_fields!(raw, from_s, to_s)
    end
    _rename_run_log!(img, from_s, to_s)

    on_log("[INFO] Done.")
    Dict{String,Any}("renamed" => true, "from" => from_s, "to" => to_s)
end
