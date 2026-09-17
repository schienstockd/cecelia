# ── Internal helpers ──────────────────────────────────────────────────────────
# Project listing reads project.json directly (lightweight discovery, no object
# graph). Image/set payloads are sourced from the model (CciaImage/CciaSet) so
# ccid.json parsing lives in one place; the API only shapes the response.

function _scan_projects_raw()::Vector{Dict{String,Any}}
    isdir(projects_dir()) || return Dict{String,Any}[]
    projects = Dict{String,Any}[]
    for entry in readdir(projects_dir(); join=true)
        isdir(entry) || continue
        meta_file = joinpath(entry, "project.json")
        isfile(meta_file) || continue
        try
            raw  = JSON3.read(read(meta_file, String))
            proj = Dict{String,Any}(String(k) => v for (k, v) in raw)
            proj["path"] = entry
            # `type` and `kind` are legacy — project-wide static/live/flow distinction was dropped
            # in favour of per-image axis gating (Cecelia.task_applies). Fields kept for on-disk
            # round-trip only; not surfaced by the frontend.
            push!(projects, proj)
        catch e
            @warn "Skipping malformed project" dir=entry exception=e
        end
    end
    sort!(projects; by=p -> string(get(p, "lastOpenedAt", get(p, "createdAt", ""))), rev=true)
    projects
end

# `meta_int` / `meta_float` / `meta_str` (`app/src/model/image.jl`) are the ONE way to read a
# typed value out of an `img.meta` bag — see the docstring block there for the drift they exist to
# close. The three private wrappers this file used to carry moved into the package and are exported.

# QC docs for the payload — persisted sidecars + the computed calibration fallback (see all_qc_docs).
# ONE canonical merge in the package (Cecelia.all_qc_docs), shared with the observer session briefing,
# so the table indicator and the briefing's flagged-list can never diverge.
_image_qc_payload(img::CciaImage) = Cecelia.all_qc_docs(img)

# Meta keys already surfaced as first-class payload fields (below) or internal bookkeeping — excluded
# from `extraMeta` so the image-info dialog's "other metadata" section shows only genuinely-extra
# keys, never a duplicate of a field we already render or noise like funParams / display colormaps.
const _SURFACED_META_KEYS = Set([
    "SizeC", "SizeT", "SizeZ",
    "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit", "PhysicalSizeZ_raw",
    "TimeIncrement", "TimeIncrementUnit",
    "ori_path", "channel_names", "channel_colormaps", "funParams",
])

# Any scalar meta key not already surfaced as a field and not internal — rendered verbatim in the
# image-info dialog. Nested dicts/vectors are skipped (they'd be funParams-style noise, not metadata).
function _extra_meta(meta::AbstractDict)
    out = Dict{String,Any}()
    for (k, v) in meta
        ks = string(k)
        (ks in _SURFACED_META_KEYS || v isa AbstractDict || v isa AbstractVector) && continue
        out[ks] = v
    end
    out
end

# Enrich the per-image run log with each entry's OUTPUT value name — the STORED IMAGE VERSION it
# wrote — so the frontend can draw a lineage (`inputValueName → outputValueName`) without
# replicating spec lookup in TypeScript. The input name is already the entry's `valueName` field.
#
# Output-name resolution has TWO paths in the package and this has to try both, in order:
#   1. `task_output_name(fun, params)` — for tasks whose output is a USER-SET param carrying a
#      `namespace` (e.g. `editImages.cropImage` → `outputValueName`).
#   2. `_spec_output_value_name(task, "")` — for tasks with a spec-declared TOP-LEVEL
#      `outputValueName` (all the `cleanupImages.*` and most `editImages.*` — `driftCorrect` →
#      `driftCorrected`, `smooth` → `smoothed`, …). This is what the handlers themselves call
#      (see `_spec_output_value_name` in `app/src/tasks/task.jl` and the callsites in
#      `app/src/tasks/cleanupImages/*.jl`).
#
# `outputValueName` is only added for tasks that WRITE A NEW IMAGE VERSION — `cleanupImages.*` and
# `editImages.*`. Other task categories (`segment.*`, `tracking.*`, `behaviour.*`, `opticalFlow.*`,
# `clustPops.*`, `clustTracks.*`, `clustRegions.*`, `spatialAnalysis.*`, `exportImages.*`) also
# carry an `outputValueName`, but there it names a LABELS/TRACKS/MEASUREMENTS output rather than
# an image version. Including those would inject spurious edges into the version tree — a
# `segment.cellposeMeasure` writing labels named `default` from `intensityValueName="smoothed"`
# would otherwise claim the `default` image version was produced from `smoothed` and close a
# cycle with the real cleanupImages edges, hiding the whole tree.
#
# Wrapped per-entry so a removed task doesn't fail the payload.
const _IMAGE_VERSION_WRITING_TASK_PREFIXES = ("cleanupImages.", "editImages.")

function _fun_writes_image_version(fun::AbstractString)
    any(p -> startswith(fun, p), _IMAGE_VERSION_WRITING_TASK_PREFIXES)
end

function _enriched_run_log(img::CciaImage)
    entries = read_run_log(img)
    out = Vector{Any}(undef, length(entries))
    for (i, e) in pairs(entries)
        d = Dict{String,Any}(String(k) => v for (k, v) in pairs(e))
        fun = String(get(d, "fun", ""))
        if _fun_writes_image_version(fun)
            p = get(d, "params", nothing)
            params = p isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in p) : Dict{String,Any}()
            try
                name = Cecelia.task_output_name(fun, params)
                if isempty(name)
                    task = Cecelia._task_from_fun_name(fun)
                    if !isnothing(task)
                        name = Cecelia._spec_output_value_name(task, "")
                    end
                end
                isempty(name) || (d["outputValueName"] = name)
            catch
                # unknown fun / spec load failure — leave outputValueName absent
            end
        end
        out[i] = d
    end
    out
end

# Frontend-shaped payload for one image, sourced from the model. Response shaping
# (camelCase, field selection) is the API's job; data access goes through CciaImage
# so ccid.json parsing has a single home.
function _image_payload(img::CciaImage)
    fps = Dict{String,String}(k => v for (k, v) in img.filepath if k != VERSIONED_ACTIVE_KEY)
    # Lenient (no write-back): surface the default zarr if present but unregistered (legacy data).
    if isempty(fps) && isdir(joinpath(img_zero_dir(img), "ccidImage.ome.zarr"))
        fps["default"] = "ccidImage.ome.zarr"
    end
    active_vn = versioned_active(img.filepath)
    active_fn = something(versioned_get(img.filepath), get(fps, VERSIONED_DEFAULT_VAL, ""))
    ch        = channel_names(img)
    (;
        uid             = img.uid,
        name            = img.name,
        status          = string(img.status),
        sizeC           = meta_int(img.meta, "SizeC"),
        sizeT           = meta_int(img.meta, "SizeT"),
        sizeZ           = meta_int(img.meta, "SizeZ"),
        # Raw/nullable — NOT img_physical_sizes' 1.0-default-for-computation fallback. The UI
        # needs to tell "genuinely missing" apart from "explicitly confirmed 1.0".
        physicalSizeX     = meta_float(img.meta, "PhysicalSizeX"),
        physicalSizeY     = meta_float(img.meta, "PhysicalSizeY"),
        physicalSizeZ     = meta_float(img.meta, "PhysicalSizeZ"),
        physicalSizeUnit  = meta_str(img.meta, "PhysicalSizeUnit"),
        # set when the ImageJ-TIFF Z-spacing auto-fix overrode bioformats2raw's value at import
        # (see omezarr.jl) — the corrected number is still only as good as the source file's own
        # ImageJ tag, so the frontend keeps flagging it for the user to confirm, not just silently
        # trusting it because the ratio now looks plausible.
        physicalSizeZCorrected = haskey(img.meta, "PhysicalSizeZ_raw"),
        timeIncrement     = meta_float(img.meta, "TimeIncrement"),
        timeIncrementUnit = meta_str(img.meta, "TimeIncrementUnit"),
        channelNames    = isnothing(ch) ? String[] : ch,
        # Original source file location (before OME-Zarr conversion), kept in meta as `ori_path`.
        # The image-info dialog surfaces it so users can trace a converted image back to its raw file.
        oriPath         = meta_str(img.meta, "ori_path"),
        # Any other meta the dialog can show generically (see _extra_meta) — empty for most images.
        extraMeta       = _extra_meta(img.meta),
        filepath        = active_fn,
        activeValueName = active_vn,
        filepaths       = fps,
        labels          = img.labels,
        # Value names that have a MEASUREMENT TABLE but not necessarily a mask. `labels` and
        # `label_props` are two independent registries (see model/image.jl) written by two different
        # tasks, and a directly-imported track set registers only the second: there are no mask pixels
        # to register. Without this the client could not tell such a set exists at all — it had no
        # viewer row, so no tracks toggle, while gating and the observer listed it happily.
        labelPropsNames = [v for v in versioned_keys(img.label_props) if !is_reserved_value_name(v)],
        # Skeleton labels written by segment.branching — kept separate from `labels` on purpose
        # so the generic labels picker (measure / segment / tracking) never lists them
        # (BRANCHING_PLAN Decision 6). The Viewer surfaces them as a separate toggle.
        branchLabels    = img.branch_labels,
        # Spatial neighbour graphs built by spatialAnalysis.cellNeighbours, keyed by RUN suffix (the
        # graph pools across segmentations, so it is not a value_name — see img_spatial_graph_suffixes).
        # Surfaced as a versioned-style dict so a `valueNameSelection` with `field: "spatialGraphs"`
        # offers the graphs present on ALL selected images — which is exactly the set a pooled analysis
        # can run over. Discovered by listing spatialGraph/, not registered in ccid.json.
        spatialGraphs   = Dict{String,Any}(s => "$(s).h5ad" for s in img_spatial_graph_suffixes(img)),
        # Which segmentations have MEASURED tracks, from the `{vn}__tracks.h5ad` sidecars on disk —
        # the same listing convention as spatialGraphs, not a ccid.json registration. Surfaced so the
        # client can answer "is this image tracked" from data it already holds: the run log cannot (a
        # migrated project has tracks and no `tracking.*` entry), and `labels`/`label_props` look
        # identical tracked or not. One readdir per image, no HDF5 open.
        trackValueNames = img_track_value_names(img),
        # Interaction-stats runs (spatialAnalysis.neighbourStats), keyed by run suffix — same listing
        # convention as spatialGraphs above. Feeds the `stats` namespace of a `valueNameInput`.
        statsSuffixes   = img_stats_suffixes(img),
        # Clustering runs recorded for the ACTIVE segmentation, split by family so a `clusters.immune`
        # and a `regions.immune` can coexist. Per (image, value_name) rather than per image — a
        # clustering belongs to a segmentation (VALUE_NAME_INPUT_PLAN → D6) — so this is the active
        # one's list; switching segmentation re-fetches the payload anyway.
        clusterSuffixes = img_cluster_suffixes(img; family = "clusters"),
        regionSuffixes  = img_cluster_suffixes(img; family = "regions"),
        attr            = img.attr,
        # Include/exclude in further processing (default true). Excluded images are greyed in the
        # GUI, unselectable for runs, and hard-skipped by the runners; `note` is the optional reason.
        included        = img.included,
        note            = img.note,
        # A plain user bookmark, any number per set — drives the Starred row filter and nothing else
        # (no effect on selection, runs, or processing). See model/image.jl.
        starred         = img.starred,
        # QC findings per "funName/valueName" (docs/todo/QC_PLAN.md) — advisory "output looks off"
        # flags the GUI renders as a badge + tooltip. Includes the live calibration fallback so
        # pre-migration images still surface metadata warnings (see _image_qc_payload).
        qc              = _image_qc_payload(img),
        # automatic provenance: which task functions ran on this image + when ({fun, valueName, at});
        # the image table shows it in a cog popover after the uid. Appended by the scheduler on success.
        runLog          = _enriched_run_log(img),
    )
end

_set_payload(s::CciaSet) = (; uid=s.uid, name=s.name,
                              images=[_image_payload(i) for i in s._images])
