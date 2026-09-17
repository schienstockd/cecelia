# ── Metadata management ───────────────────────────────────────────────────────

function _parse_meta_request(body_bytes)
    data = try JSON3.read(String(body_bytes)) catch
        return nothing, nothing, "Invalid JSON body"
    end
    project_uid = _wstr(data, :projectUid)
    isempty(project_uid) && return nothing, nothing, "projectUid required"
    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return nothing, nothing, "Project not found: $project_uid"
    proj_dir, data, nothing
end

# Load each listed image as a CciaImage, apply f!(img), and persist via save!.
# Delegating to the model keeps every ccid.json field intact (status, attr,
# channel names, filepath versions) — see the CciaImage round-trip contract.
function _mutate_images!(f!::Function, project_uid::String, image_uids)
    for uid in image_uids
        img = init_object(project_uid, uid)
        img isa CciaImage || continue
        f!(img)
        save!(img)
    end
end

# Attribute names and values are user-typed free text, and these three routes are the ONLY place they
# enter the model — so normalise here rather than in each consumer. Untrimmed, `"a"` and `" a "` are two
# distinct attribute values (two filter chips in the image table, two segments in a generated movie
# name), and `" Location"` is a second column beside `Location`. `_movie_basename` already had to defend
# against a whitespace-only value downstream; that defence belongs at the write.
#
# Whitespace-only collapses to `""`, which is the canonical *unset* — deliberately NOT a delete:
# `attr/create` seeds a new column with `""` on every image, and the key's presence is the only thing
# that makes the column exist. Deleting on blank would make a column vanish as you cleared it.
_norm_attr(s::AbstractString) = String(strip(s))

function api_images_attr_create(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    attr_name   = _norm_attr(_wstr(data, :attrName))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))

    _mutate_images!(project_uid, image_uids) do img
        haskey(img.attr, attr_name) || (img.attr[attr_name] = "")
    end
    200, JSON3.write((; ok=true))
end

function api_images_attr_delete(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    attr_name   = _norm_attr(_wstr(data, :attrName))
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))

    _mutate_images!(project_uid, image_uids) do img
        delete!(img.attr, attr_name)
    end
    200, JSON3.write((; ok=true))
end

function api_images_attr_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    attr_name   = _norm_attr(_wstr(data, :attrName))
    values_raw  = get(data, :values, nothing)
    isempty(attr_name) && return 400, JSON3.write((; error="attrName required"))
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    values = Dict{String,String}(String(k) => _norm_attr(string(v)) for (k, v) in values_raw)
    for (image_uid, val) in values
        _mutate_images!(project_uid, [image_uid]) do img
            img.attr[attr_name] = val
        end
    end
    # Echo back what was actually STORED, and the normalised name. Callers update their local store from
    # this rather than from what they sent — otherwise the client would show the untrimmed input while
    # the file holds the trimmed value, and trimming client-side too would mean two normalisers.
    200, JSON3.write((; ok=true, attrName=attr_name, values=values))
end

function api_images_delete_labels(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    image_uid   = _wstr(body, :imageUid)
    value_name  = _wstr(body, :valueName)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error="valueName required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    task_dir = joinpath(proj_dir, "1", image_uid)
    ccid     = state_file(task_dir)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))
    isfile(ccid)    || return 404, JSON3.write((; error="Image not found"))

    raw = read_ccid_raw(ccid)

    # Registered stores: labels[vn] under labels/, branch_labels[vn] under branchLabels/. Branch label
    # sets share the value_name of the segmentation they were skeletonised from, so they go with it —
    # leaving `branchLabels/` behind is exactly the orphan this route exists to prevent.
    for (field, subdir) in (("labels", "labels"), ("branch_labels", "branchLabels"))
        entries = get(raw, field, Dict{String,Any}())
        entry   = get(entries, value_name, get(entries, Symbol(value_name), nothing))
        isnothing(entry) && continue
        for fn in (entry isa AbstractVector ? entry : [string(entry)])
            p = joinpath(task_dir, subdir, string(fn))
            ispath(p) && rm(p; recursive = true)
        end
    end

    # NOT swept: `gating/{vn}.json` (+ the `__tracks` variant). Gate polygons are hand-drawn user work,
    # not derived output — nothing can regenerate them, and re-running the segmentation under the same
    # value_name makes the existing strategy apply to the new cells. `reset_image_analysis!` keeps them
    # for the same reason (ANALYSIS_KEEP), so the two delete scopes agree.
    #
    # Also not swept, and correctly so: `spatialGraph/{suffix}.h5ad` + `spatialStats/{suffix}.json` are
    # keyed by RUN SUFFIX, not value_name — the graph pools across segmentations, so there is no
    # per-value_name file to take (see img_spatial_graph_path).

    # labelProps sidecars: the registered `{vn}.h5ad` PLUS every companion derived from it —
    # `{vn}__tracks.h5ad`, `{vn}__branch.h5ad`, `{vn}.clustfeatures.json`, `{vn}__tracks.clustfeatures.json`.
    # Prefix-driven rather than a suffix list, so a companion added later is swept too; the `.`/`__`
    # boundary is what stops value_name "B" from eating "B2.h5ad".
    props_dir = joinpath(task_dir, "labelProps")
    if isdir(props_dir)
        for f in readdir(props_dir)
            (startswith(f, value_name * ".") || startswith(f, value_name * "__")) || continue
            p = joinpath(props_dir, f)
            isfile(p) && rm(p)
        end
    end

    # Commit under the image's lock, and only now — the deletes above can be a multi-GB label store,
    # which must not be held under it. Re-derive from the FRESH raw inside the transaction so a
    # concurrent task's registration isn't clobbered (`raw` above is only used to find what to delete).
    commit_state!(task_dir) do fresh
        for field in ("labels", "label_props", "branch_labels")
            entries = get(fresh, field, Dict{String,Any}())
            fresh[field] = Dict{String,Any}(String(k) => v for (k, v) in entries
                                            if string(k) != value_name)
        end
    end

    img = init_object(project_uid, image_uid)
    img isa CciaImage || return 200, JSON3.write((; ok = true))
    @info "Deleted label set" value_name image=image_uid project=project_uid
    200, JSON3.write((; ok = true, image = _image_payload(img)))
end

function api_images_channelnames(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    ch_names    = [String(n) for n in get(data, :channelNames, [])]
    isempty(image_uids) && return 400, JSON3.write((; error="imageUids required"))
    isempty(ch_names)   && return 400, JSON3.write((; error="channelNames required"))

    _mutate_images!(project_uid, image_uids) do img
        set_channel_names!(img, ch_names; check_length=false)
    end
    200, JSON3.write((; ok=true))
end

# Generic bulk merge into an image's `meta` dict — one endpoint for any meta field (physical
# size/unit, time interval, …) rather than a one-off route per field. `values` maps
# uid → partial dict of meta keys to merge in (same shape idea as api_images_attr_set, but the
# per-uid value is itself a dict instead of a scalar).
function api_images_meta_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    values_raw  = get(data, :values, nothing)
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    for (image_uid, fields_raw) in values_raw
        fields = Dict{String,Any}(String(k) => v for (k, v) in fields_raw)
        _mutate_images!(project_uid, [String(image_uid)]) do img
            for (k, v) in fields
                # a JSON `null` deletes the key (e.g. clearing a stale PhysicalSizeZ_raw marker
                # once a trusted value replaces an auto-corrected one) rather than setting it
                isnothing(v) ? delete!(img.meta, k) : (img.meta[k] = v)
            end
        end

        # Copy any physical-size/timing edit INTO the zarr's own calibration (`.zattrs` scale/units
        # + OME-XML `<Pixels>`), so napari renders what ccid.json/analysis use — without it a fix
        # only changes ccid's display copy and napari keeps showing the old value / "t = N". Same
        # translator the importer uses (`sync_zarr_calibration!`), so the field→zarr mapping lives in
        # ONE place. Targets the "default" (bioformats2raw) zarr, NOT the active version: processed
        # variants (drift/cellpose-correct) carry a flat NGFF layout with no unit/OME-XML, and
        # `resync_ome_meta!` re-reads the default anyway. See CLAUDE.md → OME-ZARR dual-format.
        if Cecelia.has_calibration_meta(fields)
            img = init_object(project_uid, String(image_uid))
            if img isa CciaImage
                zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
                (isnothing(zarr_path) || !isdir(zarr_path)) ||
                    Cecelia.sync_zarr_calibration!(zarr_path, fields)
                # recompute calibration QC from the saved meta so a fixed image clears its warning
                # (or a bad edit re-flags it) — the image-table indicator reads this, not the payload.
                Cecelia.write_metadata_qc!(img)
            end
        end
    end
    200, JSON3.write((; ok=true))
end

# Set the per-image user flags for one or more images. `values` maps uid → a partial dict
# {included?, note?, starred?}; only the keys present are changed (toggle inclusion without
# clobbering a note, star without touching inclusion). First-class CciaImage fields, so this rounds
# through the model (save! preserves every other field) rather than the meta bag. One route for all
# three because they are the same operation — flip a user-owned flag on an image — and a second
# route would duplicate the load/mutate/save path.
function api_images_inclusion_set(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    values_raw  = get(data, :values, nothing)
    isnothing(values_raw) && return 400, JSON3.write((; error="values required"))

    for (image_uid, fields_raw) in values_raw
        fields = Dict{String,Any}(String(k) => v for (k, v) in fields_raw)
        _mutate_images!(project_uid, [String(image_uid)]) do img
            haskey(fields, "included") && (img.included = Bool(fields["included"]))
            haskey(fields, "note")     && (img.note     = string(fields["note"]))
            haskey(fields, "starred")  && (img.starred  = Bool(fields["starred"]))
        end
        # Notify observers (mcp/) that a note was set — first-class user context (OBSERVER.md §4).
        if haskey(fields, "note")
            broadcast_ws(Dict{String,Any}(
                "type" => "image_note_added", "projectUid" => project_uid,
                "imageUid" => String(image_uid), "note" => string(fields["note"])))
        end
    end
    200, JSON3.write((; ok=true))
end

