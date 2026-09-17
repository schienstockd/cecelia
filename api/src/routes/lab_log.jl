# ── Lab log (per-project append-only markdown; see docs/ai-assist/LAB-LOG.md) ─────────────────────
# read → raw content + parsed entries (newest-first) + file mtime (unix seconds, nothing if absent).
function api_lablog_read(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    content = read_lab_log(proj)
    p = lab_log_path(proj)
    # uid→name map for the panel's "Show names" toggle: the log stores stable image UIDs; the panel
    # swaps them to current names on demand (names change, so resolution is always against live data).
    image_names = Dict(img.uid => img.name for img in images(proj))
    # LabArchives context rides along with the log the panel already fetches — the card and the
    # "no notebook linked" hint both live in this panel, so a second round-trip buys nothing.
    # NOTE this reports whether a notebook is LINKED, not whether the user's Claude session has the
    # LabArchives connector: that connector is managed by the claude.ai account, not the local config
    # we can read, so "is it connected" is not answerable from here and we don't pretend otherwise.
    la_doc = read_la_doc(proj)
    la = (; present = get(la_doc, "present", false),
            readable = get(la_doc, "readable", true),
            notebookName = string(get(get(la_doc, "source", Dict()), "notebookName", "")),
            url = string(get(get(la_doc, "source", Dict()), "url", "")),
            syncedAt = string(get(la_doc, "syncedAt", "")),
            sections = get(la_doc, "sections", Any[]),
            gaps = la_gaps(proj, la_doc))
    200, JSON3.write((; content, entries=parse_lab_log(content),
                        dismissed=read_dismissed(proj), imageNames=image_names,
                        labarchives=la,
                        mtime=(isfile(p) ? mtime(p) : nothing)))
end

# dismiss → hide/un-hide a single entry from the PANEL (config sidecar; the log file is never edited —
# append-only). Body {projectUid, id, dismissed}. Returns the updated dismissed-id list.
function api_lablog_dismiss(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    # `_wstr` / `_wbool` (sockets.jl) absorb an explicit JSON null at the boundary — a client sending
    # `{"projectUid": null}` reaches `String(nothing)`/`Bool(nothing)` otherwise and aborts the handler.
    project_uid = _wstr(body, :projectUid)
    entry_id    = _wstr(body, :id)
    dismissed   = _wbool(body, :dismissed)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(entry_id)    && return 400, JSON3.write((; error="id required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    ids = try
        set_dismissed!(proj, entry_id, dismissed)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, dismissed=ids))
end

# append → one dated, author-tagged block. Server injects date + author tag (append-only, lock-guarded
# in append_lab_log!); body is {projectUid, author, lines: string | [string]}.
function api_lablog_append(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    author      = _wstr(body, :author)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(author)      && return 400, JSON3.write((; error="author required"))
    lines_raw = get(body, :lines, nothing)
    lines = if lines_raw isa AbstractString
        [String(lines_raw)]
    elseif lines_raw isa AbstractVector
        String[String(l) for l in lines_raw]
    else
        return 400, JSON3.write((; error="lines required (string or array of strings)"))
    end
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    # A `[LabArchives]` tag is a PROVENANCE claim — "this came from the lab notebook" — and the caller
    # picks it, so nothing else verifies it. The one check the server can make honestly: you cannot
    # claim notebook provenance on a project with no notebook linked. It does not (and cannot) prove a
    # given line really came from the ELN; it removes the case where none of them could have.
    if startswith(lowercase(strip(author)), "labarchives") &&
       !get(read_la_doc(proj), "present", false)
        return 409, JSON3.write((; error =
            "No LabArchives notebook is linked to this project. Call set_labarchives_context first, " *
            "or append as [Claude]."))
    end
    block = try
        append_lab_log!(proj, author, lines)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    # Notify observers (mcp/) of USER-written entries only — not the observer's own [Claude] writes
    # (would loop) nor [Cecelia] auto-digests (not a user decision). See OBSERVER.md §4.
    # `labarchives` is excluded for the same reason as `claude`: a [LabArchives] block is written BY
    # the observer (through the MCP append tool), so notifying observers of it would feed the monitor
    # its own write — the loop this guard exists to prevent.
    let a = lowercase(strip(author))
        if !startswith(a, "claude") && !startswith(a, "cecelia") && !startswith(a, "labarchives")
            broadcast_ws(Dict{String,Any}(
                "type" => "lab_log_entry_added", "projectUid" => project_uid,
                "summary" => join(lines, " ")))
        end
    end
    # Panel-reload signal for EVERY append (any author) — an external Chat-to-Claude session appends
    # straight through this route with no frontend action, so without this the open lab-log panel stays
    # stale until the user closes+reopens it. Distinct from `lab_log_entry_added` above (that's the MCP
    # observer's user-only, anti-loop notification); the frontend just reloads, so there's no loop.
    broadcast_ws(Dict{String,Any}("type" => "lab_log_updated", "projectUid" => project_uid))
    200, JSON3.write((; ok=true, block, entries=parse_lab_log(read_lab_log(proj))))
end

# capture → append an auto-generated [Cecelia] digest of run-log activity since the last capture.
# Returns captured=false (and appends nothing) when there's no new activity. Backs the panel's
# "Capture" button and the auto-on-open toggle.
function api_lablog_capture(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    block = try
        capture_context!(proj)
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, captured=(block !== nothing), block,
                        entries=parse_lab_log(read_lab_log(proj))))
end

# Backfill physical-size/timing meta for images imported before this metadata was tracked (or
# whose ccid.json lost these fields) — re-derives them from the already-converted OME-ZARR (same
# reader ImportOmezarr uses) without touching the original source file or re-running
# bioformats2raw. Returns the refreshed payload per uid so the frontend can drop the warning icon
# immediately, no page reload needed.
function api_images_meta_resync(body_bytes::Vector{UInt8})
    proj_dir, data, err = _parse_meta_request(body_bytes)
    isnothing(proj_dir) && return 400, JSON3.write((; error=err))
    project_uid = _wstr(data, :projectUid)
    image_uids  = [String(u) for u in get(data, :imageUids, [])]
    isempty(image_uids) && return 400, JSON3.write((; error="imageUids required"))

    images = Dict{String,Any}()
    for uid in image_uids
        img = init_object(project_uid, uid)
        img isa CciaImage || continue
        resync_ome_meta!(img)
        reloaded = init_object(project_uid, uid)
        reloaded isa CciaImage && (images[uid] = _image_payload(reloaded))
    end
    200, JSON3.write((; ok=true, images=images))
end

