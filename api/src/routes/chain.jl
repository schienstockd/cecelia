function api_chains_list(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    dir   = _chains_dir_for_project(uid)
    names = isdir(dir) ?
        sort([splitext(f)[1] for f in readdir(dir) if endswith(f, ".json") && !startswith(f, ".")]) :
        String[]
    200, JSON3.write((; chains=names))
end

function api_chains_get(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    name  = get(query, "name", "")
    isempty(uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    _valid_chain_name(name) || return _bad_chain_name(name)
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    path  = joinpath(_chains_dir_for_project(uid), "$(name).json")
    isfile(path) || return 404, JSON3.write((; error="Chain not found: $name"))
    # Return raw JSON — the template may contain extra fields (e.g. whiteboard positions)
    # that the frontend added; preserve them verbatim.
    200, read(path, String)
end

function api_chains_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    name = String(get(body, :name, ""))
    isempty(uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    _valid_chain_name(name) || return _bad_chain_name(name)
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    path = joinpath(_chains_dir_for_project(uid), "$(name).json")
    isfile(path) || return 404, JSON3.write((; error="Chain not found: $name"))
    rm(path)
    @info "Deleted chain" name project=uid
    _broadcast_chains_updated(uid)   # keep other open clients' pickers in step
    200, JSON3.write((; ok=true))
end

# GET /api/chains/runs?projectUid — list persisted run records (newest first) for the Live view.
# Reads each run.json's header fields directly (cheap; no template-cache resolution). Runs live under
# settings/chains/runs/<id>/run.json.
function api_chains_runs(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    runs_dir = joinpath(_chains_dir_for_project(uid), "runs")
    isdir(runs_dir) || return 200, JSON3.write((; runs=Any[]))
    out = Any[]
    for d in readdir(runs_dir; join=true)
        isdir(d) || continue
        rj = joinpath(d, "run.json")
        isfile(rj) || continue
        raw = try JSON3.read(read(rj, String)) catch; continue end
        push!(out, (; runId     = string(get(raw, :id, basename(d))),
                      chainName  = string(get(raw, :chain_name, "")),
                      createdAt  = Float64(get(raw, :created_at, 0.0)),
                      imageCount = length(get(raw, :image_uids, []))))
    end
    sort!(out; by = r -> r.createdAt, rev = true)
    200, JSON3.write((; runs = out))
end

# GET /api/chains/run?projectUid&runId — a single persisted run's frozen template (nodes/edges for
# the layered layout) + per-image per-node status, so the Live view can render a past run from disk.
function api_chains_run(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    uid   = get(query, "projectUid", "")
    rid   = get(query, "runId", "")
    (isempty(uid) || isempty(rid)) && return 400, JSON3.write((; error="projectUid and runId required"))
    proj = try load_project(uid) catch e; return 404, JSON3.write((; error=sprint(showerror, e))) end
    run  = try load_chain_run(proj, rid) catch; return 404, JSON3.write((; error="run not found: $rid")) end
    nodes = [(; id=n.id, fn=n.fn, params=n.params) for n in run.template_snapshot.nodes]
    edges = [(; from=e.from, to=e.to) for e in run.template_snapshot.edges]
    states = Dict{String,Any}()
    for (u, nm) in run.image_states
        states[u] = Dict{String,Any}(nid => string(st.status) for (nid, st) in nm)
    end
    200, JSON3.write((; runId=run.id, chainName=run.chain_name, createdAt=run.created_at,
                        imageUids=run.image_uids, nodes=nodes, edges=edges, imageStates=states))
end

function api_chains_save(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    tmpl = get(body, :template, nothing)
    isempty(uid)      && return 400, JSON3.write((; error="projectUid required"))
    isnothing(tmpl)   && return 400, JSON3.write((; error="template required"))
    name = String(get(tmpl, :name, ""))
    isempty(name)     && return 400, JSON3.write((; error="template.name required"))
    _valid_chain_name(name) || return _bad_chain_name(name)
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))

    # SAME CHECKER AS THE CREATE ROUTE. This wrote verbatim on the premise recorded below in
    # `api_chains_create` — "the whiteboard cannot express an invalid template, but an outside author
    # can". It can: deleting the start dot's edge leaves `startTargets: []`, and `_prune_to_start`
    # reads that as "run the whole chain", so an unwired dot surfaced NOWHERE between the canvas and
    # the executor. One checker, both doors.
    template = try
        chain_template_from_raw(tmpl; name = name)
    catch e
        return 400, JSON3.write((; error="Could not read template: $(sprint(showerror, e))"))
    end

    # REPAIR, not reject, for the empty start — same as the create route. An empty list is a
    # legitimate run-everything, so filling it with the roots is identical in semantics and merely
    # says so out loud, as a dot the user can see. Rejecting would break that documented mode and
    # hard-fail every chain saved before this route validated (two in the MERTK project alone).
    # Only this one field is ever rewritten; everything else is written back as it arrived.
    out = Dict{String,Any}(String(k) => v for (k, v) in tmpl)
    if isempty(template.start_targets)
        roots    = chain_root_ids(template)
        template = ChainTemplate(template.name, template.nodes, template.edges, roots)
        out["startTargets"] = roots
    end

    try
        validate_chain_template(template)
    catch e
        e isa ChainTemplateError || rethrow()
        return 400, JSON3.write((; error=e.msg))
    end

    dir  = _chains_dir_for_project(uid)
    mkpath(dir)
    # Still the whiteboard's own body — `positions` and any other canvas-only sidecar field survive.
    write_json_atomic(joinpath(dir, "$(name).json"), out)
    200, JSON3.write((; ok=true))
end

# POST /api/chains/create — author a chain from OUTSIDE the whiteboard (today: Claude via the MCP's
# `create_chain`). Body {projectUid, template:{name, nodes, edges, startTargets?}}.
#
# Deliberately NOT a relaxation of /api/chains/save, which must stay a verbatim overwrite — that one
# is the user saving their own canvas. This one is the additive counterpart of /api/notebooks/write:
#
#   • CREATE-ONLY (409 on an existing name) — an outside author can never replace a chain the user
#     wired. A "revision" is a new chain beside the old one, which the user compares and then keeps
#     or deletes. Chains have no snapshot/versioning, and don't need it: a ChainRun stores a
#     content-hashed copy of the template it ran, so past runs are unaffected either way.
#   • VALIDATED (400 naming the offender) — the whiteboard cannot express an invalid template, but an
#     outside author can, and until now nothing checked until run_chain, i.e. after the USER pressed
#     Run. See validate_chain_template (app/src/tasks/chain/validate.jl) for what is and isn't checkable here.
#
# Params may be SPARSE: send only what you mean to set. The whiteboard merges each task's spec
# defaults when it loads the template (applyTemplate), so an omitted param means "use the default".
function api_chains_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :projectUid, ""))
    tmpl = get(body, :template, nothing)
    isempty(uid)    && return 400, JSON3.write((; error="projectUid required"))
    isnothing(tmpl) && return 400, JSON3.write((; error="template required"))
    name = String(get(tmpl, :name, ""))
    isempty(name)   && return 400, JSON3.write((; error="template.name required"))
    _valid_chain_name(name) || return _bad_chain_name(name)
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))

    dir  = _chains_dir_for_project(uid)
    path = joinpath(dir, "$(name).json")
    isfile(path) && return 409, JSON3.write((;
        error="Chain '$name' already exists — pick another name (this route never overwrites)"))

    # Parse through the package's own reader so what we validate is exactly what run_chain will
    # load, then validate. Unknown/absent fields default the same way a loaded template does.
    template = try
        chain_template_from_raw(tmpl; name = name)
    catch e
        return 400, JSON3.write((; error="Could not read template: $(sprint(showerror, e))"))
    end

    # Fill the UML start dot when the author didn't. Execution doesn't need it (empty start_targets runs
    # the whole chain), but the WHITEBOARD does: with no target and no saved position the dot isn't drawn
    # at all, so the chain opens with nothing marking where a run begins and the user has to add and wire
    # one by hand. Every hand-wired chain has it, so an authored one should too. Roots only — that is
    # exactly "run the whole chain", i.e. the same semantics the empty list already had.
    if isempty(template.start_targets)
        template = ChainTemplate(template.name, template.nodes, template.edges,
                                 chain_root_ids(template))
    end

    try
        validate_chain_template(template)
    catch e
        e isa ChainTemplateError || rethrow()
        return 400, JSON3.write((; error=e.msg))
    end

    mkpath(dir)
    # Write through save_chain_template! so the on-disk shape is the one the package writes — an
    # outside author supplies no `positions`, and the whiteboard lays the nodes out on first load.
    save_chain_template!(load_project(uid), template)
    @info "Created chain" name project=uid nodes=length(template.nodes)
    _broadcast_chains_updated(uid)
    200, JSON3.write((; ok=true, name, nodeCount=length(template.nodes)))
end

# POST /api/chains/rename — body {projectUid, name, newName}. One atomic move, rather than the
# frontend hand-rolling save-as + delete (which leaves both copies behind if the second call fails).
#
# Past run records are deliberately NOT rewritten: ChainRun.chain_name is what the run ran AS, a
# historical fact, the same reasoning that freezes its template copy. Old runs keep the old label.
# Renaming only degrades a run of this chain that is IN FLIGHT — the Live view fetches the current
# template by name for its column layout (and already falls back to a task-derived layout when the
# name misses), which is why the GUI disables the control while the chain has a live run.
function api_chains_rename(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid     = String(get(body, :projectUid, ""))
    name    = String(get(body, :name, ""))
    newname = String(strip(String(get(body, :newName, ""))))
    isempty(uid)     && return 400, JSON3.write((; error="projectUid required"))
    isempty(name)    && return 400, JSON3.write((; error="name required"))
    isempty(newname) && return 400, JSON3.write((; error="newName required"))
    _valid_chain_name(name)    || return _bad_chain_name(name)
    _valid_chain_name(newname) || return _bad_chain_name(newname)
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))

    dir = _chains_dir_for_project(uid)
    src = joinpath(dir, "$(name).json")
    dst = joinpath(dir, "$(newname).json")
    isfile(src) || return 404, JSON3.write((; error="Chain not found: $name"))
    name == newname && return 200, JSON3.write((; ok=true, name=newname))   # no-op, not an error
    isfile(dst) && return 409, JSON3.write((; error="Chain '$newname' already exists"))

    # The template carries its own `name` field (load_chain_template falls back to the filename, but
    # the whiteboard reads the field) — so rewrite it in the same move, or the renamed chain saves
    # itself back under the old name.
    raw = try
        r = read_ccid_raw(src)   # the canonical "state JSON → mutable String-keyed Dict" reader
        r["name"] = newname
        r
    catch e
        return 500, JSON3.write((; error="Could not read chain '$name': $(sprint(showerror, e))"))
    end
    write_json_atomic(dst, raw)
    rm(src)
    @info "Renamed chain" from=name to=newname project=uid
    _broadcast_chains_updated(uid)
    200, JSON3.write((; ok=true, name=newname))
end

