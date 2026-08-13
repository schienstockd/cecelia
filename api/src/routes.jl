using Dates
import Base64   # decode base64 PNGs when migrating legacy inline board images to sidecar files

# ── Chain template CRUD ───────────────────────────────────────────────────────

# Per-project persisted UI config lives under `<proj>/settings/` (chains, analysis-canvas boards, …).
_settings_dir_for_project(project_uid::String) = joinpath(projects_dir(), project_uid, "settings")

# Analysis-board image assets (napari screenshots) live as SIDECAR PNGs under settings/board-assets/,
# NOT base64 inside analysisBoards.json — so the board JSON stays small (autosave-friendly) and the
# images are transparent on disk. See docs/todo/ANIMATION_PLAN.md.
_board_assets_dir(project_uid::String) = joinpath(_settings_dir_for_project(project_uid), "board-assets")
_valid_asset_id(id::AbstractString) = occursin(r"^[A-Za-z0-9_-]+$", id)   # guard against path traversal

# Rendered movies (single-image / animation / batch recordings) live under {proj}/movies/ as .mp4.
# The movie player (/movies page) lists and streams them from here. This mirrors `_movies_dir(img)`
# in napari_api.jl — {proj}/movies == projects_dir()/<uid>/movies — but keyed by project uid (no image
# needed just to list). Filenames are sanitised at write time to [A-Za-z0-9._-] (see _movie_basename /
# _movie_named_path), so this guard both validates and blocks path traversal.
_movies_dir_for_project(project_uid::String) = joinpath(projects_dir(), project_uid, "movies")
_valid_movie_name(name::AbstractString) = occursin(r"^[A-Za-z0-9._-]+\.mp4$", name)

# GET /api/movies?projectUid=… → { movies: [{name, size, mtime, displayName, starred, tags,
# producedBy, hasConfig, configKind, configStale}] } sorted newest-first. Lists the project's rendered
# .mp4s for the player playlist, each merged with its `settings/movies.json` entry; the bytes are
# streamed separately (range-served) by try_serve_movie in server.jl. Empty list (not 404) when the
# movies dir doesn't exist yet.
#
# The listing itself lives in `movies_with_meta` (movies_api.jl), which also reconciles the registry
# against the directory in the same pass. The saved CONFIG is deliberately not in this response — a
# keyframe config is large and the list renders none of it; `/api/movies/meta` fetches one on demand.
function api_movies_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found"))
    200, JSON3.write((; movies = movies_with_meta(uid)))
end

# Copy a captured PNG (temp file) into settings/board-assets/<id>.png; returns the new asset id.
function _save_board_asset_file(project_uid::String, src_png::String)::String
    dir = _board_assets_dir(project_uid); mkpath(dir)
    id = gen_uid()
    cp(src_png, joinpath(dir, id * ".png"); force = true)
    id
end

function _chains_dir_for_project(project_uid::String)
    newdir = joinpath(_settings_dir_for_project(project_uid), "chains")
    olddir = joinpath(projects_dir(), project_uid, "chains")   # legacy location (pre-settings/)
    if isdir(olddir) && !isdir(newdir)
        try; mkpath(_settings_dir_for_project(project_uid)); mv(olddir, newdir)
        catch e; @warn "Could not migrate chains into settings/" project=project_uid exception=e; end
    end
    newdir
end

# A chain name becomes a filename (`<name>.json`) under settings/chains/, so it needs the same guard
# as the other name→path routes here (_valid_asset_id, _valid_movie_name). Every chain route checks
# it: the templates dir is inside the project, and a name is now supplied by the MCP as well as by
# the whiteboard, so `../../` must not resolve anywhere. Also keeps names round-trippable in a URL.
_valid_chain_name(name::AbstractString) =
    occursin(r"^[A-Za-z0-9][A-Za-z0-9._ -]{0,63}$", name) && !occursin("..", name)

_bad_chain_name(name) = (400, JSON3.write((;
    error="Invalid chain name '$name' — use letters, numbers, spaces, . _ - (max 64 chars)")))

# The chains dir changed → tell every open whiteboard to re-read the LIST. Without this, a chain
# written by anything other than the whiteboard itself (Claude via /api/chains/create, the REPL) is
# invisible until a full page reload: the picker is filled by `loadChainList()`, which runs on mount
# and project switch only, and `ChainModule` lives under <KeepAlive> so navigating away and back does
# not remount it. The ↻ button does NOT help — it reloads the ACTIVE chain's content, not the list.
# Same reasoning (and the same shape) as `lab_log_updated` further down this file: an out-of-band write
# needs a signal, or the open page silently shows stale state. Refreshing the list is non-destructive —
# `loadChainList` only switches chains if the active one vanished, so it never clobbers unsaved edits.
_broadcast_chains_updated(project_uid::AbstractString) =
    broadcast_ws(Dict{String,Any}("type" => "chains_updated", "projectUid" => String(project_uid)))

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
    dir  = _chains_dir_for_project(uid)
    mkpath(dir)
    # Write verbatim — preserves any extra fields (positions, etc.) the whiteboard added.
    write_json_atomic(joinpath(dir, "$(name).json"), tmpl)
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
#     Run. See validate_chain_template (app/src/tasks/chain.jl) for what is and isn't checkable here.
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

# ── Task definitions (single-source from package JSON specs) ──────────────────

const _TASK_SPECS_ROOT = joinpath(@__DIR__, "..", "..", "app", "src", "tasks")

function api_task_definitions(req::HTTP.Request)
    uri    = HTTP.URI(req.target)
    query  = HTTP.queryparams(uri)
    cat    = get(query, "category", "")   # optional filter

    raw = Dict{String, Vector{Any}}()
    isdir(_TASK_SPECS_ROOT) || return 200, JSON3.write(raw)

    frag_dir = joinpath(_TASK_SPECS_ROOT, "fragments")

    for entry in readdir(_TASK_SPECS_ROOT; join=true)
        isdir(entry) || continue
        entry == frag_dir && continue          # skip shared fragments
        basename(entry) == "testTasks" && continue   # dev-only stubs for the test suite — never user-facing
        category = basename(entry)
        (!isempty(cat) && category != cat) && continue
        specs = Any[]
        for f in readdir(entry; join=true)
            endswith(f, ".json") || continue
            try
                parsed = JSON3.read(read(f, String), Dict{String,Any})
                resolved = Cecelia._resolve_spec_includes(parsed, frag_dir)
                push!(specs, resolved)
            catch e
                @warn "Skipping malformed task spec" path=f exception=e
            end
        end
        isempty(specs) || (raw[category] = specs)
    end

    # ── User drop-in modules (custom tasks) ────────────────────────────────────
    # Same directory-driven contract as the built-ins, and the SAME co-located layout, but rooted at
    # the per-user config dir (<config_dir>/modules/<category>/<name>.json). Category = subdir name, so
    # a custom task in an existing category (e.g. behaviour/) appears in that module page automatically.
    # Built-ins win on a fun_name clash. See docs/CUSTOM_MODULES.md and Cecelia.load_custom_modules!.
    builtin_funs = Set{String}()
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        isempty(fn) || push!(builtin_funs, fn)
    end
    user_defs_root = joinpath(Cecelia.config_dir(), "modules")
    if isdir(user_defs_root)
        for entry in readdir(user_defs_root; join=true)
            isdir(entry) || continue
            category = basename(entry)
            category in ("sources", "inputDefinitions", "python") && continue  # legacy layout dirs
            (!isempty(cat) && category != cat) && continue
            for f in readdir(entry; join=true)
                endswith(f, ".json") || continue
                try
                    parsed = JSON3.read(read(f, String), Dict{String,Any})
                    fn     = string(get(parsed, "fun_name", ""))
                    (isempty(fn) || fn ∈ builtin_funs) && continue   # need a fun_name; built-ins win
                    resolved = Cecelia._resolve_spec_includes(parsed, frag_dir)
                    push!(get!(raw, category, Any[]), resolved)
                catch e
                    @warn "Skipping malformed custom task spec" path=f exception=e
                end
            end
        end
    end

    # Runtime-enumerated options (e.g. CellposeSegment's Model picker: built-ins + files under
    # <install>/models/cellposeModels/ + <config_dir>/models/cellposeModels/) — mutate specs in
    # place via the same dispatch hook `validate_params` uses (`_inject_dynamic_options!`), so
    # picker and validation stay in sync. See docs/SEGMENTATION.md → Custom cellpose checkpoints.
    fun_map = Cecelia._fun_name_map()
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        (isempty(fn) || !haskey(fun_map, fn)) && continue
        Cecelia._needs_dynamic_options(fun_map[fn]) || continue
        Cecelia._inject_dynamic_options!(spec, fun_map[fn])
    end

    # Stamp the task-preview trait onto each spec. Declared in Julia beside the task
    # (`task_previewable`, task.jl) and stamped here rather than written into the JSON, because the JSON
    # is the PARAM spec — a capability of the compute doesn't belong in it, and duplicating it there
    # would let the two disagree. The frontend reads `previewable` instead of guessing from the params.
    # Composites resolve through their own overload, so `segment.cellposeMeasure` reports true.
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        haskey(fun_map, fn) || continue
        spec["previewable"] = try
            Cecelia.task_previewable(fun_map[fn])
        catch e
            # a task's own overload must never take the whole picker down (same guard as
            # `_live_outputs_for`): report not-previewable and carry on
            @warn "task_previewable failed" fun=fn exception=e
            false
        end
    end

    # Build fun_name → spec lookup so composite tasks can pull params from their steps.
    by_fun = Dict{String, Any}()
    for specs in values(raw)
        for spec in specs
            fn = string(get(spec, "fun_name", ""))
            isempty(fn) || (by_fun[fn] = spec)
        end
    end

    # For composite specs: merge params from sub-task specs (dedup by key, first wins).
    result = Dict{String, Vector{Any}}()
    for (category, specs) in raw
        out = Any[]
        for spec in specs
            composite = get(spec, "composite", nothing)
            if !isnothing(composite) && !isempty(composite)
                merged = Any[]
                seen   = Set{String}()
                # Union `requires.axes` across sub-tasks so the frontend gate sees the composite's
                # true axis needs without walking steps (mirrors Cecelia.task_requires_axes on the
                # backend). A composite with its own explicit `requires.axes` still contributes.
                req_axes = Set{String}()
                own_req  = get(spec, "requires", nothing)
                if own_req isa AbstractDict
                    for a in get(own_req, "axes", String[])
                        s = uppercase(string(a))
                        isempty(s) || push!(req_axes, s)
                    end
                end
                for fn_ref in composite
                    sub = get(by_fun, string(fn_ref), nothing)
                    isnothing(sub) && continue
                    for p in get(sub, "params", [])
                        k = string(get(p, "key", ""))
                        k ∈ seen && continue
                        # Skip params derived inside the composite (e.g. hmm_transitions.hmmStates,
                        # set automatically from the states step) — they shouldn't appear in the form.
                        (p isa AbstractDict && get(p, "hideInComposite", false) == true) && continue
                        push!(seen, k)
                        push!(merged, p)
                    end
                    sub_req = get(sub, "requires", nothing)
                    sub_req isa AbstractDict || continue
                    for a in get(sub_req, "axes", String[])
                        s = uppercase(string(a))
                        isempty(s) || push!(req_axes, s)
                    end
                end
                spec["params"] = merged
                if !isempty(req_axes)
                    spec["requires"] = Dict{String,Any}("axes" => sort!(collect(req_axes)))
                end
                push!(out, spec)
            else
                push!(out, spec)
            end
        end
        result[category] = out
    end

    200, JSON3.write(result)
end

# ── Custom (user drop-in) modules ─────────────────────────────────────────────
# GET  /api/tasks/custom-modules         → load report + category list (see below)
# POST /api/tasks/custom-modules/reload  → rescan <config_dir>/modules for NEWLY dropped .jl, then
#                                          return the same report. (Edits to already-loaded modules
#                                          need a server restart — same as any app/ struct change.)
# See docs/CUSTOM_MODULES.md and Cecelia.load_custom_modules!.

# Categories present among the user's custom specs, each flagged whether a built-in page already owns
# that category (a matching dir under app/src/tasks). The frontend renders a generic page + nav entry
# only for the NEW categories (builtin == false); tasks in an existing category already show there.
function _custom_module_categories()
    user_defs_root = joinpath(Cecelia.config_dir(), "modules")
    isdir(user_defs_root) || return Any[]
    builtin = Set(basename(e) for e in readdir(_TASK_SPECS_ROOT; join=true) if isdir(e))
    cats = Any[]
    for entry in readdir(user_defs_root; join=true)
        isdir(entry) || continue
        category = basename(entry)
        category in ("sources", "inputDefinitions", "python") && continue  # legacy layout dirs
        funs = String[]
        for f in readdir(entry; join=true)
            endswith(f, ".json") || continue
            try
                parsed = JSON3.read(read(f, String), Dict{String,Any})
                fn = string(get(parsed, "fun_name", ""))
                isempty(fn) || push!(funs, fn)
            catch
            end
        end
        isempty(funs) && continue
        # cohortFuns = the category's funs that bank cohort-comparable metrics (Cecelia.COHORT_METRICS,
        # populated at load incl. custom modules' register_cohort_metrics!). Drives the "Check cohort"
        # button on the generic custom page WITHOUT any hardcoded per-page list — a custom module that
        # declares its metrics gets the button automatically.
        cohort_funs = String[f for f in funs if haskey(Cecelia.COHORT_METRICS, f)]
        push!(cats, (; name = category, builtin = category ∈ builtin, funNames = funs, cohortFuns = cohort_funs))
    end
    cats
end

function api_custom_modules_status(::HTTP.Request)
    200, JSON3.write((; dir        = Cecelia.custom_modules_dir(),
                        modules    = Cecelia.custom_modules_report(),
                        categories = _custom_module_categories()))
end

function api_custom_modules_reload(::Vector{UInt8})
    res = Cecelia.load_custom_modules!()
    200, JSON3.write((; dir        = Cecelia.custom_modules_dir(),
                        loaded     = res.loaded,
                        skipped    = res.skipped,
                        removed    = res.removed,
                        failed     = [(; path = p, error = m) for (p, m) in res.failed],
                        modules    = Cecelia.custom_modules_report(),
                        categories = _custom_module_categories()))
end

# ── Task param memory (funParams) ─────────────────────────────────────────────
# GET /api/tasks/funparams?projectUid=&fun=&imageUid=&setUid=
# Returns the last-used params for `fun`, resolved image → set → none (R parity). The frontend
# passes imageUid only when exactly one image is selected (else the shared set-level default).
function api_task_fun_params(req::HTTP.Request)
    q     = HTTP.queryparams(HTTP.URI(req.target))
    proj  = get(q, "projectUid", "")
    fun   = get(q, "fun", "")
    imgu  = get(q, "imageUid", "")
    setu  = get(q, "setUid", "")
    (isempty(proj) || isempty(fun)) &&
        return 400, JSON3.write((; error = "projectUid and fun are required"))

    proj_root = joinpath(projects_dir(), proj)
    params = isempty(imgu) ? nothing :
             Cecelia.read_module_fun_params(joinpath(proj_root, "1", imgu), fun)
    if isnothing(params) && !isempty(setu)
        params = Cecelia.read_module_fun_params(joinpath(proj_root, "1", setu), fun)
    end
    200, JSON3.write((; params = params))
end

# ── Resource pools ───────────────────────────────────────────────────────────

# Each pool as {name, limit, running, queued} — the throttle sliders use `limit`, the occupancy
# readout uses `running`/`queued`. Poll this for a live view (there is no pool:* WS event).
# Pools are the RUNNER's when it is executing tasks — its process owns them, so its numbers are the
# real ones and this server's are an idle copy of the same config. Report the runner's when there is
# one; fall back to local so the panel is never blank (and is correct for the in-process path).
# NOT a merge: two schedulers cannot share one GPU, so exactly one set of numbers is ever the truth
# (docs/todo/TASK_RUNNER_PLAN.md → Decision 1). Adding them up would invent a budget nobody enforces.
function api_pools_list(_req)
    remote = try; _runner_enabled() ? Cecelia.runner_pools(_RUNNER) : nothing; catch; nothing; end
    isnothing(remote) || isempty(remote) ||
        return 200, JSON3.write(sort(remote, by = p -> String(get(p, "name", ""))))
    200, JSON3.write(sort(pool_status(), by=p->p.name))
end

# Set a pool's concurrency limit live (Settings sliders): resize now + persist to custom.toml.
# Only already-configured pools are settable (no typo pools accumulating in custom.toml).
# ── Image store compression ──────────────────────────────────────────────────
# GET → { current, default, choices: [{name, label, detail}] }. The choice list is served, never
# duplicated in Vue — same rule as task param specs (CLAUDE.md → the JSON spec is the single source).
function api_compressor_get(_req)
    choices = [(; name = c.name, label = c.label, size = c.size, ratio = c.ratio,
                  write = c.write, read = c.read, url = c.url)
               for c in Cecelia.IMAGE_COMPRESSOR_CHOICES]
    200, JSON3.write((; current = Cecelia.image_compressor(),
                        default = Cecelia.IMAGE_COMPRESSOR_DEFAULT,
                        measuredOn = Cecelia.IMAGE_COMPRESSOR_MEASURED_ON,
                        choices = choices))
end

# Set it live: persists to custom.toml + hot-reloads, so the NEXT task writes with it. Existing
# stores are untouched (a re-write is rechunk_zarr.py's job) — the UI says so.
function api_compressor_set(body_bytes)
    data = JSON3.read(body_bytes)
    name = String(get(data, :name, ""))
    isempty(name) && return 400, JSON3.write((; error = "name required"))
    try
        200, JSON3.write((; current = Cecelia.set_image_compressor!(name)))
    catch e
        e isa ArgumentError ? (400, JSON3.write((; error = e.msg))) : rethrow()
    end
end

# ── Store LAYOUT defaults (zarr format + chunk separator) ─────────────────────────
# GET → { current, default, measuredOn, choices: [...] }. Shaped like the compressor endpoint on
# purpose: it is the same kind of decision and Settings renders it the same way, as a TABLE with the
# measured numbers, because the trade-off is the only reason there is a choice.
#
# The choices are the three VIABLE combinations of NGFF version + separator, not two independent
# controls — flat keys and NGFF 0.5 cannot be combined (bioformats2raw silently writes zarr v2 for that
# pair), so offering combinations makes the impossible state unreachable rather than warned.
#
# These are DEFAULTS the import form pre-fills, not a switch over what happens next: format and
# separator are fixed per image at import (no converter) and derived stores inherit from their source.
# docs/todo/ZARR_V3_PLAN.md D10.
function api_store_layout_get(_req)
    choices = [(; name = c.name, label = c.label, keys = c.keys, dirs = c.dirs,
                  size = c.size, read = c.read, detail = c.detail,
                  ngffVersion = c.ngffVersion, chunkSeparator = c.chunkSeparator)
               for c in Cecelia.STORE_LAYOUT_CHOICES]
    200, JSON3.write((; current = Cecelia.store_layout().name,
                        default = Cecelia.STORE_LAYOUT_DEFAULT,
                        measuredOn = Cecelia.STORE_LAYOUT_MEASURED_ON,
                        choices = choices))
end

function api_store_layout_set(body_bytes)
    data = JSON3.read(body_bytes)
    name = String(get(data, :name, ""))
    isempty(name) && return 400, JSON3.write((; error = "name required"))
    try
        200, JSON3.write((; current = Cecelia.set_store_layout!(name)))
    catch e
        e isa ArgumentError ? (400, JSON3.write((; error = e.msg))) : rethrow()
    end
end

function api_pool_set(body_bytes)
    data  = JSON3.read(body_bytes)
    name  = String(get(data, :name, ""))
    limit = Int(get(data, :limit, 0))
    isempty(name) && return 400, JSON3.write((; error = "name required"))
    known = Set(p.name for p in list_pools())
    name in known || return 400, JSON3.write((; error = "unknown pool '$name'"))
    applied = set_pool_limit!(name, limit)
    # The throttle has to reach the process that actually rations the slots, or the sliders move a
    # budget nothing enforces. Applied locally too (above), so the in-process fallback path stays
    # governed by the same numbers. Best-effort: a runner that is down must not fail the control.
    if _runner_enabled()
        try; Cecelia.runner_set_pool_limit(_RUNNER, name, applied)
        catch e; @warn "Could not apply the pool limit on the runner" name limit exception = e; end
    end
    200, JSON3.write((; name = name, limit = applied))
end

# Point-in-time snapshot of queued/running tasks (reporting only — no control).
# The WS `task:*` / `chain:node:*` stream is the live feed; this fills in what is
# already in-flight when a console first connects.
# Merged, unlike pools: work genuinely lives in BOTH processes. Chains, background jobs and the
# in-process fallback are here; module-page tasks are on the runner. A snapshot missing either half
# is a quit/export busy-check that reads idle, and a browser that adopts only some running rows.
# De-duplicated by id (runner wins) so a task that somehow appears in both is one row, not two.
function api_tasks_list(_req)
    local_tasks = list_tasks()
    remote = try; _runner_enabled() ? Cecelia.runner_tasks(_RUNNER) : Any[]; catch; Any[]; end
    isempty(remote) && return 200, JSON3.write(local_tasks)
    seen = Set(String(get(t, "id", "")) for t in remote)
    200, JSON3.write(vcat(remote, [t for t in local_tasks if !(t.id in seen)]))
end

# Terminal outcomes of recently finished tasks (reporting only). The companion to /api/tasks: that
# one answers "what is in flight", this one "how did the ones that left it end". A poller needs both
# because the WS `task:status` frame carrying the outcome is dropped for a slow client by design —
# without this the console can only report "finished, outcome unseen". `since` (a previous poll's
# newest `finished_at`) returns just the newer entries. NOT run history — that is
# /api/tasks/history, on disk and permanent.
function api_tasks_recent(req)
    q = HTTP.queryparams(HTTP.URI(req.target))
    200, JSON3.write(recent_tasks(; since = get(q, "since", "")))
end

# ── Filesystem browser ────────────────────────────────────────────────────────

# FS_ROOT: OPTIONAL sandbox. Empty (the default) = browse the whole filesystem — required to reach
# mounted network drives / external storage (SMB, `/mnt`, `/media`, …), which live OUTSIDE the home
# dir. Set CECELIA_FS_ROOT to confine the browser to one subtree. (Was hard-clamped to homedir(),
# which made network drives unreachable from the import file picker.)
const FS_ROOT = get(ENV, "CECELIA_FS_ROOT", "")

# Whether a path is inside the sandbox (always true when no sandbox is configured).
_fs_confined(path::AbstractString)::Bool = isempty(FS_ROOT) || startswith(path, FS_ROOT)

# Parent dir, or `nothing` at the navigable ceiling (the filesystem root `/` or a Windows drive, or
# FS_ROOT when sandboxed) so the UI stops offering "up".
function _fs_parent(base::String)
    par = dirname(base)
    par == base && return nothing            # filesystem root (/ or C:\)
    _fs_confined(par) ? par : nothing        # sandbox floor
end

# Quick-jump shortcuts: home + the common mount parents where external/network drives appear.
function _fs_shortcuts()
    sc = Tuple{String,String}[("Home", homedir())]
    if Sys.iswindows()
        for c in 'A':'Z'
            d = string(c, ":\\"); isdir(d) && push!(sc, (string(c, ":"), d))
        end
    else
        for d in ("/", "/mnt", "/media", "/Volumes", "/run/media")
            isdir(d) && push!(sc, (d == "/" ? "Root" : basename(d), d))
        end
    end
    [(; label, path) for (label, path) in sc if _fs_confined(path)]
end

const IMAGE_EXTS = Set([
    # TIFF family
    ".tif", ".tiff", ".btf", ".tf8", ".tf2",
    # OME-ZARR
    ".zarr",
    # Zeiss
    ".czi", ".lsm",
    # Leica
    ".lif", ".lei", ".xlef", ".scn",
    # Nikon
    ".nd2",
    # Olympus / Evident
    ".oir", ".oib", ".oif", ".vsi",
    # MetaMorph
    ".nd", ".stk",
    # 3i Slidebook
    ".sld",
    # Imaris / HDF5
    ".ims", ".h5", ".hdf5",
    # PerkinElmer
    ".flex",
    # Whole slide imaging
    ".svs", ".ndpi", ".mrxs", ".qptiff",
    # DICOM
    ".dcm", ".dicom",
    # Common raster (useful for testing / simple imports)
    ".png", ".jpg", ".jpeg",
])

function api_fs_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    p     = get(query, "path", "")
    # Absolute paths are used as-is (browse anywhere); empty → home; a relative path resolves against
    # home (back-compat with the old relative-to-root contract). Confine to FS_ROOT only if it's set.
    base = isempty(p)      ? homedir() :
           isabspath(p)    ? normpath(p) :
                             normpath(joinpath(homedir(), p))
    _fs_confined(base) || (base = FS_ROOT)
    isdir(base) || return 400, JSON3.write((; error="Not a directory: $base"))

    names = try
        readdir(base; join=false)
    catch e
        return 400, JSON3.write((; error="Cannot read directory: $(sprint(showerror, e))"))
    end
    entries = map(names) do name
        full   = joinpath(base, name)
        ext    = lowercase(splitext(name)[2])
        isdir_ = try isdir(full) catch; false end          # broken symlink / no perms → treat as file
        (; name, path=full, isdir=isdir_,                   # ABSOLUTE path (import resolves it directly)
           isimage=!isdir_ && ext ∈ IMAGE_EXTS, ext,
           size=isdir_ ? nothing : (try filesize(full) catch; nothing end))
    end
    visible = filter(e -> !startswith(e.name, "."), entries)
    sorted  = sort(visible; by=e -> (!e.isdir, lowercase(e.name)))
    200, JSON3.write((; root=homedir(), current=base, parent=_fs_parent(base),
                       shortcuts=_fs_shortcuts(), entries=sorted))
end

# ── Project management ────────────────────────────────────────────────────────

function api_projects_list(req::HTTP.Request)
    projects = _scan_projects_raw()
    200, JSON3.write((; projects, projectsDir=projects_dir()))
end

# GET /api/projects/bundles — .ccbundle files in the default export dir, for the import picker.
function api_projects_bundles(::HTTP.Request)
    try
        200, JSON3.write((; bundles = list_bundles(), exportDir = default_export_dir()))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# GET /api/projects/bundle-info?path= — peek a bundle's uid/name + whether that uid already exists,
# so the import UI can prompt (replace / copy / cancel) BEFORE unpacking.
function api_projects_bundle_info(req::HTTP.Request)
    path = get(HTTP.queryparams(HTTP.URI(req.target)), "path", "")
    isempty(path) && return 400, JSON3.write((; error = "path required"))
    info = bundle_info(path)
    isnothing(info) && return 400, JSON3.write((; error = "Not a cecelia bundle: $path"))
    200, JSON3.write(info)
end

function api_projects_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    name = String(strip(String(get(body, :name, ""))))
    isempty(name) && return 400, JSON3.write((; error="Project name is required"))

    existing = _scan_projects_raw()
    any(p -> get(p, "name", "") == name, existing) &&
        return 400, JSON3.write((; error="A project named \"$name\" already exists"))

    # Project-wide static/live/flow distinction was dropped — applicability is per-image, derived
    # from axes (see Cecelia.img_axes / task_applies). `kind` is retained on the struct as a
    # vestigial no-op so pre-existing project.json files still round-trip.
    proj = create_project!(name=name)
    meta = Dict{String,Any}("uid"=>proj.uid, "name"=>proj.name, "path"=>proj.root,
                             "meta"=>proj.meta, "set_uids"=>proj.set_uids,
                             "createdAt"=>string(now()), "lastOpenedAt"=>string(now()))
    @info "Created project" name uid=proj.uid
    200, JSON3.write((; project=meta))
end

function api_projects_load(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :uid, ""))
    isempty(uid) && return 400, JSON3.write((; error="Project UID is required"))

    projects = _scan_projects_raw()
    idx = findfirst(p -> get(p, "uid", "") == uid, projects)
    isnothing(idx) && return 404, JSON3.write((; error="Project not found: $uid"))

    project = projects[idx]
    proj_dir = string(project["path"])

    # Update lastOpenedAt
    meta_file = joinpath(proj_dir, "project.json")
    try
        raw = read_ccid_raw(meta_file)
        raw["lastOpenedAt"] = string(now())
        write_json_atomic(meta_file, raw)
        project["lastOpenedAt"] = raw["lastOpenedAt"]
    catch e
        @warn "Could not update lastOpenedAt" uid exception=e
    end

    proj_obj = load_project(uid)
    sets     = [_set_payload(s) for s in proj_obj._sets]

    # Analysis-canvas boards saved with the project (settings/); null when none saved yet.
    # Animation page: captured view snapshots (settings/animations.json). Sidecar PNGs live in the same
    # board-assets/ store as the board strip (shared capture path), so this JSON stays small.
    animations = nothing
    anim_file = joinpath(_settings_dir_for_project(uid), "animations.json")
    if isfile(anim_file)
        try; animations = JSON3.read(read(anim_file, String)); catch e
            @warn "Could not read animations" uid exception=e
        end
    end

    # Normalised through the one reader (app/src/analysis_boards.jl) so the client always sees the
    # current shape and, crucially, the `version` its next autosave has to echo back. `null` when the
    # project has never saved a board.
    boards_doc = read_boards_doc(boards_doc_path(proj_dir))
    boards = boards_doc.present ? boards_doc_payload(boards_doc) : nothing

    # Per-object module-page canvas layouts, stored WITH each object at 1/{uid}/moduleCanvases.json
    # (like ccid.json / labelProps — locality + auto-cleanup on delete). Reassemble the per-canvas-key
    # map by merging every object's file; null when none saved.
    moduleCanvases = nothing
    onedir = joinpath(proj_dir, "1")
    if isdir(onedir)
        merged_entries = Dict{String,Any}(); merged_geom = Dict{String,Any}()
        for obj in readdir(onedir)
            f = joinpath(onedir, obj, "moduleCanvases.json")
            isfile(f) || continue
            try
                d = JSON3.read(read(f, String))
                for (k, v) in pairs(get(d, :entries, Dict{Symbol,Any}())); merged_entries[String(k)] = v; end
                for (k, v) in pairs(get(d, :geom, Dict{Symbol,Any}()));    merged_geom[String(k)] = v;    end
            catch e
                @warn "Could not read module canvases" obj exception=e
            end
        end
        (isempty(merged_entries) && isempty(merged_geom)) ||
            (moduleCanvases = Dict("entries" => merged_entries, "geom" => merged_geom))
    end

    @info "Opened project" name=get(project, "name", "?") uid sets=length(sets)
    200, JSON3.write((; project, sets, boards, moduleCanvases, animations))
end

# POST /api/projects/boards  { projectUid, boards: { tabs, layouts } }
# Debounced AUTOSAVE of the /analysis boards (tabs + grid layouts + slot state incl. strip snapshots) →
# settings/analysisBoards.json. Board IMAGES are sidecar files (board-assets/, see below), NOT base64
# in this JSON, so it stays small and cheap to rewrite on every edit. Mirrors api_projects_canvases (the
# module-page autosave). Opaque frontend JSON, stored verbatim. `lastOpenedAt` is stamped on project
# OPEN (api_projects_load), so there's nothing to touch in project.json here. Replaces the old
# api_projects_save + the manual save button.
function api_projects_boards(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    boards = get(body, :boards, nothing)
    boards === nothing && return 200, JSON3.write((; ok=true))
    path = boards_doc_path(joinpath(projects_dir(), uid))
    try
        current  = read_boards_doc(path)
        incoming = normalise_boards(boards)
        # OPTIMISTIC CONCURRENCY. The client echoes the version it last read; if the document has moved
        # on since (another browser tab, or the MCP add-a-board route later), reject rather than let the
        # later writer win silently — which is what two tabs open on one project used to do to each
        # other. The client reloads from the returned document and retries. A client that sends no
        # version at all is an OLD frontend against a new server: let it through rather than wedge the
        # autosave, since that is exactly the pairing a mid-session reload produces.
        want = get(body, :version, nothing)
        sent = want isa Integer ? Int(want) : want isa Real ? Int(round(want)) : -1
        if want !== nothing && current.present && current.readable && sent != current.version
            return 409, JSON3.write((; error="Boards changed since you loaded them",
                                       code="stale_version", boards=boards_doc_payload(current)))
        end
        version = write_boards_doc(path, incoming; version = current.version + 1)
        # Tell every OTHER open client to pick this up. The writer identifies its own echo by `clientId`,
        # NOT by version: this broadcast goes out before the response does, so the writer still holds the
        # pre-write version when its own frame arrives — a version test made every autosave reload and
        # re-render the board that had just been saved. Absent for a non-browser writer (the MCP
        # add-a-board route), which is correct: every browser should pick that one up.
        broadcast_ws(Dict{String,Any}("type" => "boards:changed", "projectUid" => uid,
                                      "version" => version,
                                      "clientId" => String(get(body, :clientId, ""))))
        return 200, JSON3.write((; ok=true, version))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/boards/add  { projectUid, name, plots:[…], template? }
# CREATE-ONLY: adds ONE board and can never touch an existing one — the write surface behind the MCP
# `add_analysis_board` tool (docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 3). Deliberately distinct from
# the autosave above, exactly as /api/chains/create is distinct from /api/chains/save: allow-listing the
# autosave would have let a caller replace every board in the project with one request, and the server
# could not have validated a single field of it.
#
# 409 on a duplicate name, 422 on a spec the project cannot plot (unknown plot id, a chart that spec
# doesn't offer, a population that doesn't exist) — rejected BEFORE writing, because a bad `tkey`
# renders an empty panel with no error at all. The expansion and every check live in the package
# (`expand_board`), so they are headless-testable and identical from the REPL.
function api_boards_add(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    proj = try load_project(uid) catch e
        return 404, JSON3.write((; error="Could not load project: $(sprint(showerror, e))"))
    end
    # Normalised on the way IN, so the duplicate check below, the stored tab and the response all talk
    # about the same string. `append_board` normalises again (it owns the invariant, and the REPL reaches
    # it without this route); `board_display_name` is idempotent, so doing both costs nothing. Without
    # this, an agent that HTML-escaped an ampersand got a tab titled "Behaviour &amp; tracking" that it
    # could not rename — add_analysis_board is add-only.
    name = board_display_name(String(get(body, :name, "")))
    plots = get(body, :plots, nothing)
    template = String(get(body, :template, ""))
    path = boards_doc_path(joinpath(projects_dir(), uid))
    try
        doc = read_boards_doc(path)
        doc.present && !doc.readable &&
            return 409, JSON3.write((; error="The project's boards file could not be read; not adding to it"))
        # Name collision is 409 (a conflict with existing state) rather than 422 (a bad spec), and is
        # checked before expanding so the caller is told the cheap thing first. `append_board` asserts
        # it again — it is the invariant's owner, and the REPL reaches it without this route.
        if any(t -> t isa AbstractDict &&
                    strip(string(get(t, :name, get(t, "name", "")))) == strip(name), doc.tabs)
            return 409, JSON3.write((; error="A board named \"$(strip(name))\" already exists in this project",
                                       code="duplicate_board_name"))
        end
        layout = expand_board(proj, name, plots; template = template)
        updated, id = append_board(doc, name, layout)
        version = write_boards_doc(path, updated; version = doc.version + 1)
        broadcast_ws(Dict{String,Any}("type" => "boards:changed", "projectUid" => uid, "version" => version))
        return 200, JSON3.write((; ok=true, tabId=id, name=strip(name), version,
                                   slots=length(layout["contents"])))
    catch e
        e isa BoardSpecError && return 422, JSON3.write((; error=e.msg, code="invalid_board_spec"))
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# GET /api/projects/boards?projectUid — the boards document on its own, with its `version`.
# The cheap read behind both recovery paths: a 409'd autosave reloading before it retries, and a client
# reacting to the `boards:changed` broadcast. Project OPEN still gets boards inline in
# api_projects_load — this exists so neither of those has to re-run a whole project load.
function api_projects_boards_get(req::HTTP.Request)
    uid = String(get(HTTP.queryparams(HTTP.URI(req.target)), "projectUid", ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    200, JSON3.write((; boards=boards_doc_payload(read_boards_doc(boards_doc_path(joinpath(projects_dir(), uid))))))
end

# POST /api/projects/animations  { projectUid, animations }
# Debounced AUTOSAVE of the Animation page's captured view snapshots → settings/animations.json. The
# frame PNGs are sidecar files (board-assets/, shared with the board strip), so this JSON stays small.
# Mirrors api_projects_boards. Opaque frontend JSON, stored verbatim.
function api_projects_animations(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    animations = get(body, :animations, nothing)
    if animations !== nothing
        try
            settings = _settings_dir_for_project(uid); mkpath(settings)
            write_json_atomic(joinpath(settings, "animations.json"), animations)
        catch e
            return 500, JSON3.write((; error=sprint(showerror, e)))
        end
    end
    200, JSON3.write((; ok=true))
end

# POST /api/board-assets/save  { projectUid, png(base64) }  → { assetId }
# Write a board image to a sidecar PNG (settings/board-assets/<id>.png) and return its id. Used to
# MIGRATE legacy boards that still carry inline base64 in a cell's `src` into a sidecar on first load.
# (Fresh captures are saved directly by the screenshot endpoint — no base64 round-trip.)
function api_board_asset_save(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); png = String(get(body, :png, ""))
    (isempty(uid) || isempty(png)) && return 400, JSON3.write((; error="projectUid and png required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    b64 = replace(png, r"^data:image/[^;]+;base64," => "")   # tolerate a data-URL prefix
    bytes = try Base64.base64decode(b64) catch
        return 400, JSON3.write((; error="Invalid base64 png"))
    end
    try
        dir = _board_assets_dir(uid); mkpath(dir); id = gen_uid()
        write(joinpath(dir, id * ".png"), bytes)
        return 200, JSON3.write((; assetId = id))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/board-assets/delete  { projectUid, assetId }  → { ok }
# Best-effort removal of a sidecar board image (when a frame/board is deleted). Missing file is fine.
function api_board_asset_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); aid = String(get(body, :assetId, ""))
    (isempty(uid) || isempty(aid)) && return 400, JSON3.write((; error="projectUid and assetId required"))
    _valid_asset_id(aid) || return 400, JSON3.write((; error="Invalid assetId"))
    f = joinpath(_board_assets_dir(uid), aid * ".png")
    isfile(f) && rm(f; force=true)
    200, JSON3.write((; ok=true))
end

# POST /api/board-assets/copy  { projectUid, assetId }  → { assetId }
# Duplicate a sidecar board image to a NEW id — so a duplicated board owns independent asset files
# (deleting a frame in one board must not orphan the copy that shares it). Missing source → 404.
function api_board_asset_copy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, "")); aid = String(get(body, :assetId, ""))
    (isempty(uid) || isempty(aid)) && return 400, JSON3.write((; error="projectUid and assetId required"))
    _valid_asset_id(aid) || return 400, JSON3.write((; error="Invalid assetId"))
    src = joinpath(_board_assets_dir(uid), aid * ".png")
    isfile(src) || return 404, JSON3.write((; error="Asset not found: $aid"))
    try
        dir = _board_assets_dir(uid); mkpath(dir); id = gen_uid()
        cp(src, joinpath(dir, id * ".png"); force=true)
        return 200, JSON3.write((; assetId = id))
    catch e
        return 500, JSON3.write((; error=sprint(showerror, e)))
    end
end

# POST /api/projects/canvases  { projectUid, objects: { <objUid>: {entries, geom} } }
# Autosaved module-page canvas layouts, written PER OBJECT to 1/{objUid}/moduleCanvases.json (the
# object = the image or set the canvas is scoped to; frontend groups by canvas key). Stored with the
# object → survives with it and is removed when it's deleted; the debounced autosave rewrites only the
# object(s) that changed, never a global blob. Opaque frontend JSON, stored verbatim.
function api_projects_canvases(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :projectUid, ""))
    isempty(uid) && return 400, JSON3.write((; error="projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error="Project not found: $uid"))
    objects = get(body, :objects, nothing)
    if objects !== nothing
        for (objUid, data) in pairs(objects)
            objdir = joinpath(projects_dir(), uid, "1", String(objUid))
            isdir(objdir) || continue   # object deleted/unknown → skip (no stray files)
            try
                write_json_atomic(joinpath(objdir, "moduleCanvases.json"), data)
            catch e
                @warn "Could not save module canvases" uid obj=String(objUid) exception=e
            end
        end
    end
    200, JSON3.write((; ok=true))
end

# POST /api/images/value-name-check  { projectUid, valueName, imageUids: [...] }
# Partition images by whether they carry the labelProps value_name (segmentation) — a generic
# building block for any feature that must skip images lacking a value_name (e.g. copy gating across
# images). Just a value_name-presence check per image (img_has_value_name); returns {available, missing}.
function api_images_value_name_check(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    proj = String(get(body, :projectUid, ""))
    vn   = String(get(body, :valueName, ""))
    uids = get(body, :imageUids, nothing)
    (uids isa AbstractVector) || return 400, JSON3.write((; error="imageUids required"))
    isdir(joinpath(projects_dir(), proj)) || return 404, JSON3.write((; error="Project not found: $proj"))
    available = String[]; missing = String[]
    for u in uids
        uid = String(u)
        ok = try
            img = init_object(proj, uid)
            img isa CciaImage && img_has_value_name(img, vn)
        catch; false end
        ok ? push!(available, uid) : push!(missing, uid)
    end
    200, JSON3.write((; available, missing))
end

function api_projects_rename(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid  = String(get(body, :uid,  ""))
    name = String(strip(String(get(body, :name, ""))))
    isempty(uid)  && return 400, JSON3.write((; error="uid required"))
    isempty(name) && return 400, JSON3.write((; error="name required"))
    proj_dir = joinpath(projects_dir(), uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))

    meta_file = joinpath(proj_dir, "project.json")
    try
        raw = read_ccid_raw(meta_file)
        raw["name"] = name
        write_json_atomic(meta_file, raw)
    catch
        return 500, JSON3.write((; error="Failed to write project metadata"))
    end
    200, JSON3.write((; ok=true, name))
end

# delete → permanently remove a project directory from disk. Body {uid}. The frontend guards against
# deleting the currently-open project; this is the raw removal (the recent list is a scan of
# projects_dir, so it refreshes automatically). Destructive + irreversible.
function api_projects_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    uid = String(get(body, :uid, ""))
    isempty(uid) && return 400, JSON3.write((; error="uid required"))
    proj_dir = joinpath(projects_dir(), uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found"))
    try
        rm(proj_dir; recursive=true, force=true)
    catch e
        return 500, JSON3.write((; error="Failed to delete project: " * sprint(showerror, e)))
    end
    200, JSON3.write((; ok=true, uid))
end

# ── Set management ────────────────────────────────────────────────────────────

function api_sets_create(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    name        = String(get(body, :name, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(name)        && return 400, JSON3.write((; error="name required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))

    proj = load_project(project_uid)
    s    = add_set!(proj; name=name)
    @info "Created set" name uid=s.uid project=project_uid
    200, JSON3.write((; uid=s.uid, name))
end

function api_sets_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    idx  = findfirst(s -> s.uid == set_uid, proj._sets)
    n    = isnothing(idx) ? 0 : length(proj._sets[idx].image_uids)
    delete_set!(proj, set_uid)

    @info "Deleted set" uid=set_uid project=project_uid images=n
    200, JSON3.write((; ok=true))
end

# ── Image management ──────────────────────────────────────────────────────────

function api_images_register(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    filepaths   = [String(p) for p in get(body, :filepaths, [])]

    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(filepaths)   && return 400, JSON3.write((; error="filepaths required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    set_ = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(set_) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[set_]

    registered = Dict{String,Any}[]
    for filepath in filepaths
        abs_path = isabspath(filepath) ? filepath : joinpath(FS_ROOT, filepath)
        isfile(abs_path) || begin; @warn "Skipping missing file" path=abs_path; continue; end

        task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
        img = add_image!(s; name=splitext(basename(abs_path))[1],
                         meta=Dict{String,Any}("ori_path" => abs_path))
        for subdir in values(task_dirs)
            mkpath(joinpath(proj_dir, "1", img.uid, string(subdir)))
        end

        push!(registered, Dict{String,Any}(
            "uid"       => img.uid,
            "name"      => img.name,
            "status"    => "pending",
            "filepath"  => abs_path,            # SOURCE path, for display only (not the converted zarr)
            # No versioned `filepaths` yet — the OME-ZARR doesn't exist until the import task converts it.
            # (Faking `{default: …}` here made a pending row look "imported" — see isImported / the crop
            # + open gates. The conversion task writes the real versioned filepath.)
            "filepaths" => Dict{String,Any}(),
        ))
    end

    @info "Registered images" count=length(registered) set=set_uid
    200, JSON3.write((; images=registered))
end

# POST /api/import/scan-legacy {sourceProjectDir, rscript?, imageUids?} → read-only preview manifest
# of a legacy R/Shiny cecelia project (what will/won't transfer per image). See
# app/src/tasks/importImages/scan_legacy_run.py and docs/todo/LEGACY_MIGRATION_PLAN.md.
function api_import_scan_legacy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    src = String(get(body, :sourceProjectDir, ""))
    isempty(src) && return 400, JSON3.write((; error="sourceProjectDir required"))
    abs_src = isabspath(src) ? src : joinpath(FS_ROOT, src)
    isdir(joinpath(abs_src, "ANALYSIS")) ||
        return 400, JSON3.write((; error="Not a legacy cecelia project (no ANALYSIS/ dir): $abs_src"))

    run_dir     = mktempdir()
    result_file = joinpath(run_dir, "scan.result.json")
    params = Dict{String,Any}("sourceProjectDir" => abs_src, "resultPath" => result_file,
                              "rscript" => String(get(body, :rscript, "Rscript")))
    haskey(body, :imageUids) && (params["imageUids"] = [String(u) for u in body.imageUids])
    logs = String[]
    ok = try
        Cecelia.run_py("tasks/importImages/scan_legacy_run.py", params, run_dir; on_log = l -> push!(logs, l))
    catch e
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="scan failed: $(sprint(showerror, e))"))
    end
    if !(ok && isfile(result_file))
        tail = isempty(logs) ? "no output (is Rscript available? try the Rscript path option)" :
               join(last(logs, 8), " | ")
        rm(run_dir; recursive=true, force=true)
        return 500, JSON3.write((; error="Scan failed: $tail"))
    end
    manifest = read(result_file, String)
    rm(run_dir; recursive=true, force=true)
    200, manifest   # already JSON
end

# POST /api/import/register-legacy {projectUid, setUid, sourceProjectDir, images:[{uid,name,kind}]}
# Registers a placeholder image per legacy image, PRESERVING its UID and stashing the source in meta,
# so the per-image importImages.migrateLegacy task can run. Mirrors api_images_register.
function api_import_register_legacy(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body")) end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    src         = String(get(body, :sourceProjectDir, ""))
    rsc         = String(get(body, :rscript, ""))
    imgs_in     = get(body, :images, [])
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(src)         && return 400, JSON3.write((; error="sourceProjectDir required"))
    abs_src = isabspath(src) ? src : joinpath(FS_ROOT, src)

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    proj = load_project(project_uid)
    si   = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(si) && return 404, JSON3.write((; error="Set not found in project: $set_uid"))
    s = proj._sets[si]

    task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    registered = Dict{String,Any}[]
    for im in imgs_in
        uid  = String(get(im, :uid, ""))
        isempty(uid) && continue
        name = String(get(im, :name, uid))
        # Legacy `kind` on the R side (static/live/flow) is intentionally dropped — the new app
        # gates per-image on axes (Cecelia.task_applies), not project-wide.
        meta = Dict{String,Any}("legacySourceDir" => abs_src, "legacySourceUid" => uid)
        isempty(rsc) || (meta["legacyRscript"] = rsc)
        img = add_image!(s; name=name, uid=uid, meta=meta)
        for subdir in values(task_dirs)
            mkpath(joinpath(proj_dir, "1", img.uid, string(subdir)))
        end
        push!(registered, Dict{String,Any}(
            "uid" => img.uid, "name" => img.name, "status" => "pending"))
    end
    @info "Registered legacy images" count=length(registered) set=set_uid
    200, JSON3.write((; images=registered))
end

function api_images_meta(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(state_file(proj_dir, image_uid)) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    obj = init_object(project_uid, image_uid)
    obj isa CciaImage || return 404, JSON3.write((; error="Not an image: $image_uid"))
    200, JSON3.write((; image=_image_payload(obj)))
end

# GET /api/images?projectUid → a read-only listing of the project's sets + images (uid, name,
# per-image status). Unlike POST /api/projects/load this has NO side effects (load bumps
# lastOpenedAt), so the MCP observer can enumerate images while keeping its no-mutation guarantee.
# Backs the observer's get_project_info + list_images tools.
function api_images_list(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    sets = [(; uid=s.uid, name=s.name, imageCount=length(s.image_uids)) for s in proj._sets]
    imgs = Vector{Any}()
    # `attr` is the per-image ASSIGNMENT (Mouse => "3"), distinct from GET /api/plots/attrs, which is
    # the set's attribute AXES (name + distinct values) and stays the one discovery route. Both are
    # needed to choose a cross-image plot: the axes say what you may group by, the assignment says how
    # many images land in each group — see docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
    for s in proj._sets, img in images(s)
        push!(imgs, (; uid=img.uid, name=img.name, status=img.status,
                       included=image_included(img), setUid=s.uid, setName=s.name,
                       attr=Dict(string(k) => string(v) for (k, v) in img.attr)))
    end
    200, JSON3.write((; projectUid=project_uid, name=proj.name,
                        count=length(imgs), sets, images=imgs))
end

# GET /api/images/tasklog?projectUid&imageUid&fun → the raw task log for one fun on one image.
# Reads {img._dir}/logs/{fun}.log (written by _wrap_log_with_file in the scheduler). Read-only;
# backs the MCP observer's get_task_log tool. Returns exists=false + "" when no log exists yet.
# The per-image task log is CUMULATIVE: one file per (image, fun_name), appended by every run, with each
# line stamped in LOCAL time by `_wrap_log_with_file`. A caller that wants ONE run's output — the GUI
# backfilling the log of a task that was already running when the tab connected — passes that task's
# `started_at`, and the slice happens HERE: this is the process whose clock wrote the stamps, so it is the
# only place where the local/UTC comparison has a single answer.
#
# Julia's stdlib carries no timezone database (and TimeZones.jl is not a dependency), so the offset is
# taken as `now() - now(UTC)`, rounded to the minute. That is the CURRENT offset, so a run that straddled a
# DST change could be sliced up to an hour off — acceptable for a live task's log, which is what this is for.
_tasklog_local_offset() = round(Dates.now() - Dates.now(UTC), Dates.Minute)

const _TASKLOG_STAMP = r"^\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\]"

"""
    _tasklog_since(content, since) -> String

Keep only the lines a run wrote, given its ISO-8601 UTC start (`TASK_TS_FORMAT`).

An UNSTAMPED line inherits the previous line's fate rather than being dropped on its own, so a log line
that itself contained a newline survives intact. An unparseable `since` returns the content untouched — a
backfill showing too much beats one showing nothing.
"""
function _tasklog_since(content::AbstractString, since::AbstractString)
    t0 = try
        DateTime(String(since), TASK_TS_FORMAT) + _tasklog_local_offset()
    catch
        return String(content)
    end
    out  = IOBuffer()
    keep = false
    for line in split(String(content), '\n'; keepempty=true)
        m = match(_TASKLOG_STAMP, line)
        if !isnothing(m)
            ts   = try DateTime(m.captures[1], dateformat"yyyy-mm-dd HH:MM:SS") catch; nothing end
            keep = isnothing(ts) ? keep : ts >= t0
        end
        keep && println(out, line)
    end
    String(take!(out))
end

function api_images_tasklog(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    fun         = get(query, "fun", "")
    since       = get(query, "since", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(fun)         && return 400, JSON3.write((; error="fun required"))
    # fun becomes a filename ({fun}.log) — reject separators / traversal so it can't escape logs/
    (occursin('/', fun) || occursin('\\', fun) || occursin("..", fun)) &&
        return 400, JSON3.write((; error="invalid fun"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))
    img_dir = joinpath(proj_dir, "1", image_uid)
    isfile(state_file(img_dir)) ||
        return 404, JSON3.write((; error="Image not found: $image_uid"))

    logfile = joinpath(img_dir, "logs", fun * ".log")
    isfile(logfile) || return 200, JSON3.write((; projectUid=project_uid, imageUid=image_uid,
                                                  fun, exists=false, content=""))
    content = read(logfile, String)
    isempty(since) || (content = _tasklog_since(content, since))
    200, JSON3.write((; projectUid=project_uid, imageUid=image_uid, fun,
                        exists=true, content, bytes=sizeof(content)))
end

# GET /api/tasks/history?projectUid[&limit] → recent task runs across all images, newest first.
# Aggregates each image's runlog.json (fun, valueName, timestamp) plus the image's current status.
# Read-only; backs the MCP observer's get_task_history tool. (Attempt counts arrive with the
# per-node counter in a later slice.) limit caps the returned rows (default 100).
function api_tasks_history(req::HTTP.Request)
    uri   = HTTP.URI(req.target)
    query = HTTP.queryparams(uri)
    project_uid = get(query, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    parsed = tryparse(Int, get(query, "limit", ""))
    limit  = (isnothing(parsed) || parsed <= 0) ? 100 : parsed
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    # run-log entries may deserialise with String or Symbol keys depending on the JSON3 path — try both
    _rl(e, k) = (v = get(e, k, get(e, Symbol(k), nothing)); v === nothing ? "" : String(v))
    rows = Vector{Any}()
    for img in images(proj), e in read_run_log(img)
        rs = _rl(e, "status")                        # per-RUN outcome; legacy entries have none → "done"
        push!(rows, Dict{String,Any}(
            "imageUid" => img.uid, "imageName" => img.name, "status" => img.status,  # image's status
            "runStatus" => (isempty(rs) ? "done" : rs),                              # this run's outcome
            "fun" => _rl(e, "fun"), "valueName" => _rl(e, "valueName"), "at" => _rl(e, "at"),
            # the tuning trail: the params this run used (run_log.jl; {} on legacy entries). Lets the
            # observer suggest a param adjustment on an outlier without a second per-image call.
            "params" => get(e, :params, get(e, "params", Dict{String,Any}()))))
    end
    # newest first — the run-log timestamp is yyyy-mm-ddTHH:MM:SS, so lexicographic == chronological
    sort!(rows, by = r -> r["at"], rev = true)
    length(rows) > limit && (rows = rows[1:limit])
    200, JSON3.write((; projectUid=project_uid, count=length(rows), history=rows))
end

# GET /api/qc/cohort?projectUid&setUid&funName[&valueName][&threshold]
# Recompute the cohort QC summary for one (task, output) across a set's included images and return
# it (also writes the sidecar). `threshold` is the robust modified-z cutoff (default 3.5). Feeds the
# MCP get_cohort_qc tool + the morning summary.
function api_qc_cohort(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    fun_name    = get(q, "funName", "")
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    haskey(COHORT_METRICS, fun_name) ||
        return 400, JSON3.write((; error = "No cohort metrics for fun '$fun_name'",
                                   known = sort(collect(keys(COHORT_METRICS)))))
    vn_param = get(q, "valueName", "")
    run_param = get(q, "run", "")   # clustering: restrict to one run's value_names (see cohort_runs)
    thr = something(tryparse(Float64, get(q, "threshold", "")), Cecelia._COHORT_MODZ_THRESHOLD)
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    # READ-ONLY: compute + return, write nothing (a GET must be safe). The write path — set sidecar +
    # per-image cohort findings — is the explicit POST /api/qc/cohort/check below.
    # No valueName → discover every value_name this fun banked and return per-value_name cohorts (a
    # `byValueName` map): clustering banks per label set (T/B), segment/tracking under "default", so a
    # caller that doesn't know the suffix still gets all cohorts. An explicit valueName returns just that
    # one cohort (single doc, backward-compatible).
    if isempty(vn_param)
        byval = try
            cohort_qc_for_all(set, fun_name; threshold = thr, run = run_param)
        catch e
            return 500, JSON3.write((; error = sprint(showerror, e)))
        end
        return 200, JSON3.write((; funName = fun_name, valueNames = sort(collect(keys(byval))), byValueName = byval))
    end
    doc = try
        cohort_qc_for(set, fun_name, vn_param; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(doc)
end

# GET /api/qc/cohort/runs?projectUid&setUid&funName — the distinct clustering RUNS a fun banked across
# the set (cheap: scans QC filenames + reads each doc's runSuffix, no cohort math). Powers the Check-
# cohort button's run selector: cluster QC is banked per run, so the user picks WHICH run to check
# rather than the button re-checking every past iteration. `[]` for funs that keep no runs (segment/
# tracking/HMM) — the button then shows no selector and checks as before. Newest run first.
function api_qc_cohort_runs(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    fun_name    = get(q, "funName", "")
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    runs = try
        [(; run = r.run, valueNames = r.valueNames) for r in cohort_runs(set, fun_name)]
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write((; funName = fun_name, runs = runs))
end

# Shared GET handler for the observer's project-scoped summary routes (analysis/*): parse projectUid +
# optional image/set scope, load the project (404), run `build(proj, image_uid, set_uid)` (500), return
# JSON. Each route is then a one-liner over its builder — the same consolidation as the Julia
# `observer_image_summary` scaffold and the MCP `_analysis_summary` client helper.
function _observer_summary_route(req::HTTP.Request, build::Function)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    out = try
        build(proj, get(q, "imageUid", ""), get(q, "setUid", ""))
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# GET /api/analysis/lineage — synthesized pipeline (steps + seg/track/cluster/gating links, chains,
# boards, rollup). GET /api/analysis/populations — the gate/filter DEFINITIONS behind lineage's
# gatedPops. Both READ-ONLY, summary-level. See analysis_lineage / populations_summary and Slices A/B
# of OBSERVER_DATA_ACCESS_PLAN.
api_analysis_lineage(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> analysis_lineage(p; image_uid = i, set_uid = s))
api_analysis_populations(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> populations_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/measures — per-population phenotype + motility summaries (gated pops, else the base
# tracked/all-cells pop). Heavier (touches cell data via pop_df); prefer image/set scope. Slice C.
api_analysis_measures(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> measure_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/behaviour — HMM state distribution + transition counts. GET /api/analysis/clusters —
# per clustering run: n clusters, sizes, largest fraction, features. Both read obs via pop_df. Slice D.
api_analysis_behaviour(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> behaviour_summary(p; image_uid = i, set_uid = s))
api_analysis_clusters(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> cluster_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/spatial — per image, region-clustering runs (regions.{suffix}) + pairwise cell-type
# contact log-odds (neighbourStats sidecars). Flat + interpretable for MCP.
api_analysis_spatial(req::HTTP.Request) =
    _observer_summary_route(req, (p, i, s) -> spatial_summary(p; image_uid = i, set_uid = s))
# GET /api/analysis/chains — the project's whiteboard chains: wired templates (node DAG) + recent runs.
# Project-level (ignores image/set scope). Slice E.
api_analysis_chains(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> chains_summary(p))
# GET /api/analysis/boards — the Analysis boards a project already has and WHAT THEY SHOW (a summary,
# never the stored layout geometry). Lineage's `boards` is tab names only; this is the plot detail, so
# the observer can see an existing board instead of proposing a duplicate. Project-level, read-only.
# See board_summaries + docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
api_analysis_boards(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> board_summaries(p))

# GET /api/observer/briefing?projectUid — the observer SESSION BRIEFING (Observer Phase 2 §2): a small
# startup context (project name + image count, flagged images, recent lab log) a fresh Chat-to-Claude
# session pulls first so the user need not re-explain. Project-level, READ-ONLY. Backs get_session_briefing.
api_observer_briefing(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> session_briefing(p))

# GET /api/mcp/connections — every MCP server registered in the user's Claude config, for the Settings
# "MCP connections" panel. Machine-level (no project), READ-ONLY. Generic: it enumerates what's there,
# so a connector added later needs no change here. It cannot see claude.ai ACCOUNT connectors (e.g.
# LabArchives) — absence from this list is NOT evidence of "disconnected"; see `mcp_connections`.
function api_mcp_connections(::HTTP.Request)
    out = try
        (; configPath = Cecelia.claude_config_path(),
           present = isfile(Cecelia.claude_config_path()),
           connections = mcp_connections())
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# GET /api/observer/labarchives?projectUid — the FULL LabArchives context sidecar + derived gaps (the
# briefing carries only headings). Read-only; see app/src/ai/labarchives.jl.
api_observer_labarchives(req::HTTP.Request) =
    _observer_summary_route(req, (p, _i, _s) -> merge(read_la_doc(p),
                                                      Dict{String,Any}("gaps" => la_gaps(p))))

# PUT /api/observer/labarchives — REPLACE the sidecar. Body {projectUid, source, sections, cohort,
# syncedBy}. The one write; cecelia never fetches from LabArchives itself (no credentials, by design —
# the connector lives in the user's Claude session), so this is how the context arrives.
function api_observer_labarchives_set(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    proj = try load_project(project_uid) catch e
        return 404, JSON3.write((; error=sprint(showerror, e)))
    end
    doc = try
        write_la_doc!(proj;
                      source     = json_native(get(body, :source, Dict{String,Any}())),
                      sections   = json_native(get(body, :sections, Any[])),
                      cohort     = json_native(get(body, :cohort, Any[])),
                      synced_by  = String(get(body, :syncedBy, "claude")))
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end
    # Same panel-reload signal the lab log uses — the context card sits in that panel, and an external
    # Chat-to-Claude session writes here with no frontend action at all.
    broadcast_ws(Dict{String,Any}("type" => "lab_log_updated", "projectUid" => project_uid))
    200, JSON3.write(merge(doc, Dict{String,Any}("gaps" => la_gaps(proj, doc))))
end

# GET /api/repl/api — the notebook/REPL data-access surface (Observer Phase 2 foundation): the
# NOTEBOOK_API accessors with their live docstrings, plus the docs/REPL.md cookbook when present. Backs
# the MCP get_repl_api tool so Claude can generate correct `using Cecelia` notebooks without guessing
# the interface. Project-independent, read-only. `doc` is "" if REPL.md isn't shipped (installed app).
function api_repl_api(::HTTP.Request)
    out = try
        api = [(; name = e.name, exported = e.exported, documented = e.documented, doc = e.doc)
               for e in repl_api_reference()]
        p = Cecelia.repl_doc_path()
        (; api = api, doc = isfile(p) ? read(p, String) : "")
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    200, JSON3.write(out)
end

# POST /api/qc/cohort/check — the explicit "Check cohort consistency" action: recompute AND persist
# (set sidecar + per-image `cohort.{fun}` findings so outliers surface on the image). Body:
# {projectUid, setUid, funName, valueName?, threshold?}. This is the ONLY cohort write path.
function api_qc_cohort_check(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, "")); set_uid = String(get(body, :setUid, ""))
    fun_name    = String(get(body, :funName, ""))
    (isempty(project_uid) || isempty(set_uid) || isempty(fun_name)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and funName required"))
    haskey(COHORT_METRICS, fun_name) ||
        return 400, JSON3.write((; error = "No cohort metrics for fun '$fun_name'",
                                   known = sort(collect(keys(COHORT_METRICS)))))
    vn_param = String(get(body, :valueName, ""))
    run_param = String(get(body, :run, ""))   # clustering: check only this run's value_names (see cohort_runs)
    tv  = get(body, :threshold, nothing)
    thr = tv isa Real ? Float64(tv) : Cecelia._COHORT_MODZ_THRESHOLD
    set = try
        obj = init_object(project_uid, set_uid)
        obj isa CciaSet || error("Not a set: $set_uid")
        obj
    catch e
        return 404, JSON3.write((; error = sprint(showerror, e)))
    end
    # Cecelia authors a "Cohort check" lab-log entry ONLY for docs that flagged (an all-clear would be
    # noise). This is the cross-image analysis — image UIDs (stable; the panel resolves uid→name on
    # demand), the metric, its value vs the cohort median — the durable record the amber button points
    # at (no toast). Best-effort; a lab-log hiccup never fails the check. Author "Cecelia — …" so the
    # append route treats it as a Cecelia entry.
    log_flagged(docs) = begin
        flagged = [d for d in docs if d isa AbstractDict && Cecelia.cohort_has_outliers(d)]
        isempty(flagged) && return
        try
            proj = load_project(project_uid)
            for d in flagged
                Cecelia.append_lab_log!(proj, "Cecelia — Cohort check",
                                        Cecelia.cohort_qc_summary_lines(d))
            end
        catch e
            @warn "cohort check: lab-log append failed" exception = e
        end
    end
    # No valueName → check EVERY value_name the fun banked (per label set); else just the one.
    if isempty(vn_param)
        byval = try
            cohort_qc_for_all!(set, fun_name; threshold = thr, run = run_param)
        catch e
            return 500, JSON3.write((; error = sprint(showerror, e)))
        end
        log_flagged(collect(values(byval)))
        return 200, JSON3.write((; funName = fun_name, valueNames = sort(collect(keys(byval))), byValueName = byval))
    end
    doc = try
        cohort_qc_for!(set, fun_name, vn_param; threshold = thr)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    log_flagged([doc])
    200, JSON3.write(doc)
end

function api_images_delete(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    set_uid     = String(get(body, :setUid, ""))
    image_uid   = String(get(body, :imageUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found: $project_uid"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    s = init_object(project_uid, set_uid)
    s isa CciaSet || return 404, JSON3.write((; error="Not a set: $set_uid"))
    delete_image!(s, image_uid)

    @info "Deleted image" uid=image_uid set=set_uid project=project_uid
    200, JSON3.write((; ok=true))
end

# POST /api/images/move {projectUid, imageUid, fromSetUid, toSetUid?|newSetName?}
# Move an image to another set in the same project. Provide EITHER an existing `toSetUid` OR a
# `newSetName` to create the destination set on the fly. Manifest-only — no image data moves on
# disk (see move_image!). Returns the resolved destination {toSetUid, toSetName, createdSet}.
function api_images_move(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid  = String(get(body, :projectUid, ""))
    image_uid    = String(get(body, :imageUid, ""))
    from_set_uid = String(get(body, :fromSetUid, ""))
    to_set_uid   = String(get(body, :toSetUid, ""))
    new_set_name = strip(String(get(body, :newSetName, "")))
    isempty(project_uid)  && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)    && return 400, JSON3.write((; error="imageUid required"))
    isempty(from_set_uid) && return 400, JSON3.write((; error="fromSetUid required"))
    (isempty(to_set_uid) && isempty(new_set_name)) &&
        return 400, JSON3.write((; error="toSetUid or newSetName required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))

    proj = load_project(project_uid)

    # resolve (or create) the destination set
    created = false
    to_name = ""
    if isempty(to_set_uid)
        existing = findfirst(s -> s.name == new_set_name, proj._sets)
        if isnothing(existing)
            s = add_set!(proj; name=String(new_set_name))
            to_set_uid = s.uid; to_name = s.name; created = true
        else
            s = proj._sets[existing]
            to_set_uid = s.uid; to_name = s.name
        end
    else
        ti = findfirst(s -> s.uid == to_set_uid, proj._sets)
        isnothing(ti) && return 404, JSON3.write((; error="Destination set not found: $to_set_uid"))
        to_name = proj._sets[ti].name
    end

    try
        move_image!(proj, image_uid, from_set_uid, to_set_uid)
    catch e
        return 400, JSON3.write((; error=sprint(showerror, e)))
    end

    @info "Moved image" uid=image_uid from=from_set_uid to=to_set_uid project=project_uid createdSet=created
    200, JSON3.write((; ok=true, toSetUid=to_set_uid, toSetName=to_name, createdSet=created))
end

# POST /api/images/version/remove {projectUid, imageUid, valueName, newDefault}
# Delete ONE image version's store and clear its ccid.json entry, re-pointing `_active` at
# `newDefault`. A thin adapter over `remove_image_version!` (app/src/storage.jl) — the same core the
# `importImages.remove` task and the storage reclaim use, so there is one deletion path, not three.
# The caller loops for several versions and must order `default` LAST (docs/todo/IMAGE_DELETE_PLAN.md
# Decision 11), so the safe-primary un-import lands at the end rather than mid-loop.
function api_images_version_remove(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    image_uid   = String(get(body, :imageUid,   ""))
    value_name  = String(get(body, :valueName,  ""))
    new_default = String(get(body, :newDefault, VERSIONED_DEFAULT_VAL))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error="imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error="valueName required"))

    isdir(joinpath(projects_dir(), project_uid)) ||
        return 404, JSON3.write((; error="Project not found: $project_uid"))
    img = init_object(project_uid, image_uid)
    img isa CciaImage || return 404, JSON3.write((; error="Image not found: $image_uid"))

    res = remove_image_version!(img, value_name, new_default)
    isnothing(res) && return 404, JSON3.write((; error="No version '$value_name' on this image"))
    freed, cleared = res

    fresh = init_object(project_uid, image_uid)
    @info "Removed image version" value_name new_default image=image_uid project=project_uid freed
    200, JSON3.write((; ok=true, freedBytes=freed, cleared=cleared,
                        image = fresh isa CciaImage ? _image_payload(fresh) : nothing))
end

# POST /api/images/analysis/reset {projectUid, imageUids: [...]}
# Drop everything DERIVED from each image, keeping the image itself: every child of `1/{uid}` except
# the keep-list, plus the `labels`/`label_props`/`branch_labels` registrations. Touches no image store
# — shedding a version is /api/images/version/remove's job (IMAGE_DELETE_PLAN Decision 9). Core:
# `reset_image_analysis!` (app/src/storage.jl).
function api_images_analysis_reset(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error="Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    image_uids = get(body, :imageUids, nothing)
    (image_uids isa AbstractVector && !isempty(image_uids)) ||
        return 400, JSON3.write((; error="imageUids (non-empty) required"))
    isdir(joinpath(projects_dir(), project_uid)) ||
        return 404, JSON3.write((; error="Project not found: $project_uid"))

    freed  = 0
    images = Dict{String,Any}()
    for uid in image_uids
        img = init_object(project_uid, string(uid))
        img isa CciaImage || continue
        f, _ = reset_image_analysis!(img)
        freed += f
        fresh = init_object(project_uid, string(uid))
        fresh isa CciaImage && (images[string(uid)] = _image_payload(fresh))
    end

    @info "Reset image analysis" n=length(images) project=project_uid freed
    200, JSON3.write((; ok=true, freedBytes=freed, images=images))
end

# ── Metadata management ───────────────────────────────────────────────────────

function _parse_meta_request(body_bytes)
    data = try JSON3.read(String(body_bytes)) catch
        return nothing, nothing, "Invalid JSON body"
    end
    project_uid = String(get(data, :projectUid, ""))
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
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = _norm_attr(String(get(data, :attrName, "")))
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
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = _norm_attr(String(get(data, :attrName, "")))
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
    project_uid = String(get(data, :projectUid, ""))
    attr_name   = _norm_attr(String(get(data, :attrName, "")))
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
    project_uid = String(get(body, :projectUid, ""))
    image_uid   = String(get(body, :imageUid,   ""))
    value_name  = String(get(body, :valueName,  ""))
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
    project_uid = String(get(data, :projectUid, ""))
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
    project_uid = String(get(data, :projectUid, ""))
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
    project_uid = String(get(data, :projectUid, ""))
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
    project_uid = String(get(body, :projectUid, ""))
    entry_id    = String(get(body, :id, ""))
    dismissed   = Bool(get(body, :dismissed, false))
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
    project_uid = String(get(body, :projectUid, ""))
    author      = String(get(body, :author, ""))
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
    project_uid = String(get(body, :projectUid, ""))
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
    project_uid = String(get(data, :projectUid, ""))
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

function _meta_int(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) && return nothing
    v isa Integer ? v : tryparse(Int, string(v))
end

function _meta_float(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) && return nothing
    v isa Real ? Float64(v) : tryparse(Float64, string(v))
end

function _meta_str(meta::AbstractDict, key::String)
    v = get(meta, key, nothing)
    isnothing(v) ? nothing : string(v)
end

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
        status          = img.status,
        sizeC           = _meta_int(img.meta, "SizeC"),
        sizeT           = _meta_int(img.meta, "SizeT"),
        sizeZ           = _meta_int(img.meta, "SizeZ"),
        # Raw/nullable — NOT img_physical_sizes' 1.0-default-for-computation fallback. The UI
        # needs to tell "genuinely missing" apart from "explicitly confirmed 1.0".
        physicalSizeX     = _meta_float(img.meta, "PhysicalSizeX"),
        physicalSizeY     = _meta_float(img.meta, "PhysicalSizeY"),
        physicalSizeZ     = _meta_float(img.meta, "PhysicalSizeZ"),
        physicalSizeUnit  = _meta_str(img.meta, "PhysicalSizeUnit"),
        # set when the ImageJ-TIFF Z-spacing auto-fix overrode bioformats2raw's value at import
        # (see omezarr.jl) — the corrected number is still only as good as the source file's own
        # ImageJ tag, so the frontend keeps flagging it for the user to confirm, not just silently
        # trusting it because the ratio now looks plausible.
        physicalSizeZCorrected = haskey(img.meta, "PhysicalSizeZ_raw"),
        timeIncrement     = _meta_float(img.meta, "TimeIncrement"),
        timeIncrementUnit = _meta_str(img.meta, "TimeIncrementUnit"),
        channelNames    = isnothing(ch) ? String[] : ch,
        # Original source file location (before OME-Zarr conversion), kept in meta as `ori_path`.
        # The image-info dialog surfaces it so users can trace a converted image back to its raw file.
        oriPath         = _meta_str(img.meta, "ori_path"),
        # Any other meta the dialog can show generically (see _extra_meta) — empty for most images.
        extraMeta       = _extra_meta(img.meta),
        filepath        = active_fn,
        activeValueName = active_vn,
        filepaths       = fps,
        labels          = img.labels,
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
        runLog          = read_run_log(img),
    )
end

_set_payload(s::CciaSet) = (; uid=s.uid, name=s.name,
                              images=[_image_payload(i) for i in s._images])
