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


include(joinpath(@__DIR__, "routes", "chain.jl"))

# ── Task definitions (single-source from package JSON specs) ──────────────────

const _TASK_SPECS_ROOT = joinpath(@__DIR__, "..", "..", "app", "src", "tasks")

# The task form's CURRENT values, for options that depend on what the user just typed (see
# `_inject_dynamic_options!`'s three-argument method). Sent as one JSON blob rather than flattened into
# the query string, because param keys are the task's own and would collide with `category`/`params`.
# Malformed input yields an empty form rather than a 400: these values only ever add SUGGESTIONS, so
# losing them degrades the picker instead of failing the request that draws the whole page.
function _form_params(query::AbstractDict)::Dict{String,Any}
    raw = get(query, "params", "")
    isempty(raw) && return Dict{String,Any}()
    try
        JSON3.read(String(raw), Dict{String,Any})
    catch e
        @warn "Ignoring malformed task-form params" exception=e
        Dict{String,Any}()
    end
end

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
                # A task spec is identified by its `fun_name`, exactly as the user/plugin scan below
                # requires. Not every .json beside a task IS one: `tracking/cell_config.json` is the
                # vendored btrack TrackerConfig, and without this it was served as a task — rendering
                # a BLANK entry in the function picker that threw `def.params is undefined` the moment
                # it was selected. The two scans now agree on what a spec is.
                isempty(string(get(parsed, "fun_name", ""))) && continue
                resolved = Cecelia._resolve_spec_includes(parsed, frag_dir)
                push!(specs, resolved)
            catch e
                @warn "Skipping malformed task spec" path=f exception=e
            end
        end
        isempty(specs) || (raw[category] = specs)
    end

    # ── User drop-in modules (custom tasks) ────────────────────────────────────
    # Same directory-driven contract as the built-ins, and the SAME co-located layout, rooted at the
    # per-user config dir — in BOTH shapes:
    #   <config_dir>/modules/<category>/<name>.json                    hand-dropped
    #   <config_dir>/modules/plugins/<plugin>/<category>/<name>.json   a plugin (one directory)
    # Category = the subdir name in both, so a custom task in an existing category (e.g. behaviour/)
    # appears on that module page automatically.
    #
    # Enumeration, the legacy skip list and fun_name precedence all live in ONE place —
    # `Cecelia.user_task_specs` — shared with `_custom_module_categories` below. They each used to
    # hand-roll the same one-level `readdir`, which is how the scans drifted from the (recursive)
    # Julia loader and why a plugin could register a task that had no form. Built-ins still win, which
    # is what `exclude_funs` says. See docs/CUSTOM_MODULES.md, docs/todo/PLUGINS_PLAN.md.
    builtin_funs = Set{String}()
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        isempty(fn) || push!(builtin_funs, fn)
    end
    for e in Cecelia.user_task_specs(; category = cat, exclude_funs = builtin_funs)
        try
            push!(get!(raw, e.category, Any[]), Cecelia._resolve_spec_includes(e.spec, frag_dir))
        catch err
            @warn "Skipping malformed custom task spec" path=e.path exception=err
        end
    end

    # Runtime-enumerated options (e.g. CellposeSegment's Model picker: built-ins + files under
    # <install>/models/cellposeModels/ + <config_dir>/models/cellposeModels/) — mutate specs in
    # place via the same dispatch hook `validate_params` uses (`_inject_dynamic_options!`), so
    # picker and validation stay in sync. See docs/SEGMENTATION.md → Custom cellpose checkpoints.
    # Resolution goes through `_task_from_fun_name` — the canonical resolver, which falls back to the
    # custom/plugin registry — NOT `_fun_name_map`, which holds built-ins only. That gate was a real
    # desync: a custom or plugin task overloading these hooks DID get its options at validation time
    # (via `_task_spec`, which dispatches on the instance) but not in the served form, so the picker
    # and the validator disagreed — the one thing this block exists to prevent.
    #
    # `form` is the values currently in the open task form, for options that depend on what the user
    # just typed (an importer offering the columns of the file they picked) rather than on what is on
    # disk. Empty for a plain fetch, and ignored by every option source that doesn't need it.
    # …and the task-preview trait is stamped in the SAME pass. It is declared in Julia beside the task
    # (`task_previewable`, task.jl) rather than written into the JSON, because the JSON is the PARAM
    # spec — a capability of the compute doesn't belong in it, and duplicating it there would let the
    # two disagree. Composites resolve through their own overload, so `segment.cellposeMeasure` reports
    # true. One loop, one resolution — both the option-source pass and the task-preview stamp iterate
    # over the same `raw` spec, so a plugin task cannot be stamped by one path and missed by the other.
    form = _form_params(query)
    for specs in values(raw), spec in specs
        fn = string(get(spec, "fun_name", ""))
        isempty(fn) && continue

        # `optionsFrom` / `defaultFrom` FIRST, and above the task gate — they are declared in the spec
        # and need no task instance, so a spec whose `fun_name` has no registered Julia task (a plugin
        # whose module failed to load) still gets its vault picker filled. This route used to resolve
        # only the dispatch hook, so when the three model pickers moved to `optionsFrom` it served the
        # spec's literal options alone: the coastal Model select offered nothing but "None" while the
        # Optical Flow vault manager listed every model, both reading the same `list_coastal_models`.
        spec isa Dict{String,Any} && Cecelia.resolve_spec_sources!(spec)

        task = try Cecelia._task_from_fun_name(fn) catch; nothing end
        task === nothing && continue

        Cecelia._needs_dynamic_options(task) &&
            Cecelia._inject_dynamic_options!(spec, task, form)

        spec["previewable"] = try
            Cecelia.task_previewable(task)
        catch e
            # a task's own overload must never take the whole picker down (same guard as
            # `_live_outputs_for`): report not-previewable and carry on
            @warn "task_previewable failed" fun=fn exception=e
            false
        end

        # `outputEffect` — the on-disk artefact this task produces (new-image / new-version /
        # in-place). Declared beside the task in Julia (task_output_effect) for the same reason as
        # `previewable`: a static capability of the compute belongs with the compute, not in the
        # PARAM JSON. Stamped ONTO the spec so the module page's function picker can surface it
        # without a second endpoint. `nothing` means "don't show a line" (segment / measure / etc).
        effect = try
            Cecelia.task_output_effect(task)
        catch e
            @warn "task_output_effect failed" fun=fn exception=e
            nothing
        end
        effect === nothing || (spec["outputEffect"] = effect)
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
                # Union `requires` across sub-tasks so the frontend gate sees the composite's true
                # needs without walking steps (mirrors Cecelia.task_requires_axes /
                # task_requires_scale on the backend). A composite with its own explicit `requires`
                # still contributes. BOTH keys are collected together: the assignment below replaces
                # the whole `requires` dict, so gathering one key and not the other would silently
                # drop it.
                req_axes  = Set{String}()
                req_scale = Set{String}()
                collect_req!(req) = begin
                    req isa AbstractDict || return
                    for (key, into) in (("axes", req_axes), ("scale", req_scale))
                        for a in get(req, key, String[])
                            s = uppercase(string(a))
                            isempty(s) || push!(into, s)
                        end
                    end
                end
                collect_req!(get(spec, "requires", nothing))
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
                    collect_req!(get(sub, "requires", nothing))
                end
                spec["params"] = merged
                if !isempty(req_axes) || !isempty(req_scale)
                    req = Dict{String,Any}()
                    isempty(req_axes)  || (req["axes"]  = sort!(collect(req_axes)))
                    isempty(req_scale) || (req["scale"] = sort!(collect(req_scale)))
                    spec["requires"] = req
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
    specs = Cecelia.user_task_specs()   # both layouts, deduped by precedence — see api_task_definitions
    isempty(specs) && return Any[]
    builtin = Set(basename(e) for e in readdir(_TASK_SPECS_ROOT; join=true) if isdir(e))
    # Interactive plots a plugin asked for on its page (PLUGINS_PLAN Decision 11). The id names a
    # built-in view; the registry it names is in the frontend, so this route carries the declaration
    # and the canvas is the half that can tell whether it resolves.
    views = Cecelia.plugin_views()
    cats = Any[]
    for category in unique(String[e.category for e in specs])
        funs = String[e.fun_name for e in specs if e.category == category]
        isempty(funs) && continue
        # cohortFuns = the category's funs that bank cohort-comparable metrics (Cecelia.COHORT_METRICS,
        # populated at load incl. custom modules' register_cohort_metrics!). Drives the "Check cohort"
        # button on the generic custom page WITHOUT any hardcoded per-page list — a custom module that
        # declares its metrics gets the button automatically.
        cohort_funs = String[f for f in funs if haskey(Cecelia.COHORT_METRICS, f)]
        push!(cats, (; name = category, builtin = category ∈ builtin, funNames = funs,
                       cohortFuns = cohort_funs,
                       views = [(; v.view, v.label, v.plugin) for v in views if v.moduleName == category]))
    end
    cats
end

# `plugins` = the installed plugin sets (docs/todo/PLUGINS_PLAN.md); `clashes` = fun_names a module
# registered but did NOT get, which loading alone cannot report — the file `include`s fine, it just
# lost the name to a higher tier. Without it the task is simply missing from the UI with nothing
# saying why. The running version is passed in because `requiresCecelia` is checked here, not in the
# package (and is skipped outright for a "dev" checkout — see Cecelia.plugin_version_warning).
_custom_modules_payload() = (; dir        = Cecelia.custom_modules_dir(),
                               modules    = Cecelia.custom_modules_report(),
                               plugins    = Cecelia.plugins_report(; running_version = _running_version()),
                               clashes    = Cecelia.custom_task_clashes(),
                               registry   = Cecelia.plugin_registry_status(),
                               # Example plugins in THIS checkout (docs/examples/plugins) — installable
                               # with no network, because that copy is the SOURCE the GitHub mirror is
                               # published from. Empty in an installed app with no `docs/`.
                               bundled    = Cecelia.bundled_plugins(),
                               categories = _custom_module_categories())

function api_custom_modules_status(::HTTP.Request)
    200, JSON3.write(_custom_modules_payload())
end

function api_custom_modules_reload(::Vector{UInt8})
    res = Cecelia.load_custom_modules!()
    200, JSON3.write((; _custom_modules_payload()...,
                        loaded     = res.loaded,
                        skipped    = res.skipped,
                        removed    = res.removed,
                        failed     = [(; path = p, error = m) for (p, m) in res.failed]))
end

# ── View profiles (curated sidebar) ───────────────────────────────────────────
# GET  /api/profiles         → { dir, profiles: [{id,label,items}], errors: [{file,error}] }
# POST /api/profiles/save    → create/update one: { label, items, id? } → the stored profile
# POST /api/profiles/delete  → { id } → { deleted: Bool }
#
# A profile is a named, ordered SUBSET of sidebar routes (docs/todo/VIEW_PROFILES_PLAN.md). The server
# validates SHAPE only — it does not know the route table (that is `frontend/src/main.ts`), so an item
# pointing at a route that no longer exists is resolved and reported in the frontend against the live
# router. Files are the storage format; the GUI builder is the authoring path.
function api_view_profiles(::HTTP.Request)
    res = Cecelia.read_view_profiles()
    200, JSON3.write((; dir = res.dir, profiles = res.profiles, errors = res.errors))
end

function api_view_profile_save(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes; allow_empty = true)
    body isa Tuple && return body
    label = _wstr(body, :label)
    isempty(strip(label)) && return 400, JSON3.write((; error="label required"))
    items = get(body, :items, nothing)
    items isa AbstractVector || return 400, JSON3.write((; error="items must be an array of route paths"))
    id = get(body, :id, nothing)
    try
        prof = Cecelia.write_view_profile(label, items;
                                         id = id === nothing ? nothing : string(id))
        200, JSON3.write((; profile = prof))
    catch e
        # A bad shape is the user's typo, not a server fault — hand back the message the reader would
        # have reported, so the editor can show it inline.
        e isa ArgumentError || rethrow()
        400, JSON3.write((; error = e.msg))
    end
end

function api_view_profile_delete(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes; allow_empty = true)
    body isa Tuple && return body
    id = _wstr(body, :id)
    isempty(strip(id)) && return 400, JSON3.write((; error="profile id required"))
    try
        200, JSON3.write((; deleted = Cecelia.delete_view_profile!(id)))
    catch e
        e isa ArgumentError || rethrow()
        400, JSON3.write((; error = e.msg))
    end
end

# ── Task param memory (funParams) ─────────────────────────────────────────────
# GET /api/tasks/funparams?projectUid=&fun=&imageUid=&setUid=&valueName=
# Returns the last-used params for `fun`, resolved image → set → none (R parity). The frontend
# passes imageUid only when exactly one image is selected (else the shared set-level default).
#
# `valueName` is the OUTPUT name the form is currently naming (e.g. the label set "Tcell"). Given one,
# each level prefers what was last run under that name and falls back to that level's flat blob — so
# picking an existing output restores ITS parameters, and a new name still starts from the last run
# rather than from bare defaults. Optional: omit it and this behaves exactly as it did.
#
# `matched` says the params came from a BY-NAME record, not a fallback. The form uses it to decide
# whether to replace what the user is looking at: switching to a name with params banked for it
# should restore them, but a name with none must leave the form alone — applying the fallback there
# would discard edits the user had just made.
#
# By-name wins across BOTH levels before either flat blob is considered: a set-level record for the
# name the user is actually naming is a better answer than the image's record of some other run.
#
# `imageUids` (all selected) is asked ONLY the by-name question. `imageUid` is the single-selection
# driving image, so with several selected the flat blob correctly resolves at the set — but the by-name
# answer for a batch is on the images: a run under a name banks the same params on every image it ran
# on, and the run log that backfills names from before that record existed is per-image and has no
# set-level half. Without this, naming `Neutrophil` restored nothing whenever more than one image was
# selected, which for segmentation is the normal case. The flat-blob resolution is untouched.
function api_task_fun_params(req::HTTP.Request)
    q     = HTTP.queryparams(HTTP.URI(req.target))
    proj  = get(q, "projectUid", "")
    fun   = get(q, "fun", "")
    imgu  = get(q, "imageUid", "")
    setu  = get(q, "setUid", "")
    vname = get(q, "valueName", "")
    imgus = filter(!isempty, String.(split(get(q, "imageUids", ""), ',')))
    (isempty(proj) || isempty(fun)) &&
        return 400, JSON3.write((; error = "projectUid and fun are required"))

    proj_root = joinpath(projects_dir(), proj)
    dir(u) = joinpath(proj_root, "1", u)

    params, matched = nothing, false
    if !isempty(vname)
        for u in unique(filter(!isempty, [imgu; imgus; setu]))
            params = Cecelia.read_module_fun_params_by_name(dir(u), fun, vname)
            isnothing(params) || (matched = true; break)
        end
    end
    if isnothing(params)
        for u in (imgu, setu)
            isempty(u) && continue
            params = Cecelia.read_module_fun_params(dir(u), fun)
            isnothing(params) || break
        end
    end
    200, JSON3.write((; params = params, matched = matched))
end

# GET /api/tasks/funparams/sources?projectUid=&setUid=&fun=
# List (image, valueName) pairs in the SET that have banked params for `fun` — the source list the
# "Copy from a previous run" picker draws from. The pair the user picks is fetched via
# `/api/tasks/funparams?imageUid=<pick>&valueName=<pick>` (that endpoint already resolves it).
#
# Two data sources per image, unioned:
#   • `meta.funParamsByName[fun]` keys — the exact record the by-name reader uses.
#   • `run_log` entries whose status is "done" and whose `task_output_name(fun, params)` resolves — the
#     retroactive half, so runs that predate the by-name record (or ran in `Cecelia.RUN_LOG_CAP`'s
#     window but the by-name blob has since been overwritten by a different name) are still reachable.
#     Same rule as `run_log_params_for_output`.
#
# Same-set scope (rather than project-wide) matches the mental model — settings are typically copied
# between sibling images that share an acquisition — and it keeps the sweep small.
function api_task_fun_params_sources(req::HTTP.Request)
    q    = HTTP.queryparams(HTTP.URI(req.target))
    proj = get(q, "projectUid", "")
    setu = get(q, "setUid", "")
    fun  = get(q, "fun", "")
    (isempty(proj) || isempty(setu) || isempty(fun)) &&
        return 400, JSON3.write((; error = "projectUid, setUid and fun are required"))

    proj_root = joinpath(projects_dir(), proj)
    isdir(proj_root) || return 404, JSON3.write((; error = "Project not found"))
    set_file = state_file(proj_root, setu)
    isfile(set_file) || return 404, JSON3.write((; error = "Set not found: $setu"))

    # Resolved once for the whole sweep — a task that no longer exists costs one lookup, not one per
    # entry. Same reasoning as run_log_params_for_output.
    task = try
        Cecelia._task_from_fun_name(String(fun))
    catch
        nothing
    end

    set_raw = read_ccid_raw(set_file)
    image_uids = String[String(u) for u in get(set_raw, "image_uids", String[])]

    rows = Dict{String,Any}[]
    for uid in image_uids
        img_file = state_file(proj_root, uid)
        isfile(img_file) || continue
        img_raw = try read_ccid_raw(img_file) catch; continue end
        img_name = String(get(img_raw, "name", uid))
        seen = Dict{String,String}()   # valueName → newest `at` (empty when unknown)

        # 1. by-name blob — keys are the value_names this image has a record for.
        meta = get(img_raw, "meta", nothing)
        if meta isa AbstractDict
            byn = get(meta, "funParamsByName", nothing)
            if byn isa AbstractDict
                per_fun = get(byn, String(fun), nothing)
                if per_fun isa AbstractDict
                    for k in keys(per_fun)
                        name = String(k)
                        isempty(name) && continue
                        seen[name] = ""
                    end
                end
            end
        end

        # 2. run log — retroactive backfill and provider of the `at` timestamp. Newest-first so the
        # first hit per name wins. `task` is nil for an unknown fun, so no log walk at all.
        if !isnothing(task)
            for e in Iterators.reverse(read_run_log(joinpath(proj_root, "1", uid)))
                st = string(get(e, "status", get(e, :status, "")))
                (isempty(st) || st == "done") || continue
                string(get(e, "fun", get(e, :fun, ""))) == String(fun) || continue
                p = get(e, "params", get(e, :params, nothing))
                p isa AbstractDict || continue
                params = Dict{String,Any}(String(k) => v for (k, v) in p)
                name = Cecelia.task_output_name(task, params)
                isempty(name) && continue
                at = string(get(e, "at", get(e, :at, "")))
                cur = get(seen, name, nothing)
                if cur === nothing || isempty(cur)
                    seen[name] = at
                end
            end
        end

        for (name, at) in seen
            row = Dict{String,Any}(
                "imageUid" => uid, "imageName" => img_name, "valueName" => name)
            isempty(at) || (row["at"] = at)
            push!(rows, row)
        end
    end

    # Newest first; ties break by image name for a stable order.
    sort!(rows, by = r -> (get(r, "at", ""), String(r["imageName"])), rev = true)
    200, JSON3.write(rows)
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
# ── Task thread budget ───────────────────────────────────────────────────────
# The CPU sibling of the pool throttles, and it sits in the same popover. A pool limit rations how
# many tasks run at once; this rations how wide ONE of them may go. Both are properties of the
# machine, so both belong to the same control — a per-task param would be N knobs that all had to
# agree (see `cpu_utils.py`).
#
# GET → { workers, default, max, derived }. `derived` says the effective number came from the box
# rather than the config, which is the difference between "16 because you chose it" and "16 because
# this happens to be a 64-core machine" — the second follows the hardware, the first does not.
function api_task_threads_get(_req)
    conf_n = get(get(Cecelia.cecelia_conf(), "tasks", Dict{String,Any}()), "workerThreads", nothing)
    200, JSON3.write((; workers = Cecelia.task_worker_threads(),
                        default = Cecelia.default_task_worker_threads(),
                        max     = Cecelia.task_workers_max(),
                        derived = isnothing(conf_n),
                        # `cores` is what this PROCESS may use (affinity mask + cgroup quota), which
                        # on a cluster node or in a container is not the machine's count — and the
                        # machine's is what a budget must NOT be sized from. Both are reported so the
                        # UI can say which it means; they are equal on an ordinary workstation.
                        cores        = Cecelia.usable_cpus(),
                        machineCores = Sys.CPU_THREADS,
                        # Whether a stage MEASURED to scale linearly may take the usable CPU count
                        # instead of the budget's lone-run-pessimistic share. Only meaningful while
                        # the budget is derived — an explicit number is the user saying how wide a
                        # task may go — so the UI hides the control rather than showing a toggle
                        # that would do nothing. See `Cecelia.task_workers_widen`.
                        widen        = Cecelia.task_workers_widen(),
                        widenCap     = Cecelia.usable_cpus()))
end

# Set it live: persists + hot-reloads, so the NEXT task spawns with it. `workers <= 0` clears the
# setting and goes back to the machine-derived default.
#
# Forwarded to the runner when it is enabled, for the same reason the pool limit is: with the runner
# on, the runner is the process that spawns Python, so it is the process whose config decides
# `CECELIA_TASK_WORKERS`. Applied locally too, so the in-process fallback path stays governed by the
# same number. Best-effort — a runner that is down must not fail the control.
function api_task_threads_set(body_bytes)
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    # `widen` alone: the linear-stage flag without touching the budget. Two controls on one endpoint
    # because they are one setting to the user ("how wide may a task go"), and a second route would
    # let the two disagree about which took effect.
    if haskey(data, :widen) && !haskey(data, :workers)
        on = try Bool(get(data, :widen, false))
        catch; return 400, JSON3.write((; error = "widen must be a boolean")) end
        applied = Cecelia.set_task_workers_widen!(on)
        return 200, JSON3.write((; workers = Cecelia.task_worker_threads(),
                                   derived = Cecelia.task_workers_derived(),
                                   widen = applied))
    end
    n = try _wint(data, :workers, 0) catch; return 400, JSON3.write((; error = "workers must be an integer")) end
    applied = Cecelia.set_task_worker_threads!(n)
    if _runner_enabled()
        try; Cecelia.runner_set_task_workers(_RUNNER, n)
        catch e; @warn "Could not apply the task thread budget on the runner" n exception = e; end
    end
    200, JSON3.write((; workers = applied, derived = n <= 0,
                        widen = Cecelia.task_workers_widen()))
end

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
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    name = _wstr(data, :name)
    isempty(name) && return 400, JSON3.write((; error = "name required"))
    try
        200, JSON3.write((; current = Cecelia.set_image_compressor!(name)))
    catch e
        e isa ArgumentError ? (400, JSON3.write((; error = e.msg))) : rethrow()
    end
end

# ── Reprocessing: keep previous version ──────────────────────────────────────
# Global toggle for the guarded-writer path — see `Cecelia.keep_previous_version`. Default off;
# users flip it on for the niche cases (A/B compare, publication freeze, chain branching,
# regression investigation, sharing intermediates). Same knob future autonomous execution flips
# programmatically so a Claude-triggered run can't overwrite user data.
function api_keep_previous_version_get(_req)
    200, JSON3.write((; current = Cecelia.keep_previous_version(),
                        default = Cecelia.KEEP_PREVIOUS_VERSION_DEFAULT))
end

function api_keep_previous_version_set(body_bytes)
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    haskey(data, :value) || return 400, JSON3.write((; error = "value required (Bool)"))
    v = data[:value]
    v isa Bool || return 400, JSON3.write((; error = "value must be Bool"))
    200, JSON3.write((; current = Cecelia.set_keep_previous_version!(v)))
end

# ── TLS toggle — persisted preference for HTTPS + HTTP/2 ─────────────────────
#
# The server's ACTUAL protocol lives in `/api/diagnostics` as `protocol` (either
# `HTTPS/HTTP2` or `HTTP/1.1`); this endpoint is the persisted PREFERENCE. Requires a
# server restart to take effect on the wire — the UI shows a "restart required" hint when
# `desired != current-on-wire`.
function api_tls_get(_req)
    is_dev = _is_dev()
    200, JSON3.write((;
        desired = Cecelia.tls_desired(is_dev = is_dev),
        protocol = _PROTOCOL[],
        envOverride = haskey(ENV, "CECELIA_TLS"),   # settings toggle is a no-op while env forces it
    ))
end

function api_tls_set(body_bytes)
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    on = get(data, :on, nothing)
    (on === true || on === false) || return 400, JSON3.write((; error = "on (bool) required"))
    is_dev = _is_dev()
    effective = Cecelia.set_tls_desired!(on; is_dev = is_dev)
    200, JSON3.write((;
        desired = effective,
        protocol = _PROTOCOL[],
        envOverride = haskey(ENV, "CECELIA_TLS"),
        restartRequired = effective != (_PROTOCOL[] == "HTTPS/HTTP2"),
    ))
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
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    name = _wstr(data, :name)
    isempty(name) && return 400, JSON3.write((; error = "name required"))
    try
        200, JSON3.write((; current = Cecelia.set_store_layout!(name)))
    catch e
        e isa ArgumentError ? (400, JSON3.write((; error = e.msg))) : rethrow()
    end
end

function api_pool_set(body_bytes)
    data  = _parse_body(body_bytes)
    data isa Tuple && return data
    name  = _wstr(data, :name)
    limit = _wint(data, :limit, 0)
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


include(joinpath(@__DIR__, "routes", "project.jl"))

# ── Set management ────────────────────────────────────────────────────────────

function api_sets_create(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    # TRIMMED, like `rename` and the `newSetName` paths on move/copy — this route was the one that
    # wasn't, so " Day 3 " and "Day 3" could become two sets whose picker rows look identical. Same rule
    # as the `attr/*` routes' `_norm_attr`: normalise where the user's keystrokes enter the model.
    name        = strip(_wstr(body, :name))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(name)        && return 400, JSON3.write((; error="name required"))

    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error="Project not found: $project_uid"))

    proj = load_project(project_uid)
    # 409, the same code `rename` and `chains/rename` use for a taken name. The guard itself is
    # `add_set!`'s (so the REPL and the copy/move paths get it too); this only maps it to a status.
    set_name_taken(proj, name) &&
        return 409, JSON3.write((; error="A set named \"$name\" already exists in this project"))
    s    = add_set!(proj; name=String(name))
    @info "Created set" name uid=s.uid project=project_uid
    200, JSON3.write((; uid=s.uid, name=s.name))
end

# POST /api/sets/rename  { projectUid, setUid, name }  → { uid, name }
#
# A set's name is display-only — its identity is the uid (see `rename_set!`) — so this is a one-field
# metadata edit, not the re-identify a project rename needs. Nothing else has to be told: the images
# are attached by uid, and every per-set UI setting is keyed by uid too.
#
# The duplicate-name guard is the MODEL's (`set_name_taken` / `rename_set!`), so it holds for a REPL
# caller too; this handler only maps it onto the status code `api_chains_rename` already uses for the
# same shape of refusal — 409, "the target name is taken". Renaming to the set's own name is a 200
# no-op, which is what makes a re-run idempotent.
function api_sets_rename(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
    name        = strip(_wstr(body, :name))
    isempty(project_uid) && return 400, JSON3.write((; error="projectUid required"))
    isempty(set_uid)     && return 400, JSON3.write((; error="setUid required"))
    # Trimmed, so a whitespace-only name is an empty one — a set that renders as a blank row in the
    # picker is unselectable by name and looks like a bug.
    isempty(name)        && return 400, JSON3.write((; error="name required"))

    proj_dir      = joinpath(projects_dir(), project_uid)
    set_meta_file = state_file(proj_dir, set_uid)
    isdir(proj_dir)       || return 404, JSON3.write((; error="Project not found"))
    isfile(set_meta_file) || return 404, JSON3.write((; error="Set not found: $set_uid"))

    proj = load_project(project_uid)
    set_name_taken(proj, name; except = set_uid) &&
        return 409, JSON3.write((; error="A set named \"$name\" already exists in this project"))
    s = rename_set!(proj, set_uid, String(name))
    @info "Renamed set" uid=set_uid name project=project_uid
    200, JSON3.write((; uid=s.uid, name=s.name))
end

function api_sets_delete(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    set_uid     = _wstr(body, :setUid)
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


include(joinpath(@__DIR__, "routes", "image.jl"))

include(joinpath(@__DIR__, "routes", "metadata.jl"))

include(joinpath(@__DIR__, "routes", "lab_log.jl"))

include(joinpath(@__DIR__, "routes", "helpers.jl"))
