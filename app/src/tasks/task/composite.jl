# ── CompositeTask — struct + methods + _run_task overloads ──────────────────
# Extracted from tasks/task.jl (2026-09-16). Originally sat in two blocks around
# the sections helpers; reunited here.

# ── Composite task ────────────────────────────────────────────────────────────
# A composite chains multiple sub-tasks sequentially.  Each step's returned
# valueName is injected as the next step's input param.  No .jl file needed for
# composite tasks — they are declared entirely in a JSON spec with a "composite"
# array.  Register composite specs in _COMPOSITE_SPEC_PATHS (task_registry.jl).
#
# **ADDING A TASK TRAIT? DECIDE HOW IT RECURSES HERE.** A composite's steps run through `_run_task`
# directly, so they never register TaskRecords and are never consulted as tasks in their own right —
# whatever the composite answers IS the answer. Every trait describing a task's behaviour therefore
# needs an explicit `::CompositeTask` method that folds over `_composite_steps(task)`:
#
#   task_requires_axes  → union of the steps' required axes
#   _section_keys       → union of the steps' section param keys
#   live_outputs        → concatenation of the steps' live outputs
#   task_output_name    → the FIRST step that names an output
#
# Forgetting one is SILENT and looks like "the feature doesn't work for the composite" — which is
# exactly how the live preview first shipped broken (the segmentation module page runs
# `segment.cellposeMeasure`, not `segment.cellpose`). The shared half — reading the spec and resolving
# step names — is `_composite_steps`; only the combiner is per-trait, and that part genuinely differs
# (union vs concat), so it stays explicit rather than hidden behind a registry.

struct CompositeTask <: CciaTask
    fun_name::String
end

const _COMPOSITE_SPEC_PATHS = Dict{String, String}()

function _spec_path(task::CompositeTask)::Union{String, Nothing}
    get(_COMPOSITE_SPEC_PATHS, task.fun_name, nothing)
end

# ── Composite step resolution — the ONE place that reads `spec["composite"]` ───
# Six call sites used to re-derive this (the three trait recursions below, `validate_params`, and both
# `_run_task` methods), each re-reading the spec and re-resolving step names, three of them
# byte-identical. Returns empty for a non-composite (no `composite` key), so a caller never has to ask
# "is this a composite" first.
#
# Two forms, because the difference is real:
# * `_composite_steps` resolves to tasks and **skips** a name that doesn't resolve — what every
#   read-only consumer (traits, validation) wants: describe what you can, don't throw while
#   introspecting.
# * `_composite_step_names` returns the declared names. The executor needs those for its progress log
#   AND must **hard-fail** on one that doesn't resolve — a typo in a composite spec has to stop the run,
#   not silently shorten it — so it resolves them itself rather than using the skipping form.
function _composite_step_names(task::CciaTask)::Vector{String}
    spec = _task_spec(task)
    isnothing(spec) && return String[]
    String[string(s) for s in get(spec, "composite", String[])]
end

function _composite_steps(task::CciaTask)::Vector{CciaTask}
    out = CciaTask[]
    for step in _composite_step_names(task)
        sub = try _task_from_fun_name(step) catch; nothing end
        isnothing(sub) || push!(out, sub)
    end
    out
end

# Composite: union the steps' live outputs. This is the overload that MATTERS in practice — the
# segmentation module page runs `segment.cellposeMeasure` (cellpose → measureLabels), not
# `segment.cellpose`, and a composite's steps run via `_run_task` directly, so they never register
# TaskRecords of their own. Without this the composite is the only record there is and it would
# declare nothing, leaving the most common way to start a segmentation with no preview.
#
# Passing the composite's params straight through is correct: composites carry no params of their own,
# so a step's params (`outputValueName`, `models`) already sit at the top level of this same dict —
# the same assumption `_section_keys` and the executor's `cur_params` make.
function live_outputs(task::CompositeTask, params::AbstractDict)::Vector{LiveOutput}
    out = LiveOutput[]
    for sub in _composite_steps(task)
        append!(out, live_outputs(sub, params))
    end
    unique(out)
end

# Composite: previewable if ANY step is. Same reasoning as `live_outputs` above, and the same trap —
# `segment.cellposeMeasure` is what the segmentation page actually runs, so without this the most common
# way to start a segmentation would report itself unpreviewable. `any`, not `all`: the preview shows one
# step's output (the segmentation), and the measurement step that follows has nothing to preview but must
# not veto it.
task_previewable(task::CompositeTask)::Bool =
    any(task_previewable, _composite_steps(task))

# Composite: the effect the user sees is the strongest one across the steps. A composite that runs an
# AF correction and then a drift correction still just adds a version; a composite that ends by
# creating a whole new image should say so. Priority (strongest last): in-place < new-version <
# new-image. A step that declares nothing is skipped (a measurement step in a segment+measure
# composite has no image-output effect and shouldn't blank the segmenter's).
function task_output_effect(task::CompositeTask)::Union{String, Nothing}
    rank = Dict("in-place" => 1, "new-version" => 2, "new-image" => 3)
    best::Union{String, Nothing} = nothing
    best_rank = 0
    for sub in _composite_steps(task)
        e = task_output_effect(sub)
        e === nothing && continue
        r = get(rank, e, 0)
        if r > best_rank
            best = e
            best_rank = r
        end
    end
    best
end

# Composite: the previewable step owns the translation. Params are shared across a composite's steps
# (they sit flat in one dict — see `live_outputs(::CompositeTask, …)`), so the first step that can be
# previewed is the one whose Python will consume them.
function preview_params(task::CompositeTask, params::AbstractDict, img::CciaImage)::AbstractDict
    for sub in _composite_steps(task)
        task_previewable(sub) && return preview_params(sub, params, img)
    end
    params
end


# Composite: the FIRST step that names an output. A composite carries no params of its own — the form
# is the union of its steps' (see `api_task_definitions`) — so the name the user typed belongs to a
# step's spec, and the composite writes under it. First, not last: the producing step comes first
# (`cellpose` → `measureLabels`), and a later step that measures ONTO that output names nothing.
#
# This is the trait-recursion trap this section warns about, and it shipped: params banked per output
# name keyed off `task_output_name`, which answered `""` for every composite — so the segmentation
# page, which runs `segment.cellposeMeasure`, banked nothing under `Tcell` no matter how often it ran.
# The frontend was not affected (the definitions route merges composite params before it sees them),
# so the field looked right and only the memory was missing.
function task_output_name(task::CompositeTask, params::Dict{String,Any})::String
    for sub in _composite_steps(task)
        name = task_output_name(sub, params)
        isempty(name) || return name
    end
    ""
end

# Composite: union `requires.axes` across the steps (plus the composite's own, if any). So an HMM
# composite (states → transitions) inherits :T from its steps without repeating it in its own JSON.
function task_requires_axes(task::CompositeTask)::Set{Symbol}
    spec = _task_spec(task)
    isnothing(spec) && return Set{Symbol}()
    axes = _axes_from_requires(get(spec, "requires", nothing))
    for sub in _composite_steps(task)
        union!(axes, task_requires_axes(sub))
    end
    axes
end

# Same union, same reason: a composite needs whatever any of its steps needs, so segment+measure
# inherits the segmenter's scale requirement without restating it.
function task_requires_scale(task::CompositeTask)::Set{Symbol}
    spec = _task_spec(task)
    isnothing(spec) && return Set{Symbol}()
    scale = _scale_from_requires(get(spec, "requires", nothing))
    for sub in _composite_steps(task)
        union!(scale, task_requires_scale(sub))
    end
    scale
end

# Override spec caching: CompositeTask type alone is not unique — include fun_name.
function _task_spec(task::CompositeTask)::Union{Dict{String,Any}, Nothing}
    key = "CompositeTask:$(task.fun_name)"
    lock(_SPEC_CACHE_LOCK) do
        haskey(_SPEC_CACHE, key) && return _SPEC_CACHE[key]
        spec_file = _spec_path(task)
        isnothing(spec_file) && return nothing
        isfile(spec_file)    || return nothing
        spec = JSON3.read(read(spec_file, String), Dict{String,Any})
        spec = _resolve_spec_includes(spec, _FRAGMENTS_DIR)
        _SPEC_CACHE[key] = spec
        spec
    end
end

function validate_params(task::CompositeTask, params::Dict{String,Any};
                         extra_options::Set{String} = Set{String}(),
                         in_composite::Bool = false)
    # An unresolvable step is skipped here (`_composite_steps`) and hard-errors in `_run_task`, where
    # the run can actually be stopped — validation stays about the PARAMS.
    #
    # `in_composite = true` is the whole point of this method: a step is being validated as part of a
    # composite, so its `hideInComposite` params are the composite's business, not the user's. (The
    # keyword is accepted and forwarded so a composite nested in a composite behaves the same.)
    for sub_task in _composite_steps(task)
        validate_params(sub_task, params; extra_options, in_composite = true)
    end
end

function _run_task(task::CompositeTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    spec  = _task_spec(task)
    steps = _composite_step_names(task)   # NAMES: needed for the log + the unknown-step error below
    if isempty(steps)
        on_log("[ERROR] Composite task '$(task.fun_name)' has no steps")
        return nothing
    end

    # If the spec defines outputValueName, snapshot ccid.json filepath keys now
    # so we can remove intermediate sub-task entries after the chain completes.
    out_vn_raw = get(spec, "outputValueName", nothing)
    out_vn     = isnothing(out_vn_raw) ? nothing : string(out_vn_raw)
    ccid       = state_file(img)
    pre_keys   = Set{String}()
    if !isnothing(out_vn) && isfile(ccid)
        raw0 = read_ccid_raw(ccid)
        fp0  = get(raw0, "filepath", nothing)
        if fp0 isa AbstractDict
            for k in keys(fp0)
                sk = string(k)
                sk != VERSIONED_ACTIVE_KEY && push!(pre_keys, sk)
            end
        end
    end

    n_steps            = length(steps)
    cur_params         = copy(params)
    result             = nothing
    intermediate_files = String[]   # filenames created by non-final steps

    for (i, step_fun_name) in enumerate(steps)
        on_log("[INFO] Composite step $i/$n_steps: $step_fun_name")
        step_task = try
            _task_from_fun_name(step_fun_name)
        catch e
            on_log("[ERROR] Unknown composite step '$step_fun_name': $e")
            return nothing
        end

        # Scale progress: step i maps to the range [(i-1)/n, i/n] of 0..100
        step_on_progress = (done, total) -> begin
            total > 0 || return
            scaled = ((i - 1) * total + done) / (n_steps * total)
            on_progress(round(Int, scaled * 100), 100)
        end

        result = _run_task(step_task, img, cur_params;
                           on_log, on_progress = step_on_progress, on_process)

        isnothing(result) && return nothing   # step failed — abort chain

        # Track intermediate output files (all steps except the last)
        if i < n_steps && result isa AbstractDict
            fn = get(result, "filename", nothing)
            isnothing(fn) || push!(intermediate_files, string(fn))
        end

        # Wire the step's output valueName as the next step's input. Both keys are forwarded —
        # `valueName` (what most tasks read) AND `outputValueName` (what producer-style tasks read,
        # e.g. measureLabels). A step that only sets `valueName` still works; one that also sets
        # `outputValueName` (cellpose, coastal, segment.correct) carries it through so the next
        # step doesn't fall back to `VERSIONED_DEFAULT_VAL` and re-measure the WRONG segmentation.
        if result isa AbstractDict
            vn  = get(result, "valueName",       nothing)
            ovn = get(result, "outputValueName", nothing)
            isnothing(vn)  || (cur_params = merge(cur_params,
                                                  Dict{String,Any}("valueName"       => string(vn))))
            isnothing(ovn) || (cur_params = merge(cur_params,
                                                  Dict{String,Any}("outputValueName" => string(ovn))))

            # `skipDownstream` — a step-level "chain complete, no more work needed" signal. Distinct
            # from a failure (`nothing`): the run succeeded, but the remaining steps have nothing to
            # do. Introduced for `segment.correct` in the `segment.correct_measures` composite when
            # every op resolved to a no-op — re-measuring the whole labels store to rebuild an
            # identical h5ad is pure waste (and would fail loudly on any pre-existing shape drift
            # between the labels and image, unrelated to the correction). Any producer task can opt
            # in by returning `"skipDownstream" => true`.
            if get(result, "skipDownstream", false) === true
                remaining = n_steps - i
                if remaining > 0
                    on_log("[INFO] Step '$step_fun_name' signalled skipDownstream — " *
                           "skipping remaining $remaining composite step(s).")
                end
                break
            end
        end
    end

    # Remove intermediate files from disk — they were only needed as inputs to the next step
    if !isempty(intermediate_files)
        proj_dir = dirname(dirname(img._dir))
        im_dir   = joinpath(proj_dir, "0", img.uid)
        for fn in intermediate_files
            p = joinpath(im_dir, fn)
            if ispath(p)
                on_log("[INFO] Removing intermediate file: $fn")
                rm(p; recursive = true)
            end
        end
    end

    # If outputValueName is set: replace all intermediate ccid.json entries with
    # a single canonical entry under out_vn pointing to the last step's file.
    if !isnothing(out_vn) && result isa AbstractDict && isfile(ccid)
        out_filename = string(get(result, "filename", ""))
        if !isempty(out_filename)
            registered = false
            commit_state!(img) do raw2
                fp = get(raw2, "filepath", nothing)
                fp isa AbstractDict || return
                fp2 = Dict{String,Any}(String(k) => v for (k, v) in fp)
                # Remove intermediate entries added by sub-tasks (not in pre-snapshot, not canonical)
                for k in collect(keys(fp2))
                    k == VERSIONED_ACTIVE_KEY && continue
                    k ∈ pre_keys              && continue
                    k == out_vn               && continue
                    delete!(fp2, k)
                end
                fp2[out_vn] = out_filename
                fp2[VERSIONED_ACTIVE_KEY] = out_vn
                raw2["filepath"] = fp2
                registered = true
            end
            registered && on_log("[INFO] Composite output registered as '$out_vn' → $out_filename")
            result = Dict{String,Any}("valueName" => out_vn, "filename" => out_filename)
        end
    end

    result
end

# Set-scope composite: run each step's set-scope form over the whole image vector, in sequence
# (e.g. behaviour.hmm = hmm_states → hmm_transitions, fitted/computed jointly across the set).
# Steps wire `valueName` forward like the image-scope composite, but there is no intermediate-file
# or ccid.json rewriting — set-scope behaviour tasks add obs columns, they don't create value_names.
function _run_task(task::CompositeTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    steps = _composite_step_names(task)   # NAMES: needed for the log + the unknown-step error below
    if isempty(steps)
        on_log("[ERROR] Composite task '$(task.fun_name)' has no steps")
        return nothing
    end
    n_steps    = length(steps)
    cur_params = copy(params)
    result     = nothing
    for (i, step_fun_name) in enumerate(steps)
        on_log("[INFO] Composite step $i/$n_steps: $step_fun_name")
        step_task = try
            _task_from_fun_name(step_fun_name)
        catch e
            on_log("[ERROR] Unknown composite step '$step_fun_name': $e")
            return nothing
        end
        step_on_progress = (done, total) -> begin
            total > 0 || return
            on_progress(round(Int, ((i - 1) * total + done) / (n_steps * total) * 100), 100)
        end
        result = _run_task(step_task, imgs, cur_params;
                           on_log, on_progress = step_on_progress, on_process)
        isnothing(result) && return nothing
        if result isa AbstractDict
            # Same dual-key forwarding as the per-image executor above — see the note there.
            vn  = get(result, "valueName",       nothing)
            ovn = get(result, "outputValueName", nothing)
            isnothing(vn)  || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("valueName"       => string(vn))))
            isnothing(ovn) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("outputValueName" => string(ovn))))
            # Thread an HMM states step's produced column into the next step (transitions) as its
            # `hmmStates` input, so `behaviour.hmm` (states → transitions) chains on a single
            # user-set `colName` without exposing the derived state column in the composite form.
            sc = get(result, "stateColumn", nothing)
            isnothing(sc) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("hmmStates" => [string(sc)])))
            # `skipDownstream` — mirrors the per-image executor; see the note there.
            if get(result, "skipDownstream", false) === true
                remaining = n_steps - i
                if remaining > 0
                    on_log("[INFO] Step '$step_fun_name' signalled skipDownstream — " *
                           "skipping remaining $remaining composite step(s).")
                end
                break
            end
        end
    end
    result
end
