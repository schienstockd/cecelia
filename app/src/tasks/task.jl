abstract type CciaTask end

# ── Spec loading ──────────────────────────────────────────────────────────────

const _SPEC_CACHE      = Dict{String, Any}()
# _task_spec is called on every run_task (validate_params/task_scope/pool lookup) and from
# handle_task_run — concurrent under `-t auto`. An unlocked lazy Dict write can rehash mid-read
# on another thread → corruption/crash. Serialise the check-and-fill (specs are tiny; contention nil).
const _SPEC_CACHE_LOCK = ReentrantLock()
const _FRAGMENTS_DIR   = joinpath(@__DIR__, "fragments")

# Expand a params array: items with {"$include": "name"} are replaced in-place
# by all items from fragments/name.json. Recurses into nested dicts.
function _expand_params_array(arr, fdir::String)::Vector{Any}
    result = Any[]
    for item in arr
        if item isa AbstractDict
            item_dict = Dict{String,Any}(String(k) => v for (k, v) in item)
            inc = get(item_dict, "\$include", nothing)
            if !isnothing(inc)
                frag_file = joinpath(fdir, "$(string(inc)).json")
                if isfile(frag_file)
                    frag = JSON3.read(read(frag_file, String), Vector{Any})
                    append!(result, _expand_params_array(frag, fdir))
                else
                    @warn "Task param fragment not found: $frag_file"
                end
            else
                push!(result, _resolve_spec_includes(item_dict, fdir))
            end
        else
            push!(result, item)
        end
    end
    result
end

function _resolve_spec_includes(obj::Dict, fdir::String)::Dict{String,Any}
    result = Dict{String,Any}()
    for (k, v) in obj
        sk = String(k)
        if sk == "params" && v isa AbstractVector
            result[sk] = _expand_params_array(v, fdir)
        elseif v isa AbstractDict
            result[sk] = _resolve_spec_includes(
                Dict{String,Any}(String(k2) => v2 for (k2, v2) in v), fdir)
        else
            result[sk] = v
        end
    end
    result
end

function _task_spec(task::CciaTask)::Union{Dict{String,Any}, Nothing}
    key = string(typeof(task))
    lock(_SPEC_CACHE_LOCK) do
        haskey(_SPEC_CACHE, key) && return _SPEC_CACHE[key]

        # Map type name → relative spec path inside app/src/tasks/
        spec_file = _spec_path(task)
        isnothing(spec_file) && return nothing
        isfile(spec_file) || return nothing

        spec = JSON3.read(read(spec_file, String), Dict{String,Any})
        spec = _resolve_spec_includes(spec, _FRAGMENTS_DIR)
        _SPEC_CACHE[key] = spec
        spec
    end
end

# Resolve a producer task's output value_name from its JSON spec's top-level "outputValueName".
# This makes the output handle a single, introspectable source of truth (the JSON) rather than a
# constant buried in the task's .jl: the whiteboard reads the same field to prefill a downstream
# node's input `valueName` (see ChainModule value-name propagation). Falls back to `default` when
# the spec declares no fixed output (e.g. tasks whose output name is a user-set param instead).
function _spec_output_value_name(task::CciaTask, default::String)::String
    spec = _task_spec(task)
    isnothing(spec) && return default
    v = get(spec, "outputValueName", nothing)
    isnothing(v) ? default : string(v)
end

# Subclasses define their spec path by implementing this or we use naming convention.
# Default: look for <category>/<task>.json next to the .jl file.
function _spec_path(task::CciaTask)::Union{String, Nothing}
    nothing  # concrete tasks override via the table below
end

# Concrete _spec_path overloads and _FUN_NAME_MAP live in task_registry.jl,
# included after all task type definitions.

# ── Param validation ──────────────────────────────────────────────────────────

struct ParamValidationError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ParamValidationError) = print(io, "ParamValidationError: ", e.msg)

function _validate_leaf(key, value, spec::Dict{String,Any})
    type_str = get(spec, "type", "")

    if type_str == "int"
        v = value isa Integer ? value : tryparse(Int, string(value))
        isnothing(v) && throw(ParamValidationError("'$key' must be an integer, got: $value"))
        mn = get(spec, "min", nothing)
        mx = get(spec, "max", nothing)
        (!isnothing(mn) && v < mn) && throw(ParamValidationError("'$key' = $v is below minimum $mn"))
        (!isnothing(mx) && v > mx) && throw(ParamValidationError("'$key' = $v exceeds maximum $mx"))

    elseif type_str == "float"
        v = value isa AbstractFloat ? value : tryparse(Float64, string(value))
        isnothing(v) && throw(ParamValidationError("'$key' must be a number, got: $value"))
        mn = get(spec, "min", nothing)
        mx = get(spec, "max", nothing)
        (!isnothing(mn) && v < mn) && throw(ParamValidationError("'$key' = $v is below minimum $mn"))
        (!isnothing(mx) && v > mx) && throw(ParamValidationError("'$key' = $v exceeds maximum $mx"))

    elseif type_str == "bool"
        value isa Bool || throw(ParamValidationError("'$key' must be a boolean, got: $value"))

    elseif type_str == "select"
        options = get(spec, "options", [])
        valid   = [string(get(o, "value", "")) for o in options]
        string(value) ∈ valid ||
            throw(ParamValidationError("'$key' = \"$value\" is not a valid option. Valid: $(join(valid, ", "))"))
    end
    # text, channelSelection, valueNameSelection, group, section — no scalar constraint to enforce
end

function _validate_params_against_spec(params::Dict{String,Any}, spec_params::Vector)
    for p in spec_params
        p isa AbstractDict || continue
        key      = string(get(p, "key", ""))
        type_str = string(get(p, "type", ""))
        isempty(key) && continue

        if type_str == "section"
            inner = get(p, "params", [])
            isempty(inner) || _validate_params_against_spec(params, inner)
            continue
        end

        if type_str == "group"
            # Group params are dicts keyed by index string; validate each entry's sub-params.
            inner = get(p, "params", [])
            val   = get(params, key, nothing)
            if !isnothing(val) && val isa AbstractDict
                for (_, entry) in val
                    entry isa AbstractDict || continue
                    entry_dict = Dict{String,Any}(string(k) => v for (k, v) in entry)
                    _validate_params_against_spec(entry_dict, inner)
                end
            end
            continue
        end

        required = get(p, "required", false)
        val = get(params, key, nothing)

        if isnothing(val) || val == ""
            required && throw(ParamValidationError("Required param '$key' is missing"))
            continue  # optional and absent — skip range/type checks
        end

        _validate_leaf(key, val, Dict{String,Any}(string(k) => v for (k, v) in p))
    end
end

"""
Validate params against the task's co-located JSON spec.
Throws ParamValidationError with a clear message if any constraint is violated.
No-ops if the spec file is not found (allows tasks without a spec).
"""
function validate_params(task::CciaTask, params::Dict{String,Any})
    spec = _task_spec(task)
    isnothing(spec) && return
    spec_params = get(spec, "params", [])
    isempty(spec_params) && return
    _validate_params_against_spec(params, spec_params)
end

# ── Internal dispatch ─────────────────────────────────────────────────────────
# run_task / run_tasks live in scheduler.jl (included after task_registry.jl).
# All task execution — REPL and API — goes through the scheduler's pool machinery.

"""
Internal dispatch — implement this in concrete task types.
Public callers use run_task (scheduler.jl), which validates params and acquires a
resource-pool slot before calling here.
"""
function _run_task(task::CciaTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    error("_run_task not implemented for $(typeof(task))")
end

"""
Set-scope variant — called by the chain executor for scope=\"set\" nodes.
Receives all images in the set; runs once, not once per image.
Default raises; override in concrete set-scope task types.
"""
function _run_task(task::CciaTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    error("$(typeof(task)) does not support set-scope execution (scope=\"set\")")
end

# ── Composite task ────────────────────────────────────────────────────────────
# A composite chains multiple sub-tasks sequentially.  Each step's returned
# valueName is injected as the next step's input param.  No .jl file needed for
# composite tasks — they are declared entirely in a JSON spec with a "composite"
# array.  Register composite specs in _COMPOSITE_SPEC_PATHS (task_registry.jl).

struct CompositeTask <: CciaTask
    fun_name::String
end

const _COMPOSITE_SPEC_PATHS = Dict{String, String}()

function _spec_path(task::CompositeTask)::Union{String, Nothing}
    get(_COMPOSITE_SPEC_PATHS, task.fun_name, nothing)
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

"""
    task_scope(task) -> "image" | "set"

A task's invocation scope, from its spec's `"scope"` field (default `"image"`). `"set"` tasks run
once over a whole image vector (`_run_task(task, imgs::Vector{CciaImage}, …)`) — e.g. `behaviour.hmm`,
which fits across the set. Used by the API to route a `task:run` to the single- or set-image path.
"""
task_scope(task::CciaTask)::String =
    (s = _task_spec(task); isnothing(s) ? "image" : string(get(s, "scope", "image")))

function validate_params(task::CompositeTask, params::Dict{String,Any})
    spec  = _task_spec(task)
    steps = isnothing(spec) ? String[] : [string(s) for s in get(spec, "composite", String[])]
    for step_fun_name in steps
        sub_task = try _task_from_fun_name(step_fun_name) catch; continue; end
        validate_params(sub_task, params)
    end
end

function _run_task(task::CompositeTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    spec  = _task_spec(task)
    steps = isnothing(spec) ? String[] :
            [string(s) for s in get(spec, "composite", [])]
    if isempty(steps)
        on_log("[ERROR] Composite task '$(task.fun_name)' has no steps")
        return nothing
    end

    # If the spec defines outputValueName, snapshot ccid.json filepath keys now
    # so we can remove intermediate sub-task entries after the chain completes.
    out_vn_raw = get(spec, "outputValueName", nothing)
    out_vn     = isnothing(out_vn_raw) ? nothing : string(out_vn_raw)
    ccid       = joinpath(img._dir, "ccid.json")
    pre_keys   = Set{String}()
    if !isnothing(out_vn) && isfile(ccid)
        raw0 = JSON3.read(read(ccid, String))
        fp0  = get(raw0, :filepath, nothing)
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

        # Wire the step's output valueName as the next step's input
        if result isa AbstractDict
            vn = get(result, "valueName", nothing)
            isnothing(vn) || (cur_params = merge(cur_params,
                                                  Dict{String,Any}("valueName" => string(vn))))
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
            raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
            fp   = get(raw2, "filepath", nothing)
            if fp isa AbstractDict
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
                open(ccid, "w") do io; JSON3.write(io, raw2); end
                on_log("[INFO] Composite output registered as '$out_vn' → $out_filename")
            end
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
    spec  = _task_spec(task)
    steps = isnothing(spec) ? String[] : [string(s) for s in get(spec, "composite", [])]
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
            vn = get(result, "valueName", nothing)
            isnothing(vn) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("valueName" => string(vn))))
            # Thread an HMM states step's produced column into the next step (transitions) as its
            # `hmmStates` input, so `behaviour.hmm` (states → transitions) chains on a single
            # user-set `colName` without exposing the derived state column in the composite form.
            sc = get(result, "stateColumn", nothing)
            isnothing(sc) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("hmmStates" => [string(sc)])))
        end
    end
    result
end

# ── fun_name dispatch ─────────────────────────────────────────────────────────
# _FUN_NAME_MAP is populated in task_registry.jl (included after all task types).

function _task_from_fun_name(fun_name::String)::CciaTask
    map = _fun_name_map()
    haskey(map, fun_name) ||
        error("Unknown fun_name: \"$fun_name\". Available: $(join(keys(map), ", "))")
    map[fun_name]
end
