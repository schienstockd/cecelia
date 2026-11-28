# ── POST /api/tasks/validate — form-time param advisory, resolved server-side ─────────────────
#
# The frontend has a `backendAdvisor(funName, paramKey)` factory (frontend/src/tasks/paramAdvisors.ts)
# that returns a `ParamAdvisor` whose only job is to POST here and render the reply. The rule lives
# in Julia (`app/src/tasks/param_validators.jl` + a registration in each task file), so the pre-run
# advisory and the run-time refusal read from the same source of truth.
#
# Request body:  { funName, paramKey, value, projectUid, imageUids, siblingValues? }
# Response 200:  { severity, message, tip [, flag] } — or `null` when there is nothing to say
#                (no validator registered, or the validator returned nothing usable).
# Response 4xx:  { error } on shape errors (missing funName/paramKey, unresolvable project).
#
# Images that fail to resolve are dropped silently — an advisory speaks about the set that did
# resolve, and if none resolve the validator returns nothing (silence beats a wrong number).

function api_task_validate(req::HTTP.Request, body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes))
    catch e
        return _gerr(400, "invalid JSON body: " * sprint(showerror, e))
    end
    fun_name  = get(body, :funName, "") |> String
    param_key = get(body, :paramKey, "") |> String
    isempty(fun_name)  && return _gerr(400, "funName is required")
    isempty(param_key) && return _gerr(400, "paramKey is required")

    project_uid = get(body, :projectUid, "") |> String
    image_uids  = String[String(u) for u in get(body, :imageUids, String[])]
    siblings_raw = get(body, :siblingValues, nothing)
    siblings = Dict{String,Any}()
    if siblings_raw isa AbstractDict
        for (k, v) in siblings_raw
            siblings[String(k)] = v
        end
    end
    value = get(body, :value, nothing)
    # Nothing registered: answer before loading any image.
    haskey(PARAM_VALIDATORS, (fun_name, param_key)) || return 200, "null"

    # Resolve images. `_gating_image` returns (img, err); on err we just drop the image — the
    # validator judges the subset that DID resolve. An empty set is a legitimate "nothing to say".
    imgs = CciaImage[]
    if !isempty(project_uid)
        for uid in image_uids
            img, _ = _gating_image(project_uid, uid)
            img === nothing || push!(imgs, img)
        end
    end

    result = validate_param(fun_name, param_key, value, imgs, siblings)
    result === nothing && return 200, "null"
    return 200, JSON3.write(result)
end

# ── POST /api/tasks/write-check — what a run WOULD write, and whether that replaces something ─────
#
# Asked before submitting, by Claude starting work in a GUI user's project (the autonomous server in GUI
# mode, mcp/cecelia_mcp/autonomous.py; docs/todo/AGENT_GUI_RUNS_PLAN.md D3). The answer is
# `run_write_check` (app/src/tasks/chain/preflight.jl) — the same `run_replaces` the run log stamps at open.
#
# Request body:  { funName, params, projectUid, imageUids }
# Response 200:  { output?: {name, ns, into, key?}, replaces?: {imageUid: {...}}, free? }
# Response 4xx:  { error } — missing funName/projectUid/imageUids, or an image that does not resolve
#                (unlike the advisory above, an image left out would be a "nothing replaced" not checked).
function api_task_write_check(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes))
    catch e
        return _gerr(400, "invalid JSON body: " * sprint(showerror, e))
    end
    fun_name    = string(get(body, :funName, ""))
    project_uid = string(get(body, :projectUid, ""))
    image_uids  = String[String(u) for u in get(body, :imageUids, String[])]
    isempty(fun_name)    && return _gerr(400, "funName is required")
    isempty(project_uid) && return _gerr(400, "projectUid is required")
    isempty(image_uids)  && return _gerr(400, "imageUids is required")
    raw = get(body, :params, nothing)
    params = raw isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in raw) : Dict{String,Any}()
    imgs = CciaImage[]
    for uid in image_uids
        img, err = _gating_image(project_uid, uid)
        img === nothing && return err
        push!(imgs, img)
    end
    return 200, JSON3.write(run_write_check(fun_name, imgs, params))
end
