# ── /api/correction-plan/* — surface for slices 3a + 3b + 3d of docs/todo/CORRECTION_QC_PLAN.md ────
#
# Five endpoints:
#   GET  /api/correction-plan/presets                                → [{id,name,description,orderHints,validationStatus}, …]
#   POST /api/correction-plan/recommend { projectUid, imageUid,
#                                        cardId?, wizard? }          → plan dict (`_plan_to_dict`)
#   GET  /api/correction-plan/get?projectUid=&imageUid=              → { plan | null, exists, stale }
#   POST /api/correction-plan/save    { projectUid, imageUid,
#                                        cardId?, wizard? }          → plan dict (side-effect: writes plan.json)
#   POST /api/correction-plan/mount   { projectUid, imageUid,
#                                        overwrite? }                → { ok, name, nodeCount, created }
#
# The recommend endpoint is pure (no disk write). Save runs recommend AND persists to plan.json so
# the sidecar records the user's card choice + wizard answers atomically. Get returns whether the
# sidecar exists and whether its `saturationFingerprint` still matches the image's current meta
# (`stale = true` means the image was re-imported since the plan was saved). The frontend uses that
# to decide "load saved" vs "recommend fresh" without a second round-trip.
#
# Mount converts the saved plan into a ChainTemplate and writes it into the project's chains dir. It
# requires the plan to be on disk (mounting a fresh, unpersisted recommendation would create a chain
# whose provenance can't be traced back to a card the user actually picked). Chain name is fixed by
# `plan_to_chain_template` at `correction-plan-{imageUid}` — canonical per-image — so re-mounting the
# same image's plan is the expected way to sync a chain to a changed card, gated by `overwrite: true`
# to keep an accidental overwrite of a hand-edited chain from being silent.
#
# Every response uses the plan.json field-name convention (`funName`, `orderWeight`, …) so the
# frontend types match plan.json 1:1 — the saved sidecar and the recommend response are one shape.

function api_correction_plan_presets(req::HTTP.Request)
    presets = Cecelia.CORRECTION_PRESETS
    ids     = sort!(collect(keys(presets)); by = String)
    rows    = Vector{Dict{String,Any}}(undef, length(ids))
    @inbounds for (i, id) in enumerate(ids)
        p = presets[id]
        rows[i] = Dict{String,Any}(
            "id"               => String(p.id),
            "name"             => p.name,
            "description"      => p.description,
            "orderHints"       => p.order_hints,
            "validationStatus" => String(p.validation_status),
        )
    end
    return 200, JSON3.write(rows)
end

# Shared body parser for the two POST endpoints — cardId is `Union{Symbol,Nothing}` (nothing =
# auto-pick), wizard values become Symbols (enum round-trip) so `_plan_from_dict` decodes the same
# shape whether the plan came from a live POST or from disk.
function _parse_recommend_body(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes))
    catch e
        return nothing, _gerr(400, "invalid JSON body: " * sprint(showerror, e))
    end
    return body, nothing
end

function _pick_card_and_wizard(body)
    card_id = let c = get(body, :cardId, nothing)
        c === nothing || (c isa AbstractString && isempty(c)) ? nothing : Symbol(String(c))
    end
    wizard = Dict{Symbol,Any}()
    let w = get(body, :wizard, nothing)
        if w isa AbstractDict
            for (k, v) in w
                wizard[Symbol(String(k))] = v isa AbstractString ? Symbol(String(v)) : v
            end
        end
    end
    return card_id, wizard
end

function api_correction_plan_recommend(req::HTTP.Request, body_bytes::Vector{UInt8})
    body, err = _parse_recommend_body(body_bytes)
    body === nothing && return err
    img, gerr = _gating_image(String(get(body, :projectUid, "")),
                              String(get(body, :imageUid, "")))
    img === nothing && return gerr

    card_id, wizard = _pick_card_and_wizard(body)
    plan = Cecelia.recommend_plan(img; card_id = card_id, wizard = wizard)
    return 200, JSON3.write(Cecelia._plan_to_dict(plan))
end

function api_correction_plan_save(req::HTTP.Request, body_bytes::Vector{UInt8})
    body, err = _parse_recommend_body(body_bytes)
    body === nothing && return err
    img, gerr = _gating_image(String(get(body, :projectUid, "")),
                              String(get(body, :imageUid, "")))
    img === nothing && return gerr

    card_id, wizard = _pick_card_and_wizard(body)
    plan = Cecelia.recommend_plan(img; card_id = card_id, wizard = wizard)
    Cecelia.save_plan(img, plan)
    return 200, JSON3.write(Cecelia._plan_to_dict(plan))
end

# GET is a query-param handler — matches the shape of every other /api/*?projectUid=&imageUid= route.
# Returns a wrapper object (not the bare plan) so the caller can distinguish "no plan on disk" from
# "plan exists but failed to load" without a second endpoint — and so `stale` travels with it.
function api_correction_plan_get(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, gerr = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    img === nothing && return gerr

    plan = Cecelia.load_plan(img)
    if plan === nothing
        return 200, JSON3.write(Dict{String,Any}("plan" => nothing, "exists" => false, "stale" => false))
    end
    # Fingerprint check against the image's CURRENT meta — a re-import shifts the fingerprint, and
    # that's the load-time flag the plan doc calls out (§8 provenance: the plan reads "still valid?"
    # from the sidecar, not from ceceliaVersion, because a re-import happens without a version bump).
    current_fp = ""
    try
        ccid = Cecelia.state_file(img)
        if isfile(ccid)
            raw  = Cecelia.read_ccid_raw(ccid)
            meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
            current_fp = Cecelia.saturation_fingerprint(meta)
        end
    catch
        # Fingerprinting can't fail meaningfully on a well-formed ccid; on a bad one, treat as "no
        # current fingerprint" (stale = false rather than a fabricated true).
    end
    stale = !isempty(plan.saturation_fingerprint) && !isempty(current_fp) &&
            plan.saturation_fingerprint != current_fp
    return 200, JSON3.write(Dict{String,Any}(
        "plan"   => Cecelia._plan_to_dict(plan),
        "exists" => true,
        "stale"  => stale,
    ))
end

# Mount = load the saved plan → ChainTemplate → save_chain_template!. Returns 409 if a chain with
# the target name already exists and `overwrite` was not sent — a re-mount overwrite is the expected
# flow, so the client's confirm-then-retry is the safety net for the case where a user (or another
# author) edited the chain by hand and would lose those edits.
function api_correction_plan_mount(req::HTTP.Request, body_bytes::Vector{UInt8})
    body, err = _parse_recommend_body(body_bytes)
    body === nothing && return err
    proj_uid = String(get(body, :projectUid, ""))
    img, gerr = _gating_image(proj_uid, String(get(body, :imageUid, "")))
    img === nothing && return gerr

    plan = Cecelia.load_plan(img)
    plan === nothing && return 409, JSON3.write((;
        error="No saved plan for this image — Save the plan first"))
    isempty(plan.included) && return 409, JSON3.write((;
        error="Nothing to mount — the plan has no included steps"))

    template = Cecelia.plan_to_chain_template(plan)
    try
        validate_chain_template(template)
    catch e
        e isa ChainTemplateError || rethrow()
        return 400, JSON3.write((; error="Plan translated to an invalid chain: $(e.msg)"))
    end

    overwrite = get(body, :overwrite, false) === true
    dir  = _chains_dir_for_project(proj_uid)
    path = joinpath(dir, "$(template.name).json")
    if isfile(path) && !overwrite
        return 409, JSON3.write((;
            error="Chain '$(template.name)' already exists — pass overwrite: true to replace it",
            name = template.name,
            existed = true,
        ))
    end

    created = !isfile(path)
    save_chain_template!(load_project(proj_uid), template)
    @info "Mounted correction plan to chain" name=template.name project=proj_uid nodes=length(template.nodes) created=created
    _broadcast_chains_updated(proj_uid)
    return 200, JSON3.write((;
        ok        = true,
        name      = template.name,
        nodeCount = length(template.nodes),
        created   = created,
    ))
end
