# ── /api/correction-plan/* — read-only surface for slice 3a of docs/todo/CORRECTION_QC_PLAN.md ─────
#
# Two endpoints:
#   GET  /api/correction-plan/presets                            → [{id,name,description,orderHints,validationStatus}, …]
#   POST /api/correction-plan/recommend { projectUid, imageUid,
#                                        cardId?, wizard? }      → the plan dict (`_plan_to_dict`)
#
# The recommend endpoint does NOT save. Slice 3b adds save/load; slice 3c the wizard; slice 3d the
# chain mount. Every response uses the plan.json field-name convention (`funName`, `orderWeight`, …)
# so the frontend types match plan.json 1:1 — the saved sidecar and the recommend response are one
# shape.

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

function api_correction_plan_recommend(req::HTTP.Request, body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes))
    catch e
        return _gerr(400, "invalid JSON body: " * sprint(showerror, e))
    end
    img, err = _gating_image(String(get(body, :projectUid, "")),
                             String(get(body, :imageUid, "")))
    img === nothing && return err

    card_id = let c = get(body, :cardId, nothing)
        c === nothing || (c isa AbstractString && isempty(c)) ? nothing : Symbol(String(c))
    end
    wizard = Dict{Symbol,Any}()
    let w = get(body, :wizard, nothing)
        if w isa AbstractDict
            for (k, v) in w
                # Wizard values are enums (`:yes`/`:no`/`:unknown`/`:resonance`/…) — string→Symbol so
                # the round-trip through the API matches how `plan_from_dict` decodes them.
                wizard[Symbol(String(k))] = v isa AbstractString ? Symbol(String(v)) : v
            end
        end
    end

    plan = Cecelia.recommend_plan(img; card_id = card_id, wizard = wizard)
    return 200, JSON3.write(Cecelia._plan_to_dict(plan))
end
