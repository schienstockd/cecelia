# /api/tasks/validate + /api/correction-plan + /api/cell_cards + _require_ids testsets —
# extracted from api/test/runtests.jl.
#
# Four testsets:
#  - `API: /api/tasks/validate — wiring` (form-time advisory dispatcher; validator logic
#    itself is pinned in the package suite).
#  - `API: /api/correction-plan — wiring` (docs/todo/CORRECTION_QC_PLAN.md slice 3a).
#  - `API: /api/cell_cards — metadata + sidecar cache on the synthetic fixture`.
#  - `API: _require_ids returns 400 on missing/empty ids` (guard helper).
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── POST /api/tasks/validate — the generic form-time advisory endpoint ─────────────────
#
# The handler resolves images and hands off to `Cecelia.validate_param`; the validator's own logic
# is pinned in the PACKAGE suite (`_support_temporal_window_advisory`). Here we test the WIRING
# only: bad shapes → 4xx, unknown validator → 200 null, a registered validator round-trips.
@testset "API: /api/tasks/validate — wiring" begin
    _valpost(body) = api_task_validate(HTTP.Request("POST", "/api/tasks/validate"),
                                       Vector{UInt8}(JSON3.write(body)))

    # Bad JSON → 400.
    st, _ = api_task_validate(HTTP.Request("POST", "/api/tasks/validate"), Vector{UInt8}("{not json"))
    @test st == 400

    # Missing funName / paramKey → 400 each.
    st, _ = _valpost(Dict("paramKey" => "x", "value" => 1))
    @test st == 400
    st, _ = _valpost(Dict("funName" => "t", "value" => 1))
    @test st == 400

    # Unknown validator → 200, body is literal "null" (frontend renders nothing).
    st, body = _valpost(Dict("funName" => "no.such.task", "paramKey" => "no.such.key",
                             "value" => 1, "projectUid" => "", "imageUids" => []))
    @test st == 200
    @test body == "null"

    # Register a test validator that ignores images so we can round-trip without a project fixture,
    # then clean up. Same shape a real task file would register.
    Cecelia.register_param_validator!("test.validate.echo", "x",
        (v, _imgs, _sibs) -> (severity = "ok", message = "value=$v", tip = "echoed"))
    try
        st, body = _valpost(Dict("funName" => "test.validate.echo", "paramKey" => "x",
                                 "value" => 42, "projectUid" => "", "imageUids" => []))
        @test st == 200
        obj = JSON3.read(body)
        @test String(obj.severity) == "ok"
        @test occursin("42", String(obj.message))

        # `siblingValues` reaches the validator as an AbstractDict (the shape validators receive).
        Cecelia.register_param_validator!("test.validate.echo_siblings", "x",
            (_, _, sibs) -> (severity = "ok", message = "sib=" * String(get(sibs, "lr", "?")),
                             tip = "sib"))
        st, body = _valpost(Dict("funName" => "test.validate.echo_siblings", "paramKey" => "x",
                                 "value" => 1, "projectUid" => "", "imageUids" => [],
                                 "siblingValues" => Dict("lr" => "0.001")))
        @test st == 200
        @test occursin("sib=0.001", String(JSON3.read(body).message))

        # A throwing validator is swallowed by validate_param — 200 null, no 500.
        Cecelia.register_param_validator!("test.validate.throws", "x",
            (_, _, _) -> error("boom"))
        st, body = _valpost(Dict("funName" => "test.validate.throws", "paramKey" => "x",
                                 "value" => 1, "projectUid" => "", "imageUids" => []))
        @test st == 200
        @test body == "null"
    finally
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.echo", "x"))
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.echo_siblings", "x"))
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.throws", "x"))
    end
end

# ── /api/correction-plan/* — slice 3a of docs/todo/CORRECTION_QC_PLAN.md ─────────────────────────
#
# Wiring only. `recommend_plan` / `_plan_to_dict` are pinned in the package suite; here we test that
# the HTTP adapter (a) shapes errors correctly, (b) hands the wizard/card through as the right Julia
# types, and (c) returns the plan dict shape the frontend types are built against.
@testset "API: /api/correction-plan — wiring" begin
    # GET presets is fixture-free: it enumerates a constant registry.
    st, body = api_correction_plan_presets(HTTP.Request("GET", "/api/correction-plan/presets"))
    @test st == 200
    presets = JSON3.read(body)
    ids = Set(String(p.id) for p in presets)
    @test issubset(Set(["resonance", "galvo", "spinning_disk", "deep_3d", "custom"]), ids)
    # each row has the frontend-facing keys
    p1 = presets[1]
    @test hasproperty(p1, :name) && hasproperty(p1, :description)
    @test hasproperty(p1, :orderHints) && hasproperty(p1, :validationStatus)

    _recpost(body) = api_correction_plan_recommend(
        HTTP.Request("POST", "/api/correction-plan/recommend"), Vector{UInt8}(JSON3.write(body)))

    # Bad JSON → 400.
    st, _ = api_correction_plan_recommend(
        HTTP.Request("POST", "/api/correction-plan/recommend"), Vector{UInt8}("{not json"))
    @test st == 400
    # Missing projectUid → 400 (from _gating_image).
    st, _ = _recpost(Dict("imageUid" => "x"))
    @test st == 400
    # Unknown project → 404.
    st, _ = _recpost(Dict("projectUid" => "no-such", "imageUid" => "no-such"))
    @test st == 404

    # Round-trip against the standard testpr fixture (KDIeEm has T axis → driftCorrect included).
    proj_dir = api_fixture("testpr")
    if !api_have_fixture(proj_dir)
        @test_skip "testpr fixture missing"
    else
        dir = mktempdir()
        cp(proj_dir, joinpath(dir, "testpr"))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir

            # Default: no cardId → auto-picked via recommend_card (empty wizard → :custom).
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 200
            plan = JSON3.read(body)
            @test plan.planVersion == 1
            @test String(plan.imageUid) == "KDIeEm"
            @test String(plan.presetId) == "custom"
            @test hasproperty(plan, :included) && hasproperty(plan, :excluded)
            @test hasproperty(plan, :qcScores) && hasproperty(plan, :saturationFingerprint)
            # Each step carries the plan.json field names, not the Julia struct names.
            if !isempty(plan.included)
                s = plan.included[1]
                @test hasproperty(s, :funName) && hasproperty(s, :orderWeight)
                @test hasproperty(s, :source) && hasproperty(s, :params)
            end

            # cardId explicit → wins over auto-pick.
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                     "cardId" => "resonance"))
            @test st == 200
            @test String(JSON3.read(body).presetId) == "resonance"

            # Wizard W5=yes → recommend_card returns :deep_3d when no cardId.
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                     "wizard" => Dict("W5" => "yes")))
            @test st == 200
            @test String(JSON3.read(body).presetId) == "deep_3d"

            # ── slice 3b: /get and /save (round-trip through plan.json on disk) ─────────
            _get(qs) = api_correction_plan_get(HTTP.Request("GET", "/api/correction-plan/get?$qs"))
            _save(body) = api_correction_plan_save(HTTP.Request("POST", "/api/correction-plan/save"),
                                                   Vector{UInt8}(JSON3.write(body)))

            # Fresh fixture — no plan.json yet.
            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test st == 200
            got = JSON3.read(body)
            @test got.exists === false
            @test got.plan === nothing
            @test got.stale === false

            # Save with a card, then GET reads it back.
            st, saved_body = _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                         "cardId" => "resonance"))
            @test st == 200
            saved = JSON3.read(saved_body)
            @test String(saved.presetId) == "resonance"

            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test st == 200
            got = JSON3.read(body)
            @test got.exists === true
            @test got.stale === false                                     # same meta ⇒ fingerprint matches
            @test String(got.plan.presetId) == "resonance"

            # Save with a different card overwrites plan.json.
            _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm", "cardId" => "custom"))
            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test String(JSON3.read(body).plan.presetId) == "custom"

            # /get errors mirror /recommend errors (missing / unknown project).
            st, _ = _get("imageUid=x")
            @test st == 400
            st, _ = _get("projectUid=no-such&imageUid=no-such")
            @test st == 404

            # Bad JSON on /save → 400 (mirrors /recommend).
            st, _ = api_correction_plan_save(
                HTTP.Request("POST", "/api/correction-plan/save"), Vector{UInt8}("{nope"))
            @test st == 400

            # ── slice 3d: /mount (plan.json → chain template on disk) ──────────────────
            _mount(body) = api_correction_plan_mount(HTTP.Request("POST", "/api/correction-plan/mount"),
                                                     Vector{UInt8}(JSON3.write(body)))

            # KDIeEm has no SizeT in its shipped meta → axis.T_present = 0 → driftCorrect excluded →
            # empty plan (nothing to mount). Bump SizeT on-disk so mount has real work to do; re-save
            # the plan so the new fingerprint matches. This is scoped to the mount half of the
            # testset — the /get/save assertions above already used the unmodified fixture.
            ccid_path = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
            let raw = JSON3.read(read(ccid_path, String), Dict{String,Any})
                raw["meta"] = merge(get(raw, "meta", Dict{String,Any}()), Dict("SizeT" => 100))
                open(io -> JSON3.pretty(io, raw), ccid_path, "w")
            end
            _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm", "cardId" => "resonance"))

            # First mount → creates a new chain. Chain name is fixed per-image.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 200
            mounted = JSON3.read(body)
            @test mounted.ok === true
            @test String(mounted.name) == "correction-plan-KDIeEm"
            @test mounted.nodeCount >= 1                     # T-present → at least driftCorrect
            @test mounted.created === true

            # Chain landed on disk in the project's chains dir.
            chains_dir = joinpath(dir, "testpr", "settings", "chains")
            chain_path = joinpath(chains_dir, "correction-plan-KDIeEm.json")
            @test isfile(chain_path)

            # Second mount without overwrite → 409 conflict.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 409
            conflict = JSON3.read(body)
            @test conflict.existed === true
            @test String(conflict.name) == "correction-plan-KDIeEm"

            # With overwrite: true → replaces, created=false.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "overwrite" => true))
            @test st == 200
            @test JSON3.read(body).created === false

            # No saved plan → 409 with an actionable message (delete plan.json to prove it).
            plan_path = joinpath(dir, "testpr", "1", "KDIeEm", "plan.json")
            rm(plan_path)
            rm(chain_path)                                   # so the conflict path can't mask the missing-plan error
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 409
            @test occursin("Save the plan first", String(JSON3.read(body).error))

            # Bad JSON, missing project, unknown project → mirror /save wiring.
            st, _ = api_correction_plan_mount(
                HTTP.Request("POST", "/api/correction-plan/mount"), Vector{UInt8}("{nope"))
            @test st == 400
            st, _ = _mount(Dict("imageUid" => "x"))
            @test st == 400
            st, _ = _mount(Dict("projectUid" => "no-such", "imageUid" => "no-such"))
            @test st == 404
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

# ── /api/cell_cards — metadata + sidecar cache ──────────────────────────────────
# Exercises the pool-first pipeline on the synthetic clustering fixture (`docs/todo/CELL_CARDS_PLAN.md`
# Decision 0). No OME-Zarr is on disk for `KDIeEm`, so the filmstrip PNGs deliberately come back
# empty — the card metadata (medoid triple, stats, pop name/colour/n) is what this pins.
@testset "API: /api/cell_cards — metadata + sidecar cache on the synthetic fixture" begin
    h5    = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    sidec = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B__tracks.clustfeatures.json")
    gate  = api_fixture("testpr", "1", "KDIeEm", "gating", "B__trackclust.json")
    if !(api_have_fixture(h5) && api_have_fixture(sidec) && api_have_fixture(gate))
        @test_skip "cell-cards fixture missing (see test-data/README.md)"
    else
        dir = mktempdir()
        cp(api_fixture("testpr"), joinpath(dir, "testpr"))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir

            call(body) = api_cell_cards(Vector{UInt8}(JSON3.write(body)))
            req = Dict{String,Any}("projectUid" => "testpr", "rootUid" => "KDIeEm",
                                    "valueName" => "B", "suffix" => "movement",
                                    "pops" => [
                                        Dict("path"=>"/Scanning", "clusterIds"=>[0]),
                                        Dict("path"=>"/Directed", "clusterIds"=>[1]),
                                        Dict("path"=>"/Meandering", "clusterIds"=>[2])])
            st, body = call(req)
            @test st == 200
            resp = JSON3.read(body)

            # Pool is the single-image pool of one (fixture has partOf=["KDIeEm"]).
            @test length(resp.pool) == 1
            @test String(resp.pool[1].uid) == "KDIeEm"
            @test String(resp.pool[1].value_name) == "B"

            # Three cards in request order; each carries a medoid triple pinning (uid, vn, track_id).
            @test length(resp.cards) == 3
            names   = [String(c.name)   for c in resp.cards]
            colours = [String(c.colour) for c in resp.cards]
            @test names   == ["Scanning", "Directed", "Meandering"]
            @test colours == ["#4c78a8", "#f58518", "#54a24b"]
            @test all(c -> Int(c.n) > 0, resp.cards)             # every pop has rows in the pool
            @test all(c -> haskey(c.medoid, :uid) && haskey(c.medoid, :value_name)
                        && haskey(c.medoid, :track_id), resp.cards)
            @test all(c -> String(c.medoid.uid) == "KDIeEm", resp.cards)
            @test all(c -> String(c.medoid.value_name) == "B", resp.cards)
            # Different pops must pick different medoid tracks — the medoid picker collapsed cluster
            # separation once during Phase 1 development (a subset that copied by ref); pin it.
            tids = Set(Int(c.medoid.track_id) for c in resp.cards)
            @test length(tids) == 3

            # Stats footer carries median + IQR for every canonical measure the tracks table has.
            @test all(c -> length(c.stats) >= 10, resp.cards)
            first_stat = resp.cards[1].stats[1]
            @test String(first_stat.name) == "live.track.speed"
            @test first_stat.q25 <= first_stat.median <= first_stat.q75

            # Filmstrip is EMPTY on this fixture (no OME-Zarr on disk) — the metadata still lands.
            @test all(c -> isempty(c.filmstrip), resp.cards)

            # Sidecar written under analysis/cell_cards/{value_name}__{suffix}.json.
            sidecar_path = joinpath(dir, "testpr", "1", "KDIeEm", "analysis", "cell_cards",
                                     "B__movement.json")
            @test isfile(sidecar_path)
            side = JSON3.read(read(sidecar_path, String), Dict{String,Any})
            @test haskey(side, "pool") && haskey(side, "cards") && haskey(side, "clusterMtime")
            @test length(side["cards"]) == 3

            # valueName omitted → server derives from co_clustered_value_names(suffix). Same medoid
            # triples land — a Phase 2 change that lets the cluster panel skip plumbing valueName.
            no_vn = Dict{String,Any}("projectUid" => "testpr", "rootUid" => "KDIeEm",
                                     "suffix" => "movement",
                                     "pops" => [
                                         Dict("path"=>"/Scanning", "clusterIds"=>[0]),
                                         Dict("path"=>"/Directed", "clusterIds"=>[1]),
                                         Dict("path"=>"/Meandering", "clusterIds"=>[2])])
            st_dv, body_dv = call(no_vn)
            @test st_dv == 200
            rdv = JSON3.read(body_dv)
            @test length(rdv.cards) == 3
            @test [Int(c.medoid.track_id) for c in rdv.cards] ==
                  [Int(c.medoid.track_id) for c in JSON3.read(body).cards]

            # Cache hit — second call with unchanged mtime returns the same PARSED content.
            # Byte comparison would drift with Julia Dict key order + int/float encoding; parse first.
            st2, body2 = call(req)
            @test st2 == 200
            r1 = JSON3.read(body);  r2 = JSON3.read(body2)
            @test length(r1.cards) == length(r2.cards)
            @test [String(c.name) for c in r1.cards] == [String(c.name) for c in r2.cards]
            @test [Int(c.medoid.track_id) for c in r1.cards] ==
                  [Int(c.medoid.track_id) for c in r2.cards]

            # Bad body → 400.
            st3, _ = api_cell_cards(Vector{UInt8}("{not json"))
            @test st3 == 400
            st4, _ = call(Dict{String,Any}("projectUid" => "testpr"))
            @test st4 == 400
            # Unknown suffix → 404.
            st5, _ = call(merge(req, Dict{String,Any}("suffix" => "no-such")))
            @test st5 == 404
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

# gating_api._require_ids — audit task #33. Sites that used to hand-roll `body["projectUid"]`
# now go through this helper so a missing id is a 400 with the field name, never a bare
# KeyError. Both empty AND absent should behave the same.
@testset "API: _require_ids returns 400 on missing/empty ids" begin
    # gating_api.jl is already included by server.jl at the top of this file, so `_require_ids`
    # is available at top level.
    let body = Dict{String,Any}("projectUid" => "p", "imageUid" => "i")
        pu, iu, err = _require_ids(body)
        @test err === nothing && pu == "p" && iu == "i"
    end
    let body = Dict{String,Any}("imageUid" => "i")   # projectUid absent
        _, _, err = _require_ids(body)
        @test err !== nothing
        st, msg = err
        @test st == 400
        @test occursin("projectUid", msg)             # field name in the message
    end
    let body = Dict{String,Any}("projectUid" => "", "imageUid" => "i")   # empty
        _, _, err = _require_ids(body)
        @test err !== nothing && err[1] == 400 && occursin("projectUid", err[2])
    end
    let body = Dict{String,Any}("projectUid" => "p", "imageUid" => "")   # empty
        _, _, err = _require_ids(body)
        @test err !== nothing && err[1] == 400 && occursin("imageUid", err[2])
    end
end

