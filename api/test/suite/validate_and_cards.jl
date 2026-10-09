# /api/tasks/validate + /api/cell_cards + _require_ids testsets —
# extracted from api/test/runtests.jl.
#
# Three testsets (plus the behaviour-card ranking):
#  - `API: /api/tasks/validate — wiring` (form-time advisory dispatcher; validator logic
#    itself is pinned in the package suite).
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
            @test haskey(side, "pool") && haskey(side, "cards") && haskey(side, "stamp")
            @test haskey(side["stamp"], "clusterMtime") && haskey(side["stamp"], "specsMtime")
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

            # Reshuffle — `seed` > 0 on ONE pop swaps that card to another near-centre track and
            # leaves the others on their medoid. Consecutive seeds never repeat (fixed walk order).
            medoid_tids = [Int(c.medoid.track_id) for c in r1.cards]
            seeded(sd) = merge(req, Dict{String,Any}("pops" => [
                Dict("path"=>"/Scanning", "clusterIds"=>[0]),
                Dict("path"=>"/Directed", "clusterIds"=>[1], "seed"=>sd),
                Dict("path"=>"/Meandering", "clusterIds"=>[2])]))
            st_s1, body_s1 = call(seeded(1)); rs1 = JSON3.read(body_s1)
            @test st_s1 == 200
            s1_tids = [Int(c.medoid.track_id) for c in rs1.cards]
            @test s1_tids[2] != medoid_tids[2]
            @test s1_tids[[1, 3]] == medoid_tids[[1, 3]]
            side_s1 = JSON3.read(read(sidecar_path, String), Dict{String,Any})
            # stamp.pops rows are [path, ids, seed, name, colour], sorted by path.
            @test [(String(r[1]), Int(r[3])) for r in side_s1["stamp"]["pops"]] ==
                  [("/Directed", 1), ("/Meandering", 0), ("/Scanning", 0)]
            n_dir = Int(rs1.cards[2].n)
            if n_dir >= 3   # candidate floor of 3 → ≥2 non-medoid tracks → seed 2 is a third track
                s2_tid = Int(JSON3.read(call(seeded(2))[2]).cards[2].medoid.track_id)
                @test s2_tid ∉ (medoid_tids[2], s1_tids[2])
            end
            # Seed 0 again = back to the medoid (re-rendered, not a stale seeded sidecar).
            @test [Int(c.medoid.track_id) for c in JSON3.read(call(req)[2]).cards] == medoid_tids

            # Stamp: a pop recolour on disk invalidates the sidecar — a refetch must not serve the old
            # colour. The per-card memo keeps unchanged cards' render keys identical.
            keys_before = JSON3.read(read(sidecar_path, String), Dict{String,Any})["renderKeys"]
            gate_path = joinpath(dir, "testpr", "1", "KDIeEm", "gating", "B__trackclust.json")
            gdoc = JSON3.read(read(gate_path, String), Dict{String,Any})
            for gp in gdoc["populations"]
                gp["name"] == "Directed" && (gp["colour"] = "#123456")
            end
            write(gate_path, JSON3.write(gdoc))
            st_c, body_c = call(req)
            @test st_c == 200
            @test [String(c.colour) for c in JSON3.read(body_c).cards] == ["#4c78a8", "#123456", "#54a24b"]
            keys_after = JSON3.read(read(sidecar_path, String), Dict{String,Any})["renderKeys"]
            @test keys_after["/Scanning"] == keys_before["/Scanning"]
            @test keys_after["/Directed"] != keys_before["/Directed"]

            # valueName / suffix are joined onto labelProps/ and analysis/cell_cards/, so each must be
            # one path component. Unguarded, `../labelProps/B` and the absolute path both resolved to
            # the real run (200) and wrote the sidecar to analysis/labelProps/ and labelProps/.
            img_dir = joinpath(dir, "testpr", "1", "KDIeEm")
            for (field, bad) in (("valueName", "../labelProps/B"),
                                 ("valueName", joinpath(img_dir, "labelProps", "B")),
                                 ("valueName", ".."), ("suffix", "../movement"),
                                 ("suffix", "a\\b"))
                st_bad, body_bad = call(merge(req, Dict{String,Any}(field => bad)))
                @test st_bad == 400
                @test occursin(field, String(JSON3.read(body_bad).error))
            end
            @test !isdir(joinpath(img_dir, "analysis", "labelProps"))
            @test !isfile(joinpath(img_dir, "labelProps", "B__movement.json"))

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

# ── Behaviour cards: "show another example" ranking (all three families) ──────────────────────
# No committed fixture carries motif.class / live.cell.hmm.state.*, so the motif + HMM handlers are
# pinned at their pure ranking pieces; the cell-cards testset above covers the handler round-trip.
@testset "Behaviour cards: pick_card_example + motif/HMM example ranking" begin
    # Shared picker: seed 0 = medoid; seeds walk the top quartile (≥3) without repeats, then wrap.
    ranked = collect(1:20)                      # quartile = 5 → 4 non-medoid candidates
    @test pick_card_example(ranked, 0) == 1
    walk = [pick_card_example(ranked, sd) for sd in 1:4]
    @test sort(walk) == [2, 3, 4, 5]            # all distinct, all near the top, medoid excluded
    @test pick_card_example(ranked, 5) == walk[1]
    @test pick_card_example([7, 8], 1) == 8     # floor of 3 → a 2-member group still has an alternative
    @test pick_card_example([7], 3) == 7        # nothing else to show → the medoid

    # Motif: instances by mean distance, ties by id — medoid first, deterministic.
    @test _motif_ranked_instances(Dict(10 => 0.5, 11 => 0.2, 12 => 0.2, 13 => 0.9)) == [11, 12, 10, 13]

    # HMM: tracks by in-state fraction, then longest run. Track 1: 4/4 in state; track 2: 3/4 with
    # a 3-long run; track 3: 3/4 split into runs of 2+1; track 4: never in state (excluded).
    st  = Float64[1,1,1,1,  1,1,1,0,  1,0,1,1,  0,0,0,0]
    tid = Float64[1,1,1,1,  2,2,2,2,  3,3,3,3,  4,4,4,4]
    ts  = Float64[0,1,2,3,  0,1,2,3,  0,1,2,3,  0,1,2,3]
    m0 = _medoid_state_run(st, tid, ts, 1.0)
    @test m0.track_id == 1 && length(m0.run_rows) == 4
    alts = Set(_medoid_state_run(st, tid, ts, 1.0; seed = sd).track_id for sd in 1:2)
    @test alts == Set([2, 3])                   # the two other in-state tracks, never track 4
    @test length(_medoid_state_run(st, tid, ts, 1.0; seed = 1).run_rows) in (2, 3)
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

