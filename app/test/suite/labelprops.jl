# ── BayesianTracking param validation ─────────────────────────────────────
@testset "BayesianTracking params" begin
    task = BayesianTracking()
    good = Dict{String,Any}(
        "valueName" => "default", "popsToTrack" => "NONE",
        "maxSearchRadius" => 20, "maxLost" => 3, "trackBranching" => false,
        "minTimepoints" => 5, "accuracy" => 0.8, "probToAssign" => 0.8,
        # advanced section params are flattened by the frontend before submit
        "noiseInital" => 300, "distThresh" => 10.0, "segmentationMissRate" => 0.1,
    )
    @test begin validate_params(task, good); true end
    # maxSearchRadius max is 200
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("maxSearchRadius" => 500)))
    # segmentationMissRate min is 0.001
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("segmentationMissRate" => 0.0)))
end

# ── TrackMeasures param validation ────────────────────────────────────────
@testset "TrackMeasures params" begin
    task = TrackMeasures()
    @test begin validate_params(task,
        Dict{String,Any}("valueName" => "B", "forceRecompute" => false)); true end
    # forceRecompute is bool — a non-bool must be rejected. Also a guard that params use
    # "key" (not "id"): with "id" the spec key resolves empty and validation silently skips.
    @test_throws ParamValidationError validate_params(
        task, Dict{String,Any}("valueName" => "B", "forceRecompute" => "yes"))
end

# ── Labels field round-trip ───────────────────────────────────────────────
# Regression guard: the `labels` Dict written by cellposeSegment must survive
# save!/init_object and land at the agreed location in ccid.json.
@testset "Labels field round-trip" begin
    proj = create_project!(name="labels-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.labels["default"] = ["default.zarr", "default_nuc.zarr"]
    save!(img)

    # Reloaded value is correct in memory
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test haskey(r.labels, "default")
    @test r.labels["default"] == ["default.zarr", "default_nuc.zarr"]

    # On-disk shape: top-level "labels" dict with string-vector values
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "labels")
    label_val = collect(String, raw["labels"]["default"])
    @test label_val == ["default.zarr", "default_nuc.zarr"]

    rm(proj.root; recursive=true)
end

# ── CompositeTask — spec loads and composite array is correct ─────────────
@testset "CompositeTask spec" begin
    task = CompositeTask("segment.cellposeMeasure")
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    @test haskey(spec, "composite")
    steps = [string(s) for s in spec["composite"]]
    @test steps == ["segment.cellpose", "segment.measureLabels"]
    @test get(spec, "fun_name", "") == "segment.cellposeMeasure"
end

@testset "a composite does not validate the params it derives itself" begin
    # `behaviour.hmm` = hmm_states → hmm_transitions. `hmm_transitions.hmmStates` is
    # `required` AND `hideInComposite`: the definitions route strips it from the merged form
    # (api/src/routes.jl), and the composite threads the states step's `stateColumn` into it in
    # `_run_task` — AFTER validation. Validating the step standalone-style therefore failed EVERY
    # composite run on "Select the state columns — run HMM states first", naming a field the form
    # does not have. Reported on a real set; reproduced here without one, because validation is
    # pure.
    #
    # Both halves matter. `hideInComposite` is skipped only IN a composite — run the step on its
    # own (a chain node, a REPL call) and the requirement is real, because then nothing supplies it.
    base = Dict{String,Any}("pops" => ["A/tracked"], "colName" => "default",
                            "modelMeasurements" => ["live.cell.speed", "live.cell.angle"],
                            "numStates" => 2)
    hmm   = Cecelia._task_from_fun_name("behaviour.hmm")
    trans = Cecelia._task_from_fun_name("behaviour.hmm_transitions")

    @test validate_params(hmm, copy(base)) === nothing            # composite: hmmStates derived
    @test_throws ParamValidationError validate_params(trans, copy(base))   # standalone: required
    @test validate_params(trans, merge(base,
        Dict{String,Any}("hmmStates" => ["live.cell.hmm.state.default"]))) === nothing

    # ABSENT and EMPTY are the same thing for a multi-pick (that is the rule `required` encodes),
    # and a chain node saved off the standalone form carries the spec default `[]`. Both must pass
    # in the composite, or the fix only covers the module-page path.
    @test validate_params(hmm, merge(base, Dict{String,Any}("hmmStates" => Any[]))) === nothing
end

@testset "a task's own validate_params overload survives a keyword call" begin
    # Keywords do not participate in dispatch: a keyword-LESS method is skipped outright when the
    # caller passes one, and the call falls through to the `::CciaTask` fallback — silently, no
    # error. So `validate_params(task, p; extra_options=…)` (chain template validation) ran the
    # spec half only and never the task's own check. Same hole would have swallowed `in_composite`.
    tc = Cecelia._task_from_fun_name("tracking.correct")
    bad = Dict{String,Any}("trackOps" => "nonsense!!")
    @test_throws ParamValidationError validate_params(tc, copy(bad))
    @test_throws ParamValidationError validate_params(tc, copy(bad); extra_options = Set{String}())
    @test_throws ParamValidationError validate_params(tc, copy(bad); in_composite = true)

    # Every overload of `validate_params` must therefore accept keywords — declared, or `kwargs...`.
    for m in methods(Cecelia.validate_params)
        @test !isempty(Base.kwarg_decl(m))
    end
end

# ── $include fragment resolution ──────────────────────────────────────────
# Verifies that {"$include": "imageTiling"} in cellpose.json is expanded
# to the 4 shared tiling params (blockSize, overlap, blockSizeZ, overlapZ).
@testset "\$include fragment resolution" begin
    task = CellposeSegment()
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    # Find the imageTiling section
    tiling_sec = nothing
    for p in spec["params"]
        p isa AbstractDict && string(get(p, "key", "")) == "imageTiling" &&
            (tiling_sec = p; break)
    end
    @test !isnothing(tiling_sec)
    tiling_params = tiling_sec["params"]
    keys_in_tiling = [string(get(p, "key", "")) for p in tiling_params if p isa AbstractDict]
    # Fragment contributes these 4; cellpose.json adds labelOverlap
    @test "blockSize"  ∈ keys_in_tiling
    @test "overlap"    ∈ keys_in_tiling
    @test "blockSizeZ" ∈ keys_in_tiling
    @test "overlapZ"   ∈ keys_in_tiling
    @test "labelOverlap" ∈ keys_in_tiling
    @test length(keys_in_tiling) == 5   # 4 from fragment + 1 inline
    # No raw $include entries should survive
    @test !any(p isa AbstractDict && haskey(p, "\$include") for p in tiling_params)
end

# ── label_props field round-trip ──────────────────────────────────────────
@testset "label_props field round-trip" begin
    proj = create_project!(name="lp-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.label_props["default"] = "default.h5ad"
    save!(img)

    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test get(r.label_props, "default", nothing) == "default.h5ad"

    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "label_props")
    @test string(raw["label_props"]["default"]) == "default.h5ad"

    rm(proj.root; recursive=true)
end

# ── Param validation — AfCorrect (group with flat sub-params) ─────────────

# ── Param validation — DriftCorrect ───────────────────────────────────────

# ── Versioned helpers ─────────────────────────────────────────────────────
@testset "Versioned dict helpers" begin
    d = Dict{String,Any}()
    versioned_set_field!(d, "filepath", "ccidImage.ome.zarr")
    @test versioned_get_field(d, "filepath") == "ccidImage.ome.zarr"
    @test versioned_active(d["filepath"]) == "default"

    versioned_set_field!(d, "filepath", "ccidDriftCorrected.ome.zarr", "driftCorrected")
    @test versioned_get_field(d, "filepath", "driftCorrected") == "ccidDriftCorrected.ome.zarr"
    @test versioned_get_field(d, "filepath") == "ccidDriftCorrected.ome.zarr"  # active = driftCorrected

    versioned_set_field!(d, "filepath", nothing, "driftCorrected")
    @test isnothing(get(d["filepath"], "driftCorrected", nothing))
end

# ── Inner-version helpers ─────────────────────────────────────────────────
# Symmetry with the outer versioned_* set above. The composer
# `versioned_get_field_at` walks BOTH axes and is what P1b/c/d readers will
# migrate to. Legacy (bare scalar) entries must keep resolving as implicit v1.
# Full design: docs/todo/VN_VERSIONING_PLAN.md.
@testset "Inner version helpers" begin
    # bare version dict — the inner shape used by P2 writers
    v = Dict{String,Any}("v1" => "a.zarr", "v2" => "b.zarr", LATEST_ACTIVE_KEY => "v2")
    @test is_versioned_entry(v)
    @test version_latest(v) == "v2"
    @test version_get(v) == "b.zarr"              # latest
    @test version_get(v, "v1") == "a.zarr"        # explicit
    @test isnothing(version_get(v, "v3"))         # missing
    @test sort(version_keys(v)) == ["v1", "v2"]   # excludes _latest

    # set adds a new version and moves _latest
    version_set!(v, "c.zarr", "v3")
    @test version_latest(v) == "v3"
    @test version_get(v) == "c.zarr"
    @test sort(version_keys(v)) == ["v1", "v2", "v3"]

    # set with set_latest=false leaves _latest alone
    version_set!(v, "d.zarr", "v4"; set_latest = false)
    @test version_latest(v) == "v3"
    @test version_get(v, "v4") == "d.zarr"

    # remove entry — mirrors versioned_set!'s NULL behaviour
    version_set!(v, nothing, "v4")
    @test isnothing(get(v, "v4", nothing))
    @test version_latest(v) == LATEST_DEFAULT_VAL

    # default: empty dict resolves to v1 implicitly
    empty_d = Dict{String,Any}()
    @test version_latest(empty_d) == "v1"
    @test isnothing(version_get(empty_d))
    @test !is_versioned_entry(empty_d)             # no _latest → not a versioned entry
    @test !is_versioned_entry("scalar")            # not a dict
    @test !is_versioned_entry([1, 2])              # not a dict
end

# P3 chain-pinning parser — the one canonical way to lift `params["version"]` off the raw bag into a
# typed `Union{String,Nothing}` Params field. The `""` / `nothing` / missing / non-string cases all
# collapse to `nothing` (follow `_latest`), matching the composer's default. Full plan:
# docs/todo/VN_VERSIONING_PLAN.md → P3 + the ratchets in `vn_pilot_writer.jl`.
@testset "parse_version_pin — canonical params.version parser" begin
    @test isnothing(parse_version_pin(Dict{String,Any}()))                            # absent
    @test isnothing(parse_version_pin(Dict{String,Any}("version" => nothing)))        # explicit nothing
    @test isnothing(parse_version_pin(Dict{String,Any}("version" => "")))             # empty string
    @test isnothing(parse_version_pin(Dict{String,Any}("version" => missing)))        # JSON3 null → missing
    @test isnothing(parse_version_pin(Dict{String,Any}("version" => 42)))             # non-string, defensive
    @test parse_version_pin(Dict{String,Any}("version" => "v1")) == "v1"
    @test parse_version_pin(Dict{String,Any}("version" => "v42")) == "v42"
    @test parse_version_pin(Dict{String,Any}("version" => "draft")) == "draft"        # hand-labelled vn
end

@testset "versioned_get_field_at — legacy + new shape" begin
    # LEGACY shape (bare scalar): the composer returns the scalar unchanged
    legacy = Dict{String,Any}(
        "filepath" => Dict{String,Any}("default" => "ccidImage.ome.zarr",
                                       VERSIONED_ACTIVE_KEY => "default"),
    )
    @test versioned_get_field_at(legacy, "filepath") == "ccidImage.ome.zarr"
    @test versioned_get_field_at(legacy, "filepath", "default") == "ccidImage.ome.zarr"
    @test isnothing(versioned_get_field_at(legacy, "filepath", "nonexistent"))
    @test isnothing(versioned_get_field_at(legacy, "missing_field"))

    # NEW shape (versioned inner dict): the composer walks BOTH axes
    new_shape = Dict{String,Any}(
        "filepath" => Dict{String,Any}(
            "default" => Dict{String,Any}("v1" => "a.zarr", "v2" => "b.zarr",
                                          LATEST_ACTIVE_KEY => "v2"),
            VERSIONED_ACTIVE_KEY => "default",
        ),
    )
    @test versioned_get_field_at(new_shape, "filepath") == "b.zarr"                # latest via active vn
    @test versioned_get_field_at(new_shape, "filepath", "default") == "b.zarr"
    @test versioned_get_field_at(new_shape, "filepath"; version = "v1") == "a.zarr"
    @test versioned_get_field_at(new_shape, "filepath", "default"; version = "v2") == "b.zarr"
    @test isnothing(versioned_get_field_at(new_shape, "filepath"; version = "v3"))  # missing version

    # MIXED shape (one vn legacy, one vn new): each resolves independently
    mixed = Dict{String,Any}(
        "filepath" => Dict{String,Any}(
            "default"        => "old.zarr",                                    # legacy
            "driftCorrected" => Dict{String,Any}("v1" => "d1.zarr", "v2" => "d2.zarr",
                                                 LATEST_ACTIVE_KEY => "v2"),   # new
            VERSIONED_ACTIVE_KEY => "driftCorrected",
        ),
    )
    @test versioned_get_field_at(mixed, "filepath") == "d2.zarr"                    # active → new-shape latest
    @test versioned_get_field_at(mixed, "filepath", "default") == "old.zarr"        # legacy branch
    @test versioned_get_field_at(mixed, "filepath", "driftCorrected"; version = "v1") == "d1.zarr"

    # Symbol keys — JSON3 hands us Symbol keys on read; the helpers must not silently miss.
    # Mirrors the guard in versioned_get / versioned_get_field.
    sym_new = Dict{String,Any}(
        "filepath" => Dict{Symbol,Any}(
            :default => Dict{Symbol,Any}(:v1 => "sym1.zarr", :v2 => "sym2.zarr",
                                         Symbol(LATEST_ACTIVE_KEY) => "v2"),
            Symbol(VERSIONED_ACTIVE_KEY) => "default",
        ),
    )
    @test versioned_get_field_at(sym_new, "filepath") == "sym2.zarr"
    @test versioned_get_field_at(sym_new, "filepath"; version = "v1") == "sym1.zarr"
end

@testset "unversion_value — one-arg composer for a resolved inner value" begin
    # LEGACY: bare scalar / vector passes through unchanged
    @test unversion_value("bare.zarr") == "bare.zarr"
    @test unversion_value(["a.zarr", "b.zarr"]) == ["a.zarr", "b.zarr"]
    @test unversion_value(nothing) === nothing

    # NEW shape: versioned entry unwraps to the latest (or specified) version
    ve_scalar = Dict{String,Any}("v1" => "a.zarr", "v2" => "b.zarr", LATEST_ACTIVE_KEY => "v2")
    @test unversion_value(ve_scalar) == "b.zarr"
    @test unversion_value(ve_scalar, "v1") == "a.zarr"
    @test isnothing(unversion_value(ve_scalar, "v3"))

    # NEW shape with Vector leaf (labels shape after P2 widening)
    ve_vec = Dict{String,Any}("v1" => ["a.zarr"], "v2" => ["a.zarr", "b.zarr"],
                              LATEST_ACTIVE_KEY => "v2")
    @test unversion_value(ve_vec) == ["a.zarr", "b.zarr"]
    @test unversion_value(ve_vec, "v1") == ["a.zarr"]

    # Legacy branch: a plain AbstractDict WITHOUT _latest is not a versioned entry — return as-is.
    plain_dict = Dict{String,Any}("foo" => 1)
    @test unversion_value(plain_dict) === plain_dict
end

# ── P2 infra: mint next version + guarded writer + upgrade-in-place ─────────
# The primitive that turns the versioning scheme from routing-only (P1) into
# an actual writer (P2). Three responsibilities: pick the next `vN` key
# without colliding with anything already there; refuse to overwrite (D6);
# lazily wrap a legacy scalar so a task's second-ever write turns it into a
# versioned entry without needing a schema migration.
@testset "version_next — pick the next unused vN" begin
    # Empty entry mints v1.
    @test version_next(Dict{String,Any}()) == "v1"

    # Sequential mint from the running max, ignoring `_latest`.
    d1 = Dict{String,Any}("v1" => "a", LATEST_ACTIVE_KEY => "v1")
    @test version_next(d1) == "v2"

    # Non-monotonic ordering — pick max+1, not len+1.
    d2 = Dict{String,Any}("v1" => "a", "v3" => "c", "v2" => "b", LATEST_ACTIVE_KEY => "v3")
    @test version_next(d2) == "v4"

    # Non-numeric keys are ignored — a hand-labelled "draft" doesn't skew the mint.
    d3 = Dict{String,Any}("v1" => "a", "draft" => "x", LATEST_ACTIVE_KEY => "v1")
    @test version_next(d3) == "v2"

    # `_latest` alone (no vN yet) still mints v1.
    d4 = Dict{String,Any}(LATEST_ACTIVE_KEY => "v1")
    @test version_next(d4) == "v1"
end

@testset "version_write! — guarded writer (D6, can't-overwrite)" begin
    d = Dict{String,Any}()

    # First write on an empty dict mints v1 and sets `_latest`.
    @test version_write!(d, "a.zarr") == "v1"
    @test d["v1"] == "a.zarr"
    @test version_latest(d) == "v1"

    # Subsequent writes mint the next key and move `_latest`.
    @test version_write!(d, "b.zarr") == "v2"
    @test version_latest(d) == "v2"
    @test version_get(d) == "b.zarr"

    # An EXPLICIT `version` kwarg pins the key — but only if it doesn't exist.
    @test version_write!(d, "z.zarr"; version = "v9") == "v9"
    @test version_latest(d) == "v9"

    # Guard fires on collision — critical D6 invariant.
    @test_throws ErrorException version_write!(d, "OOPS"; version = "v1")
    @test d["v1"] == "a.zarr"     # unchanged
    @test version_latest(d) == "v9" # unchanged

    # After the mint, the next mint still picks max+1 (the guard didn't
    # accidentally register the failed key).
    @test version_next(d) == "v10"
end

@testset "versioned_upgrade_entry! — legacy scalar → versioned entry on first v2 write" begin
    # OUTER dict — the shape the raw ccid.json (or an img field) carries. Two value_names:
    # `default` is legacy (bare scalar), `dtype` is already versioned.
    outer = Dict{String,Any}(
        "default"            => "ccidImage.ome.zarr",
        "dtype"              => Dict{String,Any}("v1" => "dtype.zarr", LATEST_ACTIVE_KEY => "v1"),
        VERSIONED_ACTIVE_KEY => "default",
    )

    # Upgrade `default` — wraps the bare scalar as v1 in place.
    upgraded = versioned_upgrade_entry!(outer, "default")
    @test upgraded isa AbstractDict
    @test upgraded["v1"] == "ccidImage.ome.zarr"
    @test upgraded[LATEST_ACTIVE_KEY] == "v1"
    @test outer["default"] === upgraded          # mutated in place

    # Second call is idempotent — already versioned, returns unchanged.
    same = versioned_upgrade_entry!(outer, "default")
    @test same === upgraded

    # The upgrade path composes with `version_write!` — the whole point.
    version_write!(upgraded, "v2.zarr")
    @test upgraded["v2"] == "v2.zarr"
    @test version_latest(upgraded) == "v2"

    # An entry that's already versioned is untouched by upgrade.
    dtype = versioned_upgrade_entry!(outer, "dtype")
    @test dtype["v1"] == "dtype.zarr"

    # Absent value_name errors — nothing to upgrade means the caller has a bug.
    @test_throws ErrorException versioned_upgrade_entry!(outer, "nonexistent")
end

# ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────
@testset "LabelProps reader" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps reader (fixture missing)"
    else
        # metadata (cheap reads)
        @test length(col_names(label_props(h5); data_type=:vars)) == 27
        @test channel_columns(label_props(h5)) ==
              ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2", "mean_intensity_3"]
        @test centroid_columns(label_props(h5)) == ["centroid_z", "centroid_y", "centroid_x"]
        @test temporal_columns(label_props(h5)) == ["centroid_t"]
        # order= selects BY AXIS (present only), never positionally
        @test centroid_columns(label_props(h5); order=[:x, :y, :z]) == ["centroid_x", "centroid_y", "centroid_z"]
        @test centroid_columns(label_props(h5); order=[:x, :y]) == ["centroid_x", "centroid_y"]

        # channel-name selection: request an intensity column by its CHANNEL name and the reader
        # resolves it to the raw {measure}_intensity_{i} column, returning it under the channel name.
        # This is what lets pop_df(...; pop_cols=["<channel>"]) work. channel_names is positional:
        # index i ↔ chans[i+1], so "chC" == mean_intensity_2.
        let lpc = label_props(h5; channel_names=["chA", "chB", "chC", "chD"])
            d = lpc |> select_cols(["chC"]) |> as_df
            @test "chC" in names(d)                                   # returned under the requested name
            @test !("mean_intensity_2" in names(d))                   # not the raw name
            raw = label_props(h5) |> select_cols(["mean_intensity_2"]) |> as_df
            @test d.chC == raw.mean_intensity_2                       # same underlying column
        end
        # raw names still resolve (gates/clustering pass raw) — unchanged behaviour
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["mean_intensity_2"]) |> as_df) == ["label", "mean_intensity_2"]
        # a genuinely unknown name is still ignored (not resolved to anything)
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["nope"]) |> as_df) == ["label"]

        # full frame: label + 27 vars + 3 spatial + 1 temporal + 8 obs (track lineage +
        # live.cell.* from tracking.track_measures) = 40 cols, 1377 rows
        df = label_props(h5) |> as_df
        @test size(df) == (1377, 40)
        # n_obs is the cheap dims-only count — must agree with the materialised row count
        @test n_obs(label_props(h5)) == 1377
        @test "label" in names(df)
        @test eltype(df.label) == Int64
        @test df.label[1:5] == [0, 1, 2, 3, 4]

        # X orientation correctness (audited values).
        # NOTE: intentional coupling to the committed fixture state of KDIeEm/B.h5ad — these
        # are the actual bbox values in that file, asserting /X is read with correct row/col
        # orientation (not transposed). If this breaks, it's either (a) the reader regressed,
        # or (b) the fixture was deliberately regenerated (e.g. segmentation rerun) — in which
        # case re-audit and update these constants. A failure here is NOT "the test is wrong".
        @test [df[1, "bbox-$j"] for j in 0:4] == Float32[0, 0, 71, 2, 29]
        @test [df[2, "bbox-$j"] for j in 0:4] == Float32[0, 7, 368, 4, 38]

        # is_tracked's signal: a tracked segmentation carries a track_id obs column (KDIeEm/B is
        # tracked). track_props / the track-grained gating plots key off this to say "track first"
        # (empty) instead of erroring when it's absent.
        obs = col_names(label_props(h5); data_type=:obs)
        @test "track_id" in obs
        @test !("not_a_column" in obs)

        # lazy column selection — only requested columns (+ label) are returned
        @test names(label_props(h5) |> select_cols(["area"]) |> as_df) == ["label", "area"]

        # centroid + intensity selection
        @test Set(names(label_props(h5) |> select_cols(["mean_intensity_0"]) |> view_centroid_cols |> as_df)) ==
              Set(["label", "mean_intensity_0", "centroid_z", "centroid_y", "centroid_x", "centroid_t"])

        # row filter by label
        d4 = label_props(h5) |> filter_rows([0, 1, 2]; by=:label) |> as_df
        @test sort(d4.label) == [0, 1, 2]

        # filter is intersection: nonexistent IDs are silently skipped (≤ requested, no NaN/error)
        d4b = label_props(h5) |> filter_rows([0, 1, 999_999]; by=:label) |> as_df
        @test sort(d4b.label) == [0, 1]

        # sort by area, descending
        d5 = label_props(h5) |> select_cols(["area"]) |> sort_by("area"; rev=true) |> as_df
        @test d5.area[1] == maximum(d5.area)
    end
end

# ── LabelProps writer (add_obs / save! — the chain write path) ─────────────
@testset "LabelProps writer" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps writer (fixture missing)"
    else
        tmp = joinpath(mktempdir(), "B.h5ad")
        cp(h5, tmp)

        existing = label_props(tmp) |> as_df
        some = existing.label[1:5]                       # write to a subset of labels
        df = DataFrame("label" => some,
                       "test.measure" => Float64.(1:5),
                       "test.other"   => [10.0, 20.0, 30.0, 40.0, 50.0])
        # the documented chain write idiom
        label_props(tmp) |> add_obs(df) |> save!

        # new columns appear in obs column-order
        obs_cols = col_names(label_props(tmp); data_type=:obs)
        @test "test.measure" in obs_cols
        @test "test.other" in obs_cols

        # read back via the reader, aligned by label; unset labels are NaN
        back = label_props(tmp) |> select_cols(["test.measure", "test.other"]) |> as_df
        byrow = Dict(l => i for (i, l) in enumerate(back.label))
        for (k, lab) in enumerate(some)
            @test back[byrow[lab], "test.measure"] == Float64(k)
        end
        # a label not in df → NaN
        other = first(setdiff(back.label, some))
        @test isnan(back[byrow[other], "test.measure"])

        # original data preserved (var count unchanged — obs append only, no X rewrite)
        @test length(col_names(label_props(tmp); data_type=:vars)) ==
              length(col_names(label_props(h5);  data_type=:vars))

        # idempotent overwrite: re-writing the same column updates, doesn't duplicate
        df2 = DataFrame("label" => some, "test.measure" => fill(99.0, 5))
        label_props(tmp) |> add_obs(df2) |> save!
        @test count(==("test.measure"), col_names(label_props(tmp); data_type=:obs)) == 1
        back2 = label_props(tmp) |> select_cols(["test.measure"]) |> as_df
        @test back2[Dict(l => i for (i, l) in enumerate(back2.label))[some[1]], "test.measure"] == 99.0

        # drop_obs: remove a column; gone from column-order and from as_df
        label_props(tmp) |> drop_obs(["test.measure"]) |> save!
        @test "test.measure" ∉ col_names(label_props(tmp); data_type=:obs)
        @test "test.other"   ∈ col_names(label_props(tmp); data_type=:obs)   # sibling untouched
        @test "test.measure" ∉ names(label_props(tmp) |> as_df)
        # dropping a nonexistent column is a no-op (idempotent)
        @test begin label_props(tmp) |> drop_obs(["never.existed"]) |> save!; true end

        # combined drop + add in one chain (invalidate-then-write, e.g. btrack rerun)
        df3 = DataFrame("label" => some, "test.fresh" => Float64.(1:5))
        label_props(tmp) |> drop_obs(["test.other"]) |>
                            add_obs(df3) |> save!
        cols3 = col_names(label_props(tmp); data_type=:obs)
        @test "test.other" ∉ cols3
        @test "test.fresh" ∈ cols3

        # drop + re-add the SAME column in one chain → the add wins (column survives with new
        # values). Regression: the drop used to de-list and delete the just-written dataset, so
        # e.g. overwriting a categorical hmm.state with a numeric one in one chain lost it.
        df4 = DataFrame("label" => some, "test.fresh" => Float64.(101:105))
        label_props(tmp) |> drop_obs(["test.fresh"]) |>
                            add_obs(df4) |> save!
        @test "test.fresh" ∈ col_names(label_props(tmp); data_type=:obs)
        back4 = label_props(tmp) |> select_cols(["test.fresh"]) |> as_df
        row4  = Dict(l => i for (i, l) in enumerate(back4.label))
        for (k, lab) in enumerate(some)
            @test back4[row4[lab], "test.fresh"] == Float64(100 + k)
        end
    end
end

# ── Julia ↔ Python reader parity (the duplication safety net) ──────────────
# The Julia LabelProps reader and the Python LabelPropsView are two implementations of
# ONE spec (docs/DATAMODEL.md). They can drift — a new encoding type added to one and not
# the other. This runs BOTH against the same fixture and compares. Gated on the napari
# venv + anndata being importable, so headless CI without Python skips rather than fails.
@testset "LabelProps Julia/Python parity" begin
    h5    = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    pybin = python_bin_path()
    py_ok = !isempty(pybin) && isfile(pybin) &&
            success(setenv(`$pybin -c "import anndata, numpy, pandas"`, dir=@__DIR__))
    if !have_fixture(h5) || !py_ok
        @test_skip "LabelProps parity (fixture or python+anndata unavailable)"
    else
        # Python side dumps a comparable summary as JSON via the LabelPropsView reader.
        pyscript = """
            import sys, json
            import cecelia.utils.label_props_utils as lpu
            v = lpu.LabelPropsView(sys.argv[1])
            df  = v.view_cols(["mean_intensity_0"]).as_df().sort_values("label")
            cv  = lpu.LabelPropsView(sys.argv[1]).only_centroid_cols().as_df().sort_values("label")
            print(json.dumps({
                "var_names":      list(v.var_names()),
                "obs_cols":       list(v.adata.obs.columns),
                "centroid_cols":  list(v.centroid_columns()),
                "temporal_cols":  list(v.temporal_columns()),
                "n_obs":          int(len(v.labels())),
                "labels5":        [int(x) for x in df["label"].to_numpy()[:5]],
                "mean_int0_5":    [float(x) for x in df["mean_intensity_0"].to_numpy()[:5]],
                "centroid0_5":    [float(x) for x in cv["centroid_z"].to_numpy()[:5]],
            }))
            """
        # python/ on PYTHONPATH so `import cecelia.utils...` resolves (matches run_py's PYTHONPATH).
        # `pathof(Cecelia)` = app/src/Cecelia.jl, so three dirnames up is the repo root — stable
        # regardless of whether this file sits at app/test/ or app/test/suite/.
        py_dir = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "python")
        penv = copy(ENV); penv["PYTHONPATH"] = py_dir
        out = read(setenv(`$pybin -c $pyscript $h5`, penv; dir=py_dir), String)
        py = JSON3.read(out)

        lp = label_props(h5)
        @test Set(col_names(lp; data_type=:vars)) == Set(String.(py.var_names))
        @test Set(col_names(lp; data_type=:obs))  == Set(String.(py.obs_cols))
        @test Set(centroid_columns(lp))           == Set(String.(py.centroid_cols))
        @test Set(temporal_columns(lp))           == Set(String.(py.temporal_cols))

        # value parity on the same labels (sorted), one var col + one centroid col
        dj = label_props(h5) |> select_cols(["mean_intensity_0"]) |> sort_by("label") |> as_df
        @test dj.label[1:5] == collect(Int, py.labels5)
        @test dj[1:5, "mean_intensity_0"] ≈ Float64.(py.mean_int0_5)
        cj = label_props(h5) |> view_centroid_cols |> sort_by("label") |> as_df
        @test cj[1:5, "centroid_z"] ≈ Float64.(py.centroid0_5)
    end
end

# ── Track measures: numeric cross-check vs celltrackR ─────────────────────
# Golden values — provenance:
#   Generated from celltrackR 1.2.2 (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006)
#   on the track below, via the R package's own functions (trackLength/speed/displacement/
#   straightness/asphericity/overallAngle/meanTurningAngle, degrees=TRUE; per-step via
#   subtracks()). celltrackR is the reference Cecelia ported these from; it is NOT a runtime
#   dependency — these constants pin the port to the original. If a measure here changes,
#   either the port regressed or it was deliberately changed (then re-derive from celltrackR).
@testset "Track measures (celltrackR golden)" begin
    Track = Cecelia.Track
    t  = [0.0, 10, 20, 30, 40]
    c3 = [0.0 0 0; 3 4 0; 7 4 2; 7 8 2; 10 12 5]    # (x,y,z) per position
    tr = Track(1, t, c3)

    @test Cecelia.track_length(tr)            ≈ 19.3030878498 atol=1e-6
    @test Cecelia.track_duration(tr)          == 40.0
    @test Cecelia.track_speed(tr)             ≈ 0.4825771962  atol=1e-6
    @test Cecelia.track_displacement(tr)      ≈ 16.4012194669 atol=1e-6
    @test Cecelia.max_displacement(tr)        ≈ 16.4012194669 atol=1e-6
    @test Cecelia.track_straightness(tr)      ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_displacement_ratio(tr) ≈ 1.0          atol=1e-6
    @test Cecelia.track_outreach_ratio(tr)    ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_asphericity(tr)       ≈ 0.8469835416  atol=1e-6
    @test Cecelia.track_overall_angle(tr)     ≈ 30.9637565321 atol=1e-6
    @test Cecelia.track_mean_turning_angle(tr) ≈ 64.7432782933 atol=1e-6

    # per-cell subtracks (celltrackR subtracks(·,1) speed; subtracks(·,2) overallAngle)
    ss = Cecelia.step_speeds(tr)              # cell_id 1 → NaN; i>1 = step speed to endpoint
    @test isnan(ss[1])
    @test ss[2:5] ≈ [0.5, 0.4472135955, 0.4, 0.5830951895] atol=1e-6
    sa = Cecelia.step_turning_angles(tr)      # cell_id 1,2 → NaN; i≥3 = turn angle (deg)
    @test all(isnan, sa[1:2])
    @test sa[3:5] ≈ [57.5436915381, 90.0, 46.6861433417] atol=1e-6

    # 2D path (drop z) — same functions, no call-site branching
    tr2 = Track(1, t, c3[:, 1:2])
    @test Cecelia.track_straightness(tr2)  ≈ 0.8678055195 atol=1e-6
    @test Cecelia.track_asphericity(tr2)   ≈ 0.8287305960 atol=1e-6
    @test Cecelia.track_overall_angle(tr2) ≈ 0.0          atol=1e-6  # first ∥ last step in xy

    # ── edge cases (Step 4 mandates these) ────────────────────────────────
    # single-step track (2 positions): measures needing ≥3 steps → NaN; no crash
    one = Track(1, [0.0, 10], [0.0 0 0; 3 4 0])
    @test Cecelia.track_length(one)            ≈ 5.0
    @test Cecelia.track_speed(one)             ≈ 0.5
    @test Cecelia.track_straightness(one)      ≈ 1.0          # straight by definition
    @test isnan(Cecelia.track_overall_angle(one))            # n<3
    @test isnan(Cecelia.track_mean_turning_angle(one))
    @test Cecelia.track_asphericity(one)       == 1.0         # celltrackR convention for <3

    # single-position track: no div-by-zero, sane fallbacks
    pt = Track(1, [0.0], reshape([0.0, 0, 0], 1, 3))
    @test Cecelia.track_length(pt)        == 0.0
    @test isnan(Cecelia.track_speed(pt))                     # duration 0
    @test Cecelia.track_straightness(pt)  == 1.0             # length 0 → 1
    @test isnan(Cecelia.track_displacement_ratio(pt))        # maxDisplacement 0

    # zero net displacement (returns to origin): straightness 0, no div-by-zero
    loop = Track(1, [0.0, 1, 2], [0.0 0; 1 0; 0 0])
    @test Cecelia.track_displacement(loop)       ≈ 0.0
    @test Cecelia.track_length(loop)             ≈ 2.0
    @test Cecelia.track_straightness(loop)       ≈ 0.0
    @test Cecelia.track_displacement_ratio(loop) ≈ 0.0       # disp 0 / maxDisp 1
    @test Cecelia.track_outreach_ratio(loop)     ≈ 0.5       # maxDisp 1 / length 2
end

