# ── Manual track correction (docs/todo/CORRECTION_PLAN.md, P1) ────────────────
#
# The ops engine is pure, so it is tested directly against a hand-built cell table — no fixture, no
# .h5ad. `_corr_df` mirrors the real obs layout verified on `zolIMa/…/memTom.h5ad`: every lineage
# column is Float64, and a root track has `track_parent == track_root == track_id`, generation 0.
_corr_df() = DataFrame(
    label            = Float64[1, 2, 3, 4, 5, 6, 7],
    centroid_t       = Float64[0, 1, 2, 0, 1, 2, 3],
    track_id         = Float64[10, 10, 10, 20, 20, 20, NaN],
    track_parent     = Float64[10, 10, 10, 20, 20, 20, NaN],
    track_root       = Float64[10, 10, 10, 20, 20, 20, NaN],
    track_state      = Float64[5, 5, 5, 5, 5, 5, 5],
    track_generation = Float64[0, 0, 0, 0, 0, 0, NaN],
    cell_id          = Float64[1, 2, 3, 1, 2, 3, NaN],
)

@testset "track correction — ops" begin
    # ── points.remove: untrack cells, clearing lineage but NOT track_state ────
    df = _corr_df()
    Cecelia.apply_track_ops!(df, [Dict("op" => "points.remove", "labels" => [2])])
    @test isnan(df[2, :track_id])
    @test isnan(df[2, :track_parent]) && isnan(df[2, :track_root])
    @test df[2, :track_state] == 5.0          # per-cell, not lineage — must survive
    # cell_id renumbered: track 10 now has cells at t=0,2 → ranks 1,2
    @test df[1, :cell_id] == 1.0 && df[3, :cell_id] == 2.0

    # ── track.remove ──────────────────────────────────────────────────────────
    df = _corr_df()
    Cecelia.apply_track_ops!(df, [Dict("op" => "track.remove", "trackIds" => [20])])
    @test all(isnan, df[4:6, :track_id])
    @test Cecelia.track_ids_present(df) == [10]

    # ── points.add to an EXISTING track adopts its lineage, never re-roots it ──
    df = _corr_df()
    df[4:6, :track_parent] .= 99.0            # pretend track 20 has a real parent
    df[4:6, :track_generation] .= 1.0
    Cecelia.apply_track_ops!(df, [Dict("op" => "points.add", "labels" => [7], "trackId" => 20)])
    @test df[7, :track_id] == 20.0
    @test df[7, :track_parent] == 99.0        # adopted, not reset to 20
    @test df[4, :track_parent] == 99.0        # the target's own lineage untouched
    @test df[7, :cell_id] == 4.0              # t=3 is the 4th cell of track 20

    # ── points.add with no trackId allocates max+1, ignoring NaN, and roots it ─
    df = _corr_df()
    @test Cecelia.next_track_id(df) == 21
    Cecelia.apply_track_ops!(df, [Dict("op" => "points.add", "labels" => [7])])
    @test df[7, :track_id] == 21.0
    @test df[7, :track_parent] == 21.0 && df[7, :track_root] == 21.0
    @test df[7, :track_generation] == 0.0     # a root's parent is ITSELF, not NaN
    @test df[7, :cell_id] == 1.0

    # ── points.add rejects a timepoint the target already holds ───────────────
    df = _corr_df()
    @test_throws Exception Cecelia.apply_track_op!(
        df, Dict("op" => "points.add", "labels" => [1], "trackId" => 20))   # t=0 taken by label 4
end

@testset "track correction — join and split" begin
    # ── join consumes B entirely and keeps A's lineage ────────────────────────
    df = _corr_df()
    df[4:6, :centroid_t] .= [3.0, 4.0, 5.0]   # move track 20 clear of track 10 in time
    df[4:6, :track_parent] .= 20.0
    Cecelia.apply_track_ops!(df, [Dict("op" => "track.join", "trackIds" => [10, 20])])
    @test Cecelia.track_ids_present(df) == [10]          # B is GONE, not a remnant (old R's bug)
    @test all(df[1:6, :track_id] .== 10.0)
    @test all(df[1:6, :track_parent] .== 10.0)           # B's cells adopted A's lineage
    @test df[1:6, :cell_id] == Float64[1, 2, 3, 4, 5, 6] # renumbered across the joined track

    # ── join REFUSES a temporal overlap, naming the timepoints ────────────────
    df = _corr_df()                                       # 10 and 20 both span t=0,1,2
    err = try
        Cecelia.apply_track_op!(df, Dict("op" => "track.join", "trackIds" => [10, 20]))
        nothing
    catch e; e end
    @test err !== nothing
    @test occursin("cannot be one cell", sprint(showerror, err))
    @test Cecelia.track_ids_present(df) == [10, 20]        # nothing was applied

    @test_throws Exception Cecelia.apply_track_op!(
        _corr_df(), Dict("op" => "track.join", "trackIds" => [10, 10]))
    @test_throws Exception Cecelia.apply_track_op!(
        _corr_df(), Dict("op" => "track.join", "trackIds" => [10, 999]))

    # ── split: later half becomes a new ROOT track, both halves renumbered ────
    df = _corr_df()
    Cecelia.apply_track_ops!(df, [Dict("op" => "track.split", "trackId" => 10, "atT" => 2)])
    @test df[1, :track_id] == 10.0 && df[2, :track_id] == 10.0
    @test df[3, :track_id] == 21.0
    @test df[3, :track_parent] == 21.0 && df[3, :track_root] == 21.0
    @test df[3, :cell_id] == 1.0                          # first cell of the new fragment
    @test df[1, :cell_id] == 1.0 && df[2, :cell_id] == 2.0

    # a split that would leave one side empty is rejected, not a silent rename
    @test_throws Exception Cecelia.apply_track_op!(
        _corr_df(), Dict("op" => "track.split", "trackId" => 10, "atT" => 0))
    @test_throws Exception Cecelia.apply_track_op!(
        _corr_df(), Dict("op" => "track.split", "trackId" => 10, "atT" => 99))

    # ── ops apply SEQUENTIALLY — the journal is a replay script ───────────────
    df = _corr_df()
    entries = Cecelia.apply_track_ops!(df, [
        Dict("op" => "track.split", "trackId" => 10, "atT" => 2),   # → new track 21
        Dict("op" => "track.remove", "trackIds" => [21]),           # remove what step 1 made
    ])
    @test length(entries) == 2
    @test all(e -> haskey(e, "summary") && !isempty(e["summary"]), entries)
    @test Cecelia.track_ids_present(df) == [10, 20]
    @test isnan(df[3, :track_id])
end

@testset "track correction — unknown op" begin
    @test_throws ArgumentError Cecelia.apply_track_op!(_corr_df(), Dict("op" => "track.frobnicate"))
    @test Set(Cecelia.TRACK_OP_KINDS) ==
        Set(["points.remove", "points.add", "track.remove", "track.join", "track.split"])
end

@testset "track correction — QC metrics + findings" begin
    before = Float64[10, 10, 10, 20, 20, 20, NaN]
    after  = Float64[10, 10, 10, 10, 10, 10, NaN]          # a join: 3 cells moved
    m = Cecelia.track_correction_metrics(before, after, 1)
    @test m["nOps"] == 1
    @test m["nCellsReassigned"] == 3
    @test m["nTracksBefore"] == 2 && m["nTracksAfter"] == 1
    @test m["fracCellsEdited"] ≈ 3/7 atol=1e-4

    # to/from untracked counts as a reassignment
    m2 = Cecelia.track_correction_metrics(Float64[10, 10], Float64[10, NaN], 1)
    @test m2["nCellsReassigned"] == 1

    # a big share of cells hand-corrected is a tracking-parameter problem (Decision 8)
    @test any(f -> f["code"] == "correction.large_share_edited",
              Cecelia.track_correction_qc_findings(m))
    @test isempty(Cecelia.track_correction_qc_findings(
        Cecelia.track_correction_metrics(Float64[fill(10.0, 100); 20.0],
                                         Float64[fill(10.0, 100); 20.0], 1)))

    # a split can leave tracks below tracking's own minTimepoints (4d) — warn, never re-filter.
    # A 6-cell track split 3/3 makes BOTH halves newly short: the original (was long enough) and the
    # new fragment (a new id).
    short = Cecelia.track_correction_metrics(Float64[10, 10, 10, 10, 10, 10],
                                             Float64[10, 10, 10, 21, 21, 21], 1)
    @test short["nShortTracks"] == 2
    @test any(f -> f["code"] == "correction.short_tracks",
              Cecelia.track_correction_qc_findings(short))

    # …but a track that was ALREADY short is not this correction's fault — don't blame the wrong task
    @test Cecelia.track_correction_metrics(Float64[10, 10, 20], Float64[10, 10, 20],
                                           0)["nShortTracks"] == 0

    # every finding's text resolves from the catalog (no unsubstituted {placeholder} reaches a user)
    for f in vcat(Cecelia.track_correction_qc_findings(m),
                  Cecelia.track_correction_qc_findings(short))
        @test !occursin("{", f["short"]) && !occursin("{", f["long"])
    end
end

@testset "track correction — journal sidecar" begin
    dir = mktempdir()
    @test Cecelia.corrections_path(dir, "memTom") ==
        joinpath(dir, "corrections", "memTom.json")
    @test isempty(Cecelia.load_corrections(dir, "memTom")["entries"])   # absent → empty, not an error

    Cecelia.append_corrections!(dir, "memTom",
        [Dict("op" => "track.join", "trackIds" => [10, 20], "summary" => "joined")]; run_id = "r1")
    Cecelia.append_corrections!(dir, "memTom",
        [Dict("op" => "track.remove", "trackIds" => [30], "summary" => "removed")]; run_id = "r2")

    doc = Cecelia.load_corrections(dir, "memTom")
    @test doc["valueName"] == "memTom"
    @test length(doc["entries"]) == 2                     # append-only across runs
    @test [e["seq"] for e in doc["entries"]] == [1, 2]    # monotonic, so history has a stable order
    @test [e["runId"] for e in doc["entries"]] == ["r1", "r2"]
    @test doc["entries"][1]["op"] == "track.join"
end

# ── Manual label correction (docs/todo/CORRECTION_PLAN.md, P2) ────────────────
#
# The engine is pure — validation, the rewrite-table fold and the journal. Array mutation happens
# Python-side (per Decision 2b), so these tests assert what Julia OWNS: op shape, per-t rewrite
# building, chain collapsing, journal I/O, metrics + QC. Fixtures are Dict-only.

@testset "label correction — op validation" begin
    # merge: needs t, ids (>=2, all >=1) and `into` ∈ ids
    @test Cecelia.validate_label_op(
        Dict("op" => "label.merge", "t" => 0, "ids" => [3, 5], "into" => 3)) === nothing
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.merge", "t" => 0, "ids" => [3, 5], "into" => 9))
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.merge", "t" => 0, "ids" => [3],    "into" => 3))
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.merge", "t" => 0, "ids" => [3, 0], "into" => 3))
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.merge", "t" => -1, "ids" => [3, 5], "into" => 3))
    # remove: needs t, non-empty ids all >=1
    @test Cecelia.validate_label_op(Dict("op" => "label.remove", "t" => 4, "ids" => [7])) === nothing
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.remove", "t" => 0, "ids" => Int[]))
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.remove", "t" => 0, "ids" => [0]))
    # split: needs t, a single positive `id`, and equal-length xs/ys polyline (>=2 vertices).
    @test Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [10, 30], "ys" => [20, 20])) === nothing
    # 3-vertex polyline is legal — a two-click cut is the MVP shape, longer polylines land later.
    @test Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 3, "id" => 5, "xs" => [1, 5, 9], "ys" => [1, 5, 9])) === nothing
    @test_throws ArgumentError Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 0, "id" => 0, "xs" => [1, 2], "ys" => [3, 4]))       # id=background
    @test_throws ArgumentError Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [1], "ys" => [3]))             # 1 vertex
    @test_throws ArgumentError Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [1, 2], "ys" => [3]))          # length mismatch
    @test_throws ArgumentError Cecelia.validate_label_op(
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [1, -2], "ys" => [3, 4]))      # negative coord

    # unknown op
    @test_throws ArgumentError Cecelia.validate_label_op(Dict("op" => "label.frobnicate", "t" => 0, "ids" => [1]))
    @test Set(Cecelia.LABEL_OP_KINDS) == Set(["label.merge", "label.remove", "label.split"])
end

@testset "label correction — build_rewrite folds ops into per-frame maps" begin
    # Two frames, independent. Frame 0: merge {2,3} into 2. Frame 1: remove {5}.
    m = Cecelia.build_rewrite([
        Dict("op" => "label.merge",  "t" => 0, "ids" => [2, 3], "into" => 2),
        Dict("op" => "label.remove", "t" => 1, "ids" => [5]),
    ])
    @test Set(keys(m)) == Set([0, 1])
    @test m[0] == Dict(3 => 2)
    @test m[1] == Dict(5 => 0)

    # Chain collapse within a frame: (2->3) then (3->4) ⇒ 2->4, 3->4 (one pass per pixel).
    m = Cecelia.build_rewrite([
        Dict("op" => "label.merge", "t" => 0, "ids" => [2, 3], "into" => 3),
        Dict("op" => "label.merge", "t" => 0, "ids" => [3, 4], "into" => 4),
    ])
    @test m[0][2] == 4 && m[0][3] == 4

    # A remove after a merge in the same frame should map both sources to 0.
    m = Cecelia.build_rewrite([
        Dict("op" => "label.merge",  "t" => 0, "ids" => [2, 3], "into" => 3),
        Dict("op" => "label.remove", "t" => 0, "ids" => [3]),
    ])
    @test m[0][2] == 0 && m[0][3] == 0

    # Ops on different frames don't leak into each other (Decision 6b).
    m = Cecelia.build_rewrite([
        Dict("op" => "label.remove", "t" => 0, "ids" => [7]),
        Dict("op" => "label.remove", "t" => 1, "ids" => [7]),
    ])
    @test m[0][7] == 0 && m[1][7] == 0 && length(m) == 2

    # Empty op list yields empty map, not an error.
    @test isempty(Cecelia.build_rewrite(Dict{String,Any}[]))

    # A `label.split` op is a NON-rewrite (creates new ids by CC) — build_rewrite skips it silently
    # rather than erroring, because a mixed queue (Merge + Split) is legal and callers of this
    # rewrite table only care about the rewrite half. The Python runner applies split directly.
    m = Cecelia.build_rewrite([
        Dict("op" => "label.merge", "t" => 0, "ids" => [2, 3], "into" => 2),
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [10, 30], "ys" => [20, 20]),
    ])
    @test m[0] == Dict(3 => 2)         # merge kept, split ignored

    # A malformed op inside a batch throws — no partial rewrite.
    @test_throws ArgumentError Cecelia.build_rewrite([
        Dict("op" => "label.remove", "t" => 0, "ids" => [1]),
        Dict("op" => "label.merge",  "t" => 0, "ids" => [2], "into" => 2),   # single-id merge
    ])
end

@testset "label correction — QC metrics + findings" begin
    ops = [
        Dict("op" => "label.merge",  "t" => 0, "ids" => [1, 2, 3], "into" => 1),   # 2 labels removed
        Dict("op" => "label.remove", "t" => 1, "ids" => [7]),                       # 1 label removed
    ]
    m = Cecelia.label_correction_metrics(ops, [12, 5]; n_labels_before = 10, n_labels_after = 7)
    @test m["nOps"] == 2
    @test m["nMerge"] == 1 && m["nRemove"] == 1
    @test m["nFramesTouched"] == 2
    @test m["nLabelsRemoved"] == 3
    @test m["nPixelsRewritten"] == 17
    @test m["fracLabelsEdited"] ≈ 3/10 atol=1e-4

    # per_op_pixels omitted (e.g. dry-run) → 0
    m2 = Cecelia.label_correction_metrics(ops, Int[]; n_labels_before = 10)
    @test m2["nPixelsRewritten"] == 0

    # ≥30% of labels edited fires the warn (mirrors track_correction's threshold)
    @test any(f -> f["code"] == "correction.labels_large_share_edited",
              Cecelia.label_correction_qc_findings(m))
    # a small edit is silent — advisory only, and QC noise on every small correction is not useful
    small = Cecelia.label_correction_metrics(
        [Dict("op" => "label.remove", "t" => 0, "ids" => [1])], [3];
        n_labels_before = 100)
    @test isempty(Cecelia.label_correction_qc_findings(small))

    # split contributes to `nSplit` + `nLabelsSplit` (separately from removals; the split label
    # keeps its original id on the largest fragment, so it isn't "removed" from the id space).
    split_ops = [
        Dict("op" => "label.split", "t" => 0, "id" => 5, "xs" => [10, 30], "ys" => [20, 20]),
        Dict("op" => "label.merge", "t" => 0, "ids" => [3, 4], "into" => 3),
    ]
    sm = Cecelia.label_correction_metrics(split_ops, [42, 17]; n_labels_before = 20)
    @test sm["nSplit"] == 1 && sm["nMerge"] == 1 && sm["nRemove"] == 0
    @test sm["nLabelsSplit"] == 1     # id 5
    @test sm["nLabelsRemoved"] == 1   # merge sacrificed id 4
    @test sm["fracLabelsEdited"] ≈ 2/20 atol=1e-4  # split + removed, deduped by union

    # every finding's text resolves from the catalog (no unsubstituted {placeholder} reaches a user)
    for f in Cecelia.label_correction_qc_findings(m)
        @test !occursin("{", f["short"]) && !occursin("{", f["long"])
    end
end

@testset "label correction — task wiring + param validation" begin
    @test Cecelia._task_from_fun_name("segment.correct") isa Cecelia.SegmentCorrect
    @test Cecelia._task_from_fun_name("segment.correct_measures") isa Cecelia.CompositeTask
    @test isfile(Cecelia._spec_path(Cecelia.SegmentCorrect()))
    @test haskey(Cecelia.COHORT_METRICS, "segment.correct")

    # ops arrive as a Vector (REPL/API/chain) or a JSON string (the form). One parser.
    ops = Cecelia.parse_label_ops([Dict("op" => "label.remove", "t" => 0, "ids" => [1])])
    @test length(ops) == 1 && ops[1]["op"] == "label.remove"
    @test Cecelia.parse_label_ops("[{\"op\":\"label.merge\",\"t\":0,\"ids\":[1,2],\"into\":1}]")[1]["op"] == "label.merge"

    # EMPTY is legal — same rule as tracking.correct's spec-defaults check
    for empty_val in (nothing, "", "[]", Any[])
        @test isempty(Cecelia.parse_label_ops(empty_val))
    end

    # every malformed shape is a ParamValidationError at submit time
    for bad in ("not json", "{\"op\":\"label.remove\"}",
                [Dict("op" => "nope", "t" => 0, "ids" => [1])],
                [Dict("op" => "label.remove")],                                 # no t/ids
                [Dict("op" => "label.remove", "t" => 0, "ids" => Int[])],       # empty ids
                [Dict("op" => "label.merge", "t" => 0, "ids" => [1])],          # single-id merge
                [Dict("op" => "label.merge", "t" => 0, "ids" => [1, 2], "into" => 9)])   # into ∉ ids
        @test_throws Cecelia.ParamValidationError Cecelia.parse_label_ops(bad)
    end

    # composite is a 5-step chain: snapshot the obs, correct, re-measure, restore, THEN report which
    # downstream artefacts predate the correction (P2 Decision 4b + P3 Decision 5, CORRECTION_PLAN.md).
    spec = Cecelia._task_spec(Cecelia._task_from_fun_name("segment.correct_measures"))
    @test spec["composite"] == [
        "segment.correct_carryover_snapshot",
        "segment.correct",
        "segment.measureLabels",
        "segment.correct_carryover_restore",
        "segment.staleness_report",
    ]

    # tracking composite gets the same staleness tail (narrower scope — only track-derived artefacts).
    tspec = Cecelia._task_spec(Cecelia._task_from_fun_name("tracking.correct_measures"))
    @test tspec["composite"] == [
        "tracking.correct",
        "tracking.track_measures",
        "tracking.staleness_report",
    ]
end

@testset "label correction — obs carry-over task wiring" begin
    # both phases are typed CciaTasks (the composite executor threads the same params dict through
    # every step, so a phase-toggled single task couldn't be invoked twice with different args).
    @test Cecelia._task_from_fun_name("segment.correct_carryover_snapshot") isa Cecelia.SegmentCorrectCarryOverSnapshot
    @test Cecelia._task_from_fun_name("segment.correct_carryover_restore")  isa Cecelia.SegmentCorrectCarryOverRestore
    @test isfile(Cecelia._spec_path(Cecelia.SegmentCorrectCarryOverSnapshot()))
    @test isfile(Cecelia._spec_path(Cecelia.SegmentCorrectCarryOverRestore()))
end

@testset "correction staleness — task wiring" begin
    # same shape as the carry-over pair — two typed tasks so each composite invokes its own `changed`
    # scope (a shared task would need a `stalenessScope` param whose only correct values are the
    # composite's own — worse UX than a task per scope).
    @test Cecelia._task_from_fun_name("segment.staleness_report")  isa Cecelia.SegmentStalenessReport
    @test Cecelia._task_from_fun_name("tracking.staleness_report") isa Cecelia.TrackingStalenessReport
    @test isfile(Cecelia._spec_path(Cecelia.SegmentStalenessReport()))
    @test isfile(Cecelia._spec_path(Cecelia.TrackingStalenessReport()))
end

@testset "correction staleness — enumeration by disk presence" begin
    # Build a scratch img._dir with the four artefact classes present, then assert each shows up
    # (or doesn't) under `changed = :labels` and `:tracks`. Uses a synthetic CciaImage — no real
    # h5ad/zarr needed since the enumerator is disk-presence only.
    dir = mktempdir()
    vn  = "memTom"

    # per-track h5ad
    tracks_dir = joinpath(dir, "labelProps"); mkpath(tracks_dir)
    write(joinpath(tracks_dir, "$(vn)__tracks.h5ad"), "")
    # primary cell h5ad + clustfeatures sidecar (records one cluster suffix)
    write(joinpath(tracks_dir, "$(vn).h5ad"), "")
    open(joinpath(tracks_dir, "$(vn).clustfeatures.json"), "w") do io
        JSON3.pretty(io, Dict("clusters.immune" => Dict("features" => ["area"])))
    end
    # both gating files
    gdir = joinpath(dir, "gating"); mkpath(gdir)
    open(joinpath(gdir, "$(vn).json"), "w") do io
        JSON3.pretty(io, Dict("pops" => Dict("A" => Dict(), "B" => Dict())))
    end
    open(joinpath(gdir, "$(vn)__tracks.json"), "w") do io
        JSON3.pretty(io, Dict("pops" => Dict("T1" => Dict())))
    end
    # one spatial graph
    sgdir = joinpath(dir, "spatialGraph"); mkpath(sgdir)
    write(joinpath(sgdir, "run1.h5ad"), "")

    img = Cecelia.CciaImage(; uid = "uid", name = "name", dir = dir)
    img.label_props = Dict{String,String}(vn => "$(vn).h5ad")

    label_arts = Cecelia.stale_artefacts_for(img, vn; changed = :labels)
    kinds_l = Set(a["kind"] for a in label_arts)
    @test kinds_l == Set(["tracks_h5ad", "cluster_runs", "gating_pops", "spatial_graph"])
    # gating entry on :labels is the FLOW file only (label-keyed) — the __tracks pops are on :tracks
    gating_l = only(a for a in label_arts if a["kind"] == "gating_pops")
    @test gating_l["detail"]["pop_type"] == "flow"
    @test gating_l["detail"]["count"] == 2

    track_arts = Cecelia.stale_artefacts_for(img, vn; changed = :tracks)
    kinds_t = Set(a["kind"] for a in track_arts)
    @test kinds_t == Set(["tracks_h5ad", "gating_pops"])   # clusters + spatial graph excluded
    gating_t = only(a for a in track_arts if a["kind"] == "gating_pops")
    @test gating_t["detail"]["pop_type"] == "track"

    # unknown scope refuses (an added typo is worse than a caught throw)
    @test_throws ArgumentError Cecelia.stale_artefacts_for(img, vn; changed = :something)

    # empty dir → nothing to report (an image that was never analysed carries no derived artefacts)
    empty_dir = mktempdir()
    empty_img = Cecelia.CciaImage(; uid = "uid2", name = "name2", dir = empty_dir)
    @test isempty(Cecelia.stale_artefacts_for(empty_img, vn; changed = :labels))

    # cell-cards sidecar under analysis/cell_cards/{vn}__{suffix}.json shows up on BOTH :labels and
    # :tracks — trackclust cluster codes change on :labels (via cluster reruns) and medoid track
    # centroids change on :tracks. See docs/todo/CELL_CARDS_PLAN.md Decision 8.
    cc_dir = joinpath(dir, "analysis", "cell_cards"); mkpath(cc_dir)
    write(joinpath(cc_dir, "$(vn)__movement.json"), "{}")
    # A sidecar keyed on a DIFFERENT vn must NOT be listed for this vn.
    write(joinpath(cc_dir, "other__movement.json"), "{}")
    label_arts_2 = Cecelia.stale_artefacts_for(img, vn; changed = :labels)
    cc_arts_l = filter(a -> a["kind"] == "cell_cards", label_arts_2)
    @test length(cc_arts_l) == 1
    @test occursin("$(vn)__movement.json", cc_arts_l[1]["path"])
    track_arts_2 = Cecelia.stale_artefacts_for(img, vn; changed = :tracks)
    cc_arts_t = filter(a -> a["kind"] == "cell_cards", track_arts_2)
    @test length(cc_arts_t) == 1
end

@testset "correction staleness — QC finding renders through the catalog" begin
    # Round-trip a warn finding through qc_text so the catalog placeholders (`{n}`, `{scope}`) are
    # honoured — same guard the QC_TEXT test uses for the other correction findings.
    t = Cecelia.qc_text("correction.stale_artefacts"; n = 3, scope = "labels")
    @test occursin("3", t.short)
    @test occursin("labels", t.long)
end

@testset "label correction — journal sidecar" begin
    dir = mktempdir()
    @test Cecelia.label_corrections_path(dir, "memTom") ==
        joinpath(dir, "corrections", "labels_memTom.json")     # peer to tracks' file, prefixed
    @test isempty(Cecelia.load_label_corrections(dir, "memTom")["entries"])

    Cecelia.append_label_corrections!(dir, "memTom", [
        Dict("op" => "label.merge", "t" => 0, "ids" => [2, 3], "into" => 2, "nPixels" => 12),
    ]; run_id = "r1")
    Cecelia.append_label_corrections!(dir, "memTom", [
        Dict("op" => "label.remove", "t" => 1, "ids" => [5], "nPixels" => 4),
    ]; run_id = "r2")

    doc = Cecelia.load_label_corrections(dir, "memTom")
    @test doc["valueName"] == "memTom"
    @test length(doc["entries"]) == 2
    @test [e["seq"] for e in doc["entries"]] == [1, 2]        # monotonic across runs
    @test [e["runId"] for e in doc["entries"]] == ["r1", "r2"]
    @test doc["entries"][1]["op"] == "label.merge"
end

@testset "track correction — task wiring + param validation" begin
    @test Cecelia._task_from_fun_name("tracking.correct") isa Cecelia.TrackCorrect
    @test Cecelia._task_from_fun_name("tracking.correct_measures") isa Cecelia.CompositeTask
    @test isfile(Cecelia._spec_path(Cecelia.TrackCorrect()))
    @test haskey(Cecelia.COHORT_METRICS, "tracking.correct")

    # ops arrive either as a Vector (REPL/API/chain) or a JSON string (the form) — one parser
    ops = Cecelia.parse_track_ops([Dict("op" => "track.remove", "trackIds" => [1])])
    @test length(ops) == 1 && ops[1]["op"] == "track.remove"
    @test Cecelia.parse_track_ops("[{\"op\":\"track.join\",\"trackIds\":[1,2]}]")[1]["op"] == "track.join"

    # EMPTY is legal and means "no correction" — the suite requires every task's own spec defaults to
    # validate, so `trackOps`' default ("[]") must parse. `_run_task` then reports it and no-ops.
    for empty_val in (nothing, "", "[]", Any[])
        @test isempty(Cecelia.parse_track_ops(empty_val))
    end

    # every malformed shape is a ParamValidationError at submit time, not a mid-run stack trace
    for bad in ("not json", "{\"op\":\"track.remove\"}",
                [Dict("op" => "nope")],
                [Dict("op" => "points.remove")],                       # no labels
                [Dict("op" => "points.remove", "labels" => [])],       # empty labels
                [Dict("op" => "track.join", "trackIds" => [1])],       # needs exactly 2
                [Dict("op" => "track.split", "trackId" => 1)])         # needs atT
        @test_throws Cecelia.ParamValidationError Cecelia.parse_track_ops(bad)
    end

    # composite is a 3-step chain: correct, recompute measures, then report which downstream artefacts
    # predate the correction (Decision 4 + Decision 5, docs/todo/CORRECTION_PLAN.md).
    spec = Cecelia._task_spec(Cecelia._task_from_fun_name("tracking.correct_measures"))
    @test spec["composite"] ==
        ["tracking.correct", "tracking.track_measures", "tracking.staleness_report"]
end

# ── Track-issue triage (the worklist old R had no equivalent of) ──────────────
#
# Thresholds are passed EXPLICITLY in these tests, not defaulted: the point is to pin the detection
# logic, not the tuning. The defaults were chosen by measuring on a real image (374 tracks → 31
# candidates, 8.3%) and are documented on the constants; a test that hard-coded them would fail every
# time that measurement is revisited, which is the opposite of useful.
#
# `_issue_df` builds straight-line tracks with a controllable defect.
function _issue_df(tracks::Vector{<:Tuple})   # (track_id, t0, n, x0, step)
    lab, t, tid, x = Float64[], Float64[], Float64[], Float64[]
    n = 0
    for (id, t0, len, x0, step) in tracks, k in 0:(len - 1)
        n += 1
        push!(lab, n); push!(t, t0 + k); push!(tid, id); push!(x, x0 + k * step)
    end
    DataFrame(label = lab, centroid_t = t, track_id = tid,
              centroid_x = x, centroid_y = zeros(length(x)))
end

@testset "track issues — gap → join" begin
    # track 1 ends at t=2 at x=2; track 2 starts at t=3 at x=3 — 1 µm away, one frame later
    df = _issue_df([(1, 0, 3, 0.0, 1.0), (2, 3, 3, 3.0, 1.0)])
    iss = find_track_issues(df, ["centroid_x", "centroid_y"]; gap_steps = 3.0)
    gaps = filter(i -> i.kind == "gap", iss)
    @test length(gaps) == 1
    g = only(gaps)
    @test g.op["op"] == "track.join" && g.op["trackIds"] == [1, 2]
    @test g.at_t == 2.0                              # where to look
    # `reason` is the terse WHAT (a row is scanned); `advice` is the instruction (its tooltip)
    @test occursin("t=2", g.reason) && !occursin("join", g.reason)
    @test occursin("join", g.advice)
    @test length(g.reason) < length(g.advice)

    # too far away in space → not a candidate
    far = _issue_df([(1, 0, 3, 0.0, 1.0), (2, 3, 3, 500.0, 1.0)])
    @test isempty(filter(i -> i.kind == "gap",
                         find_track_issues(far, ["centroid_x", "centroid_y"]; gap_steps = 3.0)))

    # too far away in TIME → not a candidate (gap_frames = 1 excludes a 3-frame gap)
    df2 = _issue_df([(1, 0, 3, 0.0, 1.0), (2, 5, 3, 3.0, 1.0)])
    @test isempty(filter(i -> i.kind == "gap",
                         find_track_issues(df2, ["centroid_x", "centroid_y"]; gap_frames = 1)))
end

@testset "track issues — jump → split" begin
    # one track of 1 µm steps with a single 40 µm leap between t=4 and t=5
    df = _issue_df([(1, 0, 5, 0.0, 1.0), (2, 0, 8, 100.0, 1.0)])
    push!(df, (label = 99.0, centroid_t = 5.0, track_id = 1.0,
               centroid_x = 44.0, centroid_y = 0.0))
    iss = find_track_issues(df, ["centroid_x", "centroid_y"];
                            jump_factor = 4.0, jump_quantile = 0.5)
    jumps = filter(i -> i.kind == "jump", iss)
    @test length(jumps) == 1
    j = only(jumps)
    @test j.op["op"] == "track.split" && j.op["trackId"] == 1
    @test j.op["atT"] == 5.0                         # split AT the far cell
    @test j.at_t == 5.0
    @test occursin("split", j.advice) && !occursin("split", j.reason)

    # a steady track is never a jump candidate, however fast it moves
    steady = _issue_df([(1, 0, 10, 0.0, 25.0)])
    @test isempty(filter(i -> i.kind == "jump",
                         find_track_issues(steady, ["centroid_x", "centroid_y"];
                                           jump_factor = 4.0, jump_quantile = 0.5)))

    # CONSECUTIVE suspect steps collapse into ONE candidate (out-and-back is one mistake)
    ob = _issue_df([(1, 0, 4, 0.0, 1.0), (2, 0, 8, 200.0, 1.0)])
    push!(ob, (label = 98.0, centroid_t = 4.0, track_id = 1.0, centroid_x = 60.0, centroid_y = 0.0))
    push!(ob, (label = 97.0, centroid_t = 5.0, track_id = 1.0, centroid_x = 4.0,  centroid_y = 0.0))
    cj = filter(i -> i.kind == "jump",
                find_track_issues(ob, ["centroid_x", "centroid_y"];
                                  jump_factor = 4.0, jump_quantile = 0.5))
    @test length(cj) == 1
    @test occursin("in a row", only(cj).advice)       # and it SAYS it collapsed them
    @test occursin("×2", only(cj).reason)             # …tersely, in the row itself
end

@testset "track issues — short, ordering, degenerate" begin
    # a short track is a remove candidate…
    df = _issue_df([(1, 0, 2, 0.0, 1.0), (2, 0, 9, 100.0, 1.0)])
    shorts = filter(i -> i.kind == "short",
                    find_track_issues(df, ["centroid_x", "centroid_y"]; min_len = 5))
    @test length(shorts) == 1
    @test only(shorts).op == Dict{String,Any}("op" => "track.remove", "trackIds" => [1])
    # …and not one when the minimum is lower than it
    @test isempty(filter(i -> i.kind == "short",
                         find_track_issues(df, ["centroid_x", "centroid_y"]; min_len = 2)))

    # most suspicious first — the worklist order IS the product
    many = _issue_df([(1, 0, 6, 0.0, 1.0), (2, 0, 6, 50.0, 1.0), (3, 0, 2, 200.0, 1.0)])
    iss = find_track_issues(many, ["centroid_x", "centroid_y"])
    @test issorted([i.severity for i in iss]; rev = true)

    # a completely stationary image has no defensible distance threshold — no crash, no invented µm.
    # Steps EXIST here, they are just all zero, so the scale is 0.0 (not NaN) and the detector must
    # reject it on `> 0`, not on `isfinite` alone.
    still = DataFrame(label = Float64[1, 2, 3], centroid_t = Float64[0, 1, 2],
                      track_id = Float64[1, 1, 1],
                      centroid_x = zeros(3), centroid_y = zeros(3))
    @test track_step_scale(still, ["centroid_x", "centroid_y"]) == 0.0
    @test isempty(filter(i -> i.kind in ("gap", "jump"),
                         find_track_issues(still, ["centroid_x", "centroid_y"])))

    # NO step at all (every track a single timepoint) is the other no-scale case → NaN
    single = DataFrame(label = Float64[1, 2], centroid_t = Float64[0, 0],
                       track_id = Float64[1, 2], centroid_x = Float64[0, 9],
                       centroid_y = zeros(2))
    @test isnan(track_step_scale(single, ["centroid_x", "centroid_y"]))
    @test isempty(filter(i -> i.kind in ("gap", "jump"),
                         find_track_issues(single, ["centroid_x", "centroid_y"])))

    # no centroid columns at all → nothing to say
    @test isempty(find_track_issues(_issue_df([(1, 0, 3, 0.0, 1.0)]), String[]))
end

@testset "track issues — a suggested op is SUBMITTABLE" begin
    # The whole design rests on this: a worklist row is a ready-to-run op, so nothing has to
    # translate "suggestion" into "edit". Detect → validate → apply, with no hand-editing.
    df  = _issue_df([(1, 0, 3, 0.0, 1.0), (2, 3, 3, 3.0, 1.0)])
    iss = find_track_issues(df, ["centroid_x", "centroid_y"]; gap_steps = 3.0)
    g   = only(filter(i -> i.kind == "gap", iss))

    ops = Cecelia.parse_track_ops([issue_to_dict(g)["op"]])    # survives task validation
    @test length(ops) == 1
    for k in Cecelia.TRACK_OP_KINDS
        # the op kind the detector emitted is one the engine knows
        g.op["op"] == k && @test true
    end
    Cecelia.apply_track_ops!(df, ops)                          # and applies cleanly
    @test Cecelia.track_ids_present(df) == [1]                 # the gap is closed

    # …and re-detecting finds nothing left of that kind
    @test isempty(filter(i -> i.kind == "gap",
                         find_track_issues(df, ["centroid_x", "centroid_y"]; gap_steps = 3.0)))

    # the dict form carries everything a UI needs to fly the viewer to the problem
    d = issue_to_dict(g)
    @test Set(keys(d)) == Set(["kind", "op", "trackIds", "atT", "centroid", "severity", "reason", "advice"])
    @test d["centroid"] isa Vector && length(d["centroid"]) == 2
end
