# ── pop_df_multi + tracked_pop_parents + cluster-share testsets ───────
# Three sections covering: pop_df_multi integration (KDIeEm) — cross-value_name pooling,
# tracked_pop_parents (no _tracked row that copies a deeper one, KDIeEm), and cluster pop
# auto-share (co-clustered value_names). Extracted from suite.jl to keep it small enough
# to merge without EOF conflicts on every append. The extracted file loads inside this
# file's aggregating testset scope, so any helpers defined earlier in suite.jl are still
# in scope (lexical include).

@testset "pop_df_multi integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df_multi integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        truth = sum(full.mean_intensity_0 .>= thr)
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)

        # a flow gate resolves + returns the SAME cells as an explicit pop_df("flow", …)
        direct = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        multi  = pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"])
        @test nrow(multi) == truth == nrow(direct)
        @test unique(multi.pop) == ["/pos"]

        # an unknown-type ref (no map contains it) resolves to flow and is skipped → still just /pos
        mixed = pop_df_multi(img, ["/pos", "/nonexistent"]; value_name="B", pop_cols=["area"])
        @test nrow(mixed) == truth

        # dedup: /pos ∪ all-cells root collapses to one row per cell (root = every cell)
        pooled = pop_df_multi(img, ["/pos", "/"]; value_name="B", pop_cols=["area"])
        @test nrow(pooled) == nrow(full)
        @test length(unique(pooled.label)) == nrow(pooled)   # no duplicated cell rows

        # restrict_to guard: keep only the operated-on segmentation's cells (single-seg tasks)
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="B")) == truth
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="OTHER")) == 0

        # END-TO-END on real tracked data (the user's ask): the TRACKED subset of a gate resolves
        # to CELLS via the mixed picker — impossible before (cells picker hid _tracked; the consumer
        # assumed flow and got nothing). B.h5ad carries track_id.
        tid = label_props(img; value_name="B") |> v -> select_cols(v, ["track_id"]) |> as_df
        tracked = Set(tid.label[[x isa Real && isfinite(x) && x > 0 for x in tid.track_id]])
        gate = pop_df(img, "flow", ["/pos"]; value_name="B")
        expected_tracked = count(l -> l in tracked, gate.label)
        @test 0 < expected_tracked < nrow(gate)               # a genuine tracked subset of the gate
        trkd = pop_df_multi(img, ["/pos/_tracked"]; value_name="B", restrict_to="B")
        @test nrow(trkd) == expected_tracked                  # tracked cells now resolve
        @test resolve_pop_type(img, "B", "/pos/_tracked") == "live"
        @test pop_namespace(img, ["/pos/_tracked"]; value_name="B") == "live"
        rm(td; recursive=true)
    end
end

# ── Which populations get a derived `_tracked` row (the picker's rule, on real tracked data) ──
# A `_tracked` under EVERY population that exists is what the picker used to offer: gating a
# segmentation into qc → B → subsets listed five identical "all tracks" rows plus the real subsets.
# The rule is now "only where it says something new", and this checks it against cells that really
# carry `track_id`.
@testset "tracked_pop_parents — no _tracked row that copies a deeper one (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "tracked_pop_parents (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        tid  = label_props(img; value_name="B") |> v -> select_cols(v, ["track_id"]) |> as_df
        trk_of = Dict(Int(l) => Int(t) for (l, t) in zip(tid.label, tid.track_id)
                      if t isa Real && isfinite(t) && t > 0)
        # /all takes every cell, /pos half of them — so /all's tracks ARE the segmentation's tracks
        # and /pos's are (verified below) fewer.
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", -1e12, 1e12, -1e12, 1e12))
        add_pop!(m, "pos"; parent="/all",
                 gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        pos_tracks = Set(trk_of[Int(l)] for l in pop_df(img, "flow", ["/all/pos"]; value_name="B").label
                         if haskey(trk_of, Int(l)))
        @test 0 < length(pos_tracks) < length(Set(values(trk_of)))   # a genuine subset of the tracks

        parents = tracked_pop_parents(img; value_name="B")
        @test !("" in parents)          # root: /all holds the same tracks → the root row is a copy
        @test "/all" in parents         # the population tracking effectively ran on
        @test "/all/pos" in parents     # its own, smaller track set

        # an UNTRACKED segmentation offers nothing at all — no `_tracked` under the gates it has,
        # which is what a freshly gated, not-yet-tracked segmentation used to show. (`is_tracked` is
        # the first exit; a registered value_name with nothing written yet is untracked.)
        img.label_props["C"] = "C.h5ad"
        @test !is_tracked(img; value_name="C")
        @test isempty(tracked_pop_parents(img; value_name="C"))

        # The picker asks this on every load, so the answer is cached on the gating + h5ad mtimes —
        # and a SAVED GATE EDIT has to invalidate it, or the rail keeps answering for the old tree.
        # Deleting /all/pos leaves /all as the deepest pop holding the tracks, so the set changes.
        # Functional oracle over the wall-clock timing the previous shape used: a cache hit does
        # not add a key. `@elapsed` on a sub-ms function on a shared macOS runner picked up JIT/GC
        # jitter (4.7 ms > 1 ms floor) — a hard flake, since the cost this test asserts on already
        # left the wall clock. The Dict-length check is the same assertion as "cache hit".
        n_before = length(Cecelia._TRACKED_PARENTS_CACHE)
        tracked_pop_parents(img; value_name="B")
        @test length(Cecelia._TRACKED_PARENTS_CACHE) == n_before     # second ask hit, no new entry
        del_pop!(m, "/all/pos"); sleep(0.01); save_pop_map!(m, img)
        @test tracked_pop_parents(img; value_name="B") == Set(["/all"])
        rm(td; recursive=true)
    end
end

# ── Cluster-pop auto-share across co-clustered segmentations (CLUSTER_POOLING_PLAN.md) ─────
@testset "cluster pop auto-share (co-clustered value_names)" begin
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # B & T were clustered together (both track sidecars carry suffix "movement"); C was not.
    for vn in ("B", "T")
        open(joinpath(lpdir, "$(vn)__tracks.clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["u1"])))
        end
    end
    # named trackclust pops authored ONLY under B (filter the shared clusters.movement column)
    bm = PopulationMap(pop_type="trackclust", value_name="B")
    add_pop!(bm, "Directed"; filter_measure="clusters.movement", filter_fun="in", filter_values=[3], colour="#c061cb")
    add_pop!(bm, "Scanning"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0], colour="#62a0ea")
    save_pop_map!(bm, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "C" => "C.h5ad", "_active" => "B")

    # co-clustered segmentations for run "movement" (track granularity) = B and T (not C)
    @test Set(Cecelia.co_clustered_value_names(img, "movement"; granularity=:track)) == Set(["B", "T"])

    # B has its OWN sidecar → loaded verbatim
    mb = load_pop_map(img; value_name="B", pop_type="trackclust")
    @test Set(keys(mb.pops)) == Set(["/Directed", "/Scanning"]) && mb.value_name == "B"

    # T has NO sidecar but IS co-clustered → BORROWS B's named pops, relabeled to T so
    # membership resolves over T's own track table
    mt = load_pop_map(img; value_name="T", pop_type="trackclust")
    @test Set(keys(mt.pops)) == Set(["/Directed", "/Scanning"])
    @test mt.value_name == "T" && all(p.value_name == "T" for p in values(mt.pops))
    @test mt.pops["/Directed"].filter_measure == "clusters.movement"

    # C was NOT part of the run (no clustfeatures suffix) → no borrow (empty map)
    mc = load_pop_map(img; value_name="C", pop_type="trackclust")
    @test isempty(mc.pops)

    # BARE cluster-pop ref expands across ALL co-clustered segmentations (R popDT parity)
    @test Set(Cecelia._expand_cluster_pops(img, ["/Directed"], "trackclust", "B")) ==
          Set(["B/Directed", "T/Directed"])
    # explicit value_name-prefixed ref is untouched (single-segmentation request still works)
    @test Cecelia._expand_cluster_pops(img, ["T/Scanning"], "trackclust", "B") == ["T/Scanning"]
    # unknown pop → left as-is (falls back to default_vn downstream); non-cluster type → no-op
    @test Cecelia._expand_cluster_pops(img, ["/Nope"], "trackclust", "B") == ["/Nope"]
    @test Cecelia._expand_cluster_pops(img, ["/x"], "flow", "B") == ["/x"]

    # per-cluster heatmap detection: a matrix over a clusters.{suffix} column pools co-clustered vns
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.movement") == "movement"
    @test Cecelia._cluster_matrix_suffix("matrix", "pop") === nothing      # per-population mode
    @test Cecelia._cluster_matrix_suffix("boxplot", "clusters.movement") === nothing
end
