# ── pop_df core testsets ──────────────────────────────────────────────
# Nine sections covering the pop_df read path: pop_df centroids (KDIeEm), pop_df pooling
# + dedup, pop_df drop_na, pop_df track_id dedup key, pop_df live _tracked (derived
# filter), reserved pop names (_ prefix), pop_df cache auto-invalidation, pop_df
# integration (KDIeEm), and pop_df labels honours the value_name prefix. Extracted from
# suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope (lexical include).

@testset "pop_df centroids (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df centroids (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        cp(fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad"),
           joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        cent = ["centroid_z", "centroid_y", "centroid_x", "centroid_t"]

        # a narrowed read (pop_cols) does NOT carry coordinates …
        base = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test !any(c -> c in names(base), cent)
        # … until `centroids` widens the PUSHDOWN to include them
        px = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:pixel)
        @test all(c -> c in names(px), cent)
        @test "area" in names(px)                       # the requested column is still there
        @test nrow(px) == nrow(base)

        # this fixture is UNCALIBRATED, so :physical must return PIXELS rather than relabel them µm
        @test !img_is_calibrated(img)
        ph = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:physical)
        @test ph.centroid_x == px.centroid_x

        # :pixel and :physical share ONE cached read (the cache holds the frame as read, in pixels,
        # and the unit conversion happens on the returned copy) — so a :physical call cannot leave
        # scaled values behind for a later :pixel caller.
        @test pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"],
                     centroids=:pixel).centroid_x == px.centroid_x
        # On this uncalibrated image the conversion is the identity, so the line above cannot fail.
        # Calibrate it (z ≠ x) and ask again: :physical scales, and a :pixel read after it still
        # gets pixels from the shared cache entry.
        merge!(img.meta, Dict("PhysicalSizeX" => "0.5", "PhysicalSizeY" => "0.5", "PhysicalSizeZ" => "2.0"))
        @test img_is_calibrated(img)
        phc = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:physical)
        @test phc.centroid_x ≈ px.centroid_x .* 0.5
        @test phc.centroid_z ≈ px.centroid_z .* 2.0
        @test pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"],
                     centroids=:pixel).centroid_x == px.centroid_x
        empty!(img.meta)

        # …and `false` is a DIFFERENT read (different columns), so it must not share their entry
        @test !("centroid_x" in names(pop_df(img, "labels", String[]; value_name="B",
                                             pop_cols=["area"])))

        # the no-columns read already returns coordinates, so `centroids` changes nothing there
        wide = pop_df(img, "labels", String[]; value_name="B", centroids=:pixel)
        @test all(c -> c in names(wide), cent)

        # the keyword is validated, and is advertised on the public method
        @test_throws ErrorException pop_df(img, "labels", String[]; value_name="B", centroids=:um)
        @test :centroids in Base.kwarg_decl(
            only(methods(pop_df, (CciaImage, AbstractString, Any))))
    end
end

# ── pop_df: pooling across value_names + dedup to most-specific pop ────────
@testset "pop_df pooling + dedup" begin
    dfA = DataFrame(label=[1, 2, 3], x=[1.0, 6.0, 9.0])
    dfB = DataFrame(label=[10, 11], x=[7.0, 2.0])
    mA = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(mA, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(mA, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    mB = PopulationTree(pop_type="flow", value_name="B")
    add_pop!(mB, "hi"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    maps = Dict("A" => mA, "B" => mB)
    load_map = vn -> maps[vn]
    fetch = (vn, _) -> (vn == "A" ? dfA : dfB)

    # request /p and /p/c from A, and hi from B (prefixed)
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c", "B/hi"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test Set(unique(res.value_name)) == Set(["A", "B"])      # pooled across value_names
    # dedup: label 2 & 3 are in both /p and /p/c → assigned the most specific (/p/c)
    getpop(l, vn) = only(res[(res.label .== l) .& (res.value_name .== vn), :pop])
    @test getpop(1, "A") == "/p"
    @test getpop(2, "A") == "/p/c"
    @test getpop(3, "A") == "/p/c"
    @test getpop(10, "B") == "/hi"                            # x=7 ≥ 5
    @test nrow(res[res.value_name .== "B", :]) == 1           # label 11 (x=2) excluded

    # the all-cells root under a prefix: `B/` (the picker) AND `B/root` (saved boards carried it) are
    # the whole table — `B/root` used to parse as a pop "/root" no map has and came back silently empty
    for ref in ("B/", "B/root")
        r = Cecelia._pop_df(load_map, fetch, "flow", [ref]; default_vn="A", pop_cols=["x"])
        @test sort(r.label) == [10, 11] && all(==("B"), r.value_name)
    end
    r = Cecelia._pop_df(load_map, fetch, "flow", ["B/root/hi"]; default_vn="A", pop_cols=["x"])
    @test r.label == [10] && r.pop == ["/hi"]                 # a root-anchored child path, too
end

# ── pop_df: drop_na drops NA/NaN cells in requested pop_cols ──────────────
@testset "pop_df drop_na" begin
    # NaN is in a measure column (x), NOT the gate column (g) — a NaN gate value would
    # fail the gate comparison and never enter the result, masking what drop_na does.
    df = DataFrame(label=[1, 2, 3], g=[0.0, 0.0, 0.0], x=[1.0, NaN, 9.0])
    m = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("g", "g", -1e9, 1e9, -1e9, 1e9))  # all pass
    load_map = _ -> m
    fetch = (_, _) -> df
    keep = Cecelia._pop_df(load_map, fetch, "flow", ["/p"]; default_vn="A", pop_cols=["x"])
    @test nrow(keep) == 3                                     # NaN row kept by default
    dropped = Cecelia._pop_df(load_map, fetch, "flow", ["/p"];
                              default_vn="A", pop_cols=["x"], drop_na=true)
    @test sort(dropped.label) == [1, 3]                       # label 2 (x=NaN) dropped
end

# ── pop_df: track_id joins the dedup key when present (most-specific pop still wins) ──
@testset "pop_df track_id dedup key" begin
    df = DataFrame(label=[1, 2], x=[9.0, 9.0], track_id=[10.0, 20.0])
    m = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test "track_id" in names(res)                            # track_id carried through
    @test nrow(res) == 2                                      # still one row per cell
    @test Set(res.pop) == Set(["/p/c"])                       # most-specific pop wins
end

# ── pop_df: derived live "_tracked" pop (track_id>0 filter on a gated parent) ──
@testset "pop_df live _tracked (derived filter)" begin
    # label4 fails the qc gate (x=1); label2 is in qc but untracked (track_id=NaN).
    df = DataFrame(label=[1, 2, 3, 4], x=[9.0, 9.0, 9.0, 1.0], track_id=[10.0, NaN, 20.0, 30.0])
    m = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # qc = {1,2,3}
    # "_tracked" is derived, not stored — injecting it adds a filtered child of /qc
    Cecelia._inject_derived_pops!(m, ["/qc/_tracked"], "live")
    @test has_pop(m, "/qc/_tracked")
    @test m.pops["/qc/_tracked"].filter_measure == "track_id"
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "live", ["/qc/_tracked"];
                          default_vn="A", pop_cols=["track_id"])
    @test sort(res.label) == [1, 3]                # qc ∩ track_id>0 (label2 NaN, label4 not in qc)
    @test unique(res.pop) == ["/qc/_tracked"]
    # a derived pop is only injected under its registered pop_type (foreign type → skip)
    m2 = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(m2, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    Cecelia._inject_derived_pops!(m2, ["/qc/_tracked"], "clust")  # _tracked is a `live` spec
    @test !has_pop(m2, "/qc/_tracked")
    # an unknown `_`-name is not derived either
    Cecelia._inject_derived_pops!(m2, ["/qc/_nope"], "live")
    @test !has_pop(m2, "/qc/_nope")
end

# ── reserved derived-pop namespace: `_`-prefixed names can't be hand-drawn gates ──
@testset "reserved pop names (_ prefix)" begin
    @test is_reserved_pop_name("_tracked")
    @test is_reserved_pop_name("_anything")
    @test !is_reserved_pop_name("qc")
    m = PopulationTree(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    # a hand-drawn gate may not take a reserved name
    @test_throws ErrorException add_pop!(m, "_tracked"; parent="/qc",
                                         gate=RectangleGate("x", "x", 0.0, 1.0, 0.0, 1.0))
    @test_throws ErrorException rename_pop!(m, "/qc", "_qc")
    # the derived injection (reserved_ok) is allowed to create it
    add_pop!(m, "_tracked"; parent="/qc", filter_measure="track_id", filter_fun="gt",
             filter_values=0, transient=true, reserved_ok=true)
    @test has_pop(m, "/qc/_tracked")
    # round-trips through from_tree (reconstruction bypasses the guard)
    m3 = from_tree(to_tree(m; include_transient=true))
    @test has_pop(m3, "/qc/_tracked")
end

# ── pop_df: cache key folds in file mtimes → auto-invalidates on gate/h5ad change ──
@testset "pop_df cache auto-invalidation" begin
    td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img = CciaImage(uid="X", dir=td)
    img.label_props["A"] = "A.h5ad"
    write(joinpath(td, "gating", "A.json"), "{}")
    write(joinpath(td, "labelProps", "A.h5ad"), "x")
    key() = Cecelia._pop_df_cache_key(img, "flow", "A", ["/qc"], nothing,
                                      false, true, true, false, false, :cell, String[], String[])
    k1 = key()
    sleep(0.05); touch(joinpath(td, "labelProps", "A.h5ad"))   # re-track rewrites h5ad
    k2 = key()
    @test k1 != k2
    write(joinpath(td, "gating", "A.json"), "{\"x\":1}")          # gate edit rewrites map
    k3 = key()
    @test k3 != k2
    # the KEY is the request; the inputs live in the STAMP, so an edit replaces the slot
    @test k1.key == k2.key == k3.key
    # a borrowed sibling map and the calibration are inputs too
    write(joinpath(td, "gating", "Other__clust.json"), "{}")
    @test key().stamp != k3.stamp
    # …and the image directory is part of the key: two images never share a slot
    img2 = CciaImage(uid="Y", dir=mktempdir()); img2.label_props["A"] = "A.h5ad"
    @test Cecelia._pop_df_cache_key(img2, "flow", "A", ["/qc"], nothing, false, true, true, false,
                                    false, :cell, String[], String[]).key != k1.key
    # a branch read also reads the per-branch table — a re-run of it is an input; a flow read is not
    bkey() = Cecelia._pop_df_cache_key(img, "branch", "A", ["/qc"], nothing,
                                       false, true, true, false, false, :cell, String[], String[])
    kb = bkey(); kf = key()
    write(Cecelia.img_branch_props_path(img, "A"), "branches")
    @test bkey().stamp != kb.stamp
    @test key().stamp == kf.stamp
end

# ── pop_df: integration on real KDIeEm (gate eval over real H5AD) ─────────
@testset "pop_df integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)] # ~median → discriminating
        truth = sum(full.mean_intensity_0 .>= thr)
        @test 0 < truth < nrow(full)                          # genuinely partial selection

        m = PopulationTree(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12))
        save_pop_tree!(m, img)
        df = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area", "mean_intensity_0"])
        @test nrow(df) == truth
        @test Set(names(df)) ⊇ Set(["label", "area", "mean_intensity_0", "pop", "value_name"])
        @test all(df.mean_intensity_0 .>= thr)                # every returned cell passes the gate
        @test unique(df.pop) == ["/pos"]

        # channel-name resolution: pop_df renames intensity cols to channel names by default,
        # raw_channel_names=true keeps the {measure}_intensity_{i} names (channel names are
        # stored under the default version, so value_name="B" falls back to it)
        set_channel_names!(img, ["CD4", "CD8", "CD3", "CD19"]; check_length=false)
        named = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true)
        @test "CD4" in names(named) && !("mean_intensity_0" in names(named))
        raw = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true, raw_channel_names=true)
        @test "mean_intensity_0" in names(raw) && !("CD4" in names(raw))

        # value_name=nothing resolves to the active segmentation (img.label_props _active="B")
        auto = pop_df(img, "flow", ["/pos"]; pop_cols=["area"])
        @test nrow(auto) == truth

        # cache: a request is stored under its signature key; flush_cache recomputes
        ck = Cecelia._pop_df_cache_key(img, "flow", "B", ["/pos"], ["area"],
                                       false, true, true, false, false, :cell, String[], String[])
        cached = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        @test haskey(Cecelia._POP_DF_CACHE, ck.key)
        fresh = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"], flush_cache=true)
        @test nrow(cached) == truth && nrow(fresh) == truth
        # the cache outlives the image OBJECT: every request builds a fresh `CciaImage`, and the old
        # per-object cache never hit across them
        img_b = CciaImage(uid="KDIeEm", dir=td)   # MAINT-EXEMPT: the same image as `img` above, a second object
        img_b.label_props["B"] = "B.h5ad"; img_b.label_props["_active"] = "B"
        merge!(img_b.im_channel_names, img.im_channel_names)
        ck_b = Cecelia._pop_df_cache_key(img_b, "flow", "B", ["/pos"], ["area"],
                                         false, true, true, false, false, :cell, String[], String[])
        @test ck_b == ck
        hit = Cecelia.cache_get(Cecelia._POP_DF_CACHE, ck_b.key, ck_b.stamp)
        @test hit !== nothing && nrow(something(hit)) == truth
        # a hit hands back a copy: mutating it leaves the cached frame intact
        got = pop_df(img_b, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        got.area .= -1
        @test nrow(pop_df(img_b, "flow", ["/pos"]; value_name="B", pop_cols=["area"])) == truth
        @test all(pop_df(img_b, "flow", ["/pos"]; value_name="B", pop_cols=["area"]).area .>= 0)
        # a saved gate edit recomputes in place of the old slot
        m2 = PopulationTree(pop_type="flow", value_name="B")
        add_pop!(m2, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                               -1e12, 1e12, -1e12, 1e12))
        save_pop_tree!(m2, img_b)
        @test nrow(pop_df(img_b, "flow", ["/pos"]; value_name="B", pop_cols=["area"])) == nrow(full)
    end
end

# ── pop_df "labels": the value_name PREFIX selects the segmentation ───────────────
# `labels` has no sub-populations, so the branch used to ignore `pops` entirely and read the image's
# ACTIVE segmentation — meaning the QC canvas returned the active label set's cells whichever one the
# picker asked for: the WRONG DATA under the RIGHT LABEL, with no error. The prefix half of a pop ref
# still carries the value_name, and it has to be honoured like every other pop_type's.
@testset "pop_df labels honours the value_name prefix (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df labels prefix (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        # a SECOND segmentation on the same image that disagrees with B (KDIeEx: a known third fewer
        # cells), so reading B for C, or pooling B twice, changes the counts below
        distinct_image!(td; value_name = "C", tracks = false)
        want = distinct_cell_counts(h5)
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"
        img.label_props["C"] = "C.h5ad"
        img.label_props["_active"] = "B"                # active is B — the old code always returned B

        n1 = nrow(pop_df(img, "labels", ["B/labels"]; pop_cols=["area"]))
        @test n1 == want.b

        # the PREFIX picks the segmentation, not the active one
        onlyC = pop_df(img, "labels", ["C/labels"]; pop_cols=["area"])
        @test unique(onlyC.value_name) == ["C"]
        @test nrow(onlyC) == want.x

        # both segmentations pool in ONE call — the QC canvas comparing two label sets side by side
        both = pop_df(img, "labels", ["B/labels", "C/labels"]; pop_cols=["area"])
        @test Set(unique(both.value_name)) == Set(["B", "C"])
        @test unique(both.pop) == ["/labels"]
        # `label` is unique only WITHIN a segmentation, so the pooled frame repeats ids across
        # value_names — nothing may dedup by label alone or one segmentation's cells vanish
        @test nrow(both) == want.b + want.x
        @test count(==("C"), both.value_name) == want.x

        # a segmentation absent on THIS image is skipped, not an error: a set-level call spans images
        # segmented differently (a cellpose run yielding zero objects writes no labelProps at all)
        miss = pop_df(img, "labels", ["B/labels", "nope/labels"]; pop_cols=["area"])
        @test unique(miss.value_name) == ["B"] && nrow(miss) == n1

        # no pops, or a leading-slash ref, still resolves to the ACTIVE segmentation — unchanged
        @test unique(pop_df(img, "labels", String[]; pop_cols=["area"]).value_name) == ["B"]
        @test unique(pop_df(img, "labels", ["/labels"]; pop_cols=["area"]).value_name) == ["B"]
    end
end
