# Track class (docs/todo/TRACK_SUBPOP_GATING_PLAN.md P5, Decision 5): each track's fraction of cells in
# each sub-population, and the one sub-population above the threshold — strictly — else mixed. Expected
# values below come from that definition, worked by hand, not from the code's output.

@testset "track_class_table — the definition" begin
    T, F = true, false
    # one track per case; member vectors over the cells in order
    tc(ids, ms, kids; th = 0.5) = track_class_table(ids, ms, kids; threshold = th)

    # TTTFT → A at 4/5 = 0.8
    r = tc(fill(1, 5), [[T, T, T, F, T]], ["/p/A"])
    @test r.class == ["/p/A"] && r[1, "frac./p/A"] ≈ 0.8 && r.num_cells == [5]
    # TTFF → 0.5 is not above 0.5 → mixed
    @test tc(fill(1, 4), [[T, T, F, F]], ["/p/A"]).class == [Cecelia.TRACK_CLASS_MIXED]
    # …but above a 0.4 threshold
    @test tc(fill(1, 4), [[T, T, F, F]], ["/p/A"]; th = 0.4).class == ["/p/A"]
    # a cell outside every child counts against purity: A 2/4, B 1/4, one nowhere → mixed
    r = tc(fill(1, 4), [[T, T, F, F], [F, F, T, F]], ["/p/A", "/p/B"])
    @test r.class == [Cecelia.TRACK_CLASS_MIXED] && r[1, "frac./p/B"] ≈ 0.25
    # a tie is mixed even at a low threshold: A 2/4, B 2/4
    @test tc(fill(1, 4), [[T, T, F, F], [F, F, T, T]], ["/p/A", "/p/B"]; th = 0.1).class == [Cecelia.TRACK_CLASS_MIXED]
    # overlapping children are each counted: A 3/3 and B 2/3 → A
    r = tc(fill(1, 3), [[T, T, T], [T, T, F]], ["/p/A", "/p/B"])
    @test r.class == ["/p/A"] && r[1, "frac./p/B"] ≈ 2 / 3
    # a merge (two cells of the track in one frame) is two cells: B 3/5 → B
    @test tc(fill(1, 5), [[T, T, F, F, F], [F, F, T, T, T]], ["/p/A", "/p/B"]).class == ["/p/B"]
    # several tracks, unsorted input → one sorted row each
    r = tc([7, 3, 7, 3, 3], [[T, F, T, T, T]], ["/p/A"])
    @test r.track_id == [3, 7] && r.num_cells == [3, 2] && r.class == ["/p/A", "/p/A"] && r[1, "frac./p/A"] ≈ 2 / 3
    # no children → every track mixed
    @test tc([1, 1], Vector{Bool}[], String[]).class == [Cecelia.TRACK_CLASS_MIXED]
end

@testset "track class — children, level, settings" begin
    m = PopulationTree(; pop_type = "flow", value_name = "x")
    g = RectangleGate("area", "area", 0, 1, 0, 1)
    add_pop!(m, "P"; gate = g)
    add_pop!(m, "A"; parent = "/P", gate = g); add_pop!(m, "B"; parent = "/P", gate = g)
    add_pop!(m, "A1"; parent = "/P/A", gate = g); add_pop!(m, "A2"; parent = "/P/A", gate = g)
    add_pop!(m, "tracked"; parent = "/P", filter_measure = "track_id", filter_fun = "gt", filter_values = 0)
    @test Cecelia.track_class_children(m, "/P") == ["/P/A", "/P/B"]               # `tracked` is the parent again
    @test Cecelia.track_class_children(m, "/P"; level = 2) == ["/P/A/A1", "/P/A/A2", "/P/B"]   # B stands for itself
    @test Cecelia.track_class_max_level(m, "/P") == 2 && Cecelia.track_class_max_level(m, "/P/B") == 0
    @test track_class_settings(m.pops["/P"]) == (; threshold = 0.5, level = 1)
    @test Cecelia.track_class_setting_error(1.0, 1) !== nothing && Cecelia.track_class_setting_error(0.6, 0) !== nothing
    @test Cecelia.track_class_setting_error(0.6, 2) === nothing
    # stored on the parent, round-trips through the file shape
    m.pops["/P"].track_class = Dict{String,Any}("threshold" => 0.7, "level" => 2)
    m2 = Cecelia.from_tree(JSON3.read(JSON3.write(Cecelia.to_tree(m)), Dict{String,Any}))
    @test track_class_settings(m2.pops["/P"]) == (; threshold = 0.7, level = 2)
    @test !haskey(Cecelia.to_tree(m2)["populations"][1]["children"][2], "trackClass")   # unset stays absent
end

@testset "track_classes on a tracked movie" begin
    fx = fixture_for(:cells, :movie, :tracked)
    if fx === nothing
        @test_skip "tracked movie fixture (missing)"
    else
        td = mktempdir(); cp(fx.project, joinpath(td, basename(fx.project)))
        old = get(Cecelia.cecelia_conf()["dirs"], "projects", nothing)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = td
            img = init_object(basename(fx.project), fx.uid); vn = fx.value_name
            cells = Cecelia.label_props(img; value_name = vn) |> lp -> Cecelia.select_cols(lp, ["area", "track_id"]) |> Cecelia.as_df
            a = sort(cells.area); cut = a[length(a) ÷ 2]
            m = load_pop_tree(img; value_name = vn, pop_type = "flow")
            add_pop!(m, "all"; gate = RectangleGate("area", "area", -1e12, 1e12, -1e12, 1e12))
            add_pop!(m, "small"; parent = "/all", gate = RectangleGate("area", "area", -1e12, cut, -1e12, 1e12))
            add_pop!(m, "big";   parent = "/all", gate = RectangleGate("area", "area", cut, 1e12, -1e12, 1e12))
            save_pop_tree!(m, img)

            r = track_classes(img; value_name = vn, parent = "/all")
            @test r.children == ["/all/small", "/all/big"] && r.threshold == 0.5 && r.max_level == 1
            # the definition, recomputed here from the table itself: one row per track
            tr = [(Int(t), x) for (t, x) in zip(cells.track_id, cells.area) if t isa Real && isfinite(t) && t > 0]
            @test nrow(r.table) == length(unique(first.(tr)))
            t1 = r.table.track_id[1]
            xs = [x for (t, x) in tr if t == t1]
            # gate bounds are inclusive both sides, so a cell exactly at `cut` is in both
            @test r.table[1, "frac./all/small"] ≈ count(<=(cut), xs) / length(xs)
            @test r.table[1, "frac./all/big"] ≈ count(>=(cut), xs) / length(xs)
            s = Cecelia.track_class_summary(r)
            @test sum(s.counts) + s.mixed == s.total == nrow(r.table)
            # a stricter stored threshold leaves at least as many mixed
            m = load_pop_tree(img; value_name = vn, pop_type = "flow")
            m.pops["/all"].track_class = Dict{String,Any}("threshold" => 0.9)
            save_pop_tree!(m, img)
            r9 = track_classes(img; value_name = vn, parent = "/all")
            @test r9.threshold == 0.9 && Cecelia.track_class_summary(r9).mixed >= s.mixed

            # per step: each tracked cell's (track, frame) → its sub-population, from the table itself.
            # `small` and `big` share the cells at exactly `cut`; there the first (`small`) wins.
            st = track_step_children(img; value_name = vn, parent = "/all")
            @test st.children == ["/all/small", "/all/big"]
            frames = Cecelia.as_df(Cecelia.select_cols(Cecelia.label_props(img; value_name = vn), ["area", "track_id", "centroid_t"]))
            want = Dict{Tuple{Int,Int},Int}()
            for (t, f, x) in zip(frames.track_id, frames.centroid_t, frames.area)
                (t isa Real && isfinite(t) && t > 0) || continue
                k = (Int(t), Int(round(f))); j = x <= cut ? 1 : 2
                want[k] = min(get(want, k, j), j)
            end
            @test !isempty(want) && st.step == want
            @test isempty(track_step_children(img; value_name = vn, parent = "/all/big").step)   # no sub-populations
            @test_throws ErrorException track_step_children(img; value_name = vn, parent = "/nope")
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
        end
    end
end
