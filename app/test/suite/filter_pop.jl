# ── Filter-pop + compound + FilterFun/FilterCondition testsets ────────
# Five sections covering: region composition column naming (one namer, Julia → Python),
# compound filter populations (Decision 15 — AND-ed conditions), FilterFun enum +
# FilterCondition boundary coercion + JSON round-trip, recompute! — a missing filter/gate
# column degrades to empty (no crash), and colour_by_palette (pop colour else default).
# Extracted from suite.jl to keep it small enough to merge without EOF conflicts on every
# append. The extracted file loads inside this file's aggregating testset scope, so any
# helpers defined earlier in suite.jl are still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

@testset "region composition column naming (one namer, Julia → Python)" begin
    # Julia names the composition columns AND records them in the sidecar; the Python runner is
    # handed the same list. They used to be derived independently and disagreed, so the region
    # composition heatmap asked for columns that did not exist.
    @test Cecelia._comp_col("B/qc/_tracked", "immune") == "spatial.comp.B_qc__tracked.immune"
    @test Cecelia._comp_col("T cells", "x") == "spatial.comp.T_cells.x"
    @test Cecelia._comp_col("plain", "s") == "spatial.comp.plain.s"
end

@testset "compound filter populations (Decision 15 — AND-ed conditions)" begin
    # a user-defined filter pop combining two obs conditions in ONE pop: CD4>0.5 AND speed>5
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "CD4hi_fast"; colour="#8b5cf6", filter_conditions=[
        Dict("measure" => "live.cell.cd4",   "fun" => "gt", "values" => 0.5),
        Dict("measure" => "live.cell.speed", "fun" => "gt", "values" => 5)])
    fetch = _ -> DataFrame("label" => [1, 2, 3, 4],
                           "live.cell.cd4"   => [0.9, 0.9, 0.1, 0.6],
                           "live.cell.speed" => [10,  2,   10,  7])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/CD4hi_fast")) == Set([1, 4])   # BOTH hold: (0.9,10) and (0.6,7)

    # single fields mirror conditions[1] so single-field readers still work
    p = pop_at(m, "/CD4hi_fast")
    @test p.filter_measure == "live.cell.cd4" && p.filter_fun == Cecelia.FILTER_GT && length(p.filter_conditions) == 2

    # round-trip through to_tree/from_tree preserves the conditions + membership
    m2 = from_tree(to_tree(m)); recompute!(m2, fetch)
    @test Set(cells_in_pop(m2, "/CD4hi_fast")) == Set([1, 4])
    @test length(pop_at(m2, "/CD4hi_fast").filter_conditions) == 2

    # a missing condition column → the whole pop degrades to empty (warns), never crashes
    fetch1 = _ -> DataFrame("label" => [1, 2], "live.cell.cd4" => [0.9, 0.1])   # speed absent
    @test_logs (:warn, r"live\.cell\.speed") match_mode=:any recompute!(m, fetch1)
    @test isempty(cells_in_pop(m, "/CD4hi_fast"))
end

@testset "FilterFun enum + FilterCondition — boundary coercion + JSON round-trip" begin
    # `filter_fun` is stored as a `FilterFun` enum; string/Symbol kwargs at add_pop! and API-body
    # boundaries are coerced via `parse_filter_fun`. `filter_conditions` is a `Vector{FilterCondition}`
    # after normalisation (JSON dicts and NamedTuples both accepted at the boundary).
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "single"; filter_measure="x", filter_fun="gt", filter_values=1)
    add_pop!(m, "enum_kwarg"; filter_measure="x", filter_fun=Cecelia.FILTER_GTE, filter_values=1)
    @test pop_at(m, "/single").filter_fun === Cecelia.FILTER_GT
    @test pop_at(m, "/enum_kwarg").filter_fun === Cecelia.FILTER_GTE
    @test pop_at(m, "/single").filter_conditions === nothing

    # compound: string funs and Symbol funs both coerce, and the produced conditions ARE typed
    add_pop!(m, "compound"; filter_conditions=[
        Dict("measure" => "a", "fun" => "in",  "values" => [1, 2]),
        Dict("measure" => "b", "fun" => :neq, "values" => 0)])
    p = pop_at(m, "/compound")
    @test p.filter_conditions isa Vector{Cecelia.FilterCondition}
    @test p.filter_conditions[1].fun === Cecelia.FILTER_IN
    @test p.filter_conditions[2].fun === Cecelia.FILTER_NEQ
    # single-field mirror still works (conditions[1] onto the single fields)
    @test p.filter_measure == "a" && p.filter_fun === Cecelia.FILTER_IN

    # sidecar serialises the wire string (`"in"`, NOT `"FILTER_IN"`) — enforced so the frontend and
    # any older Julia consumer keep reading a raw string.
    td = mktempdir(); save_pop_map!(m, td)
    raw = JSON3.read(read(gating_path(td, "B"), String))
    funs = String[]
    walk = node -> begin
        f = get(node, :filter, nothing)
        if f !== nothing
            push!(funs, String(get(f, :fun, "")))
            for c in get(f, :conditions, [])
                push!(funs, String(get(c, :fun, "")))
            end
        end
        for c in get(node, :children, [])
            walk(c)
        end
    end
    for root in raw.populations
        walk(root)
    end
    @test Set(funs) ⊆ Set(["gt", "gte", "in", "neq"])   # all wire-form strings
    @test !any(startswith(f, "FILTER_") for f in funs)

    # read back — the loader coerces sidecar strings into the enum + typed FilterCondition
    m2 = load_pop_map(td, "B")
    @test pop_at(m2, "/single").filter_fun === Cecelia.FILTER_GT
    p2 = pop_at(m2, "/compound")
    @test p2.filter_conditions isa Vector{Cecelia.FilterCondition}
    @test p2.filter_conditions[1].fun === Cecelia.FILTER_IN

    # parse_filter_fun rejects garbage with an ArgumentError; catalog covers all wire forms
    @test Cecelia.parse_filter_fun("lte") === Cecelia.FILTER_LTE
    @test_throws ArgumentError Cecelia.parse_filter_fun("gtish")

    # gating_engine._filter_mask now takes FilterFun | Nothing directly (no string branching)
    col = [1.0, 2.0, 3.0, 4.0]
    @test Cecelia._filter_mask(col, Cecelia.FILTER_GT,  2.0, false) == BitVector([false, false, true, true])
    @test Cecelia._filter_mask(col, Cecelia.FILTER_IN,  [1.0, 3.0], false) == BitVector([true, false, true, false])
    @test Cecelia._filter_mask(col, nothing, nothing, true) == BitVector([true, true, true, true])
end

@testset "recompute! — a missing filter/gate column degrades to empty (no crash)" begin
    # A cluster pop whose `clusters.{suffix}` column isn't in the fetched frame — e.g. evaluated
    # against a segmentation that didn't take part in that run, so `fetch_cols` silently dropped it
    # — must resolve to NO members, not raise `ArgumentError: column name … not found` and 500 the
    # whole plot. Regression: the trackclust heatmap crash (clusters.default not found).
    m = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(m, "present"; filter_measure="clusters.movement", filter_fun="in", filter_values=[1, 2], colour="#10b981")
    add_pop!(m, "absent";  filter_measure="clusters.default",  filter_fun="in", filter_values=[0, 1], colour="#ef4444")
    # frame HAS clusters.movement but NOT clusters.default
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13], "clusters.movement" => [0, 1, 2, 1])
    @test_logs (:warn, r"clusters\.default") match_mode=:any recompute!(m, fetch)  # warns, doesn't throw
    @test Set(cells_in_pop(m, "/present")) == Set([11, 12, 13])   # present column resolves normally
    @test isempty(cells_in_pop(m, "/absent"))                     # missing column → empty membership

    # same guard for a GATE whose axis column is absent from the frame
    mg = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(mg, "g"; gate=RectangleGate("x", "missingY", 0.0, 10.0, 0.0, 10.0), colour="#abcdef")
    fetchg = _ -> DataFrame("label" => [1, 2], "x" => [1.0, 2.0])   # no "missingY"
    @test_logs (:warn, r"missingY") match_mode=:any recompute!(mg, fetchg)
    @test isempty(cells_in_pop(mg, "/g"))
end

@testset "colour_by_palette — pop colour else default" begin
    # a value a user pop FILTERS for on the column → that pop's colour; the rest → OKABE_ITO by
    # sorted position. Generalises "use the population's colour where one exists" (a cluster pop is
    # just a filter on clusters.{suffix}).
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "directed";   filter_measure="clusters.mov", filter_fun="in", filter_values=[2],       colour="#ff1493")
    add_pop!(m, "crawling";   filter_measure="clusters.mov", filter_fun="in", filter_values=[0, 3, 4], colour="#ffd700")
    add_pop!(m, "unrelated";  filter_measure="clusters.other", filter_fun="in", filter_values=[1],     colour="#000001")

    pal = colour_by_palette(m, "clusters.mov", [0, 1, 2, 3, 4])
    @test pal[2] == "#ff1493"                 # user pop colour
    @test pal[0] == "#ffd700" && pal[3] == "#ffd700" && pal[4] == "#ffd700"
    @test pal[1] == OKABE_ITO[1]              # uncovered value 1 → first default
    # a pop filtering a DIFFERENT column never leaks its colour in
    @test pal[1] != "#000001"

    # numeric tolerance: a filter value stored as 2.0 still matches integer column value 2
    m3 = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m3, "d"; filter_measure="clusters.mov", filter_fun="in", filter_values=[2.0], colour="#abcdef")
    @test colour_by_palette(m3, "clusters.mov", [2])[2] == "#abcdef"

    # no matching pop → all default, by sorted position (stable)
    empty = PopulationMap(pop_type="clust", value_name="B")
    p2 = colour_by_palette(empty, "clusters.mov", [5, 3, 3, 1])
    @test p2[1] == OKABE_ITO[1] && p2[3] == OKABE_ITO[2] && p2[5] == OKABE_ITO[3]

    # pop_colour_overrides: string-keyed {value => hex} for the wire (2.0/2 → "2"); only pops on
    # the column contribute; no default fill (the bridge does that).
    ov = pop_colour_overrides(m, "clusters.mov")
    @test ov == Dict("2" => "#ff1493", "0" => "#ffd700", "3" => "#ffd700", "4" => "#ffd700")
    @test pop_colour_overrides(m3, "clusters.mov") == Dict("2" => "#abcdef")   # 2.0 → "2"
    @test isempty(pop_colour_overrides(m, "clusters.absent"))

    # pop_label_overrides: same keying, value → the POP NAME (so the legend reads "directed", not "2")
    lbl = pop_label_overrides(m, "clusters.mov")
    @test lbl == Dict("2" => "directed", "0" => "crawling", "3" => "crawling", "4" => "crawling")
    @test isempty(pop_label_overrides(m, "clusters.absent"))
end
