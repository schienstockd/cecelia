# The aggregate-run steps (aggregate_runs.jl) at their stated edges: each expectation is the docstring's
# own word — "within `dist`", "≤ `max_gap` frames back", "`max_cells ≤ 0` = no cap", "the movie's first
# frame (0)", "judged at where the event began". Why: docs/todo/TEST_VALUE_PLAN.md → P3.

@testset "aggregate run steps at their edges" begin
    # within / preceded / linking distances are inclusive: a point exactly `dist` away counts
    @test Cecelia._within_mask([0.0 0.0], [0.0], [3.0 0.0], [0.0], 3.0) == [true]
    @test Cecelia._within_mask([0.0 0.0], [0.0], [3.0 0.0], [0.0], 2.999) == [false]
    @test Cecelia._within_mask([0.0 0.0], [0.0], [0.5 0.0], [0.0], 1.0) == [true]     # 1 µm is on
    ref = Cecelia._points_by_frame([3.0 0.0], [0.0])
    @test Cecelia._preceded_fraction(1.0, [0.0, 0.0], ref, 1, 3.0, 0.0) == 1.0
    @test Cecelia._preceded_fraction(1.0, [0.0, 0.0], ref, 1, 2.999, 0.0) == 0.0
    @test Cecelia._link_events([0.0, 1.0], [0.0 0.0; 1.5 0.0], 1, 1.5) == [1, 1]
    @test Cecelia._link_events([0.0, 1.0], [0.0 0.0; 1.5 0.0], 1, 1.49) == [1, 2]
    # a gap of exactly max_gap frames still links
    @test Cecelia._link_events([0.0, 2.0], [0.0 0.0; 0.0 0.0], 2, 1.0) == [1, 1]
    @test Cecelia._link_events([0.0, 3.0], [0.0 0.0; 0.0 0.0], 2, 1.0) == [1, 2]

    # 0 µm is off even for a reference point on top of the cell; no reference points → nothing excluded
    @test Cecelia._within_mask([0.0 0.0; 5.0 0.0], [0.0, 0.0], [0.0 0.0], [0.0], 0.0) == [false, false]
    @test Cecelia._within_mask([0.0 0.0; 5.0 0.0], [0.0, 0.0], zeros(0, 2), Float64[], 2.0) == [false, false]

    # a still image (no frames): one group, isolation still judged
    @test Cecelia._isolated_mask([0.0 0.0; 1.0 0.0; 20.0 0.0], nothing, 2.0) == [false, false, true]

    # a cap of 1 keeps only single-cell aggregates
    @test Cecelia._cap_aggregates([1, 1, 2, 0], 1) == [0, 0, 2, 0]

    # one event alone is event 1
    @test Cecelia._link_events([4.0], [0.0 0.0], 1, 1.0) == [1]

    # cells in no aggregate (id 0) and not isolated are in no event; isolated-only events count from 1
    @test Cecelia._aggregate_events([1, 1, 0], [false, false, false], [0.0 0.0; 1.0 0.0; 50.0 0.0],
                                    [0.0, 0.0, 0.0], 1, 2.0) == [1, 1, 0]
    @test Cecelia._aggregate_events([0, 0], [true, true], [0.0 0.0; 50.0 0.0], [0.0, 0.0], 1, 2.0) == [1, 2]

    # the movie starts at frame 0 by default: an event at frame 1 looks back at frame 0
    ref0 = Cecelia._points_by_frame([0.0 0.0], [0.0])
    @test Cecelia._event_preceded([1], [0.0 0.0], [1.0], ref0, 5, 1.0) == [1.0]

    # judged where the event BEGAN: it starts beside the reference cell and then moves away
    ev = [1, 1]; coords = [0.0 0.0; 40.0 0.0]; frames = [3.0, 4.0]
    near_start = Cecelia._points_by_frame([0.0 0.0; 0.0 0.0; 0.0 0.0], [0.0, 1.0, 2.0])
    @test Cecelia._event_preceded(ev, coords, frames, near_start, 3, 2.0) == [1.0, 1.0]

    # only the `default` run supersedes the unnamed legacy columns
    present = Cecelia.aggregate_run_cols("flow", "")
    @test !isempty(Cecelia.legacy_aggregate_cols(present, "flow", "default"))
    @test isempty(Cecelia.legacy_aggregate_cols(present, "flow", "single"))
end
