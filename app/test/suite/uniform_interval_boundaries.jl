# `_uniform_interval` (omezarr/reader.jl) at its tolerances: a gap `DELTA_T_UNIFORM_TOL` (5%) off the
# median is still jitter, a first gap up to `DELTA_T_WARMUP_TOL` (25%) longer is still a warm-up, and
# shorter than −5% never is. Each limit is inclusive ("up to"), each side tested with a value the float
# arithmetic computes exactly. Why: docs/todo/TEST_VALUE_PLAN.md → P3.

@testset "uniform frame interval at its tolerances" begin
    ui = Cecelia._uniform_interval
    @test ui([100.0, 105.0]) == 102.5                          # 2 gaps: both judged against their median
    @test ui([100.0, 100.0, 105.0]) == 102.5                   # later gaps: 105 is 2.4% off the median
    @test ui([100.0, 95.0, 105.0, 100.0]) == 100.0             # ±5% exactly: jitter
    @test ui([100.0, 94.0, 106.0, 100.0]) === nothing          # ±6%: not
    @test ui([125.0, 100.0, 100.0]) == 100.0                   # a first gap 25% longer: warm-up
    @test ui([126.0, 100.0, 100.0]) === nothing
    @test ui([95.0, 100.0, 100.0]) == 100.0                    # 5% shorter: jitter
    @test ui([94.0, 100.0, 100.0]) === nothing                 # 6% shorter: never a warm-up
    @test ui([0.5]) == 0.5                                     # under a second is an interval
    @test ui([0.0, 0.0]) === nothing
end
