# RESULT-REVIEW: result-60da04b9
#
# `_uniform_interval` excused the first frame gap from the uniformity check with no bound, so a pause
# between the first two frames (gaps 600, 30, 30 s) recorded 30 s with no warning and every speed
# across that step came out ~20x too fast; a too-SHORT first gap was excused too. The excuse exists
# only for a scanner warm-up (+15% in the real case), so it is now bounded: up to `DELTA_T_WARMUP_TOL`
# longer, never shorter beyond the jitter tolerance.
@testset "RESULT-REVIEW result-60da04b9: a pause before the second frame is not a warm-up" begin
    @test Cecelia._uniform_interval([600.0, 30.0, 30.0]) === nothing
    @test Cecelia._uniform_interval([5.0, 30.0, 30.0, 30.0]) === nothing
    @test Cecelia._uniform_interval([34.69, 30.26, 30.27]) ≈ 30.265      # the real warm-up still passes
    @test Cecelia._uniform_interval([30.5, 30.0, 30.0]) ≈ 30.0           # jitter on the first gap passes
end
