# RESULT-REVIEW: result-465bdff7
# Detect aggregates → Preceded by: the look-back runs from the MOVIE's first frame, not the selected
# population's. Debris first appears when the first intact cell dies (frame 10 here); frames 0–9 still
# exist, and the intact cell sat on that spot in all of them, so the event's share is 1.0 — not NaN
# ("nothing before it"), which is what measuring from the population's own first frame gave.

@testset "preceded looks back from the movie start, not the population's (result-465bdff7)" begin
    ev = [1, 1]; coords = [5.0 5.0; 5.0 5.0]; frames = [10.0, 11.0]       # one event, from frame 10
    ref = Cecelia._points_by_frame(fill(5.0, 10, 2), Float64.(0:9))        # an intact cell, frames 0–9
    pre = Cecelia._event_preceded(ev, coords, frames, ref, 12, 2.0)
    @test pre == [1.0, 1.0]
end
