# Source-scan anchor for THIS file — was `joinpath(@__DIR__, "..", "src")` when these testsets lived
# in `app/test/suite.jl`. This file is one directory deeper (`app/test/suite/`), so that literal path
# now resolves to `app/test/src`, which doesn't exist, and the scans read zero files. `pathof(Cecelia)`
# always points at `app/src/Cecelia.jl`, so backing up three dirs gives the repo root regardless of
# how many suite/ layers a future split adds.
const _SUITE_APP_SRC = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "app", "src")

# ── celltrackR cell-pair analysis + double tracking ──────────────────────────
#
# Port check against celltrackR 1.2.2's own definitions (`doc/QC.Rmd` §2.3, §3.1): `angle` is between
# the two tracks' DISPLACEMENT vectors, `dist` is the MINIMUM separation at a shared timepoint, and a
# pair that never coexists has NO distance (celltrackR: NA → NaN here).
@testset "analyze_cell_pairs (celltrackR)" begin
    # two parallel tracks 4 µm apart, moving the same way, fully overlapping in time
    para = DataFrame(
        label      = Float64[1, 2, 3, 4, 5, 6],
        centroid_t = Float64[0, 1, 2, 0, 1, 2],
        track_id   = Float64[1, 1, 1, 2, 2, 2],
        centroid_x = Float64[0, 1, 2, 0, 1, 2],
        centroid_y = Float64[0, 0, 0, 4, 4, 4])
    p = analyze_cell_pairs(para, ["centroid_x", "centroid_y"])
    @test nrow(p) == 1                                  # one PAIR, not two rows
    @test p.track1[1] == 1 && p.track2[1] == 2
    @test p.angle[1] ≈ 0.0 atol=1e-9                    # same direction
    @test p.distance[1] ≈ 4.0 atol=1e-9                 # min separation at a shared t
    @test p.n_shared[1] == 3

    # opposite directions → 180°
    opp = copy(para)
    opp[4:6, :centroid_x] = Float64[2, 1, 0]
    @test analyze_cell_pairs(opp, ["centroid_x", "centroid_y"]).angle[1] ≈ 180.0 atol=1e-6

    # perpendicular → 90°, which is the reference line the drift read-off uses
    perp = copy(para)
    perp[4:6, :centroid_x] = Float64[0, 0, 0]
    perp[4:6, :centroid_y] = Float64[4, 5, 6]
    @test analyze_cell_pairs(perp, ["centroid_x", "centroid_y"]).angle[1] ≈ 90.0 atol=1e-6

    # NO overlap in time → distance undefined (NaN), exactly as celltrackR returns NA
    seq = copy(para)
    seq[4:6, :centroid_t] = Float64[10, 11, 12]
    r = analyze_cell_pairs(seq, ["centroid_x", "centroid_y"])
    @test isnan(r.distance[1])
    @test r.n_shared[1] == 0
    @test !isnan(r.angle[1])                            # an angle still exists

    # a track that never moved has no direction → NaN angle, not a spurious 0°
    still = copy(para)
    still[4:6, :centroid_x] = Float64[9, 9, 9]
    still[4:6, :centroid_y] = Float64[9, 9, 9]
    @test isnan(analyze_cell_pairs(still, ["centroid_x", "centroid_y"]).angle[1])

    # n pairs = n choose 2
    three = DataFrame(label = Float64.(1:6), centroid_t = Float64[0, 1, 0, 1, 0, 1],
                      track_id = Float64[1, 1, 2, 2, 3, 3],
                      centroid_x = Float64[0, 1, 5, 6, 9, 10],
                      centroid_y = zeros(6))
    @test nrow(analyze_cell_pairs(three, ["centroid_x", "centroid_y"])) == 3
end

@testset "find_duplicate_tracks (celltrackR QC §3.1)" begin
    # one cell segmented twice: near-identical paths, ~0.5 µm apart
    dup = DataFrame(
        label      = Float64.(1:8),
        centroid_t = Float64[0, 1, 2, 3, 0, 1, 2, 3],
        track_id   = Float64[1, 1, 1, 1, 2, 2, 2, 2],
        centroid_x = Float64[0, 2, 4, 6, 0.3, 2.3, 4.3, 6.3],
        centroid_y = Float64[0, 0, 0, 0, 0.4, 0.4, 0.4, 0.4])
    iss = find_duplicate_tracks(dup, ["centroid_x", "centroid_y"])
    @test length(iss) == 1
    d = only(iss)
    @test d.kind == "duplicate"
    @test d.op == Dict{String,Any}("op" => "track.remove", "trackIds" => [2])  # drops the HIGHER id
    @test d.track_ids == [1, 2]                         # but names both, so the user can see the pair
    @test occursin("one cell tracked twice", d.advice)
    @test occursin("remove track 2", d.advice)

    # far apart → not a duplicate, however parallel
    far = copy(dup)
    far[5:8, :centroid_y] .= 80.0
    @test isempty(find_duplicate_tracks(far, ["centroid_x", "centroid_y"]))

    # close but heading differently → not a duplicate
    cross = copy(dup)
    cross[5:8, :centroid_x] = Float64[6.3, 4.3, 2.3, 0.3]
    @test isempty(find_duplicate_tracks(cross, ["centroid_x", "centroid_y"]))

    # too few shared frames to claim they are the same cell
    @test isempty(find_duplicate_tracks(dup, ["centroid_x", "centroid_y"]; min_shared = 99))

    # the suggested op is submittable and actually removes the duplicate
    ops = Cecelia.parse_track_ops([d.op])
    Cecelia.apply_track_ops!(dup, ops)
    @test Cecelia.track_ids_present(dup) == [1]
end

@testset "track_pair_drift (celltrackR QC §2.3)" begin
    # No global directionality: far-apart pairs average ~90°. Two perpendicular tracks, far apart —
    # ONE pair, so the mean is exactly 90 by construction rather than by luck.
    nodrift = DataFrame(label = Float64.(1:4), centroid_t = Float64[0, 1, 0, 1],
                        track_id = Float64[1, 1, 2, 2],
                        centroid_x = Float64[0, 1, 100, 100],
                        centroid_y = Float64[0, 0, 0,   1])
    pairs = analyze_cell_pairs(nodrift, ["centroid_x", "centroid_y"])
    v = track_pair_drift(pairs; far_quantile = 0.0)      # judge ALL pairs in this tiny fixture
    @test v.n_far == 1
    @test v.mean_angle_far ≈ 90.0 atol=1e-6
    @test v.drifting == false

    # Everything marching the same way, even cells far apart → drift, not migration
    drift = DataFrame(label = Float64.(1:6), centroid_t = Float64[0, 1, 0, 1, 0, 1],
                      track_id = Float64[1, 1, 2, 2, 3, 3],
                      centroid_x = Float64[0, 1, 50, 51, 100, 101],
                      centroid_y = zeros(6))
    dv = track_pair_drift(analyze_cell_pairs(drift, ["centroid_x", "centroid_y"]); far_quantile = 0.0)
    @test dv.mean_angle_far ≈ 0.0 atol=1e-6
    @test dv.drifting == true

    # nothing measurable → no verdict, no crash, and NOT a false "drifting"
    empty_v = track_pair_drift(DataFrame(angle = Float64[], distance = Float64[]))
    @test empty_v.n_far == 0 && empty_v.drifting == false
    @test isnan(empty_v.mean_angle_far)
end

@testset "track_path_dicts — the wire shape both routes send" begin
    # two straight tracks; rows are deliberately NOT in time order, so ordering is a real assertion
    df = DataFrame(label = [3.0, 1.0, 2.0, 4.0, 5.0],
                   centroid_t = [2.0, 0.0, 1.0, 0.0, 1.0],
                   track_id = [1.0, 1.0, 1.0, 2.0, 2.0],
                   centroid_x = [20.0, 0.0, 10.0, 5.0, 6.0],
                   centroid_y = [0.0, 0.0, 0.0, 1.0, 1.0])

    all_paths = track_path_dicts(df, ["centroid_x", "centroid_y"])
    @test Set(keys(all_paths)) == Set(["1", "2"])
    p1 = all_paths["1"]
    @test p1["t"] == [0.0, 1.0, 2.0]            # sorted by time, not by row order
    @test p1["x"] == [0.0, 10.0, 20.0]          # coords follow their own timepoint
    @test p1["label"] == [1, 2, 3]              # labels travel with the points

    # `ids` restricts, and asking for a track that isn't there is silently empty, not an error —
    # the worklist can reference a track the geometry cap left out
    @test collect(keys(track_path_dicts(df, ["centroid_x", "centroid_y"]; ids = [2]))) == ["2"]
    @test isempty(track_path_dicts(df, ["centroid_x", "centroid_y"]; ids = [999]))

    # Untracked cells are never a path of their own — and a 0 id counts as untracked on read, so
    # one polyline can't stitch every untracked cell in the image into a fake track.
    untracked = DataFrame(label = [1.0, 2.0], centroid_t = [0.0, 1.0], track_id = [NaN, 0.0],
                          centroid_x = [1.0, 2.0], centroid_y = [0.0, 0.0])
    @test isempty(track_path_dicts(untracked, ["centroid_x", "centroid_y"]))
    @test isempty(track_ids_present(untracked))

    # 1-D segmentation: y comes back EMPTY rather than zeros invented here (the frontend decides)
    oned = track_path_dicts(df, ["centroid_x"])
    @test oned["1"]["y"] == Float64[]

    # the shape the detector's own geometry uses is the same shape — one helper, two routes
    iss = _issue_df([(1, 0, 3, 0.0, 1.0), (2, 3, 3, 3.0, 1.0)])
    geo = track_path_dicts(iss, ["centroid_x", "centroid_y"]; ids = [1, 2])
    @test sort(collect(keys(geo))) == ["1", "2"]
    @test all(k -> issubset(["t", "x", "y", "label"], collect(keys(geo[k]))), keys(geo))

    # OCCUPANCY MODE: timepoints only, for the track timeline — it draws lanes over frames and reads
    # nothing but `t`, and unlike a path plot it must show EVERY track rather than a capped top-N.
    # The keys stay put and the dropped arrays come back EMPTY, exactly as `y` already does for a 1-D
    # segmentation — so one frontend reader handles both modes and no caller has to know which
    # produced the response. A mode that omitted the keys would make that a type change.
    occ = track_path_dicts(df, ["centroid_x", "centroid_y"]; occupancy = true)
    @test Set(keys(occ)) == Set(["1", "2"])
    @test issubset(["t", "x", "y", "label"], collect(keys(occ["1"])))
    @test occ["1"]["t"] == [0.0, 1.0, 2.0]          # the timepoints are unchanged…
    @test occ["1"]["x"] == Float64[]                # …and everything else is empty, not absent
    @test occ["1"]["y"] == Float64[]
    @test occ["1"]["label"] == Int[]
    # occupancy composes with `ids` — the fly-to-napari button asks for one track WITH coordinates
    @test collect(keys(track_path_dicts(df, ["centroid_x", "centroid_y"];
                                        ids = [2], occupancy = true))) == ["2"]
end

# ── celltrackR diagnostics battery (app/src/tracking/track_diagnostics.jl) ────
#
# Two kinds of check, deliberately: ANALYTIC cases below (a straight track must be exactly ballistic,
# a zig-zag exactly anti-correlated) which stay readable without R, and GOLDEN values further down
# generated by celltrackR itself, which pin the conventions the analytic cases cannot.
#
# `_diag_df` builds tracks from explicit per-frame coordinates.
function _diag_df(tracks::Vector{<:Pair})            # track_id => Vector of (t, x, y[, z])
    lab, t, tid, x, y, z = Float64[], Float64[], Float64[], Float64[], Float64[], Float64[]
    n = 0
    for (id, pts) in tracks, p in pts
        n += 1
        push!(lab, n); push!(t, p[1]); push!(tid, id)
        push!(x, p[2]); push!(y, p[3]); push!(z, length(p) > 3 ? p[4] : 0.0)
    end
    DataFrame(label = lab, centroid_t = t, track_id = tid,
              centroid_x = x, centroid_y = y, centroid_z = z)
end
const _DIAG_XY  = ["centroid_x", "centroid_y"]
const _DIAG_XYZ = ["centroid_x", "centroid_y", "centroid_z"]

@testset "track_msd — a straight track is ballistic, exactly" begin
    # 1 µm per frame along x: displacement over lag L is L µm, so MSD(L) = L² and the log-log slope
    # is exactly 2
    df = _diag_df([1 => [(t, Float64(t), 0.0) for t in 0:10]])
    m = track_msd(df, _DIAG_XY; max_lag = 5)
    @test m.lag == [1.0, 2.0, 3.0, 4.0, 5.0]
    @test m.msd ≈ [1.0, 4.0, 9.0, 16.0, 25.0]
    @test m.n == [10, 9, 8, 7, 6]                     # overlapping subtracks, celltrackR-style
    @test all(isapprox.(m.sem, 0.0; atol = 1e-9))     # every sample identical → no spread
    @test msd_log_slope(m.lag, m.msd) ≈ MSD_SLOPE_DIRECTED
    @test msd_motion_kind(msd_log_slope(m.lag, m.msd)) == "directed"
end

@testset "track_msd — a time gap is not a lag" begin
    # frames 0,1,2 then 6,7. Indexing by POSITION would call 2→6 a lag-1 step and fold a 4-frame,
    # 4 µm displacement into the lag-1 mean. Each real step is 1 µm, so lag-1 MSD must be exactly 1.
    df = _diag_df([1 => [(0.0, 0.0, 0.0), (1.0, 1.0, 0.0), (2.0, 2.0, 0.0),
                         (6.0, 6.0, 0.0), (7.0, 7.0, 0.0)]])
    m = track_msd(df, _DIAG_XY; max_lag = 3)
    @test m.msd[1] ≈ 1.0
    @test m.n[1] == 3                                  # 0→1, 1→2, 6→7 — NOT 2→6
    @test m.n[2] == 1 && m.msd[2] ≈ 4.0                # lag 2 is 0→2 alone
end

@testset "track_msd — stationary and untracked" begin
    still = _diag_df([1 => [(t, 5.0, 5.0) for t in 0:5]])
    m = track_msd(still, _DIAG_XY; max_lag = 3)
    @test all(m.msd .== 0.0)
    @test isnan(msd_log_slope(m.lag, m.msd))           # log(0) is not a number to invent
    @test msd_motion_kind(NaN) == "unknown"

    none = DataFrame(label = [1.0, 2.0], centroid_t = [0.0, 1.0], track_id = [NaN, NaN],
                     centroid_x = [1.0, 2.0], centroid_y = [0.0, 0.0])
    @test isempty(track_msd(none, _DIAG_XY).lag)
end

@testset "track_autocorrelation — straight, reversing, turning" begin
    straight = _diag_df([1 => [(t, Float64(t), 0.0) for t in 0:8]])
    a = track_autocorrelation(straight, _DIAG_XY; max_lag = 4)
    @test a.lag == [0.0, 1.0, 2.0, 3.0, 4.0]
    @test all(isapprox.(a.acor, 1.0; atol = 1e-12))
    @test isnan(persistence_lag(a.lag, a.acor))        # never decays → report the window, not a number

    # steps alternating +x/−x: cosine −1 at odd lags, +1 at even. A whole image doing this is the
    # jitter signature the curve exists to expose.
    zig = _diag_df([1 => [(0.0, 0.0, 0.0), (1.0, 1.0, 0.0), (2.0, 0.0, 0.0),
                          (3.0, 1.0, 0.0), (4.0, 0.0, 0.0), (5.0, 1.0, 0.0)]])
    z = track_autocorrelation(zig, _DIAG_XY; max_lag = 2)
    @test z.acor[1] ≈ 1.0 && z.acor[2] ≈ -1.0 && z.acor[3] ≈ 1.0

    corner = _diag_df([1 => [(0.0, 0.0, 0.0), (1.0, 1.0, 0.0), (2.0, 1.0, 1.0)]])
    @test isapprox(track_autocorrelation(corner, _DIAG_XY; max_lag = 1).acor[2], 0.0; atol = 1e-12)
end

@testset "persistence_lag — interpolates to 1/e" begin
    @test persistence_lag([0.0, 1.0, 2.0],
                          [1.0, ACOR_PERSIST_LEVEL + 0.1, ACOR_PERSIST_LEVEL - 0.1]) ≈ 1.5
    @test persistence_lag([0.0, 1.0, 2.0], [1.0, ACOR_PERSIST_LEVEL, 0.0]) ≈ 1.0
    @test isnan(persistence_lag([0.0], [1.0]))
    @test_throws ArgumentError persistence_lag([0.0, 1.0], [1.0])
end

@testset "plane_angle_profile — the geometry, and 2D has no plane" begin
    # equal xy and z components → exactly 45° to the plane; distance is read at the step's START
    df = _diag_df([1 => [(0.0, 0.0, 0.0, 10.0), (1.0, 1.0, 0.0, 11.0)]])
    prof = plane_angle_profile(df, _DIAG_XYZ; plane_z = 0.0)
    @test nrow(prof) == 1 && prof.angle[1] ≈ 45.0 && prof.distance[1] ≈ 10.0

    flat = _diag_df([1 => [(0.0, 0.0, 0.0, 3.0), (1.0, 1.0, 1.0, 3.0)]])
    up   = _diag_df([1 => [(0.0, 0.0, 0.0, 3.0), (1.0, 0.0, 0.0, 4.0)]])
    @test plane_angle_profile(flat, _DIAG_XYZ; plane_z = 0.0).angle[1] ≈ 0.0
    @test plane_angle_profile(up,   _DIAG_XYZ; plane_z = 0.0).angle[1] ≈ 90.0

    # 2D data has no boundary plane — an empty frame, not an error (the panel hides the mode)
    @test nrow(plane_angle_profile(df, _DIAG_XY)) == 0
    @test PLANE_ANGLE_UNBIASED == 32.7                 # Beltman 2009, not a rounding
end

@testset "plane_artefact — only a NEAR deficit is suspect" begin
    near = DataFrame(distance = fill(1.0, 20), angle = fill(5.0, 20))
    far  = DataFrame(distance = fill(50.0, 20), angle = fill(35.0, 20))
    v = plane_artefact(vcat(near, far))
    @test v.suspect && v.mean_angle_near ≈ 5.0 && v.mean_angle_far ≈ 35.0

    # the SAME deficit everywhere is a directional bias, not a boundary artefact
    @test !plane_artefact(DataFrame(distance = [fill(1.0, 20); fill(50.0, 20)],
                                    angle = fill(5.0, 40))).suspect
    @test !plane_artefact(vcat(far, far)).suspect
    e = plane_artefact(DataFrame(distance = Float64[], angle = Float64[]))
    @test !e.suspect && isnan(e.mean_angle_near) && e.n_near == 0
end

@testset "drift_test — too few decorrelated samples is 'not assessed'" begin
    tiny = _diag_df([1.0 => [(0.0, 0.0, 0.0), (1.0, 1.0, 0.0)]])
    t = drift_test(tiny, _DIAG_XY; step_spacing = 10)
    @test isnan(t.p) && !t.drifting && t.n <= 1        # never a verdict from nothing
end

# A deterministic random walk: unit steps in directions from splitmix64, so the sequence is fixed
# forever and does NOT depend on a Julia release's RNG stream. `drift` adds a constant velocity;
# `x0` spreads the tracks apart.
#
# Two earlier attempts at this fixture were wrong in instructive ways, both caught by the diagnostics
# themselves:
#   • GOLDEN-ANGLE directions. Equidistributed is not decorrelated — rotating by a fixed 137.5° each
#     step traces a rosette that closes on itself, and the MSD slope came out at -0.53. The fixture
#     read as CONFINED.
#   • A plain LCG seeded linearly per track. LCG streams from nearby seeds are correlated, so all ten
#     "independent" walks drifted together and Hotelling's T² called it drift at p = 0.002 — a true
#     detection of a fake dataset. splitmix64 mixes the seed, so the streams are independent.
function _diag_walk(id::Int, n::Int; drift = (0.0, 0.0), x0 = 0.0)
    st = Ref(UInt64(id) * 0x9E3779B97F4A7C15 + 0x00DEADBEEF)
    function nextu()
        st[] += 0x9E3779B97F4A7C15
        z = st[]
        z = (z ⊻ (z >> 30)) * 0xBF58476D1CE4E5B9
        z = (z ⊻ (z >> 27)) * 0x94D049BB133111EB
        z = z ⊻ (z >> 31)
        Float64(z >> 11) / Float64(2)^53
    end
    pts = Tuple{Float64,Float64,Float64}[]
    x, y = x0, 0.0
    for k in 0:(n - 1)
        push!(pts, (Float64(k), x, y))
        θ = 2π * nextu()
        x += cos(θ) + drift[1]
        y += sin(θ) + drift[2]
    end
    Float64(id) => pts
end

@testset "drift_test — a NOISELESS drift cannot be tested, and says so" begin
    # every track marching +x by exactly 1 with no variation: the sampled steps are identical, so the
    # covariance is singular and T² does not exist. "Not assessed" is the only honest answer — a
    # verdict from a matrix that cannot be inverted would be invented. Real data always has spread.
    perfect = _diag_df([Float64(id) => [(Float64(t), Float64(t) + id, Float64(id)) for t in 0:24]
                        for id in 1:8])
    d = drift_test(perfect, _DIAG_XY; step_spacing = 10)
    @test d.n > 3                                       # there WERE samples…
    @test isnan(d.p) && !d.drifting                     # …and still no p-value
    @test d.mean_step[1] ≈ 1.0                           # the mean step is reported regardless
end

@testset "track_diagnostics — one roll-up, and it reaches the QC findings" begin
    # a real-looking drift: a quasi-random walk plus a constant +1.5 µm/frame in x, per track a
    # different phase so the steps genuinely vary
    drifting = _diag_df([_diag_walk(id, 30; drift = (1.5, 0.0), x0 = 40.0 * id) for id in 1:6])
    d = track_diagnostics(drifting, _DIAG_XY; max_lag = 6)
    @test d.summary.nTracks == 6
    @test d.summary.driftP < DRIFT_ALPHA                 # the drift IS detected
    @test d.summary.driftMeanStep[1] > 1.0
    @test d.summary.motionKind == "directed"             # a drifting field is ballistic
    @test !isempty(d.msd.lag) && !isempty(d.acor.lag)

    f = track_diagnostic_findings(d)
    @test "tracking.field_drift" in [x["code"] for x in f]
    @test all(x -> x["level"] == "warn", f)              # advisory only, never an error
    # text comes from the catalog, and the INPUTS are persisted for read-time re-rendering
    @test all(x -> haskey(x, "key") && haskey(Cecelia.QC_TEXT, x["key"]), f)
    drift = f[findfirst(x -> x["code"] == "tracking.field_drift", f)]
    @test occursin("drifting", lowercase(drift["short"]))
    @test haskey(drift, "detail") && occursin("mean step", drift["detail"])

    # …and the same walk WITHOUT the drift is quiet: no drift finding, and a slope that reads as a
    # random walk rather than "barely displaces". The battery must not cry wolf on ordinary motion.
    calm = _diag_df([_diag_walk(id, 40; x0 = 40.0 * id) for id in 1:10])
    c = track_diagnostics(calm, _DIAG_XY; max_lag = 6)
    @test c.summary.motionKind == "random walk"
    @test isempty(track_diagnostic_findings(c))
end

# ── celltrackR GOLDEN VALUES for the diagnostics battery ──────────────────────
#
# Produced by running celltrackR 1.2.2 ITSELF (from the old R version's renv) over the fixture below
# and printing to 10 decimals — the same discipline as the logicle transform's FlowUtils goldens, and
# worth more than hand-derived numbers because it pins the CONVENTIONS as well as the arithmetic:
# which lag a subtrack length means, what step.spacing strides by, which point a distance is measured
# from. Every one of those was a guess until this ran.
#
# The generator is committed at `app/test/golden/celltrackr_golden.R` — NOT run by the suite or CI
# (neither has R with celltrackR), but present so these numbers stay re-derivable. A golden value whose
# generator is lost is just a magic constant.
const _CTR_TRACKS = [   # (track_id, t, x, y, z) — 3 tracks x 8 frames, R seed 42
    (1, 0, 0, 0, 10),
    (1, 1, 2.4, -0.1, 9.9),
    (1, 2, 2.8, 1.9, 10.3),
    (1, 3, 4.2, 1.9, 10.1),
    (1, 4, 5.8, 3.2, 8.8),
    (1, 5, 7.2, 5.5, 7.6),
    (1, 6, 8.1, 4.1, 8.2),
    (1, 7, 10.6, 3.8, 8.1),
    (2, 0, 0, 0, 10),
    (2, 1, -0.8, 0.5, 9.1),
    (2, 2, 0, -0.2, 8.7),
    (2, 3, 2.3, 0.3, 8.3),
    (2, 4, 5.2, 1, 7.1),
    (2, 5, 5.7, 2, 7.1),
    (2, 6, 6.5, 1.4, 7.2),
    (2, 7, 5.7, 1.9, 7.1),
    (3, 0, 0, 0, 10),
    (3, 1, 1.8, 0.7, 10.3),
    (3, 2, 2, 1, 10.4),
    (3, 3, 1.7, 0.2, 8.9),
    (3, 4, 3.1, 1.8, 9),
    (3, 5, 3.3, 2.4, 8.8),
    (3, 6, 5.7, 2.5, 8.9),
    (3, 7, 6.3, 2.8, 9.2)]

_ctr_df() = DataFrame(label = Float64.(1:length(_CTR_TRACKS)),
                      track_id   = Float64[r[1] for r in _CTR_TRACKS],
                      centroid_t = Float64[r[2] for r in _CTR_TRACKS],
                      centroid_x = Float64[r[3] for r in _CTR_TRACKS],
                      centroid_y = Float64[r[4] for r in _CTR_TRACKS],
                      centroid_z = Float64[r[5] for r in _CTR_TRACKS])
const _CTR_XY  = ["centroid_x", "centroid_y"]
const _CTR_XYZ = ["centroid_x", "centroid_y", "centroid_z"]

@testset "diagnostics golden — MSD matches celltrackR squareDisplacement" begin
    m = track_msd(_ctr_df(), _CTR_XYZ; max_lag = 5)
    @test m.lag == [1.0, 2.0, 3.0, 4.0, 5.0]
    @test m.msd ≈ [3.6490476190, 9.8405555556, 19.2746666667, 33.8275000000, 45.9888888889] atol = 1e-9
    # celltrackR's subtrack.length L IS lag L here: both count frames, both average over every
    # overlapping subtrack of that length (21 = 3 tracks x 7 steps)
    @test m.n == [21, 18, 15, 12, 9]
end

@testset "diagnostics golden — autocorrelation matches celltrackR overallNormDot" begin
    a = track_autocorrelation(_ctr_df(), _CTR_XYZ; max_lag = 4)
    # THE CONVENTION, pinned: celltrackR dots the FIRST and LAST step of an L-step subtrack, so its
    # L maps to lag L-1 here — its L=1 is the trivial 1.0, which is our lag 0.
    @test a.lag == [0.0, 1.0, 2.0, 3.0, 4.0]
    @test a.acor ≈ [1.0000000000, 0.2384705125, 0.3443271054, 0.3646898028, 0.1748312958] atol = 1e-9
end

@testset "diagnostics golden — plane angle/distance match celltrackR angleToPlane" begin
    prof = plane_angle_profile(_ctr_df(), _CTR_XYZ)
    @test nrow(prof) == 21
    @test minimum(_ctr_df().centroid_z) ≈ 7.1000000000        # R's boundingBox(X)["min","z"]
    # compared as a SET of (distance, angle): what matters is the pairing, not which track's steps
    # celltrackR happens to emit first
    got  = sort([(round(d; digits = 6), round(a; digits = 6)) for (d, a) in zip(prof.distance, prof.angle)])
    @test got == sort([(2.900000, 2.383878), (2.800000, 11.095803), (3.200000, 8.130102), (3.000000, 32.235229), (1.700000, 24.021042), (0.500000, 19.824490), (1.100000, 2.274311), (2.900000, 43.651366), (2.000000, 20.620734), (1.600000, 9.644911), (1.200000, 21.912009), (0.000000, 0.000000), (0.000000, 5.710593), (0.100000, 6.050746), (2.900000, 8.829426), (3.200000, 15.501360), (3.300000, 60.334150), (1.800000, 2.692982), (1.900000, 17.548401), (1.700000, 2.383878), (1.800000, 24.094843)])
end

@testset "diagnostics golden — drift p matches celltrackR hotellingsTest" begin
    df = _ctr_df()
    # spacing 0 = every step, as in R; xy only, as R's dim = c("x","y") default
    every = drift_test(df, _CTR_XY; step_spacing = 0)
    @test every.n == 21
    @test every.p ≈ 0.0005222153718 rtol = 1e-8

    # …and the entire reason the parameter exists: the SAME data, decorrelated, is not significant.
    # step.spacing = 3 strides by 4 frames → 6 of the 21 steps.
    spaced = drift_test(df, _CTR_XY; step_spacing = 3)
    @test spaced.n == 6
    @test spaced.p ≈ 0.1055968743 rtol = 1e-8
    @test every.p < DRIFT_ALPHA && spaced.p > DRIFT_ALPHA
end

@testset "diagnostics golden — pair angles match celltrackR analyzeCellPairs" begin
    prs = analyze_cell_pairs(_ctr_df(), _CTR_XYZ)
    got = Dict((r.track1, r.track2) => round(r.angle; digits = 8) for r in eachrow(prs))
    @test got == Dict((1, 2) => 16.23356100, (1, 3) => 5.13519680, (2, 3) => 19.85983511)
    # every track starts at the origin here, so the min shared-time distance is 0 — which is also
    # what celltrackR's distanceCells reports (it, too, is a min over shared timepoints)
    @test all(r -> r.distance ≈ 0.0, eachrow(prs))
end

@testset "track_diagnostics — the O(n²) pair scan is guarded, and says when it skipped" begin
    # 12 tracks with the cap set to 5: the curves must still be computed (they are cheap and linear),
    # the pair half must be skipped, and `nDuplicatePairs == 0` must be accompanied by `pairsSkipped`
    # so nobody reads it as "no duplicates on this image".
    many = _diag_df([_diag_walk(id, 12; x0 = 40.0 * id) for id in 1:12])
    d = track_diagnostics(many, _DIAG_XY; max_lag = 4, max_pair_tracks = 5)
    @test d.summary.pairsSkipped
    @test d.summary.nDuplicatePairs == 0
    @test isnan(d.summary.pairAngleFar)                  # not "90°", not 0 — not measured
    @test !isempty(d.msd.lag) && !isempty(d.acor.lag)    # the cheap half always runs
    @test length(d.pairs.angle) == 0

    # under the cap, the same data DOES get the pair scan
    ok = track_diagnostics(many, _DIAG_XY; max_lag = 4, max_pair_tracks = 100)
    @test !ok.summary.pairsSkipped
    @test length(ok.pairs.angle) == 12 * 11 ÷ 2
    # the shipped default is the measured one (see PAIR_SCAN_MAX_TRACKS): 2000 tracks = 25 s of scan
    @test PAIR_SCAN_MAX_TRACKS == 800
end

@testset "flow_model_filename" begin
    # The two spellings of one model: the STEM `opticalFlow.train`'s `modelName` holds, and the vault
    # FILENAME a consumer's `model` select carries. They meet whenever one chain node trains a model
    # and a later node segments with it; appending `.pt` at each such site is how they drift.
    @test flow_model_filename("flow.cytoFg") == "flow.cytoFg.pt"
    @test flow_model_filename("flow.cyto")   == "flow.cyto.pt"
    # Idempotent, so it is safe on a value that is already a filename (a re-validated chain dict).
    @test flow_model_filename("flow.cytoFg.pt") == "flow.cytoFg.pt"
    # Dots in the stem are normal here — real names use them — and must not be treated as extensions.
    @test flow_model_filename("a.b.c") == "a.b.c.pt"
    # Round-trips against the vault's own stem list rule (`flow_model_names` strips exactly this).
    @test first(splitext(flow_model_filename("flow.cytoFg"))) == "flow.cytoFg"
end

@testset "a chain may name a model an upstream node trains" begin
    # A chain that TRAINS a model and then segments with it names something the vault does not hold at
    # author time. The `model` select's options are enumerated from the vault, so per-node validation
    # read the forward reference as a typo and rejected the template — and the whiteboard's dropdown
    # had the same blind spot, so the wiring could not be expressed at all. Only the template can tell
    # a forward reference from a mistake, hence `_chain_produced_names` feeding `extra_options`.
    train(name) = ChainNode(; id = "train", fn = "opticalFlow.train",
                            params = Dict{String,Any}("modelName" => name,
                                                      "valueName" => "smoothed"))
    seg(model)  = ChainNode(; id = "seg", fn = "segment.coastal",
                            params = Dict{String,Any}(
                                "valueName" => "smoothed",
                                "models" => Dict{String,Any}(
                                    "0" => Dict{String,Any}("model" => model))))
    tpl(nodes, edges) = ChainTemplate("t", nodes, edges, String[])

    # The point of the whole change: train → seg, seg naming the model train will write.
    @test validate_chain_template(
        tpl([train("flow.cytoFg"), seg("flow.cytoFg.pt")],
            [ChainEdge("train", "seg")])) === nothing

    # The producer holds a STEM; the consumer holds a FILENAME. Accepting only one spelling would make
    # this depend on which side the user typed it on.
    @test "flow.cytoFg.pt" in
          Cecelia._chain_produced_names(tpl([train("flow.cytoFg"), seg("")],
                                            [ChainEdge("train", "seg")]), "seg")

    # ANCESTORS ONLY. Reversed, the segment node runs BEFORE the model exists — a real wiring mistake,
    # and letting it through here would only defer it to a mid-run failure on an occupied GPU.
    @test_throws ChainTemplateError validate_chain_template(
        tpl([train("flow.cytoFg"), seg("flow.cytoFg.pt")],
            [ChainEdge("seg", "train")]))

    # Unconnected is the same case: nothing guarantees the model is there when seg runs.
    @test_throws ChainTemplateError validate_chain_template(
        tpl([train("flow.cytoFg"), seg("flow.cytoFg.pt")], ChainEdge[]))

    # A genuine typo downstream of a producer must STILL fail — the allowance is exactly the set of
    # names an ancestor writes, not "any string once a trainer is present".
    @test_throws ChainTemplateError validate_chain_template(
        tpl([train("flow.cytoFg"), seg("flow.cytoFgg.pt")],
            [ChainEdge("train", "seg")]))

    # A root node has no ancestors, so it produces nothing for itself.
    @test isempty(Cecelia._chain_produced_names(
        tpl([train("flow.cytoFg")], ChainEdge[]), "train"))
end

@testset "chain nodes run through execute_task" begin
    # `execute_task` is the canonical single-task pathway — the same one `handle_task_run` and the
    # runner's own task handler use. It resolves the task, dispatches on scope, wires
    # log/progress/status/result, and guarantees a terminal status on every exit path.
    #
    # The chain used to re-assemble that by hand, and FIVE bugs came out of the gap: a set-scope node
    # called the inner `_run_task` directly, so it wrote no `<img>/logs/<fun_name>.log`
    # (`_wrap_log_with_file`), registered no `TaskRecord` (nothing for the console or task-log view to
    # attach to — output fell through to the server's stdout), opened no run-log entry, registered no
    # `on_process` for cancel, and took no pool slot, so a node declaring `resource_pool: "gpu"` ran
    # UNQUEUED. Neither node runner wired `on_progress` either, so no chain node ever showed progress.
    #
    # The durable fix is that there is no longer a second implementation to drift from. This pins it.
    chain_dir = joinpath(_SUITE_APP_SRC, "tasks", "chain")
    src = read(joinpath(_SUITE_APP_SRC, "tasks", "chain.jl"), String) * "\n" *
          join([read(f, String) for f in
                filter(f -> endswith(f, ".jl"), readdir(chain_dir; join=true))], "\n")

    for (fname, label) in (("_execute_image_chain!", "image-scope"),
                           ("_run_set_scope_node!",  "set-scope"))
        at = findfirst(fname * "(run::ChainRun", src)
        @test at !== nothing
        body = src[first(at):end]
        nxt  = findfirst("\nfunction ", body[10:end])
        body = nxt === nothing ? body : body[1:first(nxt) + 8]

        @test occursin("execute_task(", body) ||
              error("$label chain nodes must dispatch through `execute_task` — see " *
                    "docs/SCHEDULER.md → *Chain nodes run through `execute_task`*.")
        @test !occursin(r"(?<!_)\brun_task\(", body) ||
              error("$label is calling `run_task` directly again. Everything a node needs beyond a " *
                    "standalone task is a field on `TaskRequest`, not a second copy of the wiring.")
        @test !occursin(r"\b_run_task\(", body) ||
              error("$label is calling `_run_task` directly. That bypasses the log file, the task " *
                    "record, the run log and the resource pool.")
        # The pool the node declares must actually reach the request; "" silently means `cpu`.
        @test occursin("pool_name    = node.resource_pool", body)
        # …and the correlation pair, or the GUI cannot match the task to its node row.
        @test occursin("chain_run_id = run.id", body)
        @test occursin("chain_node_id = node.id", body)
    end

    # `TaskRequest` is where a chain node's extra needs live — the shared component grew, the chain did
    # not fork. If these move back out, the fork is back.
    @test :chain_run_id  in fieldnames(Cecelia.TaskRequest)
    @test :chain_node_id in fieldnames(Cecelia.TaskRequest)
    # They cross the process boundary too, or a runner-executed node loses its node identity.
    rt = Cecelia.task_request(Cecelia.task_request_dict(
        Cecelia.TaskRequest(; task_id="t", fun_name="f", project_uid="p",
                              chain_run_id="R", chain_node_id="N")))
    @test rt.chain_run_id  == "R"
    @test rt.chain_node_id == "N"
end

@testset "a chain node's log prefix is [imageUid/nodeId]" begin
    # The prefix on a `chain:log` line is a WIRE FORMAT, not decoration. A `chain:log` frame carries no
    # taskId, so `frontend/src/stores/ws.ts` parses `[imageUid/nodeId]` off the line and attributes it
    # to the task row with that (imageUid, chainNodeId). The set-scope path emitted `[set/<node>]` — the
    # literal string "set" where a uid belongs — so the lookup matched nothing and a set-scope node's
    # Tasks-page log read "no output yet" while the same line reached the console and the log file
    # perfectly well. It was invisible until the node HAD a task row to attribute to.
    chain_dir = joinpath(_SUITE_APP_SRC, "tasks", "chain")
    src = read(joinpath(_SUITE_APP_SRC, "tasks", "chain.jl"), String) * "\n" *
          join([read(f, String) for f in
                filter(f -> endswith(f, ".jl"), readdir(chain_dir; join=true))], "\n")

    # The regex the frontend uses, transcribed: prefix, slash, node id, space.
    fe = r"^\[([^/\]]+)/([^\]]+)\] (.*)$"
    @test match(fe, "[fXgbTl/train] Epoch 1/30").captures[1] == "fXgbTl"

    # No `[set/...]` literal may come back — it parses as a uid of "set" and silently matches no task.
    @test !occursin("[set/\$(node.id)]", src) ||
          error("a set-scope node is emitting `[set/<node>]`; the frontend reads that first field as " *
                "an imageUid. Use the representative image's uid — see docs/SCHEDULER.md.")

    # Both the log and the error line interpolate a real uid.
    @test occursin("[\$(first(imgs).uid)/\$(node.id)]", src)
    # log line, error line, and the set-wide axis SKIP. The per-image SKIP uses the loop's own `uid`,
    # which is the same rule the image-scope path follows.
    @test length(collect(eachmatch(r"\[\$\(first\(imgs\)\.uid\)/\$\(node\.id\)\]", src))) == 3
    @test occursin("SKIP [\$uid/\$(node.id)]", src)
end

@testset "a chain node reports progress" begin
    # No chain node ever reported progress, of any scope. The Python side emits `[PROGRESS] n/total`
    # and `run_py` routes it to `on_progress`, but the LAST hop was missing: the standalone path wires
    # `on_progress` when it calls `run_task` (`execute_task`, and the runner's own task handler), and
    # the chain — which does not go through `execute_task` — wired it in neither of its two node
    # runners, so `run_task` took its `(n, t) -> nothing` default.
    #
    # Fixed at the CARRIER, not the call sites: the node fires a `node:progress` chain event and the one
    # frame builder both processes already subscribe to shapes it. Adding it per call site is what
    # drifted in the first place.
    frames = Dict{String,Any}[]
    pairs  = Cecelia.subscribe_chain_frames!(f -> push!(frames, f))
    try
        Cecelia._fire_chain_event!("node:progress", (
            run_id = "r1", chain_name = "c", project_uid = "p", image_uid = "img1",
            node_id = "train", fn = "opticalFlow.train", task_id = "T1", n = 3, total = 12))
        @test length(frames) == 1
        @test frames[1]["type"]     == "task:progress"
        @test frames[1]["taskId"]   == "T1"
        # A FRACTION, the shape both `ws_progress` and the runner's `_emit_progress` already send.
        @test frames[1]["progress"] ≈ 0.25

        # total = 0 must not divide by zero — a task that reports before it knows its scale.
        empty!(frames)
        Cecelia._fire_chain_event!("node:progress", (
            run_id = "r1", chain_name = "c", project_uid = "p", image_uid = "img1",
            node_id = "train", fn = "f", task_id = "T1", n = 0, total = 0))
        @test frames[1]["progress"] == 0.0

        # No task id → DROPPED, not emitted with "". A blank id mints or clobbers a blank row, which is
        # the failure the `task:log` handling already guards against.
        empty!(frames)
        Cecelia._fire_chain_event!("node:progress", (
            run_id = "r1", chain_name = "c", project_uid = "p", image_uid = "img1",
            node_id = "train", fn = "f", task_id = "", n = 1, total = 2))
        @test isempty(frames)
    finally
        for (ev, h) in pairs
            Cecelia.unsubscribe_chain_events!(ev, h)
        end
    end

    # Both node runners must actually wire it — the whole bug was that neither did.
    chain_dir = joinpath(_SUITE_APP_SRC, "tasks", "chain")
    src = read(joinpath(_SUITE_APP_SRC, "tasks", "chain.jl"), String) * "\n" *
          join([read(f, String) for f in
                filter(f -> endswith(f, ".jl"), readdir(chain_dir; join=true))], "\n")
    @test length(collect(eachmatch(r"on_progress\s+= \(n, t\) ->", src))) == 2
end
# ── Pooling several images into one reading (track_cohort.jl / pooled_track_frame) ─────────────
#
# The two track PLOTS compare conditions, so a group's images have to be judged together. Every
# diagnostic in the battery except the pair scan is per-track or per-step arithmetic, so pooling is a
# concatenation — but track ids are per IMAGE, and a plain `vcat` would merge two different cells into
# one track and invent the step between them. That is the failure these tests exist for: it produces no
# error, just a curve measured over a path that never happened.
@testset "pooled_track_frame — ids made unique, groups kept" begin
    a = _diag_df([1 => [(t, Float64(t), 0.0) for t in 0:5],
                  2 => [(t, Float64(t), 10.0) for t in 0:5]])
    b = _diag_df([1 => [(t, Float64(t), 20.0) for t in 0:5],
                  2 => [(t, Float64(t), 30.0) for t in 0:5]])

    p = pooled_track_frame([a, b])
    @test nrow(p) == nrow(a) + nrow(b)
    # FOUR tracks, not two — the whole point
    @test length(track_ids_present(p)) == 4
    @test Set(unique(p.__pool_grp)) == Set([1, 2])
    # the first frame keeps its ids (readable in a CSV), the second is offset by a round stride
    @test Set(Int.(p.track_id[p.__pool_grp .== 1])) == Set([1, 2])
    @test minimum(Int.(p.track_id[p.__pool_grp .== 2])) >= 1000

    # one frame in, one group out — and nothing renumbered
    one = pooled_track_frame([a])
    @test Set(Int.(one.track_id)) == Set([1, 2])
    @test all(one.__pool_grp .== 1)
    @test nrow(pooled_track_frame(DataFrame[])) == 0

    # an untracked cell must not be MINTED into a track by the offset
    u = copy(a); u[!, :track_id] = fill(NaN, nrow(u))
    pu = pooled_track_frame([u, b])
    @test all(isnan, pu.track_id[pu.__pool_grp .== 1])
    @test length(track_ids_present(pu)) == 2
end

@testset "track_diagnostics — a pooled frame never pairs two movies" begin
    one = _diag_df([_diag_walk(id, 12; x0 = 40.0 * id) for id in 1:4])
    d1 = track_diagnostics(one, _DIAG_XY; max_lag = 4)
    @test length(d1.pairs.angle) == 4 * 3 ÷ 2

    pooled = pooled_track_frame([one, copy(one)])
    d2 = track_diagnostics(pooled, _DIAG_XY; max_lag = 4, group_col = :__pool_grp)
    # exactly the within-image pairs, twice — no cross-movie pair, which has no distance to report
    @test length(d2.pairs.angle) == 2 * length(d1.pairs.angle)
    # the evidence that the guard is doing something: without it the scan pairs across images
    @test length(track_diagnostics(pooled, _DIAG_XY; max_lag = 4).pairs.angle) ==
          8 * 7 ÷ 2

    # the linear half pools exactly: same curve, twice the samples
    @test d2.msd.msd ≈ d1.msd.msd
    @test d2.msd.n == 2 .* d1.msd.n
    @test d2.summary.nTracks == 2 * d1.summary.nTracks

    # The O(n²) guard is on the biggest GROUP, not the pooled total. Guarding on the total would skip the
    # pair half for every pooled condition — three ordinary movies already pass PAIR_SCAN_MAX_TRACKS — and
    # the panel would report "not checked" for exactly the comparison the pooling was for.
    three = pooled_track_frame([one, copy(one), copy(one)])
    d3 = track_diagnostics(three, _DIAG_XY; max_lag = 4, max_pair_tracks = 5,
                           group_col = :__pool_grp)
    @test !d3.summary.pairsSkipped                        # 4 tracks per group, not 12
    @test length(d3.pairs.angle) == 3 * length(d1.pairs.angle)
    # …and a group that is genuinely too big is still skipped
    @test track_diagnostics(three, _DIAG_XY; max_lag = 4, max_pair_tracks = 3,
                            group_col = :__pool_grp).summary.pairsSkipped
end

# ── The (images × population) grouping the two track plots share (track_cohort.jl) ─────────────
@testset "image_attr_groups — ONE attribute join for every plot" begin
    imgs = [(; attr = Dict("Treatment" => "WT",    "Mouse" => "m1")),
            (; attr = Dict("Treatment" => "WT",    "Mouse" => "m2")),
            (; attr = Dict("Treatment" => "MerTK")),
            (; attr = Dict{String,String}())]
    uids = ["a", "b", "c", "d"]

    @test image_attr_groups(imgs, uids, ["Treatment"]) ==
          Dict("a" => "WT", "b" => "WT", "c" => "MerTK")      # "d" is ABSENT, not ""
    # combined, empty components dropped (the old R paste0(axisX, ".", interaction))
    m = image_attr_groups(imgs, uids, ["Treatment", "Mouse"])
    @test m["a"] == "WT.m1" && m["b"] == "WT.m2" && m["c"] == "MerTK"
    @test isempty(image_attr_groups(imgs, uids, String[]))
end

@testset "track plot grouping — images and populations, without touching disk" begin
    imgs = [(; attr = Dict("Treatment" => "WT")), (; attr = Dict("Treatment" => "WT")),
            (; attr = Dict("Treatment" => "MerTK"))]
    uids = ["a", "b", "c"]

    # per image (the default): one bundle each, labelled by uID
    per = Cecelia._track_image_groups(collect(zip(imgs, uids)), String[], false)
    @test [g.label for g in per] == uids
    @test all(g -> length(g.items) == 1, per)

    # by attribute: images sharing a value POOL, in first-appearance order
    byattr = Cecelia._track_image_groups(collect(zip(imgs, uids)), ["Treatment"], false)
    @test [g.label for g in byattr] == ["WT", "MerTK"]
    @test length(byattr[1].items) == 2 && length(byattr[2].items) == 1

    # pooled: one unlabelled bundle (there is nothing to distinguish)
    pooled = Cecelia._track_image_groups(collect(zip(imgs, uids)), String[], true)
    @test length(pooled) == 1 && pooled[1].label == "" && length(pooled[1].items) == 3
    # an attribute WINS over pooling — grouping by it is the pooling, and doing both would drop it
    @test length(Cecelia._track_image_groups(collect(zip(imgs, uids)), ["Treatment"], true)) == 2

    # populations: none = the whole segmentation; one bundle each; pooled = one bundle of all
    @test Cecelia._track_pop_groups(String[], "B", false)[1].refs == [("B", "")]
    pg = Cecelia._track_pop_groups(["B/tcells", "C/qc/_tracked"], "B", false)
    @test [g.refs[1] for g in pg] == [("B", "/tcells"), ("C", "/qc/_tracked")]
    # the LEAF is what a user calls a population; the path is a file path
    @test [g.label for g in pg] == ["tcells", "_tracked"]
    poolp = Cecelia._track_pop_groups(["B/a", "B/b"], "B", true)
    @test length(poolp) == 1 && length(poolp[1].refs) == 2

    # the label names both dimensions of the comparison, and stays empty when there is one group
    @test Cecelia._track_group_label("WT", "CD4") == "WT · CD4"
    @test Cecelia._track_group_label("", "") == ""
end

# A legend with the same entry twice, in two colours, is not a legend — and two groups can honestly want
# the same NAME (image names are not unique, only uIDs are; a population leaf repeats across
# segmentations). The collision gains the first dimension that actually differs.
@testset "colliding group labels are disambiguated" begin
    mkdf() = DataFrame(label = [1.0], track_id = [1.0], centroid_t = [0.0],
                       centroid_x = [0.0], centroid_y = [0.0])
    src(uid, vn, pop) = TrackPlotSource(uid, nothing, vn, pop, mkdf(), ["centroid_x", "centroid_y"])
    grp(key, label, srcs) = TrackPlotGroup(key, label, "live", srcs,
                                           ["centroid_x", "centroid_y"], NaN)

    # two IMAGES that happen to share a name → the uID tells them apart
    two_imgs = [grp("u1|B", "Image 1", [src("u1", "B", "")]),
                grp("u2|B", "Image 1", [src("u2", "B", "")])]
    @test [g.label for g in Cecelia._disambiguate_labels(two_imgs)] ==
          ["Image 1 · u1", "Image 1 · u2"]

    # the same population LEAF under two segmentations → the segmentation tells them apart
    two_vns = [grp("u1|B/tcells", "tcells", [src("u1", "B", "/tcells")]),
               grp("u1|C/tcells", "tcells", [src("u1", "C", "/tcells")])]
    @test [g.label for g in Cecelia._disambiguate_labels(two_vns)] == ["tcells · B", "tcells · C"]

    # distinct labels are left alone, and a blank one (the single-group case) is never decorated
    fine = [grp("a", "WT", [src("u1", "B", "")]), grp("b", "MerTK", [src("u2", "B", "")])]
    @test [g.label for g in Cecelia._disambiguate_labels(fine)] == ["WT", "MerTK"]
    @test Cecelia._disambiguate_labels([grp("a", "", [src("u1", "B", "")])])[1].label == ""
end

# The detector EDITS, so its cells may not come from a pooled group: a `track_id` is unique only within
# one (image, segmentation), so an op built from pooled cells would name two different ones and corrupt
# whichever it was not meant for. `track_group_frame` refuses instead of returning a frame that reads fine
# and cannot be acted on — the route then reports the ranking as unavailable rather than wrong.
@testset "track_group_frame refuses to pool cells an EDIT would name" begin
    mkdf(tid) = DataFrame(label = [1.0], track_id = [Float64(tid)], centroid_t = [0.0],
                          centroid_x = [0.0], centroid_y = [0.0])
    sp  = ["centroid_x", "centroid_y"]
    src(uid, vn, tid) = TrackPlotSource(uid, nothing, vn, "", mkdf(tid), sp)
    grp(srcs) = TrackPlotGroup("k", "", "live", srcs, sp, NaN)

    # one source → the cells, with the segmentation they actually came from
    one = track_group_frame(grp([src("u1", "memTom", 7)]))
    @test one !== nothing
    @test one.value_name == "memTom"
    @test one.spatial == sp
    @test nrow(one.df) == 1

    # two sources (a pooled cohort, or two segmentations) → nothing, however plausible the frame would look
    @test track_group_frame(grp([src("u1", "memTom", 7), src("u2", "memTom", 7)])) === nothing
    @test track_group_frame(grp([src("u1", "memTom", 7), src("u1", "importTest2", 7)])) === nothing

    # a source with no cells is not a frame either — the detector would report "0 candidates" for an
    # image it never read
    empty_src = TrackPlotSource("u1", nothing, "memTom", "", mkdf(1)[1:0, :], sp)
    @test track_group_frame(grp([empty_src])) === nothing
end

@testset "track_plot_groups + the two readouts (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "track_plot_groups (fixture missing)"
    else
        mkimg = function ()
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid = "KDIeEm", dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img.attr["Treatment"] = "WT"
            img
        end
        i1, i2 = mkimg(), mkimg()
        i2.attr["Treatment"] = "MerTK"

        # ONE image, no populations — one group, and the payload the route always sent
        gs, dropped, vn = track_plot_groups([i1], ["u1"]; value_name = "B")
        @test length(gs) == 1 && dropped == 0 && vn == "B"
        g = gs[1]
        @test g.label == ""                       # nothing to name: a legend of one is noise
        @test track_group_images(g) == ["u1"] && track_group_value_name(g) == "B"
        @test !isempty(g.spatial)
        p = track_group_paths(g; limit = 3)
        @test length(p.paths) == 3 && p.total > 3
        @test all(k -> !occursin(":", k), keys(p.paths))   # single source → the plain track id

        # TWO images, per image — two groups, each with its own uID as the label
        gs2, = track_plot_groups([i1, i2], ["u1", "u2"]; value_name = "B")
        @test [x.label for x in gs2] == ["u1", "u2"]

        # by attribute — one group per treatment, and each pools its own images
        gs3, = track_plot_groups([i1, i2], ["u1", "u2"]; value_name = "B",
                                 group_attrs = ["Treatment"])
        @test [x.label for x in gs3] == ["WT", "MerTK"]

        # pooled — ONE group over both images, and its track keys say which movie each came from
        gs4, = track_plot_groups([i1, i2], ["u1", "u2"]; value_name = "B", pool_images = true)
        @test length(gs4) == 1 && length(gs4[1].sources) == 2
        pp = track_group_paths(gs4[1]; limit = 4)
        @test all(k -> occursin(":", k), keys(pp.paths))
        @test any(k -> startswith(k, "u1:"), keys(pp.paths))
        @test any(k -> startswith(k, "u2:"), keys(pp.paths))
        # …and the pooled group holds twice the tracks of one image
        @test pp.total == 2 * p.total

        # the group CAP is a stated omission, never a silent subset
        gs5, dropped5 = track_plot_groups([i1, i2], ["u1", "u2"]; value_name = "B", max_groups = 1)
        @test length(gs5) == 1 && dropped5 == 1

        # colour values come from the per-track table, keyed the same way as the paths
        col = track_group_paths(gs4[1]; limit = 4, color_by = "live.track.speed")
        @test col.color_kind == "numeric"
        @test !isempty(col.values) && Set(keys(col.values)) ⊆ Set(keys(col.paths))
        # an unknown column is "uncoloured", not an error
        @test track_group_paths(g; limit = 2, color_by = "nope.nope").color_kind == "none"

        # the diagnostics battery over a group, pooled through pooled_track_frame
        d = track_group_diagnostics(gs4[1]; max_lag = 4)
        @test d !== nothing
        d1 = track_group_diagnostics(g; max_lag = 4)
        @test d.summary.nTracks == 2 * d1.summary.nTracks
        @test !isempty(d.msd.lag)
    end
end


@testset "every run_py call forwards on_progress" begin
    # A task that spawns Python and does not forward `on_progress` throws away every `[PROGRESS]`
    # line the runner emits: `run_py` parses them and calls a no-op, so the bar never moves and the
    # task is indistinguishable from a wedged one for its whole duration — which is usually the LONG
    # part, since the Python phase is where the work happens.
    #
    # This is the same drift the chain executor had, one layer down: the wiring is per call site, so
    # it is per call site that it gets forgotten. Ten of twenty-four calls had. A test is the only
    # thing that makes "did you pass the callback" a property of the codebase rather than of whoever
    # wrote the task.
    #
    # Balanced-paren extraction, not a line window: `run_py`'s kwargs run to 25 lines in some tasks
    # (segment/coastal.jl), so a fixed lookahead silently reports a compliant call as a gap — which is
    # exactly what a first pass at this test did.
    function _py_calls(src::AbstractString)::Vector{String}
        out = String[]
        i = firstindex(src)
        while true
            j = findnext("run_py(", src, i)
            isnothing(j) && break
            k = last(j); depth = 0
            while k <= lastindex(src)
                c = src[k]
                c == '(' && (depth += 1)
                c == ')' && (depth -= 1; depth == 0 && break)
                k = nextind(src, k)
            end
            push!(out, src[first(j):min(k, lastindex(src))])
            i = nextind(src, min(k, lastindex(src)))
        end
        out
    end

    # The ONLY exemptions, each because the call cannot report anything meaningful: a single metadata
    # value read out of a file header, over in milliseconds. Adding one here is a claim that a user
    # will never wait on it — if you find yourself exempting a task that segments, tracks, clusters or
    # trains, wire the callback instead.
    EXEMPT = Set([
        "tasks/importImages/read_ims_time_interval_run.py",       # one TimeIncrement out of an .ims
        "tasks/importImages/read_imagej_physical_size_run.py",    # one pixel size out of an ImageJ tag
    ])

    gaps = String[]
    checked = 0
    for (root, _, files) in walkdir(joinpath(_SUITE_APP_SRC, "tasks")), f in files
        endswith(f, ".jl") || continue
        path = joinpath(root, f)
        for call in _py_calls(read(path, String))
            script = match(r"run_py\(\s*\"([^\"]+)\"", call)
            isnothing(script) && continue           # a computed path — nothing to key an exemption on
            checked += 1
            script[1] in EXEMPT && continue
            occursin("on_progress", call) ||
                push!(gaps, "$(basename(path)) → $(script[1])")
        end
    end

    isempty(gaps) || @info "run_py calls not forwarding on_progress" gaps
    @test isempty(gaps)
    # The scan must actually find calls; a refactor that renamed `run_py` would otherwise "pass".
    @test checked >= 20
end

@testset "task callbacks are bound where they are used" begin
    # The sibling test above checks that a `run_py` call MENTIONS `on_progress`. It cannot check that
    # the name is actually IN SCOPE — and that is the half that shipped broken: `_write_track_props`
    # (track_measures.jl) forwarded `on_progress = on_progress` from a signature that only took
    # `on_log` and `on_process`, so every `tracking.track_measures` run died with
    # `UndefVarError: on_progress not defined` the moment it reached the per-track table. Julia cannot
    # catch this at load time: an unbound global in a function body is a RUNTIME error, on the one line
    # that uses it. Text-matching cannot catch it either — the offending line reads exactly like the 24
    # correct ones. So: parse each file and check the three task callbacks against the enclosing
    # function's arguments (or an enclosing closure's, or a local assignment).
    CALLBACKS = Set([:on_log, :on_progress, :on_process])

    _unwrap(s) = begin                       # strip `where {…}` and `::ReturnType` off a signature
        while s isa Expr
            if s.head === :where
                s = s.args[1]
            elseif s.head === :(::) && length(s.args) == 2 && s.args[1] isa Expr &&
                   s.args[1].head in (:call, :where, :tuple)
                s = s.args[1]
            else
                break
            end
        end
        s
    end

    function _names!(out, a)                 # every name an argument/binding form introduces
        if a isa Symbol
            push!(out, a)
        elseif a isa Expr
            if a.head in (:(::), :(=), :kw, :(...))
                _names!(out, a.args[1])
            elseif a.head in (:parameters, :tuple, :block)
                for p in a.args; _names!(out, p); end
            end
        end
        out
    end

    function _args(sig)                  # positional + keyword argument names of a signature
        s = _unwrap(sig)
        out = Symbol[]
        if s isa Expr && s.head === :call
            for a in s.args[2:end]; _names!(out, a); end
        else
            _names!(out, s)
        end
        out
    end

    _isfun(ex) = ex isa Expr && (ex.head === :function ||
        (ex.head === :(=) && length(ex.args) == 2 &&
         _unwrap(ex.args[1]) isa Expr && _unwrap(ex.args[1]).head === :call))

    _fname(sig) = (s = _unwrap(sig); f = s isa Expr && s.head === :call ? s.args[1] : nothing;
                   f isa Expr ? string(f.head === :(.) ? f.args[end] : f) :
                   f isa Symbol ? string(f) : "λ")

    # Default values in a signature are evaluated in the ENCLOSING scope, so they are walked there.
    function _defaults(walk, sig, bound, fn, file, ln)
        s = _unwrap(sig)
        s isa Expr || return
        for a in (s.head === :call ? s.args[2:end] : s.args)
            a isa Expr || continue
            if a.head in (:kw, :(=)) && length(a.args) == 2
                walk(a.args[2], bound, fn, file, ln)
            elseif a.head === :parameters
                for p in a.args
                    p isa Expr && p.head in (:kw, :(=)) && length(p.args) == 2 &&
                        walk(p.args[2], bound, fn, file, ln)
                end
            end
        end
    end

    offenders = String[]
    function walk(ex, bound::Set{Symbol}, fn::String, file::String, ln::Ref{Int})
        if ex isa Symbol
            ex in CALLBACKS && !(ex in bound) &&
                push!(offenders, "$file:$(ln[]) — `$ex` in `$fn` is not bound in scope")
            return
        end
        ex isa Expr || return
        if _isfun(ex)                                   # named function / short-form definition
            _defaults(walk, ex.args[1], bound, fn, file, ln)
            nb, nm = union(bound, Set(_args(ex.args[1]))), _fname(ex.args[1])
            for a in ex.args[2:end]
                a isa LineNumberNode ? (ln[] = a.line) : walk(a, nb, nm, file, ln)
            end
        elseif ex.head === :(->)                        # lambda — captures the enclosing scope
            nb = union(bound, Set(_args(ex.args[1])))
            for a in ex.args[2:end]
                a isa LineNumberNode ? (ln[] = a.line) : walk(a, nb, fn, file, ln)
            end
        elseif ex.head === :do                          # f(…) do x … end
            walk(ex.args[1], bound, fn, file, ln)
            walk(ex.args[2], bound, fn, file, ln)
        elseif ex.head in (:for, :while, :let, :generator, :comprehension, :try)
            nb = copy(bound)                            # loop/let/catch vars bind inside
            for a in ex.args
                a isa LineNumberNode && (ln[] = a.line; continue)
                if a isa Expr && a.head in (:(=), :in, :(=>)) && length(a.args) == 2
                    walk(a.args[2], nb, fn, file, ln); _names!(nb, a.args[1])
                elseif a isa Symbol
                    push!(nb, a)                        # `catch e`
                else
                    walk(a, nb, fn, file, ln)
                end
            end
        elseif ex.head === :struct                      # field declarations are not a scope
            return
        elseif ex.head === :(=) && ex.args[1] isa Symbol
            walk(ex.args[2], bound, fn, file, ln); push!(bound, ex.args[1])
        elseif ex.head === :(.) && length(ex.args) == 2  # `obj.on_log` is a field, not the name
            walk(ex.args[1], bound, fn, file, ln)
        elseif ex.head === :kw                          # `on_log = …` in a CALL: lhs is the kwarg name
            walk(ex.args[2], bound, fn, file, ln)
        elseif ex.head === :quote
            return
        else
            b2 = copy(bound)
            for a in ex.args
                a isa LineNumberNode ? (ln[] = a.line) : walk(a, b2, fn, file, ln)
            end
        end
    end

    roots   = [_SUITE_APP_SRC, joinpath(dirname(_SUITE_APP_SRC), "..", "api", "src")]
    scanned = 0
    for root in roots
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            ex = try
                Meta.parseall(read(path, String))
            catch e
                push!(offenders, "$f — could not parse: $e"); continue
            end
            scanned += 1
            walk(ex, Set{Symbol}(), "<toplevel>", f, Ref(0))
        end
    end

    isempty(offenders) || @info "task callbacks used out of scope" offenders
    @test isempty(offenders)
    @test scanned >= 40      # a moved/renamed source tree must not make this "pass" by scanning nothing
end
