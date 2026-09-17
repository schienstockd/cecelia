# ── meta accessors + axis gating + branching + anisotropy testsets ─────
# Five sections covering: the typed meta_int / meta_float / meta_str accessors' contract
# (missing / malformed / String vs Real), Axis gating (img_axes + task_applies), per-param
# requires.axes (smooth's temporal controls), Branching spec (µm keys, anisotropy sources,
# copy budget), and Anisotropy µm→px conversion. Extracted from suite.jl to keep it small
# enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope (lexical include).
#
# No `@__DIR__` scans in the extracted range — no path rewrites needed.

# ── Typed meta accessors (meta_int / meta_float / meta_str) ──────────────
#
# `img.meta` is Dict{String,Any} and carries OME-XML metadata in whatever shape the loader wrote —
# `SizeC` is typically a String ("3"), `PhysicalSizeX` a String or Real depending on the source.
# Every reader used to hand-roll its own parse (bare `Int(get(...))` crashed on the String case,
# `tryparse_i(get(...))` didn't return the caller's default, three separate `_meta_*` variants lived
# in `api/src/routes/helpers.jl` alone). These accessors are the ONE way; the testset pins the
# missing/malformed contract so a call site can rely on `something(meta_int(...), default)`.
@testset "meta_int / meta_float / meta_str contract" begin
    m = Dict{String,Any}(
        "SizeC" => "3",           # OME-XML strings — the common case
        "SizeT" => 1,             # numeric passes through
        "PhysicalSizeX" => "0.5",
        "PhysicalSizeY" => 0.5,   # Real → Float64
        "PhysicalSizeUnit" => "µm",
        "ori_path" => "/data/img.tif",
        "garbage" => "not a number",
    )
    @test meta_int(m, "SizeC")   === 3
    @test meta_int(m, "SizeT")   === 1
    @test meta_int(m, "SizeZ")   === nothing            # missing
    @test meta_int(m, "garbage") === nothing            # unparseable
    @test meta_float(m, "PhysicalSizeX") === 0.5
    @test meta_float(m, "PhysicalSizeY") === 0.5
    @test meta_float(m, "SizeZ")         === nothing
    @test meta_str(m, "PhysicalSizeUnit") == "µm"
    @test meta_str(m, "ori_path")         == "/data/img.tif"
    @test meta_str(m, "SizeC")            == "3"        # coerces non-strings, doesn't drop them
    @test meta_str(m, "missing")          === nothing

    # `something(meta_int(...), default)` is the fallback idiom the drift-consolidation depends on.
    @test something(meta_int(m, "SizeZ"), 0) === 0
    @test something(meta_int(m, "SizeC"), 0) === 3
end

# ── Axis gating (task_applies + img_axes) ────────────────────────────────
@testset "Axis gating — img_axes + task_applies" begin
    # img_axes: SizeT > 1 → :T; TimeIncrement present as fallback for pre-SizeT projects.
    img_static = CciaImage(; uid="a1", name="static", dir="")
    img_static.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    @test Cecelia.img_axes(img_static) == Set([:X, :Y, :Z, :C])
    @test !Cecelia.img_has_time(img_static)

    # CALIBRATED, which the axis assertions do not care about but `task_applies` now does: tracking
    # measures in µm/min, so a live image with no pixel size is not runnable (see the scale gate
    # below). Keeping the calibration here rather than on a fourth image keeps this testset about
    # axes with the scale as a given.
    img_live = CciaImage(; uid="a2", name="live", dir="")
    img_live.meta = Dict{String,Any}("SizeC"=>4, "SizeT"=>10, "SizeZ"=>1,
                                     "PhysicalSizeX"=>0.5, "PhysicalSizeY"=>0.5,
                                     "TimeIncrement"=>30)
    @test Cecelia.img_axes(img_live) == Set([:X, :Y, :T, :C])
    @test Cecelia.img_has_time(img_live)

    # TimeIncrement fallback for pre-SizeT imports (present + parseable → :T)
    img_legacy = CciaImage(; uid="a3", name="legacy", dir="")
    img_legacy.meta = Dict{String,Any}("TimeIncrement"=>"30")
    @test :T ∈ Cecelia.img_axes(img_legacy)
    @test Cecelia.img_has_time(img_legacy)

    # task_requires_axes: reads spec's requires.axes (BayesianTracking → {:T})
    @test Cecelia.task_requires_axes(BayesianTracking()) == Set([:T])
    @test isempty(Cecelia.task_requires_axes(ImportOmezarr()))

    # task_applies: T-requiring task rejects a static image, accepts a live one
    @test !task_applies(BayesianTracking(), img_static)
    @test  task_applies(BayesianTracking(), img_live)
    @test  task_applies(ImportOmezarr(), img_static)   # no requirement → always applies

    # Composite recursion: HMM (states + transitions) inherits T from its steps
    hmm = Cecelia._task_from_fun_name("behaviour.hmm")
    @test :T ∈ Cecelia.task_requires_axes(hmm)
    @test !task_applies(hmm, img_static)
    @test  task_applies(hmm, img_live)

    # ── The SCALE half of the same gate ──────────────────────────────────────
    # `requires.scale`: the task computes in microns, so it needs the image to RECORD a scale. The
    # failure it prevents is silent — `img_physical_sizes` falls back to 1.0 for anything absent, and
    # 1.0 is indistinguishable from a real 1 µm/px, so the run succeeds and reports pixels as microns.
    @test Cecelia.task_requires_scale(BayesianTracking()) == Set([:XY, :T])
    @test isempty(Cecelia.task_requires_scale(ImportOmezarr()))

    # Recorded, per axis, and > 0 — a zero is not a measurement.
    @test Cecelia.img_scale_axes(img_live) == Set([:XY, :T])
    @test isempty(Cecelia.img_scale_axes(img_static))
    img_zero = CciaImage(; uid="a4", name="zero", dir="")
    img_zero.meta = Dict{String,Any}("PhysicalSizeX"=>0, "PhysicalSizeY"=>0.5)
    @test isempty(Cecelia.img_scale_axes(img_zero))

    # An uncalibrated live image is BLOCKED, and the reason names the fix rather than the fact —
    # unlike a missing axis, this the user can do something about.
    img_nocal = CciaImage(; uid="a5", name="live-uncal", dir="")
    img_nocal.meta = Dict{String,Any}("SizeC"=>1, "SizeT"=>10, "SizeZ"=>1)
    @test !task_applies(BayesianTracking(), img_nocal)
    reason = Cecelia.task_applicability_reason(BayesianTracking(), img_nocal)
    @test occursin("pixel size", reason) && occursin("time interval", reason)
    @test occursin("metadata", reason)

    # Intersected with the image's own axes: a 2D task on a static image needs no time interval, so
    # a calibrated-XY static image runs a µm-measuring task that does not require T.
    img_static_cal = CciaImage(; uid="a6", name="static-cal", dir="")
    img_static_cal.meta = Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>1,
                                           "PhysicalSizeX"=>0.5, "PhysicalSizeY"=>0.5)
    @test isempty(Cecelia.task_missing_scale(CellposeSegment(), img_static_cal))
    @test Cecelia.task_missing_scale(CellposeSegment(), img_static) == Set([:XY])
    # …and a T-scale requirement does not apply to an image with no T axis at all.
    @test isempty(Cecelia.task_missing_scale(
        Cecelia._task_from_fun_name("segment.cellpose"), img_static_cal))

    # A composite inherits its steps' scale needs, same union as the axes.
    cpm = Cecelia._task_from_fun_name("segment.cellposeMeasure")
    @test :XY ∈ Cecelia.task_requires_scale(cpm)
    @test !task_applies(cpm, img_static)
    @test  task_applies(cpm, img_static_cal)

    # run_task raises TaskApplicabilityError before scheduling on a static image
    proj = create_project!(name="axis-gate-$(rand(1000:9999))")
    s   = add_set!(proj; name="s")
    img = add_image!(s; name="static-img",
                     meta=Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>1))
    @test_throws Cecelia.TaskApplicabilityError run_task(
        BayesianTracking(), img, Dict{String,Any}(); pool_name="cpu")
    rm(proj.root; recursive=true)
end

# ── Per-param image gating (`param.requires.axes`) ────────────────────────────
# The image-side twin of showIf: a param whose axes the image does not carry is deleted from the
# effective run BEFORE validation, and the handler's `get(params, key, default)` returns the "off"
# default. Smooth is the one mixed case in the tree — spatial sigma applies to a still, the temporal
# window does not — so gating the WHOLE task on `requires.axes` would refuse a run that works.
@testset "Per-param requires.axes — smooth's temporal controls" begin
    smooth = Cecelia.Smooth()

    img_still = CciaImage(; uid="p1", name="still", dir="")
    img_still.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>1)
    img_live  = CciaImage(; uid="p2", name="live", dir="")
    img_live.meta  = Dict{String,Any}("SizeC"=>2, "SizeT"=>10, "SizeZ"=>1)

    # Smooth's task-level requires no longer names T — spatial-only smoothing is a legitimate run.
    @test isempty(Cecelia.task_requires_axes(smooth))
    @test task_applies(smooth, img_still)
    @test task_applies(smooth, img_live)

    # After spec defaults + guard, a static image loses the temporal keys; the timelapse keeps them.
    base = Cecelia._apply_spec_defaults(smooth, Dict{String,Any}())
    @test base["temporalFrames"] == 3          # spec default fires
    @test base["temporalStat"]   == "median"
    @test haskey(base, "spatialSigma")

    still_run = Cecelia._apply_param_requires(smooth, img_still, copy(base))
    @test !haskey(still_run, "temporalFrames") # dropped by requires.axes
    @test !haskey(still_run, "temporalStat")
    @test still_run["spatialSigma"] == 1.0     # unguarded, survives
    @test still_run["restoreDynamicRange"] == true

    live_run = Cecelia._apply_param_requires(smooth, img_live, copy(base))
    @test live_run["temporalFrames"] == 3      # timelapse keeps them
    @test live_run["temporalStat"]   == "median"

    # Set-scope intersects: any static image in the mix drops the temporal params.
    mixed = Cecelia._apply_param_requires(smooth, CciaImage[img_live, img_still], copy(base))
    @test !haskey(mixed, "temporalFrames")

    # Vs a task with the task-level requires still in place: this must not touch its params.
    @test !isempty(Cecelia.task_requires_axes(BayesianTracking()))
    unchanged = Cecelia._apply_param_requires(BayesianTracking(), img_live, Dict{String,Any}("k"=>1))
    @test unchanged == Dict{String,Any}("k"=>1)
end

# ── Dispatch + param validation — Branching (segment.branching) ──────────────
# docs/todo/BRANCHING_PLAN.md Phase 1. New task registers via _task_from_fun_name and
# validate_params rejects out-of-range dilation sizes + wrong-typed booleans.
# Ranges, types and the unknown-select case are swept for EVERY task above. What survives here is
# specific to branching: the µm key rename, the enumerated anisotropy sources, and the copy budget.
@testset "Branching spec — µm keys, anisotropy sources, copy budget" begin
    # anisotropySource (docs/todo/SPATIAL_ANISOTROPY_PLAN.md Decision 5) — a select with three
    # allowed values. The runner raises on anything else, so an unknown value must not get past
    # validation and reach Python as a subprocess failure.
    for src in ("skeleton", "mask", "channel")
        @test begin
            validate_params(Branching(), Dict{String,Any}("anisotropySource" => src))
            true
        end
    end
    @test_throws ParamValidationError validate_params(
        Branching(), Dict{String,Any}("anisotropySource" => "intensity"))

    # The anisotropy scales are in MICRONS, and the keys carry `Um` for exactly that reason: a
    # project with saved PIXEL params (sigma 2, box 45) must not have them silently reread as µm,
    # which would be ~3 px and ~75 px. New keys mean the stale values simply do not apply.
    let spec = Cecelia._task_spec(Branching())
        bykey = Dict(String(get(p, "key", "")) => p for p in spec["params"])
        @test haskey(bykey, "structureTensorSigmaUm") && haskey(bykey, "anisotropyBoxUm")
        @test !haskey(bykey, "structureTensorSigma") && !haskey(bykey, "anisotropyBoxSize")
        @test bykey["structureTensorSigmaUm"]["default"] == 7.0
        @test bykey["anisotropyBoxUm"]["default"] == 5.0
        @test bykey["anisotropySource"]["default"] == "skeleton"
        # tips: covered repo-wide by "every task param carries a tip" + the copy-budget sweep
    end
end

# ── µm → px for the anisotropy scales ─────────────────────────────────────────────────────────
# The user sets a PHYSICAL scale (a fibre is ~2 µm thick whatever the objective); the compute is
# in pixels. Getting this backwards, or letting a sub-pixel request through, produces a grid that
# resamples noise rather than summarising structure.
@testset "Anisotropy µm→px conversion" begin
    # EaMaVq's real calibration: 0.596 µm/px
    px, clamped = Cecelia._um_to_px(7.0, 0.596; minimum_px = 0.5)
    @test px ≈ 7.0 / 0.596 && !clamped
    @test round(Int, first(Cecelia._um_to_px(5.0, 0.596; minimum_px = 3))) == 8

    # A coarser image needs FEWER pixels for the same physical scale — the whole point: the same
    # setting means the same thing on both, which a pixel setting never did.
    @test first(Cecelia._um_to_px(5.0, 1.0; minimum_px = 3)) <
          first(Cecelia._um_to_px(5.0, 0.5; minimum_px = 3))

    # Sub-minimum requests clamp AND say so, rather than silently running a different setting.
    px2, clamped2 = Cecelia._um_to_px(0.5, 0.596; minimum_px = 3)
    @test px2 == 3.0 && clamped2
    @test_throws ErrorException Cecelia._um_to_px(5.0, 0.0; minimum_px = 3)

    # Stored bytes scale as boxes, and boxes as 1/box² — so halving the spacing is 4x the file.
    # This is the number behind the "what do I actually put there" tip.
    @test Cecelia._aniso_grid_bytes(1296, 201) == 1296 * 40 * 201        # 36x36 over 201 frames
    @test Cecelia._aniso_grid_bytes(4 * 1296, 201) == 4 * Cecelia._aniso_grid_bytes(1296, 201)
    @test Cecelia._aniso_grid_bytes(100, 0) == Cecelia._aniso_grid_bytes(100, 1)  # static image

    # Advisory only, and only past the threshold — a fine grid is a legitimate choice.
    @test isempty(Cecelia._aniso_grid_findings(Cecelia._aniso_grid_bytes(1296, 201), 1296, 5.0))
    big = Cecelia._aniso_grid_bytes(36_000, 201)      # ~290 MB, a ~1 µm grid on this image
    f = Cecelia._aniso_grid_findings(big, 36_000, 1.0)
    @test length(f) == 1 && f[1]["level"] == "warn"
    @test f[1]["code"] == "branching.aniso_grid_large"
    # the number lives in `detail`, per the catalog rule that prose carries no figures
    @test occursin("289 MB", f[1]["detail"]) && occursin("36000 boxes", f[1]["detail"])
    @test !occursin("289", f[1]["short"])

    # Both branching findings come from the QC CATALOG now, not inlined at the call site
    @test haskey(Cecelia.QC_TEXT, "branching.no_branches")
    @test haskey(Cecelia.QC_TEXT, "branching.aniso_grid_large")
    @test haskey(Cecelia.QC_TEXT, "branching.uncalibrated")
    @test Cecelia._branching_qc_findings(0)[1]["short"] == "No branches found"
    @test isempty(Cecelia._branching_qc_findings(5))
end
