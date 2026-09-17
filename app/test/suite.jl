# Cecelia package smoke tests — the suite body.
#
# Split out of runtests.jl (which keeps the preamble and wraps this in the one aggregating
# @testset). The split is a PERFORMANCE fix, not organisation: as a single 8k-line `@testset
# begin ... end`, the whole body was one top-level thunk that Julia lowered and compiled in full
# before running a single assertion — ~90s of the suite's ~200s, on top of ~99s of ordinary method
# compilation, for ~11s of actual test work. Behind an include it is ~194 small statements instead.
# Keep it that way: do not re-wrap this file in a `begin` block or a single outer @testset.


# ── Config helpers + release-bundle testsets ─────────────────────────
# 9 sections covering: Config resolver (dev↔prod coordination), version-stamp consistency across
# the four files that carry it, fixture-size ratchet (in-repo dir → 1 MB cap), python_bin_path /
# rscript_bin_path (resolved paths, not bare names, macOS-GUI PATH fallbacks), expand_user
# (portable leading-~, silent no-op on Windows without it), ensure_config_dir (safe to WRITE),
# and release-bundle integrity (SHA-256 sidecars on the update payload). Extracted from this
# file to keep it small enough to merge without EOF conflicts on every append.
#
# Three `joinpath(@__DIR__, "..", "..")` repo-root reaches rerouted through `pathof(Cecelia)`
# — @__DIR__ from `app/test/suite/` resolves one dir too shallow, same fix the earlier splits did.
include(joinpath(@__DIR__, "suite", "config.jl"))
# ── Model vaults + denoise + support-training testsets ─────────────────
# 13 sections covering the cellpose / coastal / denoise model-vault + training-workflow
# pipeline (resolvers, enumeration, denoise_model_names / _target, opticalFlow.trainSupportDenoise
# wiring, _support_short_movie_refusal, _support_temporal_window_advisory, SUPPORT param
# validator, _support_train_qc_findings, _denoise_qc_findings, cleanupImages.denoise +
# CoastalSegment dynamic Model options). Extracted from this file to keep it small enough to
# merge without EOF conflicts on every append. The extracted file loads inside this file's
# aggregating testset scope, so any helpers defined earlier in suite.jl are still in scope
# for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "models_training.jl"))

# ── Cohort stages + guides + flow + CellposeSegment testsets ───────────
# 9 sections covering: cohort-stages vs cohort-metrics parity, guide catalogue name
# check (every guide names a real task), composite-half guide declaration,
# parse_temporal_scales, flow_dropped_metrics, flow_model_target, flow_training_qc_findings,
# and CellposeSegment spec dynamic Model options. Extracted from this file to keep it
# small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in suite.jl
# are still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "guides_and_flow.jl"))

# ── Python spawning + setup wizard + preview worker testsets ────────────
# Five sections covering the Python subprocess dispatch environment (custom-modules
# PYTHONPATH via config_dir(), OPENBLAS_NUM_THREADS bounds, task-workers widening under
# BOTH conditions), the first-launch config setup wizard (isolated CECELIA_DEV_DIR temp
# dir, setup_required + set_projects_dir! merge + reload), and the resident preview
# worker PYTHONPATH pin. Extracted from this file to keep it small enough to merge without
# EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the
# extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "pyspawn.jl"))

# ── Log rail testsets ───────────────────────────────────────────
# Five sections pinning the pure halves of app/src/log_stream.jl: log_record's exception
# formatting, child log-line reassembly (level + traceback), the LogRing seq that makes a
# dropped WS frame detectable, the discipline that long-lived children reach the log rail
# (never fire-and-forget), and the log-sources <-> frontend chip-set contract. Extracted
# from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope for the extracted fragments (Julia
# includes are lexical).
include(joinpath(@__DIR__, "suite", "logging.jl"))

# ── Observer / MCP / LabArchives testsets ───────────────────────
# Five sections covering the in-app AI observer runner (Claude CLI argv, MCP config,
# spawn wrapping, registration state, shadow scope cleanup), the observer prompt-as-role
# contract (loop's own tools named here, shared MCP catalogue not restated), MCP
# connections enumeration, the LabArchives context sidecar (round-trip / gaps / briefing),
# and the AI observer session sidecar (tokens + turns + clear). Extracted from this file
# to keep it small enough to merge without EOF conflicts on every append. The extracted
# file loads inside this file's aggregating testset scope, so any helpers defined earlier
# in suite.jl are still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "observer.jl"))

# ── Model + storage + observer-state testsets ─────────────────────────
# 11 sections covering: create project + image, per-image bookmarking (starred), REPL /
# notebook data-access surface (Observer Phase 2 foundation), run log (open→close + reaping),
# session briefing + all_qc_docs, lockfile guard, atomic commit_state! (concurrent-registration
# safety), durable state writes, state_file derivation, and resolve_value_name (the defaultOnly
# half of R's cciaImage$valueNames). Extracted from this file to keep it small enough to merge
# without EOF conflicts on every append. The extracted file loads inside this file's
# aggregating testset scope, so any helpers defined earlier in suite.jl are still in scope
# (lexical include).
include(joinpath(@__DIR__, "suite", "model.jl"))
# ── Lab-log + param-validation testsets ────────────────────────────────
# Per-project append-only lab log (read/append/dismiss + auto [Cecelia] activity digest capture)
# and task-dispatch param validation. Extracted from this file to keep it small enough to merge
# without EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "lablog_and_validation.jl"))

# ── meta accessors + axis gating + branching + anisotropy testsets ──────
# Five sections covering the typed meta_int / meta_float / meta_str accessors' contract,
# Axis gating (img_axes + task_applies), per-param requires.axes (smooth's temporal
# controls), Branching spec (µm keys, anisotropy sources, copy budget), and Anisotropy
# µm→px conversion. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the
# extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "meta_and_branching.jl"))

# ── QC + AF correction + channel-index resolver testsets ──────────
# Five sections covering Smoothing QC (photon-limited zeros / clipping), AF correction QC
# (retired exemption + per-channel plumbing), the every-QC-finding-carries-the-GUI-fields
# ratchet (walkdir over app/src/*), the channel names -> indices resolver (one resolver,
# 6-drift closure), and AF params being just channels. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in suite.jl
# are still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "qc_af.jl"))

# ── Spatial + neighbours + region-clustering testsets ─────────────────
# Six sections covering Cohort metrics (branching anisotropy), cellNeighbours QC findings,
# aggregate DBSCAN ids, cellContacts target-name sanitisation, neighbourStats spec, and
# clustRegions spec. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating testset
# scope, so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "spatial_neighbours.jl"))

# ── Task spec + repeatable-group + param-defaults testsets ─────────────
# Six sections covering every slider (int/float) can reach its own max and default, a
# second model group is not born a copy of the first, coastal forwards every top-level
# spec param to its runner, an int param never declares a fractional step, a repeatable
# group's run order is resolved into the group, and a group's two sets of defaults agree.
# Extracted from this file to keep it small enough to merge without EOF conflicts on every
# append. The extracted file loads inside this file's aggregating testset scope, so any
# helpers defined earlier in suite.jl are still in scope for the extracted fragments
# (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "specs_and_params.jl"))

# ── Plot specs + canvas + spatial-graph + neighbourStats-QC testsets ──────
# Seven sections covering plot specs live on the page that EXPLORES (not DEFINES), plot
# spec groupByOptions name current columns, every canvas host offers Close all, summary
# canvas is set-scoped / gating canvas is image-scoped, interaction matrix aggregates with
# NO population targets, spatial graph path accessor + discovery, and neighbourStats QC
# findings. Extracted from this file to keep it small enough to merge without EOF conflicts
# on every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted fragments
# (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "plot_specs.jl"))

@testset "params NOT declared in the spec pass through untouched" begin
    # Ranges/types are swept above. What is asserted here is the absence of a rule: a task's
    # runner may receive keys its spec does not declare, and validation must not reject an
    # unknown key — several tasks rely on carrying extra values through to their runner.
    # cropImage's spec now declares one `cropBox` param carrying the whole box (the imagePicker
    # widget's shape); everything else is unknown and passes through.
    box = Dict{String,Any}("x0"=>0, "x1"=>100, "y0"=>0, "y1"=>100,
                           "z0"=>2, "z1"=>8, "t0"=>-1, "t1"=>-1)
    @test validate_params(
        CropImage(), Dict{String,Any}("cropBox" => box, "extraKey" => "unknown")) === nothing
end

# ── Task pure-helper testsets (calibration + copy) ───────────────────
# Eight sections covering pure task helpers that carry calibration/provenance metadata
# forward: CropImage, ZProject, TProject (inherit source calibration), BinImage (rescales
# by factor), ResampleZ (rewrites SizeZ), Register (stacks channels across cycles),
# CopyImage (calibration + provenance), and the CopyImage copy-tree helper. Extracted
# from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope for the extracted fragments (Julia
# includes are lexical).
include(joinpath(@__DIR__, "suite", "calibration_helpers.jl"))

@testset "Custom module registry (drop-in tasks)" begin
    # A user drops a task by calling register_task! with an instance + a spec path; it must then
    # resolve through _task_from_fun_name / _spec_path / validate_params exactly like a built-in.
    spec_dir = mktempdir()
    spec = joinpath(spec_dir, "exampleTest.json")
    write(spec, JSON3.write(Dict(
        "fun_name"      => "customTest.exampleTest",
        "label"         => "Example test",
        "resource_pool" => "cpu",
        "scope"         => "image",
        "params"        => [Dict("key" => "n", "label" => "N", "type" => "int",
                                 "min" => 0, "max" => 10)],
    )))

    register_task!("customTest.exampleTest", _TestCustomTask(); spec = spec)

    @test _task_from_fun_name("customTest.exampleTest") isa _TestCustomTask
    @test Cecelia._spec_path(_TestCustomTask()) == spec       # default _spec_path → custom registry
    @test task_scope(_TestCustomTask()) == "image"           # spec read via the registered path
    # param validation uses the dropped spec (n has min=0/max=10)
    @test_throws ParamValidationError validate_params(
        _TestCustomTask(), Dict{String,Any}("n" => 99))
    @test validate_params(_TestCustomTask(), Dict{String,Any}("n" => 3)) === nothing

    # built-ins win on a fun_name clash — registering under an existing name must NOT shadow it
    register_task!("importImages.remove", _TestCustomTask(); spec = spec)
    @test _task_from_fun_name("importImages.remove") isa RemoveImage

    # a missing spec file is rejected up front
    @test_throws ArgumentError register_task!(
        "customTest.bad", _TestCustomTask(); spec = joinpath(spec_dir, "nope.json"))

    # unknown fun_name still errors
    @test_throws Exception _task_from_fun_name("customTest.doesNotExist")
end

# ── Plugin testsets ────────────────────────────────────────────────────────
# ~15 testsets covering plugin layout / enumeration, manifest + versioning, contributions,
# bundled examples, module staleness, fun_name precedence, custom-module load order,
# shipped examples end-to-end, points-import path, install/remove (P2), resource-pool mapping,
# live pool limit + status snapshot, and the custom-module reload prune. Extracted from this file
# to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope for the plugin fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "plugins.jl"))

# ── Section-param flattening (whiteboard/chain stores section params NESTED) ───
# Regression: a chain node saves `section` params under the section key (e.g.
# measureOptions => {extendedMeasures: true}), but tasks read them flat. run_task must lift them.
@testset "Section params flatten (chain nesting)" begin
    ml = _task_from_fun_name("segment.measureLabels")
    # measureLabels declares `measureOptions` + `imageTiling` sections
    @test "measureOptions" in Cecelia._section_keys(ml)
    @test "imageTiling"    in Cecelia._section_keys(ml)
    nested = Dict{String,Any}(
        "outputValueName" => "T",
        "measureOptions"  => Dict{String,Any}("extendedMeasures" => true),
        "imageTiling"     => Dict{String,Any}("blockSize" => 4096, "overlap" => 0))
    flat = Cecelia._flatten_sections(ml, nested)
    @test flat["extendedMeasures"] == true          # was buried under measureOptions
    @test flat["blockSize"] == 4096                  # was buried under imageTiling
    @test !haskey(flat, "measureOptions")            # section container dropped
    @test flat["outputValueName"] == "T"             # top-level survives
    # composite pulls section keys from its sub-tasks (cellpose + measureLabels)
    comp = _task_from_fun_name("segment.cellposeMeasure")
    @test "measureOptions" in Cecelia._section_keys(comp)
    @test Cecelia._flatten_sections(comp,
        Dict{String,Any}("measureOptions" => Dict{String,Any}("extendedMeasures" => true)))["extendedMeasures"] == true
    # already-flat params are unchanged (idempotent)
    @test Cecelia._flatten_sections(ml, Dict{String,Any}("extendedMeasures" => true))["extendedMeasures"] == true
end

# ── Dispatch + param validation — ClustTracks (clustTracks.cluster, set-scope) ───

# ── Image + data-model round-trip testsets ─────────────────────────────
# 12 testsets covering: legacy `kind` silently ignored on load, image status/attr round-trip,
# task-subdir housekeeping, branch labels round-trip, reserved value_name suffixes, per-image
# user flags (included / note / starred), per-task funParams memory (R moduleFunParams parity),
# versioned channel names, destructive ops, boundary contract (real module fn end-to-end),
# storage reclaim, and analysis reset. Extracted from this file to keep it small enough to merge
# without EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope (lexical include).
include(joinpath(@__DIR__, "suite", "image_model.jl"))
# ── Scheduler + task-execution testsets ────────────────────────────────
# 10 testsets covering: TaskJob target sum type, task crash tee'd into per-image log,
# cancelled run banking, submitter-release on error-path throw, terminal task-rail replay,
# scheduler timing stamps, work-start timing, set expansion + rename, sink-agnostic
# execute_task, and the detached task runner. Extracted from this file to keep it small
# enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope for the scheduler fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "scheduler_tests.jl"))
# ── Chain testsets ────────────────────────────────────────────────────────
# 25+ testsets covering template round-trip, validation, run, resume, fault isolation,
# picnic-node policies, event bus, resource pools, pipelining and the headless entry point.
# Extracted from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope for the chain fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "chain.jl"))

# QC helper for FlowRegister — the "aligner is chronically saturating" case is what a user needs to
# know about (raise the clamp or accept the deformation is beyond dense flow) and the metrics are
# cohort-comparable, so both branches deserve a pin.
@testset "flow_register QC" begin
    quiet = Dict{String,Any}(
        "flowMax"    => [0.0, 3.2, 4.1, 2.8],
        "flowMean"   => [0.0, 0.8, 1.1, 0.7],
        "maxShiftPx" => 16.0,
    )
    @test isempty(Cecelia._flow_register_qc_findings(quiet))
    qm = Cecelia._flow_register_qc_metrics(quiet)
    @test qm["peakFlowPx"] == 4.1
    # 2.525 -> 2.52 under Julia's banker's rounding at digits=2 (ties to even).
    # Assert what actually ships, not the raw mean.
    @test qm["meanPeakFlowPx"] ≈ 2.52 atol=1e-3
    @test qm["meanFlowPx"]  ≈ 0.65  atol=1e-3

    # >= 50% of frames within 85% of the 16 px clamp -> single 'high_shifts' warn.
    saturating = Dict{String,Any}(
        "flowMax"    => [0.0, 15.0, 14.5, 15.2, 14.0],
        "flowMean"   => [0.0, 5.0, 4.8, 5.3, 4.9],
        "maxShiftPx" => 16.0,
    )
    findings = Cecelia._flow_register_qc_findings(saturating)
    @test length(findings) == 1
    @test findings[1]["level"] == "warn"
    @test findings[1]["code"]  == "flow_register.high_shifts"
    @test findings[1]["detail"]["framesNearCap"] == 4
    @test findings[1]["detail"]["nFrames"]       == 5
    # numbers live in detail, not the copy (docs/UI.md → QC copy)
    @test !occursin("14", findings[1]["long"])

    # No flow trajectory at all -> no claim, rather than a spurious warn.
    @test isempty(Cecelia._flow_register_qc_findings(Dict{String,Any}("maxShiftPx" => 16.0)))
    @test isempty(Cecelia._flow_register_qc_findings(
        Dict{String,Any}("flowMax" => Float64[], "maxShiftPx" => 16.0)))

    # meanFrameCorrelation metric (Phase 1) — average of the finite entries,
    # emitted as a per-image cohort-comparable diagnostic (Galene-derived; see
    # Warren et al. 2018, eLife 7:e35800). The metric is banked even when the
    # `high_shifts` warn does NOT fire, so the sidecar has to carry it.
    with_corr = merge(quiet, Dict{String,Any}(
        "frameCorrelation" => [1.0, 0.8, 0.4, 0.2]))
    qm2 = Cecelia._flow_register_qc_metrics(with_corr)
    @test qm2["meanFrameCorrelation"] == 0.6
    # Absent → metric absent (rather than a spurious NaN or 0).
    @test !haskey(Cecelia._flow_register_qc_metrics(quiet), "meanFrameCorrelation")
end

@testset "fun_name dispatch" begin
    @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
    @test _task_from_fun_name("importImages.remove")  isa RemoveImage
    @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
    @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
    @test _task_from_fun_name("cleanupImages.stackAlign")      isa StackAlign
    @test _task_from_fun_name("cleanupImages.flowRegister")    isa FlowRegister
    @test _task_from_fun_name("cleanupImages.smooth")          isa Smooth
    @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
    @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
    cp_measure = _task_from_fun_name("segment.cellposeMeasure")
    @test cp_measure isa CompositeTask
    @test cp_measure.fun_name == "segment.cellposeMeasure"
    @test_throws ErrorException _task_from_fun_name("nonexistent.task")
    @test _task_from_fun_name("tracking.bayesian_tracking")    isa BayesianTracking
    @test _task_from_fun_name("tracking.track_measures")       isa TrackMeasures
    bt_measures = _task_from_fun_name("tracking.bayesian_track_measures")
    @test bt_measures isa CompositeTask
    @test bt_measures.fun_name == "tracking.bayesian_track_measures"
end

# ── Typed-params + label_props round-trip testsets ──────────────────────
# 10+ testsets: BayesianTracking + TrackMeasures param validation, Labels field round-trip,
# CompositeTask spec load, $include fragment resolution, label_props field round-trip,
# Param validation (AfCorrect group / DriftCorrect), Versioned helpers, LabelProps reader
# via HDF5.jl, LabelProps writer (add_obs / save! — the chain write path), Julia ↔ Python
# reader parity (the duplication safety net), and track-measures numeric cross-check vs
# celltrackR. Extracted from this file to keep it small enough to merge without EOF conflicts
# on every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope (lexical include).
include(joinpath(@__DIR__, "suite", "labelprops.jl"))
# ── Gating engine + population manager testsets ────────────────────────
# 8 sections covering: gating engine (transforms + gates + density), population manager
# (paths / tree / persistence), re-parenting (move_pop!), pop UIDs (stable id every outside
# reference points at), retired UIDs (MULTI_POP_TRACKING_ORPHANS_PLAN P2), del_children!
# (prune BELOW without dropping the pop), and boolean populations (Decision 16 — a pop defined
# by combining OTHER pops). Extracted from this file to keep it small enough to merge without
# EOF conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope (lexical).
include(joinpath(@__DIR__, "suite", "gating.jl"))
# ── Population types + cluster/region + spatial obs testsets ─────────────
# 10 sections covering GATING_POP_TYPES, img_has_value_name, clust/trackclust pop types,
# region pop type (spatial regions), contact_matrix, region pop auto-share, bare
# cluster/region pops, clustfeatures sidecar, spatial obs measures are numeric, and
# region 'other' column skipped when all-zero. Extracted from this file to keep it small
# enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "pop_types.jl"))

# ── Filter-pop + compound + FilterFun/FilterCondition testsets ──────────
# Five sections covering region composition column naming, compound filter populations
# (Decision 15 AND-ed conditions), FilterFun + FilterCondition boundary coercion + JSON
# round-trip, recompute! degrades to empty on missing filter/gate column, and
# colour_by_palette (pop colour else default). Extracted from this file to keep it small
# enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "filter_pop.jl"))

# ── Population pickers + resolve_pop_type testsets ───────────────────
# Six sections covering plot population picker (plot_pop_types / plot_population_groups),
# popScope population picker, population accepts allow-list + category tags, branch
# pop_type wiring, ensure_filter_pop! auto-created population, and resolve_pop_type +
# pop_namespace (mixed-type pickers). Extracted from this file to keep it small enough to
# merge without EOF conflicts on every append. The extracted file loads inside this file's
# aggregating testset scope, so any helpers defined earlier in suite.jl are still in scope
# for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "pop_pickers.jl"))

# ── pop_df_multi membership over real H5AD (equals per-type pop_df; unknown refs skip cleanly) ──
# ── pop_df_multi + tracked_pop_parents + cluster-share testsets ────────────
# Three sections covering pop_df_multi integration (KDIeEm), tracked_pop_parents (no
# _tracked row that copies a deeper one), and cluster pop auto-share (co-clustered
# value_names). Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the
# extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "pop_df_multi_share.jl"))

# ── Gating engine: recompute, membership, filtered (tracked) pops ─────────
# ── Gate + centroids + spatial-gate-units testsets ────────────────────
# Four sections covering recompute! + cells_in_pop, explicit-label (pick selection)
# membership, scale_centroids! maps each axis by name, and spatial gate units. Extracted
# from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope for the extracted fragments (Julia
# includes are lexical).
include(joinpath(@__DIR__, "suite", "gate_centroids.jl"))

# ── pop_df(centroids=…): coordinates without naming the columns ────────────
# `pop_df` is the primary accessor for population data (docs/POPULATION.md) — a caller should never
# have to know which centroid columns exist (they differ per segmentation) or convert units by hand.
# ── pop_df core testsets ───────────────────────────────────────────
# Nine sections covering the pop_df read path: centroids (KDIeEm), pooling + dedup,
# drop_na, track_id dedup key, live _tracked (derived filter), reserved pop names,
# cache auto-invalidation, integration (KDIeEm), labels honours the value_name prefix.
# Extracted from this file to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "pop_df.jl"))

# ── resolve_pops: cached, display-ready per-pop membership (napari points overlay) ──
@testset "resolve_pops (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
        want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12), colour="#ef4444")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
        @test L.show === true && L.is_track === false
        @test sort(L.labels) == want                       # membership == the gate's cells

        # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
        again = resolve_pops(img, "flow"; value_name="B")
        @test again === layers
        @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
    end
end

# ── resolve_pops.has_tracks — DATA fact per pop, drives ribbon eligibility next to `is_track` ─────
# The `has_tracks` field (MULTI_POP_TRACKING_PLAN.md Decision 2) says whether a pop CURRENTLY holds
# any cell with `track_id > 0` — so a flow gate on cells that have since been tracked qualifies as
# ribbon-drawable without touching its `is_track` flag (which stays "was TYPED as a track pop"). The
# fixture h5ad `KDIeEm/B.h5ad` carries a `track_id` obs column (see the test at line 8837 above), so
# a broad flow gate lands on some tracked cells.
@testset "resolve_pops has_tracks: data flag, orthogonal to is_track" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops has_tracks (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # Broad gate: every measured cell (0 → 1e12 on both axes) → includes the tracked ones. A
        # `resolve_pops` layer for this flow pop should carry `has_tracks == true`.
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "all"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              -1e12, 1e12, -1e12, 1e12), colour="#3b82f6")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.is_track === false                # flow pop → typed flag stays false
        @test L.has_tracks === true               # data flag: at least one label has track_id > 0
    end
end

# ── has_tracks attribution guard (MULTI_POP_TRACKING_ORPHANS_PLAN P0) ─────────
# `_pop_has_authored_tracks` is the predicate that fixes the wrong-attribution bug where an orphan
# row (from a deleted pop) or a shared-label row (from an overlapping live pop) bled into a
# different pop's ribbon. Pure predicate so we can pin its four rules without a fixture that
# carries a categorical `track_source` column (which requires the Python writer). See docstring +
# the plan's decision 1.
@testset "has_tracks attribution guard (pure predicate)" begin
    UID_A = "aAaAaA"
    UID_B = "bBbBbB"
    WS    = Cecelia.WHOLE_SEG_TRACK_SOURCE
    src(d) = Dict{Int,Union{String,Nothing}}(k => v for (k, v) in d)

    # 1. Empty label_to_source → false regardless of pop.uid / labels.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], src(Dict())) === false

    # 2. A ↔ its own labels: authored by A → true.
    m1 = src(Dict(1 => UID_A, 2 => UID_A))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2, 3], m1) === true

    # 3. B on labels A authored → false. THIS is the bug the guard fixes: pre-guard, B.has_tracks
    #    would fire because B's labels overlap tracked rows; now B claims only rows IT authored.
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2, 3], m1) === false

    # 4. `whole_seg` sentinel → counts for everyone (the documented prime-everything mode).
    m_ws = src(Dict(1 => WS, 2 => WS))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_ws) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_ws) === true

    # 5. Legacy row (`nothing`) → counts for everyone. Preserves pre-P1 behaviour for h5ads written
    #    before the provenance ship (decision 1's legacy branch).
    m_leg = src(Dict(1 => nothing, 2 => nothing))
    @test Cecelia._pop_has_authored_tracks(UID_A, [1, 2], m_leg) === true
    @test Cecelia._pop_has_authored_tracks(UID_B, [1, 2], m_leg) === true

    # 6. Mixed set: label 1 authored by A, label 2 by B, label 3 legacy, label 4 whole_seg,
    #    label 5 orphaned (deleted pop's UID no longer live). Guard fires when ANY qualifying
    #    label sits in `labs` — attribution is per-pop, per-label.
    ORPHAN = "zZzZzZ"
    m_mix = src(Dict(1 => UID_A, 2 => UID_B, 3 => nothing, 4 => WS, 5 => ORPHAN))
    #   A owns 1 directly → true even in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_A, [1], m_mix) === true
    #   B owns 2 → true in isolation.
    @test Cecelia._pop_has_authored_tracks(UID_B, [2], m_mix) === true
    #   Nobody but the orphan-source claims label 5. A does NOT get it.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5], m_mix) === false
    #   Legacy label 3 counts for A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [3], m_mix) === true
    #   whole_seg label 4 counts for B.
    @test Cecelia._pop_has_authored_tracks(UID_B, [4], m_mix) === true
    #   A over [5, 6] (all orphan or absent) → false. Confirms the ORPHAN row cannot bleed into A.
    @test Cecelia._pop_has_authored_tracks(UID_A, [5, 6], m_mix) === false

    # 7. Label absent from label_to_source (i.e. `track_id ≤ 0` or NaN) → not tracked, ignored.
    @test Cecelia._pop_has_authored_tracks(UID_A, [99, 100], m1) === false
end

# ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
# count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
# See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
# ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
@testset "labels pop_type + count (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "labels pop_type (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
        all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test nrow(all) > 0
        @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
        @test unique(all.pop) == ["/labels"]
        @test unique(all.value_name) == ["B"]

        # cell count via the summary aggregator over labels — one series, value == total.
        whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
        @test whole["chartType"] == "count"
        @test length(whole["series"]) == 1
        @test whole["series"][1]["value"] == Float64(nrow(all))

        # count per timepoint (group_by the temporal column) → counts partition the total.
        byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="centroid_t")
        @test byT["groupBy"] == "centroid_t"
        @test length(byT["series"]) > 1
        @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

        # a morphology distribution over labels, per timepoint
        area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                 measure="area", group_by="centroid_t")
        @test area["measure"] == "area"
        @test length(area["series"]) == length(byT["series"])

        # targets signature (the path the summary canvas + whiteboard QC row use: series =
        # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
        tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
        @test tg["chartType"] == "count"
        @test length(tg["series"]) == 1
        @test tg["series"][1]["value"] == Float64(nrow(all))
        @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
    end
end

# ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
@testset "track table helpers" begin
    td = mktempdir()
    img = CciaImage(uid="X", dir=td)
    # companion track table sits next to the cell labelProps with the __tracks suffix
    @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
    @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
    @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
    # reserved value-name suffix (a user segmentation may not end in __tracks)
    @test is_reserved_value_name("A__tracks")
    @test is_reserved_value_name("foo__tracks")
    @test !is_reserved_value_name("A")
    @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
    # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
    @test Cecelia._jsonsafe(NaN) === nothing
    @test Cecelia._jsonsafe(1.5) === 1.5
    @test Cecelia._jsonsafe(3)   === 3
    # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
    mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img.label_props["B"] = "B.h5ad"
    kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :cell, String[], String[])
    kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :track, String[], String[])
    @test kc != kt
end

# ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
@testset "pop_df :track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df :track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # track table layout: measures in X/var, lineage in obs, one row per track_id
        tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
        @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
        tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
        @test "track_root" in tobs

        # one row per track; carries measures + track_id + value_name
        tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
        @test nrow(tr) > 0
        @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
        @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
        @test unique(tr.value_name) == ["B"]

        # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
        ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
        ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
        @test nrow(tr) == ntracks_cells
        @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

        # pop_cols restriction returns just that measure (+ bookkeeping)
        sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                    pop_cols=["live.track.speed"])
        @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))

        # cell_measures aggregation (the clustTracks path): a per-cell measure is aggregated to
        # per-track feature column(s) via track_props, alongside motility — this is what lets
        # clustTracks cluster `_tracked` pops on HMM/intensity features, not just motility.
        cvars = col_names(label_props(img; value_name="B"); data_type=:vars)
        if !isempty(cvars)
            base = String(first(cvars))                        # a real per-cell measure
            ag = pop_df(img, "live", ["B/_tracked"]; granularity=:track, cell_measures=[base])
            @test any(startswith(c, base * ".") for c in names(ag))   # aggregated → {base}.…
            @test nrow(ag) == nrow(tr)                          # same tracks, extra feature cols
            @test "live.track.speed" in names(ag)               # motility still present
            @test "num_cells" in names(ag)                      # per-track cell count (minTracklength)
        end
    end
end

# ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
@testset "plot_summary_data (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # histogram of per-track speed (granularity=:track): shared edges, every track binned
        h = plot_summary_data(img, "live", ["B/_tracked"], "histogram";
                              measure="live.track.speed", granularity=:track, nbins=20)
        @test h["chartType"] == "histogram"
        @test length(h["binEdges"]) == 21
        @test length(h["series"]) == 1
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test sum(h["series"][1]["counts"]) == ntr

        # mock a categorical per-cell column, then frequency over the tracked cells
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        mock = DataFrame("label" => cells.label,
                         "mock.state" => [Float64((l % 3) + 1) for l in cells.label])
        label_props(img_label_props_path(img, "B")) |> add_obs(mock) |> save!
        f = plot_summary_data(img, "live", ["B/_tracked"], "frequency";
                              measure="mock.state", granularity=:cell, normalize=:fraction)
        @test f["chartType"] == "frequency"
        @test Set(f["categories"]) ⊆ Set(["1", "2", "3"])
        @test length(f["series"]) == 1
        props = f["series"][1]["values"]
        @test all(0 .<= props .<= 1) && isapprox(sum(props), 1.0; atol=1e-9)

        # measureType auto-detection: continuous speed → numeric; integer code set → categorical
        @test h["measureType"] == "numeric"
        @test f["measureType"] == "categorical"

        # bar: mean + all three error metrics (sd, sem = sd/√n, ci95 ≈ 1.96·sem)
        br = plot_summary_data(img, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track)
        s = br["series"][1]
        @test Set(keys(s)) ⊇ Set(["value", "sd", "sem", "ci95", "n"])
        @test s["sem"] ≈ s["sd"] / sqrt(s["n"])
        @test s["ci95"] ≈ 1.96 * s["sem"]

        # raw points: boxplot with raw_points carries downsampled values (≤ cap); "points" chart
        bx = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track,
                               raw_points=true, max_points=10)
        @test length(bx["series"][1]["points"]) == min(ntr, 10)
        pts = plot_summary_data(img, "live", ["B/_tracked"], "points";
                                measure="live.track.speed", granularity=:track, max_points=10)
        @test pts["chartType"] == "points" && length(pts["series"][1]["points"]) == min(ntr, 10)
        # without raw_points, boxplot carries no payload of values
        bx0 = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                measure="live.track.speed", granularity=:track)
        @test isempty(bx0["series"][1]["points"])
    end
end

@testset "motion dimensionality detection (2D vs 3D)" begin
    # build a Track by cumulative-summing per-step [dz,dy,dx] vectors (coords are [z,y,x])
    mk(steps) = begin
        P = zeros(length(steps) + 1, 3)
        for k in 1:length(steps); P[k+1, :] = P[k, :] .+ steps[k]; end
        Cecelia.Track(1, Float64.(0:length(steps)), P)
    end
    dy(k) = 2 + 0.5 * cos(k / 3); dx(k) = 2 + 0.5 * sin(k / 3)   # persistent forward heading in xy
    # z either tracks the persistent xy motion (real 3D) or oscillates with large amplitude (jitter)
    real(rng)   = mk([[2 + 0.5*sin(k/3), dy(k), dx(k)] for k in rng])
    jitter(rng) = mk([[(-1.0)^k * 4.0,   dy(k), dx(k)] for k in rng])
    trks_real   = [real(t*20 : t*20+14)   for t in 1:8]
    trks_jitter = [jitter(t*20 : t*20+14) for t in 1:8]

    d3 = Cecelia._detect_motion_dims(trks_real)
    @test d3.dims == 3 && d3.z_used            # persistent z → keep 3D
    d2 = Cecelia._detect_motion_dims(trks_jitter)
    @test d2.dims == 2 && !d2.z_used           # oscillating/anti-persistent z → in-plane 2D
    @test d2.metrics["autocorrZ"] < 0          # jitter signature

    # a 2D-only track set (no z column) is trivially 2D
    P2 = zeros(12, 2); for k in 1:11; P2[k+1, :] = P2[k, :] .+ [dy(k), dx(k)]; end
    @test Cecelia._detect_motion_dims([Cecelia.Track(1, Float64.(0:11), P2)]).dims == 2
end

# ── uns reader: the anisotropy grid on the branch sidecar ─────────────────────────────────────
# The one thing worth pinning here is the DIMENSION REVERSAL. HDF5 stores C-order, Julia reads
# column-major, so a numpy (T, y, x, comp) array arrives as (comp, x, y, T) — every axis flipped,
# INCLUDING the two box axes, which are equal-length and would therefore swap silently. The
# fixture's values encode their own (t, y, x) coordinates precisely so a transposed read fails
# instead of passing on symmetry.
@testset "uns reader (anisotropy grid)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        lp = label_props(h5)
        @test "orientation_coords" in uns_keys(lp) && "orientation_meta" in uns_keys(lp)

        # producer order = numpy order: (T, y_boxes, x_boxes, component)
        coor = uns_array(lp, "orientation_coords")
        @test size(coor) == (3, 4, 4, 2)
        # value encodes 100t + 10y + x, so this catches an axis swap, not just a shape match
        @test coor[1, 1, 1, 1] ≈ 0.0f0
        @test coor[3, 2, 4, 1] ≈ 100 * 2 + 10 * 1 + 3      # t=2, y=1, x=3 (0-based)
        @test coor[3, 2, 4, 2] ≈ 1000 + 100 * 2 + 10 * 1 + 3
        @test size(uns_array(lp, "orientation_eigvec")) == (3, 4, 4, 2, 2)
        @test size(uns_array(lp, "orientation_box_coherence")) == (3, 4, 4)

        # as_stored hands back the raw (reversed) layout for a caller that wants it
        @test size(uns_array(lp, "orientation_coords"; as_stored=true)) == (2, 4, 4, 3)

        # the self-describing block — strings, scalars and arrays all round-trip
        m = uns_dict(lp, "orientation_meta")
        @test m["box_size_px"] == 15 && m["sigma_px"] ≈ 12.0
        @test m["source"] == "skeleton" && m["fibre_direction"] == "minor"
        @test m["eigval_order"] == "ascending" && m["eigvec_layout"] == "vec_major"
        @test Int.(m["t_index"]) == [0, 1, 2]
        @test length(m["scale_um_per_px"]) == 2

        # absent key, and a group requested as an array (or vice versa) → nothing, not a throw
        @test uns_array(lp, "no_such_key") === nothing
        @test uns_array(lp, "orientation_meta") === nothing
        @test uns_dict(lp, "orientation_coords") === nothing

        # `orientation_summary` is a pandas DataFrame in uns — a third encoding, read by uns_df
        s = uns_df(lp, "orientation_summary")
        @test s isa DataFrame && nrow(s) == 3
        @test "anisotropy" in names(s) && "MF_full_length" in names(s)
        @test Float64.(s.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test uns_df(lp, "orientation_coords") === nothing      # a plain array is not a dataframe
        @test uns_df(lp, "no_such_key") === nothing
    end
end

# ── The notebook readouts: quiver_df / branch_segments / anisotropy_df ────────────────────────
# These three are the whole point of the anisotropy pass — the arrows, the branch network and
# the per-image scalar, as tidy frames a Pluto notebook can plot directly (docs/NOTEBOOKS.md).
# The fixture is built so a WRONG read fails: the fibre (minor) eigenvector is a pure +x unit
# vector and the major one is +y, so taking the wrong eigenvector rotates every arrow 90°.
@testset "anisotropy notebook readouts" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        # EXPLICIT uids. `@testset` reseeds the global RNG per testset, so `gen_uid()` deals
        # every testset the SAME sequence — two testsets that both create a project+set+image
        # land in the same directory. Harmless until one of them, like this pair, asserts on
        # the directory's CONTENTS.
        proj = create_project!(name="aniso-fixture")
        s = add_set!(proj; name="set-A")
        img = add_image!(s; name="img-a", uid="anisoA")
        dir = img_label_props_dir(img); mkpath(dir)
        rm.(joinpath.(dir, readdir(dir)); force=true)     # a previous run's copies
        cp(h5, img_branch_props_path(img, "SHG"))

        @test img_branch_value_names(img) == ["SHG"]

        # ── arrows ────────────────────────────────────────────────────────────────────────
        q = quiver_df(img; value_name="SHG")
        @test nrow(q) == 3 * 4 * 4                       # every frame, every box
        @test sort(unique(q.t)) == [0, 1, 2]
        # the MINOR eigenvector is (y=0, x=1) ⇒ u=1, v=0. If the reader took the major one
        # instead the arrows would come back (0, 1) — a silent 90° rotation.
        @test all(q.u .≈ 1.0) && all(q.v .≈ 0.0)
        # box centres, and that x/y did not swap: coor[...,1] is y, coor[...,2] is x
        r = only(q[(q.t .== 2) .& (q.iy .== 1) .& (q.ix .== 3), :] |> eachrow)
        @test r.y ≈ 100 * 2 + 10 * 1 + 3
        @test r.x ≈ 1000 + 100 * 2 + 10 * 1 + 3
        # the deliberately-empty box carries its zero length through, so it can be filtered out
        @test only(q[(q.t .== 0) .& (q.iy .== 0) .& (q.ix .== 0), :].length) == 0.0
        @test count(>(0.0), q.length) == 3 * (16 - 1)

        @test nrow(quiver_df(img; value_name="SHG", t=1)) == 16
        @test_throws ErrorException quiver_df(img; value_name="SHG", t=99)
        @test_throws ErrorException quiver_df(img; value_name="nope")

        # ── branch segments ───────────────────────────────────────────────────────────────
        b = branch_segments(img; value_name="SHG")
        @test nrow(b) == 6
        # x from axis 1, y from axis 0 — a swap here would mirror the whole network
        @test b.y1 == [0.0, 10, 20, 30, 40, 50] && b.x1 == [1.0, 11, 21, 31, 41, 51]
        @test b.y2 == [4.0, 14, 24, 34, 44, 54] && b.x2 == [5.0, 15, 25, 35, 45, 55]
        @test b.branch_type == [0, 1, 2, 3, 1, 2]
        @test nrow(branch_segments(img; value_name="SHG", t=1)) == 2

        # ── per-image scalar ──────────────────────────────────────────────────────────────
        a = anisotropy_df(img)
        @test nrow(a) == 3 && unique(a.uID) == [img.uid] && unique(a.value_name) == ["SHG"]
        @test a.t == [0, 1, 2]                           # from orientation_meta.t_index, not position
        @test Float64.(a.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test "occupancy" in names(a) && "branching_act" in names(a)

        # a second branch table on the same image (SHG collagen + a DCs network) — long format,
        # one block per value_name, which is what makes a cross-image comparison filterable
        cp(h5, img_branch_props_path(img, "DCs"); force=true)
        a2 = anisotropy_df(img)
        @test nrow(a2) == 6 && sort(unique(a2.value_name)) == ["DCs", "SHG"]
        @test nrow(anisotropy_df(img; value_name="SHG")) == 3

        # across images — the cohort frame Figure 4 panel D scatters
        img2 = add_image!(s; name="img-b", uid="anisoB")
        mkpath(img_label_props_dir(img2))
        cp(h5, img_branch_props_path(img2, "SHG"); force=true)
        across = anisotropy_df([img, img2]; value_name="SHG")
        @test nrow(across) == 6 && sort(unique(across.uID)) == sort([img.uid, img2.uid])

        # an image with no branch table contributes nothing — never an error, never a zero row
        img3 = add_image!(s; name="img-c", uid="anisoC")
        @test nrow(anisotropy_df(img3)) == 0
        @test nrow(anisotropy_df([img, img3]; value_name="SHG")) == 3
    end
end

# ── Branch value_names are NOT label_props value_names ────────────────────────────────────────
# Branching runs on a SEGMENTATION, which need not have a per-cell measurement table: an SHG
# collagen mask is skeletonised but never measured, so it lives in `labels`/`branch_labels`
# while `label_props` holds only the measured cell segmentations. Enumerating branch pops from
# `label_props` therefore found NOTHING — it looked for B__branch / T__branch and missed the
# SHG__branch that exists, so the branch picker came back empty. One image can carry several
# (SHG + DCs, per behaviourUbiTom3P.Rmd), so this is the plural case.
@testset "branch value_names come from the sidecars" begin
    proj = create_project!(name="bvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-a")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_branch_value_names(img) == String[]          # nothing banked yet
    for f in ("SHG__branch.h5ad", "DCs__branch.h5ad", "B.h5ad", "B__tracks.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __branch sidecars, and NOT the cell/track tables that sit beside them
    @test img_branch_value_names(img) == ["DCs", "SHG"]
    @test img_branch_props_path(img, "SHG") == joinpath(dir, "SHG__branch.h5ad")
end

# The same question for tracks, and the reason it needs its own answer: "is this image tracked" was
# being asked of the RUN LOG (does a `tracking.*` entry exist), which a project migrated from the R
# version answers "no" for while its `{vn}__tracks.h5ad` sits on disk — so the guide picker declared
# "needs a tracked image" over a project whose tracks had already been clustered. The sidecar is the
# state; the run log is only the provenance of runs this app happened to execute.
@testset "track value_names come from the sidecars" begin
    proj = create_project!(name="tvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    # Explicit uid: `@testset` reseeds the RNG, so `gen_uid` would hand this image the SAME uid — and
    # the same directory — as the branch testset above, whose sidecars are already sitting in it
    # (docs/DEV.md → the `@testset` reseeds the global RNG trap; this test hit it on the first run).
    img = add_image!(s; name="img-a", uid="tvnImgA")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_track_value_names(img) == String[]           # nothing banked yet
    for f in ("B__tracks.h5ad", "T__tracks.h5ad", "B.h5ad", "T.h5ad", "SHG__branch.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __tracks sidecars, and NOT the cell/branch tables that sit beside them
    @test img_track_value_names(img) == ["B", "T"]
    @test img_track_props_path(img, "B") == joinpath(dir, "B__tracks.h5ad")
    # and it does NOT consult the run log: no tracking entry was ever written above
    @test isempty(read_run_log(img))
end

@testset "plot groupBy (generic categorical sub-axis)" begin
    # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
    # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                   "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                   "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false, group_by="st")
    @test r["groupBy"] == "st"
    @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
    @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
    byg = Dict(s["group"] => s for s in r["series"])
    @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
    @test byg["2"]["n"] == 3
    # no group_by → single series, empty group label (back-compat)
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

    # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
    # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
    dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                    "uID" => ["x","x","y","y","x","y"],
                    "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                    "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
    rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", collapse_series=true)
    @test length(rc["series"]) == 2
    @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
    @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
    @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
    # collapse with no group_by → one pooled series over everything
    rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, collapse_series=true)
    @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
end

@testset "plot percent (% positive of a 0/1 measure)" begin
    # "% of B cells in contact with a T cell" (`…cell.contact#…`) and "how many T cells are
    # clustered" (`…cell.is.aggregate`) are ONE question: the fraction of a population whose 0/1
    # measure is positive. Both were previously only reachable as a `bar` of the MEAN — an
    # unlabelled 0..1 fraction.

    # ── the detector: a property of the data, not a list of blessed column names ──
    @test Cecelia._is_boolean_measure([0, 1, 1, 0])
    @test Cecelia._is_boolean_measure([0.0, 1.0])
    @test Cecelia._is_boolean_measure([1, missing, NaN, 0])      # missing/non-finite ignored
    @test Cecelia._is_boolean_measure([0, 0, 0])                 # all-negative is still boolean
    @test !Cecelia._is_boolean_measure([0, 1, 2])
    @test !Cecelia._is_boolean_measure([0.0, 0.5, 1.0])
    @test !Cecelia._is_boolean_measure(["a", "b"])               # categorical, not boolean
    @test !Cecelia._is_boolean_measure(Float64[])                # nothing to judge
    @test !Cecelia._is_boolean_measure([missing, NaN])

    # ── Wilson score interval (Wilson 1927), against published values ──
    lo, hi = Cecelia._wilson_ci(5, 10)
    @test isapprox(lo, 0.23659, atol = 1e-4) && isapprox(hi, 0.76341, atol = 1e-4)
    # the case Wald gets WRONG: 0 of 10 → Wald says [0,0] ("certainly never"), Wilson keeps width
    lo0, hi0 = Cecelia._wilson_ci(0, 10)
    @test lo0 == 0.0 && isapprox(hi0, 0.27753, atol = 1e-4)
    # …and at p=1 the interval's exact upper bound IS 1 (float arithmetic lands 1 ulp short)
    lo1, hi1 = Cecelia._wilson_ci(10, 10)
    @test isapprox(lo1, 0.72247, atol = 1e-4) && isapprox(hi1, 1.0)
    @test all(isnan, Cecelia._wilson_ci(0, 0))                   # no data → no interval
    # symmetric about 0.5 (a sanity property of the interval, not of our arithmetic)
    @test isapprox(1 - Cecelia._wilson_ci(3, 10)[2], Cecelia._wilson_ci(7, 10)[1], atol = 1e-12)

    # ── the aggregation ──
    df = DataFrame("value_name" => fill("B", 10), "pop" => fill("/qc", 10),
                   "contact" => [1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
    r = Cecelia._summary_agg(df, "percent"; measure="contact", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false)
    @test r["chartType"] == "percent"
    @test r["valueLabel"] == "% positive"
    @test r["measureBoolean"] === true
    s = only(r["series"])
    @test s["value"] == 30.0 && s["n"] == 10 && s["nPositive"] == 3
    # bounds are the Wilson ones (as percentages) and BRACKET the estimate asymmetrically
    wl, wh = Cecelia._wilson_ci(3, 10)
    @test isapprox(s["lower"], 100wl) && isapprox(s["upper"], 100wh)
    @test s["lower"] < s["value"] < s["upper"]
    @test !isapprox(s["value"] - s["lower"], s["upper"] - s["value"])   # asymmetric — hence 2 bounds
    @test isapprox(s["ci95"], 100 * max(wh - 0.3, 0.3 - wl))            # the wider half-width

    # a percent chart must NOT carry rank/ANOVA comparisons — 0/1 data needs a proportion test
    df2 = vcat(df, DataFrame("value_name" => fill("T", 6), "pop" => fill("/qc", 6),
                             "contact" => [1, 1, 1, 1, 1, 0]))
    r2 = Cecelia._summary_agg(df2, "percent"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false, stats_enabled=true)
    @test !haskey(r2, "comparisons")
    byvn = Dict(s["value_name"] => s for s in r2["series"])
    @test byvn["B"]["value"] == 30.0
    @test isapprox(byvn["T"]["value"], 500 / 6)

    # an all-missing series reports no percentage rather than a spurious 0%
    dfe = DataFrame("value_name" => fill("T", 3), "pop" => fill("/qc", 3),
                    "contact" => [NaN, NaN, NaN])
    se = only(Cecelia._summary_agg(dfe, "percent"; measure="contact", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=false)["series"])
    @test isnan(se["value"]) && se["n"] == 0

    # ── measureBoolean rides along on the ORDINARY charts, so the panel can offer % positive ──
    rb = Cecelia._summary_agg(df, "bar"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rb["measureBoolean"] === true
    rn = Cecelia._summary_agg(DataFrame("value_name" => fill("B", 3), "pop" => fill("/qc", 3),
                                        "dist" => [1.5, 20.0, 3.25]), "bar";
                              measure="dist", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rn["measureBoolean"] === false
    # a POPULATION SUMMARY substitutes a synthetic per-image count; counts of 0/1 are not a boolean
    # MEASURE, and offering "% positive" on them would be nonsense.
    dfp = DataFrame("value_name" => ["B", "T"], "pop" => ["/qc", "/qc"], "uID" => ["i1", "i1"])
    rp = Cecelia._summary_agg(dfp, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test rp["measureBoolean"] === false
end

@testset "plot count (raw + proportion normalize) — population summary" begin
    # two pops in two images; count → raw row counts; normalize=:fraction → each pop's share of
    # its image's plotted total (the population-summary plot). Deterministic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 10),
                   "pop" => ["/p","/p","/p","/q","/q", "/p","/q","/q","/q","/p"],
                   "uID" => ["x","x","x","x","x",       "y","y","y","y","y"])
    # image x: /p=3, /q=2 (total 5); image y: /p=2, /q=3 (total 5)
    bykey(r) = Dict((s["uID"], s["pop"]) => s["value"] for s in r["series"])
    raw = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=true)
    @test raw["chartType"] == "count"
    rk = bykey(raw)
    @test rk[("x","A/p")] == 3.0 && rk[("x","A/q")] == 2.0
    @test rk[("y","A/p")] == 2.0 && rk[("y","A/q")] == 3.0
    prop = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                normalize=:fraction, by_image=true)
    @test prop["normalize"] == "fraction"
    pk = bykey(prop)
    @test pk[("x","A/p")] ≈ 0.6 && pk[("x","A/q")] ≈ 0.4     # 3/5, 2/5
    @test pk[("y","A/p")] ≈ 0.4 && pk[("y","A/q")] ≈ 0.6     # 2/5, 3/5
    @test Dict((s["uID"], s["pop"]) => s["n"] for s in prop["series"])[("x","A/p")] == 3

    # no measure + a DISTRIBUTION chart → each IMAGE is a point (its pop count), grouped by pop:
    # boxplot/beeswarm show within-pop variability and compare pops. A/p counts = [3(x),2(y)],
    # A/q = [2(x),3(y)] → each pop has 2 points (images), median 2.5.
    bx = Cecelia._summary_agg(df, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test bx["chartType"] == "boxplot"
    bybp = Dict(s["pop"] => s for s in bx["series"])
    @test Set(keys(bybp)) == Set(["A/p", "A/q"])
    @test bybp["A/p"]["n"] == 2 && bybp["A/p"]["median"] == 2.5
    # bar over the same per-image counts → mean (A/p mean of [3,2] = 2.5)
    br = Cecelia._summary_agg(df, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test Dict(s["pop"] => s["value"] for s in br["series"])["A/p"] == 2.5

    # A SYNTHETIC METRIC IS NUMERIC BY CONSTRUCTION — never sniffed.
    #
    # `_is_categorical_col` guesses from the values, and a per-image count is a handful of small
    # integers, which its integer-level heuristic reads as CATEGORICAL. The panel then intersected
    # the spec's numeric charts with the categorical set and everything except `count` (kept
    # explicitly as measure-independent) disappeared — "population summary always defaults back to
    # count and you cannot select anything else", on the FIRST render, since the spec's first chart
    # is boxplot. The gate is "did the user name this column", so it covers `proportion` and any
    # later synthetic metric with no new name to remember.
    # backend chart names — the frontend's strip/violin both map onto `points` (backendChart)
    for (ct, nrm) in (("boxplot", :none), ("bar", :none), ("points", :none),
                      ("count", :none), ("bar", :fraction))
        r = Cecelia._summary_agg(df, ct; measure=nothing, granularity=:cell, nbins=10,
                                 normalize=nrm, by_image=true)
        @test r["measureType"] == "numeric"
        @test r["measureBoolean"] === false     # counts that happen to be 0/1 are not a boolean measure
    end
    # counts of exactly 1 are the worst case for the heuristic (a single integer level)
    df1 = DataFrame("value_name" => ["A","A"], "pop" => ["/p","/q"], "uID" => ["x","x"])
    @test Cecelia._summary_agg(df1, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)["measureType"] == "numeric"
    # …and a REAL categorical measure is still detected as categorical (the gate must not blanket
    # everything to numeric)
    dfc = DataFrame("value_name" => fill("A", 4), "pop" => fill("/p", 4),
                    "live.cell.hmm.state.movement" => [1.0, 2.0, 1.0, 2.0])
    @test Cecelia._summary_agg(dfc, "frequency"; measure="live.cell.hmm.state.movement",
                               granularity=:cell, nbins=10, normalize=:none,
                               by_image=false)["measureType"] == "categorical"

    # SPLIT BY POPULATION: two tracked pops (value_names B, T) each with clusters — proportion is
    # normalised WITHIN each value_name per image, not pooled across B+T.
    df2 = DataFrame("value_name" => ["B","B","B","T","T", "B","B","T","T","T"],
                    "pop" => ["/Dir","/Dir","/Mea","/Dir","/Mea", "/Dir","/Mea","/Dir","/Mea","/Mea"],
                    "uID" => ["x","x","x","x","x",              "y","y","y","y","y"])
    pr2 = Cecelia._summary_agg(df2, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:fraction, by_image=true)
    p2 = Dict((s["uID"], s["pop"]) => s["value"] for s in pr2["series"])
    @test p2[("x","B/Dir")] ≈ 2/3 && p2[("x","B/Mea")] ≈ 1/3   # within B (image x: B tot 3)
    @test p2[("x","T/Dir")] ≈ 1/2 && p2[("x","T/Mea")] ≈ 1/2   # within T (image x: T tot 2)
    @test p2[("y","B/Dir")] ≈ 1/2 && p2[("y","T/Mea")] ≈ 2/3   # within B / T (image y)

    # COMPLETE CASES (R tidyr::complete): image y has no /q — it must still contribute a 0 to /q's
    # distribution, not be dropped. Without completion /q would have n=1 (only x) and median 1.
    df3 = DataFrame("value_name" => fill("A", 5),
                    "pop" => ["/p","/p","/q", "/p","/p"],
                    "uID" => ["x","x","x",    "y","y"])          # x: p=2 q=1 ; y: p=2 q=0 (missing)
    bx3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)
    by3 = Dict(s["pop"] => s for s in bx3["series"])
    @test by3["A/q"]["n"] == 2 && by3["A/q"]["median"] == 0.5    # /q points [1(x), 0(y)]
    @test by3["A/p"]["n"] == 2 && by3["A/p"]["median"] == 2.0
    # proportion completes too: /q in image y = 0 / (y's A total 2) = 0 → points [1/3, 0]
    pr3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:fraction, by_image=true)
    by3f = Dict(s["pop"] => s for s in pr3["series"])
    @test by3f["A/q"]["n"] == 2 && by3f["A/q"]["median"] ≈ 1/6
end

@testset "plot raw (per-datapoint export)" begin
    # raw=true → one tidy row per datapoint (identity + value) for re-plotting externally, instead of
    # collapsing to box stats. Deterministic frame with label + a groupBy column; last row NaN measure.
    df = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                   "uID" => ["x","x","y","y","y"], "label" => [1, 2, 3, 4, 5],
                   "m"  => [1.0, 2.0, 3.0, 4.0, NaN],
                   "st" => [1.0, 1.0, 2.0, 2.0, 2.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, group_by="st", raw=true)
    @test r["chartType"] == "raw" && r["measure"] == "m" && r["groupBy"] == "st"
    @test length(r["rows"]) == 4                       # the NaN-measure row is dropped
    row1 = r["rows"][1]
    @test row1["uID"] == "x" && row1["label"] == "1" && row1["value_name"] == "A"
    @test row1["pop"] == "/p" && row1["value"] == 1.0 && row1["group"] == "1"
    @test [rw["value"] for rw in r["rows"]] == [1.0, 2.0, 3.0, 4.0]
    @test [rw["group"] for rw in r["rows"]] == ["1", "1", "2", "2"]

    # measure-less count chart → raw collapses to per-(image, pop) counts (no label column populated)
    dfc = DataFrame("value_name" => fill("A", 5), "pop" => ["/p","/p","/p","/q","/q"],
                    "uID" => ["x","x","x","x","x"])
    rc = Cecelia._summary_agg(dfc, "count"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, raw=true)
    @test rc["chartType"] == "raw" && rc["measure"] == "count"
    cbyp = Dict(rw["pop"] => rw["value"] for rw in rc["rows"])
    @test cbyp["/p"] == 3.0 && cbyp["/q"] == 2.0 && all(!haskey(rw, "label") for rw in rc["rows"])

    # TRACK granularity: `label` duplicates `track_id` in the track table → drop it, keep track_id.
    dft = DataFrame("value_name" => fill("A", 3), "pop" => fill("/_tracked", 3),
                    "uID" => ["x","x","y"], "label" => [10, 11, 12], "track_id" => [10, 11, 12],
                    "m" => [1.0, 2.0, 3.0])
    rt = Cecelia._summary_agg(dft, "boxplot"; measure="m", granularity=:track, nbins=10,
                              normalize=:none, by_image=true, raw=true)
    @test all(!haskey(rw, "label") for rw in rt["rows"])            # no meaningless label
    @test [rw["track_id"] for rw in rt["rows"]] == ["10", "11", "12"]

    # groupBy that ISN'T applied (its column isn't in the frame) → groupBy null + no group column,
    # so the export never carries an empty, misleading category column.
    rna = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, group_by="not_a_column", raw=true)
    @test rna["groupBy"] === nothing && all(!haskey(rw, "group") for rw in rna["rows"])
end

@testset "plot statUnit=image (per-image mean = each dot an image)" begin
    # collapse each image to its mean, then plot those per-image means (n = #images). Deterministic
    # frame: image x cells [1,3,5] (mean 3), image y cells [10,20,30] (mean 20).
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 3.0, 5.0, 10.0, 20.0, 30.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, stat_unit=:image)
    @test length(r["series"]) == 1                     # images pooled into ONE box
    @test r["series"][1]["n"] == 2                      # two datapoints = two images
    @test r["series"][1]["median"] == 11.5 && r["series"][1]["mean"] == 11.5   # of [3, 20]
    # default (individual) + per-image scope → one box PER image, each over its own cells (n = 3);
    # image-mean instead pools those into a single box whose points are the two image means.
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test length(r0["series"]) == 2 && all(s["n"] == 3 for s in r0["series"])
    # bar over per-image means → mean of the image means
    rb = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image)
    @test rb["series"][1]["value"] == 11.5 && rb["series"][1]["n"] == 2

    # with groupBy: per-image mean WITHIN each level → each level's points are its image means.
    df2 = DataFrame("value_name" => fill("A", 8), "pop" => fill("/p", 8),
                    "uID" => ["x","x","y","y","x","x","y","y"],
                    "m"  => [2.0, 4.0, 6.0, 8.0, 20.0, 20.0, 30.0, 10.0],
                    "st" => [1.0, 1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  2.0])
    r2 = Cecelia._summary_agg(df2, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", stat_unit=:image)
    byg = Dict(s["group"] => s for s in r2["series"])
    @test byg["1"]["n"] == 2 && byg["1"]["median"] == 5.0     # st1: x[2,4]→3, y[6,8]→7 → [3,7]
    @test byg["2"]["n"] == 2 && byg["2"]["median"] == 20.0    # st2: x[20,20]→20, y[30,10]→20

    # with attr_map: one series PER ATTRIBUTE value, points = the images in it.
    dfa = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","y","y","z","z"], "m" => [2.0, 4.0, 6.0, 8.0, 100.0, 100.0])
    am = Dict("x" => "ctrl", "y" => "ctrl", "z" => "treat")
    ra = Cecelia._summary_agg(dfa, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, attr_map=am)
    bya = Dict(s["uID"] => s for s in ra["series"])
    @test Set(keys(bya)) == Set(["ctrl", "treat"])
    @test bya["ctrl"]["n"] == 2 && bya["ctrl"]["median"] == 5.0   # images x(3), y(7) → [3,7]
    @test bya["treat"]["n"] == 1                                  # image z only

    # raw export honours it too: rows are the per-image means (label empty, value = the mean)
    rr = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, raw=true)
    @test [rw["value"] for rw in rr["rows"]] == [3.0, 20.0]
    @test all(!haskey(rw, "label") for rw in rr["rows"]) && Set(rw["uID"] for rw in rr["rows"]) == Set(["x","y"])

    # image_agg=:median collapses each image by MEDIAN, not mean — distinguishable on skewed images:
    # x cells [1,2,9] (mean 4, median 2), y [10,20,90] (mean 40, median 20).
    dfs = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 2.0, 9.0, 10.0, 20.0, 90.0])
    rmean = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:mean)
    rmed  = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:median)
    @test rmean["series"][1]["value"] == 22.0   # mean of image means [4, 40]
    @test rmed["series"][1]["value"] == 11.0    # mean of image medians [2, 20]
    rmedraw = Cecelia._summary_agg(dfs, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, stat_unit=:image, image_agg=:median, raw=true)
    @test [rw["value"] for rw in rmedraw["rows"]] == [2.0, 20.0]   # per-image medians
end

@testset "plot matrix (heatmap: profile + crosstab)" begin
    # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
    # frame into one grid (no series). Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                   "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                   "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
    pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test pr["matrixMode"] == "profile"
    @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
    cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
    @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
    @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
    @test cell(pr, "2", "speed")["n"] == 2
    @test isempty(pr["series"])

    # z-score standardises each row across its levels (mean 0) — the comparable "signature"
    prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, zscore=true,
                               matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
    zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
    @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

    # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
    # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
    dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                    "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
    ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="crosstab", category="tr")
    @test ct["matrixMode"] == "crosstab"
    @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
    ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
    @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
    # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
    ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false,
                               matrix_mode="crosstab", category="tr", matrix_normalize=:row)
    @test ctr["valueLabel"] == "P(to|from)"
    ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
    @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
    @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

    # error cases: unknown mode, missing category, profile with no present measure column
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

    # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
    dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                    "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
    prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, matrix_mode="profile",
                               measures=["speed"], category="st")
    c2 = first(c for c in prn["cells"] if c["x"] == "2")
    @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell

    # EMPTY FRAME (a population with no rows in this image) → an empty GRID, never an error. pop_df
    # returns a frame with no columns at all, which used to trip the category check and print
    # "matrix needs a `category` column present in the data" into the panel — the failure a per-image
    # board of cluster pops hits whenever one cluster is absent from one image. Every other chart type
    # answers this with an empty series; the response keys must match the populated ones so the
    # renderer's empty state fires instead of breaking.
    for (mode, extra) in (("crosstab", "normalize"), ("profile", "zscore"))
        e = Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                 normalize=:none, by_image=false, matrix_mode=mode,
                                 measures=["speed"], category="tr")
        @test e["chartType"] == "matrix" && e["matrixMode"] == mode && e["category"] == "tr"
        @test isempty(e["cells"]) && isempty(e["xLabels"]) && isempty(e["yLabels"])
        @test haskey(e, extra) && haskey(e, "valueLabel")   # same shape as the populated response
    end
    # …but an unknown mode is still an error, empty frame or not
    @test_throws ErrorException Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing,
        granularity=:cell, nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="tr")
end

@testset "plot attribute grouping (compare by attr)" begin
    # group cross-image series by an image attribute: images sharing a value pool into one series
    # labelled by the value; an image with no value falls back to its uID. No fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x1","x1","x2","x2","y1","y1"],
                   "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
    amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
    r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                             normalize=:none, by_image=true, attr_map=amap)
    @test length(r["series"]) == 2
    @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
    byu = Dict(s["uID"] => s for s in r["series"])
    @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
    # image missing the attribute → falls back to its uID
    r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
    @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
    # no attr_map → group by image as before (3 images)
    r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true)
    @test length(r3["series"]) == 3
end

# ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
@testset "plot_summary_data cross-image" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data cross-image (fixture missing)"
    else
        # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
        mk = function (uid)
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid=uid, dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img
        end
        imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

        # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
        one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
        both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
        @test "uID" in names(both)
        @test Set(unique(both.uID)) == Set(uids)
        @test nrow(both) == 2 * nrow(one)

        # boxplot per_image → one box per image (same data → equal stats), + scope field
        sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
        @test length(sp["series"]) == 2
        @test Set(s["uID"] for s in sp["series"]) == Set(uids)
        @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
        @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
        @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
        @test sp["series"][1]["n"] == nrow(one)

        # summarised → one pooled box across both images
        ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:summarised)
        @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

        # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
        br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test br["chartType"] == "bar" && length(br["series"]) == 2
        @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
        @test br["series"][1]["value"] ≈ br["series"][2]["value"]

        # histogram per_image → 2 overlay series sharing bin edges
        hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                               measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
        @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
        @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
    end
end

# ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
@testset "plot_summary_data multi-segmentation targets" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data multi-segmentation (fixture missing)"
    else
        # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
        # targets path: a (value_name, pop) per series, vcat across segmentations.
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
        img = CciaImage(uid="uM", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
        img.label_props["_active"] = "B"

        targets = [("B", "/_tracked"), ("B2", "/_tracked")]
        # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
        bp = plot_summary_data(img, "live", targets, "boxplot";
                               measure="live.track.speed", granularity=:track)
        @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
        @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
        @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
        @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

        # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
        img2 = CciaImage(uid="uN", dir=td)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
        img2.label_props["_active"] = "B"
        xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test xp["scope"] == "per_image" && length(xp["series"]) == 4
        @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
              Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
    end
end

# ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
@testset "plot_summary_data helpers" begin
    @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
    @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
    let edges = Cecelia._hist_edges([0.0, 10.0], 10)
        @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
    end
    @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
    @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
    @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
    # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
    @test derived_pop_paths("live") == ["/_tracked"]
    @test isempty(derived_pop_paths("flow"))
end

# ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
@testset "track_props (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "track_props (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # mock a categorical per-cell column to exercise the freq branch
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        label_props(img_label_props_path(img, "B")) |>
            add_obs(DataFrame("label" => cells.label,
                                      "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

        tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test nrow(tp) == ntr                                  # one row per track
        @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
        @test tp.label == tp.track_id                          # engine membership key
        # numeric aggregates present
        @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
        # categorical → per-category frequency columns
        @test "st.1" in names(tp) && "st.2" in names(tp)
        # motility joined from the track table
        @test "live.track.speed" in names(tp)
        # num_cells totals the tracked cells
        @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

        # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
        # off the decoded type + values: strings and integer code sets → categorical; continuous
        # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
        @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
        @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
        @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
        @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
        @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
        @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
        # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
        @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
        # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
        auto = track_props(img; value_name="B", cell_measures=["st"])
        @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
        # `numeric` escape-hatch forces it back to numeric aggregates when desired
        forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
        @test "st.mean" in names(forced) && !("st.1" in names(forced))

        # An UNTRACKED segmentation → the empty, well-formed table, and SILENTLY. `track_props`
        # handles this case by design, so it must ASK (`is_tracked`, which reads the obs column list
        # only) instead of selecting `track_id` and inspecting the result: `select_cols` @warns about
        # every column it cannot find, so the by-design path logged
        # `LabelProps: ignoring unknown columns ["track_id"]` once per request — six per page load of
        # a track-grained plot panel, every one of them about a column we already knew might be absent.
        td2 = mktempdir(); mkpath(joinpath(td2, "labelProps"))
        cp(h5, joinpath(td2, "labelProps", "B.h5ad"))
        img2 = CciaImage(uid="KDIeEm", dir=td2)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["_active"] = "B"
        label_props(img_label_props_path(img2, "B")) |> drop_obs(["track_id"]) |> save!
        @test !is_tracked(img2; value_name="B")
        untracked = @test_logs min_level=Logging.Warn track_props(img2; value_name="B", cell_measures=["area"])
        @test nrow(untracked) == 0
        @test Set(names(untracked)) == Set(["track_id", "num_cells", "label"])
    end
end

# ── track_cell_measures: derive base cell measures from track-property column names ──
@testset "track_cell_measures" begin
    mot = ["live.track.speed", "live.track.meanTurningAngle"]
    # motility axes need no cell aggregation
    @test isempty(track_cell_measures(["live.track.speed"], mot))
    # numeric aggregate columns → their base cell measure (suffix stripped)
    @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
          ["mean_intensity_0", "area"]
    # categorical frequency column `{base}.{cat}` → base
    @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
    # bookkeeping + motility skipped; dedup across aggregates of the same base
    @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                               "area.mean", "area.sd"], mot) == ["area"]
end

# ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
@testset "pop_df pop_type=track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df pop_type=track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # motility-only track props (the common track-gating case: no cell_measures needed)
        tp = track_props(img; value_name="B")
        @test "live.track.speed" in names(tp)
        spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
        thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
        truth = count(>=(thr), spd)
        @test 0 < truth < length(spd)

        # a TRACK gate (one point per track) on the speed axis, stored under __tracks
        m = PopulationMap(pop_type="track", value_name="B")
        add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                               thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
        @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

        # granularity=:track → gated track rows, one point per track, gate genuinely applied
        g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
        @test nrow(g) == truth
        @test length(unique(g.track_id)) == nrow(g)
        @test unique(g.pop) == ["/fast"]
        @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

        # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
        gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
        @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
        @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
        @test nrow(gc) > nrow(g)                                 # many cells per track
        @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))

        # granularity=:cell + `centroids`: the member cells' COORDINATES come too. They are what the
        # track PLOTS draw — a gated or clustered track's path — and the expansion used to carry only
        # `pop_cols`, so the frame came back with no coordinates at all and `_pop_df_finish` could do
        # nothing but warn about it. Resolved per value_name (a 2D segmentation has no centroid_z).
        gcc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell, centroids=:pixel)
        @test all(c -> c in names(gcc), ["centroid_x", "centroid_y", "centroid_t"])
        @test nrow(gcc) == nrow(gc)
        @test Set(unique(gcc.track_id)) == Set(Int.(g.track_id))
    end
end

@testset "HMM states + transitions" begin
    # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
    # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
    # `missing` (per-cell states are undefined where a measurement can't exist).
    uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
    for img in ("X", "Y"), k in 1:3, t in 1:25
        slow = t <= 12
        s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
        a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
        if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
        push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
        push!(sp, s); push!(an, a)
    end
    df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                   "live.cell.speed" => sp, "live.cell.angle" => an)

    st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
    @test length(st) == nrow(df)

    # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
    # Vector{String}); fit must not MethodError on the normalise/scale step.
    let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                             scale_measures=Union{}[], normalise=Dict{String,String}())
        @test count(!ismissing, stu) == count(!ismissing, st)
    end
    @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
    @test Set(skipmissing(st)) == Set([1, 2])
    df[!, "live.cell.hmm.state.default"] = st

    one = st[(df.uID .== "X") .& (df.track_id .== 1)]
    @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
    decoded = collect(skipmissing(one))
    @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

    tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                         include_start=false, include_self=true)
    @test length(tr) == nrow(df)
    nonmiss = collect(skipmissing(tr))
    @test all(occursin("_", x) for x in nonmiss)
    @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
    @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

    trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                          include_start=false, include_self=false)
    nm2 = Set(skipmissing(trn))
    @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

    # cross-model hybrid: two state columns paste into "a.b" before transitions
    df[!, "live.cell.hmm.state.second"] = st
    trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                          time_col="t", include_start=false, include_self=true)
    @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

    # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
    # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
    # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
    # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
          ["A/_tracked", "B/_tracked"]
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
    @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
          Set(["A", "B", "C"])

    # task registration + set-scope routing
    @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
    @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
    @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
    @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
    @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
    @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
end

# The two entry points into hmm.jl expect a `pop_df`-shaped DataFrame (uID / value_name / track_id
# / time_col / measures-or-state-cols). A rename upstream — the audit called this out for `pop_df`
# too — used to fail late with a bare `KeyError`; `_require_cols` names every missing column at once
# so a report reads "missing [t]" not "column name :t not found". The second block pins the
# read-boundary normalisation for `hmm_transitions`: state columns may arrive as Int (fresh fit) or
# String (categorical obs read-back), and both must yield the same "1" / "1.2" hybrid strings.
@testset "HMM entry guards + transition state normalisation" begin
    # A minimal well-formed DataFrame — we build DROP variants by column-subscript, because the
    # test suite only imports DataFrames.DataFrame + nrow (no `select`/`Not`).
    good = DataFrame("uID" => ["A"], "value_name" => ["V"], "track_id" => [1], "t" => [1.0],
                     "live.cell.speed" => [0.5])
    drop(df, col) = df[:, filter(!=(col), names(df))]

    # hmm_fit_states — every missing column is named in one shot (not one at a time on retry).
    err = try
        hmm_fit_states(drop(good, "live.cell.speed"), ["live.cell.speed"];
                       num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException
    @test occursin("hmm_fit_states", err.msg) && occursin("live.cell.speed", err.msg)

    err = try
        hmm_fit_states(drop(good, "t"), ["live.cell.speed"]; num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("t", err.msg)

    # hmm_transitions — same guard, same shape.
    st_df = copy(good)
    st_df[!, "state"] = [1]
    err = try
        hmm_transitions(drop(st_df, "uID"), ["state"]; time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("hmm_transitions", err.msg) && occursin("uID", err.msg)

    # Int and String state columns yield IDENTICAL transitions — normalisation at the read boundary
    # is what makes the composite (which reads back as categorical String) match a fresh in-memory
    # fit (Int). The float column proves the Int(round(v)) branch survives too.
    base = DataFrame("uID" => fill("A", 4), "value_name" => fill("V", 4),
                     "track_id" => fill(1, 4), "t" => [1.0, 2.0, 3.0, 4.0])
    int_df = copy(base);   int_df[!, "state"]   = [1, 1, 2, 2]
    str_df = copy(base);   str_df[!, "state"]   = ["1", "1", "2", "2"]
    flt_df = copy(base);   flt_df[!, "state"]   = [1.0, 1.0, 2.0, 2.0]
    tr_int = hmm_transitions(int_df, ["state"]; time_col="t", include_self=true)
    tr_str = hmm_transitions(str_df, ["state"]; time_col="t", include_self=true)
    tr_flt = hmm_transitions(flt_df, ["state"]; time_col="t", include_self=true)
    # First cell has no prev — always missing; the rest agree exactly across all three arms.
    @test isequal(tr_int, tr_str) && isequal(tr_int, tr_flt)
    @test collect(skipmissing(tr_int)) == ["1_1", "1_2", "2_2"]

    # Missing / NaN / "" all collapse to missing hybrid (and therefore missing transition), same
    # behaviour whether the state col is numeric or a string.
    for (col, tag) in ((Union{Int,Missing}[1, missing, 2, 2],   "Int+missing"),
                       (Union{Float64,Missing}[1.0, NaN, 2.0, 2.0], "Float+NaN"),
                       (["1", "", "2", "2"],                       "String+empty"))
        d = copy(base); d[!, "state"] = col
        tr = hmm_transitions(d, ["state"]; time_col="t", include_self=true)
        @test ismissing(tr[2])                                     # the gap breaks the chain
        @test collect(skipmissing(tr)) == ["2_2"]                  # only 3→4 survives ($tag)
    end

    # Hybrid column pastes with "." — Float second col rounds to "1"/"2", not "1.0"/"2.0".
    hyb = copy(base)
    hyb[!, "a"] = [1, 1, 2, 2]
    hyb[!, "b"] = [1.0, 1.0, 2.0, 2.0]
    tr_h = collect(skipmissing(hmm_transitions(hyb, ["a", "b"]; time_col="t", include_self=true)))
    @test tr_h == ["1.1_1.1", "1.1_2.2", "2.2_2.2"]
end

include(joinpath(@__DIR__, "suite", "ome_qc.jl"))

# ── Every directory whose params a USER actually sees ─────────────────────────────────────────
#
# All three copy testsets below walk THIS list. They each used to hardcode `src/tasks`, which
# exempted the custom-module examples — 7 tips, every one breaking the no-trailing-period rule,
# in the very file people COPY to write a drop-in module. Those specs are loaded by
# `load_custom_modules!` and rendered by the same `ParamRenderer`, so they are task specs that
# happen to live in `docs/`. Missing directories are skipped, so a trimmed checkout is fine.
#
# `app/src/plotDefinitions/` is deliberately NOT here, and the distinction is worth keeping
# straight: those files have a `params` array of the same SHAPE, but it is a defaults bag, not a
# form. Its only consumer is `SummaryPanel.vue` —
#     `props.spec.params?.find(p => p.key === k)?.default ?? d`
# — which reads `default` and nothing else. A `label` or `tip` there renders to nobody, so
# requiring one would have produced nine strings that look maintained and reach no user. The
# controls a user really operates for those plots are hand-rolled in the SFC, and the frontend
# ratchet already covers them. (The top-level `spec.label` IS rendered, in the plot picker, and
# is unchecked — a small, separate gap; don't fix it by dragging the whole directory in here.)
spec_dirs() = filter(isdir, [
    joinpath(dirname(dirname(pathof(Cecelia))), "src", "tasks"),
    joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "custom-modules"),
    # Plugin examples are shipped, installable task specs rendered by the same `ParamRenderer` — and
    # they are what a plugin author COPIES, so a mistake here propagates. They were outside every
    # copy ratchet until plugins existed.
    joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "plugins"),
])
# Walk every spec, yielding (label-for-messages, parsed spec) so a failure names a findable file.
# The label carries the containing directory — `tasks/x.json` vs `plotDefinitions/x.json` — because
# base names collide across surfaces (`track_measures.json` exists in both).
function each_spec(visit)
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        # `plotDefinitions/` is excluded STRUCTURALLY, not by leaving one path off `spec_dirs`. Its
        # files carry a `params` array of the same shape, but it is a defaults bag read only for
        # `default` — a `tip` there renders to nobody (see the note above `spec_dirs`). The
        # positional version held only while every such directory happened to sit outside the listed
        # roots; adding `docs/examples/plugins`, which contains one, immediately broke it.
        basename(root) == "plotDefinitions" && continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        visit(joinpath(basename(root), fname), spec)
    end
end

# ── UI copy budget: task-spec `tip` fields ────────────────────────────────────────────────────
#
# The enforceable half of `docs/ui/COPY.md`, for the surface Julia owns.
# A `tip` renders as a tooltip on the task form, so it carries the same bar as any other tooltip:
# one line, under 90 characters, no second sentence explaining itself. This lives here rather than
# in the frontend suite because task specs are backend files and the frontend never holds a copy.
#
# 56 of 175 tips had drifted past the budget (worst: 332 chars, three sentences on a form field)
# before this existed. An exact allow-list, not a count — a count silently permits swapping one
# violation for another. Before adding an entry, check whether the fact belongs in a `docs/` file:
# that was true of every tip the sweep shortened.
# ── Numeric param RANGES have to be plausible ─────────────────────────────────────────────────
#
# `min`/`max` are enforced (`_validate_leaf`) and rendered as the slider's travel, so a bound that
# was never thought about does two things: it makes the useful part of the slider a few pixels
# wide, and it lets one drag start a run nobody wants. Several were plainly copy-pasted — a cell
# **surface distance** and a "min cells" count both ran to **1000** (a cell is ~10 µm, so 1000 µm
# is 100 cell diameters; an aggregate of 1000 cells is an organ), and `nPermutations` reached
# 100 000, hours of compute one drag away.
#
# The check is a RATIO, not a table of blessed numbers: a table would just restate the JSON and
# would need editing every time a default legitimately moves. `max / default` is the tell for a
# bound nobody chose — a sane range puts the default somewhere you can reach, so a max fifty times
# the default means the default is pinned to the far left of the travel. The worst honest ratio in
# the tree is 20 (`minCells` 5→100), so 50 leaves real headroom while catching every case above.
@testset "numeric param ranges are plausible" begin
    RATIO_MAX = 50
    # A param whose range genuinely spans orders of magnitude. Empty on purpose: before adding one,
    # check that the DEFAULT isn't the thing that's wrong.
    ALLOWED_WIDE = String[]

    nums = Tuple[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            String(something(spec_get(p, "type"), "")) in ("int", "float") || return
            push!(nums, (f, String(something(spec_get(p, "key"), "?")),
                         spec_get(p, "min"), spec_get(p, "max"),
                         spec_get(p, "step"), spec_get(p, "default")))
        end
    end
    @test length(nums) > 20                      # the walk found the numeric params

    # structural sanity first — these are bugs, not judgement calls
    for (f, k, mn, mx, _, def) in nums
        mn === nothing && continue
        if mx !== nothing
            @test mn <= mx || "$f/$k: min $mn > max $mx" == ""
        end
        if def !== nothing && def isa Real
            @test def >= mn                                   || "$f/$k: default $def < min $mn" == ""
            @test mx === nothing || def <= mx                 || "$f/$k: default $def > max $mx" == ""
        end
    end

    # a step coarser than the whole range means the slider has one position
    coarse = ["$f/$k: step $st over range $mn..$mx" for (f, k, mn, mx, st, _) in nums
              if st !== nothing && mn !== nothing && mx !== nothing && st > (mx - mn)]
    @test isempty(coarse)

    # …then the judgement call, as a loose bound
    wide = ["$f/$k: max $mx is $(round(mx / def, digits = 1))× the default $def"
            for (f, k, _, mx, _, def) in nums
            if mx !== nothing && def isa Real && def > 0 && mx / def > RATIO_MAX &&
               !("$f/$k" in ALLOWED_WIDE)]
    @test isempty(wide)
end

@testset "task spec tips stay short" begin
    COPY_MAX = 90
    ALLOWED = String[]

    # `tip`s nest inside `section`/`group` params, so recurse.


    # A trailing dot here is an abbreviation, not a sentence end ("e.g. HMM state").
    ABBREV = r"(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no)\.$"i
    function multi_sentence(s)
        for m in eachmatch(r"\S*\.\s+(?=[A-Z(])", s)
            occursin(ABBREV, rstrip(m.match)) || return true
        end
        false
    end

    tips = Tuple{String,String}[]
    nspecs = 0
    each_spec() do f, spec
        nspecs += 1
        each_spec_param(spec_get(spec, "params")) do p, _
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips, (f, join(split(String(t)), " ")))
            # `tips: [{text, requires?}]` — image-dependent variants of `tip`. Each entry's text is a
            # tip in its own right and gets the same length + house-style check.
            ts = spec_get(p, "tips")
            if ts isa AbstractVector
                for entry in ts
                    entry isa AbstractDict || continue
                    et = spec_get(entry, "text")
                    et isa AbstractString && push!(tips, (f, join(split(String(et)), " ")))
                end
            end
        end
    end

    @test nspecs > 20                       # the walk found the specs
    @test length(tips) > 100                # ...and their tips

    too_long = ["$f: [$(length(t))] $t" for (f, t) in tips
                if length(t) > COPY_MAX && !(t in ALLOWED)]
    @test isempty(too_long)

    two_sentence = ["$f: $t" for (f, t) in tips if multi_sentence(t) && !(t in ALLOWED)]
    @test isempty(two_sentence)
end

# ── UI copy COVERAGE: every task param carries a `tip` ────────────────────────────────────────
#
# The testset above polices tips that EXIST. This one polices the ones that don't. `docs/UI.md`
# asks for CellProfiler-style tip DENSITY — every setting explains itself on hover — and until
# this existed nothing could see a gap: `branching.json` shipped **twelve** parameters with no
# tip at all, so the form read "Flatten Z" / "Pre-dilation" / "Anisotropy box size (px)" with no
# way to find out what any of them did short of reading the Python runner.
#
# Presence is the half a machine can decide; whether a tip is the RIGHT tip stays a review
# question, exactly as the length ratchet can't tell you a short line is a good line. The
# frontend half of this rule — settable controls with no `v-tooltip` — is `uncoveredControls`
# in `frontend/src/utils/uiCopy.ts`, checked in `uiCopy.test.ts`.
#
# SECTIONS AND GROUPS ARE EXEMPT. They are container headers ("Advanced", "Filters"), not inputs
# — a user can't set them to anything, and requiring one would buy 18 tips saying "advanced
# options". Their CHILDREN are checked like any other param.
@testset "a handler fallback never contradicts its spec default" begin
    # `run_task` now applies the spec's `default` before calling `_run_task`, so a handler's own
    # `get(params, "k", d)` fallback is unreachable for any declared param — dead weight, not a second
    # answer. It stops being harmless the moment the two DISAGREE, because then the form promises one
    # number and a REPL/chain/MCP run uses another. Five did, and each was a real production
    # divergence: clustTracks.minTracklength 1 vs 5, opticalFlow.trainRatio 1.0 vs 0.8,
    # coastal.labelSmoothing 0.0 vs 0.5, contactsMeshes.maxContactDist 10.0 vs 5,
    # track_measures.forceRecompute false vs true.
    #
    # Deliberately a TEXT scan of the handler sources: the alternative is running every task. Only
    # literal scalars are compared — a computed fallback is a different thing and is skipped.
    root = dirname(dirname(pathof(Cecelia)))
    norm(x) = x isa AbstractString ? strip(String(x), ['"']) :
              x isa Bool ? string(x) :
              x isa Number ? string(float(x)) : nothing
    bad = String[]
    for (rootdir, _, files) in walkdir(joinpath(root, "src", "tasks")), f in files
        endswith(f, ".json") || continue
        spec = try JSON3.read(read(joinpath(rootdir, f), String), Dict{String,Any}) catch; continue end
        haskey(spec, "params") || continue
        jl = joinpath(rootdir, replace(f, ".json" => ".jl"))
        isfile(jl) || continue
        src = read(jl, String)
        defaults = Dict{String,Any}()
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            # A per-param `requires.axes` gate can DROP the key from the effective run
            # (`_apply_param_requires`), so the handler's fallback here is deliberately the "off"
            # value rather than the visible spec default — the guard IS the reason they differ.
            # Same reason `showIf`-only params are already exempt (their fallback is a state the
            # form no longer names). Skip these keys from the equality check.
            if !haskey(q, "requires")
                haskey(q, "default") && (defaults[string(get(q, "key", ""))] = q["default"])
            end
            walk(get(q, "params", nothing))
        end
        walk(spec["params"])
        for (key, dflt) in defaults
            want = norm(dflt)
            isnothing(want) && continue                      # arrays/objects: not a literal fallback
            for m in eachmatch(Regex("get\\(params, \"$(key)\", ([^)]+)\\)"), src)
                got_raw = strip(m.captures[1])
                got = occursin(r"^[-0-9.]+$", got_raw) ? string(parse(Float64, got_raw)) :
                      got_raw in ("true", "false") ? got_raw :
                      startswith(got_raw, "\"") ? strip(got_raw, ['"']) : nothing
                isnothing(got) && continue                   # computed fallback — a different thing
                got == want || push!(bad, "$(basename(jl)): $key — handler $got_raw, spec $(dflt)")
            end
        end
    end
    @test isempty(bad) || (@info "handler fallback contradicts the spec default" bad; false)
end

@testset "every task spec field is declared and documented" begin
    # Spec fields drift in BOTH directions, so this checks both.
    #
    #   forward  — a field is added to `ParamDef` and rendered, and `docs/MODULES.md` never hears of
    #              it. The reference silently stops being the reference.
    #   backward — a spec declares a field no consumer reads. `clustPops/cluster.json` carried
    #              `"includeChannels": true` for a `labelPropsColsSelection`; nothing in the form
    #              read it, in Julia or in TS (the only match was `napariOverlays.ts`, an unrelated
    #              movie-overlay concept). A spec that declares something nobody reads is a lie about
    #              the form, and it reads as intent to whoever copies the spec next.
    #
    # "Read by a consumer" is either half of the contract: declared on the frontend's `ParamDef`, or
    # read by Julia — `hideInComposite` is server-only and legitimately absent from `ParamDef`.
    root      = dirname(dirname(dirname(pathof(Cecelia))))
    types_ts  = read(joinpath(root, "frontend", "src", "tasks", "types.ts"), String)
    modules   = read(joinpath(root, "docs", "MODULES.md"), String)
    # Both `tasks/task.jl` and `api/src/routes.jl` are small aggregators now — their structural
    # spec-field references (`"composite"`, `"steps"`, `"scope"`, …) live in split fragments under
    # `tasks/task/*.jl` and `api/src/routes/*.jl` respectively.
    task_dir   = joinpath(root, "app", "src", "tasks", "task")
    routes_dir = joinpath(root, "api", "src", "routes")
    julia_src = join([read(f, String) for f in vcat(
                        joinpath(root, "app", "src", "tasks", "task.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(task_dir;   join=true)),
                        joinpath(root, "api", "src", "routes.jl"),
                        filter(f -> endswith(f, ".jl"), readdir(routes_dir; join=true)))], "\n")

    # Structural keys of the params array itself, not fields a spec author sets on a param.
    STRUCTURAL = Set(["key", "label", "type", "default", "\$include"])
    fields = Set{String}()
    each_spec() do _, spec
        walk(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            union!(fields, string.(keys(q)))
            walk(get(q, "params", nothing))
        end
        walk(get(spec, "params", nothing))
    end

    undeclared = String[]; undocumented = String[]
    for f in sort(collect(setdiff(fields, STRUCTURAL)))
        occursin(Regex("^\\s*$(f)\\??:", "m"), types_ts) ||
            occursin("\"$f\"", julia_src) || push!(undeclared, f)
        occursin("`$f`", modules) || push!(undocumented, f)
    end
    @test isempty(undeclared)   || (@info "spec field read by nothing" undeclared; false)
    @test isempty(undocumented) || (@info "spec field absent from docs/MODULES.md" undocumented; false)
end

@testset "optionsFrom fills a picker from a named source" begin
    # Three tasks each carried twenty lines of identical dict-walking to do this — cellpose, coastal
    # and opticalFlow.train — differing only in which lister they called. The point for plugins: a
    # plugin author ships JSON and a task .jl, so offering a model vault used to mean writing a Julia
    # hook. Resolved for every task now, before the dispatch hook.
    flat(spec) = begin
        out = Dict{String,Any}()
        go(ps) = ps isa AbstractVector && for q in ps
            q isa AbstractDict || continue
            out[string(get(q, "key", ""))] = q
            go(get(q, "params", nothing))
        end
        go(get(spec, "params", nothing)); out
    end

    cp = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.cellpose")))["model"]
    @test cp["optionsFrom"] == "cellposeModels"
    @test !isempty(cp["options"])                                    # the built-ins are always there
    @test Set(String(m.name) for m in Cecelia.list_cellpose_models()) ==
          Set(String(o["value"]) for o in cp["options"])

    # Coastal APPENDS the vault to the literal options its spec declares, so "None" stays first and
    # stays selectable. The vault is empty until the user trains something, and an empty state should
    # be a legible choice — not a select that rejects everything including its own default.
    co = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("segment.coastal")))["model"]
    @test co["options"][1]["value"] == "" && co["options"][1]["label"] == "None"
    @test length(co["options"]) == 1 + length(Cecelia.list_coastal_models())

    # value == label here: the user types the stem, so the suggestion IS what goes in the field.
    tr = flat(Cecelia._task_spec(Cecelia._task_from_fun_name("opticalFlow.train")))["modelName"]
    @test all(o -> o["label"] == o["value"], tr["options"])

    # …and every picker filled this way lists each value ONCE. The append is what makes coastal's
    # "None" work, and it is also what duplicated cellpose's built-ins: the spec declared `cpsam_v2`
    # and `cpsam` as literals while `cellposeModels` enumerates the same tuple, so the Model select
    # showed both twice. Neither the `issubset` check above nor
    # a `Set ==` comparison can see a duplicate — both collapse them — which is why it shipped.
    for (fn, key) in (("segment.cellpose", "model"), ("segment.coastal", "model"),
                      ("opticalFlow.train", "modelName"))
        vals = [string(o["value"])
                for o in flat(Cecelia._task_spec(Cecelia._task_from_fun_name(fn)))[key]["options"]]
        @test length(vals) == length(unique(vals))
    end

    # A declared option that the source ALSO enumerates keeps the SPEC's wording and position: the
    # label is the author's, and order is what keeps a "None" first.
    dup = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "cellposeModels",
        "options" => Any[Dict{String,Any}("value" => "cpsam_v2", "label" => "Mine")])])
    Cecelia._apply_options_from!(dup)
    opts = dup["params"][1]["options"]
    @test count(o -> string(o["value"]) == "cpsam_v2", opts) == 1
    @test string(first(opts)["label"]) == "Mine"

    # An unregistered name leaves the declared options alone rather than emptying the picker — a
    # typo in a spec must not silently produce a control nobody can choose anything in.
    spec = Dict{String,Any}("params" => Any[Dict{String,Any}(
        "key" => "k", "type" => "select", "optionsFrom" => "nope",
        "options" => Any[Dict{String,Any}("value" => "a", "label" => "A")])])
    Cecelia._apply_options_from!(spec)
    @test [o["value"] for o in spec["params"][1]["options"]] == ["a"]
end

@testset "showIf conditions name a param that exists" begin
    # `showIf` is the DECLARATIVE half of "this param does not apply here": a condition on the form,
    # beside the param it is about, so a plugin author never writes Julia to make a field disappear.
    # (The other half — a condition needing a file read, like "this XML export has no columns" —
    # cannot be a spec field and stays a server hook setting `hidden`.)
    #
    # Its one silent failure mode: name a key that is not in the spec and the condition can never be
    # satisfied, so the param is hidden FOREVER with no error anywhere. A typo costs a whole control.
    bad = String[]
    each_spec() do label, spec
        present = Set{String}()
        conds   = Tuple{String,String}[]
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                push!(present, string(get(q, "key", "")))
                cond = get(q, "showIf", nothing)
                cond isa AbstractDict &&
                    for k in keys(cond); push!(conds, (string(get(q, "key", "?")), string(k))); end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
        # Sub-params of a section are stored FLAT in the value dict, so a condition may cross that
        # boundary in either direction — which is why membership is checked against the whole spec.
        for (owner, k) in conds
            k ∈ present || push!(bad, "$label: $owner showIf → '$k', which no param declares")
        end
    end
    @test isempty(bad) || (@info "showIf names a param that does not exist" bad; false)
end

@testset "a param that says segmentation reads SEGMENTATIONS" begin
    # `valueNameSelection` defaults to `filepaths` — image VERSIONS — when `field` is omitted, and
    # that default is right for the seven built-ins that omit it ("Image to segment", "Images to
    # train on"). It is silent, though: the importer's "Segmentation" picker offered smoothed,
    # afCorrected, driftCorrected — image versions with segmentation's label on them, and nothing
    # anywhere said so. Three example specs had the same bug, in the files people copy.
    #
    # The rule is narrow on purpose: only a param whose own label or tip CALLS ITSELF a segmentation
    # or a label set must read `labels`. "Image to segment" is not a claim about the picker's
    # contents, so it stays exempt.
    bad = String[]
    each_spec() do label, spec
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                if get(q, "type", "") == "valueNameSelection"
                    txt = lowercase(string(get(q, "label", ""), " ", get(q, "tip", "")))
                    claims = occursin("segmentation", txt) || occursin("label set", txt)
                    claims && get(q, "field", "") != "labels" &&
                        push!(bad, "$label → $(get(q, "key", "?")) (field=$(get(q, "field", "omitted")))")
                end
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) || (@info "valueNameSelection claims a segmentation but reads image versions" bad; false)
end

@testset "a picker gates on `labels` only when the task needs the MASK" begin
    # The mirror of the testset above, and the one that actually cost a workflow. `labels` and
    # `label_props` are two INDEPENDENT ccid.json registries: mask pixels vs a measurement table. A
    # directly-imported track set registers only the second — there are no mask pixels to register —
    # so `field: "labels"` silently drops exactly the sets `ccia-importTracks` creates.
    #
    # Every track-CONSUMING task reads the h5ad and nothing else, and all three gated on `labels`
    # anyway: `tracking.track_measures`, `tracking.correct`, and the plugin's
    # `trackTools.cumulativeChange` (which is where spotted it, from the word "Segmentation"
    # on a form that wanted tracks). You could import tracks and then not measure them, with nothing
    # saying why the set was missing from the picker.
    #
    # So the rule is what the HANDLER does, not what the label says: if neither the task's `.jl` nor
    # its `_run.py` reaches mask pixels, its picker must not gate on `labels`.
    MASK_ACCESS = r"img_labels_path|img\.labels|labelsPath|open_labels|labels_path|zarr"

    # Gates on `labels`, reads no mask, and that is DELIBERATE — with the reason, because a bare
    # exemption list is how a real hit hides.
    #
    # `tracking.bayesian_tracking` reads centroids out of the h5ad and writes lineage columns back,
    # so it touches no mask either. It stays on `labels` because it PRODUCES tracks rather than
    # consuming them: every trackable set today comes from a segmentation (the importer only writes
    # sets that are already tracked), so `labels` is not currently narrower than the truth. Revisit
    # if anything ever registers untracked detections without a mask.
    ALLOWED = Set(["bayesian_tracking.json"])

    bad = String[]
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        basename(root) == "plotDefinitions" && continue
        fname ∈ ALLOWED && continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        base = splitext(fname)[1]
        src  = join([isfile(joinpath(root, base * e)) ? read(joinpath(root, base * e), String) : ""
                     for e in (".jl", "_run.py")], "\n")
        occursin(MASK_ACCESS, src) && continue      # genuinely needs the mask
        function walk(ps)
            ps isa AbstractVector || return
            for q in ps
                q isa AbstractDict || continue
                get(q, "type", "") == "valueNameSelection" && get(q, "field", "") == "labels" &&
                    push!(bad, "$(joinpath(basename(root), fname)) → $(get(q, "key", "?"))")
                walk(get(q, "params", nothing))
            end
        end
        walk(get(spec, "params", nothing))
    end
    @test isempty(bad) ||
        (@info "picker gates on `labels` but the task never reads a mask — an imported " *
               "points-only set can never be picked; use `labelPropsNames`" bad; false)
end

@testset "every task param carries a tip" begin
    CONTAINER = ("section", "group")
    # A param whose label genuinely IS the whole explanation. Empty on purpose — same reason as
    # the length ratchet's: an allow-list that starts populated never gets emptied. Before adding
    # one, try writing the tip; it is nearly always shorter than the argument for skipping it.
    ALLOWED_NO_TIP = String[]

    # Collects every SETTABLE param (container children included), flagged tipped or not, so the
    # guard below can assert the walk actually found something — a silently empty walk would
    # otherwise report perfect coverage, which is how the QC scraper once lost 40 strings.


    params = Tuple{String,String,Bool}[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            ptype = String(something(spec_get(p, "type"), ""))
            (haskey(p, :key) || haskey(p, "key")) && !(ptype in CONTAINER) || return
            tip = spec_get(p, "tip")
            tipped = !isempty(strip(tip isa AbstractString ? String(tip) : ""))
            # `tips: [{text, requires?}]` — image-dependent variants. Any entry with a non-empty
            # `text` covers the requirement; the renderer picks the first that matches, and a
            # deliberate no-match (a T-only tip on a still) is the honest empty state.
            if !tipped
                tips = spec_get(p, "tips")
                if tips isa AbstractVector
                    for entry in tips
                        entry isa AbstractDict || continue
                        txt = spec_get(entry, "text")
                        if txt isa AbstractString && !isempty(strip(String(txt)))
                            tipped = true; break
                        end
                    end
                end
            end
            push!(params, (f, String(something(spec_get(p, "key"), "?")), tipped))
        end
    end

    @test length(params) > 150              # the walk found the params it is meant to police

    missing_tips = ["$f: $k" for (f, k, tipped) in params
                    if !tipped && !("$f: $k" in ALLOWED_NO_TIP)]
    @test isempty(missing_tips)
end

# ── UI copy house style: task-spec `label` + `tip` ────────────────────────────────────────────
#
# The Julia half of `docs/UI.md` → *House style*, mirroring the frontend checks in
# `frontend/src/utils/uiCopy.test.ts`. Split for the same reason the `tip` budget is: task specs
# are backend files and the frontend never holds a copy of one.
#
# This is the surface that actually drifted. Nothing could see the whole corpus at once, so the
# two halves of the app diverged along the storage boundary — 14 task labels went Title Case
# ("Bayesian Tracking", "Drift Correction") while every frontend label stayed sentence case, and
# all 164 tips grew a trailing period that no tooltip in the frontend had. `pixi run ui-copy`
# found it; this keeps it found. Exact allow-lists, not counts.
@testset "task spec copy follows the house style" begin
    ALLOWED_TITLE_CASE = String[]      # a label that is really a proper name
    ALLOWED_TRAILING_PERIOD = String[] # a `tip` that is genuinely a sentence

    # `@testset` bodies are their own scope, so the collector above isn't visible here — this one
    # pulls both keys in a single walk rather than re-deriving two nearly identical recursions.


    # Mirrors `isTitleCase` in uiCopy.ts — see there for why the allowances exist. A capital is
    # only evidence of Title Case when the word isn't expected to carry one: acronyms, single
    # letters, known proper nouns, and the first word after a separator ("Spatial / Time").
    PROPER = r"^(?:Cellpose|Bayesian|Dask|Cecelia|Leiden|Python|Julia|ImageJ|Fiji|OME|Napari|Zarr|Pluto|Rscript)$"
    SEPARATOR = r"^[/+&–—|]+$"
    expected_cap(w) = occursin(r"^[A-Z0-9+&/–-]+$", w) || length(w) == 1 || occursin(PROPER, w)
    function title_case(text)
        words = [w for w in split(text) if occursin(r"^[A-Za-z]", w) || occursin(SEPARATOR, w)]
        length(words) < 2 && return false
        judged = [(w = words[i], after_sep = occursin(SEPARATOR, words[i - 1]))
                  for i in 2:length(words) if !occursin(SEPARATOR, words[i])]
        isempty(judged) && return false
        any(j -> occursin(r"^[A-Z]", j.w) && !j.after_sep && !expected_cap(j.w), judged) &&
            all(j -> occursin(r"^[A-Z]", j.w) || j.after_sep || expected_cap(j.w), judged)
    end

    labels, tips2 = Tuple{String,String}[], Tuple{String,String}[]
    each_spec() do f, spec
        l = get(spec, :label, nothing)
        l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
        each_spec_param(spec_get(spec, "params")) do p, _
            l = spec_get(p, "label")
            l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips2, (f, join(split(String(t)), " ")))
        end
    end

    @test length(labels) > 150              # the walk found task + param labels

    titled = ["$f: $l" for (f, l) in labels if title_case(l) && !(l in ALLOWED_TITLE_CASE)]
    @test isempty(titled)

    # `…`/`...` is a continuation, not a sentence end.
    dotted = ["$f: $t" for (f, t) in tips2
              if occursin(r"[^.]\.$", t) && !(t in ALLOWED_TRAILING_PERIOD)]
    @test isempty(dotted)

    # Only the words with a decided winner. Create/Add, Delete/Remove and Run/Start are NOT
    # synonyms (see the vocabulary table in docs/UI.md) and are deliberately absent.
    BANNED = ["Choose" => "Select", "Pick" => "Select", "Display" => "Show",
              "Execute" => "Run", "Modify" => "Edit", "Discard" => "Remove"]
    wrong_verb = ["$f: \"$s\" — use $good" for (f, s) in vcat(labels, tips2)
                  for (bad, good) in BANNED if occursin(Regex("\\b$bad\\b", "i"), s)]
    @test isempty(wrong_verb)
end

# ── Stats module (docs/todo/STATS_ANNOTATIONS_PLAN.md) ─────────────────────
#
# Pins the glue between `run_stats` and HypothesisTests.jl. We don't test the underlying
# test math (that's HypothesisTests' own suite) — we test that the API is wired correctly:
# test dispatch, insertion order preserved (via Vector{Pair}), pairwise Bonferroni-adjusted,
# ns/star ladder, error handling for empty and too-few groups.
@testset "run_stats" begin
    # Two clearly-different groups → mannwhitney by default; p is very small; significance
    # ladder is at least ** (matches STATS_ANNOTATIONS_PLAN.md → S0-1).
    @testset "2 groups auto → mannwhitney" begin
        r = Cecelia.run_stats(["WT" => [1.0,2,3,4,5], "KO" => [10.0,11,12,13,14]])
        @test r.test == :mannwhitney
        @test r.groups == ["WT", "KO"]
        @test r.n == [5, 5]
        @test r.means[1] ≈ 3.0 && r.means[2] ≈ 12.0
        @test r.medians[1] ≈ 3.0 && r.medians[2] ≈ 12.0
        @test r.p_value < 0.05
        @test r.significance in ("*", "**", "***", "****")
        @test occursin("Mann-Whitney", r.method_note)
        @test isempty(r.comparison_pairs)   # omnibus IS the pair for 2 groups
    end

    # `auto` also has to say WHY. The UI showed the resolved test name and nothing else, so a user
    # had no way to know the basis — and deriving the explanation in the frontend would fork the
    # rule (change `_auto_test` and the tooltip would quietly keep claiming the old basis).
    @testset "auto states its basis; a NAMED test states none" begin
        two = Cecelia.run_stats(["WT" => [1.0,2,3], "KO" => [9.0,10,11]])
        @test occursin("2 groups", two.auto_reason)
        @test occursin("Mann-Whitney", two.auto_reason)
        three = Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11], "C" => [20.0,21,22]])
        @test occursin("3 groups", three.auto_reason)
        @test occursin("Kruskal-Wallis", three.auto_reason)
        # both auto choices are rank-based — that's the reassurance the note has to carry, since
        # `auto` never runs a normality check
        @test occursin("rank-based", two.auto_reason) && occursin("rank-based", three.auto_reason)
        # nothing was chosen for the user, so there is nothing to explain
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest).auto_reason)
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:mannwhitney).auto_reason)
        # the reason must name the test that actually ran — one rule, not two
        for n in (2, 3, 7)
            @test occursin(n == 2 ? "Mann-Whitney" : "Kruskal-Wallis", Cecelia._auto_reason(n))
            @test occursin("$(n) groups", Cecelia._auto_reason(n))
        end
        # …and it reaches the wire under `autoReason`
        d = Cecelia._stats_result_dict(two)
        @test d["autoReason"] == two.auto_reason
        @test isempty(Cecelia._stats_result_dict(
            Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest))["autoReason"])
    end

    # Two identical groups → p ≈ 1, "ns".
    @testset "identical groups → ns" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4,5], "B" => [1.0,2,3,4,5]])
        @test r.p_value > 0.9
        @test r.significance == "ns"
    end

    # Welch's t-test opt-in — different method note, still small p on separated data.
    @testset "ttest opt-in" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4], "B" => [10.0,11,12,13]]; test=:ttest)
        @test r.test == :ttest
        @test occursin("t-test", r.method_note)
        @test r.p_value < 0.05
    end

    # Three groups → kruskal by default, pairs are populated with Bonferroni-adjusted values.
    @testset "3 groups → kruskal + pairwise" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [10.0,11,12,13,14], "C" => [20.0,21,22,23,24]])
        @test r.test == :kruskal
        @test r.groups == ["A", "B", "C"]
        @test occursin("Kruskal-Wallis", r.method_note)
        @test length(r.comparison_pairs) == 3   # (A,B), (A,C), (B,C)
        for (a, b, p_adj, sig) in r.comparison_pairs
            @test p_adj >= 0.0 && p_adj <= 1.0
            @test sig in ("ns", "*", "**", "***", "****")
        end
        # A vs C is the widest gap → definitely significant post-Bonferroni.
        ac = only(p for (a, b, p, _) in r.comparison_pairs if a == "A" && b == "C")
        @test ac < 0.05
    end

    # ANOVA opt-in with 3 groups.
    @testset "anova opt-in (3 groups)" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [5.0,6,7,8,9], "C" => [10.0,11,12,13,14]];
            test=:anova)
        @test r.test == :anova
        @test occursin("ANOVA", r.method_note)
        @test r.p_value < 0.05
    end

    # Insertion order preserved (Vector of Pairs guarantees it — this asserts we don't sort).
    @testset "group order preserved" begin
        r = Cecelia.run_stats(["Z" => [1.0,2,3], "A" => [4.0,5,6], "M" => [7.0,8,9]])
        @test r.groups == ["Z", "A", "M"]
    end

    # Error paths.
    @testset "errors" begin
        @test_throws ArgumentError Cecelia.run_stats(["only" => [1.0,2,3]])
        @test_throws ArgumentError Cecelia.run_stats(["A" => Float64[], "B" => [1.0,2]])
        # 2-group tests refuse when given ≠2 groups.
        three = ["A" => [1.0,2], "B" => [3.0,4], "C" => [5.0,6]]
        @test_throws ArgumentError Cecelia.run_stats(three; test=:ttest)
        @test_throws ArgumentError Cecelia.run_stats(three; test=:mannwhitney)
        @test_throws ArgumentError Cecelia.run_stats(["A"=>[1.0,2], "B"=>[3.0,4]];
                                                    test=:notarealtest)
    end
end

# ── Segmentation testset ────────────────────────────────────────────────
# One big testset for algorithm-agnostic segmentation.jl + the live_outputs trait — plus the
# out-of-testset `_BadLiveTask` struct declaration it depends on. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are still in
# scope for the segmentation fragment (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "segmentation.jl"))

# ── chipSelect param type ────────────────────────────────────────────────────
# "1,2,4,8" was a raw text field, which is a parse error waiting to happen and reads as unfinished.
# The values are validated per element like a `select`, because they reach a runner that can only
# fail much later and much less clearly — a bad temporal scale corrupts the model's channel layout.
@testset "chipSelect validation" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    scales = only(p for p in spec["params"] if get(p, "key", "") == "temporalScales")
    @test scales["type"] == "chipSelect"
    @test [string(o["value"]) for o in scales["options"]] == ["1", "2", "3", "4", "6", "8", "12", "16"]

    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "2", "8"])) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "5"]))       # 5 is not offered
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => "1,2,4,8"))        # a string is no longer the shape

    # …and the runner-side parser still takes what the chips produce
    @test parse_temporal_scales(["1", "2", "8"]) == [1, 2, 8]
end


# ── the intensity loss is a dial, not a switch ───────────────────────────────
# This testset previously asserted the opposite: that `intensityWeight` defaulted to 0 and was no
# longer offered. That change was made on coastal's docstring ("prefer ConfettiForegroundLoss", with a
# measurement of the intensity target ALONE: 2535 components, median 3 px) plus the observation that
# recent models all used 0. An audit refuted the inference:
#
#   • The controlled pair was already on disk. memTom (intensity 1.0) and flow.small (intensity 0.0)
#     share image, channel, seed, epochs and frame count — and memTom reaches a LOWER foreground loss
#     (0.32507 vs 0.32747). The retained objective is fit no better without the term, so "it degrades
#     the head it supervises" does not hold.
#   • A sweep put peak IoU at 0.25 (0.622) with 0.0 at 0.606 and 1.0 at 0.523: 1.0 over-claims
#     (precision 54%) and 0.0 under-claims (recall 70%, circularity 16% above the raw signal).
#   • cecelia pins `confetti: 0.0`, so `foreground` is coastal's `ForegroundLoss` — brightness-only,
#     explicitly "the colour-blind form". The single-channel worry that shipped with the change named
#     `ConfettiForegroundLoss` and did not apply.
#
# So the default is the measured middle and the control is back. Pinned because "the form offers it"
# is the part that had no measurement behind it either way, and removing it also made
# `paramsFromManifest` drop it silently — a model trained at 1.0 handed back a form that would not
# reproduce it.
# ── the flow-boundary term and the metrics it is built from ──────────────────
# `foregroundBoundaryWeight` reached coastal as its default 0.0 because nothing passed it, and per
# `ForegroundLoss.target` it is "the ONLY path by which optical flow reaches the labels" — everywhere
# else flow enters as an input channel or through the contrastive term. Now plumbed.
#
# The trap it ships with: `coastal.loss.flow_discontinuity` builds the signal from |strain| +
# |vorticity| + |divergence|, and cecelia's DEFAULT metric set drops two of those three
# (`FLAT_FLOW_METRICS` — they are flat as input channels, which is a different question from whether
# their gradient marks a boundary). `flow_discontinuity` sums whichever are present and normalises, so
# a partial set trains against a weaker signal and says nothing. Refused rather than warned about,
# because a warning is read after the hour of training.
@testset "flow boundary weight requires the metrics it is built from" begin
    @test Set(Cecelia.FLOW_BOUNDARY_METRICS) == Set(["strain", "vorticity", "divergence"])
    # the collision is real and worth asserting, so a future change to either set surfaces it here
    @test !isempty(intersect(Set(Cecelia.FLOW_BOUNDARY_METRICS), Set(Cecelia.FLAT_FLOW_METRICS)))

    # off → nothing is required, whatever the metric set
    @test isempty(Cecelia.flow_boundary_missing(nothing, 0.0))
    @test isempty(Cecelia.flow_boundary_missing(["strain"], 0.0))

    # on with the shipped default → the two dropped ones are named
    @test Set(Cecelia.flow_boundary_missing(nothing, 0.5)) == Set(["vorticity", "divergence"])
    # on with all three ticked → nothing missing
    all_three = ["strain", "vorticity", "divergence", "acceleration"]
    @test isempty(Cecelia.flow_boundary_missing(all_three, 0.5))

    # …and the validator turns that into a submit-time error rather than a weak model
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5,
                         "flowMetrics" => all_three)) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))

    # the message has to NAME them — "check your metrics" costs a round trip to the docs
    err = try
        validate_params(TrainFlowModel(),
            Dict{String,Any}("foregroundBoundaryWeight" => 0.5, "flowMetrics" => ["strain"]))
        nothing
    catch e; e end
    @test occursin("vorticity", err.msg) && occursin("divergence", err.msg)

    # the control is offered, and OFF by default: switching it on also requires re-ticking metrics,
    # so it cannot be a silent default
    spec = Cecelia._task_spec(TrainFlowModel())
    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    fb = only(p for p in flat(spec["params"]) if get(p, "key", "") == "foregroundBoundaryWeight")
    @test fb["default"] == 0.0
end

@testset "intensity loss is an offered dial at the measured default" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    keys_of(ps) = reduce(vcat, [haskey(p, "params") ? keys_of(p["params"]) : [get(p, "key", "")]
                                for p in ps]; init = String[])
    @test "intensityWeight" ∈ keys_of(spec["params"])
    @test "foregroundWeight" ∈ keys_of(spec["params"])

    flat(ps) = reduce(vcat, [haskey(p, "params") ? flat(p["params"]) : [p] for p in ps]; init = [])
    iw = only(p for p in flat(spec["params"]) if get(p, "key", "") == "intensityWeight")
    # NOT 0.0 (the refuted default) and NOT 1.0 (over-claims); the sweep's peak.
    @test iw["default"] == 0.25
    @test iw["min"] == 0.0        # 0 stays REACHABLE — the experiment must remain runnable
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 0.0)) === nothing
    @test validate_params(TrainFlowModel(), Dict{String,Any}("intensityWeight" => 1.0)) === nothing
end

@testset "OME-ZARR metadata reads v2 and v3 alike" begin
    # `read_ome_metadata` feeds ccid.json `meta`, which docs/OBJECTMODEL.md → *Calibration* makes authoritative
    # for every physical number in the app. NGFF 0.5 nests attributes under `ome`; a reader that misses
    # that returns an EMPTY Dict, and the caller then has no PhysicalSize/TimeIncrement at all — which
    # downstream becomes 1.0 rather than an error. So the two formats are asserted to agree, against two
    # committed stores of the same real pixels. See test-data/README.md, docs/todo/ZARR_V3_PLAN.md.
    v2 = fixture_path("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = fixture_path("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(have_fixture(v2) && have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        # the series wrapper is found structurally in BOTH formats (v2 `.zattrs`, v3 `zarr.json`)
        @test Cecelia.series_base(v2) == joinpath(v2, "0")
        @test Cecelia.series_base(v3) == joinpath(v3, "0")

        # the one resolver: attributes come back unwrapped regardless of the `ome` nesting
        for p in (v2, v3)
            attrs = ngff_attrs(joinpath(p, "0"))
            @test !isnothing(attrs)
            @test haskey(attrs, :multiscales)          # NOT nested under :ome by the time we see it
            ms = ngff_multiscales(joinpath(p, "0"))
            @test !isnothing(ms) && !isempty(ms)
        end
        # a directory with no zarr metadata answers nothing rather than throwing
        @test isnothing(ngff_attrs(joinpath(v2, "does-not-exist")))
        # array metadata resolves for both; a GROUP dir must NOT be mistaken for an array (v3 shares
        # the filename `zarr.json` between the two)
        @test !isnothing(zarr_array_meta(joinpath(v3, "0", "0")))
        @test isnothing(zarr_array_meta(joinpath(v3, "0")))

        m2 = read_ome_metadata(v2)
        m3 = read_ome_metadata(v3)
        @test !isempty(m2) && !isempty(m3)
        for k in ("SizeC", "SizeT", "SizeZ")
            @test m2[k] == m3[k]
        end
        @test (m2["SizeC"], m2["SizeT"], m2["SizeZ"]) == (4, 3, 3)

        # Calibration — the whole reason these fixtures are real. Deliberately not 1.0, so a correct
        # read is distinguishable from the "unknown" fallback.
        for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "TimeIncrement")
            @test haskey(m2, k) && haskey(m3, k)
            @test isapprox(m2[k], m3[k]; rtol = 1e-9)
        end
        @test isapprox(m2["PhysicalSizeX"], 0.5964274525755702; rtol = 1e-6)
        @test !isapprox(m2["PhysicalSizeX"], 1.0; atol = 1e-6)    # not the silent fallback
        @test isapprox(m2["PhysicalSizeZ"], 3.0; rtol = 1e-6)
        @test isapprox(m2["TimeIncrement"], 30.0; rtol = 1e-6)
    end
end

@testset "bioformats2raw chunk flags" begin
    # These flags were the bug: `chunkSizeX`/`chunkSizeY` existed in omezarr.json and were read by
    # NOTHING — no tile flag ever reached the CLI, so a user who chose 512 still got bioformats2raw's
    # 1024. One `chunkSize` param now, and it is passed.
    @test Cecelia.bf2raw_chunk_flags("512") == ["--tile-width", "512", "--tile-height", "512"]
    @test Cecelia.bf2raw_chunk_flags(1024)  == ["--tile-width", "1024", "--tile-height", "1024"]

    # "auto" passes NOTHING on purpose: bioformats2raw's own default is 1024 ALREADY CAPPED to the
    # frame, which is exactly the rule we want (one chunk per plane, up to 1024) and needs no source
    # dimensions — which we do not have, since the image is not converted yet.
    @test isempty(Cecelia.bf2raw_chunk_flags("auto"))
    @test isempty(Cecelia.bf2raw_chunk_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_chunk_flags(""))

    # unparseable / absurd falls back to auto rather than raising — same call as the compression
    # flags: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_chunk_flags("banana"))
    @test isempty(Cecelia.bf2raw_chunk_flags(0))
    @test isempty(Cecelia.bf2raw_chunk_flags(-8))
    @test isempty(Cecelia.bf2raw_chunk_flags(16))       # below 32: not a sane chunk

    # every option the task spec offers must actually resolve (a spec/handler drift here is silent —
    # the import would just ignore the choice, which is the bug this whole testset exists for)
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    cs   = only(filter(p -> get(p, :key, "") == "chunkSize", collect(adv.params)))
    vals = [string(get(o, :value, o)) for o in cs.options]
    @test "auto" in vals
    @test string(cs.default) in vals
    for v in vals
        @test v == "auto" ? isempty(Cecelia.bf2raw_chunk_flags(v)) :
                            Cecelia.bf2raw_chunk_flags(v) == ["--tile-width", v, "--tile-height", v]
    end

    # and the tips must not merely restate the label — that is what made these params guesswork
    for p in vcat(collect(spec.params), collect(adv.params))
        get(p, :type, "") == "section" && continue
        tip = String(get(p, :tip, ""))
        @test !isempty(tip)
        @test lowercase(tip) != lowercase(String(get(p, :label, "")))
    end
end

@testset "bioformats2raw worker + heap flags" begin
    # Same spec/handler drift shape as bf2raw_chunk_flags — an option surfaced in the JSON that no
    # code translates is silent (import runs at the wrong worker count). Measured 2026-08-27 on
    # `Human_Lymph_Node_Manual_IBEX.ims`: workers=4 (bf2raw default) → 105 OOMs and 0-3 chunks;
    # workers=2 + -Xmx16g → 2 OOMs and 3820 chunks; workers=1 → zero OOMs. That's why Imaris auto = 1.
    @test Cecelia.bf2raw_worker_flags("1") == ["--max-workers=1"]
    @test Cecelia.bf2raw_worker_flags(2)   == ["--max-workers=2"]
    @test Cecelia.bf2raw_worker_flags(8)   == ["--max-workers=8"]

    # "auto" and unparseable both defer to bioformats2raw's own default (4) — same forgiving-fallback
    # rule as chunk/compression: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_worker_flags("auto"))
    @test isempty(Cecelia.bf2raw_worker_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_worker_flags(""))
    @test isempty(Cecelia.bf2raw_worker_flags("banana"))
    @test isempty(Cecelia.bf2raw_worker_flags(0))
    @test isempty(Cecelia.bf2raw_worker_flags(-2))

    # Extension-keyed defaults — Imaris (`.ims`) is the reader we know decompresses fat HDF5 chunks.
    # Everything else stays on "auto" (bf2raw picks 4).
    @test Cecelia.bf2raw_default_workers("/some/path/thing.ims") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/THING.IMS") == "1"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.tif") == "auto"
    @test Cecelia.bf2raw_default_workers("/some/path/thing.czi") == "auto"
    @test Cecelia.bf2raw_default_workers("")                       == "auto"

    # JVM heap parsing mirrors the workers pattern
    @test Cecelia.bf2raw_java_heap_gib("16") == 16
    @test Cecelia.bf2raw_java_heap_gib(24)   == 24
    @test Cecelia.bf2raw_java_heap_gib("auto") == 0
    @test Cecelia.bf2raw_java_heap_gib("") == 0
    @test Cecelia.bf2raw_java_heap_gib("banana") == 0
    @test Cecelia.bf2raw_java_heap_gib(0) == 0
    @test Cecelia.bf2raw_java_heap_gib(-4) == 0

    # Extension-keyed heap default — Imaris gets headroom, everything else defers to the JVM. Cap at
    # half the box RAM so a fixed literal can't wedge a small machine into swap (measured off
    # Sys.total_memory at the call site, not a hardcoded number).
    ram_gib = max(1, floor(Int, Sys.total_memory() / (1024^3)))
    exp_ims = min(16, floor(Int, ram_gib / 2))
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.ims") == exp_ims
    @test Cecelia.bf2raw_default_heap_gib("/some/path/thing.tif") == 0
    @test Cecelia.bf2raw_default_heap_gib("")                       == 0

    # Env dict shape — `heap_gib > 0` sets BIOFORMATS2RAW_OPTS with -Xmx, otherwise empty (JVM default).
    # A pre-existing value in ENV is preserved (prepended to keep our flag winning) — feedback: never
    # clobber a user-set env var. This side-tests that path without mutating the real ENV.
    empty_env = Cecelia.bf2raw_java_env(0)
    @test isempty(empty_env)
    heap_env = Cecelia.bf2raw_java_env(16)
    @test heap_env["BIOFORMATS2RAW_OPTS"] == "-Xmx16g" ||
          startswith(heap_env["BIOFORMATS2RAW_OPTS"], "-Xmx16g ")

    # every worker option in the task spec must actually resolve
    spec2 = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv2  = only(filter(p -> get(p, :type, "") == "section", collect(spec2.params)))
    mw    = only(filter(p -> get(p, :key, "") == "maxWorkers", collect(adv2.params)))
    for o in mw.options
        v = string(get(o, :value, o))
        @test v == "auto" ? isempty(Cecelia.bf2raw_worker_flags(v)) :
                            Cecelia.bf2raw_worker_flags(v) == ["--max-workers=$v"]
    end
    hp = only(filter(p -> get(p, :key, "") == "jvmHeapGiB", collect(adv2.params)))
    for o in hp.options
        v = string(get(o, :value, o))
        n = Cecelia.bf2raw_java_heap_gib(v)
        @test v == "auto" ? n == 0 : n == parse(Int, v)
    end
end

@testset "bioformats2raw format flags" begin
    # The import is the ONLY place the store format is chosen; derived stores inherit it
    # (docs/todo/ZARR_V3_PLAN.md D9).
    ff(args...; kw...) = Cecelia.bf2raw_format_flags(args...; kw...)

    @test isempty(ff("0.4", "auto"))                       # default = the command we always ran
    @test ff("0.5", "auto") == ["--ngff-version", "0.5"]
    @test ff("0.5", "1024") ==
          ["--ngff-version", "0.5", "--shard-width", "1024", "--shard-height", "1024"]

    # Sharding is NGFF 0.5 only, and is dropped for 0.4 rather than raising: they are separate controls
    # and switching the version back must still produce a working import.
    @test isempty(ff("0.4", "1024"))

    # unparseable / absurd falls back to upstream's default rather than raising
    for bad in ("banana", "0", "-8", "16", "")
        @test ff("0.5", bad) == ["--ngff-version", "0.5"]
    end

    # ── chunk-key separator ──────────────────────────────────────────────────────
    # `--no-nested` IS NEVER EMITTED (2026-08-14). Flat keys saved ~5% on a real movie at identical read
    # time, but produce a store that conforms to no published NGFF version — nested storage is what 0.2
    # introduced, so flat keys are 0.1 storage under the 0.4-shaped metadata written beside them. The
    # separator is therefore no longer a parameter of this function at all, which also retires the old
    # flat+0.5 conflict (that pair silently wrote zarr v2) by making it unrepresentable.
    for v in ("0.4", "0.5"), sh in ("auto", "1024")
        @test !("--no-nested" in ff(v, sh))
    end
    @test_throws MethodError Cecelia.bf2raw_format_flags("0.4", "auto"; separator = "flat")

    # ── shard depth ──────────────────────────────────────────────────────────────
    # The ONLY axis that reduces the file count on a 512x512 frame — width/height cap to the frame, so
    # the shard equals the chunk and packs nothing (measured: depth 13 -> 13 files vs 109).
    @test ff("0.5", "auto"; shard_depth = "13") == ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 13) ==
          ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "1") == ["--ngff-version", "0.5"]      # the default: no flag
    # "all" with no usable z count drops the flag rather than guessing a depth
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 0) == ["--ngff-version", "0.5"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 1) == ["--ngff-version", "0.5"]
    # depth is NGFF 0.5 only, like the rest of sharding
    @test isempty(ff("0.4", "auto"; shard_depth = "13"))

    # Every option the spec offers must resolve, and there must be NO option claiming to disable
    # sharding: --shard-width cannot be turned off, so bioformats2raw shards every v3 store (verified
    # against 0.12.1 — a 0.5 import with no shard flag still produces a sharding_indexed codec), and an
    # "off" option would be a lie.
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    # `chunkSeparator` is NOT in this list any more: it was a declared param that the importer never
    # read — no `--no-nested`, no `dimension_separator`, nothing — and its default `"flat"`
    # contradicted `CHUNK_SEPARATOR_DEFAULT = "nested"`, which `config.jl` notes is the only separator
    # still offered. A control that reaches nothing is worse than an absent one: it reads as a choice.
    for key in ("ngffVersion", "shardSize", "shardDepth")
        prm  = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        vals = [string(get(o, :value, o)) for o in prm.options]
        # `ngffVersion` takes its default from the Settings store layout (`defaultFrom`), so this
        # reads the RESOLVED spec — a raw file read would see the pre-resolution literal.
        @test string(prm.default) in vals
        @test !isempty(String(get(prm, :tip, "")))
    end
    shard = only(filter(p -> get(p, :key, "") == "shardSize", collect(adv.params)))
    @test !any(lowercase(string(get(o, :value, o))) in ("none", "off", "0") for o in shard.options)

    # Transparency: someone who knows zarr must be able to map each control onto what lands on disk, so
    # every one of these tips names its bioformats2raw flag or the metadata key it sets.
    for key in ("chunkSize", "ngffVersion", "shardSize", "shardDepth")
        prm = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        tip = String(get(prm, :tip, ""))
        @test occursin("--", tip) || occursin("_", tip)   # a CLI flag or a zarr metadata key
    end
end

@testset "OME-TIFF export carries the calibration" begin
    # The task exists because the OLD route (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter)
    # lost the pixel sizes: a plain TIFF has nowhere to record Z spacing, so the converter guessed the
    # voxel size. Every assertion below is about the calibration surviving — that IS the feature.

    meta = Dict{String,Any}("PhysicalSizeX" => 0.325, "PhysicalSizeY" => 0.325,
                            "PhysicalSizeZ" => 2.0,   "PhysicalSizeUnit" => "µm",
                            "TimeIncrement" => 10.0,  "TimeIncrementUnit" => "s")

    cal = Cecelia._export_calibration(meta)
    @test cal["PhysicalSizeZ"] == 2.0                     # the field the old workflow dropped
    @test cal["PhysicalSizeZUnit"] == "µm"

    # UNITS MUST BE THE OME SYMBOL, not the NGFF/UDUNITS name ccid.json stores. OME's UnitsLength and
    # UnitsTime are ENUMERATIONS; "micrometer" is not a member, so one such attribute makes <Pixels>
    # schema-invalid and Bio-Formats discards the ENTIRE OME block and falls back to counting IFDs —
    # a 31x4x32 movie then opens as 3968 timepoints, one channel, no names, no voxel size. Verified
    # against real Bio-Formats (bioformats2raw): "µm" reads back in full, "micrometer" reads nothing.
    ngff = Dict{String,Any}("PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33,
                            "PhysicalSizeZ" => 2.0,  "PhysicalSizeUnit" => "micrometer",
                            "TimeIncrement" => 15.0, "TimeIncrementUnit" => "second")
    ncal = Cecelia._export_calibration(ngff)
    for k in ("PhysicalSizeXUnit", "PhysicalSizeYUnit", "PhysicalSizeZUnit")
        @test ncal[k] == "µm"
    end
    @test ncal["TimeIncrementUnit"] == "s"
    # An unknown unit passes through rather than being guessed at — same rule as the converter.
    @test Cecelia._export_calibration(
        Dict{String,Any}("PhysicalSizeX" => 1.0, "PhysicalSizeUnit" => "furlong")
    )["PhysicalSizeXUnit"] == "furlong"
    @test cal["PhysicalSizeX"] == 0.325 && cal["PhysicalSizeXUnit"] == "µm"
    @test cal["TimeIncrement"] == 10.0 && cal["TimeIncrementUnit"] == "s"

    # A Z-MIP has no z extent left and a single frame has no interval — writing either would state a
    # geometry the file doesn't have.
    @test !haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeZ")
    @test haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeX")
    @test !haskey(Cecelia._export_calibration(meta; one_frame = true), "TimeIncrement")

    # Unknown must stay unknown. Defaulting an absent/zero/garbage size to 1.0 would tell Imaris the
    # pixel is one micron, which is a claim, not a fallback.
    for bad in (Dict{String,Any}(), Dict{String,Any}("PhysicalSizeX" => ""),
                Dict{String,Any}("PhysicalSizeX" => 0.0), Dict{String,Any}("PhysicalSizeX" => "abc"))
        @test !haskey(Cecelia._export_calibration(bad), "PhysicalSizeX")
    end

    # …and that absence is exactly what QC flags, since the write itself always "succeeds".
    codes(f) = [x["code"] for x in f]
    @test isempty(Cecelia._export_qc_findings(cal, 21))     # fully calibrated → nothing to say
    @test isempty(Cecelia._export_qc_findings(cal, 1))
    # A 2D image legitimately has no Z spacing — don't cry wolf on SizeZ == 1.
    @test isempty(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 1))
    @test "export.no_z_calibration" in
          codes(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 21))
    @test "export.no_xy_calibration" in codes(Cecelia._export_qc_findings(Dict{String,Any}(), 1))

    # channelSelection submits channel NAMES, not indices. Converting them by hand threw
    # `Int("DAPI")` straight out of the task; `channel_indices` is the resolver, and it is 0-based —
    # which is what the runner slices with, so an off-by-one here exports the wrong channel.
    names = ["DAPI", "GFP", "mem-Tom"]
    @test channel_indices(["GFP"], names; what = "channels") == [1]
    @test channel_indices(["mem-Tom", "DAPI"], names; what = "channels") == [2, 0]
    @test channel_indices(nothing, names; what = "channels") == Int[]
    @test channel_indices(String[], names; what = "channels") == Int[]
    # A name this version doesn't have must say so rather than silently pick something.
    @test_throws ErrorException channel_indices(["nope"], names; what = "channels")

    # …and the NAMES must come from `channel_names`, which falls back to the active version. Channel
    # names are typically registered only under `default` while a processed version carries none, so
    # reading the requested version's raw field returns nothing and the task reports "(none
    # registered)" for an image whose channels the picker is listing — the picker is fed by
    # `channel_names(img)`, so any other source disagrees with what the user just clicked.
    proj = create_project!(name = "chan-fallback-$(rand(1000:9999))")
    st   = add_set!(proj; name = "s")
    im   = add_image!(st; name = "chan-fallback")
    set_channel_names!(im, ["DAPI", "SHG"]; value_name = VERSIONED_DEFAULT_VAL, check_length = false)
    save!(im)

    @test channel_names(im) == ["DAPI", "SHG"]
    # An explicit version with no entry of its own still resolves — this is the bug.
    @test channel_names(im; value_name = "corrected") == ["DAPI", "SHG"]
    @test channel_indices(["SHG"],
                          something(channel_names(im; value_name = "corrected"), String[]);
                          what = "channels") == [1]

    # Dispatch + spec wiring
    @test Cecelia._task_from_fun_name("exportImages.ome_tiff") isa ExportOmeTiff
    spec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    @test String(get(spec, :fun_name, "")) == "exportImages.ome_tiff"
    @test String(get(spec, :resource_pool, "")) == "io"
    # The output is an ARTEFACT, not a version — nothing may register an image version from it.
    @test !any(String(get(p, :key, "")) == "outputValueName" for p in get(spec, :params, []))

    # One filename rule, shared with the movie recorders — an image called "… (cropped)" must not
    # produce a name that ends in a separator (that bug shipped once already).
    @test safe_name_part("A B (cropped)") == "A_B_cropped"
    @test safe_name_part("  ") == ""
    @test safe_name_part(nothing) == ""
end

# ── Canonical-helper detectors ────────────────────────────────────────────────
# These exist because both rules below have now cost real debugging time, and neither was enforced.
# The pattern is the repo's existing one (`no_bare_write_h5ad`, `TextIoDeclaresEncodingTest`, the
# store-compressor/staging conventions): scan the SOURCE, fail on a new bypass.

@testset "channelSelection params resolve through channel_indices" begin
    # A `channelSelection` param submits channel NAMES. `channel_indices` is the one resolver — it
    # takes names OR already-resolved indices, returns 0-based values, and errors by name on a miss
    # (with a case-difference hint). Its own comment records SIX handlers that had hand-rolled
    # `findfirst(==(String(ch)), ch_names)` and drifted into three different wrong behaviours: an
    # index crashed four of them, an unmatched name was silently DROPPED by five, and drift
    # correction silently fell back to channel 0 — registering a whole timelapse against SHG.
    #
    # It happened again on the OME-TIFF export (`Int("SHG")` → MethodError, straight out of the task),
    # which is why this is now a test rather than a comment.
    function _has_channel_param(ps)::Bool
        for p in ps
            p isa AbstractDict || continue
            String(get(p, "type", "")) == "channelSelection" && return true
            inner = get(p, "params", nothing)
            inner isa AbstractVector && _has_channel_param(inner) && return true
        end
        false
    end

    checked = String[]
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        _has_channel_param(get(spec, "params", [])) || continue
        spec_path = Cecelia._spec_path(task)
        isnothing(spec_path) && continue
        jl = replace(spec_path, r"\.json$" => ".jl")
        isfile(jl) || continue        # a composite resolves nothing itself; its steps are checked
        push!(checked, fun)
        # A task file may have been split into a family (e.g. af_correct/{translate,qc,run}.jl next
        # to af_correct.jl) — check the aggregator PLUS any siblings under a same-named dir, since
        # the handler that actually calls `channel_indices` may live in a sub-file.
        family = String[jl]
        subdir = replace(jl, r"\.jl$" => "")
        if isdir(subdir)
            for f in readdir(subdir; join = true)
                endswith(f, ".jl") && push!(family, f)
            end
        end
        combined = join((read(f, String) for f in family), "\n")
        @test occursin("channel_indices", combined) ||
              error("$fun declares a channelSelection param but its handler never calls " *
                    "`channel_indices`. Resolve names with it (0-based, errors by name) rather " *
                    "than converting them by hand — see CLAUDE.md and channel_index's own comment.")
    end
    # The scan must actually find tasks; a rename that silently matched nothing would "pass".
    @test length(checked) >= 8
end

@testset "zarr access routes through the canonical helpers" begin
    # OME-ZARR is the same rule as `.h5ad`: one set of readers per language, and re-opening a store
    # by hand is how the two variants drift (CLAUDE.md → *Image / OME-ZARR access*). Julia has a
    # metadata tier in `app/src/tasks/importImages/omezarr.jl` (`series_base`, `ngff_attrs`,
    # `ngff_multiscales`, `zarr_array_meta`, `ngff_version` — exported through `Cecelia.jl`) and a
    # narrow display-only pixel tier in `api/src/image_geometry.jl` (`open_level` / `open_level0` /
    # `read_native`). Every other caller routes through those helpers.
    #
    # Two ways this rule is bypassed in practice — a bare `zopen(...)` / `Zarr.open(...)`, and a
    # `joinpath(..., ".zattrs" | ".zarray" | "zarr.json")` that reads the JSON itself instead of
    # going through `ngff_attrs`/`zarr_array_meta`. Both regressions have shipped before (the
    # `read_ngff_axes` note in `image_geometry.jl` records the second: reading `.zattrs` directly
    # made every v3 store answer EMPTY).
    #
    # Docstring mentions use backticks (`` `.zattrs` ``), which don't match the double-quoted literal.
    #
    # Sanctioned owners:
    #   * `importImages/omezarr.jl` (aggregator) and every `importImages/omezarr/*.jl` sub-file
    #     collectively define the metadata reader/writer tier — those are the files allowed to read
    #     the raw `.zattrs` / `.zarray` / `zarr.json` files.
    #   * `image_geometry.jl` defines `open_level`, so it is the one file allowed to call `zopen`.

    # ".zattrs", ".zarray", "zarr.json" as a code literal (a path join into a store) — but not the
    # docstring form `` `.zattrs` ``. `\\.` in raw-string form is the literal dot.
    literal_re = r"""\"\.zattrs\"|\"\.zarray\"|\"zarr\.json\""""
    # `zopen(...)` or `Zarr.open(...)` — the two ways a store gets opened via `Zarr.jl`.
    open_re    = r"\bzopen\s*\(|\bZarr\.open\s*\("

    # Path fragment (portable on Windows via joinpath). Any file under the omezarr/ family owns
    # the raw-JSON reads; the aggregator's own basename is the sentinel for the parent file itself.
    omezarr_family = joinpath("importImages", "omezarr")
    allowed_open   = Set(["image_geometry.jl"])

    literal_hits = String[]
    open_hits    = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            # rel key is enough to distinguish siblings — importImages/omezarr.jl vs the base name.
            in_omezarr_family = occursin(omezarr_family, path)
            rel  = in_omezarr_family ? joinpath(omezarr_family, f) : f
            for (i, ln) in enumerate(eachline(path))
                startswith(strip(ln), "#") && continue          # comments don't count
                if occursin(literal_re, ln) && !in_omezarr_family
                    push!(literal_hits, "$rel:$i  $(strip(ln))")
                end
                if occursin(open_re, ln) && !(rel in allowed_open)
                    push!(open_hits, "$rel:$i  $(strip(ln))")
                end
            end
        end
    end

    isempty(literal_hits) || error(
        "bare `.zattrs` / `.zarray` / `zarr.json` read outside `importImages/omezarr.jl`:\n  " *
        join(literal_hits, "\n  ") *
        "\nRoute through `ngff_attrs` / `ngff_multiscales` / `zarr_array_meta` / `ngff_version` — " *
        "reading `.zattrs` directly made every v3 store answer EMPTY once already " *
        "(see docs/todo/ZARR_V3_PLAN.md and CLAUDE.md → *Image / OME-ZARR access*).")

    isempty(open_hits) || error(
        "bare `zopen` / `Zarr.open` outside `api/src/image_geometry.jl`:\n  " *
        join(open_hits, "\n  ") *
        "\nOpen a store through `open_level` / `open_level0` and read pixels with `read_native` — " *
        "the display-only carve-out. Anything that PROCESSES data reads through Python `zarr_utils` " *
        "(CLAUDE.md → *Image / OME-ZARR access*).")

    @test isempty(literal_hits) && isempty(open_hits)

    # The scan must actually reach the sanctioned owners; a wrong root would let a real offender
    # slip through with an empty offender list.
    saw_literal_owner = false
    saw_open_owner    = false
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src")),
        (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        occursin(omezarr_family, path) &&
            occursin(literal_re, read(path, String)) && (saw_literal_owner = true)
        f == "image_geometry.jl" &&
            occursin(open_re, read(path, String)) && (saw_open_owner = true)
    end
    @test saw_literal_owner
    @test saw_open_owner
end

@testset "a process exit check also checks termsignal" begin
    # libuv reports `exitcode = 0` for a SIGNAL-KILLED child, and `task:cancel` kills by design — so
    # `exitcode == 0` alone reads a cancelled or timed-out process as a clean success. That is how a
    # timed-out agent run had its TRUNCATED output handed to the result parser.
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path  = joinpath(dir, f)
            lines = readlines(path)
            for (i, ln) in enumerate(lines)
                occursin(".exitcode", ln) || continue
                # A window, not the same line: the check is often split over two lines, or guarded by
                # a `killed` flag derived from termsignal a few lines above.
                lo, hi = max(1, i - 6), min(length(lines), i + 6)
                any(occursin("termsignal", lines[j]) for j in lo:hi) && continue
                push!(offenders, "$(basename(path)):$i  $(strip(ln))")
            end
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("`.exitcode` used without a nearby `termsignal` check:\n  " *
                                join(offenders, "\n  ") *
                                "\nlibuv sets exitcode 0 for a signal-killed process — check " *
                                "`proc.exitcode == 0 && proc.termsignal == 0`.")
end

@testset "dirPath param validation" begin
    # A destination folder, typed by hand or picked with the FileBrowser. The failure this guards is
    # late and expensive: without it a bad destination is only discovered after the task has read,
    # converted and tried to write the whole output.
    spec = [Dict{String,Any}("key" => "outDir", "label" => "Destination", "type" => "dirPath")]

    # Empty is legal — every consumer falls back to its own default (default_export_dir()).
    Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => ""), spec)
    Cecelia._validate_params_against_spec(Dict{String,Any}(), spec)

    mktempdir() do dir
        # An existing folder is the normal case.
        Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => dir), spec)

        # One that does not exist yet is fine too — a destination is created on demand, so rejecting
        # it would stop someone naming a new subfolder, which is the obvious thing to want.
        Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => joinpath(dir, "new_subfolder")), spec)

        # An existing FILE is the one unambiguous mistake: nothing can write output into it.
        f = joinpath(dir, "not_a_dir.txt"); write(f, "x")
        @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => f), spec)
    end

    @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
        Dict{String,Any}("outDir" => 42), spec)

    # The export's destination actually uses the type — the point of adding it.
    ospec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    outdir = only(filter(p -> String(get(p, :key, "")) == "outDir", collect(ospec[:params])))
    @test String(get(outdir, :type, "")) == "dirPath"
end

@testset "units written into OME-XML are schema-valid symbols" begin
    # OME's UnitsLength / UnitsTime are ENUMERATIONS of symbols. A value outside them makes the
    # whole <Pixels> element schema-invalid, and Bio-Formats then discards the ENTIRE OME block and
    # falls back to counting IFDs — a 31x4x32 movie opened as 3968 timepoints, one channel, no
    # names, no voxel size. Verified against real Bio-Formats (bioformats2raw): "µm" round-trips in
    # full, "micrometer" yields nothing.
    #
    # The trap is that "micrometer" is CORRECT in the two places it comes from: NGFF `.zattrs` axes
    # use UDUNITS-2 names, and `ccid.json` mirrors them because the importer reads the unit from the
    # axes. Only the OME-XML boundary needs the symbol — which is what `ome_xml_unit_name` is for.
    valid_length = Set(["Ym","Zm","Em","Pm","Tm","Gm","Mm","km","hm","dam","m","dm","cm","mm",
                        "µm","nm","pm","fm","am","zm","ym","Å","thou","li","in","ft","yd","mi",
                        "ua","ly","pc","pt","pixel","reference frame"])
    valid_time   = Set(["Ys","Zs","Es","Ps","Ts","Gs","Ms","ks","hs","das","s","ds","cs","ms",
                        "µs","ns","ps","fs","as","zs","ys","min","h","d"])
    valid = union(valid_length, valid_time)

    # Every output of the converter is a member — including for inputs already in symbol form.
    for (ngff, sym) in Cecelia._OME_XML_UNIT
        @test sym in valid
        @test Cecelia.ome_xml_unit_name(ngff) == sym
        @test Cecelia.ome_xml_unit_name(sym) in valid    # idempotent: a symbol stays valid
    end
    # The vocabularies the importer actually stores in ccid.json must all convert.
    for ngff in ("micrometer", "nanometer", "millimeter", "second", "minute")
        @test Cecelia.ome_xml_unit_name(ngff) in valid
    end
    # An unknown unit passes through — we do not guess a conversion — so it is the CALLER's job not
    # to invent one, and the scan below is what keeps a caller from skipping the converter entirely.
    @test Cecelia.ome_xml_unit_name("furlong") == "furlong"

    # Anything that ASSIGNS an OME unit attribute must route through the converter. This is the
    # bypass that shipped: the OME-TIFF export copied ccid.json's "micrometer" straight into
    # PhysicalSizeXUnit, while every other writer converted.
    #
    # Compliance is per-DIRECTORY: a file that only READS these keys into an intermediate Dict (the
    # metadata reader) doesn't itself need to call `ome_xml_unit_name`, but the sibling file in the
    # same directory that WRITES OME-XML must — and the split of `importImages/omezarr.jl` into
    # `omezarr/reader.jl` + `omezarr/calibration.jl` separates the two halves, so a file-local check
    # would false-positive on reader.jl. Aggregate at directory level: the family passes iff SOMEONE
    # under the same dir calls the converter.
    dir_has_converter = Dict{String,Bool}()
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            occursin("ome_xml_unit_name", read(joinpath(dir, f), String)) || continue
            dir_has_converter[dir] = true
        end
    end
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f); src = read(path, String)
            occursin(r"\"(PhysicalSize[XYZ]Unit|TimeIncrementUnit)\"\s*(=>|\]\s*=)", src) || continue
            occursin("ome_xml_unit_name", src) && continue
            get(dir_has_converter, dir, false) && continue
            push!(offenders, basename(path))
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("these assign an OME-XML unit attribute without calling " *
                                "`ome_xml_unit_name`:\n  " * join(offenders, "\n  ") *
                                "\nccid.json/NGFF store UDUNITS names ('micrometer'); OME-XML " *
                                "needs the symbol ('µm'), and an invalid one voids the whole block.")
end

@testset "view profiles (curated sidebar)" begin
    # A profile is a named, ORDERED subset of sidebar routes, dropped in as a file under
    # <config_dir>/profiles/. The reader validates SHAPE ONLY — it deliberately does not know the route
    # table (that lives in frontend/src/main.ts), so an item naming a route that no longer exists is a
    # frontend concern, not an error here. See docs/todo/VIEW_PROFILES_PLAN.md.
    @test view_profiles_dir() == joinpath(config_dir(), "profiles")

    # ── parse: what a valid profile is ────────────────────────────────────────
    p = parse_view_profile("focused", Dict("label" => "Gating + behaviour",
                                           "items" => ["/gate", "/behaviour"]))
    @test p.id == "focused"
    @test p.label == "Gating + behaviour"
    @test p.items == ["/gate", "/behaviour"]

    # the label falls back to the id, so a file needs only `items`
    @test parse_view_profile("myview", Dict("items" => ["/gate"])).label == "myview"
    # order is the profile's, and duplicates keep their FIRST position
    @test parse_view_profile("x", Dict("items" => ["/track", "/gate", "/track"])).items ==
          ["/track", "/gate"]

    # ── parse: every rejection is a message, never a crash ────────────────────
    @test_throws ArgumentError parse_view_profile("x", Dict("label" => "no items"))
    # empty is an ERROR, not an empty profile: rendering a blank sidebar from a typo looks broken
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => String[]))
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => ["gate"]))   # not a route path
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => [42]))
    @test_throws ArgumentError parse_view_profile("x", "not an object")

    # ── ids come from a user-typed label, never a filename ────────────────────
    @test view_profile_id("Gating + behaviour") == "gating_behaviour"
    @test_throws ArgumentError view_profile_id("+++")
    # and a hostile id cannot escape the profiles dir
    @test !occursin("..", view_profile_id("../../etc/passwd"))

    # ── round-trip through disk: write → read ─────────────────────────────────
    dir = view_profiles_dir()
    saved = write_view_profile("Gating + behaviour", ["/gate", "/track", "/behaviour"])
    @test saved.id == "gating_behaviour"
    @test isfile(joinpath(dir, "gating_behaviour.json"))

    got = read_view_profiles()
    @test got.dir == dir
    mine = only(filter(p -> p.id == "gating_behaviour", got.profiles))
    @test mine.label == "Gating + behaviour"
    @test mine.items == ["/gate", "/track", "/behaviour"]
    @test isempty(got.errors)

    # renaming the LABEL must keep the id, or an active selection breaks under the user
    renamed = write_view_profile("Gating only", ["/gate"]; id = "gating_behaviour")
    @test renamed.id == "gating_behaviour"
    @test renamed.label == "Gating only"
    @test only(filter(p -> p.id == "gating_behaviour", read_view_profiles().profiles)).items == ["/gate"]

    # ── a broken file is REPORTED, never fatal, and never hides the good ones ──
    write(joinpath(dir, "broken.json"), "{ not json")
    write(joinpath(dir, "empty.json"), "{\"items\": []}")
    bad = read_view_profiles()
    @test any(p -> p.id == "gating_behaviour", bad.profiles)     # the valid one still loads
    @test Set(e.file for e in bad.errors) == Set(["broken.json", "empty.json"])

    # a non-.json file in the dir is not a profile and not an error
    write(joinpath(dir, "notes.txt"), "ignore me")
    @test Set(e.file for e in read_view_profiles().errors) == Set(["broken.json", "empty.json"])

    # ── delete ────────────────────────────────────────────────────────────────
    @test delete_view_profile!("gating_behaviour")
    @test !delete_view_profile!("gating_behaviour")             # already gone is not an error
    @test !any(p -> p.id == "gating_behaviour", read_view_profiles().profiles)

    # tidy up so a later testset reading the config dir sees no leftovers
    for f in ("broken.json", "empty.json", "notes.txt")
        rm(joinpath(dir, f); force = true)
    end
end

# ── The task runner losing its port is a normal outcome, not a crash ──────────
#
# The runner is built to OUTLIVE the API server, so on a fresh start there is often already one
# running — and two checkouts sharing a `CECELIA_DEV_DIR` share the port outright. Two things used to
# go wrong when a second one lost that race, and this pins both:
#
#   1. it died with a `TaskFailedException` stack trace, which reads as a broken app; and
#   2. WORSE — it wrote `runner.json` with its own pid BEFORE binding, clobbering the incumbent's
#      record, then its `atexit` hook deleted the file on the way out. So a collision left the
#      SURVIVING runner with no state file, which is exactly the "a stray runner is folklore" case
#      that file exists to prevent.
#
# Uses an EPHEMERAL port held by a plain socket, never the real 7657 — a test must not touch a runner
# the developer has running.
#
# It caught a THIRD way to get this wrong, on macOS CI only: `HTTP.listen!` builds a `Server` and
# spawns a task that does the bind, and the failure path notifies its ready `Event` BEFORE it rethrows —
# so `listen!` returned normally, the state file was claimed, and the EADDRINUSE surfaced at `wait` as
# a `TaskFailedException`. Both original symptoms, back. Ownership is therefore proven by asking
# `/ping` for the responder's PID (`_runner_owns_port`), which no scheduler ordering can fake — the
# raw socket this test holds never speaks HTTP, so nothing answers and the runner stands down.
@testset "runner_serve stands down when the port is taken" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    held = sock_listen(localhost, 0)              # port 0 → the OS picks a free one
    port = Int(getsockname(held)[2])

    state = joinpath(Cecelia.config_dir(), "runner.json")
    mkpath(dirname(state))
    # stand in for the incumbent's record, with a pid that is NOT ours
    marker = Dict{String,Any}("pid" => 999_999, "port" => port, "commit" => "incumbent")
    Cecelia.write_json_atomic(state, marker)

    try
        # Returns rather than throwing. The old code raised TaskFailedException out of HTTP.listen.
        r = @test_logs (:warn,) match_mode=:any Cecelia.runner_serve(; port = port)
        @test r === nothing

        # …and the incumbent's record is untouched: neither overwritten with our pid nor deleted.
        @test isfile(state)
        @test JSON3.read(read(state, String), Dict{String,Any})["pid"] == 999_999
        @test JSON3.read(read(state, String), Dict{String,Any})["commit"] == "incumbent"
    finally
        close(held)
        rm(state; force = true)
    end
end

# ── The TWIN of the test above, and the one whose absence let a regression ship ───────────────────
#
# The guard was only ever pinned from the "port is taken" side, so `_runner_owns_port` answering false
# for a port we had *just bound ourselves* looked like a pass. It shipped, and then every fresh
# `pixi run dev` printed "another process took it" with nothing listening on 7657 at all — the runner
# exited on start and every task fell back to in-process.
#
# The cause was a budget that could not be met, not a wrong idea: a freshly returned `HTTP.listen!`
# server does not serve its first in-process request for ~1.2 s (the accept path is still compiling),
# and the session's first `HTTP.get` compiles for about as long — so a 2 s per-attempt timeout inside a
# 3 s deadline bought ONE attempt, which could not succeed. This test fails outright on those numbers.
#
# Deliberately exercises `_runner_owns_port` against a real `HTTP.listen!` rather than `runner_serve`:
# a full runner installs `_runner_idle_watchdog!`, which calls `exit(0)`, and a test process that may
# exit from under the suite is not a test.
@testset "_runner_owns_port recognises a port WE just bound" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    probe = sock_listen(localhost, 0)             # port 0 → the OS names a free one
    port  = Int(getsockname(probe)[2])
    close(probe)                                   # …then hand it back, so we can bind it ourselves

    server = Cecelia.HTTP.listen!(Cecelia._runner_stream, "127.0.0.1", port)
    try
        @test Cecelia._runner_owns_port(port)
        # and it is OUR pid it recognised, not merely "something answered"
        reply = Cecelia.runner_ping(Cecelia.RunnerHandle(; port = port))
        @test reply !== nothing
        @test string(get(reply, "pid", "")) == string(getpid())
    finally
        try; close(server); catch; end
    end
end

# ── An empty response body must not corrupt the connection ────────────────────
#
# `write_http_body!` (utils.jl, where the mechanism is explained) exists for one wire-level reason:
# our responses carry no Content-Length, so HTTP.jl frames them CHUNKED and frames every `write` as
# its own chunk — so a zero-length write emits the TERMINATING `0\r\n\r\n` and `closewrite` emits a
# second one. This asserts the BYTES, over two requests on ONE keep-alive connection, because the
# response that broke was never the empty one: the extra terminator sits in the connection and the
# NEXT response is parsed starting at it.
#
# The unguarded `write` is kept as the negative control — a guard is only worth having if the test
# fails without it. Ephemeral port, and a raw socket that speaks HTTP by hand (an HTTP client would
# hide the framing, which is the whole subject).
@testset "an empty response body is written through write_http_body!" begin
    using Sockets: connect as sock_connect
    HTTP = Cecelia.HTTP

    # READING THE WIRE, without guessing at packetisation — both halves of this were measured, and
    # both got it wrong first:
    #   * `bytesavailable(sock)` is NOT a usable poll. Julia stops the libuv read loop when its
    #     buffer empties, so it reports 0 with bytes still pending — that version saw 2 terminators
    #     of 4 on half its runs. So ONE reader task blocks in `readavailable` for the socket's whole
    #     life and appends everything; `close` is what ends it.
    #   * The stopping condition must be the DATA, not a fixed sleep. The extra terminator is written
    #     separately from the head, so "both responses are in" can be true with those 4 bytes still in
    #     flight — that is how the first version of this test flaked on ubuntu CI only (2 of 4) while
    #     passing on macOS, Windows and locally. A generous deadline also absorbs the first call's
    #     compilation, which a fixed window did not.
    settle = function (acc, quiet, cap)         # wait until the accumulated bytes stop growing
        n = length(acc[]); t0 = time(); t_last = time()
        while time() - t_last < quiet && time() - t0 < cap
            sleep(0.02)
            if length(acc[]) != n; n = length(acc[]); t_last = time(); end
        end
    end

    wire = function (guarded::Bool)
        handler = function (stream)
            read(stream)
            HTTP.setstatus(stream, 200)
            HTTP.setheader(stream, "Content-Type" => "application/octet-stream")
            HTTP.startwrite(stream)
            guarded ? write_http_body!(stream, UInt8[]) : write(stream, UInt8[])
        end
        server = HTTP.listen!(handler, "127.0.0.1", 0)          # port 0 → never a real one
        try
            sock = sock_connect("127.0.0.1", HTTP.port(server))
            acc  = Ref("")
            reader = @async try
                while !eof(sock); acc[] *= String(copy(readavailable(sock))); end
            catch; end
            try
                for i in 1:2                    # request 2 is the one the stray bytes broke
                    write(sock, "GET /$i HTTP/1.1\r\nHost: x\r\n\r\n")
                    t0 = time()
                    while count("HTTP/1.1 200", acc[]) < i && time() - t0 < 20; sleep(0.02); end
                end
                settle(acc, 0.5, 5.0)
            finally
                close(sock)
            end
            wait(reader)
            acc[]
        finally
            close(server)
        end
    end

    guarded = wire(true)
    @test count("HTTP/1.1 200", guarded) == 2
    @test count("0\r\n\r\n", guarded) == 2        # exactly ONE terminating chunk per response
    @test endswith(guarded, "0\r\n\r\n")          # …and no bytes left over for the next response

    # the bug, on the wire: `…0\r\n\r\n0\r\n\r\nHTTP/1.1 …`. Vite's proxy died on exactly these bytes
    # (`HPE_INVALID_CONSTANT … rawPacket <30 0d 0a 0d 0a 30 0d 0a 0d 0a>`), failing the OTHER, good
    # requests that shared the connection with one legitimately-empty gating plot.
    bare = wire(false)
    @test count("0\r\n\r\n", bare) == 4
    @test occursin("0\r\n\r\n0\r\n\r\n", bare)

    # so no handler hands a body to `write` directly — the runner's reply path included
    src = read(joinpath(@__DIR__, "..", "src", "runner", "server.jl"), String)
    @test occursin("write_http_body!(stream, body)", src)
    @test !occursin("write(stream, ", src)
end

# ── Correction testsets ────────────────────────────────────────────────────
# Manual TRACK correction (docs/todo/CORRECTION_PLAN.md P1) + manual LABEL correction (P2)
# + track-issue triage worklist. Pure ops-engine tests against hand-built cell tables — no
# fixture, no .h5ad. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating testset
# scope, so any helpers defined earlier in suite.jl are still in scope for the correction
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "correction.jl"))

# ── Tracking / celltrackR testsets ─────────────────────────────────────────
# 5 testsets covering: analyze_cell_pairs (celltrackR port + double-tracking + angle/dist
# semantics), the diagnostics battery (app/src/tracking/track_diagnostics.jl), golden values
# cross-checked against celltrackR 1.2.2, image-pooled track_cohort readings, and the
# (images × population) grouping the two track plots share. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in suite.jl
# are still in scope for the tracking fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "tracking.jl"))

# ── bf2raw_series_subdir: which subdir under `<zarr>/` carries the multiscales, when the caller can't
# assume it is `0/`. `bioformats2raw --series N` preserves the source index in the output group name
# (so `--series 3` writes `<zarr>/3/`); this is the resolver the import task uses to decide which
# subdir to point ccid.json's `filepath["default"]` at. Same fixture shape as `make_zarr`'s :series
# layout — a numbered subdir with a `.zattrs` multiscales attr.
@testset "bf2raw_series_subdir" begin
    function _seed(dir::AbstractString, name::AbstractString)
        base = joinpath(dir, name)
        mkpath(base)
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => [Dict{String,Any}("name" => "x", "type" => "space")],
                              "datasets" => [Dict{String,Any}("path" => "0",
                                  "coordinateTransformations" =>
                                    [Dict{String,Any}("type" => "scale", "scale" => [1.0])])])])
        open(joinpath(base, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        mkpath(joinpath(base, "0"))
        write(joinpath(base, "0", ".zattrs"), "{}")
        base
    end

    mktempdir() do d
        # single-series bf2raw default: subdir "0" carries multiscales
        _seed(d, "0")
        @test Cecelia.bf2raw_series_subdir(d) == "0"
        # `prefer = 0` doesn't change the answer when 0/ exists
        @test Cecelia.bf2raw_series_subdir(d; prefer = 0) == "0"
    end
    mktempdir() do d
        # --series 3 output: only subdir "3" exists (no "0"). The resolver must find it, and
        # `prefer = 3` must beat the fallback scan (a same-name collision on 0/ never masks the pick).
        _seed(d, "3")
        @test Cecelia.bf2raw_series_subdir(d) == "3"
        @test Cecelia.bf2raw_series_subdir(d; prefer = 3) == "3"
    end
    mktempdir() do d
        # multi-series without --series: bf2raw wrote 0/ 1/ 2/ 3/. `prefer = 3` picks the requested
        # one instead of the alphabetically-first (0/), which is what the "unblock ttRMjQ" path needs.
        for n in ("0", "1", "2", "3"); _seed(d, n); end
        @test Cecelia.bf2raw_series_subdir(d; prefer = 3) == "3"
        @test Cecelia.bf2raw_series_subdir(d) == "0"  # no preference → the bf2raw default
    end
    mktempdir() do d
        # flat-store layout: multiscales AT the root (crop/correction output). Return "" — caller
        # should not descend into a subdir. Level-0 array under `0/` has empty `.zattrs`, so a naive
        # scan would otherwise still return "0" and read the wrong file.
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => [Dict{String,Any}("name" => "x", "type" => "space")],
                              "datasets" => [Dict{String,Any}("path" => "0",
                                  "coordinateTransformations" =>
                                    [Dict{String,Any}("type" => "scale", "scale" => [1.0])])])])
        open(joinpath(d, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        mkpath(joinpath(d, "0"))
        write(joinpath(d, "0", ".zattrs"), "{}")
        @test Cecelia.bf2raw_series_subdir(d) == ""
    end
    # nothing exists → nothing
    @test isnothing(Cecelia.bf2raw_series_subdir(joinpath(tempdir(), "nope-$(rand(UInt32))")))
end

# The typed per-task params structs for the cleanupImages family. Each parser is the ONE place its
# task reads the params bag, so this pins the fields + defaults that the rest of the task body
# depends on — a spec rename that isn't mirrored here would show up as a struct-field access error
# rather than a silent default at the read site.
@testset "typed params — cleanupImages" begin
    # Smooth: full dict → every field carries through; empty dict → defaults.
    let p = Cecelia.parse_smooth_params(Dict{String,Any}(
            "valueName"           => "driftCorrected",
            "channels"            => ["mem-TOM"],
            "spatialMethod"       => "bilateral_vst",
            "spatialSigma"        => 2.5,
            "bilateralColor"      => 12.0,
            "bilateralReach"      => 4.0,
            "bilateralPolish"     => 0.7,
            "temporalFrames"      => 3,
            "temporalStat"        => "mean",
            "farnebackMaxShiftPx" => 6.5,
            "restoreDynamicRange" => false))
        @test p.valueName == "driftCorrected"
        @test p.channels == ["mem-TOM"]
        @test p.spatialMethod == "bilateral_vst"
        @test p.spatialSigma === 2.5
        @test p.temporalFrames === 3
        @test p.restoreDynamicRange === false
    end
    let p = Cecelia.parse_smooth_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.spatialMethod == "gaussian"
        @test p.temporalFrames === 1
        @test p.restoreDynamicRange === true
    end

    # StackAlign
    let p = Cecelia.parse_stack_align_params(Dict{String,Any}(
            "alignChannel" => ["CD169"], "referenceMode" => "first",
            "minConfidence" => 0.5, "maxShiftPx" => 12.0))
        @test p.alignChannel == ["CD169"]
        @test p.referenceMode == "first"
        @test p.minConfidence === 0.5
        @test p.maxShiftPx === 12.0
    end

    # FlowRegister
    let p = Cecelia.parse_flow_register_params(Dict{String,Any}(
            "registerChannel" => ["CD169"], "structuralChannels" => ["SHG"],
            "referenceMode" => "first", "aggressiveness" => "gentle",
            "pyrLevels" => 3, "maxShiftPx" => 20.0))
        @test p.registerChannel == ["CD169"]
        @test p.structuralChannels == ["SHG"]
        @test p.aggressiveness == "gentle"
        @test p.pyrLevels === 3
    end

    # Denoise
    let p = Cecelia.parse_denoise_params(Dict{String,Any}(
            "model" => "supp.MERTK", "channels" => ["mem-TOM"], "batchSize" => 4))
        @test p.model == "supp.MERTK"
        @test p.channels == ["mem-TOM"]
        @test p.batchSize === 4
    end

    # DriftCorrect
    let p = Cecelia.parse_drift_correct_params(Dict{String,Any}(
            "driftChannel" => ["mem-TOM"], "driftEstimator" => "sitkRigid",
            "driftMaxAngle" => 3.0, "driftPerPlane" => true, "driftZSmoothness" => 0.5))
        @test p.driftChannel == ["mem-TOM"]
        @test p.driftEstimator == "sitkRigid"
        @test p.driftMaxAngle === 3.0
        @test p.driftPerPlane === true
        @test p.driftZSmoothness === 0.5
    end

    # AfCorrect + the two typed structs at its edges (task #21 AfChannelStats, task #22
    # AfCombinationSpec — see docs/archive/comment-audit-findings.md → Tier 1 boundary bags).
    let p = Cecelia.parse_af_correct_params(Dict{String,Any}(
            "backgroundMethod" => "otsu"))
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test p.backgroundMethod == "otsu"
    end
    let specs = Cecelia.parse_af_combinations(Dict{String,Any}(
            "afCombinations" => Dict{String,Any}(
                "CD169-Kat" => Dict{String,Any}(
                    "targetChannel"     => ["CD169-Kat"],
                    "competingChannels" => ["SHG", "CH4"]),
                "malformed" => "not a dict")))
        @test length(specs) == 1
        @test specs[1].key == "CD169-Kat"
        @test specs[1].targetChannel == ["CD169-Kat"]
        @test specs[1].competingChannels == ["SHG", "CH4"]
    end
    # AfChannelStats round-trips the Python-side per-channel block and carries a Dict{String,Float64}
    # bleedthrough. A missing bleedthrough is a legal shape (no leaks detected) → empty Dict.
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}(
            "saturatedFrac" => 0.02, "levelsUsed" => 3200.0, "levelsAvailable" => 4096.0,
            "bleedthrough" => Dict{String,Any}("SHG" => 0.15, "CD169" => 0.03)))
        @test s.saturatedFrac === 0.02
        @test s.levelsUsed === 3200.0
        @test s.levelsAvailable === 4096.0
        @test s.bleedthrough == Dict("SHG" => 0.15, "CD169" => 0.03)
    end
    let s = Cecelia.parse_af_channel_stats(Dict{String,Any}())
        @test s.saturatedFrac === 0.0
        @test s.levelsAvailable === 1.0    # min-1 floor so `used / avail` never divides by zero
        @test isempty(s.bleedthrough)
    end
end

# Typed per-task params structs for the editImages family. Each parser is the ONE place its task
# reads the params bag — a spec rename that isn't mirrored here becomes a struct-field error rather
# than a silent default at the read site. Same pattern as `typed params — cleanupImages`.
@testset "typed params — editImages" begin
    # BinImage
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}(
            "valueName" => "corrected", "factorX" => 4, "factorY" => 2, "op" => "sum"))
        @test p.valueName == "corrected"
        @test p.factorX === 4
        @test p.factorY === 2
        @test p.op == "sum"
    end
    let p = Cecelia.parse_bin_image_params(Dict{String,Any}())
        @test p.factorX === 2 && p.factorY === 2 && p.op == "mean"
    end

    # CopyImage — exactly-one-of toSetUid/newSetName is a runtime check (not enforced by the type),
    # so both empty is a legal parse (the handler catches it).
    let p = Cecelia.parse_copy_image_params(Dict{String,Any}(
            "valueName" => "corrected", "newSetName" => "  Day 3  "))
        @test p.valueName == "corrected"
        @test p.toSetUid == ""
        @test p.newSetName == "Day 3"          # whitespace stripped
    end

    # CropImage — nested CropBox with the -1 keep-axis convention.
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}(
            "cropBox" => Dict{String,Any}(
                "x0"=>10, "x1"=>200, "y0"=>0, "y1"=>256, "z0"=>-1, "z1"=>-1, "t0"=>5, "t1"=>20)))
        b = p.cropBox
        @test b.x0 === 10 && b.x1 === 200
        @test b.y0 === 0  && b.y1 === 256
        @test b.z0 === -1 && b.z1 === -1        # 2D image: keep the whole axis
        @test b.t0 === 5  && b.t1 === 20
    end
    # missing/malformed cropBox → defaulted struct (the handler's separate check rejects the run).
    let p = Cecelia.parse_crop_image_params(Dict{String,Any}("valueName" => "x"))
        @test p.cropBox.x0 === 0 && p.cropBox.z0 === -1
    end

    # DtypeConvert — dtype + rescale are LOWERCASED at parse (the handler's whitelist reads the
    # lowered value).
    let p = Cecelia.parse_dtype_convert_params(Dict{String,Any}(
            "dtype" => "UINT16", "rescale" => "NONE"))
        @test p.dtype == "uint16"
        @test p.rescale == "none"
    end

    # Flip — axis UPPERCASED at parse.
    let p = Cecelia.parse_flip_params(Dict{String,Any}("axis" => "z"))
        @test p.axis == "Z"
    end

    # Register — the audit's `regChannel` is a scalar channel NAME (not a `channelSelection`
    # array); parser preserves it verbatim.
    let p = Cecelia.parse_register_params(Dict{String,Any}(
            "regChannel" => "mem-TOM", "doAffine3d" => true, "sigma" => 0.5,
            "samplesPerParameter" => 8000))
        @test p.regChannel == "mem-TOM"
        @test p.doAffine2d === true            # default preserved
        @test p.doAffine3d === true
        @test p.sigma === 0.5
        @test p.samplesPerParameter === 8000
    end

    # ResampleZ — order LOWERCASED at parse.
    let p = Cecelia.parse_resample_z_params(Dict{String,Any}("order" => "CUBIC"))
        @test p.order == "cubic"
    end

    # TProject / ZProject — same shape, different defaults (mean vs max).
    let p = Cecelia.parse_t_project_params(Dict{String,Any}())
        @test p.op == "mean"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}())
        @test p.op == "max"
    end
    let p = Cecelia.parse_z_project_params(Dict{String,Any}("op" => "median"))
        @test p.op == "median"
    end
end

# Typed per-task params structs for the tracking family. Same pattern as
# `typed params — cleanupImages`: each parser is the ONE place its task reads the params bag, so
# a spec rename that isn't mirrored here becomes a struct-field error rather than a silent default.
@testset "typed params — tracking" begin
    # BayesianTracking — full-dict + empty-dict paths. Field spellings match btrack's own names
    # (`noiseInital` is a typo in the spec; preserved verbatim so a saved run stays reproducible).
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}(
            "valueName"      => "corrected",
            "popsToTrack"    => "T/tracked",
            "maxSearchRadius"=> 30,
            "maxLost"        => 5,
            "trackBranching" => true,
            "accuracy"       => 0.9,
            "distThresh"     => 12.5,
            "lambdaBranch"   => 100,
            "thetaDist"      => 7.5))
        @test p.valueName == "corrected"
        @test p.popsToTrack == "T/tracked"
        @test p.maxSearchRadius === 30
        @test p.maxLost === 5
        @test p.trackBranching === true
        @test p.accuracy === 0.9
        @test p.distThresh === 12.5
        @test p.lambdaBranch === 100
        @test p.thetaDist === 7.5
    end
    let p = Cecelia.parse_bayesian_tracking_params(Dict{String,Any}())
        @test p.popsToTrack == "NONE"                # default = track whole segmentation
        @test p.maxSearchRadius === 20
        @test p.noiseInital === 300                  # historical spelling preserved
        @test p.thetaDist === 5.0
    end

    # TrackCorrect — the parser delegates `trackOps` to `parse_track_ops` (the same helper that
    # `validate_params` uses); empty/nothing → empty vector, a real op passes through with its
    # own dict entries preserved.
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "valueName" => "corrected",
            "trackOps"  => Any[Dict{String,Any}(
                "op" => "track.join", "trackIds" => Any[78, 92])]))
        @test p.valueName == "corrected"
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.join"
        @test p.trackOps[1]["trackIds"] == Any[78, 92]
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}("trackOps" => nothing))
        @test isempty(p.trackOps)
    end
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
        @test isempty(p.trackOps)
    end
    # JSON string round-trips the same way as a real Vector (the form path).
    let p = Cecelia.parse_track_correct_params(Dict{String,Any}(
            "trackOps" => "[{\"op\":\"track.remove\",\"trackIds\":[10]}]"))
        @test length(p.trackOps) == 1
        @test p.trackOps[1]["op"] == "track.remove"
    end

    # TrackMeasures — `dims` is lower-cased AND stripped at parse so the handler reads one shape.
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}(
            "valueName" => "corrected", "forceRecompute" => false, "dims" => "  3D  "))
        @test p.valueName == "corrected"
        @test p.forceRecompute === false
        @test p.dims == "3d"
    end
    let p = Cecelia.parse_track_measures_params(Dict{String,Any}())
        @test p.dims == "auto"                       # default: preflight detects
        @test p.forceRecompute === true              # default is force-recompute
    end
end

# Typed per-task params structs for the segment family. Same pattern as
# `typed params — cleanupImages` — each parser is the ONE place its task reads the params bag.
@testset "typed params — segment" begin
    # CellposeSegment — `models` stays a bag (resolved separately by cellpose_models_for_python).
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}(
            "valueName" => "corrected", "outputValueName" => "cells",
            "blockSize" => 1024, "overlap" => 128, "matchThreshold" => 0.5,
            "removeUnmatched" => true, "normaliseToWhole" => false))
        @test p.valueName == "corrected"
        @test p.outputValueName == "cells"
        @test p.blockSize === 1024
        @test p.overlap === 128
        @test p.matchThreshold === 0.5
        @test p.removeUnmatched === true
        @test p.normaliseToWhole === false
    end
    let p = Cecelia.parse_cellpose_segment_params(Dict{String,Any}())
        @test p.blockSize === 512
        @test p.overlap === 64
        @test p.matchThreshold === 0.3
        @test p.normaliseToWhole === true         # default: normalise to whole image
    end

    # CoastalSegment — same shape as cellpose plus temporalScaleMode.
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}(
            "outputValueName" => "flow-cells", "temporalScaleMode" => "seconds",
            "labelSmoothing" => 1.0))
        @test p.outputValueName == "flow-cells"
        @test p.temporalScaleMode == "seconds"
        @test p.labelSmoothing === 1.0
    end
    let p = Cecelia.parse_coastal_segment_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"    # default: as trained
        @test p.labelSmoothing === 0.5           # coastal's default differs from cellpose (0.0)
    end

    # Branching — fibreChannels stays a bag (resolved via channel_indices in the handler).
    let p = Cecelia.parse_branching_params(Dict{String,Any}(
            "refPops" => "T/tracked", "calcAnisotropy" => true,
            "fibreChannels" => ["SHG"], "anisotropySource" => "channel",
            "structureTensorSigmaUm" => 5.0, "anisotropyBoxUm" => 3.0,
            "integrateTime" => true))
        @test p.refPops == "T/tracked"
        @test p.calcAnisotropy === true
        @test p.fibreChannels == ["SHG"]
        @test p.anisotropySource == "channel"
        @test p.structureTensorSigmaUm === 5.0
        @test p.anisotropyBoxUm === 3.0
        @test p.integrateTime === true
    end
    let p = Cecelia.parse_branching_params(Dict{String,Any}())
        @test p.refPops == "NONE"                # default: skeletonise whole segmentation
        @test p.calcAnisotropy === false
        @test p.anisotropySource == "skeleton"
    end

    # SegmentCorrect — same JSON-string trackOps pattern as tracking.correct.
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "valueName" => "cells",
            "labelOps" => Any[Dict{String,Any}(
                "op" => "label.merge", "t" => 0, "ids" => Any[3, 5], "into" => 3)]))
        @test p.valueName == "cells"
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.merge"
        @test p.labelOps[1]["into"] == 3
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}("labelOps" => nothing))
        @test isempty(p.labelOps)
    end
    let p = Cecelia.parse_segment_correct_params(Dict{String,Any}(
            "labelOps" => "[{\"op\":\"label.remove\",\"t\":0,\"ids\":[7]}]"))
        @test length(p.labelOps) == 1
        @test p.labelOps[1]["op"] == "label.remove"
    end

    # SegmentCorrectCarryOver — a single shared struct for both phases (they read only valueName).
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}(
            "valueName" => "corrected"))
        @test p.valueName == "corrected"
    end
    let p = Cecelia.parse_segment_correct_carry_over_params(Dict{String,Any}())
        @test p.valueName == Cecelia.VERSIONED_DEFAULT_VAL
    end

    # MeasureLabels
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}(
            "outputValueName" => "cells", "intensityValueName" => "corrected",
            "intensityMeasure" => "median", "gaussianFilter" => 1.0,
            "extendedMeasures" => true))
        @test p.outputValueName == "cells"
        @test p.intensityValueName == "corrected"
        @test p.intensityMeasure == "median"
        @test p.gaussianFilter === 1.0
        @test p.extendedMeasures === true
    end
    let p = Cecelia.parse_measure_labels_params(Dict{String,Any}())
        @test p.intensityMeasure == "mean"       # default
        @test p.extendedMeasures === false
    end
end

@testset "typed params — opticalFlow" begin
    # TrainFlowModel — 20+ fields; spot-check the ones that MEAN something.
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}(
            "valueName" => "corrected", "modelName" => "cd8-flow",
            "overwrite" => true, "trainChannels" => ["mem-TOM"],
            "temporalScaleMode" => "seconds", "temporalScales" => "2,4,8",
            "flowMetrics" => ["cell_boundary_likelihood", "cumulative_mag"],
            "epochs" => 60, "foregroundBoundaryWeight" => 0.3,
            "cropSize" => 256))
        @test p.valueName == "corrected"
        @test p.modelName == "cd8-flow"
        @test p.overwrite === true
        @test p.trainChannels == ["mem-TOM"]
        @test p.temporalScaleMode == "seconds"
        @test p.temporalScales == "2,4,8"
        @test p.flowMetrics == ["cell_boundary_likelihood", "cumulative_mag"]
        @test p.epochs === 60
        @test p.foregroundBoundaryWeight === 0.3
        @test p.cropSize === 256
    end
    let p = Cecelia.parse_train_flow_model_params(Dict{String,Any}())
        @test p.temporalScaleMode == "frames"
        @test p.temporalScales == "1,2,4,8"
        @test isnothing(p.flowMetrics)                  # nothing → shipped default
        @test p.epochs === 30
        @test p.foregroundBoundaryWeight === 0.0        # OFF by default
        @test p.foregroundBlurSigma === 1.0
    end

    # TrainSupportDenoise
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}(
            "modelName" => "supp.MERTK", "trainChannels" => ["mem-TOM"],
            "unetSize" => "large", "inputFrames" => 31,
            "trainMode" => "perChannel", "epochs" => 40, "earlyStop" => false))
        @test p.modelName == "supp.MERTK"
        @test p.unetSize == "large"
        @test p.inputFrames === 31
        @test p.trainMode == "perChannel"
        @test p.epochs === 40
        @test p.earlyStop === false
    end
    let p = Cecelia.parse_train_support_denoise_params(Dict{String,Any}())
        @test p.unetSize == "medium"
        @test p.inputFrames === 61
        @test p.trainMode == "pooled"
        @test p.earlyStop === true
        @test p.patience === 5
    end
end

# Typed per-task params for the remaining families — closing out the ChainNode.params arc.
# Bundles the small tail families (importImages, exportImages, clust{Pops,Tracks,Regions},
# behaviour, spatialAnalysis) into one testset; each parser is the ONE place its task reads the
# params bag.
@testset "typed params — remaining families" begin
    # exportImages.ExportOmeTiff — `timepoint` accepts strings + -1 sentinel; `outDir` is stripped.
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}(
            "valueName" => "corrected", "channels" => ["mem-TOM"],
            "zMip" => true, "timepoint" => "5", "outDir" => "  /tmp/out  "))
        @test p.valueName == "corrected"
        @test p.channels == ["mem-TOM"]
        @test p.zMip === true
        @test p.timepoint === 5
        @test p.outDir == "/tmp/out"
    end
    let p = Cecelia.parse_export_ome_tiff_params(Dict{String,Any}())
        @test p.zMip === false
        @test p.timepoint === -1                    # sentinel: export every frame
        @test p.outDir == ""                        # empty → default_export_dir()
    end

    # clustPops
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}(
            "popsToCluster" => ["A/root", "B/root", "NONE", ""],
            "valueNameSuffix" => "immune",
            "clusterMeasures" => ["mean_intensity_0"],
            "resolution" => 0.5, "usePaga" => true))
        @test p.popsToCluster == ["A/root", "B/root"]     # NONE + blank stripped
        @test p.valueNameSuffix == "immune"
        @test p.clusterMeasures == ["mean_intensity_0"]
        @test p.resolution === 0.5
        @test p.usePaga === true
    end
    let p = Cecelia.parse_clust_pops_params(Dict{String,Any}())
        @test isempty(p.popsToCluster)
        @test p.valueNameSuffix == "default"
        @test p.resolution === 1.0
        @test p.transformation == "NONE"
    end

    # clustTracks — same shape as clustPops plus popType + minTracklength.
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}(
            "popsToCluster" => ["A/_tracked"], "popType" => "track",
            "minTracklength" => 10, "resolution" => 2.0))
        @test p.popsToCluster == ["A/_tracked"]
        @test p.popType == "track"
        @test p.minTracklength === 10
    end
    let p = Cecelia.parse_clust_tracks_params(Dict{String,Any}())
        @test p.popType == "live"                   # default: _tracked cells
        @test p.minTracklength === 5
    end

    # clustRegions
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "graphSuffix" => "delaunay",
            "clusterMethod" => "kmeans", "numClusters" => 8))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.graphSuffix == "delaunay"
        @test p.clusterMethod == "kmeans"
        @test p.numClusters === 8
    end
    let p = Cecelia.parse_clust_regions_params(Dict{String,Any}())
        @test p.clusterMethod == "leiden"
        @test p.numClusters === 5
        @test p.includeOther === true
    end

    # behaviour.hmm_states
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "modelMeasurements" => ["live.cell.speed"],
            "numStates" => 3, "normaliseTo" => "1", "normaliseMeasurements" => ["live.cell.speed"]))
        @test p.pops == ["A/_tracked"]
        @test p.colName == "cd8"
        @test p.modelMeasurements == ["live.cell.speed"]
        @test p.numStates === 3
        @test p.normaliseTo == "1"
        @test p.normaliseMeasurements == ["live.cell.speed"]
    end
    let p = Cecelia.parse_hmm_states_params(Dict{String,Any}())
        @test p.modelMeasurements == ["live.cell.speed", "live.cell.angle"]
        @test p.numStates === 2
        @test p.normaliseTo == "none"
    end

    # behaviour.hmm_transitions — hmmStates honours nothing → derived from colName
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}(
            "pops" => ["A/_tracked"], "colName" => "cd8",
            "hmmStates" => ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"],
            "includeStart" => true))
        @test p.colName == "cd8"
        @test p.hmmStates == ["live.cell.hmm.state.cd8", "live.cell.hmm.state.mp"]
        @test p.includeStart === true
    end
    let p = Cecelia.parse_hmm_transitions_params(Dict{String,Any}())
        @test isnothing(p.hmmStates)                # nothing → derived from colName in the handler
        @test p.includeSelfTransitions === true
    end

    # spatialAnalysis (spot-check the parsers; each is small).
    let p = Cecelia.parse_aggregates_meshes_params(Dict{String,Any}(
            "pops" => ["A/B"], "maxClusterDist" => 7.5, "minCells" => 10))
        @test p.pops == ["A/B"]
        @test p.maxClusterDist === 7.5
        @test p.minCells === 10
    end
    let p = Cecelia.parse_cell_contacts_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"], "maxContactDist" => 15.0))
        @test p.popsA == ["A/T"] && p.popsB == ["A/B"]
        @test p.maxContactDist === 15.0
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}(
            "neighbourMethod" => "knn", "pops" => ["A/"], "graphSuffix" => "knn6",
            "nNeighbours" => 12, "perTimepoint" => true))
        @test p.neighbourMethod == "knn"
        @test p.pops == ["A/"]
        @test p.graphSuffix == "knn6"
        @test p.nNeighbours === 12
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_cell_neighbours_params(Dict{String,Any}())
        @test p.neighbourMethod == "delaunay"
        @test p.perTimepoint === false
        @test p.neighbourRadius === 30.0
    end
    let p = Cecelia.parse_contacts_meshes_params(Dict{String,Any}(
            "popsA" => ["A/T"], "popsB" => ["A/B"]))
        @test p.maxContactDist === 5.0
    end
    let p = Cecelia.parse_detect_aggregates_params(Dict{String,Any}(
            "pops" => ["A/B"], "clustDiameter" => 20.0, "perTimepoint" => true))
        @test p.clustDiameter === 20.0
        @test p.perTimepoint === true
    end
    let p = Cecelia.parse_neighbour_stats_params(Dict{String,Any}(
            "basisPops" => ["A/B", "A/T"], "nPermutations" => 500))
        @test p.basisPops == ["A/B", "A/T"]
        @test p.nPermutations === 500
    end

    # importImages
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}(
            "src_path" => "/data/foo.czi", "pyramidLevels" => 5,
            "stageLocal" => true, "chunkSize" => 512,
            "maxWorkers" => "2", "jvmHeapGiB" => "16"))
        @test p.src_path == "/data/foo.czi"
        @test p.pyramidLevels == 5
        @test p.stageLocal === true
        @test p.chunkSize == 512
        @test p.maxWorkers == "2"
        @test p.jvmHeapGiB == "16"
    end
    let p = Cecelia.parse_import_omezarr_params(Dict{String,Any}())
        @test p.src_path == ""                       # empty → fall back to img.meta.ori_path in the handler
        @test isnothing(p.pyramidLevels)             # nothing → fall back to pyramidScale
        @test p.pyramidScale === 2
        @test p.maxWorkers == "auto"                 # sentinel, resolved per-reader in the handler
        @test p.jvmHeapGiB == "auto"
        @test isnothing(p.ngffVersion)               # nothing → Settings default (store_layout())
    end
    let p = Cecelia.parse_migrate_legacy_params(Dict{String,Any}(
            "sourceProjectDir" => "/old/proj", "sourceUid" => "abc123", "rscript" => "Rscript"))
        @test p.sourceProjectDir == "/old/proj"
        @test p.sourceUid == "abc123"
        @test p.rscript == "Rscript"
        @test p.mode == "copy"
    end
    # `_merge_meta_preserving_legacy`: the register-time source pointers survive the meta rewrite
    # applied at the end of a successful migration, so a second run (e.g. copy → symlink) still
    # knows what to migrate. Without this the second run hits the `no legacy source` guard.
    let old = Dict{String,Any}("legacySourceDir" => "/old/proj", "legacySourceUid" => "abc123",
                               "legacyRscript" => "/usr/bin/Rscript", "orifilepath" => "/x")
        new_ = Dict{String,Any}("SizeX" => 512, "SizeY" => 512, "name" => "img1")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/old/proj"
        @test merged["legacySourceUid"] == "abc123"
        @test merged["legacyRscript"]   == "/usr/bin/Rscript"
        @test merged["SizeX"] == 512                    # new-side OME content preserved
        @test !haskey(merged, "orifilepath")            # non-legacy old-side keys NOT carried
    end
    let old = Dict{String,Any}("legacySourceDir" => "/old", "legacySourceUid" => "abc")
        # new side wins if it happens to carry a legacy key (would only happen if the Python side
        # started returning one) — nothing gets silently overwritten by the carry-over.
        new_ = Dict{String,Any}("legacySourceDir" => "/from-python")
        merged = Cecelia._merge_meta_preserving_legacy(new_, old)
        @test merged["legacySourceDir"] == "/from-python"
        @test merged["legacySourceUid"] == "abc"
    end
    let old = Dict{String,Any}("legacySourceDir" => "", "legacySourceUid" => "abc")
        # An empty string on the old side is treated as absent (guard on read uses `isempty`).
        merged = Cecelia._merge_meta_preserving_legacy(Dict{String,Any}(), old)
        @test !haskey(merged, "legacySourceDir")
        @test merged["legacySourceUid"] == "abc"
    end
    let p = Cecelia.parse_remove_image_params(Dict{String,Any}(
            "valueName" => "old", "newDefault" => "corrected"))
        @test p.valueName == "old"
        @test p.newDefault == "corrected"
    end
end

# label_props boundary types (audit task #26): `add_obs` refuses non-numeric columns at entry
# with a named-column error; `CategoricalObsColumn` names the drift instead of dying deep in the
# Python subprocess. Pure-Julia tests — no h5ad needed.
@testset "label_props boundary types" begin
    using DataFrames: DataFrame

    # add_obs — numeric passes; String / Bool refused with the column name.
    let df = DataFrame("label" => [1, 2, 3], "live.cell.speed" => [0.1, 0.2, 0.3])
        @test Cecelia._assert_float_convertible(df) === nothing
    end
    let df = DataFrame("label" => [1, 2, 3], "state" => ["A", "B", "A"])
        try
            Cecelia._assert_float_convertible(df); @test false  # should raise
        catch e
            msg = sprint(showerror, e)
            @test occursin("`state`", msg)                      # column name is IN the message
            @test occursin("write_categorical_obs", msg)        # points at the right helper
        end
    end
    let df = DataFrame("label" => [1, 2], "gated" => [true, false])
        # Bool refused deliberately — silently coerces to 0.0/1.0 and misrepresents the semantic.
        @test_throws Exception Cecelia._assert_float_convertible(df)
    end
    let df = DataFrame("label" => [1, 2], "mixed" => [missing, 0.5])
        # `missing` is allowed (maps to NaN in save!).
        @test Cecelia._assert_float_convertible(df) === nothing
    end

    # CategoricalObsColumn — direct construction, dict entry, NamedTuple entry, and error naming.
    let c = Cecelia.CategoricalObsColumn(; name = "state", labels = [1, 2, 3],
                                          values = ["A", "B", missing])
        @test c.name == "state"
        @test c.labels == [1, 2, 3]
        @test c.values[3] === nothing                  # missing → nothing (JSON null → Python)
        @test c.values[1] == "A"
    end
    let c = Cecelia._to_categorical_obs_column(
                Dict("name" => "state", "labels" => [1, 2], "values" => ["A", "B"]))
        @test c isa Cecelia.CategoricalObsColumn
        @test c.name == "state"
        @test c.labels == [1, 2]
    end
    let nt = (; name = "state", labels = [1, 2], values = ["A", "B"])
        @test Cecelia._to_categorical_obs_column(nt).labels == [1, 2]
    end
    let bad = Dict("nmae" => "state", "labels" => [1], "values" => ["A"])
        # missing "name" — error names the missing field
        try
            Cecelia._to_categorical_obs_column(bad); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
    let bad_nt = (; labels = [1], values = ["A"])
        try
            Cecelia._to_categorical_obs_column(bad_nt); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
end

# ── Typed-params ratchet ──────────────────────────────────────────────────────
# Every task's `_run_task` reads its params bag through a `parse_<task>_params(::AbstractDict)`
# helper that returns a `Base.@kwdef` struct — one authoritative statement of the task's contract
# with its inbound bag, so a spec rename becomes a struct-field error at parse rather than a silent
# default at the read site. This is the mechanical enforcement of what MAINTAINABILITY.md → *Task
# params are typed* names as a standing rule (pattern-4 fix from the maintainability audit).
#
# Ratchet shape: same as `frontend/src/utils/cssScenarios.ts` — an exact per-file baseline of the
# tasks NOT YET on typed params. New offenders outside the list fail immediately; a file that leaves
# the list must be removed from `TYPED_PARAMS_MIGRATION_BASELINE`. When the baseline is empty the
# ratchet becomes "no offenders", the shape docs/ui/PRIMITIVES.md → *Re-implementing a scenario is a
# test failure* describes.
@testset "typed params ratchet — _run_task reads params through parse_*_params" begin
    tasks_root = joinpath(@__DIR__, "..", "src", "tasks")

    # Structurally exempt — never need typing:
    #   • `task.jl` — the CciaTask / CompositeTask dispatcher; its base `_run_task` methods delegate
    #     to concrete task methods and legitimately pass the raw bag through.
    #   • `task/composite.jl` — the CompositeTask `_run_task` overloads (split out of `task.jl`);
    #     same exemption reason as the aggregator.
    #   • `testTasks/*` — minimal in-tree fixtures used ONLY by the test suite to exercise the
    #     scheduler; typing them would double their surface with zero production value.
    STRUCTURAL_EXEMPTIONS = Set([
        joinpath("tasks", "task.jl"),
        joinpath("tasks", "task", "composite.jl"),
        joinpath("tasks", "testTasks", "image_task.jl"),
        joinpath("tasks", "testTasks", "incremental_plot_task.jl"),
        joinpath("tasks", "testTasks", "set_task.jl"),
    ])

    # Migration baseline — tasks currently on raw `Dict` access, awaiting their family-arc PR. This
    # list MAY SHRINK, MUST NEVER GROW. Remove the entry in the SAME change that lands the parser.
    # Empty list ⇒ the arc is closed; the ratchet then rejects any regression.
    #
    # The arc closed with PRs #906 (cleanupImages), #909 (editImages), #910 (tracking),
    # #913 (segment), #914 (opticalFlow), #915 (tail: importImages, exportImages, clust*,
    # behaviour, spatialAnalysis). See docs/archive/comment-audit-findings.md for the register.
    TYPED_PARAMS_MIGRATION_BASELINE = Set{String}()

    # Walk every _run_task body via Meta.parseall — robust vs regex (docstrings, nested `end`,
    # helper functions that happen to name their positional arg `params`).
    function _walk(f, expr)
        f(expr)
        if expr isa Expr
            for a in expr.args
                _walk(f, a)
            end
        end
    end

    function _is_run_task_def(e::Expr)
        e.head == :function || return false
        sig = e.args[1]
        sig isa Expr || return false
        # Peel `where` clauses
        while sig.head == :where || sig.head == :(::)
            sig = sig.args[1]
        end
        sig.head == :call || return false
        name = sig.args[1]
        # Bare `_run_task` OR `Foo._run_task` (qualified)
        name === :_run_task || (name isa Expr && name.head == :. &&
                                 name.args[2] isa QuoteNode &&
                                 name.args[2].value === :_run_task)
    end

    _sym_starts(sym::Symbol, prefix::AbstractString, suffix::AbstractString) =
        let s = String(sym)
            startswith(s, prefix) && endswith(s, suffix)
        end

    # Returns (has_parser_def, run_task_calls_parser, run_task_reads_bag).
    # An unavoidable pre-parse guard (a shape check that only makes sense on the raw bag) may carry
    # `# ratchet-ok:` on that exact line + a reason — same escape-hatch discipline as the H5AD/zarr
    # readers (CLAUDE.md → deviations need an inline comment on that exact line). Everything else
    # goes through the parser.
    function _classify(src::AbstractString)
        expr = try
            Meta.parseall(src)
        catch
            return (false, false, false, "unparseable")
        end
        src_lines = split(src, '\n'; keepempty = true)
        _exempt(line::Int) = 1 <= line <= length(src_lines) &&
                             occursin("# ratchet-ok", src_lines[line])

        has_parser_def = false
        calls_parser   = false
        reads_bag      = false
        _walk(expr) do e
            e isa Expr || return
            if e.head == :function
                sig = e.args[1]
                while sig isa Expr && (sig.head == :where || sig.head == :(::))
                    sig = sig.args[1]
                end
                if sig isa Expr && sig.head == :call && sig.args[1] isa Symbol &&
                   _sym_starts(sig.args[1], "parse_", "_params")
                    has_parser_def = true
                end
            end
        end
        _walk(expr) do e
            e isa Expr || return
            _is_run_task_def(e) || return
            body = e.args[2]
            last_line = Ref(0)
            _walk(body) do inner
                if inner isa LineNumberNode
                    last_line[] = inner.line
                    return
                end
                inner isa Expr || return
                if inner.head == :call && !isempty(inner.args)
                    callee = inner.args[1]
                    if callee isa Symbol && _sym_starts(callee, "parse_", "_params")
                        length(inner.args) >= 2 && inner.args[2] === :params && (calls_parser = true)
                    end
                    if callee === :get && length(inner.args) >= 2 && inner.args[2] === :params
                        _exempt(last_line[]) || (reads_bag = true)
                    end
                end
                if inner.head == :ref && !isempty(inner.args) && inner.args[1] === :params
                    _exempt(last_line[]) || (reads_bag = true)
                end
            end
        end
        (has_parser_def, calls_parser, reads_bag, "")
    end

    offenders = String[]      # unexpected — not in either list
    stale_baseline = String[] # still in baseline but actually compliant now (list must shrink)
    for (dir, _, files) in walkdir(tasks_root)
        for f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            rel_from_src = relpath(path, joinpath(@__DIR__, "..", "src"))
            src = read(path, String)
            occursin(r"\bfunction\s+_run_task\b", src) || continue
            (has_parser, calls_parser, reads_bag, err) = _classify(src)
            is_offender = !isempty(err) || !has_parser || !calls_parser || reads_bag

            if rel_from_src in STRUCTURAL_EXEMPTIONS
                # Structural exemptions are silent whether compliant or not — they wouldn't be
                # exempt if the intent were to type them.
                continue
            end
            if rel_from_src in TYPED_PARAMS_MIGRATION_BASELINE
                if !is_offender
                    push!(stale_baseline, rel_from_src)
                end
                continue
            end
            if is_offender
                push!(offenders, rel_from_src *
                      (isempty(err) ? "" : "  ($err)"))
            end
        end
    end

    if !isempty(offenders)
        @error "typed-params ratchet: NEW file(s) on raw `get(params,…)` access.\n" *
               "Add a `Base.@kwdef struct <Task>Params` + `parse_<task>_params(::AbstractDict)` and " *
               "call it once at the top of `_run_task`. See app/src/tasks/cleanupImages/smooth.jl for " *
               "the canonical shape.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "typed-params ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `TYPED_PARAMS_MIGRATION_BASELINE` in the same change that landed the " *
               "parser (the list may shrink, must never grow):\n  " * join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Sanity: the scan must actually find `_run_task` bodies; a wrong root would let real offenders
    # slip through with an empty offender list. cleanupImages + editImages already ship the pattern.
    canonical = String[]
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, joinpath(@__DIR__, "..", "src"))
        rel in STRUCTURAL_EXEMPTIONS && continue
        rel in TYPED_PARAMS_MIGRATION_BASELINE && continue
        src = read(path, String)
        occursin(r"\bfunction\s+_run_task\b", src) || continue
        occursin(r"\bfunction\s+parse_\w+_params\b", src) && push!(canonical, rel)
    end
    @test length(canonical) >= 41   # cleanupImages (6) + editImages (9) + tracking (3) + segment (6) + opticalFlow (2) + tail (15)
end

# ── Cohort-metrics ratchet ────────────────────────────────────────────────────
# Every task that BANKS QC (calls `write_qc`) either registers cohort-comparable metrics in
# `COHORT_METRICS` (app/src/qc_cohort.jl) OR carries a `# COHORT-EXEMPT: <reason>` marker in its
# `.jl` file. Same shape as the typed-params ratchet: MAY SHRINK, MUST NEVER GROW.
#
# The ridges post-mortem exposed the discipline gap: `segment.ridges` shipped `write_qc(...,
# metrics = ...)` calls but was absent from `COHORT_METRICS`, so its per-image counts couldn't be
# outlier-checked across the cohort. The `docs/MODULES.md` "adding a task" checklist already lists
# both (bank QC + add to COHORT_METRICS) as boxes; this makes the second box mechanical.
#
# Marker discipline mirrors the H5AD/zarr readers (CLAUDE.md → deviations need an inline comment
# with a reason). `# COHORT-EXEMPT: <reason>` anywhere in the task's `.jl` file exempts it.
@testset "cohort-metrics ratchet — write_qc callers are in COHORT_METRICS or marked exempt" begin
    tasks_root = joinpath(@__DIR__, "..", "src", "tasks")

    # Baseline: tasks that bank QC today without a COHORT_METRICS entry and no exemption marker.
    # Each entry is a candidate to close — either register cohort metrics OR add an inline
    # `# COHORT-EXEMPT: <reason>` and remove from this list, in one PR. MAY SHRINK, MUST NEVER GROW.
    COHORT_METRICS_BASELINE = Set([
        # `cleanupImages.smooth` — `zeroFracIn` reads acquisition properties (sparsity), not a
        # cohort-comparable process metric. Test at suite.jl also asserts this is deliberately out
        # (`@test !haskey(COHORT_METRICS, "cleanupImages.smooth")`).
        joinpath("tasks", "cleanupImages", "smooth.jl"),
        # `cleanupImages.denoise`, `cleanupImages.stackAlign` — pending review whether these have
        # cohort-comparable output signals; on-list until the owner decides.
        joinpath("tasks", "cleanupImages", "denoise.jl"),
        joinpath("tasks", "cleanupImages", "stack_align.jl"),
        # `opticalFlow.trainSupportDenoise` — companion training task to opticalFlow.train; same
        # decision-pending as the two above.
        joinpath("tasks", "opticalFlow", "train_support_denoise.jl"),
        # `exportImages.ome_tiff` — an EXPORT (writes a downstream file). No per-image processing
        # metric that a cohort comparison would clarify.
        joinpath("tasks", "exportImages", "ome_tiff.jl"),
        # `segment.ridges` — the incident this ratchet exists to catch. Registered by name on the
        # baseline so this PR doesn't couple to a scientific decision about which ridge counts are
        # cohort-comparable. Close in a follow-up.
        joinpath("tasks", "segment", "ridges.jl"),
    ])

    # Extract the `fun_name` a task's write_qc call names (`write_qc(img, "segment.ridges", …)`).
    _fun_from_write_qc = function (src::AbstractString)
        m = match(r"write_qc\([^,]+,\s*\"([a-zA-Z_.]+)\"", src)
        return isnothing(m) ? nothing : m.captures[1]
    end

    offenders = String[]
    stale_baseline = String[]
    coverage = 0
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        src = read(path, String)
        occursin("write_qc(", src) || continue
        fun_name = _fun_from_write_qc(src)
        isnothing(fun_name) && continue
        coverage += 1
        rel = relpath(path, joinpath(@__DIR__, "..", "src"))
        in_cohort = haskey(COHORT_METRICS, fun_name)
        marked = occursin("COHORT-EXEMPT", src)
        if rel in COHORT_METRICS_BASELINE
            if in_cohort || marked
                push!(stale_baseline, "$rel: no longer needs baseline ($(in_cohort ? "in COHORT_METRICS" : "carries COHORT-EXEMPT marker"))")
            end
            continue
        end
        if !in_cohort && !marked
            push!(offenders, "$rel: `write_qc(\"$fun_name\", …)` — add `$fun_name` to `COHORT_METRICS` (app/src/qc_cohort.jl) OR add `# COHORT-EXEMPT: <reason>` in this file")
        end
    end

    if !isempty(offenders)
        @error "cohort-metrics ratchet: NEW task(s) banking QC without a COHORT_METRICS entry.\n" *
               "See docs/MODULES.md → *Cohort metrics*.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "cohort-metrics ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `COHORT_METRICS_BASELINE` in the same change that landed the " *
               "registration/marker (the list may shrink, must never grow):\n  " *
               join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Sanity: a wrong scan root would let real offenders slip through with an empty offender list.
    @test coverage >= 20
end

# ── Julia zarr-access ratchet ────────────────────────────────────────────────
# Companion to `python/cecelia/tests/test_zarr_access_convention.py`, for the Julia side. The
# Python side goes through `zarr_utils`; the Julia side has one sanctioned reader —
# `api/src/image_render.jl` (its header declares itself a "SANCTIONED, NARROW carve-out" for
# lightweight preview renders, with "Do NOT grow this into a general image reader"). Any other
# `using`/`import` of `Zarr`, `EzXML`, or `LightXML` in `app/src/**` or `api/src/**` fails this
# ratchet. Empty baseline today.
@testset "zarr-access ratchet — Julia files don't `using Zarr` outside the sanctioned reader" begin
    banned = (r"^\s*(?:using|import)\s+Zarr(?:\s|$|,)",
              r"^\s*(?:using|import)\s+EzXML(?:\s|$|,)",
              r"^\s*(?:using|import)\s+LightXML(?:\s|$|,)")
    # Sanctioned narrow reader. Its module header pins the "do not grow this" contract.
    exempt = Set([joinpath("api", "src", "image_render.jl")])

    roots = [joinpath(@__DIR__, "..", "..", "app", "src"),
             joinpath(@__DIR__, "..", "..", "api", "src")]
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, joinpath(@__DIR__, "..", ".."))
        rel in exempt && continue
        src = read(path, String)
        for (i, line) in enumerate(split(src, '\n'; keepempty = true))
            for pat in banned
                if occursin(pat, line)
                    push!(offenders, "$rel:$i: `$(strip(line))`")
                end
            end
        end
    end

    if !isempty(offenders)
        @error "zarr-access ratchet: Julia file(s) import Zarr/EzXML/LightXML outside the " *
               "sanctioned reader. On the Julia side, only `api/src/image_render.jl` is allowed to " *
               "hold this — the browser viewer / bricks path goes through the Python `zarr_utils` " *
               "reader indirectly. See CLAUDE.md → *Image / OME-ZARR access*.\n" *
               "Offending line(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    # Coverage: the sanctioned exempt must still exist and still import Zarr — otherwise we're
    # ratcheting on nothing (a moved file would silently make the scan pass).
    exempt_path = joinpath(@__DIR__, "..", "..", "api", "src", "image_render.jl")
    @test isfile(exempt_path)
    @test occursin(r"\busing\s+Zarr\b", read(exempt_path, String))
end
