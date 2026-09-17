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
# ── resolve_pops + labels + track-table + pop_df:track testsets ─────────
# Six sections covering resolve_pops (KDIeEm), resolve_pops has_tracks (data flag),
# has_tracks attribution guard, labels pop_type + count (KDIeEm), track table helpers,
# and pop_df :track (KDIeEm B). Extracted from this file to keep it small enough to
# merge without EOF conflicts on every append. The extracted file loads inside this
# file's aggregating testset scope, so any helpers defined earlier in suite.jl are still
# in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "resolve_labels.jl"))

# ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
# ── plot_summary_data + motion + uns + anisotropy testsets ─────────────
# Four sections covering plot_summary_data (KDIeEm B), motion dimensionality detection
# (2D vs 3D), uns reader (anisotropy grid), and anisotropy notebook readouts. Extracted
# from this file to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers
# defined earlier in suite.jl are still in scope for the extracted fragments (Julia
# includes are lexical).
include(joinpath(@__DIR__, "suite", "summary_motion.jl"))

# ── Branch value_names are NOT label_props value_names ────────────────────────────────────────
# Branching runs on a SEGMENTATION, which need not have a per-cell measurement table: an SHG
# collagen mask is skeletonised but never measured, so it lives in `labels`/`branch_labels`
# while `label_props` holds only the measured cell segmentations. Enumerating branch pops from
# `label_props` therefore found NOTHING — it looked for B__branch / T__branch and missed the
# SHG__branch that exists, so the branch picker came back empty. One image can carry several
# (SHG + DCs, per behaviourUbiTom3P.Rmd), so this is the plural case.
# ── plot suite testsets ────────────────────────────────────────────────
# 12 sections covering the plot family: branch/track value_names from sidecars, plot
# groupBy, plot percent, plot count (raw + proportion), plot raw (per-datapoint export),
# plot statUnit=image (per-image mean), plot matrix (heatmap: profile + crosstab), plot
# attribute grouping, plot_summary_data cross-image + multi-segmentation targets +
# helpers. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the
# extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "plots.jl"))

# ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
# ── tracking + HMM testsets ────────────────────────────────────────────
# Five sections covering the tracking + HMM analysis pipeline: track_props (KDIeEm B),
# track_cell_measures, pop_df pop_type=track (KDIeEm B), HMM states + transitions, and
# HMM entry guards + transition state normalisation. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file
# loads inside this file's aggregating testset scope, so any helpers defined earlier in
# suite.jl are still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "tracking_hmm.jl"))

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
# ── Task-spec ratchets + copy-style testsets ─────────────────────
# 11 sections pinning the task-spec surface: numeric param ranges, tips stay short,
# handler fallback contract, every field declared+documented, optionsFrom picker source,
# showIf conditions name existing param, segmentation reads SEGMENTATIONS, labels picker
# gates only when task needs the MASK, every param carries a tip, spec copy house style,
# and run_stats. Extracted from this file to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for the
# extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "task_spec_ratchets.jl"))

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
# ── OME-ZARR + bioformats2raw + OME-TIFF testsets ────────────────
# Eight sections covering the OME-ZARR / import boundary: chipSelect validation, flow
# boundary weight requires the metrics it is built from, intensity loss is an offered
# dial at the measured default, OME-ZARR metadata reads v2 and v3 alike, bioformats2raw
# chunk / worker+heap / format flags, and OME-TIFF export carries the calibration.
# Extracted from this file to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "omezarr_bf2raw.jl"))

# ── Canonical-helper detectors ────────────────────────────────────────────────
# These exist because both rules below have now cost real debugging time, and neither was enforced.
# The pattern is the repo's existing one (`no_bare_write_h5ad`, `TextIoDeclaresEncodingTest`, the
# store-compressor/staging conventions): scan the SOURCE, fail on a new bypass.

# ── Task validation + platform ratchets testsets ──────────────────
# Five sections: channelSelection params resolve through channel_indices, zarr access
# routes through the canonical helpers, a process exit check also checks termsignal,
# dirPath param validation, and units written into OME-XML are schema-valid symbols.
# Extracted from this file to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "task_validation.jl"))

# ── Runner + HTTP body testsets ─────────────────────────────────────
# Four sections: view profiles (curated sidebar), runner_serve stands down when the
# port is taken, _runner_owns_port recognises a port WE just bound, and an empty
# response body is written through write_http_body!. Extracted from this file to keep
# it small enough to merge without EOF conflicts on every append. The extracted file
# loads inside this file's aggregating testset scope, so any helpers defined earlier
# in suite.jl are still in scope for the extracted fragments (Julia includes are
# lexical).
include(joinpath(@__DIR__, "suite", "runner_http.jl"))

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
# ── Typed params testsets (per task family) ───────────────────────────
# Six sections covering the parse_*_params boundary contract per task family:
# cleanupImages, editImages, tracking, segment, opticalFlow, and remaining families.
# Extracted from this file to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "typed_params.jl"))
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
