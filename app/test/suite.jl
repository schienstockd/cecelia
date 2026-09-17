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
