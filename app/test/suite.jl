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

include(joinpath(@__DIR__, "suite", "task_machinery.jl"))

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


# ── Plugin testsets ────────────────────────────────────────────────────────
# ~15 testsets covering plugin layout / enumeration, manifest + versioning, contributions,
# bundled examples, module staleness, fun_name precedence, custom-module load order,
# shipped examples end-to-end, points-import path, install/remove (P2), resource-pool mapping,
# live pool limit + status snapshot, and the custom-module reload prune. Extracted from this file
# to keep it small enough to merge without EOF conflicts on every append.
# The extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope for the plugin fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "plugins.jl"))


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

# ── bf2raw_series_subdir testset ───────────────────────────────────
# One section pinning the bioformats2raw --series N output-subdir resolver used by the
# import task. Extracted from this file so suite.jl contains only include lines +
# section header comments. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope for
# the extracted fragment (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "bf2raw_series_subdir.jl"))
# Six sections covering the parse_*_params boundary contract per task family:
# cleanupImages, editImages, tracking, segment, opticalFlow, and remaining families.
# Extracted from this file to keep it small enough to merge without EOF conflicts on
# every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope for the extracted
# fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "typed_params.jl"))
# ── Boundary types + ratchet testsets ───────────────────────────────
# Four sections pinning type / convention boundaries across the code base: label_props
# boundary types (add_obs type refusals), typed params ratchet (every _run_task reads
# params through parse_*_params), cohort-metrics ratchet (write_qc callers in
# COHORT_METRICS or exempt), and zarr-access ratchet (Julia files don't `using Zarr`
# outside the sanctioned reader). Extracted from this file to keep it small enough to
# merge without EOF conflicts on every append. The extracted file loads inside this
# file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope for the extracted fragments (Julia includes are lexical).
include(joinpath(@__DIR__, "suite", "ratchets.jl"))
