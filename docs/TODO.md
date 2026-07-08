# TODO

IDs are permanent — never renumber. Add new items by incrementing the highest existing ID.
When an item is fixed, move it to the Fixed section with a date; do not delete it.

Items marked **🔹 needs-input** can't be finished without something only you (D) can provide — a
test asset, a domain-specific expected value, or a decision an agent shouldn't make alone.
Grep `needs-input` to list them.

---

## High priority

**#00003** — **Per-image lockfiles wired into task commit sites**
Today's `with_transaction` (in `model/project.jl`) is a deliberately naive *project-scoped*
guard and is never called. The real (rare) collision risk is two tasks doing concurrent
read-modify-write of the *same* image's `ccid.json` — e.g. a set-level operation fanning out
over images that overlap. A project-wide lock is too coarse (it would serialise unrelated
images); the original R design (`reactivePersistentObject.R`) locked per-object but held the
lock for the entire load→compute→save span.

Recommended approach (better than the original on two counts):
- **Per-image lockfile, co-located with state:** `with_transaction(f, img::CciaImage)` locking
  `joinpath(img._dir, "ccid.json") * ".lock"` (not the project). Different images never block
  each other.
- **Lock the commit, not the computation.** The original held the lock across the whole
  transaction; instead acquire it *only* around the final read-modify-write of `ccid.json`
  (reread → merge task result → write → release), leaving the long bf2raw/cellpose run
  lock-free. This is the key improvement: minimal contention, no multi-minute stale-lock
  window if a process dies mid-run.
- **Wiring:** factor each task's metadata commit (the `versioned_set_field!` + write block in
  `importImages/omezarr.jl`, `importImages/remove.jl`, `cleanupImages/cellpose_correct.jl`)
  into a small `_commit_ccid!(img) do raw … end` helper that wraps the RMW in
  `with_transaction(img)`. Tasks read/compute freely; only the commit is serialised.
- Keep it naive (existence-based) as today; per-image scope already shrinks the stale-lock
  blast radius to a single image.

Deferred: with only per-image tasks today, this collision does not occur in practice — implement
when a set-level mutating task lands.

---

## Medium priority

**#00057** — **Update README for the install / run / update flow (and switch to versioned releases)**
Once the shipping functions are all in — the installer (constructor/pixi-pack), the `pixi run app`
launcher (done), and the update path (`pixi run update` done; in-app button pending) — rewrite
`README.md` for the end-user install → run → update story (it currently predates Pixi). Tie in with
the move from commit-as-we-go to **versioned GitHub Releases** (SHIPPING.md Phase 3): once releases
exist, the README's install section should point at the release installers, not a source checkout.

**#00047** — **Temporal downsampling / overlapping tracklets for behaviour** (deferred)
The old framework computed track measures on the fly, so HMM could push `skipTimesteps` /
`subtrackOverlap` into celltrackR: a way to **downsample** tracks (e.g. treat 10s/frame data like
30s/frame to compare across acquisition rates) and to generate **overlapping tracklets**. The new
stack precomputes `live.cell.*` at native resolution (`track_measures.jl`), so these knobs were
dropped from `behaviour.hmm_states` (they were no-ops). To restore the capability from a different
angle, ideas to explore: (a) a track-measures variant that recomputes speed/angle over every k-th
position (subtrack stride + overlap) and writes `live.cell.speed@kN` style columns the HMM can
select; (b) a resampling step that emits overlapping sub-tracks as first-class rows; (c) a
per-image frame-interval normalisation so cross-rate comparison needs no manual skip. Settle the
storage/UX before building. Not urgent.

**#00063** 🔹 needs-input — **Vendor-specific OME metadata quirks: Olympus/Imaris timelapse interval, MACSIMA channel names**
> **Needs you:** a real `.oir` (Olympus), `.ims` (Imaris), or MACSIMA-exported file to verify
> against — the regex/XPath ports below are untested without one.

The old R `cciaImage.R` has two more format-specific metadata fallbacks beyond the plain OME
`TimeIncrement`/per-plane `DeltaT` case (that one *is* being ported — see the physical-size/
time-interval metadata work referencing `bioformats2raw`'s `OME/METADATA.ome.xml`):
- **`omeXMLTimelapseInfo`** (`cciaImage.R:405-516`) dispatches on the original file extension:
  `.oir` reads a `TIMELAPSE` `StructuredAnnotations` node and pulls the interval from a specific
  axis-step annotation (falling back to per-plane `DeltaT` if that's zero); `.ims` reads a
  `Time_Step`/`Time_Interval_[s]` annotation node. Neither shape is standard OME XML.
- **`omeXMLChannels`** (`cciaImage.R:336-364`) tries `StructuredAnnotations//MapAnnotation` "Dye"
  labels first (MACSIMA-specific), falling back to standard `Channel` `Name` attributes only if
  that's empty.
Both would read from the same `OME/METADATA.ome.xml` bioformats2raw already writes. Deferred
rather than blind-ported because there's no fixture to confirm the regex/XPath shapes still match
current bioformats2raw output. When a real file surfaces: port the R logic into the import task's
OME-XML fallback chain (time: after the plain-`DeltaT` fallback; channels: only when standard
`Channel.Name` is missing/generic), log clearly when a vendor path fires, and add the file as a
fixture per #00014.

**#00037** — **Whiteboard plot integration** (deferred)
Plot nodes in the chain whiteboard (`ChainModule.vue`). A collapsible **"Plots"** box is already in
the palette (under "Module functions") with a "coming soon" placeholder. Notes:
- Plot specs that opt in (`whiteboardCompatible: true`, already on the spec JSONs) become draggable
  palette nodes, like task nodes, that render a summary plot from upstream chain output.
- Needs: a plot node type for vue-flow (mirror `ChainTaskNode`), a way to bind a node to a plot spec
  + its data source (which upstream image/segmentation/pop feeds it), and persistence in the chain
  template. Reuses the `SummaryPanel`/`PlotChart` rendering; the data still comes from `/api/plot_data`.
- Cross-reference `docs/PLOTS.md` for the chart model and `family` routing.

**#00038** — **Auto-facet summary plots by image** (deferred)
Per `docs/PLOTS.md` decision C: when comparing several populations across several images (per_image
with many groups), auto-facet into **per-image columns** above a threshold instead of crowding one
x-axis. Now a straightforward follow-up — Observable Plot facets via `fx`/`fy` (the Vega
`width:'container'`+facet incompatibility that blocked this is gone). Today every group is its own x
position / overlay series (correct, just denser with many groups). Touch point:
`frontend/src/plots/plot.ts`.

**#00041** — **Tiled / spatial-map chart type** (deferred)
The heatmap matrix chart type is **done** (see Fixed #00052). Still deferred: spatial tiled maps
(binned centroid positions over the image field) → `Plot.raster` / `Plot.cell` over binned x/y. Work:
add a `tilemap` `ChartType`, a backend grid aggregation in `plot_data.jl` that bins centroids, and a
`case` builder in `plot.ts`. The engine already supports it — new chart type, not new infrastructure
(`docs/PLOTS.md` §9).

**#00042** — **Richer vis props from the old R version** (deferred)
The old `plotHelpers.R::.formatSummaryPlotData` exposed knobs we don't yet: **y-range override**
(`coord_cartesian(ylim)`), **rotate/flip axes** (`coord_flip`), **axis font size**, custom
titles/axis labels, colour palettes. All cheap in Observable Plot (`plot.ts` `BuildOpts` + the
`SeriesPicker` Options box). Add on request.

**#00004** — **`popDT()` / `summary()` / `tracksMeasures()` read accessors**
REPL-native read accessors for population data tables, experiment metadata, and track measures.
Set-aware (single image or `uids=[...]` across a set). Deferred until analysis tasks are
implemented and producing output. (Track measures now produce output — see #00029 for the
track-specific accessors split out of this.)

**#00029** — **`track_measures()` / `track_info()` REPL read accessors**
The `tracking.track_measures` task now writes **per-cell** measures to the cell `obs` (`live.cell.*`)
and **per-track** measures to a companion `{vn}__tracks.h5ad` (`X`/`var`, one row per track — see
`docs/TRACKING.md`/`docs/DATAMODEL.md`). `pop_df(…; granularity=:track)` already returns the
per-track table; what's missing is the thin REPL convenience layer mirroring R
`cciaImage$trackMeasures()` / `$trackInfo()` (porting spec Step 5):
- **`track_measures(img; value_name, measures=…)`** — read the per-cell measure columns back as a
  `DataFrame` via the `LabelProps` chain, filtered to the requested measures; this is what the HMM
  behaviour module reads as `modelMeasurements`. If absent, hint to run the task (don't recompute).
- **`track_info(img; value_name, pops)`** ✅ **DONE** as **`track_props`** (`app/src/tracking/track_props.jl`):
  per-track aggregate table (one row per `track_id`): `num_cells` + motility + per-measure stats
  (`.mean`/`.median`/`.sd`/`.sum`/`.qUp`/`.qLow`; per-category frequency `{m}.{cat}` for categorical),
  mirroring R `tracksInfo()`. Numeric/categorical auto-detected from column type (no `config.yml`
  map). Compute-on-read on top of the cell table + `{vn}__tracks.h5ad`.
Remaining for this item: only **`track_measures(img; …)`** (the per-cell read convenience layer).
Both headless, no API/Vue. Set-aware like #00004. Pure reads over the chain — no new H5AD access
patterns.

**#00021** — **Filter/gate tracks on their measures + show tracks in napari**
Track lineage (`tracking.bayesian_tracking`) and measures (`tracking.track_measures`) are done, and
the **data layer for one-point-per-track gating is now built**: per-track measures live in
`{vn}__tracks.h5ad` `X`/`var` (gateable), and `pop_df(…; granularity=:track)` returns one row per
track with expand-to-cells via `:cell` (see `docs/TRACKING.md`). Settled here: one-point-per-track
(read-time `granularity` flag), gateability (measures in `X`/`var`), storage (dedicated track h5ad).

The **`track` pop_type** is now a first-class population type (plan Phase 3a/3b, **done**):
`track_props` is the per-track data source, `pop_df(img, "track", pops; granularity=:track|:cell)`
gates directly on track properties with expand-to-cells, and the track gate map persists to
`gating/{vn}__tracks.json` (`save_pop_map!`/`load_pop_map` route by `pop_type`). Verified headless
on KDIeEm B.

**Remaining = napari + UI** (plotting-canvas-and-track-df, Phase 3d–3e):
- **3c — gating API track-awareness** ✅ **DONE.** `gating_api.jl` branches the data source on
  `popType="track"` → `track_props` (`_track_fetch`/`_plot_xy_raw`/`_live_map`); `/channels` returns
  motility `columns` + `cellMeasures` + `trackAggregates`; the gate map routes to
  `gating/{vn}__tracks.json` (CRUD persists there via `pop_type`); the cell-label napari selection
  is not injected into track maps. `track_cell_measures` (package) inverts axis names →
  base cell measures. Verified on KDIeEm B (62 tracks → gate on speed → 32 gated).
- **3d — napari Tracks layer** ✅ **DONE** (backend; visual check pending a live timecourse).
  `POST /api/napari/show-tracks` → `api_napari_show_tracks`; bridge `show_tracks`/`_tracks_matrix`
  builds `[track_id, t, (z,) y, x]` from the H5AD (centroids + `t` + `track_id`, sorted by
  `(track_id,t)`), bin-masks per gated pop, `viewer.add_tracks(color_by="track_id", turbo)`. Ports R
  `show_tracks`; per-pop reconciliation like show-populations (`docs/NAPARI.md`). Assembly logic +
  API load verified; needs a running napari + timecourse image to confirm rendering.
- **3e — track-gating canvas** ✅ **DONE.** `GatingPlots` gained a `popType` prop (the ONE gating
  canvas, reused — no clone); `TrackingModule` renders it `#below-table` with `pop-type="track"`
  alongside the task runner. Store: `selectImage(…, pt)`, `cellMeasures`/`trackAggregates`,
  `showTracks`/`refreshNapari` (routes per-pop re-push by popType). Panels default to a linear axis
  transform for track; the manager hides its point-size option for track (`docs/UI.md`). Frontend
  type-check + build green. **Live end-to-end test pending** (running backend + napari + timecourse).

**#00013** 🔹 needs-input — **Golden-output canary per language boundary** (from `opus-verification-prompt.md` Step 5.5)
> **Needs you:** a domain-confirmed expected output for the fixture (the "known-good" values an
> agent can't authoritatively produce), plus the fixture itself (shared with #00014).

A test that runs one Julia-native module and one Python-routed module against a small fixture image
with known expected output (asserted within tolerance), to catch the Julia↔Python IPC seam breaking
silently. Not writable yet: there is no Julia-native analysis module (e.g. gating), Python is
subprocess-isolated (not PythonCall), and the existing seams (bf2raw import, cellpose) need heavy
fixtures + binaries. Implement alongside the first analysis module (depends on #00004) using a tiny
committed fixture image. The rest of Step 5 (boundary contract, per-module param validation,
lockfile-on-exception, set expansion) is covered in `app/test/runtests.jl`.

**#00014** 🔹 needs-input — **Small committed fixture images for real end-to-end task validation**
> **Needs you:** 1–2 representative tiny microscopy images to commit (you wanted your own small
> test images here). A synthetic OME-TIFF is a fallback, but real sample data is preferred.

Current tests are headless and structural; the only task run end-to-end is `RemoveImage` (no binary).
Import (bioformats2raw) and `cellposeCorrect` have never been validated against a real input because
there is no committed fixture. Add 1–2 tiny images under `app/test/fixtures/` (e.g. a few-pixel,
2–3 channel OME-TIFF, optionally with a small z/t) — KB-sized so they're cheap to commit and fast to run.
Enables real assertions:
- import → `SizeC/SizeT/SizeZ`, channel names, and the OME-ZARR actually written to `0/{uid}/`;
- `cellposeCorrect` → corrected zarr produced + `cpCorrected` filepath version registered.
Gate these behind a presence check for the external deps (`bioformats2raw_bin()`, `napari/.venv`) and
**skip** when absent, so the core suite stays dependency-free and CI-portable. Shares fixtures with #00013.

---

**#00020** — **Set-scope / incremental node subprocesses not killed on chain cancel**
The per-image cancel path (#00016) kills running subprocesses. Set-scope (`_run_set_scope_node!`)
and incremental (`_run_incremental_node!`) runners call the multi-image `_run_task` directly with
`on_process = _ -> nothing` and are **not** registered in `_TASKS`, so `cancel_chain_run!` can't
reach their subprocesses mid-run (the between-node flag still stops not-yet-started ones). No real
set-scope subprocess task exists yet (only mock/plot tasks), so impact is currently nil. When the
first real set-scope subprocess task lands (e.g. HMM training), give the multi-image `_run_task`
path a `TaskRecord` + `chain_run_id` so it's cancellable like the per-image path. Low priority.

---

## Low priority

**#00003** — **Re-enable interactive pan/zoom on gating plots**
Gating plots currently lock the regl camera (`cameraIsFixed: true`, no x/y scales) so the WebGL
points align exactly with the canvas2D overlays (contours, gates) — providing scales made regl
re-fit/zoom and drift the dots off the gates. To restore pan/zoom with correct alignment, the
overlays (`PlotLayers`, `GateOverlay`) must replicate regl's full screen transform
(`projectionLocal · cameraView · model`, from the `view`-event camera matrix + `viewAspectRatio`)
instead of the plain extents mapping, and invert it for gate hit-testing. See the alignment bullet
in `docs/UI.md`.

**#00002** — **Auto-follow in task manager**
Selecting the newest running task in `TasksModule.vue` (`/tasks`) when a task starts does not
work. Approaches tried: `watch`, `watchEffect`, `computed+watch`, WS event listener
(`ws.on('task:status', ...)`). Likely a Pinia/Vue 3 deep reactivity edge case with array
element property tracking.

**#00027** — **`testTasks.*` task fun_names/files are still camelCase**
The test tasks `testTasks.imageTask`/`testTasks.setTask`/`testTasks.incrementalPlotTask` (files
`tasks/testTasks/{imageTask,setTask,incrementalPlotTask}.{jl,json}`, structs `TestImageTask`/
`TestSetTask`/`IncrementalPlotTask`) predate the snake_case convention (see `#00026`,
`feedback_julia_naming`). Rename to snake_case `fun_name`s + files (e.g. `testTasks.image_task`,
`tasks/test_tasks/image_task.{jl,json}`) — structs stay PascalCase. Touches `_spec_path`/
`_fun_name_map` in `task_registry.jl`, the `Cecelia.jl` includes, and any test references. Not
important (test-only scaffolding, no user-facing impact) but should be fixed for consistency;
batch it rather than churn standalone.

---

## Fixed

**#00069** — **Metadata resync silently reverted corrections; meta-edits missed napari's source of truth** (2026-07-04)
Audit-driven cleanup of the physical-size/timing feature (#00064/#00065). Three coherence bugs, one
root cause — the writer (`meta/set`) and the re-reader (`resync_ome_meta!`) disagreed about where
calibration lives. (1) **Resync was destructive.** `resync_ome_meta!` re-read the `"default"` zarr and
merged it with `overwrite=true`, unconditionally clearing `_OME_DERIVED_META_KEYS` first — so it reverted
the ImageJ-TIFF Z auto-fix and any human correction (both ccid.json-only, not reproducible from the zarr)
back to bioformats2raw's raw value, and dropped the `PhysicalSizeZ_raw` "confirm me" marker. Its docstring
even claimed idempotency it didn't have. Fixed: added `overwrite` to `_merge_zarr_meta_into_ccid!`; resync
now calls it **fill-only** (`overwrite=false`) — fills genuinely-absent keys, never clobbers an existing
one (channel names included). (2) **`meta/set` wrote the wrong zarr.** It patched `img_filepath(img)`
(the *active* version) — but calibration lives only in the `"default"` zarr (processed variants carry a
flat NGFF layout, no unit, no OME-XML), so on a processed-active image the patch silently no-op'd, and even
on a fresh import the authoritative default was left uncorrected → a later resync couldn't re-derive the
edit. Repointed both `update_ome_scale!`/`update_ome_xml_pixels!` at `img_filepath(img, "default")`.
(3) **Unit edits never reached the zarr.** `update_ome_scale!` rewrote scale numbers but not NGFF axis
`unit`s, and `read_ome_metadata` won't trust a t-scale (or read a spatial unit) without one — so a
unit-only or time-interval edit vanished on resync. `update_ome_scale!` now takes a `units` map and
rewrites axis units too. (4) **Import corrections never reached the zarr.** The ImageJ Z-spacing fix and
the per-plane DeltaT time interval were written only to ccid.json, so `img_physical_sizes` (analysis)
used the corrected numbers while napari — reading the zarr — kept showing the raw spacing / "t = N". The
import now copies them back into the zarr (only when something diverges: a corrected Z, or a timelapse),
so the viewer matches what analysis computes with; the value stays flagged for human confirmation. The
field→zarr translation now lives in ONE place — `sync_zarr_calibration!` — used by both the importer and
`meta/set` (was duplicated). Also hardened: the `<Plane>` DeltaT scan matches non-self-closing tags, and
the OME-XML attr insert anchors on the `<Pixels` token (handles an attribute-less `<Pixels>`). Net:
`meta/set`, `resync`, and import all agree on the default zarr as the single source of truth; edits and
auto-corrections persist, resync only backfills. Tests: new "OME metadata read/edit/resync" testset in
`runtests.jl` (`_delta_t_fallback` incl. non-self-closing, `read_ome_metadata` placeholder/fallback,
`update_ome_scale!` ratio+units, `update_ome_xml_pixels!`, `sync_zarr_calibration!`, merge
fill-only-vs-overwrite, `resync_ome_meta!` end-to-end) — the feature previously shipped with none.
Docs: API.md, OBJECTMODEL.md.

**#00068** — **Every whiteboard chain run failed: "Chain template not found"** (2026-07-03)
The API layer reads/writes chain templates under `<proj>/settings/chains/` (`_chains_dir_for_project`,
after chains were consolidated into `settings/`), but the package's `_chains_dir` (`chain.jl`) still
pointed at the legacy `<proj>/chains/`. So the whiteboard **saved** a template via the API (into
`settings/chains/`, "Chain X saved" succeeded) and then `run_chain` **loaded** it via the package
(from `<proj>/chains/`) → `ErrorException("Chain template not found: …/chains/3P.json")` on every
run. Repointed `_chains_dir` at `settings/chains` to match the API, mirroring its legacy-location
migration so REPL-first / pre-`settings/` projects are picked up too (templates, runs, and .cache all
move together). Regression guard: the round-trip test now asserts the template lands under
`settings/chains/` and not `chains/`. Docs: SCHEDULER.md (*Template vs run record* — added a
"paths must match the API" note).

**#00067** — **Per-image task-param memory (R moduleFunParams port); replaces cross-project localStorage** (2026-07-03)
Module-page task params were kept in `localStorage` keyed by `module:task` — not scoped by project,
so switching projects left the previous project's image/channel/pop selections in the form. The old
R app instead remembered params **per object in `ccid.rds`** (`saveModuleFunParams`/`moduleFunParams`).
Ported that: params live in `ccid.json` under `meta["funParams"]["<fun_name>"]`, saved on run to
each processed **image** (record of what produced it) *and* the **set** (shared last-used default),
via dir-based `write_module_fun_params!`/`read_module_fun_params` (`app/src/model/image.jl`) — a
targeted `ccid.json` read-modify-write (same idiom tasks use for `filepath`), dir-based so the set
never loads all its images. `api/src/sockets.jl` (`_remember_fun_params`) saves on `task:run` (needs
the new `setUid` in the message); `GET /api/tasks/funparams` resolves image → set → none. `TaskRunner`
fetches from it and populates image → set → task-defaults (imageUid passed only when exactly one
image is selected — the form is one config for all selected). No more localStorage for params →
never leaks across projects. Tests: funParams round-trip in `runtests.jl`. Docs: OBJECTMODEL.md,
MODULES.md, API.md.

**#00066** — **Whiteboard: set tasks weren't picnic nodes; downstream input value_name couldn't be pre-wired** (2026-07-03)
Two chain-authoring gaps surfaced while building real pipelines. (1) **Scope wasn't inherited from
the task spec.** HMM/clustering tasks declare `"scope": "set"` in their JSON, but the whiteboard
drop handler hardcoded `scope='image'` (and `chain_node`/`ChainNode` defaulted to `"image"`), so
they dropped in as ordinary per-image nodes instead of picnic nodes. The task JSON is now the single
source of truth: `_task_default_scope(fn)` (`chain.jl`, delegating to `task_scope`) resolves an
empty scope from the spec in `ChainNode`/`chain_node`/`_node_from_dict`; the frontend uses
`def.scope ?? 'image'`. An explicit scope and a frozen template's stored scope still win. (2)
**Downstream nodes couldn't select an upstream node's output value_name** (`import → cellposeCorrect
→ afDriftCorrect` — `cpCorrected` doesn't exist on the image until the chain runs). Unified the
producer output contract as a top-level `"outputValueName"` in the JSON (added to
`cellpose_correct`/`drift_correct`/`af_correct`; read via `_spec_output_value_name(task, default)`
instead of a hardcoded literal). On drawing an edge, `ChainModule.propagateValueName` prefills the
downstream node's field-compatible `valueNameSelection` params with the upstream output
(auto-populated, editable); `paramContext.extraValueNames` + `ParamRenderer` (union into the option
list, keep an already-valid edge value) make the not-yet-on-disk name selectable. Docs:
SCHEDULER.md (*Node scopes*, *Value-name propagation*), MODULES.md. Tests: scope-from-spec +
output-value-name-from-spec in `runtests.jl`.

**#00065** — **Task-manager "cancel all" + per-project task-list scoping; resync silently no-op'd against processed variants** (2026-07-03)
Three related fixes to today's real-data testing round. (1) Added a "cancel all" button next to
"clear finished" in the module Tasks sidebar — cancels every running/queued task for that
module+project via the same per-task `task:cancel`/`chain:cancel` path, deduping chain runs. (2)
`TaskList.vue` showed a previous project's (e.g. cancelled) tasks after switching projects —
`useTaskStore().forModule()`/`clearFinished()` only filtered by module, not project. Added an
optional `projectUid` filter, threaded through `TaskList`/`TaskRunner`/`ImageTable`; the global
`/tasks` manager intentionally keeps the unscoped cross-project view. (3) The new
"resync flagged from file" button (see #00064) silently did nothing on images with a processed
variant (drift/cellpose-correct) active — `resync_ome_meta!` read `img_filepath(img)` (whatever's
`active`), but those outputs use the flat NGFF layout with no axis `unit` and no OME-XML sidecar,
which `read_ome_metadata` (Julia; unlike the Python `zarr_utils.py` reader) doesn't detect at all —
it only understands the bioformats2raw nested layout. Fixed by always resolving the `"default"`
(original import) zarr instead — physical size/timing are acquisition properties, unaffected by
downstream correction tasks anyway. See CLAUDE.md → *OME-ZARR dual-format*.

**#00064** — **Physical-size & timing metadata: review, edit, and per-image warnings** (2026-07-03)
Root-caused a napari bug on a real 3D timecourse: collapsed Z-spacing and "t = n" instead of real
time. Two independent causes — `bioformats2raw` converts a source TIFF's non-micron calibration
unit (e.g. ImageJ `unit=inch`) correctly for X/Y but not Z, and this file had no genuine time
interval anywhere in its metadata. Since auto-detection can never cover every vendor quirk, built a
full review/edit system instead of a point fix: an ImageJ-tag Z-spacing auto-correction at import
(`read_imagej_physical_size_run.py`), an OME-XML per-plane `DeltaT` fallback (ports the old R
`omeXMLTimelapseInfo` crutch), raw/nullable `PhysicalSizeX/Y/Z`/`PhysicalSizeUnit`/`TimeIncrement`/
`TimeIncrementUnit` in the image payload, a warning icon + `PhysicalSizeDialog.vue` modal (Apply /
Copy-to-selected / Fill-flagged, mixed-value detection) reachable from every module's image table,
and `POST /api/images/meta/set` which keeps all three metadata copies in sync — `ccid.json`,
the OME-ZARR's own `.zattrs` NGFF scale, and `OME/METADATA.ome.xml`'s `<Pixels>` attributes (napari
reads `TimeIncrement` from the XML unconditionally, with no `.zattrs` fallback). Also added
`resync_ome_meta!`/`POST /api/images/meta/resync` + a table header button to backfill these fields
for images imported before this metadata was tracked at all, without a re-import. Deferred
vendor-specific quirks (Olympus/Imaris timelapse annotations, MACSIMA channel-name metadata) to
**#00063**. See `docs/OBJECTMODEL.md`, `docs/API.md`, `docs/UI.md`.

**#00036** — **Universal "Analysis board" page** (2026-07-01)
A standalone canvas at `/analysis` that hosts the SAME plot canvas as the per-module pages but with
the **module filter off** — every plot spec is offerable, so plots can be combined across modules /
images / segmentations on one board. Thin as anticipated: `SummaryCanvas` already supported the
universal mode (no `module` prop → `loadSpecs()` fetches all specs, persistence keys use
`summary:universal`). Added `AnalysisModule.vue` (a `ModuleLayout` with image multi-select + a
`#below-table` `SummaryCanvas`, no TaskRunner — visualise-only), registered the `/analysis` route,
and dropped the `disabled/soon` flags on the existing sidebar entry (now `requiresProject`). Image
selection namespaces under the `analysis` scope; the populations endpoint already unions across the
selected images/segmentations. (Future interactive-panel routing via `PlotSpec.family` is still
pending — see the interactive-panel family work.) See `docs/UI.md` → "Analysis-plot canvas".

**#00062** — **`pixi install` fails on macOS-arm64 building `cvxopt` from source** (2026-07-01)
A tester on macOS-arm64 got past the installer download but `pixi install` failed: `Failed to build
cvxopt==1.3.3 … fatal error: 'umfpack.h' file not found`. `cvxopt` is a transitive dep of `btrack`,
and PyPI ships no macOS-arm64 wheel, so the solve fell back to the sdist and tried to compile the
UMFPACK extension against SuiteSparse headers that aren't on a stock Mac. Fixed by declaring
`cvxopt = ">=1.3.1"` in `[dependencies]` (conda-forge, prebuilt on every platform) — pixi maps the
conda package onto btrack's PyPI requirement so it's never built from source. Lock regenerated
(pulls the SuiteSparse stack in as conda pkgs). Needs a fresh RC to ship. See SHIPPING.md.

**#00061** — **Canvas plot cleanup phase: export + gate-plot polish** (2026-07-01)
Canvas-export/theme cleanup, done as one pass (incl. several follow-ups from testing):
- **Gate plot x-axis label no longer clipped by the footer.** The gate panel's overhead (stacked
  axis controls + the plot's `min-height:200` + 68px label margins) overflowed the default 440px
  panel, so the panel's `overflow:hidden` clipped the x-axis label right at the footer row (it
  wasn't a true z-overlap — `CanvasPanel` is a flex column). Dropped `.panel-plot` `min-height` to
  150px so plot + bottom margin + footer all fit.
- **Exported PNGs are crisp — and the scatter is re-rendered, not upscaled.** `plots/export.ts` has
  two DPR-aware scales: `EXPORT_SCALE = min(4, 2×DPR)` for the SVG plots (vector → crisp at any
  factor) and a higher `RASTER_SCALE = min(8, 4×DPR)` for the WebGL/canvas composites. The point
  cloud can't be upscaled crisply from its CSS×DPR backing store, so `plotHostToImageURL` accepts an
  `opts.hiRes(cv, scale)` resolver and EVERY stacked canvas re-renders itself at export scale:
  `ScatterGL.exportCanvas` uses regl-scatterplot's `export({scale})` (on a transparent ground, so the
  cloud composites over the fill without a second opaque layer hiding it — this was the gate plot's
  "points not shown"); `PlotLayers`/`GateOverlay` re-paint their canvas2D content onto a scale×
  offscreen canvas (the gates were previously rendered only at screen DPR → "gates really low res").
  Wired for both UMAP (single WebGL canvas) and the gate plot (WebGL + two canvas2D layers).
- **UMAP now appears in the exported PNG.** Root cause was in the overlay pass, not the WebGL
  capture: `elementToImageURL({blankCanvases})` hid the `<canvas>` but kept its opaque ancestor
  background (`.uv-plot { background:#0d0b1a }`), which painted over the separately-composited
  scatter. Now the canvas's ancestor chain has its background cleared in the clone (siblings like the
  legend keep theirs).
- **Gate-plot export no longer clips the axis names.** The x/y axis labels are positioned at
  negative offsets *outside* `.panel-plot`; capture now targets a `.plot-capture` wrapper whose
  padding holds the label margins, so the axis names land inside the exported region.
- **UMAP + heatmap honour the dark-theme knob.** `ClusterHeatmapPanel` merges its `vis` prop into
  the `BuildOpts` (PlotChart already themes off `opts.darkTheme`); `UmapView` derives the scatter
  ground + label chip + **legend box background/ink** from `vis.darkTheme` (light mode gave dark ink
  on the app's dark panel → unreadable pop names) and threads a `backgroundColor` prop into
  `ScatterGL` (was hardcoded `#0d0b1a`). `ClusterPlots` passes `panelVis()` to the heatmap and into
  the interactive-view context so both follow the pop-manager's global/local styling scope.
- **Gate plot loads/renders on first open.** Two parts: `ScatterGL.render()` now re-syncs the regl
  size (`resize()`) on every draw, so a freshly-opened floating panel that hadn't laid out when the
  first draw fired no longer stays blank until a reflow; and `GatePlotPanel`'s store-readiness fetch
  is a single `{ immediate: true }` watch on `[columns, imageUid, valueName]`, so the first
  appearance loads whether the gating store became ready before or after the panel mounted (it used
  to stay empty until the user nudged a dropdown).

**#00060** — **Added LICENSE (GPL-3-or-later) + THIRD_PARTY acknowledgements** (2026-06-30)
Added the GPLv3 text as `LICENSE` (the license is **not a free choice** — the parent
`schienstockd/cecelia` R package this ports is `GPL (>= 3)`, so the port inherits
**GPL-3-or-later**), declared `license = "GPL-3.0-or-later"` in `app/Project.toml` and the pixi
`[workspace]` metadata, and added `THIRD_PARTY.md` listing derived/bundled deps + their licenses.
**celltrackR (GPL-2.0)** is called out explicitly: `app/src/tasks/tracking/track_measures.jl` is a
cited reimplementation of its measures — a from-scratch algorithm port isn't strictly a derivative
work, but we carry the credit + license notice anyway. Also acknowledged scanpy/anndata/leidenalg
(GPL-3), btrack, cellpose, napari, zarr, scikit-image, PyTorch, the Julia deps, and the Vue
frontend stack. AGPL not needed (local desktop tool, no SaaS loophole). README gained a "License"
section pointing at both files.

**#00056** — **Removed PythonCall (dropped the separate CondaPkg conda env)** (2026-06-30)
PythonCall was a declared-but-**unused** dependency in `app/Project.toml` — never imported or called
anywhere (reads use `HDF5.jl`; H5AD writes and all tasks use Python *subprocesses* via
`python_bin_path()`). Its only effect was pulling in `CondaPkg`, which built `app/.CondaPkg` (202 MB)
+ `api/.CondaPkg` (88 MB) conda envs on precompile — a whole second Python environment backing
nothing. Removed it (`Pkg.rm`), re-resolved both Manifests (CondaPkg/MicroMamba/pixi_jll gone), and
deleted the `.CondaPkg` dirs. No drift risk, ~290 MB saved, faster `instantiate`. Cleaner than the
original plan (reconfigure PythonCall onto the Pixi env), since the dep was dead. Also corrected stale
docs (ARCHITECTURE/DATAMODEL/POPULATION + the `label_props.jl` header) that wrongly said H5AD writes
go through PythonCall — they go through a subprocess.

**#00055** — **Napari tracks: per-segmentation overlay + timestamp + label naming** (2026-06-29)
Reworked the napari track overlay for the behaviour phase: (1) shows **every** segmentation's tracks
(A/B/C), not just the active one — `show-tracks` takes `valueNames` and renders one Tracks layer per
segmentation, named `({value_name}) Tracks /_tracked`; (2) a segmentation's tracks = its `_tracked`
cells (all `track_id>0`) read directly from the cell h5ad (no gating map / track-gate dependency —
that's the deferred track-gating phase); (3) the master "Show tracks" toggle is replaced by a
per-segmentation **`pi-directions`** toggle in the ViewerPanel labels list (row icons hover-reveal,
active stays visible; per-image state in `settings.get/setTrackVisibility`). Also: (4) **timestamp**
overlay restored for timecourse data (elapsed `H:MM:SS` from OME `time_increment`, follows the t
slider — ports `add_timestamp`); (5) labels layer named `(C) Labels` not `(C.zarr) Labels`; (6) gated
track pops (TEST/SDGF, `{vn}__tracks.json`) shown under a separate **global** `pi-directions` toggle
(`settings.napariShowGatedTracks`, like Show populations, re-pushed on track-gate edits) — distinct
from the per-segmentation `_tracked` directions toggles; (7) label **delete** is inline two-click
confirm (trash → red `!`, second click within 3.5 s) instead of a browser popup. Bridge `show_tracks`
rebuilt for per-pop `value_name` (matrix cached per vn). Docs: NAPARI.md.

**#00054** — **Napari colour-by an obs column (tracks + labels)** (2026-06-29)
The napari tracks AND labels layers can be shaded by any cell obs column (HMM state, a track
measure, a cluster). Bridge: `show_tracks` gained `color_by` (per-**vertex** values mapped via
`_tracks_matrix`'s new per-vertex labels + `_read_label_column`; turbo categorical / viridis
continuous), and a new `colour_labels` command applies a `DirectLabelColormap`
(`_labels_color_dict`: Okabe–Ito per level / viridis percentile; original cmap remembered for
reset). Ports old `show_tracks(color_by=…)` + `show_channel_intensity`. API: `colorBy` on
`POST /api/napari/show-tracks` + new `POST /api/napari/colour-labels`. UI: a "Colour by" dropdown
in ViewerPanel (obs columns from `/api/gating/channels`), remembered (`settings.napariColourBy`),
re-applied on open. Verified routes wired + napari 0.7.1 APIs present. **Note:** the old
`split_tracks` (one layer per value) is deferred to the upcoming Leiden track-clustering phase — a
code note in `show_tracks` marks where it goes.

**#00053** — **Compare summary plots "by attribute"** (2026-06-29)
The canvas "compare" control gained a fourth mode (alongside this image / per image / pooled): **by
attribute**, grouping the selected images by one or more shared image attributes so images sharing the
value pool into one series labelled by it. A primary + optional interaction attribute combine into one
key (`Treatment` × `Mouse` → `alpha.1`), mirroring the old R `paste0(axisX, ".", interaction)`;
`groupAttr` accepts a name or an array. Backend: `_series_groups(df;
attr_map)` remaps each row's image key (uID → attribute value; images lacking the attribute fall back
to their uID), threaded through `_summary_agg` + the cross-image `plot_summary_data` methods + a
`groupAttr` field on `/api/plot_data`. New `GET /api/plots/attrs` lists a set's attribute names+values
for the picker. Per-track aggregation is untouched (it happens in `pop_df` before series grouping).
Works on every chart type. Verified on set jFWePN: group-by-`Mouse` (shared) pools 3 images into one
series (n=1809); group-by-`Treatment` (distinct) keeps 3, relabelled A/B/C. Tests + docs (`PLOTS.md`
§1/§3, `API.md`).

**#00052** — **Heatmap (matrix) chart type — profile + crosstab** (2026-06-29)
Generic `heatmap` `ChartType` backed by `chartType:"matrix"`. `_matrix_agg` in `plot_data.jl` pools
the whole `pop_df` frame into one grid in two modes: **profile** (rows = `measures`, cols = a
categorical `category` column, cell = mean, `zscore` standardises rows → diverging RdBu — the "state
signature") and **crosstab** (a `"from<sep>to"` categorical → transition matrix, count or row/col/
total-normalised; the hybrid joins states with `.` so the first `_` splits prev|cur). Threaded through
all four `plot_summary_data` methods + `/api/plot_data` (`matrixMode`/`measures`/`category`/`separator`/
`zscore`/`matrixNormalize`). Frontend `buildHeatmap` (`Plot.cell` + viridis/diverging + value text +
continuous-legend overlay in `PlotChart`); the panel offers heatmap independent of measure type, with
Mode / Category (discovered categorical obs cols; smart default per mode) / z-score / Normalize in the
options popover. Plot defs `state_signature.json` (profile) + `transition_matrix.json` (crosstab).
Tests: in-memory profile + crosstab + z-score + row-normalise + error cases (`runtests.jl`). Docs:
`PLOTS.md` §9, `API.md`. Remaining heatmap follow-up (spatial tiled map) tracked in #00041.

**#00051** — **HMM module: cross-segmentation pops + measure picker + composite state name** (2026-06-27)
Review of the HMM module surfaced five issues, all fixed:
1. **Cross-segmentation population selection** (the big one). Dropped the single `valueName` param
   from `behaviour.hmm_states`/`hmm_transitions`. `pops` is now an across-segmentations multi-select
   (`popType:"live"`) sourced from `/api/plots/populations` (includes the derived `/_tracked`),
   value_name-prefixed (`A/_tracked`, `B/qc`, …). The handlers call `pop_df(imgs, uids, "live",
   pops)` (no `value_name`) so one run fits tracked A, B, C from every image jointly — matching the R
   `popDT`/popUtils semantics. Verified e2e on set jFWePN: 6216 cells pooled across A/B/C × 3 images.
2. **Tracking vs object measures muddled / raw `mean_intensity_0`.** `labelPropsColsSelection` now
   renders two groups — Tracking (obs `live.*`) and Object (var cols) — and maps intensity columns to
   channel names (`mean_intensity_0` → `channelNames[0]`, mirroring Julia `_channel_label`); the
   stored value stays the raw column.
3. **Composite didn't pass the state name to transitions.** The set-scope `CompositeTask` now threads
   `hmm_states`' `stateColumn` → the transitions step's `hmmStates`; the field is `hideInComposite`
   (new flag, honoured by `api_task_definitions`) so the composite form doesn't show it.
4. **Standalone transitions `hmmStates` field empty.** Root cause: the measure picker queried a
   hardcoded `valueName="default"` (the tracked sets are A/B/C). It now derives the segmentation from
   the selected pop's prefix, so the `live.cell.hmm.state.*` columns appear.
5. Picker `valueName` resolution unified across `popSelection`/`labelPropsColsSelection`.

**#00046** — **Canvas panel box now collapses fully** (2026-06-27)
`CanvasPanel.vue` collapse only hid `.panel-body` via `v-show`, but the panel kept its
fixed/resized `height` + `min-height: 320px`, leaving an empty box. Fixed: `.panel.collapsed`
sets `height:auto !important; min-height:0; resize:none` (shrinks to header), and `persist()`
skips while collapsed so the ResizeObserver doesn't save the collapsed height. Collapse state is
local (not persisted) — matches PopulationManager/SeriesPicker; persist it later if desired.

**#00045** — **Roadmap + milestones + AI disclaimer** (2026-06-27)
`docs/ROADMAP.md` = temporary forward goals (behaviour/HMM → cell & track clustering → freeze v1.0
→ packaging that bootstraps the Python env like old R cecelia → GitHub self-update + post-v1
backlog). `docs/MILESTONES.md` = durable append-only ledger of what landed + how packaged (seeded
M1 = analysis spine). `README.md` carries the AI-development disclaimer (Claude Code / Opus +
Sonnet / Garvan enterprise license; roles, sources, approach). Note: track measures + per-track
gating already exist (`tasks/tracking/track_measures.jl`, `{vn}__tracks.h5ad`).
🔹 **needs-input (D)** (tracked in ROADMAP): package licence (Phase 3); GUI shell + first-class
platforms + GPU matrix (Phase 4). Original-cecelia citation in README: Nat Commun 2025,
doi:10.1038/s41467-025-57193-y.

**#00044** — **Canvas panel/selection persistence + picker hierarchy** (2026-06-27)
Three UI bugs. (1) **Plots duplicated on navigation** — `GatingPlots` `onMounted` ran `add(); add()`
unconditionally, but panels persist per `gate:${popType}`, so Gate↔Tracking re-mounts stacked two
more each round (2→4→6…). Now seeds defaults only when the canvas is empty. (2) **Selected pops not
saved across navigation** (behaviour canvas) — panels persisted but the GLOBAL-scope selection / vis
/ scope were component refs (`gSel`/`gVis`/`scope`), reset on remount → eye-selected pops vanished.
Added a per-canvas `shared` blob to the `canvasPanels` store (exposed via `useCanvasPanels`);
`SummaryCanvas` and `GatingPlots` back their global-scope state with it, so selections + options
persist. (3) **No hierarchy in the summary picker** — `SeriesPicker` showed a flat list
(`/qc/sub` as "qc/sub"); now indents by tree depth + shows the leaf name (mirrors the gating
`PopulationManager`). (4) **Per-panel chart options not saved** (chart type reverted violin→histogram
on nav) — `SummaryPanel`'s chartType/measure/bins/normalize/errorMetric were component refs; now in
the persisted panel `state` (passed as the `ui` prop, computed get/set). (5) **General mechanism** so
this stops recurring: `composables/useViewState.ts` (Shiny-`reactiveValues`-style) — seed a `defaults`
literal into a store-backed bag, get `toRefs`; every option declared there persists automatically with
no per-field wiring. `SummaryCanvas` + `GatingPlots` use it for canvas-level options (compareMode,
scope, selection, vis, gating highlights/line-width/…). Documented in docs/UI.md ("Persisting view
state — the three scopes"). (6) **Picker hierarchy was empty** because the derived `_tracked` pop was
injected only at ROOT — tracking `qc` didn't produce a visible `/qc/_tracked`. `plot_population_groups`
now injects each derived pop at root AND **as a child of every stored pop** (`/qc/_tracked` = qc's
tracked subset), so the picker shows real nesting and the tracked subset of any pop is plottable
(verified: `/qc/_tracked` track-speed → 37 tracks). The tracker itself was fine (C.h5ad had track_id +
lineage, 521/752 cells tracked). (7) **Documented the persistence RULE** for future work — CLAUDE.md
+ MODULES.md ("RULE: persist every user-settable option"): new pages MUST use `useViewState`, never a
bare `ref()`. 538 Julia tests pass; frontend builds green. *Redraw-on-gating:
the mechanism looks correct (gate outlines reactive on `g.flat`; `popVersion` drives refetch) — the
duplicate-panel bug was the likely cause; re-test on a clean canvas.*

**#00043** — **Track pops in the plot picker + ported R plot adjustments** (2026-06-27)
Two things. (1) **Track-gated pops now show in the summary picker.** A track-granularity plot's
picker queried a single pop_type, so `track` gates (e.g. `TEST` in `{vn}__tracks.json`) were invisible
(only `live` showed). Fix: `/api/plots/populations` takes `granularity`; `granularity="track"` unions
`live` (cell gates + derived `/_tracked`) and `track` pops, tagging each with `popType`. The picker is
granularity-aware (track if ANY module spec is track-granularity — `specs[0]` may be a cell plot, the
original bug). `tkey`/`SeriesTarget` carry popType; `SummaryPanel` groups series by popType and issues
one `/api/plot_data` per group, merging series — so `/_tracked` (live) and a track gate sit on one plot.
The picker logic lives in the **package** (`plot_pop_types` / `plot_population_groups` /
`flatten_pop_tree` in `population_manager.jl`) — Revise-tracked + headless-tested (`@testset "plot
population picker"`, 536 pass); `api_plot_populations` is a thin wrapper that resolves images +
shapes JSON (the union loop was briefly in the API — moved out to respect the layer boundary).
*Decision: kept `track` as a pop_type (it's load-bearing for the track-gating workflow across ~12
files); fixed the picker locally instead — see the pop_type pro/con discussion.* (2) **Ported the old
R `plotChartsServer.R`/`plotHelpers.R` adjustments** into the `SeriesPicker` Options as collapsible
sub-groups (Layout/Points/Colours/Labels), scope-governed (`VisProps`): legend, log scale, gridlines,
rotate-X-labels, **facet** (small multiples per series, numeric charts), **dark theme** (builder ink is
`currentColor` → one `style.color` flip), Y-range; jitter type (beeswarm/random/none), colour-data,
point size/opacity; **palette** (Okabe-Ito, Tol bright/muted/light, user list); title, X/Y labels, font
size. Flagged as not-cleanly-portable (docs/PLOTS.md §9): pixel W/H (panels drag-resize), separate
axis-title-vs-label font sizes (Plot has one base size), facet for histogram/frequency, `coord_flip`,
`showFacetTitles` toggle. *Backend restarted for the `/api/plots/populations` change.* Builds green.

**#00040** — **Summary-plot engine → Observable Plot** (2026-06-27)
Replaced Vega-Lite with **Observable Plot** (`@observablehq/plot`) for the summary canvas after three
structural Vega walls hit at once: jitter (`xOffset` is for discrete grouping — points landed beside
the box or vanished), resize (`width:'container'` signal didn't re-fire on flex resize), and the look
(dashboard defaults vs. the old R `theme_classic`). New `frontend/src/plots/plot.ts`
(`buildPlotOptions(Plot, r, o)` — one builder per chart type, takes the Plot module so it's a lazy
dep) + `components/plots/PlotChart.vue` (lazy-imports Plot, injects width/height, re-renders on
ResizeObserver, `toImageURL` for PNG/SVG). Distribution charts use a **manual linear x scale, one
integer position per series** — box (`xlo/xhi` at index `i`) and **beeswarm** points (`xj` jittered
around the same `i`) share one scale → points sit ON the box by construction (the Vega bug, gone).
Violin = client-side Gaussian KDE (Silverman) → mirrored `areaX`. Horizontal axis labels (no diagonal
text). Deleted `plots/vega.ts` + `VegaChart.vue`; rewired `SummaryPanel`/`SeriesPicker`/`SummaryCanvas`
imports. Validated headless (linkedom) that every chart type's Plot options render without throwing;
`npm run build` green. Checked the old R reference (`plotHelpers.R`: theme_classic, `geom_jitter`/
`geom_quasirandom` beeswarm, `geom_tile`+viridis heatmaps) to match the look + scope follow-ups
(#00041 heatmaps/tiled maps, #00042 richer vis props). Also this session: track_measures.json now
lists all 10 celltrackR measures; CanvasPanel plots are **collapsible** (header chevron). *Frontend-
only change — no backend restart needed.* (Speed showing 0–1 was a stale `__tracks.h5ad` written
before physical sizes were in the metadata, not a plot bug — re-run Track Measures with Force
Recompute.)

**#00035** — **Behaviour summary canvas: multi-image + multi-segmentation comparison** (2026-06-26)
The behaviour page was locked to one image and one segmentation, so populations from several images /
segmentations couldn't be compared on one plot. Fixes: (1) **behaviour page is multi-select** (the
single-select restriction is gating-only) — selecting >1 image enables the per-image / pooled
"compare" control over the *selected* subset. (2) **`/api/plot_data` accepts `series:[{valueName,pop}]`
targets** — overlaying populations from different segmentations on one plot; grouped by `value_name`,
each read through `pop_df`, `vcat`-ed (the legacy `valueName+pops` form still works). (3) new
**`GET /api/plots/populations`** returns the union of populations across the selected images, grouped
by segmentation (the read-only `SeriesPicker`); derived pops surfaced generically via
`derived_pop_paths(pop_type)` (no hardcoded `/_tracked`). (4) `SummaryCanvas` decoupled from the
gating store (own populations fetch + selection); `SummaryPanel` sends `series`. (5) **Vega charts
re-fit on panel resize** (`ResizeObserver` → `view.resize()`; `width/height:'container'` only
recomputes on explicit resize). Tests: targets aggregation (single + cross-image × cross-segmentation,
4 series) + `derived_pop_paths` (513 pass). *Note: `api/src/*` isn't Revise-tracked — restart the
backend to pick up the new route + `plot_data` shape.* Follow-ups in the same change: (6) **live
updates** — the canvas subscribes to `gating:popmap` and refreshes the population list + re-pulls
panel data on any gate edit (a `live` pop's gates live in the `flow` map, so it refreshes regardless
of the broadcast's popType). (7) **overlap fix** — series are keyed by every *varying* dimension
(image · segmentation · pop), so populations sharing a path across segmentations (e.g. `/_tracked`
from A/B/C) get separate boxes/bars instead of collapsing onto one x-position. (8) **panel
persistence** — `useCanvasPanels` is backed by the `canvasPanels` store keyed per canvas
(`summary:<module>`, `gate:<popType>`), so open plots survive navigating away and back (both the
summary and gating canvases); cleared on project open/close. Exact drag positions aren't persisted
(panels reappear staggered by index). (9) **panel geometry persistence** — `CanvasPanel` takes a
`persistKey` (`${canvasKey}:${id}`) and stores drag position + size in the `canvasPanels` store, so
the dragged/resized layout (not just panel existence) survives navigation; geometry is dropped on
panel remove and project switch.

**#00039** — **Summary chart types implemented** (`docs/PLOTS.md`) (2026-06-27)
Built the agreed chart matrix. Backend (`plot_data.jl`): `measureType` (numeric/categorical, shared
`_is_categorical_col`) in every response; `rawPoints` returns downsampled (cap 1500) raw values; a
`points` chart type (for strip/violin); bar returns sd/sem/ci95 (ci95 ≈ 1.96·sem). Frontend
(`plots/vega.ts`, the single Vega builder): **theme_classic** look (white ground, thin black axes, no
grid) applied globally via `VegaChart`; group-on-X for box/bar/strip; **histogram, frequency,
stacked, 100%-stacked, bar (mean ± selectable SD/SEM/CI), boxplot + jittered raw-point overlay,
strip/jitter, violin** (client-side density). The panel offers only the charts valid for the measure
type (`chartsForMeasure`). Spec JSONs widened to the applicable sets. **Per-plot export** (CSV via
`plotDataToCsv`, PNG/SVG via `VegaChart.toImageURL`) and **visual properties** (`VisProps`: log
scale, legend, point size/opacity) in the SeriesPicker Options box, governed by the **global/local
scope** (shared vs per-plot, like the gating manager). Tests: measureType, points, rawPoints, bar
error metrics (521 pass). **Deferred**: auto-facet (#00038), ECDF, richer vis props (titles/labels/
palettes). *Restart the backend — `api/src/*` + the new route/response shape aren't Revise-tracked.*

**Plot design (`docs/PLOTS.md`)** — chart types × data source × measure type spec; decisions settled.

**#00031** — **`pop_df`: multi-segmentation pooling, derived `live`/tracked pops, cache auto-invalidation** (2026-06-26)
Made `pop_df` handle the multi-segmentation workflow (gate each of A/B/C, track each) the way the
R version did. (1) **Prefix vs leading-slash** value_name resolution confirmed + documented:
`["A/qc","B/qc","C/qc"]` pools across segmentations (value_name from prefix), while `["/qc"]` with
`value_name="A"` stays within `A` (a leading-slash path can't reach another value_name). (2)
**`live`/tracked is a *derived* filtered population, not a stored gating file** — gates live in the
`flow` map (`gating/{vn}.json`); a path whose leaf is a known filter (`"tracked"` → `track_id>0`,
`_LIVE_DERIVED_FILTERS`) is injected as a transient filtered child of its gated parent at read time,
and `recompute!` composes parent-gate ∩ filter. (3) **Cache auto-invalidation**: the request key now
folds in the on-disk mtimes of each involved `gating/{vn}.json` + `{vn}.h5ad`, so a saved gate edit
or a re-track invalidates the cache (pop_df already reloads from disk every call); `flush_cache`
stays as a manual override. Verified end-to-end on KDIeEm (3 segmentations: flow `/qc` = 804/1053/588,
pooled = 2445; live tracked = 2072, all `track_id>0`, each ⊆ its gate). This
unblocks part of #00021 — the per-track gating phase can build on the derived `live` pops.
**Generalised** the derived-pop mechanism (was `_LIVE_DERIVED_FILTERS`, live-only) into a
`_DERIVED_POPS` registry + a **reserved `_`-prefix namespace** (`DERIVED_POP_PREFIX`,
`is_reserved_pop_name`, both exported) so clustering can register the same way. Derived leaf
names are now `_`-prefixed (`_tracked`); `add_pop!`/`rename_pop!` reject `_`-leaf gate names
(→ 400), `from_tree` + the injection bypass via `reserved_ok=true`, and the frontend hints
against `_` names while typing (`GatePlotPanel.vue` add, `PopulationManager.vue` rename;
`isReservedPopName` in `stores/gating.ts`). Tests: `pop_df live _tracked (derived filter)`,
`reserved pop names (_ prefix)`, `pop_df cache auto-invalidation` (424 pass, frontend `vue-tsc`
clean). `docs/POPULATION.md` updated.

**#00030** — **`pop_df` implemented its documented-but-missing params** (2026-06-26)
`docs/POPULATION.md` documented `value_name=nothing`, `drop_na`, `flush_cache`/cache, and a
`(uID, value_name, label, track_id)` dedup key, but the implementation had none of them
(`value_name="default"`, no cache, dedup on `(value_name, label)` only). Implemented to match,
using R `cciaImage.R > popDT` as the reference (not a hard port): `value_name=nothing` resolves to
the active segmentation (parity with `label_props(img)`); `drop_na` drops cells NA/NaN in the
requested `pop_cols`; results cache on the image (`img._pop_df_cache`, runtime-only, not serialised)
keyed by request hash, cleared via `flush_cache=true` (no auto-invalidation, like R — flush after
gate edits); the `unique_labels` collapse key is now the present-column subset of
`(uID, value_name, label, track_id)` so set-level `uID` and `track_id` join it automatically. Tests:
`pop_df drop_na`, `pop_df track_id dedup key`, plus `value_name=nothing`/cache assertions in the
KDIeEm integration testset (409 pass). Also moved `FUTURE.md` into `docs/` and wired it into
`CLAUDE.md`.

**#00028** — **btrack rerun invalidates stale track-measure columns** (2026-06-25)
`tracking.track_measures` caches by sentinel (`live.cell.speed` in obs) and skips recompute if
present. Re-running `tracking.bayesian_tracking` (or the composite) produced *new* `track_id`s
while the old `live.cell.*` / `live.track.*` obs columns lingered → the second run cache-skipped
and returned measures computed against the *previous* tracking. Fixed by giving the LabelProps
chain writer a `drop_obs` verb (Julia `drop_obs` + Python `LabelPropsView.drop_obs`, both flushed
by `save!`/`save()` with crash-safe ordering: adds written → column-order rewritten → drops deleted
last). `tracking_utils.py._write_back` now drops every `live.cell.*` / `live.track.*` obs column as
it writes new lineage, so a re-track invalidates stale measures. Verified cross-language (Julia
writes a stale measure → Python btrack-style drop+add → anndata reads clean). Tests in the
`LabelProps writer` testset; `docs/DATAMODEL.md` documents `drop_obs`.

**#00026** — **`tracking.bayesianTracking` renamed to snake_case** (2026-06-25)
Renamed the task to `tracking.bayesian_tracking` (files `tasks/tracking/bayesian_tracking.{jl,json}`,
`fun_name` `tracking.bayesian_tracking`, params temp-file prefix). Struct stays `BayesianTracking`
(PascalCase). Updated `_spec_path`/`_fun_name_map` in `task_registry.jl`, the `Cecelia.jl` include,
the `bayesian_track_measures.json` composite entry, and the dispatch test. No frontend or persisted
dev-project records referenced the old `fun_name`. The legacy `"task"` label field stays camelCase
to match the other 9 sibling JSONs (it's display-only, unused for routing). Other legacy camelCase
task files (`measureLabels`, `cellposeCorrect`, …) are deliberately not renamed.

**#00025** — **`pop_df` moved out of the gating engine; image-path accessor pulled back into the model** (2026-06-25)
Drift fixes prompted by the `cciaImage.R` audit (the R image class owned a coherent accessor set;
several drifted into subsystem files in the port). Changes:
- **`pop_df`/`_pop_df`/`_group_pops_by_value_name`** moved from `gating/gating_engine.jl` to
  `gating/population_manager.jl`, co-located with the `pop_map` accessors (`load_pop_map`/
  `save_pop_map!`). `pop_df` is `pop_type`-agnostic (flow/live/clust/transient) — it belongs with
  the generic, pop_type-neutral population infrastructure, not the gating engine (gating is only one
  membership source). It still builds on `recompute!`/`cells_in_pop` (gating_engine, resolved at
  call time). Considered a standalone `pop_df.jl` but rejected it — co-locating with `pop_map`
  matches how the two were grouped and needs no new include.
- **`img_label_props_path`/`img_label_props_dir`** added to `model/image.jl` (mirrors R
  `imLabelPropsFilepath`), making the image the single owner of the labelProps path convention.
  Replaced inline `joinpath(_dir, "labelProps", …)` in `label_props.jl`, `bayesian_tracking.jl`,
  and `track_measures.jl`.
- **`img_physical_sizes`** (done earlier) similarly lives in `model/image.jl` (mirrors
  `omeXMLPixelRes`/`omeXMLTimelapseInfo`), not in the tracking task.
Remaining R accessors with no Julia callers yet (`imRegionsFilepath`, `imNeighboursFilepath`,
`spe`, `flowGatingSet`, …) were left unported — adding them now would be speculative.

**#00024** — **Remember run-table image selection across navigation** (2026-06-25)
Leaving a module page and returning lost the checkbox selection (it lived only in
`ImageTable`/`ModuleLayout` component-local refs). Now persisted in the project store
(`getImageSelection`/`setImageSelection`, keyed by `${module}|${setUid}`): `ImageTable` seeds from
it on mount / set switch and commits on every toggle; `ModuleLayout` initialises `selectedUids`
from it. Keyed by module so selections don't bleed across pages (gating single-select vs segment
multi-select). In-memory, cleared on project load/close. Generic for all module pages via
`ModuleLayout`.

**#00023** — **z-scope for napari cell selection** (2026-06-25)
A **Z toggle** + **±N stepper** in the gating bar (next to *draw region*) let the spatial cell
selection either span the whole z-stack (default) or be restricted to cells within ±N slices of the
currently displayed z in napari. The bridge (`_on_selection_changed`) reads the live z when the
polygon closes (so scrolling before finishing selects on that slice) and filters by
`|round(z_centroid) − z_now| ≤ z_window`; no-op without a z axis, `t` still ignored. Config flows
`store.napariZMode/Window` → `/api/napari/start-selection` (`zMode`/`zWindow`) → `start_cell_selection`
(`_sel_ctx`). Toggling the mode/window also **re-evaluates the already-drawn polygon live** via
`/api/napari/selection-scope` → `update_selection_scope`. **Needs a napari restart**
(`napari_bridge.py` changed).

**#00022** — **Napari population-overlay UX polish** (2026-06-25)
Fixes around showing/selecting populations in napari:
1. *Remembered show-populations* — the ViewerPanel `pi-palette` master toggle persists its state
   (`settings.napariShowPopulations`, default on) and auto-applies on image open, so populations
   show by default instead of resetting to hidden every time.
2. *Dropped the duplicate button* — removed the gating-bar `pi-palette` (`store.showInNapari`),
   which duplicated the ViewerPanel toggle and confused users. The gating bar keeps only the
   `pi-pencil` cell-selection brush.
3. *Transient pop no longer pushed to napari* — `api_napari_show_populations` excludes the
   transient "Napari selection" pop. Previously each popmap broadcast re-pushed it as a new Points
   layer, which stole napari's active layer so the user couldn't keep editing the selection shape.
4. *Clear button for the transient pop* — the manager row has a trash button
   (`store.clearNapariSelection` → `/api/napari/event` with `labels: []`); the selection is never
   persisted so there's no pop to delete and it otherwise lingered forever.
5. *Plotdata hardened* — `_plot_xy` returns empty data for an unknown pop instead of throwing
   (`pop_membership: not found: /Napari selection` 500 when a plot/highlight still pointed at a
   since-cleared selection).
6. *Plots refresh on resize* — `_node_dict` emits a `membership_sig` (hash of the label set) for
   explicit-label pops. Resizing the napari selection changed the manager's cell count but not the
   plots, because the client's `popVersion` only bumps on a gate/filter signature change and the
   transient pop has neither; the signature now folds in `membership_sig`.
7. *Clearing the selection cleans up fully* — new `POST /api/napari/stop-selection` (the trash
   button) clears the registry, re-broadcasts, and sends the bridge a `remove_layer` for the
   `Cell selection` Shapes layer (it used to linger in napari). The client also prunes the dead
   path from plot highlights / displayed parents, so a plot with no remaining selection reverts
   from the dimmed overlay backdrop to normal pseudocolour/contour instead of staying grey.

**#00007** — **Napari ↔ gating linked brushing** (2026-06-25)
Napari is wired both ways around the gating engine (Julia stays the sole evaluator).
*Consumer*: `POST /api/napari/show-populations` → bridge `show_populations` colours per-pop
centroid Points layers (centroids read locally from the H5AD via the new
`python/cecelia/utils/label_props_utils.py`). *Producer (linked brushing)*: `POST
/api/napari/start-selection` adds a `Cell selection` Shapes layer; drawing on it point-in-polygons
cell centroids and POSTs the inside label IDs to `/api/napari/event`, which mirrors them as a
**transient** "Napari selection" population (explicit-label membership; `transient` pops broadcast
but never persisted) so the flow plots highlight exactly those cells. Also added the Python
membership client `python/cecelia/cecelia_client.py` + reduced `python/cecelia/utils/pop_utils.py` (`pop_df`
resolves membership via the API, reads columns locally — no Python gate engine, no CSV).
Engine: `Population.explicit_labels`/`transient` + `recompute!` branch + `to_tree(include_transient)`.
Docs: `NAPARI.md`, `POPULATION.md`, `API.md`, `ARCHITECTURE.md`.

**#00005** — **Per-task log files** (2026-06-23)
`run_task` in `scheduler.jl` now wraps the caller's `on_log` with `_wrap_log_with_file`, which
appends every log line (timestamped `[yyyy-mm-dd HH:MM:SS]`) to
`{img._dir}/logs/{fun_name}.log` (e.g. `…/1/{uid}/logs/cleanupImages.cellposeCorrect.log`).
The directory is created on first use. The wrapper is transparent — all existing callers (API,
REPL, chain) keep their own `on_log` behaviour unchanged; the file write is additive and
swallows I/O errors silently so a bad path never kills a task. `Dates` stdlib added to
`Project.toml`.

**#00001** — **Remaining cleanup result handlers** (2026-06-20)
`avgCorrect`, `driftCorrect`, etc. — originally flagged as needing individual `ws_result` calls.
Resolved: `sockets.jl` generic `_run_task(e)` now sends `ws_result` for any task returning a
non-nothing value, so new tasks get this for free.

**#00006** — **`sockets.jl` duplicate task dispatch table** (2026-06-20)
Lines 95–105 had a hard-coded `if task_name == "omezarr" ... elseif ...` table duplicating
`task_registry.jl`. Fixed: `handle_task_run` now extracts the `module` field from the WS
payload and constructs the full `fun_name` as `module * "." * task`, then resolves via
`_task_from_fun_name`. Adding a new task now requires only `task_registry.jl`.

**#00007** — **Frontend task JSON files not deleted** (2026-06-20)
`frontend/src/tasks/definitions/importImages.json` and `cleanupImages.json` were stale copies
of the package-owned specs. Deleted; Vue now fetches from `GET /api/tasks/definitions?category=X`.

**#00008** — **Validation not enforced at `run_task` base level** (2026-06-20)
`validate_params` was only called inside each concrete task's `run_task` implementation —
a new task could forget it. Fixed: concrete impls renamed to `_run_task`; public `run_task`
always calls `validate_params` before delegating. REPL callers get enforcement unconditionally.

**#00009** — **`task:result` handler for `cellposeCorrect`** (2026-06-19)
`ws.ts` now updates `img.filepaths["cpCorrected"]` when `task:result` arrives with
`valueName + filename`. Generic handler works for any cleanup task that sends these fields.

**#00010** — **`proc.pid` FieldError in `kill_task`** (2026-06-18)
`Base.Process` has no `.pid` field; fixed with `ccall(:uv_process_get_pid, ...)`.

**#00011** — **Cancelled tasks showing green "done"** (2026-06-18)
libuv sets `exitcode=0` for signal-killed processes; added `&& proc.termsignal == 0` check
in `run_python_process` and `run_bf2raw_process`.

**#00012** — **`POST /api/projects/rename` unknown endpoint** (2026-06-18)
Revise didn't hot-reload `server.jl` and `projects.jl` together; backend restart required.
Endpoint itself was correct.

**#00019** — **`run.run_id` FieldError in chain cancel** (2026-06-22)
`ChainRun` struct has field `.id`, not `.run_id`. The `_is_cancelled()` closure in `run_chain`
referenced `run.run_id`, causing a `FieldError` on every chain run attempt. Fixed in `chain.jl`
(`_is_cancelled() = on_cancel_check(run.id)`).

**#00015** — **GPU chain tasks appeared to run concurrently / wrong elapsed time** (2026-06-22)
Pools were already config-defined (`config.toml [pools]`: `gpu=1`, `io=8`, …) and the global
worker pool *did* serialise GPU execution. The bug was **queue visibility**: `_execute_image_chain!`
marked a node `:running` *before* `run_task` acquired a pool slot, so all three cellpose nodes
showed `:running` with `startedAt≈t0` (elapsed 2/4/6 min) even though only one executed at a time.
Fixed: node is marked `:queued` before `run_task`; flips to `:running` only when a pool worker
picks it up (via `on_status_change`), so `startedAt`/elapsed is the real start. New
`chain:node:queued` event + `:queued` live-node state. Also hardened `_pool` to `@warn` (once) on
fallback to `default` instead of silently swallowing a missing pool. The dead per-run
`ChainRun._pools` semaphore layer (never wired from the API) was removed — pools are config-only.

**#00016** — **Chain cancel didn't kill running subprocesses** (2026-06-22)
`cancel_chain_run!` only set a between-node flag, so a running cellpose process kept going and the
node finished green. Fixed: `TaskRecord` gained a `chain_run_id` field; `run_task` threads
`chain_run_id = run.id`; `cancel_chain_run!` now collects matching task IDs under `_TASKS_LOCK`
and calls `cancel_task!` (SIGKILL via `_kill_tree`) on each. After the killed task returns,
`_execute_image_chain!` re-checks `_is_cancelled()` and marks the node `:cancelled` (not `:failed`),
and the frontend maps `status='cancelled'` to a cancelled entry. (Set-scope/incremental still
pending — see #00020.)

**#00017** — **SIGKILL timing race in cancel_task!** (2026-06-22)
If cancel arrived after a worker set `:running` but before `on_process` recorded `rec.proc`, the
kill was skipped. Fixed: `_execute_job!`'s `on_process` wrapper re-checks `is_cancelled(job.id)`
right after storing `rec.proc` and kills immediately if already cancelled.

**#00018** — **Chain node labels showed raw fn** (2026-06-22)
Live-tab nodes showed `cleanupImages.cellposeCorrect` instead of a friendly label. Fixed on the
frontend: `ws.ts` resolves the label via `useTaskDefsStore().labelFor(fn)` before
`addFromChainEvent` (falls back to `fn.split('.').pop()` only until defs load), and `ChainLiveNode`
renders `data.label`. No backend payload change needed.
