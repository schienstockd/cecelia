# TODO

IDs are permanent — never renumber. Add new items by incrementing the highest existing ID.
This file tracks **open work only**. When an item is done, **delete it** — the record of what changed
lives in git history / merged PRs / the GitHub Releases notes (auto-generated at each tag), not here.

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

**#00083** — **QC engine: tasks emit metrics → cohort stats → `qc_flag_fired`**
The AI observer (and any "is this image an outlier?" surfacing) needs QC *data* that mostly isn't
produced yet. The writing API already exists — `qc_finding` / `write_qc` in `app/src/qc.jl` — but only
`cleanupImages/drift_correct.jl` uses it. To make QC (and the observer's deferred `qc_flag_fired` +
cohort `get_qc_metrics`) useful:
1. **Encourage tasks to emit QC metrics.** Make it a task-authoring convention (document in
   `docs/MODULES.md`) and retrofit the high-value tasks — cellpose segment (cell count, mean
   confidence, % flagged), measure (feature ranges), tracking (track count, mean track length).
2. **Cohort statistics** — per-set baselines so an image can be compared to its group
   (`QC-PROCESS.md` step 3).
3. **`qc_flag_fired` event + a `:flagged` node state** so a bad metric surfaces automatically
   (`QC-PROCESS.md` step 1/8); then wire it into the observer (small once the data exists — see
   `docs/ai-assist/OBSERVER.md` §7 step 5 and `docs/todo/OBSERVER_INTEGRATION_PLAN.md`).
Full design sketch lives in [`docs/ai-assist/QC-PROCESS.md`](ai-assist/QC-PROCESS.md).

**#00082** — **User-drop-in custom modules (restore the old R capability)** 🔹 needs-input
Let users add custom task/module functions by dropping files into their user dir (beside
`custom.toml`, resolved by `config_dir()`) — no package edit/rebuild — like old R cecelia's
`modules/` folder. The UI half is already directory-driven; the executable/registration half needs a
runtime registry + `register_task!` + a discovery scan. Full design (locked decisions, phases, open
questions) parked in [`docs/todo/CUSTOM_MODULES_PLAN.md`](todo/CUSTOM_MODULES_PLAN.md).

**#00081** — **Clustered tracks (`trackclust`) don't show on napari open until the toggle is cycled**
Opening an image in napari with the **cluster-tracks toggle already on** renders no ribbons; toggling
it off then on shows them. Workaround exists (cycle the toggle), so medium, not high.
Investigated (2026-07-15) — **ruled out**: bridge message ordering/prematurity (`send` is
request-response and everything is single-flighted under `_viewer_lock`, so `open_image` fully
completes before `napari:opened` is broadcast and before `show-tracks` is sent); frontend state
staleness (`onNapariOpened`→`pushAllOverlays` runs after the `napariImage` watch flush; `popVisible`/
`currentSetUid` are live computeds; trackclust ribbons ignore `color_by`); and the Julia handler
(`api_napari_show_tracks` builds `track_ids` fresh each call). So the on-open push looks correct on
paper — it's a timing/rendering effect. **Two live hypotheses:** (1) napari Tracks render timing —
the Tracks layer is added in the same burst as the image open (with `as_dask` the canvas/dims may not
have settled) so it lands but doesn't paint until a later re-add — the manual toggle *is* that re-add;
(2) lazy membership cache — `_live_map(…, "trackclust")`/`cells_in_pop` returns empty on the first
access after open (`isempty(ctids) && continue` skips the pop), warm on the second.
**Next step:** add throwaway logs — bridge `show_tracks` (per-pop `len(track_ids)` + whether the layer
was added) and the Julia handler (`ctids` count per trackclust pop) — reproduce once to tell (1) from
(2). Fix if (1): a bridge-side `layer.refresh()`/canvas nudge after `add_tracks`, or one deferred
re-push on open (mirrors the working toggle); if (2): warm/await membership before the first push.
Also spotted while reading: `napari_bridge.show_tracks` sets `self._track_sigs[name] = sig` even when
`len(sub)==0` (no layer added) — harmless today (the skip needs `existing`) but worth tidying.

**#00057** — **Update README for the install / run / update flow (and switch to versioned releases)**
Once the shipping functions are all in — the installer (constructor/pixi-pack), the `pixi run app`
launcher (done), and the update path (`pixi run update` done; in-app button pending) — rewrite
`README.md` for the end-user install → run → update story (it currently predates Pixi). Tie in with
the move from commit-as-we-go to **versioned GitHub Releases** (SHIPPING.md Phase 3): once releases
exist, the README's install section should point at the release installers, not a source checkout.

**#00070** — **Ship a prebuilt Notebooks sysimage in the bundle** (release optimisation)
(1) **DONE** — build-on-demand: an **Enable fast plots** button on the Notebooks page builds
`pluto/deps.so` in a background process, notebooks stay usable (slow-first-plot until it lands), and
it's stamped so a package/Julia update marks it stale and surfaces a **Rebuild** button
(`build-sysimage` route, `_classify_sysimage`, `pluto/sysimage_stamp.jl`, `launch.jl` freshness gate).
Opt-in (a ~10 min build shouldn't auto-start). Self-contained, always correct, no CI needed.
(2) **Remaining, optional** — once the constructor/pixi packaging pins Julia per platform, build the
`-full` variant in CI and ship it in the bundle for the primary OSes so even the *first* open is
instant. It falls through to (1) wherever no prebuilt image is present, and the stamp means a shipped
image that predates the user's Julia/deps self-heals. Belongs with the packaging phase; not urgent —
the on-first-run path already gives every user a fast cache after one build.

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

## Napari viewer

**#00079** — **Improve the 3D-crop UX (works but awkward)**
The 3D crop (draw over a projection → Preview / Save as a new cropped image) works, but the workflow
is clunky — it's tagged **"work in progress"** in the Viewer panel + `docs/NAPARI.md`. Smooth it out:
a less fiddly draw/preview/save flow, clearer z/t handling (Preview can't show a t-trim — clipping is
spatial-only — which is confusing), and fewer forced napari restarts. Code: `napari/napari_bridge.py`
(`start_crop`/`apply_crop`/`crop_box`/`_z_mip`), `ViewerPanel.vue` 3D-crop section,
`app/src/tasks/editImages/cropImage.jl`.

**#00080** — **Bridge relaunch-after-`stop-napari` gets stuck**
After `stop-napari`, clicking an image should relaunch the bridge, but a hung/slow discrete-GPU launch
leaves `_viewer_starting[]` stuck `true` (only cleared in the async `launch!` `finally`), so
`_ensure_viewer!` returns `false` forever and every open is blocked until a **full server restart**.
Fix: time-out the async launch (reset `_viewer_starting[]`/stale `_viewer_ref[]` on failure), and bump
the discrete-GPU launch timeout. Code: `api/src/napari_api.jl::_ensure_viewer!`, `app/src/napari.jl::launch!`.

## Figures & movies (animation)

Full design + phased sequence: **`docs/todo/ANIMATION_PLAN.md`**. Build order
`A → D → B + C → G → F1 → E → F2`. These supersede the ad-hoc image-strip items (#00032/#00036) by
putting them on the shared **view-snapshot** foundation.

**#00071** — **View snapshot atom (foundation)** — a durable, GUI-editable JSON describing a napari
view (camera, T/Z, per-layer contrast/colormap-name/visibility, pops, split-by-feature). Bridge
`capture_view_state()` + `apply_view_state()` (whitelist + sanitise to settable scalars, only-present
layers). Generalises `save_layer_props`. Blocks everything below. See ANIMATION_PLAN Phase A + Decision 1.

**#00072** — **Zoom-to-source** — store the snapshot + `{imageUid, valueName}` in each image-strip
frame; a button reopens the image and re-applies the snapshot (durable across sessions). The
"reconstruct my figure months later" path. Needs #00071. (Phase B; was the strip half of #00036.)

**#00073** — **Image-strip colour legend** — render channel + population/feature colours (swatch +
name / µm) below each frame, read from the snapshot; toggle in the ⚙ popover. Needs #00071 + #00076.
(Phase C; was #00032.)

**#00074** — **Capture quality** — D1: hi-res napari screenshot (`scale`/`size` on
`viewer.screenshot`, currently captured at the tiny widget size). D2: fix edge clipping — the strip's
`object-fit: cover` crops the frame so the scale bar/timestamp (at the edges) are cut; use
`contain`/match aspect. Independent quick win. (Phase D.)

**#00075** — **Scale bar / timestamp as figure overlays** — E1: a clean-capture toggle (hide napari's
scale bar + timestamp for the shot) so figures aren't baked, per journal norms. E2 (stretch):
Cecelia-draws its own vector scale bar (N µm + label) + timestamp beneath the frame, from
`img_physical_sizes` + time interval. (Phase E + Decision 7.)

**#00076** — **Split-by-feature colouring** — colour tracks/points by a categorical measure column →
named groups each with a hex colour (ports R `splitTracks`/`splitPops`); a track pop may inherit its
source pop's colour. Shared by the legend + both movie tiers; builds on colour-by-obs
(`/api/napari/colour-labels`, show-tracks `colorBy`). (Phase G + Decision 5.)

**#00077** — **Batch movie generation** (ports R `generateMovies`) — author a config (channels+colours,
pops, split-by-feature, fps, resolution, version, attr-naming) → apply per selected/attr-filtered
image (contrast from saved layer props, no `browser()`) → record the T-sweep → one attr-named `.mp4`
per image. Runs as a task. The headline user win. Needs #00071 + #00076. (Phase F1 + Decisions 3–4.)

**#00078** — **Keyframe animation page** — a board-like page holding a durable, editable ordered
sequence of snapshots (keyframes) + per-transition duration/easing + output settings; hydrate →
napari-animation `Animation` → render → encode. Edit a keyframe's colour in the GUI → regenerate.
Needs #00071. (Phase F2 + Decision 2.)

---

## Low priority

**#00085** — **Per-notebook reload on the Notebooks page (ONLY if a revised open notebook still goes stale)**
Contingent on a live check. #291 made `revise` preserve cell ids so Pluto's `auto_reload_from_file`
should merge a revision into an **open** notebook in place — removing the need to restart the whole
notebook server. First verify that in the running app: open a notebook in Pluto, have the observer
`revise` it, confirm it updates **without** a server restart. If it does, this item is moot — delete it.
If Pluto's autosave still races and the open notebook stays stale, add a way to reset **one** notebook
instead of the whole server (`NotebooksModule.vue` today exposes only server-wide restart/shutdown):
- **Backend:** an endpoint to drop/reload a single Pluto session. Needs the notebook's Pluto
  `notebook_id` — the API server opens by path and doesn't track it, so query Pluto's notebook list
  (with the secret) then its shutdown-by-id. Unknown: exactly what Pluto's HTTP API exposes — scope that first.
- **Frontend:** a per-row "Reload" action in `NotebookTable` (drop + reopen the session from disk).
- **Cheaper fallback** if the endpoint is fiddly: give `revise` the same open-session guard
  `restore`/`delete` already use (warn "close the tab first"), so the behaviour is at least consistent
  and explained rather than silently stale.
🔹 needs-input (D): the live-test verdict on whether #291 alone fixes the open-notebook reload.

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

**#00084** — **Observer summary set roll-up mode (only if payload trim is insufficient)**
Set-scoped observer calls (`get_measure_summary`, `get_cluster_summary`) return per-image detail ×
many measures, big enough the observer offloads them to a subagent (~80k tokens). The first-pass trim
(dedupe cluster `features` → `featuresByRun`, drop `mean`, round to 4 sig figs, `docs/ai-assist/`
tools) should shrink this a lot. IF that's still too large, consider an OPTIONAL set roll-up mode:
per-pop median-across-images + range instead of per-image. **Caveat (Dominik):** behaviour/phenotype
vary *within* an image per population, so a median-per-image roll-up flattens real structure the
observer needs to catch outliers — so this is a fallback, not obviously correct. Keep per-image as the
default; a roll-up would be an explicit opt-in for the "compare T vs B across the set" question only.

---
