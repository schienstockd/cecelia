# TODO

**Open work only — things someone intends to do.** When an item is done, **delete it**; what changed
is recorded in git history, merged PRs and the release notes, not here.

**Does it belong here?** This is the tracker with the loosest edges, so it collects orphans. Before
adding, check the others:

| If the item is… | It goes in |
|---|---|
| a deliberate **non-goal**, or conditional on something that may never happen | `docs/FUTURE.md` |
| a **known-better approach set aside** (scale/ecosystem not ready) | `docs/FUTURE.md` |
| big enough to need **locked decisions + phases** before building | a `docs/todo/<FEATURE>_PLAN.md` |
| a **phase goal** for the current arc | `docs/ROADMAP.md` |
| something that **already shipped** | `docs/MILESTONES.md` (or nowhere — git has it) |
| **how a built subsystem works** | the relevant `docs/<AREA>.md` |

A fact you want recorded but that nobody should act on is **not** a TODO item. That is the drift this
table exists to stop: something worth knowing turns up, has no obvious home, and lands in the backlog.

**IDs** are stable so code comments can cite them (`# see TODO #00020`); increment the highest. They
are not sacred — renumber to resolve a collision, and say so in the item.

Items marked **🔹 needs-input** need something only Dominik can provide — a test asset, a
domain-specific expected value, or a decision an agent shouldn't make alone. Grep `needs-input`.

---

## Next up

**#00088** — **Per-notebook reset (re-run a notebook on new data without killing the Pluto server)**
Pluto has no filesystem watcher, so a notebook keeps rendering **stale data with no visible sign**
after a pipeline task rewrites its inputs. The `DATA_STAMP` convention
(`docs/NOTEBOOKS.md` → *Refreshing after a pipeline re-run*) is the working answer today, but it is
manual and easy to forget. The only reset currently available is the Notebooks page's **Restart**,
which stops the whole Pluto server and takes **every** open session (and their unsaved edits) with it
— which is what prompted this (Dominik, 2026-07-29).

**Mechanism, verified against the pinned Pluto (`Pluto/F6SNP`, `src/webserver/Router.jl`):**
- `GET /notebooklist` → a Dict of `notebook_id => path`. **MsgPack-encoded** (`pack`), not JSON, and
  `api/` has no MsgPack dep — so prefer the redirect route below over adding one.
- `GET|POST /shutdown?id=<uuid>` → `SessionActions.shutdown` for **that one session**.
- `GET /open?path=<abs>` → 302 to `/edit?id=<uuid>`, so the id can be read from the `Location`
  header with no new dependency. (Opening an already-open path returns the existing session.)

So: `POST /api/notebooks/reset { projectUid, file }` = resolve abs path → `open` without following
redirects → parse `id` → `shutdown?id=` → return ok. Then the row's existing **Open** runs it fresh.
UI: one per-row button in `NotebookTable.vue` (`pi-replay`), beside Snapshot/History.

**Risks to handle, not ignore:**
- These are Pluto's **desktop-app** endpoints ("normally shutdown is done through Dynamic.jl" per its
  own comment) — semi-public, not a stability guarantee. Fail gracefully and pin the Pluto version
  the behaviour was verified against.
- A reset **discards unsaved in-session edits**. Snapshot first, the way `/api/notebooks/revise`
  already does, so nothing is unrecoverable.
- `open`-then-`shutdown` on a notebook that was not running starts a session just to kill it. Cheap,
  but check `/notebooklist` first if it ever matters.

Related, and a bigger decision: **`PlutoUI` is not in `pluto/Project.toml`**. Adding it would give
`Button` (an in-notebook refresh) *and* sliders for a timepoint, but costs a re-resolve of all three
manifests. Decide that separately.

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

**#00085** — **Zarr/dask processing rework (read-frame-once + cellpose batching)**
The whole-image RAM fix landed (drift/AF/cellpose/segmentation stream per timepoint/channel via the
`zarr_utils` streaming writers). The follow-up perf/consolidation work is parked in
`docs/todo/ZARR_STREAMING_PLAN.md`: Phase 1 = read each timepoint once into a bounded frame and tile
in RAM (kills the per-tile disk over-reads that `fortify`-to-whole-RAM originally worked around);
Phase 2 = batch cellpose `dn.eval` (GPU throughput, measure first); Phase 3 = cheap cleanups
(centralize the napari byte-order fix; maybe merge the two tilers). Guardrail: only changes with a
real measured benefit — the plan explicitly rejects a grand `map_over_zarr`, an `as_dask` sweep, and
intra-task thread pools (Julia resource pools already parallelize across images).

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

**#00020** — **Set-scope / incremental node subprocesses not killed on chain cancel**
The per-image cancel path (#00016) kills running subprocesses. Set-scope (`_run_set_scope_node!`)
and incremental (`_run_incremental_node!`) runners call the multi-image `_run_task` directly with
`on_process = _ -> nothing` and are **not** registered in `_TASKS`, so `cancel_chain_run!` can't
reach their subprocesses mid-run (the between-node flag still stops not-yet-started ones). No real
set-scope subprocess task exists yet (only mock/plot tasks), so impact is currently nil. When the
first real set-scope subprocess task lands (e.g. HMM training), give the multi-image `_run_task`
path a `TaskRecord` + `chain_run_id` so it's cancellable like the per-image path. Low priority.

**#00086** — **Port `createBranching` (skeleton/branch analysis) from the old R version**
Skeletonise a segmentation into a branch/path network for fibrous non-cell structures (collagen/SHG,
nerves, FRC reticular networks). Full plan is parked in `docs/todo/BRANCHING_PLAN.md` (audited
2026-07-27: ILEE_CSK vendoring dropped in favour of `skimage.feature.structure_tensor`; Decision 6
resolved via a dedicated `branch_labels` field; Decision 2 cost re-measured against the current
`ACCEPT_TOKENS`/`POP_MAP_SUFFIX` dispatch as ~10 code sites + ~10 test assertions). In progress on
`feat/branching-port`.

**#00087** — **Ship custom Cellpose models (starting with `ccia.fluo`)**
The old R version bundled a custom Cellpose model `ccia.fluo` (~26 MiB, trained for fluorescence —
the model that actually segments dendritic and SHG branches in `mxIBEX.Rmd`, upstream of
`createBranching`). The current `segment/cellpose.json` hardcodes cellpose's four built-in models
(`cyto3`/`cyto2`/`cyto`/`nuclei`) in a `select` — no path for custom checkpoints. Needs (a) a
custom-model slot in the cellpose task JSON + Julia handler + Python runner (accept a model name
or file path, resolve to a Cellpose checkpoint), (b) a delivery mechanism for the packaged
`ccia.fluo` + other custom checkpoints (equivalent of the old `cciaModels()` downloader from
`github.com/schienstockd/ceceliaModels`, or bundled in the release). Load-bearing for branching's
real-world use case (fibrous / SHG segmentation), so schedule before v0.1.0 or note the branching
port is incomplete without it. 🔹 needs-input


---

## Backlog

**#00089** — **Parameter-tuning preview for segmentation (the other kind of preview)**
The live preview (`docs/SEGMENTATION.md` → *Previewing a running run*) lets you watch a real run as it
writes; it does not let you judge params on a couple of timepoints *before* committing to a full run,
which is the loop that actually matters when you're guessing at diameter/threshold. Design, corrected
premises (the R `seedDetectPreview` previewed a *different algorithm* — seed detection for backends
feijoa doesn't have) and a phased build sequence: [`docs/todo/TASK_PREVIEW_PLAN.md`](todo/TASK_PREVIEW_PLAN.md).
Scope is wider than segmentation: denoise and AF correction have the same judge-at-the-end problem and no
preview at all, so it is planned as ONE mechanism with three consumers. Pairs with
[`docs/todo/SEG_QUALITY_PLAN.md`](todo/SEG_QUALITY_PLAN.md), which measures param quality objectively
rather than visually.

**#00090** — **A third of a drift-corrected stack can be empty, and every task still processes it**
Measured 2026-07-31 on `k3Tx90` (project `kSUFux`… actually `4kS67f`, 201×20×544×548): drift correction
expands the canvas and pads with zeros, and on that image **z 0–2 and z 16–20 are all-zero across every
channel** — 8 of 21 planes. `EaMaVq` is the same (z 0–6 empty at t=0). The padding also MOVES per
timepoint, since the shift differs per frame.

Nothing downstream knows. A cellpose run segments all 21 planes including the 8 empty ones, so on this
image roughly **38% of the GPU time produces nothing**, and measurement/tracking then carry the empty
planes too. Surfaced by the task preview, where aiming at a padded plane returns "0 cells" and looks
exactly like a parameter problem (see `docs/todo/TASK_PREVIEW_PLAN.md`).

Worth quantifying before acting: whether skipping all-zero planes is safe for stitching (`stitch_threshold`
links labels ACROSS z, so dropping interior planes would be wrong — but these are leading/trailing) and
whether the win generalises or is specific to how much drift a movie has. A cheap first step is QC: record
the empty-plane count per corrected image, which makes the cost visible without changing any behaviour.

**#00092** — **Export an image version as OME-TIFF**
Nothing in the codebase writes a TIFF today (no `tifffile.imwrite` anywhere). The need is figures:
people render in **Imaris** rather than napari, and Imaris reads OME-TIFF, not our zarr stores. The
`.ccbundle` export is a different thing entirely (whole-project archive, tar-per-store, for moving a
project between machines).

This is a **write** path, so it's unaffected by `open_as_zarr` dropping its TIFF *reader* — an export
reads the store through the canonical reader and writes out.

Worth settling when it's built:
- **OME-TIFF, not plain TIFF** — otherwise pixel size / channel names / time increment don't survive
  into Imaris and the figure scale bar is wrong. We already hold all of it (`ome_xml_utils`).
- **What to export**: which image *version* (versioned-field picker, like every other task), which
  channels, and whether a z-MIP / single timepoint is enough — a full `201×21×4×544×548` uint16 movie
  is ~9.7 GB as one file, which needs the BigTIFF flag and may not be what anyone wants.
- **Where it lands**: not inside the project tree (it's an artefact, not data), so it wants a
  destination picker like `default_export_dir()`. Task rail + progress, staged output.

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
