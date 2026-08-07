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

**Referencing an item.** Items are keyed by their **title** — there are no numeric IDs. Cite one as
`docs/TODO.md` → *Title*.

> Numeric IDs (`#00042`) were retired on 2026-08-05. They existed so code could cite an item, and they
> failed at exactly that: of the eight code comments citing an ID, **four pointed at an item that no
> longer existed**. The cause was structural — completion *deletes* an item, so "increment the highest"
> was evaluated against a shrinking set and reissued numbers that plans, comments and git history still
> referenced. `#00087` ended up meaning two different things; `#00003` was a known duplicate. Meanwhile
> `docs/FUTURE.md` (keyed by title) and `docs/todo/*_PLAN.md` (cited by path) never collided once.
>
> Every citation in the docs and the code was repointed at the same time. `docs/prompts/` is left as
> written — those are frozen records of finished work, so a retired number there is history, not a
> broken pointer.

**From code, prefer the permanent reference.** A `docs/<AREA>.md` section or a `docs/todo/X_PLAN.md`
path cannot dangle when the work ships, which is what a TODO citation does by construction. Cite the
tracker only for work that is genuinely still open, and by title.

Items marked **🔹 needs-input** need something only Dominik can provide — a test asset, a
domain-specific expected value, or a decision an agent shouldn't make alone. Grep `needs-input`.

---

## Next up

### Widen the chip-row tooltip rule to "one tooltip, group or per-option, never both"
`duplicateTooltips` (`frontend/src/utils/uiCopy.ts`) catches ONE of the two doubles a chip row can
have: the control repeating its **heading's** tooltip, expression for expression. It cannot see the
other — a group `v-tooltip` alongside **per-option `tip`s** — because the two say the same thing in
different words (`ErrorConsole`: per-option *Show info messages* vs group *Show only messages of this
severity*), so no string comparison matches. Measured by Dominik, 2026-08-07: **7 rows** across
`ErrorConsole`, `PopulationManager`, `CanvasSidePanel`, `RenderModeToggle`, `BatchMoviesPanel` (×2)
and `GatePlotPanel`; three were fixed in the same sitting, so re-measure before trusting that count.

**This is an amendment to the shipped rule, not an addition, which is why it is one job.** Today
`uncoveredControls` *requires* a chip row to carry its own `v-tooltip`, and the widened rule would
*forbid* it wherever per-option tips exist — both cannot hold. The resolution is that per-option tips
**count as coverage** for an icon-only row, which is a direct reversal of the "PER-OPTION tips DON'T
COUNT EITHER" paragraph on `uncoveredControls`. Land the amendment, the detector and the sweep
together; a half-applied version makes the suite red on correct code.

**Why it is not a regex change.** Per-option tips live in the SCRIPT (`filterOptions` is a computed,
`AXIS_OPTIONS` a const) and reach the template as a bound prop, so the template-only parser cannot
see them — the stated reason they were excluded in the first place. The detector has to resolve the
options identifier back into the script block.

Not a blanket delete either: on the icon-only rows the per-option tips are the load-bearing ones and
the group tooltip is the redundant one, so the direction of the fix differs per row. Rule + rationale:
`docs/UI.md` → *Tooltip coverage — the presence half*.

### Per-notebook reset (re-run a notebook on new data without killing the Pluto server)
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

### Ship a prebuilt Notebooks sysimage in the bundle (release optimisation)
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

### Zarr/dask processing rework (read-frame-once + cellpose batching)
The whole-image RAM fix landed (drift/AF/cellpose/segmentation stream per timepoint/channel via the
`zarr_utils` streaming writers). The follow-up perf/consolidation work is parked in
`docs/todo/ZARR_STREAMING_PLAN.md`: Phase 1 = read each timepoint once into a bounded frame and tile
in RAM (kills the per-tile disk over-reads that `fortify`-to-whole-RAM originally worked around);
Phase 2 = batch cellpose `dn.eval` (GPU throughput, measure first); Phase 3 = cheap cleanups
(centralize the napari byte-order fix; maybe merge the two tilers). Guardrail: only changes with a
real measured benefit — the plan explicitly rejects a grand `map_over_zarr`, an `as_dask` sweep, and
intra-task thread pools (Julia resource pools already parallelize across images).

### Temporal downsampling / overlapping tracklets for behaviour (deferred)
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

### Set-scope / incremental node subprocesses not killed on chain cancel
The per-image cancel path kills running subprocesses. Set-scope (`_run_set_scope_node!`)
and incremental (`_run_incremental_node!`) runners call the multi-image `_run_task` directly with
`on_process = _ -> nothing` and are **not** registered in `_TASKS`, so `cancel_chain_run!` can't
reach their subprocesses mid-run (the between-node flag still stops not-yet-started ones). No real
set-scope subprocess task exists yet (only mock/plot tasks), so impact is currently nil. When the
first real set-scope subprocess task lands (e.g. HMM training), give the multi-image `_run_task`
path a `TaskRecord` + `chain_run_id` so it's cancellable like the per-image path. Low priority.

### Port `createBranching` (skeleton/branch analysis) from the old R version
Skeletonise a segmentation into a branch/path network for fibrous non-cell structures (collagen/SHG,
nerves, FRC reticular networks). Full plan is parked in `docs/todo/BRANCHING_PLAN.md` (audited
2026-07-27: ILEE_CSK vendoring dropped in favour of `skimage.feature.structure_tensor`; Decision 6
resolved via a dedicated `branch_labels` field; Decision 2 cost re-measured against the current
`ACCEPT_TOKENS`/`POP_MAP_SUFFIX` dispatch as ~10 code sites + ~10 test assertions). In progress on
`feat/branching-port`.

### A third of a drift-corrected stack can be empty, and every task still processes it
Measured 2026-07-31 on `k3Tx90` (project `kSUFux`… actually `4kS67f`, 201×20×544×548): drift correction
expands the canvas and pads with zeros, and on that image **z 0–2 and z 16–20 are all-zero across every
channel** — 8 of 21 planes. `EaMaVq` is the same (z 0–6 empty at t=0). The padding also MOVES per
timepoint, since the shift differs per frame.

Nothing downstream knows. A cellpose run segments all 21 planes including the 8 empty ones, so on this
image roughly **38% of the GPU time produces nothing**, and measurement/tracking then carry the empty
planes too. Surfaced by the task preview, where aiming at a padded plane returns "0 cells" and looks
exactly like a parameter problem (see `docs/todo/TASK_PREVIEW_PLAN.md`).

**The mechanism now exists — #435 landed `zarr_utils.read_valid_box`**, so a consumer can ask which part
of a store is data instead of re-deriving it, per timepoint, at any pyramid level, with `None` meaning
"all valid" for every store that never padded. That replaces this item's original suggestion (record an
empty-plane COUNT as QC): counting only made the cost visible, the box lets a task act on it. It also
works on a store that is still staged (verified) — so a producer can write the box during its own run.

What is left is the consuming decision, which the box does not make for you:
- **Is skipping safe for stitching?** `stitch_threshold` links labels ACROSS z, so dropping interior
  planes would be wrong. The empty planes here are leading/trailing, which is the safe case — but that
  needs to be checked rather than assumed, per image.
- **Do NOT crop to the box.** #435 documents two traps that apply directly: the box is per timepoint and
  each frame sits at its own offset *because* the correction aligned them in a shared canvas, so cropping
  per frame puts them back out of register — and the intersection across timepoints is EMPTY on four of
  the nine `kSUFux` movies (z-drift exceeded the 8-plane stack). The box is for masking statistics and
  skipping known-empty work, not for cropping.
- **Does the win generalise**, or is it specific to how much drift a movie has?

### `_compute_iou_matrix` is quadratic in cell count, and every nuc+cyto run pays it per frame
`SegmentationUtils._compute_iou_matrix` (`python/cecelia/utils/segmentation_utils.py`) compares every
cyto label against every nuc label with a **full-plane boolean op per pair** — `len(a) × len(b)` array
comparisons. Measured 2026-07-31 on one 590×590 plane: **1.8 s at 100×100 labels, 26.9 s at 400×400**.
It's called from `_match_nuc_cyto` once **per timepoint**, so a 201-frame two-model movie with ~400
cells/frame spends on the order of **90 minutes** just re-assigning label IDs.

Found while deciding whether the task preview could afford to run the matching step (it can't — that is
why the preview is base-model-only and says so; `docs/todo/TASK_PREVIEW_PLAN.md`). But the cost is paid
by the real pipeline too, which is the part worth fixing.

The fix is standard and O(pixels) rather than O(labels²): one co-occurrence histogram over the paired
label maps — `np.bincount(a.ravel() * (b.max() + 1) + b.ravel())` (or `scipy.sparse.coo_matrix`) gives
every pairwise intersection in a single pass, and the union follows from per-label totals. Same IoU
numbers, so the existing `match_threshold`/`removeUnmatched` behaviour is unchanged — which means it can
be pinned by asserting the new implementation matches the current one on a fixture before swapping it
(the same oracle trick #435 used for the drift refactor).

### Export an image version as OME-TIFF
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

### Auto-follow in task manager
Selecting the newest running task in `TasksModule.vue` (`/tasks`) when a task starts does not
work. Approaches tried: `watch`, `watchEffect`, `computed+watch`, WS event listener
(`ws.on('task:status', ...)`). Likely a Pinia/Vue 3 deep reactivity edge case with array
element property tracking.

### `testTasks.*` task fun_names/files are still camelCase
The test tasks `testTasks.imageTask`/`testTasks.setTask`/`testTasks.incrementalPlotTask` (files
`tasks/testTasks/{imageTask,setTask,incrementalPlotTask}.{jl,json}`, structs `TestImageTask`/
`TestSetTask`/`IncrementalPlotTask`) predate the snake_case convention (see
`docs/DEV.md` and the Julia naming rule in `CLAUDE.md`). Rename to snake_case `fun_name`s + files (e.g. `testTasks.image_task`,
`tasks/test_tasks/image_task.{jl,json}`) — structs stay PascalCase. Touches `_spec_path`/
`_fun_name_map` in `task_registry.jl`, the `Cecelia.jl` includes, and any test references. Not
important (test-only scaffolding, no user-facing impact) but should be fixed for consistency;
batch it rather than churn standalone.
