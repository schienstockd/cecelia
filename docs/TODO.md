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

**#00091** — **A cancelled run leaves a half-written zarr store — and can corrupt a REGISTERED one**
The store-level counterpart to the `ccid.json` truncation fixed in #420: `open(path, "w")` truncates a
state file, `rmtree`-then-stream truncates a store. Two distinct problems:

1. **Orphans.** A cancelled/failed segmentation leaves `labels/{vn}.zarr` that no `ccid.json` mentions —
   invisible, uncounted disk. Self-healing only if that value_name is ever re-run.
2. **Silent corruption of a registered set** (the serious one). `_open_label_store`
   (`segmentation_utils.py:495`) `shutil.rmtree`s the target *before* streaming, so re-running a
   value_name that is ALREADY registered and then cancelling leaves `ccid.json` still advertising it
   while the store is partial. On a multi-level image the next read raises `KeyError: '1'` (visible); on
   a **single-level** image (drift/AF/cellpose-corrected output — very common) there is **no error**:
   unwritten frames read as zeros, so `segment.measureLabels` and tracking produce results from a
   partial segmentation. Same hole in the IMAGE writers — `zarr_utils.py:298` and
   `open_multiscales_for_writing(mode='w')` — so a cancelled drift/AF correct can do this too.

**Fix: stage-then-rename**, the zarr analogue of `write_atomic`. Stream into a sibling
`{vn}.zarr.partial.{uid}`, then `rmtree` the final + rename on `_finalize_label_pyramid`. A failed re-run
then leaves the good store untouched, and a leftover staging dir is recognisably garbage. Do labels and
the image writers in one pass — one idiom, not two.

**Interaction to design around:** the napari bridge derives a layer's name from the store's FILE STEM
(`_show_label_stores`), so a staging filename would render `(X.partial.abc123) Labels (live)` and break
the `({vn})` prefix that `colour_labels` targets. The live preview points at the in-progress store, so it
must be handed the staging path while still displaying the value_name — decouple the layer name from the
filename (`LiveOutput` already carries `value_name` separately from `files`).

**Cheap partial step if the full fix slips:** incompleteness is exactly detectable — declared pyramid
levels > levels on disk (pinned by `MidRunReadabilityTest` in `test_segmentation_streaming.py`). Surface
that as QC so an incomplete registered store warns instead of silently feeding downstream, and sweep
orphans via a `MaintenancePatch` (dry-run then apply — the existing pattern, don't add a mechanism).


**#00089** — **Parameter-tuning preview for segmentation (the other kind of preview)**
The live preview (`docs/SEGMENTATION.md` → *Previewing a running run*) lets you watch a real run as it
writes. That is not the same thing as the R version's `seedDetectPreview`, which ran a **cheap version of
the algorithm on a timepoint sub-range** and pushed the result into napari so you could judge parameters
*before* committing to a full run — the more useful loop when you're guessing at diameter/threshold.
Worth porting as a proper task (`preview: true` param, or a `segment.previewParams` task) rather than the
R mechanism itself: that worked by shipping Python source strings over the WS to `viewer$execute` +
a generic `napari_utils.show_preview(variable)`, which this codebase deliberately replaced with typed
per-purpose bridge commands. The R sources are in `old-R-shiny-version/inst/modules/sources/segment/`
(`seedDetectPreview.R`, plus 8 backends worth of param sets).

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
