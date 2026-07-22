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
