# Manual correction — segmentation and tracks

**Status:** **P1 + P4a built** (PR #590, branch `feat/correction-seg-tracks`); P2, P3 and the
P4b/P4c surfaces are open. Written to be picked up cold by another session. **Revised 2026-08-17**
after auditing the plan's assumptions against both codebases — Decisions 2b/3b/4b/4c/4d/6b are new
and four open questions are answered; the old-R ground truth below was verified line by line and
stands. Building P1 then corrected two of those decisions against real data (see 4c).
**Origin:** a lab that used the old R version asked for four capabilities back (conference, 2026-08).
Two are general and belong in the app — **correct segmentation** and **correct tracks** (this plan).
The other two (importing tracks / segmentation in their own format) are lab-specific and become
plugins — that plan was dropped from `docs/todo/` (#588) to land with its own work, like this one.

> **The SURFACE has moved on.** P4's worklist shipped in #590 and did not survive contact — see
> [`TRACK_SCHEME_PLAN.md`](TRACK_SCHEME_PLAN.md) for the timeline-first successor and an honest record
> of why. Everything here about the OPS, the invalidation contract, the old-R ground truth and the
> segmentation half (P2) still stands; the phases describing the correction UI (P4a–P4e) are history.

## Goal

A user can fix a wrong segmentation mask and a wrong track, in napari, and have the corrected result
become the data everything downstream reads — with the staleness that creates made **explicit**
rather than silent.

## Ground truth from the old R version — read this before designing anything

The temptation is to treat this as a port. Only half of it is. Verified in
`~/cc-workspace/cecelia/old-R-shiny-version/`:

**Track correction existed in full.** `inst/app/modules/ui/trackingCorrectionUI.R` (114 lines) +
`inst/app/modules/server/trackingCorrectionServer.R` (663 lines):

| Old R surface | Detail |
|---|---|
| Points table + tracks table + edit-history table | `trackingCorrectionServer.R:479` renders `editHistory`, live-updated via `replaceData` (`:507`) |
| Points ops | **Remove / Add / save** — `:605-609` |
| Tracks ops | **Remove / Join** — `:613-616` |
| Three plotly previews | points preview, tracks preview, track traces (`trackingCorrectionUI.R:71-73`) |
| Cell selection from the image | napari `k` hotkey → `selected_points_to_output('trackingCorrectionSelectPoints')` (`inst/py/napari_utils.py:1633`), consumed as `napariModule = "tracking_correction"` (`:633`) |
| Scoped to a population | `populationUI(id)` in the sidebar (`trackingCorrectionUI.R:85`) — correction ran against a chosen population, not the whole segmentation |

There is **no track split** op in old R. It is the obvious complement to Join and should be in scope.

**The ops themselves are trivial** — all four are a single `track_id` assignment on the pop
data.table (`R/trackHelpers.R:436-484`), and **none of them touches any other column**:

| Op | What it does |
|---|---|
| `tracks.points.rm` | `track_id := NA` for the selected labels |
| `tracks.points.add` | `track_id := <chosen track>`, or `max(track_id) + 1` when no track is selected (new track) |
| `tracks.rm` | `track_id := NA` for every cell of the track |
| `tracks.join` | B's timepoints **not overlapping** A get `track_id := A` |

Two things not to port. `tracks.join` leaves B's time-overlapping points on `track_id == B`, so B
survives as a **remnant track** rather than being consumed. And the write-back
(`tracks.save.mod`, `trackHelpers.R:498`) is `labels$add_obs(as.list(popDT[, .(track_id)]))` —
**positional**, against a population-filtered DT, which is why the loader has to force
`DT[, label := factor(label, levels = labels$values_obs()$label)]; setorder(DT, label)` first
(`trackingCorrectionServer.R:217-220`, with a `TODO this will get the whole DT for track editing`
beside it). Feijoa's `add_obs` aligns **by label**, which removes that whole class of bug — that
is the reason Decision 4 insists on it, not style.

**Old R staged corrections on a `-mod` copy, and the history could roll back.** Both are missing
from the first draft of this plan and both are load-bearing UX:

- Ops mutate an in-memory DT; `tracks.save.mod` writes the **whole** labelProps out to a sibling
  `<valueName>-mod.h5ad`, which napari shows as its own layer set (`paste0(valueName(), "-mod")`,
  `trackingCorrectionServer.R:317-323`). Nothing touches the real file until the user presses
  save, which is a plain `file.copy(modPath, labelsPath, overwrite = TRUE)` (`:359-370`).
  So corrections **accumulated and were previewed** before commit.
- `recordTrackEdits` (`:14-22`) stores `track.diffs(previous, new)` — a diff of the entire
  `track_id` vector per edit — and `tracksEditHistoryDF` (`:137-160`) renders a **Rollback**
  action link per row. The history was undo, not just a log.

**Segmentation correction was the training-label path, repurposed** (Dominik, 2026-08-17). There is
no `correctSegmentation` task; the mechanism is:

```
"Save Labels" button            inst/app/modules/ui/trainModelsUI.R:47
                                inst/app/modules/ui/pixelClassificationUI.R:55
  → trainModelsServer.R:69 / pixelClassificationServer.R:308
  → R/napariUtils.R:255  saveLabels(pathToFile, layerName)
  → inst/py/napari_utils.py:473  save_labels()
  → zarr.save(filepath, layer.data.astype(np.uint16))
```

The user edits the **napari Labels layer with napari's own paint / fill / erase tools**, presses Save
Labels, and the layer is written back over `<obj>/labels/<valueName>.zarr`, registering
`imLabelsFilepath` (value name `manual` when drawn from scratch — `trainModelsServer.R:80-89`).

**Editability was a decision made at LOAD time, and it has to be here too** (Dominik, 2026-08-17).
`show_labels` takes `as_np_array` (`inst/py/napari_utils.py:530`), documented as *"boolean to load
labels as numpy array for editing"* (`R/napariUtils.R:288`). When set it collapses the pyramid to
**level 0 only, eagerly** — `im_labels[0].compute()` / `im_labels[0][:]` (`:589-598`) — and it is
declared **per module** by the page that needs to edit (`showLabelsAsNpArray = TRUE`,
`trainModelsServer.R:198`, plumbed through `imageViewerManager.R:42-46` → `viewerManager.R:189`).
There is even an abandoned `mode = 'PAN_ZOOM' if as_np_array is False else 'PAINT'` at `:655`.

This is not incidental — it is the only reason napari's paint tools worked at all, and both halves
of it matter (see Decision 2): napari sets `editable = not multiscale`
(`napari/layers/labels/labels.py:852`), and `data_setitem` (`:1438`) assigns into `self.data`,
which a dask array refuses. Feijoa currently loads every label store the opposite way —
`open_zarr(..., as_dask=True)` and the full pyramid when there is one (`napari_bridge.py:480-491`,
`nscales = len(im_dat)` in `segmentation_utils.py:193`). **A Feijoa Labels layer is un-paintable
today**, and no bridge command reads a layer back out.

Three properties of that write are **incompatible with Feijoa and must not be ported**:

1. **In place.** `zarr.save` onto the live path. Feijoa stages every store (`staged_store`) precisely
   because a half-written label store reads as **zeros with no error**.
2. **Single-level, no compressor choice.** No multiscales, no `store_compressor('labels')`.
3. **No re-measure.** The `labelProps` h5ad kept its old centroids, areas and intensities for edited
   labels. Every downstream number silently referred to the pre-edit mask.

(3) is not an old-R bug to fix in passing — it is the whole design problem of this feature.

**How the lab actually coped with (3), and why it must not be reproduced** (Dominik, 2026-08-17):
after every correction they **manually re-ran two whole tasks** —

1. measure labels again, and
2. `tracking.bayesianTracking` with **`calcTrackingStatsOnly = TRUE`**
   (`inst/modules/sources/tracking/bayesianTracking.R:74`) — a flag whose only purpose is to skip
   btrack and recompute the track stats.

Both are coarse, both are the user's job to remember, and forgetting either leaves numbers that
describe the pre-correction mask. **Feijoa has already decomposed both recomputes into standalone
tasks** — `segment.measureLabels` (`app/src/tasks/segment/measure_labels.{jl,json}`) and
`tracking.track_measures` (`app/src/tasks/tracking/track_measures.{jl,json}`), with composites
(`cellpose_measure`, `bayesian_track_measures`) already wired in `app/src/tasks/task_registry.jl`.
So the `calcTrackingStatsOnly` flag has no Feijoa equivalent and needs none: the correction task
**chains the minimal recompute itself** (Decision 4). Nobody re-runs tracking to fix a track.

**Neither external importer exists** in that checkout: `inst/modules/sources/tracking/importTracking.R`
is a no-op stub (resets and updates image info, nothing between) and
`inst/app/modules/inputDefinitions/tracking/trackmate.json` is an empty spec with no `.R` beside it.
Do not go looking for logic to port; ask the lab for their own module files.

## The central problem — the staleness cascade

A correction invalidates data that no longer describes what is on disk:

| Edit | Directly changes | Silently invalidates |
|---|---|---|
| Split / merge / delete / paint a label | the labels store: the **row set** of the segmentation | every `labelProps` measure for touched labels (centroid, area, intensity); gating populations keyed on `label`; cluster runs; track lineage referencing a deleted label; spatial neighbour graphs |
| Join / split / remove a track | `obs.track_id` (+ lineage cols) | `{value_name}__tracks.h5ad` (one row per `track_id`), `live.cell.*` per-cell track measures, HMM states + transitions, behaviour cluster runs, any track population |

Two hard constraints from `CLAUDE.md` shape the answer:

- **`X`/`var` are the producing task's job.** A label edit changes which rows exist, so the cell table
  cannot be patched — it must be **re-measured**. Only `obs` may be appended/rewritten via the canonical
  view (`add_obs` overwrites an existing column — `app/src/label_props.jl:598`, delete-then-write at
  `:687`; `drop_obs` exists for removal).
- **Stores are written staged, never in place** (`staged_store`, `python/cecelia/utils/zarr_utils.py`).

So: **track correction is an `obs` rewrite. Segmentation correction is a store rewrite plus a
re-measure.** They are not symmetric and should not share one task.

### The re-measure DESTROYS obs — this is the hard part of P2

`segment.measureLabels` does not patch a cell table, it **replaces** one. `measure_utils.py:365-398`
builds a fresh `ad.AnnData(X=…, obs = pd.DataFrame(index=df.index), var=…)` — **an empty `obs`** — and
`write_h5ad_atomic`s it to the same `labelProps/{value_name}.h5ad`. A re-measure therefore drops:

`track_id`, `track_parent`, `track_root`, `track_state`, `track_generation`, `cell_id`, every
`live.cell.*` / `live.track.*`, and any clustering / HMM / behaviour column.

Two consequences, and neither is optional:

1. The naive chain `segment.correct → measureLabels → track_measures` **cannot run**. The re-measure
   removes `track_id`, and `track_measures` then raises `"no track_id column — run btrack first"`
   (`app/src/tasks/tracking/track_measures.jl:191`) — *after* the columns are already gone.
2. So `segment.correct` must **carry obs across the re-measure itself** (Decision 4b). This is the
   one genuinely new mechanism the feature needs; everything else is wiring.

### `add_obs` has two properties that bite here

Both from `app/src/label_props.jl:657-692`:

- **Labels absent from the staged frame are written as `NaN`.** For "remove track" that is exactly
  right (untracked = `NaN`, which is what `track_measures` already skips, `:213`). But combined with
  Decision 6's population scope it is **data loss**: staging a population-scoped `track_id` frame
  wipes `track_id` for every cell *outside* the population. **The correction write always stages the
  full label set**, and the population only scopes what the UI lets you select.
- **Float64 only.** `track_id`, `cell_id` and `track_generation` are conceptually integers and are
  already stored as float64 by the same convention (the Python writer takes the same path,
  `tracking_utils.py:231`), so this is consistent — but a correction must not assume it can write an
  integer dtype, and must round on read (`_to_int`, `track_measures.jl:208`).

## Decisions

1. **Two tasks, not a page-only action.** `tracking.correct` and `segment.correct`, authored per
   `docs/MODULES.md` (`.jl` + `.json` [+ `_run.py`]). They therefore get the scheduler's log file,
   cancellation, resource pool, QC banking and chain-ability for free, and run identically from the
   REPL. A correction that only exists as a button in a Vue page cannot be replayed or audited.
2. **napari is the editing surface; Cecelia owns the write.** For segmentation: napari's native
   Labels tools (no bespoke editor). For tracks: **reuse the shipped linked-brushing selection path**
   — `start_cell_selection` (`napari/napari_bridge.py:1438`) draws a Shapes polygon, resolves the
   label ids inside it and POSTs them to Julia (`_post_selection`, `:1550`). Old R's `k`-hotkey
   channel is superseded; do **not** add a second selection mechanism.

2b. **Editability is chosen when the labels are LOADED, not when the user picks up the brush** —
   the `as_np_array` pattern above, ported. `show_labels` gains an `editable` flag; when set it
   opens **level 0 only, eagerly** — `open_zarr(path, multiscales=1, as_dask=False)` then
   `zarr_utils.fortify` (`:349`, which already does exactly the `[:]` / `.compute()` collapse) —
   and hands numpy to `napari_utils.add_labels`. Not a new reader; the flag threads through the
   existing one. Only the correction page requests it, so every other surface keeps today's lazy
   pyramid.
   *Why it must be a load-time flag and not a toggle:* napari decides `editable = not multiscale`
   at layer construction (`labels.py:852`), and a dask-backed layer cannot be assigned into at all
   (`data_setitem`, `:1438`). You cannot make a lazy layer paintable after the fact — you replace it.
   *The cost is the reason Open Question 4 is settled:* a real tracked store here
   (`zolIMa/…/labels/memTom.zarr`, 31t × 32z × 420y × 441x, uint32) is 7.2 MB on disk and
   **735 MB eager in full** — 23.7 MB for a single `t` frame. Old R loaded the whole thing and got
   away with it on 2D data; on 3D timelapses **the edit layer is one timepoint** (Decision 6b).
   Note that every label store on the dev machine today is single-level (`nscales` follows the
   *image* pyramid, `segmentation_utils.py:193`), so the dask-ness is the binding constraint in
   practice and multiscale is the latent one — the flag has to handle both.
3. **The corrected labels store replaces the same `value_name`, staged — and every edit is journalled.**
   `staged_store` + `create_multiscales` + `store_compressor('labels')`, atomic swap at the end.
   A durable sidecar `corrections/{value_name}.json` (the `gating/{value_name}.json` shape,
   `docs/POPULATION.md`) records every op in order, so the corrected state is **reproducible** from
   *the original task + the journal*.
   *Rejected:* writing each correction to a new `value_name`. It is auditable but multiplies value
   names on every pass, and every downstream selector (`versioned_*`, pop definitions, plot scopes)
   would have to follow the rename. *Consequence to accept and state in the UI:* the pre-correction
   mask is not kept as a store — recovery is "re-run the segmentation task, then replay the journal".
   *Note the interaction with 2b:* the corrected store is written multiscale, so re-opening it for a
   second correction pass must go through the `editable` path — a corrected store is not paintable
   by the default loader.

3b. **Ops accumulate against a staged working copy; committing is a separate, explicit act.** This
   is old R's `-mod` file, kept, because it is the actual workflow — you fix six tracks, look at the
   previews, then commit once. Without it every click is a destructive `.h5ad` write plus a chained
   recompute (Decision 4), which is both slow and unreviewable.
   - `tracking.correct` stages in **memory** (the ops are `track_id` assignments on a label-keyed
     frame) and journals each op as it is applied. Nothing is written until commit.
   - napari previews the pending state by re-colouring the existing layer through the shipped
     `colour_labels` path (`napari_bridge.py:871`) — **not** by writing a second `-mod` store. Old R
     needed a file because reticulate had no other channel; the bridge does.
   - Commit = one `add_obs |> save!` with the full label set + the journal write + the Decision 4
     recompute. Cancel = drop the staged frame; the file was never touched.
4. **Measures are recomputed automatically, never patched and never left to the user.** Both
   correction tasks are **composite tasks** (`docs/MODULES.md` → composite pattern) that chain the
   existing standalone recompute steps — this is the direct answer to the old-R workflow above:
   - `segment.correct` → `segment.measureLabels` (the row set changed, so `X`/`var` must be rebuilt
     by its producing task) → **obs carry-over (4b)** → and, if the image is tracked,
     `tracking.track_measures`.
   - `tracking.correct` → rewrite `obs.track_id` + the lineage columns (4c) via
     `label_props |> add_obs |> save!` (never touching `X`/`var`) → `tracking.track_measures`.

   A correction that leaves the user to remember two follow-up runs is not finished. Chaining is
   free here precisely because Feijoa already split those steps into their own tasks.

   **`tracking.track_measures` is confirmed safe to chain after an arbitrary `track_id` rewrite.**
   `_load_tracks_with_labels` (`track_measures.jl:185-228`) rebuilds every track from
   `obs.track_id` + the centroid columns, groups by id, sorts each track by time itself, and skips
   `NaN`. It never reads btrack output and never reads `cell_id`. So the plan's riskiest-looking
   assumption holds — which is what makes P1 independently shippable.

4b. **`segment.correct` carries `obs` across the re-measure.** Required, not an optimisation — see
   *The re-measure DESTROYS obs* above. The sequence inside the composite:
   1. snapshot the existing `obs` (label-keyed) before touching anything;
   2. write the corrected store (Decision 3) and run `segment.measureLabels`, which produces a fresh
      table with the new row set and an empty `obs`;
   3. re-apply the snapshot with `add_obs |> save!`. Alignment is by label, so **surviving labels keep
      their obs and new/split labels get `NaN`** — which is the correct semantics for free;
   4. `drop_obs` the columns a row-set change genuinely invalidates regardless of label survival, and
      **report them** (Decision 5) rather than silently recomputing everything.

   The drop-list is a decision, not a guess, and P0 owns writing it down. `track_id` and lineage are
   **kept** (that is what lets step 5's `track_measures` run); `live.cell.*` / `live.track.*` are
   dropped and recomputed; cluster / HMM / behaviour columns are dropped and *not* recomputed, only
   reported. There is a precedent for exactly this idiom to reuse rather than reinvent:
   `tracking_utils.py:224-231` already drops stale `live.cell.*` / `live.track.*` via
   `view.drop_obs(stale).add_obs(lineage).save()` when tracking rewrites tracks.
   *Rejected:* re-running btrack after a mask correction — expensive, and it discards every manual
   track correction the user already made. *Rejected:* teaching `measure_labels` to preserve obs
   itself — it is the *producing* task for `X`/`var` and building the file is its job
   (`CLAUDE.md` → sanctioned exception); obs carry-over is the correction's business.

4c. **Which lineage columns change, and to what.** Old R only ever wrote `track_id` and left
   `track_parent` / `track_root` / `track_generation` / `cell_id` stale, so there is no prior art —
   this must be specified, because two of them are actively wrong if ignored:
   - **`cell_id` must be renumbered.** It is a 1-based rank within a track ordered by time
     (`tracking_utils.py:110`); any join or split invalidates it for every cell of the affected
     tracks.
   - **A join must reconcile lineage, not inherit it by accident.** `_track_lineage`
     (`track_measures.jl:263-280`) takes each per-track lineage value from the **first cell of the
     track**, so joining A and B silently adopts whichever parent sorts first. Recommend: a joined
     track keeps A's `track_parent`/`track_root`/`track_generation`, and a split assigns the new
     fragment no parent (`NaN`, i.e. a root) — stated in the journal so it is auditable.
   - **A new track is a ROOT, and a root's parent is ITSELF.** Corrected while building P1 against
     real data (`zolIMa/1/fXgbTl`, 374 tracks): btrack writes
     `track_parent == track_root == track_id`, `track_generation == 0` for a parentless track. An
     earlier draft of this plan said to write `NaN` for "no parent" — that is wrong and would make a
     hand-made track read as having no lineage at all.
   - **`track_state` is left alone.** Also corrected in P1: it is btrack's *per-cell*
     classification, constant within a track in real data and carrying no lineage meaning. Clearing
     it (as this plan first proposed) would invent a value rather than repair one. Moving a cell
     between tracks does not change what btrack thought of that cell.

4d. **A split can violate the tracking task's own minimum.** `bayesian_tracking` filters tracks
   shorter than `minTimepoints` (`tracking_utils.py:103-107`). Correction bypasses that filter, so a
   split can leave tracks the tracking task would have discarded. Do not silently re-apply the
   filter — it would delete the user's edit. Surface it as a Decision 8 `warn`.
5. **Invalidation is declared and visible.** Each correction task reports exactly which downstream
   artefacts are now stale, using the keep-list machinery already in `reset_image_analysis!`
   (`app/src/storage.jl:175`) as the vocabulary of what can be dropped. Surface it as a QC `warn`
   finding plus a short line in the page — "3 cluster runs and 2 track populations now predate this
   correction". **Do not auto-delete** them; do not silently leave them either. This is the one
   place where Feijoa must beat old R rather than match it.
   *Two levels, don't conflate them:* 4b's drop-list is **obs columns inside the cell table** — the
   task deletes those itself, because leaving a stale column in the file is a correctness bug. This
   decision is about **separate artefacts** — `{vn}__tracks.h5ad`, cluster runs, gating populations,
   spatial graphs — which the task only *reports*, because deleting someone's population is their
   call. Same finding, two mechanisms.
6. **Track ops = old R's set plus Split.** Points: remove, add. Tracks: remove, join, **split**.
   Scoped to a selected population (as old R was), not the whole segmentation — but **the population
   scopes SELECTION only, never the write** (see the `add_obs` `NaN` trap above). Port the op
   semantics from `trackHelpers.R:436-484` with two corrections: a join must **consume** B rather than
   leaving its overlapping points behind as a remnant, and `max(track_id) + 1` for a new track id
   must ignore `NaN`.

6b. **A label edit is frame-local, explicitly.** Forced by 2b's memory cost, not a preference: the
   edit layer is one timepoint. State it in the UI; a propagated fix is a `SEG_QUALITY_PLAN.md`
   problem, not a correction one.

7. **Edit history is durable and per-image**, not session state — it is the journal from Decision 3,
   rendered. Old R's history table died with the Shiny session.
   **But choose what the journal buys, because old R's could roll back and an op-journal cannot.**
   Old R stored a diff of the whole `track_id` vector per edit and offered per-row Rollback
   (`trackingCorrectionServer.R:14-22, 137-160`). A journal of *ops* gives replay-from-the-original,
   which is strictly weaker: undoing edit 3 of 7 means replaying 1,2,4-7 from the segmentation task's
   output. Recommend: **ops journal + undo of the pending (uncommitted) stack only**, which is what
   3b's staging makes cheap and covers the real use ("that join was wrong, take it back"). Full
   rollback across committed corrections is replay, and the UI should say so rather than implying an
   undo stack that reaches back through commits. Note napari's Labels layer keeps its **own** undo
   history for paint strokes (`data_setitem`, `labels.py:1438`) — that covers the segmentation half
   pre-commit for free; do not build a second one.
8. **QC is mandatory** (`docs/MODULES.md`): `metrics` = objects/tracks edited, ops by kind;
   `warn` when a correction touches more than a threshold share of objects (a mask that needs 40% of
   its labels hand-fixed is a segmentation problem, not a correction job).

## Phases

- **P0 — contract + audit.** *Mostly done — see the audit results in Open questions below.* What is
  left: write the journal schema, and write down the **obs drop-list** (Decision 4b) — which columns
  survive a re-measure, which are recomputed, which are only reported. Ends with the two task JSONs
  and no compute.
- **P1 — `tracking.correct`. ✅ BUILT** (`feat/correction-seg-tracks`). The engine
  (`app/src/tracking/track_correction.jl`: the five ops, lineage reconciliation, `cell_id`
  renumbering, the journal, the QC helpers), the task (`app/src/tasks/tracking/correct.{jl,json}`),
  the composite `tracking.correct_measures`, registry + cohort-metrics wiring, and 76 assertions in
  `app/test/suite.jl`. Verified end-to-end on a **copy** of a real tracked image (6547 cells,
  374 → 372 tracks): stale measures dropped, lineage reconciled, `cell_id` densely renumbered.
  Documented in `docs/TRACKING.md` → *Manual track correction*.
  Two things P1 settled that the plan had wrong — see 4c: a root's parent is itself, and
  `track_state` is per-cell and must be left alone. One thing it added: **an empty op list is a
  legal no-op**, because the suite requires every task's own spec defaults to validate, and a task
  whose default cannot be submitted does not fit the framework.
  **Not built:** an authoring surface for an op the DETECTOR did not suggest — see P4d below. The
  `text` widget in the spec is a stopgap, not the authoring path.
- **P2 — `segment.correct`.** Three separable pieces, in order of risk:
  1. the `editable` load mode in `show_labels` (2b) — smallest, and testable on its own;
  2. a bridge command to hand the edited layer back (none exists today);
  3. the staged store write + re-measure + **obs carry-over** (4b), which is the real work.
- **P3 — invalidation surface. NOT BUILT.** Decision 5, for both tasks, plus the QC findings.
  `tracking.correct` handles 4b's half (it drops stale `live.*` obs from the cell table itself), but
  nothing reports the SEPARATE artefacts that now predate the correction — `{vn}__tracks.h5ad`, cluster
  runs, `trackclust` populations, gating pops, spatial graphs. Today a correction silently leaves them
  in place with no warning, which is precisely the silent staleness the plan calls "the one place where
  Feijoa must beat old R rather than match it".
- **P4a — triage worklist. ✅ BACKEND BUILT.** The app finds what looks wrong and pre-picks the fix;
  the user only judges it. This is the inversion of old R, which made the user find the bad track AND
  specify the repair, with no help for the first part — and finding it is the afternoon's work.
  `find_track_issues` (`app/src/tracking/track_correction.jl`) + `GET /api/tracking/issues`.
  Every candidate carries a **ready-to-submit op**, the coordinate to fly the viewer to, and a
  `reason` that is an instruction rather than a diagnosis. Four signatures:

  | Kind | Fix | Signature |
  |---|---|---|
  | `gap` | `track.join` | A ends, B starts within N frames and a few median steps |
  | `jump` | `track.split` | a step that is an outlier for its own track AND for the image |
  | `short` | `track.remove` | below `minTimepoints` — normally only after a split |
  | `duplicate` | `track.remove` | a pair moving together, ≤5° apart and never >10 µm — celltrackR QC §3.1 |

  **Thresholds are relative to the data, not absolute µm.** Measured on `zolIMa/1/fXgbTl` (374
  tracks): a hand-picked "15 µm" gap threshold flagged 79 tracks (21%) and "5 µm" flagged 4 — the
  number was doing all the work. Distances are now in units of the image's own median step
  (1.4 µm there) and the jump floor is a **quantile** of all steps, because step lengths are
  heavy-tailed (median 1.4, p90 4.9, p99 10.3 µm) and a median-multiple flags the whole tail.
  Result: **374 tracks → 31 candidates (8.3%)**, tunable to 10. That is a worklist someone finishes.
- **P4b — the visuals. ✅ BUILT.** Text cannot answer the question a candidate poses. Two track ends 2 µm
  apart are one cell if the first was heading toward the second and two cells if it was heading away
  — same number, opposite answer. **There was no track-path plotting in the frontend at all**
  (`lib/tips.ts` states the position: tracks are viewed as polylines in napari). Built so far:
  `frontend/src/plots/trackPaths.ts` — the canonical engine, pure and Vitest-covered
  (`pathPoints`, a **square** `pathDomain` so a straight run cannot render as a diagonal,
  `focusPoint`, `gapGeometry`/`gapHint` for the heading, and `normalizeTracks`/`displacementVectors`
  for star plots). The views followed: `TrackCorrectionView` (per-row thumbnails with the decision point
  marked), `TrackPathsView` (paths / star / rose, on the board too) and `TrackDiagnosticsView` (the
  celltrackR QC battery — `docs/TRACKING.md` → *Track diagnostics*). napari stays the place you look at
  the image; `POST /api/napari/centre` flies it to a candidate.

- **P4c — where it lives. ✅ SETTLED: the Track page's canvas.** No new nav entry, no new route. The
  Track page IS `GatingPlots` with `popType="track"`, so correction is a third panel kind there
  (**+ Correct**, beside **+ Tracks** and **+ Checks**), hosted through the generic `InteractivePanel`
  from an interactive-views registry entry that carries **no surface flag** — it mutates, and the
  Analysis board is read-only. The read-only track plots DO carry the board flag.

- **P4d — authoring an op the detector did not find. ✅ BUILT (track-level).** An *All tracks* mode
  beside *Suggested*: every track in a `SelectionTable` (multi-select, sortable, longest first, capped
  at 2000 with the cap stated), and the selection becomes an op — **Join** (exactly two,
  non-overlapping), **Split** (one, at a frame strictly inside it), **Remove** (any number). Blocked
  actions stay visible and say WHY, because "why can't I join these" is the question this surface exists
  to answer; the temporal-overlap refusal is the engine's own rule checked before Apply rather than
  after. A picked track flies napari to its first frame, like a suggested row. Hand-authored and
  suggested ops are indistinguishable downstream — same queue, same single task run, same journal.
  Logic + tests: `frontend/src/lib/trackCorrection.ts` (`trackRows`, `tracksOverlap`, `joinOrder`,
  `manualActions`, `build*Op`).
  **✅ And from napari, which is the half that matters.** Reading a track id off the viewer and hunting
  for it in a table is the chore the worklist exists to remove, so *All tracks* reads the selection
  instead: Draw (the existing `start-selection` brush) → Read selection →
  `GET /api/tracking/selection` resolves the enclosed labels to their tracks, ordered by how many
  selected cells each holds → **Pick** selects them, fetching any that fall outside the picker's cap.
  The same selection drives **Untrack cells** (`points.remove`), the one CELL-level op — a bad
  detection dropped without destroying the rest of its track.
  *Still open:* `points.add` (attach selected cells TO a track) has no button. It is the same
  selection plus a target track, so it is small — but "which track should these join" needs a picker
  of its own, and nobody has asked for it yet.

- **P4e — detector thresholds in the GUI. ✅ BUILT.** A collapsed *Sensitivity* section with the five
  knobs the route accepts. Measured on the reference image, the same 374 tracks give **10** candidates
  at `jumpQuantile 0.999`, 26 at `gapSteps 1`, **31** at the defaults, 96 at `gapSteps 8` +
  `gapFrames 6`, 222 at `jumpFactor 2` + `jumpQuantile 0.9`, and **309** at `minLen 15` — a range no
  single default covers, which is why this had to be a control rather than a better constant.
  **The defaults are not duplicated in the frontend.** The route echoes the thresholds it actually used
  (`thresholds` in the response); the panel seeds its inputs from those and sends only what the user
  moved, so the measured numbers stay on the Julia constants where they were measured.

### Icons for the correction surface — checked against the glossary

`frontend/src/lib/iconLegend.ts` (#589) is the reference, and `iconLegend.test.ts` is a two-way
ratchet: a rendered glyph missing from the list fails, **and so does a listed glyph nothing renders**.
So glossary entries land WITH the component, never ahead of it — do not pre-register these.

Checked availability (316 glyphs in the installed PrimeIcons) and, more importantly, consistency —
one meaning per glyph. Everything the worklist needs already exists:

| Need | Glyph | Its existing meaning |
|---|---|---|
| Which way the cell was going (the gap discriminator) | `pi-directions` | "Direction of movement" — exact |
| Show this candidate in napari | `pi-map-marker` | "…or a napari selection" |
| Dismiss a candidate | `pi-times` | "Close, cancel or clear" |
| The "needs review" list | `pi-flag` | "QC findings on this image" — a candidate *is* a finding |
| Undo a pending edit | `pi-undo` | exact |
| Commit the pending edits | `pi-save` | exact |
| The correction journal | `pi-history` | "Earlier — past runs, versions" |
| Remove a track / a duplicate | `pi-trash` | "Delete" |

**Join and Split get text buttons, not glyphs.** PrimeIcons has nothing that reads as "merge these
two" or "cut this in half" — the nearest free candidates are `pi-link` (too close to
`pi-external-link`), `pi-expand` (reads "fullscreen", and `pi-window-maximize` already owns that) and
`pi-arrow-right-arrow-left` (reads "swap"). A worklist row has room for a word, and these two ops are
destructive and asymmetric, so the label has to be exact rather than guessable. Inventing a glyph for
a concept the set does not cover is how a glossary starts lying.

One genuinely new glyph, and only when the star plot is built: **`pi-sun`** for the star / rose plot
(free; rays from a centre is literally the plot). `pi-star`/`pi-star-fill` are taken by
starred/not-starred, and `pi-asterisk` by density outliers.

### celltrackR plots worth adding to the suite

From celltrackR 1.2.2's own vignettes (extracted locally; Wortel et al. 2021,
doi:10.1016/j.crmeth.2021.100006). Cecelia already ports its *measures* in `track_measures.jl`; these
are the **plots**, and none exists in the frontend yet. Ordered by value-for-effort:

| Plot | celltrackR | What it answers | Status |
|---|---|---|---|
| Track paths (2D) | `plot.tracks` | what the tracks actually look like | engine built (`trackPaths.ts`) |
| **Star / rose plot** | `normalizeTracks` (ana-methods §1.2) | is migration directional, or is the field drifting | `normalizeTracks` built |
| Angle vs distance, cell pairs | `analyzeCellPairs` (QC §2.3, §3.1) | double tracking; global drift | `analyze_cell_pairs` built |
| MSD vs Δt (log-log) | `aggregate(…, squareDisplacement)` (§3) | random walk vs directed motion | not built |
| Autocorrelation vs Δt | `aggregate(…, overallNormDot)` (§4) | persistence time | not built |
| Angle/distance to a reference plane | `angleToPlane`/`distanceToPlane` (QC §3.2) | border artefacts, bad z-calibration | not built |

Two of these are **QC, not exploration**, and that is the interesting part: celltrackR's own QC
vignette uses the pair-angle plot to detect double tracking and tissue drift. `track_pair_drift`
implements the drift read-off (mean pair angle should sit near 90° at large separations; below it
means the whole field moves together) — reported as advisory only, because the fix is
`cleanupImages.driftCorrect` upstream, not a track edit.

The remaining three are a **track-vis canvas** rather than a correction feature, and want their own
parked plan before building — MSD and autocorrelation both need Δt aggregation that belongs in Julia
beside the existing celltrackR ports, not in the browser.

## Cross-file architecture

| Concern | File |
|---|---|
| Tasks | `app/src/tasks/tracking/correct.{jl,json}`, `app/src/tasks/segment/correct.{jl,json}` (+ `_run.py` for the store write) |
| Registry | `app/src/tasks/task_registry.jl` — `_spec_path` overload + `_fun_name_map` entries |
| Labels path / value names | `app/src/model/image.jl:114` `img_labels_path` |
| Cell-table writes | `app/src/label_props.jl` — `add_obs`/`drop_obs`/`save!` only |
| Store write | `python/cecelia/utils/zarr_utils.py` — `staged_store`, `create_multiscales`, `store_compressor('labels')` |
| Selection from the image | `napari/napari_bridge.py:1438` `start_cell_selection`, `:1550` `_post_selection` |
| **Editable label load (2b)** | `napari/napari_bridge.py:463-506` `_show_label_stores` — add the flag here; `zarr_utils.open_zarr(as_dask=False)` + `zarr_utils.fortify` (`:349`) |
| **The re-measure that wipes obs (4b)** | `python/cecelia/utils/measure_utils.py:365-398` `_to_anndata` |
| **Obs-invalidation precedent to reuse** | `python/cecelia/utils/tracking_utils.py:224-231` — `drop_obs(stale).add_obs(…).save()` |
| **Lineage column semantics (4c)** | `python/cecelia/utils/tracking_utils.py:110` (`cell_id` rank), `app/src/tasks/tracking/track_measures.jl:249,263-280` (`_TRACK_LINEAGE_COLS`, first-cell-wins) |
| Invalidation vocabulary | `app/src/storage.jl:175` `reset_image_analysis!` |
| Journal sidecar | new `corrections/{value_name}.json`, shaped after `gating/{value_name}.json` (`docs/POPULATION.md`) |
| Docs to update on landing | `docs/SEGMENTATION.md`, `docs/TRACKING.md`, `docs/MODULES.md` (if a new pattern), `docs/UI.md` |

## Open questions

**Answered by the P0 audit (2026-08-17) — kept here with their answers so the reasoning is not
re-derived:**

1. ~~**Can `segment.measureLabels` run for a label subset?**~~ **No, and the question was the wrong
   one.** It is whole-image and block-tiled, and it is the *creating* task — it writes a brand-new
   `AnnData` over the same path (`measure_utils.py:365-398`). So the cost was never the issue: a
   re-measure is **destructive to `obs`**, which is what Decision 4b now exists to handle. A subset
   path would mean changing the producing task and is not needed once obs carry-over is in place.
2. ~~**Does `tracking.track_measures` recompute cleanly when only some `track_id`s changed?**~~
   **Yes, cleanly.** It rebuilds tracks from `obs.track_id` + centroids, sorts by time itself, skips
   `NaN`, and never touches btrack or `cell_id` (`track_measures.jl:185-228`).
3. ~~**Does an edited Labels layer come back through the bridge?**~~ **There is nothing to come back
   yet** — no bridge command reads a Labels layer out (`napari_bridge.py:2073-2285`), and more
   importantly the layer is **not editable in the first place** (2b). The round-trip size question is
   answered by 2b's numbers: a frame-scoped edit layer is ~24 MB, so handing back the edited frame is
   unremarkable; handing back a whole 3D timelapse (735 MB) is not.
4. ~~**Timelapse scope of a label edit?**~~ **Frame-local**, and now forced rather than chosen — see
   Decision 6b.

**Still open:**

5. **Should `segment.correct` invalidate tracking outright?** A deleted label breaks a track's chain.
   Cheapest honest answer is a `warn` + "re-run tracking", not an automatic re-track. Decision 4b
   makes the *mild* case work (surviving labels keep their `track_id`, so `track_measures` recomputes
   over the corrected mask); the sharp case is a label deleted from the middle of a track, which
   leaves a temporal gap btrack would not have produced. Recommend the `warn`, and let the user decide
   whether the gap matters.
6. Whether correction gets its own sidebar page or lives on Segment/Track — interacts with
   [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) (this lab wants a *small* menu, and correction is
   the reason they are here).
7. **Does the `editable` load mode belong to the page or to the viewer?** Old R made it per-module
   config (`trainModelsServer.R:198`). In Feijoa the same store may already be open lazily for
   viewing when the user navigates to correction, so entering the page has to **replace** the layer
   and leaving it should arguably put the lazy one back. Cheapest version: the correction page always
   re-adds the layer on enter, and the existing eviction rule (`_LABEL_SUFFIXES`,
   `napari_bridge.py:1952`) handles the swap — but confirm that a suffix-based eviction is the right
   vocabulary for "same layer, different load mode" rather than inventing a third suffix.

## Related

- The same lab's format-specific importers — no parked plan (dropped in #588, lands with its own work).
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) — hiding the pages they do not use.
- `docs/todo/SEG_QUALITY_PLAN.md` — the "make segmentation better" track. Correction is the manual
  escape hatch, **not** a substitute for it; if a dataset needs mass correction, that is a
  `SEG_QUALITY_PLAN.md` problem (Decision 8's threshold is the tripwire).
