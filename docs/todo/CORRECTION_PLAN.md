# Manual correction — segmentation and tracks

**Status:** planning, no branch. Written to be picked up cold by another session.
**Origin:** a lab that used the old R version asked for four capabilities back (conference, 2026-08).
Two are general and belong in the app — **correct segmentation** and **correct tracks** (this plan).
The other two (importing tracks / segmentation in their own format) are lab-specific and become
plugins — see [`PLUGINS_PLAN.md`](PLUGINS_PLAN.md).

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
3. **The corrected labels store replaces the same `value_name`, staged — and every edit is journalled.**
   `staged_store` + `create_multiscales` + `store_compressor('labels')`, atomic swap at the end.
   A durable sidecar `corrections/{value_name}.json` (the `gating/{value_name}.json` shape,
   `docs/POPULATION.md`) records every op in order, so the corrected state is **reproducible** from
   *the original task + the journal*.
   *Rejected:* writing each correction to a new `value_name`. It is auditable but multiplies value
   names on every pass, and every downstream selector (`versioned_*`, pop definitions, plot scopes)
   would have to follow the rename. *Consequence to accept and state in the UI:* the pre-correction
   mask is not kept as a store — recovery is "re-run the segmentation task, then replay the journal".
4. **Measures are recomputed automatically, never patched and never left to the user.** Both
   correction tasks are **composite tasks** (`docs/MODULES.md` → composite pattern) that chain the
   existing standalone recompute steps — this is the direct answer to the old-R workflow above:
   - `segment.correct` → `segment.measureLabels` (the row set changed, so `X`/`var` must be rebuilt
     by its producing task) → and, if the image is tracked, `tracking.track_measures`.
   - `tracking.correct` → rewrite `obs.track_id` + lineage columns via
     `label_props |> add_obs |> save!` (never touching `X`/`var`) → `tracking.track_measures`.

   A correction that leaves the user to remember two follow-up runs is not finished. Chaining is
   free here precisely because Feijoa already split those steps into their own tasks.
5. **Invalidation is declared and visible.** Each correction task reports exactly which downstream
   artefacts are now stale, using the keep-list machinery already in `reset_image_analysis!`
   (`app/src/storage.jl:175`) as the vocabulary of what can be dropped. Surface it as a QC `warn`
   finding plus a short line in the page — "3 cluster runs and 2 track populations now predate this
   correction". **Do not auto-delete** them; do not silently leave them either. This is the one
   place where Feijoa must beat old R rather than match it.
6. **Track ops = old R's set plus Split.** Points: remove, add. Tracks: remove, join, **split**.
   Scoped to a selected population (as old R was), not the whole segmentation.
7. **Edit history is durable and per-image**, not session state — it is the journal from Decision 3,
   rendered. Old R's history table died with the Shiny session.
8. **QC is mandatory** (`docs/MODULES.md`): `metrics` = objects/tracks edited, ops by kind;
   `warn` when a correction touches more than a threshold share of objects (a mask that needs 40% of
   its labels hand-fixed is a segmentation problem, not a correction job).

## Phases

- **P0 — contract + audit.** Confirm the re-measure entry point exists and can run for a label
  subset; confirm the napari bridge can hand back an edited Labels layer; write the journal schema.
  Ends with the two task JSONs and no compute.
- **P1 — `tracking.correct`.** `obs` rewrite + journal + edit history + the three ops. No store
  writes, so it is independently shippable and much lower risk than P2.
- **P2 — `segment.correct`.** Bridge command to hand back the edited layer, staged store write,
  re-measure, journal.
- **P3 — invalidation surface.** Decision 5, for both tasks, plus the QC findings.
- **P4 — UX.** Where correction lives in the sidebar (a Data-group page? an affordance on Segment /
  Track?), the previews, the population scope. Consult `docs/UI.md`'s primitive catalog before
  rendering a single control.

## Cross-file architecture

| Concern | File |
|---|---|
| Tasks | `app/src/tasks/tracking/correct.{jl,json}`, `app/src/tasks/segment/correct.{jl,json}` (+ `_run.py` for the store write) |
| Registry | `app/src/tasks/task_registry.jl` — `_spec_path` overload + `_fun_name_map` entries |
| Labels path / value names | `app/src/model/image.jl:114` `img_labels_path` |
| Cell-table writes | `app/src/label_props.jl` — `add_obs`/`drop_obs`/`save!` only |
| Store write | `python/cecelia/utils/zarr_utils.py` — `staged_store`, `create_multiscales`, `store_compressor('labels')` |
| Selection from the image | `napari/napari_bridge.py:1438` `start_cell_selection`, `:1550` `_post_selection` |
| Invalidation vocabulary | `app/src/storage.jl:175` `reset_image_analysis!` |
| Journal sidecar | new `corrections/{value_name}.json`, shaped after `gating/{value_name}.json` (`docs/POPULATION.md`) |
| Docs to update on landing | `docs/SEGMENTATION.md`, `docs/TRACKING.md`, `docs/MODULES.md` (if a new pattern), `docs/UI.md` |

## Open questions

1. **Can `segment.measureLabels` run for a label subset**, or is a whole-image re-measure the only
   option? Measure the cost on a real timelapse first — if a full re-measure is a few seconds, the
   subset path is not worth building. Old R re-measured everything, so there is no prior art either way.
2. **Does `tracking.track_measures` recompute cleanly when only some `track_id`s changed?** The chain
   in Decision 4 assumes yes. If it assumes a fresh btrack output, that assumption is the first thing
   P1 has to fix.
3. **Does an edited Labels layer come back through the bridge, or does Python re-read the layer from
   napari's own state?** Old R read `layer.data` in-process; Feijoa's bridge is a separate process
   holding the viewer, so this is the same shape — confirm the round-trip size on a 3D timelapse.
4. **Timelapse scope of a label edit** — does correcting a mask on frame 12 mean frame 12 only, or a
   propagated fix? Old R had no answer. Recommend: frame-local, explicitly.
5. **Should `segment.correct` invalidate tracking outright?** A deleted label breaks a track's chain.
   Cheapest honest answer is a `warn` + "re-run tracking", not an automatic re-track.
6. Whether correction gets its own sidebar page or lives on Segment/Track — interacts with
   [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) (this lab wants a *small* menu, and correction is
   the reason they are here).

## Related

- [`PLUGINS_PLAN.md`](PLUGINS_PLAN.md) — the same lab's format-specific importers.
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) — hiding the pages they do not use.
- `docs/todo/SEG_QUALITY_PLAN.md` — the "make segmentation better" track. Correction is the manual
  escape hatch, **not** a substitute for it; if a dataset needs mass correction, that is a
  `SEG_QUALITY_PLAN.md` problem (Decision 8's threshold is the tripwire).
