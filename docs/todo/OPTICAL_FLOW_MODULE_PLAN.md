# Optical Flow module page + model vault

**Status:** design (2026-08-06). Companion to
[`COASTAL_SEGMENTATION_PLAN.md`](COASTAL_SEGMENTATION_PLAN.md), which covers the segmenter itself.
This covers only **training and managing models**.

Dominik: *"we had a train models page on the previous r version. but that was.. terrible. but the
idea is to provide a transparent module page to create and manage the model vault."*

## Scope — what lives where

The temptation is to put the whole flow pipeline on one page. Don't:

| concern | lives in | why |
|---|---|---|
| spatial smoothing | **Cleanup** (`cleanupImages.smooth`, exists) | it is a general image-cleanup step, not flow-specific |
| segmentation | **Segmentation** module page | it is a segmenter like cellpose; users look for it there |
| **training + the vault** | **Optical Flow** (new page) | the only genuinely new concern |

Page name: **Optical Flow**. Training is a **normal task** — no bespoke runner, no extras. It goes
on the task rail like everything else, so it gets progress, cancel, logs, QC and chainability for
free, and behaves the way every other task does.

## Locked decisions

1. **Follows the standard module-page shape** — image table and all. Same layout as the other
   module pages; the page is not a special case.
2. **Training is an ordinary task.** `flow.train` (or similar) with a params spec, a `_run.py`, a
   registry entry and mandatory QC, per `docs/MODULES.md`. Nothing about it bypasses `run_task`.
3. **Transparency is the point.** Every parameter that affects the model is a visible, documented
   param — spatial sigma, `temporal_scales`, `cumulative_window`, the metric set, loss weights,
   epochs, the channel and image(s) trained on. The R page's failure was opacity; the fix is that
   nothing is hidden, not a prettier form.
4. **The vault is the cellpose vault.** `<config_dir>/models/coastalModels/`, mirroring
   `cellposeModels/` — `config.jl` already owns the resolver, the enumeration and the
   `_inject_dynamic_options!` hook that rewrites a task's Model dropdown at spec-load. Follow it;
   do not invent a second model store. (`COASTAL_SEGMENTATION_PLAN.md` decision 7.)
5. **A model is weights + a manifest.** `<name>.pt` + `<name>.json` recording the training params,
   source image/channel and date. Inference MUST use the metric set it was trained on — coastal's
   `test_flow_metric_count.py` documents that a mismatch is *silent* (channels shift, the model is
   fed misaligned inputs). The manifest is what makes a model self-describing, and it is what the
   segmentation task reads to configure itself rather than trusting the user to re-enter params.

## The two panels, and what they can reuse

### Vault manager — reuse `CanvasSidePanel`

`PopulationManager.vue` is the precedent and it is close to what is wanted: a floating, collapsible,
shared manager with per-row rename-inline, delete, and a visibility toggle, built on
`CanvasSidePanel.vue`. A `ModelVaultManager` is the same shape over a different list: per row
the model name, the manifest summary (channel, sigma, metric count, date), rename, delete, and
"reveal in the segmentation picker". **Build it on `CanvasSidePanel`, not from scratch** — this
is exactly the case `CLAUDE.md` and `docs/todo/UX_PRIMITIVES_PLAN.md` warn about.

**BUILT — after a detour through the wrong shell.** The vault was a top-level `FloatingPanel` first,
which Dominik rejected: *"i want this like the pop manager. not a top level panel. that will
interfere with the viewer panel and lab log."* Exactly right — the app's window layer already holds
the Viewer and the Lab log, and a manager scoped to one canvas has no business competing for it. The
vault now wraps `CanvasSidePanel` on the flow canvas, toggled from the canvas bar (`showVault`,
persisted in the canvas `shared` bag). The shell was renamed from `PopulationPanelShell` in the same
change and its two plot-only parts — the global/local scope footer and the `PlotOptions` block — made
opt-in, since neither means anything for a list of models.

### Training panel — the canvas framework does NOT fit, and that is fine

`LayoutCanvas`/`TabbedCanvas` are comic-plate boards hosting **plot panels**. Training inputs are a
parameter form, and cecelia already has the canonical one: the task-runner param form driven by the
task JSON (`TaskRunner.vue`). Wrapping a param form in a plot canvas would be a bespoke variant of
something that already exists.

**Where the canvas DOES fit is the output.** "What did this model learn" is image-like — the prob
map, the instances, the flow metric planes — which is the `image` plot family the Analysis board
already hosts (`docs/ANALYSIS.md`, `ImageStripView.vue`). So:

- inputs → the standard task param form (existing);
- **model preview → canvas panels in the `image` family** (existing framework, new panel).

That keeps the "would be nice if we could use the same canvas approach" true where it earns its
keep, without forcing it where it doesn't.

**BUILT — and it lives on the page, not only on the board.** `FlowMetricsView` is one interactive-registry
entry flagged for both surfaces (`opticalFlowPage`, `analysisBoard`); the page hosts it through
`opticalFlow/FlowPlots.vue` in `ModuleLayout`'s `#plots` slot — the same shell the cluster and summary
canvases use (`useCanvasPanels` + `InteractivePanel` + zoom, key `flow:model:{imageUid}`). It was
board-only at first, which was wrong twice over: a module page's plots belong on the module page, and
the board flag was in fact dead (the picker filtered a hardcoded key list, so the plot appeared
nowhere). See `docs/UI.md` → *Generic plot-integration interface*.

### The plot answers "what goes IN", not "what came out"

The paragraph above got the plot's *question* wrong, and it took Dominik's figure to show it. Built as
"what did this model learn", the panel required a trained model, ran inference, and showed the metric
planes **after** dropping — so the one view that would let you judge the metric set was the one thing
it hid. And you cannot ask it before training, which is the only time the answer changes anything.

The question is **which of these 15 planes look like cells**. That needs no model: the metrics are a
property of the movie and the temporal scales. So:

- **no model → the contact sheet**: the projected input plus every metric plane. `CoastalUtils._manifest`
  falls back to the model group's own `temporalScales`/`cumulativeWindow` when there is no checkpoint
  to read them from; a real manifest still always wins, because inference must match training.
- **a model → one extra plane, the probability map.** That is the UNet's own output and genuinely
  cannot exist without it. Planes the model was trained *without* are marked, not hidden.
- **never instances.** Instances are what segmentation produces and the Segment page previews them
  through the normal preview path. A second instance renderer here would be the same picture computed
  a different way — the divergent-re-implementation trap, in the module that already has a plan
  section warning about it.

**And the user picks the set.** `dropDeadMetrics` was a single bool that dropped exactly the three
planes MY rank-AUC table scored flat on ONE dataset — a finding presented as a setting. It is now
`flowMetrics`, a chipSelect over the 11 fixed planes (the per-scale `mag_{n}` are not ticks: they follow
`temporalScales`, so offering both would let the two disagree). The three still ship unticked, as a
default rather than a rule. `flow_dropped_metrics` maps the selection to the manifest's
`droppedMetrics`, which is the existing contract inference already honours.

## Open questions

1. ~~**Does the preview need its own inference run?**~~ **Answered: reuse `preview/`, which is
   already generalised for this.** The worker (`:7656`) dispatches on `funName` and already carries
   TWO backends (cellpose and AF correction), and its explicit rule is *"no second cellpose
   implementation — it calls `CellposeUtils.predict_slice`, the same method the real task uses."*
   Coastal is a third backend calling `CoastalUtils.predict_slice`. Nothing new is needed except
   one wrinkle: the worker hands over a REGION, and a temporal segmenter also needs the window
   around it — which is the `context`/`context_index` shape Phase 1 added to the base, so the
   preview backend can build it the same way `predict_from_zarr` does. Training itself is far too
   slow for the preview loop; *inference with a chosen model* is precisely what it is for.
2. **Vault scope vs project export.** Config-dir models do not travel with a `.ccbundle`
   (`COASTAL_SEGMENTATION_PLAN.md` decision 7). The manager should show clearly that a model is
   machine-local, and the segmentation task must fail loudly on a missing model rather than falling
   back to an untrained one.
3. ~~**Which images does training run on?**~~ **Answered: the experimental SET.** Dominik,
   2026-08-06: *"the model will be trained on images from an experimental set."* So
   `opticalFlow.train` is `"scope": "set"` — one model from the set's images, not one model per
   image. Getting this wrong is not a small difference: image scope would have produced N models,
   each trained on a fraction of the data.

   Consequences already built in: metrics are computed **per movie** and the frames pooled
   afterwards (flow across a boundary between two recordings is not motion — coastal's
   `prepare_data_for_unet_batch` + `train_test_split_per_movie` are exactly this shape); a movie too
   short for the largest scale is **skipped with a warning** rather than contributing a different
   channel layout to the pool; images whose channel names disagree with the first are likewise
   skipped, because resolving names per image would train on whatever sits at that index. The
   manifest records `sourceImages`, and the QC is banked against every image that contributed.

## References
- [`COASTAL_SEGMENTATION_PLAN.md`](COASTAL_SEGMENTATION_PLAN.md) — the segmenter, the vault decision, Phase 2.
- `docs/MODULES.md` — task trio, param widget types, mandatory QC, module-page authoring.
- `docs/UI.md` — the UX-primitive catalog (**check before rendering any control**).
- `docs/ANALYSIS.md` — the canvas families, including `image`.
- `INVENTORY.md` → *Custom cellpose checkpoints (drop-in)* — the vault mechanism to mirror.
