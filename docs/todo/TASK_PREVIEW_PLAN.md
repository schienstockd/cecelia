# Task preview — one mechanism for judging long-running tasks before you commit

**Status:** planning. Started 2026-07-30 as a segmentation-only plan (`SEG_PREVIEW_PLAN.md`); widened
2026-07-31 to a shared component after Dominik pointed out that denoise and autofluorescence correction
have the same problem and no preview at all. Covers `docs/TODO.md` #00089.

## Goal

Several tasks are long-running, parameter-sensitive, and **judge-at-the-end**: you guess a diameter or a
denoise model, wait, look at the result, guess again. The live preview shipped in #421 lets you *watch* a
full run, which shortens the feedback loop but not the run.

This plan is one mechanism that runs a task's **real compute over a bounded region** — what you are
currently looking at in napari — and shows the result without registering it as data. Segmentation is the
first consumer, not the subject: it is built with a second, differently-shaped consumer (AF correction,
which outputs an image and changes the channel count) so the abstraction is proven by use rather than
designed for one and retrofitted.

## Which tasks this serves

Previewability is a property of a task's *compute*, not of a category:

| Task | Preview output | Crop-safe? |
|---|---|---|
| `segment.cellpose` | labels | Yes — modulo tile seams and whole-image normalisation |
| `cleanupImages.afCorrect` | image | Yes — per-pixel channel unmixing. **Widens the channel axis** (`af_correction_output_shape`: C + one per requested inverse), so an AF preview has more channels than the source |
| `cleanupImages.cellposeCorrect` (denoise) | image | Yes in principle, but **not a build target** — see *Denoise waits for coastal* |
| `cleanupImages.driftCorrect` | — | **No.** `drift_correction_shifts` derives shifts from the whole timecourse; a crop cannot produce the real shifts, and the shifts are the thing you would want to judge. Declares nothing. |

`driftCorrect` is why this is opt-in per task rather than automatic for a category.

## Corrected premises (carried from the segmentation-only draft)

1. **The R `seedDetectPreview` previewed a different algorithm.** It ran `LocalPeakSeedsUtils.detect_seeds`
   — local peak finding — for the seed-based backends (`blobdetect`, `donblo`, `ilee`,
   `morphoWatershed`). feijoa has none of those; cellpose has no seeds. Nothing to port literally.
2. **There is no cheap mode.** The R preview was cheap because peak detection is cheap. Cellpose has no
   lighter path, so cheapness must come from **doing less work** — one region — not a cheaper algorithm.
3. **A timepoint range was never asked for.** An earlier draft justified one with "does this hold across my
   timecourse"; that was invented, and is withdrawn. Add a range only if someone asks.

## Decisions

1. **The real compute, on a bounded region.** No approximation — the point is to judge the params you are
   about to run, so an approximation would answer a different question.
2. **The region is the current napari view in XY, ONE z-plane, the current timepoint** (Dominik,
   2026-07-31, revised the same day once the numbers came in). The first version of this decision previewed
   the whole z-stack, on the grounds that a single-plane preview shows something the run won't reproduce.
   That is true, and it costs ~90 s (Decision 8) with no trick available to reduce it — so **3D previews are
   dropped**: they are not previews.

   **If the viewer is in 3D display mode (`dims.ndisplay == 3`), the preview silently falls back to the
   current z-plane and says so** — it does not refuse, and it does not quietly pretend to be a 3D result.

   What the fallback actually costs, stated precisely because "approximate" is useless to a user: the real
   run is `do_3D=False` + `stitch_threshold`, i.e. **2D per plane, then stitched across z**
   (`cellpose_utils.py:121-134`). A one-plane preview runs the identical per-plane inference, so
   **diameter, boundaries and splitting are faithful**; what is missing is stitching, so **object counts and
   z-extents will differ from the run**. The warning should say that, not "results may vary".
3. **Output is an unpromoted staging store at FULL image shape**, filled only in the previewed region.
   - Full shape means the layer aligns with the image for free; a crop-shaped layer renders at the origin
     and needs a translate.
   - The cost is negligible because zarr only materialises written chunks — measured 2026-07-31: a
     `(50, 40, 2048, 2048)` uint32 store (33.6 GB dense) is **908 bytes** empty and **5 KB** after one
     512×512 region is written.
   - Never promoted ⇒ absent from `ccid.json` ⇒ no picker, `measureLabels`, tracking or gating can reach
     it, so a preview can never be mistaken for data. Disposal is already built: the `store-debris` data
     patch deletes `.partial` dirs.
   - `staged_store` promotes on clean exit, so this needs a `promote=False` mode rather than a hand-rolled
     staging path — otherwise it trips the `test_store_staging_convention` detector, correctly.
4. **Previewability is a declared trait, opt-in per task** — the same shape as `live_outputs`
   (`app/src/tasks/task.jl`), because in both cases the property belongs to the task's compute and not to
   tasks in general. A base method returns "not previewable"; each consumer adds one line.
   **It needs a `CompositeTask` overload.** This is exactly how the live preview shipped broken in #421:
   the segmentation module page runs `segment.cellposeMeasure`, whose steps execute via `_run_task` with no
   `TaskRecord` of their own, so the composite must answer for its steps. `_composite_steps` already exists
   for this.
5. **Global statistics come from the whole image, applied to the crop.** With `normaliseToWhole` on,
   percentiles over the visible region ≠ over the image, so a naive crop preview shows a result the full run
   won't reproduce — the preview would lie. `_compute_norm_params` already derives whole-image percentiles
   from a streamed histogram. AF and denoise normalise too, which is why this belongs to the mechanism
   rather than to segmentation.
6. **Runs on the GPU, not pooled** (Dominik, 2026-07-31). It uses the card like any cellpose run. It does
   not queue in a resource pool — a preview that waits behind a full segmentation is not a preview — so it
   belongs on the un-pooled rail (`jobs.jl:27`, "No pool or chain", how data patches and export/import
   already run). Pools stay for scheduler tasks. Consequence to observe rather than pre-empt: firing a
   preview during a run puts two cellpose processes on one card.
7. **A resident worker process, not compute inside the napari bridge.** The bridge is tempting — already
   resident, already knows the view, already does threaded work (`threading.Thread`, `run_in_executor`,
   `QTimer`) — but two costs decide it:
   - **Bridge restarts cost the view.** It isn't Revise-tracked, so iterating on preview code means
     relaunching it and losing the open image, layers, zoom and contrast. Restarting a worker costs a model
     reload.
   - **OOM blast radius.** Cellpose VRAM in the viewer means an OOM takes down the thing you are looking at,
     rather than a background process you retry.

   A fourth resident process is hard to justify for one feature; it is justifiable for two now
   (segmentation, AF) and three once coastal denoise lands — so this decision depends on the widened scope.
   Precedent exists (`mcp/`, a non-viewer resident Python service), as do the
   lifecycle primitives (`_kill_listeners_on_port`, `jobs.jl:70`, how `stop-napari` works).
8. **The worker is resident to amortise IMPORTS, not model loading** (measured 2026-07-31, RTX 2000 Ada
   Laptop 8 GB, `cyto2`). An earlier draft of this decision said model construction would dominate. It
   doesn't — it is essentially free:

   | Fixed cost, per process | |
   |---|---|
   | `import cecelia.utils` | **11.7 s** |
   | `import cellpose` (pulls torch) | 5.7 s |
   | `CellposeModel(cyto2)` construction | **0.2 s** |
   | total | **17.7 s** |

   17.7 s per invocation is fatal for an interactive loop, so the worker stays resident — but it pays that
   once at toggle-on, and holding *models* warm is a minor extra (`CellposeUtils._model_cache` already does
   it within a process).

   **Inference cost is proportional to CELLS, not pixels** — which kills the obvious optimisations. At a
   fixed diameter it looks like ~2 µs/voxel (512² plane 0.47 s, 1024² plane 2.2 s, 20×512² 9.7 s,
   40×1024² 89 s), but that is a coincidence of holding the diameter constant:

   | Region | Voxels | Time | µs/voxel |
   |---|---|---|---|
   | level 0: 40 × 1024², diam 17 px | 41.9 M | 90.1 s | 2.15 |
   | level 1: 40 × 512², diam 8.5 px | 10.5 M | 37.7 s | 3.60 |
   | level 2: 40 × 256², diam 4.25 px | 2.6 M | **35.9 s** | 13.69 |

   **16× fewer voxels buys only 2.5× less time.** Cellpose rescales internally to a canonical ~30 px
   diameter, so a scaled-down diameter makes it *upscale*: at level 2, diam 4.25 → a 7× upscale, and a 256²
   tile becomes ~1800² internally. Downsampling and diameter-scaling cancel. So:

   - **Previewing at napari's displayed pyramid level is NOT a shortcut** (an earlier draft claimed cost
     would "track the screen, not the image" — measured false).
   - **`batch_size` is not a shortcut either**: 88.4 s at the default 8, and slightly *worse* at 16/32/64.
   - **Z-depth is the whole problem.** 40 planes = 40× the cells, and nothing changes that arithmetic.

   What is left is the only thing that was ever going to work — **do less, and show it sooner**:

   | | |
   |---|---|
   | 1 plane at level 0 | **2.36 s** |
   | 1 plane at level 1 | **0.95 s** |

   So the preview segments **the plane you are looking at**, and nothing else (Decision 2).

   **Measured end-to-end on real data** (`EaMaVq`, 201 × 20 × 544 × 548 driftCorrected, cyto2, 10 µm,
   stitch 0.2, T-cell channel — the configuration `SEG_QUALITY_PLAN.md` benchmarked):

   | | |
   |---|---|
   | first preview on an image | **27.5 s** — of which 24 s is `_compute_norm_params` |
   | second preview, diameter 10 → 12 | **0.30 s** |

   The tuning loop is therefore **sub-second**, and the one-off cost is the whole-image normalisation
   statistic. That is cached per (image, channels, `normalise` percentile) — none of which are what you
   tune — so it is paid once per image and never again while you sweep diameter/thresholds/filters.

   Two things worth knowing about that 24 s: it is **specific to single-level stores**.
   `_compute_norm_params` reads the statistic off the smallest pyramid level when there is one, but a
   drift/AF-corrected store has only level 0, so it streams all 1.2 B voxels — and corrected stores are
   the normal segmentation input. And it cannot be cheapened by subsampling without changing the
   statistic, which would make the preview disagree with the run (Decision 5). See *Open questions*.
9. **One adder for preview layers, both kinds.** Labels previews can reuse `_show_label_stores`; **image**
   previews (AF now, denoise later) have no equivalent and must not grow into a second near-copy. `docs/NAPARI.md`
   records that the label adder had already drifted into two copies before it was consolidated — don't
   repeat that for images.

## Denoise waits for coastal — and that is why it isn't P0

An earlier draft opened with "extract the cellpose denoise compute into a reusable helper", because
`cleanupImages.cellposeCorrect` builds its `DenoiseModel` and calls `dn.eval` **inline in the runner**
(`app/src/tasks/cleanupImages/cellpose_correct_run.py:100-107`), where a worker cannot reach it — unlike AF
(`correction_utils.af_correct_image`) and segmentation (`CellposeUtils.predict_slice`), which are already
reusable.

**Dropped.** Cellpose denoise is unmaintained upstream and phased out in v4 (Dominik, 2026-07-31), so there
is no version to migrate it to; `coastal` replaces it. Extracting it would be refactoring code on its way
out, and the preview would then be re-pointed at the replacement anyway.

What to do instead: **define the denoise seam, not the extraction.** Segmentation already has one —
`SegmentationUtils` owns the loop and a backend implements `predict_slice`, so coastal adds a backend with a
`_run_task` plus param translation and nothing else. Whoever lands coastal denoise should give denoise the
same shape, and a denoise preview then costs one trait declaration. Recorded in `docs/SHIPPING.md` under the
pin, where someone bumping cellpose will actually read it.

So denoise is not a build target here, and **AF is the image-kind consumer instead** — it is also the harder
test, because it widens the channel axis rather than preserving the input shape.

(Unrelated, despite the name: `_af_denoise_frame` at `correction_utils.py:207` is skimage tv/wavelet
denoising *inside* AF correction, not the cellpose denoise task.)

## Phases

- **P1 — the mechanism + segmentation.** The previewable trait (+ composite overload), the region contract,
  the worker and its lifecycle, `staged_store(promote=False)`, the labels preview path (mostly existing),
  whole-image stats passed in. Ends with: toggle on, look at a region, see labels.
- **P2 — the image kind: AF correction.** The image-layer adder (Decision 9) plus AF's trait. This is what
  proves the component is general rather than segmentation-shaped, and AF stresses it properly by changing
  the channel count. If P1's interfaces only fit labels, this is where that shows — before anything else is
  built on them.
- **P2.5 — denoise, when coastal lands.** No work here now (see *Denoise waits for coastal*); if the
  mechanism and the seam are both right, this is one trait declaration.
- **P3 — the affordance.** A toggle in the viewer / module page, plus honest wording that seams and pyramid
  levels are not what the full run produces. UX primitives per `docs/UI.md` — no new button variants.

## Cross-file architecture

| File | Change |
|---|---|
| `app/src/tasks/task.jl` | the previewable trait + its `CompositeTask` overload (reuse `_composite_steps`) |
| `app/src/preview.jl` (new) | region resolution + worker request/lifecycle, algorithm-agnostic — the Julia half, mirroring how `segmentation.jl` holds the algorithm-agnostic segmentation half |
| `python/cecelia/utils/zarr_utils.py` | `staged_store(promote=False)` |
| `python/.../preview_worker.py` (new) | resident worker: warm models, one request → one region → write into the unpromoted store |
| `napari/napari_bridge.py` | one adder for preview layers of both kinds; a distinct suffix so a tuning preview isn't confused with a running run's `Labels (live)` |
| `api/src/napari_api.jl`, `api/src/server.jl` | preview toggle + status routes |
| `app/src/maintenance.jl` | nothing — the `store-debris` sweep already covers preview debris |
| Settings service panel | a fourth component entry (`SERVICE_PANEL_PLAN.md`) |

## Open questions

- **Worker transport: port + WS, or a long-lived stdin/stdout subprocess?** (Decide once — every consumer
  depends on it.) A port follows the existing
  precedent (bridge :7655, Pluto :7660, mcp) and is inspectable; a kept-alive subprocess needs no port, no
  WS server and no firewall consideration, and `jobs.jl` already tracks process handles. Leaning port for
  consistency — but the subprocess is genuinely simpler, and this is worth deciding once, deliberately,
  since three modalities will depend on it.
- **Is a 25 s first preview acceptable?** It is one-off per image (then 0.3 s/iteration), and it buys
  exactly matching the run's normalisation. Options if not: warm it at toggle-on so the wait is
  attributable rather than surprising; or offer `normaliseToWhole=false` for previewing, which is faster
  but then the preview is NOT what the run produces — the thing Decision 5 exists to prevent.
- **An empty region reads as "0 cells", which is misleading.** Drift correction pads the canvas (and the
  padding moves per timepoint — at t=0 on `EaMaVq`, z 0–6 is dead space), so a preview aimed there
  segments an all-zero tile and honestly reports nothing found. Distinguish "no signal in this region"
  from "no cells found": the tile's own max is enough to tell them apart, and the wrong one sends someone
  hunting for a parameter problem that isn't there.
- **Model load latency** — measured, not a factor (0.2 s). It decides whether the worker holds several models at once
  (segmentation + denoise, if someone toggles between them) or one at a time.
- **Does `predict_slice` work on an arbitrary crop** outside the tiling/stitching loop that normally calls
  it?
- **One preview layer that replaces itself, or one per attempt?** Comparing attempt A with B is the actual
  job, which argues for keeping the previous one — but then eviction and naming need thought.

## Related

- [`SEG_QUALITY_PLAN.md`](SEG_QUALITY_PLAN.md) — finding good params **objectively** (QC-gate pass rate over
  a whole image). This plan is the fast *visual* loop; that one is the measurement. Looking is what missed
  87–92% over-segmentation in the first place, so they should report the same metric rather than two
  similar ones.
- [`SERVICE_PANEL_PLAN.md`](SERVICE_PANEL_PLAN.md) — the start/stop/restart + status model the worker's
  lifecycle should follow.
- `docs/SEGMENTATION.md` → *Previewing a running run* — the existing live preview, whose surfacing path
  (`live_outputs` → ⚡ toggle → `refresh_labels`) this reuses.
