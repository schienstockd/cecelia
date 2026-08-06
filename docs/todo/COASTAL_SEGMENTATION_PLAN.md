# Coastal as a cecelia segmentation method — extending the base for temporal algorithms

**Status:** planning (2026-08-06). Prompted by measurements in
[`SEGMENTATION_OPEN_PROBLEM.md`](SEGMENTATION_OPEN_PROBLEM.md): after removing coastal's 8-bit cast
and separating seed blur from outline blur, the flow segmenter reaches parity with a scipy intensity
baseline on `fXgbTl` mem-TOM (33 objects vs 30, 12 µm vs 12 µm) **and** separates touching cells that
the baseline merges. It is worth making a real task.

## The problem this plan exists to solve

`SegmentationUtils` (`python/cecelia/utils/segmentation_utils.py`) is the extension point — the
analogue of the old R `SegmentationUtils`. A new algorithm subclasses it and implements one method:

```python
def predict_slice(self, tile, model_params, norm_params=None):
    """tile=[C,Z,Y,X] or [C,Y,X]. Returns uint32 label mask."""
```

and inherits XY/Z tiling, per-timepoint streaming to the label store, IoU seam stitching, size
filtering, expansion/erosion, border clearing, nuc↔cyto matching, and the multiscale pyramid.
`CellposeUtils` is the only subclass today.

**Coastal does not fit that interface.** It is temporal: the flow metrics for frame *t* are computed
from frames *t±8*, and the model is *trained on the movie* before any inference. `predict_slice`
receives a single timepoint and no model.

Three ways to resolve it, and only one is acceptable:

| option | verdict |
|---|---|
| Split train/predict, keep `predict_slice` as-is | Does not work. The metrics still need neighbouring frames the method never sees. |
| Override `predict_from_zarr` so coastal owns its loop | **Rejected.** It re-implements tiling, streaming, stitching and post-processing — a second variant of the exact thing the base exists to do once. This is the divergent-re-implementation trap in `CLAUDE.md`. |
| **Extend the base with an opt-in temporal window** | **Chosen.** One declarative change; cellpose is unaffected; coastal keeps tiling and streaming. |

Dominik, 2026-08-06: *"we should make a plan to extend the base. i don't want special cases here."*

## Locked decisions

1. **The requirement is declared by the subclass, never special-cased in the base.** The base must
   contain no mention of coastal. A subclass sets a class attribute:
   ```python
   class CoastalUtils(SegmentationUtils):
       TEMPORAL_RADIUS = 8        # frames either side; 0 (base default) = current behaviour
   ```
   The base reads it and satisfies it. Any future temporal method gets the same treatment for free.

2. **The window is read PER TILE, not per frame.** This is the load-bearing decision. The base
   currently does `read_timepoint(...)` for one timepoint and tiles it in RAM. Widening *that* to a
   window puts `W` full frames in memory — at `Dml3RG` resolution (1036×1055×35×4ch, uint16) a frame
   is ~300 MB, so a ±8 window is ~5 GB, and the whole point of the current design is that peak memory
   is one frame. Reading the window at **tile** extent keeps it at `W × tile`, i.e. tens of MB.
   Cost: `W` zarr reads per tile instead of 1. Chunks are 1024² so a 512 tile already pulls a whole
   chunk; this is IO-bound but bounded, and `read_timepoint` is the one canonical reader either way.

3. **`predict_slice` gains an optional keyword, not a changed signature.**
   ```python
   def predict_slice(self, tile, model_params, norm_params=None, context=None):
   ```
   `context` is `[W, C, Z, Y, X]` (the tile through time, centred on `t`) and is `None` unless the
   subclass asked for it. `CellposeUtils` is not touched.

4. **Edges clamp, they do not reflect.** At `t < r` or `t > T-1-r` the window is truncated and the
   centre index reported, rather than mirroring frames. Reflected frames would create motion that is
   not there — the same class of artefact as the 8-bit cast manufacturing background flow.

5. **Training is a separate phase from tiled inference, and it does not happen per tile.** A model
   trained on one 512² tile has not seen the movie. See Phase 2 — this is the part with real design
   left in it.

6. **One parameter value per SET, exposed — never a per-image default.** Dominik, 2026-08-06:
   *"we should expose the blur. and it should be the same across images. ideally you wouldn't use
   different params for different images in the same set."* Varying segmentation parameters between
   images in a set makes anything computed across that set incomparable, so the task must not
   auto-tune per image, and there must be no hidden per-image override. `seed_blur_sigma` is a
   first-class, user-visible param with a documented meaning, not an internal constant.

   Note the calibration this leaves open: `seed_blur 8` matches an intensity baseline on `fXgbTl`
   (~33 objects vs ~30) while `seed_blur 5` was needed to stop visible merging on `Dml3RG` t=92 —
   **and `fXgbTl` is a crop of `Dml3RG`**, so this is one image disagreeing with itself across time
   range and field size, not two images wanting different values. It cannot be resolved by per-image
   tuning. It needs a real yardstick (Phase 4's QC gate), not a second parameter.

7. **Trained models live in the config dir, like the cellpose vault.** Dominik: *"maybe it should
   just be in config like the cellpose vault. to use it across projects."* So
   `<config_dir>/models/coastalModels/`, matching `cellposeModels/` exactly. Consequence to accept
   knowingly: a model does NOT travel with a `.ccbundle` project export, so a shared project will
   reference a model the recipient does not have. The manifest (below) must therefore record enough
   to identify and re-train it, and the task must fail loudly — not silently fall back — when a named
   model is absent.

8. **Exposed params include the seed diameter, not just the blur.** `seed_size` is the local-maximum
   window that decides how close two cells may be and still get separate seeds; on this data it is a
   fraction of a cell diameter and directly controls merging. It is as consequential as
   `seed_blur_sigma` and gets the same treatment: visible, documented, one value per set.

9. **3D needs no separate code path — the base already does it.** `is_3d = dim_utils.is_3D()`,
   `predict_slice` receives `[C, Z, Y, X]`, and `_crop_masks`/`post_process` branch on it. A subclass
   returns `[Z, Y, X]`. Cellpose handles Z internally via `stitch_threshold`; coastal does per-z 2D
   plus `utils.match_masks_3d` inside `predict_slice`. Per-plane-then-IoU is the right choice here
   anyway: voxels are 6x anisotropic (2.0 um z vs 0.331 xy), and `SEGMENTATION_OPEN_PROBLEM.md`
   records that OpticalFlow3D's axial speeds are not credible at that sampling.

10. **Two-pass is generic, and it is just stacking model groups.** Dominik, 2026-08-06: *"two pass
   as generic. because then we don't have to reinvent if we add another segmenter. because it's
   literally just running the same thing twice ... you can stack up multiple segmentation groups on
   top of another."* So multi-pass is NOT a coastal feature and not a new mechanism — the repeatable
   `models` group in the task JSON already IS the stacking UI, and `predict_from_zarr` already loops
   `for model_key in sorted(models.keys())`.

   `matchAs` is not the lever: it pairs base/nuc, and `_match_nuc_cyto` re-assigns nucleus IDs to
   cytoplasm IDs by IoU. That is nucleus anchoring, a different operation.

   **What actually has to change is one line.** `_write_tile_to_arr` merges with
   `arr[idx] = np.maximum(arr[idx], masks)`. Within a group that is harmless — `_crop_masks` removes
   the overlap so write regions are disjoint and the maximum only ever combines with zeros. ACROSS
   groups it is not: every group's labels are offset by the running `max_labels[match_as]`, so a
   later group's IDs are always numerically larger and `np.maximum` silently lets the later group
   win every overlapping pixel. A small-diameter second pass therefore eats the first pass's cells.

   Fill semantics = keep what is already labelled, write only into unlabelled pixels. Nobody wants
   a later pass to clobber an earlier one, so this is a **bug fix as much as a feature**.

   **Contract tests:** (a) a single-group run is byte-identical to today; (b) two groups sharing
   `matchAs` leave the first group's labels untouched and the second only fills gaps; (c) tile seams
   within one group are unaffected. Cellpose gets a smaller-diameter second pass for free.

11. **Small-object handling stays in the base where the base already has it.** `minCellSize` /
   `cellSizeMax` already exist. Coastal's two-pass produces both size classes; the cells-vs-bodies
   split is a **gating** decision downstream, not a segmentation parameter (Dominik: *"in the real
   world we would just use the gating option in cecelia"*). So no apoptotic-body criterion is
   encoded in the task.

## Phases

### Phase 1 — temporal context in the base  `[small, self-contained]`
- `TEMPORAL_RADIUS = 0` class attribute on `SegmentationUtils`.
- In `predict_from_zarr`, when `TEMPORAL_RADIUS > 0`, build `context` per tile from
  `zarr_utils.read_timepoint` across the clamped window and pass it to `predict_slice`.
- `predict_slice` gains `context=None`.
- **Tests:** a stub subclass with `TEMPORAL_RADIUS=2` asserts (a) it receives the right frames in
  the right order, (b) windows clamp at both ends without reflecting, (c) with `TEMPORAL_RADIUS=0`
  the call is byte-identical to today (`CellposeUtils` must not change), (d) peak memory stays
  tile-scaled — assert the base never materialises more than one full frame.
- Exit: cellpose output unchanged, stub gets its window.

### Phase 2 — the model vault  `[DECIDED 2026-08-06; the mechanism already exists]`

Dominik: *"it might be different for different cells. so we sort of need a solid vault that people
can fill with their own models based on their own data."* So: **a trained model is a versioned
artefact the user owns**, not something a segmentation run produces and throws away.

**Discovery — do not design this from scratch.** cecelia already ships exactly this pattern for
custom cellpose checkpoints (`INVENTORY.md` → *Custom cellpose checkpoints (drop-in)*):

| piece | cellpose (exists) | coastal (mirror it) |
|---|---|---|
| resolver | `cellpose_model_path(name)` in `config.jl` — user override → bundled → nothing | `coastal_model_path(name)` |
| enumeration | `list_cellpose_models()` — built-ins + `<install>/models/cellposeModels/` + **user `<config_dir>/models/cellposeModels/`**, deduped, user wins | `list_coastal_models()` over `<config_dir>/models/coastalModels/` |
| picker | the Model select's `options` rewritten at spec-load by `_inject_dynamic_options!(::CellposeSegment)`, through the generic `_needs_dynamic_options` / `_inject_dynamic_options!` hook in `tasks/task.jl` | `_inject_dynamic_options!(::CoastalSegment)` |
| consistency | the same hook backs `/api/tasks/definitions`, so picker and `validate_params` cannot disagree | free |

So the vault is a **drop-in directory plus two `config.jl` functions**, not new architecture. A user
drops a model in (or trains one), and it appears in the picker.

What still has to be decided, and is genuinely new:

- **What a model artefact contains.** Weights alone are not enough to be reproducible: the metric
  set, `temporal_scales`, `cumulative_window`, input spatial sigma and channel must travel with it,
  because inference MUST use the same metric keys it was trained on (coastal's
  `test_flow_metric_count.py` documents that mismatch as silent — channels shift and the model is
  fed misaligned inputs). Proposal: `<name>.pt` + a sibling `<name>.json` manifest recording those
  params, the source image/channel, and the date. The segmentation task reads the manifest and
  configures itself, so the user cannot pair a model with the wrong metric set.
- **Where trained models land** — `<config_dir>/models/coastalModels/` (matches cellpose, survives
  projects) vs inside the project (travels with a `.ccbundle` export). Cellpose precedent says
  config dir; the export story argues for the project. **Undecided.**
- **Cohort scope.** Train on one representative movie, apply across a set — the manifest is what
  makes that auditable later.

### Phase 3 — `CoastalUtils` + the task  `[mechanical once 1 and 2 land]`
- `python/cecelia/utils/coastal_utils.py`: `CoastalUtils(SegmentationUtils)` with
  `TEMPORAL_RADIUS = max(temporal_scales)` and `predict_slice` doing flow → metrics → two-pass
  inference on the context window.
- `app/src/tasks/segment/coastal.{jl,json,_run.py}` + two lines in `task_registry.jl`.
- Task params surface the ones that were measured to matter: spatial sigma, `seed_blur_sigma`
  (large/small), `seed_size`, `prob_threshold`, `affinity_threshold`, `min_component_size(_small)`.
- **QC is mandatory** (`docs/MODULES.md`): object count per label type is already returned by
  `predict_from_zarr`; add a `warn` for the unambiguous bad case (e.g. count collapse or a
  fragmentation blow-up) and cohort metrics.

### Phase 4 — close the loop with gating  `[the actual deliverable]`
Measurements → `labelProps` → gating. `zolIMa` currently has **no** segmentation, `labelProps` or
gating sidecar, which is why every quality number in `SEGMENTATION_OPEN_PROBLEM.md` is an
object-count proxy rather than `SEG_QUALITY_PLAN.md` Decision 1's QC-gate yield. Phase 4 is what
makes the established yardstick usable on this data, and what lets gating do the cells-vs-bodies
split.

## Risks

- **Phase 1 touches a base class used by the only shipped segmenter.** The byte-identical test for
  `TEMPORAL_RADIUS=0` is the contract; it must exist before anything else merges.
- **IO cost of per-tile windows is unmeasured.** `W` reads per tile could dominate on a slow share.
  Measure before assuming; a per-frame LRU cache of decoded chunks is the escape hatch, but do not
  build it speculatively.
- **Coastal's parameters are tuned on one crop, one z-plane, one channel.** They are defaults to
  start from, not settled values. The 3 dropped metric planes in particular were identified on
  mem-TOM only — `divergence`/`vorticity` may carry signal on rotating or converging populations.
- **Coastal is a git dependency**, so a cecelia task depending on it pins cecelia to a coastal
  revision. Already true for `cleanupImages.smooth`; this widens the exposure.

## References
- [`SEGMENTATION_OPEN_PROBLEM.md`](SEGMENTATION_OPEN_PROBLEM.md) — the measurements this rests on,
  including which earlier numbers are unreliable.
- [`SEG_QUALITY_PLAN.md`](SEG_QUALITY_PLAN.md) — Decision 1, the QC-gate yardstick (Phase 4).
- `docs/SEGMENTATION.md` — the pipeline this plugs into.
- `docs/MODULES.md` — the task trio + mandatory QC.
- coastal `docs/SEGMENTATION.md` — the segmenter's own parameters and known issues.
