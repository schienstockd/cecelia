"""
Coastal flow-metric segmentation subclass.

Coastal segments by MOTION rather than appearance: a UNet is fed optical-flow metric planes derived
from a window of frames around the timepoint, and its embedding head drives region growing. That
makes it the first segmenter here whose prediction for `t` depends on frames other than `t`, which
is the whole reason `SegmentationUtils.TEMPORAL_RADIUS` exists — the base supplies the window, and
tiling, streaming, seam stitching, post-processing and the label store stay where they already are.

Three things in here are easy to get silently wrong, and each has a test:

* **The temporal radius must be the largest scale, not one less.** A window is TRUNCATED at the
  movie's ends (never reflected — a mirrored frame invents motion), so at `t=0` the window is only
  `r+1` frames. Coastal drops `mag_{scale}` when the window is shorter than `scale+1`, and a missing
  plane does not leave a hole at its own channel: `predict_frame` stacks in `sorted(key)` order and
  zero-fills the remainder, so every later metric shifts down a slot and the model is fed misaligned
  inputs with no error. `r = max(scales)` makes even the truncated end-windows long enough.
* **Photometric scaling must be global.** Training normalises by the whole movie's min/max. A tile's
  own window would give every tile its own scale — the patchiness `normaliseToWhole` exists to
  prevent, plus a train/inference mismatch. The base's `norm_params` are that global statistic;
  `value_range=(0, 255)` then pins coastal's second scaling step to what training saw.
* **The metric set must be the one the model was trained on.** It travels in the model's manifest,
  not in the task's params, because a user cannot be expected to re-enter it correctly and coastal
  fails silently when it is wrong.

See `docs/todo/COASTAL_SEGMENTATION_PLAN.md` and coastal's `docs/SEGMENTATION.md`.
"""

import json
import os

import numpy as np

from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.utils.gpu_utils import torch_device
import cecelia.utils.script_utils as script_utils


# Coastal's own training defaults (`prepare_data_for_unet`). Used only when a model has no manifest
# — i.e. a checkpoint dropped into the vault by hand rather than produced by the training task.
DEFAULT_TEMPORAL_SCALES = [1, 2, 4, 8]
DEFAULT_CUMULATIVE_WINDOW = 5

# Coastal's normalisation percentiles (`normalize_and_project`). The task's `normalise` param
# reaches the same place through the base's `_compute_norm_params`; this is the fallback when a
# tile has no global statistic (`normaliseToWhole` off).
PERCENTILE_LO = 0.01
PERCENTILE_HI = 99.99

# What the projected window is scaled to before coastal sees it — coastal's own convention, kept so
# a model trained through `normalize_and_project` receives the range it was trained on.
PROJECTION_MAX = 255.0


def manifest_path(model_path):
    """The `<name>.json` sitting beside `<name>.pt`."""
    return os.path.splitext(str(model_path))[0] + '.json'


def read_manifest(model_path):
    """Training provenance for a vault model — `{}` when there is none.

    A hand-dropped checkpoint has no manifest and falls back to coastal's training defaults, which
    is a guess. The training task always writes one; see `list_coastal_models` in `config.jl`, which
    reads the same file to label the picker.
    """
    path = manifest_path(model_path)
    if not os.path.isfile(path):
        return {}
    with open(path, encoding='utf-8') as f:
        return json.load(f)


def temporal_config(manifest):
    """`(scales, cumulative_window, dropped_metrics)` — the flow feature set a model expects.

    `droppedMetrics` records planes deliberately excluded at training. Three of the 15 carry no
    cell/background structure on intravital data (divergence, vorticity, flow_structure_alignment;
    cell/bg ratios 0.99/1.00/1.65), so a model may be trained without them — and inference must then
    drop exactly the same ones or every later channel shifts.
    """
    scales = [int(s) for s in (manifest.get('temporalScales') or DEFAULT_TEMPORAL_SCALES)]
    cumulative = int(manifest.get('cumulativeWindow') or DEFAULT_CUMULATIVE_WINDOW)
    dropped = tuple(str(k) for k in (manifest.get('droppedMetrics') or ()))
    return sorted(set(scales)), cumulative, dropped


class CoastalUtils(SegmentationUtils):

    def __init__(self, params, dim_utils):
        super().__init__(params, dim_utils)

        self.use_gpu, self.gpu_device = torch_device()
        self._model_cache = {}
        self._inference_cache = {}
        self._manifest_cache = {}

        models = params.get('models') or {}
        if not models:
            raise ValueError('coastal segmentation needs at least one model group')

        # The radius is a property of the RUN, not of one model group: the base builds one window
        # per tile and hands it to every group. Stacking a second group with different scales
        # therefore widens the window for both, which is harmless (each group indexes the flows it
        # needs) but must be the max, never the first group's.
        self.TEMPORAL_RADIUS = max(
            max(temporal_config(self._manifest(mp))[0]) for mp in models.values())

        if not dim_utils.is_timeseries():
            raise ValueError(
                'coastal segments by motion and needs a time series; this image has no T axis')

        n_t = int(dim_utils.dim_val('T'))
        if n_t < self.TEMPORAL_RADIUS + 1:
            # Not a warning. Coastal would drop the largest scale's plane and shift every later
            # channel, producing a plausible-looking wrong mask.
            raise ValueError(
                f'coastal needs at least {self.TEMPORAL_RADIUS + 1} timepoints for temporal scale '
                f'{self.TEMPORAL_RADIUS}; this image has {n_t}. Train a model with smaller '
                f'temporal scales, or segment a longer movie.')

    # ── Model + manifest ──────────────────────────────────────────────────────

    def _manifest(self, model_params):
        path = str(model_params.get('model', ''))
        if path not in self._manifest_cache:
            self._manifest_cache[path] = read_manifest(path)
        return self._manifest_cache[path]

    def _get_inference(self, model_params):
        """A configured `LearnedAffinityInference` per model path + parameter set.

        Two-pass segmentation is NOT built here: it is a second `models` group in the task, because
        stacking groups is how multi-pass is expressed for every segmenter (see
        `_write_tile_to_arr`). `TwoPassSegmentationInference` would be a coastal-only mechanism for
        something the base already does.
        """
        cache_key = json.dumps({k: v for k, v in sorted(model_params.items())
                                if k not in ('cellChannels', 'nucChannels')}, default=str)
        if cache_key in self._inference_cache:
            return self._inference_cache[cache_key]

        from coastal.segment import LearnedAffinityInference
        from coastal.train import load_model

        model_path = str(model_params.get('model', ''))
        if not os.path.isfile(model_path):
            raise FileNotFoundError(
                f'coastal model not found: {model_path!r}. It is resolved from the vault by the '
                f'Julia handler; a project shared from another machine will not carry it.')

        if model_path not in self._model_cache:
            self._model_cache[model_path] = load_model(
                model_path, device=self.gpu_device if self.use_gpu else 'cpu')

        inference = LearnedAffinityInference(
            self._model_cache[model_path],
            device=self.gpu_device if self.use_gpu else 'cpu',
            affinity_threshold=float(model_params.get('affinityThreshold', 0.5)),
            merge_affinity_threshold=float(model_params.get('mergeAffinityThreshold', 0.65)),
            merge_max_distance=float(model_params.get('mergeMaxDistance', 1.5)),
            prob_weight=float(model_params.get('probWeight', 0.3)),
            seed_size=int(model_params.get('seedSize', 12)),
            prob_threshold=float(model_params.get('probThreshold', 0.3)),
            embedding_blur_sigma=float(model_params.get('embeddingBlurSigma', 1.5)),
            prob_blur_sigma=float(model_params.get('probBlurSigma', 0.0)),
            seed_blur_sigma=float(model_params.get('seedBlurSigma', 0.0)),
            max_iter=int(model_params.get('maxIter', 200)),
            min_component_size=int(model_params.get('minComponentSize', 20)),
        )
        self._inference_cache[cache_key] = inference
        return inference

    # ── Input preparation ─────────────────────────────────────────────────────

    def _project_window(self, context, model_params, norm_params):
        """`[W, C, ...]` window → `[W, ...]` single-channel float32 in 0–255.

        The same two steps as coastal's `normalize_and_project` (per-channel percentile clip, then
        maximum across channels, then scale), with one deliberate difference: the clip range comes
        from the base's GLOBAL `norm_params` rather than from this window. Per-window percentiles
        would make a tile's normalisation depend on which tile it is.
        """
        channels = script_utils.channel_indices(
            model_params.get('cellChannels'), 'cellChannels',
            'coastal_models_for_python (coastal.jl)') or [0]

        projected = None
        for ch in channels:
            arr = np.asarray(context[:, ch], dtype=np.float32)
            if norm_params and ch in norm_params:
                lo, hi = norm_params[ch]
            else:
                lo = float(np.percentile(arr, PERCENTILE_LO))
                hi = float(np.percentile(arr, PERCENTILE_HI))
            arr = np.clip((arr - lo) / (hi - lo + 1e-8), 0.0, 1.0)
            projected = arr if projected is None else np.maximum(projected, arr)

        return (projected * PROJECTION_MAX).astype(np.float32)

    # The two coastal entry points sit behind one-line methods so the imports stay lazy (importing
    # this module must not pull torch) and so tests can exercise the window/tiling logic without a
    # trained checkpoint. Patching `sys.modules['coastal.*']` instead would leak a stub into every
    # later test in the process — it did, and broke an unrelated runner-import test.

    def _flow_metrics(self, window, center, scales, cumulative):
        from coastal.flow import flow_metrics_for_frame
        return flow_metrics_for_frame(window, center, temporal_scales=scales,
                                      cumulative_window=cumulative,
                                      value_range=(0.0, PROJECTION_MAX))

    def _match_3d(self, planes, stitch_threshold):
        from coastal.utils import match_masks_3d
        return match_masks_3d(planes, stitch_threshold=stitch_threshold)

    # ── Prediction ────────────────────────────────────────────────────────────

    def predict_slice(self, tile, model_params, norm_params=None,
                      context=None, context_index=None):
        """Segment one XY tile at one timepoint from its temporal window.

        tile:    [C, Z, Y, X] or [C, Y, X] — present for the base's contract; the pixels used come
                 from `context[context_index]`, which is the same data.
        context: [W, C, Z, Y, X] or [W, C, Y, X]
        Returns: uint32 labels [Z, Y, X] or [Y, X]
        """
        if context is None or context_index is None:
            raise ValueError(
                'CoastalUtils needs the temporal window; the base supplies it when '
                'TEMPORAL_RADIUS > 0 (see SegmentationUtils.predict_slice)')

        is_3d = (tile.ndim == 4)
        scales, cumulative, dropped = temporal_config(self._manifest(model_params))
        inference = self._get_inference(model_params)
        window = self._project_window(context, model_params, norm_params)

        if not is_3d:
            return self._predict_plane(
                window, context_index, scales, cumulative, dropped, inference)

        # Per-Z 2D, then IoU matching across Z. Right choice here rather than a fallback: voxels are
        # ~6x anisotropic on this data (2.0 µm z vs 0.33 xy), so a 3D flow field is not credible at
        # that sampling. `Inference3D` wraps exactly these two coastal primitives, but prints one
        # line per slice — once per tile per timepoint that is thousands of lines in the task log —
        # and the base already owns the loop it would add.
        planes = [self._predict_plane(window[:, z], context_index, scales, cumulative,
                                      dropped, inference)
                  for z in range(window.shape[1])]
        matched = self._match_3d(planes, float(model_params.get('stitchThreshold', 0.0)))
        return np.stack(matched).astype(self.LABEL_DTYPE)

    def _predict_plane(self, window, center, scales, cumulative, dropped, inference):
        """One 2D plane: `[W, Y, X]` window → uint32 labels `[Y, X]`."""
        frame, metrics = self._flow_metrics(window, center, scales, cumulative)
        if dropped:
            metrics = {k: v for k, v in metrics.items() if k not in dropped}
        _, instances, _ = inference.predict_frame(frame, metrics)
        return np.asarray(instances).astype(self.LABEL_DTYPE)
