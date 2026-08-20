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
import threading
from concurrent.futures import ThreadPoolExecutor

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

# How many z-planes are worked on at once. The per-z loop is the whole cost of a 3D coastal run and
# it was serial, so a 32-core machine ran it on one core. The two stages are sized SEPARATELY
# because they scale differently — measured on zolIMa/fXgbTl, 32 z-planes of one timepoint
# (420x441, RTX 2000 Ada):
#
#   flow metrics    17.9 s serial | 4.2 s @8 (4.3x) | 2.9 s @16 (6.2x) | 2.6 s @32 (6.8x)
#   predict_frame   11.5 s serial | 5.8 s @4 (2.0x) | 8.2 s @8 (1.4x)  | 9.3 s @16 (1.2x)
#
# Flow is `cv2`/numpy and releases the GIL, so it scales. `predict_frame` is region growing in a
# Python loop over scipy calls, so it peaks at 4 threads and gets WORSE beyond that — a single pool
# sized for flow would have run the second stage 1.6x slower than 4 threads does. Hence two stages
# and a barrier, not one map over `_predict_plane`.
#
# Capped at 8 rather than `cpu_count()` for the same reason `correction_utils._FFT_WORKERS` is:
# `segment.coastal` holds the `gpu` pool slot (limit 1) but the `cpu` pool has 20 more, so a run is
# not alone on the machine. 8 gets 4.3x of the available 6.8x.
FLOW_WORKERS = max(1, min(8, os.cpu_count() or 1))
PREDICT_WORKERS = max(1, min(4, os.cpu_count() or 1))

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
        self._quieten_cv2_threads()

        # Model input shared between the model groups that read one window — see
        # *Sharing the flow between passes*. Guarded because `_map_z` fills it from several threads.
        self._feature_cache = {}
        self._feature_cache_key = None
        self._feature_lock = threading.Lock()
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

    @staticmethod
    def _quieten_cv2_threads():
        """Hand OpenCV ONE thread, because this class now parallelises over z itself.

        `calcOpticalFlowFarneback` does not benefit from OpenCV's internal threading at this frame
        size — 59.7 ms/call at 1 thread vs 66.6 ms at 32 on a 420x441 plane, i.e. slightly WORSE —
        so the parallelism is only useful one level up, across z. Left at the default the two
        nest: FLOW_WORKERS threads each fanning out to `cpu_count()`, which oversubscribes the box
        and costs more than it buys.

        Process-global (OpenCV has no per-thread setting), which is safe here because it is set
        from the segmentation task's own subprocess (`coastal_run.py`) and cv2 reaches this env
        only as a coastal dependency — nothing else in cecelia imports it. Lazy, like every other
        coastal-side import in this module: `import cecelia.utils.coastal_utils` must not require
        coastal to be installed.
        """
        try:
            import cv2
        except ImportError:
            return
        cv2.setNumThreads(1)

    # ── Model + manifest ──────────────────────────────────────────────────────

    # Keys a caller may supply directly when there is no trained model to read them from.
    _FEATURE_KEYS = ('temporalScales', 'cumulativeWindow', 'droppedMetrics')

    def _manifest(self, model_params):
        """The feature-set config for a model group.

        A real manifest ALWAYS wins — inference must match training, and letting a task param
        override it is the silent-channel-shift bug this whole contract exists to prevent. The
        fallback to the group's own keys is for the case with no model at all: the flow-metrics
        contact sheet computes the planes so the user can decide which are worth training on, and
        it must honour the scales they picked rather than coastal's defaults.
        """
        path = str(model_params.get('model', ''))
        if path not in self._manifest_cache:
            self._manifest_cache[path] = read_manifest(path)
        manifest = self._manifest_cache[path]
        if manifest:
            return manifest
        return {k: model_params[k] for k in self._FEATURE_KEYS if k in model_params}

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
            # Thresholds are unitless and pass straight through; everything that describes a
            # LENGTH or an AREA arrives in microns and is converted here. Coastal's own API is in
            # pixels — correctly, it is an array library and knows nothing about calibration — so
            # this boundary is where the unit changes, once.
            affinity_threshold=float(model_params.get('affinityThreshold', 0.5)),
            merge_affinity_threshold=float(model_params.get('mergeAffinityThreshold', 0.65)),
            merge_max_distance=self.px_from_um(model_params.get('mergeMaxDistance', 0.5)),
            prob_weight=float(model_params.get('probWeight', 0.3)),
            # a local-maximum WINDOW, so it must be a whole number of pixels and at least 3
            seed_size=max(3, int(round(self.px_from_um(model_params.get('seedSize', 4.0))))),
            prob_threshold=float(model_params.get('probThreshold', 0.3)),
            embedding_blur_sigma=self.px_from_um(model_params.get('embeddingBlurSigma', 0.5)),
            prob_blur_sigma=self.px_from_um(model_params.get('probBlurSigma', 0.0)),
            seed_blur_sigma=self.px_from_um(model_params.get('seedBlurSigma', 2.5)),
            max_iter=int(model_params.get('maxIter', 200)),
            min_component_size=self.px_area_from_um2(model_params.get('minComponentSize', 2.0)),
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
                      context=None, context_index=None, context_id=None):
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
        feature_key = self._feature_key(context_id, model_params, scales, cumulative, dropped)

        if not is_3d:
            frame, metrics = self._cached_features(
                feature_key, 0, window, context_index, scales, cumulative, dropped)
            return np.asarray(inference.predict_frame(frame, metrics)[1]).astype(self.LABEL_DTYPE)

        # Per-Z 2D, then IoU matching across Z. Right choice here rather than a fallback: voxels are
        # ~6x anisotropic on this data (2.0 µm z vs 0.33 xy), so a 3D flow field is not credible at
        # that sampling. `Inference3D` wraps exactly these two coastal primitives, but prints one
        # line per slice — once per tile per timepoint that is thousands of lines in the task log —
        # and the base already owns the loop it would add.
        #
        # The two halves are mapped over z SEPARATELY, at their own widths — see FLOW_WORKERS /
        # PREDICT_WORKERS for the measurements that set them. Both maps preserve z order
        # (`Executor.map` yields in input order), so `_match_3d` still sees the planes bottom-to-top
        # and stitching is unchanged; the labels themselves are per-plane and independent, so this
        # is a scheduling change only and the output is identical to the serial loop.
        n_z = window.shape[1]
        features = self._map_z(
            FLOW_WORKERS, n_z,
            lambda z: self._cached_features(feature_key, z, window[:, z], context_index,
                                            scales, cumulative, dropped))
        planes = self._map_z(
            PREDICT_WORKERS, n_z,
            lambda z: np.asarray(inference.predict_frame(*features[z])[1]).astype(self.LABEL_DTYPE))

        matched = self._match_3d(planes, float(model_params.get('stitchThreshold', 0.0)))
        return np.stack(matched).astype(self.LABEL_DTYPE)

    @staticmethod
    def _map_z(workers, n_z, fn):
        """`[fn(z) for z in range(n_z)]`, on `workers` threads, in z order.

        Falls back to a plain loop at one worker so a single-thread run carries no pool at all —
        which is also what makes a failure inside `fn` raise its own traceback rather than one
        wrapped by the executor.
        """
        if workers <= 1 or n_z <= 1:
            return [fn(z) for z in range(n_z)]
        with ThreadPoolExecutor(max_workers=min(workers, n_z)) as pool:
            return list(pool.map(fn, range(n_z)))

    # ── Sharing the flow between passes ───────────────────────────────────────
    #
    # Two-pass segmentation — cells in one pass, apoptotic fragments in a second — is expressed as a
    # second `models` group, which is how EVERY segmenter here does multi-pass (see
    # `SegmentationUtils._write_tile_to_arr`). The two groups differ only in their region-growing
    # parameters: same channel, same model, same manifest. So they derive the SAME optical flow from
    # the SAME window, and computing it twice doubled the single most expensive step in the task for
    # nothing.
    #
    # The base now builds one window per (tile, timepoint) and stamps it with `context_id`, so the
    # groups sharing a window can be recognised. What is cached is the model INPUT — the projected
    # frame and its metric planes — because that is everything upstream of the first
    # group-dependent decision.
    #
    # Correctness rests on the key: two groups hit the same entry only when their window, channel
    # projection and feature set all match, and `context_id` is never reused. `normalise` is in the
    # key too — it reaches the pixels through `_project_window`'s `norm_params`, so two groups that
    # disagree about it are not looking at the same frame.
    #
    # Held for ONE window at a time (~13 float32 planes per z, ~330 MB on a 32 x 420 x 441 tile), and
    # only when there is more than one group to share it with: a single-pass run allocates nothing
    # and behaves exactly as before.

    def _feature_key(self, context_id, model_params, scales, cumulative, dropped):
        """What makes two model groups' flow inputs interchangeable — or None to not cache.

        None whenever caching cannot pay: no window stamp (an older base, or the 2D non-temporal
        path) or a single model group, which is the common case and must not carry the memory.
        """
        if context_id is None or len(self.params.get('models') or {}) < 2:
            return None
        return (context_id,
                tuple(script_utils.channel_indices(
                    model_params.get('cellChannels'), 'cellChannels',
                    'coastal_models_for_python (coastal.jl)') or [0]),
                model_params.get('normalise'),
                tuple(scales), cumulative, tuple(sorted(dropped)))

    def _cached_features(self, feature_key, z, window, center, scales, cumulative, dropped):
        """`_plane_features`, memoised per z for as long as one window is being worked on."""
        if feature_key is None:
            return self._plane_features(window, center, scales, cumulative, dropped)

        with self._feature_lock:
            if self._feature_cache_key != feature_key[0]:
                # A new window: everything cached for the previous one is unreachable now.
                self._feature_cache = {}
                self._feature_cache_key = feature_key[0]
            hit = self._feature_cache.get((feature_key, z))
        if hit is not None:
            return hit

        # Computed OUTSIDE the lock — this is the expensive call, and holding the lock across it
        # would serialise the very z-loop `FLOW_WORKERS` exists to widen. Two threads racing on the
        # same key would each compute it and one would win; they cannot race here in practice
        # because `_map_z` visits each z once per group.
        value = self._plane_features(window, center, scales, cumulative, dropped)
        with self._feature_lock:
            if self._feature_cache_key == feature_key[0]:
                self._feature_cache[(feature_key, z)] = value
        return value

    def _plane_features(self, window, center, scales, cumulative, dropped):
        """One 2D plane's model input: `[W, Y, X]` window → `(frame, metrics)`.

        The expensive half (Farneback optical flow, ~94% of it) and the half that has to hold the
        GIL are split here so each can be mapped over z at its own width.
        """
        frame, metrics = self._flow_metrics(window, center, scales, cumulative)
        if dropped:
            metrics = {k: v for k, v in metrics.items() if k not in dropped}
        return frame, metrics

    def _predict_plane(self, window, center, scales, cumulative, dropped, inference):
        """One 2D plane: `[W, Y, X]` window → uint32 labels `[Y, X]`.

        The 2D path, and what `_plane_features` + `predict_frame` do per z on the 3D one.
        """
        frame, metrics = self._plane_features(window, center, scales, cumulative, dropped)
        _, instances, _ = inference.predict_frame(frame, metrics)
        return np.asarray(instances).astype(self.LABEL_DTYPE)
