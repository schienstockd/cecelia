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
from cecelia.utils import flow_probe
from cecelia.utils.gpu_utils import torch_device
import cecelia.utils.cpu_utils as cpu_utils
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
# Flow is `cv2`/numpy and releases the GIL, so it scales with whatever the machine offers.
# `predict_frame` is region growing in a Python loop over scipy calls, so it peaks at 4 threads and
# gets WORSE beyond that — a single pool sized for flow would run the second stage 1.6x slower than
# 4 threads does. Hence two stages and a barrier, not one map over `_predict_plane`.
#
# The BUDGET comes from the machine and the scheduler (`CECELIA_TASK_WORKERS`, `[tasks].workerThreads`
# — see docs/SCHEDULER.md → *Thread budgets*); what stays here is the algorithmic CEILING on the
# second stage, because no machine makes 8 the right width for a stage that degrades past 4. So a
# bigger box widens the flow stage and leaves region growing where the measurement put it.
PREDICT_WORKER_CAP = 4

# `scales_linearly`: the measurement above is what earns it — this stage is the one that keeps
# getting faster with width, so where the budget was DERIVED and the user has opted in it takes the
# usable CPU count instead of the budget's lone-run-pessimistic share. Off by default; see
# `Cecelia.task_workers_widen`. `PREDICT_WORKERS` deliberately does NOT pass it: its curve turns
# down past 4, which is the opposite claim and already expressed by `cap=`.
FLOW_WORKERS = cpu_utils.task_workers(scales_linearly=True)
PREDICT_WORKERS = cpu_utils.task_workers(cap=PREDICT_WORKER_CAP)

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


def manifest_frame_interval(manifest):
    """Seconds per frame the model was TRAINED at, or None when that cannot be known.

    Read from `temporalReferenceInterval` when the model declared its scales in seconds, and
    otherwise from `physicalScales` (written by `opticalFlow.train`; see MODEL_VAULT_PLAN P0), which
    records one entry per source movie, unconverted, with its unit. Three cases return None, and the
    distinction matters because the caller uses this to decide whether to touch the scales at all:

    * no `physicalScales` — a model trained before P0, or a checkpoint dropped in by hand;
    * a unit that is not seconds — there is no unit converter in this codebase and inventing one to
      run over metadata is the silent numeric error P0 avoided by recording units instead;
    * source movies that DISAGREE — a model fitted across intervals has no single scale to convert
      from, so "cannot resolve" is the honest answer rather than a mean nobody chose.
    """
    # A model whose scales were DECLARED in seconds carries its own reference interval, and it is
    # authoritative: the source movies were resolved ONTO it, so they legitimately disagree with each
    # other (that is the whole point of declaring durations) and the per-movie scan below would
    # correctly refuse to pick one. See `opticalFlow.train`'s `temporalScaleMode`.
    ref = manifest.get('temporalReferenceInterval')
    if ref is not None:
        ref = float(ref)
        return ref if ref > 0 else None

    scales = manifest.get('physicalScales')
    if not isinstance(scales, dict):
        return None
    seen = set()
    for entry in scales.values():
        if not isinstance(entry, dict) or entry.get('t') is None:
            return None
        if str(entry.get('tUnit', 's')) != 's':
            return None
        seen.add(round(float(entry['t']), 6))
    if len(seen) != 1:
        return None
    dt = seen.pop()
    return dt if dt > 0 else None


def resolve_scales_for_interval(scales, cumulative, dt_model, dt_target):
    """Re-express a model's FRAME offsets as the frame offsets of the same DURATIONS on this movie.

    A temporal scale of 4 on a 15 s/frame movie is a 60 s displacement. Run the same model on a
    5 s/frame movie and scale 4 is 20 s — a different physical motion, fed to a network fitted on
    the first. Nothing detects that today: the flow is computed, normalised per plane, and comes out
    wrong-but-in-range, which is the failure mode hardest to notice in someone else's hands
    (docs/todo/MODEL_VAULT_PLAN.md -> *Would you train in physical units instead?*).

    Returns `(scales, cumulative, note, problem)`. `note` is a human-readable summary of what moved,
    or '' when nothing did. `problem` is '' when the conversion is usable and a reason when it is
    NOT — the caller raises on it rather than proceeding.

    The returned list is ALWAYS the same length as `scales`, in the same ascending order, so the
    i-th resolved offset is the i-th trained one. That is a contract, not an implementation detail:
    the per-scale planes are named `mag_{offset}` and both coastal's trainer and its inference stack
    the metric dict by `sorted(keys)`, a STRING sort — so a resolved set that is shorter, or that
    sorts differently, silently permutes the model's input channels. Two ways that happens, both
    now refused rather than reported:

    * **collapse** — on a coarser movie two declared durations land on the same offset. The old code
      deduped and returned a shorter list, which shifted every channel after the mag block and
      zero-filled the tail.
    * **clamp** — a duration shorter than one target frame. Nothing can invent frames that were not
      acquired, so this movie is simply too coarse for this model.

    Lexicographic reordering (`[1,2,4,8]` -> `[2,4,8,16]`, where `mag_16` sorts before `mag_2`) is
    NOT refused, because it is fully repairable: the caller renames the resolved planes back onto the
    trained names before the model sees them. See `mag_rename`.
    """
    trained = sorted(set(int(s) for s in scales))
    if not dt_model or not dt_target or dt_model <= 0 or dt_target <= 0:
        return trained, cumulative, '', ''
    if abs(dt_model - dt_target) < 1e-9:
        return trained, cumulative, '', ''

    ratio = dt_model / dt_target
    out, clamped = [], []
    for sc in trained:
        want = int(round(sc * ratio))
        if want < 1:
            want = 1
            clamped.append(sc)
        out.append(want)
    cum = max(1, int(round(cumulative * ratio)))

    rate = f'({dt_model:g} s/frame trained, {dt_target:g} s/frame here)'
    if clamped:
        secs = ', '.join(f'{sc * dt_model:g} s' for sc in clamped)
        return trained, cumulative, '', (
            f'this movie is too coarse for the model {rate}: {secs} is shorter than one frame here, '
            f'and frames that were not acquired cannot be interpolated')
    if len(set(out)) < len(trained):
        return trained, cumulative, '', (
            f'this movie is too coarse for the model {rate}: temporal scales {trained} land on '
            f'{sorted(set(out))} here, so two of the trained durations become the same frame offset')

    note = (f'temporal scales {trained} -> {out}  cumulative window {cumulative} -> {cum}  {rate}')
    return out, cum, note, ''


def mag_rename(trained_scales, resolved_scales):
    """`{resolved plane name: trained plane name}` for the per-scale magnitude planes.

    The repair for the channel order. `flow_metrics_for_frame` names its per-scale planes after the
    offset it computed them at, and both sides of coastal stack the metric dict by `sorted(keys)` —
    so feeding a model trained on `[1,2,4,8]` a movie resolved to `[2,4,8,16]` puts `mag_16` in the
    channel the model reads as `mag_1`, because "mag_16" < "mag_2" as strings. Renaming the planes
    onto the names training used makes the sort reproduce the training order exactly.

    Pairing is by POSITION, which is what makes it right: both lists are ascending and the same
    length (`resolve_scales_for_interval` refuses anything else), so the i-th resolved offset IS the
    i-th trained duration. Empty when nothing needs renaming.
    """
    trained = sorted(set(int(s) for s in trained_scales))
    resolved = [int(s) for s in resolved_scales]
    if len(trained) != len(resolved):
        raise ValueError(f'cannot pair {len(resolved)} resolved temporal scales with '
                         f'{len(trained)} trained ones')
    return {f'mag_{r}': f'mag_{t}' for r, t in zip(resolved, trained) if r != t}


def apply_mag_rename(metrics, rename):
    """`metrics` with the per-scale planes renamed onto the trained names. See `mag_rename`.

    A no-op (the same dict, not a copy) when there is nothing to rename, because that is the common
    case and this sits inside the per-plane, per-timepoint path.
    """
    if not rename:
        return metrics
    return {rename.get(k, k): v for k, v in metrics.items()}




def scales_from_seconds(seconds, cumulative_seconds, dt):
    """`(offsets, cumulative, problem)` — what these DURATIONS are in frames at `dt` s/frame.

    The direct resolution a model that declared its scales in seconds gets, instead of the
    trained-offsets × ratio arithmetic in `resolve_scales_for_interval`. Same refusals, one rounding
    instead of two: going through the offsets rounds `d -> n` at training and `n × ratio` again here,
    which can land a frame off the duration that was actually declared, and being exactly the span
    the user asked for is the entire reason for declaring one.

    The cumulative window clamps rather than refusing. It is a single `cumulative_mag` plane at any
    length, so unlike the per-scale planes it cannot change the channel layout — the floor of 2 is
    coastal's own (a window of 1 sums one flow and is not cumulative).
    """
    dt = float(dt)
    if dt <= 0:
        return [], 0, 'frame interval must be positive'
    wanted = sorted({float(x) for x in seconds})
    out = []
    for d in wanted:
        n = int(round(d / dt))
        if n < 1:
            return [], 0, (f'{d:g} s is shorter than one frame at {dt:g} s/frame, and frames that '
                           f'were not acquired cannot be interpolated')
        out.append(n)
    if len(set(out)) < len(out):
        return [], 0, (f'at {dt:g} s/frame the declared spans {[f"{d:g}s" for d in wanted]} land on '
                       f'{out}, so two of them become the same frame offset')
    cum = max(2, int(round(float(cumulative_seconds) / dt))) if cumulative_seconds else 0
    return out, cum, ''


#: Grid the frame-interval ceiling is searched on, in seconds. Fine enough that the answer is the
#: true bound for any span a microscope reports, coarse enough that the scan is a few thousand steps.
FRAME_INTERVAL_STEP = 0.01


def max_frame_interval(seconds):
    """The coarsest acquisition, in s/frame, at which these spans and everything FINER resolve.

    Past it two spans round to the same frame offset (or the shortest rounds below one frame), the
    model's mag planes stop being distinct features, and `scales_from_seconds` refuses. A model
    declares this so the refusal can be explained in advance instead of at the point of failure.

    Found by scanning `FRAME_INTERVAL_STEP` upward to the first interval that fails, rather than by a
    closed form, because the predicate is NOT monotone in dt and a closed form is therefore either
    wrong or far too tight. Both:

    * **Not monotone.** Spans 10 and 15 collide at 6 s/frame (`[2, 2]`) and separate again at 7
      (`[1, 2]`). So "the largest dt that works" is not a threshold — there are holes above it, and
      quoting one would promise something the resolver does not honour at every finer rate.
    * **Too tight.** The obvious closed form — the shortest span, and the smallest gap between two of
      them — gives 15 for spans 15/30/60/120, which resolve perfectly well at 20 (`[1, 2, 3, 6]`).
      That is not conservative, it is WRONG in the direction that matters: a model would tell someone
      their data is too coarse for it when it is not, and the refusal message would quote a rate
      nobody needs to hit.

    So the number returned is the strongest thing that is actually true and actually useful: every
    interval at or below it resolves. `test_the_ceiling_is_exactly_where_it_starts_failing` pins both
    halves — the ceiling works, one step past it does not.
    """
    vals = sorted({float(s) for s in seconds})
    if not vals:
        return None
    # `2 * vals[0]` is a hard stop: past it the shortest span rounds below one frame and nothing
    # coarser can ever resolve, so the scan cannot run away.
    dt = FRAME_INTERVAL_STEP
    last_ok = None
    while dt <= 2 * vals[0] + FRAME_INTERVAL_STEP:
        if scales_from_seconds(vals, 0, dt)[2]:
            break
        last_ok = dt
        dt = round(dt + FRAME_INTERVAL_STEP, 6)
    return last_ok

def temporal_seconds(manifest):
    """`(durations, cumulative_seconds)` when the model DECLARED its scales in seconds, else None.

    Written by `opticalFlow.train` under `temporalScaleMode: seconds`. The durations are
    authoritative and `temporalScales` is their resolution at `temporalReferenceInterval` — recorded
    too, so a reader (and every code path that predates this) still sees ordinary frame offsets.
    """
    if str(manifest.get('temporalScaleUnit', 'frames')) != 's':
        return None
    secs = manifest.get('temporalScaleSeconds')
    if not secs:
        return None
    return [float(x) for x in secs], float(manifest.get('cumulativeWindowSeconds') or 0)


def temporal_config(manifest):
    """`(scales, cumulative_window, dropped_metrics)` — the flow feature set a model expects.

    Frame offsets, always — including for a model that declared its scales in seconds, whose
    `temporalScales` records what those durations were at its reference interval. That is what keeps
    this one function: a seconds model is a frames model plus a statement about which frame rate its
    offsets belong to, so nothing downstream has to learn a second shape.

    `droppedMetrics` records planes deliberately excluded at training. Three of the 15 carry no
    cell/background structure on intravital data (divergence, vorticity, flow_structure_alignment;
    cell/bg ratios 0.99/1.00/1.65), so a model may be trained without them — and inference must then
    drop exactly the same ones or every later channel shifts.
    """
    scales = [int(s) for s in (manifest.get('temporalScales') or DEFAULT_TEMPORAL_SCALES)]
    cumulative = int(manifest.get('cumulativeWindow') or DEFAULT_CUMULATIVE_WINDOW)
    dropped = tuple(str(k) for k in (manifest.get('droppedMetrics') or ()))
    return sorted(set(scales)), cumulative, dropped


class _ConsecutiveOnly(dict):
    """A flow cache that keeps only consecutive-frame pairs.

    `flow_metrics_for_frame` offers every pair it computes; this decides what is worth keeping. The
    lag pairs (`(i, i+scale)` for scale > 1) move with the centre frame and are never requested a
    second time, so storing them is pure growth — three dead entries per timepoint per z-plane.
    Reads are unaffected: a rejected key simply misses.
    """

    def __setitem__(self, key, value):
        if key[1] == key[0] + 1:
            super().__setitem__(key, value)


class _PerPlaneFlowCaches:
    """`caches[z]` → the flow cache for that z-plane, created on demand under one lock.

    `_map_z` fills these from several threads, and `dict.setdefault` on the OUTER dict is what has
    to be atomic — the inner `_ConsecutiveOnly` is then touched by one z's worker only, so it needs
    no lock of its own.
    """

    def __init__(self, caches, lock):
        self._caches = caches
        self._lock = lock

    def __getitem__(self, z):
        with self._lock:
            return self._caches.setdefault(z, _ConsecutiveOnly())


class CoastalUtils(SegmentationUtils):

    def __init__(self, params, dim_utils):
        super().__init__(params, dim_utils)

        self.use_gpu, self.gpu_device = torch_device()
        self._quieten_cv2_threads()

        # Probed lazily and once — see `_check_flow_engine`. None means "not measured yet", which is
        # distinct from `{}` ("measured, and this engine cannot be probed").
        self._flow_fp = None

        # Model input shared between the model groups that read one window — see
        # *Sharing the flow between passes*. Guarded because `_map_z` fills it from several threads.
        self._feature_cache = {}
        self._feature_cache_key = None
        self._feature_lock = threading.Lock()

        # The projected window shared between the model groups that read it — see
        # `_cached_projection`. One entry per (window, channels, clip range).
        self._projection_cache = {}
        self._projection_cache_id = None
        self._projection_lock = threading.Lock()

        # Optical flow shared between TIMEPOINTS — see *Sharing the flow between timepoints*.
        self._flow_caches = {}
        self._flow_cache_tile = None
        self._flow_lock = threading.Lock()
        self._model_cache = {}
        self._inference_cache = {}
        self._manifest_cache = {}

        models = params.get('models') or {}
        if not models:
            raise ValueError('coastal segmentation needs at least one model group')

        if not dim_utils.is_timeseries():
            raise ValueError(
                'coastal segments by motion and needs a time series; this image has no T axis')

        # Resolved ONCE here rather than per plane in `predict_slice`: it reads a manifest and, in
        # `seconds` mode, does arithmetic that must give the same answer for every tile of the run.
        # Keyed by MODEL PATH, because that is all the resolution depends on (the manifest, plus the
        # run-constant mode and this image's interval) — so two groups sharing a model share the
        # answer and the warning is logged once, not once per pass.
        self._temporal = {}
        for mp in models.values():
            key = str(mp.get('model', ''))
            if key not in self._temporal:
                self._temporal[key] = self._resolve_temporal(mp)

        # The radius is a property of the RUN, not of one model group: the base builds one window
        # per tile and hands it to every group. Stacking a second group with different scales
        # therefore widens the window for both, which is harmless (each group indexes the flows it
        # needs) but must be the max, never the first group's.
        self.TEMPORAL_RADIUS = max(max(cfg[0]) for cfg in self._temporal.values())

        n_t = int(dim_utils.dim_val('T'))
        if n_t < self.TEMPORAL_RADIUS + 1:
            # Not a warning. Coastal would drop the largest scale's plane and shift every later
            # channel, producing a plausible-looking wrong mask.
            hint = ('Train a model with smaller temporal scales, or segment a longer movie.'
                    if str(self.params.get('temporalScaleMode', 'frames')) == 'frames' else
                    'This movie is faster than the model was trained on, so matching DURATIONS '
                    'needs proportionally more frames. Segment a longer movie, or set Temporal '
                    'scale back to "As trained".')
            raise ValueError(
                f'coastal needs at least {self.TEMPORAL_RADIUS + 1} timepoints for temporal scale '
                f'{self.TEMPORAL_RADIUS}; this image has {n_t}. {hint}')

    # Whether a model's temporal scales are taken as FRAMES (as trained) or as the DURATIONS they
    # represented at training and re-resolved for this movie. Default `frames` = exactly the previous
    # behaviour, because `seconds` changes what the network is fed and therefore what a re-run of an
    # existing pipeline produces. Exposed rather than chosen here: on a movie acquired at the model's
    # own interval the two modes are identical, and on any other movie the right answer depends on
    # whether the user is reproducing an old result or applying a model to new data.
    TEMPORAL_SCALE_MODES = ('frames', 'seconds')

    def _temporal_for(self, model_params):
        """The resolved `(scales, cumulative, dropped, rename)` for a group, by model path.

        Resolves on a miss rather than raising: `predict_slice` is also driven DIRECTLY — by the
        preview, by a test, by a REPL session — with a group the constructor never saw. Memoised, so
        a real run still resolves once per model and logs its warning once.
        """
        key = str(model_params.get('model', ''))
        if key not in self._temporal:
            self._temporal[key] = self._resolve_temporal(model_params)
        return self._temporal[key]

    def _resolve_temporal(self, model_params):
        """`(scales, cumulative, dropped, rename)` for one group, after the scale-mode decision.

        `rename` is the repair that makes `seconds` mode correct at all: resolving the scales renames
        the per-scale planes (`mag_{offset}`), and coastal stacks the metric dict by `sorted(keys)`
        — a string sort — at training and at inference alike. So `[1,2,4,8]` resolved to `[2,4,8,16]`
        feeds `mag_16` into the channel the model reads as `mag_1`, with the right channel COUNT and
        therefore no error. `mag_rename` maps the resolved planes back onto the trained names; see
        `apply_mag_rename`, which is where they are applied.

        The mismatch is logged in BOTH modes — that is the point. In `seconds` mode it says what was
        changed; in `frames` mode it says what was *not*, because until now nothing told you that the
        model in the picker was fitted at a different frame rate than the movie you pointed it at.
        """
        manifest = self._manifest(model_params)
        scales, cumulative, dropped = temporal_config(manifest)

        dt_model  = manifest_frame_interval(manifest)
        dt_target = self.dim_utils.im_time_increment(default=None)
        dt_target = None if dt_target is None else float(dt_target)
        if str(self.dim_utils.im_time_increment_unit()) != 's':
            dt_target = None

        mode = str(self.params.get('temporalScaleMode', 'frames'))
        if mode not in self.TEMPORAL_SCALE_MODES:
            raise ValueError(f'temporalScaleMode must be one of {self.TEMPORAL_SCALE_MODES}, '
                             f'got {mode!r}')

        # A model that DECLARED its scales in seconds resolves from the durations themselves rather
        # than from the offsets they became at its reference rate — one rounding, not two. Only in
        # `seconds` mode: `frames` still means "feed it the offsets it was trained on", which is what
        # reproduces an existing pipeline exactly.
        declared = temporal_seconds(manifest)
        if mode == 'seconds' and declared and dt_target:
            secs, cum_secs = declared
            new_scales, new_cum, problem = scales_from_seconds(
                secs, cum_secs or cumulative * (dt_model or 0), dt_target)
            if problem:
                raise ValueError(
                    f'this model was trained on spans of {", ".join(f"{d:g} s" for d in secs)} and '
                    f'{problem}. Segment a movie acquired at {max_frame_interval(secs):g} s/frame or '
                    f'finer, or set Temporal scale back to "As trained".')
            new_cum = new_cum or cumulative
            if new_scales != scales:
                self.logger.log(
                    f'>> temporal scales matched by duration: '
                    f'{[f"{d:g}s" for d in secs]} -> {new_scales} frames at {dt_target:g} s/frame '
                    f'(trained offsets {scales} at {dt_model:g} s/frame)')
            return new_scales, new_cum, dropped, mag_rename(scales, new_scales)

        new_scales, new_cum, note, problem = resolve_scales_for_interval(
            scales, cumulative, dt_model, dt_target)

        if problem and mode == 'seconds':
            # Refused, not clamped. Both ways this fails — a collapsed scale set and a duration
            # below one frame — change the CHANNEL LAYOUT the model reads, which is the silent
            # wrong-mask failure the manifest exists to prevent.
            raise ValueError(
                f'cannot match durations: {problem}. Set Temporal scale back to '
                f'"As trained" to feed the model its trained frame offsets instead.')

        if not note:
            # Nothing changed. Silent when the two intervals agree — but NOT when one of them is
            # unknown, because "these match" and "nobody can tell" are different answers and only
            # one of them is reassuring.
            #
            # Suppressed for a model with no manifest at all: `coastal_models_for_python` already
            # says that one, with the better advice (the metric set is a guess too), and repeating
            # it here would put two warnings about one cause in the log.
            if manifest and dt_model is None:
                self.logger.log('[WARN] Model records no frame interval — its temporal scales '
                                'cannot be checked against this movie. Re-train it to record one.')
            elif manifest and dt_target is None:
                self.logger.log('[WARN] This image records no frame interval in seconds — the '
                                "model's temporal scales cannot be checked against it.")
            return new_scales, new_cum, dropped, {}

        if mode == 'seconds':
            self.logger.log(f'>> temporal scales matched by duration: {note}')
            return new_scales, new_cum, dropped, mag_rename(scales, new_scales)

        self.logger.log(
            f'[WARN] Frame rate differs from the model\'s: {note}. The model sees different '
            f'displacements than it was fitted on. Set Temporal scale to "Match durations" to '
            f'convert.')
        return scales, cumulative, dropped, {}

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

    def _flow_fingerprint(self):
        """This process's flow-engine fingerprint, computed once. See `flow_probe`."""
        if self._flow_fp is None:
            self._flow_fp = flow_probe.fingerprint()
        return self._flow_fp

    def _check_flow_engine(self, manifest):
        """Warn when the flow features this engine computes are not the ones the model was fitted on.

        The manifest already pins the feature CONFIG and inference checks all of it. What nothing
        checked is the RECIPE: swap the optical-flow estimator and `temporalScales`, `metricKeys`,
        `droppedMetrics` and `coastalBuild`-minus-the-commit all still agree while the network is fed
        a different distribution. `flow_probe` measures the recipe numerically; this reports it.

        A WARN, not a refusal. The probe compares numbers, and numbers can move for a reason that is
        not a recipe change — a different cv2 build picking a different SIMD path, on a machine that
        is not the one that trained the model. `RTOL` is set orders of magnitude above that drift, so
        a fire here is almost certainly real; but "almost certainly" is not the standard for refusing
        to run somebody's segmentation, and the mismatch is stated precisely enough to act on.
        """
        if not manifest:
            # No manifest at all is already reported, with better advice, by
            # `coastal_models_for_python`. Two warnings about one cause is one too many.
            return
        recorded = manifest.get('flowFingerprint')
        if not recorded:
            # "These agree" and "nobody can tell" are different answers and only one is reassuring —
            # the same reason `_resolve_temporal` speaks up when a frame interval is unknown.
            self.logger.log('[WARN] Model records no flow-engine fingerprint — whether this build '
                            'computes the flow it was trained on cannot be checked. Re-train it to '
                            'record one.')
            return
        note = flow_probe.compare(recorded, self._flow_fingerprint())
        if note:
            self.logger.log(f'[WARN] {note}. The model was fitted on different features than it is '
                            f'being given; re-train it on this build.')

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
            # Here rather than in `_resolve_temporal`, because this is a property of the ENGINE and
            # not of the scale resolution — and here rather than per group, because this cache is
            # keyed by model path and a run's groups normally share one model.
            self._check_flow_engine(self._manifest_cache[path])
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

    def _model_channels(self, model_params):
        """The image channel indices one model group projects."""
        return script_utils.channel_indices(
            model_params.get('cellChannels'), 'cellChannels',
            'coastal_models_for_python (coastal.jl)') or [0]

    def _context_channels(self):
        """Only the channels some group actually projects — the rest are never read.

        Coastal projects `cellChannels` and nothing else, so on a 4-channel movie segmented on one
        channel the base was reading four times the pixels it could use: the temporal window is 17
        frames deep, which made it the second most expensive thing in the task (3.14 s of a 9.35 s
        timepoint on zolIMa/fXgbTl, against 4.88 s for all the flow and inference).

        The UNION across groups, because one window serves all of them (see `_context_channels` in
        the base). `_project_window` then maps an image channel onto its position in that union —
        the mapping is the whole safety of this, since dropping channels renumbers the axis and an
        unmapped index would quietly project the wrong channel.
        """
        models = (self.params.get('models') or {}).values()
        return tuple(sorted({int(c) for mp in models for c in self._model_channels(mp)}))

    def _project_window(self, context, model_params, norm_params, context_channels=None):
        """`[W, C, ...]` window → `[W, ...]` single-channel float32 in 0–255.

        The same two steps as coastal's `normalize_and_project` (per-channel percentile clip, then
        maximum across channels, then scale), with one deliberate difference: the clip range comes
        from the base's GLOBAL `norm_params` rather than from this window. Per-window percentiles
        would make a tile's normalisation depend on which tile it is.

        `context_channels` says which image channels the window carries, in axis order. When the
        base has narrowed the read (see `_context_channels`) the window's channel axis no longer
        counts 0, 1, 2, … in image terms, so every index has to go through it — `norm_params` stays
        keyed by IMAGE channel, which is what makes the two halves agree.
        """
        channels = self._model_channels(model_params)
        if context_channels:
            position = {int(c): i for i, c in enumerate(context_channels)}
            missing = [c for c in channels if c not in position]
            if missing:
                # Not a fallback: reading a different channel than the model was trained on is the
                # silent-wrong-answer failure this class exists to prevent.
                raise ValueError(
                    f'the temporal window carries channels {list(context_channels)} but this model '
                    f'group needs {missing} — `_context_channels` must return their union')
        else:
            position = None

        # IN PLACE, and the same operations in the same ORDER — `np.array_equal` with the expression
        # form, not merely close. `(arr - lo) / (hi - lo) -> clip -> * 255` reads better and allocates
        # a fresh temporary of the WHOLE window at every step: a 17-frame window of a 32 x 420 x 441
        # tile is 201.5 Mpx per channel, so each temporary is 403 MB and the projection is
        # bandwidth-bound rather than arithmetic-bound. Measured on zolIMa/fXgbTl (2 channels):
        # 5.85 s -> 1.21 s, 4.8x, bit-identical.
        #
        # `np.array` and not `np.asarray`: asarray returns the input itself when it is already
        # float32, and the arithmetic below would then rewrite the CALLER's pixels. The stores in
        # play are uint16 today, which is exactly why this would go unnoticed.
        projected = None
        for ch in channels:
            arr = np.array(context[:, ch if position is None else position[ch]], dtype=np.float32)
            if norm_params and ch in norm_params:
                lo, hi = norm_params[ch]
            else:
                lo = float(np.percentile(arr, PERCENTILE_LO))
                hi = float(np.percentile(arr, PERCENTILE_HI))
            arr -= lo
            arr /= (hi - lo + 1e-8)
            np.clip(arr, 0.0, 1.0, out=arr)
            projected = arr if projected is None else np.maximum(projected, arr, out=projected)

        projected *= PROJECTION_MAX
        return projected

    # The two coastal entry points sit behind one-line methods so the imports stay lazy (importing
    # this module must not pull torch) and so tests can exercise the window/tiling logic without a
    # trained checkpoint. Patching `sys.modules['coastal.*']` instead would leak a stub into every
    # later test in the process — it did, and broke an unrelated runner-import test.

    def _flow_metrics(self, window, center, scales, cumulative,
                      flow_cache=None, window_offset=0):
        from coastal.flow import flow_metrics_for_frame
        return flow_metrics_for_frame(window, center, temporal_scales=scales,
                                      cumulative_window=cumulative,
                                      value_range=(0.0, PROJECTION_MAX),
                                      flow_cache=flow_cache, window_offset=window_offset)

    def _match_3d(self, planes, stitch_threshold):
        from coastal.utils import match_masks_3d
        return match_masks_3d(planes, stitch_threshold=stitch_threshold)

    # ── Prediction ────────────────────────────────────────────────────────────

    def predict_slice(self, tile, model_params, norm_params=None, window=None):
        """Segment one XY tile at one timepoint from its temporal window.

        tile:   [C, Z, Y, X] or [C, Y, X] — present for the base's contract; the pixels used come
                from `window.frames[window.index]`, which is the same data.
        window: a `TemporalWindow`; `.frames` is [W, C, Z, Y, X] or [W, C, Y, X]
        Returns: uint32 labels [Z, Y, X] or [Y, X]
        """
        if window is None:
            raise ValueError(
                'CoastalUtils needs the temporal window; the base supplies it when '
                'TEMPORAL_RADIUS > 0 (see SegmentationUtils.predict_slice)')

        is_3d = (tile.ndim == 4)
        scales, cumulative, dropped, rename = self._temporal_for(model_params)
        inference = self._get_inference(model_params)
        feature_key = self._feature_key(window.id, model_params, scales, cumulative, dropped,
                                        rename)
        projected = self._cached_projection(window, model_params, norm_params)
        flow_caches = self._flow_caches_for(window.tile, window.start, window.index, cumulative)

        if not is_3d:
            frame, metrics = self._cached_features(
                feature_key, 0, projected, window.index, scales, cumulative, dropped, rename,
                flow_caches, window.start)
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
        n_z = projected.shape[1]
        features = self._map_z(
            FLOW_WORKERS, n_z,
            lambda z: self._cached_features(feature_key, z, projected[:, z], window.index,
                                            scales, cumulative, dropped, rename,
                                            flow_caches, window.start))
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

    # ── Sharing the flow between TIMEPOINTS ───────────────────────────────────
    #
    # Consecutive windows overlap by all but one frame — at radius 8, t and t+1 share 16 of 17 — and
    # the cumulative displacement is a sum of CONSECUTIVE-frame flows, so three of the four pairs it
    # needs at t are pairs it needed at t-1. Per plane that is 7 Farneback calls where 4 do: the
    # three lag pairs (scale 2, 4, 8) genuinely move with t, the consecutive one does not.
    #
    # Only consecutive pairs are kept. A lag pair `(t-1, t-1+s)` is never asked for twice, so caching
    # it would grow the cache by three entries per timepoint that nothing ever reads — which on a
    # 32-plane stack is how this turns into a gigabyte.
    #
    # The cache is per (tile, z): a flow is a property of the PIXELS, so two tiles at the same
    # timepoint, or two z-planes of one tile, share nothing. `context_tile` moving is a full reset
    # rather than a second keying level, because the base walks every tile of a timepoint before
    # moving on — entries for the previous tile are dead the moment it changes.

    # How far back a consecutive pair stays reachable. The cumulative window at `t` reaches
    # `t - cumulative // 2`, so a pair left of that can never be asked for again as `t` only
    # advances. One frame of slack, because the base may revisit a timepoint when a run is resumed.
    _FLOW_CACHE_SLACK = 1

    def _flow_caches_for(self, context_tile, context_start, context_index, cumulative):
        """Per-z flow caches for this tile, pruned to what a later timepoint can still read.

        None when there is nothing to key on — an older base, or a caller driving `predict_slice`
        directly — in which case `flow_metrics_for_frame` still memoises within the call.
        """
        if context_tile is None or context_start is None or context_index is None:
            return None

        with self._flow_lock:
            if self._flow_cache_tile != context_tile:
                self._flow_caches = {}
                self._flow_cache_tile = context_tile

            # `context_index` is t's offset in the window, so this is t in movie terms.
            oldest = (context_start + context_index) - (cumulative // 2) - self._FLOW_CACHE_SLACK
            for cache in self._flow_caches.values():
                for key in [k for k in cache if k[0] < oldest]:
                    del cache[key]
            return _PerPlaneFlowCaches(self._flow_caches, self._flow_lock)

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
    def _projection_key(self, window, model_params, norm_params):
        """What makes two groups' PROJECTIONS the same array — or None to not cache.

        Same shape of decision as `_feature_key`, one step earlier in the pipeline, and it has to be
        separate: the feature key carries the temporal scales and the dropped metric set, which the
        projection does not depend on. Two groups running the same channels off one model at different
        scales share this and not that.

        The clip RANGE is in the key, not the `normalise` percentile that produced it. They are not
        the same thing — `norm_params` comes from the base's global statistic and a group whose
        percentile differs gets different bounds — and it is the bounds that reach the pixels.
        """
        if window.id is None or len(self.params.get('models') or {}) < 2:
            return None
        channels = tuple(self._model_channels(model_params))
        bounds = tuple((c, None if not norm_params else norm_params.get(c)) for c in channels)
        return (window.id, channels, bounds)

    def _cached_projection(self, window, model_params, norm_params):
        """`_project_window`, shared between the model groups that read one window.

        The projection was the one part of a timepoint that ran once PER GROUP on identical input:
        `_feature_key` covers the flow metrics, but the projection sat outside it. On zolIMa/fXgbTl's
        two-group config the two calls return `np.array_equal` arrays and cost 5.85 s each — the
        single largest line in the timepoint, ahead of all the flow and all 64 plane predictions.
        Sharing it holds ONE window instead of allocating a second, so peak memory goes down, not up.

        What this does NOT do is carry the projection between TIMEPOINTS, where 16 of 17 frames repeat
        — see *Sharing the flow between timepoints* for the same argument applied to the flow. That
        needs the per-frame planes to be the storage rather than a `[W, Z, Y, X]` array, because
        holding both would double the peak on a 1046 x 1104 movie. Left out deliberately, not missed.
        """
        key = self._projection_key(window, model_params, norm_params)
        if key is None:
            return self._project_window(window.frames, model_params, norm_params, window.channels)

        with self._projection_lock:
            if self._projection_cache_id != window.id:
                # A new window: the previous one's projection is unreachable, and it is the largest
                # single allocation this class makes.
                self._projection_cache = {}
                self._projection_cache_id = window.id
            hit = self._projection_cache.get(key)
        if hit is not None:
            return hit

        # Outside the lock, for the reason `_cached_features` gives: this is the expensive call. Two
        # groups cannot race here — the base walks them in sequence.
        value = self._project_window(window.frames, model_params, norm_params, window.channels)
        with self._projection_lock:
            if self._projection_cache_id == window.id:
                self._projection_cache[key] = value
        return value

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

    def _feature_key(self, context_id, model_params, scales, cumulative, dropped, rename=None):
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
                # `rename` too, and not because it changes the flow: two groups can resolve to the
                # SAME offsets from different trained ones (one model fitted at 5 s/frame, another at
                # 15), and then the planes they need are named differently. Sharing on the offsets
                # alone would hand the second group the first one's channel names.
                tuple(scales), cumulative, tuple(sorted(dropped)),
                tuple(sorted((rename or {}).items())))

    def _cached_features(self, feature_key, z, window, center, scales, cumulative, dropped,
                         rename=None, flow_caches=None, window_offset=0):
        """`_plane_features`, memoised per z for as long as one window is being worked on."""
        if feature_key is None:
            return self._plane_features(window, center, scales, cumulative, dropped, rename,
                                        flow_caches, z, window_offset)

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
        value = self._plane_features(window, center, scales, cumulative, dropped, rename,
                                     flow_caches, z, window_offset)
        with self._feature_lock:
            if self._feature_cache_key == feature_key[0]:
                self._feature_cache[(feature_key, z)] = value
        return value

    def _plane_features(self, window, center, scales, cumulative, dropped, rename=None,
                        flow_caches=None, z=0, window_offset=0):
        """One 2D plane's model input: `[W, Y, X]` window → `(frame, metrics)`.

        The expensive half (Farneback optical flow, ~94% of it) and the half that has to hold the
        GIL are split here so each can be mapped over z at its own width.
        """
        cache = None if flow_caches is None else flow_caches[z]
        frame, metrics = self._flow_metrics(window, center, scales, cumulative,
                                            flow_cache=cache, window_offset=window_offset)
        # Renamed BEFORE the drop, so `droppedMetrics` keeps meaning the names training recorded.
        # A dropped plane is never a `mag_*` one today (they follow the scales and are not offered as
        # ticks) but the two must not be free to disagree about which spelling they use.
        metrics = apply_mag_rename(metrics, rename)
        if dropped:
            metrics = {k: v for k, v in metrics.items() if k not in dropped}
        return frame, metrics

    def _predict_plane(self, window, center, scales, cumulative, dropped, inference, rename=None):
        """One 2D plane: `[W, Y, X]` window → uint32 labels `[Y, X]`.

        The 2D path, and what `_plane_features` + `predict_frame` do per z on the 3D one.
        """
        frame, metrics = self._plane_features(window, center, scales, cumulative, dropped, rename)
        _, instances, _ = inference.predict_frame(frame, metrics)
        return np.asarray(instances).astype(self.LABEL_DTYPE)
