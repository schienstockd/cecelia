"""
Optical-flow model training.

Trains a coastal flow-metric UNet on Z planes of EVERY image in an experimental set and saves it to
the model vault as a PAIR: `<name>.pt` plus a `<name>.json` manifest. The manifest is not
documentation — it is what `CoastalUtils` configures inference from, because the flow metric set is a
silent train/inference contract: `predict_frame` stacks metrics in sorted-key order and zero-fills
the remainder, so a set that does not match shifts every later channel with no error.

Metrics are computed PER SEQUENCE and the frames pooled afterwards. Motion only exists within one
recording of one plane — flow across a boundary between two movies is meaningless, and so is flow
between plane k and plane k+1 of the same timepoint — which is what `prepare_data_for_unet_batch`
encodes, so the pooling goes through it rather than through a concatenated array.

A sequence is therefore (movie × plane), not (movie): `zPlanes` planes per movie, each its own
`[T, H, W]`. Inference runs the model per plane over the whole stack, so training on one plane makes
the model's whole experience of the data one depth — which for an intravital stack is one slice
through the tissue, at one signal level. `zPlanes = 1` reproduces the old single-middle-plane
behaviour exactly.

Note what this multiplies: pooled frames = movies × planes × timepoints, and the metric stack is
~11-15 float32 planes per frame. Five Z planes is five times the memory of one, which is what the
per-movie frame cap exists to bound.

Parameter contract (JSON written by Julia):
  movies                   - [{uID, imPath}, …]; every image of the set that resolved
  taskDir, modelPath, qcOutPath
  valueName                - provenance for the manifest
  trainChannels            - 0-based indices, merged by maximum
  channelName              - display name(s) for the manifest and the picker label
  zPlanes                  - how many evenly-spaced Z planes per movie (1 = the middle)
  maxFrames                - cap on the contiguous frames each movie contributes; 0 = all
  trainRatio               - fraction of each sequence to train on; 1.0 = no held-out split
  temporalScales           - already parsed + validated by Julia
  cumulativeWindow, droppedMetrics, epochs, embeddingDim, seed, normalise
  foregroundWeight, intensityWeight, temporalWeight, foregroundBlurSigma,
  foregroundBoundaryWeight
"""

import json
import datetime
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.gpu_utils import torch_device
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils import coastal_utils
from cecelia.utils import norm_cache
from cecelia.utils import flow_probe
import cecelia.utils.cpu_utils as cpu_utils


# Advisory only, and deliberately not a limit: what is too much depends on the box, and refusing to
# run would be worse than a run the user chose knowing the size. The threshold is "more than a
# workstation comfortably holds alongside torch".
MEMORY_WARN_GB = 16.0

# How the pooled flow metrics are HELD — the single largest allocation in the task by an order of
# magnitude (frames × pixels × metrics), and the one that decides whether a whole-set run fits.
#
# float16 rather than the float32 coastal produces, because nothing computes in this dtype: coastal's
# dataset and its contrastive loss both do `torch.from_numpy(arr).float()`, so the model sees float32
# either way and this only halves what is carried between production and that cast. The cost is ~0.05%
# relative precision on a metric that is an input to a contrastive loss over sampled pixels — orders
# of magnitude below the frame-to-frame variation the loss is reading. Recorded in the manifest, so a
# model's provenance says what it was trained on rather than leaving it to be inferred.
METRIC_DTYPE = np.float16

# Most sequences one movie may contribute under `zSpacing`. The count form is bounded by the form
# control; the spacing form is not — `spacing = 1` over a 45-plane stack asks for 45 sequences, 45×
# the memory of a single-plane run, which nobody types on purpose. Same ceiling as the `zPlanes`
# control, so the two forms cannot disagree about what is too much.
MAX_Z_PLANES = 25

# How far a random training crop is kept off the frame border, as a fraction of each axis.
#
# Two reasons, both about the data rather than the geometry: the edge of an intravital frame is
# routinely outside the specimen (the flow panels centre their crop for exactly this), and Farneback
# has nothing beyond the boundary to match against, so the outermost pixels carry the least reliable
# flow in the movie. 10% still leaves ~300 px of jitter for a 512 window on a 1046-px axis, so the
# windows are genuinely different between movies.
CROP_BORDER_FRAC = 0.1


def _open(im_path):
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    return im_dat, dim_utils


def z_planes(n_z, n, spacing=0):
    """`n` evenly-spaced plane indices through a stack of `n_z` — the centres of `n` equal bins.

    `spacing` says how far apart, and the two COMBINE — `n` planes, `spacing` apart, centred on the
    stack. They are different questions and both are worth pinning: the count is how many sequences
    this movie contributes (the memory), while the interval is how much DEPTH they span, which
    without it is whatever the stack happens to be — `n = 3` is 15 planes apart on a 45-deep stack
    and ~12 on a 35-deep one, so the same request samples the deeper movie more coarsely in µm.

    Centred, not spread, when a spacing is given: naming an interval is asking for a block of tissue
    at that interval, and the middle of the stack is where it is. `spacing = 0` keeps the original
    reading — `n` planes over the WHOLE stack — which is the only sensible one with no interval.

    Clamped to what the stack holds at that interval (and to `MAX_Z_PLANES`), so 10 planes 2 apart
    from a 9-deep stack yields fewer rather than reading past the end. The caller logs the shortfall.

    Bin centres, not `linspace(0, n_z - 1, n)`, and the difference matters at both ends of `n`:

    - `n = 1` lands on `n_z // 2`, which is exactly the single-middle-plane rule this replaced. An
      existing config keeps training on the same data, so the parameter can be introduced without
      silently retraining every model on something else.
    - No `n` picks plane 0 or `n_z - 1` until `n` approaches `n_z`. The top and bottom of an
      intravital stack are usually outside the tissue, and `linspace` would spend two of five planes
      on them — the model would learn from noise, and nothing in the run would say so.

    Clamped to `n_z`: asking for more planes than exist yields each plane once, not duplicates
    (which would silently weight those frames twice in the pool).
    """
    n_z = int(n_z)
    spacing = int(spacing or 0)
    if spacing >= 1:
        # `n_z // spacing` is the fit: one plane per `spacing` planes of depth. It is what bounds the
        # count, so a stack too shallow for the request gives fewer planes rather than reading past
        # the end — and it leaves a margin at each end in every case but an exact fit, which keeps
        # the run off plane 0 and `n_z - 1` the way the count rule does (the top and bottom of an
        # intravital stack are usually outside the tissue).
        count = max(1, min(int(n), n_z // spacing, MAX_Z_PLANES))
        start = (n_z - (count - 1) * spacing) // 2
        return [start + i * spacing for i in range(count)]
    n = max(1, min(int(n), n_z))
    return sorted({int((i + 0.5) * n_z / n) for i in range(n)})


def crop_window(shape, size, rng, border_frac=CROP_BORDER_FRAC):
    """`(y0, x0, h, w)` — a random square window of `size`, or `None` for the whole plane.

    RANDOM position, not the centred crop the two flow panels render (`FLOW_INSPECT_MAX_PX`). The
    panels answer "do these metrics look like cells" about one picture, where the middle is the safe
    bet; training is fitted to whatever it is shown, so one fixed window per movie would make the
    model's whole experience of every recording the same patch of field. The window is drawn per
    (movie × plane) from the run's seed, so it is reproducible from the manifest and different across
    runs — the same argument as `frame_window`, in the other two axes.

    PADDED off the border by `border_frac` of each axis. The edge of an intravital frame is routinely
    outside the specimen, and it is also where Farneback has nothing beyond the boundary to match, so
    the flow planes there are the least trustworthy in the movie. The margin shrinks rather than
    fails when the window nearly fills the axis — a crop the size of the frame is a whole frame, not
    an error.

    Crops an axis only where the window is smaller than it, so a 768 window on a 1039×700 plane takes
    768 of the long axis and all 700 of the short one.
    """
    h, w = int(shape[0]), int(shape[1])
    size = int(size or 0)
    if size <= 0 or (size >= h and size >= w):
        return None
    out = []
    for length in (h, w):
        span = min(size, length)
        margin = min(int(length * border_frac), (length - span) // 2)
        free = length - span - 2 * margin
        out.append((margin + int(rng.integers(0, free + 1)), span))
    (y0, hh), (x0, ww) = out
    return y0, x0, hh, ww


def frame_window(n_t, max_frames, seed, movie_idx):
    """`(start, stop)` — the contiguous run of frames one movie contributes.

    CONTIGUOUS because the metrics are temporal: `mag_8` is the flow between frame *t* and *t+8*, so
    a random subset of frames is not a shorter movie, it is a movie with the motion taken out.

    The start is SEED-DERIVED rather than 0. Always taking the head of every movie samples one part
    of the experiment — before the interesting event as often as not, and at whatever bleaching level
    the start happens to have — and no amount of pooling fixes a window that is always in the same
    place. Deriving it from the seed and the movie index makes it reproducible (the seed is in the
    manifest) while giving different runs different views.

    `max_frames <= 0` means no cap.
    """
    n_t = int(n_t)
    if max_frames is None or int(max_frames) <= 0 or int(max_frames) >= n_t:
        return 0, n_t
    n_use = int(max_frames)
    # Seeded per movie, so adding or reordering images does not reshuffle the others' windows.
    rng = np.random.default_rng([int(seed), int(movie_idx)])
    start = int(rng.integers(0, n_t - n_use + 1))
    return start, start + n_use


def pool_frames(all_frames):
    """The per-movie sequences as ONE training set — an array when they agree, a flat list when not.

    Movies in one experiment are rarely the same size (a set of six from zolIMa spans 1033×1037 to
    1095×1106 — different crops of different fields), so `np.concatenate` across them raises. coastal
    already falls back to a flat list of frames inside `train_test_split_per_movie`; this is the same
    fallback for the no-split path, so the two branches produce the same KIND of object and everything
    downstream can treat the pool as a sequence of frames rather than an array.

    A ragged pool is trainable, not a degraded mode: coastal's dataset indexes frames one at a time
    and the runner leaves `batch_size` at its default of 1, so nothing ever stacks two differently
    shaped frames. It is why the runner must NOT start passing a batch size — that is the constraint
    this fallback buys, and the alternative (cropping every movie to the smallest) would throw away
    real data to satisfy a stacking step that never happens.
    """
    if len({f.shape[1:] for f in all_frames}) == 1:
        return np.concatenate(all_frames, axis=0)
    return [f for arr in all_frames for f in arr]


def reduce_metrics(per_frame, dropped):
    """One sequence's metric dicts as training will HOLD them — unwanted keys gone, rest `METRIC_DTYPE`.

    Called per sequence, at the point of production, because both reductions are about the peak and
    neither is about the arithmetic:

    - Dropping here rather than after the split is what actually frees the dropped planes. Filtering
      later builds new dicts that SHARE the surviving arrays, so the originals stay reachable through
      the un-filtered list for the rest of the run — the dropped metrics were being carried through
      metric computation, the split, and all of training.
    - The cast is storage only. Every consumer does `torch.from_numpy(arr).float()`.
    """
    return [{k: v.astype(METRIC_DTYPE, copy=False) for k, v in mm.items() if k not in dropped}
            for mm in per_frame]


def _coastal_build():
    """Which coastal produced this model — `{version, commit}`, or `None` if neither is knowable.

    `coastal.__version__` alone is not enough: it is `0.1.0` and is not bumped per change, while the
    pin in `pixi.toml` is a **git revision** and is described there as "a HARD floor, not a snapshot".
    So the useful identifier is the commit, which pip records in the distribution's `direct_url.json`
    for a VCS install. Absent for a PyPI or editable install, and `None` then rather than a version
    number that implies more than it knows.

    Worth recording because coastal's inference is under active change (`perf/coastal-speed`), and one
    of those changes moved a DEFAULT that decides object size. A model whose output cannot be tied to
    an engine build cannot be reproduced, which is the first thing a published model has to be.
    See docs/todo/MODEL_VAULT_PLAN.md.
    """
    out = {}
    try:
        import coastal
        v = getattr(coastal, '__version__', None)
        if v:
            out['version'] = str(v)
    except Exception:
        pass
    try:
        import importlib.metadata as md
        from pathlib import Path
        dist = md.distribution('coastal')
        # Located and read explicitly rather than through `Distribution.read_text`, which takes no
        # encoding — and `test_atomic_io` rightly refuses text IO without one.
        for f in (dist.files or ()):
            if f.name != 'direct_url.json':
                continue
            info = json.loads(Path(dist.locate_file(f)).read_text(encoding='utf-8'))
            commit = (info.get('vcs_info') or {}).get('commit_id')
            if commit:
                out['commit'] = str(commit)
            break
    except Exception:
        # No `direct_url.json` is the normal PyPI case, not a failure worth a warning: the manifest
        # simply says less.
        pass
    return out or None


def pooled_offsets(mode, scales, cumulative, declared, cum_seconds, movie_dt):
    """`(offsets, cumulative, reference_interval)` — the canonical channel names for the pool.

    In `frames` mode the offsets are already canonical and every movie was read at them, so this is
    the identity and there is no reference interval to state.

    In `seconds` mode the pooled sequences were read at DIFFERENT offsets — `mag_2` on a 15 s/frame
    movie and `mag_6` on a 5 s/frame one are both "30 s" — and coastal stacks the metric dict by
    `sorted(keys)`, so pooling them as-is either trips the `key_sets` guard in `run` or, downstream,
    feeds two different spans to one channel. So every sequence is renamed onto ONE set of offsets:
    the spans resolved at the FINEST interval among the movies actually used.

    Finest, for three reasons: that movie then needs no rename at all, no canonical offset can round
    below one of its frames, and it is a real acquisition rather than a number nobody recorded.

    The payoff is that the model stays an ordinary frame-offset model. `temporalScales` means what it
    always meant; `temporalReferenceInterval` says which frame rate those offsets belong to, and
    inference re-resolves from `temporalScaleSeconds`. Nothing downstream needs a second shape.

    A separate function, and not inlined, for the reason `RunnerLocalsAreNotShadowedTest` states:
    `scales` and `cumulative` feed the manifest, so rebinding them here would make what gets written
    depend on a branch fifty lines up. The pooled values get their own names, bound once.
    """
    if mode != 'seconds':
        return list(scales), cumulative, None
    ref = min(movie_dt.values())
    offsets, cum, problem = coastal_utils.scales_from_seconds(declared, cum_seconds, ref)
    if problem:
        # Unreachable through the per-movie guard (the finest movie resolved, or it was skipped) and
        # left in anyway: this is the one place the canonical names are chosen, and a bad set here
        # mislabels every channel of the model.
        raise ValueError(f'cannot name the pooled channels: {problem}')
    return offsets, cum or cumulative, ref


def _physical_scale(dim_utils, planes):
    """What one pixel and one frame of THIS movie are, physically — or `None` where the file is silent.

    Recorded because every number coastal is configured with is in pixels and frames, and none of them
    means anything without this: `temporalScales` are FRAMES (5 s/frame and 30 s/frame present entirely
    different displacements at scale 1), `cropSize` and `foregroundBlurSigma` are PIXELS. A model is
    therefore only applicable to a movie acquired at a comparable scale, and until this was written down
    nothing — not the manifest, not the vault UI, not a shared model — could answer that. Same class of
    silent train/inference mismatch as the metric set, which is why the manifest exists at all.
    See docs/todo/MODEL_VAULT_PLAN.md.

    Values are recorded AS READ, with their units, and deliberately NOT converted: there is no unit
    converter in this codebase and inventing one to run over metadata that is already µm/s in practice
    would be a silent numeric error waiting for the one file that isn't. `None` where OME carried
    nothing — an invented 1.0 (which is what `im_physical_size`'s default would give) reads as a real
    measurement and is worse than an admitted gap.

    `z` is the gap between the planes actually TRAINED ON, not the stack's own step: training takes
    every `zSpacing`-th plane, so the stack's 1 µm step at spacing 2 means the model saw 2 µm.
    """
    out = {}
    dx = dim_utils.im_physical_size('x', default=None)
    dy = dim_utils.im_physical_size('y', default=None)
    if dx is not None:
        out['x'] = float(dx)
        out['xUnit'] = dim_utils.im_physical_unit('x')
    # Y only when it differs from X — the anisotropic case is real and rare, and repeating an equal
    # value on every entry would bury it.
    if dy is not None and (dx is None or float(dy) != float(dx)):
        out['y'] = float(dy)
        out['yUnit'] = dim_utils.im_physical_unit('y')
    dz = dim_utils.im_physical_size('z', default=None)
    if dz is not None and planes and len(planes) > 1 and planes[0] is not None:
        gaps = {b - a for a, b in zip(planes, planes[1:])}
        # One gap, or the plane list was not evenly spaced (it is, by construction — but if that ever
        # changes, a single number would be a lie rather than an approximation).
        if len(gaps) == 1:
            out['z'] = float(dz) * gaps.pop()
            out['zUnit'] = dim_utils.im_physical_unit('z')
    dt = dim_utils.im_time_increment(default=None)
    if dt is not None:
        out['t'] = float(dt)
        out['tUnit'] = dim_utils.im_time_increment_unit()
    return out or None


def _training_sequence(im_dat, dim_utils, params, z, window=None, crop=None, stats=None):
    """`[T, H, W]` float32 in 0–255 for ONE Z plane — the same projection inference builds per tile.

    Percentiles are taken over the WHOLE plane sequence — every timepoint, including those outside
    `window`, and every pixel, including those outside `crop` — which is what makes this the global
    statistic `normaliseToWhole` reproduces at inference from the image pyramid. If the two ever
    diverge, the model sees a different photometric range than it was trained on.

    That ordering is the whole subtlety of the frame cap: normalise over the movie, THEN cut. Cutting
    first would scale a 50-frame window by its own percentiles while inference scales the 200-frame
    movie by the movie's, and the mismatch is silent — the same structure at a different brightness.

    **Which is why `window` and `crop` arrive here rather than being applied by the caller.** The
    scale and the clip are elementwise, so they commute with slicing exactly: taking the percentiles
    over everything and then cutting gives bit-identical numbers to cutting first and clipping the
    remainder, at 2% of the arithmetic. Measured on `zolIMa/VJy1Nx`, clipping a whole 181-frame plane
    costs ~3.9 s against ~0.07 s for the [60, 256, 256] block the run keeps.

    The percentiles run on the RAW integer dtype rather than on a float32 copy: 4.9 s against 6.4 s,
    and it drops a 1.1 s cast of an array about to be discarded. **This one changes the number, and
    for a reason worth knowing.** `np.percentile` interpolates between two order statistics at a
    virtual index of `(n-1) * q`, and it does that arithmetic in the INPUT's dtype. On a plane
    sequence of ~5.7 M samples that index is ~5.7e6, where float32's spacing is 0.5 — so the old path
    quantised the interpolation weight to halves, and `hi` landed up to half an intensity unit away
    from the true percentile. Reading the raw integers promotes to float64 and gets it right.

    Measured over the ten planes x two channels of `zolIMa/fXgbTl` at 99.99: nineteen pairs agree
    exactly (both neighbours are the same integer, so no weight can matter) and one does not —
    z11/ch1, `hi` 150.818 against float32's 150.5. That plane's output moves on 3.8% of its pixels,
    by at most 0.53 of 255. So this is not cosmetic: it is a small correction, in the direction of
    correct, and a model trained before this differs slightly from one trained after.

    `stats` is the `norm_cache` dict for this image, read and WRITTEN THROUGH: a hit skips the full
    read as well as the percentiles, because the range is the only thing the rest of the movie was
    needed for. Pass `None` to compute every time.

    Per plane, deliberately: each plane is normalised on its own statistics. Note that inference does
    NOT do this — `norm_params` is one range per image channel, applied to every plane — so the two
    diverge on the deep planes of a dim channel. That is a known open question, not something this
    function should paper over; see `docs/todo/SEGMENTATION_OPEN_PROBLEM.md`.
    """
    channels = list(params['trainChannels'])
    percentile_hi = float(params.get('normalise', 99.99))

    level = im_dat[0]
    ia = {ax: i for i, ax in enumerate(dim_utils.im_dim_order)}
    n_t = int(dim_utils.dim_val('T'))
    # Axes left after C (and Z, when a plane was picked) are T + Y + X in the image's own order.
    remaining = [ax for ax in dim_utils.im_dim_order
                 if ax != 'C' and not (z is not None and ax == 'Z')]
    t_pos = remaining.index('T')

    t0, t1 = window if window is not None else (0, n_t)
    y0, x0, hh, ww = crop if crop is not None else (0, 0, None, None)
    ys = slice(y0, None if hh is None else y0 + hh)
    xs = slice(x0, None if ww is None else x0 + ww)

    projected = None
    for ch in channels:
        ck = norm_cache.key(ch, z, percentile_hi)
        lo_hi = stats.get(ck) if stats is not None else None

        idx = [slice(None)] * level.ndim
        idx[ia['C']] = ch
        if z is not None:
            idx[ia['Z']] = z

        if lo_hi is not None:
            # The range is already known, so the whole movie is no longer needed — read only the
            # block that survives the cut. 0.13 s against 1.1 s for the full plane sequence.
            idx[ia['T']] = slice(t0, t1)
            idx[ia['Y']] = ys
            idx[ia['X']] = xs
            arr = np.moveaxis(np.asarray(level[tuple(idx)]), t_pos, 0).astype(np.float32)
            assert arr.shape[0] == t1 - t0, f'expected {t1 - t0} frames, got {arr.shape[0]}'
            lo, hi = lo_hi
        else:
            raw = np.moveaxis(np.asarray(level[tuple(idx)]), t_pos, 0)
            assert raw.shape[0] == n_t, f'expected {n_t} frames, got {raw.shape[0]}'
            lo = float(np.percentile(raw, 100 - percentile_hi))
            hi = float(np.percentile(raw, percentile_hi))
            if stats is not None:
                stats[ck] = (lo, hi)
            # `astype` copies, so the full-movie array is released here rather than kept alive by a
            # view — the allocation `cropSize` exists to avoid.
            arr = raw[t0:t1, ys, xs].astype(np.float32)
            del raw

        arr = np.clip((arr - lo) / (hi - lo + 1e-8), 0.0, 1.0)
        projected = arr if projected is None else np.maximum(projected, arr)

    return (projected * coastal_utils.PROJECTION_MAX).astype(np.float32)


def run(params):
    log = script_utils.get_logfile_utils(params)

    # Imported here, not at module scope, so the module can be introspected without torch. Note
    # `prepare_data_for_unet` is in coastal.FLOW while the rest are in coastal.TRAIN — the batch
    # wrapper around it lives in train and reads as a flow helper, which cost an end-to-end run to
    # find once (unit tests stub coastal, so nothing caught it).
    from coastal.flow import prepare_data_for_unet
    from coastal.train import (train_test_split_per_movie,
                               train_with_metrics, save_model)

    movies = list(params['movies'])
    scales = [int(s) for s in params['temporalScales']]
    cumulative = int(params.get('cumulativeWindow', 5))
    dropped = tuple(params.get('droppedMetrics') or ())
    epochs = int(params.get('epochs', 30))

    # ── Temporal scale mode ─────────────────────────────────────────────────────────────────────
    # `frames` (default) pools every movie at the SAME frame offsets, which is the same physical
    # displacement only if every movie was acquired at the same rate. `seconds` declares the spans
    # instead and resolves them per movie, so a set that mixes 5 s/frame and 15 s/frame contributes
    # one feature geometry rather than three-fold-different ones under identical channel names.
    #
    # See docs/todo/MODEL_VAULT_PLAN.md -> *Would you train in physical units instead?* for why this
    # is the half of "physical units" that buys anything: coastal normalises every metric plane, so
    # converting px/frame to um/s would be a no-op, while WHICH time spans the stack covers is not
    # normalised out by anything.
    mode = str(params.get('temporalScaleMode', 'frames'))
    if mode not in ('frames', 'seconds'):
        raise ValueError(f"temporalScaleMode must be 'frames' or 'seconds', got {mode!r}")
    declared = sorted({float(x) for x in (params.get('temporalScaleSeconds') or ())})
    cum_seconds = float(params.get('cumulativeWindowSeconds') or 0.0)
    if mode == 'seconds' and not declared:
        raise ValueError('temporalScaleMode is "seconds" but no temporal spans were given')

    use_gpu, gpu_device = torch_device()
    log.log(f'>> GPU: {gpu_device if use_gpu else "none (CPU)"}')
    log.log(f'>> reading {len(movies)} movie(s) — project, normalise, crop (no flow yet)')

    # ── progress ────────────────────────────────────────────────────────────────────────────────
    # One monotonic scale over the whole run: a tick per movie prepared, one for the flow metrics,
    # then one per epoch. Without this the task reported NOTHING for its entire duration — `run_py`
    # routes `[PROGRESS] n/total` to `on_progress` and this runner never printed one, so a training
    # job that takes tens of minutes was indistinguishable from a wedged one.
    #
    # The phases are wildly unequal in wall-clock (metrics is one tick and minutes long) so the bar
    # does not move smoothly. That is the honest shape: the alternative is inventing weights per
    # phase, which would be a guess dressed up as measurement.
    n_planes = int(params.get('zPlanes', 1))
    seed = int(params.get('seed', 42))
    total_steps = len(movies) + 1 + epochs
    log.progress(0, total_steps)
    # A cap per movie, not a total. Without one, pooling is weighted by how long each recording
    # happened to run: a 200-frame movie contributes ~7x what a 30-frame one does, so the model is
    # mostly fitted to whichever image the microscope was left on longest. Nothing in the run or the
    # manifest showed that — the frame count is a single pooled number.
    max_frames = int(params.get('maxFrames', 0))
    # Every metric plane is paid for at the frame's full area, so this is the one knob that divides
    # the whole cost rather than multiplying part of it: 512 of a 1046×1104 field is 22% of the
    # pixels, i.e. 22% of the metric memory AND of the Farneback time. What it costs is field of
    # view per sequence, which is why the position is random rather than fixed — see `crop_window`.
    crop_size = int(params.get('cropSize', 0))
    z_spacing = int(params.get('zSpacing', 0))
    sequences, used, planes_used, windows, crops = [], [], {}, {}, {}
    # Parallel to `sequences`, because in `seconds` mode the offsets are a property of the MOVIE and
    # every sequence of a movie shares them. A dict keyed by uID would not do: `_one` works by
    # sequence index and the movie is not recoverable from it.
    seq_scales = []
    # uID -> (s/frame, resolved offsets), for the reference interval and for the manifest. Only the
    # movies that were actually USED, so a skipped one cannot set the reference nobody trained at.
    movie_dt, movie_scales = {}, {}
    # NOT `scales` — that name is the temporal scale LIST, six lines up, and shadowing it turned
    # `max(scales)` into a crash and would have written this dict into the manifest as
    # `temporalScales`.
    phys_scales = {}
    for i, m in enumerate(movies):
        im_path = m['imPath']
        uid = m.get('uID', '')
        log.log(f'>> [{i + 1}/{len(movies)}] {uid}: {im_path}')
        im_dat, dim_utils = _open(im_path)

        if not dim_utils.is_timeseries():
            log.log(f'>> [WARN] {uid} has no T axis — skipped')
            continue
        # In `seconds` mode the offsets are this movie's own — the whole point — so they are
        # resolved before the length guard, which has to see the scales THIS movie will be read at.
        m_scales, m_cum = scales, cumulative
        dt_movie = None
        if mode == 'seconds':
            dt_movie = dim_utils.im_time_increment(default=None)
            unit = str(dim_utils.im_time_increment_unit())
            if dt_movie is None or float(dt_movie) <= 0 or unit != 's':
                # Skipped, not guessed. A movie whose frame interval is unknown (or recorded in some
                # other unit — there is no unit converter here, deliberately; see `_physical_scale`)
                # cannot be resolved onto a duration, and pooling it at somebody else's frame offsets
                # is exactly the mix this mode exists to stop.
                said = 'no frame interval' if dt_movie is None else f'its interval in {unit!r}'
                log.log(f'>> [WARN] {uid} records {said} — cannot resolve spans in seconds, skipped')
                continue
            dt_movie = float(dt_movie)
            m_scales, m_cum, problem = coastal_utils.scales_from_seconds(
                declared, cum_seconds, dt_movie)
            if problem:
                # His call, 2026-08-24: drop it and say so, rather than clamping onto the closest
                # frames it has. A clamped movie contributes different spans than the rest under the
                # same channel names — the quiet version of the corruption the metric-set contract
                # exists to prevent.
                log.log(f'>> [WARN] {uid} at {dt_movie:g} s/frame is too coarse for this model — '
                        f'{problem}; skipped')
                continue

        n_t = int(dim_utils.dim_val('T'))
        start, stop = frame_window(n_t, max_frames, seed, i)
        n_use = stop - start
        # Checked against the CAPPED length, not the movie's. A 200-frame movie capped to 5 produces
        # no `mag_8` plane, which is the same silent corruption as a genuinely short movie — the
        # guard has to see what the run will actually feed coastal.
        if n_use < max(m_scales) + 1:
            # The same guard CoastalUtils applies. Below this the largest scale produces no plane,
            # so this movie would contribute a DIFFERENT channel layout than the rest — which is a
            # silent corruption of the pooled training set, not just a short movie.
            of = f'{n_use} of {n_t}' if n_use < n_t else f'{n_t}'
            span = (f' ({max(declared):g} s at {dt_movie:g} s/frame)' if mode == 'seconds' else '')
            log.log(f'>> [WARN] {uid} has {of} timepoints, needs '
                    f'{max(m_scales) + 1} for scale {max(m_scales)}{span} — skipped')
            continue
        if n_use < n_t:
            windows[uid] = [start, stop]

        # Planes are resolved PER MOVIE because depth is: asking for 3 of a 31-plane stack and 3 of
        # a 9-plane one are different indices, and a single global list would read past the end of
        # the shallow one.
        axes = set(dim_utils.im_dim_order)
        depth = ''
        if 'Z' in axes:
            n_z = int(dim_utils.dim_val('Z'))
            planes = z_planes(n_z, n_planes, z_spacing)
            planes_used[uid] = planes
            if z_spacing >= 1:
                # The interval in µm as well as in planes — the number that means anything across
                # images acquired at different Z steps, and not something the form can show.
                dz = dim_utils.im_physical_size('z', default=0) or 0
                depth = f' every {z_spacing} of {n_z}'
                if dz:
                    depth += f' ({z_spacing * dz:.1f} {dim_utils.im_physical_unit("z")})'
            # ONE shortfall warning for both forms: whatever the rule, the thing worth saying is that
            # this movie is contributing fewer planes than was asked for.
            if len(planes) < n_planes:
                deep = f' at spacing {z_spacing}' if z_spacing >= 1 else ''
                log.log(f'>> [WARN] {uid} has {n_z} Z planes — training on {len(planes)}{deep}, '
                        f'not the {n_planes} requested')
        else:
            planes = [None]

        # Per movie, like `zPlanesUsed` and for the same reason: pooling movies from two microscopes
        # (or two objectives) is legitimate and invisible in a single pooled number.
        scale = _physical_scale(dim_utils, planes)
        if scale is not None:
            phys_scales[uid] = scale

        n_y, n_x = int(dim_utils.dim_val('Y')), int(dim_utils.dim_val('X'))
        # The percentiles are the one part of the preparation that has to see every pixel of the
        # movie, and they do not depend on the frame window or the crop — so they survive a rerun
        # that changes only the epoch count or a loss weight. Read once per image, written back
        # below. A stale file cannot be read: the fingerprint covers the store's pixels and the key
        # covers every setting that changes the number (see `norm_cache`).
        fp = norm_cache.fingerprint(im_path, im_dat[0].shape, im_dat[0].dtype)
        stats = norm_cache.read(im_path, fp)
        # The keys THIS run needs, intersected — not `len(stats)`, which counts entries left by runs
        # at other planes or another percentile and would report reuse that did not happen.
        need = {norm_cache.key(ch, z, float(params.get('normalise', 99.99)))
                for z in planes for ch in params['trainChannels']}
        reused = len(need & set(stats))

        for zi, z in enumerate(planes):
            # Decided BEFORE the read, so `_training_sequence` can narrow what it loads — it needs
            # the window and the crop to cut with, and both are known from the plane's shape and the
            # run's seed. The percentiles are still taken over everything; see its docstring.
            #
            # Seeded per (movie, plane) so each window is independent — two planes of one stack are
            # two views of the tissue, and giving them the same XY window would make them more alike
            # than they need to be. Reproducible from the manifest's seed either way.
            win = crop_window((n_y, n_x), crop_size,
                              np.random.default_rng([seed, i, zi]))
            seq = _training_sequence(im_dat, dim_utils, params, z, (start, stop), win, stats)
            if win is not None:
                crops.setdefault(uid, []).append([int(v) for v in win])
            sequences.append(seq)
            seq_scales.append((m_scales, m_cum))
            del seq

        # AFTER the planes, not per plane: one write per image rather than one per (plane × channel),
        # and a run killed mid-movie still leaves the previous file intact rather than a partial one.
        if reused >= len(need):
            log.log(f'>>   normalisation: all {len(need)} ranges reused from a previous run')
        else:
            norm_cache.write(im_path, fp, stats)
            log.log(f'>>   normalisation: {len(need) - reused} of {len(need)} ranges computed'
                    + (', cached for the next run' if fp else
                       ' (not cacheable — the store has no readable metadata)'))
        used.append(uid)
        if mode == 'seconds':
            movie_dt[uid], movie_scales[uid] = dt_movie, list(m_scales)
        where = f'Z {planes}' if planes != [None] else '2D'
        span = f'{n_use} frames' if n_use == n_t else f'frames {start}–{stop - 1} of {n_t}'
        # The positions themselves go to the manifest, not the log — one line per plane would bury
        # the run, and "which window" is a question asked of a saved model, not of a scrolling log.
        at = (f'{sequences[-1].shape[1:]} cropped at random from ({n_y}, {n_x})'
              if uid in crops else f'{sequences[-1].shape[1:]}')
        log.log(f'>>   {where}{depth}: {len(planes)} × {span} of {at}')
        # `i + 1`, not `len(used)`: the scale is over the movies ATTEMPTED, so a skipped movie still
        # advances the bar rather than leaving it short by however many were unusable.
        log.progress(i + 1, total_steps)

    if not sequences:
        raise ValueError('no usable movies — every image was skipped (see the warnings above)')

    # Say how big this is BEFORE the expensive step. Every metric plane is held at once, so the
    # pooled frame count — movies × Z planes × timepoints — is the number that decides whether the
    # machine survives, and `zPlanes` multiplies it directly. Nothing on the form hints at that, and
    # the failure mode is the run being killed tens of minutes in.
    #
    # Frames and megapixels, not an estimated GB: the metric count is only known once coastal has
    # produced them, and the alternative — a copy of the metric list here — would be a second
    # spelling of `train.jl`'s `FIXED_FLOW_METRICS` free to disagree with it. The real total is
    # logged below, from the metrics that actually exist.
    # A RANGE, not `sequences[0]`: the movies are usually different sizes, and quoting the first
    # one's as if it were the pool's is exactly the number this line exists to let you judge.
    n_frames_pooled = sum(int(s.shape[0]) for s in sequences)
    mpx = sorted({float(np.prod(s.shape[1:])) / 1e6 for s in sequences})
    at = f'{mpx[0]:.2f} MP' if len(mpx) == 1 else f'{mpx[0]:.2f}–{mpx[-1]:.2f} MP'
    log.log(f'>> pooling {n_frames_pooled} frames from {len(sequences)} sequence(s) at {at}')

    # The channel names the pooled sequences share — see `pooled_offsets`. Bound once, under their own
    # names, so `scales` and `cumulative` still say what the FORM asked for when the manifest is
    # written and these say what the pool was actually keyed on.
    pool_scales, pool_cumulative, ref_interval = pooled_offsets(
        mode, scales, cumulative, declared, cum_seconds, movie_dt)
    if mode == 'seconds':
        spans = ', '.join(f'{d:g}s' for d in declared)
        log.log(f'>> temporal spans {spans} -> offsets {pool_scales} at the finest interval used '
                f'({ref_interval:g} s/frame), cumulative window {pool_cumulative}')
        for uid in sorted(movie_scales):
            if movie_scales[uid] != pool_scales:
                log.log(f'>>   {uid} at {movie_dt[uid]:g} s/frame read at {movie_scales[uid]} '
                        f'-> renamed onto {pool_scales}')

    log.log(f'>> computing flow metrics for {len(sequences)} sequence(s) '
            f'(scales {pool_scales}, cumulative {pool_cumulative})')
    # Sequences are INDEPENDENT — `prepare_data_for_unet_batch` is a per-movie loop with no
    # cross-movie state — so the only thing that ever forced one at a time was memory: the full
    # float32 metric stack of a sequence is live while it is computed, and six whole 1046x1104 movies
    # at once measured ~23 GB before training allocated anything.
    #
    # That reason expires at crop size. A 256x256 crop of 60 frames holds ~0.24 GB, not 1.55 GB, so
    # the same decision that was right for whole movies leaves 86% of this phase (measured: Farneback
    # is 86%, metric materialisation 14%) running one-at-a-time for nothing.
    #
    # So the width is DERIVED, not chosen: compute the FIRST sequence alone, price its transient peak
    # from the process high-water mark, then divide the memory we may use by that — capped by the task
    # thread budget, because this is also CPU work and the budget is what the throttle sets. A
    # measurement rather than a constant means it follows the crop size and the machine, and the
    # number is logged so a bad estimate is visible rather than silent.
    #
    # THREADS, not processes: the heavy parts release the GIL (coastal's flow is joblib, which forks
    # its own workers; the metric materialisation is numpy), and results stay in this process instead
    # of being pickled back. Two reductions still happen per sequence as before — drop the unwanted
    # metrics, hold the rest as float16 — so what accumulates is the reduced stack, not the raw one.
    #
    # coastal's own logging is OFF (`verbose=False`, see `_one`). It printed ~40 lines per call — a
    # banner, a per-metric dtype list, and "PROCESSING 1 MOVIES" — which at 60 sequences was ~2400
    # lines of identical output that buried this task's log and read as if the flow were being
    # computed twice. One line per sequence replaces it, carrying what the banner never said: how long
    # it took.
    n_seq = len(sequences)
    all_frames, all_metrics = [None] * n_seq, [None] * n_seq

    def _one(i):
        seq = sequences[i]
        i_scales, i_cum = seq_scales[i]
        # Onto the canonical offsets — see *The reference interval*. `{}` for every sequence in
        # `frames` mode and for the finest movie in `seconds` mode, where `apply_mag_rename` is a
        # no-op rather than a rebuilt dict.
        rename = coastal_utils.mag_rename(pool_scales, i_scales)
        t_seq = time.perf_counter()
        # `prepare_data_for_unet` rather than the `_batch` wrapper around it, for `verbose=False`. The
        # wrapper is a per-movie loop that calls exactly this and prints a banner the loop cannot turn
        # off, and we hand it one sequence anyway. Silencing it with `redirect_stdout` instead was the
        # first attempt and is WRONG here: that swaps `sys.stdout` process-wide, so with several
        # sequences in flight the threads swallow each other's output and race to restore it.
        seq_frames, flows, cum, seq_metrics = prepare_data_for_unet(
            seq, temporal_scales=i_scales, cumulative_window=i_cum, verbose=False)
        all_frames[i] = seq_frames
        all_metrics[i] = reduce_metrics(
            [coastal_utils.apply_mag_rename(mm, rename) for mm in seq_metrics], dropped)
        del flows, cum
        # The source plane sequence is a normalised copy inside `seq_frames` now; holding the original
        # as well costs a frame stack per sequence for nothing.
        sequences[i] = None
        del seq, seq_frames, seq_metrics
        return time.perf_counter() - t_seq

    rss_before = cpu_utils.rss_bytes()
    dt = _one(0)
    log.log(f'>>   [1/{n_seq}] flow metrics in {dt:.1f}s')

    # Two readings, largest wins, because each can miss on its own:
    #   • the high-water mark is the PROCESS's, so it may already have been set by reading the movies
    #     — then it prices this sequence at zero;
    #   • the current delta is only what was RETAINED (the reduced float16 stack), so it understates
    #     the transient float32 peak.
    # A `None`/0 answer means "could not price it", and `concurrency_for_memory` then falls back to
    # the CPU cap rather than inventing a size. The 50% reserve is what covers the understatement.
    peak, after = cpu_utils.peak_rss_bytes(), cpu_utils.rss_bytes()
    per_seq = None if rss_before is None else max(
        (0 if peak is None else peak - rss_before),
        (0 if after is None else after - rss_before), 0)
    avail = cpu_utils.available_memory_bytes()
    workers = cpu_utils.concurrency_for_memory(per_seq, avail, cap=cpu_utils.task_workers())
    if n_seq > 1:
        gb = lambda b: 'unknown' if b is None else f'{b / 2**30:.2f} GB'
        log.log(f'>> {n_seq - 1} sequence(s) left, {workers} at a time '
                f'(one costs {gb(per_seq)}, {gb(avail)} available)')

    if workers <= 1:
        for i in range(1, n_seq):
            log.log(f'>>   [{i + 1}/{n_seq}] flow metrics in {_one(i):.1f}s')
    else:
        with ThreadPoolExecutor(max_workers=workers) as ex:
            futures = {ex.submit(_one, i): i for i in range(1, n_seq)}
            for n, fut in enumerate(as_completed(futures), start=2):
                # Completion order, not submission order — the index says WHICH sequence finished,
                # the counter says how many are done, and conflating them would report progress that
                # goes backwards.
                dt = fut.result()
                log.log(f'>>   [{n}/{n_seq} done] sequence {futures[fut] + 1} '
                        f'flow metrics in {dt:.1f}s')

    assert all(f is not None for f in all_frames), 'a sequence produced no frames'
    log.progress(len(movies) + 1, total_steps)

    # Pool AFTER the per-sequence metrics: concatenating frames first would make flow cross a
    # boundary between two recordings — or between two Z planes of one timepoint — which is not
    # motion either way.
    #
    # `train_test_split_per_movie` splits WITHIN each sequence and then concatenates, so every movie
    # and every Z plane appears on both sides — a split that held whole movies out would measure
    # "does this transfer to another recording", which is a different (and much harder) question
    # than "has this converged or memorised". It takes the tail of each sequence, which for temporal
    # data is the right cut: the held-out frames are a stretch the optimiser never saw.
    #
    # The metrics were computed over the FULL sequence before the split, so a val frame's flow
    # metrics were derived partly from frames that ended up in training — about `max(scales)` frames
    # of overlap at the seam. That is not label leakage (coastal is unsupervised; there are no
    # labels), and the val frames themselves were never fed to the optimiser, which is what the
    # comparison rests on. Computing metrics per side instead would change the metrics themselves at
    # both seams, which is worse.
    train_ratio = float(params.get('trainRatio', 1.0))
    split = 0.0 < train_ratio < 1.0
    # Both branches re-reference the same metric dicts (the split copies the FRAMES, never the metric
    # planes), so once the pool exists `all_frames`/`all_metrics` have no reader — but they are still
    # a reference, and what they are holding a reference to is the 9 GB. Dropped explicitly rather
    # than left to fall out of scope at the end of a function that then trains for an hour.
    if split:
        frames_prep, val_frames_arr, metrics, val_metrics = train_test_split_per_movie(
            all_frames, all_metrics, train_ratio=train_ratio, shuffle=False)
    else:
        frames_prep = pool_frames(all_frames)
        metrics = [mm for per_sequence in all_metrics for mm in per_sequence]
        val_frames_arr, val_metrics = None, None
    del all_frames, all_metrics, sequences
    val_metrics = val_metrics or None

    # Both sides, or the model trains on one channel layout and is scored on another.
    key_sets = {tuple(sorted(mm.keys())) for mm in metrics + (val_metrics or [])}
    if len(key_sets) > 1:
        # Would train the model on inconsistent channel layouts — the silent failure this whole
        # contract exists to prevent, so it stops the run.
        raise ValueError(f'movies produced different metric sets: {sorted(key_sets)}')
    metric_keys = sorted(metrics[0].keys())
    # `len`, not `.shape[0]` — the pool is a flat list of frames whenever the movies differ in size
    # (see `pool_frames`), and every count from here on has to read it as a sequence.
    n_pooled = len(frames_prep)
    log.log(f'>> {n_pooled} pooled frames, {len(metric_keys)} metrics: '
            f'{", ".join(metric_keys)}')
    if not isinstance(frames_prep, np.ndarray):
        # In the run's own log, because it is the reason a re-run with one image behaves differently
        # from a re-run with six — and the constraint (`batch_size` 1) it puts on the training call.
        log.log('>> movies differ in size — pooled frame by frame, one frame per batch')
    if split:
        log.log(f'>> holding out {len(val_metrics)} frames ({(1 - train_ratio) * 100:.0f}%) '
                f'for validation')

    # The real figure, from the metrics that exist. Still worth logging after the fact: training is
    # the long part and it holds all of this, so a run that is going to die of memory says so here
    # rather than at an arbitrary epoch — and the number tells you WHICH knob to turn, since it is
    # linear in Z planes, images and timepoints alike.
    #
    # BOTH sides. The held-out frames' metrics are held for the whole run too — they are evaluated
    # every epoch — so counting only the training pool understates the peak by the split fraction,
    # which is the one direction this number must not be wrong in.
    pooled_px = sum(int(f.size) for f in frames_prep)
    if val_frames_arr is not None:
        pooled_px += sum(int(f.size) for f in val_frames_arr)
    # `METRIC_DTYPE`'s itemsize, not a hardcoded 4: the number has to be what is actually held, or
    # the warning fires on a run that fits and stays quiet on one that does not.
    metrics_gb = pooled_px * len(metric_keys) * np.dtype(METRIC_DTYPE).itemsize / 1024 ** 3
    log.log(f'>> ~{metrics_gb:.1f} GB of flow metrics held in memory')
    if metrics_gb > MEMORY_WARN_GB:
        log.log(f'>> [WARN] ~{metrics_gb:.0f} GB of metrics — if the run is killed, reduce '
                f'Z planes, images or timepoints')

    # Keyed the way coastal keys its loss history, so the manifest can pair each curve with the
    # weight that scales it. `history` records the RAW term; the total is the weighted sum, so a term
    # only "adds anything" in proportion to weight × term — with no weights recorded, a curve cannot
    # be read. The terms coastal supports but this task does not expose are pinned at 0 here rather
    # than left to coastal's defaults, so the manifest states them outright.
    # Coastal's own default, restated here so the manifest records what was used rather than
    # whatever coastal's signature happens to say later.
    blur_sigma = float(params.get('foregroundBlurSigma', 1.0))
    loss_weights = {
        'intensity': float(params.get('intensityWeight', 0.25)),
        'foreground': float(params.get('foregroundWeight', 1.0)),
        'temporal': float(params.get('temporalWeight', 2.0)),
        'variance': 0.0, 'confetti': 0.0, 'warp': 0.0, 'boundary': 0.0,
    }

    model = train_with_metrics(
        frames_prep, metrics, variance_metrics_norm=None,
        # No `val_flow_pairs` — the warp term is at weight 0 here (see `loss_weights`), so the
        # missing flow pairs cost nothing and `val_total` stays comparable with `total`. If warp is
        # ever given a weight, this needs the pairs or the two curves stop meaning the same thing.
        val_frames=val_frames_arr, val_temporal_metrics_norm=val_metrics,
        # The long phase, and the only one that can report from inside itself. coastal's own prints
        # fire every tenth epoch and are written for a notebook reader, so the callback is what an
        # application can drive a bar from.
        on_epoch=lambda epoch, n_epochs, losses: log.progress(
            len(movies) + 1 + epoch, total_steps),
        num_epochs=epochs,
        intensity_weight=loss_weights['intensity'],
        foreground_weight=loss_weights['foreground'],
        foreground_blur_sigma=blur_sigma,
        # The flow-boundary term inside ForegroundLoss: subtracts a blob-scaled flow-discontinuity
        # map from the target, so the prob map pinches where the velocity field tears. 0 = off, which
        # is the default; `validate_params` refuses a non-zero weight unless the three metrics
        # `flow_discontinuity` needs are ticked, because it degrades silently on a partial set.
        foreground_boundary_weight=float(params.get('foregroundBoundaryWeight', 0.0)),
        temporal_weight=loss_weights['temporal'],
        variance_weight=loss_weights['variance'],
        warp_weight=loss_weights['warp'],
        boundary_weight=loss_weights['boundary'],
        confetti_weight=loss_weights['confetti'],
        variance_as_input=False,
        embedding_dim=int(params.get('embeddingDim', 16)),
        seed=int(params.get('seed', 42)),
        device=gpu_device if use_gpu else 'cpu')

    history = None
    if isinstance(model, tuple):
        model, history = model[0], (model[1] if len(model) > 1 else None)

    model_path = params['modelPath']
    os.makedirs(os.path.dirname(model_path), exist_ok=True)

    # The per-epoch loss belongs WITH the model, not only in the run's QC: QC is keyed by the task run
    # (set-scope dir, and a model can be renamed away from it), while "did this model converge" is a
    # question you ask of the model, months later, from the vault. A few hundred floats per term.
    curves, floors = _split_floors(_loss_curves(history))
    losses = curves.get('total', [])

    manifest = {
        # The POOLED offsets, which in `frames` mode are the form's own and in `seconds` mode are the
        # declared spans at `temporalReferenceInterval`. Either way this is what the channels are
        # named after, which is the only thing inference can configure itself from.
        'temporalScales': pool_scales,
        'cumulativeWindow': pool_cumulative,
        # ── What the offsets above MEAN ──────────────────────────────────────────────────────────
        # `frames` (or absent, for every model trained before this) = the offsets are the setting and
        # nothing says which frame rate they belong to beyond `physicalScales`. `s` = the spans were
        # declared and the offsets are their resolution at `temporalReferenceInterval`; inference
        # re-resolves from the SPANS, so a recipient's movie adapts itself instead of the recipient
        # matching a frame count. See MODEL_VAULT_PLAN -> *Would you train in physical units*.
        'temporalScaleUnit': 's' if mode == 'seconds' else 'frames',
        'droppedMetrics': list(dropped),
        'metricKeys': metric_keys,
        'metricDtype': np.dtype(METRIC_DTYPE).name,
        'channelName': params.get('channelName', ''),
        'trainChannels': list(params['trainChannels']),
        'epochs': epochs,
        'embeddingDim': int(params.get('embeddingDim', 16)),
        'seed': int(params.get('seed', 42)),
        'normalise': float(params.get('normalise', 99.99)),
        'sourceImages': used,
        'sourceValueName': params.get('valueName', ''),
        'nFrames': n_pooled,
        'foregroundWeight': float(params.get('foregroundWeight', 1.0)),
        # Not just the weight — the SHAPE. Two runs at foregroundWeight 1.0 and different blurs fit
        # different targets, and their loss curves are NOT comparable: a wider blur softens the
        # target, raising its entropy and therefore the floor, so the better-shaped objective scores
        # worse. Without this in the manifest that difference is invisible.
        'foregroundBlurSigma': blur_sigma,
        'foregroundBoundaryWeight': float(params.get('foregroundBoundaryWeight', 0.0)),
        'intensityWeight': float(params.get('intensityWeight', 0.25)),
        'temporalWeight': float(params.get('temporalWeight', 2.0)),
        'maxFrames': max_frames,
        'trainRatio': train_ratio,
        # Only the movies that were actually cut. The window is seed-derived, so this is what makes
        # "which frames did it see" answerable without re-deriving it from the seed by hand.
        'frameWindows': windows,
        'zPlanes': n_planes,
        'zSpacing': z_spacing,
        # The crop SIZE alone would not let anyone re-derive what the model saw — the window is
        # random per (movie, plane), so the positions are the record. Same reasoning as
        # `frameWindows`: seed-derived is reproducible only if you also know the rule, and a saved
        # model outlives anyone's memory of it. Empty when training on whole frames.
        'cropSize': crop_size,
        'cropWindows': crops,
        # The indices, not just the count: "3 planes" of a 31-deep stack and of a 9-deep one are
        # different depths, and which ones a model saw is the question you ask when it does badly on
        # a stack of a different thickness. Empty for 2D movies.
        'zPlanesUsed': planes_used,
        # What a pixel and a frame ARE, per movie — see `_physical_scale`. The one field that says
        # whether this model can be applied to somebody else's movie, and the reason a vault entry
        # can state a resolution range at all (docs/todo/MODEL_VAULT_PLAN.md).
        'physicalScales': phys_scales,
        # Whether that is a measurement or a gap: `ome` = every movie carried it, `partial` = some
        # did, `none` = the images have no physical metadata and this model's scale is unknown. A
        # reader must not have to compare `physicalScales`' keys against `sourceImages` to find out.
        'physicalScaleSource': ('ome' if len(phys_scales) == len(used)
                                else 'none' if not phys_scales else 'partial'),
        # The engine, not just its parameters. Coastal's inference is under active change and one of
        # those changes moved a default that decides object size, so "which coastal" is part of what
        # this model IS — see `_coastal_build`.
        'coastalBuild': _coastal_build(),
        # What that build's flow actually COMPUTES, measured rather than named. `coastalBuild`
        # identifies the code; it cannot say whether a given change touched the feature recipe, so as
        # a check it fires on every commit. This is the numeric answer: a summary of every metric
        # plane the real inference entry point returns on a fixed synthetic window, which inference
        # re-measures and compares. ~4 ms, and the coastal import it needs is already paid.
        # See `cecelia.utils.flow_probe` and MODEL_VAULT_PLAN.md P0.
        'flowFingerprint': flow_probe.fingerprint(),
        'lossCurves': curves,
        # SEPARATE from lossCurves, not a `floor_foreground` entry inside it: a reader that walks
        # lossCurves to draw one line per term would otherwise draw the floors as terms of their own.
        # Keyed identically to lossCurves (`foreground`, `val_foreground`) so the two join by term.
        # Absent for the contrastive terms, which have no floor — see coastal.loss.bce_floor.
        'lossFloors': floors,
        'lossWeights': loss_weights,
        'trainedAt': _now_iso(),
    }

    if mode == 'seconds':
        # Only on a seconds model, rather than as nulls on every one. A key that is absent means "this
        # model does not work that way"; a key that is present and null reads as a measurement that
        # went missing, which is the distinction `_physical_scale` already makes for the same reason.
        manifest.update({
            'temporalScaleSeconds': declared,
            'cumulativeWindowSeconds': cum_seconds or None,
            # The frame rate `temporalScales` belongs to — the finest among the movies used. Read by
            # `manifest_frame_interval` in preference to `physicalScales`, which cannot answer for a
            # model pooled across rates and correctly returns None there.
            'temporalReferenceInterval': ref_interval,
            # The coarsest acquisition this model can be applied to. Past it two declared spans round
            # to the same frame offset and the mag planes stop being distinct features, which
            # inference refuses rather than clamps — so the ceiling is stated up front instead of at
            # the point of failure.
            'maxFrameInterval': coastal_utils.max_frame_interval(declared),
            # Per movie, because here they legitimately differ — the record of what each one was
            # actually read at, before its planes were renamed onto `temporalScales`.
            'temporalScalesPerMovie': movie_scales,
        })

    # BOTH records, from one dict and before either is written: `save_model` embeds it in the
    # checkpoint and the sidecar is written from the same object. Updating between the two would give
    # a model two manifests that disagree about what it is.
    save_model(model, model_path, metadata=manifest)
    # Sidecar as well as the checkpoint's own metadata: the picker, the vault manager and
    # `list_coastal_models` all need this without importing torch.
    write_json_atomic(coastal_utils.manifest_path(model_path), manifest)
    log.log(f'>> saved {model_path}')

    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        qc = {'epochs': epochs, 'nImages': len(used)}
        if losses:
            qc['finalLoss'] = float(losses[-1])
            qc['lossDrop'] = float(losses[0] / losses[-1]) if losses[-1] > 0 else float('inf')
        # The held-out curve, same shape. `lossDrop` alone cannot tell converging from memorising —
        # it is measured on the frames the weights were fitted to — so where there is a split, the
        # same ratio on the held-out set is the finding worth banking.
        val_losses = curves.get('val_total', [])
        if val_losses:
            qc['valFinalLoss'] = float(val_losses[-1])
            qc['valLossDrop'] = (float(val_losses[0] / val_losses[-1]) if val_losses[-1] > 0
                                 else float('inf'))
        write_json_atomic(qc_out_path, qc)
        log.log(f'>> saved training QC: {qc}')

    log.log('>> done')


def _now_iso():
    """Local wall-clock, to the minute — "when was this trained", read by a human in the vault.

    The vault's Date column comes from the .pt's mtime, which a copy or a restore rewrites. This one
    travels inside the manifest and does not.
    """
    return datetime.datetime.now().strftime('%Y-%m-%d %H:%M')


def _loss_curves(history):
    """Per-epoch loss PER TERM — `{'total': [...], 'temporal': [...], …}`.

    Every term, not just the total, because the total is the one curve that cannot answer the
    question you ask a loss curve: coastal optimises a weighted sum (`intensity`, `temporal`,
    `variance`, `foreground`, …) and `foregroundWeight`/`intensityWeight`/`temporalWeight` are task
    params, so "which term is this weight actually moving" is the reason to look.

    Every non-empty series is kept, including the flat ones. Filtering here on "the raw values are
    all zero" would be the wrong test in both directions: coastal computes some terms whatever their
    weight (a raw `variance` curve at weight 0 contributes nothing but is not zero), and a term that
    genuinely reached zero is a result worth seeing. The weights travel beside these as
    `lossWeights`, which is what makes a curve readable.

    Best-effort by design: the curves are provenance, and a coastal version that returns the history
    differently must not fail a training run that otherwise succeeded.
    """
    if history is None:
        return {}
    if isinstance(history, dict):
        out = {}
        for key, series in history.items():
            try:
                vals = [float(v) for v in series]
            except (TypeError, ValueError):
                continue
            if vals:
                out[str(key)] = vals
        return out
    try:
        return {'total': [float(v) for v in history]}
    except (TypeError, ValueError):
        return {}


def _split_floors(curves):
    """`(curves, floors)` — coastal's flat history split on the `floor_` prefix.

    coastal returns one dict (`foreground`, `val_foreground`, `floor_foreground`,
    `val_floor_foreground`, …) because that is what its accumulation loop naturally produces. The
    manifest keeps them apart so `lossCurves` stays exactly "one entry per loss term": anything that
    iterates it to draw a line per term — the frontend does — would otherwise draw three extra
    "terms" that are not terms.

    The prefix is stripped and `val_` restored to the FRONT, so a floor is keyed exactly like the
    curve it belongs to and the two join by term with no special cases:
    `val_floor_foreground` -> `floors['val_foreground']`.
    """
    out, floors = {}, {}
    for key, vals in curves.items():
        val, stem = (True, key[4:]) if key.startswith('val_') else (False, key)
        if stem.startswith('floor_'):
            floors[('val_' if val else '') + stem[len('floor_'):]] = vals
        else:
            out[key] = vals
    return out, floors


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
