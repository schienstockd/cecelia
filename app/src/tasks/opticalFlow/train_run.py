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
  foregroundWeight, intensityWeight, temporalWeight, foregroundBlurSigma
"""

import json
import datetime
import os

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.gpu_utils import torch_device
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils import coastal_utils


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


def _training_sequence(im_dat, dim_utils, params, z, window=None):
    """`[T, H, W]` float32 in 0–255 for ONE Z plane — the same projection inference builds per tile.

    Percentiles are taken over the WHOLE plane sequence — every timepoint, including those outside
    `window` — which is what makes this the global statistic `normaliseToWhole` reproduces at
    inference from the image pyramid. If the two ever diverge, the model sees a different photometric
    range than it was trained on.

    That ordering is the whole subtlety of the frame cap: normalise over the movie, THEN cut. Cutting
    first would scale a 50-frame window by its own percentiles while inference scales the 200-frame
    movie by the movie's, and the mismatch is silent — the same structure at a different brightness.

    Per plane, deliberately: each plane is normalised on its own statistics, the way inference
    normalises the plane it is given. Sharing one range across planes would push the dim deep planes
    toward zero and make them contribute nothing.
    """
    channels = list(params['trainChannels'])
    percentile_hi = float(params.get('normalise', 99.99))

    level = im_dat[0]
    ia = {ax: i for i, ax in enumerate(dim_utils.im_dim_order)}
    n_t = dim_utils.dim_val('T')

    projected = None
    for ch in channels:
        idx = [slice(None)] * level.ndim
        idx[ia['C']] = ch
        if z is not None:
            idx[ia['Z']] = z
        arr = np.asarray(level[tuple(idx)], dtype=np.float32)

        # Axes left are T + Y + X in the image's own order; move T to the front.
        remaining = [ax for ax in dim_utils.im_dim_order
                     if ax != 'C' and not (z is not None and ax == 'Z')]
        arr = np.moveaxis(arr, remaining.index('T'), 0)

        lo = float(np.percentile(arr, 100 - percentile_hi))
        hi = float(np.percentile(arr, percentile_hi))
        arr = np.clip((arr - lo) / (hi - lo + 1e-8), 0.0, 1.0)
        projected = arr if projected is None else np.maximum(projected, arr)

    assert projected.shape[0] == n_t, f'expected {n_t} frames, got {projected.shape[0]}'
    # Cut only now — after every percentile has seen the whole movie (see the docstring).
    if window is not None:
        projected = projected[window[0]:window[1]]
    return (projected * coastal_utils.PROJECTION_MAX).astype(np.float32)


def run(params):
    log = script_utils.get_logfile_utils(params)

    # All three live in coastal.train — `prepare_data_for_unet_batch` reads as a flow helper and is
    # not one, which cost an end-to-end run to find (unit tests stub coastal, so nothing caught it).
    from coastal.train import (prepare_data_for_unet_batch, train_test_split_per_movie,
                               train_with_metrics, save_model)

    movies = list(params['movies'])
    scales = [int(s) for s in params['temporalScales']]
    cumulative = int(params.get('cumulativeWindow', 5))
    dropped = tuple(params.get('droppedMetrics') or ())
    epochs = int(params.get('epochs', 30))

    use_gpu, gpu_device = torch_device()
    log.log(f'>> GPU: {gpu_device if use_gpu else "none (CPU)"}')
    log.log(f'>> {len(movies)} movie(s) to prepare')

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
    scales = {}
    for i, m in enumerate(movies):
        im_path = m['imPath']
        uid = m.get('uID', '')
        log.log(f'>> [{i + 1}/{len(movies)}] {uid}: {im_path}')
        im_dat, dim_utils = _open(im_path)

        if not dim_utils.is_timeseries():
            log.log(f'>> [WARN] {uid} has no T axis — skipped')
            continue
        n_t = int(dim_utils.dim_val('T'))
        start, stop = frame_window(n_t, max_frames, seed, i)
        n_use = stop - start
        # Checked against the CAPPED length, not the movie's. A 200-frame movie capped to 5 produces
        # no `mag_8` plane, which is the same silent corruption as a genuinely short movie — the
        # guard has to see what the run will actually feed coastal.
        if n_use < max(scales) + 1:
            # The same guard CoastalUtils applies. Below this the largest scale produces no plane,
            # so this movie would contribute a DIFFERENT channel layout than the rest — which is a
            # silent corruption of the pooled training set, not just a short movie.
            of = f'{n_use} of {n_t}' if n_use < n_t else f'{n_t}'
            log.log(f'>> [WARN] {uid} has {of} timepoints, needs '
                    f'{max(scales) + 1} for scale {max(scales)} — skipped')
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
            scales[uid] = scale

        n_y, n_x = int(dim_utils.dim_val('Y')), int(dim_utils.dim_val('X'))
        for zi, z in enumerate(planes):
            seq = _training_sequence(im_dat, dim_utils, params, z, (start, stop))
            # Cropped AFTER the projection, never before: the percentiles are taken over the whole
            # plane and the whole movie (see `_training_sequence`), which is the statistic inference
            # reproduces. Normalising a crop by its own percentiles would scale the same structure
            # differently depending on where the window landed.
            #
            # Seeded per (movie, plane) so each window is independent — two planes of one stack are
            # two views of the tissue, and giving them the same XY window would make them more alike
            # than they need to be. Reproducible from the manifest's seed either way.
            win = crop_window(seq.shape[1:], crop_size,
                              np.random.default_rng([seed, i, zi]))
            if win is not None:
                y0, x0, hh, ww = win
                # A COPY, not the slice's view: a view keeps the whole uncropped plane stack alive,
                # which is the entire allocation this parameter exists to avoid.
                seq = np.ascontiguousarray(seq[:, y0:y0 + hh, x0:x0 + ww])
                crops.setdefault(uid, []).append([y0, x0, hh, ww])
            sequences.append(seq)
            del seq
        used.append(uid)
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

    log.log(f'>> computing flow metrics for {len(sequences)} sequence(s) '
            f'(scales {scales}, cumulative {cumulative})')
    # ONE SEQUENCE AT A TIME, and each one reduced to what training keeps before the next is computed.
    # `prepare_data_for_unet_batch` is a plain per-movie loop with no cross-movie state, so this is
    # the same computation — but handing it all six at once means the full float32 metric stack of
    # every movie is live at the peak, which for a six-movie zolIMa set is ~23 GB held before
    # training allocates anything. Reduced here it is ~9 GB held (measured 1.55 GB per movie), and
    # what sits above that is one movie's flow fields rather than six.
    #
    # The two reductions are `reduce_metrics`: drop the unwanted metrics here rather than after the
    # split, and hold the rest as float16.
    all_frames, all_metrics = [], []
    for i, seq in enumerate(sequences):
        seq_frames, seq_metrics = prepare_data_for_unet_batch(
            [seq], temporal_scales=scales, cumulative_window=cumulative)
        all_frames.append(seq_frames[0])
        all_metrics.append(reduce_metrics(seq_metrics[0], dropped))
        # The source plane sequence is a normalised copy inside `seq_frames` now; holding the
        # original as well costs a frame stack per movie for nothing.
        sequences[i] = None
        del seq, seq_frames, seq_metrics
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
        'intensity': float(params.get('intensityWeight', 1.0)),
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
        'temporalScales': scales,
        'cumulativeWindow': cumulative,
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
        'intensityWeight': float(params.get('intensityWeight', 1.0)),
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
        'physicalScales': scales,
        # Whether that is a measurement or a gap: `ome` = every movie carried it, `partial` = some
        # did, `none` = the images have no physical metadata and this model's scale is unknown. A
        # reader must not have to compare `physicalScales`' keys against `sourceImages` to find out.
        'physicalScaleSource': ('ome' if len(scales) == len(used)
                                else 'none' if not scales else 'partial'),
        # The engine, not just its parameters. Coastal's inference is under active change and one of
        # those changes moved a default that decides object size, so "which coastal" is part of what
        # this model IS — see `_coastal_build`.
        'coastalBuild': _coastal_build(),
        'lossCurves': curves,
        # SEPARATE from lossCurves, not a `floor_foreground` entry inside it: a reader that walks
        # lossCurves to draw one line per term would otherwise draw the floors as terms of their own.
        # Keyed identically to lossCurves (`foreground`, `val_foreground`) so the two join by term.
        # Absent for the contrastive terms, which have no floor — see coastal.loss.bce_floor.
        'lossFloors': floors,
        'lossWeights': loss_weights,
        'trainedAt': _now_iso(),
    }

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
