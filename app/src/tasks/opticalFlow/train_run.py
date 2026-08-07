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
  temporalScales           - already parsed + validated by Julia
  cumulativeWindow, droppedMetrics, epochs, embeddingDim, seed, normalise
  foregroundWeight, intensityWeight, temporalWeight
"""

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


def _open(im_path):
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    return im_dat, dim_utils


def z_planes(n_z, n):
    """`n` evenly-spaced plane indices through a stack of `n_z` — the centres of `n` equal bins.

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
    n = max(1, min(int(n), n_z))
    return sorted({int((i + 0.5) * n_z / n) for i in range(n)})


def _training_sequence(im_dat, dim_utils, params, z):
    """`[T, H, W]` float32 in 0–255 for ONE Z plane — the same projection inference builds per tile.

    Percentiles are taken over the WHOLE plane sequence, which is what makes this the global
    statistic `normaliseToWhole` reproduces at inference from the image pyramid. If the two ever
    diverge, the model sees a different photometric range than it was trained on.

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
    return (projected * coastal_utils.PROJECTION_MAX).astype(np.float32)


def run(params):
    log = script_utils.get_logfile_utils(params)

    # All three live in coastal.train — `prepare_data_for_unet_batch` reads as a flow helper and is
    # not one, which cost an end-to-end run to find (unit tests stub coastal, so nothing caught it).
    from coastal.train import prepare_data_for_unet_batch, train_with_metrics, save_model

    movies = list(params['movies'])
    scales = [int(s) for s in params['temporalScales']]
    cumulative = int(params.get('cumulativeWindow', 5))
    dropped = tuple(params.get('droppedMetrics') or ())
    epochs = int(params.get('epochs', 30))

    use_gpu, gpu_device = torch_device()
    log.log(f'>> GPU: {gpu_device if use_gpu else "none (CPU)"}')
    log.log(f'>> {len(movies)} movie(s) to prepare')

    n_planes = int(params.get('zPlanes', 1))
    sequences, used, planes_used = [], [], {}
    for i, m in enumerate(movies):
        im_path = m['imPath']
        uid = m.get('uID', '')
        log.log(f'>> [{i + 1}/{len(movies)}] {uid}: {im_path}')
        im_dat, dim_utils = _open(im_path)

        if not dim_utils.is_timeseries():
            log.log(f'>> [WARN] {uid} has no T axis — skipped')
            continue
        n_t = int(dim_utils.dim_val('T'))
        if n_t < max(scales) + 1:
            # The same guard CoastalUtils applies. Below this the largest scale produces no plane,
            # so this movie would contribute a DIFFERENT channel layout than the rest — which is a
            # silent corruption of the pooled training set, not just a short movie.
            log.log(f'>> [WARN] {uid} has {n_t} timepoints, needs '
                    f'{max(scales) + 1} for scale {max(scales)} — skipped')
            continue

        # Planes are resolved PER MOVIE because depth is: asking for 3 of a 31-plane stack and 3 of
        # a 9-plane one are different indices, and a single global list would read past the end of
        # the shallow one.
        axes = set(dim_utils.im_dim_order)
        if 'Z' in axes:
            n_z = int(dim_utils.dim_val('Z'))
            planes = z_planes(n_z, n_planes)
            planes_used[uid] = planes
            if len(planes) < n_planes:
                log.log(f'>> [WARN] {uid} has {n_z} Z planes — training on {len(planes)}, '
                        f'not the {n_planes} requested')
        else:
            planes = [None]

        for z in planes:
            sequences.append(_training_sequence(im_dat, dim_utils, params, z))
        used.append(uid)
        where = f'Z {planes}' if planes != [None] else '2D'
        log.log(f'>>   {where}: {len(planes)} × {n_t} frames of {sequences[-1].shape[1:]}')

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
    n_frames_pooled = sum(int(s.shape[0]) for s in sequences)
    mpx = float(np.prod(sequences[0].shape[1:])) / 1e6
    log.log(f'>> pooling {n_frames_pooled} frames from {len(sequences)} sequence(s) '
            f'at {mpx:.2f} MP')

    log.log(f'>> computing flow metrics for {len(sequences)} sequence(s) '
            f'(scales {scales}, cumulative {cumulative})')
    all_frames, all_metrics = prepare_data_for_unet_batch(
        sequences, temporal_scales=scales, cumulative_window=cumulative)

    # Pool AFTER the per-sequence metrics: concatenating frames first would make flow cross a
    # boundary between two recordings — or between two Z planes of one timepoint — which is not
    # motion either way.
    frames_prep = np.concatenate(all_frames, axis=0)
    metrics = [{k: v for k, v in mm.items() if k not in dropped}
               for per_movie in all_metrics for mm in per_movie]

    key_sets = {tuple(sorted(mm.keys())) for mm in metrics}
    if len(key_sets) > 1:
        # Would train the model on inconsistent channel layouts — the silent failure this whole
        # contract exists to prevent, so it stops the run.
        raise ValueError(f'movies produced different metric sets: {sorted(key_sets)}')
    metric_keys = sorted(metrics[0].keys())
    log.log(f'>> {frames_prep.shape[0]} pooled frames, {len(metric_keys)} metrics: '
            f'{", ".join(metric_keys)}')

    # The real figure, from the metrics that exist. Still worth logging after the fact: training is
    # the long part and it holds all of this, so a run that is going to die of memory says so here
    # rather than at an arbitrary epoch — and the number tells you WHICH knob to turn, since it is
    # linear in Z planes, images and timepoints alike.
    metrics_gb = (frames_prep.shape[0] * float(np.prod(frames_prep.shape[1:]))
                  * len(metric_keys) * 4 / 1024 ** 3)
    log.log(f'>> ~{metrics_gb:.1f} GB of flow metrics held in memory')
    if metrics_gb > MEMORY_WARN_GB:
        log.log(f'>> [WARN] ~{metrics_gb:.0f} GB of metrics — if the run is killed, reduce '
                f'Z planes, images or timepoints')

    # Keyed the way coastal keys its loss history, so the manifest can pair each curve with the
    # weight that scales it. `history` records the RAW term; the total is the weighted sum, so a term
    # only "adds anything" in proportion to weight × term — with no weights recorded, a curve cannot
    # be read. The terms coastal supports but this task does not expose are pinned at 0 here rather
    # than left to coastal's defaults, so the manifest states them outright.
    loss_weights = {
        'intensity': float(params.get('intensityWeight', 1.0)),
        'foreground': float(params.get('foregroundWeight', 1.0)),
        'temporal': float(params.get('temporalWeight', 2.0)),
        'variance': 0.0, 'confetti': 0.0, 'warp': 0.0, 'boundary': 0.0,
    }

    model = train_with_metrics(
        frames_prep, metrics, variance_metrics_norm=None,
        num_epochs=epochs,
        intensity_weight=loss_weights['intensity'],
        foreground_weight=loss_weights['foreground'],
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
    curves = _loss_curves(history)
    losses = curves.get('total', [])

    manifest = {
        'temporalScales': scales,
        'cumulativeWindow': cumulative,
        'droppedMetrics': list(dropped),
        'metricKeys': metric_keys,
        'channelName': params.get('channelName', ''),
        'trainChannels': list(params['trainChannels']),
        'epochs': epochs,
        'embeddingDim': int(params.get('embeddingDim', 16)),
        'seed': int(params.get('seed', 42)),
        'normalise': float(params.get('normalise', 99.99)),
        'sourceImages': used,
        'sourceValueName': params.get('valueName', ''),
        'nFrames': int(frames_prep.shape[0]),
        'foregroundWeight': float(params.get('foregroundWeight', 1.0)),
        'intensityWeight': float(params.get('intensityWeight', 1.0)),
        'temporalWeight': float(params.get('temporalWeight', 2.0)),
        'zPlanes': n_planes,
        # The indices, not just the count: "3 planes" of a 31-deep stack and of a 9-deep one are
        # different depths, and which ones a model saw is the question you ask when it does badly on
        # a stack of a different thickness. Empty for 2D movies.
        'zPlanesUsed': planes_used,
        'lossCurves': curves,
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


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
