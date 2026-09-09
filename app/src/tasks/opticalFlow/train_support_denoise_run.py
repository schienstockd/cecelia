"""Train a SUPPORT model on N images from an experimental set.

Thin caller — the training loop itself lives in `coastal.support.train_support` (algorithm), this
file owns only the IO half: opening OME-ZARR volumes through `zarr_utils`, running the free-VRAM
pre-flight, and writing the `.pt` + `.json` manifest atomically. Same split as `smooth_run.py`
uses for `coastal.smooth`.

The manifest schema is unchanged from Phase B/C — `arch` mirrors `SUPPORT(...)` kwargs so the
inference runner (`denoise_run.py`) can rebuild the network via `coastal.support.build_model`.
DENOISE_INTEGRATION_PLAN.md → D3 amendment (pool channels into one training run) applies here
too: `trainChannels` is multi-select. `trainMode` (added in SUPPORT_PERCHANNEL_PLAN.md 2026-09-07)
picks between:
  - pooled (default): one `.pt` + `.json` at `modelPath`, patches from every channel pool into one
    training set. Best when channels are comparable in SNR.
  - perChannel: one `.pt` + `.json` per channel inside a bundle folder at `bundleDir`, plus a
    top-level `manifest.json` for the bundle. Used when a channel would collapse in a pooled run.

`auto` is parked — the v1 SNR precheck (`channel_snr_proxy` + `_resolve_mode` below) over-fires on
narrow-DR-but-clean channels. Post-run collapse QC in `cleanupImages.denoise` is the reliable signal.
See SUPPORT_PERCHANNEL_PLAN.md → Deferred.

Parameter contract (JSON written by Julia):
  movies              - [{uID, imPath}], set-scope
  taskDir             - the run's task dir (log/QC live here)
  modelPath           - absolute `.pt` target for a POOLED run
  bundleDir           - absolute directory target for a PER-CHANNEL run
  qcOutPath           - JSON with loss curve + arch, read by _support_train_qc_findings
  valueName           - versioned filepath key (usually driftCorrected)
  trainMode           - "pooled" | "perChannel"
  trainChannels       - list of 0-based channel indices
  channelNames        - human names in the same order, stored in the manifest
  inputFrames         - temporal window (odd; centre is the target)
  patchXY             - spatial patch size
  epochs              - passes over the pooled patches
  batchSize           - patches per gradient step
  learningRate        - Adam lr
  midChannels/depth/blindConvChannels - UNet architecture
  unetSize            - "small"/"medium"/"large" — drives the VRAM pre-flight only
  midZOnly            - True = middle Z per movie (matches per-Z inference); False = all Z
"""
import re
from pathlib import Path

import numpy as np
import torch

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.gpu_utils import torch_device, require_free_vram_gb
from cecelia.utils.atomic_io import atomic_path, write_json_atomic

from coastal.support import train_support

# Free-VRAM budgets per UNet size, measured on RTX 2000 Ada Laptop 8 GB (~5 GB free after other
# processes). These are the peak-training footprints — inference is much smaller and does not
# need the pre-flight check. Keys mirror `_SUPPORT_UNET_SIZES` in train_support_denoise.jl.
_MIN_FREE_VRAM_GB = {'small': 1.5, 'medium': 3.5, 'large': 5.0}


def _axis_len(dim_utils, letter, shape):
    idx = dim_utils.dim_idx(letter)
    return (idx, shape[idx]) if idx is not None else (None, 1)


def _volume_for_zc(level, dim_utils, shape, c_idx, z_idx, c, z):
    """Return a [T, Y, X] numpy volume for one (channel, z) plane through the whole time axis."""
    sl = [slice(None)] * len(shape)
    if c_idx is not None:
        sl[c_idx] = c
    if z_idx is not None:
        sl[z_idx] = z
    return np.asarray(level[tuple(sl)], dtype=np.float32)


def _load_training_volumes(movies, value_name, channels, input_frames, mid_z_only, log):
    """Open each image via zarr_utils and pull one (or all) mid-Z volumes per selected channel as
    float32 [T, Y, X]. Returns `{channel_idx: [tensor, ...]}` — the pooled and per-channel training
    paths differ only in whether they flatten this dict or iterate its keys, so keeping the grouping
    intact here lets the caller pick.

    `coastal.support.train_support` treats each patch independently, so the pooled path just chains
    every channel's list together. Measured on fXgbTl 2026-09-05 (all three channels ~equal SNR):
    the pooled model matched or beat per-channel specialists. Measured on x4E5HU 2026-09-07 (one
    weak channel, CD169-Kat): the pooled prior collapsed the weak channel, motivating the
    perChannel escape hatch (SUPPORT_PERCHANNEL_PLAN.md)."""
    vols = {int(c): [] for c in channels}
    for m in movies:
        im_path = m['imPath']
        uid = m['uID']
        log.log(f'>> open {uid}: {im_path}')
        im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
        level = im_dat[0]

        omexml = ome_xml_utils.parse_meta(im_path)
        du = DimUtils(omexml, use_channel_axis=True)
        du.calc_image_dimensions(level.shape)

        shape = tuple(level.shape)
        _, nt = _axis_len(du, 'T', shape)
        c_idx, nc = _axis_len(du, 'C', shape)
        z_idx, nz = _axis_len(du, 'Z', shape)

        if nt < input_frames:
            log.log(f'[WARN] {uid}: only {nt} timepoints, need {input_frames} — skipped')
            continue

        for c in channels:
            if c >= nc:
                log.log(f'[WARN] {uid}: channel {c} out of range ({nc} channels) — skipped')
                continue
            planes = [nz // 2] if mid_z_only else list(range(nz))
            for z in planes:
                v = _volume_for_zc(level, du, shape, c_idx, z_idx, c, z)
                vols[int(c)].append(torch.from_numpy(v).float())
            log.log(f'   ch{c}: pooled {len(planes)} Z plane(s) ({v.shape[0]} × {v.shape[1]} × {v.shape[2]})')
    return vols


def _volume_snr(vol):
    """Poisson SNR proxy for ONE volume — head-room of the p99 signal above the median of NONZERO
    pixels, in units of shot-noise σ at that floor. Nonzero-median avoids the trap seen on fXgbTl
    2026-09-07: the raw p50 is 0 on mostly-dark microscopy frames, which pinned `sqrt(max(0,1))=1`
    as the noise σ and passed every channel by construction."""
    flat = np.asarray(vol).ravel() if not hasattr(vol, 'numpy') else vol.numpy().ravel()
    nonzero = flat[flat > 0]
    if nonzero.size < 10:
        return {'p50': 0.0, 'p99': 0.0, 'snr': 0.0}
    p50 = float(np.percentile(nonzero, 50))
    p99 = float(np.percentile(flat, 99))
    snr = (p99 - p50) / float(np.sqrt(max(p50, 1.0)))
    return {'p50': p50, 'p99': p99, 'snr': snr}


def channel_snr_proxy(vols_c):
    """Per-volume SNR proxies + the aggregate that drives the auto-switch. `worstSnr` is the MIN
    across movies — one weak movie in a training set of 20 will contribute weak-channel patches
    that the pooled prior smooths away, so the switch must fire on the weakest, not the average.
    See SUPPORT_PERCHANNEL_PLAN.md → D4(a). Pure, tested in `test_denoise_precheck.py`."""
    if not vols_c:
        return {'perVolume': [], 'worstSnr': float('nan'), 'meanSnr': float('nan')}
    per_vol = [_volume_snr(v) for v in vols_c]
    snrs = [d['snr'] for d in per_vol]
    return {'perVolume': per_vol,
            'worstSnr': float(min(snrs)),
            'meanSnr':  float(sum(snrs) / len(snrs))}


# Parked scaffolding for a future `trainMode=auto`. The v1 metric over-fires on channels that are
# narrow-DR but clean (e.g. nuc-GFP on fXgbTl + x4E5HU, worstSnr ≈ 1 despite no collapse), so the
# runtime call was removed 2026-09-08 — post-run collapse QC in `cleanupImages.denoise` catches the
# real problem case. `_resolve_mode` + tests stay live so the metric-v2 work has a home to land in.
# See SUPPORT_PERCHANNEL_PLAN.md → Deferred.
SNR_FAIL_THRESHOLD = 3.0


def _slug(name):
    """Filesystem-safe channel slug — alphanumerics + `.-_`, rest → `_`. Used for the per-channel
    `.pt` filenames inside a bundle folder. Collisions between two channel names that slug to the
    same value are rare in practice (would need e.g. `CD169-Kat` and `CD169+Kat`) — we error at
    save time rather than mangling further."""
    s = re.sub(r'[^A-Za-z0-9._-]', '_', str(name).strip())
    return s or '_'


def _train_one(vols_list, arch, epochs, batch_size, lr, device, log, label,
               patience=None, min_delta=5e-3):
    """One `train_support` call for `vols_list` (already the pooled or per-channel slice). Returns
    (state_dict, epoch_losses, summary_dict). `label` is prefixed on the log lines so a perChannel
    run's three sub-training passes are readable in one log.

    Also records a sub-epoch loss trace (`stepLosses` + matching `stepIndices`) at a stride chosen
    so at most ~5000 points land in the manifest regardless of dataset size. The training
    convergence plot uses it for the log(step) view — SUPPORT typically converges within the first
    ~100 gradient steps and the per-epoch mean buries that, making a converged run look flat.

    `patience` (in epochs) enables coastal's plateau early-stop; `None` keeps the classic "train
    to `epochs`" behaviour. `stoppedEarly` / `stopEpoch` in the summary let the caller carry the
    stop into the manifest so the Training convergence plot's Detail view can annotate it.
    """
    log.log(f'>> [{label}] training on {len(vols_list)} volume(s)')
    step_losses = []
    step_indices = []
    # We don't know the exact per-epoch batch count without opening the dataset here, so cap by a
    # rolling stride: log every step until we hit 500 points, then double the stride each time we
    # would overflow the ~5000-point budget. Bounded storage, keeps early-epoch fine resolution.
    _state = {'stride': 1, 'next_at': 1}
    _MAX_POINTS = 5000
    def _on_batch_loss(step, loss):
        if step >= _state['next_at']:
            step_losses.append(float(loss))
            step_indices.append(int(step))
            _state['next_at'] = step + _state['stride']
            if len(step_losses) >= _MAX_POINTS:
                # thin to half: keep every other point, double the stride going forward
                del step_losses[::2]
                del step_indices[::2]
                _state['stride'] *= 2
    state_dict, epoch_losses = train_support(
        volumes=vols_list, arch=arch, epochs=epochs, batch_size=batch_size, lr=lr,
        device=device, on_progress=log.progress, on_log=log.log,
        on_batch_loss=_on_batch_loss,
        patience=patience, min_delta=min_delta,
    )
    final = float(epoch_losses[-1]) if epoch_losses else float('nan')
    first = float(epoch_losses[0]) if epoch_losses else float('nan')
    drop  = (first / final) if (final and final > 0) else float('nan')
    stopped_early = len(epoch_losses) < epochs
    summary = {'finalLoss': final, 'firstLoss': first, 'lossDrop': drop,
               'epochLosses':  list(map(float, epoch_losses)),
               'stepLosses':   step_losses,
               'stepIndices':  step_indices,
               'stoppedEarly': stopped_early,
               'stopEpoch':    len(epoch_losses),   # 1-based; equal to `epochs` when it ran full
               'epochBudget':  int(epochs)}
    return state_dict, epoch_losses, summary


def _resolve_mode(requested, precheck, log):
    """Decide the effective trainMode from the user's choice and the precheck. A channel FAILS if
    its `worstSnr` across movies is below `SNR_FAIL_THRESHOLD` — one weak movie in a set of 20 is
    enough because a pooled prior smooths that movie's weak-channel patches away. `pooled` and
    `perChannel` are forced. Returns (mode, reason_line)."""
    if requested == 'perChannel':
        return 'perChannel', 'trainMode=perChannel (forced by user)'
    failed = [str(c) for c, stats in precheck.items()
              if np.isfinite(stats.get('worstSnr', np.nan))
                 and stats['worstSnr'] < SNR_FAIL_THRESHOLD]
    if requested == 'pooled':
        if failed:
            log.log(f'[WARN] SNR precheck flags channel(s) {failed} '
                    f'(worst<{SNR_FAIL_THRESHOLD:.1f}) but trainMode=pooled is forced — pooled '
                    f'prior may collapse the weak channel(s).')
        return 'pooled', 'trainMode=pooled (forced by user)'
    # auto
    if failed:
        return 'perChannel', (f'trainMode=auto → perChannel (worstSnr<{SNR_FAIL_THRESHOLD:.1f} '
                              f'on channel(s) {failed})')
    return 'pooled', (f'trainMode=auto → pooled (worstSnr≥{SNR_FAIL_THRESHOLD:.1f} on every '
                      f'channel across every movie)')


def run(params):
    log = script_utils.get_logfile_utils(params)

    movies         = list(params.get('movies') or [])
    model_path     = str(params.get('modelPath', ''))
    bundle_dir     = str(params.get('bundleDir', ''))
    qc_out_path    = params.get('qcOutPath')
    channels       = script_utils.channel_indices(
        params.get('trainChannels'), 'trainChannels', 'train_support_denoise.jl')
    channel_names  = list(params.get('channelNames') or [])
    input_frames   = int(params.get('inputFrames', 61))
    patch_xy       = int(params.get('patchXY', 128))
    epochs         = int(params.get('epochs', 20))
    batch_size     = int(params.get('batchSize', 2))
    lr             = float(params.get('learningRate', 5e-4))
    mid_channels   = list(params.get('midChannels', [64, 128, 256, 512]))
    depth          = int(params.get('depth', 4))
    # `blindConvChannels` is a UNet hidden-dim (integer), not a channel-selection param.
    # Extracted to a local so the NoBareChannelCoercion guard's regex (which flags
    # `int(params.get('...Channel*'))`) does not false-positive on it.
    _blind_conv_hidden = params.get('blindConvChannels', 64)
    blind_ch       = int(_blind_conv_hidden)
    mid_z_only     = bool(params.get('midZOnly', True))
    value_name     = str(params.get('valueName', ''))
    unet_size      = str(params.get('unetSize', 'medium'))
    train_mode_req = str(params.get('trainMode', 'pooled'))
    # Early stop. Runner passes patience=None when disabled → coastal keeps the classic behaviour.
    early_stop     = bool(params.get('earlyStop', True))
    patience_val   = int(params.get('patience', 5)) if early_stop else None
    min_loss_delta = float(params.get('minLossDelta', 5e-3))

    if train_mode_req not in ('pooled', 'perChannel'):
        log.log(f'[ERROR] trainMode must be pooled|perChannel, got "{train_mode_req}"')
        raise SystemExit(1)

    if not movies:
        log.log('[ERROR] no movies to train on')
        raise SystemExit(1)

    _, device = torch_device()
    if device is None:
        device = torch.device('cpu')
    log.log(f'>> device: {device}, arch UNet {mid_channels} depth {depth}, '
            f'inputFrames {input_frames}, patch {patch_xy}, epochs {epochs}, batch {batch_size}')

    # Pre-flight: refuse cleanly if the picked size cannot fit in free VRAM. Cheaper than an opaque
    # `CUDA out of memory` stack half-way through epoch 1, and points at the actionable fix (pick a
    # smaller size). No-op on MPS/CPU because there's no queryable free/total pair there.
    min_gb = _MIN_FREE_VRAM_GB.get(unet_size)
    if min_gb is not None:
        require_free_vram_gb(min_gb, f'Model size "{unet_size}"', log=log, device=device)

    if not channels:
        log.log('[ERROR] no channels selected — trainChannels was empty')
        raise SystemExit(1)

    # ── data ────────────────────────────────────────────────────────────────
    vols_by_ch = _load_training_volumes(movies, value_name, channels, input_frames, mid_z_only, log)
    if not any(vols_by_ch.values()):
        log.log('[ERROR] no usable volumes across the set')
        raise SystemExit(1)

    # `auto` is parked (see module doc + SNR_FAIL_THRESHOLD note above); the runtime honours the
    # user's chip verbatim. `_resolve_mode` + tests stay for the metric-v2 landing spot.
    ch_name_by_idx = {int(channels[i]): (channel_names[i] if i < len(channel_names)
                                         else str(channels[i]))
                      for i in range(len(channels))}
    mode = train_mode_req
    reason = f'trainMode={mode}'
    log.log(f'>> {reason}')

    # ── target-path sanity ─────────────────────────────────────────────────
    if mode == 'pooled':
        if not model_path:
            log.log('[ERROR] pooled mode needs modelPath from Julia')
            raise SystemExit(1)
    else:
        if not bundle_dir:
            log.log('[ERROR] perChannel mode needs bundleDir from Julia')
            raise SystemExit(1)

    # ── train (owned by coastal.support) ───────────────────────────────────
    # The arch dict is the shipping contract with the inference runner (`denoise_run.py` reads
    # these exact keys back via `coastal.support.build_model`); keep both in sync.
    arch = dict(
        inputFrames=input_frames,
        patchXY=patch_xy,
        midChannels=mid_channels,
        depth=depth,
        blindConvChannels=blind_ch,
        oneByOneChannels=[32, 16],
        lastLayerChannels=[64, 32, 16],
        bsSize=[3, 3],
        bp=False,
    )

    common_training = {
        'imageUids': [m['uID'] for m in movies],
        'valueName': value_name,
        'epochs': epochs,
        'batchSize': batch_size,
        'learningRate': lr,
        'midZOnly': mid_z_only,
    }

    per_channel_summaries = {}   # for the QC sidecar; keyed by channel name

    if mode == 'pooled':
        all_vols = [v for c in channels for v in vols_by_ch.get(int(c), [])]
        state_dict, epoch_losses, summary = _train_one(
            all_vols, arch, epochs, batch_size, lr, device, log, 'pooled',
            patience=patience_val, min_delta=min_loss_delta)

        # ── save (pooled) ──────────────────────────────────────────────────
        # `.pt` and manifest are two files; both must land atomically so a picker never sees a
        # half-pair (a .pt without a manifest is a hard error for the denoise runner — see D7).
        with atomic_path(model_path) as tmp_pt:
            torch.save(state_dict, tmp_pt)

        manifest = {
            'kind': 'denoise-support',
            'mode': 'pooled',
            'channels': channel_names,
            'arch': arch,
            'training': dict(common_training,
                             channelIndices=[int(c) for c in channels],
                             framesPerImage=[int(v.shape[0]) for v in all_vols],
                             **summary),
        }
        manifest_path = str(Path(model_path).with_suffix('.json'))
        write_json_atomic(manifest_path, manifest)
        log.log(f'>> saved {model_path}')
        log.log(f'>> saved {manifest_path}')

        per_channel_summaries['__pooled__'] = summary

    else:  # perChannel
        # One bundle folder, one `.pt` + `.json` per channel, plus a top-level manifest listing the
        # covered channels. See SUPPORT_PERCHANNEL_PLAN.md → D2 for the on-disk shape.
        bundle_root = Path(bundle_dir)
        bundle_root.mkdir(parents=True, exist_ok=True)

        # Slug-collision guard: two channel names that map to the same filesystem-safe slug would
        # silently overwrite each other's `.pt`. Detect up front and refuse with the pair.
        slugs_seen = {}
        for c in channels:
            slug = _slug(ch_name_by_idx[int(c)])
            if slug in slugs_seen:
                log.log(f'[ERROR] channel names "{slugs_seen[slug]}" and '
                        f'"{ch_name_by_idx[int(c)]}" both map to slug "{slug}" — rename one '
                        f'and retrain.')
                raise SystemExit(1)
            slugs_seen[slug] = ch_name_by_idx[int(c)]

        bundled_channels = []
        for c in channels:
            vols_c = vols_by_ch.get(int(c), [])
            if not vols_c:
                log.log(f'[WARN] channel {c} ({ch_name_by_idx[int(c)]}) has no volumes — skipped')
                continue
            state_dict, epoch_losses, summary = _train_one(
                vols_c, arch, epochs, batch_size, lr, device, log,
                f'perChannel {ch_name_by_idx[int(c)]}',
                patience=patience_val, min_delta=min_loss_delta)

            slug = _slug(ch_name_by_idx[int(c)])
            sub_pt = bundle_root / f'{slug}.pt'
            sub_json = bundle_root / f'{slug}.json'
            with atomic_path(str(sub_pt)) as tmp_pt:
                torch.save(state_dict, tmp_pt)
            sub_manifest = {
                'kind': 'denoise-support',
                'mode': 'perChannel-sub',
                'channels': [ch_name_by_idx[int(c)]],
                'arch': arch,
                'training': dict(common_training,
                                 channelIndices=[int(c)],
                                 framesPerImage=[int(v.shape[0]) for v in vols_c],
                                 **summary),
            }
            write_json_atomic(str(sub_json), sub_manifest)
            log.log(f'>> saved {sub_pt}')
            log.log(f'>> saved {sub_json}')

            per_channel_summaries[ch_name_by_idx[int(c)]] = summary
            bundled_channels.append({'index': int(c),
                                     'name': ch_name_by_idx[int(c)],
                                     'slug': slug,
                                     'pt': f'{slug}.pt'})

        if not bundled_channels:
            log.log('[ERROR] perChannel run produced no models — nothing was trained')
            raise SystemExit(1)

        # Top-level bundle manifest — enumerated so the inference resolver can look up a channel
        # without having to grep the directory. `training.perChannelLosses` carries one loss curve
        # per trained channel so the Training convergence plot (FlowTrainingView) can draw them all
        # as separate series, keyed by channel name (matches the chip labels).
        # `perChannelStepLosses` / `perChannelStepIndices` carry the sub-epoch trace per channel so
        # the plot's Detail view can render the log(step) descent + plateau per channel.
        top_manifest = {
            'kind': 'denoise-support',
            'mode': 'perChannel',
            'channels': [b['name'] for b in bundled_channels],
            'perChannel': bundled_channels,
            'arch': arch,
            'training': dict(common_training,
                             perChannelLosses={name: s['epochLosses']
                                               for name, s in per_channel_summaries.items()},
                             perChannelStepLosses={name: s.get('stepLosses', [])
                                                   for name, s in per_channel_summaries.items()},
                             perChannelStepIndices={name: s.get('stepIndices', [])
                                                    for name, s in per_channel_summaries.items()},
                             # Aggregate early-stop signals (any-of / max-of) match the QC sidecar
                             # shape; per-channel dicts let the Detail view annotate per series.
                             stoppedEarly=any(bool(s.get('stoppedEarly', False))
                                              for s in per_channel_summaries.values()),
                             stopEpoch=max((int(s.get('stopEpoch', 0))
                                            for s in per_channel_summaries.values()), default=0),
                             epochBudget=int(epochs),
                             perChannelStoppedEarly={name: bool(s.get('stoppedEarly', False))
                                                     for name, s in per_channel_summaries.items()},
                             perChannelStopEpoch={name: int(s.get('stopEpoch', 0))
                                                  for name, s in per_channel_summaries.items()}),
        }
        top_manifest_path = str(bundle_root / 'manifest.json')
        write_json_atomic(top_manifest_path, top_manifest)
        log.log(f'>> saved {top_manifest_path}')

    # ── QC sidecar ──────────────────────────────────────────────────────────
    # Pooled: one loss curve at __pooled__ + arch + effective mode. PerChannel: one loss curve per
    # channel name. The Julia handler picks a headline number for the badge (see the QC section).
    if qc_out_path:
        # For back-compat with the earlier pooled QC schema, keep the flat `finalLoss`/`lossDrop`
        # keys — they now carry the WORST (highest final loss) across channels in perChannel mode,
        # which is the honest headline number for a bundle whose weakest channel gates its utility.
        worst_final, worst_first, worst_drop, worst_epochs = float('nan'), float('nan'), float('nan'), 0
        headline = None
        for name, s in per_channel_summaries.items():
            fl = s.get('finalLoss', float('nan'))
            if headline is None or (np.isfinite(fl) and (not np.isfinite(worst_final) or fl > worst_final)):
                worst_final = fl
                worst_first = s.get('firstLoss', float('nan'))
                worst_drop  = s.get('lossDrop', float('nan'))
                worst_epochs = len(s.get('epochLosses', [])) or epochs
                headline = name
        write_json_atomic(qc_out_path, {
            'mode': mode,
            'requestedMode': train_mode_req,
            'reason': reason,
            'perChannelSummaries': per_channel_summaries,
            'finalLoss': worst_final,
            'firstLoss': worst_first,
            'lossDrop':  worst_drop,
            'epochLosses': per_channel_summaries[headline]['epochLosses'] if headline else [],
            # Sub-epoch trace of the headline (worst-final) channel — the Julia QC handler doesn't
            # currently thread this into the QC metrics dict (nothing on the metric side needs it),
            # but the FlowTrainingView reads the model's manifest directly, so this is diagnostic-
            # only. Keep the shape so a future QC finding (e.g. "converged at step N") can build
            # on it without another schema bump.
            'stepLosses':  per_channel_summaries[headline].get('stepLosses', []) if headline else [],
            'stepIndices': per_channel_summaries[headline].get('stepIndices', []) if headline else [],
            # Early-stop signals — bucketed as ANY channel stopping early for perChannel bundles,
            # since a plateau on the weakest channel is what we care about; ALL for pooled.
            # `stopEpoch` is the headline channel's stop point (max of `stopEpoch` across channels
            # for perChannel, which is the last channel to plateau).
            'stoppedEarly': any(bool(s.get('stoppedEarly', False))
                                for s in per_channel_summaries.values()),
            'stopEpoch':   max((int(s.get('stopEpoch', 0)) for s in per_channel_summaries.values()),
                               default=0),
            'epochs': worst_epochs,
            'nImages': len(movies),
            'arch': arch,
        })
        log.log(f'>> saved QC stats: {qc_out_path}')

    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
