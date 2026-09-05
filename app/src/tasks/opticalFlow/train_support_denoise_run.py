"""Train a SUPPORT model on N images from an experimental set.

Self-supervised temporal blind-spot training — predicts the centre frame of an `input_frames`
window from the surrounding frames. The vault stores exactly what inference needs to reconstruct
the network: `<name>.pt` (state_dict) + `<name>.json` (manifest with the `arch` block the
denoise runner reads via `_build_model`). Reference training loop:
`scratchpad/denoise/run_support_v2.py` from the 2026-09-05 evaluation.

Parameter contract (JSON written by Julia):
  movies              - [{uID, imPath}], set-scope
  taskDir             - the run's task dir (log/QC live here)
  modelPath           - absolute `.pt` target in <config_dir>/models/denoiseModels/
  qcOutPath           - JSON with loss curve + arch, read by _support_train_qc_findings
  valueName           - versioned filepath key (usually driftCorrected)
  trainChannel        - 0-based channel index (single channel — SUPPORT is per-channel)
  channelName         - human name, stored in the manifest
  inputFrames         - temporal window (odd; centre is the target)
  patchXY             - spatial patch size
  epochs              - passes over the pooled patches
  batchSize           - patches per gradient step
  learningRate        - Adam lr
  midChannels/depth/blindConvChannels - UNet architecture
  midZOnly            - True = middle Z per movie (matches per-Z inference); False = all Z
"""
import time
from pathlib import Path

import numpy as np
import torch
from torch.utils.data import DataLoader

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.gpu_utils import torch_device, require_free_vram_gb
from cecelia.utils.atomic_io import atomic_path, write_json_atomic

# Free-VRAM budgets per UNet size, measured on RTX 2000 Ada Laptop 8 GB (~5 GB free after other
# processes). These are the peak-training footprints — inference is much smaller and does not
# need the pre-flight check. Keys mirror `_SUPPORT_UNET_SIZES` in train_support_denoise.jl.
_MIN_FREE_VRAM_GB = {'small': 1.5, 'medium': 3.5, 'large': 5.0}

from cecelia.vendor.support import SUPPORT, DatasetSUPPORT, random_transform


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
    float32 [T, Y, X]. All volumes go into ONE pooled list — SUPPORT's dataset treats each patch
    independently, so mixing channels teaches the model a richer noise distribution. Measured on
    fXgbTl 2026-09-05: a pooled model matched (visibly beat) a per-channel specialist on the
    strongest channel and generalised across the pool."""
    vols = []
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
                vols.append(torch.from_numpy(v).float())
            log.log(f'   ch{c}: pooled {len(planes)} Z plane(s) ({v.shape[0]} × {v.shape[1]} × {v.shape[2]})')
    return vols


def run(params):
    log = script_utils.get_logfile_utils(params)

    movies         = list(params.get('movies') or [])
    model_path     = str(params['modelPath'])
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
    vols = _load_training_volumes(movies, value_name, channels, input_frames, mid_z_only, log)
    if not vols:
        log.log('[ERROR] no usable volumes across the set')
        raise SystemExit(1)

    train_ds = DatasetSUPPORT(
        vols,
        patch_size=[input_frames, patch_xy, patch_xy],
        patch_interval=[5, patch_xy // 2, patch_xy // 2],
        load_to_memory=True,
    )
    train_ds.precompute_indices()
    train_dl = DataLoader(train_ds, batch_size=batch_size, shuffle=False,
                          num_workers=0, pin_memory=(device.type == 'cuda'))
    log.log(f'>> {len(train_ds)} patches per epoch')

    # ── model ───────────────────────────────────────────────────────────────
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
    model = SUPPORT(
        in_channels=arch['inputFrames'],
        mid_channels=arch['midChannels'],
        depth=arch['depth'],
        blind_conv_channels=arch['blindConvChannels'],
        one_by_one_channels=arch['oneByOneChannels'],
        last_layer_channels=arch['lastLayerChannels'],
        bs_size=arch['bsSize'],
        bp=arch['bp'],
    ).to(device)
    n_params = sum(p.numel() for p in model.parameters())
    log.log(f'>> model params: {n_params / 1e6:.2f}M')

    optim = torch.optim.Adam(model.parameters(), lr=lr, betas=(0.9, 0.999))
    L1 = torch.nn.L1Loss(); L2 = torch.nn.MSELoss()
    rng = np.random.default_rng(0)

    # ── train ───────────────────────────────────────────────────────────────
    # One tick per BATCH — the operational unit and the same beat as smooth_run's per-z ticks.
    total = epochs * max(1, len(train_dl))
    done = 0
    log.progress(done, total)

    epoch_losses = []
    t0 = time.time()
    for ep in range(epochs):
        model.train()
        train_ds.precompute_indices()
        losses = []
        for noisy_image, _, _ in train_dl:
            noisy_image = noisy_image.to(device)
            noisy_image, _ = random_transform(noisy_image, None, rng, True)
            T = noisy_image.size(1)
            target = noisy_image[:, T // 2, :, :].unsqueeze(1)
            optim.zero_grad()
            out = model(noisy_image)
            loss = 0.5 * L1(out, target) + 0.5 * L2(out, target)
            loss.backward()
            optim.step()
            losses.append(loss.item())
            done += 1
            log.progress(done, total)
        ep_loss = float(np.mean(losses))
        epoch_losses.append(ep_loss)
        log.log(f'   epoch {ep + 1}/{epochs}: loss {ep_loss:.4f}  wall {time.time() - t0:.0f}s')

    # ── save ────────────────────────────────────────────────────────────────
    # `.pt` and manifest are two files; both must land atomically so a picker never sees a half-pair
    # (a .pt without a manifest is a hard error for the denoise runner — see D7 rationale).
    with atomic_path(model_path) as tmp_pt:
        torch.save(model.state_dict(), tmp_pt)

    manifest = {
        'kind': 'denoise-support',
        'channels': channel_names,
        'arch': arch,
        'training': {
            'imageUids': [m['uID'] for m in movies],
            'valueName': value_name,
            'channelIndices': channels,
            'epochs': epochs,
            'batchSize': batch_size,
            'learningRate': lr,
            'midZOnly': mid_z_only,
            'framesPerImage': [int(v.shape[0]) for v in vols],
        },
    }
    manifest_path = str(Path(model_path).with_suffix('.json'))
    write_json_atomic(manifest_path, manifest)
    log.log(f'>> saved {model_path}')
    log.log(f'>> saved {manifest_path}')

    # ── QC sidecar ──────────────────────────────────────────────────────────
    if qc_out_path:
        final = epoch_losses[-1] if epoch_losses else float('nan')
        first = epoch_losses[0] if epoch_losses else float('nan')
        drop = (first / final) if (final and final > 0) else float('nan')
        write_json_atomic(qc_out_path, {
            'finalLoss': final,
            'firstLoss': first,
            'lossDrop':  drop,
            'epochLosses': epoch_losses,
            'epochs': epochs,
            'nImages': len(movies),
            'arch': arch,
        })
        log.log(f'>> saved QC stats: {qc_out_path}')

    log.progress(total, total)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
