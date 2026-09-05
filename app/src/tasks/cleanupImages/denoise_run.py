"""Denoising task — SUPPORT (vendored under `cecelia.vendor.support`).

Runs a trained SUPPORT model on selected channels, per-Z, with **mirror-padded temporal input** so
every original frame gets a centred prediction (DENOISE_INTEGRATION_PLAN.md D8). Without the pad,
the first and last `input_frames//2` frames come back as zeros because SUPPORT is a temporal
blind-spot model and can only predict *centre* frames.

The model architecture is rebuilt from the manifest (`inputFrames`, `midChannels`, `depth`,
`blindConvChannels`, `bsSize` etc.) rather than inferred from the checkpoint — SUPPORT does not
encode shape in the `.pt` file. A missing/corrupt manifest is a hard error in the Julia handler; if
one slips through, `SUPPORT(...)` errors loud rather than silently building the wrong network.

Parameter contract (JSON written by Julia):
  imPath           - absolute path to input .ome.zarr
  imOutputPath     - absolute path to write the denoised .ome.zarr
  modelPath        - absolute path to the model .pt
  manifest         - the parsed <name>.json manifest (arch + training + imaging + checksum)
  channels         - list of 0-based channel indices to denoise (guaranteed non-empty; already
                     stripped of any saturated channels by the Julia handler)
  channelsSkipped  - channels the saturation gate refused (recorded in QC)
  batchSize        - patches per forward pass
  qcOutPath        - where to persist stats for the Julia QC step
"""
import numpy as np
import torch
from torch.utils.data import DataLoader

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.gpu_utils import torch_device
from cecelia.utils.atomic_io import write_json_atomic

from cecelia.vendor.support import SUPPORT, DatasetSUPPORT_test_stitch


def _axis_len(dim_utils, letter, shape):
    idx = dim_utils.dim_idx(letter)
    return (idx, shape[idx]) if idx is not None else (None, 1)


def _plane_slice(ndim, t_idx, t, c_idx, c, z_idx, z):
    sl = [slice(None)] * ndim
    if t_idx is not None:
        sl[t_idx] = t
    if c_idx is not None:
        sl[c_idx] = c
    if z_idx is not None:
        sl[z_idx] = z
    return tuple(sl)


def _volume_slice(ndim, t_idx, c_idx, c, z_idx, z):
    """Slice tuple selecting one (channel, z) sub-volume with the full T range."""
    sl = [slice(None)] * ndim
    if c_idx is not None:
        sl[c_idx] = c
    if z_idx is not None:
        sl[z_idx] = z
    # t_idx: leave as slice(None) — we want the whole time axis
    return tuple(sl)


def _build_model(manifest, device):
    """Rebuild SUPPORT from the manifest's `arch` block. Manifest keys mirror `SUPPORT(...)` args."""
    arch = manifest.get('arch', {}) or {}
    kwargs = dict(
        in_channels=int(arch.get('inputFrames', 61)),
        mid_channels=list(arch.get('midChannels', [16, 32, 64, 128, 256])),
        depth=int(arch.get('depth', 5)),
        blind_conv_channels=int(arch.get('blindConvChannels', 64)),
        one_by_one_channels=list(arch.get('oneByOneChannels', [32, 16])),
        last_layer_channels=list(arch.get('lastLayerChannels', [64, 32, 16])),
        bs_size=list(arch.get('bsSize', [3, 3])),
        bp=bool(arch.get('bp', False)),
    )
    model = SUPPORT(**kwargs).to(device)
    return model, kwargs


def _mirror_pad_time(arr, pad_t):
    """Mirror-pad the time axis by `pad_t` on both sides so every original frame becomes a centre.

    Skips index 0 / -1 in the reflection so the boundary frame is not duplicated (`arr[1:pad+1]`
    reversed rather than `arr[:pad]` reversed).
    """
    if pad_t <= 0:
        return arr
    head = arr[1:pad_t + 1][::-1]
    tail = arr[-pad_t - 1:-1][::-1]
    return np.concatenate([head, arr, tail], axis=0)


def _denoise_one_z(model, arr_zt, pad_t, patch_xy, batch_size, device):
    """Denoise one (channel, z) sub-volume of shape [T, Y, X] and return same shape."""
    padded = _mirror_pad_time(arr_zt, pad_t)
    padded_t = torch.from_numpy(padded.astype(np.float32)).float()

    input_frames = pad_t * 2 + 1
    test_ds = DatasetSUPPORT_test_stitch(
        padded_t,
        patch_size=[input_frames, patch_xy, patch_xy],
        patch_interval=[1, patch_xy // 2, patch_xy // 2],
        load_to_memory=True,
    )
    test_dl = DataLoader(test_ds, batch_size=batch_size, shuffle=False,
                         num_workers=0, pin_memory=(device.type == 'cuda'))
    denoised = np.zeros(test_ds.noisy_image.shape, dtype=np.float32)

    with torch.no_grad():
        for noisy_image, _, coord in test_dl:
            noisy_image = noisy_image.to(device)
            out = model(noisy_image)
            T = noisy_image.size(1)
            for bi in range(noisy_image.size(0)):
                sw0 = int(coord['stack_start_w'][bi]); sw1 = int(coord['stack_end_w'][bi])
                pw0 = int(coord['patch_start_w'][bi]); pw1 = int(coord['patch_end_w'][bi])
                sh0 = int(coord['stack_start_h'][bi]); sh1 = int(coord['stack_end_h'][bi])
                ph0 = int(coord['patch_start_h'][bi]); ph1 = int(coord['patch_end_h'][bi])
                s0 = int(coord['init_s'][bi])
                denoised[s0 + (T // 2), sh0:sh1, sw0:sw1] = \
                    out[bi].squeeze()[ph0:ph1, pw0:pw1].cpu().numpy()

    # de-normalise using the stitching dataset's own mean/std
    denoised = denoised * test_ds.std_image.item() + test_ds.mean_image.item()
    return denoised[pad_t:pad_t + arr_zt.shape[0]]


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path        = params['imPath']
    out_path       = params['imOutputPath']
    model_path     = params['modelPath']
    manifest       = params.get('manifest', {}) or {}
    channels       = [int(c) for c in (params.get('channels') or [])]
    skipped        = [int(c) for c in (params.get('channelsSkipped') or [])]
    batch_size     = int(params.get('batchSize', 2))

    if not channels:
        log.log('[ERROR] no channels to denoise (all skipped or none selected)')
        raise SystemExit(1)

    arch = manifest.get('arch', {}) or {}
    input_frames = int(arch.get('inputFrames', 61))
    patch_xy     = int(arch.get('patchXY', 128))
    pad_t = input_frames // 2

    _, device = torch_device()
    if device is None:
        device = torch.device('cpu')
    log.log(f'>> device: {device}, input_frames={input_frames}, patch_xy={patch_xy}')

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in = im_dat[0]

    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)

    shape = tuple(level_in.shape)
    t_idx, nt = _axis_len(dim_utils, 'T', shape)
    c_idx, nc = _axis_len(dim_utils, 'C', shape)
    z_idx, nz = _axis_len(dim_utils, 'Z', shape)

    if nt < input_frames:
        log.log(f'[ERROR] movie has {nt} timepoints but the model needs at least {input_frames} '
                f'(training input_frames). Pick a longer clip or retrain a smaller-window model.')
        raise SystemExit(1)

    others = [c for c in range(nc) if c not in channels]
    log.log(f'>> dims {dim_utils.im_dim_order} {shape}')
    log.log(f'>> denoising channels {channels} (skipped {skipped}, pass-through {others})')

    log.log(f'>> load model: {model_path}')
    model, kwargs = _build_model(manifest, device)
    state = torch.load(model_path, map_location=device)
    if isinstance(state, dict) and 'state_dict' in state:
        state = state['state_dict']
    model.load_state_dict(state)
    model.eval()
    log.log(f'   arch: {kwargs}')

    total = nz * len(channels) + 1  # one tick per (channel, z) plus pyramid
    done = 0
    log.progress(done, total)

    per_ch_stats = {}

    with zarr_utils.staged_store(out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)

        dtype_max = np.iinfo(zarr_utils.native_dtype(level_in.dtype)).max \
            if np.issubdtype(level_in.dtype, np.integer) else None

        for c in channels:
            in_min, in_max = np.inf, -np.inf
            out_min, out_max = np.inf, -np.inf
            for z in range(nz):
                # [T, Y, X] for the current (c, z)
                sub = np.asarray(
                    level_in[_volume_slice(len(shape), t_idx, c_idx, c, z_idx, z)],
                    dtype=np.float32)
                if sub.ndim == 2:
                    # static image (no T) — nothing to do for a temporal denoiser
                    log.log(f'[WARN] no T axis on this image; passing channel {c} z {z} through')
                    denoised = sub
                else:
                    denoised = _denoise_one_z(model, sub, pad_t, patch_xy, batch_size, device)

                in_min = min(in_min, float(sub.min())); in_max = max(in_max, float(sub.max()))
                out_min = min(out_min, float(denoised.min())); out_max = max(out_max, float(denoised.max()))

                # write per (t, c, z) back into level0
                if dtype_max is not None:
                    denoised = np.clip(np.rint(denoised), 0, dtype_max)
                for ti in range(nt):
                    level0[_plane_slice(len(shape), t_idx, ti, c_idx, c, z_idx, z)] = \
                        denoised[ti].astype(level0.dtype)

                done += 1
                log.progress(done, total)
                log.log(f'   c {c} z {z + 1}/{nz}')

            per_ch_stats[str(c)] = dict(inMin=in_min, inMax=in_max,
                                        outMin=out_min, outMax=out_max)

        # Pass-through channels unchanged. Streamed per (t, c, z) so peak memory stays bounded.
        for c in others:
            for z in range(nz):
                for ti in range(nt):
                    sl = _plane_slice(len(shape), t_idx, ti, c_idx, c, z_idx, z)
                    level0[sl] = level_in[sl]

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(staging, im_path, changed_shape=shape, dim_utils=dim_utils)
        zarr_utils.write_calibration(staging, dim_utils)
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log('   carried the source valid box forward')

    stats = {
        'channelsRun': channels,
        'channelsSkipped': skipped,
        'inputFrames': input_frames,
        'perChannelMinMax': per_ch_stats,
        'shape': [int(x) for x in shape],
        'model': model_path,
    }

    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        write_json_atomic(qc_out_path, stats)
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
