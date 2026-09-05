"""Denoising task — SUPPORT (algorithm lives in `coastal.support`).

Thin caller — the mirror-padded inference itself is `coastal.support.denoise_stack` (DENOISE_
INTEGRATION_PLAN.md D8 is baked in there, so no caller can forget it). This file owns the IO half:
opening the input OME-ZARR through `zarr_utils`, iterating (channel, z) sub-volumes, and writing
the staged output store. The model architecture is rebuilt from the manifest's `arch` block via
`coastal.support.build_model`; a missing/corrupt manifest is a hard error in the Julia handler.

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

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.gpu_utils import torch_device
from cecelia.utils.atomic_io import write_json_atomic

from coastal.support import denoise_stack


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
    return tuple(sl)


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

    # Load the state_dict once and hand it to `denoise_stack` per (channel, z). The wrapper builds
    # a fresh model per call — cheap (<5M params) compared to inference wall time, and it keeps the
    # `state_dict + arch → denoised_tyx` contract with no shared torch objects across planes.
    log.log(f'>> load model: {model_path}')
    state_dict = torch.load(model_path, map_location=device)
    if isinstance(state_dict, dict) and 'state_dict' in state_dict:
        state_dict = state_dict['state_dict']
    log.log(f'   arch: {arch}')

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
                    denoised = denoise_stack(sub, state_dict, arch,
                                             batch_size=batch_size, device=device)

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
