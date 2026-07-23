"""
Cellpose denoising / deblurring / upsampling correction.

Reads an OME-ZARR image, applies one or more Cellpose DenoiseModel variants
to specified channels, and writes the corrected image as a new OME-ZARR
multiscale store.  Called by the Julia cellposeCorrect task handler.

Parameter contract (JSON written by Julia):
  imPath            - absolute path to the input .ome.zarr
  imCorrectionPath  - absolute path to write the corrected .ome.zarr
  models            - dict keyed by string index ("0", "1", …), each entry:
      model           - DenoiseModel type string or "NONE" to skip
      modelChannels   - list of 0-based integer channel indices
      modelDiameter   - expected cell diameter in pixels (at native resolution)
  useGPU            - bool; use MPS (Apple Silicon) if available
  useDask           - bool; load image as Dask array (reduces peak RAM)
"""

import sys
# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.gpu_utils import torch_device

from cellpose import denoise
import numpy as np


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path            = script_utils.get_param(params, 'imPath',           default=None)
    models             = script_utils.get_param(params, 'models',           default=dict())
    use_dask           = script_utils.get_param(params, 'useDask',          default=False)
    im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default=None)

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=use_dask)

    omexml     = ome_xml_utils.parse_meta(im_path)
    dim_utils  = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> models: {models}')

    # Auto-detect GPU: prefer CUDA, fall back to MPS (Apple Silicon), then CPU.
    use_gpu, gpu_device = torch_device()
    log.log(f'>> GPU: {gpu_device if gpu_device else "none (CPU)"}')

    # Stream the output to disk instead of holding the whole level 0 in RAM (was
    # `fortify(im_dat[0])` — a full T×C×Z×Y×X copy, the OOM on large time-lapses). Create the
    # on-disk level 0, copy the input through one timepoint at a time (so uncorrected channels /
    # frames carry over unchanged), then overwrite only the corrected planes in place below.
    out_dtype = im_dat[0].dtype
    group, output_image, pchunks = zarr_utils.open_multiscales_for_writing(
        im_correction_path, im_dat[0].shape, out_dtype, dim_utils, nscales=len(im_dat))
    zarr_utils.copy_stream(output_image, im_dat[0], dim_utils)

    # Physical pixel size (µm/px); divides the user-supplied µm diameter to get pixels.
    # Normalise to µm — DimUtils reports the stored unit ('um' after µ→u, 'mm', …).
    scaling_factor = dim_utils.im_physical_size('x')
    if dim_utils.im_physical_unit('x') == 'mm':
        scaling_factor *= 1000

    # Rescale factor to convert the normalised cellpose output back to the
    # original bit depth range.
    im_rescale_factor = np.iinfo(im_dat[0].dtype).max

    # Pre-compute slices for all active model entries so we know the total upfront.
    model_work = []
    for i, x in models.items():
        if x['model'] == 'NONE':
            continue
        slices = [
            dim_utils.create_channel_slices(channel=int(ch))
            for ch in x['modelChannels']
        ]
        if dim_utils.is_3D():
            slices = dim_utils.expand_slices([list(s) for s in slices], dim='Z')
        if dim_utils.is_timeseries():
            slices = dim_utils.expand_slices([list(s) for s in slices], dim='T')
        model_work.append((i, x, slices))

    total_slices = sum(len(slices) for _, _, slices in model_work)
    done_slices  = 0
    log.progress(0, total_slices)

    im_max = 10 + 1   # cellpose denoise output range is [-1, 10]

    for i, x, slices in model_work:
        model_type = x['model']
        log.log(f'> Entry {i}: model={model_type}, slices={len(slices)}')

        dn       = denoise.DenoiseModel(model_type=model_type, gpu=use_gpu, device=gpu_device)
        diameter = x['modelDiameter']

        for sl in slices:
            corrected = dn.eval([zarr_utils.fortify(im_dat[0][sl])], channels=[0, 0],
                                diameter=diameter / scaling_factor)[0]
            output_image[sl] = (((corrected[..., 0] + 1) / im_max) * im_rescale_factor).astype(out_dtype)
            done_slices += 1
            log.progress(done_slices, total_slices)

    log.log(f'>> build pyramid + save: {im_correction_path}')
    zarr_utils.write_multiscale_pyramid(group, output_image, dim_utils, len(im_dat), list(pchunks))

    log.log('>> save OME-XML metadata')
    ome_xml_utils.save_meta_in_zarr(
        im_correction_path, im_path,
        dim_utils=dim_utils,
    )

    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
