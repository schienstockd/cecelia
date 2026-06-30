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
import os
# Add app/ to sys.path so `import py.*` resolves correctly.
# __file__ is at app/py/tasks/cleanupImages/cellpose_correct.py → go up 4 levels.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))

import py.utils.zarr_utils as zarr_utils
import py.utils.ome_xml_utils as ome_xml_utils
from py.utils.dim_utils import DimUtils
import py.utils.script_utils as script_utils

from cellpose import denoise
import torch
import ome_types
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
    if torch.cuda.is_available():
        use_gpu    = True
        gpu_device = torch.device('cuda')
    elif hasattr(torch.backends, 'mps') and torch.backends.mps.is_available():
        use_gpu    = True
        gpu_device = torch.device('mps')
    else:
        use_gpu    = False
        gpu_device = None
    log.log(f'>> GPU: {gpu_device if gpu_device else "none (CPU)"}')

    # Load the full resolution level into RAM (or keep as Dask for large images).
    if use_dask:
        output_image = zarr_utils.get_dask_copy(im_dat[0])
    else:
        output_image = zarr_utils.fortify(im_dat[0])

    # Physical pixel size (µm/px); divides the user-supplied µm diameter to get pixels.
    scaling_factor = dim_utils.im_physical_size('x')
    if dim_utils.omexml.images[0].pixels.physical_size_x_unit == ome_types.model.UnitsLength.MILLIMETER:
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
            corrected = dn.eval([im_dat[0][sl]], channels=[0, 0],
                                diameter=diameter / scaling_factor)[0]
            output_image[sl] = ((corrected[..., 0] + 1) / im_max) * im_rescale_factor
            done_slices += 1
            log.progress(done_slices, total_slices)

    log.log(f'>> save corrected image: {im_correction_path}')
    zarr_utils.create_multiscales(
        output_image, im_correction_path,
        dim_utils=dim_utils,
        reference_zarr=im_dat[0],
        nscales=len(im_dat),
    )

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
