"""
Cellpose segmentation task entry point.

Called by the Julia cellposeSegment task handler as a subprocess.
Reads params from a JSON file (path passed via --params), runs tiled
cellpose segmentation, and writes labels as multiscale OME-ZARR.

Parameter contract (JSON written by Julia):
  imPath            - absolute path to the input .ome.zarr
  taskDir           - metadata directory ({proj}/1/{uid}/)
  outputValueName   - output subdirectory name (default 'default')
  models            - dict keyed "0","1",…; each entry:
      model           - model type ('cyto3', 'cyto2', 'nuclei', …)
      matchAs         - 'base' or 'nuc'
      cellChannels    - 0-based channel indices for cell signal
      nucChannels     - 0-based indices for nucleus signal (optional)
      cellDiameter    - expected cell diameter in µm
      normalise       - percentile for intensity normalisation (default 99.9)
      medianFilter    - median filter kernel size (0 = off)
      gaussianFilter  - gaussian sigma (0.0 = off)
      threshold       - absolute intensity threshold (0 = off)
      stitchThreshold - cellpose z-stitch threshold (0.0 = per-slice 2D)
  blockSize         - XY tile size in pixels (default 512)
  overlap           - XY tile overlap in pixels (default 64)
  blockSizeZ        - Z tile size in slices (0 = whole stack; Z tiling not yet active)
  overlapZ          - Z tile overlap in slices (future use)
  labelOverlap      - IoU threshold for tile seam stitching (0 = simple max merge)
  matchThreshold    - IoU threshold for nuc/base matching (default 0.3)
  removeUnmatched   - remove base cells with no matching nuc (default false)
  minCellSize       - remove objects smaller than N pixels (0 = off)
  cellSizeMax       - remove objects larger than N pixels (0 = off)
  labelExpansion    - expand labels by N pixels (0 = off)
  labelErosion      - erode labels by N pixels (0 = off)
  clearTouchingBorder - remove cells touching image XY border
  clearDepth        - remove cells touching first/last Z slice (3D only)
  normaliseToWhole  - use lowest-res level for global percentile (default true)
  useDask           - load image as dask (reduces peak RAM)
"""

import sys
import os
# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.cellpose_utils import CellposeUtils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path  = params['imPath']
    use_dask = bool(params.get('useDask', False))

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=use_dask)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> models: {sorted(params.get("models", {}).keys())}')

    cp = CellposeUtils(params, dim_utils)
    log.log(f'>> GPU: {cp.gpu_device if cp.use_gpu else "none (CPU)"}')
    log.log(f'>> tiling: block={cp.block_size} overlap={cp.overlap} normalise_to_whole={cp.normalise_to_whole}')

    cp.predict_from_zarr(im_dat)

    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
