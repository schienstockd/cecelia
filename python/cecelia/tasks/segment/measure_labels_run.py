"""
Measure labels task entry point.

Called by the Julia measureLabels task handler as a subprocess.
Reads label zarrs and the intensity image, computes per-cell morphology and
intensity properties, and writes an AnnData .h5ad file.

Parameter contract (JSON written by Julia):
  imPath            - absolute path to the intensity image .ome.zarr
  taskDir           - metadata directory ({proj}/1/{uid}/)
  outputValueName   - label set name (default 'default')
  labelDir          - absolute path to {task_dir}/labels/
  labelFiles        - list of zarr filenames to load, e.g. ['default.zarr', 'default_nuc.zarr']
  intensityMeasure  - 'mean' or 'median'
  gaussianFilter    - Gaussian sigma for intensity pre-smooth (0 = off)
  extendedMeasures  - bool: trimesh 3D shape descriptors (3D images only)
  saveMeshes        - bool: save per-cell .stl meshes to {task_dir}/meshes/
  blockSize         - XY tile size (informational; measurement is per-timepoint)
  overlap           - XY tile overlap (informational)
  blockSizeZ        - Z tile size (informational)
  overlapZ          - Z tile overlap (informational)
"""

import sys
import os
# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.measure_utils import MeasureUtils


def _label_type_from_filename(filename: str, output_value_name: str) -> str:
    """
    Derive the label type from the zarr filename.
    '{outputValueName}.zarr'       → 'base'
    '{outputValueName}_{type}.zarr' → type (e.g. 'nuc')
    """
    stem = filename.replace('.zarr', '')
    prefix = f'{output_value_name}_'
    if stem == output_value_name:
        return 'base'
    if stem.startswith(prefix):
        return stem[len(prefix):]
    return stem


def run(params: dict):
    log = script_utils.get_logfile_utils(params)

    im_path    = params['imPath']
    label_dir  = params['labelDir']
    label_files = params['labelFiles']
    out_vn     = params.get('outputValueName', 'default')
    use_dask   = bool(params.get('useDask', False))

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=use_dask)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    # Open each label zarr and map to its label type
    label_zarrs: dict = {}
    for fname in label_files:
        fpath = os.path.join(label_dir, fname)
        if not os.path.exists(fpath):
            log.log(f'[WARN] Label file not found, skipping: {fpath}')
            continue
        ltype = _label_type_from_filename(fname, out_vn)
        try:
            # open labels through the shared reader (same as the image above) — a list of
            # multiscale levels; measure_from_zarr takes level 0 (full-res). NOT hand-rolled zarr.
            label_zarrs[ltype], _ = zarr_utils.open_as_zarr(fpath, as_dask=use_dask)
            log.log(f'>> opened label "{ltype}": {fpath}')
        except Exception as e:
            log.log(f'[WARN] Could not open {fpath}: {e}')

    if 'base' not in label_zarrs:
        log.log('[ERROR] No base label file found — cannot measure')
        raise SystemExit(1)

    mu = MeasureUtils(params, dim_utils)
    out_path = mu.measure_from_zarr(label_zarrs, im_dat, log)

    if out_path is None:
        log.log('[ERROR] Measurement returned no output')
        raise SystemExit(1)

    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
