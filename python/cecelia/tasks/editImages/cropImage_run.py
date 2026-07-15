"""
Crop image task.

Reads an OME-ZARR image, slices it to a pixel bounding box (X/Y always; Z/T when present — all channels
kept), and writes the cropped region as a NEW OME-ZARR multiscale store. The Julia CropImage handler
registers the output as a new image in the set; this runner only does the zarr read → slice → write.
Ports the "read source zarr, transform, write new zarr" pattern of the correction runners.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the cropped .ome.zarr
  x0,x1    - X pixel bounds (half-open) at FULL resolution
  y0,y1    - Y pixel bounds (half-open)
  z0,z1    - Z pixel bounds (half-open); -1 = keep full Z (2D image / no z crop)
  t0,t1    - T pixel bounds (half-open); -1 = keep full T (no time trim)
"""

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


def crop_slice_tuple(ndim, axis_idx, bounds):
    """Build a slice tuple of length ``ndim`` cropping the given axes to half-open pixel bounds.

    ``bounds`` maps an axis letter ('X'/'Y'/'Z'/'T') → ``(lo, hi)`` in pixels; ``axis_idx`` maps the
    same letters → the array axis index (or None if the image lacks that axis). An axis is left FULL
    (``slice(None)``) when it's absent from ``axis_idx``, its bound is None, ``lo < 0``, or
    ``hi <= lo`` — so channels and any un-cropped axis pass through unchanged. Pure/testable."""
    slices = [slice(None)] * ndim
    for ax, lohi in bounds.items():
        idx = axis_idx.get(ax)
        if idx is None or lohi is None:
            continue
        lo, hi = lohi
        if lo is None or hi is None or lo < 0 or hi <= lo:
            continue
        slices[idx] = slice(int(lo), int(hi))
    return tuple(slices)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    bounds = {
        'X': (int(params['x0']), int(params['x1'])),
        'Y': (int(params['y0']), int(params['y1'])),
        'Z': (int(params.get('z0', -1)), int(params.get('z1', -1))),
        'T': (int(params.get('t0', -1)), int(params.get('t1', -1))),
    }

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    axis_idx = {ax: dim_utils.dim_idx(ax) for ax in ('X', 'Y', 'Z', 'T')}
    slices   = crop_slice_tuple(im_dat[0].ndim, axis_idx, bounds)
    cropped  = im_dat[0][slices]
    log.log(f'>> crop {im_dat[0].shape} -> {cropped.shape}  bounds={bounds}')

    log.progress(1, 3)
    log.log(f'>> write cropped image: {im_out_path}')
    zarr_utils.create_multiscales(
        cropped, im_out_path,
        dim_utils=dim_utils,          # scale/axes are per-pixel — unchanged by cropping
        reference_zarr=im_dat[0],
        nscales=len(im_dat),
    )

    log.progress(2, 3)
    log.log('>> save OME-XML metadata')
    ome_xml_utils.save_meta_in_zarr(
        im_out_path, im_path,
        changed_shape=cropped.shape,   # SizeX/Y/Z/T shrink; PhysicalSize*/TimeIncrement carry over
        dim_utils=dim_utils,
    )

    log.progress(3, 3)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
