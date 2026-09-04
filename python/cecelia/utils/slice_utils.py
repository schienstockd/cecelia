"""
Spatial slicing utilities for tiled and multiscale image processing.

Provides functions to generate numpy slice tuples for:
  - Tiled 2-D / 3-D processing with configurable block size and overlap.
  - Time-series expansion of per-frame slice lists.
  - Multiscale downsampling slices (power-of-two strides in X and Y).

Used by zarr_utils.create_multiscales and downstream segmentation tasks.
"""

import math
import numpy as np

"""
Convert coords to slices
"""

"""
Create slices from image dimensions
"""

"""
Create slices from image dimensions (3D time)
"""

"""
Create slices from image dimensions (2D time)
"""
    
"""
Combine time and frame slices
"""

"""
Create slices from image dimensions (3D)
"""

"""
Create slices from image dimensions (2D)
"""

"""
Create multiscale slices
"""
def create_slices_multiscales(im_dim, dim_utils = None,
                              x_idx = None, y_idx = None,
                              nscales = 1, ignore_channel = False,
                              squeeze = False, idx_adjust = 0):
  # TODO this will only create multiscales for X and Y
  # Would you want to have scaling in Z?
  
  # create slices
  slices = [[slice(None) for _ in range(len(im_dim))] for _ in range(nscales)]
  
  # X and Y have to be defined if no DimUtils given
  if dim_utils is not None:
    # get idx
    y_idx = dim_utils.dim_idx('Y', ignore_channel = ignore_channel, squeeze = squeeze) + idx_adjust
    x_idx = dim_utils.dim_idx('X', ignore_channel = ignore_channel, squeeze = squeeze) + idx_adjust
  
  # get max
  y_max = im_dim[y_idx]
  x_max = im_dim[x_idx]
  
  # create scaled slices
  for i, x in enumerate(slices):
    slices[i][x_idx] = slice(0, x_max, 2**(i+1))
    slices[i][y_idx] = slice(0, y_max, 2**(i+1))
  
  # convert to tuples
  
  return [tuple(x) for x in slices]


def preview_region_bounds(xy_bounds, z_index, t_index, axis_len, ndisplay=2):
    """Decide the region a task preview computes: the visible XY box, ONE z-plane, one timepoint.

    Returns ``(bounds, fallback_2d)`` where `bounds` is the axis→(lo, hi) mapping
    ``crop_slice_tuple`` consumes, so this only makes the DECISION and the existing helper builds the
    slices. Pure — the napari coupling (reading `corner_pixels` at `data_level` and scaling it to
    level 0 by `downsample_factors`) stays in the bridge, which is the only place a viewer exists.

    ``xy_bounds``: {'X': (lo, hi), 'Y': (lo, hi)} in LEVEL-0 pixels, as the viewer reports them.
    ``axis_len``: axis letter → that axis's length, used to clamp (a zoomed-out view reports corners
    beyond the image edge, and a slice past the end would silently return fewer pixels than asked).
    ``ndisplay``: the legacy viewer's display mode. **3 means the viewer is in 3D and we preview one plane
    anyway**, returning ``fallback_2d=True`` so the caller can say so — a whole z-stack costs ~90 s
    with no available shortcut (see docs/todo/TASK_PREVIEW_PLAN.md, Decisions 2 and 8), which is not a
    preview. Per-plane inference is identical to the real run; what a single plane cannot show is
    z-stitching, so counts and z-extents differ.
    """
    bounds = {}
    for ax in ('X', 'Y'):
        lohi = (xy_bounds or {}).get(ax)
        if lohi is None:
            continue
        lo, hi = int(lohi[0]), int(lohi[1])
        limit = axis_len.get(ax)
        lo = max(0, lo)
        if limit is not None:
            hi = min(int(limit), hi)
        if hi > lo:
            bounds[ax] = (lo, hi)

    # exactly one plane / one timepoint — never a range, whatever the viewer is displaying
    for ax, idx in (('Z', z_index), ('T', t_index)):
        if idx is None:
            continue
        limit = axis_len.get(ax)
        if limit is None:
            continue
        i = min(max(0, int(idx)), int(limit) - 1)
        bounds[ax] = (i, i + 1)

    return bounds, bool(ndisplay == 3 and axis_len.get('Z', 1) > 1)


def crop_slice_tuple(ndim, axis_idx, bounds):
    """Build a slice tuple of length ``ndim`` cropping the given axes to half-open pixel bounds.

    ``bounds`` maps an axis letter ('X'/'Y'/'Z'/'T') → ``(lo, hi)`` in pixels; ``axis_idx`` maps the
    same letters → the array axis index (or None if the image lacks that axis). An axis is left FULL
    (``slice(None)``) when it's absent from ``axis_idx``, its bound is None, ``lo < 0``, or
    ``hi <= lo`` — so channels and any un-cropped axis pass through unchanged. Pure/testable.
    (Used by the editImages/cropImage task runner; kept here so the pure logic stays in the IO
    library and remains unit-testable independent of the run-by-path task script.)"""
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
