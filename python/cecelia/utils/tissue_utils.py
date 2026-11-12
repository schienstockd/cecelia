"""
Where the tissue is on a slide: a coarse mask, from a pyramid level, so statistics and checks can be
taken inside the tissue rather than over the glass around it.

A whole-slide scan is mostly glass — on the PhenoCycler run this was built on, the sections cover
about a quarter of the scan — so a statistic over the whole plane (a patch grid, a median, a
registration metric) mostly describes the slide, not the sample. The mask is computed on a coarse
level (`coarse_level`), never on level 0: gigapixel planes, and a tissue boundary does not need
cellular resolution.

The threshold is Otsu's on the log-scaled, smoothed mean of the channels, over the pixels the scanner
imaged (it writes exact zeros where it skipped a tile): inside the scanned tiles glass and tissue are
two comparable classes. The log keeps bright nuclei from setting the scale.

Otsu N (1979) *A threshold selection method from gray-level histograms*, IEEE Trans Syst Man Cybern
9(1):62-66, doi:10.1109/TSMC.1979.4310076.
"""
import numpy as np


#: The finest pyramid level at most this many pixels per plane is the one the mask is computed on.
TISSUE_LEVEL_MAX_PX = 4 * 1024 * 1024
#: Gaussian smoothing (pixels at the mask's level) before thresholding: joins the nuclei of one
#: section into one region without bridging the gap between sections.
TISSUE_SMOOTH_PX = 2.0
#: Regions smaller than this fraction of the plane are dropped (debris, dust, a bright speck).
TISSUE_MIN_REGION_FRAC = 1e-4
#: Bins of the log-intensity histogram the threshold is read from.
TISSUE_HIST_BINS = 256


def coarse_level(level_shapes, max_px=None):
    """Index of the finest level whose plane (last two axes) has at most ``max_px`` pixels; the
    coarsest level when none is that small. ``level_shapes`` is level 0 first; ``max_px`` defaults to
    `TISSUE_LEVEL_MAX_PX`."""
    max_px = TISSUE_LEVEL_MAX_PX if max_px is None else max_px
    for i, shape in enumerate(level_shapes):
        if int(shape[-2]) * int(shape[-1]) <= max_px:
            return i
    return len(level_shapes) - 1


def level_factor(level0_shape, level_shape):
    """``(fy, fx)``: level-0 pixels per pixel of ``level_shape`` (2 per level in an NGFF pyramid, but
    read from the shapes so an odd edge does not drift)."""
    return (level0_shape[-2] / level_shape[-2], level0_shape[-1] / level_shape[-1])


def tissue_mask(planes):
    """Boolean tissue mask for one or more 2-D planes of the same shape (the channels of one coarse
    level). Each channel is log-scaled against its own 99.5th percentile of non-zero pixels, so a dim
    channel counts as much as a bright one; their mean is smoothed, thresholded (Otsu over the
    scanned pixels), closed, hole-filled and cleared of regions under `TISSUE_MIN_REGION_FRAC` of the
    plane.
    All False when the planes are empty."""
    from scipy import ndimage as ndi            # lazy: scipy.ndimage import is not free
    planes = [np.asarray(p, dtype=np.float32) for p in (planes if isinstance(planes, (list, tuple)) else [planes])]
    acc = np.zeros(planes[0].shape, dtype=np.float32)
    for p in planes:
        nz = p[p > 0]
        if nz.size == 0:
            continue
        top = float(np.percentile(nz, 99.5))
        if top <= 0:
            continue
        acc += np.log1p(np.clip(p, 0, top) / top * 1000.0)
    acc /= max(1, len(planes))
    if not acc.any():
        return np.zeros(acc.shape, dtype=bool)
    from skimage.filters import threshold_otsu   # lazy: skimage import is not free
    smooth = ndi.gaussian_filter(acc, TISSUE_SMOOTH_PX)
    # only what the scanner imaged: a slide scanner writes exact zeros where it skipped a tile with
    # no tissue, and counting those makes the cut separate scanned from unscanned, not tissue from glass
    scanned = ndi.binary_erosion(acc > 0, iterations=int(np.ceil(2 * TISSUE_SMOOTH_PX)))
    vals = smooth[scanned] if scanned.any() else smooth.ravel()
    if float(vals.max()) <= float(vals.min()):
        return np.zeros(acc.shape, dtype=bool)
    cut = float(threshold_otsu(vals, nbins=TISSUE_HIST_BINS))
    mask = (smooth > cut) & (acc > 0)
    mask = ndi.binary_closing(mask, structure=np.ones((3, 3), bool), iterations=2)
    mask = ndi.binary_fill_holes(mask)
    labels, n = ndi.label(mask)
    if n:
        sizes = np.bincount(labels.ravel())
        keep = sizes >= max(4, TISSUE_MIN_REGION_FRAC * mask.size)
        keep[0] = False
        mask = keep[labels]
    return mask


def level_tissue(levels, channels=None, max_px=None, lead=None):
    """``(mask, level_index, (fy, fx))`` for an opened multiscale image (``levels`` level 0 first,
    each indexable as ``[..., y, x]``). ``lead`` is the index tuple for the axes before Y/X with the
    channel position given as ``None`` (e.g. ``(0, None, 0)`` for t,c,z,y,x at t=0, z=0); ``channels``
    the channel indices to combine (all when None). The planes are read at the coarse level only."""
    shapes = [tuple(lv.shape) for lv in levels]
    li = coarse_level(shapes, max_px)
    lv = levels[li]
    if lead is None:
        lead = tuple([0] * (lv.ndim - 2))
    c_pos = next((i for i, v in enumerate(lead) if v is None), None)
    n_c = lv.shape[c_pos] if c_pos is not None else 1
    chans = range(n_c) if channels is None else channels
    planes = []
    for c in chans:
        sl = list(lead)
        if c_pos is not None:
            sl[c_pos] = int(c)
        planes.append(np.asarray(lv[tuple(sl)]))
    return tissue_mask(planes), li, level_factor(shapes[0], shapes[li])
