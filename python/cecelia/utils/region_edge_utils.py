"""
Where a region's boundary is, from its cells, and how far every cell sits from it.

A region here is a set of cells — the whole tissue (every cell), a region cluster, a gate, aggregated
cells. Its SHAPE is drawn from their centroids alone, never from pixels, so it works on any segmented
image (a split tissue, a crop mid-section, a timecourse) and on any channel panel:

1. the centroids are rasterised on a grid of `step` µm (`close_um / GRID_PER_RADIUS`, at least 1 µm);
2. closed with a disk (2D) or ball (3D) of radius `close_um` — a gap narrower than about twice the
   radius between cells is inside the region, a wider one is not (the erosion is one step smaller, so
   no cell is left behind as a lone pixel). The closing runs as if the image went on past the frame
   (empty there), so a region running off the frame is not eaten back from that side, and a region
   within `FRAME_SNAP_UM` of the frame is extended to it — it runs off the image there;
3. holes are filled when `fill_holes` (default): in a tissue a hole is a tear, not a boundary.

Then, per cell: `inside` (the cell's grid point is in the shape) and `dist` (µm to the boundary, from
whichever side). The frame is never boundary: the inside distance is taken with the frame padded as
region, so a cell next to where the tissue leaves the field of view reads the distance to the nearest
edge that IS visible. Near the frame that is an upper bound — the true edge may lie outside the image.

The grid distance is centre-to-centre; half a step is taken off and floored at 0, so a cell on the
boundary reads ~0 rather than one step. Accuracy is about ±`step`/2 (5 µm grid at the default 75 µm).

`edge_contours` turns a 2D shape into the boundary polylines the viewer draws, cut wherever they run
along the frame.
"""
import numpy as np

#: Grid steps per closing radius. 15 keeps a 75 µm radius on a 5 µm grid.
GRID_PER_RADIUS = 15
#: Finest grid step, µm — below this the grid is finer than any centroid's meaning.
MIN_STEP_UM = 1.0


def grid_step(close_um):
    """Grid step (µm) for a closing radius."""
    return max(MIN_STEP_UM, float(close_um) / GRID_PER_RADIUS)


def _ball(r, ndim):
    grids = np.ogrid[tuple(slice(-r, r + 1) for _ in range(ndim))]
    return sum(g * g for g in grids) <= r * r


def _grid_index(coords_um, step, shape):
    idx = np.floor(np.asarray(coords_um, float) / step).astype(int)
    return tuple(np.clip(idx[:, k], 0, shape[k] - 1) for k in range(len(shape)))


def grid_shape(extent_um, step):
    """Grid shape covering ``extent_um`` (µm per axis, same order as the coordinates)."""
    return tuple(int(np.ceil(float(e) / step)) + 1 for e in extent_um)


#: A region this close to the frame (about a cell: the last centroid never sits ON the frame) runs off
#: the image there, so it is extended to the frame; a wider gap is empty glass and keeps its edge.
FRAME_SNAP_UM = 20.0


def _snap_to_frame(mask, step):
    """``mask`` extended straight to the frame, along each axis, wherever the region comes within
    `FRAME_SNAP_UM` of that side — the strip between a region that runs off the image and the frame.
    Per axis, so a thin stack (every plane near its top or bottom) does not grow in x and y."""
    from scipy import ndimage as ndi
    k = max(1, int(np.ceil(FRAME_SNAP_UM / step)))
    out = mask.copy()
    for ax, n in enumerate(mask.shape):
        line = np.zeros((2 * k + 1,) + (1,) * (mask.ndim - 1), bool); line[:] = True
        line = np.moveaxis(line, 0, ax)
        # only the k grid steps at each end can change, and they see at most 2k steps in: dilate those
        # slabs alone, not the volume (sequential over axes, so corners fill too)
        w = min(n, 2 * k + 1)
        before = out.copy()                       # both ends grow from the same state (they may overlap)
        for lo, hi, keep in ((0, w, slice(0, min(k, n))), (n - w, n, slice(max(0, w - k), w))):
            sl = [slice(None)] * mask.ndim; sl[ax] = slice(lo, hi)
            grown = ndi.binary_dilation(before[tuple(sl)], line)
            dst = [slice(None)] * mask.ndim; dst[ax] = slice(lo + keep.start, lo + keep.stop)
            src = [slice(None)] * mask.ndim; src[ax] = keep
            out[tuple(dst)] |= grown[tuple(src)]
    return out


# Dilation / erosion by `_ball(r)` through a distance transform: the same ball (every grid point within
# r), but in time linear in the grid. A structuring element's cost grows with the ball's volume — on a
# 3D stack at a fine step (~14k voxels) that took tens of minutes for a few thousand cells.
def _dilate(mask, r):
    """``mask`` dilated by `_ball(r)`: every grid point within ``r`` steps of a set one."""
    from scipy import ndimage as ndi
    if not mask.any():
        return mask.copy()
    d = ndi.distance_transform_edt(~mask)
    return d * d <= r * r + 0.5          # squared distances are integers: exact, no float edge


def _erode(mask, r):
    """``mask`` eroded by `_ball(r)`, everything outside the array unset: a point stays when no unset
    point lies within ``r`` steps. One unset layer around the array stands for all of outside (the
    nearest outside point to any inside one lies in it); the distance transform alone ignores outside."""
    from scipy import ndimage as ndi
    d = ndi.distance_transform_edt(np.pad(mask, 1))[tuple(slice(1, -1) for _ in range(mask.ndim))]
    return d * d > r * r + 0.5


def region_mask(coords_um, extent_um, close_um=75.0, fill_holes=True):
    """``(mask, step)``: the region's shape on a grid of ``step`` µm, from its cells' centroids
    (``coords_um``, n × ndim µm, same axis order as ``extent_um``). Empty mask when no cells."""
    from scipy import ndimage as ndi            # lazy: scipy.ndimage import is not free
    step = grid_step(close_um)
    shape = grid_shape(extent_um, step)
    pts = np.zeros(shape, bool)
    coords_um = np.asarray(coords_um, float).reshape(-1, len(shape))
    if len(coords_um) == 0:
        return pts, step
    pts[_grid_index(coords_um, step, shape)] = True
    r = max(1, int(round(close_um / step)))
    # erode one grid step LESS than the dilation: a rasterised disk closing leaves the corner cell of a
    # region as a lone pixel (a separate piece, and a boundary line around it); one step of slack keeps
    # every cell joined to its shape, at the cost of the boundary sitting ≤ one step further out
    # The closing runs on a grid padded past the frame with EMPTY space, wider than the radius: a region
    # cut off by the frame closes as if the image went on (the frame does not erode it), and empty
    # glass between the region and the frame stays empty. Counting the frame as region instead
    # (`border_value=1`) glued any region within ~a radius of the frame to it, erasing that real edge.
    if r > 1:
        pad = r + 1
        closed = _erode(_dilate(np.pad(pts, pad), r), r - 1)
        mask = closed[tuple(slice(pad, pad + n) for n in shape)]
    else:
        mask = _dilate(pts, 1)
    mask |= pts
    mask = _snap_to_frame(mask, step)
    if fill_holes:
        mask = ndi.binary_fill_holes(mask)
    return mask, step


#: A piece of a region's shape holding fewer of its cells than this is a speck — stray cells away from
#: the region's body. Most of a region in specks means its cells are scattered, with no edge to speak of.
SMALL_PIECE_CELLS = 10


def frac_in_small_pieces(mask, step, coords_um):
    """Share of the cells at ``coords_um`` (the region's own) that sit in a piece of ``mask`` holding
    fewer than `SMALL_PIECE_CELLS` of them. 0 for no cells."""
    from scipy import ndimage as ndi
    coords_um = np.asarray(coords_um, float).reshape(-1, mask.ndim)
    if len(coords_um) == 0 or not mask.any():
        return 0.0
    lab = ndi.label(mask)[0][_grid_index(coords_um, step, mask.shape)]
    sizes = np.bincount(lab)
    return float(np.mean(sizes[lab] < SMALL_PIECE_CELLS))


def n_pieces(mask):
    """How many separate pieces the shape falls into — many means the radius is too small for how
    sparse the region's cells are."""
    from scipy import ndimage as ndi
    return int(ndi.label(mask)[1]) if mask.any() else 0


def edge_distances(mask, step, coords_um):
    """``(inside, dist)`` for cells at ``coords_um``: inside the shape (bool) and µm to its boundary
    from either side. ``dist`` is NaN for every cell when the shape has no visible boundary (empty, or
    covering the whole frame)."""
    from scipy import ndimage as ndi
    coords_um = np.asarray(coords_um, float).reshape(-1, mask.ndim)
    idx = _grid_index(coords_um, step, mask.shape)
    inside = mask[idx] if len(coords_um) else np.zeros(0, bool)
    if not mask.any() or mask.all():
        return inside, np.full(len(coords_um), np.nan)
    # inside: distance to the nearest non-region grid point, the frame padded as region
    d_in = ndi.distance_transform_edt(np.pad(mask, 1, constant_values=True), sampling=step)
    d_in = d_in[tuple(slice(1, -1) for _ in range(mask.ndim))]
    # outside: distance to the nearest region grid point (the frame is simply not region)
    d_out = ndi.distance_transform_edt(~mask, sampling=step)
    d = np.where(inside, d_in[idx], d_out[idx])
    return inside, np.maximum(d - step / 2.0, 0.0)


def edge_contours(mask2d, step):
    """The boundary of a 2D shape as polylines, µm ``(y, x)`` each (k × 2), with every stretch that runs
    along the image frame removed — the frame is not boundary. Grid cell ``i`` covers
    ``[i·step, (i+1)·step)``, so a contour at grid coordinate ``c`` is at ``(c + 0.5)·step`` µm. Each line
    is simplified to within half a grid step (Douglas–Peucker, `skimage.measure.approximate_polygon`)."""
    from skimage.measure import find_contours, approximate_polygon   # lazy
    h, w = mask2d.shape
    out = []
    if not mask2d.any():
        return out
    for c in find_contours(np.pad(mask2d, 1, constant_values=False).astype(np.uint8), 0.5):
        c = c - 1.0
        on_frame = (c[:, 0] < 0) | (c[:, 0] > h - 1) | (c[:, 1] < 0) | (c[:, 1] > w - 1)
        start = None
        for i, f in enumerate(np.append(on_frame, True)):
            if not f and start is None:
                start = i
            elif f and start is not None:
                if i - start > 1:
                    out.append((approximate_polygon(c[start:i], tolerance=0.5) + 0.5) * step)
                start = None
    return out
