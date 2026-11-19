"""Find the separate tissues on a slide scan, for `editImages.splitTissues`.

ONE derivation, shared by the run (`app/src/tasks/editImages/splitTissues_run.py`) and the task
preview (`preview/preview_worker.py` → `_preview_split_tissues`), so a preview shows exactly the
tissues a run would cut.

The detection runs on a low-resolution pyramid level (~10 µm/px): tissue outlines are millimetres
across, so full resolution would only cost time. Steps:

1. **Tissue signal** (fluorescence): every picked channel measured above its own glass level,
   scaled to its 99.5th percentile, then averaged — a tissue
   that is dim in DAPI but bright in another marker still counts; a channel with no contrast is left
   out. Z is max-projected.
2. **Foreground**: smoothed, thresholded at a fraction of Otsu, gaps closed, holes filled, specks
   dropped.
3. **Pieces**: an over-seeded watershed on the foreground's distance transform.
4. **Merge pieces into tissues** (`_agglomerate`): touching pieces stay together unless the neck
   between them is narrow compared with both sides (`neck < splitNeck × either half-width` —
   scale-free, so a wide section with a dent stays whole and two sections touching at an edge come
   apart); a piece within `mergeGapUm` of a much larger one joins it (a crumb of a torn section).
5. **Each tissue's mask** = its own foreground, grown by `marginUm` without crossing into a
   neighbour (`expand_labels`). The run zeroes everything outside it, which is what separates
   tissues that interlace: their bounding boxes overlap, their masks do not.

**Limit:** the smoothing and gap closing in step 2 bridge gaps under ~100 µm, so two sections that
lie closer than that along their whole shared edge read as one — the neck test then sees a wide join.
Sections that touch only in places (the usual case) still come apart. A higher threshold tightens the
foreground and opens such gaps.

Tissues are numbered in reading order (rows top to bottom, left to right within a row), so the
preview's numbers are the output images' numbers.
"""
from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np

#: The detection level is the finest one at least this coarse (µm/px). Tissue outlines are mm-scale.
DETECT_TARGET_UM = 10.0
#: Foreground pieces under this area are dust, not tissue (mm²) — before grouping.
SPECK_MM2 = 0.01
#: Smoothing of the tissue signal and gap closing of the foreground (µm).
SMOOTH_UM = 30.0
CLOSE_UM = 50.0


@dataclass
class SplitParams:
    """Detection params, in physical units so they mean the same at any pyramid level."""
    thresholdAdjust: float = 0.3      # share of the way from glass (0) to Otsu; lower picks up fainter tissue
    mergeGapUm: float = 300.0         # pieces closer than this are one tissue
    splitNeck: float = 0.25           # split where the neck is narrower than this × either side
    minAreaMm2: float = 0.1           # tissues smaller than this are dropped
    marginUm: float = 50.0            # mask grown by this much (never into a neighbour)

    @classmethod
    def from_dict(cls, d):
        d = d or {}
        out = cls()
        for k in ('thresholdAdjust', 'mergeGapUm', 'splitNeck', 'minAreaMm2', 'marginUm'):
            if d.get(k) is not None:
                setattr(out, k, float(d[k]))
        return out


@dataclass
class Tissue:
    label: int
    area_mm2: float
    #: half-open box in FULL-resolution pixels: (y0, y1, x0, x1)
    box: tuple
    centroid: tuple = field(default=(0.0, 0.0))

    def as_dict(self):
        y0, y1, x0, x1 = self.box
        return {'label': int(self.label), 'areaMm2': round(float(self.area_mm2), 4),
                'y0': int(y0), 'y1': int(y1), 'x0': int(x0), 'x1': int(x1)}


def detection_level(level_shapes_yx, px_um, target_um=DETECT_TARGET_UM):
    """Index of the finest level whose pixel is at least `target_um`, else the coarsest. Level k's
    pixel is `px_um × (full Y / level-k Y)` — the actual ratio, not an assumed 2^k."""
    full_y = level_shapes_yx[0][0]
    for k, (ny, _) in enumerate(level_shapes_yx):
        if px_um * full_y / max(ny, 1) >= target_um:
            return k
    return len(level_shapes_yx) - 1


#: Width of the slide-edge frame the glass level is read from (fraction of the shorter side).
GLASS_FRAME_FRAC = 0.02
#: A channel whose contrast against the glass (99.5th percentile) is under this many noise SDs of the
#: glass carries no tissue — a blank cycle, a camera offset — and is left out.
MIN_CONTRAST_SD = 8.0


def _glass_frame(shape):
    """Boolean frame along the plane's edge — on a slide scan that is glass, not tissue."""
    w = max(2, int(round(min(shape) * GLASS_FRAME_FRAC)))
    f = np.ones(shape, bool)
    f[w:-w, w:-w] = False
    return f


def tissue_signal(planes):
    """`planes` [C, Y, X] → one [Y, X] float map, 0 on glass, towards 1 on tissue. Fluorescence:
    tissue is brighter than the glass.

    Each channel is measured above its own glass level — the median of the slide-edge frame's pixels
    below the channel's Otsu cut, so a section that runs along the edge cannot pass for glass —
    scaled to its 99.5th percentile, clipped to 1, and averaged over the channels. A channel whose
    contrast is under `MIN_CONTRAST_SD` noise SDs of the edge is left out, so a blank or offset channel
    cannot lift the glass into the foreground (when every channel is, all are used). Pixels the
    scanner skipped (exact zero in every channel, while most of the slide edge is not) are 0."""
    planes = np.asarray(planes, dtype=np.float32)
    out = np.zeros(planes.shape[1:], dtype=np.float32)
    scanned = np.any(planes != 0, axis=0)
    if not scanned.any():
        return out
    edge = _glass_frame(out.shape)
    # Exact zeros are tiles the scanner skipped — unless most of the slide edge is zero, in which case
    # zero is the glass itself (a background-subtracted image) and every pixel is data.
    if scanned[edge].mean() < 0.5:
        scanned = np.ones_like(scanned)
    frame = edge & scanned
    from skimage.filters import threshold_otsu
    scaled, flat = [], []
    for c in planes:
        rim, vals = c[frame], c[scanned]
        # the edge's glass: its pixels below the channel's own Otsu cut, so a section running along the
        # edge is not taken for glass
        # Otsu on values clipped at the 99th percentile: bright specks (dust, precipitate) cannot drag
        # the cut above the tissue
        v = np.minimum(vals, np.percentile(vals, 99))
        cut = float(threshold_otsu(v)) if float(v.max()) > float(v.min()) else float(v.max())
        glass = rim[rim <= cut]
        glass = glass if glass.size else rim
        bg = float(np.median(glass))
        sd = 1.4826 * float(np.median(np.abs(glass - bg)))
        hi = float(np.percentile(vals, 99.5)) - bg
        if hi <= 0:
            continue
        (flat if hi < MIN_CONTRAST_SD * sd else scaled).append(np.clip((c - bg) / hi, 0, 1))
    use = scaled or flat
    for x in use:
        out += x
    out[~scanned] = 0
    return out / max(len(use), 1)


#: A tissue's interior — where its signal level is read for its area — starts this far inside its
#: outline (µm), past the blur of the smoothing.
AREA_INSET_UM = 2 * SMOOTH_UM


def _tissue_areas(own, signal, inset_px):
    """Pixel area per label of `own` — the area INSIDE the tissue's outline, holes (a lumen, a crack)
    included, without the smoothing's blur: the tissue's total signal over its interior's mean level.
    The outline (smoothed, low threshold, gaps closed) reaches past the tissue, and counting the pixels
    above the threshold misses the dark gaps in a textured tissue; the signal sum over the level is
    neither — exact for a solid disc and for speckle alike. Assumes the stain is about as bright at the
    edge as inside: a tissue dimmer at its rim reads small. Capped at the outline's own area; a tissue
    too small to have an interior is its outline."""
    from scipy import ndimage as ndi
    n = int(own.max())
    out = np.zeros(n + 1)
    for i, sl in enumerate(ndi.find_objects(own), start=1):
        if sl is None:
            continue
        m = own[sl] == i
        core = ndi.binary_erosion(m, structure=_disk(inset_px)) if inset_px >= 1 else m
        level = float(signal[sl][core].mean()) if core.any() else 0.0
        out[i] = min(float(signal[sl][m].sum()) / level, m.sum()) if level > 0 else m.sum()
    return out


def _disk(r):
    from skimage.morphology import disk
    return disk(max(int(round(r)), 0))


#: Watershed seeds are distance maxima at least this far apart, before necks are judged (µm).
SEED_SPACING_UM = 200.0
#: A piece within `mergeGapUm` of a neighbour joins it only when it is at most this fraction of the
#: neighbour's area — a crumb of a torn section, not a second section lying close by.
FRAGMENT_FRACTION = 0.25


def _pieces(dist, mask, seed_spacing_px):
    """Over-seeded watershed of `mask` on `-dist`: one seed per distance maximum `seed_spacing_px`
    apart, plus one for any component that has none."""
    from skimage import measure, segmentation
    from skimage.feature import peak_local_max

    comps = measure.label(mask)
    peaks = peak_local_max(dist, min_distance=max(int(seed_spacing_px), 1), labels=comps,
                           exclude_border=False)
    seeds = np.zeros(mask.shape, np.int32)
    seeds[tuple(peaks.T)] = np.arange(1, len(peaks) + 1)
    for p in measure.regionprops(comps):
        sl = p.slice
        if not seeds[sl][p.image].any():
            local = np.where(p.image, dist[sl], -1)
            iy, ix = np.unravel_index(np.argmax(local), local.shape)
            seeds[sl[0].start + iy, sl[1].start + ix] = seeds.max() + 1
    return segmentation.watershed(-dist, seeds, mask=mask)


def _border_pairs(lab, value=None):
    """`{(u, v): max over their 4-neighbour border of min(value)}` for every pair of touching labels
    (`value=None` → 1 for each pair)."""
    out = {}
    for sa, sb in (((slice(None), slice(None, -1)), (slice(None), slice(1, None))),
                   ((slice(None, -1), slice(None)), (slice(1, None), slice(None)))):
        a, b = lab[sa], lab[sb]
        m = (a != b) & (a > 0) & (b > 0)
        if not m.any():
            continue
        u, v = np.minimum(a[m], b[m]), np.maximum(a[m], b[m])
        d = np.ones(len(u)) if value is None else np.minimum(value[sa][m], value[sb][m])
        order = np.lexsort((d, v, u))                      # last per (u, v) = its max d
        u, v, d = u[order], v[order], d[order]
        last = np.r_[(u[1:] != u[:-1]) | (v[1:] != v[:-1]), True]
        for uu, vv, dd in zip(u[last], v[last], d[last]):
            key = (int(uu), int(vv))
            out[key] = max(out.get(key, -1.0), float(dd))
    return out


def _agglomerate(lab, dist, neck_ratio, gap_px, fragment_fraction=FRAGMENT_FRACTION):
    """Merge watershed pieces back into tissues. Two pieces are one tissue when either

    * they touch and the neck between them is not narrow: `saddle >= neck_ratio × min(peak_a,
      peak_b)` — the saddle is the highest distance value on their shared border (the neck's
      half-width), a peak the largest distance in a piece (its half-width); or
    * they lie within `gap_px` of each other and the smaller is at most `fragment_fraction` of the
      larger — a crumb of a torn section.

    Necks first, to completion, so a crumb is judged against whole tissues rather than the pieces
    one was cut into; then crumbs. Greedy, re-judged after every merge. Pure."""
    from skimage import segmentation

    n = int(lab.max())
    if n <= 1:
        return lab
    peak = np.zeros(n + 1)
    np.maximum.at(peak, lab.ravel(), dist.ravel())
    area = np.bincount(lab.ravel(), minlength=n + 1).astype(float)
    saddle = _border_pairs(lab, dist)
    near = _border_pairs(segmentation.expand_labels(lab, gap_px / 2)) if gap_px > 0 else {}
    parent = list(range(n + 1))

    def find(i):
        while parent[i] != i:
            parent[i] = parent[parent[i]]
            i = parent[i]
        return i

    def current(pairs):
        out = {}
        for (u, v), d in pairs.items():
            ru, rv = find(u), find(v)
            if ru != rv:
                key = (min(ru, rv), max(ru, rv))
                out[key] = max(out.get(key, -1.0), d)
        return out

    def merge(ru, rv):
        parent[rv] = ru
        peak[ru] = max(peak[ru], peak[rv])
        area[ru] += area[rv]

    # 1. necks: widest first, until every touching pair left is joined by a narrow neck
    while True:
        best, best_r = None, -1.0
        for (ru, rv), d in current(saddle).items():
            r = d / max(min(peak[ru], peak[rv]), 1e-9)
            if (neck_ratio <= 0 or r >= neck_ratio) and r > best_r:
                best, best_r = (ru, rv), r
        if best is None:
            break
        merge(*best)
    # 2. crumbs: smallest relative size first; a crumb TOUCHING a tissue joins it before a close one
    while True:
        touching = current(saddle)
        best, best_score = None, -1.0
        for (ru, rv) in current(near):
            f = min(area[ru], area[rv]) / max(max(area[ru], area[rv]), 1.0)
            score = (1.0 if (ru, rv) in touching else 0.0) + (1.0 - f)
            if f <= fragment_fraction and score > best_score:
                best, best_score = (ru, rv), score
        if best is None:
            break
        merge(*best)
    root = np.array([find(i) for i in range(n + 1)])
    root[0] = 0
    return root[lab]


def _reading_order(props):
    """Labels sorted into reading order: a tissue whose centroid lies within the vertical extent of
    the row's first tissue joins that row; rows top to bottom, left to right within a row."""
    rest = sorted(props, key=lambda p: p.centroid[0])
    order = []
    while rest:
        head = rest[0]
        y0, y1 = head.bbox[0], head.bbox[2]
        row = [p for p in rest if y0 <= p.centroid[0] < y1]
        order += sorted(row, key=lambda p: p.centroid[1])
        rest = [p for p in rest if p not in row]
    return [p.label for p in order]


def split_tissue_labels(signal, px_um, params: SplitParams):
    """Tissue labels on the detection level: `(labels uint16 [Y, X], areas_mm2 {label: mm²})`,
    numbered 1..N in reading order. 0 = background. Pure — no I/O."""
    from scipy import ndimage as ndi
    from skimage import filters, measure, morphology, segmentation

    px2_mm2 = (px_um / 1000.0) ** 2
    sig = filters.gaussian(signal, SMOOTH_UM / px_um)
    if not np.any(sig > 0):
        return np.zeros(signal.shape, np.uint16), {}
    th = filters.threshold_otsu(sig) * params.thresholdAdjust
    fg = sig > th
    # closed on an edge-padded copy: past the image border is unknown, not glass — closing against a
    # glass border would shave the closing radius off a tissue that runs off the edge
    r = max(int(round(CLOSE_UM / px_um)), 1)
    fg = ndi.binary_closing(np.pad(fg, r, mode='edge'), structure=_disk(CLOSE_UM / px_um))[r:-r, r:-r]
    fg = ndi.binary_fill_holes(fg)
    fg = morphology.remove_small_objects(fg, max_size=max(int(SPECK_MM2 / px2_mm2), 1))
    if not fg.any():
        return np.zeros(signal.shape, np.uint16), {}

    dist = ndi.distance_transform_edt(fg)
    own = _agglomerate(_pieces(dist, fg, SEED_SPACING_UM / px_um), dist,
                       params.splitNeck, params.mergeGapUm / px_um)
    raw_area = _tissue_areas(own, signal, AREA_INSET_UM / px_um) * px2_mm2
    keep = [p for p in measure.regionprops(own) if raw_area[p.label] >= params.minAreaMm2]
    if not keep:
        return np.zeros(signal.shape, np.uint16), {}
    order = _reading_order(keep)
    remap = np.zeros(int(own.max()) + 1, np.uint16)
    areas = {}
    for new, old in enumerate(order, start=1):
        remap[old] = new
    for p in keep:
        areas[int(remap[p.label])] = float(raw_area[p.label])
    own = remap[own]
    if params.marginUm > 0:
        own = segmentation.expand_labels(own, params.marginUm / px_um).astype(np.uint16)
    return own, areas


def tissues_from_labels(labels, areas, factor_yx, full_yx):
    """Per-tissue boxes in full-resolution pixels. `factor_yx` = full pixels per detection pixel."""
    from skimage import measure
    fy, fx = factor_yx
    out = []
    for p in measure.regionprops(labels):
        y0, x0, y1, x1 = p.bbox
        box = (int(np.floor(y0 * fy)), min(int(np.ceil(y1 * fy)), full_yx[0]),
               int(np.floor(x0 * fx)), min(int(np.ceil(x1 * fx)), full_yx[1]))
        out.append(Tissue(int(p.label), areas.get(int(p.label), 0.0), box,
                          (float(p.centroid[0] * fy), float(p.centroid[1] * fx))))
    return sorted(out, key=lambda t: t.label)


def labels_block(labels, factor_yx, ys, xs):
    """The detection labels at full-resolution pixel rows `ys` × columns `xs` (nearest). Any pixel
    grid works — a crop's rows, or every 2^k-th row for pyramid level k."""
    fy, fx = factor_yx
    yi = np.minimum((np.asarray(ys) / fy).astype(np.int64), labels.shape[0] - 1)
    xi = np.minimum((np.asarray(xs) / fx).astype(np.int64), labels.shape[1] - 1)
    return labels[yi][:, xi]


@dataclass
class Detection:
    labels: np.ndarray        # [Y, X] uint16 on the detection level
    level: int
    factor_yx: tuple          # full-res pixels per detection pixel
    full_yx: tuple            # full-res Y, X
    px_um: float              # detection-level pixel size
    tissues: list             # [Tissue]

    def largest_share(self):
        """The largest tissue's area as a share of the whole image — near 1 means the glass was read
        as tissue (the "can't tell" answer, banked as QC), not that one section fills the scan."""
        total = self.full_yx[0] * self.full_yx[1] * (self.px_um / self.factor_yx[0] / 1000.0) ** 2
        return max((t.area_mm2 for t in self.tissues), default=0.0) / total if total > 0 else 0.0


def detect_tissues(levels, dim_utils, px_um_full, params: SplitParams, channels=None):
    """Read the detection level (t = 0, Z max-projected, `channels` or all) and split it.
    `levels` = the image's pyramid as opened by `zarr_utils.open_as_zarr`; `dim_utils` describes
    level 0 (only its axis ORDER is used to read the coarser level)."""
    from cecelia.utils import zarr_utils
    yi, xi = dim_utils.dim_idx('Y'), dim_utils.dim_idx('X')
    shapes = [(int(lv.shape[yi]), int(lv.shape[xi])) for lv in levels]
    k = detection_level(shapes, px_um_full)
    frame = zarr_utils.read_timepoint(levels[k], dim_utils, 0, channels=channels)
    planes = _as_cyx(np.asarray(frame), dim_utils)
    full_yx = shapes[0]
    factor = (full_yx[0] / shapes[k][0], full_yx[1] / shapes[k][1])
    px_um = px_um_full * factor[0]
    labels, areas = split_tissue_labels(tissue_signal(planes), px_um, params)
    return Detection(labels, k, factor, full_yx, px_um,
                     tissues_from_labels(labels, areas, factor, full_yx))


def _as_cyx(frame, dim_utils):
    """One timepoint (T dropped) → [C, Y, X], max-projecting Z."""
    order = [a for a in dim_utils.im_dim_order if a != 'T']
    if 'C' not in order:
        frame = frame[np.newaxis]
        order = ['C'] + order
    if 'Z' in order:
        frame = frame.max(axis=order.index('Z'))
        order = [a for a in order if a != 'Z']
    return np.transpose(frame, [order.index(a) for a in ('C', 'Y', 'X')])
