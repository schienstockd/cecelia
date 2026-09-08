"""
Label correction task entry point.

Called by the Julia SegmentCorrect handler as a subprocess. Reads the labels zarr, applies the
op list in order per touched frame (label.merge / label.remove), stages the rewritten store via
`zarr_utils.staged_store` + `store_compressor('labels')`, and writes a small result blob for the
Julia side to bank for QC and journalling.

Parameter contract (JSON written by Julia):
  taskDir     - metadata directory ({proj}/1/{uid}/)
  imPath      - absolute path to the intensity image .ome.zarr (used only for dim_utils axes)
  labelsPath  - absolute path to the labels zarr to rewrite ({taskDir}/labels/{vn}.zarr)
  valueName   - label set name (informational; the file is identified by labelsPath)
  ops         - list of ops, each `{op: 'label.merge'|'label.remove', t: int, ids: [int, ...],
                                    into?: int}` — pre-validated Julia-side
  resultFile  - absolute path to write `{perOpPixels, nLabelsBefore, nLabelsAfter}` for Julia

WHY OP-BY-OP INSTEAD OF THE FOLDED REWRITE. Julia's `build_rewrite` folds ops within a frame into a
single {src → tgt} table (one pass per frame). We choose the OP-BY-OP path here because the per-op
pixel count is what feeds the journal, and folding it away means the two ops that touched a shared
pixel report an arbitrary split. Op-by-op is one array pass per op which is O(nOps) — fine for
correction (a session queues 5–50 ops, not 5000).

WHY SINGLE-LEVEL ONLY. Decision 2b: every label store on the dev machine today is single-level
(`nscales` follows the image pyramid, `segmentation_utils.py:193`), and multiscale label edits need
a paired downsample per touched frame. Multi-level correction is a `SEG_QUALITY_PLAN.md` extension,
not a correction one — this runner errors on a multi-level input rather than silently rewriting
level 0 and leaving stale downsamples that a viewer might still read.
"""

import json
import os

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.dim_utils import DimUtils


def _t_axis(dim_utils, level_shape):
    """The index of the T axis in the labels array, or None for a still image."""
    order = list(dim_utils.im_dim_order)
    if 'T' not in order:
        return None
    # The labels array may have fewer axes than the intensity image (no C axis). Find T by walking
    # the intensity order and skipping any axis whose size disagrees with the labels level — the
    # labels' T size must match, so it's the first order-consistent slot.
    # In practice labels are (T, Z, Y, X) or (Z, Y, X); the T index in the labels array is 0 iff
    # the leading axis size equals the intensity's nT.
    t_idx = order.index('T')
    # Labels-store layout differs from the intensity by dropping C — reduce t_idx accordingly.
    if 'C' in order and order.index('C') < t_idx:
        t_idx -= 1
    if t_idx >= len(level_shape):
        return None
    return t_idx


def _unique_labels(arr):
    """Set of non-zero ids present in `arr` (background 0 excluded). Small compared to arr size."""
    u = np.unique(arr)
    return set(int(x) for x in u if int(x) != 0)


def _bresenham(x0, y0, x1, y1):
    """Yield the (y, x) integer pixels along the line from (x0,y0) to (x1,y1), inclusive.

    Bresenham's algorithm — vendored (no skimage dep here so the correction runner stays lean
    and portable to a Windows install that hasn't linked skimage's Cython). O(max(|dx|,|dy|))
    per segment; N-vertex polyline is O(sum of segment lengths). Reference: Bresenham J.E.,
    "Algorithm for computer control of a digital plotter" (IBM Systems J., 1965).
    """
    x0, y0, x1, y1 = int(x0), int(y0), int(x1), int(y1)
    dx = abs(x1 - x0); sx = 1 if x0 < x1 else -1
    dy = -abs(y1 - y0); sy = 1 if y0 < y1 else -1
    err = dx + dy
    x, y = x0, y0
    while True:
        yield (y, x)
        if x == x1 and y == y1:
            break
        e2 = 2 * err
        if e2 >= dy:
            err += dy
            x += sx
        if e2 <= dx:
            err += dx
            y += sy


def _rasterise_polyline(xs, ys, shape):
    """Rasterise a polyline (`xs`, `ys` parallel int arrays) as a boolean mask of shape
    `shape=(H, W)`. Out-of-bounds pixels are skipped. `len(xs) == len(ys) >= 2` (validated by
    the Julia side)."""
    H, W = int(shape[0]), int(shape[1])
    mask = np.zeros((H, W), dtype=bool)
    for k in range(len(xs) - 1):
        for (y, x) in _bresenham(xs[k], ys[k], xs[k + 1], ys[k + 1]):
            if 0 <= y < H and 0 <= x < W:
                mask[y, x] = True
    return mask


def _apply_split_inplace(frame, op, log):
    """Cut label `id` at frame `t` along the polyline (xs, ys). Returns pixels reassigned.

    The polyline is rasterised as a 1-pixel-wide cut line, subtracted from the label's mask, and
    the remainder is split by connected components. The largest component keeps the original id;
    every other component gets a fresh id (max_id_in_frame + 1, +2, …). A cut that fails to
    divide the label (still one component) is a no-op — logged, pixels=0. This is the RIGHT
    failure mode for a user-drawn cut: a shallow line that clipped the label's edge should not
    silently mangle it.

    The cut pixels themselves are assigned to the LARGEST fragment (they were part of the label
    before, and the largest fragment inherits the id), so nothing goes to background.
    """
    from scipy.ndimage import label as cc_label
    id_ = int(op['id'])
    xs = [int(x) for x in op['xs']]
    ys = [int(y) for y in op['ys']]
    frame2d = frame  # frame is (Y, X); a 3D (Z, Y, X) input would need a plane index — see docstring
    if frame2d.ndim != 2:
        # 3D labels (Z, Y, X) or 4D (T, Z, Y, X) at a single T: split needs a plane. Defer to
        # follow-up — surface, don't crash. The MVP two-click Split ships against 2D+T stacks.
        log.log(f'[WARN] label.split on a {frame2d.ndim}D frame is not supported yet — skipping.')
        return 0
    mask = (frame2d == id_)
    if not mask.any():
        log.log(f'[WARN] label.split: label {id_} not present at this frame — no-op.')
        return 0
    cut = _rasterise_polyline(xs, ys, frame2d.shape)
    remainder = mask & ~cut
    labeled, n = cc_label(remainder)
    if n <= 1:
        log.log(f'[WARN] label.split: cut did not divide label {id_} (n={n} component(s)) — no-op.')
        return 0
    # Component pixel counts (skip background 0). Largest keeps the id; smaller get fresh ids.
    sizes = np.bincount(labeled.ravel())
    ordering = sorted(range(1, n + 1), key=lambda k: -int(sizes[k]))
    frame_max = int(frame2d.max())
    next_id = frame_max + 1
    reassigned = 0
    # Smaller fragments get fresh ids first, before the cut pixels are explicitly re-stamped —
    # order matters: if the cut re-stamp ran first, a small fragment sitting adjacent to the cut
    # line would still be overwritten in the loop below (it operates on `labeled`, not `frame2d`),
    # so the order is safe either way, but assigning cut LAST reads more like "the largest wins".
    for k in ordering[1:]:
        new_mask = (labeled == k)
        n_pix = int(new_mask.sum())
        frame2d[new_mask] = next_id
        reassigned += n_pix
        log.log(f'>> split label {id_} → new label {next_id} ({n_pix} px)')
        next_id += 1
    # Cut pixels: explicitly re-stamped to id_ (the largest fragment inherits the id). Today they
    # already hold id_ because we never zero the mask before the fragment loop — but that's an
    # invariant we shouldn't rely on. A future reorder (e.g. zeroing the mask first, then filling
    # by component) would silently orphan the cut without this line.
    frame2d[cut & mask] = id_
    return reassigned


def _apply_op_inplace(frame, op, log):
    """Apply one op to `frame` in place; return the number of pixels rewritten by this op.

    The count is measured AT THE TIME the op fires — a merge queued after another merge that
    already moved some pixels reports the pixels IT sees, not the historical ones. That is the
    right semantics for the journal: the journal is a replay script, and each entry describes
    what its op moves at its point in the sequence.
    """
    kind = op['op']
    if kind == 'label.split':
        return _apply_split_inplace(frame, op, log)
    ids = [int(x) for x in op['ids']]
    if kind == 'label.merge':
        into = int(op['into'])
        n = 0
        for src in ids:
            if src == into:
                continue
            mask = (frame == src)
            n += int(mask.sum())
            frame[mask] = into
        return n
    if kind == 'label.remove':
        n = 0
        for src in ids:
            mask = (frame == src)
            n += int(mask.sum())
            frame[mask] = 0
        return n
    raise ValueError(f'unknown label op: {kind!r}')


def run(params: dict):
    log = script_utils.get_logfile_utils(params)

    labels_path = params['labelsPath']
    im_path     = params['imPath']
    ops         = list(params.get('ops', []))
    result_file = params['resultFile']

    if not ops:
        log.log('[INFO] No ops — nothing to do.')
        write_json_atomic(result_file,
                          {'perOpPixels': [], 'nLabelsBefore': 0, 'nLabelsAfter': 0})
        return

    # ── Open source and derive axes ─────────────────────────────────────────
    log.log(f'>> open labels: {labels_path}')
    src_levels, _ = zarr_utils.open_as_zarr(labels_path, as_dask=False)
    if len(src_levels) != 1:
        raise RuntimeError(
            f'labels store has {len(src_levels)} levels — this runner only handles single-level '
            f'stores (Decision 2b). Multi-level correction is a SEG_QUALITY_PLAN.md extension.')
    src = src_levels[0]
    log.log(f'>> shape: {tuple(src.shape)}  dtype: {src.dtype}')

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(src.shape)   # calibration from the intensity image, dims from labels
    t_idx = _t_axis(dim_utils, src.shape)

    # This runner iterates by t on axis 0 (`src[tt]`). A labels store where T is present but on a
    # different axis would silently drop every op at t≥1 into the still-image branch, applying only
    # `ops_by_t.get(0, [])` and mis-reporting `nLabelsAfter`. Refuse loudly rather than corrupt.
    # Same-shape stores (T on axis 0, or no T at all) go through unchanged.
    if t_idx is not None and t_idx != 0:
        raise RuntimeError(
            f'labels store has T on axis {t_idx} (shape={tuple(src.shape)}); the correction '
            f'runner requires T on axis 0. This layout is not produced by cecelia today — if you '
            f'hit this, the labels store was written by a foreign pipeline.')

    # A still image with a T-frame-scoped op is a user mistake, not a runner one — surface it.
    max_t = src.shape[0] if t_idx == 0 else 1
    for i, op in enumerate(ops):
        t = int(op.get('t', 0))
        if t < 0 or t >= max_t:
            raise RuntimeError(
                f'op[{i}] targets t={t} but the labels store has {max_t} timepoint(s)')

    # Count labels before — cheap on a per-frame basis, and the sum-of-unions IS the id space.
    labels_before = set()
    n_frames = max_t
    if t_idx == 0:
        for tt in range(n_frames):
            labels_before |= _unique_labels(np.asarray(src[tt]))
    else:
        labels_before |= _unique_labels(np.asarray(src[:]))

    # ── Stage the write ─────────────────────────────────────────────────────
    log.log(f'>> stage: {labels_path}')
    per_op_pixels = [0] * len(ops)
    labels_after = set()

    with zarr_utils.staged_store(labels_path) as staging:
        # Labels store is single-level (asserted above); pass nscales=1 explicitly. `kind='labels'`
        # picks the store_compressor labels codec — Decision 3.
        group, level0, _pchunks = zarr_utils.open_multiscales_for_writing(
            staging, src.shape, src.dtype, dim_utils, nscales=1, kind='labels',
            reference_zarr=labels_path)

        # Group ops by t so we process each frame once and apply all its ops in sequence.
        ops_by_t = {}
        for i, op in enumerate(ops):
            ops_by_t.setdefault(int(op['t']), []).append((i, op))

        if t_idx == 0:
            for tt in range(n_frames):
                frame = np.asarray(src[tt])                     # numpy copy — mutation-safe
                for (i, op) in ops_by_t.get(tt, []):
                    per_op_pixels[i] = _apply_op_inplace(frame, op, log)
                level0[tt] = frame
                labels_after |= _unique_labels(frame)
                if (tt + 1) % max(1, n_frames // 10) == 0:
                    log.log(f'[PROGRESS] {tt + 1}/{n_frames}')
        else:
            # Still image / no T axis: whole array in one pass, all ops apply against t=0.
            frame = np.asarray(src[:])
            for (i, op) in ops_by_t.get(0, []):
                per_op_pixels[i] = _apply_op_inplace(frame, op, log)
            level0[:] = frame
            labels_after |= _unique_labels(frame)
            log.log(f'[PROGRESS] 1/1')

        # No pyramid to build — nscales=1. If we ever add multi-level, this is where
        # `write_multiscale_pyramid(group, level0, dim_utils, nscales, pchunks)` fires.

        # Carry the source's valid box onto the staged store. A label correction never moves
        # pixels — merge rewrites `src → into`, remove writes to 0 — so the geometry of "where
        # real data lives" is identical, and dropping the box would make every downstream
        # consumer treat the padded borders as data (that's the incident `carry_valid_box`
        # exists for).
        zarr_utils.carry_valid_box(labels_path, staging)

    log.log(f'>> {len(labels_before)} labels before → {len(labels_after)} after')
    log.log(f'>> per-op pixels: {per_op_pixels}')

    write_json_atomic(result_file, {
        'perOpPixels': per_op_pixels,
        'nLabelsBefore': len(labels_before),
        'nLabelsAfter':  len(labels_after),
    })


if __name__ == '__main__':
    import sys
    params_path = sys.argv[1]
    with open(params_path, 'r', encoding='utf-8') as f:
        params = json.load(f)
    run(params)
