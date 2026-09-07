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


def _apply_op_inplace(frame, op):
    """Apply one op to `frame` in place; return the number of pixels rewritten by this op.

    The count is measured AT THE TIME the op fires — a merge queued after another merge that
    already moved some pixels reports the pixels IT sees, not the historical ones. That is the
    right semantics for the journal: the journal is a replay script, and each entry describes
    what its op moves at its point in the sequence.
    """
    kind = op['op']
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

    # A still image with a T-frame-scoped op is a user mistake, not a runner one — surface it.
    max_t = src.shape[t_idx] if t_idx is not None else 1
    for i, op in enumerate(ops):
        t = int(op.get('t', 0))
        if t < 0 or t >= max_t:
            raise RuntimeError(
                f'op[{i}] targets t={t} but the labels store has {max_t} timepoint(s)')

    # Count labels before — cheap on a per-frame basis, and the sum-of-unions IS the id space.
    labels_before = set()
    n_frames = max_t
    for tt in range(n_frames):
        frame = src[tt] if t_idx == 0 else src[:]     # 2D single-t: read whole array once
        labels_before |= _unique_labels(np.asarray(frame))
        if t_idx != 0:
            break

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
                    per_op_pixels[i] = _apply_op_inplace(frame, op)
                level0[tt] = frame
                labels_after |= _unique_labels(frame)
                if (tt + 1) % max(1, n_frames // 10) == 0:
                    log.log(f'[PROGRESS] {tt + 1}/{n_frames}')
        else:
            # Still image / no T axis: whole array in one pass, all ops apply against t=0.
            frame = np.asarray(src[:])
            for (i, op) in ops_by_t.get(0, []):
                per_op_pixels[i] = _apply_op_inplace(frame, op)
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
