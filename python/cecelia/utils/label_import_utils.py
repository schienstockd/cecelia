"""Import an external label image onto an existing image — the streaming core of `segment.importLabels`.

A label set made outside Cecelia becomes indistinguishable from one Cecelia segmented: a
`labels/{vn}.zarr` (+ `{vn}_{role}.zarr`) store shaped like the image minus C, uint32, with ids unique
across timepoints and as many pyramid levels as the image. See docs/SEGMENTATION.md → *Importing
labels*.

Nothing here holds more than one BLOCK of a frame: sources are read chunk-aligned through
`zarr_utils.open_label_source` (a zarr store, or a TIFF through tifffile's zarr view) and written into
a staged store, because the images this is for run to 45k x 28k px.

Roles follow the segmenters' `matchAs` vocabulary: `base` is the cell table, every other role only
adds `{role}_*` columns to it, keyed by id. So a non-base id must name the base object it belongs to.
Our segmenters guarantee that by overlap matching; an imported pair either already has it, or asks for
the same matching here (`match_to_base`).
"""

import contextlib

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.segmentation_utils import SegmentationUtils, object_stats

LABEL_DTYPE = SegmentationUtils.LABEL_DTYPE   # the segmenters' label dtype, so the two cannot drift
_UINT32_MAX = int(np.iinfo(np.uint32).max)

# XY edge of a block, in pixels: one block per array is what this holds in RAM (64 MB as uint32).
BLOCK_XY = 4096


class LabelImportError(ValueError):
    """A source that cannot become a label store as given. The message is shown to the user."""


def label_layout(im_shape, im_axes):
    """``(shape, axes)`` of the label store for an image: the image minus its C axis — the same
    layout every segmenter writes."""
    axes = [str(a).upper() for a in im_axes]
    keep = [i for i, a in enumerate(axes) if a != 'C']
    return tuple(int(im_shape[i]) for i in keep), [axes[i] for i in keep]


def axis_map(src_shape, label_shape, label_axes, src_axes=None, what='Labels'):
    """For each label axis, the source axis it reads from — or None for a length-1 label axis the
    source does not carry.

    Shapes must match apart from length-1 axes, in order: a 2D TIFF lands on a ``(1, 1, Y, X)`` image
    and a ``TYX`` store on a ``TZYX`` image with Z=1. Anything else raises, naming both shapes —
    padding axes until the numbers fit is how the old importer read a z-stack as a time series.
    Where the source names its axes (NGFF), the names must agree too."""
    src_nz = [i for i, s in enumerate(src_shape) if s != 1]
    lab_nz = [i for i, s in enumerate(label_shape) if s != 1]
    if [src_shape[i] for i in src_nz] != [label_shape[i] for i in lab_nz]:
        raise LabelImportError(
            f'{what} shape {tuple(src_shape)}'
            + (f' ({"".join(src_axes)})' if src_axes else '')
            + f' does not match the image minus C {tuple(label_shape)} ({"".join(label_axes)}).'
            + ' Only length-1 axes may differ.')
    if src_axes and len(src_axes) == len(src_shape):
        got = [src_axes[i] for i in src_nz]
        want = [label_axes[i] for i in lab_nz]
        if got != want:
            raise LabelImportError(
                f'{what} axes {"".join(got)} do not match the image axes {"".join(want)} '
                f'(shape {tuple(src_shape)} vs {tuple(label_shape)}).')
    mapping = [None] * len(label_shape)
    for li, si in zip(lab_nz, src_nz):
        mapping[li] = si
    return mapping


def check_source_dtype(dtype, what='Labels'):
    """A label image is integers. A bool array is a binary MASK — one object per frame once cast —
    so it is refused rather than imported as something it is not."""
    dtype = np.dtype(dtype)
    if dtype.kind == 'b':
        raise LabelImportError(
            f'{what} is a binary mask ({dtype}), not a label image: each object needs its own id.')
    if dtype.kind not in 'iuf':
        raise LabelImportError(f'{what} has dtype {dtype}; a label image must be integers.')


def to_label_ids(block, what='Labels'):
    """Cast one block to uint32 ids, refusing values a cast would silently change: negatives,
    ids past uint32, and non-integer floats."""
    kind = block.dtype.kind
    if kind == 'f':
        if not np.all(np.isfinite(block)) or np.any(block != np.trunc(block)):
            raise LabelImportError(f'{what} holds non-integer values; ids must be whole numbers.')
    if kind in 'if' and block.size and block.min() < 0:
        raise LabelImportError(f'{what} holds negative ids.')
    if block.size and block.max() > _UINT32_MAX:
        raise LabelImportError(f'{what} holds ids above {_UINT32_MAX}, the uint32 maximum.')
    return block.astype(LABEL_DTYPE, copy=False)


def read_block(src, mapping, label_slices, block_shape):
    """The source pixels under ``label_slices`` (one slice per label axis), shaped as the label
    block. Source axes that map to nothing are length 1 and read at index 0."""
    idx = [0] * len(src.shape)
    for li, si in enumerate(mapping):
        if si is not None:
            idx[si] = label_slices[li]
    return np.asarray(src[tuple(idx)]).reshape(block_shape)


def frame_offsets(frame_ranges):
    """Per-frame id offset that makes ids unique across timepoints, from each frame's
    ``(min, max)`` non-zero id (None for an empty frame).

    A frame is shifted only when its ids could collide with an earlier frame's (its minimum is not
    above the running maximum), so a movie already labelled uniquely keeps its ids exactly, and one
    labelled per frame from 1 — or with the same id along a track — gets the segmenters' monotonic
    numbering. Raises when the shifted ids would pass uint32."""
    offsets, running = [], 0
    for rng in frame_ranges:
        if rng is None:
            offsets.append(0)
            continue
        lo, hi = rng
        off = running if lo <= running else 0
        if hi + off > _UINT32_MAX:
            raise LabelImportError(
                f'Making ids unique across timepoints needs ids above {_UINT32_MAX}; '
                'relabel the source with smaller ids.')
        offsets.append(off)
        running = max(running, hi + off)
    return offsets


def match_to_base(pair_base, pair_other, inter, base_px, other_px, threshold):
    """Overlap matching of non-base ids to base ids, from SPARSE overlap counts: ``(other_ids,
    base_ids)``, the id each matched other object takes.

    The rule is the segmenters' `SegmentationUtils._match_nuc_cyto`, which the tests use as the
    oracle: each base object takes the other object it has the highest IoU with (ties to the lower
    id), if that IoU reaches ``threshold``; an other object two base objects both picked goes to the
    later (higher) base id, because the segmenter writes them in ascending order. Other objects no
    base object picked are dropped (id 0). Same float32 IoU.

    Why not call `_match_nuc_cyto`: it needs both whole frames in RAM and a dense base x other IoU
    matrix — on a 45k x 28k tilescan that is ~10 GB of frames and a matrix of cells squared. Here the
    counts are accumulated block by block and only co-occurring pairs exist.

    ``pair_base``/``pair_other``/``inter`` list every co-occurring pair and its shared pixels;
    ``base_px``/``other_px`` are each pair member's total pixels."""
    if len(pair_base) == 0:
        return np.zeros(0, LABEL_DTYPE), np.zeros(0, LABEL_DTYPE)
    inter = np.asarray(inter, dtype=np.float64)
    iou = (inter / (np.asarray(base_px, np.float64) + np.asarray(other_px, np.float64) - inter)
           ).astype(np.float32)
    # per base id: highest IoU first, then the lower other id (np.argmax over ascending ids)
    order = np.lexsort((pair_other, -iou, pair_base))
    pb, po, pi = pair_base[order], pair_other[order], iou[order]
    first = np.r_[True, pb[1:] != pb[:-1]]
    keep = pi[first] >= threshold
    best_b, best_o = pb[first][keep], po[first][keep]
    # an other id picked by several base ids: the last (highest) base id wins
    order2 = np.lexsort((best_b, best_o))
    bo, bb = best_o[order2], best_b[order2]
    last = np.r_[bo[1:] != bo[:-1], True] if bo.size else np.zeros(0, bool)
    return bo[last].astype(LABEL_DTYPE), bb[last].astype(LABEL_DTYPE)


def remap_ids(block, src_ids, dst_ids):
    """``block`` with every id in ``src_ids`` (sorted) replaced by its ``dst_ids`` entry; any other
    non-zero id becomes 0."""
    out = np.zeros_like(block)
    nz = block > 0
    if not nz.any() or src_ids.size == 0:
        return out
    vals = block[nz]
    pos = np.minimum(np.searchsorted(src_ids, vals), src_ids.size - 1)
    hit = src_ids[pos] == vals
    mapped = np.zeros_like(vals)
    mapped[hit] = dst_ids[pos[hit]]
    out[nz] = mapped
    return out


class _Counter:
    """Pixel counts per key, accumulated block by block and reduced once per frame."""

    def __init__(self):
        self.keys, self.counts = [], []

    def add(self, values):
        if values.size:
            k, c = np.unique(values, return_counts=True)
            self.keys.append(k)
            self.counts.append(c)

    def reduce(self):
        if not self.keys:
            return np.zeros(0, np.uint64), np.zeros(0, np.int64)
        k = np.concatenate(self.keys)
        c = np.concatenate(self.counts)
        uk, inv = np.unique(k, return_inverse=True)
        return uk, np.bincount(inv, weights=c, minlength=uk.size).astype(np.int64)


def _block_steps(label_shape, label_axes, dest_chunks, src_chunks):
    """Block edge per label axis: never smaller than either store's chunk (so no chunk is decoded
    twice per block), T one frame, and XY grown towards `BLOCK_XY` when a block is one plane."""
    steps = [max(int(d), int(s)) for d, s in zip(dest_chunks, src_chunks)]
    for i, ax in enumerate(label_axes):
        if ax == 'T':
            steps[i] = 1
    plane = all(steps[i] == 1 for i, ax in enumerate(label_axes) if ax not in ('Y', 'X'))
    for i, ax in enumerate(label_axes):
        if ax in ('Y', 'X') and plane and steps[i] < BLOCK_XY:
            steps[i] *= max(1, BLOCK_XY // steps[i])
        steps[i] = min(steps[i], int(label_shape[i]))
    return steps


def _frame_blocks(label_shape, label_axes, steps, t):
    """Every block of frame ``t`` as a tuple of slices over the label axes."""
    ranges = []
    for i, ax in enumerate(label_axes):
        if ax == 'T':
            ranges.append([slice(t, t + 1)])
        else:
            n, s = int(label_shape[i]), steps[i]
            ranges.append([slice(a, min(a + s, n)) for a in range(0, n, s)])
    blocks = [()]
    for r in ranges:
        blocks = [b + (s,) for b in blocks for s in r]
    return blocks


def _shape_of(slices):
    return tuple(s.stop - s.start for s in slices)


def import_labels(sources, out_paths, im_path, im_shape, nscales, dim_utils, log,
                  match_ids=False, match_threshold=0.3):
    """Write each ``sources[role]`` (a zarr store or TIFF path) to ``out_paths[role]`` as a label
    store for the image at ``im_path``. Returns the QC payload (`labelCounts`, `objectStats`, …) the
    Julia handler banks.

    ``sources`` must hold ``'base'``. Every shape is checked before anything is written. With
    ``match_ids`` each non-base role is overlap-matched to the base ids per frame; without it the
    ids are kept, and the share of overlapping pixels whose ids disagree is reported as
    ``idMismatchFrac`` so an unpaired import is flagged rather than silently mis-attributed."""
    if 'base' not in sources:
        raise LabelImportError('No base (cell) labels given.')
    label_shape, label_axes = label_layout(im_shape, dim_utils.im_dim_order)
    roles = ['base'] + [r for r in sources if r != 'base']
    others = roles[1:]
    t_ax = label_axes.index('T') if 'T' in label_axes else None
    n_t = label_shape[t_ax] if t_ax is not None else 1

    with contextlib.ExitStack() as stack:
        src, maps = {}, {}
        for role in roles:
            what = f'{role} labels ({sources[role]})'
            try:
                arr, axes = stack.enter_context(zarr_utils.open_label_source(sources[role]))
            except (ValueError, OSError) as e:   # not a store/TIFF, unreadable, missing
                raise LabelImportError(f'Cannot read {what}: {e}') from e
            check_source_dtype(arr.dtype, what)
            maps[role] = axis_map(tuple(arr.shape), label_shape, label_axes, axes, what)
            src[role] = arr
            log.log(f'>> {role}: {sources[role]} {tuple(arr.shape)} {arr.dtype}')

        def read(role, sl):
            return to_label_ids(read_block(src[role], maps[role], sl, _shape_of(sl)),
                                f'{role} labels')

        def src_chunks(role):
            ch = zarr_utils.chunks(src[role])
            return [ch[si] if si is not None else 1 for si in maps[role]]

        staged = {r: stack.enter_context(zarr_utils.staged_store(out_paths[r])) for r in roles}
        stores = {r: zarr_utils.open_multiscales_for_writing(
                      staged[r], label_shape, LABEL_DTYPE, dim_utils, axes=label_axes,
                      nscales=nscales, kind='labels', reference_zarr=im_path)
                  for r in roles}
        dest_chunks = stores['base'][2]
        steps = _block_steps(label_shape, label_axes, dest_chunks,
                             np.max([src_chunks(r) for r in roles], axis=0))
        blocks_per_frame = len(_frame_blocks(label_shape, label_axes, steps, 0))
        passes = 1 + (1 if n_t > 1 else 0) + (len(others) if match_ids else 0)
        total, done = passes * n_t * blocks_per_frame, 0
        log.progress(0, total)

        # Pass 0 (time-lapse only): each frame's id range, to decide the per-frame offsets.
        offsets = [0] * n_t
        if n_t > 1:
            ranges = []
            for t in range(n_t):
                lo, hi = None, None
                for sl in _frame_blocks(label_shape, label_axes, steps, t):
                    b = read('base', sl)
                    nz = b[b > 0]
                    if nz.size:
                        lo = int(nz.min()) if lo is None else min(lo, int(nz.min()))
                        hi = int(nz.max()) if hi is None else max(hi, int(nz.max()))
                    done += 1
                    log.progress(done, total)
                ranges.append(None if lo is None else (lo, hi))
            offsets = frame_offsets(ranges)
            if any(offsets):
                log.log(f'>> ids repeat across timepoints: renumbered {sum(o > 0 for o in offsets)}'
                        f' of {n_t} frame(s) so every object has its own id')

        counts = {r: 0 for r in roles}
        frame_sizes = []
        conflict_px, shared_px = 0, 0
        n_matched = {r: 0 for r in others}
        for t in range(n_t):
            off = offsets[t]
            base_ids = _Counter()
            pairs = {r: _Counter() for r in others} if match_ids else {}
            other_ids = {r: _Counter() for r in others}
            for sl in _frame_blocks(label_shape, label_axes, steps, t):
                b = read('base', sl)
                if off:
                    b[b > 0] += LABEL_DTYPE(off)
                stores['base'][1][sl] = b
                base_ids.add(b[b > 0])
                for r in others:
                    o = read(r, sl)
                    both = (b > 0) & (o > 0)
                    if match_ids:
                        # (base id, other id) packed into one uint64 key per shared pixel
                        pairs[r].add((b[both].astype(np.uint64) << np.uint64(32))
                                     | o[both].astype(np.uint64))
                        other_ids[r].add(o[o > 0])
                    else:
                        if off:
                            if int(o.max()) + off > _UINT32_MAX:
                                raise LabelImportError(
                                    f'{r} ids pass uint32 once made unique across timepoints.')
                            o[o > 0] += LABEL_DTYPE(off)
                        stores[r][1][sl] = o
                        other_ids[r].add(o[o > 0])
                        shared_px += int(both.sum())
                        conflict_px += int((both & (b != o)).sum())
                done += 1
                log.progress(done, total)

            b_keys, b_px = base_ids.reduce()
            counts['base'] += int(b_keys.size)
            frame_sizes.append(b_px)

            for r in others:
                if not match_ids:
                    counts[r] += int(other_ids[r].reduce()[0].size)
                    continue
                o_keys, o_px = other_ids[r].reduce()
                p_keys, p_px = pairs[r].reduce()
                pb = (p_keys >> np.uint64(32)).astype(np.uint64)
                po = (p_keys & np.uint64(_UINT32_MAX)).astype(np.uint64)
                src_ids, dst_ids = match_to_base(
                    pb, po, p_px,
                    b_px[np.searchsorted(b_keys, pb)] if pb.size else pb,
                    o_px[np.searchsorted(o_keys, po)] if po.size else po,
                    match_threshold)
                order = np.argsort(src_ids)
                src_ids, dst_ids = src_ids[order], dst_ids[order]
                n_matched[r] += int(src_ids.size)
                kept = _Counter()
                for sl in _frame_blocks(label_shape, label_axes, steps, t):
                    o = remap_ids(read(r, sl), src_ids, dst_ids)
                    stores[r][1][sl] = o
                    kept.add(o[o > 0])
                    done += 1
                    log.progress(done, total)
                counts[r] += int(kept.reduce()[0].size)

        for r in roles:
            g, level0, chunks = stores[r]
            zarr_utils.write_label_pyramid(g, level0, label_axes, nscales, chunks)

    qc = {
        'labelCounts': counts,
        'objectStats': object_stats(
            frame_sizes,
            (dim_utils.im_physical_size('x', default=1.0),
             dim_utils.im_physical_size('y', default=1.0),
             dim_utils.im_physical_size('z', default=1.0)),
            dim_utils.is_3D()),
        'idsOffset': bool(any(offsets)),
        'matchIds': bool(match_ids),
    }
    if others and not match_ids:
        qc['idMismatchFrac'] = (conflict_px / shared_px) if shared_px else None
    if others and match_ids:
        qc['nMatched'] = n_matched
    return qc
