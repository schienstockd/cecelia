"""
Base class for tiled image segmentation.

Handles the common loop (T × XY tiles), global label ID management,
post-processing, nucleus-cytoplasm matching, and label zarr writing.

Subclasses implement predict_slice() for specific algorithms (cellpose,
stardist, etc.).
"""

import contextlib
import os
import numpy as np
import dask.array as da
import zarr

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.intensity_utils as intensity_utils

from skimage import morphology, segmentation
from scipy import ndimage


def count_labels(arr):
    """Number of distinct non-zero label IDs in a label array — the objective cell count for QC.
    Label IDs are assigned globally-incrementing across tiles AND timepoints, so this is the total
    number of segmented object instances (matching one row per object in the measured .h5ad)."""
    return int(np.unique(arr[arr > 0]).size)


class SegmentationUtils:

    LABEL_DTYPE = np.uint32

    # Frames either side of t that `predict_slice` needs. 0 = the tile only, which is every
    # algorithm that segments a single timepoint (cellpose). A TEMPORAL algorithm — one whose
    # prediction for t depends on t±r, e.g. optical-flow metrics — sets this and the base supplies
    # the window. Declared by the SUBCLASS so the base never special-cases a particular method.
    #
    # The window is read at TILE extent, not whole frames: widening the per-timepoint read would
    # hold r*2+1 full frames in RAM (~300 MB each on a 1036x1055x35x4ch uint16 movie), destroying
    # the property that peak memory is one frame. See docs/todo/COASTAL_SEGMENTATION_PLAN.md.
    TEMPORAL_RADIUS = 0

    def __init__(self, params, dim_utils):
        self.params = params
        self.dim_utils = dim_utils
        # Physical pixel size, for the params expressed in MICRONS. Those are the ones that describe
        # a CELL rather than a grid: the same number then means the same biology on every image of a
        # set, which is the whole point of one parameter value per set. A px value silently means
        # something different the moment the zoom changes.
        self.phys_size_x = (dim_utils.im_physical_size('x', default=1.0) if dim_utils else 1.0)
        self.phys_size_y = (dim_utils.im_physical_size('y', default=self.phys_size_x)
                            if dim_utils else 1.0)
        self.block_size = int(params.get('blockSize', 512))
        self.overlap = int(params.get('overlap', 64))
        # Z tiling — 0 means no Z tiling (whole stack passed to cellpose, which uses stitch_threshold internally)
        self.block_size_z = int(params.get('blockSizeZ', 0))
        self.overlap_z = int(params.get('overlapZ', 0))
        # IoU threshold for tile seam stitching; 0 = simple np.maximum merge (no seam matching)
        self.label_overlap = float(params.get('labelOverlap', 0.0))
        self.match_threshold = float(params.get('matchThreshold', 0.3))
        self.remove_unmatched = bool(params.get('removeUnmatched', False))
        # Sizes are AREAS in square microns; erosion/expansion are lengths in microns. Same reason
        # as the coastal params: a px value silently means something else the moment the zoom
        # changes, so one value per set is only meaningful in physical units.
        self.min_cell_size = self.px_area_from_um2(params.get('minCellSize', 0))
        self.cell_size_max = self.px_area_from_um2(params.get('cellSizeMax', 0))
        # Boundary smoothing, in pixels of gaussian sigma. Cosmetic and OFF by default: it changes
        # every measured shape descriptor, so it must be a deliberate choice, not a silent default.
        self.label_smoothing = self.px_from_um(params.get('labelSmoothing', 0.0))
        # A morphological radius has to be a whole number of pixels, and a value the user SET must
        # never round to "off" — that reads as the control being broken.
        self.label_expansion = self._px_radius(params.get('labelExpansion', 0))
        self.label_erosion = self._px_radius(params.get('labelErosion', 0))
        self.clear_touching_border = bool(params.get('clearTouchingBorder', False))
        self.clear_depth = bool(params.get('clearDepth', False))
        self.normalise_to_whole = bool(params.get('normaliseToWhole', True))
        self.task_dir = params['taskDir']
        self.output_value_name = params.get('outputValueName', 'default')

    def px_from_um(self, um):
        """Microns → pixels on this image's X axis. 0 stays 0, so "off" survives the conversion."""
        um = float(um)
        return 0.0 if um <= 0 else um / max(self.phys_size_x, 1e-6)

    def _px_radius(self, um):
        """Microns → a whole-pixel morphological radius. 0 stays off; anything set is at least 1 px."""
        px = self.px_from_um(um)
        return 0 if px <= 0 else max(1, int(round(px)))

    def px_area_from_um2(self, um2):
        """Square microns → pixel COUNT. Uses both axes: assuming square pixels is fine on this
        data and wrong in general, and a size filter that is quietly 2x off is hard to spot."""
        um2 = float(um2)
        if um2 <= 0:
            return 0
        return int(round(um2 / max(self.phys_size_x * self.phys_size_y, 1e-12)))

    def predict_slice(self, tile, model_params, norm_params=None,
                      context=None, context_index=None):
        """Override in subclass. tile=[C,Z,Y,X] or [C,Y,X]. Returns uint32 label mask.

        `context`/`context_index` are passed ONLY when the subclass sets `TEMPORAL_RADIUS > 0`, so a
        subclass that does not want them never has to accept them (cellpose does not, and neither
        does any existing third-party subclass).

        context:       the same tile through time, [W, ...tile axes], W <= 2*TEMPORAL_RADIUS+1
        context_index: index of `tile`'s own timepoint within `context`. NOT always the middle —
                       the window is TRUNCATED at the start and end of the movie rather than
                       reflected or edge-padded, because repeating a frame invents zero motion and
                       mirroring invents motion outright.

        `context` matches `tile` on every axis they share — including z when the valid-box skip has
        narrowed this timepoint (see *Skipping the padding a drift correction added* below). A
        subclass that takes its pixels from the window therefore never has to ask which of the two
        was narrowed.
        """
        raise NotImplementedError

    # ── Main loop ─────────────────────────────────────────────────────────────

    def predict_from_zarr(self, im_dat):
        """Segment: iterate T × XY tiles for each model, ONE TIMEPOINT AT A TIME.

        Each timepoint is filled (tiles), seam-stitched, post-processed, nuc/base matched, and
        streamed to its on-disk label store before the next — so peak memory is one FRAME of labels
        per type, not the whole T×Z×Y×X stack (the previous version allocated the full stack and
        OOM'd on large time-lapses). This is a pure reordering: every post-fill step already looped
        per timepoint, and the only cross-frame state is the monotonic ``max_labels`` counter (which
        the per-frame steps never touch), so the label output is byte-identical."""
        models = self.params.get('models', {})
        dim_utils = self.dim_utils

        # Global norm params (from lowest-res level)
        all_norm_params = {}
        if self.normalise_to_whole:
            for key, mp in models.items():
                all_norm_params[key] = self._compute_norm_params(im_dat, mp)

        # Image label store shape derives from the full image shape (tiling now reads whole frames
        # via zarr_utils.read_timepoint, so per-axis image indices are no longer needed here).
        im_shape = list(im_dat[0].shape)

        T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
        H = dim_utils.dim_val('Y')
        W = dim_utils.dim_val('X')
        is_3d = dim_utils.is_3D()

        # On-disk label store shape/axes: image shape without C (may include T)
        label_axes = [ax for ax in dim_utils.im_dim_order if ax != 'C']
        label_shape = [im_shape[i] for i, ax in enumerate(dim_utils.im_dim_order) if ax != 'C']
        store_la_t = label_axes.index('T') if 'T' in label_axes else None

        # Per-FRAME label buffer: label axes without T (the unit we hold in RAM and process at a time)
        frame_axes = [ax for ax in label_axes if ax != 'T']
        frame_shape = [label_shape[i] for i, ax in enumerate(label_axes) if ax != 'T']
        fa_y = frame_axes.index('Y')
        fa_x = frame_axes.index('X')
        fa_z = frame_axes.index('Z') if 'Z' in frame_axes else None

        # Input-image frame axes = image axes without T — KEEPS the channel axis, so its Y/X indices
        # differ from the (channel-less) label frame's. Used to tile the in-RAM input frame.
        in_axes = [ax for ax in dim_utils.im_dim_order if ax != 'T']
        ifa_y = in_axes.index('Y')
        ifa_x = in_axes.index('X')
        ifa_z = in_axes.index('Z') if 'Z' in in_axes else None

        # Which planes/rows/columns actually hold data, per timepoint. `None` for every store that
        # never padded, which is most of them — then the spans are the whole frame and nothing changes.
        store_la_z = label_axes.index('Z') if 'Z' in label_axes else None
        store_la_y = label_axes.index('Y')
        store_la_x = label_axes.index('X')
        n_z = im_shape[dim_utils.im_dim_order.index('Z')] if 'Z' in dim_utils.im_dim_order else 0
        im_path_for_box = self.params.get('imPath')

        # Axis indices on the FULL input array (time axis still present) — the in-RAM frame has T
        # dropped, so its indices cannot address other timepoints. Only used for temporal context.
        ia_t = dim_utils.im_dim_order.index('T') if 'T' in dim_utils.im_dim_order else None
        ia_y = dim_utils.im_dim_order.index('Y')
        ia_x = dim_utils.im_dim_order.index('X')
        ia_z = dim_utils.im_dim_order.index('Z') if 'Z' in dim_utils.im_dim_order else None

        # Collect unique matchAs labels in order; 'base' is always the primary type
        match_as_list = list(dict.fromkeys(
            mp.get('matchAs', 'base') for mp in models.values()
        ))
        max_labels = {ma: 0 for ma in match_as_list}

        labels_dir = os.path.join(self.task_dir, 'labels')
        os.makedirs(labels_dir, exist_ok=True)
        nscales = len(im_dat)

        def _store_path(ma):
            # 'base' → {outputValueName}.zarr; other types → {outputValueName}_{ma}.zarr
            name = f'{self.output_value_name}.zarr' if ma == 'base' \
                else f'{self.output_value_name}_{ma}.zarr'
            return os.path.join(labels_dir, name)

        counts = {ma: 0 for ma in match_as_list}

        # Every timepoint's valid span, resolved UP FRONT rather than inside the loop. Two things
        # need it early: the XY span decides how many tiles a frame is cut into, and the progress
        # total has to be known before the first tile is reported. One cheap attr read per timepoint.
        spans = {}
        for t in range(T):
            box = (zarr_utils.read_valid_box(im_path_for_box, timepoint=t)
                   if im_path_for_box else None)
            spans[t] = {
                'Z': self._valid_span(box, 'Z', n_z) if store_la_z is not None else (0, n_z),
                'Y': self._valid_span(box, 'Y', H),
                'X': self._valid_span(box, 'X', W),
            }

        # Tiles are cut over the NARROWED frame, so their count can differ per timepoint.
        xy_tiles_by_t = {t: self._create_xy_tiles(sp['Y'][1] - sp['Y'][0],
                                                  sp['X'][1] - sp['X'][0])
                         for t, sp in spans.items()}
        total = sum(len(v) for v in xy_tiles_by_t.values())
        done = 0

        _extent = {'Z': n_z, 'Y': H, 'X': W}

        # BOTH skip messages are reported here, once, for the whole movie — the z one used to print
        # from inside the loop under `if t == 0`. The spans ARE per timepoint (`spans[t]`, and every
        # narrow/read/write-back below uses that frame's own), but one line of frame 0's numbers
        # reads exactly like a single decision carried over all 200 frames. The distinct-span count
        # is what tells the two apart at a glance, so it is part of the message rather than something
        # to go and check on the store.
        def _span_msg(axes):
            if not any(spans[t][ax] != (0, _extent[ax]) for ax in axes for t in spans):
                return None
            at_t0 = ', '.join(f'{ax.lower()} {spans[0][ax][0]}:{spans[0][ax][1]} of {_extent[ax]}'
                              for ax in axes)
            n = len({tuple(spans[t][ax] for ax in axes) for t in spans})
            return at_t0 + (f' at t0, {n} distinct span(s) over {T} timepoints' if T > 1 else '')

        _z_msg = _span_msg(['Z'])
        if _z_msg:
            print(f'>> skipping padded z planes: segmenting {_z_msg}', flush=True)
        _yx_msg = _span_msg(['Y', 'X'])
        if _yx_msg:
            print(f'>> skipping padded XY: segmenting {_yx_msg}', flush=True)

        # Each store is written through `zarr_utils.staged_store`: streamed into a staging sibling
        # and renamed onto its final path only once its pyramid is complete. So re-running a
        # value_name that is ALREADY registered and then cancelling leaves the existing labels
        # intact, instead of truncating them while ccid.json still advertises them. The stack holds
        # one staging context per label type; each promotes as it unwinds.
        # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
        with contextlib.ExitStack() as stack:
            staged = {ma: stack.enter_context(zarr_utils.staged_store(_store_path(ma)))
                      for ma in match_as_list}
            # Open one on-disk store per label type up front; frames are streamed into level 0 below.
            stores = {ma: self._open_label_store(staged[ma], label_shape, label_axes, nscales)
                      for ma in match_as_list}

            for t in range(T):
                # Read this timepoint's image ONCE into RAM (time axis dropped → frame_axes layout), then
                # tile it in memory. Reading each tile from the store instead re-fetches whole chunks per
                # tile — the over-read the old whole-level fortify() worked around. See
                # zarr_utils.read_timepoint / docs/todo/ZARR_STREAMING_PLAN.md (Phase 1).
                frame_in = zarr_utils.read_timepoint(im_dat[0], dim_utils, t, drop_time=True)

                # Narrow to the region that holds data. Everything below — tiling, cellpose, post-
                # processing, nuc/cyto matching — then runs on the reduced frame unchanged; only the
                # WRITE puts it back at its offset, so the store keeps its full shape.
                z0, z1 = spans[t]['Z'] if (ifa_z is not None and store_la_z is not None) else (0, n_z)
                y0, y1 = spans[t]['Y']
                x0, x1 = spans[t]['X']
                narrowed = (z0, z1) != (0, n_z)
                if narrowed:
                    frame_in = self._narrow_axis(frame_in, ifa_z, z0, z1)
                frame_in = self._narrow_axis(frame_in, ifa_y, y0, y1)
                frame_in = self._narrow_axis(frame_in, ifa_x, x0, x1)

                # One frame's labels per type (uint32 zeros), no time axis — sized to the span we
                # actually segment, so the buffer and the tiles agree. Allocated AFTER the span is
                # known, not before the read.
                frame_shape_t = list(frame_shape)
                if fa_z is not None:
                    frame_shape_t[fa_z] = z1 - z0
                frame_shape_t[fa_y] = y1 - y0
                frame_shape_t[fa_x] = x1 - x0
                frame = {ma: np.zeros(frame_shape_t, dtype=self.LABEL_DTYPE) for ma in match_as_list}

                for read_yx, write_yx, crop_yx in xy_tiles_by_t[t]:
                    for model_key in sorted(models.keys()):
                        model_params = models[model_key]
                        match_as = model_params.get('matchAs', 'base')
                        norm_p = all_norm_params.get(model_key)

                        # tile from the in-RAM input frame (t_idx=None: no time axis; input-frame Y/X)
                        tile = self._extract_tile(frame_in, 0, None, ifa_y, ifa_x, read_yx)

                        if self.TEMPORAL_RADIUS > 0 and ia_t is not None:
                            # tile-extent reads across the clamped window; truncated at the movie
                            # edges, never reflected or edge-padded (see predict_slice docstring)
                            lo = max(0, t - self.TEMPORAL_RADIUS)
                            hi = min(T - 1, t + self.TEMPORAL_RADIUS)
                            # Narrowed to THIS timepoint's span, like the tile — the window is the
                            # tile through time and must match it on every axis. These frames are
                            # read from the full store rather than from `frame_in` (which holds only
                            # t), so the narrowing has to be re-applied here; a subclass takes its
                            # pixels from the window, so a full-depth one silently re-segments the
                            # padding. Each frame's OWN span may differ — drift moves the stack — but
                            # the window is one array with one z extent, and t's span is the one the
                            # output is written back at.
                            #
                            # `read_yx` indexes the NARROWED frame, but these frames come from the
                            # full store — so the XY offset has to be added back, exactly as the z
                            # span is re-applied below. Without it the window would be read from the
                            # wrong part of the image whenever XY was narrowed, silently handing a
                            # temporal subclass pixels that do not correspond to its own tile.
                            read_yx_full = (slice(read_yx[0].start + y0, read_yx[0].stop + y0),
                                            slice(read_yx[1].start + x0, read_yx[1].stop + x0))
                            context = np.stack([
                                self._extract_tile(im_dat[0], t2, ia_t, ia_y, ia_x, read_yx_full,
                                                   z_idx=ia_z if narrowed else None, z=(z0, z1))
                                for t2 in range(lo, hi + 1)])
                            masks = self.predict_slice(tile, model_params, norm_p,
                                                       context=context, context_index=t - lo)
                        else:
                            # unchanged call for every non-temporal subclass
                            masks = self.predict_slice(tile, model_params, norm_p)
                        masks = self._crop_masks(masks, crop_yx, is_3d)

                        if np.any(masks > 0):
                            masks[masks > 0] += max_labels[match_as]
                            max_labels[match_as] = int(masks.max())

                        # la_t=None → write into the frame buffer at its Y/X (no time index)
                        self._write_tile_to_arr(
                            frame[match_as], masks, 0, None, fa_y, fa_x, write_yx)

                    done += 1
                    print(f'[PROGRESS] {done}/{total}', flush=True)

                # Per-frame post-fill steps. Passing la_t=None, T=1 takes the whole-array branch of each
                # helper — i.e. exactly one iteration of the loop each already ran over timepoints.
                if self.label_overlap > 0:
                    for ma in frame:
                        # narrowed extent, not the canvas — the seams are where THIS frame's tiles met
                        frame[ma] = self._stitch_tile_seams(
                            frame[ma], y1 - y0, x1 - x0, None, fa_y, fa_x, 1)

                for ma in frame:
                    # no `real_border`: a run processes whole frames, so its array edge IS the image edge
                    frame[ma] = self.post_process(frame[ma], frame_axes, None, 1, is_3d)

                if 'base' in frame and 'nuc' in frame:
                    frame['base'], frame['nuc'] = self._match_nuc_cyto(
                        frame['base'], frame['nuc'], None, 1)

                # Stream each type's frame to disk and tally its (globally unique) label IDs. IDs never
                # repeat across timepoints (max_labels is monotonic), so per-frame counts sum to the
                # whole-stack distinct-ID count the previous code returned.
                for ma in match_as_list:
                    _, level0, _ = stores[ma]
                    sl = tuple(t if i == store_la_t else
                               slice(z0, z1) if i == store_la_z else
                               slice(y0, y1) if i == store_la_y else
                               slice(x0, x1) if i == store_la_x else
                               slice(None) for i in range(level0.ndim))
                    level0[sl] = frame[ma]
                    counts[ma] += count_labels(frame[ma])

            # The label store inherits the same fact about itself: outside these spans it is zero
            # because nothing was segmented there, not because nothing was found. Recording it means
            # a downstream consumer skips the same region without re-deriving the geometry — the
            # propagation rule in docs/ARCHITECTURE.md → *The valid box*.
            #
            # Only the axes that were ACTUALLY narrowed are recorded: writing a full-extent span for
            # an axis nothing skipped would claim a restriction that isn't one.
            recorded = [ax for ax in ('Z', 'Y', 'X')
                        if (ax != 'Z' or store_la_z is not None)
                        and any(spans[t][ax] != (0, _extent[ax]) for t in spans)]
            if recorded:
                for ma in match_as_list:
                    zarr_utils.write_valid_box(
                        staged[ma], recorded,
                        {t: {ax: spans[t][ax] for ax in recorded} for t in sorted(spans)})

            # Build the pyramids from the on-disk level 0 (bounded — one timepoint at a time)
            for ma in match_as_list:
                g, level0, chunks = stores[ma]
                self._finalize_label_pyramid(g, level0, label_axes, nscales, chunks)

        # Objective QC count per label type (banked by the Julia handler via the qc/ sidecar).
        return counts

    # ── Tile helpers ──────────────────────────────────────────────────────────

    def _create_xy_tiles(self, H, W):
        """Generate (read_yx, write_yx, crop_yx) tuples for XY tiling with overlap."""
        ov = self.overlap
        tiles = []
        y = 0
        while y < H:
            y1 = min(y + self.block_size, H)
            x = 0
            while x < W:
                x1 = min(x + self.block_size, W)

                ry0 = max(0, y - ov)
                ry1 = min(H, y1 + ov)
                rx0 = max(0, x - ov)
                rx1 = min(W, x1 + ov)

                # Actual padding added on each side
                pad_top    = y - ry0
                pad_bottom = ry1 - y1
                pad_left   = x - rx0
                pad_right  = rx1 - x1

                tiles.append((
                    (slice(ry0, ry1), slice(rx0, rx1)),   # read region
                    (slice(y, y1),    slice(x, x1)),       # write region
                    (pad_top, pad_bottom, pad_left, pad_right),  # crop amounts
                ))
                x = x1
            y = y1
        return tiles

    def _extract_tile(self, im_data, t, t_idx, y_idx, x_idx, read_yx, z_idx=None, z=None):
        """Extract one XY tile for timepoint t. Returns numpy array.

        `z_idx`/`z` narrow the read to the `(z0, z1)` planes that hold data — pushed into the index
        rather than sliced off afterwards, so the padded planes are never read at all. `z_idx=None`
        (the default) reads the whole stack, which is every caller that is not the valid-box skip.
        """
        idx = [slice(None)] * len(im_data.shape)
        if t_idx is not None:
            idx[t_idx] = t
        idx[y_idx] = read_yx[0]
        idx[x_idx] = read_yx[1]
        if z_idx is not None and z is not None:
            idx[z_idx] = slice(int(z[0]), int(z[1]))
        return np.asarray(im_data[tuple(idx)])

    def _crop_masks(self, masks, crop_yx, is_3d):
        """Remove overlap padding from predictions. crop_yx=(top, bottom, left, right)."""
        pt, pb, pl, pr = crop_yx
        if is_3d:
            Y, X = masks.shape[1], masks.shape[2]
            return masks[:, pt:Y - pb if pb else None, pl:X - pr if pr else None]
        else:
            Y, X = masks.shape[0], masks.shape[1]
            return masks[pt:Y - pb if pb else None, pl:X - pr if pr else None]

    def _write_tile_to_arr(self, arr, masks, t, la_t, la_y, la_x, write_yx):
        """Merge a tile into the label array, KEEPING whatever is already labelled there.

        Stacking model groups is how multi-pass segmentation is expressed: a second group with a
        smaller diameter picks up cells the first missed. `predict_from_zarr` loops the repeatable
        `models` group, and every group's labels are offset by the running `max_labels[match_as]`,
        so a later group's IDs are always numerically LARGER than an earlier group's.

        This used to merge with `np.maximum`, which therefore let the later group win every
        overlapping pixel — a small-diameter second pass silently ate the first pass's cells. Nobody
        wants that, so the merge fills only unlabelled pixels.

        Within a single group this is a no-op: `_create_xy_tiles` write regions are
        `(slice(y, y1), slice(x, x1))` with `x = x1` advancing, i.e. exactly tiling and disjoint
        (only the READ regions overlap, and `_crop_masks` removes that padding). So the destination
        is always 0 there and `np.maximum`, assignment and fill-only all agree.
        """
        idx = [slice(None)] * arr.ndim
        if la_t is not None:
            idx[la_t] = t
        idx[la_y] = write_yx[0]
        idx[la_x] = write_yx[1]
        idx = tuple(idx)
        arr[idx] = np.where(arr[idx] > 0, arr[idx], masks)

    # ── Normalisation ─────────────────────────────────────────────────────────

    def _subsample_time(self, darr, max_frames):
        """Evenly stride the time axis down to at most `max_frames` frames; identity when that isn't
        needed or possible. A no-op for a single-timepoint image (a large tiled mosaic), which is why
        subsampling can't hurt the case that most depends on a global window."""
        if not max_frames or max_frames < 1:
            return darr
        t_idx = self.dim_utils.dim_idx('T')
        if t_idx is None or darr.shape[t_idx] <= max_frames:
            return darr
        stride = -(-darr.shape[t_idx] // max_frames)      # ceil → at most max_frames frames
        sl = [slice(None)] * darr.ndim
        sl[t_idx] = slice(0, None, stride)
        return darr[tuple(sl)]

    def _compute_norm_params(self, im_dat, model_params, max_frames=None):
        """Per-channel percentile clipping range for scale-to-whole normalisation.

        Why scale-to-whole exists at all: on a large TILED image, per-tile normalisation gives each
        tile its own window and the segmentation comes out **patchy** — visibly inconsistent from tile
        to tile. A global window is the fix. (It sometimes also helps intravital, and sometimes not —
        which is why `normaliseToWhole` is a user option, `segmentationOptions/normaliseToWhole`.)

        ``max_frames`` — read at most this many TIMEPOINTS (evenly strided) instead of all of them.
        Opt-in, default None = exact, because this function also serves real runs and silently
        changing their statistic would change existing results. The **preview** passes it: the exact
        statistic costs ~28 s on a single-level timelapse (measured, `EaMaVq` 201 × 20 × 544 × 548,
        ~2.4 GB read), which is the entire latency of a preview whose inference is 0.35 s.

        Subsampling trades only TEMPORAL coverage — every frame it reads is read in full, so spatial
        coverage is untouched. That is what makes it safe for the case that most needs a global window:
        a large tiled mosaic is typically a single timepoint, so the stride is 1 and the result is
        exact. It degrades only on timelapses, where consecutive frames are highly redundant.

        Scale-to-whole is required — a per-tile/per-frame window would swing with local brightness
        and give inconsistent masks — so the percentile is GLOBAL. Two ways to get it without a
        per-tile/frame dependency:
          • pyramided store → take it from the small lowest-res level (a cheap whole-image proxy),
            exactly as before.
          • single-level store (drift/AF/cellpose-corrected outputs, nscales=1) → `im_dat[-1]` IS the
            full-res level, so materialising it OOMs on large movies. Instead stream a per-value
            histogram (exact for integer data, ~256 KB/channel regardless of size) and read the
            percentile off its CDF. Same statistic, bounded memory. See ZARR_STREAMING_PLAN.md.
        Excludes background zeros in both paths (matches the historical `data[data > 0]`)."""
        c_idx = self.dim_utils.dim_idx('C')
        normalise_perc = float(model_params.get('normalise', 99.9))
        channels = script_utils.channel_indices(
            list(model_params.get('cellChannels', [])) + list(model_params.get('nucChannels', [])),
            'cellChannels/nucChannels', 'cellpose_models_for_python (cellpose.jl)')
        result = {}

        if len(im_dat) == 1:
            # bounded streaming histogram over the (single, full-res) level
            level = im_dat[0]
            darr = level if isinstance(level, da.Array) else da.from_array(level)
            darr = self._subsample_time(darr, max_frames)
            hists = intensity_utils.channel_histograms(darr, c_idx, channels=channels)
            for ch, hist in zip(channels, hists):
                hist = hist.copy()
                hist[0] = 0                       # drop background zeros
                if int(hist.sum()) > 100:
                    result[ch] = (float(intensity_utils.hist_percentile(hist, 100 - normalise_perc)),
                                  float(intensity_utils.hist_percentile(hist, normalise_perc)))
            return result

        low_res = np.asarray(im_dat[-1])
        for ch in channels:
            idx = [slice(None)] * low_res.ndim
            idx[c_idx] = ch
            ch_data = low_res[tuple(idx)].ravel()
            valid = ch_data[ch_data > 0]
            if len(valid) > 100:
                result[ch] = (float(np.percentile(valid, 100 - normalise_perc)),
                              float(np.percentile(valid, normalise_perc)))
        return result

    # ── Post-processing ───────────────────────────────────────────────────────

    def post_process(self, arr, label_axes, la_t, T, is_3d, real_border=None):
        """Apply erosion, expansion, min-size filter, and border clearing.

        ``real_border`` says which of the array's Y/X faces are the **image** edge, as
        ``{'Y': (lo_is_image_edge, hi_is_image_edge), 'X': (…)}``. ``None`` (the default, and what the
        full run passes) means all of them are — the run processes whole frames, so its array edge IS
        the image edge.

        It exists for the task preview, which runs this on a CROP of one plane. There, most edges are
        just where the user stopped looking, and two steps would otherwise be silently wrong about
        them — both in the direction of showing fewer cells than the run produces:

        * ``clear_touching_border`` would clear every cell at the crop edge. The more you zoom in, the
          more it deletes, so a parameter the run applies at the image border only would look
          catastrophic. Passed as ``mask`` to ``clear_border``, where ``False`` marks the bands that
          are genuinely image edge.
        * the size filter would judge a cell on its **clipped** pixel count, dropping cells that are
          only small because the crop cut them. Labels touching a non-image edge are exempted.

        ``label_expansion`` stays approximate on a crop — an edge cell cannot grow into pixels outside
        it. That needs a halo around the region to fix, which would change what the preview reads;
        it is an edge-only cosmetic difference, so it is left alone deliberately.
        """
        for t in range(T):
            if la_t is not None:
                idx = tuple(t if i == la_t else slice(None) for i in range(arr.ndim))
                vol = arr[idx].copy()
            else:
                vol = arr.copy()

            # Smoothing FIRST: it cleans the segmenter's raw outline, and erosion/expansion are
            # deliberate size changes the user then applies to a clean shape.
            if self.label_smoothing > 0:
                vol = self._smooth_labels(vol, self.label_smoothing, is_3d)

            if self.label_erosion > 0:
                vol = self._erode_labels(vol, self.label_erosion, is_3d)

            if self.label_expansion > 0:
                vol = segmentation.expand_labels(vol, self.label_expansion)

            # Labels the crop cut through, and the mask marking which bands to actually clear. Built
            # once per volume because both steps below need it (see the docstring).
            border_mask, clipped = self._crop_edges(vol, real_border)

            if self.min_cell_size > 0 or self.cell_size_max > 0:
                labels, counts = np.unique(vol[vol > 0], return_counts=True)
                for lb, cnt in zip(labels, counts):
                    if int(lb) in clipped:
                        continue        # measured short by the crop — not a size the run would see
                    if (self.min_cell_size > 0 and cnt < self.min_cell_size) or \
                       (self.cell_size_max > 0 and cnt > self.cell_size_max):
                        vol[vol == lb] = 0

            if self.clear_depth and is_3d:
                # Clear labels touching Z axis borders (first and last Z slice)
                z_axis = label_axes.index('Z') if 'Z' in label_axes else 0
                for face_idx in [0, vol.shape[z_axis] - 1]:
                    face = tuple(face_idx if i == z_axis else slice(None) for i in range(vol.ndim))
                    for lb in np.unique(vol[face]):
                        if lb > 0:
                            vol[vol == lb] = 0

            if self.clear_touching_border:
                if is_3d:
                    # Clear Y/X borders per Z slice; don't clear Z borders
                    for z in range(vol.shape[0]):
                        vol[z] = segmentation.clear_border(vol[z], mask=border_mask)
                else:
                    vol = segmentation.clear_border(vol, mask=border_mask)

            if la_t is not None:
                arr[idx] = vol
            else:
                arr[:] = vol

        return arr

    # The two Y/X faces of each axis, as an index into the LAST two axes of `vol` — so one table
    # serves a 2D [Y,X] mask and a 3D [Z,Y,X] volume (`Ellipsis` absorbs the leading axes).
    _YX_FACES = {'Y': ((0, slice(None)), (-1, slice(None))),
                 'X': ((slice(None), 0), (slice(None), -1))}

    def _crop_edges(self, vol, real_border):
        """`(border_mask, clipped_labels)` for `post_process` — see its docstring.

        `border_mask` is what `clear_border` takes as `mask`: **False marks a band to clear**, i.e. a
        real image edge. `clipped_labels` are the ids touching a non-image edge, whose pixel count the
        crop cut short. `real_border=None` → `(None, set())`, which is `clear_border`'s own default
        (every edge cleared) and no exemptions: exactly the behaviour before this argument existed.
        """
        if real_border is None:
            return None, set()

        border_mask = np.ones(vol.shape[-2:], dtype=bool)
        clipped = set()
        for ax_name, (lo_idx, hi_idx) in self._YX_FACES.items():
            lo_real, hi_real = real_border.get(ax_name, (True, True))
            for is_image_edge, idx in ((lo_real, lo_idx), (hi_real, hi_idx)):
                if is_image_edge:
                    border_mask[idx] = False       # the run would clear here too
                else:
                    face = vol[(Ellipsis,) + idx]
                    clipped.update(int(lb) for lb in np.unique(face) if lb > 0)
        return border_mask, clipped

    def _stitch_tile_seams(self, arr, H, W, la_t, la_y, la_x, T):
        """Merge label IDs split at tile boundaries using IoU matching.

        After np.maximum tile merge, cells straddling a tile boundary appear with
        different IDs on each side. For each seam, labels in the overlap zone on
        one side are matched against the other; pairs with IoU >= label_overlap are
        remapped to the same ID.
        """
        ov = self.overlap
        for t in range(T):
            if la_t is not None:
                t_idx = tuple(t if i == la_t else slice(None) for i in range(arr.ndim))
                vol = arr[t_idx].copy()
            else:
                vol = arr.copy()

            y = self.block_size
            while y < H:
                vol = self._stitch_seam(vol, la_y, y, ov, H)
                y += self.block_size

            x = self.block_size
            while x < W:
                vol = self._stitch_seam(vol, la_x, x, ov, W)
                x += self.block_size

            if la_t is not None:
                arr[t_idx] = vol
            else:
                arr[:] = vol
        return arr

    def _stitch_seam(self, vol, axis, pos, ov, dim_size):
        """Match and remap labels across a single tile seam at `pos` along `axis`."""
        half = min(ov, pos, dim_size - pos)
        if half <= 0:
            return vol

        make_idx = lambda sl: tuple(sl if i == axis else slice(None) for i in range(vol.ndim))
        left_zone  = vol[make_idx(slice(pos - half, pos))]
        right_zone = vol[make_idx(slice(pos, pos + half))]

        labels_l = np.unique(left_zone[left_zone > 0])
        labels_r = np.unique(right_zone[right_zone > 0])
        if len(labels_l) == 0 or len(labels_r) == 0:
            return vol

        a = np.where(np.isin(vol, labels_l), vol, 0)
        b = np.where(np.isin(vol, labels_r), vol, 0)
        iou_mat, lab_l, lab_r = self._compute_iou_matrix(a, b)
        if iou_mat.size == 0:
            return vol

        for j, lb_r in enumerate(lab_r):
            best_i = int(np.argmax(iou_mat[:, j]))
            if iou_mat[best_i, j] >= self.label_overlap:
                vol[vol == lb_r] = lab_l[best_i]
        return vol

    def _smooth_labels(self, vol, sigma, is_3d):
        """Round each label's XY outline by `sigma` px, without letting it take a neighbour's pixels.

        Blur the label's binary mask and re-threshold at 0.5 — the standard boundary smoother, and the
        one that gives a σ knob rather than a structuring-element size, so "a tiny bit" is expressible.

        Two constraints make it safe to run on a whole label image rather than one mask:

        * **A label may only occupy pixels that were background or its own.** Otherwise a smoothed
          label bulges into the neighbour it touches and quietly steals area — on a cytoplasmic
          reporter, where cells touch constantly, that is a measurement error rather than a cosmetic
          one. The consequence to accept knowingly: a boundary SHARED by two labels does not move, so
          this rounds the free outline and leaves genuine contacts alone.
        * **The guard reads the ORIGINAL volume**, so no label's result depends on how many were
          processed before it. Deterministic, and independent of label numbering.

        XY only in 3D (`sigma_z = 0`). The wrinkle is in the in-plane outline, and voxels here are ~6x
        anisotropic (2.0 µm z against 0.33 xy), so blurring across z would move an object between
        planes — a much bigger change than the one being asked for.

        A label that would vanish entirely is left as it was: smoothing is cosmetic and must never be
        a size filter (that is `minCellSize`, which is explicit about it).
        """
        if sigma <= 0:
            return vol
        sigma_vec = (0.0, sigma, sigma) if is_3d else (sigma, sigma)
        pad = int(np.ceil(3 * sigma))
        out = vol.copy()
        for lb_idx, sl in enumerate(ndimage.find_objects(vol)):
            if sl is None:
                continue
            lb = lb_idx + 1
            grown = tuple(slice(max(0, s.start - pad), min(dim, s.stop + pad))
                          for s, dim in zip(sl, vol.shape))
            sub = vol[grown]
            mask = (sub == lb)
            new = ndimage.gaussian_filter(mask.astype(np.float32), sigma_vec) > 0.5
            new &= (sub == 0) | mask          # background or self, never a neighbour
            if not new.any():
                continue
            out_sub = out[grown]
            out_sub[mask & ~new] = 0          # pixels the label gave up
            out_sub[new & (sub == 0)] = lb    # pixels it gained, from background only
        return out

    def _erode_labels(self, vol, amount, is_3d):
        """Erode each label independently by `amount` pixels."""
        from skimage.morphology import disk, ball
        struct = ball(amount) if is_3d else disk(amount)
        out = np.zeros_like(vol)
        for lb in np.unique(vol[vol > 0]):
            eroded = ndimage.binary_erosion(vol == lb, struct)
            out[eroded] = lb
        return out

    # ── Nucleus-cytoplasm matching ────────────────────────────────────────────

    def _match_nuc_cyto(self, cyto, nuc, la_t, T):
        """Re-assign nuc label IDs to match cyto IDs by IoU, per timepoint."""
        cyto_out = cyto.copy()
        nuc_out = np.zeros_like(nuc)

        for t in range(T):
            if la_t is not None:
                idx = tuple(t if i == la_t else slice(None) for i in range(cyto.ndim))
                c_vol = cyto[idx]
                n_vol = nuc[idx]
            else:
                c_vol = cyto
                n_vol = nuc

            iou_mat, labels_c, labels_n = self._compute_iou_matrix(c_vol, n_vol)
            new_nuc = np.zeros_like(n_vol)
            matched = set()

            for i, lc in enumerate(labels_c):
                if len(labels_n) == 0:
                    break
                j = int(np.argmax(iou_mat[i]))
                if iou_mat[i, j] >= self.match_threshold:
                    new_nuc[n_vol == labels_n[j]] = lc
                    matched.add(lc)

            if la_t is not None:
                nuc_out[idx] = new_nuc
                if self.remove_unmatched:
                    c_copy = cyto_out[idx].copy()
                    for lc in labels_c:
                        if lc not in matched:
                            c_copy[c_copy == lc] = 0
                    cyto_out[idx] = c_copy
            else:
                nuc_out[:] = new_nuc
                if self.remove_unmatched:
                    for lc in labels_c:
                        if lc not in matched:
                            cyto_out[cyto_out == lc] = 0

        return cyto_out, nuc_out

    # ── Skipping the padding a drift correction added ─────────────────────────
    #
    # A drift-corrected canvas holds each frame at its own offset and zeroes the rest: 3-56% of the
    # z planes across the movies on this machine, 8 valid planes in an 18-plane canvas at worst. The
    # whole z-stack goes to cellpose in ONE call (it stitches across z internally), so those planes
    # cost real GPU time and produce nothing.
    #
    # Safe because a valid box is a CONTIGUOUS [start, stop) by construction, so restricting z to it
    # can only drop LEADING/TRAILING planes. Interior planes inside the span are kept, and the
    # dropped ones are all-zero, so they yield no labels for `stitch_threshold` to link across —
    # the stitching semantics inside the span are unchanged rather than merely assumed to be.
    #
    # Deliberately NOT a crop: the output store keeps its full shape and the skipped planes stay
    # zero. Each frame sits at its own offset because the correction aligned them in a shared
    # canvas; cropping per frame would put them back out of register.
    #
    # The skip is a property of the BASE, not of any one algorithm: `predict_from_zarr` narrows what
    # it hands `predict_slice` and puts the labels back at the offset, so every subclass gets it by
    # implementing nothing. What a subclass must be able to rely on is that EVERY array it is handed
    # for one timepoint — the tile and, when `TEMPORAL_RADIUS > 0`, the temporal window — is narrowed
    # to the same span. It was not: the window was read from the full store and came back full depth
    # while the tile was narrowed, which broke coastal on any drift-corrected 3D image.
    @staticmethod
    def _narrow_axis(arr, axis, lo, hi):
        """`arr` restricted to `[lo, hi)` along `axis` — for an array already in RAM.

        The window frames are narrowed by `_extract_tile`'s `z_idx=`/`z=` instead, which is the same
        restriction pushed into the store index so the padded planes are never read at all. That is
        only possible where the read has not happened yet, which is why there are two spellings.
        """
        if (lo, hi) == (0, arr.shape[axis]):
            return arr
        return arr[tuple(slice(lo, hi) if i == axis else slice(None) for i in range(arr.ndim))]

    @staticmethod
    def _narrow_z(arr, axis, z0, z1):
        """`arr` restricted to z planes `[z0, z1)`. The z spelling of `_narrow_axis`."""
        return SegmentationUtils._narrow_axis(arr, axis, z0, z1)

    @staticmethod
    def _valid_span(box, axis, n, min_span=2):
        """`(lo, hi)` of `axis` to segment for one timepoint, given that frame's valid `box`.

        The axis-generic form of `_valid_z_span` — the rule is a property of the BOX, not of z, and a
        drift correction pads XY exactly as it pads Z. How much that matters is per-image and worth
        measuring rather than assuming: on a 5.8 px-drift movie the XY padding is 0.4% and not worth
        the read, while on a 139.9 px-drift one the canvas is 605x617 around a 512x512 frame and 30%
        of every cellpose pass is padding (zolIMa/Dml3RG and WIaUjL/p6t4mC respectively, measured).

        See `_valid_z_span` for the safety argument, which is the same on every axis: a valid box is
        a contiguous [start, stop), so narrowing can only drop LEADING/TRAILING slices, and those are
        all-zero. Every ambiguous case widens rather than narrows.
        """
        if not box or n <= 0:
            return 0, n
        rng = box.get(axis)
        if not rng:
            return 0, n
        lo, hi = int(rng[0]), int(rng[1])
        lo, hi = max(0, lo), min(int(n), hi)
        if hi - lo < max(1, min_span) or (lo == 0 and hi == n):
            return 0, n
        return lo, hi

    @staticmethod
    def _valid_z_span(box, n_z, min_span=2):
        """`(z0, z1)` of z to segment for one timepoint, given that frame's valid `box`.

        Returns the whole stack whenever there is nothing trustworthy to narrow to — no box, no Z in
        it, a degenerate range, or a span so thin that segmenting it is not meaningfully 3D. Doing
        MORE work is always the safe direction here: the cost of a wrong narrow span is missing
        cells, and the cost of a wrong wide one is the status quo.

        The thin-span branch is a **safety net, not a live path**: drift places each frame whole, so
        a real box is always the SOURCE depth (8, 13 or 31 planes across the stores on this machine
        — never below 2). Measured 2026-08-12; the invariant behind it is pinned by
        `test_drift_geometry.py::test_every_frames_z_span_is_the_source_depth`, so if a future
        producer starts emitting thin boxes that test fails rather than this guard quietly
        switching the skip off.

        Kept as the named z spelling — it carries the z-specific rationale above, and it is what the
        `_valid_z_span` tests address. The rule itself lives in `_valid_span`.
        """
        return SegmentationUtils._valid_span(box, 'Z', n_z, min_span=min_span)

    def _compute_iou_matrix(self, a, b):
        """IoU matrix between all pairs of non-zero labels in a and b.

        ONE co-occurrence histogram over the paired label maps, not a full-plane boolean op per
        label pair — O(pixels) instead of O(labels²) array comparisons. `_match_nuc_cyto` calls
        this once per timepoint, so the old form cost ~27 s/frame at 400×400 labels (≈90 min on a
        201-frame two-model movie) purely to re-assign label IDs.

        The IoU values are identical to the pairwise form; `IouMatrixOracleTest` pins the two
        against each other, so `match_threshold`/`removeUnmatched` behaviour is unchanged.
        """
        labels_a = np.unique(a[a > 0])
        labels_b = np.unique(b[b > 0])
        na, nb = len(labels_a), len(labels_b)
        if na == 0 or nb == 0:
            return np.zeros((na, nb), dtype=np.float32), labels_a, labels_b

        # Label VALUES are arbitrary and non-contiguous (cellpose leaves gaps), so histogram over
        # dense 0..n-1 indices rather than the raw ids — searchsorted is exact here because
        # np.unique returns sorted values and every pixel below is drawn from that same set.
        both = (a > 0) & (b > 0)
        ia = np.searchsorted(labels_a, a[both]).astype(np.int64)
        ib = np.searchsorted(labels_b, b[both]).astype(np.int64)
        inter = np.bincount(ia * nb + ib, minlength=na * nb).reshape(na, nb)

        # Union needs no second pass over the volume: |A| + |B| - |A∩B| from the per-label totals.
        counts_a = np.bincount(np.searchsorted(labels_a, a[a > 0]), minlength=na)
        counts_b = np.bincount(np.searchsorted(labels_b, b[b > 0]), minlength=nb)

        # Only pairs that actually co-occur can have IoU > 0, and there are O(labels) of those, not
        # O(labels²) — so index them rather than building a second dense matrix for the union.
        iou_mat = np.zeros((na, nb), dtype=np.float32)
        ii, jj = np.nonzero(inter)
        overlap = inter[ii, jj]
        iou_mat[ii, jj] = overlap / (counts_a[ii] + counts_b[jj] - overlap)

        return iou_mat, labels_a, labels_b

    # ── Zarr output ───────────────────────────────────────────────────────────

    def _open_label_store(self, out_path, label_shape, label_axes, nscales):
        """Create the label multiscales group + an EMPTY level-0 array on disk, streamed one frame
        at a time by predict_from_zarr. Returns ``(group, level0, chunks)``. Writes the shared NGFF
        metadata (see zarr_utils.multiscales_metadata) — one layout for image and label stores;
        ``label_axes`` already excludes the channel axis.

        ``out_path`` is a STAGING path handed over by ``zarr_utils.staged_store`` — never the final
        store path. Nothing is cleared here: the staging path is guaranteed absent on entry, and
        clearing the final path is exactly the truncation that staging exists to prevent."""
        dim_utils = self.dim_utils
        full_scale = dim_utils.im_scale()  # one value per image axis (including C)
        # Map base scale by axis NAME so it survives the label array dropping the channel axis.
        ax_to_scale = {ax: full_scale[i] for i, ax in enumerate(dim_utils.im_dim_order)}

        # A label set INHERITS the format of the image it segments (ZARR_V3_PLAN D9) — a v3 image must
        # not acquire v2 labels. The CODEC is not inherited: labels are plain zstd for a measured reason
        # (LABEL_COMPRESSOR), so format and codec stay separate axes.
        enc = zarr_utils.store_encoding_of(self.params.get('imPath'))
        fmt = enc['zarr_format']
        g = zarr.open_group(out_path, mode='w', zarr_format=fmt)
        ms_meta = zarr_utils.multiscales_metadata(
            label_axes, nscales, scale_for_axis=ax_to_scale)
        # Same stamp as the image writers — where this format keeps it, versioned.
        zarr_utils.write_multiscales_attrs(g, ms_meta, fmt)

        chunks = self._label_chunks(tuple(label_shape), label_axes)
        level0 = g.create_array('0', shape=tuple(label_shape),
                                chunks=chunks, dtype=self.LABEL_DTYPE,
                                **zarr_utils._codec_kwargs('labels', fmt, separator=enc['separator']))
        return g, level0, chunks

    def _finalize_label_pyramid(self, g, level0, label_axes, nscales, chunks):
        """Build downsampled label pyramid levels from the on-disk level 0 (bounded per timepoint).
        Labels have no channel axis, so pass explicit X/Y/T indices into the shared pyramid writer
        rather than the image dim_utils."""
        la_y = label_axes.index('Y')
        la_x = label_axes.index('X')
        la_t = label_axes.index('T') if 'T' in label_axes else None
        zarr_utils.write_multiscale_pyramid(
            g, level0, None, nscales, list(chunks),
            x_idx=la_x, y_idx=la_y, t_idx=la_t, kind='labels')

    def _label_chunks(self, shape, label_axes):
        return tuple(
            min(shape[i], 512) if ax in ('Y', 'X') else 1
            for i, ax in enumerate(label_axes)
        )
