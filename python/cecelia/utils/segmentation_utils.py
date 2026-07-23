"""
Base class for tiled image segmentation.

Handles the common loop (T × XY tiles), global label ID management,
post-processing, nucleus-cytoplasm matching, and label zarr writing.

Subclasses implement predict_slice() for specific algorithms (cellpose,
stardist, etc.).
"""

import os
import shutil
import numpy as np
import dask.array as da
import zarr

import cecelia.utils.zarr_utils as zarr_utils
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

    def __init__(self, params, dim_utils):
        self.params = params
        self.dim_utils = dim_utils
        self.block_size = int(params.get('blockSize', 512))
        self.overlap = int(params.get('overlap', 64))
        # Z tiling — 0 means no Z tiling (whole stack passed to cellpose, which uses stitch_threshold internally)
        self.block_size_z = int(params.get('blockSizeZ', 0))
        self.overlap_z = int(params.get('overlapZ', 0))
        # IoU threshold for tile seam stitching; 0 = simple np.maximum merge (no seam matching)
        self.label_overlap = float(params.get('labelOverlap', 0.0))
        self.match_threshold = float(params.get('matchThreshold', 0.3))
        self.remove_unmatched = bool(params.get('removeUnmatched', False))
        self.min_cell_size = int(params.get('minCellSize', 0))
        self.cell_size_max = int(params.get('cellSizeMax', 0))
        self.label_expansion = int(params.get('labelExpansion', 0))
        self.label_erosion = int(params.get('labelErosion', 0))
        self.clear_touching_border = bool(params.get('clearTouchingBorder', False))
        self.clear_depth = bool(params.get('clearDepth', False))
        self.normalise_to_whole = bool(params.get('normaliseToWhole', True))
        self.task_dir = params['taskDir']
        self.output_value_name = params.get('outputValueName', 'default')

    def predict_slice(self, tile, model_params, norm_params=None):
        """Override in subclass. tile=[C,Z,Y,X] or [C,Y,X]. Returns uint32 label mask."""
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

        # Input-image frame axes = image axes without T — KEEPS the channel axis, so its Y/X indices
        # differ from the (channel-less) label frame's. Used to tile the in-RAM input frame.
        in_axes = [ax for ax in dim_utils.im_dim_order if ax != 'T']
        ifa_y = in_axes.index('Y')
        ifa_x = in_axes.index('X')

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

        # Open one on-disk store per label type up front; frames are streamed into level 0 below.
        stores = {ma: self._open_label_store(_store_path(ma), label_shape, label_axes, nscales)
                  for ma in match_as_list}
        counts = {ma: 0 for ma in match_as_list}

        xy_tiles = self._create_xy_tiles(H, W)
        total = T * len(xy_tiles)
        done = 0

        for t in range(T):
            # one frame's labels per type (uint32 zeros), no time axis
            frame = {ma: np.zeros(frame_shape, dtype=self.LABEL_DTYPE) for ma in match_as_list}

            # Read this timepoint's image ONCE into RAM (time axis dropped → frame_axes layout), then
            # tile it in memory. Reading each tile from the store instead re-fetches whole chunks per
            # tile — the over-read the old whole-level fortify() worked around. See
            # zarr_utils.read_timepoint / docs/todo/ZARR_STREAMING_PLAN.md (Phase 1).
            frame_in = zarr_utils.read_timepoint(im_dat[0], dim_utils, t, drop_time=True)

            for read_yx, write_yx, crop_yx in xy_tiles:
                for model_key in sorted(models.keys()):
                    model_params = models[model_key]
                    match_as = model_params.get('matchAs', 'base')
                    norm_p = all_norm_params.get(model_key)

                    # tile from the in-RAM input frame (t_idx=None: no time axis; input-frame Y/X)
                    tile = self._extract_tile(frame_in, 0, None, ifa_y, ifa_x, read_yx)
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
                    frame[ma] = self._stitch_tile_seams(frame[ma], H, W, None, fa_y, fa_x, 1)

            for ma in frame:
                frame[ma] = self._post_process(frame[ma], frame_axes, None, 1, is_3d)

            if 'base' in frame and 'nuc' in frame:
                frame['base'], frame['nuc'] = self._match_nuc_cyto(
                    frame['base'], frame['nuc'], None, 1)

            # Stream each type's frame to disk and tally its (globally unique) label IDs. IDs never
            # repeat across timepoints (max_labels is monotonic), so per-frame counts sum to the
            # whole-stack distinct-ID count the previous code returned.
            for ma in match_as_list:
                _, level0, _ = stores[ma]
                if store_la_t is not None:
                    sl = tuple(t if i == store_la_t else slice(None) for i in range(level0.ndim))
                    level0[sl] = frame[ma]
                else:
                    level0[:] = frame[ma]
                counts[ma] += count_labels(frame[ma])

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

    def _extract_tile(self, im_data, t, t_idx, y_idx, x_idx, read_yx):
        """Extract one XY tile for timepoint t. Returns numpy array."""
        idx = [slice(None)] * len(im_data.shape)
        if t_idx is not None:
            idx[t_idx] = t
        idx[y_idx] = read_yx[0]
        idx[x_idx] = read_yx[1]
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
        """Merge tile into label array via np.maximum."""
        idx = [slice(None)] * arr.ndim
        if la_t is not None:
            idx[la_t] = t
        idx[la_y] = write_yx[0]
        idx[la_x] = write_yx[1]
        idx = tuple(idx)
        arr[idx] = np.maximum(arr[idx], masks)

    # ── Normalisation ─────────────────────────────────────────────────────────

    def _compute_norm_params(self, im_dat, model_params):
        """Per-channel percentile clipping range for scale-to-whole normalisation.

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
        channels = [int(c) for c in (list(model_params.get('cellChannels', [])) +
                                     list(model_params.get('nucChannels', [])))]
        result = {}

        if len(im_dat) == 1:
            # bounded streaming histogram over the (single, full-res) level
            level = im_dat[0]
            darr = level if isinstance(level, da.Array) else da.from_array(level)
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

    def _post_process(self, arr, label_axes, la_t, T, is_3d):
        """Apply erosion, expansion, min-size filter, and border clearing."""
        for t in range(T):
            if la_t is not None:
                idx = tuple(t if i == la_t else slice(None) for i in range(arr.ndim))
                vol = arr[idx].copy()
            else:
                vol = arr.copy()

            if self.label_erosion > 0:
                vol = self._erode_labels(vol, self.label_erosion, is_3d)

            if self.label_expansion > 0:
                vol = segmentation.expand_labels(vol, self.label_expansion)

            if self.min_cell_size > 0 or self.cell_size_max > 0:
                labels, counts = np.unique(vol[vol > 0], return_counts=True)
                for lb, cnt in zip(labels, counts):
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
                        vol[z] = segmentation.clear_border(vol[z])
                else:
                    vol = segmentation.clear_border(vol)

            if la_t is not None:
                arr[idx] = vol
            else:
                arr[:] = vol

        return arr

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

    def _compute_iou_matrix(self, a, b):
        """IoU matrix between all pairs of non-zero labels in a and b."""
        labels_a = np.unique(a[a > 0])
        labels_b = np.unique(b[b > 0])
        if len(labels_a) == 0 or len(labels_b) == 0:
            return np.zeros((len(labels_a), len(labels_b))), labels_a, labels_b

        a_counts = {int(la): int(np.sum(a == la)) for la in labels_a}
        b_counts = {int(lb): int(np.sum(b == lb)) for lb in labels_b}
        iou_mat = np.zeros((len(labels_a), len(labels_b)), dtype=np.float32)

        for i, la in enumerate(labels_a):
            a_mask = a == la
            for j, lb in enumerate(labels_b):
                inter = int(np.sum(a_mask & (b == lb)))
                if inter > 0:
                    union = a_counts[int(la)] + b_counts[int(lb)] - inter
                    iou_mat[i, j] = inter / union

        return iou_mat, labels_a, labels_b

    # ── Zarr output ───────────────────────────────────────────────────────────

    def _open_label_store(self, out_path, label_shape, label_axes, nscales):
        """Create the label multiscales group + an EMPTY level-0 array on disk, streamed one frame
        at a time by predict_from_zarr. Returns ``(group, level0, chunks)``. Writes the shared NGFF
        metadata (see zarr_utils.multiscales_metadata) — one layout for image and label stores;
        ``label_axes`` already excludes the channel axis."""
        if os.path.exists(out_path):
            shutil.rmtree(out_path)

        dim_utils = self.dim_utils
        full_scale = dim_utils.im_scale()  # one value per image axis (including C)
        # Map base scale by axis NAME so it survives the label array dropping the channel axis.
        ax_to_scale = {ax: full_scale[i] for i, ax in enumerate(dim_utils.im_dim_order)}

        g = zarr.open_group(out_path, mode='w', zarr_format=2)
        g.attrs['multiscales'] = zarr_utils.multiscales_metadata(
            label_axes, nscales, scale_for_axis=ax_to_scale)

        chunks = self._label_chunks(tuple(label_shape), label_axes)
        level0 = g.create_array('0', shape=tuple(label_shape),
                                chunks=chunks, dtype=self.LABEL_DTYPE)
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
            x_idx=la_x, y_idx=la_y, t_idx=la_t)

    def _label_chunks(self, shape, label_axes):
        return tuple(
            min(shape[i], 512) if ax in ('Y', 'X') else 1
            for i, ax in enumerate(label_axes)
        )
