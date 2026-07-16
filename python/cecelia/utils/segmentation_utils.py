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
import zarr

import cecelia.utils.zarr_utils as zarr_utils

from skimage import morphology, segmentation
from scipy import ndimage


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
        """Segment: iterate T × XY tiles for each model."""
        models = self.params.get('models', {})
        dim_utils = self.dim_utils

        # Global norm params (from lowest-res level)
        all_norm_params = {}
        if self.normalise_to_whole:
            for key, mp in models.items():
                all_norm_params[key] = self._compute_norm_params(im_dat, mp)

        # Axis indices in image array
        im_shape = list(im_dat[0].shape)
        t_idx = dim_utils.dim_idx('T')
        c_idx = dim_utils.dim_idx('C')
        y_idx = dim_utils.dim_idx('Y')
        x_idx = dim_utils.dim_idx('X')

        T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
        H = dim_utils.dim_val('Y')
        W = dim_utils.dim_val('X')
        is_3d = dim_utils.is_3D()

        # Label array shape: image shape without C
        label_axes = [ax for ax in dim_utils.im_dim_order if ax != 'C']
        label_shape = [im_shape[i] for i, ax in enumerate(dim_utils.im_dim_order) if ax != 'C']

        la_t = label_axes.index('T') if 'T' in label_axes else None
        la_y = label_axes.index('Y')
        la_x = label_axes.index('X')

        # Collect unique matchAs labels in order; 'base' is always the primary type
        match_as_list = list(dict.fromkeys(
            mp.get('matchAs', 'base') for mp in models.values()
        ))

        # In-memory label arrays (uint32 zeros)
        label_arrs = {ma: np.zeros(label_shape, dtype=self.LABEL_DTYPE) for ma in match_as_list}
        max_labels = {ma: 0 for ma in match_as_list}

        xy_tiles = self._create_xy_tiles(H, W)
        total = T * len(xy_tiles)
        done = 0

        for t in range(T):
            for read_yx, write_yx, crop_yx in xy_tiles:
                for model_key in sorted(models.keys()):
                    model_params = models[model_key]
                    match_as = model_params.get('matchAs', 'base')
                    norm_p = all_norm_params.get(model_key)

                    tile = self._extract_tile(im_dat[0], t, t_idx, y_idx, x_idx, read_yx)
                    masks = self.predict_slice(tile, model_params, norm_p)
                    masks = self._crop_masks(masks, crop_yx, is_3d)

                    if np.any(masks > 0):
                        masks[masks > 0] += max_labels[match_as]
                        max_labels[match_as] = int(masks.max())

                    self._write_tile_to_arr(
                        label_arrs[match_as], masks,
                        t, la_t, la_y, la_x, write_yx,
                    )

                done += 1
                print(f'[PROGRESS] {done}/{total}', flush=True)

        # Seam stitching: merge cell IDs split at tile boundaries
        if self.label_overlap > 0:
            for ma in label_arrs:
                label_arrs[ma] = self._stitch_tile_seams(
                    label_arrs[ma], H, W, la_t, la_y, la_x, T)

        # Post-processing per label type
        for ma in label_arrs:
            label_arrs[ma] = self._post_process(label_arrs[ma], label_axes, la_t, T, is_3d)

        # Base-nuc matching: re-assign nuc IDs to match base IDs by IoU
        if 'base' in label_arrs and 'nuc' in label_arrs:
            label_arrs['base'], label_arrs['nuc'] = self._match_nuc_cyto(
                label_arrs['base'], label_arrs['nuc'], la_t, T)

        labels_dir = os.path.join(self.task_dir, 'labels')
        os.makedirs(labels_dir, exist_ok=True)

        nscales = len(im_dat)
        # 'base' → {outputValueName}.zarr; other types → {outputValueName}_{ma}.zarr
        # All written to {task_dir}/labels/
        primary = label_arrs['base'] if 'base' in label_arrs else next(iter(label_arrs.values()))
        self._write_labels_zarr(
            primary,
            os.path.join(labels_dir, f'{self.output_value_name}.zarr'),
            label_axes, nscales)
        for ma, arr in label_arrs.items():
            if ma != 'base':
                self._write_labels_zarr(
                    arr,
                    os.path.join(labels_dir, f'{self.output_value_name}_{ma}.zarr'),
                    label_axes, nscales)

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
        """Compute per-channel percentile clipping range from lowest-res level."""
        low_res = np.asarray(im_dat[-1])
        c_idx = self.dim_utils.dim_idx('C')
        normalise_perc = float(model_params.get('normalise', 99.9))
        result = {}

        all_channels = (list(model_params.get('cellChannels', [])) +
                        list(model_params.get('nucChannels', [])))
        for ch in all_channels:
            idx = [slice(None)] * low_res.ndim
            idx[c_idx] = int(ch)
            ch_data = low_res[tuple(idx)].ravel()
            valid = ch_data[ch_data > 0]
            if len(valid) > 100:
                norm_min = float(np.percentile(valid, 100 - normalise_perc))
                norm_max = float(np.percentile(valid, normalise_perc))
                result[int(ch)] = (norm_min, norm_max)
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

    def _write_labels_zarr(self, arr, out_path, label_axes, nscales):
        """Write label array as multiscale OME-ZARR v2."""
        if os.path.exists(out_path):
            shutil.rmtree(out_path)

        dim_utils = self.dim_utils
        full_scale = dim_utils.im_scale()  # one value per image axis (including C)
        # Map base scale by axis NAME so it survives the label array dropping the channel axis.
        ax_to_scale = {ax: full_scale[i] for i, ax in enumerate(dim_utils.im_dim_order)}

        g = zarr.open_group(out_path, mode='w', zarr_format=2)
        # Shared multiscales builder (see zarr_utils.multiscales_metadata) — one layout for
        # image and label stores. label_axes already excludes C.
        g.attrs['multiscales'] = zarr_utils.multiscales_metadata(
            label_axes, nscales, scale_for_axis=ax_to_scale)

        chunks = self._label_chunks(arr.shape, label_axes)
        g.create_array('0', data=arr.astype(self.LABEL_DTYPE), chunks=chunks)

        la_y = label_axes.index('Y')
        la_x = label_axes.index('X')
        for lvl in range(1, nscales):
            ds_idx = tuple(
                slice(None, None, 2 ** lvl) if i in (la_y, la_x) else slice(None)
                for i in range(arr.ndim)
            )
            g.create_array(str(lvl), data=arr[ds_idx].astype(self.LABEL_DTYPE), chunks=chunks)

    def _label_chunks(self, shape, label_axes):
        return tuple(
            min(shape[i], 512) if ax in ('Y', 'X') else 1
            for i, ax in enumerate(label_axes)
        )
