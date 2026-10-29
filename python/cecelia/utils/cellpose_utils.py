"""
Cellpose segmentation subclass.

Implements predict_slice() for BOTH cellpose 4 (Cellpose-SAM) and cellpose 3 (cyto2/cyto3). The
version is detected once at import from the package metadata; the runner runs under exactly one
env at a time (default = v4, opt-in `cellpose-v3` on Mac = v3), so the branching is inside the
`predict_slice` method rather than two files.

Three things about v4 shape this file, all verified against `cellpose==4.2.1.1`:

* **`model_type=` is dead.** It is accepted and IGNORED with a log warning, as is any unknown
  `pretrained_model` name — which then silently loads `cpsam_v2`. Nothing here may rely on either:
  the model always arrives as a v4 built-in name or a checkpoint path (resolved in `cellpose.jl`,
  which rejects the v3 zoo up front).
* **`channels=` is dead.** v4 takes channels in any order; `channel_axis` is the only control, and
  input is coerced to exactly 3 channels (fewer are zero-padded, more are truncated with a warning).
* **`z_axis` is only legal in 3D mode.** `eval(z_axis=0, do_3D=False, stitch_threshold=0)` raises
  `ValueError: 2D image processing selected, but z_axis is not None` — the exact call v3 took for
  "independent 2D slices". A Z stack with no stitching therefore goes through the list-of-planes
  form instead, which returns per-plane independent labels (the same thing v3 did).

Two things about cellpose's Z stitching, both versions (`utils.stitch3D`, unchanged in 4.2.1.1 and on
upstream main):

* **It reuses label ids across an empty plane.** Until the first pair of adjacent planes has been
  matched, an empty plane resets the running maximum id and the next plane keeps its raw ids 1..k,
  which are already taken by cells on the planes above. Two unrelated cells several planes apart
  then share ONE label, and its centroid lands in the empty space between them. Sparse labelling
  (few cells over a few Z planes) hits it often. A stitched cell is z-contiguous by construction,
  so `split_z_gaps` gives every z-run its own id.
* **Its cost grows with the square of the tile area** (a dense overlap matrix per plane pair), so
  the v4 path no longer calls it: planes are segmented separately and joined by `stitch_planes`,
  the same rule at sparse cost. The cellpose 3 path still uses it.

Cellpose 3 (opt-in `cellpose-v3` env, Mac only — MPS is much faster on the v3 CNNs than on the v4
transformer):

* **Uses `model_type='cyto2' | 'cyto3'`** — the positional zoo names, which v4 rejected. Only these
  two are shipped; custom v3 checkpoints are not on the drop path today.
* **`channels=[cyto, nuc]` is REQUIRED** — 1-indexed positional pair. `[0, 0]` means grayscale
  (no nuc); `[1, 2]` means "cell channel is axis 0, nuclear channel is axis 1" in the last-axis
  stack this file already assembles.
* **`z_axis=0, do_3D=False` gives independent 2D slices** (the exact call v4 rejects). With
  `stitch_threshold>0`, cellpose stitches labels across Z. Return is `(masks, flows, styles)`.

See docs/todo/CELLPOSE_V4_PLAN.md and docs/todo/CELLPOSE_V3_OPTIN_PLAN.md.
"""

import importlib.metadata

import numpy as np
from scipy import ndimage
from skimage import filters

from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.utils.gpu_utils import torch_device
import cecelia.utils.script_utils as script_utils


def _cellpose_major_version() -> int:
    """`3` under the `cellpose-v3` opt-in env, `4` under the default env. Read from the installed
    distribution's metadata: neither cellpose 3 nor 4 defines `cellpose.__version__`, and reading
    that sent every v3 run down the v4 path (#1552). Any failure falls back to 4."""
    try:
        return int(importlib.metadata.version('cellpose').split('.')[0])
    except Exception:
        return 4


_CELLPOSE_MAJOR = _cellpose_major_version()
_CELLPOSE_V3_BUILTINS = ('cyto2', 'cyto3')


def split_z_gaps(masks):
    """Give every z-contiguous run of a label its own id, so one label is one object.

    The repair for cellpose's `stitch3D` id reuse (see the module docstring). Exact for it rather than
    a heuristic: stitching only ever links ADJACENT planes, so a label with an empty plane inside its
    z-range was never stitched; it is two cells that collided. The lowest run keeps the id and the
    others get fresh ones above the current maximum. A label with no gap is left alone, so a correct
    stitch passes through unchanged.

    masks: [Z, Y, X] labels. Returns a uint32 copy.
    """
    masks = np.array(masks, dtype=np.uint32)
    nxt = int(masks.max()) + 1
    for lb, sl in enumerate(ndimage.find_objects(masks), 1):
        if sl is None:
            continue
        sub = masks[sl]                                   # a view: writes land in `masks`
        runs, n = ndimage.label((sub == lb).any(axis=(1, 2)))
        for r in range(2, n + 1):
            zs = np.flatnonzero(runs == r)
            block = sub[zs[0]:zs[-1] + 1]                 # one run, so no other piece of `lb` inside
            block[block == lb] = nxt
            nxt += 1
    return masks


def stitch_planes(planes, stitch_threshold):
    """Join per-plane 2D labels into 3D objects: cellpose's `stitch3D` rule, at sparse cost.

    The rule is `stitch3D`'s, plane pair by plane pair:
    - a pair below `stitch_threshold` IoU does not count;
    - each label on the plane below keeps only its best match(es) on the plane above;
    - each label above takes the lowest-numbered label below among what is left;
    - an unmatched label gets a fresh id.

    Two differences, both in cost or ids, never in which pixels form an object:

    * **Sparse.** `stitch3D` builds a DENSE overlap matrix of (labels on this plane) × (the largest id
      over every plane stitched so far), so its time and memory grow with the square of the tile
      area (measured in `docs/SEGMENTATION.md` → *Tiling*). Here only the label pairs that actually
      share a pixel are counted, which is O(pixels) per plane pair.
    * **No id reuse across an empty plane.** `stitch3D` lets the plane after an empty one keep its
      raw ids until the first match (module docstring). Here an unmatched label always gets a fresh
      id, so `split_z_gaps` has nothing to repair; it still runs, and passes this through unchanged.

    planes: [Z, Y, X] per-plane labels (0 = background). Returns uint32 [Z, Y, X].
    """
    masks = np.array(planes, dtype=np.uint32)
    if masks.shape[0] < 2:
        return masks
    next_id = int(masks[0].max()) + 1
    for z in range(1, masks.shape[0]):
        below, above = masks[z - 1], masks[z]
        n_above = int(above.max())
        if n_above == 0:
            continue
        lut = np.zeros(n_above + 1, dtype=np.uint32)
        both = (below > 0) & (above > 0)
        if both.any():
            # Every co-occurring (above, below) pair and its pixel count, then IoU from the areas.
            pairs, inter = np.unique(np.stack([above[both], below[both]]), axis=1,
                                     return_counts=True)
            a_lab, b_lab = pairs[0].astype(np.int64), pairs[1].astype(np.int64)
            a_area = np.bincount(above.ravel(), minlength=n_above + 1)
            b_ids, b_counts = np.unique(below[below > 0], return_counts=True)
            b_area = b_counts[np.searchsorted(b_ids, b_lab)]
            iou = inter / (a_area[a_lab] + b_area - inter)
            iou[iou < stitch_threshold] = 0.0
            # each label below keeps only its best match(es) — `iou[iou < iou.max(axis=0)] = 0`
            b_best = np.zeros(int(b_lab.max()) + 1)
            np.maximum.at(b_best, b_lab, iou)
            iou[iou < b_best[b_lab]] = 0.0
            keep = iou > 0
            # each label above takes its highest-IoU partner, the lowest id on a tie (`argmax`)
            order = np.lexsort((b_lab[keep], -iou[keep], a_lab[keep]))
            a_k, b_k = a_lab[keep][order], b_lab[keep][order]
            first = np.ones(len(a_k), dtype=bool)
            first[1:] = a_k[1:] != a_k[:-1]
            lut[a_k[first]] = b_k[first]
        present = np.zeros(n_above + 1, dtype=bool)
        present[np.unique(above)] = True
        present[0] = False
        fresh = np.flatnonzero(present & (lut == 0))
        lut[fresh] = np.arange(next_id, next_id + len(fresh), dtype=np.uint32)
        next_id += len(fresh)
        masks[z] = lut[above]
    return masks


def _refuse_unknown_v4_model(model_type):
    """Raise unless cellpose 4 can load `model_type` AS ITSELF — a v4 built-in or a checkpoint file.

    Cellpose 4 answers any other name with a log warning and `cpsam_v2`, so without this a cellpose 3
    model reaching the default env (a preview worker started there, #1555) segments with SAM under the
    cyto3 label. Julia routes v3 names to the `cellpose-v3` env; this is the backstop for anything that
    gets here anyway — run or preview.
    """
    import os
    if model_type in _CELLPOSE_V4_BUILTINS or os.path.isfile(str(model_type)):
        return
    if model_type in _CELLPOSE_V3_BUILTINS:
        raise ValueError(
            f'{model_type!r} is a cellpose 3 model and this process runs cellpose 4 (the default env). '
            "It runs in the 'cellpose-v3' env — install it from the task's page (Install under the model parameter).")
    raise ValueError(
        f'Cellpose model {model_type!r} is neither a cellpose 4 built-in '
        f'({", ".join(_CELLPOSE_V4_BUILTINS)}) nor a checkpoint file; cellpose 4 would silently '
        'load cpsam_v2 in its place.')


#: Cellpose 4 built-in names — `BUILTIN_CELLPOSE_MODELS` with backend `:v4` (app/src/config/models.jl).
_CELLPOSE_V4_BUILTINS = ('cpsam_v2', 'cpsam')


class CellposeUtils(SegmentationUtils):

    def __init__(self, params, dim_utils):
        super().__init__(params, dim_utils)

        self.use_gpu, self.gpu_device = torch_device()

        self._model_cache = {}   # phys_size_x now comes from the base (px_from_um)

    # ── Model loading ─────────────────────────────────────────────────────────

    def _get_model(self, model_type):
        """Load (or retrieve cached) CellposeModel.

        v4: one branch — built-in name and checkpoint path go through the same `pretrained_model`
        arg. A cellpose 3 checkpoint is rejected by cellpose itself; the message it raises does not
        say which file, so it is re-raised with the path.

        v3: the built-in names (`cyto2`/`cyto3`) go through `model_type=`. Custom v3 checkpoints
        aren't shipped, so path-loading is a future extension — an unrecognised name here in the v3
        env raises with a clear message rather than silently falling through.
        """
        if model_type in self._model_cache:
            return self._model_cache[model_type]

        from cellpose import models

        if _CELLPOSE_MAJOR == 3:
            if model_type not in _CELLPOSE_V3_BUILTINS:
                raise ValueError(
                    f'{model_type!r} is not a supported cellpose-v3 built-in. '
                    f'Shipped v3 models: {", ".join(_CELLPOSE_V3_BUILTINS)}.')
            model = models.CellposeModel(
                gpu=self.use_gpu, device=self.gpu_device,
                model_type=model_type,
            )
        else:
            _refuse_unknown_v4_model(model_type)
            try:
                model = models.CellposeModel(
                    gpu=self.use_gpu, device=self.gpu_device,
                    pretrained_model=model_type,
                )
            except ValueError as e:
                if 'CP4' in str(e):
                    raise ValueError(
                        f'{model_type!r} is a Cellpose 3 checkpoint and cellpose 4 cannot load it. '
                        'It has to be retrained on cellpose 4, or pick a built-in model.') from e
                raise

        self._model_cache[model_type] = model
        return model

    # ── Channel preparation ───────────────────────────────────────────────────

    def _prepare_channel(self, ch_data, model_params, norm_params, ch_idx):
        """Threshold → filter → percentile-normalise a single channel."""
        arr = ch_data.astype(np.float32)

        threshold = int(model_params.get('threshold', 0))
        if threshold > 0:
            arr[arr < threshold] = 0

        med = int(model_params.get('medianFilter', 0))
        if med > 0:
            arr = ndimage.median_filter(arr, size=med)

        gauss = float(model_params.get('gaussianFilter', 0.0))
        if gauss > 0:
            arr = filters.gaussian(arr, sigma=gauss)

        if norm_params and ch_idx in norm_params:
            norm_min, norm_max = norm_params[ch_idx]
        else:
            normalise_perc = float(model_params.get('normalise', 99.9))
            valid = arr[arr > 0]
            if len(valid) > 100:
                norm_min = float(np.percentile(valid, 100 - normalise_perc))
                norm_max = float(np.percentile(valid, normalise_perc))
            else:
                norm_min, norm_max = 0.0, float(arr.max()) or 1.0

        if norm_max > norm_min:
            arr = np.clip((arr - norm_min) / (norm_max - norm_min), 0.0, 1.0)
        else:
            # A flat window has no contrast to segment. Still has to land in [0, 1]: cellpose no
            # longer normalises after us (`normalize=False` in predict_slice).
            arr = np.clip(arr - norm_min, 0.0, 1.0)

        return arr

    # ── Prediction ────────────────────────────────────────────────────────────

    def _model_channels(self, model_params):
        """Cell + nucleus channels — the same defaults `predict_slice` applies, so the read and the
        model can never disagree about which channels a group needs."""
        cell = script_utils.channel_indices(
            model_params.get('cellChannels'), 'cellChannels', 'cellpose_models_for_python (cellpose.jl)') or [0]
        nuc = script_utils.channel_indices(
            model_params.get('nucChannels'), 'nucChannels', 'cellpose_models_for_python (cellpose.jl)')
        return list(cell) + list(nuc)

    def _object_size_um(self, model_params):
        return float(model_params.get('cellDiameter', 15))

    # Measured on `cpsam_v2` (RTX 2000 Ada, 8 GB; docs/SEGMENTATION.md → *Auto tile size*), per XY
    # pixel of the padded tile:
    # - GPU: a fixed ~2.5 GB (model + allocator floor) plus ~580 bytes per pixel AFTER cellpose
    #   rescales the plane to diameter 30 — Z planes go through one at a time, so it is per plane;
    # - RAM: ~70 bytes per pixel for a single plane, ~35 per pixel per plane for a stitched stack.
    _VRAM_FIXED = 2.5e9
    _VRAM_PER_RESCALED_PX = 580.0
    _RAM_PER_PX_2D = 70.0
    _RAM_PER_PX_PER_PLANE = 35.0

    def _tile_cost(self, n_z):
        from cecelia.utils.tile_budget import TileCost
        # The smallest diameter is the costliest group: cellpose upsamples it the most.
        d_px = min(self.px_from_um(self._object_size_um(mp))
                   for mp in (self.params.get('models') or {'0': {}}).values())
        rescale = (30.0 / max(d_px, 1.0)) ** 2
        ram = self._RAM_PER_PX_2D if n_z <= 1 else self._RAM_PER_PX_PER_PLANE * n_z
        return TileCost(ram_per_px=ram, vram_per_px=self._VRAM_PER_RESCALED_PX * rescale,
                        vram_fixed=self._VRAM_FIXED)

    def predict_slice(self, tile, model_params, norm_params=None):
        """
        Run cellpose on one XY tile.

        tile: [C, Z, Y, X] (3D image) or [C, Y, X] (2D)
        Returns: uint32 label array [Z, Y, X] or [Y, X]
        """
        model_type    = model_params.get('model', 'cpsam_v2')
        # indices, not names — see script_utils.channel_indices for what a name here means
        cell_channels = script_utils.channel_indices(
            model_params.get('cellChannels'), 'cellChannels', 'cellpose_models_for_python (cellpose.jl)') or [0]
        nuc_channels = script_utils.channel_indices(
            model_params.get('nucChannels'), 'nucChannels', 'cellpose_models_for_python (cellpose.jl)')
        is_3d = (tile.ndim == 4)  # [C, Z, Y, X]

        # Merge cell channels via np.maximum
        cell_im = None
        for ch in cell_channels:
            ch_data = self.tile_channel(tile, ch)  # [Z, Y, X] or [Y, X]
            prep    = self._prepare_channel(ch_data, model_params, norm_params, ch)
            cell_im = prep if cell_im is None else np.maximum(cell_im, prep)

        # Merge nuc channels
        nuc_im = None
        for ch in nuc_channels:
            ch_data = self.tile_channel(tile, ch)
            prep    = self._prepare_channel(ch_data, model_params, norm_params, ch)
            nuc_im  = prep if nuc_im is None else np.maximum(nuc_im, prep)

        if bool(model_params.get('invert', False)):
            # Dark cells on a bright background. Done here because cellpose's own `invert` lives in the
            # normalisation `normalize=False` switches off below.
            cell_im = 1.0 - cell_im

        # Stack for cellpose: last axis = [cyto, nuc]. v4 zero-pads to its 3 channels itself, and
        # the old `channels=[1,2]` / `[0,0]` pairs are gone — `channel_axis` carries all of it.
        if nuc_im is not None:
            im_input     = np.stack([cell_im, nuc_im], axis=-1)
            channel_axis = -1
        else:
            im_input     = cell_im
            channel_axis = None

        stitch_threshold = float(model_params.get('stitchThreshold', 0.0))

        # Shared by every call below, both versions.
        #   normalize=False — the channels are already windowed to [0, 1] above. Left at cellpose's
        #   default, it re-stretches every tile to that tile's own 1st–99th percentile: a dim
        #   background-only tile comes out as bright as a full one, which undoes `normaliseToWhole`
        #   and makes the `normalise` percentile nearly irrelevant.
        #   flowThreshold defaults to 1.0, not cellpose's 0.4: at 0.4 cpsam_v2 discarded about half the
        #   cells on dense intravital data, and above ~1 the check stops discarding (docs/SEGMENTATION.md
        #   → *Normalisation — ours, never cellpose's*). cellprob keeps cellpose's 0.0.
        eval_kw = dict(
            channel_axis=channel_axis,
            diameter=self.px_from_um(model_params.get('cellDiameter', 15)),
            normalize=False,
            cellprob_threshold=float(model_params.get('cellprobThreshold', 0.0)),
            flow_threshold=float(model_params.get('flowThreshold', 1.0)),
            augment=bool(model_params.get('augment', False)),
        )

        model = self._get_model(model_type)

        if _CELLPOSE_MAJOR == 3:
            # v3 API: 1-indexed positional `channels=[cyto, nuc]`. `[0, 0]` = grayscale (no nuc);
            # `[1, 2]` = cell axis 0 + nuclear axis 1 of the last-axis stack this file builds.
            eval_kw['channels'] = [1, 2] if nuc_im is not None else [0, 0]
            if is_3d and stitch_threshold > 0:
                masks, _, _ = model.eval(im_input, z_axis=0, stitch_threshold=stitch_threshold,
                                         do_3D=False, **eval_kw)
            elif is_3d:
                # v3 accepts `z_axis=0, do_3D=False, stitch_threshold=0` — independent 2D slices,
                # per-plane numbering. This is the call v4 rejects; v3 has no equivalent restriction.
                masks, _, _ = model.eval(im_input, z_axis=0, do_3D=False, **eval_kw)
            else:
                masks, _, _ = model.eval(im_input, **eval_kw)
        elif is_3d and stitch_threshold > 0:
            # v4: 2D per Z plane, stitched across Z. The same three steps as cellpose's own stitched
            # eval (`_compute_masks`): per-plane masks with the size filter OFF (`min_size=-1`; a
            # small 2D piece may belong to a large 3D cell), the stitch, then holes filled and
            # objects under 15 voxels removed on the stitched volume. Only the stitch is ours —
            # `stitch_planes` says why.
            planes = [im_input[z] for z in range(im_input.shape[0])]
            per_plane, _, _ = model.eval(planes, min_size=-1, **eval_kw)
            masks = stitch_planes(np.stack(per_plane), stitch_threshold)
            from cellpose.utils import fill_holes_and_remove_small_masks
            masks = fill_holes_and_remove_small_masks(masks, min_size=15)
        elif is_3d:
            # v4: independent 2D slices. As a list of planes, because the same call with `z_axis=0`
            # and no stitching is a ValueError in v4 (see the module docstring). Labels are numbered
            # per plane, which is what "0 = independent 2D slices" has always meant here.
            planes = [im_input[z] for z in range(im_input.shape[0])]
            per_plane, _, _ = model.eval(planes, **eval_kw)
            masks = np.stack(per_plane)
        else:
            # v4 2D.
            masks, _, _ = model.eval(im_input, **eval_kw)

        if is_3d and stitch_threshold > 0:
            return split_z_gaps(masks)
        return masks.astype(np.uint32)
