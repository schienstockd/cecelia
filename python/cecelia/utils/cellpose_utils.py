"""
Cellpose segmentation subclass.

Implements predict_slice() using the cellpose 4 (Cellpose-SAM) `CellposeModel` API.

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

See docs/todo/CELLPOSE_V4_PLAN.md.
"""

import numpy as np
from scipy import ndimage
from skimage import filters

from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.utils.gpu_utils import torch_device
import cecelia.utils.script_utils as script_utils


class CellposeUtils(SegmentationUtils):

    def __init__(self, params, dim_utils):
        super().__init__(params, dim_utils)

        self.use_gpu, self.gpu_device = torch_device()

        self._model_cache = {}   # phys_size_x now comes from the base (px_from_um)

    # ── Model loading ─────────────────────────────────────────────────────────

    def _get_model(self, model_type):
        """Load (or retrieve cached) CellposeModel.

        One branch, not two: v4 takes a built-in name and a checkpoint path through the same
        `pretrained_model` argument. A cellpose 3 checkpoint is rejected by cellpose itself; the
        message it raises does not say which file, so it is re-raised with the path.
        """
        if model_type not in self._model_cache:
            from cellpose import models
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
        return self._model_cache[model_type]

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

        return arr

    # ── Prediction ────────────────────────────────────────────────────────────

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
            ch_data = tile[ch]  # [Z, Y, X] or [Y, X]
            prep    = self._prepare_channel(ch_data, model_params, norm_params, ch)
            cell_im = prep if cell_im is None else np.maximum(cell_im, prep)

        # Merge nuc channels
        nuc_im = None
        for ch in nuc_channels:
            ch_data = tile[ch]
            prep    = self._prepare_channel(ch_data, model_params, norm_params, ch)
            nuc_im  = prep if nuc_im is None else np.maximum(nuc_im, prep)

        # Stack for cellpose: last axis = [cyto, nuc]. v4 zero-pads to its 3 channels itself, and
        # the old `channels=[1,2]` / `[0,0]` pairs are gone — `channel_axis` carries all of it.
        if nuc_im is not None:
            im_input     = np.stack([cell_im, nuc_im], axis=-1)
            channel_axis = -1
        else:
            im_input     = cell_im
            channel_axis = None

        # µm → pixels
        cell_diam_px = self.px_from_um(model_params.get('cellDiameter', 15))

        stitch_threshold = float(model_params.get('stitchThreshold', 0.0))

        model = self._get_model(model_type)

        if is_3d and stitch_threshold > 0:
            # 2D-per-Z-slice, stitched across Z by cellpose. `z_axis` is only accepted here.
            masks, _, _ = model.eval(
                im_input,
                channel_axis=channel_axis,
                z_axis=0,
                diameter=cell_diam_px,
                stitch_threshold=stitch_threshold,
                do_3D=False,
            )
        elif is_3d:
            # Independent 2D slices. As a list of planes, because the same call with `z_axis=0` and
            # no stitching is a ValueError in v4 (see the module docstring). Labels are numbered per
            # plane, which is what "0 = independent 2D slices" has always meant here.
            planes = [im_input[z] for z in range(im_input.shape[0])]
            per_plane, _, _ = model.eval(
                planes,
                channel_axis=channel_axis,
                diameter=cell_diam_px,
            )
            masks = np.stack(per_plane)
        else:
            masks, _, _ = model.eval(
                im_input,
                channel_axis=channel_axis,
                diameter=cell_diam_px,
            )

        return masks.astype(np.uint32)
