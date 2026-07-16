"""
Cellpose segmentation subclass.

Implements predict_slice() using the cellpose CellposeModel API.
"""

import numpy as np
from scipy import ndimage
from skimage import filters

from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.utils.gpu_utils import torch_device


class CellposeUtils(SegmentationUtils):

    def __init__(self, params, dim_utils):
        super().__init__(params, dim_utils)

        self.use_gpu, self.gpu_device = torch_device()

        # Physical pixel size for µm → pixel diameter conversion
        self.phys_size_x = dim_utils.im_physical_size('x', default=1.0)
        self._model_cache = {}

    # ── Model loading ─────────────────────────────────────────────────────────

    def _get_model(self, model_type):
        """Load (or retrieve cached) CellposeModel."""
        if model_type not in self._model_cache:
            from cellpose import models
            if __import__('os').path.isfile(model_type):
                model = models.CellposeModel(
                    gpu=self.use_gpu, device=self.gpu_device,
                    pretrained_model=model_type,
                )
            else:
                model = models.CellposeModel(
                    gpu=self.use_gpu, device=self.gpu_device,
                    model_type=model_type,
                )
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
        model_type    = model_params.get('model', 'cyto3')
        cell_channels = [int(c) for c in model_params.get('cellChannels', [])] or [0]
        nuc_channels  = [int(c) for c in model_params.get('nucChannels',  [])]
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

        # Stack for cellpose: last axis = [cyto, nuc], channels=[1, 2]
        if nuc_im is not None:
            im_input    = np.stack([cell_im, nuc_im], axis=-1)
            cp_channels = [1, 2]
            channel_axis = -1
        else:
            im_input    = cell_im
            cp_channels = [0, 0]
            channel_axis = None

        # µm → pixels
        cell_diam_um = float(model_params.get('cellDiameter', 15))
        cell_diam_px = cell_diam_um / max(self.phys_size_x, 1e-6)

        stitch_threshold = float(model_params.get('stitchThreshold', 0.0))

        model = self._get_model(model_type)

        if is_3d:
            # 2D-per-Z-slice with optional stitch across Z
            masks, _, _ = model.eval(
                im_input,
                channels=cp_channels,
                channel_axis=channel_axis,
                z_axis=0,
                diameter=cell_diam_px,
                stitch_threshold=stitch_threshold,
                do_3D=False,
            )
        else:
            masks, _, _ = model.eval(
                im_input,
                channels=cp_channels,
                channel_axis=channel_axis,
                diameter=cell_diam_px,
            )

        return masks.astype(np.uint32)
