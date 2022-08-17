import os
import numpy as np
import subprocess
import tifffile

# utils
from py.segmentation_utils import SegmentationUtils

import py.tiff_utils as tiff_utils
from py.imagej_utils import ImagejUtils

class DonbloUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)

    # init fiji
    self.fiji_path = params['fiji_path']
    self.scripts_path = params['scripts_path']
    self.imagej_utils = ImagejUtils(
        self.fiji_path, self.scripts_path
    )

    # init params
    self.blob_channels = list(params['blob_channels'])
    self.donut_channels = list(params['donut_channels'])
    self.cell_radius = params['cell_radius']
    self.gaussian_filter = params['gaussian_filter']
    self.median_filter = params['median_filter']
    self.minimum_filter = params['minimum_filter']
    self.maximum_filter = params['maximum_filter']
    # self.post_variance_filter = params['post_variance_filter']
    self.detection_thresh_adj = params['detection_thresh_adj']
    # self.filtering_after_seg = params['filtering_after_seg']
    self.rolling_radius = params['rolling_radius']
    self.omexml = params['omexml']

  """
  Predict slice
  """
  def predict_slice(self, im_dat, im_slices):
    cur_im_dat = im_dat[im_slices]
    cur_im_path = os.path.join(self.task_dir, 'donblo_tmp.tif')
    cur_seg_path = os.path.join(self.task_dir, 'donblo_tmp.seg.tif')

    # save to disk
    tiff_utils.save_as_tiff(
      cur_im_path,
      self.dim_utils.transpose_array_axis_to_save(cur_im_dat),
      self.dim_utils)

    # segment labels
    self.imagej_utils.run_donblo(
      cur_im_path,
      seg_path = cur_seg_path,
      blob_channels = self.blob_channels,
      donut_channels = self.donut_channels,
      cell_radius = self.cell_radius,
      gaussian_filter = self.gaussian_filter,
      median_filter = self.median_filter,
      maximum_filter = self.maximum_filter,
      minimum_filter = self.minimum_filter,
      detection_thresh_adj = self.detection_thresh_adj,
      # filtering_after_seg = self.filtering_after_seg,
      rolling_radius = self.rolling_radius
      # post_variance_filter = self.post_variance_filter
    )

    # load segmentation if successful
    labels_dat = None
    if os.path.exists(cur_seg_path):
      labels_dat = tifffile.imread(cur_seg_path).astype(np.int)

      # remove labels
      os.unlink(cur_seg_path)

    if labels_dat is not None:
      # transpose array axis to image data
      labels_dat = self.dim_utils.transpose_array_axis_to_show(labels_dat)

    # remove image slice
    os.unlink(cur_im_path)

    return {
      'base': labels_dat
      }
