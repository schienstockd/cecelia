# https://nbviewer.jupyter.org/github/stardist/stardist/blob/master/examples/2D/3_prediction.ipynb
# https://qupath.readthedocs.io/en/latest/docs/advanced/stardist.html
import numpy as np
import math
import zarr
import os
import json

# utils
from py.segmentation_utils import SegmentationUtils
import py.config_utils as cfg

from csbdeep.utils import Path, normalize
from csbdeep.data import Normalizer, normalize_mi_ma
from stardist.models import StarDist2D

class StarDistUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    # function params
    self.nuclei_channels = params['nuclei_channels']
    
    # normalization range
    self.norm_range = tuple(params['norm_range'])
    
    # use normaliser class
    self.normalizer = SDNormalizer(self.norm_range[0], self.norm_range[1])
    
    # define axes and remove "C" from axis
    self.axes = self.dim_utils.im_dim_order.copy()
    self.axes.remove("C")
    self.axes = "".join(self.axes)
    
    # pre-trained 2D
    # self.model = StarDist2D.from_pretrained('2D_versatile_fluo')
    self.model = StarDist2D(
      None,
      cfg.data['python']['stardist']['model2D']['name'],
      os.path.join(self.ccia_path, cfg.data['python']['stardist']['model2D']['dir']))
    
  """
  Predict slice
  """
  def predict_slice(self, im_dat, dat_slices, norm_im = None):
    # add channel
    dat_slices = list(dat_slices)
    dat_slices[self.dim_utils.dim_idx("C")] = self.nuclei_channels
    dat_slices = tuple(dat_slices)
    
    im = im_dat[dat_slices]
    
    # apply threshold?
    if self.threshold > 0:
      im[im < self.threshold] = self.threshold
      im = im - self.threshold
      
    # apply relative threshold?
    if self.rel_threshold > 0:
      rel_threshold = np.percentile(im, self.rel_threshold)
      im[im < rel_threshold] = rel_threshold
      im = im - rel_threshold
    
    # predict labels
    sd_labels, polys = self.model.predict_instances(
      im, axes = self.axes, normalizer = self.normalizer)
      
    return {
      'base': sd_labels
      }

"""
Create a custom normaliser
https://nbviewer.jupyter.org/github/stardist/stardist/blob/master/examples/other2D/predict_big_data.ipynb
"""
class SDNormalizer(Normalizer):
    def __init__(self, mi, ma):
        self.mi, self.ma = mi, ma
    def before(self, x, axes):
        return normalize_mi_ma(x, self.mi, self.ma, dtype = np.float32)
    def after(*args, **kwargs):
        assert False
    @property
    def do_after(self):
        return False
