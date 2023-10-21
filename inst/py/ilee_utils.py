import os
import numpy as np

# utils
from py.segmentation_utils import SegmentationUtils
import py.script_utils as script_utils
import py.label_utils as label_utils
import py.config_utils as cfg

# ILEE specifics
# https://github.com/phylars/ILEE_CSK/wiki/Tutorial
# There were some errors coming up with the Github version
# so use an adapted version
import py.ILEE_CSK.ILEE_CSK as ILEE_CSK

class IleeUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    # get params
    self.filament_channels = script_utils.get_param(params, 'filament_channels', default = [])
    self.normalise = script_utils.get_param(params, 'normalise', default = 0)
    self.k1 = script_utils.get_param(params, 'k1', default = 2.5)
    self.k2 = script_utils.get_param(params, 'k2', default = 550)
    
    # TODO ILEE is using MATLAB and CUDA device
    # I am not sure that this will translate to MPS
    # # init params
    # self.gpu_device = None
    # 
    # # TODO is there a better way?
    # # check which GPU to use
    # # Apple M1
    # if torch.backends.mps.is_available():
    #   # self.gpu_device = 'mps'
    #   self.gpu_device = torch.device('mps')

  """
  Predict slice
  """
  def predict_slice(self, im_dat, dat_slices, norm_im = None):
    cur_im_dat = im_dat[dat_slices]
    t_idx = self.dim_utils.dim_idx('T', ignore_channel = True)
    c_idx = self.dim_utils.dim_idx('C', ignore_time = self.integrate_time)
    
    # check whether to integrate time
    if self.dim_utils.is_timeseries() and self.integrate_time is True:
      self.logfile_utils.log('> Average time')
      
      if self.integrate_time_mode == 'avg':
        cur_im_dat = np.average(cur_im_dat, axis = t_idx)
      else:
        cur_im_dat = np.max(cur_im_dat, axis = t_idx)
    
    # get label shape
    label_shape = list(cur_im_dat.shape)
    label_shape.pop(c_idx)
    
    if self.dim_utils.is_timeseries() and self.integrate_time is False:
      # remove time if present
      label_shape.pop(t_idx)
    
    label_shape = tuple(label_shape)
    
    # merge channels
    im = np.zeros(label_shape, dtype = np.uint32)
    for i in self.filament_channels:
      im = np.maximum(im, np.squeeze(np.take(cur_im_dat, i, axis = c_idx)))
      
    # generate diff image
    im[im < np.percentile(im, self.normalise)] = 0
    im = im-im.min()
    im = im.astype('float')
    
    # go through z-slices instead of 3D segmentation
    im_slices = [slice(None) for _ in range(len(im.shape))]
    z_idx = self.dim_utils.dim_idx('z', ignore_channel = True, ignore_time = True)
    diff_im = np.zeros_like(im)
    
    # go through z individually - this seems to work better than 3D
    # TODO but is a lot slower - can you make this parallel?
    for i in range(self.dim_utils.dim_val('z')):
      im_slices = list(im_slices)
      im_slices[z_idx] = slice(i, i+1, 1)
      im_slices = tuple(im_slices)
      
      im_to_process = np.squeeze(im[im_slices])
      
      self.logfile_utils.log(f'> Process Z: {i}')
      
      # otherwise this will fail if there is no signal
      if im_to_process.max() > 0:
        diff_im[im_slices] = ILEE_CSK.ILEE_2d(
          im_to_process,
          k2 = self.k2, k1 = self.k1,
          pL_type = 'pL_8', gauss_dif = True)
    
    return {'base': np.squeeze(diff_im)}

