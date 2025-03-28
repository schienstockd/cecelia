import os
import numpy as np

# utils
from py.segmentation_utils import SegmentationUtils
import py.script_utils as script_utils
import py.label_utils as label_utils
import py.config_utils as cfg

from tqdm import tqdm

import scipy.ndimage as ndi
import skimage.filters
import skimage.morphology
import skimage.measure
import skimage.feature
import skimage.segmentation
import ome_types

# cellpose specifics
from cellpose import models
import torch

class CellposeUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    # init params
    self.models = params['models']
    self.match_threshold = script_utils.get_param(params, 'match_threshold', default = 0.8)
    self.remove_unmatched = script_utils.get_param(params, 'remove_unmatched', default = True)
    
    self.gpu_device = None
    
    # TODO is there a better way?
    # check which GPU to use
    # Apple M1
    if self.use_gpu and torch.backends.mps.is_available():
      # self.gpu_device = 'mps'
      self.gpu_device = torch.device('mps')

  """
  get masks from model
  """
  def get_masks(self, model_name, model, im, cell_diameter, normalise_intensity = False,
                channels = [0, 0], channel_axis = None, z_axis = None, stitch_threshold = 0.0):
    #TODO is there a better way to do this .. ?
    try:
      masks = np.zeros(1)
      
      do_3D = self.dim_utils.is_3D()
      
      # if that does not work well
      if do_3D is True and stitch_threshold > 0.0:
        do_3D = False
        stitch_threshold = stitch_threshold
      elif do_3D is False:
        # do not use stitching when not 3D
        # otherwise the segmentation will not return masks
        stitch_threshold = 0.0
        
      if model_name in cfg.data['python']['cellpose']['models']:
        # masks, flows, styles, diams = model.eval(
        masks, flows, styles = model.eval(
          im, channels = channels, diameter = cell_diameter,
          channel_axis = channel_axis, z_axis = z_axis,
          do_3D = do_3D,
          stitch_threshold = stitch_threshold,
          batch_size = 4,
          augment = False,
          # net_avg = False,
          normalize = normalise_intensity,
          invert = False,
          flow_threshold = 0.99)
      else:
        masks, flows, styles = model.eval(
          im, channels = channels, diameter = cell_diameter,
          channel_axis = channel_axis, z_axis = z_axis,
          do_3D = do_3D,
          stitch_threshold = stitch_threshold,
          # batch_size = 8,
          # augment = True, net_avg = True,
          # normalize = True, invert = False,
          batch_size = 4,
          augment = False,
          # net_avg = False,
          normalize = normalise_intensity,
          invert = False,
          flow_threshold = 0.99)
          # anisotropy=anisotropy) # Does not work
    except ValueError as e:
      self.logfile_utils.log(f'Cellpose Prediction error {e}')
      
    return masks

  """
  Prepare im for run
  """
  def prepare_im(self, im, model_params, normalise_percentile = 99.9, norm_im = None):
    # apply threshold?
    if 'threshold' in model_params.keys() and model_params['threshold'][0] > 0:
      im[im < model_params['threshold'][0]] = model_params['threshold'][0]
      im = im - model_params['threshold'][0]
      
    # apply relative threshold?
    if 'relThreshold' in model_params.keys() and model_params['relThreshold'][0] > 0:
      rel_threshold = np.percentile(im, model_params['relThreshold'][0])
      im[im < rel_threshold] = rel_threshold
      im = im - rel_threshold
      
    # prepare image
    # im_to_predict = im
    im_to_predict = np.squeeze(im)
    
    # apply sum?
    if 'sumFilter' in model_params.keys() and model_params['sumFilter'][0] > 0:
      if self.dim_utils.is_3D():
        sum_selem = skimage.morphology.ball(model_params['sumFilter'][0])
      else:
        sum_selem = skimage.morphology.disk(model_params['sumFilter'][0])
      
      im_to_predict = skimage.filters.rank.sum(im_to_predict, sum_selem)
    
    # apply median?
    if model_params['medianFilter'][0] > 0:
      if self.dim_utils.is_3D():
        median_selem = skimage.morphology.ball(model_params['medianFilter'][0])
      else:
        median_selem = skimage.morphology.disk(model_params['medianFilter'][0])
      
      im_to_predict = skimage.filters.median(im_to_predict, median_selem)
      
    # apply gaussian?
    if model_params['gaussianFilter'][0] > 0:
      im_to_predict = ndi.gaussian_filter(im_to_predict, model_params['gaussianFilter'][0])
    
    # normalise image
    if normalise_percentile > 0:
      # get min/max values for channels
      if norm_im is None:
        max_percentile = np.percentile(im_to_predict, normalise_percentile)
        min_percentile = np.percentile(im_to_predict, 100 - normalise_percentile)
      else:
        max_percentile = np.percentile(norm_im, normalise_percentile)
        min_percentile = np.percentile(norm_im, 100 - normalise_percentile)
      
      # calculate relative values
      im_to_predict = (im_to_predict - min_percentile) / (max_percentile - min_percentile)
      
      # adjust
      im_to_predict[im_to_predict < 0] = 0
      im_to_predict[im_to_predict > 1] = 1
    
    return im_to_predict

  # """
  # Match cells and nuclei
  # TODO this is slow
  # """
  # def match_cells_and_nuclei(self, nuc_im, cyto_im):
  #   # save merged labels
  #   nuc_labels_merged = np.zeros_like(nuc_im)
  #   cyto_labels_merged = np.zeros_like(cyto_im)
  #   
  #   for i in tqdm(np.unique(nuc_im)):
  #     cur_nuc = nuc_im == i
  #     cur_cyto = cur_nuc * cyto_im
  # 
  #     # map onto cytoplasm labels
  #     cyto_label_ids, cyto_label_freq = np.unique(
  #       cur_cyto, return_counts = True
  #     )
  #     
  #     if len(cyto_label_freq) > 1:
  #       # get max index
  #       # ie/ that cell which is overlapping most
  #       max_idx = cyto_label_freq[1:].argmax(axis = 0)
  #       
  #       # get max label
  #       cur_cyto_label = cyto_label_ids[1:][max_idx]
  # 
  #       # assign nucleus and cytoplasm back
  #       cyto_labels_merged = np.maximum(
  #         cyto_labels_merged,
  #         (cyto_im == cur_cyto_label) * cur_cyto_label
  #       )
  #       nuc_labels_merged = np.maximum(
  #         nuc_labels_merged,
  #         (cyto_im == cur_cyto_label) * cur_cyto_label * cur_nuc
  #       )
  #       
  #   self.logfile_utils.log('merging results')
  #   self.logfile_utils.log(np.max(nuc_labels_merged))
  #   self.logfile_utils.log(np.max(cyto_labels_merged))
  #   
  #   return nuc_labels_merged, cyto_labels_merged

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
    
    if norm_im is not None:
      norm_shape = list(norm_im.shape)
      norm_shape.pop(c_idx)
      
      if self.dim_utils.is_timeseries() and self.integrate_time is False:
        # remove time if present
        norm_shape.pop(t_idx)
      
      norm_shape = tuple(norm_shape)

    # save masks in list
    model_masks = {
      'unmatched': list(),
      'cyto': list(),
      'nuc': list()
      }
    
    # go through models
    for i, x in self.models.items():
      # get model
      cp_model = x['model'][0]
      
      self.logfile_utils.log(f'>> Evaluate {i}: {cp_model}')
      self.logfile_utils.log(x['cellChannels'])
      
      # normalise_intensity = x['normalise'][0]
      normalise_percentile = x['normalise'][0]
      
      if len(x['cellChannels']) > 0:
        # TODO this should be one function call for both zero and norm image
        im_to_predict = np.zeros(label_shape, dtype = np.uint32)
        nuc_im_to_predict = None
        norm_cyto_im = None
        norm_nuc_im = None
        
        # prepare normalisation image
        if norm_im is not None:
          norm_cyto_im = np.zeros(norm_shape, dtype = np.uint32)
          
          for y in x['cellChannels']:
            norm_cyto_im = np.maximum(
              norm_cyto_im, np.squeeze(np.take(norm_im, y, axis = c_idx)))
                
          # add nuclei channel?
          if len(x['nucChannels']) > 0:
            norm_nuc_im = np.zeros(norm_shape, dtype = np.uint32)
            
            for y in x['nucChannels']:
              norm_nuc_im = np.maximum(
                norm_nuc_im, np.squeeze(np.take(norm_im, y, axis = c_idx)))
        
        for y in x['cellChannels']:
          im_to_predict = np.maximum(
            # im, np.squeeze(np.take(cur_im_dat, y, axis = c_idx)))
            # TODO is this ok? This will prepare each image before merge
            im_to_predict, self.prepare_im(
              np.squeeze(np.take(cur_im_dat, y, axis = c_idx)),
              x, normalise_percentile = normalise_percentile, norm_im = norm_cyto_im))
              
        # add nuclei channel?
        if len(x['nucChannels']) > 0:
          nuc_im_to_predict = np.zeros(label_shape, dtype = np.uint32)
          
          for y in x['nucChannels']:
            nuc_im_to_predict = np.maximum(
              nuc_im_to_predict, self.prepare_im(
                np.squeeze(np.take(cur_im_dat, y, axis = c_idx)),
                x, normalise_percentile = normalise_percentile, norm_im = norm_cyto_im))
              
        cell_diameter = x['cellDiameter'][0]
        
        # adjust diameter for image resolution
        # cell_diameter /= self.dim_utils.omexml.images[0].pixels.physical_size_x
        scaling_factor = self.dim_utils.im_physical_size('x')
        
        if self.dim_utils.omexml.images[0].pixels.physical_size_x_unit == ome_types.model.UnitsLength.MILLIMETER:
          scaling_factor *= 1000
        
        cell_diameter /= scaling_factor
        
        # prepare images
        # grayscale
        channels = [0, 0]
        channel_axis = None
        z_axis = None
        
        # Done this already above
        # im_to_predict = self.prepare_im(
        #   im, x, normalise_percentile = normalise_percentile, norm_im = norm_cyto_im)
        # nuc_im_to_predict = None
        
        if self.dim_utils.is_3D():
          # z_axis = self.dim_utils.dim_idx('Z', ignore_time = True, squeeze = True)
          z_axis = 0
        
        # this is already done
        # if nuc_im is not None:
        #   nuc_im_to_predict = self.prepare_im(
        #     nuc_im, x, normalise_percentile = normalise_percentile, norm_im = norm_nuc_im)
        
        if nuc_im_to_predict is not None:
          # add nucleus channel to image
          im_to_predict = np.stack((im_to_predict, nuc_im_to_predict), axis = -1)
          channels = [1, 2]
          channel_axis = len(im_to_predict.shape) - 1
        
        self.logfile_utils.log(f'>> im shape {im_to_predict.shape}')
        self.logfile_utils.log(f'> channels: {channels}')
        self.logfile_utils.log(f'> channel_axis: {channel_axis}')
        self.logfile_utils.log(f'> z_axis: {z_axis}')
        
        # init model
        if cp_model in cfg.data['python']['cellpose']['models']:
          model = models.CellposeModel(
            gpu = self.use_gpu,
            device = self.gpu_device,
            model_type = cp_model
            # omni = self.use_omni
            )
        else:
          model = models.CellposeModel(
            gpu = self.use_gpu,
            device = self.gpu_device,
            # omni = self.use_omni,
            pretrained_model = os.path.join(
              self.ccia_path,
              cfg.data['python']['cellpose']['modelsDir'],
              cp_model
              )
            )
        
        ## EVAL ##
        masks = self.get_masks(
          cp_model, model, im_to_predict,
          cell_diameter = cell_diameter,
          channels = channels, channel_axis = channel_axis,
          z_axis = z_axis,
          stitch_threshold = x['stitchThreshold'][0]
          )
          # normalise_intensity = normalise_intensity)
        
        # merge labels?
        if x['mergeLabels'][0] is True:
          # TODO is that the best/easiest way to do that?
          # binarise segmentation
          masks[masks > 0] = 1
          
          # take a minimum
          masks = ndi.minimum_filter(masks, 1)
          
          # label connected components
          masks = skimage.measure.label(masks)
          
          # then expand again
          masks = skimage.segmentation.expand_labels(masks)
          
        # # filter labels based on donut ratio?
        # if x['filterByDonutRatio'][0] is True:
        #   # make donuts from segmentation and measure intensity
        #   donuts_inner = skimage.morphology.erosion(
        #     masks, selem = skimage.morphology.ball(x['donutSize'][0]) \
        #     if self.dim_utils.is_3D() else skimage.morphology.disk(x['donutSize'][0]))
        #   donuts_outer = masks - donuts_inner
        #   
        #   donuts_inner_props = skimage.measure.regionprops_table(
        #     donuts_inner, im_to_predict, properties = ['mean_intensity'])
        #   donuts_outer_props = skimage.measure.regionprops_table(
        #     donuts_outer, im_to_predict, properties = ['mean_intensity'])
        #     
        #   # take ratio
        #   donuts_ratio = donuts_inner_props['mean_intensity_0']/donuts_outer_props['mean_intensity_0']
        #   
        #   if x['donutRatioType'][0] == 'inside':
        
        # add masks to list
        if np.max(masks) > 0:
          if x['matchAs'][0] == 'cyto':
            model_masks['cyto'].append(masks)
          elif x['matchAs'][0] == 'nuc':
            model_masks['nuc'].append(masks)
          else:
            model_masks['unmatched'].append(masks)

    # combine masks
    interm_labels = dict()
    
    # TODO this will push 'unmatched' labels lowest
    max_label_val = 0
    for i, x in model_masks.items():
      if len(x) > 0:
        for j, y in enumerate(x):
          # add previous label value
          model_masks[i][j][y > 0] += max_label_val
    
          # get maximum label value
          max_label_val = np.max(model_masks[i][j])
          
        # intermediate merge
        interm_labels[i] = np.zeros(label_shape, dtype = np.uint32)
        
        for y in model_masks[i]:
          interm_labels[i] = np.maximum(interm_labels[i], y)
    
    # match labels
    if len(model_masks['cyto']) > 0 and len(model_masks['nuc']) > 0:
      self.logfile_utils.log(f'>> Merge nuclei and cyto')

      # match cells to a nucleus - some cells might not have a nucleus
      # TODO can you rank the labels after merging?
      labels_merged = label_utils.match_masks(
        [interm_labels['nuc'], interm_labels['cyto']],
        # [interm_labels['cyto'], interm_labels['nuc']],
        stitch_threshold = self.match_threshold,
        remove_unmatched = self.remove_unmatched
      )
      # labels_merged_nuc, labels_merged_cyto = label_utils.match_masks(
      #   interm_labels['nuc'], interm_labels['cyto']
      # )

      # add masks to list
      if np.max(labels_merged[0]) > 0 and np.max(labels_merged[1]) > 0:
        interm_labels['cyto'] = labels_merged[1]
        interm_labels['nuc'] = labels_merged[0]
      # if np.max(labels_merged_nuc) > 0 and np.max(labels_merged_cyto) > 0:
      #   interm_labels['nuc'] = labels_merged_nuc
      #   interm_labels['cyto'] = labels_merged_cyto

    # final merge of cyto and base
    merged_labels = np.zeros(label_shape, dtype = np.uint32)
    
    if set({'unmatched', 'cyto'}).issubset(interm_labels.keys()):
      merged_labels = np.maximum(interm_labels['cyto'], interm_labels['unmatched'])
    elif 'cyto' in interm_labels.keys():
      merged_labels = interm_labels['cyto']
    elif 'unmatched' in interm_labels.keys():
      merged_labels = interm_labels['unmatched']
      
    if 'nuc' in interm_labels.keys():
      return {
        'nuc': np.squeeze(interm_labels['nuc']),
        'base': np.squeeze(merged_labels)
      }
    else:
      return {'base': np.squeeze(merged_labels)}

