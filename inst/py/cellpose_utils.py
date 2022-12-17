import os
import numpy as np

# utils
from py.segmentation_utils import SegmentationUtils
import py.script_utils as script_utils
import py.config_utils as cfg

import scipy.ndimage as ndi
import skimage.filters
import skimage.morphology
import skimage.measure
import skimage.feature
import skimage.segmentation

# cellpose specifics
from cellpose import models
import mxnet as mx

class CellposeUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    # init params
    self.models = params['models']
    # self.use_omni = script_utils.get_param(params, 'use_omni', default = False)

  """
  get masks from model
  """
  def get_masks(self, model_name, model, im, cell_diameter, normalise_intensity = False):
    #TODO is there a better way to do this .. ?
    try:
      masks = np.zeros(1)
      
      if model_name in cfg.data['python']['cellpose']['models']:
        masks, flows, styles, diams = model.eval(
          im, channels = [0,0], diameter = cell_diameter,
          do_3D = self.dim_utils.is_3D(),
          batch_size = 4,
          augment = False, net_avg = False,
          normalize = normalise_intensity,
          invert = False,
          flow_threshold = 0.99)
      else:
        masks, flows, styles = model.eval(
          im, channels = [0,0], diameter = cell_diameter,
          do_3D = self.dim_utils.is_3D(),
          # batch_size = 8,
          # augment = True, net_avg = True,
          # normalize = True, invert = False,
          batch_size = 4,
          augment = False, net_avg = False,
          normalize = normalise_intensity,
          invert = False,
          flow_threshold = 0.99)
          # anisotropy=anisotropy) # Does not work
    except ValueError as e:
      self.logfile_utils.log('Cellpose Prediction error')
      
    return masks

  """
  Predict slice
  """
  def predict_slice(self, im_dat, dat_slices):
    cur_im_dat = im_dat[dat_slices]
    
    # get label shape
    label_shape = list(cur_im_dat.shape)
    label_shape.pop(self.dim_utils.dim_idx("C"))
    
    # remove time if present
    if self.dim_utils.is_timeseries():
      label_shape.pop(self.dim_utils.dim_idx("T"))
     
    label_shape = tuple(label_shape)

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
      
      if len(x['cellChannels']) > 0:
        # init model
        if cp_model in cfg.data['python']['cellpose']['models']:
          model = models.Cellpose(
            gpu = self.use_gpu, model_type = cp_model
            # omni = self.use_omni
            )
        else:
          model = models.CellposeModel(
            gpu = self.use_gpu,
            # omni = self.use_omni,
            pretrained_model = os.path.join(
              self.ccia_path,
              cfg.data['python']['cellpose']['modelsDir'],
              cp_model
              )
            )
        
        im = np.zeros(label_shape, dtype = np.uint32)
        
        for y in x['cellChannels']:
          im = np.maximum(
            im, np.squeeze(np.take(
              cur_im_dat, y, axis = self.dim_utils.dim_idx("C"))
              ))
        
        # squeeze image
        #im = np.squeeze(im)
        
        # apply threshold?
        if 'threshold' in x.keys() and x['threshold'][0] > 0:
          im[im < x['threshold'][0]] = x['threshold'][0]
          im = im - x['threshold'][0]
          
        # apply relative threshold?
        if 'relThreshold' in x.keys() and x['relThreshold'][0] > 0:
          rel_threshold = np.percentile(im, x['relThreshold'][0])
          im[im < rel_threshold] = rel_threshold
          im = im - rel_threshold
          
        cell_diameter = x['cellDiameter'][0]
        # normalise_intensity = x['normalise'][0]
        normalise_percentile = x['normalise'][0]
        
        # adjust diameter for image resolution
        # TODO use when ome_type 0.21 is released 
        # cell_diameter /= self.dim_utils.omexml.images[0].pixels.physical_size_x
        scaling_factor = self.dim_utils.im_physical_size('x')
        
        if self.dim_utils.omexml.image().Pixels.get_PhysicalSizeXUnit() == 'mm':
          scaling_factor *= 1000
        
        cell_diameter /= scaling_factor
        
        # prepare image
        im_to_predict = im
        
        if x['medianFilter'][0] > 0:
          if self.dim_utils.is_3D():
            median_selem = skimage.morphology.ball(x['medianFilter'][0])
          else:
            median_selem = skimage.morphology.disk(x['medianFilter'][0])
          
          im_to_predict = skimage.filters.median(im_to_predict, median_selem)
        
        im_to_predict = ndi.gaussian_filter(im_to_predict, x['gaussianFilter'][0])
        
        # normalise image
        if normalise_percentile > 0:
          # get min/max values for channels
          max_percentile = np.percentile(im_to_predict, normalise_percentile)
          min_percentile = np.percentile(im_to_predict, 100 - normalise_percentile)
          
          # calculate relative values
          im_to_predict = (im_to_predict - min_percentile) / (max_percentile - min_percentile)
          
          # adjust
          im_to_predict[im_to_predict < 0] = 0
          im_to_predict[im_to_predict > 1] = 1
        
        masks = self.get_masks(
          cp_model, model, im_to_predict,
          cell_diameter = cell_diameter)
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
        
        # add masks to list
        if np.max(masks) > 0:
          if x['matchAs'][0] == 'cyto':
            model_masks['cyto'].append(masks)
          elif x['matchAs'][0] == 'nuc':
            model_masks['nuc'].append(masks)
          else:
            model_masks['unmatched'].append(masks)

    # combine masks
    max_label_val = 0
    
    # TODO this will put unmatched labels as lowest
    for i, x in 
      for i, x in enumerate(model_masks):
        # add previous label value
        model_masks[i][x > 0] += max_label_val
  
        # get maximum label value
        max_label_val = np.max(x)
    
    # go through all nuclei labels
    # TODO does every cell need a nucleus .. ?
    # TODO generalise as this is the same as for mesmer
    if len(model_masks['cyto']) > 0 and len(model_masks['nuc']) > 0:
      for i in tqdm(np.unique(nuc_labels[0, ..., 0])):
        cur_nuc = nuc_labels[0, ..., 0] == i
        cur_cyto = cur_nuc * cyto_labels[0, ..., 0]
    
        # map onto cytoplasm labels
        cyto_label_ids, cyto_label_freq = np.unique(
          cur_cyto, return_counts = True
        )
        
        if len(cyto_label_freq) > 1:
          # get max index
          # ie/ that cell which is overlapping most
          max_idx = cyto_label_freq[1:].argmax(axis = 0)
          
          # get max label
          cur_cyto_label = cyto_label_ids[1:][max_idx]
  
          # assign nucleus and cytoplasm back
          cyto_labels_merged = np.maximum(
            cyto_labels_merged,
            (cyto_labels[0, ..., 0] == cur_cyto_label) * cur_cyto_label
          )
          nuc_labels_merged = np.maximum(
            nuc_labels_merged,
            (cyto_labels[0, ..., 0] == cur_cyto_label) * cur_cyto_label * cur_nuc
          )
          
      # expand labels
      if label_expansion > 0:
        cyto_labels_merged = skimage.segmentation.expand_labels(cyto_labels_merged, label_expansion)
        
      # add masks to list
      if np.max(nuc_labels_merged) > 0 and np.max(cyto_labels_merged) > 0:
        model_masks['nuc'].append(nuc_labels_merged)
        model_masks['cyto'].append(cyto_labels_merged)
    
    # merge together
    merged_labels = np.zeros(label_shape, dtype = np.uint32)
    for x in model_masks:
      merged_labels = np.maximum(merged_labels, x)
    
    if len(cyto_masks) > 0:
      return {
        'nuc': np.squeeze(nuc_labels)
        'base': np.squeeze(merged_labels)
      }
    else:
      return {'base': np.squeeze(merged_labels)}
