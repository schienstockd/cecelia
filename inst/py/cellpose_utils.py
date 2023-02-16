import os
import numpy as np

# utils
from py.segmentation_utils import SegmentationUtils
import py.script_utils as script_utils
import py.config_utils as cfg

from tqdm import tqdm

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
    self.match_threshold = script_utils.get_param(params, 'match_threshold', default = 0.8)
    self.remove_unmatched = script_utils.get_param(params, 'remove_unmatched', default = True)

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
        
      # do not use stitching when not 3D
      # otherwise the segmentation will not return masks
      if do_3D is False:
        stitch_threshold = 0.0
      
      if model_name in cfg.data['python']['cellpose']['models']:
        masks, flows, styles, diams = model.eval(
          im, channels = channels, diameter = cell_diameter,
          channel_axis = channel_axis, z_axis = z_axis,
          do_3D = do_3D,
          stitch_threshold = stitch_threshold,
          batch_size = 4,
          augment = False, net_avg = False,
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
          augment = False, net_avg = False,
          normalize = normalise_intensity,
          invert = False,
          flow_threshold = 0.99)
          # anisotropy=anisotropy) # Does not work
    except ValueError as e:
      self.logfile_utils.log('Cellpose Prediction error')
      
    return masks

  """
  Prepare im for run
  """
  def prepare_im(self, im, model_params, normalise_percentile = 99.9):
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
    
    if model_params['medianFilter'][0] > 0:
      if self.dim_utils.is_3D():
        median_selem = skimage.morphology.ball(model_params['medianFilter'][0])
      else:
        median_selem = skimage.morphology.disk(model_params['medianFilter'][0])
      
      im_to_predict = skimage.filters.median(im_to_predict, median_selem)
    
    im_to_predict = ndi.gaussian_filter(im_to_predict, model_params['gaussianFilter'][0])
    
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
    
    return im_to_predict

  """
  Adapted from cellpose.metrics to find intersection between masks
  https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L168
  """
  def intersection_over_union(self, x, y):
    overlap = self.label_overlap(x, y)
    n_pixels_x = np.sum(overlap, axis = 0, keepdims = True)
    n_pixels_true = np.sum(overlap, axis = 1, keepdims = True)
    iou = overlap / (n_pixels_x + n_pixels_true - overlap)
    iou[np.isnan(iou)] = 0.0
    
    return iou
    
  """
  Adapted from cellpose.metrics to find overlap between masks
  https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L133
  """
  def label_overlap(self, x, y):
    # put label arrays into standard form then flatten them 
    x = x.ravel()
    y = y.ravel()
    
    # preallocate a 'contact map' matrix
    overlap = np.zeros((1 + x.max(), 1 + y.max()), dtype = np.uint)
    
    # loop over the labels in x and add to the corresponding
    # overlap entry. If label A in x and label B in y share P
    # pixels, then the resulting overlap is P
    # len(x)=len(y), the number of pixels in the whole image 
    for i in range(len(x)):
        overlap[x[i],y[i]] += 1
        
    return overlap

  """
  Match cells and nuclei
  adapted from cellpose.utils.stitch3D
  https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/utils.py#L353
  """
  def match_cells_and_nuclei(self, masks, stitch_threshold = 0.2, remove_unmatched = False):
    # save merged labels
    mmax = masks[0].max()
    empty = 0
    
    for i in range(len(masks)-1):
      # limit signal if no unmatched labels should be found
      if remove_unmatched is True:
        masks[i] = (masks[i + 1] > 0) * masks[i]
      
      # get intersection
      iou = self.intersection_over_union(masks[i + 1], masks[i])[1:, 1:]
      
      if not iou.size and empty == 0:
        masks[i + 1] = masks[i + 1]
        mmax = masks[i + 1].max()
      elif not iou.size and not empty == 0:
        icount = masks[i + 1].max()
        istitch = np.arange(mmax + 1, mmax + icount + 1, 1, int)
        mmax += icount
        istitch = np.append(np.array(0), istitch)
        masks[i + 1] = istitch[masks[i + 1]]
      else:
        iou[iou < stitch_threshold] = 0.0
        iou[iou < iou.max(axis = 0)] = 0.0
        istitch = iou.argmax(axis = 1) + 1
        ino = np.nonzero(iou.max(axis = 1) == 0.0)[0]
        istitch[ino] = np.arange(mmax + 1, mmax + len(ino) + 1, 1, int)
        mmax += len(ino)
        istitch = np.append(np.array(0), istitch)
        masks[i + 1] = istitch[masks[i + 1]]
        empty = 1

    # only accept common labels
    if remove_unmatched is True:
      common_labels = list()

      # get common labels from all masks
      for i in range(len(masks)):
        if i > 0:
          common_labels = np.intersect1d(common_labels, masks[i])
        else:
          common_labels = np.unique(masks[i])

      # remove non-matched labels
      for i in range(len(masks)):
        masks[i] = masks[i] * np.isin(masks[i], common_labels)

    return masks
  
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
  def predict_slice(self, im_dat, dat_slices):
    cur_im_dat = im_dat[dat_slices]
    
    # get label shape
    label_shape = list(cur_im_dat.shape)
    label_shape.pop(self.dim_utils.dim_idx('C'))
    
    # remove time if present
    if self.dim_utils.is_timeseries():
      label_shape.pop(self.dim_utils.dim_idx('T'))
     
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
      
      self.logfile_utils.log(f'>> Evaluate {i}: {cp_model}')
      self.logfile_utils.log(x['cellChannels'])
      
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
        nuc_im = None
        
        for y in x['cellChannels']:
          im = np.maximum(
            im, np.squeeze(np.take(
              cur_im_dat, y, axis = self.dim_utils.dim_idx('C'))
              ))
              
        # add nuclei channel?
        if len(x['nucChannels']) > 0:
          nuc_im = np.zeros(label_shape, dtype = np.uint32)
          
          for y in x['nucChannels']:
            nuc_im = np.maximum(
              nuc_im, np.squeeze(np.take(
                cur_im_dat, y, axis = self.dim_utils.dim_idx('C'))
                ))

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
        
        # prepare images
        # grayscale
        channels = [0, 0]
        channel_axis = None
        z_axis = None
        im_to_predict = self.prepare_im(im, x, normalise_percentile = normalise_percentile)
        nuc_im_to_predict = None
        
        if self.dim_utils.is_3D():
          # z_axis = self.dim_utils.dim_idx('Z', ignore_time = True, squeeze = True)
          z_axis = 0
        
        if nuc_im is not None:
          nuc_im_to_predict = self.prepare_im(nuc_im, x, normalise_percentile = normalise_percentile)
          
          # add nucleus channel to image
          im_to_predict = np.stack((im_to_predict, nuc_im_to_predict), axis = -1)
          channels = [1, 2]
          channel_axis = len(im_to_predict.shape) - 1
        
        self.logfile_utils.log(f'>> im shape {im_to_predict.shape}')
        self.logfile_utils.log(f'> channels: {channels}')
        self.logfile_utils.log(f'> channel_axis: {channel_axis}')
        self.logfile_utils.log(f'> z_axis: {z_axis}')
        
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
      labels_merged = self.match_cells_and_nuclei(
        [interm_labels['nuc'], interm_labels['cyto']],
        # [interm_labels['cyto'], interm_labels['nuc']],
        stitch_threshold = self.match_threshold,
        remove_unmatched = self.remove_unmatched
      )
      # labels_merged_nuc, labels_merged_cyto = self.match_cells_and_nuclei(
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

