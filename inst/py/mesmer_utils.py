# https://github.com/vanvalenlab/deepcell-tf/blob/master/notebooks/applications/Mesmer-Application.ipynb
import numpy as np
import math
import zarr
import os
import json

from time import sleep
from tqdm import tqdm

# utils
from py.segmentation_utils import SegmentationUtils
import py.correction_utils as correction_utils
import py.config_utils as cfg

from deepcell.applications import Mesmer
import tensorflow as tf

class MesmerUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    # function params
    self.nuclei_channels = params['nuclei_channels']
    self.cyto_channels = params['cyto_channels']
    self.normalise_percentile = params['normalise_percentile']
    
    # define axes and remove "C" from axis
    self.axes = self.dim_utils.im_dim_order.copy()
    self.axes.remove("C")
    self.axes = "".join(self.axes)
    
    # pre-trained
    if self.segment is True:
      self.model = Mesmer(model = tf.keras.models.load_model(
          os.path.join(self.ccia_path, cfg.data['python']['mesmer']['modelsDir'])
        ))
    
  """
  Predict slice
  """
  def predict_slice(self, im_dat, dat_slices):
    # create training image
    # Mesmer expects [batch, X, Y, channels]
    # it requires two channels - nucleus and cytoplasm/membrane
    # get order for XY
    xy_order = [x for x in self.dim_utils.im_dim_order if x in ['X', 'Y']]
    a_order = self.dim_utils.dim_idx(xy_order[0])
    b_order = self.dim_utils.dim_idx(xy_order[1])
    
    X_train = np.zeros((
      1,
      dat_slices[a_order].stop - dat_slices[a_order].start,
      dat_slices[b_order].stop - dat_slices[b_order].start,
      2))
      
    # nucleus
    X_train[0, ..., 0] = correction_utils.combine_norm_channels(
      im_dat, self.nuclei_channels, dat_slices,
      self.dim_utils, normalise_percentile = self.normalise_percentile
    )
    
    # cytoplasm
    X_train[0, ..., 1] = correction_utils.combine_norm_channels(
      im_dat, self.cyto_channels, dat_slices,
      self.dim_utils, normalise_percentile = self.normalise_percentile
    )
    
    # get segmentation
    nuc_labels = self.model.predict(
      X_train,
      compartment = 'nuclear',
      image_mpp = self.dim_utils.im_physical_size('x')
      )
    cyto_labels = self.model.predict(
      X_train,
      compartment = 'whole-cell',
      image_mpp = self.dim_utils.im_physical_size('x')
      )
    
    # TODO is there a better way to do this?
    # calculate overlap between cytoplasm and nuclei
    # to assign nuclei to cytoplasm and match their label IDs
    # go through and find maximum overlap between nucleus and cytoplasm
    # binarise nucleus, then map onto cytoplasm labels
    # count number of labels
    nuc_labels_merged = np.zeros_like(nuc_labels[0, ..., 0])
    cyto_labels_merged = np.zeros_like(cyto_labels[0, ..., 0])
    
    # go through all nuclei labels
    for i in tqdm(np.unique(nuc_labels[0, ..., 0])):
      cur_nuc = nuc_labels[0, ..., 0] == i
      cur_cyto = cur_nuc * cyto_labels[0, ..., 0]
  
      # map onto cytoplasm labels
      cyto_label_ids, cyto_label_freq = np.unique(
        cur_cyto, return_counts = True
      )
      
      if len(cyto_label_freq) > 1:
        # get max index
        max_idx = cyto_label_freq[1:].argmax(axis=0)

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
    
    return {
      'nuc': nuc_labels_merged,
      'base': cyto_labels_merged
    }
