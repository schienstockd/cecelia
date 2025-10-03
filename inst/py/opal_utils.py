import numpy as np

from skimage.segmentation import expand_labels
from skimage.filters import threshold_otsu, gaussian, median
from skimage.morphology import disk, closing, label, remove_small_objects

import py.zarr_utils as zarr_utils

"""
Extract labels for cores
"""
def extract_cores_labels(im_dat, median_filter = 5, otsu_adjust = 1, closing_filter = 5,
                         small_objects_size = 1000, label_expansion = 5, im_min = None, im_max = None,
                         dtype = np.uint16):
  # get min max if not defined
  if any([im_min is None, im_max is None]):
    im_min, im_max = zarr_utils.get_minmax_from_low_res(im_dat)
  
  # normalise image
  im_new = im_dat[-1].map_blocks(zarr_utils.apply_min, im_min = im_min)
  im_new = (((im_new - im_min) / (im_max - im_min)) * np.iinfo(dtype).max)
  im = zarr_utils.fortify(im_new[0,:,:].astype(dtype))
  
  # get labels for cores
  clean_im = median(im, disk(median_filter))
  th_im = clean_im >= threshold_otsu(clean_im) * otsu_adjust
  morph_im = closing(th_im, disk(closing_filter))
  
  # remove small objects
  label_im = label(remove_small_objects(morph_im, small_objects_size))
  
  # expand labels
  return expand_labels(label_im, label_expansion), im
