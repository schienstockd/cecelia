import math
import numpy as np

"""
prepare points for viewing
"""
def prep_points(points_df, dim_utils, centroid_cols, im_scale = None):
  # ignore z?
  im_dim = len(dim_utils.default_dim_order(ignore_channel = True))
  ignore_z = False
  
  if im_scale is not None and len(im_scale) < im_dim:
    im_dim = len(dim_utils.default_dim_order(ignore_channel = True, ignore_z = True))
    ignore_z = True
    
  # create zero array
  zero_array = np.zeros((points_df.shape[0], im_dim))
  
  # fill with points
  for x in centroid_cols:
    dim_idx = dim_utils.get_idx_from_centroid_col(
      x, ignore_channel = True, ignore_z = ignore_z)
      
    if dim_idx is not None:
      zero_array[:, dim_idx] = points_df[x]
    
  return zero_array
