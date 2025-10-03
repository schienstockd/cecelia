import numpy as np
from scipy.sparse import coo_array
from sklearn import preprocessing

"""
3D Bresenham for line digitisation
https://www.geeksforgeeks.org/bresenhams-algorithm-for-3-d-line-drawing/
"""
def bresenham_3D(x1, y1, z1, x2, y2, z2):
  # init first points
  points = [(x1, y1, z1)]
  
  # TODO RuntimeWarning: overflow encountered in uint_scalars
  # dx = abs(x2 - x1)
  # dy = abs(y2 - y1)
  # dz = abs(z2 - z1)
  dx = max(x1, x2) - min(x1, x2)
  dy = max(y1, y2) - min(y1, y2)
  dz = max(z1, z2) - min(z1, z2)
  
  if (x2 > x1):
    xs = 1
  else:
    xs = -1
  if (y2 > y1):
    ys = 1
  else:
    ys = -1
  if (z2 > z1):
    zs = 1
  else:
    zs = -1

  # Driving axis is X-axis"
  if (dx >= dy and dx >= dz):        
    p1 = 2 * dy - dx
    p2 = 2 * dz - dx
    while (x1 != x2):
      x1 += xs
      if (p1 >= 0):
        y1 += ys
        p1 -= 2 * dx
      if (p2 >= 0):
        z1 += zs
        p2 -= 2 * dx
      p1 += 2 * dy
      p2 += 2 * dz
      points.append((x1, y1, z1))

  # Driving axis is Y-axis"
  elif (dy >= dx and dy >= dz):       
    p1 = 2 * dx - dy
    p2 = 2 * dz - dy
    while (y1 != y2):
      y1 += ys
      if (p1 >= 0):
        x1 += xs
        p1 -= 2 * dy
      if (p2 >= 0):
        z1 += zs
        p2 -= 2 * dy
      p1 += 2 * dx
      p2 += 2 * dz
      points.append((x1, y1, z1))

  # Driving axis is Z-axis"
  else:        
    p1 = 2 * dy - dz
    p2 = 2 * dx - dz
    while (z1 != z2):
      z1 += zs
      if (p1 >= 0):
        y1 += ys
        p1 -= 2 * dz
      if (p2 >= 0):
        x1 += xs
        p2 -= 2 * dz
      p1 += 2 * dy
      p2 += 2 * dx
      points.append((x1, y1, z1))
      
  return np.array(points)

"""
2D Bresenham for line digitisation
Use 3D version for 2D?
There is a 2D version but it did work that well for some points
https://babavoss.pythonanywhere.com/python/bresenham-line-drawing-algorithm-implemented-in-py
"""
def bresenham_2D(x1, y1, x2, y2):
  return bresenham_3D(x1, y1, 0, x2, y2, 0)[:, 0:2]

# """
# 2D Bresenham for line digitisation
# https://babavoss.pythonanywhere.com/python/bresenham-line-drawing-algorithm-implemented-in-py
# """
# def bresenham_2D(x1, y1, x2, y2):
#   x, y = x1, y1
#   
#   dx = abs(x2 - x1)
#   dy = abs(y2 - y1)
#   gradient = dy/float(dx)
#   # https://stackoverflow.com/a/68118106
#   # gradient = dx or dy/float(dx)
#   
#   # catch inf if dx is 0
#   print(gradient, gradient == float('inf'))
#   if gradient == np.inf:
#     # dx, dy = dy, dx
#     # x, y = y, x
#     x1, y1 = y1, x1
#     x2, y2 = y2, x2
#   elif gradient > 1:
#     # dx, dy = dy, dx
#     # x, y = y, x
#     x1, y1 = y1, x1
#     x2, y2 = y2, x2
#   
#   p = 2*dy - dx
#   
#   print(dx, dy, gradient, gradient > 1, p)
#   
#   # Initialize the plotting points
#   points = [(x, y)]
#   
#   print(range(2, dx + 2))
#   
#   # draw vertical
#   if gradient == np.inf:
#     for k in range(2, dy + 2):
#       if p > 0:
#         x = x + 1 if x < x2 else x - 1
#         p = p + 2 * (dx - dy)
#       else:
#         p = p + 2 * dx
#   
#       y = y + 1 if y < y2 else y - 1
#   
#       points.append((x, y))
#   else:
#     for k in range(2, dx + 2):
#       if p > 0:
#         y = y + 1 if y < y2 else y - 1
#         p = p + 2 * (dy - dx)
#       else:
#         p = p + 2 * dy
#   
#       x = x + 1 if x < x2 else x - 1
#   
#       points.append((x, y))
#     
#   return points

"""
Adapted from cellpose.metrics to find intersection between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L168
"""
def intersection_over_union(x, y, dtype = np.uint16):
  # get overlap
  overlap = label_overlap(x, y, dtype = dtype)
  
  # n_pixels_x = np.sum(overlap, axis = 0, keepdims = True, dtype = np.uint16)
  # n_pixels_true = np.sum(overlap, axis = 1, keepdims = True, dtype = np.uint16)
  # n_compose = (n_pixels_x + n_pixels_true - overlap)
  # iou = np.divide(overlap, n_compose, dtype = np.float16, where = n_compose != 0)
  # iou[np.isnan(iou)] = 0.0
  
  # # TODO there must be a better way to use sparse matrices
  # n_pixels_x = overlap.sum(axis = 0).astype(np.uint16)
  # n_pixels_true = overlap.sum(axis = 1).astype(np.uint16)
  # 
  # # reshape and compose
  # n_pixels_true = n_pixels_true.reshape((n_pixels_true.shape[0], 1))
  # n_compose = (n_pixels_x + n_pixels_true - overlap)
  # 
  # iou = np.divide(overlap.toarray(), n_compose,
  #                 dtype = np.float16, where = n_compose != 0)
  # iou[np.isnan(iou)] = 0.0
  
  # return iou
  return preprocessing.normalize(overlap, norm='l1', axis=1).astype(np.float32)
  
"""
Adapted from cellpose.metrics to find overlap between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L133
"""
def label_overlap(x, y, dtype = np.uint16):
  # put label arrays into standard form then flatten them 
  x = x.ravel()
  y = y.ravel()
  z = [1] * len(x)
  
  # preallocate a 'contact map' matrix
  # TODO can you make a sparse matrix out of that .. ?
  # overlap = np.zeros((1 + x.max(), 1 + y.max()), dtype = dtype)
  
  # loop over the labels in x and add to the corresponding
  # overlap entry. If label A in x and label B in y share P
  # pixels, then the resulting overlap is P
  # len(x)=len(y), the number of pixels in the whole image 
  # for i in range(len(x)):
  #   overlap[x[i],y[i]] += 1
  
  return coo_array((z, (x, y)), shape = (len(x), len(y)), dtype = dtype)
  # return overlap

"""
Return base key
"""
def get_base_key(labels_array):
  base_key = 'base'
  
  if 'halo' in labels_array.keys():
    base_key = 'halo'
    
  return base_key

"""
Match label IDs
"""
def match_label_ids(labels_array, match_to = 'base'):
  _, idx = np.unique(labels_array[match_to], return_index = True)
  label_ids = labels_array[match_to].ravel()[idx[1:]]
  
  # go through to match labels
  # TODO there should be a better way
  for j in [k for k in labels_array.keys() if k != match_to]:
    if labels_array[j] is not None:
      # remove labels not present in match
      labels_array[j][np.invert(np.isin(labels_array[j], label_ids))] = 0

"""
Match masks
adapted from cellpose.utils.stitch3D
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/utils.py#L353
"""
def match_masks(masks, stitch_threshold = 0.0, remove_unmatched = False,
                only_unmatched = False, dtype = None, logfile_utils = None):
  mmin = min([x[x > 0].min() - 1 if np.any(x) else 0 for x in masks])
  empty = 0
  
  # preserve dtype - use first image as reference
  if dtype is None:
    dtype = masks[0].dtype
  
  # TODO adjust label number to keep computation low
  # Maybe a sparse matrix would be better .. ?
  for i in range(len(masks)):
    masks[i][masks[i] > 0] = masks[i][masks[i] > 0] - mmin
    
  # get max for processing from lefthand image
  mmax = masks[0].max()
  
  for i in range(len(masks)-1):
    # limit signal if no unmatched labels should be found
    if remove_unmatched is True:
      masks[i] = (masks[i + 1] > 0) * masks[i]
    
    # get intersection
    iou = intersection_over_union(masks[i + 1], masks[i])[1:, 1:]
    no_stitch = False
    
    if not iou.size and empty == 0:
      masks[i + 1] = masks[i + 1]
      # mmax = masks[i + 1].max()
      no_stitch = True
    # elif not iou.size and not empty == 0:
    #   icount = masks[i + 1].max()
    #   istitch = np.arange(mmax + 1, mmax + icount + 1, 1, dtype = dtype)
    #   # mmax += icount
    #   istitch = np.append(np.array(0), istitch)
    #   masks[i + 1] = istitch[masks[i + 1]]
    else:
      iou = iou > stitch_threshold
      x = np.array(iou.argmax(axis = 0))
      if len(x.shape) > 1:
        x = x[0,:]
      
      y = np.arange(0, x.size, 1, dtype = dtype)
      z = iou.max(axis = 0).toarray()
      if len(z.shape) > 1:
        z = z[0,:]

      iou = coo_array((z, (x, y)), shape = (len(x), len(y)))
      
      # iou = iou >= iou.max(axis = 0).toarray()
      # iou[iou < stitch_threshold] = 0
      # iou[iou < iou.max(axis = 0).toarray()] = 0
      
      istitch = iou.argmax(axis = 1)
      # if not isinstance(istitch, np.ndarray):
      # if len(istitch.shape) > 1:
      if hasattr(istitch, 'A'):
        istitch = istitch.A.ravel() + 1
      else:
        istitch = istitch.ravel() + 1
      ino = np.nonzero(iou.max(axis = 1).toarray() == 0.0)[0]
      istitch[ino] = np.arange(mmax + 1, mmax + len(ino) + 1, 1, dtype = dtype)
      # mmax += len(ino)
      istitch = np.append(np.array(0), istitch)
      masks[i + 1] = istitch[masks[i + 1]]
      empty = 1

  # only accept common or not common labels
  if any([remove_unmatched, only_unmatched]):
    common_labels = list()

    # get common labels from all masks
    for i in range(len(masks)):
      if i > 0:
        common_labels = np.intersect1d(common_labels, np.unique(masks[i])[1:])
      else:
        common_labels = np.unique(masks[i])[1:]

    # remove non-matched labels
    if remove_unmatched is True:
      for i in range(len(masks)):
        masks[i] = masks[i] * np.isin(masks[i], common_labels)
    elif only_unmatched is True:
      for i in range(len(masks)):
        masks[i] = masks[i] * np.invert(np.isin(masks[i], common_labels))
      
  # readjust label numbers
  for i in range(len(masks)):
    masks[i][masks[i] > 0] = masks[i][masks[i] > 0] + mmin
  
  # reduce numbers after merging
  if no_stitch is False:
    for i in range(1, len(masks)):
      # masks[i][masks[i] > 0] = masks[i][masks[i] > 0] - mmax
      masks[i][masks[i] > mmax] = masks[i][masks[i] > mmax] - mmax
  
  return masks
