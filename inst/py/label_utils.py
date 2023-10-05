import numpy as np
from scipy.sparse import coo_array
from sklearn import preprocessing

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
  return preprocessing.normalize(overlap, norm='l1', axis=1).astype(np.float16)
  
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
      # x = np.array(iou.argmax(axis = 0))[0,:]
      x = np.array(iou.argmax(axis = 0))
      y = np.arange(0, x.size, 1, dtype = dtype)
      z = iou.max(axis = 0).toarray()[0,:]
      iou = coo_array((z, (x, y)), shape = (len(x), len(y)))
      
      # iou = iou >= iou.max(axis = 0).toarray()
      # iou[iou < stitch_threshold] = 0
      # iou[iou < iou.max(axis = 0).toarray()] = 0
      # istitch = iou.argmax(axis = 1).A.ravel() + 1
      istitch = iou.argmax(axis = 1).ravel() + 1
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
