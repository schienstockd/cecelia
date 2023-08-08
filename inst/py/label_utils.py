import numpy as np

"""
Adapted from cellpose.metrics to find intersection between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L168
"""
def intersection_over_union(x, y):
  overlap = label_overlap(x, y)
  n_pixels_x = np.sum(overlap, axis = 0, keepdims = True)
  n_pixels_true = np.sum(overlap, axis = 1, keepdims = True)
  iou = overlap / (n_pixels_x + n_pixels_true - overlap)
  iou[np.isnan(iou)] = 0.0
  
  return iou
  
"""
Adapted from cellpose.metrics to find overlap between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L133
"""
def label_overlap(x, y):
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
Match masks
adapted from cellpose.utils.stitch3D
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/utils.py#L353
"""
def match_masks(masks, stitch_threshold = 0.2, remove_unmatched = False, dtype = None):
  # save merged labels
  mmax = masks[0].max()
  # mmin = masks[0].min()
  empty = 0
  
  # preserve dtype - use first image as reference
  if dtype is None:
    dtype = masks[0].dtype
  
  for i in range(len(masks)-1):
    # limit signal if no unmatched labels should be found
    if remove_unmatched is True:
      masks[i] = (masks[i + 1] > 0) * masks[i]
    
    # get intersection
    iou = intersection_over_union(masks[i + 1], masks[i])[1:, 1:]
    
    if not iou.size and empty == 0:
      masks[i + 1] = masks[i + 1]
      mmax = masks[i + 1].max()
    elif not iou.size and not empty == 0:
      icount = masks[i + 1].max()
      istitch = np.arange(mmax + 1, mmax + icount + 1, 1, dtype = dtype)
      mmax += icount
      istitch = np.append(np.array(0), istitch)
      masks[i + 1] = istitch[masks[i + 1]]
    else:
      iou[iou < stitch_threshold] = 0.0
      iou[iou < iou.max(axis = 0)] = 0.0
      istitch = iou.argmax(axis = 1) + 1
      ino = np.nonzero(iou.max(axis = 1) == 0.0)[0]
      istitch[ino] = np.arange(mmax + 1, mmax + len(ino) + 1, 1, dtype = dtype)
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
