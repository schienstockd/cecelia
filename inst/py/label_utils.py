import numpy as np

"""
Adapted from cellpose.metrics to find intersection between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L168
"""
def intersection_over_union(x, y, dtype = np.uint16, logfile_utils = None):
  overlap = label_overlap(x, y, dtype = dtype, logfile_utils = logfile_utils)
  n_pixels_x = np.sum(overlap, axis = 0, keepdims = True)
  n_pixels_true = np.sum(overlap, axis = 1, keepdims = True)
  iou = overlap / (n_pixels_x + n_pixels_true - overlap)
  iou[np.isnan(iou)] = 0.0
  
  return iou
  
"""
Adapted from cellpose.metrics to find overlap between masks
https://github.com/MouseLand/cellpose/blob/4e8205125750c0c82e03386f28ff6d4bef1da6c7/cellpose/metrics.py#L133
"""
def label_overlap(x, y, dtype = np.uint16, logfile_utils = None):
  # put label arrays into standard form then flatten them 
  x = x.ravel()
  y = y.ravel()
  
  # preallocate a 'contact map' matrix
  # TODO can you .. incorporate min?
  overlap = np.zeros((1 + x.max(), 1 + y.max()), dtype = dtype)
  
  if logfile_utils is not None:
    logfile_utils.log('>> overlap')
    logfile_utils.log(f'> MAX {x.max()}:{y.max()}')
    logfile_utils.log_mem_usage(context = locals())
  
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
def match_masks(masks, stitch_threshold = 0.2, remove_unmatched = False, dtype = None, logfile_utils = None):
  # save merged labels
  mmax = np.max([x.max() for x in masks])
  mmin = np.min([x[x > 0].min() - 1 if np.any(x) is True else 0 for x in masks])
  empty = 0
  
  # preserve dtype - use first image as reference
  if dtype is None:
    dtype = masks[0].dtype
  
  if logfile_utils is not None:
    logfile_utils.log('>> mmin')
    logfile_utils.log(f'> {mmin}')
  
  # TODO adjust label number to keep computation low
  # Maybe a sparse matrix would be better .. ?
  for i in range(len(masks)):
    masks[i][masks[i] > 0] = masks[i][masks[i] > 0] - mmin
  
  for i in range(len(masks)-1):
    # limit signal if no unmatched labels should be found
    if remove_unmatched is True:
      masks[i] = (masks[i + 1] > 0) * masks[i]
    
    # get intersection
    iou = intersection_over_union(masks[i + 1], masks[i], logfile_utils = logfile_utils)[1:, 1:]
    
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
      iou[iou < stitch_threshold] = 0
      iou[iou < iou.max(axis = 0)] = 0
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
      
  # readjust label numbers
  for i in range(len(masks)):
    masks[i][masks[i] > 0] = masks[i][masks[i] > 0] + mmin
  
  return masks
