import os
import numpy as np
# import working directory to check functions
# os.chdir('~/R/x86_64-pc-linux-gnu-library/4.2/cecelia/inst')
import sys
os.chdir("/home/schienstockd/R/x86_64-pc-linux-gnu-library/4.2/cecelia")
sys.path.append("/home/schienstockd/R/x86_64-pc-linux-gnu-library/4.2/cecelia")

# config
import py.config_utils as cfg
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.correction_utils as correction_utils
import math
import shutil
import zarr

from skimage.color import separate_stains, combine_stains, hdx_from_rgb, rgb_from_hdx
from skimage.registration import phase_cross_correlation
from skimage.transform import warp_polar, rotate, rescale, warp, SimilarityTransform, AffineTransform
from skimage.util import img_as_float
  
"""
Get shifts for image registration
TODO is ITK easier and better?
https://simpleitk.readthedocs.io/en/master/registrationOverview.html
"""
def image_registration_shifts(ims, channel_ids, radius = 1000, upsample_factor = 20):
  # warp image for rotation
  im_polar = warp_polar(np.amax(ims[0], axis = 0), radius = radius, scaling = 'log')
  
  shifts = list()
  rotations = list()
  
  # go through images and find shifts based on first image
  for i in range(1, len(ims)):
    # apply translation shifts
    shift, error, diffphase = phase_cross_correlation(ims[0], ims[i], upsample_factor = upsample_factor)
    shifts.append(shift)
    
    tform = SimilarityTransform(translation = (-shift[0], -shift[1]))
    im_warped = warp(ims[i], tform)

    # apply rotation
    other_polar = warp_polar(np.amax(im_warped, axis = 0), radius = radius, scaling = 'log')

    shift, error, diffphase = phase_cross_correlation(im_polar, other_polar, upsample_factor = upsample_factor)
    rotations.append(shift)

    tform = AffineTransform(rotation = math.radians(-shift[0]))
    im_warped = warp(im_warped, tform)

    # apply translation shifts
    shift, error, diffphase = phase_cross_correlation(ims[0], im_warped, upsample_factor = upsample_factor)
    shifts[-1] = shifts[-1] + shift
  
  # convert to array
  return np.vstack(shifts), np.vstack(rotations)

base_dir = '/data/scratch/projects/punim1124/cecelia/USERS/schienstockd/wEsnzt/ANALYSIS/'

# Liver
im_paths = [
  os.path.join(base_dir, '0', 'SXejeM','ccidImage.ome.zarr'),
  os.path.join(base_dir, '0', 'Iksums','ccidImage.ome.zarr'),
  os.path.join(base_dir, '0', 'PTsqyH','ccidImage.ome.zarr'),
  os.path.join(base_dir, '0', 'OO0Q6w','ccidImage.ome.zarr')
]
channel_ids = [3, 3, 3, 3]

# get image information
im_zarrs = [zarr_utils.open_as_zarr(x, as_dask = True) for x in im_paths]
im_zarrs = [x[0] for x in im_zarrs]

# create dim utils for images
dim_utils = [
  DimUtils(ome_xml_utils.parse_meta(x), use_channel_axis = True) for x in im_paths
]

for i, x in enumerate(dim_utils):
  x.calc_image_dimensions(im_zarrs[i][0].shape)
  
input_arrays = im_zarrs
dim_utils_list = dim_utils
phase_shift_channels = channel_ids
reg_path = '/data/scratch/projects/punim1124/cecelia/USERS/schienstockd/TMP/reg000'
shifts = None
upsample_factor = 20
normalise_percentile = 98

# get shifts
if shifts is None:
  print('>> Get shifts')
  # get shifts from lowest res
  low_res = len(input_arrays[0]) - 1

  im_slices = [0 for _ in range(len(input_arrays[0][0].shape))]
  im_slices[dim_utils_list[0].dim_idx('X')] = slice(0, min([x.dim_val('X', scale = low_res) for x in dim_utils_list]))
  im_slices[dim_utils_list[0].dim_idx('Y')] = slice(0, min([x.dim_val('Y', scale = low_res) for x in dim_utils_list]))

  if dim_utils_list[0].is_3D() is True:
    im_slices[dim_utils_list[0].dim_idx('Z')] = slice(None)

  # get low resolution images for registration
  ims = list()

  for i, x in enumerate(input_arrays):
    im_slices[dim_utils[i].dim_idx('C')] = phase_shift_channels[i]
    ims.append(zarr_utils.fortify(x[low_res][tuple(im_slices)]))

    # normalise image
    percentile = (np.percentile(ims[-1], normalise_percentile),
            np.percentile(ims[-1], 100 - normalise_percentile))
    ims[-1] = ((ims[-1] - percentile[1]) / (percentile[0] - percentile[1]))

  # get shifts and rotations
  shifts, rotations = image_registration_shifts(
    ims, phase_shift_channels,
    upsample_factor = upsample_factor)

# get shifts summary
shifts_summary = correction_utils.shifts_summary(shifts, cumulative = False)

# get new image dimensions
reg_im_shape, reg_im_shape_round = correction_utils.correction_im_shape(
  input_arrays[0][0], dim_utils_list[0], shifts_summary
)

# get first image position
first_im_pos = correction_utils.correction_first_im_pos(
  reg_im_shape, dim_utils_list[0], shifts_summary
)

# remove previous folder
if reg_path is not None and os.path.exists(reg_path) is True:
  shutil.rmtree(reg_path)

# add channels for new image
reg_im_shape_round = list(reg_im_shape_round)
# remove channel for registration from sum?
#reg_im_shape_round[dim_utils_list[0].dim_idx('C')] = sum([x.dim_val('C') - 1 for x in dim_utils_list]) + 1
reg_im_shape_round[dim_utils_list[0].dim_idx('C')] = sum([x.dim_val('C') for x in dim_utils_list])
reg_im_shape_round = tuple(reg_im_shape_round)

# create array
reg_zarr = zarr.create(
  reg_im_shape_round,
  dtype = input_arrays[0][0].dtype,
  chunks = input_arrays[0][0].chunksize,
  store = reg_path
)

# use first position for slice
slices = first_im_pos

print('>> Apply shifts')

# go through images and register to first image
for i, x in enumerate(input_arrays):
  # create slice
  if i > 0:
    new_slices = list()

    # adjust slices
    for j, y in enumerate(slices):
      new_slices.append(slice(
        # subtract '1' because there is no
        # shift for the first frame
        y.start + shifts[i - 1, j],
        y.stop + shifts[i - 1, j],
        1
      ))

    # push back
    slices = new_slices

  # round for slicing
  new_slices = [0 for _ in range(len(reg_im_shape_round))]
  im_slices = [0 for _ in range(len(reg_im_shape_round))]

  # set Z, X, Y for new and slices
  for j, y in enumerate(('Z', 'Y', 'X')):
    new_slices[dim_utils_list[i].dim_idx(y)] = slice(round(slices[j].start), round(slices[j].stop), 1)
    im_slices[dim_utils_list[i].dim_idx(y)] = slice(None)
  
  new_slices = tuple(new_slices)
  im_slices = tuple(im_slices)
  
  print(f'>> reg {i}')
  print(f'> {new_slices}')
  print(f'> {im_slices}')

  # add to image list
  new_image = np.zeros(reg_im_shape_round)

  # check that slices match dimension
  if new_image[new_slices].shape != x[0][im_slices].shape:
    # get wrong dimensions
    dif_dim = [y - z for y, z in zip(
      new_image[new_slices].shape,
      x[0][im_slices].shape
    )]
    
    print(f'>> Diff {dif_dim}')
    new_slices = list(new_slices)
    
    # get slice mapping
    slice_mapping = [dim_utils_list[i].dim_idx(y) for y in dim_utils_list[i].im_dim_order if y in dim_utils_list[i].spatial_axis()]

    # adjust dimensions
    for j, y in enumerate(dif_dim):
      # match to mapping
      k = slice_mapping[j]
      
      # adjust stop positions
      new_slices[k] = slice(new_slices[k].start, new_slices[k].stop - y, 1)
        
      # cut off the end if needed
      # TODO ... ideally, you crop to a common region
      # or expand the image shape that this does not happen
      if new_slices[k].stop >= reg_zarr.shape[j]:
        new_slices[k] = slice(new_slices[k].start, reg_zarr.shape[k], 1)

  # apply rotation and push to zarr
  prev_channels = sum([x.dim_val('C') for x in dim_utils_list[0:i]])
  new_slices = list(new_slices)
  im_slices = list(im_slices)
  
  #for j in range(dim_utils_list[i].dim_val('C')):
  for j in [3]:
    # set channel for new slice
    new_slices[dim_utils_list[i].dim_idx('C')] = prev_channels + j
    im_slices[dim_utils_list[i].dim_idx('C')] = j
      
    reg_zarr[tuple(new_slices)] = warp(
      x[0][tuple(im_slices)],
      AffineTransform(rotation = math.radians(-rotations[i - 1][0])))
