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
import SimpleITK as sitk
import py.sitkibex as sitkibex

import math
import shutil
import zarr

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
phase_shift_channels = channel_ids
reg_path = '/data/scratch/projects/punim1124/cecelia/USERS/schienstockd/TMP/reg000.zarr'

### Registration with ITK from here ###
# Credit goes to sitkibex
# https://github.com/niaid/imaris_extensions
# https://github.com/niaid/sitk-ibex
# https://zenodo.org/record/4632320#.Y5pbnOJBzRY

# get new image dimensions
# reg_im_shape = list(min([x[0].shape for x in input_arrays]))
reg_im_shape = list(input_arrays[0][0].shape)

# add channels
reg_im_shape[dim_utils[0].dim_idx('C')] = sum([x.dim_val('C') for x in dim_utils])

# remove previous folder
if reg_path is not None and os.path.exists(reg_path) is True:
  shutil.rmtree(reg_path)

# create array
reg_zarr = zarr.create(
  reg_im_shape,
  dtype = input_arrays[0][0].dtype,
  chunks = input_arrays[0][0].chunksize,
  store = reg_path
)

# get slicing
slices = [[slice(None) for _ in range(len(input_arrays[0][0].shape))]] * len(input_arrays)

for i, x in enumerate(slices):
  slices[i][dim_utils[i].dim_idx('T')] = 0
  
  # DEBUG ONLY
  slices[i][dim_utils[i].dim_idx('Z')] = slice(0, 2, 1)

# get transforms
reg_tx = list()

# get fixed image
slices[0][dim_utils[0].dim_idx('C')] = phase_shift_channels[0]
fixed_im = sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(input_arrays[0][0][tuple(slices[0])])))

# go through arrays
for i, x in enumerate(input_arrays[1:]):
  print(f'>> Register {i}')
  
  # set slicing
  slices[i][dim_utils[i].dim_idx('C')] = phase_shift_channels[i]
  
  # apply
  # TODO can you do this somehow in the low res image and then scale up .. ?
  reg_tx.append(sitkibex.registration(
    fixed_im,
    sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(x[0][tuple(slices[i + 1])]))),
    do_fft_initialization = False,
    do_affine2d = True,
    do_affine3d = False,
    ignore_spacing = True,
    sigma = 1.0,
    auto_mask = False,
    samples_per_parameter = 5000,
    expand = None))
    
# apply transforms
reg_slices = [slice(None) for _ in range(len(reg_zarr.shape))]
im_slices = [slice(None) for _ in range(len(x[0].shape))]
reg_slices[dim_utils[0].dim_idx('T')] = 0
im_slices[dim_utils[0].dim_idx('T')] = 0

# DEBUG ONLY
reg_slices[dim_utils[0].dim_idx('Z')] = slice(0, 2, 1)
im_slices[dim_utils[0].dim_idx('Z')] = slice(0, 2, 1)

# push first image
reg_slices[dim_utils[0].dim_idx('C')] = phase_shift_channels[0]
im_slices[dim_utils[0].dim_idx('C')] = phase_shift_channels[0]
reg_zarr[tuple(reg_slices)] = input_arrays[0][0][tuple(im_slices)]

channel_sum = dim_utils[0].dim_val('C')

# go through arrays
for i, x in enumerate(input_arrays[1:]):
  # go through channels
  for j in range(dim_utils[i + 1].dim_val('C')):
    # set slicing
    reg_slices[dim_utils[0].dim_idx('C')] = channel_sum + j
    im_slices[dim_utils[i + 1].dim_idx('C')] = j
    
    # push to zarr
    reg_zarr[tuple(reg_slices)] = sitk.GetArrayFromImage(sitkibex.resample(
      fixed_image = fixed_im,
      moving_image = sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(x[0][tuple(im_slices)]))),
      transform = reg_tx[i]))
  
  channel_sum += dim_utils[i + 1].dim_val('C')
