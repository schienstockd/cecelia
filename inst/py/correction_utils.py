from copy import copy
import numpy as np
import shutil

import dask.array as da
import dask_image.imread
import dask_image.ndfilters
import zarr

from skimage.registration import phase_cross_correlation
from skimage.restoration import rolling_ball
import skimage.morphology
import skimage.filters
# import scipy.ndimage.filters
import scipy.ndimage

import py.zarr_utils as zarr_utils

"""
Get drift correction shift
"""
def drift_correction_shifts(
  image_array, phase_shift_channel, dim_utils = None,
  timepoints = None, upsample_factor = 100,
  normalisation = None, time_idx = None,
  channel_idx = None):
  # get shifts
  shifts = list()

  # get image dimension information
  if channel_idx is None:
    channel_idx = dim_utils.dim_idx('C')
  
  if time_idx is None:
    time_idx = dim_utils.dim_idx('T')

  # create slices for corrections
  slices = [slice(None) for x in range(len(image_array.shape))]
  
  if channel_idx is not None:
    slices[channel_idx] = slice(phase_shift_channel, phase_shift_channel + 1, 1)

  # get all timepoints if not specified
  if timepoints is None:
    timepoints = range(1, dim_utils.dim_val('T'))

  # go through timepoints
  for x in timepoints:
    if x % 10 == 0:
      print(x)

    # define timepoints
    slices_a = slices.copy()
    slices_b = slices.copy()
    slices_a[time_idx] = slice(x - 1, x, 1)
    slices_b[time_idx] = slice(x, x + 1, 1)
    
    # convert to tuple
    slices_a = tuple(slices_a)
    slices_b = tuple(slices_b)
    
    # (sub)pixel precision
    # TODO There is a lot more correction you can do
    # https://scikit-image.org/docs/stable/auto_examples/registration/plot_register_rotation.html
    shift, error, diffphase = phase_cross_correlation(
      np.squeeze(image_array[slices_a]),
      np.squeeze(image_array[slices_b]),
      upsample_factor = upsample_factor,
      normalization = normalisation
    )

    # add shift
    shifts.append(shift)

  # convert to array
  return np.vstack(shifts)

"""
get max, min and sum shift
"""
def shifts_summary(shifts, cumulative = True, is_3D = True):
  shift_size = 3 if is_3D is True else 2
  
  max_shifts = np.zeros((shift_size))
  min_shifts = np.zeros((shift_size))
  cur_shifts = np.zeros((shift_size))

  # get maximum shifts for left and right
  for x in shifts:
    # get new shifts
    if cumulative is True:
      cur_shifts = cur_shifts + x
    else:
      cur_shifts = x

    # set max and min shifts
    max_shifts = np.maximum(cur_shifts, max_shifts)
    min_shifts = np.minimum(cur_shifts, min_shifts)

  min_shifts = abs(min_shifts)
  sum_shifts = max_shifts + min_shifts

  return {
    'max': max_shifts,
    'min': min_shifts,
    'sum': sum_shifts
    }

"""
get new image dimensions for correction
"""
def correction_im_shape(image_array, dim_utils, shifts_sum):
  # get new shape
  new_shape = list(image_array.shape)

  # assuming shifts are Z, Y, X
  if dim_utils.is_3D():
    new_shape[dim_utils.dim_idx('Z')] += abs(shifts_sum['sum'][0])
    new_shape[dim_utils.dim_idx('Y')] += abs(shifts_sum['sum'][1])
    new_shape[dim_utils.dim_idx('X')] += abs(shifts_sum['sum'][2])
  else:
    new_shape[dim_utils.dim_idx('Y')] += abs(shifts_sum['sum'][0])
    new_shape[dim_utils.dim_idx('X')] += abs(shifts_sum['sum'][1])

  # round new shape for new array
  new_shape_round = tuple([
    round(x) for x in new_shape
  ])

  return new_shape, new_shape_round

"""
drift correct image
"""
def drift_correct_im(
  input_array, dim_utils, phase_shift_channel,
  timepoints = None, drift_corrected_path = None,
  upsample_factor = 100, shifts = None, chunk_size = None
  ):
  # get all timepoints if not specified
  if timepoints is None:
    timepoints = range(dim_utils.dim_val('T'))

  # get shifts
  if shifts is None:
    shifts = drift_correction_shifts(
      input_array, phase_shift_channel, dim_utils,
      # check that the first timepoint is not used
      timepoints = timepoints[1:],
      upsample_factor = upsample_factor
      )

  # get shifts summary
  shifts_sum = shifts_summary(shifts, is_3D = dim_utils.is_3D())

  # get new image dimensions
  drift_im_shape, drift_im_shape_round = correction_im_shape(
    input_array, dim_utils, shifts_sum
  )

  # get first image position
  first_im_pos = correction_first_im_pos(
    drift_im_shape, dim_utils, shifts_sum
  )

  # use new shape for chunking
  # TODO !! this assumes smaller images as we usually have for 2P
  if chunk_size is None:
    chunk_size = list(input_array.chunksize)
  
  for x in ('Y', 'X'):
    chunk_size[dim_utils.dim_idx(x)] = drift_im_shape_round[dim_utils.dim_idx(x)]
  chunk_size = tuple(chunk_size)

  # remove previous folder
  if drift_corrected_path is not None:
    shutil.rmtree(drift_corrected_path)

  # create array
  drift_correction_zarr = zarr.create(
    drift_im_shape_round,
    dtype = input_array.dtype,
    chunks = chunk_size,
    store = drift_corrected_path
  )

  # use first position for slice
  slices = first_im_pos

  # get timepoint shape
  tp_slice = [slice(None) for x in range(len(drift_im_shape_round))]
  tp_slice[dim_utils.dim_idx('T')] = slice(0, 1, 1)
  tp_slice = tuple(tp_slice)
  tp_shape = drift_correction_zarr[tp_slice].shape

  # go through timepoints and add images
  for i in timepoints:
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
    new_slices = [slice(None) for _ in range(len(drift_im_shape_round))]
    im_slices = [slice(None) for _ in range(len(drift_im_shape_round))]

    # set Z, X, Y for new slices
    for j, y in enumerate(dim_utils.spatial_axis()):
      new_slices[dim_utils.dim_idx(y)] = slice(round(slices[j].start), round(slices[j].stop), 1)

    # set time for image slice
    im_slices[dim_utils.dim_idx('T')] = slice(i, i + 1, 1)

    # convert to tuple
    new_slices = tuple(new_slices)
    im_slices = tuple(im_slices)

    if i % 10 == 0:
      print(i)

    # add to image list
    new_image = np.zeros(tp_shape)
    
    # check that slices match dimension
    if new_image[new_slices].shape != input_array[im_slices].shape:
      # get wrong dimensions
      dif_dim = [x - y for x, y in zip(
        new_image[new_slices].shape,
        input_array[im_slices].shape
      )]

      # adjust dimensions
      new_slices = list(new_slices)
      
      for j, y in enumerate(dif_dim):
        if y > 0:
          # add?
          new_slices[j] = slice(
              new_slices[j].start + y,
              new_slices[j].stop, 1)

        if y < 0:
          # add?
          if new_slices[j].start - y >= 0:
            new_slices[j] = slice(
              new_slices[j].start + y,
              new_slices[j].stop, 1)

          # subtract?
          elif new_slices[j].stop + y < drift_correction_zarr.shape[j]:
            new_slices[j] = slice(
              new_slices[j].start,
              new_slices[j].stop + y, 1)

      new_slices = tuple(new_slices)

    # copy to 'zero' image
    new_image[new_slices] = input_array[im_slices]

    # push to zarr
    drift_correction_zarr[im_slices] = new_image

  # return
  return drift_correction_zarr

"""
get position of first image for correction
"""
def correction_first_im_pos(drift_im_shape, dim_utils, shifts_sum):
  # place the first image
  if dim_utils.is_3D():
    new_pos = np.take(
      drift_im_shape,
      [dim_utils.dim_idx('Z'), dim_utils.dim_idx('Y'), dim_utils.dim_idx('X')]
      )

    shift_size = 3
  else:
    new_pos = np.take(
      drift_im_shape,
      [dim_utils.dim_idx('Y'), dim_utils.dim_idx('X')]
      )

    shift_size = 2
    
  first_pos = tuple(
    [slice(shifts_sum['min'][i],
           new_pos[i] - shifts_sum['max'][i],
           1) for i in range(shift_size)]
  )

  return first_pos

"""
Apply filter for multiple channels
"""
def apply_filter(
  input_image, dim_utils, channels_to_filter = None,
  filter_fun = 'gaussian', filter_value = 1):
  # create channel list
  output_image = [input_image[dim_utils.create_channel_slices(i)]
                  for i in range(dim_utils.dim_val('C'))]

  # create filter values
  filter_values = [0] * len(dim_utils.im_dim)

  # set filter to values to X, Y, Z
  for x in ('X', 'Y', 'Z'):
    filter_values[dim_utils.dim_idx(x)] = filter_value

  # Apply filter
  for i, x in enumerate(output_image):
    if filter_fun == 'gaussian':
      # apply gaussian
      output_image[i] = dask_image.ndfilters.gaussian_filter(
        x, sigma = filter_values
      )

  # combine dask arrays back
  # https://docs.dask.org/en/latest/array-stack.html
  output_dask = da.concatenate(output_image, axis = dim_utils.dim_idx('C'))

  # adjust chunksize and data type
  output_dask = output_dask.rechunk(chunks = input_image.chunksize)
  output_dask = output_dask.astype(input_image.dtype)

  return output_dask

"""
Correct autofluorescence using ratio of one to multiple channels
"""
def af_correct_channel(
  data, channel_idx, correction_channel_idx, dim_utils,
  channel_percentile = 80, correction_percentile = 40,
  gaussian_sigma = 1, use_dask = True, correction_mode = 'subtract',
  median_filter = 0, rolling_ball_radius = 50):
  # get slices to access data
  slices = dim_utils.create_channel_slices(channel_idx)

  # create correction image
  correction_slices = dim_utils.create_channel_slices(correction_channel_idx[0])

  # create copy
  correction_im = copy(data[correction_slices])

  if len(correction_channel_idx) > 1:
    for x in correction_channel_idx[1:]:
      correction_slices = dim_utils.create_channel_slices(x)
      correction_im = da.maximum(correction_im, data[correction_slices])

  # subtract background
  if use_dask is True:
    cleaned_image = data[slices].map_blocks(
      subtract_background,
      percentile_min = channel_percentile,
      dtype = '>u2')

    cleaned_correction = correction_im.map_blocks(
      subtract_background,
      percentile_min = correction_percentile,
      dtype = '>u2')
  else:
    # this will cause the percentile to be calculated
    # on the whole image rather than individual blocks
    # this should help to alleviate the flickering
    # in the signal when correcting
    cleaned_image = subtract_background(
      data[slices].compute(),
      percentile_min = channel_percentile)

    cleaned_correction = subtract_background(
      correction_im.compute(),
      percentile_min = correction_percentile)
      
    # create dask arrays
    cleaned_image = da.from_array(
      cleaned_image, chunks = data[slices].chunksize)
    cleaned_correction = da.from_array(
      cleaned_correction, chunks = data[slices].chunksize)

    cleaned_image.astype('>u2')
    cleaned_correction.astype('>u2')

  # remove AF from channel
  if correction_mode == 'divide':
    corrected_data = (cleaned_image + 1) / (cleaned_correction + 1)
  else:
    corrected_data = cleaned_image
  
  # apply median filter
  if median_filter > 0:
    footprint = skimage.morphology.ball(median_filter)
    footprint = np.expand_dims(footprint, axis = dim_utils.dim_idx('T'))
    footprint = np.expand_dims(footprint, axis = dim_utils.dim_idx('C'))
    
    # footprint_scale = [1] * len(dim_utils.im_dim)
    # footprint_scale[dim_utils.dim_idx('Z')] = dim_utils.omexml.images[0].pixels.physical_size_z
    # footprint_scale[dim_utils.dim_idx('Y')] = dim_utils.omexml.images[0].pixels.physical_size_y
    # footprint_scale[dim_utils.dim_idx('X')] = dim_utils.omexml.images[0].pixels.physical_size_x
    
    # reshape to image pixel size
    # footprint = scipy.ndimage.zoom(footprint, tuple(footprint_scale))
    
    corrected_data = dask_image.ndfilters.median_filter(
      corrected_data, footprint = footprint
    )
    
  # apply rolling ball
  if rolling_ball_radius > 0:
    corrected_data = corrected_data - rolling_ball(corrected_data)
  
  # create AF
  # af_data = (cleaned_correction + 1)/(cleaned_image + 1)
  # af_data = da.subtract(correction_im, corrected_data)

  # create filter values
  filter_values = [0] * len(dim_utils.im_dim)
  
  # set filter to values to X, Y, Z
  # for x in ('X', 'Y', 'Z'):
  for x in dim_utils.spatial_axis():
    filter_values[dim_utils.dim_idx(x)] = gaussian_sigma

  # apply filter
  filtered_im = dask_image.ndfilters.gaussian_filter(
    corrected_data, sigma = filter_values
  )

  # filtered_af = dask_image.ndfilters.gaussian_filter(
  #   af_data, sigma = filter_values
  # )

  # return filtered_im, filtered_af
  return filtered_im

# subtract background from array
# simple percentile
def subtract_background(array, percentile_min = 80):
  if 0 not in array.shape:
    # get percentile to subtract
    subtract_val = np.percentile(array, percentile_min)

    # subtract
    array[array < subtract_val] = subtract_val
    array = array - subtract_val

  return array

# subtract shg from array
def subtract_shg(array, footprint = None):
  if 0 not in array.shape:
    array_shape = array.shape
    
    print(footprint.shape)
    
    # apply morphological operation
    # array = array - skimage.morphology.opening(
    # array = skimage.morphology.closing(
    array = scipy.ndimage.filters.median_filter(
    # array = array - skimage.morphology.white_tophat(
    # array = skimage.morphology.black_tophat(
    # array = skimage.morphology.white_tophat(
      # np.squeeze(array), selem = footprint
      np.squeeze(array), footprint = footprint
    ).reshape(array_shape)

  return array

"""
Correct autofluoresence of image for multiple channels
"""
def af_correct_image(input_image, af_combinations, dim_utils,
                     gaussian_sigma = 1, use_dask = True,
                     apply_gaussian_to_others = True):
  # create channel list
  output_image = [input_image[dim_utils.create_channel_slices(i)]
                  for i in range(dim_utils.dim_val('C'))]

  # create AF image
  af_im = None

  # ensure that names are integers
  af_combinations = {int(i):af_combinations[i] for i in af_combinations}

  # remove empty channel items
  # https://stackoverflow.com/a/12118700/13766165
  af_combinations = {
    i: x for i, x in af_combinations.items() if len(x['divisionChannels']) > 0
    }

  # AF correct channels
  for i, x in af_combinations.items():
    # output_image[i], new_af_im = af_correct_channel(
    output_image[i] = af_correct_channel(
      input_image, i, x['divisionChannels'], dim_utils = dim_utils,
      channel_percentile = x['channelPercentile'],
      correction_percentile = x['correctionPercentile'],
      correction_mode = x['correctionMode'],
      # TODO is there a good way to correct for SHG?
      median_filter = x['medianFilter'],
      gaussian_sigma = gaussian_sigma,
      use_dask = use_dask,
      rolling_ball_radius = x['rollingBallRadius']
    )

    # # combine AF
    # if af_im is None:
    #   af_im = copy(new_af_im)
    # else:
    #   af_im = da.maximum(af_im, new_af_im)

  # if len(af_combinations) > 0:
  #   # add AF
  #   output_image.append(af_im)

  # create filter values
  filter_values = [0] * len(dim_utils.im_dim)

  # set filter to values to X, Y, Z
  # for x in ('X', 'Y', 'Z'):
  for x in dim_utils.spatial_axis():
    filter_values[dim_utils.dim_idx(x)] = gaussian_sigma

  # Apply gaussian for channels not used for
  # AF correction, they already have a gaussian
  if apply_gaussian_to_others is True:
    for i, x in enumerate(output_image):
      if len(af_combinations) == 0 or i not in af_combinations.keys():
        # apply gaussian
        output_image[i] = dask_image.ndfilters.gaussian_filter(
          x, sigma = filter_values
        )

  # combine dask arrays back
  # https://docs.dask.org/en/latest/array-stack.html
  output_dask = da.concatenate(output_image, axis = dim_utils.dim_idx('C'))

  # adjust chunksize and data type
  output_dask = output_dask.rechunk(chunks = input_image.chunksize)
  output_dask = output_dask.astype(input_image.dtype)

  return output_dask

"""
Normalise channel
"""
def norm_channel(im, normalise_percentile = 99.5, threshold = 0,
                 rel_threshold = 0):
  # apply threshold?
  if threshold > 0:
    im[im < threshold] = threshold
    im = im - threshold
    
  # apply relative threshold?
  if rel_threshold > 0:
    rel_threshold = np.percentile(im, rel_threshold)
    im[im < rel_threshold] = rel_threshold
    im = im - rel_threshold
  
  # get min/max values for channels
  max_percentile = np.percentile(im, normalise_percentile)
  min_percentile = np.percentile(im, 100 - normalise_percentile)
  
  # calculate relative values
  im = (im - min_percentile) / (max_percentile - min_percentile)
  
  # adjust
  im[im < 0] = 0
  im[im > 1] = 1
  
  return im

"""
Combine and normalise images
"""
def combine_norm_channels(im, channels, slices, dim_utils, normalise_percentile = 99.5,
                          threshold = 0, rel_threshold = 0):
  # prepare image
  slices = list(slices)
  slices[dim_utils.dim_idx('C')] = 0
  
  combined_im = np.zeros_like(im[tuple(slices)])
  
  for i in channels:
    slices = list(slices)
    slices[dim_utils.dim_idx('C')] = i
    
    # combine and normalise
    combined_im = np.maximum(
      combined_im, norm_channel(
        im[tuple(slices)],
        normalise_percentile = normalise_percentile,
        threshold = threshold,
        rel_threshold = rel_threshold
        )
      )
      
  return combined_im

"""
Normalise channels
"""
def normalise_channels(im, dim_utils, norm_percentile = 99.98, slices = None, dtype = None):
  # get slices
  if slices is None:
    slices = [slice(None) for _ in range(len(im.shape))]
    
  # define dtype
  if dtype is None:
    dtype = im.dtype
  
  # go through channels and normalise
  for x in range(dim_utils.dim_val('C')):
    norm_range = dim_utils.get_norm_range(im, channels = [x],
      min_perc = 100 - norm_percentile, max_perc = norm_percentile)
    x_min = norm_range[0]
    x_max = norm_range[1]

    # get norm
    slices[0] = slice(x, x+1, 1)
    norm_im = (im[tuple(slices)] - x_min) / (x_max - x_min)
    norm_im[norm_im > 1] = 1
    norm_im[norm_im < 0] = 0

    # copy back
    im[tuple(slices)] = norm_im * (np.iinfo(dtype).max - 1)
  
  return im
