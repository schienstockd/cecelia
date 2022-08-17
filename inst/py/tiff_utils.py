import tifffile

# save data to tiff
def save_as_tiff(im_path, im_data, dim_utils = None,
                 physical_sizes = {i: 1 for i in ['x', 'y', 'z']},
                 physical_units = {i: 'um' for i in ['x', 'y', 'z']},
                 convert_sizes = True):
  # get physical sizes
  if dim_utils is not None:
    physical_sizes = dim_utils.im_physical_sizes()
    physical_units = dim_utils.im_physical_units()
  else:
    physical_sizes = physical_sizes
    physical_units = physical_units
  
  # convert physical unit if necessary
  physical_unit = physical_units['x']
  
  if physical_unit == 'Centimeter':
    physical_unit = 'um'
    physical_sizes = {i:x/10**4 for i, x in physical_sizes.items()}
  
  # create metadata
  metadata = {
    'unit': physical_unit
  }
  
  # create resolution
  resolution = (
    physical_sizes['x'],
    physical_sizes['y']
    )
  
  if convert_sizes is True:
    resolution = tuple([1./x for x in resolution])
  
  # add spacing?
  if 'z' in physical_sizes.keys():
    metadata['spacing'] = physical_sizes['z'],
  
  tifffile.imwrite(
    im_path, im_data,
    imagej = True,
    resolution = resolution,
    # 'axes': "TZCYX", # gives error - not sure why
    metadata = metadata
  )
