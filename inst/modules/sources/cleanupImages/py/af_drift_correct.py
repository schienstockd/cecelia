# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.correction_utils as correction_utils
import py.script_utils as script_utils

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(
    params['imPath'], as_dask = True)
    # params['imPath'], as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(params['imPath'])

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  logfile_utils.log('>> correct image')
  logfile_utils.log(params['afCombinations'])

  # correct af channels
  corrected_image = correction_utils.af_correct_image(
    im_dat[0], params['afCombinations'],
    dim_utils = dim_utils,
    logfile_utils = logfile_utils,
    apply_gaussian = params['applyGaussian'],
    apply_gaussian_to_others = params['applyGaussianToOthers'],
    # correct percentile on whole image
    # rather than on individual dask blocks
    # TODO is there a better way .. ?
    use_dask = False
  )

  if params['applyDriftCorrection'] is True:
    logfile_utils.log('>> get shifts')
    
    # get shifts
    shifts = correction_utils.drift_correction_shifts(
      corrected_image, int(params['driftChannel']), dim_utils,
      normalisation = params['driftNormalisation'] if params['driftNormalisation'] != 'none' else None
      )
      
    logfile_utils.log(shifts)
    logfile_utils.log('>> apply shifts')
    
    # do drift correction
    drift_image = correction_utils.drift_correct_im(
      corrected_image, dim_utils,
      params['driftChannel'],
      shifts = shifts
    )
  else:
    drift_image = corrected_image

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    drift_image, params['imCorrectionPath'],
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    params['imCorrectionPath'], params['imPath'],
    changed_shape = drift_image.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_dict_except = {
  	  'afCombinations': ['divisionChannels']
  	  }
  )

  # run AF and drift correction
  run(params)

if __name__ == '__main__':
  main()
