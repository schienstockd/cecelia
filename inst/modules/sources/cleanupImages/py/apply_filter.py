# add CCIA modules
import sys
import os
sys.path.append("./")

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.correction_utils as correction_utils
import py.script_utils as script_utils

# apply filter
def run(params):
  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(
    # params['imPath'], as_dask = True)
    params['imPath'], as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(params['imPath'])

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  # correct af channels
  corrected_image = correction_utils.apply_filter(
    im_dat[0],
	  dim_utils = dim_utils,
	  filter_fun = params['filterFun'],
    filter_value = params['filterValue']
  )

  # save everything into first multilevel
  corrected_path = os.path.join(params['imCorrectionPath'], "0")

  # save back
  zarr_utils.create_multiscales(
    corrected_image, params['imCorrectionPath'],
    nscales = len(im_dat), dim_utils = dim_utils)

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    params['imCorrectionPath'], params['imPath'],
    changed_shape = corrected_image.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params()

  # run AF and drift correction
  run(params)

if __name__ == "__main__":
  main()
