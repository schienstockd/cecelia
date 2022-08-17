# add CCIA modules
import sys
sys.path.append("./")

import py.zarr_utils as zarr_utils
from py.donblo_utils import DonbloUtils
from py.dim_utils import DimUtils
import py.ome_xml_utils as ome_xml_utils
import py.script_utils as script_utils

# segment image
def run(params):
  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(params['imPath'])

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(params['imPath'])

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  # prepare parameters
  seg_params = {
    'ccia': params['ccia'],
    # init generals
    'halo_size': params['haloSize'],
	'halo_whole_cell': params['haloWholeCell'],
	'remove_small_objects': params['minCellSize'],
    'subtract_edges': False, # ValueError: The parameter `image` must be a 2-dimensional array
    'process_as_zarr': True,
    'dim_utils': dim_utils,
    'omexml': omexml,
    'task_dir': params['taskDir'],
    'im_path': params['imPath'],
    'measure': True,
    # init params
    'fiji_path': params['fijiPath'],
    'scripts_path': params['scriptsPath'],
    'blob_channels': params['blobChannels'],
    'donut_channels': params['donutChannels'],
    'cell_radius': params['cellRadius'],
    'gaussian_filter': params['gaussianFilter'],
    'median_filter': params['medianFilter'],
    'maximum_filter': params['maximumFilter'],
    'minimum_filter': params['minimumFilter'],
    'detection_thresh_adj': params['detectionThreshAdj'],
    'rolling_radius': params['rollingRadius'],
    'label_expansion': params['labelExpansion'],
	'label_erosion': params['labelErosion'],
	# segmentation blocks
  	'block_size': params['blockSize'] if 'blockSize' in params else 512,
  	'overlap': params['overlap'] if 'overlap' in params else 64,
  	'block_size_z': params['blockSizeZ'] if 'blockSizeZ' in params else 100,
  	'overlap_z': params['overlapZ'] if 'overlapZ' in params else 10,
  	'context': params['context'] if 'context' in params else 52,
  	# post-processing parameters
  	'extended_measures': params['extendedMeasures'] if 'extendedMeasures' in params else False,
  	'clear_depth': params['clearDepth'] if 'clearDepth' in params else False,
  	'clear_touching_border': params['clearTouchingBorder'] if 'clearTouchingBorder' in params else False,
  	'rank_labels': params['rankLabels'] if 'rankLabels' in params else True
  }

  # call stardist on highest resolution
  donblo_utils = DonbloUtils(seg_params)
  labels = donblo_utils.predict(im_dat[0], nscales = len(im_dat))

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ["donutChannels", "blobChannels"]
  )

  # run donblo
  run(params)

if __name__ == "__main__":
  main()
