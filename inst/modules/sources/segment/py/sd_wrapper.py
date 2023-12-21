# add CCIA modules
import sys
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.stardist_utils import StarDistUtils
from py.dim_utils import DimUtils
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

  # get max from lowest res
  norm_range = dim_utils.get_norm_range_from_low_res(
    im_dat, params['nucleiChannels'])

  # prepare parameters
  seg_params = {
	'ccia': params['ccia'],
	'nuclei_channels': params['nucleiChannels'],
	'label_expansion': params['labelExpansion'],
	'label_erosion': params['labelErosion'],
	'halo_size': params['haloSize'],
	'halo_whole_cell': params['haloWholeCell'],
	'remove_small_objects': params['minCellSize'],
	'subtract_edges': True,
	'norm_range': norm_range,
	'process_as_zarr': True,
	'dim_utils': dim_utils,
	'task_dir': params['taskDir'],
	'im_path': params['imPath'],
	'segment': params['segment'] if 'segment' in params else True,
	'measure': True,
	'update_measures': params['updateMeasures'] if 'updateMeasures' in params else False,
	'save_measures': params['saveMeasures'] if 'saveMeasures' in params else True,
	'save_meshes': params['saveMeshes'] if 'saveMeshes' in params else False,
	'find_contours': False,
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
  sd = StarDistUtils(seg_params)
  labels = sd.predict(im_dat)

def main():
  # get params
  params = script_utils.script_params()

  # run stardist
  run(params)

if __name__ == '__main__':
  main()
