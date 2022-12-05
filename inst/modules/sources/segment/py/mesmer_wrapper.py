# add CCIA modules
import sys
sys.path.append("./")

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.mesmer_utils import MesmerUtils
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

  # prepare parameters
  seg_params = {
	'ccia': params['ccia'],
	'models': params['models'],
	'labels_suffixes': ['nuc', 'cyto'],
	'halo_size': params['haloSize'],
	'halo_whole_cell': params['haloWholeCell'],
	'remove_small_objects': params['minCellSize'],
	'subtract_edges': True,
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
	# TODO is this important for label matching to turn off?
	'rank_labels': params['rankLabels'] if 'rankLabels' in params else False
  }

  # call stardist on highest resolution
  mesmer_utils = MesmerUtils(seg_params)
  labels = mesmer_utils.predict(im_dat[0], nscales = len(im_dat))

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['models']
  )

  # run stardist
  run(params)

if __name__ == "__main__":
  main()
