# add CCIA modules
import sys
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.cellpose_utils import CellposeUtils
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
    'label_suffixes': params['labelSuffixes'],
    'remove_small_objects': params['minCellSize'],
    'subtract_edges': False,
    'process_as_zarr': True,
    'dim_utils': dim_utils,
    'task_dir': params['taskDir'],
    'im_path': params['imPath'],
    'segment': params['segment'] if 'segment' in params else True,
    'measure': params['measure'] if 'measure' in params else True,
    'use_dask': params['useDask'] if 'useDask' in params else False,
    'update_measures': params['updateMeasures'] if 'updateMeasures' in params else False,
    'save_measures': params['saveMeasures'] if 'saveMeasures' in params else True,
    'save_meshes': params['saveMeshes'] if 'saveMeshes' in params else False,
    'find_contours': False,
    'use_gpu': params['useGPU'],
    # 'use_omni': params['useOmni'] if 'useOmni' in params else False,
    # segmentation blocks
    'match_threshold': params['matchThreshold'],
    'remove_unmatched': params['removeUnmatched'],
    'halo_size': params['haloSize'],
    'halo_whole_cell': params['haloWholeCell'],
    'label_expansion': params['labelExpansion'],
    'label_erosion': params['labelErosion'],
    'block_size': params['blockSize'] if 'blockSize' in params else 512,
    'overlap': params['overlap'] if 'overlap' in params else 64,
    'block_size_z': params['blockSizeZ'] if 'blockSizeZ' in params else 100,
    'overlap_z': params['overlapZ'] if 'overlapZ' in params else 10,
    'context': params['context'] if 'context' in params else 52,
    'label_overlap': params['labelOverlap'] if 'labelOverlap' in params else 0,
    # post-processing parameters
    'extended_measures': params['extendedMeasures'] if 'extendedMeasures' in params else False,
    'calc_median_intensities': params['calcMedianIntensities'] if 'calcMedianIntensities' in params else False,
    'integrate_time': params['integrateTime'] if 'integrateTime' in params else False,
    'integrate_time_mode': params['integrateTimeMode'] if 'integrateTimeMode' in params else 'max',
    'normalise_to_whole': params['normaliseToWhole'] if 'normaliseToWhole' in params else False,
    'top_hat': params['topHat'] if 'topHat' in params else 0,
    'clear_depth': params['clearDepth'] if 'clearDepth' in params else False,
    'clear_touching_border': params['clearTouchingBorder'] if 'clearTouchingBorder' in params else False,
    'rank_labels': params['rankLabels'] if 'rankLabels' in params else True,
    'timepoints': params['timepoints'] if 'timepoints' in params else None
  }

  # call cellpose on highest resolution
  cp = CellposeUtils(seg_params)
  # labels = cp.predict(im_dat[0], nscales = len(im_dat))
  labels = cp.predict(im_dat)

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['models', 'modelsOrder', 'labelSuffixes']
  )

  # run cellpose
  run(params)

if __name__ == '__main__':
  main()
