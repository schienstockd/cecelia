# add CCIA modules
import sys
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils
from py.ilee_utils import IleeUtils

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
    'dim_utils': dim_utils,
    'task_dir': params['taskDir'],
    'im_path': params['imPath'],
    'segment': True,
    'measure': False,
    'use_dask': params['useDask'] if 'useDask' in params else False,
    'use_gpu': params['useGPU'],
    'process_as_zarr': True,
    # segmentation blocks
    'block_size': params['blockSize'] if 'blockSize' in params else 512,
    'overlap': params['overlap'] if 'overlap' in params else 64,
    'block_size_z': params['blockSizeZ'] if 'blockSizeZ' in params else 100,
    'overlap_z': params['overlapZ'] if 'overlapZ' in params else 10,
    'context': params['context'] if 'context' in params else 52,
    'label_overlap': 0, # always MAX
    # ILEE
    'filament_channels': params['filamentChannels'],
    'normalise': params['normalise'] if 'normalise' in params else 0,
    'integrate_time': params['integrateTime'] if 'integrateTime' in params else False,
    'integrate_time_mode': params['integrateTimeMode'] if 'integrateTimeMode' in params else 'max',
    'normalise_to_whole': params['normaliseToWhole'] if 'normaliseToWhole' in params else False
  }

  # call ilee
  ilee = IleeUtils(seg_params)
  labels = ilee.predict(im_dat)

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['filamentChannels']
  )

  # run cellpose
  run(params)

if __name__ == '__main__':
  main()
