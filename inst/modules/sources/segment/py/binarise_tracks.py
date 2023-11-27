# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np
import zarr
import itertools
#from scipy.sparse import coo_matrix
# from skimage.morphology import binary_dilation, disk, ball
import sparse
from tqdm import tqdm

# https://stackoverflow.com/a/34365537
# Create new `pandas` methods which use `tqdm` progress
# (can use tqdm_gui, optional kwargs, etc.)
tqdm.pandas()

from py.label_props_utils import LabelPropsUtils
from py.pop_utils import PopUtils
import py.label_utils as label_utils
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils
import py.config_utils as cfg

# TODO is there a more generic way?
# skeletonise tracks
def skeletonise_tracks_3D(points, digi_lines = []):  
  # get lines for tracks
  lines = [label_utils.bresenham_3D(points[i, 0], points[i, 1], points[i, 2],
                                    points[i+1, 0], points[i+1, 1], points[i+1, 2]) for i in range(points.shape[0] - 1)]
  digi_lines.append(np.array(list(itertools.chain(*lines))))

def skeletonise_tracks_2D(points, digi_lines = []):  
  lines = [label_utils.bresenham_2D(points[i, 0], points[i, 1],
                                    points[i+1, 0], points[i+1, 1]) for i in range(points.shape[0] - 1)]
  digi_lines.append(np.array(list(itertools.chain(*lines))))

# cluster cell meshes
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  im_path = script_utils.get_param(params, 'imPath')
  pops = script_utils.get_param(params, 'pops', default = list())
  
  # get data and meta data
  im_data, _ = zarr_utils.open_zarr(im_path, as_dask = True)
  omexml = ome_xml_utils.parse_meta(im_path)
  dim_utils = DimUtils(omexml)
  dim_utils.calc_image_dimensions(im_data[0].shape)
  
  # init pop utils
  pop_utils = PopUtils()
  
  # get populations
  pop_df = pop_utils.pop_df(
    task_dir,
    LabelPropsUtils(task_dir),
    'live',
    cols = ['label', 'track_id', 'centroid_z', 'centroid_y', 'centroid_x'],
    pops = pops
  )
  
  # binarise tracks
  bin_lines = list()
  
  logfile_utils.log(pop_df['centroid_z'].max())
  
  # tracks.sort_values(['track_id', 'centroid_t'], ascending=[True, True])\
  if dim_utils.is_3D():
    pop_df.groupby(['value_name', 'pop', 'track_id'])\
      .progress_apply(lambda x: skeletonise_tracks_3D(
        x[[f'centroid_{i}' for i in ['x', 'y', 'z']]].to_numpy().astype(np.uint32),
        bin_lines))
  else:
    pop_df.groupby(['value_name', 'pop', 'track_id'])\
      .progress_apply(lambda x: skeletonise_tracks_2D(
        x[[f'centroid_{i}' for i in ['x', 'y']]].to_numpy().astype(np.uint32),
        bin_lines))

  # put values into array
  tracks_array = np.array(list(itertools.chain(*bin_lines)))
  
  if dim_utils.is_3D():
    logfile_utils.log(max(tracks_array[:,0]))
    logfile_utils.log(max(tracks_array[:,1]))
    logfile_utils.log(max(tracks_array[:,2]))
    logfile_utils.log(tuple([dim_utils.dim_val(i) for i in ['z', 'y', 'x']]))
    logfile_utils.log(np.isnan(tracks_array).any())
    logfile_utils.log(np.isinf(tracks_array).any())
    
    tracks_mat = sparse.COO(
      coords = (tracks_array[:,2], tracks_array[:,1], tracks_array[:,0]),
      data = [1] * tracks_array.shape[0],
      shape = tuple([dim_utils.dim_val(i) for i in ['z', 'y', 'x']]))
  else:
    tracks_mat = sparse.COO(
      coords = (tracks_array[:,1], tracks_array[:,0]),
      data = [1] * tracks_array.shape[0],
      shape = tuple([dim_utils.dim_val(i) for i in ['y', 'x']]))
  
  # save back
  logfile_utils.log(f'> save zarr')
  
  store_name = value_name + '.tracks'
  store_path = os.path.join(task_dir, cfg.value_dir(store_name, 'labels'))
  
  # get shape
  store_shape = list(im_data[0].shape)
  store_shape.pop(dim_utils.dim_idx('C'))
  store_shape.pop(dim_utils.dim_idx('T'))
  store_shape = tuple(store_shape)
  
  store_chunks = list(im_data[0].chunksize)
  store_chunks.pop(dim_utils.dim_idx('C'))
  store_chunks.pop(dim_utils.dim_idx('T'))
  store_chunks = tuple(store_chunks)
  
  tracks_store, _ = zarr_utils.create_zarr_from_ndarray(
    tracks_mat.todense() > 0, dim_utils, im_data[0], store_path = store_path,
    copy_values = True, ignore_channel = True, ignore_time = True, remove_previous = True)
  
  nscales = len(im_data)
  if nscales > 1:
    multiscales_file_path = store_path + '.multiscales'

    zarr_utils.create_multiscales(
      tracks_store, multiscales_file_path,
      dim_utils = dim_utils,
      nscales = nscales,
      keyword = 'labels',
      ignore_channel = True,
      squeeze = True
    )

    # remove previous labels and rename multiscales
    shutil.rmtree(store_path)
    os.rename(multiscales_file_path, store_path)

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['pops']
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
