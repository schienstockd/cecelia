# add CCIA modules
import sys
import os
sys.path.append("./")

import trimesh
from collections import Counter
import re
import pandas as pd
import numpy as np
import igraph as ig

from py.label_props_utils import LabelPropsUtils
from py.pop_utils import PopUtils

import py.morpho_utils as morpho_utils
import py.slice_utils as slice_utils

import py.script_utils as script_utils
import py.config_utils as cfg

# cluster cell meshes
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  pop_type = script_utils.get_param(params, 'popType', default = 'default')
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  im_res = script_utils.get_param(params, 'imRes', default = None)
  is_3D = script_utils.get_param(params, 'is3D', default = False)
  is_timecourse = script_utils.get_param(params, 'isTimecourse', default = False)

  # prepare property columns to get
  prop_cols = [f'bbox_{side}_{axis}' for side in ['min', 'max'] for axis in ['x', 'y', 'z']]
  prop_cols.extend([f'centroid_{axis}' for axis in ['t', 'x', 'y', 'z']])

  # get properties
  min_cell_num = script_utils.get_param(params, 'minCellNum', default = 3)
  max_cluster_dist = script_utils.get_param(params, 'maxClusterDist', default = 16)
  noise_filter = script_utils.get_param(params, 'noiseFilter', default = 0)
  
  logfile_utils.log(f'>> Detect clusters with {min_cell_num} cells and {max_cluster_dist} apart')
  
  # init pop utils
  pop_utils = PopUtils()

  # go through populations
  for pop in script_utils.get_param(params, 'popsToCluster', default = list()):
    # get timepoints
    timepoints = [-1]
    
    if is_timecourse is True:
      timepoints = script_utils.get_param(params, 'timepoints', default = None)

      if timepoints is None:
        timepoints = pop_df_a['centroid_t'].unique()

    # save collision ids
    collision_ids = []
    
    # get population for bbox information
    pop_df = pop_utils.pop_df(
        task_dir,
        LabelPropsUtils(task_dir),
        pop_type,
        cols = prop_cols,
        pops = [pop]
    )
    
    # same value for all within a population
    pop_value_name = pop_df['value_name'][0]

    # go through timepoints and get collisions
    for t in timepoints:
      # load meshes
      meshes = morpho_utils.df_to_meshes(
        task_dir,
        pop_df, pop_value_name,
        'centroid_t' if t >= 0 else 'NONE',
        [t] if t >= 0 else ['NONE'],
        im_res = im_res, is_3D = is_3D,
        add_value_name_to_name = True)

      # add meshes to collision manager and graph
      m = trimesh.collision.CollisionManager()
      g = ig.Graph()

      for i, x in meshes.items():
        m.add_object(i, x)
        
      # dict_keys are not hashable
      g.add_vertices([str(x) for x in meshes.keys()])
      
      # go through all objects and get objects with minimum distances
      contacts = dict()
      if len(meshes) > 1:
        for i, x in meshes.items():
          # remove current mesh
          m.remove_object(i)
  
          has_contact = True
          removed_objects = dict()
  
          # get number of contacts in surface distance
          while has_contact is True:
            min_dist = [max_cluster_dist + 1]
            
            try:
              # get distance to nearest object
              min_dist = m.min_distance_single(x, return_name = True)
            except TypeError:
              # TypeError: 'reversed' object is not subscriptable
              # this happens if there is no mesh in CollisionManager
              # TODO is there a way to get the number of meshes
              # in the collision manager?
              pass
            
            # is below threshold?
            if min_dist[0] > max_cluster_dist:
              has_contact = False
            else:
              # add edge to graph
              g.add_edge(i, min_dist[1])
  
              # remove object
              m.remove_object(min_dist[1])
              removed_objects[min_dist[1]] = meshes[min_dist[1]]
  
          # add objects back
          for j, y in removed_objects.items():
            m.add_object(j, y)
          m.add_object(i, x)

      # graph cluster detection
      clusters = [x for x in g.components().subgraphs() if x.vcount() >= min_cell_num]
      
      # add ids
      collision_ids.extend(
        [int(re.findall('(?<=#)\d+', y.attributes()['name'])[0]) for x in clusters for y in x.vs])

      logfile_utils.log(f'>> (t {t}) {len(collision_ids)} collisions')

    logfile_utils.log(f'>> {len(collision_ids)} collisions detected')

    # convert to dataframe
    clusters_df = pd.DataFrame(
      pop_df['label'].tolist(),
      columns = ['label_id']
      )

    # add cluster column
    cluster_col = f'{pop_type}.cell.is.clust'
    clusters_df[cluster_col] = False
    clusters_df.loc[clusters_df['label_id'].isin(collision_ids), cluster_col] = True

    # drop na and sort by label id
    clusters_df.dropna(axis = 0, inplace = True)
    clusters_df.sort_values('label_id', inplace = True)

    # get label props
    label_props_utils = LabelPropsUtils(task_dir, value_name = pop_value_name)

    # save back to labels
    labels_ids = label_props_utils.label_props_view()\
      .view_label_col()\
      .values_obs()

    # merge clusters to labels
    merged_cluster_ids = labels_ids.join(clusters_df.set_index('label_id'), on = 'label')

    # set NaN to False
    merged_cluster_ids.replace(np.NaN, False, inplace = True)
    
    # filter noise
    # if noise_filter > 0:
      # TODO what is a good way to do this .. ?
      # https://stackoverflow.com/a/44870171/13766165
      # df.loc[track_id > 0].groupby('track_id')['live.cell.is.clust'].rolling(3).median()

    # get track ids as dict
    cluster_dict = {
      cluster_col: merged_cluster_ids[cluster_col]
    }

    logfile_utils.log(
      "> Save to " + str(label_props_utils.label_props_view()\
        .adata_filepath()))

    # add to obs and save
    label_props_utils.label_props_view()\
        .add_obs(cluster_dict)\
        .save()

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['popsToCluster', 'timepoints']
  )

  # run
  run(params)

if __name__ == "__main__":
  main()
