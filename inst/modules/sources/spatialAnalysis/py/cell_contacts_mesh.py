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
from tqdm import tqdm

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
  # pop_type = script_utils.get_param(params, 'popType', default = 'default')
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  im_res = script_utils.get_param(params, 'imRes', default = None)
  is_3D = script_utils.get_param(params, 'is3D', default = False)
  is_timecourse = script_utils.get_param(params, 'isTimecourse', default = False)
  
  # prepare bbox and centroid axis
  bbox_axis = ['x', 'y', 'z'] if is_3D is True else ['x', 'y']
  centroid_axis = ['t'] + bbox_axis if is_timecourse is True else bbox_axis
  
  # prepare property columns to get
  prop_cols = [f'bbox_{side}_{axis}' for side in ['min', 'max'] for axis in bbox_axis]
  prop_cols.extend([f'centroid_{axis}' for axis in centroid_axis])

  # get properties
  max_contact_dist = script_utils.get_param(params, 'maxContactDist', default = 5)
  pops_a = script_utils.get_param(params, 'popsA', default = list())
  pops_b = script_utils.get_param(params, 'popsB', default = list())
  invert_pops_a = script_utils.get_param(params, 'invertPopsA', default = False)
  check_b_contains_a = script_utils.get_param(params, 'checkBContainsA', default = False)

  # init pop utils
  pop_utils = PopUtils()
  
  # go through pops A
  for pop_a in pops_a:
    # get pop name and type
    pop_split = pop_a.split('.', 1)
    pop_type_a = pop_split[0]
    pop_a = pop_split[1]
    
    # get population for bbox information
    pop_df_a = pop_utils.pop_df(
      task_dir,
      LabelPropsUtils(task_dir),
      pop_type_a,
      cols = prop_cols,
      # TODO invert population for randomised testing
      invert = invert_pops_a,
      pops = [pop_a]
    )
    
    # same value for all within a population
    pop_value_name_a = pop_df_a['value_name'][0]
    
    # get label props
    label_props_utils = LabelPropsUtils(task_dir, value_name = pop_value_name_a)

    # save back to labels
    labels_ids = label_props_utils.label_props_view()\
      .view_label_col()\
      .values_obs()
    
    # go through pops B
    for pop_b in pops_b:
      # get pop name and type
      pop_split = pop_b.split('.', 1)
      pop_type_b = pop_split[0]
      pop_b = pop_split[1]
      
      # get contact populations
      pop_df_b = pop_utils.pop_df(
        task_dir,
        LabelPropsUtils(task_dir),
        pop_type_b,
        cols = prop_cols,
        pops = [pop_b]
        )
      
      # define columns
      if invert_pops_a is True:
        dist_col = f'{pop_type_a}.cell.min_distance.inv#{pop_type_b}.{pop_b}'
        contact_col = f'{pop_type_a}.cell.contact.inv#{pop_type_b}.{pop_b}'
        contained_col = f'{pop_type_a}.cell.contained_by.inv#{pop_type_b}.{pop_b}'
        contains_n_col = f'{pop_type_a}.cell.contains_n.inv#{pop_type_b}.{pop_b}'
        contact_id_col = f'{pop_type_a}.cell.contact_id.inv#{pop_type_b}.{pop_b}'
      else:
        dist_col = f'{pop_type_a}.cell.min_distance#{pop_type_b}.{pop_b}'
        contact_col = f'{pop_type_a}.cell.contact#{pop_type_b}.{pop_b}'
        contained_col = f'{pop_type_a}.cell.contained_by#{pop_type_b}.{pop_b}'
        contains_n_col = f'{pop_type_a}.cell.contains_n#{pop_type_b}.{pop_b}'
        contact_id_col = f'{pop_type_a}.cell.contact_id#{pop_type_b}.{pop_b}'
        
      if pop_df_b is not None:
        # same value for all within a population
        pop_value_name_b = pop_df_b['value_name'][0]
    
        # get timepoints
        timepoints = [-1]
        
        if is_timecourse is True:
          timepoints = script_utils.get_param(params, 'timepoints', default = None)
    
          if timepoints is None:
            timepoints = pop_df_a['centroid_t'].unique()
        
        # go through timepoints and get contacts
        contacts = dict()
        contained = dict()
        contains_n = dict()
        contact_ids = dict()
        
        for i, t in tqdm(enumerate(timepoints)):
          # load meshes
          meshes_a = morpho_utils.df_to_meshes(
            task_dir,
            pop_df_a, pop_value_name_a,
            'centroid_t' if t >= 0 else 'NONE',
            [t] if t >= 0 else ['NONE'],
            im_res = im_res, is_3D = is_3D,
            add_value_name_to_name = False)
          meshes_b = morpho_utils.df_to_meshes(
            task_dir,
            pop_df_b, pop_value_name_b,
            'centroid_t' if t >= 0 else 'NONE',
            [t] if t >= 0 else ['NONE'],
            im_res = im_res, is_3D = is_3D,
            add_value_name_to_name = False)
    
          logfile_utils.log(f'>> (t {t}) {pop_a} loaded {len(meshes_a)} meshes')
          logfile_utils.log(f'>> (t {t}) {pop_b} loaded {len(meshes_b)} meshes')
    
          if len(meshes_b) > 0:
            # add pop B to collision manager
            m = trimesh.collision.CollisionManager()
            
            for j, y in meshes_b.items():
              m.add_object(j, y)
            
            # go through pop A and get minimum distances to pop B
            meshes_dist = {j: m.min_distance_single(y, return_name = True) for j, y in meshes_a.items()}
            
            # update contacts
            contacts.update({j: y[0] for j, y in meshes_dist.items()})
            contact_ids.update({j: y[1] for j, y in meshes_dist.items()})
              
            # check whether B contains A
            contained.update({j: meshes_b[contact_ids[j]].contains([y.center_mass]).all() for j, y in meshes_a.items()})
            
            # check how many B are contained in A
            contains_n.update({
              j: [x.contains([z.center_mass for z in meshes_b.values()])].count(True)
              for j, y in meshes_a.items()
              })
            
        logfile_utils.log(f'>> Add distances back')
        
        # convert to dataframe
        contact_df = pd.DataFrame.from_dict({
          'label_id': contacts.keys(),
          dist_col: contacts.values(),
          contact_col: [x <= max_contact_dist for x in contacts.values()],
          contained_col: contained.values(),
          contains_n_col: contains_n.values(),
          contact_id_col: contact_ids.values()
        })
        
        # merge contacts to labels
        merged_contacts_ids = labels_ids.join(contact_df.set_index('label_id'), on = 'label')
      else:
        # fill with NaN
        merged_contacts_ids = labels_ids.copy()
        merged_contacts_ids[dist_col] = np.NaN
        merged_contacts_ids[contact_col] = np.NaN
        merged_contacts_ids[contained_col] = np.NaN
        merged_contacts_ids[contains_n_col] = np.NaN
        merged_contacts_ids[contact_id_col] = np.NaN
        
      # set NaN to False
      merged_contacts_ids[dist_col].replace(np.NaN, -1, inplace = True)
      merged_contacts_ids[contact_col].replace(np.NaN, False, inplace = True)
      merged_contacts_ids[contained_col].replace(np.NaN, False, inplace = True)
      merged_contacts_ids[contains_n_col].replace(np.NaN, 0, inplace = True)
      merged_contacts_ids[contact_id_col].replace(np.NaN, -1, inplace = True)
  
      # convert column to dict
      contact_dict = {
        dist_col: merged_contacts_ids[dist_col],
        contact_col: merged_contacts_ids[contact_col],
        contained_col: merged_contacts_ids[contained_col],
        contains_n_col: merged_contacts_ids[contains_n_col],
        contact_id_col: merged_contacts_ids[contact_id_col]
      }
  
      logfile_utils.log(
        "> Save to " + str(label_props_utils.label_props_view()\
          .adata_filepath()))
  
      # # add to obs and save
      label_props_utils.label_props_view()\
          .add_obs(contact_dict)\
          .save()

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['popsA', 'popsB', 'timepoints']
  )

  # run
  run(params)

if __name__ == "__main__":
  main()
