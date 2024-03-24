# add CCIA modules
import sys
import os
sys.path.append('./')
import numpy as np
import json

import py.script_utils as script_utils
import py.zarr_utils as zarr_utils

from n2v.models import N2VConfig, N2V
from n2v.utils.n2v_utils import manipulate_val_data
from n2v.internals.N2V_DataGenerator import N2V_DataGenerator

# generate training images
def run(params):
  # there should an image path and a number of directories
  # to save the training images to
  # these directories should be generated beforehand in R
  # as images belonging to a training set

  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get parameters
  value_name = script_utils.get_ccia_param(params, 'value_name', default = None)
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  zero_dir = script_utils.get_param(params, 'zeroDir', default = 'default')
  im_source = script_utils.get_param(params, 'imSource', default = 'default')
  uids = script_utils.get_param(params, 'uIDs', default = 'default')
  patch_xy = script_utils.get_param(params, 'patchXY', default = 64)
  patch_z = script_utils.get_param(params, 'patchZ', default = 4)
  train_epochs = script_utils.get_param(params, 'trainEpochs', default = 20)
  model_dir = script_utils.get_param(params, 'modelDir', default = '/tmp')
  model_name = script_utils.get_param(params, 'modelName', default = '')
  model_desc = script_utils.get_param(params, 'modelDesc', default = '')
  model_authors = script_utils.get_param(params, 'modelAuthors', default = [])
  
  logfile_utils.log('> load training images')
  
  # get zarr
  imgs = [zarr_utils.open_as_zarr(os.path.join(zero_dir, i, im_source)) for i in uids]
  
  # get arrays for training
  imgs = [np.squeeze(np.array(zarr_utils.fortify(x[0]))) for x in imgs]
  
  # add time
  imgs = [np.expand_dims(x, axis = 0) for x in imgs]
  
  # add RGB
  imgs = [np.expand_dims(x, axis = len(imgs[0].shape)) for x in imgs]
  
  # adapted from https://github.com/juglab/n2v/tree/main/examples/3D
  # init datagenerator
  datagen = N2V_DataGenerator()
  
  # extract patches for training and validation
  if len(imgs[0].shape) > 4:
    patch_shape = (patch_z, patch_xy, patch_xy)
  else:
    patch_shape = (patch_xy, patch_xy)
    
  logfile_utils.log(f'> init patches {patch_shape} for {len(imgs)} images')
  
  patches = datagen.generate_patches_from_list(
    imgs[:1], shape = patch_shape, shuffle = True)
  
  # Patches are created so they do not overlap.
  # (Note: this is not the case if you specify a number of patches. See the docstring for details!)
  # Non-overlapping patches enable us to split them into a training and validation set.
  split_val = round(len(patches) * 0.8)
  
  X = patches[:split_val]
  X_val = patches[split_val:]
  
  # train_steps_per_epoch = int(X.shape[0]/64)
  
  # create training config
  # TODO do you need to be able to adjust anything else?
  config = N2VConfig(
    X, unet_kern_size = 3, 
    # train_steps_per_epoch = train_steps_per_epoch,
    train_epochs = train_epochs,
    train_loss = 'mse', batch_norm = True, 
    train_batch_size = 4, n2v_perc_pix = 0.198, n2v_patch_shape = patch_shape, 
    n2v_manipulator = 'uniform_withCP', n2v_neighborhood_radius = 5,
    # TODO this is a fix because n2v fails on ModelCheckpoint from keras
    train_checkpoint = "best.weights.h5")
    
  logfile_utils.log('> start training')
  
  # We are now creating our network model.
  model = N2V(config = config, name = value_name, basedir = model_dir)
  history = model.train(X, X_val)

  # save history to JSON
  # json.dump(history, open(os.path.join(model_dir, value_name + '.json'), 'w'))
  
  # TODO return training history to plot in shiny
  model.export_TF(
    name = model_name, 
    description = model_desc, 
    authors = model_authors,
    test_img = X_val[0,...], axes = 'ZYXC' if len(imgs[0].shape) > 4 else 'YXC',
    patch_shape = patch_shape,
    result_path = model_dir)
  
def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['patchShape', 'uIDs']
  )

  # run main function
  run(params)

if __name__ == '__main__':
  main()
