#!/bin/bash
# params
if [ -d ~/opt/anaconda3 ]; then
  CONDA_DIR=~/opt/anaconda3
elif [ -d ~/anaconda3 ]; then
  CONDA_DIR=~/anaconda3
elif [ -d ~/opt/miniconda3 ]; then
  CONDA_DIR=~/opt/miniconda3
elif [ -d ~/miniconda3 ]; then
  CONDA_DIR=~/miniconda3
elif [ -d /opt/miniconda3 ]; then
  CONDA_DIR=/opt/miniconda3
elif [ -d /opt/miniconda ]; then
  CONDA_DIR=/opt/miniconda
fi

CONDA_ENV=r-cecelia-env
# https://stackoverflow.com/a/70861080
CECELIA_DIR="$(cd "$(dirname "$0")/.." > /dev/null 2>&1 || exit; pwd -P)"
CECELIA_PORT=68$((1 + $RANDOM % 9))$((1 + $RANDOM % 9))

# R_CALL="library(cecelia);cciaUse('$CECELIA_DIR',initJupyter=TRUE);cciaCreateApp();cciaRunApp(port=6860,launch.browser=TRUE)"
# R_CALL="library(cecelia);cciaUse('$CECELIA_DIR',initJupyter=TRUE);cciaRunApp(port=6860,launch.browser=TRUE)"
# TODO flowCore load flowCore first, otherwise there is an issue with
# different versions of Protobuf
# TODO get 'bad value' in vroom if not loaded before
R_CALL="library(vroom);library(flowCore);library(cecelia);cciaUse('$CECELIA_DIR',initJupyter=TRUE);cciaRunApp(port=$CECELIA_PORT,launch.browser=TRUE)"

# export homebrew
export PATH=/opt/homebrew/bin:$PATH

# OMP: Error #15: Initializing libiomp5.dylib, but found libomp.dylib already initialized.
# https://github.com/dmlc/xgboost/issues/1715#issuecomment-438924721
export KMP_DUPLICATE_LIB_OK=TRUE
echo $R_CALL

# init conda
. $CONDA_DIR/bin/activate
conda activate $CONDA_ENV

# run cecelia
R -e $R_CALL
