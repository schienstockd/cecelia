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
fi

CONDA_ENV=r-cecelia-env
# https://stackoverflow.com/a/70861080
CECELIA_DIR="$(cd "$(dirname "$0")/.." > /dev/null 2>&1 || exit; pwd -P)"
R_CALL="library(cecelia);cciaUse('$CECELIA_DIR',initJupyter=TRUE,launch.browser=TRUE);cciaRunApp(port=6860)"

echo $R_CALL

# init conda
. $CONDA_DIR/bin/activate
conda activate $CONDA_ENV

# run cecelia
R -e $R_CALL
