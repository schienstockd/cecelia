#!/bin/bash

# load modules
echo 'Load Modules'
module load Java/8.372
module load Miniconda3/22.11.1-1
. /apps/easybuild-2022/easybuild/software/Core/Miniconda3/22.11.1-1/bin/activate

# use different conda env path
export CONDA_ENVS_PATH=/data/gpfs/projects/punim1124/cecelia/envs/

# activate conda environment
echo 'Activate conda environment'
conda activate r-cecelia-env

# install cecelia
echo 'Install Cecelia'
R -e 'remotes::install_github("schienstockd/cecelia", repos = "https://cloud.r-project.org")'