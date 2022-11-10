#!/bin/bash

# load modules
echo 'Load Modules'
module load java/1.8.0_241
module load miniconda3/4.9.2
. /usr/local/easybuild-2019/easybuild/software/core/miniconda3/4.9.2/bin/activate

# activate conda environment
echo 'Activate conda environment'
conda activate r-cecelia-env

# install cecelia
echo 'Install Cecelia'
R -e 'remotes::install_github("schienstockd/cecelia", repos = "https://cloud.r-project.org")'