#!/bin/bash

# load modules
echo 'Load Modules'
module load foss/2021b
module load java/1.8.0_241
module load miniconda3/4.9.2
. /usr/local/easybuild-2019/easybuild/software/core/miniconda3/4.9.2/bin/activate

# create conda environment
echo 'Create conda environment'
conda create -y -n r-cecelia-env -c conda-forge python=3.9
conda activate r-cecelia-env

# install R in conda
echo 'Install R'
conda install -y -c conda-forge r-base=4.2.0

# install cecelia
echo 'Install Cecelia'
R -e 'if (!require("remotes", quietly = TRUE)) install.packages("remotes", repos = "http://cran.us.r-project.org")'
R -e 'remotes::install_github("schienstockd/cecelia")'
R -e 'library(cecelia);cciaBiocRequirements();cciaCondaCreate("image-nogui", rebuild = TRUE)'