#!/bin/bash

# load modules
echo 'Load Modules'
module load java/1.8.0_241
module load miniconda3/4.9.2
. /usr/local/easybuild-2019/easybuild/software/core/miniconda3/4.9.2/bin/activate

# create conda environment
echo 'Create conda environment'
conda create -y -n r-cecelia-env -c conda-forge python=3.9
conda activate r-cecelia-env

# install R in conda
echo 'Install R'
# has to be 4.1.2 for now
# https://github.com/gagolews/stringi/issues/452#issuecomment-1051951327
conda install -y -c conda-forge r-base=4.1.2
conda install -y -c conda-forge openssl

# install cecelia
echo 'Install Cecelia'
R -e 'if (!require("remotes", quietly = TRUE)) install.packages("remotes", repos = "https://cloud.r-project.org")'
R -e 'remotes::install_github("schienstockd/cecelia", repos = "https://cloud.r-project.org")'
# run this in slurm job if it fails
# it needs to donwload packages - not sure that reticulate has `pip download` .. ?
if R -e 'library(cecelia);cciaBiocRequirements();cciaCondaCreate(envType = "image-nogui")'; then
  echo "OK"
else
  sbatch --wait -o ~/cciaConda.log -e ~/cciaConda.log ./scripts/hpc/slurm/installConda.slurm 
fi