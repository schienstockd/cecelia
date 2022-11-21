#!/bin/bash

# load modules
echo 'Load Modules'
module load java/1.8.0_241
module load miniconda3/4.9.2
. /usr/local/easybuild-2019/easybuild/software/core/miniconda3/4.9.2/bin/activate

# create conda environment
echo 'Create R/python conda environment'
# R 'openssl' needs openssl1
conda create -y -n r-cecelia-env -c r r-openssl r-base=4.1.3
conda activate r-cecelia-env
conda install -y -c conda-forge python=3.9

# install cecelia
echo 'Install Cecelia'
R -e 'if (!require("remotes", quietly = TRUE)) install.packages("remotes", repos = "https://cloud.r-project.org")'
R -e 'remotes::install_github("schienstockd/cecelia", repos = "https://cloud.r-project.org")'

# install BioConductor requirements
R -e 'cecelia::cciaBiocRequirements(ask = FALSE, ncpus = 1)'

# run this in slurm job if it fails
# it needs to donwload packages - not sure that reticulate has `pip download` .. ?
if R -e 'cecelia::cciaCondaCreate(envType = "image-nogui")'; then
  echo "OK"
else
  echo "Killed - submit job to install packages"
  sbatch --wait -o ~/cciaConda.log -e ~/cciaConda.log ./scripts/hpc/slurm/installConda.slurm
  echo "OK"
fi