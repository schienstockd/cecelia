#!/bin/bash

# load modules
echo 'Load Modules'
module load Java/8.372
module load Miniconda3/22.11.1-1
. /apps/easybuild-2022/easybuild/software/Core/Miniconda3/22.11.1-1/bin/activate

# use different conda env path
export CONDA_ENVS_PATH=/data/gpfs/projects/punim1124/cecelia/envs/

# create conda environment
echo 'Create R/python conda environment'
# R 'openssl' needs openssl1
conda create -y -n r-cecelia-env -c conda-forge r-openssl r-base=4.3.1
conda activate r-cecelia-env
conda install -y -c conda-forge python=3.9
pip install cmake

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

# `GLIBCXX_3.4.30` not found
conda install -c conda-forge libstdcxx-ng=12