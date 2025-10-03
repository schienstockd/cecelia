#!/bin/bash
# Based on the University of Melbourne job script generator for SLURM

# Partition for the job:
#SBATCH --partition=cascade

# Multithreaded (SMP) job: must run on one node
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="[CCIA] Install conda environment"

# The project ID which this job should run under:
#SBATCH --account="punim1124"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2

# The amount of memory in megabytes per process in the job:
#SBATCH --mem=51200

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=00-01:00:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from the directory where it was launched (default)
module load Java/8.372
module load Miniconda3/22.11.1-1

# otherwise rJava does not work
export LD_LIBRARY_PATH=~/.conda/envs/r-cecelia-env/lib:$JAVA_HOME/jre/lib/amd64/server/

# use different conda env path
export CONDA_ENVS_PATH=/data/gpfs/projects/punim1124/cecelia/envs/

# need to activate conda environment for R to work
. /apps/easybuild-2022/easybuild/software/Core/Miniconda3/22.11.1-1/bin/activate
conda activate 'r-cecelia-env'

# The job command(s):
R -e 'cecelia::cciaCondaCreate(envType = "image-nogui")'
