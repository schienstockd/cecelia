#!/bin/bash
# Based on the University of Melbourne job script generator for SLURM

# Partition for the job:
##SBATCH --qos=$CCIA_QOS
#SBATCH --partition=$CCIA_PARTITIONS

# Multithreaded (SMP) job: must run on one node
#SBATCH --nodes=$CCIA_nNODES

# The name of the job:
#SBATCH --job-name="$CCIA_JOB_NAME"

# The project ID which this job should run under:
#SBATCH --account="$CCIA_PROJECT_ACCOUNT"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=$CCIA_nTASKS
#SBATCH --cpus-per-task=$CCIA_CPU_PER_TASK

# Number of GPUs requested per node:
#SBATCH --gres=gpu:$CCIA_GPU_PER_TASK
# The amount of memory in megabytes per process in the job:
#SBATCH --mem=$CCIA_MEMORY

# Send yourself an email when the job:
$CCIA_EMAIL_ALERTS

# Use this email address:
#SBATCH --mail-user=$CCIA_EMAIL_ADDRESS

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=$CCIA_WALLTIME

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from the directory where it was launched (default)

# The modules to load:
# use tensorflow==2.4.0
# https://www.tensorflow.org/install/source#gpu
# module load fosscuda/2020b
# module load openblas/0.3.18
# module load cudnn/8.1.0.77-cuda-11.2.1
# compiled with gcccore/10.2.0; 'module av python/'
# module load python/3.9.6
# module load r/4.2.0
# ImageJ does not work with 11
# module load java/11.0.2
module load java/1.8.0_241
module load miniconda3/4.9.2

# for segmentation calculations
# module load eigen/3.3.8
# module load cgal/4.14.1-python-3.7.4

# otherwise rJava does not work
export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64/server/

# otherwise cudnn libraries are not found
export LD_LIBRARY_PATH=/usr/local/easybuild-2019/easybuild/software/core/cudnn/8.0.4.30-cuda-11.1.1/lib64\
${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

# need to activate conda environment for R to work
. /usr/local/easybuild-2019/easybuild/software/core/miniconda3/4.9.2/bin/activate
conda activate 'r-cecelia-env'

# The job command(s):
$CCIA_JOB_COMMAND
