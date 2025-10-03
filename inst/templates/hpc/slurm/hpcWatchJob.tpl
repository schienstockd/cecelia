#!/bin/bash
# has the job already finished?
if test -f "$JOB_SLURM_OUT.fail"
then
  echo "NOT OK"
  exit 1
fi

if test -f "$JOB_SLURM_OUT.success"
then
  echo "OK"
  exit 0
fi

# else wait, then exit

watch -g -t -n 0.1 "ls $JOB_SLURM_OUT | grep '.[success|fail]'"

if test -f "$JOB_SLURM_OUT.fail"
then
  echo "NOT OK"
  exit 1
fi
