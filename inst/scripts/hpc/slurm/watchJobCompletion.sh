#!/bin/bash
# has the job already finished?
if test -f "$1.fail"; then
  echo "FAIL"
  exit 1
elif test -f "$1.success"; then
  exit 0
fi


# else wait, then exit
watch -g -t -n 1.0 "ls $1* | grep 'success\|fail'"

if test -f "$1.fail"
then
  echo "FAIL"
  exit 1
fi
