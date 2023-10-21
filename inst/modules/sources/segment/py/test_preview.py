# add CCIA modules
import sys
import os
sys.path.append("./")

import time

import py.script_utils as script_utils

# test logs
def run(params, inplace = True):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  for i in range(100):
    logfile_utils.log(f'python sleep {i}')

    time.sleep(1/10)


def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params, inplace = False)

if __name__ == '__main__':
  main()
