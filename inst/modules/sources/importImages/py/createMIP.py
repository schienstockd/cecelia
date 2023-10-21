# add CCIA modules
import sys
sys.path.append("./")

import py.script_utils as script_utils
from py.imagej_utils import ImagejUtils

# segment image
def run(params):
	# init fiji
	imagej_utils = ImagejUtils(
	    params['fijiPath'], params['scriptsPath']
	)

	# run MIP script
	imagej_utils.run_script('createMIP', {
	  'imPathIn': params['imPathIn'],
	  'imPathOut': params['imPathOut']
	})

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
