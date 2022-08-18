# add CCIA modules
import sys
sys.path.append("./")

import py.script_utils as script_utils
import py.ome_xml_utils as ome_xml_utils

# segment image
def run(params):
	# change pixel type
	ome_xml_utils.change_pixel_type(params['imPath'], params['pixelType'])

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == "__main__":
  main()
