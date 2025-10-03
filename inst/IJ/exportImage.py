#@String imPathIn
#@String imPathOut
#@Boolean wildcard

import sys
import os

# why do I need to do this .. ?
sys.path.append(".")
sys.path.append("/Users/schiend/Desktop/DOHERTY/R-workspace/cecelia-dev/ImageJ/Jython")

# import modules
from ccia.wrappers.bioformatsWrapper import BFUtils

# export image
BFUtils.exportImage(imPathIn, imPathOut, wildcard = wildcard)
