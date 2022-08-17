#@String imPathIn
#@String imPathOut
#@Integer switchZforC

import sys
import os
from ij import IJ

from ij.plugin import FolderOpener

# why do I need to do this .. ?
sys.path.append(".")

# open image
imp = FolderOpener.open(imPathIn, "");

# switch channels and time?
if switchZforC > 0:
	dim = imp.getDimensions()
	imp.setDimensions(dim[3], dim[2], dim[4])

# save back
IJ.saveAs(imp, "Tiff", imPathOut)