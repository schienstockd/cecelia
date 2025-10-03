#@String imPathIn
#@String imPathOut

import sys
import os
from ij import IJ

from ij.plugin import ZProjector
from loci.plugins.in import ImagePlusReader,ImporterOptions,ImportProcess

# why do I need to do this .. ?
sys.path.append(".")

# import image
# do the following instead of BF.openImagePlus()
# that did not return a regular imageplus somehow
opts = ImporterOptions()
opts.setId(imPathIn)
process = ImportProcess(opts)
process.execute()
impReader = ImagePlusReader(process)
imps = impReader.openImagePlus()
imp = imps[0]

# run MIP
imp = ZProjector.run(imp, "max all")

# save back
IJ.saveAs(imp, "Tiff", imPathOut)
