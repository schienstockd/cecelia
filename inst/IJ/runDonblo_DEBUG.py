imPath = "/Volumes/Analysis_SSD/Dominik/cecelia/projects/pEdOoZ/ANALYSIS/2/5LgH0E/donblo_tmp_copy.tif"
segPath = "/Volumes/Analysis_SSD/Dominik/cecelia/projects/pEdOoZ/ANALYSIS/2/5LgH0E/donblo_tmp_copy.seg.tif"
cellRadius = 2.0
blobChannels = [5, 6, 27]
donutChannels = []
gaussianFilter = 1
medianFilter = 1
maximumFilter = 1
minimumFilter = 1
detectionThreshAdj = 0.1
filteringAfterSeg = 0
rollingRadius = 10
doSegment = 1
postVarianceFilter = 0

import sys
import os
from ij import IJ

# why do I need to do this .. ?
sys.path.append("/Users/Dominik/R-workspace/cecelia/inst/IJ")
os.chdir("/Users/Dominik/R-workspace/cecelia/inst/IJ")

# import modules
from ccia.histoImProcessor import processHistoImage

# process image
processHistoImage(
	imPath = imPath,
	segPath = segPath,
	cellRadius = cellRadius,
	blobChannels = blobChannels,
	donutChannels = donutChannels,
	gaussianFilter = gaussianFilter,
	medianFilter = medianFilter,
	maximumFilter = maximumFilter,
	minimumFilter = minimumFilter,
	detectionThreshAdj = detectionThreshAdj,
	filteringAfterSeg = filteringAfterSeg,
	rollingRadius = rollingRadius,
	doSegment = doSegment,
	postVarianceFilter = postVarianceFilter
)

# https://forum.image.sc/t/processing-filter-headless-in-jython/48055/4
# os.system('kill -1 $PPID')
