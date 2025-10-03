#@String imPath
#@String segPath
#@Float cellRadius
#@Integer[] blobChannels
#@Integer[] donutChannels
#@Integer gaussianFilter
#@Integer medianFilter
#@Integer maximumFilter
#@Integer minimumFilter
#@Float detectionThreshAdj
#@Integer filteringAfterSeg
#@Integer rollingRadius
#@Integer doSegment
#@Integer postVarianceFilter

import sys
import os
from ij import IJ

# why do I need to do this .. ?
sys.path.append(".")

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
os.system('kill -1 $PPID')
