# -*- coding: utf-8 -*-

import os

from ij import IJ, ImagePlus

from ij.plugin import ChannelSplitter, ImageCalculator
from ij.process import ImageConverter, StackConverter

from inra.ijpb.plugins import AnalyzeRegions3D
from inra.ijpb.measure import IntensityMeasures

from mcib3d.image3d.regionGrowing import Watershed3D
from mcib3d.image3d import ImageHandler
from mcib3d.image3d.segment import Segment3DSpots, LocalThresholderGaussFit, SpotSegmenterMax

def segmentImage3D(imp, markerImp, filteringAfterSeg = 0):
	"""
	Segment 3D spots

	Arguments:
	    imp {imagePlus} 			-- Image to segment
	    markerImp {imagePlus}   	-- Image with seeds

	Returns:
	    segImp {imagePlus} 	    	-- Segmented image
	"""
	# get calibration
	cal = imp.getCalibration()

	# segment spots
	segSpots = Segment3DSpots(
		ImageHandler.wrap(imp), ImageHandler.wrap(markerImp))

	# the following values were derived by trial and error
	# TODO is there a better way to show why the individual values
	# are what they are?
	segSpots.setLocalThresholder(
		LocalThresholderGaussFit(10, 1))
	segSpots.setSeedsThreshold(15)
	segSpots.setUseWatershed(True)
	segSpots.setVolumeMin(1)
	segSpots.setVolumeMax(1000000)
	segSpots.setSpotSegmenter(SpotSegmenterMax())
	segSpots.setBigLabel(True)

	segSpots.segmentAll()

	size = segSpots.getObjects().size()
	IJ.log("Number of labelled objects: " + str(size))

	# get segmentation
	segImp = ImagePlus("seg", segSpots.getInternalLabelImage().getImageStack())
	IJ.run(segImp, "Maximum...", "sigma=%d stack" % (filteringAfterSeg))
	IJ.run(segImp, "Median...", "radius=%d stack" % (filteringAfterSeg))

	# set calibration
	segImp.setCalibration(cal)

	return segImp

def separateCells(segImp):
	"""
	Separate segmented Cells

	TODO does that also work in 3D?

	Arguments:
	    segImp {imagePlus} 			-- Segmented cells to process

	Returns:
	    segImp {imagePlus} 	    	-- Separated cells
	"""
	# get dimensions
	dim = segImp.getDimensions()

	# duplicate
	edgesImp = segImp.duplicate()

	# get edges from segmented cells
	IJ.run(edgesImp, "Find Edges", "stack")

	# convert to 8bit before thresholding
	if dim[3] > 1:
		StackConverter(edgesImp).convertToGray8()
	else:
		ImageConverter(edgesImp).convertToGray8()

	# threshold and skeletonise
	IJ.setAutoThreshold(edgesImp, "Default dark")
	IJ.run(edgesImp, "Convert to Mask", "stack")
	IJ.run(edgesImp, "Skeletonize", "stack")

	# invert and set all values to '1'
	IJ.run(edgesImp, "Invert", "stack")
	IJ.run(edgesImp, "Max...", "value=1")

	# subtract skeleton from cell segmentation
	segImp = ImageCalculator().run(
		"Multiply create stack", segImp, edgesImp)
	edgesImp.close()

	return segImp

def analyseCells(
	imp, segImp, filename, morphologiesDir, intensitiesDir):
	"""
	Extract intensities and morphologies for segmented cells

	Arguments:
	    segImp {imagePlus} 			-- Segmented cells to process

	Returns:
	    results {boolean} 	    	-- Success or fail
	"""
	# size and shape
	rsTable = AnalyzeRegions3D().process(segImp)

	# save results
	rsTable.save(os.path.join(morphologiesDir, filename + ".csv"))

	# split channels for intensities
	splitC = ChannelSplitter.split(imp)

	IJ.run(imp, "Gaussian Blur...", "sigma=1 stack")

	# create directory
	if not os.path.exists(os.path.join(intensitiesDir, filename)):
	    os.makedirs(os.path.join(intensitiesDir, filename))

	# signal intensities
	for i in range(0, len(splitC)):
	    # get mean
	    rsTable = IntensityMeasures(splitC[i], segImp).getMean()

	    rsTable.save(os.path.join(intensitiesDir,
			filename, ("C%d" % (i + 1)) + ".csv"))

	    splitC[i].close()

	return True

# adapted from https://forum.image.sc/t/filter-spots-in-trackmate-via-scripting/30744
def seedBasedWatershed(
	implus, model, postFilt, spotsThreshold, thrAdj = 0.5, channelToSegment = 1):
	"""
	Segment cells with seeds from TM

	Arguments:
	    implus {ImagePlus}		-- ImagePlus of the image to use for detection
	    model {Model}   		-- TM model
	    postFilt {bool}			-- post segmentation filter
	    spotsThreshold{double}	-- threshold for watershed segmentation
	    thrAdj {double}			-- adjustment for threshold

	Returns:
	   	 {ImagePlus} -- Segmented image
	"""
	# Returns the dimensions of this image (width, height, nChannels, nSlices, nFrames) as a 5 element int array.
	dim = implus.getDimensions()
	cal = implus.getCalibration()

	# get tracking model
	trackModel = model.getTrackModel()

	# create new image
	trackIDImps = list()
	surfaceIDImps = list()

	surfaceIDs = dict()

	surfaceIDCounter = 0

	# go through timepoints and segment
	for curT in range(1, dim[4] + 1):
		IJ.log(">> curT " + str(curT))

		# duplicate timepoint
		curImp = Duplicator().run(implus, channelToSegment, channelToSegment,
										1, dim[3], curT, curT);

		# prepare image to save spots
		markersStack = ImageStack.create(dim[0], dim[1], dim[3], 16)

		# go over visible spots and add to image
		if model.getSpots().getNSpots(True) > 0:
			for i, curSpot in enumerate(model.getSpots().iterable(curT, True)):
				curTrackID = trackModel.trackIDOf(curSpot)

				if curTrackID is not None and curTrackID > 0:
					markersStack.setVoxel(
						int(curSpot.getFeature(Spot.POSITION_X)/cal.pixelWidth),
						int(curSpot.getFeature(Spot.POSITION_Y)/cal.pixelHeight),
						int(curSpot.getFeature(Spot.POSITION_Z)/cal.pixelDepth),
						trackModel.trackIDOf(curSpot))

			# segment, use half the spots threshold
			curWater = Watershed3D(curImp.getImageStack(), markersStack, spotsThreshold*thrAdj, 0).getWatershedImage3D()

			del markersStack

			# transfer pixel values to new image with track IDs
			curTrackIDImp = ImageHandler.newBlankImageHandler('Track IDs', curWater)
			curSurfaceIDImp = ImageHandler.newBlankImageHandler('Surface IDs', curWater)

			curWaterStack = curWater.getImagePlus().getImageStack()

			for i, curSpot in enumerate(model.getSpots().iterable(curT, True)):
				curTrackID = trackModel.trackIDOf(curSpot)

				if curTrackID is not None and curTrackID > 0 and trackModel.isVisible(curTrackID) is True:
					curX = int(curSpot.getFeature(Spot.POSITION_X)/cal.pixelWidth)
					curY = int(curSpot.getFeature(Spot.POSITION_Y)/cal.pixelHeight)
					curZ = int(curSpot.getFeature(Spot.POSITION_Z)/cal.pixelDepth)

					# small square
					curVoxels = [2**5]

					# adjust X, Y, Z for boundaries
					curX = curX if curX < dim[0] - 2 else dim[0] - 3
					curY = curY if curY < dim[1] - 2 else dim[1] - 3
					curZ = curZ if curZ < dim[3] - 2 else dim[3] - 3

					curX = curX if curX - 2 >= 0 else 2
					curY = curY if curY - 2 >= 0 else 2
					curZ = curZ if curZ - 2 >= 0 else 2

					curVoxels = curWaterStack.getVoxels(curX - 2, curY - 2, curZ - 2,
						5, 5, 5, curVoxels)
					curSurfaceID = max(curVoxels)

					if curSurfaceID > 0:
						# increment surface ID
						surfaceIDCounter += 1

						# assign surface info to spot
						surfaceIDs[curSpot.ID()] = surfaceIDCounter

						curWater.transfertPixelValues(curTrackIDImp, int(curSurfaceID), curTrackID)
						curWater.transfertPixelValues(curSurfaceIDImp, int(curSurfaceID), surfaceIDCounter)

			# close waterstack
			del curWaterStack

			# add to image
			trackIDImps.append(curTrackIDImp.getImagePlus())
			surfaceIDImps.append(curSurfaceIDImp.getImagePlus())

		# close duplicate
		curImp.close()
		del curImp

	surfaceIDImp = Concatenator().concatenate(surfaceIDImps, False)
	surfaceIDImp.setTitle("surfaces IDs")

	trackIDImp = Concatenator().concatenate(trackIDImps, False)
	trackIDImp.setTitle("track IDs")

	if postFilt > 0:
		IJ.run(surfaceIDImp, "Median...", "radius=%d stack" % postFilt)
		IJ.run(trackIDImp, "Median...", "radius=%d stack" % postFilt)

	return surfaceIDImp, trackIDImp, surfaceIDs
