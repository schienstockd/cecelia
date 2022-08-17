# -*- coding: utf-8 -*-

from ij import IJ
import sys
import os
from ConfigParser import SafeConfigParser
from java.lang.System import getProperty

# ImageJ
from ij.process import StackConverter, ImageProcessor, ImageConverter
from fiji.plugin.trackmate import Spot

# import modules
from ccia.wrappers.trackingWrapper import trackSpots
from ccia.wrappers.segmentationWrapper import seedBasedWatershed
import ccia.correct3Ddrift as c3dd

def getBooleanFromString(stringVal):
	return False if stringVal == "False" else True

def processImageLive(
    imPath, segPath = None, cellRadius = 3.0,
    blobChannels = list(), donutChannels = list(),
    gaussianFilter = 1, medianFilter = 1,
    maximumFilter = 1, minimumFilter = 1,
    detectionThreshAdj = 0.5, filteringAfterSeg = 0, rollingRadius = 0,
    doSegment = 1):
	"""
	Process histology image

	Arguments:
	    imPath {string} 			-- Path to image
	   	segPath {string} 			-- Path to segmented output
	    cellRadius {float}       	-- Cell radius
	    blobChannels {list}         -- Channels resembling blobs
	    donutChannels {list}        -- Channels resembling donuts
	    gaussianFilter {float}      -- Gaussian filter
	    medianFilter {float}      	-- Median filter
	    maximumFilter {float}       -- Maximum filter
	    minimumFilter {float}       -- Minimum filter
	    detectionThreshAdj {float}  -- Adjust autothreshold
	    filteringAfterSeg {int}     -- Filtering value after segmentation
	    rollingRadius {int}         -- Value for rolling ball background subtraction
	   	doSegment {boolean}         -- Segment image

	Returns:

	"""
	#Read config file
	cfg = SafeConfigParser()
	cfg.read(os.path.join("ccia", "imProcessor.config"))

	if blobChannels is None or blobChannels == "":
	    blobChannels = list()
	elif type(blobChannels) == str:
	    blobChannels = blobChannels.split(',')
	    blobChannels = [int(i) for i in blobChannels]

	if donutChannels is None or donutChannels == "":
	    donutChannels = list()
	elif type(donutChannels) == str:
	    donutChannels = donutChannels.split(',')
	    donutChannels = [int(i) for i in donutChannels]

	# default parameter definition
	makeIsotropic = getBooleanFromString(cfg.get("SEGMENTATION", "makeIsotropic"))
	make8bit = getBooleanFromString(cfg.get("SEGMENTATION", "make8bit"))
	detectionThresh = float(cfg.get("SEGMENTATION", "detectionThresh"))
	doSubpixelDetection = getBooleanFromString(cfg.get("SEGMENTATION", "doSubpixelDetection"))
	doMedianFiltering = getBooleanFromString(cfg.get("SEGMENTATION", "doMedianFiltering"))
	doAutofiltering = getBooleanFromString(cfg.get("SEGMENTATION", "doAutofiltering"))
	doSeparateCells = getBooleanFromString(cfg.get("SEGMENTATION", "doSeparateCells"))

	IJ.log(">> STARTED")

	# set current directory and filename
	rawDir = os.path.dirname(imPath)
	imFilename = os.path.splitext(os.path.basename(imPath))[0]

	IJ.log(">> Process " + imFilename)

	# open image
	imp = IJ.openImage(imPath)
	imp.setTitle("raw")

	# (width, height, nChannels, nSlices, nFrames)
	dim = imp.getDimensions()
	cal = imp.getCalibration()

	# make "isotropic"
	if dim[3] > 1 and makeIsotropic is True:
	    newDepth = int(dim[3] * (cal.pixelDepth/cal.pixelWidth))

	    # resize image to make "isotropic"
	    imp = Resizer().zScale(imp, newDepth, ImageProcessor.BILINEAR)

	    # update calibration
	    cal = imp.getCalibration()

	# convert to 8bit to speed up processing
	converter = StackConverter(imp)
	if make8bit is True:
		converter.convertToGray8()
	else:
	    converter.convertToGray16()

	# create donut and blob image
	donbloImp = createDonutBlobImage(
		imp, donutChannels, blobChannels, gaussianFilter, medianFilter, maximumFilter, minimumFilter)

	# invert to spots
	IJ.run(donbloImp, "Invert", "stack")

	# remove background
	if rollingRadius > 0:
	    IJ.run(donbloImp, "Subtract Background...",
	        "rolling=%d stack" % rollingRadius)

	# smoothen
	IJ.run(donbloImp, "Gaussian Blur...", "sigma=1 stack")

	# reset calibration from original image
	donbloImp.setTitle("markerDetection")
	donbloImp.setCalibration(cal)

	# extract seeds
	detectedSpots, threshCalc = cellDetection3D(
	    donbloImp, cellRadius, detectionThresh, doSubpixelDetection,
	    doMedianFiltering, doAutofiltering,
	    autoFilteringValue = Spot.QUALITY,
	    detectionThreshAdj = detectionThreshAdj)

	print(str(doSegment))
	print(str(detectedSpots))
	
	if doSegment > 0 and detectedSpots is not None:
		# prepare marker image
		markerImp = writeSpotsToImage(imp, detectedSpots)
	
		#path = os.path.join(
		#	rawDir, imFilename + ".markers" + cfg.get("EXTENSIONS", "csv"))
	
		# save markers to CSV if wanted
		#writeSpotsToCSV(imp, detectedSpots,
		#	os.path.join(
		#		rawDir, imFilename + cfg.get("EXTENSIONS", "csv")))
	
		# set calibration from original image
		markerImp.setTitle("markers")
		markerImp.setCalibration(cal)
	
		# TODO for testing only!
		#IJ.saveAs(donbloImp, "Tiff",
		#    os.path.join(rawDir, imFilename + "_donblo_" + cfg.get("EXTENSIONS", "tif")))
	
		#IJ.saveAs(markerImp, "Tiff",
		#    os.path.join(rawDir, imFilename + "_markers_" + cfg.get("EXTENSIONS", "tif")))
	
		# segment spots with extracted seeds
		segImp = segmentImage3D(
		    donbloImp, markerImp, filteringAfterSeg)
	
		# close markers
		markerImp.close()
	
		# find edges to separate the segmentation better
		if doSeparateCells is True:
		    segImp = separateCells(segImp)
	
		# get segmentation path
		if segPath == "" or segPath is None:
			segPath = os.path.join(rawDir, imFilename + ".seg" + cfg.get("EXTENSIONS", "tif"))
	
		# save segmentation
		IJ.saveAs(segImp, "Tiff", segPath)
	
		# close images
		segImp.close()

	# close images
	imp.close()

	# close all to be sure
	IJ.run("Close All", "")
