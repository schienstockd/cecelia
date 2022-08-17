# -*- coding: utf-8 -*-

import re
import os
import sys
import csv

# ImageJ imports
from net.imglib2.img.display.imagej import ImageJFunctions

from ij.plugin.frame import RoiManager
from ij.plugin import ChannelSplitter, ImageCalculator, ZProjector, Resizer
from ij.process import StackConverter, ImageProcessor, ImageConverter
from ij.measure import ResultsTable
from ij.gui import PointRoi
from ij import ImageStack, ImagePlus, IJ

# TrackMate imports
from fiji.plugin.trackmate import Model, Settings, TrackMate, SelectionModel, Logger, Spot
from fiji.plugin.trackmate.detection import LogDetectorFactory
from fiji.plugin.trackmate.providers import EdgeAnalyzerProvider, TrackAnalyzerProvider, SpotAnalyzerProvider
from fiji.plugin.trackmate.tracking import LAPUtils
from fiji.plugin.trackmate.tracking.kalman import KalmanTrackerFactory
from fiji.plugin.trackmate.tracking.sparselap import SparseLAPTrackerFactory

import fiji.plugin.trackmate.util.TMUtils as TMUtils
import fiji.plugin.trackmate.features.FeatureFilter as FeatureFilter
from fiji.plugin.trackmate.action import LabelImgExporter

def cellDetection3D(
	imp, cellRadius, detectionThresh, doSubpixelDetection,
	doMedianFiltering, doAutofiltering, targetChannel = 1,
	autoFilteringValue = None, detectionThreshAdj = 1):
	"""
	Detect cells in 2D/3D using TrackMate
	adapted from
	https://forum.image.sc/t/filter-spots-in-trackmate-via-scripting/30744

	Arguments:
	    imp {imagePlus} 			-- Image for detection
	    cellRadius {int}       		-- Radius of the cell to detect
	    detectionThresh {int}   	-- Intensity threshold for the detection
	    doSubpixelDetection {bool}  -- Option for subpixel detection
	    doMedianFiltering {bool}    -- Option for median filter before detection
	    doAutofiltering {bool}	    -- Do autofiltering based on quality on spots
		autoFilteringValue {string} -- Value to use for autofiltering
		detectionThreshAdj {double} -- Value to adjust autofiltering value

	Returns:
	    markersImp {imagePlus} 		-- Image with markersImp
		spotsTresh {double}			-- Calculated threshold
	"""
	# adapted from https://imagej.net/Scripting_TrackMate
	model = Model()

	# send all messages to ImageJ log window.
	model.setLogger(Logger.IJ_LOGGER)

	# settings
	settings = Settings(imp)

	# configure detector
	settings.detectorFactory = LogDetectorFactory()
	# Downsample detector is not so good because then the detection
	# is a bit "blocky"
	# settings.detectorFactory = DownsampleLogDetectorFactory()
	settings.detectorSettings = {
	    'DO_SUBPIXEL_LOCALIZATION' : doSubpixelDetection,
	    'RADIUS' : cellRadius,
	    'TARGET_CHANNEL' : targetChannel,
	    'THRESHOLD' : detectionThresh,
	    'DO_MEDIAN_FILTERING' : doMedianFiltering
	    # 'DOWNSAMPLE_FACTOR': downsampling
	}

	# prepare Trackmate
	settings.trackerFactory = SparseLAPTrackerFactory()
	settings.trackerSettings = LAPUtils.getDefaultLAPSettingsMap()

	# init TM
	trackmate = TrackMate(model, settings)

	# get autofiltering value
	if (doAutofiltering == True):
		trackmate.execDetection()
		trackmate.execInitialSpotFiltering()
		trackmate.computeSpotFeatures(False)

		# Apply auto threshold
		detectedSpots = model.getSpots()
		# https://github.com/fiji/TrackMate/commit/6413d8d85df83acaaf8de96c2b2a08858f698808
		#spotsValues = detectedSpots.collectValues(autoFilteringValue, False)
		spotsValues = [x.getFeature(autoFilteringValue) for x in detectedSpots.iterable(False)]

		spotsTresh = 0
		if len(spotsValues) > 0:
			spotsTresh = TMUtils.otsuThreshold(spotsValues)

			# adjust detection threshold
			spotsTresh = spotsTresh * detectionThreshAdj

		# Add the filter on quality
		filter1 = FeatureFilter(autoFilteringValue, spotsTresh, True)
		settings.addSpotFilter(filter1)

	spots = None

	ok = trackmate.checkInput()
	if not ok:
	    #sys.exit(str(trackmate.getErrorMessage()))
	    print(str(trackmate.getErrorMessage()))
	else:
		ok = trackmate.process()
		if not ok:
		    #sys.exit(str(trackmate.getErrorMessage()))
		    print(str(trackmate.getErrorMessage()))
		else:
			spots = model.getSpots()

	return spots, spotsTresh

def writeSpotsToImage(imp, detectedSpots):
	"""
	Write detected spots to image

	Arguments:
		imp {imagePlus} 				-- Image used for detection
	    detectedSpots {SpotCollection} 	-- Detected spots from TM

	Returns:
	    markersImp {imagePlus} 		-- Image with markersImp
	"""
	# The TrackMate way produces an 8bit
	# cal = imp.getCalibration()
	# # prepare calibration
	# newCal = (
	# 		cal.pixelWidth,
	# 		cal.pixelHeight,
	# 		cal.pixelDepth,
	# 		cal.frameInterval
	# )
	#
	# # Add spots to image
	# exportSpotsAsDots = True
	# exportTracksOnly = False
	# markerImp = LabelImgExporter.createLabelImagePlus(
	# 	model, imp.getDimensions(), newCal,
	# 	exportSpotsAsDots, exportTracksOnly)

	# prepare image to save spots
	dim = imp.getDimensions()
	cal = imp.getCalibration()
	markerStack = ImageStack.create(dim[0], dim[1], dim[3], 8)

	# go over visible spots and add to image
	if detectedSpots.getNSpots(True) > 0:
		for i, curSpot in enumerate(detectedSpots.iterable(True)):
			markerStack.setVoxel(
				int(curSpot.getFeature(Spot.POSITION_X)/cal.pixelWidth),
				int(curSpot.getFeature(Spot.POSITION_Y)/cal.pixelHeight),
				int(curSpot.getFeature(Spot.POSITION_Z)/cal.pixelDepth),
				2**8-1)
	markerImp = ImagePlus("", markerStack)

	return markerImp

def writeSpotsToCSV(imp, detectedSpots, filename):
	"""
	Write detected spots to csv

	Arguments:
		imp {imagePlus} 				-- Image used for detection
	    detectedSpots {SpotCollection} 	-- Detected spots from TM
	    filename {string} 				-- Path to CSV file

	Returns:

	"""
	dim = imp.getDimensions()
	cal = imp.getCalibration()

	# go over visible spots and add to file
	if detectedSpots.getNSpots(True) > 0:
		# open file
		with open(filename, 'w') as csvfile:
			csvwriter = csv.writer(csvfile)

			# write header
			csvwriter.writerow(["SPOT_ID", "POS_X", "POS_Y", "POS_Z"])

			# go through spots
			for i, curSpot in enumerate(detectedSpots.iterable(True)):
				row = [
					curSpot.ID(),
					int(curSpot.getFeature(Spot.POSITION_X)/cal.pixelWidth),
					int(curSpot.getFeature(Spot.POSITION_Y)/cal.pixelHeight),
					int(curSpot.getFeature(Spot.POSITION_Z)/cal.pixelDepth)
				]

				csvwriter.writerow(row)

	return True

def trackSpots(imp, rad, thresh = 0., doMedian = False, downsampling = 4,
				autofilt = True, filterValue = Spot.QUALITY, linkingDist = 20.,
				threshAdj = 1, subPixel = False, maxFrameGap = 2, chnlToTrack = 1,
				trackingType = "linear"):
	"""
	Track cells with TM

	Arguments:
	    implus {imagePlus} 		-- ImagePlus of the image to use for detection
	    rad {double}  			-- radius for spots
	    targetC {int}			-- target channel to track
	    thresh {int}			-- Pixel threshold
	    doMedian {bool}			-- do median before spot detection
	    downsampling {int}		-- downsampling factor
	    autofilt {bool}			-- do autofiltering on spots
	    filterValue {string} 	-- Value to filter spots on
	    linkingDist {double}	-- distance to link spots
	    threshAdj {double}		-- Adjust auto threshold
	    subPixel {bool}			-- subpixel localisation
	    maxFrameGap {int}		-- maximum frame gap

	Returns:
	   	 {Model} -- TM model
	"""
	model = Model()

	# Send all messages to ImageJ log window.
	model.setLogger(Logger.IJ_LOGGER)

	# settings
	settings = Settings(imp)

	# Configure detector
	#settings.detectorFactory = DownsampleLogDetectorFactory()
	settings.detectorFactory = LogDetectorFactory()
	settings.detectorSettings = {
	    'DO_SUBPIXEL_LOCALIZATION': subPixel,
	    'RADIUS': rad,
	    'TARGET_CHANNEL': chnlToTrack,
	    'THRESHOLD': thresh,
	    'DO_MEDIAN_FILTERING': doMedian,
		#'DOWNSAMPLE_FACTOR': downsampling
	}

	# Configure tracker
	if trackingType == "brownian":
		settings.trackerFactory = SparseLAPTrackerFactory()
		settings.trackerSettings = LAPUtils.getDefaultLAPSettingsMap()
		settings.trackerSettings['ALLOW_TRACK_SPLITTING'] = False
		settings.trackerSettings['ALLOW_TRACK_MERGING'] = False
		settings.trackerSettings['LINKING_MAX_DISTANCE'] = linkingDist
		settings.trackerSettings['ALLOW_GAP_CLOSING'] = True
		settings.trackerSettings['GAP_CLOSING_MAX_DISTANCE'] = linkingDist
		settings.trackerSettings['MAX_FRAME_GAP']  = maxFrameGap
	elif trackingType == "linear":
		settings.trackerFactory = KalmanTrackerFactory()
		settings.trackerSettings['KALMAN_SEARCH_RADIUS'] = linkingDist
		settings.trackerSettings['LINKING_MAX_DISTANCE'] = linkingDist
		settings.trackerSettings['MAX_FRAME_GAP']  = maxFrameGap

	# Filter tracks
	settings.addTrackAnalyzer(TrackDurationAnalyzer())
	# 4 frames ~ 3 min
	settings.addTrackFilter(FeatureFilter('TRACK_DURATION', 4, True))
	# Should move at least four cell widths
	# settings.addTrackFilter(FeatureFilter('TRACK_DISPLACEMENT', 20, True))

	# init TM
	trackmate = TrackMate(model, settings)

	if (autofilt == True):
		trackmate.execDetection()
		trackmate.computeSpotFeatures(False)

		# Apply auto threshold
		spotsDetected = model.getSpots()
		spotsValues = [x.getFeature(autoFilteringValue) for x in detectedSpots.iterable(False)]
		spotsThreshold = TMUtils.otsuThreshold(spotsValues)
		spotsThreshold = spotsThreshold * threshAdj

		# Add the filter on quality
		settings.initialSpotFilterValue = spotsThreshold

		# execute filtering
		trackmate.execInitialSpotFiltering()

	###
	# Process
	###

	ok = trackmate.checkInput()
	if not ok:
	    sys.exit(str(trackmate.getErrorMessage()))

	ok = trackmate.process()
	if not ok:
	    sys.exit(str(trackmate.getErrorMessage()))

	# model.getLogger().log(str("--- Model ---"))
	# model.getLogger().log(str(model))

	return model, spotsThreshold
