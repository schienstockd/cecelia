# -*- coding: utf-8 -*-

# ImageJ imports
from ij import IJ
from ij.plugin import ImageCalculator

import sys
import os

from ccia.processors.imageUtils import combineChannels

def convertBlobsToDonuts(imp, gaussianFilter, medianFilter, maximumFilter, minimumFilter):
	"""
	Process blob like cells to resemble donuts (ie/ membrane like)

	Arguments:
	    imp {imagePlus} 			-- Image to process
	    gaussianFilter {int}       	-- Gaussian filter value
	    medianFilter {int}   	    -- Median filter value
	    maximumFilter {int}       	-- Maximum filter value
	    minimumFilter {int}   	    -- Minimum filter value

	Returns:
	    DonutImp {imagePlus} 		-- Image with donut like signal
	"""
	# make blobs a bit bigger
	IJ.run(imp, "Gaussian Blur...", "sigma=%d stack" % (gaussianFilter))

	# this will make donuts bigger with increasing value
	IJ.run(imp, "Maximum...", "sigma=%d stack" % (maximumFilter))

	# make donuts
	IJ.run(imp, "Find Edges", "stack")

	# this will make donuts bigger with increasing value
	IJ.run(imp, "Minimum...", "radius=%d stack" % (minimumFilter))

	# smoothen
	IJ.run(imp, "Median...", "radius=%d stack" % (medianFilter))

	return imp

def enhanceDonuts(imp, gaussianFilter, medianFilter, minimumFilter):
	"""
	Process donuts like cells to enhance edges

	Arguments:
	    imp {imagePlus} 			-- Image to process
	    gaussianFilter {int}       	-- Gaussian filter value
	    medianFilter {int}   	    -- Median filter value
	    minimumFilter {int}   	    -- Minimum filter value

	Returns:
	    EnhancedImp {imagePlus} 	-- Image with enhanced donut like signal
	"""
    # enhance edges
	IJ.run(imp, "Gaussian Blur...", "sigma=%d stack" % (gaussianFilter))

	# this will make donuts bigger with increasing value
	IJ.run(imp, "Minimum...", "radius=%d stack" % (minimumFilter))

	# make donuts
	IJ.run(imp, "Find Edges", "stack")

	# smoothen
	IJ.run(imp, "Median...", "radius=%d stack" % (medianFilter))

	return imp

def createDonutBlobImage(imp, donutChannels, blobChannels,
		gaussianFilter, medianFilter, maximumFilter, minimumFilter):
	"""
	Create combined donut and blob image

	Arguments:
	    imp {imagePlus} 			-- Image to process
	    donutChannels {list}   	    -- Donut channels
	    blobChannels {list}   	    -- Blob channels
	   	gaussianFilter {int}       	-- Gaussian filter value
	    medianFilter {int}   	    -- Median filter value
		maximumFilter {int}       	-- Maximum filter value
	    minimumFilter {int}   	    -- Minimum filter value

	Returns:
	    donbloImp {imagePlus} 	    -- Combined donuts/blobs image
	"""
	# Blobs
	blobsImp = None

	if blobChannels is not None and len(blobChannels) > 0:
	    blobsImp = combineChannels(imp, blobChannels)

	    # process blobs to donuts
	    blobsImp = convertBlobsToDonuts(blobsImp, gaussianFilter, medianFilter, maximumFilter, minimumFilter)

	# Donuts
	donutsImp = None

	if donutChannels is not None and len(donutChannels) > 0:
	    donutsImp = combineChannels(imp, donutChannels)

	    # enhance donuts
	    donutsImp = enhanceDonuts(donutsImp, gaussianFilter, medianFilter, minimumFilter)

	# combine images
	if blobsImp is not None and donutsImp is not None:
	    donbloImp = ImageCalculator().run(
	        "Max create stack", blobsImp, donutsImp)
	elif blobsImp is not None:
	    donbloImp = blobsImp
	else:
	    donbloImp = donutsImp

	return donbloImp
