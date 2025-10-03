# -*- coding: utf-8 -*-

from ij import IJ
import os
import glob

# load from Bioformats as virtual stack
from loci.plugins import BF, LociExporter
from loci.plugins.in import ImagePlusReader, ImporterOptions, ImportProcess
from loci.plugins.out import Exporter

class BFUtils:
	@staticmethod
	def exportImage(imPathIn, imPathOut,
		wildcard = False, compression = "zlib"):
		"""
		Export image with bioformats
		adapted from
		https://forum.image.sc/t/bio-formats-ome-tiff-write-time-issue-bug/18847/17
	
		Arguments:
		    imPathIn {String} 			-- Path to image
		   	imPathOut {String} 			-- Path to output
		   	wildcard {Boolean} 			-- Use wildcard search to find input file
		   	compression {String} 		-- Compression method
	
		Returns:
		"""
		# find input file
		if wildcard is True:
			imPathIn = glob.glob(imPathIn)[0]
		
		# load image
		imp = BFUtils.importImage(imPathIn)

		# remove if already exists
		# otherwise it will added .. ? Not sure how that works
		if os.path.exists(imPathOut) is True:
			os.remove(imPathOut) 
		
		# save as ome-tiff
		paramstring = "outfile=[" + imPathOut + "] windowless=true compression=" + compression + " saveROI=false"
		plugin = LociExporter()
		plugin.arg = paramstring
		exporter = Exporter(plugin, imp)
		exporter.run()

	@staticmethod
	def importImage(imPathIn):
		"""
		Import image with bioformats
	
		Arguments:
		    imPathIn {String} 			-- Path to image
	
		Returns:
			{ImagePlus} 				-- Imported image
		"""
		# load image
		# https://javadoc.scijava.org/Bio-Formats/loci/plugins/in/ImporterOptions.html
		opts = ImporterOptions()
		opts.setId(imPathIn)
		opts.setVirtual(True) 
		opts.setUpgradeCheck(False) 
		
		process = ImportProcess(opts)
		process.execute()
	
		impReader = ImagePlusReader(process)
		imps = impReader.openImagePlus()
		
		imp = imps[0]
	
		return(imp)
