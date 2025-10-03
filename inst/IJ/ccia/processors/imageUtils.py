# -*- coding: utf-8 -*-

# ImageJ imports
from ij.plugin import ChannelSplitter, ImageCalculator

def combineChannels(imp, channelsToCombine):
	"""
	Combine channels

	Arguments:
	    imp {imagePlus} 			-- Image to process
	    channelsToCombine {list}    -- Channel list to combine

	Returns:
	    combinedImp {imagePlus} 	-- Combined image
	"""
	# split image
	channelsImp = ChannelSplitter.split(imp)

	combinedImp = channelsImp[channelsToCombine[0]]

	# max all
	if len(channelsToCombine) > 1:
		for i in channelsToCombine[1:]:
			combinedImp = ImageCalculator().run(
				"Max create stack", combinedImp, channelsImp[i])

	return combinedImp
