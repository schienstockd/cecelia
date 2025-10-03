###
# #%L
# Script to register time frames (stacks) to each other.
# %%
# Copyright (C) 2010 - 2016 Fiji development team
# %%
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/gpl-3.0.html>.
# #L%
###

# Robert Bryson-Richardson and Albert Cardona 2010-10-08 at Estoril, Portugal
# EMBO Developmental Imaging course by Gabriel Martins
#
# Register time frames (stacks) to each other using Stitching_3D library
# to compute translations only, in all 3 spatial axes.

###
# Dominik Schienstock (schienstockd@student.unimelb.edu.au)
# 20191212
# Adjusted for headless execution
###

#@ ImagePlus imp

from ij import VirtualStack, IJ, CompositeImage, ImageStack, ImagePlus
from ij.process import ColorProcessor
from ij.plugin import HyperStackConverter
from ij.io import DirectoryChooser, FileSaver, SaveDialog
from ij.gui import GenericDialog, YesNoCancelDialog, Roi
from mpicbg.imglib.image import ImagePlusAdapter
from mpicbg.imglib.algorithm.fft import PhaseCorrelation
from org.scijava.vecmath import Point3i  #from javax.vecmath import Point3i # java6
from org.scijava.vecmath import Point3f  #from javax.vecmath import Point3f # java6
from java.io import File, FilenameFilter
from java.lang import Integer
import math, os, os.path

# sub-pixel translation using imglib2
from net.imagej.axis import Axes
from net.imglib2.img.display.imagej import ImageJFunctions
from net.imglib2.realtransform import RealViews, Translation3D, Translation2D
from net.imglib2.view import Views
from net.imglib2.img.imageplus import ImagePlusImgs
from net.imglib2.converter import Converters
from net.imglib2.converter.readwrite import RealFloatSamplerConverter
from net.imglib2.interpolation.randomaccess import NLinearInterpolatorFactory

def compute_stitch(imp1, imp2):
  """ Compute a Point3i that expressed the translation of imp2 relative to imp1."""
  phc = PhaseCorrelation(ImagePlusAdapter.wrap(imp1), ImagePlusAdapter.wrap(imp2), 5, True)
  phc.process()
  p = phc.getShift().getPosition()
  if len(p)==3: # 3D data
    p3 = p
  elif len(p)==2: # 2D data: add zero shift
    p3 = [p[0],p[1],0]
  return Point3i(p3)

def extract_frame(imp, frame, channel, z_min, z_max):
  """ From a VirtualStack that is a hyperstack, contained in imp,
  extract the timepoint frame as an ImageStack, and return it.
  It will do so only for the given channel. """
  stack = imp.getStack() # multi-time point virtual stack
  stack2 = ImageStack(imp.width, imp.height, None)
  
  for s in range(int(z_min), int(z_max)+1):
    i = imp.getStackIndex(channel, s, frame)  
    stack2.addSlice(str(s), stack.getProcessor(i))
    
  return stack2

def extract_frame_process(imp, frame, channel, background, z_min, z_max):
  # extract frame and channel 
  imp_frame = ImagePlus("", extract_frame(imp, frame, channel, z_min, z_max)).duplicate()

  # subtract background  
  if background > 0:
    #IJ.log("Subtracting "+str(background));
    IJ.run(imp_frame, "Subtract...", "value="+str(background)+" stack");

  # return
  return imp_frame

def add_Point3f(p1, p2):
  p3 = Point3f(0,0,0)
  p3.x = p1.x + p2.x
  p3.y = p1.y + p2.y
  p3.z = p1.z + p2.z
  return p3

def subtract_Point3f(p1, p2):
  p3 = Point3f(0,0,0)
  p3.x = p1.x - p2.x
  p3.y = p1.y - p2.y
  p3.z = p1.z - p2.z
  return p3
  
def compute_and_update_frame_translations_dt(imp, channel, dt, background, z_min, z_max):
  """ imp contains a hyper virtual stack, and we want to compute
  the X,Y,Z translation between every t and t+dt time points in it
  using the given preferred channel. 
  if shifts were already determined at other (lower) dt 
  they will be used and updated.
  """
  nt = imp.getNFrames()

  shifts = []
  for t in range(nt):
    shifts.append(Point3f(0,0,0))
  # compute shifts

  for t in range(dt, nt+dt, dt):
    if t > nt-1: # together with above range till nt+dt this ensures that the last data points are not missed out
      t = nt-1 # nt-1 is the last shift (0-based)
    IJ.log("      between frames "+str(t-dt+1)+" and "+str(t+1))      
    # get image at t-dt
    imp1 = extract_frame_process(imp, t+1-dt, channel, background, z_min, z_max)
    
    # get image at t
    imp2 = extract_frame_process(imp, t+1, channel, background, z_min, z_max)

    # compute shift
    local_new_shift = compute_stitch(imp2, imp1)
      
    # determine the shift that we knew alrady
    local_shift = subtract_Point3f(shifts[t],shifts[t-dt])
    
    # compute difference between new and old measurement (which come from different dt)   
    add_shift = subtract_Point3f(local_new_shift,local_shift)

    # update shifts from t-dt to the end (assuming that the measured local shift will presist till the end)
    for i,tt in enumerate(range(t-dt,nt)):
      # for i>dt below expression basically is a linear drift predicition for the frames at tt>t
      # this is only important for predicting the best shift of the ROI 
      # the drifts for i>dt will be corrected by the next measurements
      shifts[tt].x += 1.0*i/dt * add_shift.x
      shifts[tt].y += 1.0*i/dt * add_shift.y
      shifts[tt].z += 1.0*i/dt * add_shift.z

  return shifts

def convert_shifts_to_integer(shifts):
  int_shifts = []
  for shift in shifts: 
    int_shifts.append(Point3i(int(round(shift.x)),int(round(shift.y)),int(round(shift.z)))) 
  return int_shifts

def compute_min_max(shifts):
  """ Find out the top left up corner, and the right bottom down corner,
  namely the bounds of the new virtual stack to create.
  Expects absolute shifts. """
  minx = Integer.MAX_VALUE
  miny = Integer.MAX_VALUE
  minz = Integer.MAX_VALUE
  maxx = -Integer.MAX_VALUE
  maxy = -Integer.MAX_VALUE
  maxz = -Integer.MAX_VALUE
  for shift in shifts:
    minx = min(minx, shift.x)
    miny = min(miny, shift.y)
    minz = min(minz, shift.z)
    maxx = max(maxx, shift.x)
    maxy = max(maxy, shift.y)
    maxz = max(maxz, shift.z)  
  return minx, miny, minz, maxx, maxy, maxz

def zero_pad(num, digits):
  """ for 34, 4 --> '0034' """
  str_num = str(num)
  while (len(str_num) < digits):
    str_num = '0' + str_num
  return str_num

def invert_shifts(shifts):
  """ invert shifts such that they can be used for correction.
  """
  for shift in shifts:
    shift.x *= -1
    shift.y *= -1
    shift.z *= -1
  return shifts

def register_hyperstack(imp, channel, shifts):
  """ Takes the imp, determines the x,y,z drift for each pair of time points, using the preferred given channel,
  and outputs as a hyperstack."""
  # Compute bounds of the new volume,
  # which accounts for all translations:
  minx, miny, minz, maxx, maxy, maxz = compute_min_max(shifts)
  # Make shifts relative to new canvas dimensions
  # so that the min values become 0,0,0
  for shift in shifts:
    shift.x -= minx
    shift.y -= miny
    shift.z -= minz
  #print "shifts relative to new dimensions:"
  #for s in shifts:
  #  print s.x, s.y, s.z
  # new canvas dimensions:r
  width = imp.width + maxx - minx
  height = maxy - miny + imp.height
  slices = maxz - minz + imp.getNSlices()

  print "New dimensions:", width, height, slices
  # Prepare empty slice to pad in Z when necessary
  empty = imp.getProcessor().createProcessor(width, height)

  # if it's RGB, fill the empty slice with blackness
  if isinstance(empty, ColorProcessor):
    empty.setValue(0)
    empty.fill()
  # Write all slices to files:
  stack = imp.getStack()

  registeredstack = ImageStack(width, height, imp.getProcessor().getColorModel())
  names = []
  
  for frame in range(1, imp.getNFrames()+1):
    shift = shifts[frame-1]
    
    #print "frame",frame,"correcting drift",-shift.x-minx,-shift.y-miny,-shift.z-minz
    IJ.log("    frame "+str(frame)+" correcting drift "+str(-shift.x-minx)+","+str(-shift.y-miny)+","+str(-shift.z-minz))
    
    fr = "t" + zero_pad(frame, len(str(imp.getNFrames())))
    # Pad with empty slices before reaching the first slice
    for s in range(shift.z):
      ss = "_z" + zero_pad(s + 1, len(str(slices))) # slices start at 1
      for ch in range(1, imp.getNChannels()+1):
        name = fr + ss + "_c" + zero_pad(ch, len(str(imp.getNChannels()))) +".tif"
        names.append(name)
        
        empty = imp.getProcessor().createProcessor(width, height)
        registeredstack.addSlice(str(name), empty)
    
    # Add all proper slices
    stack = imp.getStack()
    for s in range(1, imp.getNSlices()+1):
      ss = "_z" + zero_pad(s + shift.z, len(str(slices)))
      for ch in range(1, imp.getNChannels()+1):
         ip = stack.getProcessor(imp.getStackIndex(ch, s, frame))
         ip2 = ip.createProcessor(width, height) # potentially larger
         ip2.insert(ip, shift.x, shift.y)
         name = fr + ss + "_c" + zero_pad(ch, len(str(imp.getNChannels()))) +".tif"
         names.append(name)

         registeredstack.addSlice(str(name), ip2)

    # Pad the end
    for s in range(shift.z + imp.getNSlices(), slices):
      ss = "_z" + zero_pad(s + 1, len(str(slices)))
      for ch in range(1, imp.getNChannels()+1):
        name = fr + ss + "_c" + zero_pad(ch, len(str(imp.getNChannels()))) +".tif"
        names.append(name)

        registeredstack.addSlice(str(name), empty)
  
  registeredstack_imp = ImagePlus("registered time points", registeredstack)
  registeredstack_imp.setCalibration(imp.getCalibration().copy())
  registeredstack_imp.setProperty("Info", imp.getProperty("Info"))
  registeredstack_imp = HyperStackConverter.toHyperStack(registeredstack_imp, imp.getNChannels(), len(names) / (imp.getNChannels() * imp.getNFrames()), imp.getNFrames(), "xyczt", "Composite");    
  
  return registeredstack_imp

def run(imp, opts):
	""" Main processing; Call with imageplus attribute """
	IJ.log("Correct_3D_Drift")
	
	channel = opts['channel']
	background = 0
	z_min = 1
	z_max = imp.getNSlices()
	process = False
	
	# compute shifts
	IJ.log("computing drifts...");
	IJ.log(" at frame shifts of 1"); 
	dt = 1; shifts = compute_and_update_frame_translations_dt(imp, channel, dt, background, z_min, z_max)
	
	# invert measured shifts to make them the correction
	shifts = invert_shifts(shifts)
	
	# apply shifts
	IJ.log("  applying shifts...");
	shifts = convert_shifts_to_integer(shifts)
	registered_imp = register_hyperstack(imp, channel, shifts)
	  
	if imp.getNChannels() > 1:
		registered_imp.copyLuts(imp)

	# save registered image back
	#IJ.saveAs(registered_imp, "Tiff", os.path.join(result_path, result_name + "_reg"))

	return registered_imp