import os
import tifffile
from ome_types import from_xml, from_tiff, to_xml

"""
save (changed) OME XML from previous image into zarr structure
"""
def save_meta_in_zarr(
  store_path,
  original_im_path = None, omexml = None,
  changed_shape = None, dim_utils = None,
  new_shape = None):
  # get previous metadata
  if omexml is None:
    omexml = parse_meta(original_im_path)
  
  # add new dimensions
  # TZCYX
  if changed_shape is not None and dim_utils is not None:
    if dim_utils.dim_idx('T') is not None:
      omexml.images[0].pixels.size_t = changed_shape[dim_utils.dim_idx('T')]
    if dim_utils.dim_idx('Z') is not None:
      omexml.images[0].pixels.size_z = changed_shape[dim_utils.dim_idx('Z')]
    if dim_utils.dim_idx('C') is not None:
      omexml.images[0].pixels.size_c = changed_shape[dim_utils.dim_idx('C')]
    if dim_utils.dim_idx('Y') is not None:
      omexml.images[0].pixels.size_y = changed_shape[dim_utils.dim_idx('Y')]
    if dim_utils.dim_idx('X') is not None:
      omexml.images[0].pixels.size_x = changed_shape[dim_utils.dim_idx('X')]
  elif new_shape is not None:
    # reset shape
    omexml.images[0].pixels.size_t = 1
    omexml.images[0].pixels.size_z = 1
    omexml.images[0].pixels.size_c = 1
    omexml.images[0].pixels.size_y = 1
    omexml.images[0].pixels.size_x = 1
    
    for i, x in new_shape.items():
      if i == 'T':
        omexml.images[0].pixels.size_t = x
      if i == 'Z':
        omexml.images[0].pixels.size_z = x
      if i == 'C':
        omexml.images[0].pixels.size_c = x
      if i == 'Y':
        omexml.images[0].pixels.size_y = x
      if i == 'X':
        omexml.images[0].pixels.size_x = x
  
  # save OME XML in zarr directory
  # create directory
  ome_dir = os.path.join(store_path, 'OME')
  if os.path.exists(ome_dir) is False:
    os.mkdir(ome_dir)
  
  # write back
  write_ome_xml(store_path, omexml)
  
"""
Get im size dict
"""
def get_im_size_dict(omexml):
  im_dim_dict = dict()
  im_dim_order = list(omexml.images[0].pixels.dimension_order.value)
  
  # go through dimension order  
  for x in im_dim_order:
    if x == 'T':
      im_dim_dict[x] = omexml.images[0].pixels.size_t
    if x == 'Z':
      im_dim_dict[x] = omexml.images[0].pixels.size_z
    if x == 'C':
      im_dim_dict[x] = omexml.images[0].pixels.size_c
    if x == 'Y':
      im_dim_dict[x] = omexml.images[0].pixels.size_y
    if x == 'X':
      im_dim_dict[x] = omexml.images[0].pixels.size_x
      
  return im_dim_dict

"""
Set physical size with dict
"""
def set_physical_size_with_dict(omexml, omedict):
  # go through dimensions
  for i, x in omedict.items():
    if i == 'T':
      omexml.images[0].pixels.physical_size_t = x
    if i == 'Z':
      omexml.images[0].pixels.physical_size_z = x
    if i == 'C':
      omexml.images[0].pixels.physical_size_c = x
    if i == 'Y':
      omexml.images[0].pixels.physical_size_y = x
    if i == 'X':
      omexml.images[0].pixels.physical_size_x = x
      
  return omexml

"""
Set im size with dict
"""
def set_im_size_with_dict(omexml, omedict):
  # go through dimensions
  for i, x in omedict.items():
    if i == 'T':
      omexml.images[0].pixels.size_t = x
    if i == 'Z':
      omexml.images[0].pixels.size_z = x
    if i == 'C':
      omexml.images[0].pixels.size_c = x
    if i == 'Y':
      omexml.images[0].pixels.size_y = x
    if i == 'X':
      omexml.images[0].pixels.size_x = x
      
  return omexml

"""
Write OME-XML

TODO this only works on OME-ZARR for now
"""
def write_ome_xml(im_path, omexml):
  # flip dimension order in pixels and adjust sizes
  if os.path.splitext(im_path)[1] == ".zarr":
    ome_path = os.path.join(im_path, 'OME')
    
    if os.path.exists(ome_path) is False:
      os.makedirs(ome_path)
    
    # write to file
    ome_xml_file = open(os.path.join(ome_path, 'METADATA.ome.xml'), 'w')
    
    if isinstance(omexml, str):
      ome_xml_file.write(omexml)
    else:
      ome_xml_file.write(omexml.to_xml())
    
    # TODO use when ome_type 0.21 is released  
    # ome_xml_file.write(to_xml(omexml))
    
    ome_xml_file.close()

"""
Change pixel type

TODO this only works on OME-ZARR for now
"""
def change_pixel_type(im_path, pixel_type = 'uint16'):
  # flip dimension order in pixels and adjust sizes
  if os.path.splitext(im_path)[1] == ".zarr":
    omexml = parse_meta(im_path)
    
    omexml.images[0].pixels.type = pixel_type
    
    # write back
    write_ome_xml(im_path, omexml)

"""
Flip dimension order

TODO this only works on OME-ZARR for now
"""
def switch_dim_order(im_path, dim_order = 'XYZCT'):
  # flip dimension order in pixels and adjust sizes
  if os.path.splitext(im_path)[1] == '.zarr':
    omexml = parse_meta(im_path)
    
    # get previous order
    prev_dim_order = omexml.images[0].pixels.dimension_order
    
    # get dimension dict
    dim_dict = get_im_size_dict(omexml)
    
    # get dimension mapping
    dim_mapping = {dim_order[i]: x for i, x in enumerate(prev_dim_order)}
    
    # set new sizes
    # go through dimension order  
    for i, x in {dim_mapping[i]: x for i, x in dim_dict.items()}.items():
      if i == 'T':
        omexml.images[0].pixels.size_t = x
      if i == 'Z':
        omexml.images[0].pixels.size_z = x
      if i == 'C':
        omexml.images[0].pixels.size_c = x
      if i == 'Y':
        omexml.images[0].pixels.size_y = x
      if i == 'X':
        omexml.images[0].pixels.size_x = x
        
    # set new dimension order
    omexml.images[0].pixels.dimension_order = dim_order
    
    # write back
    write_ome_xml(im_path, omexml)

"""
Parse raw ome XML from tiff
"""
def parse_from_tiff(im_path):
  # TODO is there a better way to do this .. ?
  tif = tifffile.TiffFile(im_path)

  return tif.ome_metadata

"""
Parse metadata from tiff
"""
def parse_meta_from_tiff(im_path):
  return from_tiff(im_path)

"""
Parse metadata from zarr
"""
def parse_meta_from_zarr(zarr_path):
  # open XML file
  # (I) at the moment, the XML is in the Zarr directory
  # https://github.com/zarr-developers/zarr-specs/issues/112
  # https://github.com/ome/ngff/issues/27
  xml_path = os.path.join(zarr_path, "METADATA.ome.xml")
  
  if os.path.isfile(xml_path) is False:
    xml_path = os.path.join(zarr_path, "OME", "METADATA.ome.xml")
  
  # https://codecap.org/typeerror-a-bytes-like-object-is-required-not-str/
  with open(xml_path, "rb") as f:
    xml_lines = f.readlines()
    
  # decode bytes
  xml_lines = [x.decode() for x in xml_lines]
  
  return from_xml(xml_path)

"""
Parse metadata from zarr file
"""
def parse_meta(im_path):
  omexml = None
  
  # get extension
  im_ext = os.path.splitext(im_path)[1]
  
  # open zarr
  if im_ext == ".zarr":
    omexml = parse_meta_from_zarr(im_path)
  else:
    omexml = parse_meta_from_tiff(im_path)
    
  return omexml
