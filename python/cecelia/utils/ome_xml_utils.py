"""
OME-XML metadata helpers for OME-TIFF and OME-ZARR images.

Wraps ome-types to parse, modify, and write OME-XML embedded in ZARR stores
or TIFF files.  The canonical on-disk location for OME-ZARR is:
  {store}/OME/METADATA.ome.xml
"""

import os
from functools import lru_cache

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
Read ImageJ's own metadata (the ImageDescription tag) from a tiff
"""
def read_imagej_metadata(im_path):
  """ImageJ's own metadata dict (the ImageDescription tag: `unit`, `spacing`, …) from a TIFF, or
  None if the file carries no ImageJ metadata. DISTINCT from OME-XML (`parse_meta`) — ImageJ
  calibration lives in its own tag. The single place a TIFF's ImageJ tags are read; raises if
  `im_path` isn't a readable TIFF (a caller wanting a soft skip wraps it in try/except)."""
  with tifffile.TiffFile(im_path) as tif:
    return tif.imagej_metadata

"""
Parse metadata from zarr
"""
def parse_meta_from_zarr(zarr_path):
  # the canonical OME-XML location for an OME-ZARR store is {store}/OME/METADATA.ome.xml (older
  # stores kept it at the root); load_ome_xml checks both. Raise if neither exists so callers that
  # rely on metadata fail loudly rather than on a None dereference downstream.
  omexml = load_ome_xml(zarr_path)
  if omexml is None:
    raise FileNotFoundError(f"no OME-XML metadata under {zarr_path}")
  return omexml


@lru_cache(maxsize=64)
def _parse_ome_xml_cached(xml_path, _mtime):
  """Parse one OME-XML file, cached on (path, mtime) so a long-lived reader (e.g. the napari
  bridge) parses each store's metadata at most once; a rewrite (new mtime) invalidates the entry.
  ome-types is already imported at module top, so the pydantic model-build cost is paid on the
  first parse regardless."""
  with open(xml_path) as f:
    return from_xml(f.read())


def load_ome_xml(path):
  """Locate + parse a store's OME-XML (OME/METADATA.ome.xml, else the legacy root METADATA.ome.xml),
  or None if absent/unparseable. The ONE OME-XML reader — the napari bridge and the pipeline both
  go through here instead of re-opening the file themselves."""
  for candidate in (
      os.path.join(path, "OME", "METADATA.ome.xml"),
      os.path.join(path, "METADATA.ome.xml"),
  ):
    if os.path.isfile(candidate):
      try:
        return _parse_ome_xml_cached(candidate, os.path.getmtime(candidate))
      except Exception:
        return None
  return None


def read_pixel_unit(path, default="µm"):
  """Physical pixel unit from a store's OME-XML (physical_size_x_unit), e.g. 'µm'/'nm'/'mm'. The
  RAW unit string is preserved (NOT normalised to 'um') so napari's unit-aware rendering matches
  the image. Returns `default` when the metadata is absent."""
  omexml = load_ome_xml(path)
  if omexml is not None:
    try:
      unit = omexml.images[0].pixels.physical_size_x_unit
      if unit is not None:
        return unit.value
    except Exception:
      pass
  return default


def read_scale_from_ome_xml(path, axes):
  """Per-axis physical pixel scale from OME-XML, in the given `axes` order (an axis with no size →
  1.0), or None if there's no metadata. The fallback for stores whose NGFF metadata carries no
  `coordinateTransformations` (see zarr_utils.read_scale)."""
  omexml = load_ome_xml(path)
  if omexml is None:
    return None
  try:
    pixels = omexml.images[0].pixels
    sizes = {"x": pixels.physical_size_x, "y": pixels.physical_size_y, "z": pixels.physical_size_z}
    return [float(sizes.get(ax.lower()) or 1.0) for ax in axes]
  except Exception:
    return None


def read_time_increment(path):
  """Seconds between timepoints from a store's OME-XML `pixels.time_increment` (converting a
  ms/min/h unit to seconds), or None. Used for the timecourse timestamp overlay."""
  omexml = load_ome_xml(path)
  if omexml is None:
    return None
  try:
    pixels = omexml.images[0].pixels
    inc = pixels.time_increment
    if inc is None:
      return None
    secs = float(inc)
    unit = getattr(getattr(pixels, "time_increment_unit", None), "value", None)
    return {"ms": secs / 1000.0, "min": secs * 60.0, "h": secs * 3600.0}.get(unit, secs)
  except Exception:
    return None

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
