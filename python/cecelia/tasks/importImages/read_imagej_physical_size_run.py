"""
Independently re-derive PhysicalSizeZ from a source TIFF's ImageJ metadata.

bioformats2raw correctly converts X/Y pixel size to micrometers (via the standard TIFF
XResolution/YResolution + ResolutionUnit tags) but does not apply the same conversion to Z (from
ImageJ's own `spacing` tag) when the file's calibration unit isn't already micron — it writes the
raw, unconverted `spacing` value straight into PhysicalSizeZ. ImageJ's ImageDescription tag carries
`unit` + `spacing` directly, so we can recompute the correct micrometer value ourselves without
touching bioformats2raw. Called by the Julia ImportOmezarr task handler, only for TIFF sources.

Parameter contract (JSON written by Julia):
  imPath     - absolute path to the ORIGINAL source file (pre-conversion)
  resultPath - absolute path to write the result JSON to

Result JSON: {"PhysicalSizeZ": <float, µm>, "sourceUnit": <str>} or {} if there was nothing
usable to correct (not a TIFF, no ImageJ metadata, no `spacing`, or unit already micron).
"""

import json

import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils

# ImageJ calibration unit → micrometers
_UNIT_TO_MICRON = {
    'micron': 1.0, 'microns': 1.0, 'um': 1.0, 'µm': 1.0, 'micrometer': 1.0,
    'nm': 1e-3, 'nanometer': 1e-3,
    'mm': 1e3, 'millimeter': 1e3,
    'cm': 1e4, 'centimeter': 1e4,
    'inch': 25400.0, 'inches': 25400.0,
}


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    result_path = params['resultPath']
    result = {}

    try:
        meta = ome_xml_utils.read_imagej_metadata(im_path)
    except Exception as e:
        meta = None
        log.log(f'>> not a readable TIFF, skipping ImageJ Z-spacing check: {e}')

    if meta:
        unit    = str(meta.get('unit', '')).lower()
        spacing = meta.get('spacing')
        factor  = _UNIT_TO_MICRON.get(unit)

        if spacing is not None and factor is not None and unit not in ('micron', 'microns', 'um', 'µm', 'micrometer'):
            result = {'PhysicalSizeZ': float(spacing) * factor, 'sourceUnit': unit}
            log.log(f'>> ImageJ spacing={spacing} unit={unit} -> PhysicalSizeZ={result["PhysicalSizeZ"]} um')
        elif spacing is not None and factor is None:
            log.log(f'>> ImageJ unit "{unit}" has no known micron conversion, skipping')

    with open(result_path, 'w') as f:
        json.dump(result, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
