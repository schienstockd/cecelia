"""Read the acquisition settings an Akoya `.qptiff` records, in the shape `lif_meta` gives a widefield `.lif`.

A `.qptiff` (PerkinElmer/Akoya QPI TIFF) is a pyramidal TIFF: one page per channel at full resolution,
then a thumbnail, the reduced-resolution levels, an overview and a label image. Every page carries
its own `PerkinElmer-QPI-ImageDescription` XML document in the ImageDescription tag, so the settings
are read per channel page, never from pixels (`ome_xml_utils.tiff_page_descriptions`). Field meanings
follow Bio-Formats' `VectraReader` (written with PerkinElmer): `ExposureTime` in microseconds,
`Biomarker` (when present) is the channel's marker and `Name` its fluor,
https://github.com/ome/bioformats/blob/develop/components/formats-gpl/src/loci/formats/in/VectraReader.java.

Checked on Fusion 2.3.0-2.4.1 files (Sony IMX421 camera): a single fluorescence scan (3 channels, LED),
a PhenoCycler run (DAPI + 6 markers in 2 cycles, `Biomarker` and `AutofluorescenceSubtracted` per page)
and a brightfield RGB scan (`ScanProfile` mode `im_Brightfield`, white LED).

Series 0 is the full-resolution channel pages (`ImageType` FullResolution), as Bio-Formats numbers it;
the thumbnail, overview and label are not the image, so other series give a reason.

The run's experiment file (`.xpd`, JSON) sits beside the `.qptiff` in a PhenoCycler scan folder: its
`wells` are the cycles in run order, each listing marker, barcode, reporter, clone, dye and planned
exposure; a well whose markers are all "--" is a blank (background) cycle. When exactly one `.xpd` is
there it is read; a marker's n-th page (in write-time order) is matched to its n-th item by name.

Readings that are ours, not Akoya's, stated where made:
- `system`: Fusion software (`AcquisitionSoftware` "Fusion …") runs the PhenoImager Fusion, the imager
  of a PhenoCycler-Fusion; the file names no instrument model (`InstrumentType` is an internal code).
- the emission band of a channel: the multiband emission filter's band at the same position as the
  excitation filter's active band (both are listed in wavelength order).
- `tiled`: the image is wider or taller than one camera field (`CameraSettings/ROI`), so it was
  stitched from fields.
- `acquiredAt`: the earliest channel page's TIFF DateTime, the scanner PC's local time; no zone.
- a channel's `cycle`: the file has no cycle field. A PhenoCycler run writes each cycle's channels at
  one time (DAPI with the first cycle). With the `.xpd`, a marker's pages in write-time order are its
  wells in run order and `cycle` is the well's run position, blanks counted; without it, the rank of the
  page's write time (imaged cycles only). `cyclic` is stamped when the channels carry markers and were
  written at more than one time.

Fixtures: `test-data/acquisition/qptiff_*`, from real files.
"""
import glob
import json
import os
import xml.etree.ElementTree as ET  # vendor QPI XML, not OME-XML: test_zarr_access_convention _BASELINE

from cecelia.utils import ome_xml_utils

#: Bump when the settings dict gains a field (its row in acquisition_readers.json must match).
READER_VERSION = 1

_US_PER_MS = 1000   # QPI `ExposureTime` is in µs (VectraReader: UNITS.MICROSECOND)
_FULL = 'FullResolution'
_BLANK_MARKER = '--'   # .xpd: a channel with no marker in a well


def _num(s):
    try:
        return float(s) if s is not None and s.strip() != '' else None
    except ValueError:
        return None


def _int(s):
    x = _num(s)
    return None if x is None else int(x)


def _text(el, path):
    t = el.findtext(path) if el is not None else None
    return t.strip() if t and t.strip() else None


def _band(lo, hi):
    lo, hi = _num(lo), _num(hi)
    return None if lo is None or hi is None else f'{round(lo)}-{round(hi)} nm'


def _filters(doc):
    """(excitation band, emission band) of a channel: the excitation filter's active band, and the
    emission filter's band at the same position (our reading: both list bands in wavelength order)."""
    ex_bands = list(doc.iterfind('ExcitationFilter/Bands/Band'))
    em_bands = list(doc.iterfind('EmissionFilter/Bands/Band'))
    active = [i for i, b in enumerate(ex_bands) if (_text(b, 'Active') or '').lower() == 'true']
    if len(active) != 1:
        return None, None
    i = active[0]
    ex = _band(_text(ex_bands[i], 'Cuton'), _text(ex_bands[i], 'Cutoff'))
    em = (_band(_text(em_bands[i], 'Cuton'), _text(em_bands[i], 'Cutoff'))
          if len(em_bands) == len(ex_bands) else None)
    return ex, em


def _bool(s):
    return None if s is None else s.strip().lower() == 'true'


def read_xpd(path):
    """The `.xpd` experiment dict, or None when unreadable."""
    try:
        with open(path, encoding='utf-8-sig') as f:
            return json.load(f)
    except (OSError, ValueError):
        return None


def _xpd_occurrences(xpd):
    """marker name → [(cycle, item), …] in run order: the cycle is the well's 1-based position in
    `wells` (blank wells count, they ran). A marker imaged in two cycles has two entries."""
    out = {}
    for k, w in enumerate((xpd or {}).get('wells', [])):
        for it in w.get('items', []):
            name = it.get('markerName')
            if name and name != _BLANK_MARKER:
                out.setdefault(name, []).append((k + 1, it))
    return out


def _cycles_and_items(full, times, cyclic, xpd):
    """Per channel page (cycle, `.xpd` item). With an `.xpd`: a marker's n-th page in write-time order
    is its n-th occurrence in the wells, and its cycle that well's run position (DAPI, listed in every
    well, is its first); a page the `.xpd` does not name takes the cycle of the pages written with it,
    else none — one scale per image. Without one: the rank of the page's write time — blank cycles
    write no page, so this counts imaged cycles only."""
    if not cyclic:
        return [(None, None)] * len(full)
    occ, seen, out = _xpd_occurrences(xpd), {}, []
    order = sorted(range(len(full)), key=lambda i: (full[i][1] or '', i))
    res = [None] * len(full)
    for i in order:
        doc, t, _ = full[i]
        marker = _text(doc, 'Biomarker')
        n = seen.get(marker, 0)
        seen[marker] = n + 1
        hits = occ.get(marker, [])
        if xpd and n < len(hits):
            res[i] = hits[n]
        elif not xpd:
            res[i] = (times.index(t) + 1 if t in times else None, None)
    if xpd:   # unnamed pages: the cycle their write time already has on the .xpd scale
        at = {full[i][1]: res[i][0] for i in range(len(full)) if res[i] is not None and full[i][1]}
        res = [r if r is not None else (at.get(full[i][1]), None) for i, r in enumerate(res)]
    return res


def _xpd_cycles(xpd):
    wells = (xpd or {}).get('wells', [])
    blank = sum(1 for w in wells if all(it.get('markerName') in (_BLANK_MARKER, 'DAPI') for it in w.get('items', [])))
    return {'planned': len(wells), 'blank': blank, 'experiment': xpd.get('name')}


def _xpd_value(v):
    return None if v in (None, '', 'None', 'NA') else v


def _channel(order, doc, camera, cycle, xpd_item, default_name=None):
    exposure = _num(_text(doc, 'ExposureTime'))
    marker, fluor = _text(doc, 'Biomarker'), _text(doc, 'Name') or default_name
    ex, em = _filters(doc)
    planned = (xpd_item or {}).get('exposure')
    return {
        'order': order,
        'name': marker or fluor,
        'dyePreset': _xpd_value((xpd_item or {}).get('dye')) or (fluor if marker else None),
        'biomarker': marker,
        'cycle': cycle,
        'afSubtracted': _bool(_text(doc, 'AutofluorescenceSubtracted')),
        'barcode': _xpd_value((xpd_item or {}).get('barcode')),
        'reporter': _xpd_value((xpd_item or {}).get('reporter')),
        'clone': _xpd_value((xpd_item or {}).get('clone')),
        'plannedExposureMs': planned if isinstance(planned, (int, float)) and planned > 0 else None,
        'laser': None,
        'detector': camera,
        'hv': None,
        'gain': _num(_text(doc, 'CameraSettings/Gain')),
        'offset': _num(_text(doc, 'CameraSettings/OffsetCounts')),
        'bitDepth': _int(_text(doc, 'CameraSettings/BitDepth')),
        'excitationFilter': ex,
        'emissionFilter': em,
        'filterCube': _text(doc, 'ExcitationFilter/Name'),
        'exposureMs': None if exposure is None else round(exposure / _US_PER_MS, 3),
        'illuminationIntensity': None,
    }


def _system(software):
    return 'PhenoImager Fusion' if software and software.split()[0] == 'Fusion' else None


def _iso(dt):
    """TIFF DateTime "2026:11:05 09:13:34" → "2026-11-05T09:13:34" (local, as written)."""
    if not dt or len(dt) < 19:
        return None
    return f'{dt[:10].replace(":", "-")}T{dt[11:19]}'


def _tiled(doc, shape):
    w, h = _int(_text(doc, 'CameraSettings/ROI/Width')), _int(_text(doc, 'CameraSettings/ROI/Height'))
    if w is None or h is None or not shape or len(shape) < 2:
        return False
    b = _int(_text(doc, 'CameraSettings/Binning')) or 1   # the ROI is in sensor pixels; binning merges them
    w, h = w // b, h // b
    return shape[1] > w or shape[0] > h


def _brightfield(doc, shape):
    """The scan profile's mode says so; an RGB page (3 samples) is the fallback evidence."""
    mode = _text(doc, 'ScanProfile/root/Mode')
    if mode:
        return mode == 'im_Brightfield'
    return bool(shape) and len(shape) == 3 and shape[-1] in (3, 4)


def parse_settings(pages, series=0, xpd=None):
    """Settings from the per-page dicts of `ome_xml_utils.tiff_page_descriptions` (+ the run's `.xpd`
    dict when there is one), or `{'reason': …}`."""
    if series != 0:
        return {'reason': f'series {series} is not the scanned image (thumbnail, overview or label)'}
    full = []
    for p in pages:
        if 'PerkinElmer-QPI-ImageDescription' not in p['description']:
            continue
        try:
            doc = ET.fromstring(p['description'].replace('encoding="utf-16"', '', 1).encode('utf-8'))
        except ET.ParseError:
            continue
        if _text(doc, 'ImageType') == _FULL:
            full.append((doc, p.get('datetime'), p.get('shape')))
    if not full:
        return {'reason': 'no full-resolution QPI channel pages'}
    first, _, shape = full[0]
    camera = _text(first, 'CameraName') or _text(first, 'CameraType')
    software = _text(first, 'AcquisitionSoftware')
    objective = _text(first, 'Objective')
    mag = _num(objective.lower().rstrip('x')) if objective else None
    brightfield = _brightfield(first, shape)
    times = sorted({t for _, t, _ in full if t})
    cyclic = len(times) > 1 and any(_text(d, 'Biomarker') for d, _, _ in full)
    per_page = _cycles_and_items(full, times, cyclic, xpd)
    # a brightfield page names no channel; its scan profile does ("BF")
    default = _text(first, 'ScanProfile/root/Name') if brightfield else None
    channels = [_channel(i + 1, d, camera, per_page[i][0], per_page[i][1], default)
                for i, (d, _, _) in enumerate(full)]
    modes = (['tiled'] if _tiled(first, shape) else []) + (['cyclic'] if cyclic else [])
    out = {
        'source': 'qptiff',
        'readerVersion': READER_VERSION,
        'technology': 'brightfield' if brightfield else 'widefield',
        'modes': modes,
        'system': _system(software),
        'softwareVersion': software,
        'acquiredAt': _iso(times[0]) if times else None,
        'objective': {'name': objective, 'magnification': mag, 'na': None, 'immersion': None},
        'lasers': [],
        'lightSource': _text(first, 'BFLampType' if brightfield else 'LampType'),
        'processing': _text(first, 'ImageProcessingApplied'),
        'saturationProtection': _text(first, 'ScanProfile/root/ScanSaturationProtectionType'),
        'pixelSizeUm': _num(_text(first, 'ScanProfile/root/ScanResolution/PixelSizeMicrons')),
        'camera': {
            'name': camera,
            'binning': _int(_text(first, 'CameraSettings/Binning')),
            'bitDepth': _int(_text(first, 'CameraSettings/BitDepth')),
            'sensorPixelUm': None,
        },
        'channels': channels,
    }
    if cyclic:
        out['cycles'] = {'count': len(times), **(_xpd_cycles(xpd) if xpd else {})}
    return out


def _sidecar_xpd(path):
    found = glob.glob(os.path.join(glob.escape(os.path.dirname(os.path.abspath(path))), '*.xpd'))
    return read_xpd(found[0]) if len(found) == 1 else None


def acquisition_settings(path, series=0):
    """Settings for `series` of a `.qptiff`, or `{'reason': …}` when none can be read. Never raises."""
    try:
        pages = ome_xml_utils.tiff_page_descriptions(path)
    except Exception as e:   # tifffile raises its own TiffFileError besides OSError/ValueError
        return {'reason': f'not a readable QPTIFF: {e}'}
    return parse_settings(pages, series, xpd=_sidecar_xpd(path))
