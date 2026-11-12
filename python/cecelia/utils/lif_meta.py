"""Read the acquisition settings a Leica `.lif` records, in the shape `oir_meta` gives an `.oir`.

A `.lif` opens with one XML document that describes every image in the file; readlif
(https://github.com/nimne/readlif) reads it, the same library the series picker uses.

One file holds many images. `series` is the picker's index (`probe_series_run._probe_lif`, stored as
`meta.ori_series`): the position in readlif's `LifFile.image_list`. The element is found from that
entry's own path and name, never by counting elements a second way — two counts that disagree (a
zero-size image, a nested folder) would attach a neighbour's settings.

Domain: camera (widefield) elements, which Leica marks `HardwareSetting DataSourceTypeName="Camera"`
and describes in `ATLCameraSettingDefinition` plus one `WideFieldChannelInfo` per configured channel.
Checked on LAS X 3.4.2 files from a DM5500B with a DFC365 FX camera. A confocal element
(`ATLConfocalSettingDefinition`) returns a reason: no confocal file has been seen, so no parse is
guessed for it.

Two readings are ours, not Leica's, stated where made: a channel's settings are matched to the
recorded channel by LUT name (the configuration lists every channel set up, the data can hold fewer —
a single-channel image still lists both); and `acquiredAt` is the first time stamp, a Windows FILETIME
(100 ns ticks since 1601, UTC).

Fixtures: `test-data/acquisition/lif_*`, from real files.
"""
import xml.etree.ElementTree as ET  # vendor .lif XML, not OME-XML: test_zarr_access_convention _BASELINE
from datetime import datetime, timedelta, timezone

#: Bump when the settings dict gains a field: images read by an older reader are re-read
#: (its row in app/src/tasks/importImages/acquisition_readers.json must match).
READER_VERSION = 1

_FILETIME_EPOCH = datetime(1601, 1, 1, tzinfo=timezone.utc)
#: `DimensionDescription DimID`: 1 x, 2 y, 3 z, 4 t (Bio-Formats `LIFReader.translateImageNodes`).
_DIM_Z, _DIM_T = '3', '4'


def _num(s):
    try:
        return float(s) if s is not None and s.strip() != '' else None
    except ValueError:
        return None


def _int(s):
    v = _num(s)
    return None if v is None else int(v)


def image_element(root, image_list, series):
    """The `Element` of `image_list[series]` (readlif's entries: `path` "Project/", `name` "Sub/Image002"),
    or None. Same-named images under one parent are told apart by their order, as readlif lists them."""
    entry = image_list[series]
    top = entry['path'].split('/')[0]
    segs = entry['name'].split('/')
    nodes = root.findall('./Children/Element') or root.findall('./Element')
    if top:
        nodes = [n for n in nodes if n.get('Name') == top]
        for seg in segs:
            nodes = [c for n in nodes for c in n.findall('./Children/Element') if c.get('Name') == seg]
    else:   # an image at the top level
        nodes = [n for n in nodes if n.get('Name') == segs[-1]]
    nodes = [n for n in nodes if n.find('Data/Image') is not None]
    k = sum(1 for e in image_list[:series] if (e['path'], e['name']) == (entry['path'], entry['name']))
    return nodes[k] if k < len(nodes) else None


def _acquired_at(image):
    stamps = (image.findtext('TimeStampList') or '').split()
    if not stamps:
        return None
    try:
        ticks = int(stamps[0], 16)
    except ValueError:
        return None
    return (_FILETIME_EPOCH + timedelta(microseconds=ticks / 10)).isoformat(timespec='seconds')


def _axes(image):
    out = {}
    dims = {d.get('DimID'): d for d in image.iterfind('ImageDescription/Dimensions/DimensionDescription')}
    z, t = dims.get(_DIM_Z), dims.get(_DIM_T)
    n_z = _int(z.get('NumberOfElements')) if z is not None else None
    if n_z and n_z > 1:
        length = _num(z.get('Length'))
        step = None if length is None else round(abs(length) / (n_z - 1) * 1e6, 4)   # metres → µm
        out['zStack'] = {'stepUm': step, 'count': n_z}
    n_t = _int(t.get('NumberOfElements')) if t is not None else None
    if n_t and n_t > 1:
        length = _num(t.get('Length'))
        out['timelapse'] = {'intervalS': None if length is None else round(length / (n_t - 1), 4),
                            'count': n_t}
    return out


def _camera_name(raw):
    """`DFC365FX-630613412` → `DFC365FX`: the model, without the unit's serial number."""
    return raw.split('-')[0] if raw else None


def _channels(image, cam, camera):
    """One entry per RECORDED channel (`ChannelDescription`), with the settings of the configured
    channel whose LUT matches it; positional when the LUTs don't tell them apart."""
    recorded = list(image.iterfind('ImageDescription/Channels/ChannelDescription'))
    configured = [w for w in cam.iter('WideFieldChannelInfo') if w.get('Active', '1') == '1']
    by_lut = {}
    for w in configured:
        by_lut.setdefault((w.get('LUT') or '').lower(), []).append(w)
    out = []
    for i, c in enumerate(recorded):
        hits = by_lut.get((c.get('LUTName') or '').lower(), [])
        w = hits[0] if len(hits) == 1 else (configured[i] if len(recorded) == len(configured) else None)
        exposure = _num(w.get('ExposureTime')) if w is not None else None
        out.append({
            'order': i + 1,
            'name': (w.get('UserDefName') if w is not None else None) or c.get('LUTName'),
            'dyePreset': None,
            'laser': None,
            'detector': camera,
            'hv': None,
            'gain': _num(w.get('Gain')) if w is not None else None,
            'offset': None,
            'bitDepth': _int(c.get('Resolution')),
            'emissionFilter': None,
            'filterCube': w.get('FluoCubeName') if w is not None else None,
            'exposureMs': None if exposure is None else round(exposure * 1000, 3),
            'illuminationIntensity': _num(w.get('IL_Light-Intensity')) if w is not None else None,
        })
    return out


def _objective(cam):
    name = (cam.get('ObjectiveName') or '').strip() or None
    return {'name': ' '.join(name.split()) if name else None,
            'magnification': _num(cam.get('Magnification')),
            'na': _num(cam.get('NumericalAperture')),
            'immersion': (cam.get('Immersion') or '').strip().lower() or None}


def parse_settings(root, image_list, series=0):
    """Normalised settings dict for image `series` (readlif's `xml_root` + `image_list`), or `{'reason': …}`."""
    if not 0 <= series < len(image_list):
        return {'reason': f'series {series} not in the file ({len(image_list)} images)'}
    el = image_element(root, image_list, series)
    if el is None:
        return {'reason': f'image {image_list[series]["name"]!r} not found in the header'}
    image = el.find('Data/Image')
    hw = image.find("Attachment[@Name='HardwareSetting']")
    if hw is None:
        return {'reason': 'no hardware settings recorded for this image'}
    if hw.find('.//ATLConfocalSettingDefinition') is not None:
        return {'reason': 'Leica confocal settings are not read yet (no confocal .lif seen)'}
    cam = hw.find('.//ATLCameraSettingDefinition')
    if cam is None:
        return {'reason': f'no camera settings ({hw.get("DataSourceTypeName") or "unknown source"})'}
    fmt = cam.find('CameraFormat')
    camera = _camera_name(cam.get('CameraName') or next(
        (e.get('CameraName') for e in hw.iter() if e.get('CameraName')), None))
    channels = _channels(image, cam, camera)
    if not channels:
        return {'reason': 'no channels in the image description'}
    sensor = _num(cam.get('TheoCamSensorPixelSizeX'))
    return {
        'source': 'lif',
        'readerVersion': READER_VERSION,
        'technology': 'widefield',
        'system': cam.get('MicroscopeModel') or None,
        'softwareVersion': hw.get('Software') or None,
        'acquiredAt': _acquired_at(image),
        'imageName': el.get('Name'),
        'objective': _objective(cam),
        'lasers': [],
        'camera': {
            'name': camera,
            'binning': _int(fmt.get('Binning')) if fmt is not None else _int(cam.get('BinningValuePrecise')),
            'bitDepth': _int(fmt.get('Resolution')) if fmt is not None else None,
            'sensorPixelUm': None if sensor is None else round(sensor * 1e6, 3),   # metres → µm
        },
        'channels': channels,
        **_axes(image),
    }


def acquisition_settings(path, series=0):
    """Settings for image `series` of a `.lif`, or `{'reason': …}` when none can be read. Never raises."""
    try:
        from readlif.reader import LifFile   # lazily: only a .lif needs it
        lf = LifFile(path)
    except (OSError, ValueError, ET.ParseError) as e:
        return {'reason': f'not a readable LIF: {e}'}
    return parse_settings(lf.xml_root, lf.image_list, series)
