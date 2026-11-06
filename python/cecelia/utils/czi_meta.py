"""Read the acquisition settings a Zeiss `.czi` records, in the shape `oir_meta` gives an `.oir`.

A `.czi` is a ZISRAW segment container. Its metadata is one XML document (`ImageDocument`) in the
`ZISRAWMETADATA` segment, whose file offset the file header holds — so the read is two short seeks,
not a scan. Layout: Zeiss's reference reader libCZI, `Src/libCZI/CziStructs.h`
(`FileHeaderSegmentData`, packed: `MetadataPosition` at byte 60 of the data, after the 32-byte segment
header), https://github.com/ZEISS/libczi. The offsets are also checked on 24 public files from ZEN
black (LSM 780/880) and ZEN blue (LSM 800, light sheet), fetched by byte range.

The settings come from `Information/Image/Dimensions/Channels`, which both ZEN editions write, and
two edition-specific extras from the acquisition block that ran (`ExperimentBlockIndex`): laser names
from a ZEN black track setup, and whether automatic Z brightness correction was on. Domain: laser-
scanning LSM files (checked: LSM 780 two-photon and confocal, LSM 880, LSM 800). One rule for
image-level values (scan settings, pinhole): reported only when every channel records the same value,
else None — never one track's value for all. Values are reported as recorded, units in the key names.
Three readings are ours, not Zeiss's, and stated where made: a ZEN black channel's `Gain` is the
detector HV (it equals the track setup's `Voltage` in every ZEN black file checked); laser
transmission is 1 − `Attenuation` when `Transmission` is absent (it matches the track setup's
`Transmission` there); and averaging is not called a sum or a mean, because the channel block does
not say which.

Fixtures: `test-data/acquisition/czi_*`, from real files.
"""
import struct
import xml.etree.ElementTree as ET  # vendor .czi XML, not OME-XML: test_zarr_access_convention _BASELINE

_FILE_MAGIC = b'ZISRAWFILE'
_META_MAGIC = b'ZISRAWMETADATA'
_SEGMENT_HEADER = 32           # 16-byte id, int64 allocated size, int64 used size
_META_POSITION_AT = 92         # file header: segment header, 4 int32, 2 GUIDs, int32, int64 → here
_META_DATA_HEADER = 256        # metadata segment: int32 XML size, int32 attachment size, spare


def read_settings_xml(path):
    """The metadata XML bytes of a `.czi`, or None when the file is not a CZI or has none."""
    with open(path, 'rb') as f:
        head = f.read(_META_POSITION_AT + 8)
        if len(head) < _META_POSITION_AT + 8 or not head.startswith(_FILE_MAGIC):
            return None
        pos = struct.unpack_from('<q', head, _META_POSITION_AT)[0]
        if pos <= 0:
            return None
        f.seek(pos)
        seg = f.read(_SEGMENT_HEADER + 4)
        if len(seg) < _SEGMENT_HEADER + 4 or not seg.startswith(_META_MAGIC):
            return None
        size = struct.unpack_from('<i', seg, _SEGMENT_HEADER)[0]
        f.seek(pos + _SEGMENT_HEADER + _META_DATA_HEADER)
        xml = f.read(size)
    return xml if len(xml) == size and size > 0 else None


def _num(s):
    try:
        return float(s) if s is not None and s.strip() != '' else None
    except ValueError:
        return None


def _text(el, path):
    t = el.findtext(path) if el is not None else None
    return t.strip() if t and t.strip() else None


def _bool(s):
    return None if s is None else s.strip().lower() == 'true'


def _nm(x):
    """A wavelength in nm, rounded: ZEN stores 775.00000000000023 in one place and 7.75e-07 m in another."""
    return None if x is None else round(x * 1e9 if x < 1e-3 else x, 1)


def _active_block(md):
    """The acquisition block that ran: `ExperimentBlockIndex` picks it among the experiment's blocks."""
    blocks = list(md.iterfind('Experiment/ExperimentBlocks/AcquisitionBlock'))
    idx = _num(_text(md, 'Experiment/ExperimentBlockIndex'))
    i = int(idx) if idx is not None else 0
    return blocks[i] if 0 <= i < len(blocks) else None


def _active(el):
    return el.get('IsActivated', 'true').strip().lower() != 'false'


def _track_lasers(block):
    """ZEN black: laser name per wavelength, from the active block's track setups."""
    out = {}
    for a in block.iterfind('MultiTrackSetup/TrackSetup/Attenuators/Attenuator') if block is not None else ():
        wl = _nm(_num(_text(a, 'Wavelength')))
        if wl is not None and _text(a, 'Laser'):
            out.setdefault(wl, _text(a, 'Laser'))
    return out


def _one(values):
    """The value every channel shares, else None: one number must not stand for tracks that differ,
    nor for a channel that does not record it."""
    vals = list(values)
    return vals[0] if vals and vals[0] is not None and all(v == vals[0] for v in vals) else None


def _detector_names(info):
    out = {}
    for d in info.iterfind('Instrument/Detectors/Detector'):
        out[d.get('Id')] = _text(d, 'Manufacturer/Model') or d.get('Name') or None
    return out


def _channels(info, block):
    names = _track_lasers(block)
    dets = _detector_names(info)
    image_bits = _num(_text(info, 'Image/ComponentBitCount'))
    chans, lasers = [], {}
    for i, ch in enumerate(info.iterfind('Image/Dimensions/Channels/Channel')):
        on = []      # every line exciting this channel: one channel can have two at once
        for ls in ch.iterfind('LightSourcesSettings/LightSourceSettings'):
            wl = _nm(_num(_text(ls, 'Wavelength')))
            tr = _num(_text(ls, 'Transmission'))
            if tr is None and _num(_text(ls, 'Attenuation')) is not None:
                tr = 1 - _num(_text(ls, 'Attenuation'))      # our reading: see the module docstring
            pct = None if tr is None else round(tr * 100, 3)
            laser = names.get(wl) or (f'{wl:g} nm' if wl is not None else None)
            if laser is not None:
                on.append(laser)
                lasers.setdefault((laser, wl, pct), {'name': laser, 'wavelengthNm': wl, 'transmissionPct': pct})
        ds = ch.find('DetectorSettings')
        det_id = ds.find('Detector').get('Id') if ds is not None and ds.find('Detector') is not None else None
        bits = _num(_text(ch, 'ComponentBitCount')) or image_bits
        chans.append({
            'order': i + 1,
            'name': ch.get('Name'),
            'dyePreset': _text(ch, 'Fluor'),
            'laser': ' + '.join(dict.fromkeys(on)) or None,
            'detector': dets.get(det_id),      # the model; ZEN black often records none
            # ZEN blue writes `Voltage`; ZEN black writes the same HV as `Gain` (our reading, checked on
            # laser-scanning channels only: a camera channel's gain is not an HV).
            'hv': _num(_text(ds, 'Voltage')) if _text(ds, 'Voltage') else (
                _num(_text(ds, 'Gain')) if ch.find('LaserScanInfo') is not None else None),
            'gain': _num(_text(ds, 'DigitalGain')),
            'offset': _num(_text(ds, 'Offset')),
            'detectorMode': _text(ds, 'DetectorMode'),   # Integration / PhotonCounting, ZEN blue only
            'bitDepth': int(bits) if bits else None,
        })
    return chans, list(lasers.values())


def _scanner(info):
    """Scan settings shared by every channel; a setting the tracks set differently is None."""
    scans = [lsi for lsi in info.iterfind('Image/Dimensions/Channels/Channel/LaserScanInfo')]
    if not scans:
        return None
    dwell = _one(_num(_text(x, 'PixelTime')) for x in scans)
    avgs = [_num(_text(x, 'Averaging')) for x in scans]
    avg = _one(avgs)
    return {
        'type': None,
        'zoom': _one(_num(_text(x, 'ZoomX')) for x in scans),
        'pixelDwellUs': None if dwell is None else round(dwell * 1e6, 4),
        # Averaging count only: the channel block does not say mean or sum (our reading: unknown).
        'integration': None if avg is None else {
            'type': 'Average' if avg > 1 else 'None', 'count': int(avg), 'average': None},
    }


def _objective(info):
    ref = info.find('Image/ObjectiveSettings/ObjectiveRef')
    objs = list(info.iterfind('Instrument/Objectives/Objective'))
    obj = next((o for o in objs if ref is not None and o.get('Id') == ref.get('Id')), objs[0] if objs else None)
    if obj is None:
        return None
    return {
        'name': _text(obj, 'Manufacturer/Model') or obj.get('Name'),
        'magnification': _num(_text(obj, 'NominalMagnification')),
        'na': _num(_text(obj, 'LensNA')),
        'immersion': (_text(obj, 'Immersion') or '').lower() or None,
        'refractiveIndex': _num(_text(info, 'Image/ObjectiveSettings/RefractiveIndex')),
    }


def _bright_z(block):
    """Automatic Z brightness correction (Zeiss's depth ramp) in the block that ran: on or off. ZEN
    black records it as `ZStackSetup/StackBrightnessCorrection`, ZEN blue as
    `UseAutoBrightnessCorrection` under the z-stack setup; a z-stack setup switched off means no ramp.
    The ramp points are not read: no file checked had it on, so their layout is unseen."""
    if block is None:
        return None
    on = _text(block, 'ZStackSetup/StackBrightnessCorrection')
    if on is None:
        for z in block.iter('ZStackSetup'):
            el = next(z.iter('UseAutoBrightnessCorrection'), None)
            if el is not None:
                on = el.text if _active(z) else 'false'
                break
    return None if on is None else {'enabled': _bool(on), 'points': []}


def _axes(info, md):
    out = {}
    size_z, size_t = _num(_text(info, 'Image/SizeZ')), _num(_text(info, 'Image/SizeT'))
    if size_z and size_z > 1:
        z = next((d for d in md.iterfind('Scaling/Items/Distance') if d.get('Id') == 'Z'), None)
        step = _num(_text(z, 'Value'))
        out['zStack'] = {'stepUm': None if step is None else round(step * 1e6, 4), 'count': size_z}
    if size_t and size_t > 1:
        out['timelapse'] = {'intervalS': _num(_text(info, 'Image/Dimensions/T/Positions/Interval/Increment')),
                            'count': size_t}
    return out


def parse_settings(xml_bytes):
    """Normalised settings dict from the `ImageDocument` metadata XML."""
    root = ET.fromstring(xml_bytes)
    md = root.find('Metadata')
    info = md.find('Information') if md is not None else None
    if info is None:
        return {'reason': 'no Information block in the metadata'}
    block = _active_block(md)
    channels, lasers = _channels(info, block)
    if not channels:
        return {'reason': 'no channels in the metadata'}
    # One rule: an image-level pinhole only when every channel records the same one. ZEN black keeps
    # pinholes per track detector, not per channel, so it never qualifies (a non-descanned or
    # transmitted channel has none).
    pinhole = _one(None if v is None else round(v, 2) for v in (
        _num(_text(c, 'PinholeSize')) for c in info.iterfind('Image/Dimensions/Channels/Channel')))
    return {
        'source': 'czi',
        'system': _text(info, 'Instrument/Microscopes/Microscope/System'),
        'softwareVersion': _text(info, 'Application/Version'),
        'objective': _objective(info),
        'lasers': lasers,
        'brightZ': _bright_z(block),
        'scanner': _scanner(info),
        'autoHV': None,
        'pinholeUm': pinhole,     # None when channels differ (multi-track)
        'channels': channels,
        **_axes(info, md),
    }


def acquisition_settings(path):
    """Settings for a `.czi`, or `{'reason': …}` when none can be read. Never raises."""
    try:
        xml = read_settings_xml(path)
    except OSError as e:
        return {'reason': f'could not read the source file: {e}'}
    if xml is None:
        return {'reason': 'no metadata segment found in the file'}
    try:
        return parse_settings(xml)
    except ET.ParseError as e:
        return {'reason': f'metadata did not parse: {e}'}
