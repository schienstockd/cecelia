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
transmission is 1 − `Attenuation` when `Transmission` is absent, in a ZEN black file (it matches the
track setup's `Transmission` there); and averaging is not called a sum or a mean, because the channel block does
not say which.

Beyond laser scanning (checked on an Axio Scan.Z1 slide scan, LSM 880 spectral, LSM 800/900/980
Airyscan): `technology` from the channels — `WideField` acquisition mode → widefield (camera fields:
exposure, filter cube = `Reflector`, light-source intensity, binning); `AiryscanSettings` → airyscan;
a multiphoton mode or a line at or above 680 nm → two-photon; other laser scanning → confocal. `modes`
adds `spectral` (`SpectralImaging`: channels are wavelength bins) and `tiled` (`SizeM` > 1). ZEN blue
writes `System` "LSM 800" for an LSM 900 and 980 alike; the model is the imaging device's name
(`HardwareSetting/Configuration/Device[@Id="MTBLSMImagingDevice"]`), and the stand moves to
`microscopeBody`. A slide scanner records no `System`: its stand name is the system.

Per channel, for a card laid out by track (version 3): the track (ZEN names a multi-track channel
`<detector>-T<n>`, both editions — our reading: the suffix is the only track link the channel block
has), the lines on it with their power (`laserLines`), the detection band (`DetectionWavelength/Ranges`),
the pinhole in µm and Airy units, and the beam splitters (`LaserScanInfo` ids, named from
`Instrument/Dichroics`). The 1 − `Attenuation` reading below was checked on ZEN black files only
(`Application/Name` "AIMApplication"); elsewhere an attenuation-only power is not read. ZEN 2.3 blue
writes 1.9, 2.6 and 0.95 on an LSM 800 with no `Transmission`: not fractions, unit unknown — read as
1 − attenuation they gave −90 % and −160 %.

Fixtures: `test-data/acquisition/czi_*`, from real files.
"""
import re
import struct
import xml.etree.ElementTree as ET  # vendor .czi XML, not OME-XML: test_zarr_access_convention _BASELINE

#: Bump when the settings dict gains a field (its row in acquisition_readers.json must match).
READER_VERSION = 3   # 2: technology, modes, camera fields, the LSM model name; 3: per-channel track,
#                       band, pinhole, laser lines and beam splitters

#: Shortest excitation that still means multiphoton (`MULTIPHOTON_MIN_NM`, frontend acquisitionLightPath.ts).
_MULTIPHOTON_MIN_NM = 680
_NS_PER_MS = 1e6   # ZEN `ExposureTime` is in ns

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


_TRACK_SUFFIX = re.compile(r'-T(\d+)$')


def _band(ch):
    """The detection band(s) as recorded, "555-624 nm" (two ranges joined by ", "), or None."""
    out = []
    for r in (_text(ch, 'DetectionWavelength/Ranges') or '').split(','):
        lo, _, hi = r.strip().partition('-')
        lo, hi = _num(lo), _num(hi)
        if lo is not None and hi is not None:
            out.append(f'{round(lo)}-{round(hi)}')
    return f"{', '.join(out)} nm" if out else None


def _splitters(ch, names):
    """The beam splitters a laser-scanning channel ran with, by name, main before secondary."""
    lsi = ch.find('LaserScanInfo')
    ids = [_text(lsi, k) for k in ('MainBeamSplitterVis', 'MainBeamSplitterInVis', 'SecondaryBeamSplitter')]
    return [names.get(i) or i for i in ids if i]


def _channels(info, block):
    names = _track_lasers(block)
    dets = _detector_names(info)
    splitter_names = {d.get('Id'): (d.get('Name') or '').strip() or None for d in info.iterfind('Instrument/Dichroics/Dichroic')}
    image_bits = _num(_text(info, 'Image/ComponentBitCount'))
    xml_chans = list(info.iterfind('Image/Dimensions/Channels/Channel'))
    # 1 − attenuation only where it was checked: ZEN black (see the module docstring)
    att_known = _text(info, 'Application/Name') == 'AIMApplication'
    chans, lasers = [], {}
    for i, ch in enumerate(xml_chans):
        on, lines = [], []      # every line exciting this channel: one channel can have two at once
        for ls in ch.iterfind('LightSourcesSettings/LightSourceSettings'):
            wl = _nm(_num(_text(ls, 'Wavelength')))
            tr = _num(_text(ls, 'Transmission'))
            if tr is None and _num(_text(ls, 'Attenuation')) is not None and att_known:
                tr = 1 - _num(_text(ls, 'Attenuation'))      # our reading: see the module docstring
            pct = None if tr is None else round(tr * 100, 3)
            laser = names.get(wl) or (f'{wl:g} nm' if wl is not None else None)
            if laser is not None:
                on.append(laser)
                lines.append({'wavelengthNm': wl, 'pct': pct})
                lasers.setdefault((laser, wl, pct), {'name': laser, 'wavelengthNm': wl, 'transmissionPct': pct})
        track = _TRACK_SUFFIX.search(ch.get('Name') or '')
        ph_um, ph_au = _num(_text(ch, 'PinholeSize')), _num(_text(ch, 'PinholeSizeAiry'))
        ds = ch.find('DetectorSettings')
        det_id = ds.find('Detector').get('Id') if ds is not None and ds.find('Detector') is not None else None
        bits = _num(_text(ch, 'ComponentBitCount')) or image_bits
        mode = _text(ch, 'AcquisitionMode')
        camera = mode == 'WideField'
        exposure = _num(_text(ch, 'ExposureTime'))
        intensity = _text(ch, 'LightSourcesSettings/LightSourceSettings/Intensity')
        chans.append({
            'order': i + 1,
            'acquisitionMode': mode,
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
            'track': int(track.group(1)) if track else None,
            'laserLines': lines,
            'emissionFilter': _band(ch),
            'pinholeUm': None if ph_um is None else round(ph_um, 2),
            'pinholeAiry': None if ph_au is None else round(ph_au, 2),
            'beamSplitters': _splitters(ch, splitter_names),
            **({'filterCube': _text(ch, 'Reflector'),
                'exposureMs': None if exposure is None else round(exposure / _NS_PER_MS, 3),
                'illuminationIntensity': _num((intensity or '').replace('%', '')),
                'binning': _text(ds, 'Binning')} if camera else {}),
            # ZEN writes Mode "Off" on a plain LSM channel: only another mode is Airyscan
            'airyscan': None if (_text(ch, 'AiryscanSettings/Mode') or 'Off') == 'Off' else _text(ch, 'AiryscanSettings/Mode'),
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


def _imaging_device(md):
    """The LSM model ZEN records on the imaging device ("LSM 980"), or None."""
    d = md.find("HardwareSetting/Configuration/Device[@Id='MTBLSMImagingDevice']")
    return (d.get('Name') or '').strip() or None if d is not None else None


def _technology(channels, lasers):
    modes = {c.get('acquisitionMode') or '' for c in channels}
    if modes and modes <= {'WideField'}:
        return 'widefield'
    if any(c.get('airyscan') for c in channels):
        return 'airyscan'
    if any('MultiPhoton' in m for m in modes) or any((l['wavelengthNm'] or 0) >= _MULTIPHOTON_MIN_NM for l in lasers):
        return 'two-photon'
    if any(m in ('LaserScanningConfocalMicroscopy', 'SpectralImaging') for m in modes):
        return 'confocal'
    return None


def _camera(info, channels):
    cams = [c for c in channels if c.get('acquisitionMode') == 'WideField']
    if not cams:
        return None
    binning = _one(c.get('binning') for c in cams)
    b = _num(binning.split(',')[0]) if binning else None
    return {'name': _one(c['detector'] for c in cams), 'binning': None if b is None else int(b),
            'bitDepth': _one(c['bitDepth'] for c in cams), 'sensorPixelUm': None}


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
    pinhole = _one(c['pinholeUm'] for c in channels)
    pinhole_airy = _one(c['pinholeAiry'] for c in channels)
    shared = _one(tuple(c['beamSplitters']) for c in channels)
    stand = info.find('Instrument/Microscopes/Microscope')
    stand_name = stand.get('Name') if stand is not None else None
    device = _imaging_device(md)
    modes = [m for m, on in (
        ('spectral', any(c.get('acquisitionMode') == 'SpectralImaging' for c in channels)),
        ('tiled', (_num(_text(info, 'Image/SizeM')) or 1) > 1)) if on]
    camera = _camera(info, channels)
    for c in channels:
        c.pop('binning', None)
    return {
        'source': 'czi',
        'readerVersion': READER_VERSION,
        'technology': _technology(channels, lasers),
        'modes': modes,
        'system': device or _text(info, 'Instrument/Microscopes/Microscope/System') or stand_name,
        'microscopeBody': stand_name if (device or _text(info, 'Instrument/Microscopes/Microscope/System')) else None,
        'camera': camera,
        'softwareVersion': _text(info, 'Application/Version'),
        'objective': _objective(info),
        'lasers': lasers,
        'brightZ': _bright_z(block),
        'scanner': _scanner(info),
        'autoHV': None,
        'pinholeUm': pinhole,     # None when channels differ (multi-track)
        'pinholeAiry': pinhole_airy,
        # image-level only when every channel ran with the same ones; else per channel (`beamSplitters`)
        'dichroics': [{'slot': 'beam splitter', 'name': n} for n in shared] if shared else [],
        'channels': channels,
        **_axes(info, md),
    }


def acquisition_settings(path, series=0):
    """Settings for a `.czi`, or `{'reason': …}` when none can be read. Never raises. `series` is
    the reader contract (`acquisition_readers.json`); the settings are file-level, so it is unused."""
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
