"""Read the acquisition settings an Olympus/Evident `.oir` records but Bio-Formats drops.

An `.oir` is a block container. Its settings are one embedded XML document,
`lsmimage:imageProperties`, stored after the first frame. Bio-Formats copies part of it into the OME
model and part into the unstructured original-metadata table, and drops the rest. Measured on an
FV4000 (3.2.1) and an FVMPE-RS (2.3.2) file, the dropped part includes what matters most for
two-photon acquisition QC: the Bright Z depth/power ramp and whether it was on, pixel dwell and line
time, auto HV, the per-channel laser, detector and emission filter, and the dichroics in the path. This reads the document directly,
the same way `ims_meta` recovers Imaris timing.

Values are reported as recorded, with units in the key names. Two readings are ours, not Evident's,
and are stated where they are made: the scanner timing unit (ms) and which Z axis is the active one.
Nothing here is interpreted — whether a setting was sensible is the caller's question.

Fixtures: `test-data/acquisition/oir_*_imageProperties.xml`, cut from real files.
"""
import re
import xml.etree.ElementTree as ET  # vendor .oir XML, not OME-XML: test_zarr_access_convention _BASELINE

#: Bumped when the reader starts returning a field it did not before, so Refresh metadata re-reads
#: settings stored by an older reader (the `.oir` row of
#: app/src/tasks/importImages/acquisition_readers.json must match — asserted in app/test/suite/segmentation.jl
#: → "language boundaries agree on their protocol"). 2: date, body, filters, dichroics.
READER_VERSION = 2

#: The settings document's root element, as it appears in the raw bytes.
_ROOT_TAG = b'lsmimage:imageProperties'
_START = b'<?xml'

#: Read the container in chunks until the settings document is found. It sits after the first frame
#: (8 MB in on a 4-channel 1024² FV4000 file), so a file with very large frames needs a generous cap.
_CHUNK = 8 << 20
_MAX_SCAN = 1 << 30


def _local(tag):
    return tag.split('}', 1)[-1]


def _strip_namespaces(root):
    """Drop namespace prefixes so paths read as the vendor's local names."""
    for el in root.iter():
        el.tag = _local(el.tag)
        el.attrib = {_local(k): v for k, v in el.attrib.items()}
    return root


def _num(text):
    try:
        return float(text)
    except (TypeError, ValueError):
        return None


def _text(el, path):
    found = el.find(path) if el is not None else None
    if found is None or found.text is None:
        return None
    return found.text.strip()


def _bool(el, path):
    t = _text(el, path)
    return None if t is None else t.lower() == 'true'


def extract_settings_xml(data):
    """The `imageProperties` document from raw `.oir` bytes, or None.

    Takes bytes rather than a path so the fixture (the document alone) and a real container go
    through the same code.
    """
    root_at = data.find(b'<' + _ROOT_TAG)
    if root_at < 0:
        return None
    start = data.rfind(_START, 0, root_at)
    end = data.find(b'</' + _ROOT_TAG + b'>', root_at)
    if start < 0 or end < 0:
        return None
    return data[start:end + len(_ROOT_TAG) + 3]


def read_settings_xml(path, chunk=_CHUNK, max_scan=_MAX_SCAN):
    """Scan an `.oir` for its settings document. Reads only as far as the document's end."""
    buf = b''
    with open(path, 'rb') as f:
        while len(buf) < max_scan:
            block = f.read(chunk)
            if not block:
                break
            buf += block
            if b'</' + _ROOT_TAG + b'>' in buf[-(len(block) + 64):]:
                break
    return extract_settings_xml(buf)


def _lasers(acq, phase, root):
    """laserDataId -> {name, wavelengthNm, transmissionPct} for the lasers this phase used. Name from
    the system laser list."""
    names = {_text(l, 'id'): _text(l, 'name') for l in root.findall('lsm/laser')}
    lasers = {}
    for ml in phase.findall('imagingMainLaser'):
        if ml.get('enable') != 'true' or _bool(ml, 'forcedOff'):
            continue                   # stored but not used for this acquisition
        lasers[ml.get('id')] = {
            'name': names.get(ml.get('laserId'), ml.get('laserId')),
            'wavelengthNm': _num(_text(ml, 'wavelength')),
        }
    for ml in acq.findall('imagingParam/mainLaser'):
        if ml.get('laserDataId') in lasers:
            lasers[ml.get('laserDataId')]['transmissionPct'] = _num(_text(ml, 'transmissivity'))
    return lasers


def _emission_filters(root, phase):
    """channel id -> barrier (emission) filter name, from the image-info block of the same phase."""
    out = {}
    for info in root.findall('imageInfo/phase'):
        if info.get('id') not in (None, phase.get('id')):
            continue
        for ch in info.findall('group/channel'):
            name = _text(ch, 'productData/filterName')
            if name:
                out[ch.get('id')] = name
    return out


#: Light-path slots that hold a dichroic or mirror cube, as the vendor names them. Which slot does
#: what (laser combiner, frame cube) is our reading of the element names, so the slot is kept as-is
#: and the GUI labels it.
_DICHROIC_SLOTS = (
    ('excitationDM', 'productData/lightpath/excitationDM/name'),
    ('dmSlider', 'productData/lightpath/dmSlider/name'),
    ('irLaserDichroic', 'productData/lightpath/irLaserLightpath/dichroicMirror/name'),
)
_CUBE_SLOTS = (('firstMirrorCube', 'firstMirrorCube/name'), ('secondMirrorCube', 'secondMirrorCube/name'))


def _dichroics(acq, phase):
    """Dichroic / mirror-cube slots in the light path of this phase and the microscope frame, in file
    order, each as recorded — `Empty` included, so "every slot empty" (recorded) stays distinct from
    "no slot found" (`[]`, not recorded). A slot this file's model does not have is left out."""
    found = [(slot, _text(phase, path)) for slot, path in _DICHROIC_SLOTS]
    frame = acq.find('.//lsmMicroscopePhase/lightpath')
    if frame is not None:
        found += [(slot, _text(frame, path)) for slot, path in _CUBE_SLOTS]
    return [{'slot': slot, 'name': name} for slot, name in found if name]


def _channels(acq, phase, lasers, filters=None):
    """Enabled channels of this phase in acquisition order, each with its laser and detector settings."""
    filters = filters or {}
    # Keyed by CHANNEL: two channels can share a detector (sequential groups), each with its own HV.
    # A `<pmt>` without a channelId (seen on bright-Z points of older files) falls back to the detector.
    pmts = {(p.get('channelId') or p.get('detectorId')): p for p in acq.findall('imagingParam/pmt')}
    out = []
    for ch in phase.findall('group/channel'):
        if ch.get('enable') != 'true':
            continue
        det = pmts.get(ch.get('id')) or pmts.get(ch.get('detectorId'))
        out.append({
            'order': int(ch.get('order', '0')),
            'name': _text(ch, 'name'),
            'dyePreset': _text(ch, 'dyeName'),
            'laser': lasers.get(_text(ch, 'laserDataId'), {}).get('name'),
            'detector': _text(ch, 'deviceName'),
            'detectorId': ch.get('detectorId'),
            'bitDepth': _num(_text(ch, 'bitCount')),
            'hv': _num(_text(det, 'voltage')),
            'gain': _num(_text(det, 'gain')),
            'offset': _num(_text(det, 'offset')),
            'emissionFilter': filters.get(ch.get('id')),
        })
    return sorted(out, key=lambda c: c['order'])


def _bright_z(acq, phase, lasers):
    """Bright Z: whether it was ON, and the stored (z, laser %) points.

    The points are stored even when it is off (seen on the FVMPE-RS file), so `enabled` is what says
    whether they were applied. `zFromStartUm` is the vendor's `relativeCoordinate`: distance from the
    start of the stack, not from the tissue surface.
    """
    points = []
    # One block per phase AND per Z drive (motor/piezo); only the drive that moved the stack counts.
    drive = _text(acq, 'imagingParam/zDriveUnitType')
    blocks = [b for b in acq.findall('imagingParam/brightnessAdjustmentParam/phaseBrightnessAdjustment')
              if b.get('phaseId') in (None, phase.get('id'))
              and (drive is None or b.get('zDriveUnitType') in (None, drive))]
    for z in (z for b in blocks for z in b.findall('zBrightnessAdjustment')):
        points.append({
            'zFromStartUm': _num(_text(z, 'relativeCoordinate')),
            # Only lasers this phase used: a switched-off laser keeps its stored ramp values.
            'transmissionPct': {lasers[lp.get('laserDataId')]['name']: _num(_text(lp, 'transmissivity'))
                                for lp in z.findall('laserParam') if lp.get('laserDataId') in lasers},
            'hv': {(pp.get('channelId') or pp.get('detectorId')): _num(_text(pp, 'voltage'))
                   for pp in z.findall('pmtParam')},
        })
    return {
        'enabled': _bool(acq, 'imagingParam/brightnessAdjustmentParam/enable'),
        'points': sorted(points, key=lambda p: p['zFromStartUm'] or 0.0),
    }


def _scanner(acq):
    """Scan timing for the scanner that was used.

    The file keeps a block per scanner type (galvo and resonant); the one matching
    `configuration/scannerType` is the one that acquired. Times are in ms — our reading, from
    `lineSpeed` × lines ≈ `frameSpeed` on both files; Evident does not label the unit.
    """
    kind = _text(acq, 'configuration/scannerType')
    timing = {}
    for s in acq.findall('scannerSettings'):
        if s.get('type') == kind:
            info = s.find('param/speed/speedInformation')
            pixel_ms = _num(_text(info, 'pixelSpeed'))
            timing = {
                'pixelDwellUs': None if pixel_ms is None else round(pixel_ms * 1000, 6),
                'lineTimeMs': _num(_text(info, 'lineSpeed')),
                'frameTimeMs': _num(_text(info, 'frameSpeed')),
            }
    integ = acq.find('imagingParam/method/integration')
    kalman = acq.find('imagingParam/productData/integration')
    return {
        'type': kind,
        'zoom': _num(_text(acq, 'imagingParam/area/zoom')),
        **timing,
        # `average="false"` read as summing the passes rather than averaging them — our reading of
        # the attribute name; it matters because a sum scales pixel values by the count.
        'integration': None if integ is None else {
            'type': _text(integ, 'type'), 'count': _num(_text(integ, 'count')),
            'average': None if integ.get('average') is None else integ.get('average') == 'true'},
        'kalman': None if kalman is None else {
            'average': _num(_text(kalman, 'averageKalmanCount')),
            'accumulate': _num(_text(kalman, 'accumulateKalmanCount'))},
    }


def _axes(acq):
    """Z-stack and time-lapse as set. Two ZSTACK axes are stored (two ways of defining a range);
    the one with `paramEnable="true"` is the one that ran — our reading, matching the stored image
    dimensions on both files."""
    out = {}
    for ax in acq.findall('imagingParam/axis'):
        name = _text(ax, 'axis')
        if ax.get('enable') != 'true':
            continue
        if name == 'ZSTACK' and ax.get('paramEnable') == 'true':
            out['zStack'] = {'stepUm': _num(_text(ax, 'step')), 'count': _num(_text(ax, 'maxSize'))}
        elif name == 'TIMELAPSE':
            out['timelapse'] = {'intervalS': _num(_text(ax, 'step')), 'count': _num(_text(ax, 'maxSize'))}
    return out


def _objective(acq):
    obj = acq.find('microscopeConfiguration/objectiveLens')
    if obj is None:
        return None
    return {
        'name': _text(obj, 'displayName') or _text(obj, 'name'),
        'magnification': _num(_text(obj, 'magnification')),
        'na': _num(_text(obj, 'naValue')),
        'immersion': (_text(obj, 'immersion') or '').lower() or None,
        'refractiveIndex': _num(_text(obj, 'refraction')),
    }


def parse_settings(xml_bytes):
    """Normalised settings dict from the `imageProperties` document."""
    root = _strip_namespaces(ET.fromstring(xml_bytes))
    acq = root.find('acquisition')
    if acq is None:
        return {'reason': 'no acquisition block in the settings document'}
    phases = acq.findall('phase')
    if not phases:
        return {'reason': 'no acquisition phase in the settings document'}
    phase = phases[0]
    lasers = _lasers(acq, phase, root)
    return {
        'source': 'oir',
        'readerVersion': READER_VERSION,
        'system': _text(root, 'system/systemName'),
        'softwareVersion': _text(root, 'system/systemVersion'),
        # As recorded, ISO 8601 with the scope PC's UTC offset.
        'acquiredAt': _text(root, 'general/creationDateTime'),
        'microscopeBody': _text(root, 'microscope/name'),
        'objective': _objective(acq),
        'lasers': [dict(v) for v in lasers.values()],
        'brightZ': _bright_z(acq, phase, lasers),
        'scanner': _scanner(acq),
        'autoHV': _bool(acq, 'imagingParam/pmtCommon/autoHV'),
        'pinholeUm': _num(_text(phase, 'productData/lightpath/pinholeDiameter')),
        'channels': _channels(acq, phase, lasers, _emission_filters(root, phase)),
        'dichroics': _dichroics(acq, phase),
        # Only the first phase is read. A multi-phase (sequential) acquisition says so here rather
        # than having its phases' channels and ramps merged into one.
        'phaseCount': len(phases),
        **_axes(acq),
    }


def acquisition_settings(path, series=0):
    """Settings for an `.oir`, or `{'reason': …}` when none can be read. Never raises. `series` is
    the reader contract (`acquisition_readers.json`); an `.oir` holds one image, so it is unused."""
    try:
        xml = read_settings_xml(path)
    except OSError as e:
        return {'reason': f'could not read the source file: {e}'}
    if xml is None:
        return {'reason': 'no settings document found in the file'}
    try:
        return parse_settings(xml)
    except ET.ParseError as e:
        return {'reason': f'settings document did not parse: {e}'}
