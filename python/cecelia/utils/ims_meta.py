"""Read calibration out of an Imaris `.ims` that Bio-Formats does not put in the OME model.

`ImarisHDFReader` loads the file's timing into Bio-Formats' *original metadata* table — the
unstructured key/value dump ImageJ prints under "Show Info" — and never promotes it into the OME
model. Measured on a 180-timepoint Imaris 11 file:

    Pixels TimeIncrement = null
    PlaneCount           = 0          (so there is no per-plane DeltaT either)

while the same table shows `Time_Step = 30.000` and 180 wall-clock `TimePointN` stamps 30 s apart. So
bioformats2raw has nothing to write, the converted store carries no interval at all, and cecelia
correctly refuses to invent one (docs/OBJECTMODEL.md → *a `unit` on the t axis means the interval is
KNOWN*). The number is in the file; it just never reaches us through Bio-Formats. This reads it
directly, the same way `read_imagej_physical_size_run.py` re-derives Z spacing for ImageJ TIFFs.

Three sources, best first — see `time_increment`.
"""
import os
import re
from datetime import datetime

#: Fractional tolerance on the gap between consecutive timepoints. Acquisition clocks jitter by a
#: frame or two of scan time; a genuinely irregular series (a paused or resumed acquisition) is a
#: different animal and must NOT be flattened to its median — an interval is either a property of the
#: whole series or it is not something we can record.
UNIFORM_TOL = 0.05

#: Imaris stores `DataSetTimes/Time` Birth/Death in nanoseconds.
_NS_PER_S = 1e9

_TIMESTAMP_FORMATS = (
    '%Y-%m-%d %H:%M:%S.%f',
    '%Y-%m-%d %H:%M:%S',
)


def _h5py():
    """Imported lazily — the `cecelia` IO tier does not depend on h5py, and only Imaris needs it."""
    import h5py
    return h5py


def _attr(group, key):
    """An Imaris attribute as a string. They are stored as arrays of one-byte chars, not as strings."""
    if key not in group.attrs:
        return None
    v = group.attrs[key]
    if hasattr(v, 'tolist'):
        v = v.tolist()
        if isinstance(v, list):
            return b''.join(x if isinstance(x, bytes) else str(x).encode() for x in v).decode(
                'utf-8', 'replace')
        if isinstance(v, bytes):
            return v.decode('utf-8', 'replace')
    return str(v)


def summarise_gaps(gaps, tol=UNIFORM_TOL):
    """`(interval, uniform, spread)` for a list of consecutive gaps in seconds.

    `interval` is the median, `spread` the largest fractional deviation from it. A caller must only
    record the interval when `uniform` — see `UNIFORM_TOL`.
    """
    if not gaps:
        return None, False, None
    ordered = sorted(gaps)
    n = len(ordered)
    median = ordered[n // 2] if n % 2 else (ordered[n // 2 - 1] + ordered[n // 2]) / 2
    if median <= 0:
        return None, False, None
    spread = max(abs(g - median) for g in gaps) / median
    return median, spread <= tol, spread


def _from_dataset_times(f):
    """`DataSetTimes/Time` — a per-timepoint record whose `Birth` is nanoseconds from the start.

    The best source: numeric, exact, and written for every Imaris timelapse regardless of the
    acquisition software that fed the converter.
    """
    ds = f.get('DataSetTimes/Time')
    if ds is None or len(ds) < 2:
        return None
    names = getattr(ds.dtype, 'names', None) or ()
    if 'Birth' not in names:
        return None
    births = [float(row['Birth']) / _NS_PER_S for row in ds[...]]
    return [births[i + 1] - births[i] for i in range(len(births) - 1)]


def _from_timepoint_stamps(f):
    """`DataSetInfo/TimeInfo/TimePointN` — wall-clock stamps, one per timepoint (1-based)."""
    ti = f.get('DataSetInfo/TimeInfo')
    if ti is None:
        return None
    idx = sorted(int(m.group(1)) for m in
                 (re.fullmatch(r'TimePoint(\d+)', k) for k in ti.attrs) if m)
    if len(idx) < 2:
        return None
    stamps = []
    for i in idx:
        raw = _attr(ti, f'TimePoint{i}')
        # `%f` takes at most six fractional digits; Imaris writes nine for a file converted from a
        # Zeiss .lsm (`12:20:54.742000000`). Nanoseconds are far below any frame interval.
        raw = re.sub(r'(\.\d{6})\d+$', r'\1', raw.strip()) if isinstance(raw, str) else raw
        for fmt in _TIMESTAMP_FORMATS:
            try:
                stamps.append(datetime.strptime(raw.strip(), fmt))
                break
            except (ValueError, AttributeError):
                continue
        else:
            return None                      # one unparseable stamp invalidates the series
    return [(stamps[i + 1] - stamps[i]).total_seconds() for i in range(len(stamps) - 1)]


# The NOMINAL interval a vendor's software was set to, in seconds, by the group Imaris copies it into:
# Olympus `.oir` and Zeiss `.lsm` sources. Last resort: what was requested, not what happened.
_NOMINAL_STEPS = (('DataSetInfo/OIR Dataset Size', 'Time Step'),
                  ('DataSetInfo/LSM Dataset Size', 'Time Interval [s]'))


def _from_nominal_step(f, group, key):
    g = f.get(group)
    if g is None:
        return None
    raw = _attr(g, key)
    try:
        step = float(raw)
    except (TypeError, ValueError):
        return None
    return step if step > 0 else None


def time_increment(path, tol=UNIFORM_TOL):
    """Frame interval of an Imaris timelapse, in seconds.

    Returns a dict — always with `source` and, when the interval could be established,
    `TimeIncrement`. **Always seconds**, and deliberately WITHOUT a unit field: the unit belongs to
    the tier that stores the value, and the two tiers spell it differently (ccid/NGFF want the UDUNITS
    name `second`, OME-XML wants the symbol `s`). The Julia caller stamps the ccid unit, exactly as it
    does for the `_delta_t_fallback` path, and `sync_zarr_calibration!` converts at the OME-XML
    boundary. Sources in order:

      1. `DataSetTimes/Time` `Birth` (nanoseconds) — numeric and exact
      2. `DataSetInfo/TimeInfo/TimePointN` wall-clock stamps — millisecond resolution
      3. `DataSetInfo/OIR Dataset Size/Time Step` (Olympus) or `LSM Dataset Size/Time Interval [s]`
         (Zeiss) — NOMINAL

    1 and 2 are measured, so they are checked for uniformity and yield nothing when the series is
    irregular; `spread` reports how irregular. 3 is a single declared number with nothing to check.
    """
    h5py = _h5py()
    if not os.path.isfile(path):
        return {'source': None, 'reason': 'file not found'}
    try:
        with h5py.File(path, 'r') as f:
            for name, fn in (('DataSetTimes/Time', _from_dataset_times),
                             ('DataSetInfo/TimeInfo', _from_timepoint_stamps)):
                try:
                    gaps = fn(f)
                except (KeyError, TypeError, ValueError):
                    continue
                if not gaps:
                    continue
                interval, uniform, spread = summarise_gaps(gaps, tol=tol)
                if interval is None:
                    continue
                if not uniform:
                    # Deliberately not the median: an irregular series has no one interval, and a
                    # plausible number here would be indistinguishable from a measured one downstream.
                    return {'source': name, 'frames': len(gaps) + 1, 'uniform': False,
                            'spread': spread, 'median': interval,
                            'reason': f'gaps vary by {spread:.1%} — no single interval'}
                return {'TimeIncrement': interval, 'source': name,
                        'frames': len(gaps) + 1, 'uniform': True, 'spread': spread}

            for group, key in _NOMINAL_STEPS:
                step = _from_nominal_step(f, group, key)
                if step is not None:
                    return {'TimeIncrement': step, 'source': group, 'nominal': True}
    except OSError as e:
        return {'source': None, 'reason': f'could not read: {e}'}
    return {'source': None, 'reason': 'no timing in the file'}


_UNIT_TO_UM = {'um': 1.0, 'µm': 1.0, 'micron': 1.0, 'nm': 1e-3, 'mm': 1e3, 'm': 1e6}


def voxel_size(path):
    """Voxel size of an Imaris file as `[x, y, z]` in µm, from `DataSetInfo/Image`.

    Imaris stores no voxel size, only the physical extent (`ExtMin0..2`, `ExtMax0..2`) and the voxel
    count (`X`, `Y`, `Z`); the size is extent / count — the same derivation Bio-Formats' reader makes,
    so this agrees with what the import writes. Used to cross-check calibration a third party
    published about the file (Immunemap's `spacing`) against the file itself. Returns
    `{'voxelSizeXYZ': [...], 'unit': 'µm'}`, or `{'voxelSizeXYZ': None, 'reason': ...}`.
    """
    h5py = _h5py()
    if not os.path.isfile(path):
        return {'voxelSizeXYZ': None, 'reason': 'file not found'}
    try:
        with h5py.File(path, 'r') as f:
            g = f['DataSetInfo/Image']
            unit = (_attr(g, 'Unit') or 'um').strip()
            scale = _UNIT_TO_UM.get(unit)
            if scale is None:
                return {'voxelSizeXYZ': None, 'reason': f'unknown unit {unit!r}'}
            out = []
            for i, n_key in enumerate(('X', 'Y', 'Z')):
                lo, hi, n = (_attr(g, f'ExtMin{i}'), _attr(g, f'ExtMax{i}'), _attr(g, n_key))
                out.append((float(hi) - float(lo)) / int(n) * scale)
    except (KeyError, TypeError, ValueError, ZeroDivisionError) as e:
        return {'voxelSizeXYZ': None, 'reason': f'no extent in DataSetInfo/Image: {e}'}
    except OSError as e:
        return {'voxelSizeXYZ': None, 'reason': f'could not read: {e}'}
    return {'voxelSizeXYZ': out, 'unit': 'µm'}


def _num(s):
    try:
        return float(str(s).strip())
    except (TypeError, ValueError):
        return None


def _objective(text, na):
    """`{name, magnification, na}` from an objective string like `Plan-Neofluar 10x/0.3`."""
    if not text:
        return None
    m = re.search(r'(\d+(?:\.\d+)?)\s*[xX×]\s*/\s*(\d+(?:\.\d+)?)', text)
    return {'name': text.strip(),
            'magnification': float(m.group(1)) if m else None,
            'na': na if na is not None else (float(m.group(2)) if m else None),
            'immersion': None}


def acquisition_settings(path):
    """Acquisition settings an Imaris file records, normalised like `oir_meta` / `czi_meta`.

    Imaris copies the source microscope's metadata into `DataSetInfo`: the objective and NA
    (`Image`), per channel the excitation wavelength, emission filter and pinhole (`Channel N`,
    `LSM*` keys), the vendor (`Manufactor Information`) and the planned z-stack and time-lapse
    (`LSM Dataset Size`). Only those are read. The Zeiss `Subblock*` groups carry lasers and detector
    gains too, but which entries were active is encoded in flags whose meaning is unverified
    (`Acquire: 0` on the laser that took the image), so they are left alone rather than guessed.
    Returns `{'reason': …}` when there is nothing to read. Never raises.
    """
    if not os.path.isfile(path):
        return {'reason': 'file not found'}
    h5py = _h5py()
    try:
        with h5py.File(path, 'r') as f:
            info = f.get('DataSetInfo')
            if info is None:
                return {'reason': 'no DataSetInfo in the file'}
            img = info.get('Image')
            obj_text = na = when = None
            if img is not None:
                obj_text = _attr(img, 'Objective') or (_attr(img, 'Description') or '').replace(
                    'Objective:', '').strip() or None
                na = _num(_attr(img, 'NumericalAperture'))
                when = _attr(img, 'RecordingDate')
            chans = []
            for key in sorted((k for k in info if re.fullmatch(r'Channel \d+', k)),
                              key=lambda k: int(k.split()[1])):
                g = info[key]
                obj_text = obj_text or _attr(g, 'Objective')
                ex = _num(_attr(g, 'LSMExcitationWavelength'))
                pin = _num(_attr(g, 'LSMPinhole'))
                chans.append({
                    'order': int(key.split()[1]) + 1,
                    'name': _attr(g, 'Name'),
                    'dyePreset': None,
                    'laser': f'{ex:g} nm' if ex else None,
                    'detector': None, 'hv': None, 'gain': None, 'offset': None,
                    'emissionFilter': (_attr(g, 'LSMEmissionWavelength') or '').strip() or None,
                    'pinholeUm': pin if pin else None,    # 0 = none (non-descanned detection)
                })
            maker = info.get('Manufactor Information')
            system = None
            if maker is not None:
                parts = [(_attr(maker, k) or '').strip() for k in ('Manufactor String', 'Manufactor Model')]
                system = ' '.join(p for p in parts if p and p != 'not known') or None
            out = {
                'source': 'ims',
                'system': system,
                'softwareVersion': None,
                'acquiredAt': when.strip().replace(' ', 'T', 1)[:19] if when else None,
                'objective': _objective(obj_text, na),
                'lasers': [{'name': f'{w:g} nm', 'wavelengthNm': w, 'transmissionPct': None}
                           for w in dict.fromkeys(_num(_attr(info[k], 'LSMExcitationWavelength'))
                                                  for k in info if re.fullmatch(r'Channel \d+', k))
                           if w],
                'channels': chans,
            }
            size = info.get('LSM Dataset Size')
            if size is not None:
                nz = _num(_attr(size, 'Number of Z-Slices (Dimension Z)'))
                nt = _num(_attr(size, 'Number of Timepoints (Dimension T)'))
                if nz and nz > 1:
                    out['zStack'] = {'stepUm': _num(_attr(size, 'Voxelsize Z [um]')), 'count': int(nz)}
                if nt and nt > 1:
                    out['timelapse'] = {'intervalS': _num(_attr(size, 'Time Interval [s]')),
                                        'count': int(nt)}
    except OSError as e:
        return {'reason': f'could not read: {e}'}
    if not out['channels'] and out['objective'] is None and out['system'] is None:
        return {'reason': 'no acquisition metadata in DataSetInfo'}
    return out
