"""
What landed in the file: per-channel pixel statistics of an OME-ZARR store, from one streamed pass.

The import probe (`app/src/tasks/importImages/saturation_run.py`) calls `measure_store` on the store it
just wrote, and the result becomes ccid `meta["saturation"]` — the clipping verdict, the range the
channel uses, what its zeros are, the noise floor, and per-depth / per-frame numbers. Headless on
purpose: a path in, a dict out, no project, so it can be run on any store from a shell
(`docs/todo/ACQUISITION_ADVISOR_PLAN.md` → P4b).

Every number is computed from integer histograms (`intensity_utils`), so a full pass costs one read of
the store (~3 s/GB) and no subsampling: the clipping test counts voxels, and a strided pass would scale
the pile-up down with it.

Drift is NOT part of the import probe: through drift correction's own estimator it is ~19 min on a
47 GB movie (5x the rest of the probe together), it feeds only the acquisition check, and drift
correction re-estimates it anyway. `measure_store(drift=True)` or `measure_drift` on an already-probed
store add it on request (the acquisition check's "Measure N movies", `importImages.measureSignal`).
"""
import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.intensity_utils as intensity_utils
import cecelia.utils.photon_utils as photon_utils
from cecelia.utils.dim_utils import DimUtils

#: Timepoints the signal pass (`measure_signal`) samples, evenly spread; all of them when fewer.
SIGNAL_SAMPLE_T = 4
#: Z planes per sampled timepoint, evenly spread; all of them when fewer.
SIGNAL_SAMPLE_Z = 16
#: Voxels per plane kept for the object and shared-structure statistics (same positions in every channel).
SIGNAL_SAMPLE_VOXELS = 20000


def _open_store(im_path):
    """`(level0, axes, dim_utils)` for the store at `im_path`: its full-resolution level, the C/T/Z axis
    indices (`intensity_utils.stack_axis_indices`) and its dims from the OME-XML."""
    dim_utils = DimUtils(ome_xml_utils.load_ome_xml(im_path))
    levels, _ = zarr_utils.open_as_zarr(im_path)
    level0 = levels[0]
    dim_utils.calc_image_dimensions(level0.shape)
    from_ome = {d: (dim_utils.dim_idx(d) if d in dim_utils.im_dim_order else None) for d in ('C', 'T', 'Z')}
    axes = intensity_utils.stack_axis_indices(zarr_utils.read_axes(im_path), level0.ndim, from_ome)
    return level0, axes, dim_utils


def measure_store(im_path, log=None, drift=False):
    """`{"channels": [...], "sampling": "full pass"}` for the store at `im_path`; one entry per channel
    with `intensity_utils.saturation_stats` + `index`, `containerBits`, `range` (`range_stats`),
    `floor` (`floor_stats`), `noise` (`noise_stats`) and `profile` (`PlaneProfile.summary`), plus
    `partialTimepoint` when the last timepoint stopped mid-volume (left out of every stat), plus the
    signal pass (`measure_signal`): a `photons` entry per channel and top-level `sharedStructure` —
    and `drift` only with ``drift=True`` (see the module docstring). Raises on a store that is not
    integer data (`channel_histograms`) — the caller decides whether that fails anything."""
    say = log or (lambda _m: None)
    level0, axes, dim_utils = _open_store(im_path)
    c_idx = axes['C']

    say(f'>> measuring {level0.dtype} {level0.shape}: clipping, range, noise, per plane')
    # One pass feeds three things: the per-plane profile, plane occupancy (does each plane hold a
    # non-zero voxel — the stopped-acquisition check) and the last timepoint's histograms. If that
    # timepoint turns out incomplete, its black planes are not acquisition data and come out of every
    # stat below and the profile — kept or trimmed, they would otherwise read as zero voxels the
    # detector recorded.
    lead = [i for i in range(level0.ndim - 2) if i != c_idx]
    profile = intensity_utils.PlaneProfile(lead, t_axis=axes['T'], z_axis=axes['Z'])
    occupied = np.zeros([level0.shape[i] for i in lead], dtype=bool)
    t_pos = lead.index(axes['T']) if axes['T'] in lead else None
    last_t = level0.shape[axes['T']] - 1 if t_pos is not None else None
    last_hists = {}

    def _per_plane(c, ix, h):
        profile(c, ix, h)
        if not occupied[ix]:
            occupied[ix] = bool(h[1:].any())
        if t_pos is not None and ix[t_pos] == last_t:
            last_hists[c] = last_hists.get(c, 0) + h

    hists = intensity_utils.channel_histograms(level0, c_idx, on_plane=_per_plane)
    partial = _partial_timepoint(occupied, lead, axes)
    if partial is not None:
        hists = [h - last_hists.get(c, 0) for c, h in enumerate(hists)]
        profile.drop_t(partial['t'])

    bits = int(np.dtype(level0.dtype).itemsize * 8)
    channels = []
    for i, h in enumerate(hists):
        stats = intensity_utils.saturation_stats(h)
        stats['index'] = i
        stats['containerBits'] = bits
        stats['range'] = intensity_utils.range_stats(h)
        stats['floor'] = intensity_utils.floor_stats(h)
        stats['noise'] = intensity_utils.noise_stats(h, floor=stats['floor'])
        stats['profile'] = profile.summary(i, intensity_utils.profile_ceiling(stats))
        channels.append(stats)
        say(f'   ch{i}: {"SATURATED" if stats["saturated"] else "ok"} '
            f'(top occupied value {stats["topValue"]}, {stats["topCount"]} voxels '
            f'= {stats["clippedSignalFrac"] * 100:.4f}% of signal)')
    out = {'channels': channels, 'sampling': 'full pass'}
    skip_t = partial['t'] if partial is not None else None
    out.update(measure_signal(level0, axes, dim_utils, channels, skip_t=skip_t, log=say, drift=drift))
    if partial is not None:
        out['partialTimepoint'] = partial
        say(f'>> [WARN] last timepoint (t{partial["t"]}) is incomplete: '
            f'{len(partial["emptyZ"])} of {partial["sizeZ"]} z-planes empty in every channel '
            '(acquisition stopped mid-volume); the stats above leave it out')
    return out


def _partial_timepoint(occupied, lead, axes):
    """`intensity_utils.partial_last_timepoint` on the occupancy array, reshaped to `[T, Z]` (Z of 1 for
    a 2-D timelapse). `axes` is the store's own `stack_axis_indices`, like the profile's. None without
    a time axis."""
    if axes['T'] not in lead:
        return None
    t_pos = lead.index(axes['T'])
    z_pos = lead.index(axes['Z']) if axes['Z'] in lead else None
    occ = np.moveaxis(occupied, t_pos, 0)
    if z_pos is None:
        occ = occ.reshape(occ.shape[0], -1).any(axis=1)[:, None]
    else:
        z_pos = z_pos if z_pos > t_pos else z_pos + 1      # its position after T moved to the front
        occ = np.moveaxis(occ, z_pos, 1)
        occ = occ.reshape(occ.shape[0], occ.shape[1], -1).any(axis=2)
    return intensity_utils.partial_last_timepoint(occ)


def _sample_positions(n, k, seed):
    rng = np.random.default_rng(seed)
    return rng.choice(n, size=min(k, n), replace=False)


def measure_signal(level0, axes, dim_utils, channels, skip_t=None, log=None, drift=False):
    """The test-acquisition checks that need where a voxel sits, from a SAMPLE of planes
    (`docs/todo/ACQUISITION_TEST_CHECK_PLAN.md`): `SIGNAL_SAMPLE_T` timepoints x `SIGNAL_SAMPLE_Z`
    planes, every channel read at the same planes and positions.

    Adds ``photons`` to each entry of ``channels`` (this pass's per-channel result, from
    `measure_store`): ``{fit: photon_utils.fit_photon_transfer, objects: photon_utils.object_photons}``,
    offset at the channel's background median and windows at its ceiling dropped when it clipped.
    Returns ``{sharedStructure: photon_utils.shared_structure}``, plus ``drift`` with ``drift=True``
    (`_movie_drift`: ``None`` for a still image).
    A sample suffices: none of these is a voxel count (clipping is, and stays on the full pass).
    ``skip_t`` is a timepoint that is not acquisition data (a stopped acquisition's last frame).
    """
    say = log or (lambda _m: None)
    c_idx, t_idx, z_idx = axes['C'], axes['T'], axes['Z']
    def spread(axis, k):
        if axis is None:
            return [None]
        n = level0.shape[axis]
        idx = sorted(set(np.linspace(0, n - 1, min(k, n)).round().astype(int).tolist()))
        return [i for i in idx if not (axis == t_idx and i == skip_t)]
    ts, zs = spread(t_idx, SIGNAL_SAMPLE_T), spread(z_idx, SIGNAL_SAMPLE_Z)
    n_c = level0.shape[c_idx] if c_idx is not None else 1
    say(f'>> signal pass: {len(ts)} timepoint(s) x {len(zs)} plane(s): photons, objects, shared structure')

    ceil = {i: (ch['topValue'] if ch.get('saturated') else None) for i, ch in enumerate(channels)}
    # a photon-counted channel's zeros are photon-free voxels: data for the fit, and its zero-photon
    # level is 0 itself (its background median is untestable, most voxels being 0)
    counted = {i: (ch.get('floor') or {}).get('kind') == 'photon-counted' for i, ch in enumerate(channels)}
    win_mu = {c: [] for c in range(n_c)}
    win_var = {c: [] for c in range(n_c)}
    smooth = {c: [] for c in range(n_c)}
    struct = {c: [] for c in range(n_c)}
    raw = {c: [] for c in range(n_c)}
    seed = 0
    for t in ts:
        for z in zs:
            planes = []
            for c in range(n_c):
                sl = [0] * (level0.ndim - 2) + [slice(None), slice(None)]
                for ax, v in ((c_idx, c), (t_idx, t), (z_idx, z)):
                    if ax is not None:
                        sl[ax] = v
                planes.append(np.asarray(level0[tuple(sl)]))
            pos = _sample_positions(planes[0].size, SIGNAL_SAMPLE_VOXELS, seed)
            seed += 1
            for c, p in enumerate(planes):
                m, v = photon_utils.window_stats(p, ceil.get(c), drop_zeros=not counted.get(c, False))
                win_mu[c].append(m)
                win_var[c].append(v)
                smooth[c].append(photon_utils.smooth_plane(p).ravel()[pos])
                struct[c].append(photon_utils.structure_plane(p).ravel()[pos])
                raw[c].append(p.ravel()[pos])

    for c in range(n_c):
        offset = (0.0 if counted.get(c) else
                  (channels[c].get('noise') or {}).get('bgMedian') if c < len(channels) else None)
        fit = photon_utils.fit_photon_transfer(np.concatenate(win_mu[c]), np.concatenate(win_var[c]),
                                               offset=offset)
        objects = photon_utils.object_photons(np.concatenate(smooth[c]), fit, ceiling=ceil.get(c),
                                              raw=np.concatenate(raw[c]))
        if c < len(channels):
            channels[c]['photons'] = {'fit': fit, 'objects': objects}
        g = fit['greyPerPhoton']
        say(f'   ch{c}: ' + (f'{g:.1f} grey levels per photon'
                             + (f', ceiling = {objects["ceilingPhotons"]:.1f} photons'
                                if objects and objects.get('ceilingPhotons') is not None else '')
                             + (f', objects p50 {objects["p50Photons"]:.1f} photons'
                                if objects and objects.get('p50Photons') is not None else '')
                             if g is not None else f'photons untestable ({fit["untestable"]})'))
    out = {'sharedStructure': photon_utils.shared_structure(struct)}
    if drift:
        out['drift'] = _movie_drift(level0, axes, dim_utils, channels, say, skip_t=skip_t)
    return out


def _movie_drift(level0, axes, dim_utils, channels, say, skip_t=None):
    """`_drift_check` on a movie of more than two (acquired) timepoints; ``None`` otherwise — nothing
    to register, which is an answer, not a missing measurement."""
    t_idx = axes['T']
    if t_idx is None or level0.shape[t_idx] - (skip_t is not None) <= 2:
        return None
    return _drift_check(level0, dim_utils, channels, t_idx, axes['C'], say, skip_t=skip_t)


def measure_drift(im_path, channels, skip_t=None, log=None):
    """`{"drift": ...}` for an already-probed store: the drift `measure_store(drift=True)` would add,
    without the full pass and signal pass again. ``channels`` is the stored probe's per-channel list
    (its ``photons`` pick the channel registration locks onto); ``skip_t`` the stored partial last
    timepoint, if any."""
    say = log or (lambda _m: None)
    level0, axes, dim_utils = _open_store(im_path)
    say(f'>> drift: {level0.dtype} {level0.shape}')
    return {'drift': _movie_drift(level0, axes, dim_utils, channels, say, skip_t=skip_t)}


class _FirstFrames:
    """A read-only view of a level's first `n` timepoints, so a stopped acquisition's last frame can be
    left out of a whole-movie estimator without reading or copying the level. Indexing passes straight
    through; only `shape` (what the estimator sizes itself by) changes."""

    def __init__(self, level, t_axis, n):
        self._level, self.dtype, self.ndim = level, level.dtype, level.ndim
        self.shape = tuple(n if i == t_axis else s for i, s in enumerate(level.shape))

    def __getitem__(self, key):
        return self._level[key]


def _drift_check(level0, dim_utils, channels, t_idx, c_idx, say, skip_t=None):
    """Drift of the whole movie through drift correction's own estimator
    (`correction_utils.estimate_drift`, its defaults), on the channel with the most photons on its
    objects — the one registration would lock onto best. Numbers only: the verdict (residual over
    drift correction's `DRIFT_RESIDUAL_WARN_PX`) is the Julia QC's, as for the drift task.
    ``skip_t`` (a stopped acquisition's last frame, always the last) is left out, as from every other
    stat: its black planes would register as movement."""
    import copy
    from cecelia.utils import correction_utils
    if skip_t is not None:
        level0 = _FirstFrames(level0, t_idx, skip_t)
        if dim_utils is not None:
            # the same dims with T one shorter (`calc_image_dimensions` matches the OME's own sizes,
            # so it cannot be re-run on the shortened shape)
            dim_utils = copy.deepcopy(dim_utils)
            dim_utils.im_dim = list(dim_utils.im_dim)
            dim_utils.im_dim[dim_utils.dim_idx('T')] = skip_t
    def score(i):
        obj = ((channels[i].get('photons') or {}).get('objects') or {})
        return obj.get('p50Photons') or 0.0
    ch = max(range(len(channels)), key=score) if channels else 0
    try:
        est = correction_utils.estimate_drift(level0, ch, dim_utils, time_idx=t_idx, channel_idx=c_idx)
    except Exception as e:                                 # advisory: never fails the probe
        say(f'   drift: not measured ({e})')
        return {'channel': int(ch), 'untestable': str(e)}
    pos = np.asarray(est.positions, dtype=np.float64)
    steps = np.linalg.norm(np.diff(pos, axis=0), axis=1) if len(pos) > 1 else np.zeros(0)
    out = {'channel': int(ch), 'axes': list(est.axes),
           'excursionPx': [round(float(v), 2) for v in np.ptp(pos, axis=0)],
           'maxStepPx': round(float(steps.max()), 2) if steps.size else 0.0,
           'residualRmsPx': None if est.residual_rms is None else round(float(est.residual_rms), 3),
           'residualP90Px': None if est.residual_p90 is None else round(float(est.residual_p90), 3),
           'interpolatedFrames': len(est.interpolated)}
    say(f'   drift (ch{ch}): excursion {dict(zip(out["axes"], out["excursionPx"]))} px, '
        f'residual RMS {out["residualRmsPx"]} px')
    return out

