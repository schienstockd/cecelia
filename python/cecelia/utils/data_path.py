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
"""
import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.intensity_utils as intensity_utils
from cecelia.utils.dim_utils import DimUtils


def measure_store(im_path, log=None):
    """`{"channels": [...], "sampling": "full pass"}` for the store at `im_path`; one entry per channel
    with `intensity_utils.saturation_stats` + `index`, `containerBits`, `range` (`range_stats`),
    `floor` (`floor_stats`), `noise` (`noise_stats`) and `profile` (`PlaneProfile.summary`), plus
    `partialTimepoint` when the last timepoint stopped mid-volume (left out of every stat). Raises on a store that is not
    integer data (`channel_histograms`) — the caller decides whether that fails anything."""
    say = log or (lambda _m: None)
    dim_utils = DimUtils(ome_xml_utils.load_ome_xml(im_path))
    levels, _ = zarr_utils.open_as_zarr(im_path)
    level0 = levels[0]
    dim_utils.calc_image_dimensions(level0.shape)
    from_ome = {d: (dim_utils.dim_idx(d) if d in dim_utils.im_dim_order else None) for d in ('C', 'T', 'Z')}
    axes = intensity_utils.stack_axis_indices(zarr_utils.read_axes(im_path), level0.ndim, from_ome)
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
