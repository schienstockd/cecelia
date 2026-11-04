"""
What landed in the file: per-channel pixel statistics of an OME-ZARR store, from one streamed pass.

The import probe (`app/src/tasks/importImages/saturation_run.py`) calls `measure_store` on the store it
just wrote, and the result becomes ccid `meta["saturation"]` — the clipping verdict, the range the
channel uses, the noise floor, and per-depth / per-frame numbers. Headless on purpose: a path in, a
dict out, no project, so it can be run on any store from a shell
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
    `noise` (`noise_stats`) and `profile` (`PlaneProfile.summary`). Raises on a store that is not
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
    lead = [i for i in range(level0.ndim - 2) if i != c_idx]
    profile = intensity_utils.PlaneProfile(lead, t_axis=axes['T'], z_axis=axes['Z'])
    hists = intensity_utils.channel_histograms(level0, c_idx, on_plane=profile)

    bits = int(np.dtype(level0.dtype).itemsize * 8)
    channels = []
    for i, h in enumerate(hists):
        stats = intensity_utils.saturation_stats(h)
        stats['index'] = i
        stats['containerBits'] = bits
        stats['range'] = intensity_utils.range_stats(h)
        stats['noise'] = intensity_utils.noise_stats(h)
        stats['profile'] = profile.summary(i, intensity_utils.profile_ceiling(stats))
        channels.append(stats)
        say(f'   ch{i}: {"SATURATED" if stats["saturated"] else "ok"} '
            f'(top occupied value {stats["topValue"]}, {stats["topCount"]} voxels '
            f'= {stats["clippedSignalFrac"] * 100:.4f}% of signal)')
    return {'channels': channels, 'sampling': 'full pass'}
