"""
Detect channels that CLIPPED AT ACQUISITION, from the store the import just wrote.

Runs on every import. A clipped channel has lost information the pipeline cannot recover — no
correction, threshold or rescale puts it back — so the only useful moment to say so is at import,
while the answer is still "re-acquire with less gain" rather than "re-do the analysis".

Detection is `intensity_utils.is_saturated`: structural (a pile-up in the brightest occupied bin),
so it does not need to know the detector's bit depth. That matters here — measured on the nine
`kSUFux` movies, the sensor clips at 4095 inside 16-bit words, so the obvious test (fraction of
voxels at the dtype maximum) reports zero on data that is visibly clipped. See that docstring.

Cost: one streamed pass over the store, ~3 s/GB measured (5.3 s for a 1.74 GB store). Deliberately a
FULL pass rather than a strided one — the threshold is a voxel count, so subsampling scales the
pile-up down with it and flips marginal channels.

Parameter contract (JSON written by Julia):
  imPath     - absolute path to the .ome.zarr the import just wrote
  resultPath - absolute path to write the result JSON to

Result JSON: {"channels": [{"index", "saturated", "topValue", "topCount", "topFrac", "profile"}, …]}
(`profile` = `intensity_utils.PlaneProfile.summary`: per-depth and per-frame numbers), plus
"partialTimepoint": {"t", "emptyZ", "sizeZ"} when the acquisition stopped mid-volume (see
`intensity_utils.partial_last_timepoint`) — the Julia handler turns this into ccid meta + QC findings
and, unless the import keeps it, trims that timepoint. `{}` when the store cannot be read as integer data
(nothing to say, and a QC pass must never fail an otherwise-good import).
"""

import numpy as np

import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.intensity_utils as intensity_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.atomic_io import write_json_atomic
import cecelia.utils.ome_xml_utils as ome_xml_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    result_path = params['resultPath']
    result = {}

    log.progress(0, 2)
    try:
        dim_utils = DimUtils(ome_xml_utils.load_ome_xml(im_path))
        levels, _ = zarr_utils.open_as_zarr(im_path)
        level0 = levels[0]
        dim_utils.calc_image_dimensions(level0.shape)
        from_ome = {d: (dim_utils.dim_idx(d) if d in dim_utils.im_dim_order else None)
                    for d in ('C', 'T', 'Z')}
        axes = intensity_utils.stack_axis_indices(zarr_utils.read_axes(im_path), level0.ndim, from_ome)
        c_idx = axes['C']

        log.log(f'>> checking {level0.dtype} {level0.shape} for clipping at acquisition')
        log.progress(1, 2)
        # One pass feeds three things: the per-plane profile, plane occupancy (does each plane hold a
        # non-zero voxel — the stopped-acquisition check below) and the last timepoint's histograms.
        # If that timepoint turns out incomplete, its black planes are not acquisition data and come
        # out of the clipping/sparsity stats and the profile — kept or trimmed, they would otherwise
        # read as zero voxels the detector recorded.
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

        channels = []
        for i, h in enumerate(hists):
            stats = intensity_utils.saturation_stats(h)
            stats['index'] = i
            # Where in the stack: per depth / per frame percentiles, zeros and ceiling voxels.
            stats['profile'] = profile.summary(i, intensity_utils.profile_ceiling(stats))
            channels.append(stats)
            log.log(f'   ch{i}: {"SATURATED" if stats["saturated"] else "ok"} '
                    f'(top occupied value {stats["topValue"]}, {stats["topCount"]} voxels '
                    f'= {stats["clippedSignalFrac"] * 100:.4f}% of signal)')
        result = {'channels': channels}
        if partial is not None:
            result['partialTimepoint'] = partial
            log.log(f'>> [WARN] last timepoint (t{partial["t"]}) is incomplete: '
                    f'{len(partial["emptyZ"])} of {partial["sizeZ"]} z-planes empty in every channel '
                    '(acquisition stopped mid-volume); the stats above leave it out')
    except Exception as e:
        # Advisory QC on a store that already converted successfully: report and move on. A
        # non-integer dtype (channel_histograms raises) or an unreadable OME is not an import failure.
        log.log(f'>> [WARN] could not check for saturation: {e}')

    write_json_atomic(result_path, result)
    log.progress(2, 2)


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


def main():
    params = script_utils.script_params()
    if params is None:
        return
    run(params)


if __name__ == '__main__':
    main()
