"""Example PLUGIN Python compute — attach externally-tracked spots to this segmentation.

Launched by the `tracking.importCsvTracks` Julia task through `run_py`. Two imports matter here and
they resolve by different mechanisms:

  from track_readers import read_track_file        <- the PLUGIN's own python/ dir, on PYTHONPATH
  from cecelia.utils.label_props_utils import …    <- the cecelia IO library, also on PYTHONPATH

Both come from `run_py`; there is no `sys.path` bootstrapping in a runner, ever.
See docs/CUSTOM_MODULES.md and docs/todo/PLUGINS_PLAN.md.

Parameter contract (JSON written by the Julia task):
  labelPropsPath - absolute path to the segmentation's labelProps .h5ad
  csvPath        - the external track export (TrackMate track XML, or a delimited table)
  mapping        - RESOLVED column mapping, ignored for XML (it has no columns):
                   trackColumn / frameColumn / xColumn / yColumn / zColumn / frameBase / skipRows,
                   plus spotUnit = "physical" (calibrated, µm) or "pixel"
  delimiter      - sniffed ONCE Julia-side and passed; this side never guesses
  physicalSizesZYX - Z,Y,X µm per pixel from ccid.json; reversed here to match x,y,z columns
  maxDistance    - match cutoff, in PIXELS
  outColumn      - obs column to write; always , the one name downstream tasks read
"""
import numpy as np
import pandas as pd

import cecelia.utils.script_utils as script_utils
from cecelia.utils.label_props_utils import LabelPropsView

from track_readers import read_track_file, match_spots_to_cells  # the plugin's own shared helper


def run(params):
    log     = script_utils.get_logfile_utils(params)
    path    = params['labelPropsPath']
    out_col = params.get('outColumn', 'track_id')   # the canonical column; see the .jl
    max_d   = float(params.get('maxDistance', 10.0))
    # The mapping arrives ALREADY RESOLVED (template + the user's overrides merged Julia-side), so this
    # runner knows nothing about templates or tool names — it just reads the columns it is told to.
    mp = params.get('mapping', {})

    log.progress(0, 2)
    # ONE entry point for every format: the reader decides from the file itself (TrackMate's track
    # XML has no columns to map), so this runner never branches on format.
    mp = dict(mp); mp['delimiter'] = params.get('delimiter', ',')
    tracks, frames, pos = read_track_file(params['csvPath'], mp)
    log.log(f'[INFO] Read {len(tracks)} tracked spots in {len(np.unique(tracks))} tracks')
    log.progress(1, 2)

    view = LabelPropsView(path)
    labels = view.labels()
    # Centroids come back in the segmentation's own axis order; ask for x/y/z explicitly so the spot
    # columns and the cell columns are in the SAME order — getting this wrong swaps axes and every
    # distance is quietly meaningless.
    cent_cols = view.centroid_columns(order=['x', 'y', 'z'])
    tcols = view.temporal_columns()
    # view_centroid_cols() is the accessor for these: centroids and centroid_t live in OBS, and
    # view_cols() selects from the measure (var) columns only — asking it for centroids silently
    # returns a frame with just `label`, and every downstream distance would then be meaningless.
    df = view.view_centroid_cols().as_df()

    cell_pos = df[cent_cols].to_numpy(dtype=float)
    cell_frames = (df[tcols[0]].to_numpy(dtype=float).round().astype(int)
                   if tcols else np.zeros(len(df), dtype=int))

    # External tools write positions in PHYSICAL units when the image is calibrated; cecelia centroids are
    # in pixels. Convert the spots rather than the cells, so `maxDistance` stays in pixels — the unit
    # the user can actually judge against a segmentation.
    pos = pos[:, :cell_pos.shape[1]]
    cell_pos = cell_pos[:, :pos.shape[1]]        # a 2D export matches on x/y even for a 3D image
    if str(mp.get('spotUnit', 'physical')) == 'physical':
        # `physicalSizesZYX` is Z, Y, X — the codebase convention (cf. scale_centroids(df, sizes_zyx)).
        # Our columns are X, Y, Z, so this MUST be reversed. Getting it backwards divides x by the z
        # spacing: on anisotropic data every distance is wrong by the aspect ratio and nothing matches,
        # which is precisely what maxDistance is here to turn into a loud failure rather than a guess.
        zyx = np.asarray(params.get('physicalSizesZYX', [1.0, 1.0, 1.0]), dtype=float)
        sizes = np.where(zyx > 0, zyx, 1.0)[::-1][:pos.shape[1]]
        pos = pos / sizes
        log.log(f'[INFO] Converted spot positions to pixels using (x,y,z) {list(sizes)} µm/px')

    track_of_cell, matched = match_spots_to_cells(
        pos, frames, tracks, cell_pos, cell_frames, labels, max_d)

    if matched == 0:
        # Loud, because the usual causes are a unit mismatch or an export from a different image, and
        # a silent column of -1 looks exactly like a successful import of a sparsely-tracked movie.
        log.log('[WARNING] No spot matched any cell — check spotUnit, maxDistance, and that this '
                'export belongs to this image')

    out = pd.DataFrame({'label': labels})
    out[out_col] = track_of_cell
    view.add_obs(out).save()
    log.log(f'[INFO] Wrote {out_col}: {matched}/{len(labels)} cells matched a track')
    log.progress(2, 2)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
