"""Shared helper for the `tracktools-example` plugin — read an external per-spot track export.

**Format-neutral by design.** Every column name, the frame base, the header offset and the coordinate
unit are parameters, because there is no single "external tracks" format — labs export from ImageJ
Manual Tracking, TrackMate, Imaris, or something bespoke. Ready-made mappings live in
`../templates/*.json` and the user's own field entries override them, so an unlisted tool is supported
by mapping its columns once, not by changing this file.

> **The shipped templates are inferred, not confirmed against a real export.** `Track n°` / `Slice n°`
> carry a non-ASCII `°` (U+00B0) and ImageJ does not reliably write UTF-8; Imaris's preamble length has
> varied between versions. Check a template against an actual file before trusting it — and if it is
> nearly right, fix the one field on the form rather than editing code. See docs/todo/PLUGINS_PLAN.md.

**This file is the point of the example.** It lives in the plugin's `python/` directory, which
`run_py` puts on `PYTHONPATH` (`_custom_modules_pydirs`), so the task runner beside it imports it as a
plain top-level module:

    from csv_tracks import read_spot_csv, match_spots_to_cells

Note what is NOT in that import: the plugin's directory name. `tracktools-example` contains a hyphen
and is not a Python identifier, so anything that spelled the plugin name into a module path could
never work. Naming `python/` on the path directly is what frees a plugin directory to be called
whatever its repo is called. See docs/todo/PLUGINS_PLAN.md → R2.

**Why a spatial match and not a join.** The driving case is a lab that tracked cells in another tool
*independently of cecelia*, and wants those tracks attached to cecelia's own segmentation. Such an
export has **no cecelia label ids** — it has a track id plus spot positions and a frame. There is
nothing to join on, so each spot is matched to the nearest segmented cell centroid
IN THE SAME FRAME, within a distance cutoff. That cutoff is the whole safety margin: without it every
spot finds *some* nearest cell and a completely mismatched export still produces a full, wrong column.

**Standard library + what the env already ships.** A plugin may use whatever the Cecelia Python env
has (`numpy`, `scipy`) but cannot declare its own pip dependencies — see docs/CUSTOM_MODULES.md →
*Limits*.
"""
import csv

import numpy as np
from scipy.spatial import cKDTree

# TrackMate writes several header rows after the real one (units, descriptions); they are recognisable
# because the ID/track column does not parse as a number. We skip any row that fails to parse rather
# than hard-coding "skip 3 rows", which differs between TrackMate versions.


def read_spot_csv(path, track_column='Track n°', frame_column='Slice n°',
                  pos_columns=('X', 'Y', 'Z'), frame_base=1, skip_rows=0):
    """Read a per-spot track export → `(track_ids, frames, positions)`.

    There is no single external-tracks format, so nothing here is hard-coded: the caller supplies a
    **column mapping**, and `templates/*.json` beside this file ship ready-made ones (ImageJ Manual
    Tracking, TrackMate, Imaris). Adding a new source is a new template, not new code.

    `frame_base` is subtracted so frames come back 0-based, matching cecelia's `centroid_t` — an
    off-by-one here silently matches every cell against the wrong timepoint, which produces a full
    column of plausible nonsense rather than an error.

    `skip_rows` drops lines BEFORE the header row (Imaris writes a 3-line preamble above its real
    header). This is separate from the junk-row tolerance below, which handles extra rows *after* the
    header: TrackMate writes unit/description rows there, and they are recognised by failing to parse
    rather than by a hard-coded count, which differs between versions.

    `positions` is an (n, d) float array in the file's own units, with one column per entry of
    `pos_columns` that is actually present — so a 2D export with no `POSITION_Z` yields (n, 2).

    Rows that do not parse (TrackMate's extra header rows, blank lines, spots with no track) are
    skipped. Raises `KeyError` naming the columns actually present if a required one is missing —
    that is the single most likely thing to go wrong with someone else's export, and the message is
    the whole diagnosis.
    """
    # errors='replace' rather than strict: ImageJ does not reliably write UTF-8, and a single odd byte
    # in a comment column must not take down an otherwise readable export. The mapped column NAMES are
    # matched after decoding, so a mangled byte elsewhere is harmless.
    with open(path, newline='', encoding='utf-8', errors='replace') as fh:
        for _ in range(int(skip_rows)):
            fh.readline()
        reader = csv.DictReader(fh)
        if reader.fieldnames is None:
            raise KeyError(f'{path} is empty — no header row')
        have = list(reader.fieldnames)
        pos_cols = [c for c in pos_columns if c in have]
        missing = [c for c in (track_column, frame_column) if c not in have]
        if missing or not pos_cols:
            raise KeyError(
                f'{path}: missing {missing or "any of " + str(list(pos_columns))}; found: {have}')

        tracks, frames, positions = [], [], []
        for row in reader:
            try:
                t = int(float(row[track_column]))
                f = int(float(row[frame_column])) - frame_base
                p = [float(row[c]) for c in pos_cols]
            except (TypeError, ValueError):
                continue          # TrackMate's unit/description header rows land here, as intended
            if t < 0:
                continue          # TrackMate marks an untracked spot with a negative/blank TRACK_ID
            tracks.append(t)
            frames.append(f)
            positions.append(p)

    if not tracks:
        return np.array([], dtype=int), np.array([], dtype=int), np.zeros((0, len(pos_cols)))
    return (np.array(tracks, dtype=int), np.array(frames, dtype=int),
            np.array(positions, dtype=float))


def match_spots_to_cells(spot_pos, spot_frames, spot_tracks,
                         cell_pos, cell_frames, cell_labels, max_distance):
    """Assign each cell a track id by nearest spot **within its own frame**.

    Returns `(track_of_cell, n_matched)` where `track_of_cell` is aligned with `cell_labels` and is
    `-1` for any cell with no spot inside `max_distance`. Untracked is a real answer: the external
    tool tracked what it could see, cecelia segmented what it could see, and the two do not have to
    agree cell-for-cell.

    Matching is per frame, so a spot can never be attached to a cell at a different timepoint however
    close it is in space. Within a frame it is a KD-tree nearest lookup — the cutoff, not the ranking,
    is what makes a wrong export fail loudly instead of silently.
    """
    track_of_cell = np.full(len(cell_labels), -1, dtype=np.int32)
    if len(spot_tracks) == 0 or len(cell_labels) == 0:
        return track_of_cell, 0

    for f in np.unique(cell_frames):
        cell_idx = np.flatnonzero(cell_frames == f)
        spot_idx = np.flatnonzero(spot_frames == f)
        if cell_idx.size == 0 or spot_idx.size == 0:
            continue
        tree = cKDTree(spot_pos[spot_idx])
        dist, nn = tree.query(cell_pos[cell_idx], k=1, distance_upper_bound=max_distance)
        ok = np.isfinite(dist)          # query returns inf for "nothing within the cutoff"
        track_of_cell[cell_idx[ok]] = spot_tracks[spot_idx[nn[ok]]]

    return track_of_cell, int((track_of_cell >= 0).sum())
