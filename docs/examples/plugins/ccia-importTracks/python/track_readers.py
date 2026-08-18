"""Shared helper for the `ccia-importTracks` plugin — read an external track export.

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

    from track_readers import read_track_file, match_spots_to_cells

Note what is NOT in that import: the plugin's directory name. `ccia-importTracks` contains a hyphen
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
import os
import xml.etree.ElementTree as ET

import numpy as np
from scipy.spatial import cKDTree

# TrackMate writes several header rows after the real one (units, descriptions); they are recognisable
# because the ID/track column does not parse as a number. We skip any row that fails to parse rather
# than hard-coding "skip 3 rows", which differs between TrackMate versions.


def read_spot_csv(path, track_column='Track n°', frame_column='Slice n°',
                  pos_columns=('X', 'Y', 'Z'), frame_base=1, skip_rows=0, delimiter=','):
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
        # The delimiter is PASSED, never sniffed here: the task's Julia already sniffed it to
        # populate the column picker, and a second guess could disagree with the first on one file.
        reader = csv.DictReader(fh, delimiter=delimiter)
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


def read_tracks_xml(path):
    """Read TrackMate's **Export tracks to XML** → `(track_ids, frames, positions)`.

    That export is not a table and has no columns to map, so it bypasses the mapping entirely::

        <Tracks nTracks="314" spaceUnits="micron" frameInterval="15.0" timeUnits="sec">
          <particle nSpots="24">
            <detection t="6" x="87.2" y="72.3" z="55.1" />

    **The grouping IS the track.** A `<particle>` carries no id, so the track id is its ordinal
    position in the file — stable for one export, and the only identifier there is. `t` is the frame
    (already 0-based here), and coordinates are in the file's `spaceUnits`, micron in every export
    seen, which is why the caller still converts to pixels.

    This is a DIFFERENT TrackMate export from the "Spots in tracks statistics" CSV, which does have
    TRACK_ID/POSITION_X columns and goes through `read_spot_csv`. Both come out of the same tool, so
    the file itself has to say which one it is — hence dispatching on the extension in
    `read_track_file` rather than asking the user to know.
    """
    root = ET.parse(path).getroot()
    tracks, frames, positions = [], [], []
    for i, particle in enumerate(root.findall('particle')):
        for d in particle.findall('detection'):
            try:
                t = int(float(d.get('t')))
                xyz = [float(d.get(a)) for a in ('x', 'y', 'z') if d.get(a) is not None]
            except (TypeError, ValueError):
                continue          # one malformed detection must not lose the whole export
            if not xyz:
                continue
            tracks.append(i)
            frames.append(t)
            positions.append(xyz)
    if not tracks:
        return np.array([], dtype=int), np.array([], dtype=int), np.zeros((0, 3))
    width = min(len(p) for p in positions)      # 2D and 3D exports both land square
    return (np.array(tracks, dtype=int), np.array(frames, dtype=int),
            np.array([p[:width] for p in positions], dtype=float))


def read_track_file(path, mapping):
    """Read whatever the user pointed at — XML or delimited — as `(track_ids, frames, positions)`.

    ONE entry point, so the runner never branches on format and a new source is added here rather
    than at the call site. `.xml` is TrackMate's track export (no columns); anything else is a
    delimited table read through the caller's column mapping.
    """
    if os.path.splitext(str(path))[1].lower() == '.xml':
        return read_tracks_xml(path)
    pos_cols = tuple(c for c in (mapping.get('xColumn'), mapping.get('yColumn'),
                                 mapping.get('zColumn')) if c)
    return read_spot_csv(path,
                         mapping.get('trackColumn', 'TRACK_ID'),
                         mapping.get('frameColumn', 'FRAME'),
                         pos_cols,
                         int(mapping.get('frameBase', 0)),
                         int(mapping.get('skipRows', 0)),
                         mapping.get('delimiter', ','))
