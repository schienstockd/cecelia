"""Stitch several .mp4s into one grid-stitched .mp4.

Called as a subprocess by the offline compare-grid renderer's row + column composes
(``api/src/movie_rail.jl`` → ``_stitch_offline``). Julia has rendered the per-cell mp4s (one per
grid cell); this composes them side-by-side (or stacked) exactly like the napari-bridge
``stitch_movies`` command does, using the SAME helper (``cecelia.utils.movie_io.stitch_movies``).
Kept out of Julia because ``movie_io`` is the one imageio writer in the repo, and captions come
from ``title_card.caption_band`` which is the one PIL text stack.

The path-of-least-resistance replacement for ``napari_utils`` bridging when the movie rail no
longer needs napari — the compare grid is the last piece.
"""

import cecelia.utils.script_utils as script_utils
from cecelia.utils.movie_io import stitch_movies


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    sources = list(params['sources'])
    labels = params.get('labels')
    if labels is not None:
        labels = list(labels)
    n = stitch_movies(
        sources, params['outPath'],
        fps=float(params.get('fps', 15)),
        labels=labels,
        layout=params.get('layout', 'row'),
        # A `[PROGRESS] i/total` line per stitched frame — `run_py` parses that and forwards it as
        # `task:progress`, matching what the napari-bridged stitch used to report.
        on_progress=lambda i, total: log.log(f'[PROGRESS] {i}/{total}'),
    )
    log.log(f'[INFO] stitched {n} frame(s) to {params["outPath"]}')

    # Title card handling matches encode_movie_run.py: prepended AFTER the stitch, so the card is
    # rendered at the composed movie's exact resolution — one card, one PIL stack, one place.
    card = params.get('titleCard')
    if isinstance(card, dict) and card.get('enabled', True):
        from cecelia.utils import title_card
        duration = float(card.get('durationSec', 3.0))
        k = title_card.prepend_title_to_movie(params['outPath'], card, duration_sec=duration)
        log.log(f'[INFO] prepended {k} title-card frame(s)')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
