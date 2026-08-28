"""Raw-frame movie encoder entry point.

Called as a subprocess by the offline renderer's timelapse sweep (``api/src/movie_render.jl``) to turn a file of
raw RGB24 frames into an mp4. Julia renders the frames — it owns the compositing, the LUTs and the z
projection — and this owns the encode, because ``cecelia.utils.movie_io`` is the one imageio writer in
the repo and a second one in Julia would be two answers about codec, pixel format and even-dimension
handling. It is a data-layer writer, not a scheduler task, so it lives in the IO library under
``python/cecelia/writers/``.
"""

import cecelia.utils.script_utils as script_utils
from cecelia.utils.movie_io import encode_raw_frames


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    overlays = params.get('overlays')
    per_frame_overlay = None
    if isinstance(overlays, list) and overlays:
        # Timestamp + scale bar — anti-aliased text + a white bar via `title_card.draw_frame_overlays`.
        # The Julia renderer has no anti-aliased text primitive, so overlay drawing happens once in the
        # encode pass instead of per-frame in-Julia. `overlays[i]` is a dict `{timestamp?, scaleBar?}`
        # matching the helper's kwargs; a shorter list means the missing frames get no overlay (which
        # keeps a title-card + main-clip sequence consistent — the card is prepended AFTER the encode
        # and picks up nothing from this list).
        from cecelia.utils.title_card import draw_frame_overlays

        def _apply(i, frame):
            if i < len(overlays):
                item = overlays[i] or {}
                return draw_frame_overlays(frame,
                                            timestamp=item.get('timestamp'),
                                            scale_bar=item.get('scaleBar'))
            return frame
        per_frame_overlay = _apply
    n = encode_raw_frames(
        params['rawPath'], params['outPath'],
        width=params['width'], height=params['height'],
        frames=params['frames'], fps=params.get('fps', 15), log=log,
        per_frame_overlay=per_frame_overlay)
    log.log(f'[INFO] encoded {n} frames to {params["outPath"]}')

    # Title card is prepended AFTER the encode rather than composited into the raw frames: the card is
    # rendered at the encoded movie's exact resolution (read back from the mp4), so a card written by
    # Julia at write-time would need to duplicate movie_io's even-crop rule and its font stack. Reuse
    # the same helper the napari path uses (`title_card.prepend_title_to_movie`) — one path for both
    # renderers, and its test suite already covers the render+prepend.
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
