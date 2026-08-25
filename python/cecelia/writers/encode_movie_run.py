"""Raw-frame movie encoder entry point.

Called as a subprocess by renderer C's timelapse sweep (``api/src/movie_render.jl``) to turn a file of
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
    n = encode_raw_frames(
        params['rawPath'], params['outPath'],
        width=params['width'], height=params['height'],
        frames=params['frames'], fps=params.get('fps', 15), log=log)
    log.log(f'[INFO] encoded {n} frames to {params["outPath"]}')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
