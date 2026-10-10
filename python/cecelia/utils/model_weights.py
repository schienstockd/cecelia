"""Fetch cellpose's built-in model weights ahead of first use.

Cellpose downloads a built-in model's weights the first time it is loaded — for `cpsam_v2` that is
~1.2 GB, which inside a preview request outlasts the browser's timeout and reads as "Preview failed".
This module is what the model-weights job (`api/src/system_api.jl`, through `run_py`) and the
installers run instead, so a download happens where the user can see and retry it.

    python -m cecelia.utils.model_weights cpsam_v2 [cyto3 …]      # installers
    run_py("utils/model_weights.py", Dict("models" => [...]), dir)  # the job

Cellpose 4 weights are fetched here, into cellpose's own cache (`models.MODEL_DIR`, from cellpose's
`cache_model_path`) under the model's name — the file cellpose looks for, so nothing else changes.
Cellpose 3 (`cyto2`/`cyto3`, the opt-in `cellpose-v3` env) caches its own files on construction, so
there the model is simply constructed once.

Progress is `[PROGRESS] bytes/total` through `StdoutLogger`. Exit 0 = every model present.
"""
import importlib.metadata
import os
import sys
from urllib.request import urlopen

from cecelia.utils.atomic_io import write_atomic
from cecelia.utils.script_utils import StdoutLogger

_CHUNK = 1 << 20          # 1 MiB


def download(url: str, dst: str, logger=None, opener=urlopen) -> None:
    """Stream `url` to `dst` atomically: an interrupted download leaves no file, so a present file
    always means complete weights (the presence check in system_api.jl relies on that). Reports every
    chunk; `StdoutLogger.progress` decides which are worth a line."""
    logger = logger or StdoutLogger()
    with opener(url) as resp:
        total = int(resp.headers.get("Content-Length") or 0)
        done = 0
        os.makedirs(os.path.dirname(dst) or ".", exist_ok=True)
        with write_atomic(dst, "wb") as f:
            while True:
                buf = resp.read(_CHUNK)
                if not buf:
                    break
                f.write(buf)
                done += len(buf)
                logger.progress(done, total)
            # inside the `with`: raising here discards the temp instead of committing a short file
            if total and done != total:
                raise OSError(f"download of {url} ended at {done} of {total} bytes")


def _cellpose_major() -> int:
    # `cellpose` has no `__version__`; the installed distribution's metadata is the reliable answer.
    # Not `cellpose_utils._cellpose_major_version`: that pulls in the segmentation stack, and this
    # runs in both envs as a one-shot.
    return int(importlib.metadata.version("cellpose").split(".")[0])


def fetch(name: str, logger=None) -> None:
    """Make sure the built-in model `name` has its weights on disk."""
    logger = logger or StdoutLogger()
    from cellpose import models
    if _cellpose_major() < 4:
        logger.log(f"loading {name} (cellpose 3 caches its own weights)…")
        models.CellposeModel(model_type=name)
        return
    if name not in models.MODEL_NAMES:
        raise ValueError(f"{name!r} is not a cellpose 4 built-in model ({', '.join(models.MODEL_NAMES)})")
    dst = os.fspath(models.MODEL_DIR.joinpath(name))
    if os.path.isfile(dst):
        logger.log(f"{name}: already present at {dst}")
        return
    logger.log(f"{name}: downloading to {dst}")
    download(models._MODEL_URL + name, dst, logger=logger)
    logger.log(f"{name}: done")


def main(argv=None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    if "--params" in argv:   # the model-weights job, through `run_py`: {"models": [...]}
        from cecelia.utils import script_utils
        params = script_utils.script_params() or {}
        names = [str(n) for n in params.get("models", [])]
    else:                    # the installers: model names on the command line
        names = argv
    if not names:
        print("usage: python -m cecelia.utils.model_weights <model> [<model> …]", file=sys.stderr)
        return 2
    logger = StdoutLogger()
    for name in names:
        try:
            fetch(name, logger)
        except Exception as e:  # noqa: BLE001 — one line per model the job log can show
            logger.log(f"[ERROR] {name}: {e}")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
