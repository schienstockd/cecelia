# SUPPORT — vendored

**Upstream:** https://github.com/NICALab/SUPPORT
**Commit pinned:** `9fba0f415b35106d7d7b9c0144e72917329f0575` (README update, current HEAD 2026-09-05)
**License:** GNU GPL-3.0-or-later — compatible with cecelia's GPL-3-or-later.
**Paper:** Eom et al., *Statistically unbiased prediction enables accurate denoising of voltage
imaging data.* Nature Methods (2023).

## What was vendored

- `model/SUPPORT.py` — the temporal blind-spot network. Unchanged except for one import fix.
- `model/convhole.py` — the `ConvHole2D` primitive the blind-spot path is built from. Unchanged.
- `dataset.py` — merged from upstream `src/utils/dataset.py` (three classes + two helpers) and
  `src/utils/util.py::get_coordinate` (stitching coordinates for tiled inference). The two-file
  layout upstream forces a package `src.utils`; a single file keeps the vendor surface small.

## Why vendored, not `pip install`ed

SUPPORT is not on PyPI as a working package (the sibling `deepcad`/`deepcad-rt` PyPI wheels are
empty metadata stubs — verified 2026-09-05 while evaluating DeepCAD-RT on 2h06xA). The upstream
repo is a research repo — not `pip install`-ready, no `setup.py` / `pyproject.toml`. Vendoring at a
pinned commit is the working option.

## Local changes

- `model/SUPPORT.py`: `from model.convhole import ConvHole2D` → `from .convhole import ConvHole2D`,
  so it resolves as a submodule of `cecelia.vendor.support.model`.
- `dataset.py`: merged the `get_coordinate` function into the same file (dropped `from
  src.utils.util import get_coordinate` upstream dep); removed `gen_train_dataloader` (unused — its
  `skio.imread` + `zarr.open` path is replaced by the cecelia runner which reads through
  `zarr_utils`); removed the `tqdm` progress bar in `DatasetSUPPORT.__init__` (the runner streams
  its own progress). No numerical changes.

## Not vendored

- `src/utils/dataset_pyqt.py` — Qt-specific.
- `src/utils/util.py` — CLI/argparse plumbing. Only `get_coordinate` is needed; it was inlined.
- `colab/`, `data/`, `docs/`, `Beginner_guide.md`, `README.md`, images — user-facing docs, not code.

## Evaluation record

The 2026-09-05 evaluation that led to picking SUPPORT is recorded in the auto-memory
`project_denoising_methods_eval` and in `docs/todo/DENOISE_INTEGRATION_PLAN.md`.
