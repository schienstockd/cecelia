# Python-helper packaging plan

> **Update (2026-07, branch `feat/py-package-boundary`):** the boundary was tightened further —
> **task runners moved back out** of the package into `app/src/tasks/<cat>/<name>_run.py`
> (co-located with their `.jl`, run by path via `run_py`), so `python/cecelia/` is now the **IO
> library only** (`cecelia` + `cecelia.utils` + writers; `pyproject` ships only those). The vendored
> btrack `cell_config.json` moved next to the tracking task (`app/src/tasks/tracking/`) and is passed
> to `cecelia.utils.tracking_utils` by path (no more package-data reach). So the "`app/` is Julia-only"
> statements below are historical — `app/` now also holds each task's Python runner.

Status: **Phases 1+2 + top-level move — DONE, verified in-env** on branch `feat/py-packaging`.
Package now at top-level `python/cecelia/` (import name `cecelia`); `app/` is Julia-only. Prompted by
a sibling project (`coastal`) needing to import cecelia's stateless Python IO helpers without a
machine-specific `sys.path` hack. Verified on this machine (linux-64): `pixi install` (env builds,
`pixi.lock` regenerated with the editable `cecelia` dep), `pixi run test-py` green, `pixi run
test-pkg` green (625 pass / 0 fail). The redundant per-file `sys.path` bootstraps were removed;
`run_py`'s `PYTHONPATH=python/` is retained as the canonical launcher mechanism. Only unverified
surface: cross-platform resolution on win-64 / osx-arm64 (left to CI).

## Goal

Make cecelia's Python helpers (`app/py/…`) a **proper, installable, importable package** so that:

1. External consumers (starting with `coastal`) can `pip install` cecelia's helpers and
   `import <name>.utils.zarr_utils` — no absolute `sys.path.insert`, no PYTHONPATH assumption.
2. The public import name is not `py` (a real PyPI package — it shadows / is shadowed by
   pytest's legacy `py` dependency; a library must not claim the `py` top-level name).
3. Consumers pull only the deps they need (IO helpers must not drag in napari/torch/scanpy/btrack).

Non-goal: changing what the helpers *do*, or the `run_py` subprocess model. This is packaging +
a rename only.

## Why now / what's forcing it

`coastal` (segmentation/tracking, a separate Python package) loads microscopy data through
cecelia's `zarr_utils` / `ome_xml_utils` / `dim_utils`, and runs btrack from
`app/py/tasks/tracking/cell_config.json`. Today its notebooks do:

```python
sys.path.insert(0, "/abs/path/to/cecelia-pineapple/app")
import py.utils.zarr_utils as zarr_utils
```

That works only because `run_py` sets `PYTHONPATH=app` (see `app/src/py_runner.jl` `run_py`,
where `cmd = addenv(…, "PYTHONPATH" => app)` and scripts are found via `joinpath(app, "py",
script_rel)`). For an *external* consumer it's a machine-specific hardcode with no dependency
contract. coastal has an interim `CECELIA_APP` env-var bootstrap
(see coastal `docs/DATA.md`, `docs/TODO.md`) that this plan supersedes.

## Current state (facts)

- Package dir: `app/py/`, with an **empty** `app/py/__init__.py`. Import name `py` resolves *only*
  because `PYTHONPATH=app`. Not pip-installable.
- Internal usage of the `py.` name: **19 Python files** with `import py.utils.X` / `from py.utils.X`
  (most-used: `script_utils` ×11, `zarr_utils` ×7, `ome_xml_utils` ×6, `dim_utils` ×5).
- Julia references the dir name in **exactly one place**: `joinpath(app, "py", script_rel)` in
  `app/src/py_runner.jl`. All `run_py("tasks/…/x_run.py")` call sites pass *relative* paths (no
  `py/` prefix) — so the dir rename touches one Julia line, not every call site.
- Env is managed by pixi (`pixi.toml [pypi-dependencies]`: zarr≥3, dask, anndata, scanpy, numpy,
  scipy, scikit-image, pandas, trimesh, btrack, leidenalg, ome-types, cellpose, napari, pyqt5,
  websockets). Tests: `test-py = python -m unittest discover -s app/py/tests`.
- Helper coupling (from a full survey): `zarr_utils`, `ome_xml_utils`, `dim_utils`, `slice_utils`,
  `math_helpers`, `label_props_utils`, `correction_utils`, `measure_utils`, `segmentation_utils`
  are **state-free** (paths / numpy / dask in, no `CciaImage`/`ccid.json`/API). `pop_utils` +
  `cecelia_client` need the running Julia API; `cellpose_utils`/`tracking_utils`/`clustering_utils`
  import heavy deps (torch/btrack/scanpy) at module load. `tifffile` is imported by the IO helpers
  but is **not** an explicit pixi dep (transitive today — must be declared, see Decision 4).

## Decisions (locked — 2026-07-08)

1. **Rename the top-level import name away from `py`.** Recommended: **`cecelia`** (dist name
   `cecelia` or `cecelia-py`; import `cecelia`). Alternatives considered: `ccia` (matches the
   internal `Ccia*`/`ccid` naming but cryptic), `cecelia_py`. `py` is rejected outright (PyPI
   collision). *Open sub-decision for Dominik:* confirm the exact dist name + check PyPI availability
   before any publish; for a local editable install the name only needs to be unique in-env.
   The Julia module is also `Cecelia` — cross-language namespaces don't collide, but note it.
2. **Ship as an installable package via `pyproject.toml`, installed editable into the pixi env.**
   Add the package as a path-based editable pypi dependency in `pixi.toml` so `import cecelia.…`
   resolves env-wide without `PYTHONPATH`. `run_py` keeps working and **retains `PYTHONPATH=python/` as the
   canonical, explicit launcher mechanism** for the task scripts it spawns (set once, centrally — not
   re-derived per script). The editable install is what makes `import cecelia.*` resolve in the
   *other* contexts (external `pip install`, `pixi run`, the napari bridge, a REPL). *(Revised from an
   earlier draft that proposed dropping `PYTHONPATH`: keeping the one central mechanism is simpler and
   more robust than relying solely on the install; the real duplication — the scattered per-file
   `sys.path` hacks — was removed instead.)* *Source-of-truth split (this is what keeps the two files from drifting):* `pyproject.toml`
   owns the **base IO deps** — the set that must reach an external `pip`/`uv` resolver like coastal,
   so `zarr>=3` lives here. `pixi.toml` owns everything **heavy + conda-sourced + per-platform-index**
   — `cellpose==3.1.1.2`, the cu124 `torch`/`torchvision` blocks, `cvxopt` from conda-forge,
   `python`/`openjdk`. The two sets are **disjoint**: `pixi.toml` drops the six IO-tier deps it listed explicitly
   (`numpy`, `zarr`, `dask`, `scipy`, `scikit-image`, `ome-types`) — they now arrive transitively via
   the editable `cecelia` dep, and `pandas` stays (anndata-tier, pixi-only) — so every pin lives in
   exactly one file — no drift, and it honours CLAUDE.md's
   "pixi.toml is the single source of truth" for the pins it still owns.
3. **Lightweight IO base, heavy deps pixi-only — no extras.** The state-free IO helpers' deps become
   the package's **base `[project.dependencies]`**: numpy, zarr≥3, dask, tifffile, ome-types, scipy,
   scikit-image, tqdm — covering `zarr_utils`, `ome_xml_utils`, `dim_utils`, `slice_utils`,
   `math_helpers`, `correction_utils` (the tier coastal imports: `zarr_utils`/`ome_xml_utils`/
   `dim_utils`). (Verified against the actual module-level imports: `pandas` is **not** used by this
   tier so it's dropped from the base; `tqdm` **is** — `correction_utils` imports it at load — so it's
   declared here, same class of transitive-only-in-pixi omission as `tifffile`.) The heavy deps — anndata, cellpose, torch, btrack, scanpy, leidenalg, napari, pyqt5,
   websockets, trimesh — are **not declared as package deps at all**; they stay in `pixi.toml` as
   cecelia's *environment* requirements. So `pip install cecelia` yields a light IO package and
   `import cecelia.utils.zarr_utils` resolves with **no** napari/torch/scanpy pulled in; cecelia's own
   heavy modules (`cellpose_utils`, `tracking_utils`, `clustering_utils`, the napari bridge) still
   import fine because pixi provides their deps in cecelia's env. Rejected: per-module extras
   (`[segment]`/`[cluster]`/…) — YAGNI, the only external consumer (coastal) needs just the IO tier.
   *If* a future non-cecelia consumer ever needs the anndata/h5ad readers (`label_props_utils`,
   `measure_utils`, `obs_utils`), add a single `[anndata]` extra then — a purely additive change.
4. **Declare `tifffile` and `tqdm` explicitly** (Decision 3 base deps) — both only transitive in
   pixi today (`tifffile` via the IO stack, `tqdm` via cellpose); a light `pip install cecelia` needs
   them declared or `zarr_utils`/`correction_utils` fail at import.
5. **Layout: move the package to a top-level `python/` dir (sibling of `app/`), NOT nested under
   `app/`.** Final layout: `python/pyproject.toml` + `python/cecelia/{utils,tasks,writers,tests}`.
   *(Revised 2026-07 — supersedes the original "rename `app/py`→`app/cecelia`, pyproject at `app/`"
   plan.)* Rationale: `app/` is the **Julia** package (`Project.toml`/`Manifest.toml`); dropping a
   `pyproject.toml` there made `app/` claim to be a Python project root too — fragile (relies on an
   explicit setuptools `include`) and confusing. A top-level `python/` keeps one ecosystem per
   top-level dir and matches the repo's existing convention (`napari/` is already top-level Python).
   The churn is contained because `run_py` call sites pass *relative* script paths — only the base
   path resolves differently: `py_runner.jl` gains `_python_dir() = joinpath(dirname(_app_dir()),
   "python")`, and `PYTHONPATH`/the pixi editable path/`test-py`/the bootstrap `_APP_DIR`s point at
   `python/`. Rejected: `app/python/` (still invents a second "where Python goes" pattern) and a
   `src/` layout (churns script paths).

## Phases

Each phase is independently shippable and CI-gated (all four suites: `test-pkg`, `test-api`,
`test-py`, `test-frontend`).

### Phase 1 — make it installable under a pip package (no rename yet)
- Add `python/pyproject.toml` (setuptools or hatchling) declaring the package + **base IO deps**
  (Decision 3–4), temporarily packaging the existing `py` dir (`packages = ["py", "py.utils", …]`
  or `find`).
- Add an editable path dependency in `pixi.toml [pypi-dependencies]` (`cecelia = { path = "python", editable = true }`) and remove the six now-transitive IO deps (`numpy`/`zarr`/`dask`/`scipy`/
  `scikit-image`/`ome-types`) from pixi's explicit list (Decision 2 disjoint-sets split).
- Confirm the base-import boundary: the core modules (`zarr_utils`/`ome_xml_utils`/`dim_utils`/
  `slice_utils`/`math_helpers`/`correction_utils`) have **no module-level heavy imports** (torch/
  cellpose/scanpy/btrack/anndata/napari) — if any imports a heavy dep at load, make it lazy, else a
  light `pip install cecelia` consumer hits `ImportError`.
- `pixi install`; verify `import py.utils.zarr_utils` resolves **via the install** with `PYTHONPATH`
  unset (else you can't tell the install from the path), and `test-py` passes on all platforms.
  Checkpoint: env-parity confirmed. `PYTHONPATH=python/` is kept in `run_py` as the canonical
  mechanism (Decision 2); the redundant per-file `sys.path` bootstraps are removed instead.

> **Implementation status (branch `feat/py-packaging`).** Phases 1 + 2 + the Decision-5 top-level
> move are all done together on this branch. **Final on-disk state:** the package lives at
> `python/cecelia/` (top-level, sibling to `app/`) with `python/pyproject.toml`; `app/` is Julia-only.
> `run_py` resolves scripts via `_python_dir()` and sets `PYTHONPATH=python/`; `pixi.toml` has the
> editable `cecelia = { path = "python" }` dep and `test-py -s python/cecelia/tests`; the six IO deps
> are removed from pixi's explicit list. All 19 imports + the 4 self-bootstrap files updated to
> `cecelia.*`. **Verified in an isolated scratch venv** (`pip install -e python`, `PYTHONPATH` unset,
> cwd outside the tree): `import cecelia` resolves to `python/cecelia/__init__.py`; the coastal trio
> (`zarr_utils`/`ome_xml_utils`/`dim_utils`) + full IO tier import; base deps present (zarr 3.2.1,
> numpy 2.5.1, tifffile/ome-types/dask/scipy/scikit-image/tqdm); **zero** heavy deps present
> (torch/cellpose/napari/scanpy/btrack/anndata/leidenalg/trimesh/pyqt5); built wheel contains only
> `cecelia/*.py` (no `.jl`/`Project.toml` leakage). Core-module import boundary confirmed clean.
> **Verified in-env on this machine (linux-64):** `pixi install` (env builds; `pixi.lock` regenerated
> with the editable `cecelia` dep), `pixi run test-py` green, and `pixi run test-pkg` green (625 pass /
> 11 broken / 0 fail — incl. the Julia↔Python LabelProps parity subtest against the workspace fixture).
> The four redundant per-file `sys.path` bootstraps (`napari_bridge.py`, `rechunk_zarr.py`,
> `test_zarr_store.py`, `write_categorical_obs_run.py`) were removed and the removal re-verified
> (import from a neutral cwd via the editable install + `test-py`); `run_py`'s `PYTHONPATH=python/` is
> retained (Decision 2). **Only unverified surface:** cross-platform resolution on win-64 / osx-arm64
> (this machine is linux-64) — left to CI.

### Phase 2 — rename `py` → `cecelia` + relocate to top-level `python/` (atomic)
- Relocate the package to top-level `python/cecelia/` (sibling of `app/`) with `python/pyproject.toml` (Decision 5).
- Rewrite imports in the 19 files: `import py.utils.` → `import cecelia.utils.` /
  `from py.utils.` → `from cecelia.utils.` (mechanical; grep-verify zero `\bpy\.` left).
- `app/src/py_runner.jl`: add `_python_dir()` and resolve scripts via `joinpath(_python_dir(), "cecelia", script_rel)`; set `PYTHONPATH=python/`.
- `pixi.toml`: editable dep `path = "python"`; `test-py` discover path `-s python/cecelia/tests`.
- Fix the **four** self-bootstrap files that `sys.path.insert` `app` and import `py.*`:
  `napari/napari_bridge.py`, `python/cecelia/utils/rechunk_zarr.py`, `python/cecelia/tests/test_zarr_store.py`,
  and `python/cecelia/writers/write_categorical_obs_run.py` (this last one is a *writer*, not a test —
  easy to miss). After the editable install + rename, most of these `sys.path` hacks can be
  deleted outright (the install makes `import cecelia.*` resolve); update the imports either way.
- Run all four CI suites. Checkpoint: full pipeline green (import a movie, segment, track, cluster).

### Phase 3 — expose to consumers + document — mostly DONE (2026-07)
- External install: `pip install <cecelia>/python` (frozen wheel copy) or `pip install -e
  <cecelia>/python` (editable, live) — the light IO base, `import cecelia.utils.zarr_utils`.
- **DONE — `cell_config.json` now ships as package-data** (`[tool.setuptools.package-data]` on
  `cecelia.tasks.tracking`; `cecelia.tasks`/`cecelia.tasks.tracking` added to the packaged set), so
  it's locatable via `importlib.resources` on a **wheel OR editable** install — not just a source
  tree. Pinned by `python/cecelia/tests/test_package_data.py`.
- **DONE — `coastal` cut over**: dropped the `CECELIA_APP`/`sys.path` bootstrap in the three
  notebooks, switched to `import cecelia.utils.*`, resolves `BTRACK_CONFIG` via `importlib.resources`,
  added a `notebooks` extra (`btrack`), relicensed to GPL, and installs cecelia via
  `scripts/link_cecelia.sh` (editable = live). btrack is coastal's own dep; from cecelia it needs
  only the config file. coastal `docs/DATA.md` updated + its integration TODO closed.
- **Remaining:** publish cecelia to PyPI (gated on the Decision-1 dist-name check) so consumers can
  drop the path/editable bridge for a pinned `cecelia>=x.y` dep (coastal has a TODO for the switch);
  and promote the durable "how cecelia exposes Python" content into `docs/SHIPPING.md`, then mark
  this plan DONE.

## Risks / watch

- **Cross-platform editable path deps in pixi** — verify on linux-64 / win-64 / osx-arm64 (pixi
  has per-target pypi blocks already).
- **`run_py` script discovery** — after Phase 2 scripts are still invoked by file path
  (`joinpath(_python_dir(), "cecelia", script_rel)`); the editable install is what makes `import cecelia.*`
  inside them resolve. **`PYTHONPATH=python/` is kept** in `run_py` as the canonical mechanism (Decision 2); the
  redundant per-file `sys.path` bootstraps were removed. The editable install covers the non-`run_py`
  contexts (`pip install`, `pixi run`, napari, REPL).
- **Data files** — **addressed for `cell_config.json`**: declared as package-data on
  `cecelia.tasks.tracking`, so `importlib.resources` locates it on any install (test-pinned). Any
  *future* bundled data (e.g. models) must be added the same way, or external consumers hardcode paths.
- **Name check** — confirm `cecelia`/`cecelia-py` dist name before publishing (Decision 1).

## References

- `app/src/py_runner.jl` — `run_py`, `_python_dir()` + `joinpath(_python_dir(), "cecelia", …)` + `PYTHONPATH=python`.
- `pixi.toml` — `[pypi-dependencies]`, `test-py` task.
- `python/cecelia/utils/*.py` — the helper modules.
- Consumer side: coastal `docs/DATA.md` (§Notebook data loading), coastal `docs/TODO.md`
  (Cecelia integration), coastal `docs/JULIA_PORT.md` (reunification angle — a clean
  `pip install cecelia[…]` is the seam a Python *or* Julia coastal would consume).
- Related parked plan: `docs/todo/CLUSTERING_PLAN.md` (also cited from `pixi.toml`).
