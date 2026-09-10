# Cellpose v3 as a Mac-only opt-in

**Status:** planning, 2026-09-10. Branch `feat/cellpose-v3-optin`, worktree
`cecelia-cellpose-v3-optin`.

Dominik, 2026-09-10: *"just tried to run the cellpose segmentation on a mac m3 laptop. so slow.
like. really slow"* — cellpose 4 (Cellpose-SAM) is a ~300M-param transformer and MPS is much slower
per op than CUDA. On the R version, cyto/cyto2/cyto3 on MPS worked fine; the migration to v4
(`CELLPOSE_V4_PLAN.md`, #610) is what regressed Mac users.

## Goal

Give macOS users a fast segmentation path without regressing anyone else. Keep v4 as default
everywhere. On Mac, warn on v4 selection and offer a one-click install of a second pixi env
carrying cellpose 3.

## Decisions

1. **v3 opt-in is Mac only.** `[feature.cellpose-v3]` is scoped `platforms = ["osx-arm64"]`.
   Linux/Windows CUDA users don't need it — v4 is fast on CUDA, and the extra env would be ~3 GB.
   The warning banner and Install button also gate on `Sys.isapple()`.
2. **Two pixi envs, opt-in.** `default` keeps `cellpose>=4.2`. A new `cellpose-v3` env carries
   `cellpose>=3.1,<4` (3.1 has native MPS — no monkey-patch, unlike the R-era
   `inst/patches/cellpose/core.py`). Pixi does not install non-default environments until asked, so
   users who never hit the button pay nothing.
3. **Ship cyto2 + cyto3 only.** Bundled via `scripts/models_fetch.py` so first-run works offline —
   no silent 100 MB download on first segmentation. `nuclei` and tissue-specifics deferred.
4. **One cellpose task, unified dropdown.** Catalog entries in `BUILTIN_CELLPOSE_MODELS` gain a
   `backend :: Symbol` field (`:v3` or `:v4`). Frontend shows a small `v3` / `v4` pill per row
   (reuse the existing tag primitive — grep before writing). Model choice drives which env
   `run_py` shells.
5. **Warning uses `components/InlineNote.vue`.** New param advisory registered in the same
   ParamRenderer registry that already carries `imageVersionAdvisory`
   (`valueNameSelection`). Keyed on the cellpose model param. Fires when `platform=darwin`
   AND `selected.backend == :v4`. InlineNote takes the note; the Install button sits **beside**
   InlineNote as a trailing control (see its docstring: *"the host can add a trailing control … without
   their tooltips firing on top of each other"*).
6. **Install runs from the app via jobs.jl.** `POST /api/system/envs/install?env=cellpose-v3`
   shells `pixi install -e cellpose-v3`, registers as a cancellable job, streams into the log
   rail. Refused with a clear error on non-Mac.
7. **Probe endpoint** `GET /api/system/envs` returns `{cellpose-v3: {installed: bool, size_mb}}`.
   Frontend hides the Install button once installed; note copy switches to *"Switch to a v3 model
   for ~10× speedup on Apple Silicon."*
8. **No silent env fallback.** If a v3 model is picked but the v3 env is absent, `run_py` fails
   loudly with a message pointing at the install action. Never silently reroute to v4 — that would
   be a silent-result change (same class of bug the v4 migration plan called out).

## Phases

Each phase is independently shippable but the whole thing lands in one PR (per the "finish feature
before opening PR" convention).

### Phase 1 — pixi env + install job

- **`pixi.toml`** — add `[feature.cellpose-v3.pypi-dependencies]` with `cellpose>=3.1,<4`,
  `torch>=2.6` (default index, MPS-capable), `torchvision>=0.21`, plus the same `cvxopt` / `pyproj`
  osx-arm64 workarounds already in `default`. Add `[environments.cellpose-v3]` with
  `no-default-feature = false` so the shared base features still apply. Set `platforms =
  ["osx-arm64"]` on the feature so a Linux/Windows solve doesn't try to resolve v3.
- **`scripts/models_fetch.py`** — un-retire the v3 pull for `cyto2` + `cyto3` only. Place under a
  path the v3 env's cellpose reads without a lookup (`~/.cellpose/models` or via
  `CELLPOSE_LOCAL_MODELS_PATH`, whichever the runner sets — verify by reading the v3 code path).
- **`app/src/api/system.jl`** (new or existing) — `GET /api/system/envs` and
  `POST /api/system/envs/install`. The POST registers a `jobs.jl` job that shells
  `pixi install -e cellpose-v3`. Cancel via the Task Manager. On non-Mac, POST returns 400 with a
  human message.
- **Test:** `pixi run test-api` for both endpoints (mock the pixi shell for the POST — full install
  is too slow for CI); `pixi run test-py` for the models_fetch change.
- **Windows compat:** every path in the install job goes through the helpers already in
  `docs/DEV.md → Windows compatibility` — no inline `kill`/`taskkill`, always `joinpath`,
  `python_bin_path` for the interpreter probe.

### Phase 2 — v3 code path in the runner

- **`python/cecelia/utils/cellpose_utils.py`** — version-branched `predict_slice`. v3 uses
  `models.CellposeModel(gpu=True, device=torch.device('mps'), model_type='cyto3')` and
  `model.eval(x, channels=[cyto,nuc], z_axis=0, do_3D=False, ...)`. The existing
  `cellChannels`/`nucChannels` merge translates to v3's positional `channels`; the no-nuc path is
  `[cyto,0]`. **Detection:** import `cellpose` and branch on
  `getattr(cellpose, '__version__', '0').split('.')[0]`.
- **`app/src/tasks/segment/cellpose.jl`** — `BUILTIN_CELLPOSE_MODELS` gets a `backend` field
  (default `:v4` for existing rows); add rows `(name="cyto2", backend=:v3, ...)` and
  `(name="cyto3", backend=:v3, ...)`. `list_cellpose_models` exposes `backend` on each row.
  `cellpose_models_for_python` also returns which env to shell in.
- **`app/src/py_runner.jl` (`run_py`)** — new keyword `env=:default | :cellpose_v3`. Resolves to
  `.pixi/envs/<name>/bin/python` (Windows: `Scripts\python.exe` — use `python_bin_path`). Default
  arg keeps every existing caller unchanged.
- **Test:** `test-py` unit test for the v3 branch of `predict_slice` (fixture-based, no live
  cellpose — mock `models.CellposeModel`); `test-pkg` for the catalog and env-selection logic.

### Phase 3 — frontend dropdown + warning

- **Model dropdown** — small pill next to each name. Grep for the existing tag/pill primitive
  first; **do not** write a new one.
- **Param advisory** — register `cellposeModelAdvisory` in the same registry that carries
  `imageVersionAdvisory`. Decision matrix:
  - `platform=darwin` AND `selected.backend=v4` AND `!envs.cellpose-v3.installed` →
    `severity: 'warn'`, note "Cellpose 4 runs slowly on Apple Silicon.", trailing button
    `[Install cellpose-v3 (~500 MB)]`.
  - `platform=darwin` AND `selected.backend=v4` AND `envs.cellpose-v3.installed` →
    `severity: 'warn'`, note "Switch to cyto3 for ~10× speedup on Apple Silicon." No button.
  - Every other case → silent.
- **Pinia store** — small probe of `/api/system/envs` so the advisory recomputes when the install
  job completes. Reuse the existing job-completion event bus, don't poll.
- **UI copy** — all strings through `uiCopy.ts` (ratcheted by `uiCopy.test.ts`).
- **Test:** `test-frontend` covers the four-branch decision matrix; `uiCopy.test.ts` for the new
  strings.

### Phase 4 — docs

- **`docs/SEGMENTATION.md`** — new *"Cellpose v3 vs v4"* subsection: when each is appropriate,
  the Mac-only opt-in, the env's install mechanics, the pill convention.
- **`docs/SHIPPING.md`** — env table gains the `cellpose-v3` row.
- **`docs/todo/CELLPOSE_V4_PLAN.md`** — outcome note at the top pointing here: *"v3 revived as
  Mac-only opt-in — see `CELLPOSE_V3_OPTIN_PLAN.md`."*
- **`docs/todo/README.md`** — index row added.
- **`INVENTORY.md` / `docs/inventory/BACKEND.md`** — if `run_py` gains `env=`, note that under
  the *"Spawning Python"* entry so no one hand-rolls a second-env launcher.

## Reservations

- **Install-from-app is new ground.** No other cecelia action installs pixi envs at runtime. In
  installed-app mode the bundled `pixi` binary must be resolvable from the running Julia process
  (`docs/SHIPPING.md` decides how). If it isn't, the Install button silently fails. Verifiable
  only on a real Mac install; on `pixi run dev` it works trivially.
- **Not wet-run from here.** Everything lands in tests + a browser dev-mode smoke; the real
  verification is on an M-series Mac with the v3 env installed running cyto3 on a real image.
- **Phase 0 (measure v3 on MPS) skipped by user 2026-09-10** — premise trusted from the R-era
  history AND from the upstream evidence below.
- **First real-Mac test MUST run a v3 eval on real data, not just the mock unit test.** The v3
  code path in `cellpose_utils.py` is verified with a `unittest.mock` FakeModel — it pins the
  API contract (`channels=[cyto,nuc]`, `z_axis=0, do_3D=False`) but a stale wheel or a v3 sparse-
  tensor path that MPS still doesn't implement (#1063 was on torch 2.5.1) would only surface here.
  Do not tell users to click Install until one real M-series segmentation has produced labels.

## External evidence (2026-09-10, cecelia-e6 audit of MouseLand/cellpose issue tracker)

The "v4 on MPS is unusably slow" premise this plan rests on is **confirmed upstream**, not
inferred from one user report:

- **cellpose maintainer** (`ta-cameron`, #1269, 2025-07-18): *"cellpose4 will be incredibly slow
  this way because the cpsam models are far more complex than the previous ones. For your own
  sanity, use the last cellpose3 version instead."*
- **CPSAM's own startup banner** says the same: *"CPSAM is much larger than in previous versions
  and CPU execution is slow."*
- No open MPS issue currently reports **v3** broken. The only open MPS ticket (#1468) is v4
  bfloat16 on an *Intel* Mac (MPS not supported there at all — separate universe).

The `torch>=2.7` osx-arm64 pin in `pixi.toml` is not cosmetic:
- **#1269 / #1468** — CPSAM (v4) inference in bfloat16 raises
  `ERROR: BFloat16 is not supported on MPS` on older torch. Fixed in torch ~2.7 for inference.
- **cellpose PR #1278** (merged 2025-07-23) — float32 fallback for training under the same class.
- **#1063** — v3 on torch 2.5.1 + cellpose 3.1.0 hit
  `aten::_sparse_coo_tensor_with_dims_and_tensors` on `SparseMPS`. Likely resolved on the plan's
  `cellpose>=3.1,<4 + torch>=2.7` combo, but is exactly what an unmocked Phase 0 would have caught.

These are the load-bearing citations — do not weaken the pin without re-reading the tickets.

## References

- `CELLPOSE_V4_PLAN.md` (shipped #610) — the migration this partially reverses on Mac.
- `docs/todo/README.md` — index entry required by the parked-plans convention.
- `feedback_repair_dont_warn` (memory) — the Install button, not a paste-this command.
- `feedback_use_existing_framework` (memory) — InlineNote, existing pill primitive, existing job
  event bus. No new components.
