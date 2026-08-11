# TODO

**Open work only — things someone intends to do.** When an item is done, **delete it**; what changed
is recorded in git history, merged PRs and the release notes, not here.

**Does it belong here?** This is the tracker with the loosest edges, so it collects orphans. Before
adding, check the others:

| If the item is… | It goes in |
|---|---|
| a deliberate **non-goal**, or conditional on something that may never happen | `docs/FUTURE.md` |
| a **known-better approach set aside** (scale/ecosystem not ready) | `docs/FUTURE.md` |
| big enough to need **locked decisions + phases** before building | a `docs/todo/<FEATURE>_PLAN.md` |
| a **phase goal** for the current arc | `docs/ROADMAP.md` |
| something that **already shipped** | `docs/MILESTONES.md` (or nowhere — git has it) |
| **how a built subsystem works** | the relevant `docs/<AREA>.md` |

A fact you want recorded but that nobody should act on is **not** a TODO item. That is the drift this
table exists to stop: something worth knowing turns up, has no obvious home, and lands in the backlog.

**Referencing an item.** Items are keyed by their **title** — there are no numeric IDs. Cite one as
`docs/TODO.md` → *Title*.

> Numeric IDs (`#00042`) were retired on 2026-08-05. They existed so code could cite an item, and they
> failed at exactly that: of the eight code comments citing an ID, **four pointed at an item that no
> longer existed**. The cause was structural — completion *deletes* an item, so "increment the highest"
> was evaluated against a shrinking set and reissued numbers that plans, comments and git history still
> referenced. `#00087` ended up meaning two different things; `#00003` was a known duplicate. Meanwhile
> `docs/FUTURE.md` (keyed by title) and `docs/todo/*_PLAN.md` (cited by path) never collided once.
>
> Every citation in the docs and the code was repointed at the same time. `docs/prompts/` is left as
> written — those are frozen records of finished work, so a retired number there is history, not a
> broken pointer.

**From code, prefer the permanent reference.** A `docs/<AREA>.md` section or a `docs/todo/X_PLAN.md`
path cannot dangle when the work ships, which is what a TODO citation does by construction. Cite the
tracker only for work that is genuinely still open, and by title.

Items marked **🔹 needs-input** need something only Dominik can provide — a test asset, a
domain-specific expected value, or a decision an agent shouldn't make alone. Grep `needs-input`.

---

## Next up

### Per-notebook reset (re-run a notebook on new data without killing the Pluto server)
Pluto has no filesystem watcher, so a notebook keeps rendering **stale data with no visible sign**
after a pipeline task rewrites its inputs. The `DATA_STAMP` convention
(`docs/NOTEBOOKS.md` → *Refreshing after a pipeline re-run*) is the working answer today, but it is
manual and easy to forget. The only reset currently available is the Notebooks page's **Restart**,
which stops the whole Pluto server and takes **every** open session (and their unsaved edits) with it
— which is what prompted this (Dominik, 2026-07-29).

**Mechanism, verified against the pinned Pluto (`Pluto/F6SNP`, `src/webserver/Router.jl`):**
- `GET /notebooklist` → a Dict of `notebook_id => path`. **MsgPack-encoded** (`pack`), not JSON, and
  `api/` has no MsgPack dep — so prefer the redirect route below over adding one.
- `GET|POST /shutdown?id=<uuid>` → `SessionActions.shutdown` for **that one session**.
- `GET /open?path=<abs>` → 302 to `/edit?id=<uuid>`, so the id can be read from the `Location`
  header with no new dependency. (Opening an already-open path returns the existing session.)

So: `POST /api/notebooks/reset { projectUid, file }` = resolve abs path → `open` without following
redirects → parse `id` → `shutdown?id=` → return ok. Then the row's existing **Open** runs it fresh.
UI: one per-row button in `NotebookTable.vue` (`pi-replay`), beside Snapshot/History.

**Risks to handle, not ignore:**
- These are Pluto's **desktop-app** endpoints ("normally shutdown is done through Dynamic.jl" per its
  own comment) — semi-public, not a stability guarantee. Fail gracefully and pin the Pluto version
  the behaviour was verified against.
- A reset **discards unsaved in-session edits**. Snapshot first, the way `/api/notebooks/revise`
  already does, so nothing is unrecoverable.
- `open`-then-`shutdown` on a notebook that was not running starts a session just to kill it. Cheap,
  but check `/notebooklist` first if it ever matters.

Related, and a bigger decision: **`PlutoUI` is not in `pluto/Project.toml`**. Adding it would give
`Button` (an in-notebook refresh) *and* sliders for a timepoint, but costs a re-resolve of all three
manifests. Decide that separately.

### Ship a prebuilt Notebooks sysimage in the bundle (release optimisation)
Build-on-demand already covers every user: the Notebooks page's **Enable fast plots** builds
`pluto/deps.so` in the background and re-stamps it when Julia/deps move. What is left is the
packaging half — once the constructor/pixi packaging pins Julia per platform, build the `-full`
variant in CI and ship it for the primary OSes so even the *first* open is instant. It falls through
to the on-demand path wherever no prebuilt image is present, and the freshness stamp means a shipped
image that predates the user's Julia/deps self-heals. Belongs with the packaging phase in
`docs/ROADMAP.md`; not urgent, since one on-demand build already gives every user a fast cache.

### Set-scope / incremental node subprocesses not killed on chain cancel
The per-image cancel path kills running subprocesses. Set-scope (`_run_set_scope_node!`)
and incremental (`_run_incremental_node!`) runners call the multi-image `_run_task` directly with
`on_process = _ -> nothing` and are **not** registered in `_TASKS`, so `cancel_chain_run!` can't
reach their subprocesses mid-run (the between-node flag still stops not-yet-started ones). No real
set-scope subprocess task exists yet (only mock/plot tasks), so impact is currently nil. When the
first real set-scope subprocess task lands (e.g. HMM training), give the multi-image `_run_task`
path a `TaskRecord` + `chain_run_id` so it's cancellable like the per-image path. Low priority.

### Segmentation still runs on the empty planes a drift correction padded in
Drift correction expands the canvas and pads with zeros. Measured 2026-07-31 on `4kS67f`
(201×20×544×548), **z 0–2 and z 16–20 are all-zero across every channel** — 8 of 21 planes, and the
padding MOVES per timepoint since the shift differs per frame. A cellpose run segments all 21, so
roughly **38% of the GPU time on that image produces nothing**, and measurement/tracking then carry
the empty planes too.

`zarr_utils.read_valid_box` (#435) already answers *which part of a store is data*, per timepoint, at
any pyramid level, and the preview worker and the smoothing/drift runners consume it. What is left is
only the decision to skip that work in segmentation:
- **Is skipping safe for stitching?** `stitch_threshold` links labels ACROSS z, so dropping interior
  planes would be wrong. The empty planes here are leading/trailing, which is the safe case — but that
  needs checking rather than assuming, per image.
- **Do NOT crop to the box.** Each frame sits at its own offset *because* the correction aligned them
  in a shared canvas, so cropping per frame puts them back out of register — and the intersection
  across timepoints is EMPTY on four of the nine `kSUFux` movies (z-drift exceeded the 8-plane stack).
  The box is for masking statistics and skipping known-empty work, never for cropping.
- **Does the win generalise**, or is it specific to how much drift a movie has?

### `_compute_iou_matrix` is quadratic in cell count, and every nuc+cyto run pays it per frame
`SegmentationUtils._compute_iou_matrix` (`python/cecelia/utils/segmentation_utils.py`) compares every
cyto label against every nuc label with a **full-plane boolean op per pair** — `len(a) × len(b)` array
comparisons. Measured 2026-07-31 on one 590×590 plane: **1.8 s at 100×100 labels, 26.9 s at 400×400**.
It's called from `_match_nuc_cyto` once **per timepoint**, so a 201-frame two-model movie with ~400
cells/frame spends on the order of **90 minutes** just re-assigning label IDs.

Found while deciding whether the task preview could afford to run the matching step (it can't — that is
why the preview is base-model-only and says so; `docs/todo/TASK_PREVIEW_PLAN.md`). But the cost is paid
by the real pipeline too, which is the part worth fixing.

The fix is standard and O(pixels) rather than O(labels²): one co-occurrence histogram over the paired
label maps — `np.bincount(a.ravel() * (b.max() + 1) + b.ravel())` (or `scipy.sparse.coo_matrix`) gives
every pairwise intersection in a single pass, and the union follows from per-label totals. Same IoU
numbers, so the existing `match_threshold`/`removeUnmatched` behaviour is unchanged — which means it can
be pinned by asserting the new implementation matches the current one on a fixture before swapping it
(the same oracle trick #435 used for the drift refactor).

### Export an image version as OME-TIFF (so it opens in Imaris with the right voxel size) 🔹 needs-input
Nothing in the codebase writes a TIFF today (no `tifffile.imwrite` anywhere). The need is that people
render figures in **Imaris**, not napari, and Imaris cannot read our zarr stores. The `.ccbundle`
export is a different thing entirely (whole-project archive, tar-per-store, for moving a project
between machines).

**The route this replaces** (Dominik, from the old framework): export OME-TIFF → open in ImageJ →
re-export as plain TIFF → run Imaris File Converter — and the converter then **did not pick up the
pixel sizes correctly**. That symptom has a known cause and it is the ImageJ hop: a plain TIFF has no
place to record **Z spacing** (ImageJ smuggles `spacing=` into the ImageDescription string, which the
converter doesn't read), so voxel size in Z is exactly what goes missing on a TIFF-series import.
Newer Imaris (≥9.9) at least *prompts* for a missing voxel size instead of silently guessing.

So the whole point of this task is to delete both hops, and it is only worth building if the
calibration survives. That makes the metadata the requirement, not a detail:
- **Write OME-TIFF, and write the full `<Pixels>`** — `PhysicalSizeX/Y/Z` **with units**,
  `TimeIncrement`, channel names. We already hold all of it (`ome_xml_utils`, and see *Calibration —
  three copies, one stamp* in `CLAUDE.md`; `ccid.json` is the authoritative source, not the store).
  Z is the one that breaks, so it is the one to assert in a test.
- **The reader to satisfy is Bio-Formats, not Imaris directly.** Imaris File Converter does its
  reading through `ImarisConvertBioformats`, an open-source CLI/plugin that wraps **Bio-Formats** and
  writes IMS (`github.com/imaris/ImarisConvertBioformats`). So a correct OME-TIFF is the input for
  both destinations — `.ims` needs no work from us beyond that, using a tool the user already has
  installed with Imaris. Test against Bio-Formats' OME-TIFF reader, not against a guess.
  **Don't shell out to that converter from cecelia.** It ships installers for Windows and macOS only;
  Linux is a supported *build* target but needs HDF5, zlib, lz4, FreeImage, Boost, a JDK+JRE and the
  Bio-Formats jar assembled by hand. Imaris itself is Windows/macOS, so whoever wants IMS already has
  the converter on the machine that matters. Our deliverable stops at the OME-TIFF.
- **Decide the metadata flavour, and verify it once in Imaris.** `tifffile` cannot write `ome=True`
  and `imagej=True` at the same time, and some readers prefer the TIFF `XResolution`/`YResolution`
  tags over the OME-XML when they disagree. Which combination survives end to end needs Dominik to
  open one output in Imaris. **🔹 needs-input.**
- **Do NOT reach for `PyImarisWriter`** to write `.ims` from Python. Checked 2026-08-11: last release
  0.7.0 (2021-08-13), a `py3-none-any` wheel that ships **only Windows DLLs** (`bpImarisWriter96.dll`)
  behind ctypes, classified `Operating System :: Microsoft :: Windows`. It cannot work on Linux or
  macOS, so it fails our cross-platform requirement outright. The C++ `ImarisWriter` it wraps is alive
  but ships no releases — building it per platform is not worth it when the bullet above makes it
  unnecessary.
- **What to export**: which image *version* (versioned-field picker, like every other task), which
  channels, and whether a z-MIP / single timepoint is enough — a full `201×21×4×544×548` uint16 movie
  is ~9.7 GB as one file, which needs the BigTIFF flag and may not be what anyone wants.
- **Where it lands**: not inside the project tree (it's an artefact, not data), so it wants a
  destination picker like `default_export_dir()`. Task rail + progress, staged output.

This is a **write** path, so it's unaffected by `open_as_zarr` dropping its TIFF *reader* — an export
reads the store through the canonical reader and writes out.

### `testTasks.*` task fun_names/files are still camelCase
The test tasks `testTasks.imageTask`/`testTasks.setTask`/`testTasks.incrementalPlotTask` (files
`tasks/testTasks/{imageTask,setTask,incrementalPlotTask}.{jl,json}`, structs `TestImageTask`/
`TestSetTask`/`IncrementalPlotTask`) predate the snake_case convention (see
`docs/DEV.md` and the Julia naming rule in `CLAUDE.md`). Rename to snake_case `fun_name`s + files (e.g. `testTasks.image_task`,
`tasks/test_tasks/image_task.{jl,json}`) — structs stay PascalCase. Touches `_spec_path`/
`_fun_name_map` in `task_registry.jl`, the `Cecelia.jl` includes, and any test references. Not
important (test-only scaffolding, no user-facing impact) but should be fixed for consistency;
batch it rather than churn standalone.
