# Changelog

All notable changes to Cecelia Feijoa are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
While the project is pre-1.0, minor versions may contain breaking changes.

Cecelia Feijoa is a ground-up reimplementation of the original R/Shiny
[Cecelia](https://github.com/schienstockd/cecelia-legacy) in a Julia + Python + Vue
stack. Per-tag notes are also on the
[GitHub releases](https://github.com/schienstockd/cecelia/releases) page.

## [Unreleased]

_Changes on `main` that have not yet been tagged in a release._

## [0.1.3] — 2026-08-15

37 pull requests since `v0.1.2`. Still `0.1.x`.

**Read the first three sections before re-opening old work.** Several changes correct results rather
than add anything: autofluorescence correction could erase a channel, asking for one segmentation
could return another's cells, and the store layout `v0.1.1` announced is reversed. Anything derived
from those paths should be re-checked, and one class of saved canvas layout does not survive the
upgrade.

### Changed — autofluorescence correction

- **AF correction could erase a channel.** Where one channel leaks into another, the leak was removed
  by the *dominance weight*, which scales rather than subtracts, so every voxel carrying both channels
  read as the brighter one's. Measured on `WIaUjL/p6t4mC` (CH3 leaking 2.3% into CH2, ~7x brighter above
  background): corrected CH2 came out 98-99% zero, and segmenting it found the residue at the bright CH3
  spots — i.e. it found CH3. The preview was faithful throughout; the correction under it was broken.
  Bleedthrough is now unmixed as an amount *before* the weight, and a channel identified as a leak
  source is dropped from the weight's denominator. **Anything AF-corrected before this is wrong, along
  with what was derived from it** (#555).
- **The bleedthrough coefficient is estimated per channel combination, and which estimator is right is
  a question about the specimen.** Each combination carries a **Different cell types** switch, on by
  default: on → the total slope (with nothing co-labelled, the whole proportional relationship is leak);
  off → the lower envelope. The previous release used the envelope for everything, which under-corrected
  mutually exclusive reporters (0.0248 where the slope gives 0.113). The default's cost, tested: a
  genuinely co-labelled experiment left on it over-subtracts (+80% on synthetic co-positive data) (#559).
- AF correction reports real progress instead of one number for the whole long stage (#559).

### Changed — store format

- **Chunk keys are always nested now; the flat layout is removed.** A flat-keyed store conforms to no
  published OME-NGFF version — nested storage is what NGFF 0.2 introduced, so flat keys are 0.1
  *storage* under the 0.4-shaped metadata written beside them, which is why bioformats2raw stamped `0.1`
  on such imports while our own writers stamped `0.4`. This reverses the default `v0.1.1` announced.
  Re-measured, flat saved ~5% on a real 3.5 GB movie (not the ~14% its 81 MB fixture suggested) at
  identical read time (#551).
- **Reading is untouched — every existing store still opens and nothing needs re-importing.** The
  separator is self-describing (v2 `.zarray` `dimension_separator`, v3 `chunk_key_encoding`). Settings →
  Storage still chooses the layout, now between **zarr v2 · nested keys** and **zarr v3 · sharded**.
- **Derived stores no longer inherit the separator**, so a correction of a legacy flat image comes out
  nested; the zarr *format* is still inherited. Flat stores already on disk keep claiming NGFF 0.4, and
  there is no converter — `rechunk_zarr.py` preserves a store's layout — so a re-import is the only fix
  if that label matters.

### Changed — segmentation results

- **Asking for one segmentation could return another's cells.** `pop_df` with `pop_type="labels"`
  ignored the requested populations entirely and always read the image's **active** segmentation, so on
  a two-segmentation image a request for `Neutrophil` returned `Tcell`'s cells — the wrong data under
  the right label, with no error. Comparing two label sets was not expressible at all. The value_name
  prefix now selects the segmentation, several pool in one call, and a value_name absent on a given
  image is skipped rather than raising. **This is shared code, so it reaches anything reading raw
  segmentation output, not only the QC plot it was found in** (#564).
- **A fresh 3D segmentation stitches its z-planes.** Both segmentation tasks shipped
  `stitchThreshold: 0.0` in the model-group default — the row that seeds a new task — so a new 3D run
  segmented each plane independently while the Advanced spec said 0.2. **Only new tasks change; a saved
  task keeps its stored value** (#553).
- **Segmentation honours the valid box on Y and X**, not just Z, so padded regions introduced by drift
  correction are no longer segmented (#567).
- **Every image-version picker now says when a run would read a version the image is not on.** Segmenting
  `WIaUjL/p6t4mC` on `default` (512×512 raw import) while it was active on `afCorrected` (605×617, the
  drift-corrected canvas) reported **done**, banked 92 374 cells, and laid a 512×512 label store over a
  605×617 image — every cell displaced in XY, with nothing in the app saying the run and the view
  disagreed. The picker now warns, names the active version, and stays silent when only one version
  exists (#575).
- **Smoothing offers `gated` as a third temporal statistic**, preserving sharpness where the median
  smears moving cells: 45% of noise removed at punctum amplitude and motion sharpness 1.00/1.01, against
  0.85/0.91 for `median(5)`. `median` stays the default — `gated` is measured on 30 s intravital data
  only, costs ~0.12 s/plane, and the photon-limited case the task was built for is untested with it (#554).
- Optical-flow training handles a whole set — ragged pool, bounded memory, cropping, z-spacing (#549).

### Changed — saved state and on-disk layout

- **The module summary canvas is scoped to the set, not to whichever image was first in the selection.**
  The old key tied both the saved layout and what got plotted to the first ticked image, so re-ticking
  silently swapped the canvas for a different image's plots, and ticking five images showed one.
  `compareMode` now seeds to **per-image**. **Saved module-page layouts from earlier versions are not
  ported** — there is no honest merge from N per-image layouts into one, so each set starts from an
  empty canvas. The stale entries stay on disk, unread. Gating canvases remain per (image, value_name),
  because a gate does belong to one image (#568).
- **The task run log records a run twice — at start and at finish — so a killed run leaves a trace.**
  It was append-on-finish and skipped `:cancelled`, which is also how a task ends when its process is
  killed, so 22 minutes of GPU segmentation could end with nothing written down anywhere. A new
  `running` status means "started, outcome not yet known" (#574).
- **Per-image task subdirectories are created only once something writes to them.** Ten directories were
  pre-created for every image; six of them (`populations`, `stats`, `shapes`, `models`, `out`, `cl`) were
  empty on all 30 images in the dev projects dir and have no writer anywhere in the codebase (#579).

### Added

- **A detached task runner** (dev only, off by default). Tasks and chains run in a second process, so a
  backend restart no longer kills work in flight: Restart leaves it running, Quit takes it. It keeps the
  code it started with and flags "old code" with the commit when behind (#543).
- **Output names are a first-class control, and pick their settings back up.** Re-running a segmentation
  as `Tcell` when the last run was `Neutrophil` meant retyping the output name and every model parameter.
  A new `SuggestInput` opens on focus showing the names already in use and narrows as you type — for
  output names, image-attribute values and movie tags — and choosing one restores that name's parameters.
  Existing projects are covered by falling back to `runlog.json`, which has always recorded each run's
  params (#573).
- **Sets can be renamed**, with one duplicate-name guard behind every path that names a set — including
  the REPL. This also fixes `copyImage` creating a second set with an identical name instead of reusing
  the existing one (#563).
- **Plots facet by image.** `Facet by` — None / Image / Series — replaces the on/off toggle; the
  time-series chart could not facet at all, so five images × two segmentations was ten overlaid curves on
  one axis. A chart that cannot honour the setting now says so instead of silently ignoring it (#569).
- **`find_object` resolves a uid to its project in one call**, so a uid quoted in a note or a filename no
  longer means listing every project until one matches (#571).
- **The observer briefs its own session** — the MCP server describes what it can do, so the in-app prompt
  is one line; boards can compare by attribute (#546).
- **"Close all" on every plot canvas**, armed before firing, scoped to the canvas's current key (#566).
- Movie comparisons can be **wrapped into a grid** — four movies as 2x2 instead of a wide strip (#558).
- Optical-flow plots export their contact sheets (#562).
- Task parameter options can carry a note, and the smoothing methods explain what each is for (#556).
- The task console shows which project each row belongs to (#547).
- Imaris `.ims` import repairs soft-linked sources and recovers the frame interval (#548).

### Fixed

- **Quit, Ctrl-C and `pixi run stop` now actually stop the app.** Four separate pieces of Julia 1.12
  signal/exit behaviour meant stopping left processes running behind hundreds of lines of backtrace that
  read like a crash: `exit()` segfaulted with a worker mid-compile and the supervisor read the fault as a
  crash and **relaunched the app you had just quit**; a SIGTERM did not stop a process whose threads were
  not at a safepoint, while `pixi run stop` printed "stopped" regardless; Ctrl-C left the task runner
  orphaned on :7657 every time. The backend now runs detached and is asked to stop first, escalating only
  if ignored, and the 18 per-OS shell one-liners behind `stop` are one tested Julia file (#580).
- **Task state survives a backend restart.** The image-table badge picked whichever run sat first in an
  array rather than rolling up an image's runs; a run that started while the socket was down stayed
  Queued forever; and the task log came back truncated because the backfill was skipped for any row that
  already had lines. A **Reload log from disk** button reads a run whole (#577).
- **Killing an already-finished process segfaulted the server** — the pid came from a raw libuv call that
  dereferences null once a process is reaped, which is the common case when cancelling a task whose
  Python just exited, and a segfault is not catchable. The supervisors now also relaunch the backend
  after a crash instead of treating it as a quit, leaving the runner and viewer for the fresh server to
  adopt (#560).
- **Component output is no longer discarded.** `run(cmd; wait = false)` swallows both streams to
  devnull, so the napari bridge, the preview worker, the task runner and Vite were writing to nowhere —
  not the console, not the terminal. Every component now logs onto one rail (#572).
- **The segmentation QC plot's per-timepoint view was unreachable**, and three further defects kept it
  dark without erroring: the time axis was never offered (the spec named a column renamed long ago), the
  trend line drew negative counts where LOESS overshot a cliff, and the count axis still read `count`
  with Fraction on. The x axis now reads `Time (s)`, converted per image, and stays in frames when the
  interval is unknown rather than assuming 1 s (#564).
- A task's progress frames could write into a project that was not its own (#550).
- The live task preview is keyed by the output label name, not the input version (#552).
- A parameter load that did not happen no longer resets the form to defaults (#559).
- QC findings say which task raised them (#561).
- An exported SVG references its raster with `xlink:href`, which SVG 1.1 renderers read (#562).
- The padded-skip spans are reported per timepoint in the log, which is what they always were — the
  message just printed frame 0's numbers once. Log only; nothing segments differently (#570).

### Performance

- **Segmentation measurement was `O(cells × volume)`.** `_extended_3d_measures` ran marching cubes over
  the entire label volume once per cell, which scales the wrong way twice. On `WIaUjL/p6t4mC`,
  measurement was two-thirds of a `segment.cellposeMeasure` run: 100 253 cells took a projected 3.0 h
  against 10 min for 5 216. Meshes now come from the shared `mesh_utils.build_label_meshes` — the slow
  copy was the duplicate. **60.32 s → 1.94 s per timepoint (31x); that image's measureLabels 3.0 h →
  5.8 min**, with all 21 columns bit-identical over 719 real cells (#567).
- **The optical-flow module's plots answer the t and z sliders about 3x faster.** Nudging a slider on
  the flow-metrics sheet or the model-probability map took ~3.1 s to redraw and sent a 7.3 MB reply;
  it is now ~1.0 s and 3.6 MB, measured on a 181-frame 4-channel movie at the default 512 px crop.
  Two causes, both pure overhead: the temporal window the metrics are built from was read one frame
  at a time through a lazy array handle, which rebuilt a task graph per frame (12x slower than
  reading the same pixels directly), and each plane's colour map was expanded to full RGB before the
  PNG was encoded rather than being written as the PNG's own palette. The pictures are unchanged —
  byte-identical once decoded. The coastal segmentation preview read its window the same way and got
  the same fix (#565).
- Gated smoothing is 3.8x faster: one gate for all channels, which it had to be anyway since the AF
  weight is a cross-channel ratio (#557).

### Internal

- Both task lists render through `SelectionTable`, the canonical list, instead of two independently
  hand-rolled ones that shared no markup, selection idiom or selection colour (#576, #578).

## [0.1.2] — 2026-08-13

A fix release. Fresh installs of `v0.1.1` did not launch — if you hit
`pluto/sysimage_stamp.jl not found`, this is the fix. Update in the app, or re-run the installer.

### Fixed

- **The release bundle was missing three directories the app loads at runtime**, so every stable
  install failed on first launch: the API server reads a file from `pluto/` while starting up, and
  `preview/` (task preview) and `mcp/` (the AI observer) were absent by the same gap, failing more
  quietly. The `dev` channel was never affected, which is why it took an install to find (#540).
  Reported by @lxfhfut, with the cause and the fix.
- **Packaged launches no longer print `fatal: not a git repository`** to the console. The dev
  diagnostics probe git on startup; in an installed app there is no repository to find, and the
  failure was harmless but read like a broken install.
- **Smoothing reports progress while it runs.** It stood at 50% through the whole streaming stage —
  the long one — and now advances per z-plane.
- **The daily lab-log digest no longer calls a task failed when a later re-run succeeded.** A day
  where four images failed and were all re-run successfully was headed ❌ *"4 failed"*; it now
  reports where the day left each image, and notes the retry separately.

## [0.1.1] — 2026-08-13

60 pull requests since `v0.1.0`. Still `0.1.x` deliberately — this is the framework's first iteration
and it is still finding its shape, so the minor bump waits for it to settle rather than for the next
substantial change.

Two sections lead because they are the only parts a user can act on wrongly: the **store format**, and
**how images are corrected and segmented**. Everything else is additive.

### Changed — store format (read this first)

- **Zarr v3 / OME-NGFF 0.5 stores can now be written.** Opt-in and **not** the default: the default
  stays flat-key v2, chosen from a measured storage-vs-access table in Settings → Storage (v3 costs
  ~14% less on disk and reads ~40% slower). Import picks the format and **every derived store inherits
  it**; there is no converter.
- **A store written as v3 cannot be opened by `v0.1.0`.** v3 *reading* landed in this same cycle, so an
  older install has no reader for it. Nothing forces the choice, but it is a one-way door per store.
- **New imports are little-endian** (`<u2` rather than `>u2`), following bioformats2raw 0.12 — which
  retires the byte-order class of bug entirely. Existing stores are untouched and still read correctly.
- **The NGFF version is now stamped** on the stores we write. Anything written before this has none,
  which is why the metadata modal reports it per stored version.
- Chunk-key separator and shard depth are import settings, inherited by derived stores like the format.

### Changed — drift correction and segmentation (read this second)

- **Drift correction has a new default estimator, `multiLag`, and it produces a different trajectory
  than `v0.1.0` did.** It estimates from redundant frame pairs rather than chaining frame-to-frame, so
  an error in one pair no longer propagates through the rest of the movie. Re-running a correction you
  already ran will not reproduce the old result — it should be a better one, but it is not the same
  one. On the drifting test movie: 105.0 s → 56.1 s and the padded canvas 9.21× → 3.51×. On a clean
  movie the difference is small (42.4 s → 39.8 s, 1.05× → 1.02×). The previous chain algorithm remains
  selectable.
- **Segmentation now skips the empty z planes a drift correction padded in**, and the valid box
  survives smoothing and correction rather than being dropped or widened to the union over frames — so
  the saving is reachable from the version people actually segment. Measured across 17 corrected
  stores here: ~24% of plane-frames skipped, range 3.1–55.6%. Those figures describe **this machine's
  data**, not the feature.
- **Every Python task now runs with a bounded BLAS thread pool** (4 threads, set by the launcher).
  Uncapped, a single drift task took every core on a 32-core box, so concurrent tasks fought each
  other: four together went 309.7 s → 70.7 s, and one alone 56.3 s → 31.8 s, with an **identical**
  residual at every setting. This is pure overhead removed, not accuracy traded.
- **The Import page is now Manage images.** It hosts add/copy/move/delete and export alongside the
  import tasks, so the old name described a third of it.

### Added

- **Movies are a managed collection, not a directory listing** — a per-project registry under
  `settings/` (so it travels with a `.ccbundle`), an in-app player, star / tags / rename / delete and
  bulk actions on a checked selection, and a sortable, resizable, collapsible list. A movie banks the
  config that produced it, so it can be **reopened for editing on the page that made it**.
- **The movie list shows its source image** — channel and attribute columns behind a Details toggle,
  with the image table's attribute filter (now one shared control, `AttrFilterPanel`).
- **Side-by-side comparison movies** — image versions across the columns × segmentation masks down the
  rows, recorded one pass per cell and composed into one file.
- Movie options throughout: explicit output size, frame range, title cards, mask outline width,
  filename suffix and attribute-based naming, and recording on the task rail with progress + cancel.
- **The Animation page picks its own image**, having become a proper module page.
- **A LabArchives notebook can be linked to a project.** The person analysing the images is often not
  the one who ran the experiment, and the design — cohort, protocol, the question being asked — lives
  in a lab notebook nothing here could see. Cecelia never talks to LabArchives and deliberately never
  learns how: Claude reads it through the user's own authenticated session and hands over a summary,
  which is cached beside the data and carried into the next session's briefing and the GUI.
- **Export an image version as OME-TIFF**, for the people who render figures in Imaris and cannot read
  a zarr store. The point is the calibration, not the file: `PhysicalSizeX/Y/Z` with units,
  `TimeIncrement` and channel names are written from `ccid.json` — the authoritative copy — and an
  unknown size is **omitted rather than defaulted to 1.0**. The old route (OME-TIFF → ImageJ → plain
  TIFF → converter) silently lost Z spacing, because a plain TIFF has nowhere to record it.
- **In-app guides** — bubble walkthroughs of the basics, on your own data, from a compass in the
  header. A guide points and observes; it never clicks, selects or runs anything. Prerequisites are
  checked live and shown before you start, never enforced. There is also an orientation tour of the app
  itself, which starts once on a first launch, and GitHub / Zulip links beside the compass.
- **Optical-flow segmentation** — a training task, a model vault page, and a preview backend. **Early
  and still moving**: it ships here because the import and correction changes above should not wait
  for it, not because it is finished.
- The HMM state-frequency and transition-matrix plots **discover their measures from the data** instead
  of offering a hardcoded `movement` suffix.
- **The MCP observer can author analysis boards** (add-only; the user keeps them) and **design chain
  templates it cannot run**, and it can see image attributes and existing boards.
- Chain whiteboard: automatic layout for a DAG with no saved positions, plus a Tidy button.
- Gates are positioned in µm, through one pixel scale.

### Fixed

- The Movies page rendered blank (a temporal-dead-zone throw in setup), and the detector gap that let
  it through is closed.
- The analysis board re-rendered itself to Vue's recursion limit; a client reloaded its own write on
  every autosave; the read-back could not tell two boards apart. Boards are now versioned so two tabs
  stop clobbering each other.
- Skeletons vanished from a recording exactly like masks did, and a mask's outline width did not reach
  napari — so recordings came out filled.
- The mp4 writer leaked a staged `.tmp.mp4` on a failed or cancelled render.
- Movie title cards rendered every non-ASCII character as a box.
- A re-import reverted renamed channels; the import chunk-size parameter was wired to nothing.
- **The Metadata panel's "Original path" regex read the wrong path** — it fed the *converted* store
  name (`ccidImage.ome.zarr`), identical on every image, so the option could never do what it exists
  for. It now reads the recorded source location, and the builder gained a `/ folder` separator,
  2nd/3rd-last field positions (an absolute path has a variable number of leading folders, so counting
  from the start is useless), and a `strip extension` that applies only to the last field — applied to
  a `2026.07.16` date folder it had matched nothing at all.
- Column widths in `SelectionTable` were squeezed below what was specified while the frozen-column
  offsets were computed from the specified values, so sticky columns sat misaligned — visible on the
  Movies table once the Details columns ran past the panel.
- **Copying a corrected image version could lose every channel name.** Channel names are usually
  registered only under `default` while a processed version carries none of its own, and the copy read
  the version's own field with no fallback — so the copy came out with no names at all.
- **`segment.coastal` died at the first timepoint** on any drift-corrected image once the padded-plane
  skip landed: the tile was narrowed to the valid z range but the temporal window was still read at
  full depth, and the mask came back the wrong shape.
- **The flow-metrics panel worked on exactly one image** and failed everywhere else with `message too
  large`. Not the image — a websocket frame cap nobody had set on the Julia side, where the default is
  16 MiB and a whole-frame reply is ~36 MB. One cap now covers all four ends of the napari and preview
  legs, which removes the same latent failure from the AF and segmentation previews over a large view.
  The panel also renders a **centred crop** (512 px by default, 256/512/768 selectable) rather than the
  whole frame — a crop, not a downsample, because the panel's claim is that these are the planes a run
  is actually fed.
- The guide picker declared "needs a tracked image" for projects migrated from the R version — it
  scanned the run log, which records what *this app executed*, instead of asking what tracks are on
  disk. Same substitution as the earlier "no imported images" report.
- The IoU hot path in the label-matching code, and a `testTasks` name that no longer described it.

### Infrastructure

- **Release notes now come from `CHANGELOG.md`.** They used to be GitHub's auto-generated pull-request
  list — which is also what the in-app What's New modal rendered, so the app's answer to "what
  changed" was a list of branch names. This section is what you are reading in both places.
- The notebook table takes the shared resize path (drag-to-resize, persisted widths, a reset).
- One write-behind autosave helper for the three stores that each had their own, `rafCoalesce` for
  paint-rate work, and a written-down coalescing rule with detectors for the two ways it breaks.
- Three more canonical-helper bypasses fixed, with detectors for the two that keep recurring: reading a
  versioned field without its active-version fallback, and treating `exitcode == 0` as success for a
  process that was signal-killed.
- The first-use hint callouts on module pages are gone. Three of them asserted a prerequisite, which is
  a question the app can answer live and a static sentence gets wrong for the user who already met it.

## [0.1.0] — 2026-08-05

**The first plain release.** Everything before this was an `-rcN` snapshot; nine of them never
converged, and because Julia sorts an `rc10` prerelease *below* `rc9` as a string, no further rc could
have reached an installed client at all. This tag ends that: it outranks every prerelease, and
`releases/latest` resolves for the first time.

498 commits since `v0.1.0-rc9`. Highlights:

### Added
- **Statistics on summary plots** — between-group hypothesis tests (Mann-Whitney / Kruskal-Wallis by
  default, t/ANOVA opt-in), Prism-parity brackets with p-value labels, Compact Letter Display as
  hoverable HTML overlays that survive PDF/PNG/SVG export, a Compare-groups toggle, which test `auto`
  picked, a sibling `{name}.stats.csv` per plot, and raw+stats zipped into one download.
- **Task preview** — a resident worker (`:7656`) runs a task's *own* compute over the visible region so
  parameters can be judged before a full run. Previewability is a declared task trait; re-previews are
  debounced on view change with a visible state, contrast windows survive moving T/Z, corrected
  channels keep their original's name and colour, and a failure says why somewhere readable.
- **Branching / skeleton analysis** (`segment.branching`) — skeleton extraction, a `branch` pop type
  with per-branch-type filter populations, anisotropy via `skimage.feature.structure_tensor` expressed
  in µm, napari skeleton + branch-type visualisation, and notebook readouts for the orientation field.
- **Spatial analysis + region clustering** — neighbour graphs, neighbourhood-composition region
  clustering with a `region` pop type, cell–cell contact statistics, and a CODEX-style contact heatmap.
- **Structured image delete** — one modal on the Import page with four scopes (whole images, versions
  with the new active picked, label sets with their companions, all analysis), replacing five deletion
  entry points spread across four screens. `reset_image_analysis!` drops derived output while keeping
  the image and never touching gate definitions.
- **Image file operations on the selection** — Copy / Move / Delete in the Import action bar instead of
  hidden at the end of each table row, applied to every checked image at once.
- **Copy an image version into a new image/set** (`editImages.copyImage`) — a re-import shortcut.
- **Custom cellpose checkpoints are drop-in** — `ccia.fluo` (fluorescence, for dendritic/SHG stroma)
  is fetched at install time; any checkpoint dropped into the models dir appears in the picker without
  a restart.
- **What's New + tip of the day** — a release-notes modal on launch, with feijoa sketches on the cards.
- **Observer / MCP** — one-click terminal setup for Claude, shadowed-entry detection and repair.
- **A set reference image**, nominated by the user, plus axis-based task gating (replacing the
  project-wide static/live distinction).
- **QC**: clipped-detector channels flagged on every import; AF's derived ceiling banked.

### Changed
- **The acquired bit depth is kept** — the 16→8-bit import conversion is gone as a non-goal, and the
  store codec is now an explicit, measured decision surfaced in Settings.
- **Every Julia↔Python boundary is versioned**, not just the preview worker.
- **UI unification** — one `.cc-btn` family, one `CcToggle`, one task-status colour map, semantic
  scenario utilities (`.cc-muted` / `.cc-readout` / `.cc-empty` …) with detectors that fail the build
  on a hand-rolled variant, and a tooltip *coverage* ratchet on top of the length one.
- **Storage reporting** — the Settings storage box now accounts for derived analysis, not just images.

### Fixed
- **`rc10` sorted below `rc9`**, so the updater told every client it was up to date. Prerelease digits
  now compare numerically.
- **Staged updates apply on every launch**, not only the first.
- **A worktree's Python env could resolve `cecelia` into a different checkout** — `PYTHONPATH` is
  pinned per checkout, and the shutdown path now stops every resident child.
- **Calibration**: the time scale reaches NGFF metadata and napari; both zarr layouts resolve when
  syncing; OME-XML is written into staged stores instead of being silently skipped.
- **Python text I/O declares UTF-8**, so Windows stops decoding sources as cp1252.
- **A store is identified by structure, not file extension**; the debris sweep finds what is
  incomplete rather than what is named `.partial`.
- Numerous AF-correction, import-window, QC-text, tooltip-layout and task-param fixes.

## [0.1.0-rc9] — 2026-07-21

### Added
- **Spatial analysis + region clustering** — squidpy neighbour graphs, neighbourhood-composition
  region clustering with a new `region` pop type and cross-poptype region queries, cell–cell contact
  statistics (Julia kNN points + trimesh mesh), and a CODEX-style log-odds contact heatmap on a new
  Spatial Analysis page (with napari region colouring and per-timepoint behaviour regions).
- **MCP observer** — read-only Claude access to a running project: session briefing, synthesized
  analysis lineage, population definitions, per-population phenotype/motility summaries, HMM behaviour
  + cluster summaries, spatial stats, cohort QC, whiteboard chains, task-param specs, and board plot
  types; plus notebook tools (create / revise / describe / read) and the REPL data-access surface.
- **In-app AI observer** — Ask/Chat-to-Claude with token readout, sessions, activity log, on-demand
  (non-auto) operation, and a "What Claude can do here" overview.
- **QC system** — objective per-task metrics from segmentation/measure/tracking/clustering/behaviour,
  cohort-consistency checks (median/MAD outliers, flags at n=3) banked per-image, a colour-blind-safe
  severity model, and `[Cecelia]` lab-log digests.
- **Per-project lab log** — auto activity digests, mute-by-category, reactions, and stale
  backend/napari diagnostics.
- **Notebooks (Pluto)** — real versioning (snapshot/restore), create-from-cells, in-place revise, and
  short descriptions.
- **Filter populations** — compound AND-ed conditions with full add/edit/delete in the Population Manager.
- **Custom modules** — user drop-in Julia + Python tasks with no rebuild.
- **Import** — optional 16→8-bit rescale + local staging for large OIRs; per-image metadata dialog;
  whole-filesystem file browser.
- **Analysis board** — unified figure/CSV export; true-vector SVG export for dot plots.

### Changed
- **Gating transforms follow the axis** — spatial/centroid axes (`centroid_x/_y/_z/_t`) default to
  linear and re-derive on axis/channel change; intensity columns select by channel name.
- Storage reclaim frees every non-active version (not just the original).
- Removed Harmony batch integration from clustering.

### Fixed
- **`pixi run dev` GUI shutdown** and **`pixi run stop`** no longer hang on Linux (use `ss`, not `lsof`).
- Revising a notebook preserves cell ids so an open notebook reloads live; revise/describe keep the
  existing description instead of blanking it.
- Self-heal stale `pluto`/`api` manifests on new app deps; macOS CI pin fixes; assorted import and
  scheduler fixes.

## [0.1.0-rc8] — 2026-07-16

### Added
- **Legacy migration**: import original R/Shiny Cecelia projects (images +
  segmentation + tracking).
- **First-launch setup wizard** + onboarding; **system-wide install scope** and
  in-app update surfacing.
- **Per-project lab log** — backend, routes, and Vue panel: auto-generated
  `[Cecelia]` activity digests, mute-by-category, reactions on entries, and
  GUI diagnostics that flag a stale backend or napari bridge.
- **MCP observer server** — read-only Claude access to a running project
  (Phase 1, Slice A).
- **napari 3D crop** — draw a box, preview, save as a new image.
- **Animation module** — keyframe render engine + timeline editor, batch
  movies (one mp4 per image), clean-capture publication stills, vector scale
  bar and timestamp.
- `INVENTORY.md` + a discovery-first rule; raw-datapoint CSV export, duplicate
  boards, and per-image stat units on the analysis board.

### Changed
- Plots aligned with the original R/ggplot look (UMAP, heatmap, palette).
- Consolidated zarr/OME image readers into one path; ImageJ TIFF metadata read
  via `ome_xml_utils`.
- Track/cluster populations render in their own colour under colour-by.

### Fixed
- Installer now expands a leading `~` in `CECELIA_HOME`.
- README config path; PDF/CSV export waits for plots to render (no fixed sleep).

## [0.1.0-rc7] — 2026-07-14

### Changed
- Fetch bioformats2raw at install time instead of bundling it — slims the
  release download.

## [0.1.0-rc6] — 2026-07-13

### Added
- **Population summary** — count / % specs, distribution charts, per-popType
  backbones (gated and tracked populations).
- **Analysis board** view-snapshot atom (zoom-to-source, sidecar board images,
  autosave); per-slot titles for PDF captions.
- Cluster UMAP **faceting**, colour-by population or image attribute, and
  **run-global cluster pops** pooled across co-clustered segmentations.
- Generic `ConfirmDeleteButton` and `TeleportPopover`; napari animation movie
  recorder; extracted `cecelia.utils.napari_utils` layer helpers.
- Frontend typecheck script + CI gate (`vue-tsc -b`).

### Changed
- **Gating overhaul**: 2D raster gating (dropped WebGL), multi-segmentation
  napari pops with autoscale, FlowJo-style dot plot, copy-strategy-to-images.
- Nav sidebar regrouped by pipeline stage; Settings moved to the footer.

### Fixed
- Gates re-project on scale change; freshly-drawn gates show immediately;
  morphology axes auto-linearise.
- Numerous plot/export fixes (square gate dots on PDF, heatmap left margin,
  canvas placement under zoom transforms).
- Dev supervisor: worktree switch relaunches the frontend; free a port by
  killing only the listening process; tear down Vite children on Ctrl-C.

## [0.1.0-rc5] — 2026-07-02

### Added
- **Universal Analysis canvas** (`/analysis`) — tabbed multi-board layout,
  gating-strategy plot, filmstrip, PDF/CSV export.
- One plot-hosting mechanism across surfaces with unified hi-res export; cluster
  UMAP, heatmap, and HMM plots on the board; read-only cluster manager.

### Changed
- Migrated theming to `@primeuix/themes` (`@primevue/themes` deprecated).

### Fixed
- Honour `yMin`/`yMax` on count/proportion charts; keep gate name labels
  on-canvas; CI bumped off the deprecated Node 20 runtime.

## [0.1.0-rc4] — 2026-07-01

### Added
- **Clustering** — Leiden cell + track clustering (`clust` / `trackclust` pop
  types), UMAP endpoint and module pages, cluster population manager, a generic
  interactive-plot canvas (UMAP + heatmap), and cluster HMM behaviour plots.
- **`LICENSE` (GPL-3.0-or-later)** + third-party acknowledgements.
- `run_py` launcher; collapsible left nav + right function/tasks panel; CI +
  Release status badges.

### Changed
- Route all Python spawns through `run_py` (drop inline spawn + `sys.path`);
  standardise Python runner filenames; move the update control to Settings.
- Removed the unused `PythonCall` dependency.

### Fixed
- macOS-arm64 install (cvxopt source build) + full cross-platform CI; installer
  404 while only prereleases exist; restore `ome-types`; several API/napari
  robustness fixes (missing `labelProps`, single-timepoint overlay, name coercion).

## [0.1.0-rc3] — 2026-06-30

### Added
- Bundle **bioformats2raw + Java** for out-of-the-box image import; per-OS README
  guidance for the projects folder / `custom.toml`.

### Fixed
- Use `read_idle_timeout` (`readtimeout` deprecated in HTTP.jl) in the updater.

## [0.1.0-rc2] — 2026-06-30

### Added
- **In-app update** — check + staged-apply-on-restart.

### Fixed
- README links point at the `cecelia-legacy` R/Shiny source.

## [0.1.0-rc1] — 2026-06-30

### Added
- **Initial release** — Cecelia ported to a Julia + Python + Vue stack: the core
  pipeline (image import, segmentation, tracking, gating, population management,
  behavioural track measures), headless-runnable `Cecelia.jl`, and the Vue
  frontend.
- **Bootstrap installer** + release workflow (`release.yml`); CI smoke-test
  workflow; README + docs.

[Unreleased]: https://github.com/schienstockd/cecelia/compare/v0.1.3...HEAD
[0.1.3]: https://github.com/schienstockd/cecelia/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/schienstockd/cecelia/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/schienstockd/cecelia/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc9...v0.1.0
[0.1.0-rc9]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc8...v0.1.0-rc9
[0.1.0-rc8]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc7...v0.1.0-rc8
[0.1.0-rc7]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc6...v0.1.0-rc7
[0.1.0-rc6]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc5...v0.1.0-rc6
[0.1.0-rc5]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc4...v0.1.0-rc5
[0.1.0-rc4]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc3...v0.1.0-rc4
[0.1.0-rc3]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc2...v0.1.0-rc3
[0.1.0-rc2]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc1...v0.1.0-rc2
[0.1.0-rc1]: https://github.com/schienstockd/cecelia/releases/tag/v0.1.0-rc1
