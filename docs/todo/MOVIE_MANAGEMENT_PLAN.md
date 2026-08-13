# Movie management

**Status:** BUILT — Phases 0–6. All eight table surfaces are on `SelectionTable` bar `FileBrowser`, a
stated exception (see Phase 5). Supersedes `docs/archive/movie-management-component-prompt.md`.

## Goal

Turn the Movies page from a flat listing of files into an interface for **organising** a collection
that grows in *kind*, not just in count — with/without segmentation, tracking, processing steps,
animations, and more categories over time. Name, star, tag, filter, delete; and remember how each
movie was made so it can be remade with one field changed.

## What was actually there (the audit that set the design)

Three premises in the originating prompt did not hold, and each moved the design:

1. **There was no per-movie record of any kind.** A movie was `{name, size, mtime}` from `readdir`
   (`api_movies_list`). The `note` the prompt referred to is `TitleCardCfg.note` — the second line
   printed *on the title card*, i.e. generation config — and separately `ImageTable` has a per-*image*
   note. So there was no mechanism to extend: star, tag, display name and saved config all needed a
   store that did not exist.
2. **The napari protocol versions are the wrong shape to reuse.** `NAPARI_PROTOCOL` /
   `PREVIEW_PROTOCOL` / `PY_CONTRACT_VERSION` are live handshakes between two *running* processes, and
   the remedy at every one is *refuse to adopt, kill, relaunch* (`docs/ARCHITECTURE.md`). A config
   saved in March cannot be relaunched. Saved config needs **migration**, and that pattern only knows
   rejection. See Decision 6.
3. **Both generation configs already round-trip.** The claim that a viewer recording has no
   recoverable config was wrong: `seedConfigFromViewState` (live view → config) and
   `POST /api/napari/apply-movie-config` (config → live view) are both built and in use on the Batch
   page ("fill from view" / "Preview on open image"). No new capture mechanism was needed.

Two further findings, not in the prompt, that the design has to survive:

- **The generation config was browser-local.** `getMovieConfig`/`getBatchMovieConfig` write
  `cc.napariSetPrefs` in **localStorage** — per *set*, last-config-wins, not in the project, gone on
  another machine. Saved config therefore belongs in the project, not in the settings store.
- **Movies overwrite silently.** `_movie_named_path` is the sanitised image name + suffix, so
  re-recording replaces the file. Any registry keyed by filename must expect the bytes under a key to
  change. See Decision 5.

## Decisions (2026-08-09)

**1. The registry is `settings/movies.json`, keyed by filename.**
A near-copy of `settings/notebooks.json` (`api/src/notebooks_api.jl`), which is this exact problem
already solved: per-project metadata decorating files on disk, the directory listing as the source of
truth, the registry as decoration. It lands under `settings/`, which `_mirror_tree!` copies verbatim
into a `.ccbundle`, so it travels with the project. Written through `write_json_atomic`.

*Rejected:* a sidecar `.json` per movie (N files to keep in step with N movies, and the movies dir is
also the user-facing output folder — it should hold movies); extending `ccid.json` (a movie is not
owned by an image: a batch produces one per image, and a comparison movie spans several).

**2. Rename is a DISPLAY NAME in the registry. The file is never renamed.**
The prompt asked whether the codebase separates the two — it does not, and it should here. Renaming a
file means re-keying the registry, and every path that resolves a movie by name has to be re-audited
each time one is added. A display name makes rename free, non-destructive, and unable to break
anything. Note the notebooks registry has create/describe/delete/duplicate and **deliberately no
rename** — same filename-key reason.

**3. Category is free-form tags (`string[]`), plus a derived `producedBy`.**
The taxonomy is stated to keep growing, and the repo already chose free-form for exactly this on
images (`img.attr` + `/api/images/attr/*`). A fixed enum would need a code change per new kind, which
the requirement rules out. `producedBy` (`viewer` | `animation` | `batch`) is written by the recorder,
never by the user, and gives the fixed axis for free.

**4. Star is a plain bookmark, independent of tags.**
Same semantics as `isStarred` on images (`utils/inclusion.ts`): any number, nothing downstream reads
it, it drives a filter and nothing else.

**5. The list route reconciles; the registry never asserts.**
The directory listing is the truth. On every read: drop entries whose file is gone (they accumulate
otherwise, and a stale row that plays nothing is worse than no row), and mark an entry's config
**stale** when the file's `mtime` is newer than the entry's `recordedAt` — that is the silent-overwrite
case, where the metadata survived but the bytes under it are from a different run.

**6. Saved config is versioned but read TOLERANTLY — and the real risk is dangling references.**
Each entry carries `configVersion`. A config is read field-by-field through defaults, the way
`buildBatchMovieConfig` already reads a legacy `valueName` into `valueNames`. The version exists to
*report* what could not be restored, never to reject an entry.
The failure mode worth engineering for is **not** field renaming: a config names value_names,
segmentations, populations and a colour-by column **by string**, so deleting a segmentation leaves a
config that is structurally perfect and semantically dead. Phase 6 resolves references against the
image and reports the dead ones; it must not silently apply a config whose targets are gone.

**7. There are two config KINDS, deliberately, and both already exist.**

| | **look** | **keyframes** |
|---|---|---|
| Shape | `BatchMovieCfg` — channels+colormaps, overlays, colour-by, masks + outline, z mode | `AnimSnapshot[]` — napari viewStates: camera, dims, per-layer props |
| Capture | `seedConfigFromViewState` (`utils/batchMovie.ts`) | `POST /api/napari/screenshot` |
| Re-apply | `POST /api/napari/apply-movie-config` | `applyViewState` (`utils/napariOverlays.ts`) |
| Producers | viewer, batch | animation |

The registry stores whichever the producer used, tagged. No third format, and "edit movie" routes to
the page that owns that kind.

**8. Contrast stays image state, and the props autosave becomes the default.**
Contrast is not copied into a movie config — it lives in the per-image layer-props JSON, which the
bridge autosaves the moment it changes, and which the movie path force-loads (`autoLoadProps = true`
in `_apply_movie_config!`). One canonical place, same rule as everything else.
But `napariAutoSaveLayerProps` **defaulted to false**, and on the interactive open path one flag drives
both directions (`autoSaveProps: autoProps, autoLoadProps: autoProps`) — so on a default install
nothing was ever written, that force-load found no file, and napari auto-contrasted per image. A saved
look was then not reproducible for the people most likely to be using saved looks. **The default flips
to true** (Phase 0), which also makes the two paths agree.

**9. ONE table for every consumer — yes, but SelectionTable grows two axes first, and `ImageTable`
migrates LAST.** *(Revised 2026-08-09 after counting the surfaces; the first version of this decision
said "don't merge", which answered a narrower question — "should `ImageTable` move onto
`SelectionTable` as it stands" — than the one worth asking.)*

There are **eight** table surfaces, not two:

| Surface | Rows | Select | Columns | Cell editing | Sort/resize |
|---|---|---|---|---|---|
| `SettingsModule` (compressor, layout) | 3–5 | single | static | — | — |
| `MoviesModule` | N | single | static | — | ✔ |
| `FlowModelVault` | N | single | static | — | — |
| `NotebookTable` | N | **none** | static | ✔ description | ✘ |
| `ProjectPanel` | N | single (radio) | static | — | ✘ |
| `FileBrowser` | N | **none** (row click = navigate) | static | — | ✘ |
| `LegacyMigrateDialog` | N | **multi** (checkbox) | static | — | ✘ |
| `ImageTable` | N | **multi** (Set + shift-range + select-all) | **dynamic** (1/channel, 1/attr) | ✔ attr, channel, note | ✔ |

The four hand-rolled ones (`NotebookTable`, `ProjectPanel`, `FileBrowser`, `LegacyMigrateDialog`) are
**257 lines of table markup and 19 table-CSS rules**, and **not one of them can sort or resize** —
capabilities `SelectionTable` already has and they would inherit for free. That is the win, and it is
measured rather than asserted. Three of the four migrated; see Phase 5 for why `FileBrowser` did not.

The capability gap is **two axes**, both additive:
- `selectionMode: 'none' | 'single' | 'multi'` — today the radio is unconditional.
- a per-column cell slot (`#cell-<key>`), falling back to the verbatim value — which is what lets a
  caller put an inline edit, a badge or an icon in a cell without forking the table.

Sequencing, and why: prove both axes on the four hand-rolled tables, where a mistake costs a dialog.
`ImageTable` goes **last**, once multi-select and cell slots have carried real traffic — it is the
selection surface every module page runs on, it is 1065 lines, and it is *already* on the shared
pieces (`useColumnResize`, `sortRows`, `useInlineEdit`), so it is simultaneously the highest migration
risk and the smallest remaining gain. Its bulk is per-image domain logic no generic table absorbs.

Same shape of answer for the image strip: it and the animation store are already two persisters of "a
napari viewState you can re-apply" and already share the re-apply half; the missing shared piece is
the provenance envelope, not a merged component.

## Phases

**Phase 0 — the props autosave default.** Flip `napariAutoSaveLayerProps` to true (Decision 8).
Independent of everything below and worth having on its own.

**Phase 1 — the registry.** `api/src/movies_api.jl`: read/write helpers, reconcile-on-read, and
`/api/movies/meta` (set display name / star / tags) + `/api/movies/delete`. `api_movies_list` merges
the registry into each row. Julia tests for reconcile, the stale-config rule, and name sanitising.

**Phase 2 — the Movies page.** Display name (inline edit via `useInlineEdit`), star, tags, delete —
through `SelectionTable`'s existing `#actions` slot. No new primitives.

**Phase 3 — filters.** Generalise `utils/rowFilters.ts` off `Includable` so it serves both tables, and
compose star / tag / `producedBy` with the existing column sort.

**Phase 4 — bank the config at record time.** `run_single_movie` / `run_batch_movies` register the
movie with its `producedBy` + config after a successful write. Backend only — nothing reads it yet,
but every movie recorded from here carries its provenance.

**Phase 5 — one table (Decision 9).** 5a: `SelectionTable` grows the axes the hand-rolled tables were
missing — `selectionMode`, `#cell-<key>`, `rowClass`, `#row-detail` (gated by `isExpanded`), `#empty`,
`row-dblclick`, a tri-state select-all that comes with `multi`, and a generic row type so slots hand a
caller its own type instead of a bare record. 5b: `ProjectPanel`, `LegacyMigrateDialog` and
`NotebookTable` migrated, each gaining sortable headers. 5c: `ImageTable` migrated (−131 lines), keeping
its selection and sort as models (`v-model:selected`, `v-model:sort`) because both live in the project
store per (scope, set) and order through the domain-aware `sortImages`.

**What the migration actually cost, against Decision 9's estimate.** `ImageTable` shed ~130 lines, not
the larger number implied — most of its bulk is domain logic (napari open, QC badges, run tags, crop
and copy dialogs, CSV) that no generic table absorbs — and `SelectionTable` took on ten configuration
axes to get there. The counted win was always the four hand-rolled tables; the win here is that sticky
columns, select-all, sort and resize have ONE implementation, so a fix lands everywhere. Three bugs
found on first render were all collisions between the shared table and its caller, none visible to
typecheck or the suite: a caller's `width` losing to `.sel-table.sized`, `position: relative` on a
header beating `position: sticky` (which shifted the pinned headers 60px right, hiding a column), and a
`#cell-` slot whose `v-if` renders nothing falling through to `row[key]`.

**`FileBrowser` is a deliberate exception** — found by doing the migration, not by planning it. Its row
CLICK is per-row semantics rather than a table concern (a directory navigates, a file selects), its
`..` row is synthetic and not in the data, and it *omits* the checkbox on unselectable rows where
`disabledIds` would render a disabled one. Sorting it would need `..` pinned regardless of the sort —
a new problem, not a free capability. The counted win there was sort/resize, and neither is worth
those three concessions on a control people use to find files.

**Phase 6 — edit / recreate.** A ⚙-style action on any movie row that banked a config, reopening the
page that AUTHORS that kind, prefilled. The three questions this was parked on, and the answers
(Dominik, 2026-08-10):

1. **It opens the page prefilled; it does not re-render.** Everything lands filled in and the user
   presses Render themselves — so "remake it with one field changed" is the same two clicks as "remake
   it unchanged", and nothing seizes napari for minutes from a page nobody is looking at.
2. **It REPLACES what the page holds, and offers an Undo.** One line above the controls — *Loaded from
   `<movie>` · Undo* (`components/RestoreNotice.vue`). A confirm dialog was the alternative and was
   rejected as a click on every trip, including the overwhelmingly common one where the page holds
   nothing anyone cares about. Undo restores by SNAPSHOT, which is why `replaceBatchMovieConfig` exists:
   a merge cannot remove a key, so undoing through one would leave behind every option the restored
   config set and the previous one did not.
3. **A dead reference is named in that same line, and never blocks.** `missingRefs` resolves the
   config's version / segmentation / channel names against what the destination images actually offer;
   `restoreNote` words the result. Decision 6, made visible.

Three things the earlier phases had not noticed, all fixed here rather than worked around:

- **Nothing banked WHICH IMAGE.** A movie is named after its image, but no path turns that name back
  into a uid — so a look had no idea what it was recorded on. `imageUid` (single) / `imageUids` (the
  batch's whole selection) now ride in the config, and the Batch page restores the selection with it.
- **An animation banked the RENDER payload, not the editor's model.** `keyframes` is
  `{viewState, steps}` — everything the recorder needs and none of what the timeline needs. A parallel
  `keyframeMeta` (thumbnail, title, seconds) now rides along, which costs a few hundred bytes rather
  than duplicating every view state. An animation from before this restores as bare views, and says so.
- **A restore settles WHICH SET first, from the movie's own images** (`restoreTargetSet`), and switches
  to it. Both pages store per set — the batch's config and output, both pages' image selection — and
  both originally checked the ACTIVE set, so a movie recorded in another set (or a page with no set
  active yet) restored the config, skipped the selection, and reported *"switch to its set to see it"*
  (Dominik, 2026-08-10). One click should repair what it can. Images spanning two sets have no single
  answer: the active set stands and the difference is reported.
- **The timeline is per-image, so a restore replaces ONE image's keyframes**, not the store. Written to
  the animation store's refs directly rather than through `load()` — that one exists for hydrating from
  the project-open response and deliberately suppresses the autosave, which is right there and wrong
  here: a restored timeline IS the working one.

The banked keys are now a contract across two languages with nothing type-checking it, so
*"API: movie config banks what the edit page reads"* (`api/test/runtests.jl`) pins them at the one place
that writes them. A dropped key would otherwise degrade the edit page silently.

**And one type for the authored config, which is what surfaced the frame range.** Restoring needed
`fileAttrs` back on the config, which is when it became clear there were **two** `BatchMovieCfg`
definitions: the canonical one in `utils/batchMovie.ts`, and a twenty-field restatement inside
`stores/settings.ts` — in a file that already imports from it, so there was never a layering reason for
the copy. The two had drifted by three fields nothing read: `trackValueNames` (the builder derives it)
and `tStart`/`tEnd`.

`tStart`/`tEnd` turned out not to be dead but **unreachable**: `run_batch_movies` reads them, feeds
`_t_sweep_frames`, and the whole stack down to `napari_utils.record_timelapse` honours them — there was
simply no control anywhere that could ask. So the store's bag became `BatchMovieCfg` itself, the two
dead fields went, and the frame range was wired up properly instead (Dominik, 2026-08-10): a shared
`MovieTimeRange.vue` on the two surfaces that sweep T, the missing forwarding on the single-record path
(`_t_range` → `run_single_movie` → `record_timelapse!`), and the range banked with the movie so a
recreate reproduces it. Details in `docs/NAPARI.md` → *Movie frame range*; the reason it is frame
indices with an OPEN end is that one authored range runs across timelapses of unequal length.

## Not doing

- **A movie is not an image.** No versions/labels/analysis delete scopes — `DeleteImagesDialog`'s four
  scopes are meaningless here. Deleting a movie deletes one file.
- **No filesystem rename** (Decision 2).
- **No per-movie contrast snapshot** (Decision 8).
