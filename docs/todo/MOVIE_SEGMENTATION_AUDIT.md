# Movie comparison — segmentations audit

> **Status: (a) and (b) are BUILT** (2026-08-08, branch `work/movie-seg-audit`). The findings below are
> kept as the record of what was wrong and why the fix took the shape it did; the durable description
> of the result lives in `docs/NAPARI.md` → *Side-by-side comparison* and `INVENTORY.md`. The two
> decisions at the bottom were answered — and D-a was answered the *other* way in the end: not one axis
> at a time but a **grid**, versions across and masks down, so picking two of both gives the
> cross-product instead of a choice. A single list still lays out side by side in columns. D-b: yes,
> the viewer inherits its masks into every cell.

Audit of the two movie surfaces (viewer single-shot record, Batch Movies) against two asks:

* **(a)** a movie should be able to **include segmentations** (label masks);
* **(b)** a movie should be able to compare **several segmentations side by side**.

Reference for what exists: `docs/todo/MOVIE_COMPARE_PLAN.md`, `docs/NAPARI.md` → *Side-by-side
version comparison*, `INVENTORY.md` entries 132–134.

---

## Verdict

**(a) is broken, not missing.** Label masks are supported everywhere *except* the one code path a
movie config runs through. Every surface that applies a movie config re-opens the image — which
clears the napari canvas — and the re-open never asks for label layers back. So a comparison drops
the masks, and the batch drops them for every image after the first. The `labels` chip already in the
Batch Movies panel is a **silent no-op**.

**(b) is genuinely unbuilt, but the backend is already shaped for it.** `_record_columns!` takes
`(label, config)` column specs, not version names (D9 of the compare plan anticipated exactly this).
A segmentation comparison is a new column builder plus a UI axis — no change to the pass loop, the
stitcher, cancel, staging or progress.

Two design questions need your call before (b) can be built; they're at the bottom.

---

## (a) Including segmentations

### The mechanism

`_apply_movie_config!` (`api/src/napari_api.jl:640`) is the one function that turns an authored movie
config into a napari view. Its step 1 calls `api_napari_open`, and the bridge's `open_image` starts
with `self._viewer.layers.clear()` (`napari/napari_bridge.py:133`). Everything on the canvas is gone.

What step 1 sends:

```julia
_call_napari_api(api_napari_open, (; projectUid, imageUid, valueName = vn,
                                     autoLoadProps = true, autoSaveProps = false))
```

`api_napari_open` accepts `showLabels` + `allLabels` (`{valueName => [files]}`) and re-adds the label
layers itself (`napari_api.jl:392`) — the interactive viewer path uses exactly that. The movie config
path **never passes them**. Steps 3–5 restore tracks, population points and label *colouring*; there
is no step that restores the label *layers*.

`_apply_movie_config!` has everything it needs to fix this: `img.labels` is already
`Dict{String,Vector{String}}` — the exact shape `_parse_all_labels`/`_show_all_labels!` consume.

### Four consequences, all currently live

**1. The Batch Movies `labels` chip does nothing.**
`BatchMoviesPanel.vue:153` offers a chip tipped *"Colour label masks by the colour-by measure"* →
`colourLabels: true` → `_apply_movie_config!` step 5 → `api_napari_colour_labels` → the bridge's
`colour_labels`, which does:

```python
targets = [l for l in self._viewer.layers if getattr(l, "name", "").endswith(") Labels") ...]
if not targets:
    return {}
```

After the re-open there are no `) Labels` layers, so it returns empty and nothing happens. It never
*creates* a layer — by design; it only recolours one.

**2. The batch's first image can differ from the rest.**
`_apply_movie_config!` skips the re-open when the target is already on screen and no version is
selected (`already_open`, line 649). So if image 1 of a batch happens to be the image open in napari
with its masks showing, image 1 gets masks and images 2..N do not. A batch that looks half-right.

**3. A viewer comparison loses *every* overlay in columns 2+, not just labels.**
`run_single_movie` builds its columns from an **empty** base config:

```julia
_version_columns(Dict{Symbol,Any}(), value_names)     # napari_api.jl:1136
```

so each column config carries only `:valueName`. Column 1 is protected by `_version_is_open` (its
`valueName` is blanked, so no re-open, so the live view survives); columns 2+ re-open with
`showTracks=false, showPopulations=false, colourLabels=false` and come back bare. **The output is a
comparison where the left column has your tracks/points/masks and the right column has none.**

This is the single most visible defect of the three, and it contradicts D1 of the compare plan
("full overlay support per column (tracks/pops/labels/colour-by all work, because each pass IS
today's recording)") and `docs/NAPARI.md` ("records each column through the SAME path a single movie
uses — so overlays … keep working"). Both statements are true of the *batch* config and false of the
*viewer* record. Those two doc claims need correcting whether or not this is fixed.

**4. The batch "Preview config" button lies.**
`api_napari_apply_movie_config` calls `_apply_movie_config!(…; do_open = false)` on purpose (line
715) — so the preview leaves the canvas alone and any masks the user has on screen stay visible. The
recording then re-opens and drops them. Preview shows masks; the movie has none.

### Adjacent finding — the auto-show race

Every `api_napari_open`, including the ones inside a recording, broadcasts `napari:opened`
(`napari_api.jl:407`). The app-level `useNapariAutoShow` listens unconditionally and fires
`pushAllOverlays()` (`useNapariAutoShow.ts:334`), which *does* push labels. Those POSTs block on
`_viewer_lock`, which `_record_columns!` holds for the whole multi-pass sequence, so they land
**after** the movie is finished — one queued burst per column per image. Harmless to the file, but:
the masks reappear in the window when the render ends, which is exactly why this gap is easy to miss
when eyeballing it live. If (a) is fixed, this path should also take a `suppressAutoShowOnce`-style
claim (the registry already exists, used by analysis-board zoom-to-source) so a recording isn't
racing the UI.

### What (a) costs

| File | Change |
|---|---|
| `api/src/napari_api.jl` | `_apply_movie_config!`: pass `showLabels`/`allLabels` on the open (or a `_show_all_labels!` call after it), driven by a new config key. ~10 lines. |
| `api/src/napari_api.jl` | `run_single_movie`: stop building columns from an empty config — seed the base from the live view so columns 2+ match column 1. This is finding 3 and is the bigger of the two edits. |
| `frontend/src/utils/batchMovie.ts` | `labelValueNames?: string[]` on `BatchMovieCfg` + `BatchMovieRequestConfig` + `buildBatchMovieConfig` (+ its tests). |
| `frontend/src/modules/batchmovies/BatchMoviesPanel.vue` | a segmentation picker; `segNames` is already computed there (line 62) for `trackValueNames`. |
| `frontend/src/components/ViewerPanel.vue` / `api/src/sockets.jl` | carry the live overlay state into `movie:record` if finding 3 is fixed frontend-side rather than backend-side. |
| `docs/NAPARI.md`, `docs/todo/MOVIE_COMPARE_PLAN.md` | correct the two "overlays keep working" claims. |
| `api/test/runtests.jl` | a test that a built column config carries the label request. |

`colourLabels` needs a decision too: today it is a standalone chip that implies masks. Once masks are
their own control, `colourLabels` should probably become dependent on it (a colouring of something
you asked to show) rather than a peer.

---

## (b) Several segmentations side by side

### What already generalises

`_record_columns!` is deliberately agnostic:

```julia
const MovieColumn = NamedTuple{(:label, :config),Tuple{String,Dict{Symbol,Any}}}
```

with the header comment *"The loop never asks what makes two columns differ, so comparing two
colour-by measures or two segmentations later is a config change rather than a rewrite (D9)"*. That
holds up: the pass loop, per-frame progress across passes (`_comparison_frame_total`), cancel,
staging, the `stitch_movies` compose and the per-column caption band all key off the column list, not
off versions. `_version_columns` is the only version-specific piece, and it is 5 lines.

The bridge is ready too: label layers are namespaced `({vn}) Labels`, and `show_labels` with
`show=false` removes a layer (`_show_label_stores`, `napari_bridge.py:357`), so a per-column "show
these segmentations, hide the others" swap is a supported operation.

There is a bonus: a segmentation comparison keeps the **same image version** in every column, so
`already_open` short-circuits the re-open for every pass after the first. No re-open means no
contrast re-sampling and no wasted pyramid load — it is meaningfully cheaper than a version
comparison, though still N render passes.

### What's missing

1. **A column builder.** `_segmentation_columns(config, seg_names)` mirroring `_version_columns`:
   same base config, per column `:labelValueNames => [vn]`, `label = vn`. Depends on (a) — there is
   no `labelValueNames` key to vary until (a) exists.
2. **A UI axis.** `MovieCompareControls.vue` is versions-only, and its contract is D8: *the selection
   is the mode*. A second `ChipSelect` over segmentations needs that contract extended.
3. **Filename disambiguation.** `compareSuffix` (`movieCompare.ts:41`) joins version names
   (`default-vs-af_corrected`). A segmentation comparison of one version would produce a blank
   suffix and overwrite the plain movie.
4. **Cost/label plumbing.** `comparePasses`, `compareActionTip` and `_config_value_names` all read
   the version list; each needs to read whichever axis is active.

Roughly the same size as (a) — the work is in the frontend and in naming, not the render path.

---

## Two decisions needed

**D-a — One comparison axis, or a cross-product?**
Two versions × two segmentations is four columns and four render passes. My recommendation: **one
axis at a time** (a radio: *compare versions* / *compare segmentations*), with the non-compared axis
staying a plain multi-select applied to every column. A cross-product is easy to ask for and hard to
read at four columns wide, and it makes the caption band ambiguous (`default / A`?). It also keeps
D8's "the selection is the mode" intact per axis.

**D-b — Should the viewer's record inherit the live overlays into every column (finding 3)?**
It reads as an obvious bug to me, but it is a behaviour change: today a viewer comparison's later
columns are deliberately bare-ish, and fixing it means the recorder starts pushing overlays it
currently doesn't touch. The alternative — give the viewer's recorder the same overlay chips the
batch panel has — is more work but more explicit. My recommendation is to inherit: "record what's on
screen" is the viewer recorder's stated promise, and it already holds for column 1.

---

## Not looked at / out of scope

* The Animation module (keyframes) records the live view without re-opening, so it does not have the
  (a) defect — but it also cannot compare at all. Untouched by either ask.
* Branch (skeleton) labels have the same gap as cell labels (`allBranchLabels` is likewise never sent
  from a movie config). Same one-line fix location; worth including in (a).
* Nothing here was run — this is a read of the code paths, not a reproduction.
