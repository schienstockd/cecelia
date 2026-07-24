# Analysis figures & movies (animation)

Status (updated 2026-07-24): A–G + F1/F2 **done/merged**. **Phase H (movie title card) IN PROGRESS**
— see the H section below. Supersedes the ad-hoc parts of the image-strip tasks (#00032 legend,
#00036 zoom-to-source) — both now live on the shared snapshot foundation.

## Goal

Make Cecelia the tool that produces the **figures and movies** people actually publish — channel
layers in chosen colours with tracks/populations overlaid, coloured by a feature — and make them
**reproducible months later**. The north star: your boss asks (4 months on) to turn the green cells
red; instead of an hour hunting the image and rebuilding the zoom/orientation/contrast, you reopen
the saved **view snapshot**, change one colour (a JSON edit in the GUI), and regenerate.

## Why / use cases

- **The R `generateMovies` workflow** (`old-R-shiny-version/vignettes/runAnimation.Rmd`) is the
  proven pattern, repeated across every example: pick images (often attr-filtered) → show a few
  channels each in a colour → overlay tracks/populations → **colour them by a categorical feature**
  (movement cluster, HMM state) → record the timelapse → one attr-named `.mp4` per image. The only
  manual step was an R `browser()` pause to fix contrast per image.
- **Reproducibility**: the same saved view must reopen identically and be editable long after.

## The atom: a durable view snapshot (our own JSON)

Everything here stands on **one persisted, GUI-editable JSON object** describing a napari view —
captured *from* napari, re-applicable *to* napari, and readable by the UI (for legends).

Illustrative shape (final schema settled in Phase A):

```jsonc
{
  "imageUid": "…", "valueName": "corrected",           // which image + version
  "camera": { "center": [z,y,x], "zoom": 4.2, "angles": [0,0,0], "perspective": 0 },
  "dims":   { "ndisplay": 2, "order": [0,1,2], "point": [t,z,y,x] },   // incl. T/Z position
  "layers": {                                           // captured per napari layer
    "gBT":  { "visible": true, "colormap": "green", "contrast_limits": [50, 800],
              "gamma": 1.0, "opacity": 1.0, "blending": "additive" }
  },
  // higher-level authoring form (F1 batch), expanded into layers/pops on apply:
  "channels": { "gBT": "green", "SHG": "gray" },        // channel → colormap
  "pops": [
    { "popType": "live", "valueName": "gBT", "path": "tcells/tracked",
      "showTracks": true, "pointsSize": 8, "colour": "#00bfff",
      "splitBy": { "measure": "live.cell.track.clusters.movement",
                   "groups": { "Directed":  { "values": [2],     "colour": "#ff1493" },
                               "Crawling":  { "values": [0,3,4], "colour": "#ffd700" },
                               "Sessile":   { "values": [1],     "colour": "#00bfff" } } } }
  ],
  "scaleBar":  { "show": true },
  "timestamp": { "show": true }
}
```

Two faces of the same atom: the **captured** face (`camera`/`dims`/`layers`, read from napari) drives
**zoom-to-source**; the **authored** face (`channels`/`pops`/`splitBy`) drives **batch movies** and
expands into layers/pops when applied. They converge — capturing fills the layer face; authoring the
config face.

### Why our own JSON, not napari-animation's `ViewerState`

`ViewerState.from_viewer` is trivial (`layer.as_layer_data_tuple()[1]` + `camera.dict()` +
`dims.dict()`), and `apply` is just `setattr` per field, wrapped in `suppress(AttributeError)` and
only-if-changed — **very tolerant** of scalar inputs. But its captured dicts hold napari enums, pint
`Unit`s and `ColorArray`s (verified: 6 unserialisable values in a trivial 2-channel case), so storing
them verbatim ties our durable data to napari's internal types across versions. Instead we capture
into **settable scalar forms** (colormap by *name*, enums → strings, arrays → lists) — durable,
human-readable, GUI-editable, and re-applied by plain `setattr`. Verified: a whitelisted/sanitised
snapshot round-trips through `ViewerState.apply`, including a green→red colormap change.

We already autosave per-image layer props (contrast/colormap/gamma/opacity/blending + T/Z) via
`save_layer_props` — the snapshot generalises that (adds camera + pops + split) and makes it a
first-class, named, portable artifact.

## Decisions (2026-07-13)

1. **Own the snapshot JSON.** Durable store = our schema of settable-scalar values. Never persist
   napari/napari-animation objects. (Rationale above.)
2. **Keep napari-animation — as a transient render engine only, not storage.** At *generate* time we
   hydrate our JSON → `ViewerState` → `KeyFrame` → `Animation(viewer)` → `animate(path)` for
   interpolated (Tier-2) movies. If it ever breaks, only the render step changes; saved snapshots are
   safe. `Animation` runs headless (no widget needed).
3. **Two movie tiers.**
   - **Tier 1 — batch time-playback** (the `generateMovies` port): a config applied across
     selected/attr-filtered images → record the T-sweep → one attr-named `.mp4` per image. This is
     the common, automatable case; it does **not** need napari-animation (T-sweep + encode).
   - **Tier 2 — keyframe animation page**: an ordered, durable, editable sequence of snapshots with
     interpolation (camera moves, fades, colour transitions), rendered via napari-animation.
4. **Contrast without `browser()`.** Batch movies use each image's already-saved layer props for
   contrast/colormap; no manual pause. A colour change is a JSON edit in the GUI, then regenerate.
5. **Split-by-feature is a shared primitive.** Colour tracks/points by a categorical measure column →
   named groups each with a hex colour (ports `splitTracks`/`splitPops`). Reused by both movie tiers,
   the strip legend, and static overlays. A track pop may inherit its source pop's colour.
6. **The #37 in-napari recorder stays** (harmless quick path); the durable/editable workflow is the
   new page. napari-animation is not removed.
7. **Scale bar / timestamp are figure overlays, not baked pixels** (for stills). Journals want them
   drawn in Illustrator/Inkscape. Cecelia offers a clean-capture toggle and, ideally, draws its own
   vector scale bar (N µm + label) + timestamp beneath the frame. (Movies may keep them baked.)

## F1 batch config (mirrors `generateMovies` params)

| R param | Cecelia config | Notes |
|---|---|---|
| `uIDs` (+ `exp.info` filter) | selected images / attr filter | e.g. `Include=="Y" & Treatment=="CNO"` |
| `channelsToShow` | `channels: {name → colormap}` | only these shown; rest hidden; default gray |
| `matchChannels` | fuzzy channel-name match | crude in R; optional |
| `popsToShow` | `pops[].path` | overlay as tracks and/or points |
| `splitTracks` / `splitPops` | `pops[].splitBy` | categorical measure → {group: values+colour} |
| `showTracks`, `tracksBlending`, `pointsSize` | per-pop render opts | |
| `fps`, `windowSizeX/Y` | output fps + resolution | |
| `fileAttrs` | attr-based output naming | `Day_Treatment_<uid>.mp4` |
| `valueName` | image version | `default` / `corrected` |
| `browser()` contrast | **saved layer props** (Decision 4) | no manual pause |

## Phases (independently shippable; order)

**A → D → B + C → G → F1 → E → F2.** A/B/C/D land value fast; F1 is the headline user win and
matches how figures are actually made; F2 is the advanced follow-on.

**Progress:** ~~A~~ · ~~B~~ · ~~C~~ · ~~D~~ · ~~G~~ · ~~F1~~ · ~~E~~ · ~~F2~~. **(all done)**

- **~~A. Snapshot atom~~ — DONE** — `capture_view_state()` + `apply_view_state(json)` in the bridge
  (whitelist + sanitise + only-present-layers filter); capture folded into the screenshot reply;
  round-trip tested (incl. colormap change). Camera + T/Z + contrast + colormap restore.
- **~~B. Zoom-to-source (#00036)~~ — DONE** — each strip frame stores `{imageUid, valueName, snapshot,
  colourBy}`; `ImageStripView.zoomToSource` reopens the image + `POST /api/napari/apply-view-state`
  restores the exact view, then **re-pushes the tracks/pops** the frame had — derived from the snapshot's
  overlay layer names (`utils/overlayLayers`) + the captured colour-by, via `utils/napariOverlays`
  (show-tracks/show-populations/colour-labels). (Fixes: overlays weren't restored because the board's
  ViewerPanel may be closed, so open alone only recreated channel layers.) Durable across sessions.
- **~~C. Strip legend (#00032)~~ — DONE** *(needs A, G)* — channel + population + colour-by colour swatches
  below each frame, from the snapshot + a read-only legend, toggled in the ⚙.
  - **~~Backbone (S1)~~ + ~~channel section~~:** the shared model `utils/viewLegend.ts` + `ViewLegend.vue`.
  - **~~Populations + colour-by sections~~:** captured with the screenshot via `POST /api/napari/overlay-legend`
    (pure Julia, reuses `_colour_overrides_for`/`_pop_labels_for` + pop-map colours — no viewer touched):
    populations = each shown point-pop's name+colour, colour-by = value→pop colour+name (clusters read as
    their pop names). Stored on the frame (durable). Sections stack bottom-left with **channels at the
    bottom, pops/colour-by above** (per the layout ask); timestamp top-left, scale bar bottom-right (E2).
- **~~D. Capture quality~~ — DONE** — D1: the board screenshot uses napari `export_figure` (tight-fit to
  the data extent at native resolution → no black margins). D2: the strip went `object-fit: contain`
  (letterbox) so a scale bar / timestamp isn't cropped (E2's own vector scale bar will let frames go
  edge-to-edge again).
- **~~E. Scale bar / timestamp for stills~~ — DONE.**
  - **~~E1 clean-capture toggle~~ — DONE:** `save_screenshot(clean=True)` hides napari's baked scale bar
    + timestamp for the shot (restored after); threaded `POST /api/napari/screenshot {clean}` → the
    persisted **"clean capture"** toggle in the image-strip ⚙ (`settings.cleanCapture`). Scoped to stills
    (animation keyframes keep the timestamp). See docs/NAPARI.md → *Clean capture*.
  - **~~E2 vector scale bar + timestamp~~ — DONE:** the screenshot reply carries `extent_um` (data shape ×
    `_im_scale`; `export_figure` tight-fits to the data extent → the frame's physical size). The strip
    draws its own crisp scale bar + elapsed-time timestamp on the clean capture via `StillOverlay.vue` —
    an SVG whose `viewBox` is the extent (µm) with `preserveAspectRatio="xMidYMid meet"` (matches the
    frame's `object-fit: contain`), so geometry is correct even when letterboxed. Toggled per strip in
    the ⚙. `utils/stillOverlay` (`niceScaleBar`, `elapsedLabel` — shared with the timeline), unit-tested.
    (Movies keep baked overlays — this is specifically for publication stills; see Decision 7.)
- **~~G. Split-by-feature colouring~~ — DONE** — colour napari tracks/labels by a categorical measure
  (`/api/napari/colour-labels`, show-tracks `colorBy`); a value a population *filters for* takes that
  pop's colour (`_colour_overrides_for`/`pop_colour_overrides`), else Okabe–Ito; user recolours +
  editable schemes for categories with no population (HMM states); legend returns pop-name labels,
  deduped. Reused by the viewer panel, F1 batch, and F2.
- **~~F1. Batch movie generation~~ — DONE** *(needs A, G)* — author a config (table above) → apply per
  selected image (contrast from saved props) → record T-sweep → attr-named `.mp4` per image, on the
  **Batch movies** page (`/batch-movies`). *Reproduces the `runAnimation.Rmd` pattern.*
  - **CHANGED from the original sketch:** runs **not** as a scheduler task but as a WS-orchestrated
    async loop on the single shared viewer (`movie:batch`), because napari is a UI-serial viewer in
    `api/`, not pooled headless compute. Output is **attr-named** (`<attr…>_<uid>.mp4`), not the
    `{uid}_{valueName}.mp4` the schema sketch showed.
  - **F1.1 — DONE.** The recording primitive: `napari_utils.record_timelapse` (keyframe→`animate`→mp4)
    + bridge `record_timelapse` + `POST /api/napari/record-timelapse` + a one-click "Record timelapse"
    button that records the open image's CURRENT view to `{project}/movies/{uid}_{valueName}.mp4`. See
    docs/NAPARI.md → *One-click timelapse recording*.
  - **F1.2 — DONE.** `_apply_movie_config!` (api/src/napari_api.jl) applies an authored config to an
    image by REUSING the existing handlers (open with per-channel colormaps + visibility via a partial
    view-state; `show_tracks`/`show_populations`/`colour_labels` for overlays + colour-by) — no divergent
    re-implementation. Exposed as `POST /api/napari/apply-movie-config` (preview on the open image).
    Contrast comes from each image's saved layer props (Decision 4). Config schema: `{ valueName,
    channels:{name→colormap}, colourBy, showTracks, trackValueNames, tailWidth, showGatedTracks,
    showTrackclust, showPopulations, popType, pointsSize, colourLabels, colourOverrides, tStart, tEnd }`.
  - **F1.3 — DONE.** Batch the config across the selected images → one **attr-named** `.mp4` each
    (`<attr1>_<attr2>_..._<uid>.mp4`, `_movie_basename`). WS-triggered (`movie:batch`), run async on the
    ONE shared viewer sequentially under `_viewer_lock` (napari can't render offscreen — GL frames are
    black — so it drives the live window; the page warns the user it's busy). Reports over the normal
    task events (progress/log/status/result keyed by the client taskId) so it shows in the task list
    with a progress bar + Cancel (a per-run flag, `request_batch_cancel!`, stops it after the current
    image). Frontend: the **Batch movies** page (`/batch-movies`, `BatchMoviesModule.vue`) — image
    multi-select + a config panel + Generate. NOT a scheduler task: napari is a single UI-serial viewer
    in `api/`, not pooled headless compute (see the runner-architecture decision, 2026-07-14).
- **~~F2. Animation page~~ — DONE** *(needs A; big)* — the durable/editable page under the Analysis nav.
  - **~~MVP (S2)~~ — DONE:** `/animation` route + nav entry; captures the current napari view as a **view
    snapshot** (screenshot sidecar + view state, the same path as the board strip) into a per-project
    `animations.json` (`/api/projects/animations`, autosaved via `stores/animation.ts`); a gallery of
    snapshots each with its **`ViewLegend`** (S1 backbone) + a **Record movie** button.
  - **~~Render engine~~ — DONE:** `napari_utils.record_keyframes` (apply each keyframe's `viewState` →
    capture with `steps` tween frames from the previous → animate) + bridge + `record_keyframes!` +
    `POST /api/napari/record-animation`. Interpolates camera/contrast/colour/T. Offscreen smoke-tested;
    unit-tested.
  - **~~Timeline editor~~ — DONE:** a **row/track matrix** — columns = keyframes (captured `viewState`s +
    duration), rows = **channels · populations · camera** inferred from each `viewState`; per-cell edits
    mutate that column's `viewState` (data model = ordered list of `viewState`s). Plus: keyframe select →
    sync napari, update-from-napari, per-keyframe reset + "edited" badge, drag-reorder, timepoint (h/min),
    fps 1–40, amber selection (`--cc-selected`). Decision (2026-07-14): **timeline, not a node graph** —
    a movie is 1-D, so the graph's branching buys nothing.

## H. Movie title card (prepended description slide) — IN PROGRESS

A toggle on movie generation that prepends a short **title card** to the recorded `.mp4` — an
auto-generated description slide (image name + attributes, the channels shown with their colours, the
populations/tracks with colours, and the colour-by legend), plus an optional free-text note. Answers
"which image / mouse / location is this, and what are the colours?" without opening the project or a
separate doc. Applies to all three movie paths: single record, batch, animation page.

### Decisions (2026-07-24, confirmed with Dominik)

1. **Content = full auto + optional note.** Auto-fill from the image + view: `title` = `img.name`
   joined with its `img.attr` values ("MERTK — mouse 1 — location B"); a **channels** section
   (name + colour swatch); a **populations/tracks** section (name + colour); a **colour-by** section
   (value label + colour); plus one optional user note line. No retyping.
2. **Duration** default **3 s**, adjustable **1–10 s** per movie.
3. **On by default.** The toggle defaults ON — every generated movie gets a card unless turned off.
4. **Reuse the ONE canonical legend source — do not add a third.** Pops + colour-by come from the
   same Julia helpers behind `POST /api/napari/overlay-legend` (`_colour_overrides_for` /
   `_pop_labels_for` / `pop_at().name/.colour`); extract a shared `overlay_legend_content(img, …)`
   in `napari_api.jl` so the endpoint AND the card call it (consolidation, not duplication). The board
   strip legend + napari strip already consume this — the card must produce identical rows.
5. **Channels are read from the live napari viewer at record time**, NOT from a hardcoded map.
   There is no Julia/Python channel-colour function — channel colour lives only in the napari layer
   state (the frontend `viewLegend.ts`/`napariColormap.ts` approximates it for the board strip, where
   there's no live viewer). Recording always drives the live window, so at record time the visible
   image layers carry the authoritative `name` + `colormap`; Python reads `layer.colormap.map([1.0])`
   → hex. This avoids duplicating the frontend's colormap→hex table.
6. **Card is composited in Python as a post-record step** (`python/cecelia/utils/title_card.py`),
   NOT inside napari-animation (its `anim.animate()` writes the mp4 directly; frames aren't exposed).
   After the movie is written, render N = `duration × fps` title frames (PIL, at the movie's exact
   resolution + fps) and prepend them, re-writing via **imageio-ffmpeg** (already a dep). One honest
   cost: this **re-encodes the clip once** — fine for short intravital movies; documented.
7. **Threading:** one `titleCard: { enabled, note, durationSec }` object (camelCase in JSON/Julia,
   snake_case in the bridge cmd + Python) flows each frontend → Julia handler → the shared
   `record_timelapse!` / `record_keyframes!` (`app/src/napari.jl`) → bridge cmd
   (`napari_bridge.py`) → `napari_utils.record_*` → `title_card`. Non-channel content (title, note,
   pops, colour-by) is assembled in Julia and passed in `title_card`; channels are added in Python
   from the live viewer.

### Sub-phases (independently shippable; order H1 → H2 → H3 → H4)

- **H1 — render primitive (backend, unit-tested).** `python/cecelia/utils/title_card.py`:
  `render_title_card(content, width, height, *, fps, duration_sec) -> list[frames]` and
  `prepend_title_to_movie(movie_path, content, *, duration_sec)` (reads the movie's fps+size via
  imageio, renders the card at that size, writes card frames + movie frames to a temp file, replaces
  the original). `content = { title, note, sections: [{ heading, items: [{label, colour}] }] }`. Pure
  + testable without napari (`python/cecelia/tests/test_title_card.py`). Add Pillow to the env if not
  already declared.
- **~~H2 — batch path~~ — DONE (scoped)** (richest config; the headline `generateMovies` use).
  `overlay_legend_content` extracted in `napari_api.jl` and reused by the overlay-legend endpoint AND
  `_title_card_content(img, config)` (title = `img.name` + attr values; **colour-by** section from the
  shared helper). `titleCard {enabled, note, durationSec}` threads `run_batch_movies` →
  `record_timelapse!` (`napari.jl`) → bridge → `napari_utils.record_timelapse`, which reads the
  **Channels** section from the live viewer (`_visible_channel_legend`, colormap→hex) and prepends the
  card best-effort (`_maybe_prepend_title` — never fails the recording). Frontend toggle/note/duration
  in `BatchMoviesPanel.vue` (config + clamp in `utils/batchMovie.ts`, default ON), persisted per set.
  **Deferred:** the explicit point-**populations** rows (deriving the exact shown-pop set from the
  batch config robustly needs care; the colour-by section already covers the split-by-cluster case).
  Card content so far = title + channels + colour-by + note.
- **H3 — single record** (`ViewerPanel.vue` → `api_napari_record_timelapse`). No overlay config
  exists on this path, so the card shows title + channels (+ note); pops/colour-by best-effort/empty.
- **H4 — animation page** (`AnimationModule.vue` → `record_keyframes`). Card reflects the FIRST
  keyframe's view; channels read after applying keyframe 0. Pops/colour-by derived from the first
  keyframe's overlay layers if feasible, else title + channels + note.

## References

- **R reference**: `old-R-shiny-version/vignettes/runAnimation.Rmd` — `generateMovies` (L28) +
  ~10 real examples (channels+colours, `splitTracks`/`splitPops`, `saveTimeAnimation`).
- **napari-animation**: `ViewerState` (`viewer_state.py`: `from_viewer`/`apply`), `Animation`,
  `KeyFrame`, `interpolation`; export via imageio-ffmpeg. Added as a PyPI dep (see
  `docs/SHIPPING.md` → *napari-animation is PyPI, not conda-forge*; `docs/NAPARI.md` → *Animation
  recorder*).
- **Existing code to build on**: `save_layer_props`/`load_layer_props` + autosave
  (`napari/napari_bridge.py`); `ImageStripView.vue` (capture → data URL); `/api/napari/screenshot`
  (`api/src/napari_api.jl`); colour-by (`/api/napari/colour-labels`, show-tracks `colorBy`); the
  pop-manager palette (`plot_pop_types`/`pop_df`, frontend `plots/plot.ts` okabe-ito); board
  persistence pattern (`settings/`).
- **Related plans**: `docs/todo/ANALYSIS_CANVAS_PLAN.md` (image/filmstrip slot, Phase D).
