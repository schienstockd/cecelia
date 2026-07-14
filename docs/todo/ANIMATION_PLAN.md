# Analysis figures & movies (animation)

Status (updated 2026-07-14): **mostly shipped.** A, B, D, G, F1 (all of F1.1–F1.3) and F2 (MVP +
render engine + timeline editor) are **merged**. Two partials remain: **C** (channel legend live; the
populations + colour-by legend sections still to wire into the strip) and **E** (E1 clean-capture toggle
done; **E2** — Cecelia-drawn vector scale bar + timestamp — still to build). Supersedes the ad-hoc parts
of the image-strip tasks (#00032 legend, #00036 zoom-to-source) by putting them on a shared foundation.

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

**Progress:** ~~A~~ · ~~B~~ · C (partial) · ~~D~~ · ~~G~~ · ~~F1~~ · E (E1 done, **E2 left**) · ~~F2~~.

- **~~A. Snapshot atom~~ — DONE** — `capture_view_state()` + `apply_view_state(json)` in the bridge
  (whitelist + sanitise + only-present-layers filter); capture folded into the screenshot reply;
  round-trip tested (incl. colormap change). Camera + T/Z + contrast + colormap restore.
- **~~B. Zoom-to-source (#00036)~~ — DONE** — each strip frame stores `{imageUid, valueName, snapshot}`;
  `ImageStripView.zoomToSource` reopens the image + `POST /api/napari/apply-view-state` restores the
  exact view. Durable across sessions.
- **C. Strip legend (#00032)** *(needs A, G)* — render channel + population/feature colours (swatch +
  name / µm) below each frame, from the snapshot; toggle in the ⚙ popover.
  - **~~Backbone (S1)~~ + ~~channel section~~ — DONE:** the shared legend model `utils/viewLegend.ts`
    (`LegendSection`, `channelLegend`, `viewLegendSections`) + `components/ViewLegend.vue`; the image
    strip renders the **channel** section live from the frame's snapshot. Unit-tested.
  - **LEFT:** wire the **populations + colour-by** legend sections into the strip (the model already has
    `viewLegendSections({populations, colourBy})`; the snapshot needs to carry those, or fetch them per
    frame). Only remaining piece of C.
- **~~D. Capture quality~~ — DONE** — D1: the board screenshot uses napari `export_figure` (tight-fit to
  the data extent at native resolution → no black margins). D2: the strip went `object-fit: contain`
  (letterbox) so a scale bar / timestamp isn't cropped (E2's own vector scale bar will let frames go
  edge-to-edge again).
- **E. Scale bar / timestamp for stills** — partial.
  - **~~E1 clean-capture toggle~~ — DONE:** `save_screenshot(clean=True)` hides napari's baked scale bar
    + timestamp for the shot (restored after); threaded `POST /api/napari/screenshot {clean}` → the
    persisted **"clean capture"** toggle in the image-strip ⚙ (`settings.cleanCapture`). Scoped to stills
    (animation keyframes keep the timestamp). See docs/NAPARI.md → *Clean capture*.
  - **LEFT — E2 (stretch):** Cecelia-drawn **vector** scale bar (N µm + label) + timestamp beneath/on the
    frame, from `img_physical_sizes` + time interval (crisp + Illustrator-editable, drawn on the clean
    capture). Needs the captured frame's µm-per-pixel; verify against live napari `export_figure` extent.
    (Movies may keep baked overlays — this is specifically for publication stills; see Decision 7.)
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
