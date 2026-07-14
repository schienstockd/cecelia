# Analysis figures & movies (animation)

Status: **planning** (no branch yet). Supersedes the ad-hoc parts of the image-strip tasks
(#00032 legend, #00036 zoom-to-source) by putting them on a shared foundation.

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

- **A. Snapshot atom** *(blocks all)* — settle the JSON schema; bridge `capture_view_state()` +
  `apply_view_state(json)` (whitelist + sanitise + only-present-layers filter); fold capture into the
  screenshot reply. Reuse/extend `save_layer_props`. Round-trip test (incl. colormap change).
  *Checkpoint:* capture → JSON → apply restores camera + T/Z + contrast + colormap.
- **B. Zoom-to-source (#00036)** *(needs A)* — strip cell stores `{imageUid, valueName, snapshot}`;
  a button reopens the image + applies the snapshot. Durable across sessions.
- **C. Strip legend (#00032)** *(needs A, G)* — render channel + population/feature colours (swatch +
  name / µm) below each frame, from the snapshot; toggle in the ⚙ popover.
  - **Backbone DONE (S1):** the shared legend model `utils/viewLegend.ts` (`LegendSection`,
    `channelLegend`, `viewLegendSections`) + presentational `components/ViewLegend.vue`, reused by the
    image strip (channel section, live now) and the animation page. Populations + colour-by sections
    plug into the same model once the snapshot carries them (next). Unit-tested (`viewLegend.test.ts`).
- **D. Capture quality** *(independent, quick)* — D1 hi-res screenshot (`scale`/`size` on
  `viewer.screenshot`); D2 fix edge clipping (strip `object-fit: cover` → `contain`/match aspect, so
  scale bar/timestamp aren't cropped).
- **E. Scale bar / timestamp for stills** — E1 clean-capture toggle (hide napari scale bar +
  timestamp for the shot); E2 (stretch) Cecelia-drawn vector scale bar (N µm + label) + timestamp
  beneath the frame, from `img_physical_sizes` + time interval.
- **G. Split-by-feature colouring** *(feeds C, F1, F2)* — categorical measure → palette applied to
  napari tracks/points; track pop inherits source-pop colour; surface the pop-manager palette into
  overlays + legend. Builds on the existing colour-by-obs (`/api/napari/colour-labels`,
  show-tracks `colorBy`).
- **F1. Batch movie generation** *(needs A, G)* — author a config (table above) → apply per selected
  image (contrast from saved props) → record T-sweep → attr-named `.mp4` per image. "Generate movies"
  button; runs as a task (progress/cancel). *Checkpoint:* reproduce a `runAnimation.Rmd` example.
  - **F1.1 — DONE.** The recording primitive: `napari_utils.record_timelapse` (keyframe→`animate`→mp4)
    + bridge `record_timelapse` + `POST /api/napari/record-timelapse` + a one-click "Record timelapse"
    button that records the open image's CURRENT view to `{project}/movies/{uid}_{valueName}.mp4`. See
    docs/NAPARI.md → *One-click timelapse recording*.
  - **F1.2** (next) — an authored config (channels/colormaps, pops, colour-by, T-range, fps, scale)
    applied to the open image before recording.
  - **F1.3** — batch the config across selected / attr-filtered images (one attr-named `.mp4` each),
    run as a cancellable task.
- **F2. Animation page** *(needs A; big)* — the durable/editable page under the Analysis nav.
  - **MVP DONE (S2):** `/animation` route + nav entry; captures the current napari view as a **view
    snapshot** (screenshot sidecar + view state, the same path as the board strip) into a per-project
    `animations.json` (`/api/projects/animations`, autosaved via `stores/animation.ts`); a gallery of
    snapshots each with its **`ViewLegend`** (S1 backbone) + a **Record movie** button (apply the
    snapshot to the open image → `record_timelapse`, enabled when that image is open).
  - **Render engine DONE:** `napari_utils.record_keyframes` (apply each keyframe's `viewState` → capture
    with `steps` tween frames from the previous → animate) + bridge + `record_keyframes!` +
    `POST /api/napari/record-animation`. Interpolates camera/contrast/colour/T. Offscreen smoke-tested
    (zoom + colormap tween → valid mp4); unit-tested.
  - **Timeline editor (building):** a **row/track matrix** — columns = keyframes (captured `viewState`s
    + duration), rows = **channels · populations · camera**, all *inferred from each `viewState`*
    (layers by type + camera). Capture establishes a base "look" (contrast/colormap/framing); keyframes
    are copies varied via per-cell overrides (toggle `layer.visible`, camera). Data model stays "ordered
    list of `viewState`s" (what the render engine eats). Rows are a projection; a cell edit mutates that
    column's `viewState`. Decision (2026-07-14): **timeline, not a node graph** — a movie is 1-D, so the
    graph's branching buys nothing; see the row-matrix sketch in the session notes.

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
