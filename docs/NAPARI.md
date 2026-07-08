# NAPARI.md — Napari integration guide

Everything non-obvious about how Cecelia talks to napari.

> Update this file in the same change whenever you modify the bridge protocol, OME-ZARR loading, contrast logic, layer props, or viewer options.

---

## Process model

Napari is **not** embedded in the Julia server. It runs as a separate Python subprocess launched on demand. Communication is a private WebSocket on `localhost:7655`.

```
Browser ──HTTP──▶ Julia server (8080)
                      │
                      │ WebSocket (7655)
                      ▼
               napari_bridge.py
                      │
                      ▼
               napari.Viewer (Qt main thread)
```

`napari_bridge.py` is the entry point. It starts a WebSocket server (asyncio) in a background thread and runs `napari.run()` (Qt event loop) on the main thread. The two are linked by a `queue.Queue` that the bridge drains via `QTimer` every 100 ms.

**Consequence:** every napari API call must happen on the Qt main thread. The QTimer drain is the only safe path. Never call napari APIs from the asyncio thread.

---

## What needs restarting

| File | Reload mechanism |
|---|---|
| `app/src/**/*.jl` | Revise — reload on save |
| `api/src/napari_api.jl` | **Server restart required** — `api/` is not Revise-tracked |
| `napari/napari_bridge.py` | **Napari restart required** — running subprocess, not hot-reloaded. A **server restart is not enough**: `_ensure_viewer!` *adopts* an existing bridge on port 7655, so the stale process survives. Kill the `napari_bridge.py` process (the backend respawns a fresh one with current code on next use) — kill by explicit pid, never inline `pkill -f napari_bridge.py` (the pattern self-matches the invoking shell). |

Changes to `napari_api.jl` that seem to have no effect almost certainly just need a server restart.

---

## Command protocol

Julia sends JSON over the WebSocket; the bridge replies with JSON. All calls are synchronous from Julia's perspective — `send(v, cmd)` blocks until the reply arrives.

```julia
send(v, Dict{String,Any}("type" => "open_image", "path" => zarr_path, ...))
```

The bridge's `execute_command` dispatches on `cmd["type"]`. Adding a new command means adding a branch there **and** a method on `NapariState`.

Errors are returned as `{"type": "error", "msg": "..."}` — `send()` raises on these so Julia's try/catch blocks catch them.

---

## Opening an image

The full call chain for "eye button clicked":

1. Frontend `POST /api/napari/open` with `{projectUid, imageUid, valueName, show3D, asDask, autoSaveProps, autoLoadProps}`
2. `api_napari_open` in `napari_api.jl`:
   - Resolves `zarr_path` via `versioned_get_field(raw, "filepath", value_name)`
   - Falls back to default channel names if the corrected value has no dedicated `imChannelNames` entry
   - If auto-save is on, saves layer props for the currently open image before switching
   - Calls `_do_open!` → sends `set_task_dir` + `open_image` commands to bridge
   - If auto-load is on, sends `load_layer_props` after
3. Bridge `open_image`: loads zarr, reads axes/scale/unit from metadata, calls `viewer.add_image`, sets contrast limits, optionally enters 3D mode

### Pending open

If napari isn't running when the eye button is clicked, `_ensure_viewer!` starts it asynchronously and stores the request in `_pending_open[]`. `_execute_pending_open` fires once the bridge is ready — it **re-resolves** the active filepath at that moment (a task may have finished in between). All options (`show_3d`, `as_dask`, `auto_load_props`) are stored in the pending struct.

---

## OME-ZARR dual layout

Two zarr layouts coexist and both must work:

| Source | Layout | Where multiscales metadata lives |
|---|---|---|
| `bioformats2raw` | Series wrapper: data at `zarr/0/0`, `zarr/0/1`, … | `zarr/0/.zattrs` |
| `create_multiscales()` | Flat: data at `zarr/0`, `zarr/1`, … | root `.zattrs` |

`_series_base(path)` detects which by checking whether `path/0` is a directory whose `.zattrs` contains `multiscales`. The rest of the bridge always works relative to the resolved base.

---

## Byte order (big-endian zarr)

`bioformats2raw` writes zarr arrays with big-endian dtype (`>u2` for uint16). On x86 (little-endian), napari/OpenGL misinterprets the bytes and the image appears empty or garbled.

**Fix applied in correction tasks** (`af_correct.py`, `drift_correct.py`): output arrays are cast to native byte order before writing:

```python
output_np = output_np.astype(input_image.dtype.newbyteorder('='))
```

If a raw (uncorrected) image appears empty, check `zarr_array.dtype` — `>u2` on an x86 machine is the culprit. The original bioformats2raw output is never rewritten; only correction outputs are guaranteed little-endian.

---

## Contrast limits

`layer.reset_contrast_limits()` silently sets `[0, 65535]` for dask/zarr arrays that haven't been computed yet (napari 0.7.1 behaviour — it can't scan an uncomputed array).

Instead we use `_set_contrast_from_sample(layer)`:
- Samples the **coarsest** pyramid level (`raw[-1]`), not the full-res level — this is a contrast
  *estimate*, so reading the smallest level is orders of magnitude less I/O. It matters because this
  runs once **per visible layer**, and with `channel_axis` set each channel is its own layer, so a
  full-res sample would read one full-res plane per channel on every open.
- Indexes the middle position along every axis except Y and X (so for CZYX: middle C, middle Z, all Y, all X)
- Computes 1st–99.9th percentile of non-zero pixels
- Falls back to `reset_contrast_limits()` only if the sample is too sparse or computation fails

This runs on every visible layer after `add_image`.

**OME-XML metadata is parsed once per store.** `_read_unit_from_ome_xml` / `_read_scale_from_ome_xml`
/ `_read_time_increment` all go through `_load_ome_xml`, which is `lru_cache`d on `(path, mtime)` — a
long-lived bridge parses each store's `METADATA.ome.xml` at most once instead of 2–3× per open, and
the lazy `ome_types` import (pydantic model build) is paid only on the first parse.

---

## Writing multiscale zarr (`create_multiscales`)

Tasks that produce corrected images write their own OME-ZARR via `create_multiscales` in `zarr_utils.py`. Rules that must hold for napari to read them correctly:

**Always write zarr v2.** Use `zarr.open_group(..., zarr_format=2)` and ensure all sub-arrays are also zarr v2. If zarr v3 is written (`zarr.json`), `_series_base` won't find `.zattrs` and axis/scale detection will silently fail — the image opens with no scale bar and possibly no channel names.

**Three required metadata fields in `.zattrs`:**

1. `axes` — list of `{name: "t"/"c"/"z"/"y"/"x"}` dicts. Without this napari cannot determine `channel_axis` and will reject a list of channel names.
2. `datasets[i].coordinateTransformations` — `{type: "scale", scale: [...]}` per resolution level. Without this napari shows the scale bar in pixels (1:1) instead of physical units.
3. XY scale at level `i` must multiply by `2^i` (standard power-of-two downsampling pyramid).

The flat layout (`zarr/0/`, `zarr/1/`, … at the root, not inside a `0/` series wrapper) is what `create_multiscales` produces. `_series_base` detects this correctly.

---

## Dask vs zarr loading (`asDask` toggle)

| Mode | How loaded | Trade-off |
|---|---|---|
| Dask (default, ticked) | `da.from_array(zarr_arr, chunks=arr.chunks)` | Fast open; slices computed on demand during pan/zoom |
| Zarr direct (unticked) | `zarr.open_group()[level]` — raw `zarr.Array` | No dask task graph; napari accesses chunks directly |

Neither mode loads the full array into memory. The old numpy option (`np.asarray`) was removed — it would block for minutes on large datasets.

---

## Scale and units

Physical scale is read in priority order:

1. OME-ZARR `coordinateTransformations[0].scale` in `multiscales` metadata (fast, always present for bioformats2raw output)
2. Fallback: OME-XML `METADATA.ome.xml` → `pixels.physical_size_{x,y,z}` (used when the zarr metadata has no coordinateTransformations)

Unit is always read from OME-XML (`physical_size_x_unit`), defaulting to `µm`.

The channel axis is excluded from the scale array before passing to `viewer.add_image` — napari does not want a scale value for the channel dimension.

Units are set per-layer via `layer.units` (a tuple matching the spatial axes). `viewer.scale_bar.unit = None` is intentional — setting a unit string on the scale bar is deprecated in napari 0.7.1; the layer units drive it instead.

---

## Channel names

`open_image` passes `channel_names` to `viewer.add_image(..., channel_axis=c, name=channel_names)`. Napari creates one layer per channel and names them from the list.

If `channel_axis` is `None` (single-channel or no `c` axis in metadata), napari rejects a list for `name` — the bridge collapses it to `name[0]`.

**Channel name fallback in `napari_api.jl`:** corrected images (`afCorrected`, `driftCorrected`, etc.) have no dedicated `imChannelNames` entry in `ccid.json`. The API falls back to the default channel names:

```julia
ch_raw = versioned_get_field(raw, "imChannelNames", value_name)
if isnothing(ch_raw) && !isnothing(value_name)
    ch_raw = versioned_get_field(raw, "imChannelNames", nothing)
end
```

---

## Layer props persistence

Auto-save/load stores napari layer visual properties (contrast limits, colormap, opacity, blending, visible, gamma) as a pickle file:

```
{task_dir}/data/{basename(zarr_path)}.pkl
```

Example: `projects/NRUBxU/1/KDIeEm/data/ccidImage.ome.zarr.pkl`

The `data/` directory is created by `mkpath` in `_try_save_layer_props!` if it doesn't exist. Saving happens **before** the new image opens; loading happens **after**. Load is a no-op if the `.pkl` doesn't exist yet.

Only `Image` layers are saved/loaded. Labels, points, etc. are not included.

---

## Current image tracking

`_current_zarr_path[]` and `_current_task_dir[]` (module-level `Ref`s in `napari_api.jl`) track what's currently open. These are the source for auto-save before switching — if they're `nothing`, no save happens (i.e. first open of the session never tries to save).

They are reset on server restart (the `Ref`s are re-initialised). If napari is closed and reopened mid-session, the refs still hold the old path — auto-save will attempt a `send()` which will fail gracefully (caught by the try/catch in `_try_save_layer_props!`).

---

## Reloading: data vs image

**Reloading a shown image refreshes DATA only — never the image pyramid — unless the user ticks
"reset".** Data reload = re-push the overlays via the existing endpoints (`show-labels`,
`show-populations`, `show-tracks`, `colour-labels`), each of which re-reads from disk and **replaces its
layer in place** (`_remove_layer` then add). The pyramid + camera stay. This is fully frontend-orchestrated
(`ViewerPanel.reloadViewer()` → `pushAllOverlays()`); `POST /api/napari/open` is only for a *full* reopen.

Who triggers what:
- **Image-table eye** on the already-open image → `project.requestNapariReload()` → `ViewerPanel` reloads
  data (full reopen only if reset). A *different* image → full `/api/napari/open`.
- **Task finishes** (with the auto-update toggle on) → data reload (unless reset).
- **`napariResetOnReload`** toggle (viewer panel, `pi-image`, default off) → reload reopens the whole
  image. Needed when a task changed the *pixels* (drift/denoise). Mirrors the old R `viewerManager.R`
  (reopen only on uID change / reset). See `docs/todo/TASK_DATA_REFRESH_PLAN.md`.

Plot/data freshness elsewhere (not napari) rides `project.dataVersion` — a **per-image** version map
bumped for the image a task touched (`ws.ts`, `task:status == 'done'`). Views watch
`dataVersionFor(theirImages)` and refetch only when an image THEY show changed (targeted, not
project-wide), which is why they no longer carry per-plot reload buttons. See
`docs/todo/TASK_DATA_REFRESH_PLAN.md`.

---

## 3D mode

Setting `viewer.dims.ndisplay = 3` programmatically after `add_image` leaves the camera uninitialized for the 3D extent — the image is invisible until the user manually toggles (which calls `reset_view()` internally). Always follow with `viewer.reset_view()`. The `show_3d` toggle is a per-set preference applied *where possible*, so it only takes effect when the image has a z-axis with depth (a 2D image stays 2D — see *Viewer preference scoping*):

```python
if show_3d and (self._z_axis_len() or 0) > 1:
    self._viewer.dims.ndisplay = 3
    self._viewer.reset_view()
```

---

## Labels

`show_labels` looks for `{task_dir}/{value_name}/labels.zarr` and adds it as a napari Labels layer. It uses `_im_scale` from the last `open_image` call, so `open_image` must always be called first.

The labels zarr is also a multiscale pyramid — the bridge loads as many levels as the image has.

**Layer name = `({value_name}) Labels`** — the `.zarr` extension is stripped from the label
filename (so `C.zarr` → `(C) Labels`, not `(C.zarr) Labels`). `colour_labels` targets layers by the
`({value_name})` prefix.

---

## Timestamp overlay (timecourse)

For timecourse data (an image with a `t` axis), `open_image` shows an **elapsed-time text overlay**
(top-left, white) that updates as the `t` slider moves — `t_index × frame_interval`, formatted
`H:MM:SS`. The frame interval (seconds) is read from OME-XML `pixels.time_increment` (with its unit);
when absent it falls back to the frame index (`t = N`). Wired via `dims.events.current_step`. Default
ON for timecourse; hidden when there's no `t` axis. Ports the old `napari_utils.add_timestamp`.

---

## Populations & linked brushing (gating ↔ napari)

Napari and the web flow plots are linked **both ways** around the gating engine — Julia stays
the sole gate evaluator (`docs/POPULATION.md`); napari never evaluates gates.

### Consumer direction — `show_populations` (Julia → napari)

`POST /api/napari/show-populations` → `api_napari_show_populations` recomputes the map and sends
a `show_populations` command per the protocol. The **transient "Napari selection" pop is
deliberately excluded** from what's pushed back to napari — it is the *source* of a selection,
and re-rendering it as a Points layer (on every popmap broadcast) added a layer that stole
napari's active layer, so the user couldn't keep editing the selection shape. It still appears on
the flow plots.

```json
{ "type": "show_populations", "pop_type": "flow", "value_name": "B", "points_size": 6,
  "pops": [ { "path": "root", "name": "root", "colour": "#9ca3af", "show": true,
              "is_track": false, "label_ids": [...] },
            { "path": "/lnk", "name": "lnk", "colour": "#f59e0b", "show": true,
              "is_track": false, "label_ids": [0,1,2,...] } ] }
```

The bridge owns **only** display: it reads cell **centroids locally** from the H5AD
(`cecelia.utils.label_props_utils.LabelPropsView`, via `app/` on `sys.path`), maps each
`centroid-i` (skimage z,y,x order) + temporal `t` onto the image's display axes, and renders one
Points layer per population, coloured by the pop colour (layer `visible` = the pop's `show` flag).
Ports the old `napari_utils.show_pop_mapping`, minus the per-pop CSV crutch. Two details:
- **Layer name = full population path**: `(pop_type) {path}` (e.g. `(flow) /A/B/C`), not the leaf
  name — so nested pops are unambiguous in the layer list. The bridge falls back to `name` only if
  `path` is absent.
- **The root (whole segmentation) is included** as a `"path": "root"` pop (grey), so all cells are
  visible and the user can spatially select from the full segmentation, not just gated children.

**Per-pop reconciliation (no full flush).** `show_populations` does **not** remove and re-add all
layers each call — that full flush was prohibitively slow on CODEX images (many populations ×
many cells). Instead it reconciles per population: layers whose pop is gone are removed; new pops
get a layer; existing layers are **mutated in place** (`layer.data/face_color/size/visible`); and
a layer is **skipped entirely** when its signature `(hash(label_ids), colour, size, visible)` is
unchanged. So a single gate edit touches only the one population (+ descendants) that changed. The
signature cache (`_pop_sigs`) is reset on `open_image`.

Points layers carry the **same `units` as the image layer** (else napari warns "Inconsistent
units across layers" and drops unit-aware rendering). Point size comes from `pointsSize` (old
GUI default 6).

### Consumer direction — `show_tracks` (Julia → napari) — per-segmentation tracks

`POST /api/napari/show-tracks` → `api_napari_show_tracks` shows the tracks of one or more
**segmentations**, one napari Tracks layer per segmentation. A segmentation's tracks = its
`_tracked` cells (all `track_id > 0`), read **directly** from the cell h5ad (`_fetch(img,
vn)(["track_id"])`) — no gating map needed (the per-cell `track_id` IS the membership). The request
takes `valueNames` (per-segmentation `_tracked` toggles), `showGatedTracks` (a global toggle — overlay
the gated track populations from `{vn}__tracks.json`, e.g. TEST/SDGF, across **all** segmentations,
like *Show populations*), and `colorBy`. Each pop carries its own `value_name`:
```json
{ "type": "show_tracks", "tail_width": 4, "color_by": "live.cell.hmm.state.movement",
  "pops": [ { "value_name": "C", "path": "/_tracked", "name": "_tracked", "track_ids": [..] },
            { "value_name": "C", "path": "/TEST", "name": "TEST", "colour": "#f59e0b", "track_ids": [..] } ] }
```
The `_tracked` whole-segmentation pop is read directly from the cell `track_id`; gated pops come from
the `track` map (`cells_in_pop` → track_ids) only when `showGatedTracks` is on.
The bridge builds the napari **Tracks** data `[track_id, t, (z,) y, x]` **per segmentation**
(`_tracks_matrix`, `track_id > 0`, sorted by `(track_id, t)`; cached per `value_name` in
`_tracks_cache`, plus per-vertex labels for colour-by), bin-masks to each pop's `track_id`s, and
calls `viewer.add_tracks(..., color_by, colormap, tail_width, scale, units)`. Details:
- **Layer name = `({value_name}) Tracks {path}`** (e.g. `(C) Tracks /_tracked`) — prefixed by the
  **segmentation**, so A/B/C tracks are distinguishable. Reconcile removes any `" Tracks "` layer not
  in the new desired set (across all segmentations), so toggling one segmentation off clears only it.
- **Recreated on change** (a Tracks graph can't be mutated in place); unchanged layers (signature
  `(value_name, hash(track_ids), tail_width, tail_length, visible, color_by)` in `_track_sigs`) are
  skipped. Caches reset on `open_image`.
- **`color_by`** shades each vertex by an obs column (see the colour-by section above). For a
  **categorical** column the tracks use the **same Okabe–Ito step colormap as the Labels layer**
  (`_categorical_track_colormap`) so a given level (e.g. HMM state 2) is the **same colour on tracks
  and labels**; **continuous** → viridis (also matching labels). Resolved per segmentation; a column
  missing for one segmentation falls back to `track_id` turbo for that one.
- Requires a **time axis** (`_tracks_matrix` empty when no `t`). Units + scale must match the image
  layer (else napari's *"Inconsistent units"* warning disables unit rendering for all layers).
- The track-gating phase (`{vn}__tracks.json` gates) is deferred (`docs/TRACKING.md`); when it lands
  it can add gated track-pop layers alongside these whole-segmentation `_tracked` layers.

Each pop carries a **`pop_type`** (`track` for `_tracked`/gated-track pops, `trackclust` for
track-cluster pops); the bridge names layers `({pop_type}) ({value_name}) Tracks {path}`, so track
and trackclust ribbons (and every segmentation) coexist without colliding.

**UI controls (ViewerPanel).** All POST `/api/napari/show-tracks` **with the full desired set each
time** (`valueNames` + `showGatedTracks` + `showTrackclust`; reconcile clears the rest) — so the
several toggles below share one `pushTracks()` call:
- **Per-segmentation `pi-directions`** toggle in each labels-list row → that segmentation's `_tracked`
  whole-track overlay. Per-image state `settings.get/setTrackVisibility` (default off).
- **Global `pi-directions`** toggle in the options row → the gated track (`track`, `{vn}__tracks.json`)
  populations across all segmentations. `settings.napariShowGatedTracks` (default off); re-pushed on
  `track` `gating:popmap` edits.
- **Global `pi-sitemap`** toggle → the **trackclust** (`{vn}__trackclust.json`) cluster populations as
  ribbons across all segmentations. `settings.popVisible('trackclust')` (default off); re-pushed on
  open and on `trackclust` `gating:popmap` edits.

Row action icons (eye / directions / trash) are hidden until row-hover to keep the narrow sidebar
tidy — an *active* toggle (or an armed delete) stays visible. **Delete** uses inline two-click confirm
(the trash flips to a red `pi-exclamation-triangle`; a second click within 3.5 s deletes) instead of a
browser popup.

**UI controls** (all POST to this route): the **ViewerPanel** has a *Populations* sub-menu with one
toggle **per CELL-grained pop type** — `flow` (`pi-chart-scatter`) and `clust` (`pi-palette`), icons
matching the sidebar module nav — each sending `popType` + `show` (`show:false` → empty pops → bridge
clears that pop type's layers) and a **blank valueName so the server resolves the ACTIVE segmentation**
(where gating/clustering live; `labelNames[0]` was wrong and left clust pops unresolved). The
bridge namespaces point layers by `(popType)`, so flow and clust coexist. State is per-pop-type,
**remembered** (`settings.popVisible`/`setPopVisible`), and auto-applied on open (`onNapariOpened`).
`track`/`trackclust` are **not** here (track-grained → membership is track_ids, not cell labels);
their viz is ribbons via `/api/napari/show-tracks`. The **population manager** has a per-pop visibility column
(`pi-images`, flips the pop's persisted `show` flag via `/api/gating/pop/update` then re-pushes
silently) and a **Napari dots** size slider in its Options box (drives `pointsSize`, re-pushes on
release). The manager's `pi-eye` is unrelated — it highlights on the *flow plots*, not napari.

**Live update while gating**: ViewerPanel subscribes to `gating:popmap`; whenever the tree
changes for the image open in napari (gate edit, pop add/remove/rename, cell selection) **and**
populations are currently shown, it re-pushes `show-populations` so the napari overlay tracks the
gating in real time (mirrors the old `gatePopulationsServer.R` `flowListenToGating` observers).

### Colour tracks + labels by an obs column (`color_by` / `colour_labels`)

Both the Tracks overlay and the Labels layer can be shaded by any cell obs column (e.g. an HMM
state, a `live.track.*` measure, a cluster id) instead of their defaults. Ports the old
`napari_utils.show_channel_intensity` (labels) and `show_tracks(color_by=…)` (tracks).

- **Tracks** — `show_tracks` takes an optional `color_by` column. The bridge reads it per cell
  (`_read_label_column`, cached) and maps it onto each track **vertex** (one row = one cell at one
  timepoint, via `_tracks_matrix`'s per-vertex labels), so a single track is shaded *segment by
  segment* as its value changes over time. `add_tracks(..., properties={col: …}, color_by=col,
  colormap=…)` — categorical → an **Okabe–Ito step colormap matching the Labels layer**
  (`_categorical_track_colormap`; consistent colour scheme across layers), continuous → **viridis**;
  NaN → `-1` (grey). `color_by=""`/`"track_id"` keeps the default per-track turbo
  colouring. `color_by` is part of the layer signature so changing it re-renders.
- **Labels** — `colour_labels(value_name, column)` recolours the `({value_name}.zarr) Labels`
  layer via a **`napari.utils.DirectLabelColormap`** (`_labels_color_dict`): categorical → the
  Okabe–Ito palette per level, continuous → viridis over the `[100-p, p]` percentile range; NaN /
  background / unmapped → transparent. The layer's original colormap is remembered
  (`_labels_orig_cmap`) so `column=""` **resets** it. Routes: `POST /api/napari/colour-labels`
  (labels) and the `colorBy` field on `POST /api/napari/show-tracks`.

> **Future (Leiden track clustering phase):** the old `show_tracks(split_tracks=…)` rendered ONE
> layer per *cluster* value (each a flat colour, independently toggle-able) — that is for clustering
> whole tracks, **not** the per-timepoint colour-by here. A code note in `show_tracks` marks where
> that split-per-value path will be added.

**UI control — "Colour by" dropdown (ViewerPanel).** Next to the Show populations / Show tracks
toggles, a dropdown lists the open segmentation's obs columns (`/api/gating/channels`). Picking one
re-pushes the tracks with `colorBy` (if shown) and POSTs `colour-labels`; "colour: default" resets
both. The choice is remembered **per set** (see *Viewer preference scoping* below) and re-applied on
open — labels recoloured after they're shown, tracks via `pushTracks`. It is **NOT global**: a
colour-by chosen in one experiment must never bleed onto another set's images (that silently
recoloured plain labels — and for a segmentation with no obs columns there's no dropdown to reset it,
so napari's distinct default colouring just vanished). On-open the labels are recoloured **only if the
opened segmentation actually has that column** (`obsCols.includes`); if the current image's
segmentation lacks the set's column the local selection is blanked for display **but the persisted
per-set value is kept** (another image in the set may have it — it's restored per image on open).

### Viewer preference scoping — global / per-set / per-image

napari viewer preferences persist at **three** scopes; the rule is: **per-image** when applying it to
the wrong image is destructive or can't be undone from the UI; **per-set** when it's an experiment-level
viewing choice you set once and hold across a set's images; **global** when it's a workflow/UI
preference that either no-ops or shows something obvious-and-toggleable everywhere.

| Scope | Settings (store keys) | Why |
|---|---|---|
| **Per-image** (`getLabelVisibility`/`getTrackVisibility`, keyed by image uid) | which of *this image's* segmentations show labels / tracks (the per-segmentation rows) | segmentations differ image to image; row state is inherently image-specific |
| **Per-set** (`get/set{ColourBy,Show3D,ShowGatedTracks,PointSize,PopVisible}`, one `cc.napariSetPrefs` map keyed by **set uid**) | colour-by · show-3D · gated-tracks toggle · point size · per-popType overlay visibility | one experiment = consistent viewing; set once, holds as you click through the set. Bleed to *another* set is prevented, but re-picking per image is avoided |
| **Global** (plain `localStorage`) | update-on-task · reset-on-reload · auto-save-props · as-dask · task-follow · auto-refresh · sidebar/right collapse | workflow/UI prefs, not viewing state |

The set uid for the open/gated image comes from `projectStore.setUidOfImage(imageUid)`. Historical note:
the old R app made these **global** (Shiny bookmarks made that easy), but per-set was always the intent —
global colour-by is exactly what silently broke plain-label colouring across images. **show-3D** is
applied "where possible": the bridge only switches to `ndisplay = 3` when the image has a z-axis with
depth (`_z_axis_len() > 1`), so a 2D image opened with the set's 3D toggle on stays flat.

### Producer direction — cell selection (napari → flow plots)

"I see these cells in XY — where are they in channel space?" `POST /api/napari/start-selection`
→ `start_cell_selection` adds a **`Cell selection` Shapes layer** in polygon mode and wires its
`events.data`. napari can only edit Shapes in 2-D, so this also drops the viewer to
`dims.ndisplay = 2` (you can't draw on a 3-D render). The layer carries the image's `scale` +
`units` — so the polygon aligns with the cells and napari doesn't warn *"Inconsistent units
across layers"*. When the user **closes a polygon**, the bridge automatically point-in-polygons
the cell centroids (in the **currently displayed** dims — other dims like z/t are ignored, so it
selects across slices) and POSTs the inside label IDs back (no key press / polling). The polygon
vertices are 2-D (in-plane) even on an N-D image, so they're indexed by their own columns, not the
viewer dim indices (which would overflow). Mid-draw events (a polygon with <3 vertices) are ignored
so the API isn't spammed with empty selections while clicking; clearing all shapes clears it.

**z scope.** By default the polygon selects across the whole z-stack (z is ignored). The gating
bar's **Z toggle** (next to the draw-region button) switches to `z_mode="slice"`, which keeps only
cells whose z-centroid is within ±`z_window` slices of the **currently displayed** z — read *live*
when the polygon closes, so scrolling to a different slice before finishing selects on that slice.
`z_window` (the ± stepper, 0 = current slice only) and the mode live in `_sel_ctx`. Changing the
toggle/stepper **re-evaluates the already-drawn polygon immediately** via `POST
/api/napari/selection-scope` → `update_selection_scope` (updates `_sel_ctx` then re-runs
`_on_selection_changed`), and the value is also picked up by the next `start_cell_selection`. No-op
on images without a z axis, and when no selection is active. `t` is always ignored (selects across
timepoints).

On `open_image` the viewer's **axis labels** are set to the dimension names (`t`/`z`/`y`/`x`, channel
excluded) so the sliders read meaningfully instead of `-1`/`-2`.

```
POST {api_url}/api/napari/event
{ "type": "cellSelection", "projectUid", "imageUid", "valueName", "labels": [12, 87, ...] }
```

`api_napari_event` stores them in an in-memory registry keyed by `(task_dir, value_name)` and
broadcasts `gating:popmap`. The map served/broadcast everywhere injects these as a **transient
population** "Napari selection" (cyan, explicit-label membership, `transient: true`) — so the
flow plots light up exactly those cells with no new persisted population. The selection is
**never written to disk** (`save_pop_map!` drops transient pops); an empty `labels` list clears
it. The POST runs on a daemon thread so the Qt UI never blocks. The population manager shows the
transient pop with a **trash button** that clears it (`store.clearNapariSelection` →
`POST /api/napari/stop-selection`): the server clears the registry, re-broadcasts the tree without
the pop, **and** sends the bridge a `remove_layer` for the `Cell selection` Shapes layer — so
deleting the selection also removes its draw layer. The frontend then prunes the now-dead path
from every plot's highlight set, so a plot with no remaining selection reverts from the dimmed
overlay backdrop to normal pseudocolour/contour. The plotdata path is also hardened: a plot still
pointing at a since-cleared selection gets empty data back, not a 500. See `docs/POPULATION.md`.

## What isn't implemented yet

From the old R/Shiny viewer options — not yet ported:

- `show_neighbours` — neighbourhood graph overlay
- `show_shapes` — tissue region shapes (the gating selection layer is unrelated)
- `show_branching` — branching structure overlay
- `squeeze` — squeeze length-1 dimensions before display
- `downsample_z` — subsample Z for faster 3D rendering
- `as_mip` — maximum intensity projection along Z

These will be added as separate toggle buttons and bridge commands as needed.
