# HTTP / WebSocket API

The Julia API server (`api/src/server.jl`) exposes the package over HTTP + WS on port
**8080** (Vite dev proxies `/api` and `/ws` → 8080). It binds **`127.0.0.1`** by default
(Cecelia is a local app); set `CECELIA_HOST=0.0.0.0` to expose it on the network. Handlers run on
the thread pool (`-t auto`), so a blocking read doesn't stall the accept loop; Julia HDF5 access is
serialised (`_with_h5`). The package itself (`Cecelia.jl`) is headless and HTTP-agnostic
(`ARCHITECTURE.md`); routes are thin adapters that resolve objects and call package functions.

## Conventions

- **Routing**: a flat dispatch in `server.jl` `handle_http(req, body_bytes)` — GET reads
  `HTTP.queryparams`, POST parses `JSON3.read(String(body_bytes))`. Each handler returns
  `(status::Int, body)`.
- **Response body**: a `String` (→ `Content-Type: application/json`) or an
  `AbstractVector{UInt8}` (→ `application/octet-stream`). The handler in `handle_stream`
  picks the content type from the body type — so a route serves binary just by returning
  bytes. CORS is open (`*`).
- **Object resolution**: `init_object(projectUid, imageUid)` → `CciaImage`/`CciaSet`;
  `projects_dir()` is the root. `api/src/*.jl` is **not** Revise-tracked — restart the
  server after editing routes.
- **Errors**: `{"error": "..."}` with `400` (bad request), `404` (not found). Any uncaught
  handler exception is logged and returned as `500 {"error": "<message>"}` (not an opaque 500).
- **WS broadcast**: `broadcast_ws(Dict(...))` pushes JSON to all connected clients
  (`stores/ws.ts` dispatches by `type`). See WS message reference in `ARCHITECTURE.md`.

### HTTP.jl v2 conventions

- Use `HTTP.listen(handle_stream, host, port)` — **not** `HTTP.serve`. `HTTP.serve` is the
  high-level request→response API; `HTTP.listen` is the stream API that supports WS upgrades.
- Stream handler signature: `handle_stream(stream::HTTP.Stream)` — access the request via
  `req = stream.message`.
- WS upgrade check: `HTTP.WebSockets.isupgrade(req)` then
  `HTTP.WebSockets.upgrade(handle_ws, stream; check_origin=(req, origin)->true)`.
  `check_origin = (req, origin) -> true` is required in dev — the Vite proxy sends a different
  origin.
- HTTP responses in a stream handler: `HTTP.setstatus(stream, N)` + `HTTP.setheader(stream, k=>v)`
  + `HTTP.startwrite(stream)` + `write(stream, body)` — do **not** return an `HTTP.Response` object.
- Read the POST body before writing a response: `body_bytes = read(stream)`, before any
  `HTTP.setstatus` / `HTTP.startwrite`.
- WS message loop: `while true; msg = HTTP.WebSockets.receive(ws); ...` — not `for msg in ws`.

## Route index

| Method | Path | Purpose |
|---|---|---|
| GET | `/api/health` | liveness |
| GET | `/api/diagnostics` | server health: threads, Julia version, memory, bound host/port, projects dir, debug-console state — powers Settings → Diagnostics |
| POST | `/api/repl` | `{code}` — evaluate Julia in the server's `Main` (value + captured stdout/stderr + error). **Gated**: only when the debug console is toggled on AND the server is loopback-bound; a `0.0.0.0` bind always refuses it. Localhost-only dev tool (`repl_api.jl`) |
| POST | `/api/repl/config` | `{enabled}` — flip the runtime debug-console toggle (Settings → Developer). Not a security boundary; eval is still loopback-gated |
| GET | `/api/projects`, POST `/api/projects/{list,create,load,save,rename}` | project CRUD |
| POST | `/api/sets/{create,delete}` | set CRUD |
| GET | `/api/images/meta`, POST `/api/images/{register,delete,channelnames,labels/delete}`, `/api/images/attr/{create,delete,set}` | image CRUD/metadata |
| POST | `/api/images/meta/set` | `{projectUid, values: {uid: {<meta keys>}}}` — generic bulk merge into an image's `meta` dict (physical size/unit, time interval, or any future field) via `_mutate_images!`, same shape idea as `attr/set` but the per-uid value is a partial dict instead of a scalar. Add new `meta` fields here, not a new one-off route. Physical-size/timing edits also propagate into the **`"default"`** (original bioformats2raw) zarr — its `.zattrs` NGFF scale **and axis units** (`update_ome_scale!`) plus the `OME/METADATA.ome.xml` `<Pixels>` attrs (`update_ome_xml_pixels!`) — never the active version, so the acquisition source of truth stays correct and a later `meta/resync` re-derives the edit instead of reverting it (see CLAUDE.md → *OME-ZARR dual-format*). |
| POST | `/api/images/meta/resync` | `{projectUid, imageUids: [...]}` — backfills physical-size/timing `meta` for images imported before that metadata was tracked, by re-reading the `"default"` (original bioformats2raw) zarr — never the active version, see CLAUDE.md → *OME-ZARR dual-format* — via `resync_ome_meta!`/`read_ome_metadata`. Strictly **fill-only** (`overwrite=false`): fills genuinely-absent fields, never clobbers a value already on disk — so it's safe to run on a human-corrected or ImageJ-auto-corrected image (those live only in ccid.json and would otherwise be reverted to bioformats2raw's raw value). No re-import, no source-file access. Returns `{ok, images: {uid: <full image payload>}}` so the frontend can drop the warning icon immediately. |
| GET | `/api/fs/list`, `/api/pools`, `/api/tasks/definitions` | filesystem, pools, task specs |
| GET | `/api/tasks/funparams?projectUid&fun&imageUid?&setUid?` | last-used task params, resolved image → set → none (R `moduleFunParams`; see `docs/MODULES.md` → *Remembering task params*) |
| GET | `/api/chains`, `/api/chains/get`, POST `/api/chains/{save,delete}` | chain templates |
| GET | `/api/chains/runs?projectUid` · `/api/chains/run?projectUid&runId` | list persisted run records / load one run's frozen template + per-node status (Live view run history; see `docs/SCHEDULER.md` → *Loading past runs*) |
| GET | `/api/napari/status`, POST `/api/napari/{open,close,restart,show-labels,show-populations,start-selection,stop-selection,event}` | napari bridge + gating linked brushing |
| POST | `/api/napari/screenshot` | `projectUid` | **binary** PNG of the current napari canvas (`canvas_only`) — `save_screenshot!` to a temp file → stream the bytes → delete. `400` if napari not running. Feeds the Analysis-canvas image / filmstrip slots. |
| — | **Gating** (below) | population manager + gating |

Task execution + status flow over **WS** (`task:run`/`task:status`/…), not HTTP — see
`ARCHITECTURE.md` and `SCHEDULER.md`.

---

## Gating (`api/src/gating_api.jl`)

Population manager + gating engine (`docs/POPULATION.md`). Synchronous, in-process — no
task pool. Common params: `projectUid`, `imageUid`, `valueName` (default = active
labelProps version), `popType` (default `flow`). Mutations persist the per-segmentation
sidecar `gating/{valueName}.json` and **broadcast `gating:popmap`**.

> **`popType="track"` — gate on per-track properties (one point per track).** All gating routes
> accept `popType="track"`: the data source switches from the cell table to the **per-track table**
> (`track_props`, label == `track_id`; motility from `{valueName}__tracks.h5ad` + on-read cell→track
> aggregates — `docs/POPULATION.md`/`docs/TRACKING.md`), and the gate map persists to a separate
> sidecar `gating/{valueName}__tracks.json`. Plot/membership are over tracks (the scatter shows one
> point per track; `cells_in_pop` returns `track_id`s). The transient napari selection (cell labels)
> is **not** injected into a track map. `/api/gating/channels` returns a track-specific shape (below).

### Read

A blank/stale `valueName` is resolved server-side to a real labelProps key (the active one)
— clients may send `"default"` for an image whose segmentation is named otherwise.

> **labelProps has no `_active` pointer.** Unlike `filepath`/`labels`/`imChannelNames`,
> `ccid.json`'s `label_props` is a plain map `{value_name => file}` with **no** `_active` key.
> So `_active_vn` (in `api/src/gating_api.jl`) must **not** use `versioned_active` (that would
> return the literal `"default"`, which isn't a real key, and `label_props(...)` would throw
> `No labelProps for value_name=default`). It prefers an explicit `_active` if present, else
> falls back to the **first real key** (e.g. `"B"`). `_resolve_vn` routes blank/stale requests
> through it. (`api/` is not Revise-tracked — restart the backend after editing this.)

| Method | Path | Params | Returns |
|---|---|---|---|
| GET | `/api/gating/channels` | `projectUid,imageUid,valueName,popType` | flow/live: `{columns, channels, channelNames, channelNameVersions, valueNames}` — gateable feature columns, intensity columns, channel display names (the version whose length matches the intensity-column count; AF correction adds channels). **`popType=track`** (and **`trackclust`**, which reads the same track table): `{columns, cellMeasures, trackAggregates, valueNames, valueName, popType}` — `columns` = motility track measures (directly gateable); `cellMeasures` = cell columns aggregatable into track properties; `trackAggregates` = `["mean","median","sum","qUp","qLow","sd"]` (client builds an axis `{measure}.{agg}`; `track_cell_measures` inverts it server-side). **Cluster fields (all branches)**: `clusterSuffixes` (the `clusters.{suffix}` runs in the table), `clusterFeatures` `{suffix→[features]}` + `clusterMembers` `{suffix→[uIDs]}` + `clusterIds` `{suffix→[ids]}` (from the `{props}.clustfeatures.json` manifest + the cluster column) — the heatmap's feature picker, the run's `partOf` membership, and the tickable cluster IDs for the pop-manager |
| GET | `/api/gating/popmap` | `projectUid,imageUid,valueName,popType` | `{tree}` — nested `{name,gate,filter,children}` |
| GET | `/api/gating/stats` | `…,pop` | `{count, parentCount, pctParent}` |
| GET | `/api/gating/membership` | `…,pops=/a,/b[,binary=1]` | JSON `{membership:{pop:[labels]}}`, or (binary, single pop) raw `Int32[]` |
| GET | `/api/gating/plotmeta` | `…,x,y,pop,xt,yt,<transform params>,densityThreshold,x0,y0` | `{n, mode:"scatter"\|"density", xExtent,yExtent, xLabel,yLabel, xTicks,yTicks}` — `x0=1`/`y0=1` → "whole-dataset" axis for that axis: `[transformed(0), transformed(full-dataset max)]` (max over **all** cells, not the `pop` subset), so the axis stays fixed across populations; omitted/`0` = autoscale to the displayed pop |
| GET | `/api/gating/plotdata` | `…,x,y,pop,xt,yt,…` | **binary** `Float32` interleaved `[x0,y0,x1,y1,…]` (already transformed) |
| GET | `/api/gating/density` | `…,x,y,pop,xt,yt,bins` | **binary** `Float32` grid `bins×bins` (row-major counts) |

**Axis transforms** (per axis, prefix `x`/`y`): `xt=linear\|log\|asinh\|logicle`. Params:
`xfloor` (log), `xcof` (asinh cofactor), `xT,xW,xM,xA` (logicle). Gates and plot
coordinates live in **transformed** space; `xTicks`/`yTicks` give `{pos, label}` where
`pos` is the transformed position and `label` the raw (inverse) value.

### Mutate (POST, JSON body) — each returns `{tree}` and broadcasts `gating:popmap`

| Path | Body (besides project/image/valueName/popType) |
|---|---|
| `/api/gating/pop/add` | `name`, `parent` (default `root`), `colour`, `show`, `gate` (gate spec) or `filter` `{measure,fun,values,default_all}`, `is_track` |
| `/api/gating/pop/set-gate` | `path`, `gate` (gate spec) |
| `/api/gating/pop/update` | `path`, `colour?`, `show?` (recolour / visibility), `filter? {measure?,fun?,values?,default_all?}` (only the keys present are mutated — the tick-cluster-into-pop UX rewrites `filter.values` to retoggle which cluster IDs belong to a `clust`/`trackclust` pop) |
| `/api/gating/pop/delete` | `path` (cascades to descendants) |
| `/api/gating/pop/rename` | `path`, `newName` (cascades child paths) → also returns `path` (new) |

**Gate spec** (JSON, readable by Julia + Python):
```json
{ "kind": "rectangle", "x_channel": "mean_intensity_0", "y_channel": "mean_intensity_1",
  "x_transform": {"kind":"logicle","T":262144,"W":0.5,"M":4.5,"A":0},
  "y_transform": {"kind":"linear"},
  "x_min": 50, "x_max": 1e12, "y_min": -1e12, "y_max": 1e12 }
```
```json
{ "kind": "polygon", "x_channel": "...", "y_channel": "...",
  "x_transform": {...}, "y_transform": {...},
  "vertices": [[x0,y0],[x1,y1],...] }
```

### WS push

`{ "type": "gating:popmap", "projectUid", "imageUid", "valueName", "popType", "tree" }`
— sent after any mutation so all clients re-render the tree (re-entrancy guard on the
client suppresses echo, see `POPULATION.md`).

### Binary parsing (client)

`plotdata`/`density`/membership(`binary=1`) return raw little-endian arrays — read
`response.arrayBuffer()` → `new Float32Array(buf)` (or `Int32Array` for membership). Call
`plotmeta` first for `n`, extents, ticks, and scatter-vs-density mode.

---

## Analysis-plot canvas (`api/src/plotting_api.jl`)

Summary plots (rendered with Observable Plot) — server-side aggregation so Vue never receives raw
cells. Thin wrappers over the package `plot_summary_data` (`docs/ARCHITECTURE.md` layer boundary).

| Method | Path | Params | Response |
|---|---|---|---|
| GET | `/api/plots/umap` | `…,popType=clust\|trackclust,suffix,pop?` | **binary** `Float32` interleaved `[x0,y0,code0,x1,y1,code1,…]` — the `obsm['X_umap.{suffix}']` embedding + `clusters.{suffix}` code per point (unclustered → `-1`). `clust` reads the cell table; `trackclust` the per-track table (one point per track). Optional `pop` subsets to a population's membership. The UMAP-scatter data source for the cluster module pages. |
| GET | `/api/plots/definitions` | `module?` (filter) | flat array of plot-type specs (PACKAGE JSON under `app/src/plotDefinitions/`; each carries `module`, `chartTypes`, `dataSource`, `scopeModes`, `params`). The frontend groups by module (per-module canvas) or shows all (universal canvas). |
| GET | `/api/plots/populations` | `{projectUid, popType?, granularity?,` **image selector:** `setUid [+imageUids subset]` **or** `imageUid}` | populations available across the selected images, **grouped by segmentation** (union; dedup by `(popType,path)` per segmentation, first image wins colour/name): `[{valueName, populations:[{path,name,colour,popType}]}]`. Derived pops (`derived_pop_paths`, e.g. `/_tracked` under `live`) are added since they're injected at query time, not stored. **`granularity="track"` unions `live` + `track` pops** (a track plot shows `live` `/_tracked` *and* track gates from `{vn}__tracks.json`); each population carries the `popType` it must be fetched under, so the panel groups series by popType and issues one `/api/plot_data` request per group. This is the read-only series picker for the summary canvas. (Route is a thin wrapper over the package `plot_population_groups` / `plot_pop_types` — logic + tests live in `app/src`.) |
| GET | `/api/plots/attrs` | `{projectUid, setUid [+imageUids? subset]}` | image-attribute names + distinct values across the set's images: `{attrs:[{name, values:[…]}]}`. Powers the summary canvas "compare → by attribute" picker (group images by e.g. `Treatment`). Single image (no `setUid`) → `{attrs:[]}`. |
| POST | `/api/plot_data` | `{projectUid, popType, granularity:"cell"\|"track", chartType:"histogram"\|"frequency"\|"bar"\|"boxplot"\|"points"\|"matrix", measure, bins?, normalize?, rawPoints?, groupBy?, collapseSeries?, groupAttr? (cross-image: an attribute name or array of names → group images by the combined value, joined with ".", one series per value),` **matrix (heatmap):** `matrixMode:"profile"\|"crosstab", measures:[…] (profile rows), category (categorical col), separator?="_", zscore?, matrixNormalize?:"none"\|"row"\|"col"\|"total",` **population selector:** `series:[{valueName,pop}]` (multi-segmentation) **or** `valueName?+pops:[…]` (legacy single-segmentation)`,` **image selector:** `imageUid` (single) **or** `setUid [+imageUids subset] +scope:"per_image"\|"summarised"` (cross-image)`}` | every (non-matrix) response carries `measureType:"numeric"\|"categorical"` (auto-detected → drives which charts the panel offers) and every series carries `uID` (source image; `""` single-image) + `group` (groupBy level, `""` when none). `groupBy` splits the measure by a categorical column (one series per level); `collapseSeries` pools across pops/segmentations/images so series form only by `groupBy`. histogram → `{…, binEdges:[…], series:[{pop,value_name,uID,group,counts:[…]}]}`; frequency → `{…, categories:[…], series:[{…,counts,values}]}`; bar → `{…, series:[{…,value(mean),sd,sem,ci95,n}]}` (all three error metrics; ci95 = 1.96·sem); boxplot → `{…, series:[{…,q1,median,q3,lower,upper,mean,n,points}]}` (Tukey, outliers omitted; `points`=downsampled raw values when `rawPoints`); points → `{…, series:[{…,points:[…]}]}` (downsampled raw values for strip/violin); **matrix → `{matrixMode, xLabels:[…], yLabels:[…], cells:[{x,y,value,n\|count}], valueLabel, zscore?\|normalize?, category, series:[]}`** — pools the whole frame into ONE grid (profile = measures×category means, z-scorable; crosstab = a `"from<sep>to"` categorical → transition matrix; docs/PLOTS.md §9). Chart type is **independent of the data source**. Cross-image adds `scope` to the response |
| GET | `/api/tracking/motion-dims` | `{projectUid, imageUid, valueName?}` | `{dims:2\|3, zUsed, confidence:"high"\|"low", reason, metrics:{autocorrX/Y/Z, persist2D/3D, nSteps}, valueName}`. Auto 2D-vs-3D recommendation for `tracking.track_measures` — whether the z-axis carries real migration or only jitter (run-form preflight + the task's `dims:auto`). Thin wrapper over the package `detect_motion_dims` (cached by the h5ad mtime). |

Each `series` target is one **plot series** — a population on a specific segmentation, keyed by its
`value_name+path` id (`pop`). Listing several targets overlays populations from **different
segmentations** on one plot (e.g. `[{valueName:"base",pop:"/T cells"},{valueName:"nuc",pop:"/macs"}]`);
in cross-image `per_image` scope each (image, segmentation, pop) is its own series. The legacy
`valueName + pops:[…]` form is still accepted (all pops on one segmentation). `granularity=:track`
reads the per-track table; histogram shares one set of bin edges across series; frequency shares one
category axis (`normalize=true` → within-series proportions). Caching/auto-invalidation is inherited
from `pop_df` (`docs/POPULATION.md`).

---

## Napari ↔ gating (linked brushing)

Bridge a napari viewer to the flow plots (`api/src/napari_api.jl`, `docs/NAPARI.md`). Julia
remains the sole gate evaluator.

| Method | Path | Body | Effect |
|---|---|---|---|
| POST | `/api/napari/show-populations` | `projectUid,imageUid,valueName,popType,pointsSize?,show?` | recompute the map and send each pop's `{label_ids, colour, name, show}` to the bridge → coloured Points layers (`200 {ok,n}`; `400` if napari not running). `show:false` sends empty pops → bridge removes the layers (the viewer-panel master toggle's off state) |
| POST | `/api/napari/show-tracks` | `projectUid,imageUid,valueNames?,showGatedTracks?,showTrackclust?,tailWidth?,colorBy?` | send the full desired ribbon set to the bridge → napari **Tracks** layers: per-segmentation `_tracked` (from `valueNames`), gated `track` pops (`showGatedTracks`), and `trackclust` cluster pops (`showTrackclust`). Each pop carries `{value_name, pop_type, track_ids, colour, name, show}`; the bridge names layers `({pop_type}) ({value_name}) Tracks {path}` so all coexist, and reconciles (removes any not sent). `200 {ok,n}`; `400` if napari not running. Requires a time axis. Ports R `show_tracks` |
| POST | `/api/napari/start-selection` | `projectUid,imageUid,valueName,apiUrl?,zMode?,zWindow?` | tell the bridge to add a `Cell selection` Shapes layer; drawing on it POSTs back to `/event` (`apiUrl` defaults to `http://localhost:8080`). `zMode:"slice"` restricts the selection to ±`zWindow` slices around the live z; `"stack"` (default) selects across the whole z-stack |
| POST | `/api/napari/selection-scope` | `zMode,zWindow` | change the z scope of the **active** selection and re-evaluate the drawn polygon live (bridge re-runs point-in-polygon + z filter → POSTs the new labels). No-op when nothing is drawn (`200 {ok}`) |
| POST | `/api/napari/stop-selection` | `projectUid,imageUid,valueName,popType` | clear the transient "Napari selection" pop (registry + re-broadcast) **and** remove the `Cell selection` Shapes layer from napari (best-effort). Backs the manager's trash button (`200 {ok}`) |
| POST | `/api/napari/event` | `type:"cellSelection",projectUid,imageUid,valueName,popType,labels:[…]` | store the selected label IDs as the transient "Napari selection" pop and **broadcast `gating:popmap`** (empty `labels` clears it) |

The transient pop appears in `gating:popmap` with `"transient": true` and explicit-label
membership; it is queryable like any pop (`plotdata`/`stats`/`membership`) but is never
persisted (`docs/POPULATION.md`).
