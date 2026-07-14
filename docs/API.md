# HTTP / WebSocket API

The Julia API server (`api/src/server.jl`) exposes the package over HTTP + WS on port
**8080** (Vite dev proxies `/api` and `/ws` → 8080). It binds **`127.0.0.1`** by default
(Cecelia is a local app); set `CECELIA_HOST=0.0.0.0` to expose it on the network. Handlers run on
the thread pool (`-t auto`), so a blocking read doesn't stall the accept loop; Julia HDF5 access is
serialised (`_with_h5`). The package itself (`Cecelia.jl`) is headless and HTTP-agnostic
(`ARCHITECTURE.md`); routes are thin adapters that resolve objects and call package functions.

## Thread safety (`-t auto`)

Every handler runs on the thread pool, so two requests can execute concurrently. Anything they
share must be guarded. All Python runs as a **subprocess** (`run_py`) — there is no in-process
CPython/PyCall, so the GIL is a non-issue. The locks that exist:

| Shared state | Lock | Where |
|---|---|---|
| HDF5 / `.h5ad` access | `_HDF5_LOCK` (`_with_h5`) | `app/src/label_props.jl` |
| Task-spec cache (`_SPEC_CACHE`) | `_SPEC_CACHE_LOCK` | `app/src/tasks/task.jl` |
| Motion-dims cache (`_MOTION_DIMS_CACHE`) | `_MOTION_DIMS_CACHE_LOCK` | `app/src/tasks/tracking/track_measures.jl` |
| Gating pop CRUD (load→mutate→save) | `_POPMAP_LOCK` + atomic temp-file write | `api/src/gating_api.jl`, `app/src/gating/population_manager.jl` |
| Scheduler task registry / pools / cancels | `_TASKS_LOCK` / `_POOLS_LOCK` / `_CANCELLED_CHAINS_LOCK` | `app/src/tasks/scheduler.jl` |
| The single napari bridge process | `_viewer_lock` (`_with_viewer`) — every handler holds it across its send-sequence | `api/src/napari_api.jl` |

`TaskRecord.proc` is `@atomic` (written by a worker, read by `cancel_task!`).

**Known limitation — debug console output capture.** `/api/repl` uses `redirect_stdout`, which
swaps the **process-global** stream. `_REPL_LOCK` serialises evals against each other, but any
*other* handler/task running concurrently with an eval has its `println`/`@info` captured into the
eval's pipe (or missing from the server log) for the eval's duration. Accepted, not fixed: a real
fix needs per-task output capture, disproportionate for a loopback-only, opt-in dev console. The
Settings → Debug console UI shows a note to this effect.

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
| GET | `/api/diagnostics` | server health: threads, Julia version, **installed version** (`.cecelia-version`), memory, bound host/port, projects dir, debug-console state — powers Settings → Diagnostics |
| GET | `/api/diagnostics/packages` | installed-package inventory: `{julia, python, pythonError}` — Julia via in-process `Pkg.dependencies()`, Python via `pixi list --json` (lazy; backs the Settings → Diagnostics "Packages…" dialog) |
| POST | `/api/repl` | `{code}` — evaluate Julia in the server's `Main` (value + captured stdout/stderr + error). **Gated**: only when the debug console is toggled on AND the server is loopback-bound; a `0.0.0.0` bind always refuses it. Localhost-only dev tool (`repl_api.jl`) |
| POST | `/api/repl/config` | `{enabled}` — flip the runtime debug-console toggle (Settings → Developer). Not a security boundary; eval is still loopback-gated |
| GET | `/api/projects`, POST `/api/projects/{list,create,load,rename}` | project CRUD (`load` stamps `lastOpenedAt`). No manual save — see `/projects/boards` + `/projects/canvases` autosaves |
| POST | `/api/projects/boards` | `projectUid, boards:{tabs,layouts}` | debounced **autosave** of the /analysis boards → `settings/analysisBoards.json`. Board images are sidecar files (below), not inline, so the JSON stays small. Mirrors `/projects/canvases`; replaces the old `/projects/save` + manual save button |
| POST | `/api/projects/animations` | `projectUid, animations:{snapshots}` | debounced **autosave** of the Animation page's captured view snapshots → `settings/animations.json`. Frame PNGs are the same sidecar board-assets, so the JSON stays small. Mirrors `/projects/boards`. (`load` returns `animations` alongside `boards`/`moduleCanvases`.) |
| GET/POST | `/api/board-assets`, `/api/board-assets/{save,delete}` | board-image sidecars under `settings/board-assets/<id>.png`. **GET** `?projectUid&assetId` serves the PNG as `image/png` (for `<img>`); **save** `{projectUid,png(base64)}`→`{assetId}` (migrate legacy inline images); **delete** `{projectUid,assetId}` (frame removed). Fresh captures are written directly by `/napari/screenshot` |
| POST | `/api/sets/{create,delete}` | set CRUD |
| GET | `/api/images/meta`, POST `/api/images/{register,delete,channelnames,labels/delete}`, `/api/images/attr/{create,delete,set}` | image CRUD/metadata |
| POST | `/api/images/meta/set` | `{projectUid, values: {uid: {<meta keys>}}}` — generic bulk merge into an image's `meta` dict (physical size/unit, time interval, or any future field) via `_mutate_images!`, same shape idea as `attr/set` but the per-uid value is a partial dict instead of a scalar. Add new `meta` fields here, not a new one-off route. Physical-size/timing edits also propagate into the **`"default"`** (original bioformats2raw) zarr — its `.zattrs` NGFF scale **and axis units** (`update_ome_scale!`) plus the `OME/METADATA.ome.xml` `<Pixels>` attrs (`update_ome_xml_pixels!`) — never the active version, so the acquisition source of truth stays correct and a later `meta/resync` re-derives the edit instead of reverting it (see CLAUDE.md → *OME-ZARR dual-format*). |
| POST | `/api/images/inclusion/set` | `{projectUid, values: {uid: {included?, note?}}}` — set include/exclude and/or the free-text note per image (only the keys present change). First-class `CciaImage` fields, so this rounds through the model via `_mutate_images!`. Excluded images (`included:false`) are greyed in the GUI, unselectable for runs, and hard-skipped by the runners. Same `values`-map shape as `meta/set`. |
| POST | `/api/images/meta/resync` | `{projectUid, imageUids: [...]}` — backfills physical-size/timing `meta` for images imported before that metadata was tracked, by re-reading the `"default"` (original bioformats2raw) zarr — never the active version, see CLAUDE.md → *OME-ZARR dual-format* — via `resync_ome_meta!`/`read_ome_metadata`. Strictly **fill-only** (`overwrite=false`): fills genuinely-absent fields, never clobbers a value already on disk — so it's safe to run on a human-corrected or ImageJ-auto-corrected image (those live only in ccid.json and would otherwise be reverted to bioformats2raw's raw value). No re-import, no source-file access. Returns `{ok, images: {uid: <full image payload>}}` so the frontend can drop the warning icon immediately. |
| GET | `/api/fs/list`, `/api/pools`, `/api/tasks/definitions` | filesystem, pools, task specs |
| GET | `/api/tasks/funparams?projectUid&fun&imageUid?&setUid?` | last-used task params, resolved image → set → none (R `moduleFunParams`; see `docs/MODULES.md` → *Remembering task params*) |
| GET | `/api/chains`, `/api/chains/get`, POST `/api/chains/{save,delete}` | chain templates |
| GET | `/api/chains/runs?projectUid` · `/api/chains/run?projectUid&runId` | list persisted run records / load one run's frozen template + per-node status (Live view run history; see `docs/SCHEDULER.md` → *Loading past runs*) |
| GET | `/api/napari/status`, POST `/api/napari/{open,close,restart,show-labels,show-populations,start-selection,stop-selection,event}` | napari bridge + gating linked brushing |
| POST | `/api/napari/screenshot` | `projectUid` | JSON `{assetId, viewState, imageUid}` of the current napari view. The PNG is written to a **sidecar** file (`settings/board-assets/<assetId>.png`, served via `/api/board-assets`), not returned inline, so the board JSON stays small. Uses napari `export_figure` (tight-fit to the **data extent** at native resolution) so the figure has **no black margins** and matches the viewer — not a tiny image in a big black canvas (plain-canvas `scale` only enlarges the canvas at a fixed zoom, adding margins). The **view snapshot** (camera + dims + per-layer display props) is captured atomically with the shot (folded into the bridge's `save_screenshot` reply) so the frame carries its exact provenance for zoom-to-source (`docs/todo/ANIMATION_PLAN.md`). `400` if napari not running or `projectUid` missing. |
| POST | `/api/napari/apply-view-state` | `viewState` | re-apply a saved snapshot to the running viewer (zoom-to-source restore); image must already be open. Bridge skips missing layers / unsettable attrs. `200 {ok}`; `400` if napari not running or `viewState` missing. |
| POST | `/api/napari/view-state` | — | return the CURRENT view snapshot (camera/dims/per-layer colormap+visibility) of the open image + `imageUid`, via the bridge `capture_view_state` (no PNG side-effect). The Batch movies page uses it to **seed** the config from the first selected image's live colours + overlays. `200 {ok,viewState,imageUid}`; `400` if napari not running. |
| POST | `/api/napari/overlay-legend` | `projectUid,imageUid,colourBy?,pointPops?[{valueName,popType,path}],colourOverrides?` | **read-only** legend for a strip still's overlays (Phase C) — pure Julia, no viewer touched. Returns `{colourBy:{column,items:[{value,colour,label}]}, populations:[{name,colour}]}`: the colour-by section is the pop colour + pop name per value on `colourBy` (same population-colour rule as colour-labels; clusters read as their pop names), the populations section is each requested point-pop's name+colour from its map. Captured with the screenshot so the frame's legend is durable. `200 {ok,colourBy,populations}`. |
| POST | `/api/napari/record-timelapse` | `projectUid,imageUid[,fps,scale]` | record the open image's **T-sweep of the current view** (channels/pops/colour-by as shown) to an `.mp4` via napari-animation (`record_timelapse!` → bridge `record_timelapse` → `napari_utils.record_timelapse`). Saves to `{project}/movies/{imageName}.mp4` (named by the IMAGE, not a segmentation — the view can show several). `200 {ok,path,frames,nTimepoints}`; `400` if napari not running or the image has ≤1 timepoint. F1.1 of the batch-movie work (`docs/todo/ANIMATION_PLAN.md`). |
| POST | `/api/napari/record-animation` | `projectUid,imageUid,keyframes:[{viewState,steps}][,fps]` | render an **interpolated keyframe animation** to `{project}/movies/{imageName}_animation.mp4` (`record_keyframes!` → bridge `record_keyframes` → `napari_utils.record_keyframes`): applies each keyframe's `viewState` + captures it with `steps` tween frames from the previous → camera/contrast/colour/T interpolation. `200 {ok,path,frames,keyframes}`; `400` if napari not running or <2 keyframes. The timeline animation editor's render (F2). |
| POST | `/api/napari/apply-movie-config` | `projectUid,imageUid,config` | **F1.2 preview**: apply an authored movie config to the **currently open** image (no recording), so the user can eyeball the look the batch will record. Reuses the existing open/`show-tracks`/`show-populations`/`colour-labels` handlers via `_apply_movie_config!` (channels→colormap+visibility, overlays, colour-by). `config` = `{valueName,channels:{name→colormap},colourBy,showTracks,trackValueNames,tailWidth,showGatedTracks,showTrackclust,showPopulations,popType,pointsSize,colourLabels,colourOverrides,tStart,tEnd}`. `200 {ok}`; `400` if napari not running or `config` missing. |
| POST | `/api/app/shutdown` | the global "Quit everything" (Settings → System). Best-effort stops children (napari `close!`, notebook server) then `exit(0)` from a detached task so the response flushes first. Dev: ends `pixi run dev`; packaged: server exit ends `app.py`. (`api/src/app_api.jl`) |
| POST | `/api/app/restart` | **dev-only** backend restart (button gated on `diag.dev`). Stops children then `exit(42)` (`RESTART_EXIT_CODE`); the **supervisor** relaunches in place — `api/dev.jl` in dev, `app.py`'s loop in prod. `409` when not supervised (no `CECELIA_SUPERVISED` — a bare `julia src/server.jl`). Replaced the old detached-relauncher, which couldn't reattach to a foreground terminal. |
| GET | `/api/logs/recent` | `{logs: [{level,message}]}` — the server-log ring buffer (last 500), so a freshly-opened console **window** backfills recent lines. Fed by the `BroadcastLogger` tee that also emits the `server:log` WS event (see below). |
| — | **Gating** (below) | population manager + gating |

Task execution + status flow over **WS** (`task:run`/`task:status`/…), not HTTP — see
`ARCHITECTURE.md` and `SCHEDULER.md`. One extra WS message reuses this task-event channel WITHOUT the
scheduler: **`movie:batch`** `{taskId,projectUid,imageUids,config,fileAttrs,fps,scale}` (`handle_movie_batch`,
`api/src/sockets.jl`) kicks off the F1.3 batch-movie run async on the single napari viewer and emits the
same `task:progress/log/status/result` (keyed by the client's `taskId`), so it shows in the task list with
a progress bar + Cancel. `task:cancel` also flags it (`request_batch_cancel!`, stops after the current
image) — napari is a UI-serial viewer in `api/`, not a pooled scheduler task. See `docs/NAPARI.md` →
*Authored config + batch* and `docs/todo/ANIMATION_PLAN.md` → F1.3.
Task events (`task:log`/`task:status`/`task:progress`/`task:result`)
are **broadcast to every connected client** (`_broadcast_task` → `broadcast_ws`), not sent point-to-point
to the launching socket — so a second GUI tab and the read-only **task console** (`api/task_console.jl`,
`pixi run console`) both see live progress. (They're keyed by `taskId`, so clients filter to what they
care about. Chain events already broadcast.) The server also tees its **own** logs (`@info`/`@warn`/
`@error` — startup banner, napari warnings, …) to WS as **`server:log`** `{level, message}` via a global
`BroadcastLogger` installed in `start()` (never under `CECELIA_NO_SERVE`), keeping a 500-line ring
buffer (`GET /api/logs/recent`). This is what makes the Settings console window a real "pixi console",
not just a task log. Broadcast is **decoupled from the caller, per client**:
each connected socket has its own bounded, drop-on-full queue drained by its own background task, and
`broadcast_ws` enqueues a pre-serialised frame onto every client's queue (non-blocking; it skips a
client whose queue is full). This is deliberate — task events fan out on every log/progress line from
many worker threads, so writing sockets inline would let concurrent threads corrupt a shared socket
and let one slow/half-open client block a worker (which strands a pool slot → tasks stuck at `queued`).
Workers must never block on WS I/O. The **per-client** queue (rather than one shared drainer) also
means a single stuck client only ever loses *its own* frames — it can't head-of-line-block delivery to
the other tabs or the console. WS telemetry is lossy-safe: the console reconciles the authoritative
state from `GET /api/tasks`, so a dropped frame self-heals. `handle_task_run` forwards
`queued`/`running` **and
`cancelled`** from `on_status_change` immediately (cancel has no result to order before it), so
cancelling a task — especially a still-**queued** one — reflects at once instead of only when a worker
later dequeues and skips it; `done`/`failed` are held until the result is sent. The console drops its
whole view on reconnect (a localhost drop = server restart), so stale tasks don't linger.

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
| GET | `/api/gating/channels` | `projectUid,imageUid,valueName,popType` | flow/live: `{columns, channels, channelNames, channelNameVersions, obsColumns, trackColourColumns, valueNames}` — gateable feature columns, intensity columns, channel display names (the version whose length matches the intensity-column count; AF correction adds channels). `obsColumns` = per-cell obs measures; **`trackColourColumns`** = the track table's `clusters.*` columns, offered as napari colour-by options (the bridge broadcasts a track column to its cells — see docs/NAPARI.md). **`popType=track`** (and **`trackclust`**, which reads the same track table): `{columns, cellMeasures, trackAggregates, valueNames, valueName, popType}` — `columns` = motility track measures (directly gateable); `cellMeasures` = cell columns aggregatable into track properties; `trackAggregates` = `["mean","median","sum","qUp","qLow","sd"]` (client builds an axis `{measure}.{agg}`; `track_cell_measures` inverts it server-side). **Cluster fields (all branches)**: `clusterSuffixes` (the `clusters.{suffix}` runs in the table), `clusterFeatures` `{suffix→[features]}` + `clusterMembers` `{suffix→[uIDs]}` + `clusterIds` `{suffix→[ids]}` (from the `{props}.clustfeatures.json` manifest + the cluster column) — the heatmap's feature picker, the run's `partOf` membership, and the tickable cluster IDs for the pop-manager |
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

**Copy gating across images** (does NOT return `{tree}`): `POST /api/gating/copy` `{projectUid, imageUid (source), valueName, popType, toImageUids:[…]}` → `{copied:[uid], skipped:{uid→why}}`. Replaces each target's gating sidecar for the ONE gating pop_type (`flow`/`track`; validated via `is_gating_pop_type`) with the source's — membership recomputes per image on read, so gates alone suffice (no per-image recompute). Targets lacking the `valueName` segmentation are skipped. Broadcasts `gating:popmap` per target. Plot layout is copied client-side (canvas store), not here. Ports R "Propagate to Selected".

`POST /api/images/value-name-check` `{projectUid, valueName, imageUids:[…]}` → `{available:[uid], missing:[uid]}` — generic value_name-presence check per image (`img_has_value_name`), NOT gating-specific; the copy dialog uses it (`imagesWithValueName`) to flag/exclude targets without the segmentation up front.

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
| GET | `/api/plots/umap` | `…,popType=clust\|trackclust,suffix,pop?,colourPops?` | **binary** `Float32` interleaved `[x0,y0,code0,popIdx0,x1,…]` (4 floats/point) — the `obsm['X_umap.{suffix}']` embedding + `clusters.{suffix}` code + `popIdx` per point. `clust` reads the cell table; `trackclust` the per-track table (one point per track). Optional `pop` subsets to a population's membership. Optional `colourPops` = comma list of `popType~valueNamePrefixedPath` (e.g. `live~B/qc/_tracked`): the endpoint resolves each point's membership via `pop_df` (grain-matched — `:track` rolls a `live` cell pop up to its track_ids) and sets `popIdx` = its 0-based index in that list (`-1` = none), so the UMAP can colour/facet by the tracked populations (docs/todo/UMAP_COLOUR_FACET_PLAN.md). The UMAP-scatter data source for the cluster module pages. |
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
| POST | `/api/napari/show-populations` | `projectUid,imageUid,valueName?/valueNames?,popType,pointsSize?,show?` | send each pop's `{value_name, label_ids, colour, name, show}` to the bridge → coloured Points layers, one per (segmentation, pop), named `({popType}) ({value_name}) {path}` so pops from different segmentations coexist. **Scope:** an explicit `valueNames` list (or a single non-blank `valueName`) refreshes ONLY those segmentations and the bridge prunes stale layers only within them (`scoped`); **blank → ALL** real segmentations (full refresh + global prune). Live gate edits pass the edited segmentation so a keystroke doesn't recompute every segmentation; open / the master toggle pass blank. Membership per segmentation comes from `resolve_pops` (cached by gating-map + h5ad mtime → an unchanged segmentation is free even on a full push). `200 {ok,n}`; `400` if napari not running. `show:false` sends empty pops → bridge removes the (in-scope) layers |
| POST | `/api/napari/show-tracks` | `projectUid,imageUid,valueNames?,showGatedTracks?,showTrackclust?,tailWidth?,colorBy?` | send the full desired ribbon set to the bridge → napari **Tracks** layers: per-segmentation `_tracked` (from `valueNames`), gated `track` pops (`showGatedTracks`), and `trackclust` cluster pops (`showTrackclust`). Each pop carries `{value_name, pop_type, track_ids, colour, name, show}`; the bridge names layers `({pop_type}) ({value_name}) Tracks {path}` so all coexist, and reconciles (removes any not sent). When `colorBy` is a categorical column, colours follow the **`colour_by_palette` rule** — a value a user population **filters for** on that column takes that pop's colour, else an Okabe–Ito default (server sends the `{value→colour}` overrides; response returns the `legend` `{value→hex}`). `200 {ok,n,legend}`; `400` if napari not running. Requires a time axis. Ports R `show_tracks` (`splitTracks`) |
| POST | `/api/napari/colour-labels` | `projectUid,imageUid,valueName?,column` | recolour the Labels layer by an obs `column` (`""`=reset). Categorical → the same `colour_by_palette` rule (population colour where one filters the value, else Okabe–Ito); continuous → viridis. Server computes `{value→colour}` overrides from the cell pops; response returns the categorical `legend` `{value→hex}`. `200 {ok,legend}` |
| POST | `/api/napari/start-selection` | `projectUid,imageUid,valueName,apiUrl?,zMode?,zWindow?` | tell the bridge to add a `Cell selection` Shapes layer; drawing on it POSTs back to `/event` (`apiUrl` defaults to `http://localhost:8080`). `zMode:"slice"` restricts the selection to ±`zWindow` slices around the live z; `"stack"` (default) selects across the whole z-stack |
| POST | `/api/napari/selection-scope` | `zMode,zWindow` | change the z scope of the **active** selection and re-evaluate the drawn polygon live (bridge re-runs point-in-polygon + z filter → POSTs the new labels). No-op when nothing is drawn (`200 {ok}`) |
| POST | `/api/napari/stop-selection` | `projectUid,imageUid,valueName,popType` | clear the transient "Napari selection" pop (registry + re-broadcast) **and** remove the `Cell selection` Shapes layer from napari (best-effort). Backs the manager's trash button (`200 {ok}`) |
| POST | `/api/napari/event` | `type:"cellSelection",projectUid,imageUid,valueName,popType,labels:[…]` | store the selected label IDs as the transient "Napari selection" pop and **broadcast `gating:popmap`** (empty `labels` clears it) |

The transient pop appears in `gating:popmap` with `"transient": true` and explicit-label
membership; it is queryable like any pop (`plotdata`/`stats`/`membership`) but is never
persisted (`docs/POPULATION.md`).
