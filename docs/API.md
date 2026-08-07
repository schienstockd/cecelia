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

- **Routing**: two lookup tables in `server.jl` — `_GET_ROUTES` / `_POST_ROUTES`, both
  `Dict{String, Function}` of `path => (req, body_bytes) -> handler(...)`. `handle_http` picks the
  table by method, looks the path up and calls it (miss → `404`, unknown method → `405`). GET reads
  `HTTP.queryparams`, POST parses `JSON3.read(String(body_bytes))`. Each handler returns
  `(status::Int, body)`. **Add a route by adding a table entry** — one line, same as before.
  > **Do NOT turn these back into an `if/elseif` chain.** They used to be one, and it cost **42 s of
  > a 53 s server boot**, on every restart. Compiling a 156-branch chain forces the compiler to infer
  > *every* handler call though exactly one runs, and it lands in a single method (the
  > `handle_stream` closure that inlines it). With a table only the invoked handler compiles: boot
  > 54 s → 11 s, compile 50 s → 6 s. Splitting the chain into per-method functions does **not** help
  > — measured, no change — because it is still one compilation request. A table is also faster per
  > request (a hash lookup, not up to 156 string comparisons). Pinned by the *"HTTP router — the full
  > route table still dispatches"* testset in `api/test/runtests.jl`.
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
| GET | `/api/diagnostics` | server health: threads, Julia version, **installed version** (`.cecelia-version`), memory, bound host/port, projects dir, **`setupRequired`** (first-launch flag → `/setup` redirect), debug-console state — powers Settings → Diagnostics |
| GET | `/api/diagnostics/packages` | installed-package inventory: `{julia, python, pythonError}` — Julia via in-process `Pkg.dependencies()`, Python via `pixi list --json` (lazy; backs the Settings → Diagnostics "Packages…" dialog) |
| POST | `/api/repl` | `{code}` — evaluate Julia in the server's `Main` (value + captured stdout/stderr + error). **Gated**: only when the debug console is toggled on AND the server is loopback-bound; a `0.0.0.0` bind always refuses it. Localhost-only dev tool (`repl_api.jl`) |
| POST | `/api/repl/config` | `{enabled}` — flip the runtime debug-console toggle (Settings → Developer). Not a security boundary; eval is still loopback-gated |
| GET | `/api/projects`, POST `/api/projects/{list,create,load,rename}` | project CRUD (`load` stamps `lastOpenedAt`). No manual save — see `/projects/boards` + `/projects/canvases` autosaves |
| POST | `/api/projects/boards` | `projectUid, boards:{tabs,layouts}` | debounced **autosave** of the /analysis boards → `settings/analysisBoards.json`. Board images are sidecar files (below), not inline, so the JSON stays small. Mirrors `/projects/canvases`; replaces the old `/projects/save` + manual save button |
| POST | `/api/projects/animations` | `projectUid, animations:{snapshots}` | debounced **autosave** of the Animation page's captured view snapshots → `settings/animations.json`. Frame PNGs are the same sidecar board-assets, so the JSON stays small. Mirrors `/projects/boards`. (`load` returns `animations` alongside `boards`/`moduleCanvases`.) |
| GET/POST | `/api/board-assets`, `/api/board-assets/{save,delete,copy}` | board-image sidecars under `settings/board-assets/<id>.png`. **GET** `?projectUid&assetId` serves the PNG as `image/png` (for `<img>`); **save** `{projectUid,png(base64)}`→`{assetId}` (migrate legacy inline images); **delete** `{projectUid,assetId}` (frame removed); **copy** `{projectUid,assetId}`→`{assetId}` duplicates a PNG to a new id so a **duplicated board** owns independent assets (deleting a frame in one board can't orphan the copy); `404` if the source is missing. Fresh captures are written directly by `/napari/screenshot` |
| GET/POST | `/api/lablog?projectUid` · `/api/lablog/{append,capture,dismiss}` | per-project **lab log** — an append-only markdown file `{proj}/lab-log.md` (cross-session AI+human analysis memory; see `docs/ai-assist/LAB-LOG.md`). **GET** → `{content, entries:[{date,author,lines,raw}] newest-first, dismissed:[entryId…], imageNames:{uid→name}, mtime}` (`dismissed` = entry ids hidden from the panel view; `imageNames` = current uid→name map — the log stores **stable image UIDs**, the panel's "Show names" toggle resolves them to names on demand). **POST /append** `{projectUid, author, lines: string \| [string]}` → appends one `## <date> [<author>]` block; server **injects the date + author tag** (callers never send the header); **append-only** + project-lock-guarded (`with_transaction`). **POST /capture** `{projectUid}` → appends an auto-generated `[Cecelia]` digest of net activity since the last capture, **grouped by module category** (task-manager tags) and **collapsed across images**; idempotent via a `settings/lab-log-context.json` snapshot; `{ok, captured, block, entries}`. **POST /dismiss** `{projectUid, id, dismissed}` → hide/un-hide one entry from the panel view → `settings/lab-log-dismissed.json` (config sidecar; the log file is never edited); `{ok, dismissed}`. `400` empty/missing field; `404` unknown project. |
| POST | `/api/sets/{create,delete}` | set CRUD |
| GET | `/api/images/meta`, POST `/api/images/{register,delete,move,channelnames,labels/delete}`, `/api/images/attr/{create,delete,set}` | image CRUD/metadata. `move` `{projectUid, imageUid, fromSetUid, toSetUid?\|newSetName?}` reassigns an image to another set in the same project — manifest-only (no data moves on disk, see `move_image!`); pass `newSetName` to create the destination on the fly. Returns `{ok, toSetUid, toSetName, createdSet}`.<br>**`attr/*` normalise user input on write** (`_norm_attr` = trim): these three routes are the only place free-text attribute names/values enter the model, so `"a"` and `" a "` can't become two distinct values (two filter chips, two movie-name segments) or `" Location"` a second column. Whitespace-only collapses to `""` — the canonical *unset*, deliberately **not** a delete, since `attr/create` seeds a new column with `""` on every image and the key's presence is what makes the column exist. `attr/set` therefore returns `{ok, attrName, values}` echoing **what was stored**; callers must update local state from that, not from what they sent, or the UI shows the untrimmed input while the file holds the trimmed value (and trimming client-side too would mean two normalisers). |
| GET | `/api/images?projectUid` | **read-only** project listing for the MCP observer: `{name, kind, count, sets:[{uid,name,imageCount}], images:[{uid,name,status,setUid,setName}]}`. Unlike `POST /api/projects/load` this has **no side effects** (load stamps `lastOpenedAt`), so the observer can enumerate images while staying non-mutating. Backs `get_project_info` + `list_images` (see `mcp/`, `docs/ai-assist/OBSERVER.md`). |
| GET | `/api/images/geometry?projectUid&imageUid&valueName?` | frame extent of ONE image **version** — `{sizeX,sizeY,sizeZ,sizeT,valueName}`, read off that version's zarr (metadata only, no pixels). Omit `valueName` for the **active** version, which is what a task runs against. **Ask this rather than storing a size on the image**: the extent is version-dependent — drift correction expands the canvas (`output.canvas_expansion`), a crop shrinks it — so EaMaVq is 512×512 as imported and 544×548 corrected. Implemented in `api/src/image_geometry.jl`, which also backs `/api/crop/*`. |
| GET | `/api/images/stores?projectUid&imageUid` | What each of an image's stored things IS on disk — encoding + size: `{versions: {valueName: {bytes, label?, codec?, level?, shuffle?, zarrFormat?, ngffVersion?, chunks?, shard?} | null}, labels: {valueName: {bytes}}}`. The codec is read from each store's level-0 `.zarray` (metadata only, no pixels — same sanctioned carve-out as `/geometry`, `api/src/image_geometry.jl`); `label` is the name Settings → Storage uses for it so the two surfaces agree, a codec outside `IMAGE_COMPRESSOR_CHOICES` gets a descriptive label instead (`blosc/lz4-9`), and matching includes the WRAPPER — a blosc-wrapped unshuffled zstd is not the bare `zstd` choice. `null` for a version with no registered filepath; codec fields **omitted** (and `bytes: 0`) when its store is missing or unreadable, so the caller shows `—` for that row without losing the others. A label row's `bytes` sums its files (base + nuc). **Layout as well as codec** — `zarrFormat` (2 or 3), `ngffVersion` (the OME-NGFF spec the store declares, a different question from the zarr format), the `chunks` shape and the `shard` shape. For a SHARDED array `chunks` is the inner chunk (the unit of compression) and `shard` is the outer grid (one file on disk) — easy to report the wrong way round, so the `ZARRFMT` fixtures deliberately use shard ≠ chunk. `shard` is **null when unsharded** rather than omitted, because "not sharded" and "unreadable" are different answers. Both formats coexist on disk indefinitely (no converter — `docs/todo/ZARR_V3_PLAN.md` D7), which is why this is surfaced at all. EVERY version in one call: the consumer (the image-metadata modal) lists them all. **`bytes` is a directory walk** (`Cecelia._path_bytes`) — measured 0.24 s warm for a whole image (3 versions of ~4 GB / 10k chunks + 3 label sets), ~2 s per store on a cold cache. Affordable because the modal is opened deliberately for one image and fills the section in after it is on screen. Do not fold it into the `/api/images` listing; the project-wide walk belongs to Settings → Storage. |
| GET | `/api/images/tasklog?projectUid&imageUid&fun[&since]` | raw task log for one `fun` on one image — reads `{img._dir}/logs/{fun}.log` (written by the scheduler's `_wrap_log_with_file`). `{projectUid, imageUid, fun, exists, content[, bytes]}`; `exists:false`+`""` when never run. `fun` is filename-sanitised (rejects `/`,`\`,`..`). **`since`** (a task's ISO-8601 UTC `started_at`) returns only that RUN's lines: the file is cumulative — one per image+fun, appended by every run — and its lines are stamped in *local* time, so the slice happens server-side where the clock that wrote them lives (`_tasklog_since`; stdlib has no tz database, so the offset is `now() - now(UTC)` and a run straddling a DST change can be off by an hour). Used by the GUI to backfill an adopted row's log. Read-only; backs the observer's `get_task_log`. |
| GET | `/api/tasks/history?projectUid[&limit]` | recent task runs across **all** images, newest first: `{count, history:[{imageUid,imageName,status,runStatus,fun,valueName,at,params}]}` — aggregates each image's `runlog.json`. `status` = the image's current status; `runStatus` = that run's outcome (`"done"`/`"failed"` — failures are recorded too so repeated failures are visible; legacy entries → `"done"`). `params` = the params that run used (the tuning trail; `{}` on legacy entries) — lets the observer suggest a param adjustment on an outlier. `limit` default 100. Read-only; backs the observer's `get_task_history`. |
| GET | `/api/tasks/recent[?since]` | the banked terminal frames of recently finished task-rail work, oldest → newest: `[{id,status,fun_name,pool_name,image_uid,image_uids,started_at,finished_at}]` (`recent_tasks()`, a bounded in-memory log — see `docs/SCHEDULER.md`). `started_at` (`""` when the unit never ran) is what lets a recovered frame report the task's real duration instead of the poll delay — see *Elapsed time is served, not guessed* below. The companion to `/api/tasks`: that answers *what is in flight*, this *how the ones that left it ended*. Exists because the terminal `task:status` frame is dropped for a slow client by design, so a client needs a lossy-safe way to learn the outcome (see the WS section below). Written at the rail's two status *sinks* — `ws_status` and the `chain:node:done`/`failed` bridge — so it covers **every** producer: scheduler tasks, chain nodes, background jobs (`pool="job"`), batch movies (`pool="viewer"`). `image_uids` = every image the unit touched (a set-scope task's full member list, which exists only on that frame). `since` = a previous poll's newest `finished_at`, **inclusive** (two units finishing in the same millisecond must not fall through the gap — de-duplicate by `id`). Not run history: that is `/api/tasks/history`, on disk and permanent. Read-only. |
| GET | `/api/storage/summary?projectUid` | Settings **storage box** (`api/src/storage_api.jl`). Walks every image store (expensive — the frontend calls it on a "Scan" button, not on open) + `diskstat`: `{diskTotal, diskAvailable, imageBytes, reclaimableBytes, reclaimable:[{imageUid,name,setUid,bytes,activeVersion,versions:[{valueName,bytes}]}]}` (`imageBytes` = image OME-ZARR versions only, NOT labels/other task-dir data). `reclaimable` = one entry per image with freeable versions — every version EXCEPT the active one (`reclaimable_versions`), incl. the original `default`; biggest first. |
| POST | `/api/storage/reclaim` | `{projectUid, imageUids:[…]}` → free every NON-active version of each image, keeping only the active one (shared `reclaim_inactive!` / `remove_image_version!`; the active version is never touched, so its channel names/dims survive). `{ok, freedBytes, reclaimed:[uid]}`; images with nothing to reclaim are skipped. `400` empty `imageUids`. |
| GET | `/api/storage/compressor` | Image-store compression (Settings → Storage, advanced): `{current, default, choices:[{name,label,detail}]}`. The choice list + its measured trade-off text are SERVED, not duplicated in Vue — same rule as task param specs. Source: `app/src/config.jl` → `IMAGE_COMPRESSOR_CHOICES`. |
| GET/POST | `/api/storage/layout` · `/api/storage/layout/set` | Store LAYOUT defaults: `{ngffVersion, chunkSeparator, defaults, ngffVersionChoices, chunkSeparatorChoices}`. Unlike the compressor these are **defaults the import form pre-fills**, not a switch over what happens next — format and separator are fixed per image at import (there is no converter, `docs/todo/ZARR_V3_PLAN.md` D7) and derived stores inherit from their source (D9/D11). The choice list is served, never duplicated in Vue. `set` takes `{key, value}` and rejects an unknown key or value rather than persisting it, since it writes `custom.toml`. Note `chunkSeparator: "flat"` and `ngffVersion: "0.5"` cannot be combined — bioformats2raw silently writes zarr v2 for that pair — so the import drops 0.5, warns, and the UI flags it. |
| POST | `/api/storage/compressor/set` | `{name}` → persist `[zarr].imageCompressor` to `custom.toml` + hot-reload, so the NEXT task writes with it (no restart). `{current}`; `400` on an unknown name. Existing stores are untouched — `python/cecelia/utils/rechunk_zarr.py` re-lands them. Label stores are deliberately not configurable (`zarr_utils.LABEL_COMPRESSOR`). |
| GET | `/api/movies?projectUid` | list the project's rendered `.mp4`s under `{project}/movies/` for the **Movies** player (`/movies`, `MoviesModule.vue`): `{movies:[{name,size,mtime}]}`, newest-first. Empty list (not 404) when the folder doesn't exist yet. Bytes are streamed separately by `/api/movies/file`. |
| GET | `/api/movies/file?projectUid&name` | **range-served** movie bytes as `video/mp4` for the `<video>` player — the server's only `Accept-Ranges: bytes` route (a `<video>` element issues `Range` requests in every browser, so this is what makes seeking work). No `Range` → `200` full body; `Range: bytes=…` → `206 Partial Content` with `Content-Range` + only that slice; streamed in 64 KB chunks (never buffers the whole file). Served at the **stream** level (`try_serve_movie` in `server.jl`, needs the socket for partial writes), not the JSON router. `name` must be a sanitised `[A-Za-z0-9._-]+.mp4` (blocks traversal); unknown/invalid → falls through to `404`. |
| GET | `/api/qc/cohort?projectUid&setUid&funName[&valueName][&run][&threshold]` | **READ-ONLY**: recompute + return the **cohort QC summary** for one (task, output) across a set's *included* images: `{funName, valueName, nIncluded, metrics:{<key>:{n,median,mad,mean,sd,threshold,outliers:{uid:{value,z|relDev}}}}}`. Aggregates the per-image counts banked by `write_qc` (segment/measure/tracking); `funName` must be a known metric producer (`COHORT_METRICS`), else 400. Outliers use a **robust modified z-score** (median/MAD, Iglewicz–Hoaglin) so a clear anomaly flags even at n=3 and one bad image can't mask itself; the entry carries `z` (that score), or `relDev` (relative departure) when the cohort has no spread (MAD 0, ≥half identical — e.g. `[800,800,100]`). `threshold` = the z cutoff (default 3.5). No `valueName` → `{funName, valueNames, byValueName}` over every value_name the fun banked. `run` restricts to one **clustering run**'s value_names (see `/runs`). Writes **nothing** (a GET must be safe) — feeds the observer's cohort view. |
| GET | `/api/qc/cohort/runs?projectUid&setUid&funName` | The distinct **clustering runs** a fun banked across the set → `{funName, runs:[{run, valueNames}]}`, newest first. Clustering banks QC PER RUN under composite `{labelSet}.{suffix}` keys (`write_cluster_qc!`, e.g. `T.movement`/`T.test`), so a later run no longer overwrites an earlier one. `[]` for funs that keep no runs (segment/tracking/HMM). Cheap (scans QC filenames + each doc's `runSuffix`, no cohort math). Powers the Check-cohort button's run selector. |
| POST | `/api/qc/cohort/check` `{projectUid, setUid, funName[, valueName, run, threshold]}` | The explicit **"check cohort consistency"** action — same computation as the GET, but PERSISTS: writes the per-set sidecar `{proj}/1/{set_uid}/qc/cohort/{funName}/{valueName}.json` AND per-image `cohort.{funName}` findings so outliers surface on the image (table indicator, whiteboard, lab log, MCP). `run` checks only that clustering run's value_names (default: all). The ONLY cohort write path. Returns the same summary doc. |
| GET | `/api/analysis/chains?projectUid` | **READ-ONLY**, project-level: the whiteboard **chains** — `templates: [{name, nodes: [{id, fun, scope}], edges: [{from, to}], startTargets}]` (the wired pipeline DAG) + `runs: [{id, chainName, at, imageCount, nodeStatus}]` (recent executions, node-outcome roll-up). The INTENDED pipeline + actual runs, distinct from the run-log window. `chains_summary`, Slice E. |
| GET | `/api/analysis/behaviour?projectUid[&imageUid][&setUid]` | **READ-ONLY**: per-image **HMM behaviour distribution** — `behaviour: [{valueName, kind: state\|transitions, column, n, nStates\|nDistinct, distribution: [{value, n, fraction}]}]` from the `live.cell.hmm.*` obs columns. `n` = decoded cells. Slice D. |
| GET | `/api/analysis/clusters?projectUid[&imageUid][&setUid]` | **READ-ONLY**: per-image **clustering summary** — `clusters: [{valueName, suffix, granularity: cell\|track, nClusters, n, largestFrac, sizes: [{value, n, fraction}]}]` (one per segmentation × run) + a top-level `featuresByRun: {suffix => features}` (each run's feature list once, not per entry). `largestFrac`→1 or low `nClusters` vs peers = collapsed clustering. Slice D. |
| GET | `/api/analysis/measures?projectUid[&imageUid][&setUid]` | **READ-ONLY**: per-population **phenotype + motility summaries**. Per image `summaries: [{population, valueName, kind: phenotype\|motility, n, measures: [{name, n, median, q25, q75, mean}]}]` — phenotype = per-cell channel intensities (channel-named) + morphology; motility = per-track `live.track.*`. Summarised over the MEANINGFUL populations (gated pops like `T/_qc` when present, else the base `_tracked`/all-cells), not the raw segmentation. Heavier than lineage/populations (reads cell data via `pop_df` with column pushdown — no raw rows); prefer image/set scope. `measure_summary`, Slice C of `docs/todo/OBSERVER_DATA_ACCESS_PLAN.md`. |
| GET | `/api/analysis/populations?projectUid[&imageUid][&setUid]` | **READ-ONLY**: per-image **population definitions** — the detail behind the lineage's `gatedPops`. Each pop `{path, name, parent, popType, valueName, colour, isTrack, gate, filter}`: `gate` (flow/track) = the drawn `{kind, x_channel, y_channel, transforms, geometry}`; `filter` (clust/live) = `{measure, fun, values}` (e.g. `clusters.movement in {3}`, which also links the run). `truncated` flags a capped list. Definitions only — cheap sidecar read; membership counts are the measure slice. `populations_summary`, Slice B of `docs/todo/OBSERVER_DATA_ACCESS_PLAN.md`. |
| GET | `/api/observer/briefing?projectUid` | **READ-ONLY**, project-level: the observer **session briefing** — `{projectUid, projectName, imageCount, flagged:[{uid,name,worst,findings:[{level,short}]}], recentLabLog:[{date,author,summary}]}`. `flagged` = images with a warn/fail QC finding (same source as the image table, `all_qc_docs`); `recentLabLog` = last 7 days, newest-first. The startup context a Chat-to-Claude session pulls first. `session_briefing`, Observer Phase 2 §2. |
| GET | `/api/observer/status[?projectUid]` | availability + setup for the in-app assistant: `{available, models, defaultModel, prompt, mcpConfigPath[, session]}`. `available` = an assistant CLI is on PATH (drives the disabled-with-why gate; resolved via `agent_bin_path`, which also tries `.cmd`/`.bat` on Windows — `Sys.which` alone never finds an npm-installed `claude.cmd`, which made the whole feature invisible there); `prompt` = the system prompt it runs under (transparency). **`mcpConfigPath`** is the resolved observer MCP config, (re)written on every call by `_write_observer_mcp_config` (the fallback line if one-click setup fails). **`terminal`** = `{state, ready, shadowedDirs}` for the user's OWN terminal — `state` ∈ `missing`/`stale`/`shadowed`/`current` from reading Claude Code's `~/.claude.json` (`observer_registration_state`; `CLAUDE_CONFIG_DIR` honoured), `ready` only for `current`. A *stale* entry (another checkout's interpreter, or a different API port) is NOT ready: it fails silently in the user's session. *shadowed* = registered correctly at `user` scope but overridden by a per-directory `local`-scope entry (`projects[<dir>].mcpServers`, which Claude Code resolves FIRST), listed in `shadowedDirs` — a dead one there kills the server with `ENOENT` and the tools are simply absent, which read as "the setup button does nothing" while status claimed `current`. This drives which button the lab-log toolbar shows — **Set up my terminal** / **Fix terminal setup** until ready, then **Chat to Claude**. Detection is a file read on purpose: `claude mcp get`/`list` health-check every server, spawning our own Python MCP process on each status refresh. With `projectUid`, adds that project's `session` (id + token totals + activity passes). |
| POST | `/api/observer/register` | **one-click terminal setup** (the lab-log info dialog's button): runs `claude mcp add-json cecelia-observer <spec> -s user` so the user's own `claude` sessions get the observer tools — no path for them to copy. Body ignored; the spec is server-derived (`observer_mcp_spec`) and `scope` is not client-settable. `add-json` rejects an existing name, so a re-sync **removes first** → idempotent, and fixes a stale entry after a move/port change. Also clears `local`-scope `cecelia-observer` entries that would shadow ours (`claude mcp remove … -s local`, spawned *in* each offending directory since the command acts on its cwd) — only after the user-scope entry is good, only for entries that differ from what we'd register, and each named in `message`. Never edits `~/.claude.json` directly (the CLI owns that format): an already-`current` entry is a no-op, a first-time register runs no `remove`, and if a re-sync's add fails the prior entry is restored — a failed click can't cost the user their registration. Returns `{ok, available, name, message, error, terminal}` with the CLI's own output in `message` and `terminal` re-read from the config afterwards (the UI flips its button on that, not on the exit code). The only route that writes to the user's Claude Code config (`~/.claude.json`), and only on that click. |
| GET | `/api/repl/api` | **READ-ONLY**, project-independent: the **notebook/REPL data-access surface** — `{api: [{name, exported, documented, doc}], doc}`. `api` is the `NOTEBOOK_API` allow-list of read accessors (`load_project`/`images`/`image`/`pop_df`/`label_props`+view/`track_props`/`plot_summary_data`/…) with their **live docstrings** (introspected from the running package, so never stale); `doc` is the `docs/REPL.md` cookbook (the `\|>` chain idiom + the notebook write rules). Backs the `get_repl_api` MCP tool so Claude writes correct `using Cecelia` notebooks. `api_repl_api`, Observer Phase 2 foundation. |
| GET | `/api/analysis/lineage?projectUid[&imageUid][&setUid]` | **READ-ONLY**: the synthesized **analysis lineage** — how each image's data was produced, so the observer needn't have the workflow re-explained. Per image `{uid, name, included, steps, segmentations, tracked, clusterRuns, gatedPops}`: `steps` is the ordered run-log pipeline (`{stage, fun, valueName, status, at}`, `stage` ∈ import/cleanup/edit/segment/track/behaviour/cluster/other); `clusterRuns` = `[{suffix, valueNames}]`; `gatedPops` = gate-defined pops (names/counts). Plus project `chains` (`[{name, tasks}]`), `boards` (tab names, best-effort) and a `rollup` (`{pipeline, divergences}` — common stage sequence + which images diverge). Summary-level only (names/counts/order, no raw rows). Slice A of `docs/todo/OBSERVER_DATA_ACCESS_PLAN.md`; backed by `analysis_lineage`. |
| POST | `/api/images/meta/set` | `{projectUid, values: {uid: {<meta keys>}}}` — generic bulk merge into an image's `meta` dict (physical size/unit, time interval, or any future field) via `_mutate_images!`, same shape idea as `attr/set` but the per-uid value is a partial dict instead of a scalar. Add new `meta` fields here, not a new one-off route. Physical-size/timing edits also propagate into the **`"default"`** (original bioformats2raw) zarr — its `.zattrs` NGFF scale **and axis units** (`update_ome_scale!`) plus the `OME/METADATA.ome.xml` `<Pixels>` attrs (`update_ome_xml_pixels!`) — never the active version, so the acquisition source of truth stays correct and a later `meta/resync` re-derives the edit instead of reverting it (see CLAUDE.md → *OME-ZARR dual-format*). |
| POST | `/api/images/inclusion/set` | `{projectUid, values: {uid: {included?, note?, starred?}}}` — set the per-image user flags (only the keys present change). First-class `CciaImage` fields, so this rounds through the model via `_mutate_images!`. Excluded images (`included:false`) are greyed in the GUI, unselectable for runs, and hard-skipped by the runners. `starred` is a plain bookmark, any number per set, that drives the Starred row filter and nothing else. One route for all three because they are the same operation. Same `values`-map shape as `meta/set`. |
| POST | `/api/images/meta/resync` | `{projectUid, imageUids: [...]}` — backfills physical-size/timing `meta` for images imported before that metadata was tracked, by re-reading the `"default"` (original bioformats2raw) zarr — never the active version, see CLAUDE.md → *OME-ZARR dual-format* — via `resync_ome_meta!`/`read_ome_metadata`. Strictly **fill-only** (`overwrite=false`): fills genuinely-absent fields, never clobbers a value already on disk — so it's safe to run on a human-corrected or ImageJ-auto-corrected image (those live only in ccid.json and would otherwise be reverted to bioformats2raw's raw value). It then pushes the merged result **back** into that zarr (`sync_zarr_calibration!`), so resync converges the two copies rather than only reading one: this is the repair path for a store whose calibration is stale while ccid's is right (e.g. an import whose NGFF write was skipped) — the metadata editor only syncs fields the user actually re-types. No re-import, no source-file access. Returns `{ok, images: {uid: <full image payload>}}` so the frontend can drop the warning icon immediately. |
| POST | `/api/images/labels/delete` | `{projectUid, imageUid, valueName}` — delete ONE label set and **everything derived from that name**: the registered `labels[vn]` zarr, the `branch_labels[vn]` zarr under `branchLabels/` (a separate registry that previously had no delete anywhere), and every `labelProps/` sidecar whose filename starts `{vn}.` or `{vn}__` — so `{vn}.h5ad`, `{vn}__tracks.h5ad`, `{vn}__branch.h5ad`, `{vn}.clustfeatures.json` all go. Prefix-driven rather than a suffix list so a companion added later is swept too; the `.`/`__` boundary is what stops value_name `B` from eating `B2.h5ad`. Clears the `labels`/`label_props`/`branch_labels` registrations in one `commit_state!` AFTER the deletes (a multi-GB store must not be held under the image lock). Returns `{ok, image}`. **Deliberately NOT deleted:** `gating/{vn}.json` (+ the `__tracks` variant) — gate polygons are hand-drawn user work, not derived output, so re-running the segmentation under the same value_name makes the existing strategy apply again (`reset_image_analysis!` keeps them for the same reason). `spatialGraph`/`spatialStats` are keyed by run suffix rather than value_name, so there is nothing per-set to take. Reached from the Import page's Delete modal → *Label sets*. |
| POST | `/api/images/version/remove` | `{projectUid, imageUid, valueName, newDefault}` — delete ONE image version's store and clear its `filepath` entry, re-pointing `_active` at `newDefault`. A thin adapter over `remove_image_version!` (`app/src/storage.jl`) — the same core the (hidden) `importImages.remove` task and the Settings reclaim use, so there is one deletion path rather than three. `404` when the version isn't registered. Returns `{ok, freedBytes, cleared, image}` (`cleared` = the safe-primary un-import fired: the primary went and nothing survived). **Callers looping over several versions must order `"default"` LAST** so that un-import lands at the end rather than mid-loop — the frontend does this via `orderDefaultLast` (`utils/imageDelete.ts`). Reached from the Import page's Delete modal → *Versions*. |
| POST | `/api/images/analysis/reset` | `{projectUid, imageUids: [...]}` — drop everything DERIVED from each image while keeping the image: every child of `1/{uid}` except `ANALYSIS_KEEP` (`ccid.json`, `runlog.json`, `gating/` — gate definitions are user work, not output), plus the `labels`/`label_props`/`branch_labels` registrations. Touches **no image store** — shedding a version is `version/remove`'s job, and the two are deliberately orthogonal. Core: `reset_image_analysis!`. Returns `{ok, freedBytes, images: {uid: <full image payload>}}` so the table drops its segmentation/run state immediately. Reached from the Import page's Delete modal → *All analysis*. |
| GET | `/api/fs/list?path` | filesystem browser for the import file picker. `path` is an ABSOLUTE server path (empty → home; a relative path resolves against home). Browses the WHOLE filesystem so mounted network drives / external storage are reachable — set `CECELIA_FS_ROOT` to sandbox to a subtree. Returns `{root(home), current, parent(null at the ceiling), shortcuts:[{label,path}] (home + /mnt,/media,/Volumes or Windows drives), entries:[{name, path(absolute), isdir, isimage, ext, size}]}`. Hidden dotfiles filtered; unreadable dir → `400`. |
| GET | `/api/pools`, `/api/tasks/definitions` | pools (`[{name, limit, running, queued}]` — `limit` drives the throttle sliders; `running` = in-flight slots, `queued` = tasks waiting for this pool, via `pool_status()`; poll it for live occupancy, there is no `pool:*` WS event), task specs |
| POST | `/api/pools/set` | `{name, limit}` — set a scheduler pool's concurrency limit live (Settings sliders): `resize_pool!` now + persist to `custom.toml [pools]`. Only already-configured pool names accepted (else `400`); limit clamped to `[1, POOL_LIMIT_MAX]`. See `docs/SCHEDULER.md` → *Live pool limits*. |
| GET | `/api/tasks/funparams?projectUid&fun&imageUid?&setUid?` | last-used task params, resolved image → set → none (R `moduleFunParams`; see `docs/MODULES.md` → *Remembering task params*) |
| GET | `/api/chains`, `/api/chains/get`, POST `/api/chains/{save,create,rename,delete}` | chain templates — `save` is the whiteboard's own verbatim overwrite; `create` is the create-only + **validated** route for an outside author (the MCP), 409 on an existing name; `rename` is one atomic move (past runs keep the old name on purpose). Every one guards the name with `_valid_chain_name` — it becomes a filename |
| GET | `/api/chains/runs?projectUid` · `/api/chains/run?projectUid&runId` | list persisted run records / load one run's frozen template + per-node status (Live view run history; see `docs/SCHEDULER.md` → *Loading past runs*) |
| GET | `/api/napari/status`, POST `/api/napari/{open,close,restart,show-labels,show-populations,start-selection,stop-selection,event}` | napari bridge + gating linked brushing. `status` also carries `canvasSizeX/Y` — the size a movie records at when none is requested (from the bridge `ping` reply, so no extra round-trip; read via the `useNapariStatus` composable) |
| GET | `/api/preview/status`, POST `/api/preview/{start,stop,run}` | task preview worker (:7656) — real compute over the visible region |
| POST | `/api/napari/screenshot` | `projectUid` | JSON `{assetId, viewState, imageUid}` of the current napari view. The PNG is written to a **sidecar** file (`settings/board-assets/<assetId>.png`, served via `/api/board-assets`), not returned inline, so the board JSON stays small. Uses napari `export_figure` (tight-fit to the **data extent** at native resolution) so the figure has **no black margins** and matches the viewer — not a tiny image in a big black canvas (plain-canvas `scale` only enlarges the canvas at a fixed zoom, adding margins). The **view snapshot** (camera + dims + per-layer display props) is captured atomically with the shot (folded into the bridge's `save_screenshot` reply) so the frame carries its exact provenance for zoom-to-source (`docs/todo/ANIMATION_PLAN.md`). `400` if napari not running or `projectUid` missing. |
| POST | `/api/napari/apply-view-state` | `viewState` | re-apply a saved snapshot to the running viewer (zoom-to-source restore); image must already be open. Bridge skips missing layers / unsettable attrs. `200 {ok}`; `400` if napari not running or `viewState` missing. |
| POST | `/api/napari/view-state` | — | return the CURRENT view snapshot (camera/dims/per-layer colormap+visibility) of the open image + `imageUid`, via the bridge `capture_view_state` (no PNG side-effect). The Batch movies page uses it to **seed** the config from the first selected image's live colours + overlays. `200 {ok,viewState,imageUid}`; `400` if napari not running. |
| POST | `/api/napari/overlay-legend` | `projectUid,imageUid,colourBy?,overlayPops?[{valueName,popType,path}],colourOverrides?` | **read-only** legend for a strip still's overlays (Phase C) — pure Julia, no viewer touched. Returns `{colourBy:{column,items:[{value,colour,label}]}, populations:[{name,colour}]}`: the colour-by section is the pop colour + pop name per value on `colourBy` (same population-colour rule as colour-labels; clusters read as their pop names), the populations section is each requested pop's name+colour from its map — **points AND track/track-cluster ribbons** (entries that aren't a named pop, e.g. `/_tracked`, are skipped). Captured with the screenshot so the frame's legend is durable. `200 {ok,colourBy,populations}`. |
| POST | `/api/napari/apply-movie-config` | `projectUid,imageUid,config` | **F1.2 preview**: apply an authored movie config to the **currently open** image (no recording), so the user can eyeball the look the batch will record. Reuses the existing open/`show-tracks`/`show-populations`/`colour-labels` handlers via `_apply_movie_config!` (channels→colormap+visibility, overlays, colour-by). `config` = `{valueName,channels:{name→colormap},colourBy,showTracks,trackValueNames,tailWidth,showGatedTracks,showTrackclust,showPopulations,popType,pointsSize,colourLabels,colourOverrides,tStart,tEnd}`. `200 {ok}`; `400` if napari not running or `config` missing. |
| POST | `/api/app/shutdown` | the global "Quit everything" (Settings → System). Best-effort stops **every** resident child then `exit(0)` from a detached task so the response flushes first — graceful stop (napari `close!`, notebook server, `_stop_preview_worker!`) followed by a port-level kill on each, which is what also catches a child we merely *adopted* or that outlived a crash. The list is asserted by *"shutdown stops EVERY resident child"* in `api/test/runtests.jl`, tied to the `*Port` keys `/api/diagnostics` reports — adding a child there without adding it here fails. Dev: ends `pixi run dev`; packaged: server exit ends `app.py`. (`api/src/app_api.jl`) |
| POST | `/api/app/restart` | **dev-only** backend restart (button gated on `diag.dev`). Stops children then `exit(42)` (`RESTART_EXIT_CODE`); the **supervisor** relaunches in place — `api/dev.jl` in dev, `app.py`'s loop in prod. `409` when not supervised (no `CECELIA_SUPERVISED` — a bare `julia src/server.jl`). Replaced the old detached-relauncher, which couldn't reattach to a foreground terminal. |
| GET | `/api/logs/recent` | `{logs: [{level,message}]}` — the server-log ring buffer (last 500), so a freshly-opened console **window** backfills recent lines. Fed by the `BroadcastLogger` tee that also emits the `server:log` WS event (see below). |
| GET | `/api/setup/defaults` | `{projectsDir}` — OS-correct pre-fill for the first-launch wizard (`joinpath(homedir(), "cecelia-projects")`). (`api/src/setup_api.jl`) |
| GET | `/api/setup/validate?path=` | `{ok, message, willCreate}` — live projects-dir feedback (pure check, no side effects). |
| POST | `/api/setup/init` | `{projectsDir}` → `{ok, projectsDir, restartRequired}` \| `400`. Validate → `mkpath` → write `custom.toml` (`Cecelia.set_projects_dir!`) → hot-reload config. `restartRequired` is `false` on the normal path (config reloads in place). Drives the `/setup` wizard; `/api/diagnostics` exposes `setupRequired` to trigger it. See `docs/todo/ONBOARDING_PLAN.md`. |
| GET | `/api/version` | `{version, installed}` — running version + whether this is an installed bundle (vs dev checkout). (`api/src/update_api.jl`) |
| GET | `/api/update/check` | `{current, latest, updateAvailable, url, scope}` vs the newest GitHub release. `scope` ∈ `user`\|`system`\|`dev` gates the UI: `user` self-updates, `system` shows an admin note, `dev` hides it. |
| POST | `/api/update/apply` | `{version}` → downloads + **stages** the release bundle (`app.py` applies it on next restart). `403` on a `system` install (admin-only), `400` on a dev checkout. |
| — | **Gating** (below) | population manager + gating |

Task execution + status flow over **WS** (`task:run`/`task:status`/…), not HTTP — see
`ARCHITECTURE.md` and `SCHEDULER.md`. One extra WS message reuses this task-event channel WITHOUT the
scheduler: **`movie:batch`** `{taskId,projectUid,imageUids,config,fileAttrs,fps[,sizeX,sizeY]}` (`handle_movie_batch`,
`api/src/sockets.jl`) kicks off the F1.3 batch-movie run async on the single napari viewer and emits the
same `task:progress/log/status/result` (keyed by the client's `taskId`), so it shows in the task list with
a progress bar + Cancel. `task:cancel` also flags it (`request_batch_cancel!`, stops after the current
image) — napari is a UI-serial viewer in `api/`, not a pooled scheduler task. See `docs/NAPARI.md` →
*Authored config + batch* and `docs/todo/ANIMATION_PLAN.md` → F1.3.
**`movie:record`** `{taskId,projectUid,imageUid[,keyframes,fps,sizeX,sizeY,suffix,titleCard,apiUrl]}`
(`handle_movie_record` → `run_single_movie`) records ONE movie on the same rail — it replaced the REST
routes `/api/napari/record-timelapse` and `record-animation`, which blocked for the whole render and so
could offer neither progress nor a cancel: `keyframes` present ⇒
the interpolated animation, absent ⇒ the open image's T-sweep. Per-frame `task:progress` arrives via the
bridge posting `{type:"recordProgress",taskId,frame,total}` to `POST /api/napari/event`; `task:cancel`
stops it **mid-render** (the bridge answers `record_cancel` off-queue). `suffix` is a filename addition
so two movies of one image — the corrected version and the raw import — don't overwrite each other.

`sizeX`/`sizeY` are the movie output size in pixels on all three of these; **absent or blank means the
napari canvas size**, which is the default. One reader (`_movie_size_params`, `api/src/napari_api.jl`);
the pixel rules (4096 clamp, even axes) are Python's, in `movie_io.coerce_movie_size`. See
`docs/NAPARI.md` → *Movie output size*.
**`maintenance:run`** `{taskId,patchId,projectUid,apply}` (`handle_maintenance_run`, `api/src/sockets.jl`)
runs a project-scoped **data patch** (Settings → Data patches; catalogue at `GET /api/maintenance/patches`)
via `run_maintenance_patch` → `run_py`, streaming the same `task:log/progress/status` keyed by `taskId`;
**`maintenance:cancel`** `{taskId}` → `cancel_maintenance!`. Confined to the one named project. See
`docs/DEV.md` → *Data patches*.
**`project:export`** `{taskId,projectUid,outDir?}` (`handle_project_export`) bundles a project to a
`.ccbundle` (each zarr store `tar`'d in parallel); **`project:import`** `{taskId,bundle,mode?}` restores
one — `mode` (`error` default / `replace`; `copy` exists but is UI-hidden, see PROJECT_IO_PLAN.md)
resolves a uid collision. Both are background jobs
(`project_io.jl` on `jobs.jl`) that stream the same `task:log/progress/status` and are cancelled by
`task:cancel` → `cancel_job!`. Neither needs an open project (export reads a dir by uid, import creates
one). Supporting GETs: `/api/projects/bundles` (list bundles + export dir, for the picker) and
`/api/projects/bundle-info?path=` (a bundle's uid/name + whether it collides, so the UI can prompt).
UI: `ProjectPanel.vue`. See `docs/JOBS.md`, `docs/todo/PROJECT_IO_PLAN.md`.
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
the other tabs or the console. WS telemetry is lossy-safe *because the console reconciles against
`GET /api/tasks` in **both** directions* — that snapshot is the authoritative in-flight set (whole
registry, under its lock, deregistered on completion), so the console adds/updates rows from it **and
retires rows that vanish from it** (2 consecutive misses; tallied `ended` — "finished, outcome unseen"
— rather than guessed as done/failed). Only add-and-update is not enough: a lost terminal
`task:status` frame — dropped for a slow client, or never delivered on a half-open socket — otherwise
strands the row as `running` forever, so the console lists tasks the scheduler has long since finished
while every pool reads idle. Rows for WS-only producers (jobs, batch movies) never appear in the
snapshot and are exempt from retiring — they identify themselves with `fun` + `pool` on their status
frames. That identity is also the console's other retire rule: a row with **neither** (`_unattributed`)
has only ever seen `task:log`/`task:progress` frames, so it can't be live work and is dropped by the
snapshot too — silently, since we never saw what it was. Without that rule such a row is immortal:
absent from the scheduler, so no snapshot can ever mark it eligible. That was the **zombie queued row**
— `task:log` used to get-or-*create* its row unconditionally, and a cancelled task's killed subprocess
flushes its remaining stdout *after* the terminal frame, so each trailing line minted a fresh blank row
stuck at the default `queued` (six cancels → six phantom rows, `GET /api/tasks` returning `[]`). Log
frames now honour `SEEN_TERM` like status/progress do: still shown in the logs pane, never resurrecting
a row. The reconciliation half is split out as the socket-free
`_reconcile_snapshot!(rows)` and pinned by the *API: task console reconciles snapshot removals* and
*ignores post-mortem log frames* testsets (the script's entrypoint is `PROGRAM_FILE`-guarded so the
suite can `include` it).
**The outcome itself is polled, not streamed** — `GET /api/tasks/recent`. Retiring a vanished row
keeps the *table* honest but says nothing about how the task ENDED, and the outcome had exactly one
carrier: the terminal `task:status` frame, which the paragraph above drops on purpose. So a single
missed frame per task was permanent, and nine images that all succeeded read **`0 done · 17 ended`**.
Terminal frames are therefore banked for replay as they are emitted (`record_task_outcome!` inside
`ws_status`, `docs/SCHEDULER.md` → *Terminal outcomes are banked for replay*), the console polls that log
beside the snapshot each tick — outcomes first, so a task is never retired as unseen while the server can
still name it — and `_note_terminal!` de-duplicates whichever of the two arrives second. WS still drives
the *live feel*; HTTP makes the numbers true. The first poll after connecting is a **prime** pass: the log
predates the console, so those ids are marked seen without a tally and the counters keep meaning "since
you started looking". Pinned by *API: task console counts outcomes without the WS frame*.

**The browser does the same, and recovers the frame rather than the number** — the ws store polls the same
route while this tab has work in flight and re-emits the missing frame through its one `dispatch` path, so
all five completion listeners fire as they would have. `docs/UI.md` → *A dropped terminal frame is
recovered, not tolerated*.

**`GET /api/tasks` has the same two consumers**, mirrored the same way: `_reconcile_snapshot!` in the
console and `adoptableTasks` (`frontend/src/utils/runningTasks.ts`) in the browser, which adopts the
in-flight set on connect so a reloaded tab lists work it never watched start. `chain_node_id` exists for
that consumer: a chain row is keyed `runId::nodeId::imageUid` in the GUI, so adopting a node needs the node,
not just the run. `params` exists for it too — the params the run was submitted with, so an adopted row can
offer **Re-run** instead of withholding it; without them the client knows the `fun_name` but not the
configuration, and re-running would silently substitute the JSON spec's defaults. They are published
**all-or-nothing**: `null` if any value isn't a JSON-native shape (`_publishable_params`), because the
whole snapshot is one `JSON3.write` and a partial set would be re-run as if complete. That check is a
whitelist rather than an attempted write — JSON3 throws on a `Function` but turns an arbitrary struct
into an object, which a probe would then publish as if it were the param. `{}` (a task that takes no
params) and `null` (unknown) are therefore different answers, and the client must not conflate them. The
field names are
therefore a contract with two clients that share no runtime — pinned on both sides (*Scheduler records
queued/started timestamps* in the package suite, `runningTasks.test.ts` in the frontend) so a rename fails
a test rather than silently blanking a column. The browser deliberately omits the console's
retire-on-miss half: it has the outcome poll, so it recovers a real outcome instead of guessing `ended`.

**Chain nodes** report their outcome through a different door: a chain run emits no `task:status`
frames, so the console attributes one from the `taskId` on the terminal `chain:node:done`/`failed`
frame (see `docs/SCHEDULER.md` → *Event bus*). A row already retired as `ended` is *corrected* if its
real outcome arrives late — moving the tally rather than keeping a number known to be wrong — which
leaves `ended` meaning what it says: telemetry genuinely lost. That correction applies to **every**
door, which was itself the second half of the `0 done` bug: `handle_task_run`'s late frames returned
early on `SEEN_TERM` *before* reaching it, so only chain frames could ever correct an `ended` and a
late `done` was simply discarded. `handle_task_run` forwards
`queued`/`running` **and
`cancelled`** from `on_status_change` immediately (cancel has no result to order before it), so
cancelling a task — especially a still-**queued** one — reflects at once instead of only when a worker
later dequeues and skips it; `done`/`failed` are held until the result is sent. The console drops its
whole view on reconnect (a localhost drop = server restart), so stale tasks don't linger — and a failed
20s keepalive now tears the socket down instead of being swallowed, so a half-open connection actually
reaches that reconnect rather than leaving the reader blocked forever on a socket that never speaks again.

**Elapsed time is served, not guessed.** Every carrier on the rail publishes the task's own timestamps,
ISO-8601 UTC to the millisecond (`TASK_TS_FORMAT`, `app/src/tasks/task_outcomes.jl` — one format for the
whole rail):

| Carrier | Fields |
|---|---|
| `GET /api/tasks` | `queued_at`, `started_at` (`""` until a pool slot admits it) |
| `GET /api/tasks/recent` | `started_at` (`""` if it never ran), `finished_at` |
| `task:status` | `startedAt`, `finishedAt` (terminal only) |
| `chain:node:running` / `:done` / `:failed` | `startedAt`, `finishedAt` |

The scheduler stamps `queued_at` at registration and `started_at` when the pool slot is acquired
(`_set_status!`), so `started_at − queued_at` *is* the queue wait — that's why a task blocked on a busy
GPU reads as waiting rather than as a run of zero seconds. The start is **also** banked on the rail
(`note_task_started!`) because the `TaskRecord` is deregistered the instant the task finishes and the
duration is mostly wanted afterwards: the chain bridge fires `node:done` only once `run_task` has
returned, and a dropped terminal frame is recovered from `recent_tasks` seconds or minutes later. Same
sink rule as the outcomes — the scheduler stamps when it can, `ws_status` stamps on the first `running`
frame otherwise, first write wins — so background jobs and batch movies (no record at all) are covered by
the same mechanism. `record_task_outcome!` **returns the row it banked** and every caller publishes those
values on the live frame, so the live and the replayed frame cannot disagree about when a task ran.

Both clients consume it and keep their own clock only as a fallback: the console's `_set_phase!` adopts
the server's instant whenever one is present (upgrading a row it had been timing itself), and only a
locally-clocked phase whose start it did not witness renders `≥4m 12s` — a floor that says so, the same
rule as tallying `ended` instead of guessing an outcome. The browser does the same in
`utils/taskElapsed.ts`. Pinned by *API: status frames carry the task's timing*, *API: task console times
each task*, and *Scheduler records queued/started timestamps* / *Task start timing* (package suite).

One gap, deliberate: the browser's Task Manager is built from WS events only, so a tab opened mid-run has
no row to time at all — it isn't that its elapsed is wrong, the task simply isn't listed. Rebuilding rows
from `GET /api/tasks` is a separate change (`runningTasks.ts` only counts them today).

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
| POST | `/api/plot_data` | `{projectUid, popType, granularity:"cell"\|"track", chartType:"histogram"\|"frequency"\|"bar"\|"boxplot"\|"points"\|"matrix", measure, bins?, normalize?, rawPoints?, raw? (export mode — see below), statUnit?:"individual"\|"image" (image → collapse each image to its per-series mean/median, one datapoint per image; box/points/bar only), imageAgg?:"mean"\|"median" (how each image is collapsed when statUnit=image), groupBy?, collapseSeries?, groupAttr? (cross-image: an attribute name or array of names → group images by the combined value, joined with ".", one series per value),` **matrix (heatmap):** `matrixMode:"profile"\|"crosstab", measures:[…] (profile rows), category (categorical col), separator?="_", zscore?, matrixNormalize?:"none"\|"row"\|"col"\|"total",` **population selector:** `series:[{valueName,pop}]` (multi-segmentation) **or** `valueName?+pops:[…]` (legacy single-segmentation)`,` **image selector:** `imageUid` (single) **or** `setUid [+imageUids subset] +scope:"per_image"\|"summarised"` (cross-image)`}` | every (non-matrix) response carries `measureType:"numeric"\|"categorical"` (auto-detected → drives which charts the panel offers) and every series carries `uID` (source image; `""` single-image) + `group` (groupBy level, `""` when none). `groupBy` splits the measure by a categorical column (one series per level); `collapseSeries` pools across pops/segmentations/images so series form only by `groupBy`. histogram → `{…, binEdges:[…], series:[{pop,value_name,uID,group,counts:[…]}]}`; frequency → `{…, categories:[…], series:[{…,counts,values}]}`; bar → `{…, series:[{…,value(mean),sd,sem,ci95,n}]}` (all three error metrics; ci95 = 1.96·sem); boxplot → `{…, series:[{…,q1,median,q3,lower,upper,mean,n,points}]}` (Tukey, outliers omitted; `points`=downsampled raw values when `rawPoints`); points → `{…, series:[{…,points:[…]}]}` (downsampled raw values for strip/violin); **`raw:true` → `{chartType:"raw", measure, measureType, granularity, groupBy, rows:[{uID,value_name,pop,label?,track_id?,group?,value}]}`** — ALL per-datapoint rows (not summaries; `series:[]`), the identity + value behind the plot for external re-plotting (the board CSV export path). Only USEFUL identity is emitted: `label` (cell id) is included for the CELL table only (it duplicates `track_id` on the track table, so dropped there); `track_id` when present; `group` (+ a non-null `groupBy`) only when the groupBy column was actually applied (a cell-level groupBy on a track measure is echoed but never applied). The frontend CSV writer additionally drops any identity column that's empty for every row (single-image uID, population-summary label). A measure-less count/proportion plot returns per-image counts; non-finite values dropped; matrix ignores `raw`; **matrix → `{matrixMode, xLabels:[…], yLabels:[…], cells:[{x,y,value,n\|count}], valueLabel, zscore?\|normalize?, category, series:[]}`** — pools the whole frame into ONE grid (profile = measures×category means, z-scorable; crosstab = a `"from<sep>to"` categorical → transition matrix; docs/PLOTS.md §9). Chart type is **independent of the data source**. Cross-image adds `scope` to the response |
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
| POST | `/api/napari/show-labels` | `allLabels?{vn→[files]},allBranchLabels?{vn→[files]},showLabels,labelsCache?,preview?` | add/remove label layers: `allLabels` → `labels/` store (`({vn}) Labels`), `allBranchLabels` → `branchLabels/` (`({vn}) Branches`). Both payloads share the one `showLabels` flag and either may be empty. **`preview:true`** (`allLabels` only) shows a store a task is STILL WRITING as `({vn}) Labels (live)` — level 0 only, caching forced off; the bridge evicts the finished layer and the preview for each other, so a store never shows twice. `200 {ok}`; `400` if napari not running; `500` with an aggregate message if any set failed to load (a merely-absent zarr is skipped bridge-side, not an error) |
| POST | `/api/napari/refresh-labels` | `allLabels{vn→[files]}` | re-read live-preview layers **in place** (reassign `layer.data` from a fresh view — no teardown, so position/opacity/colour survive). Driven by `task:progress`, throttled frontend-side to one read per 2 s. A value_name with no `(live)` layer is a no-op, so it is safe to fire regardless of viewer state. Empty payload → `200` without touching the viewer. `400` if napari not running |
| GET | `/api/preview/status` | — | task preview: `{alive,starting,port,imageUid,zarrPath,taskDir}`. **The route that answers "what is the viewer looking at?"** — `napari_api` tracked the open image but exposed nothing, so out-of-band callers had to be told, and a wrong guess acts on an image the user isn't looking at. `imageUid`/`zarrPath`/`taskDir` are `null` until an image is open; a client seeing nulls must prompt, never fall back to a guess |
| POST | `/api/preview/start` | — | warm the preview worker (:7656) so its **17.7 s** of torch+cellpose imports is paid at toggle-on, not on the first parameter change. Adopts a worker already on the port. `200 {alive,starting,port}` — `alive:false, starting:true` means launching |
| POST | `/api/preview/stop` | `valueName?` | toggle-off: remove the `({vn}) Preview` layer, then stop the worker. Stopping is the **only** thing that releases the VRAM a warm cellpose model holds, so this is a real user action, not cleanup. `200 {alive:false,stopped:true}` |
| POST | `/api/preview/run` | `projectUid,imageUid,valueName?,funName,params` | run the task's real compute over the region the viewer is looking at; result is shown as an in-memory `({vn}) Preview` labels layer (mask block returned by the worker, never written to disk — `cecelia.utils.block_transfer`). `imageUid`/`params.valueName` are **checked, not used to select**: `409 {code}` for `no-image-open`, `image-mismatch` (+`openImageUid`), `version-mismatch` (+`wantedValueName`) or `no-region`. A running segmentation's staging store counts as the same store, so previewing mid-run is not a mismatch. `202 {starting:true}` while the worker boots. `200 {counts,region,fallback2d,hasSignal,noSignalWhy,runSeams,blockSize,valueName}` — `hasSignal:false` with `noSignalWhy:"padding"` means the region is outside the store's valid box (`zarr_utils.read_valid_box`), `"blank"` means every pixel is zero; either way "0 cells" is about the region, not the parameters. `runSeams` non-empty means the RUN would split this region at a tile boundary that the preview does not reproduce. `funName` is required for the task's own param translation (`preview_params` — channel names → indices, custom model → path); a missing custom checkpoint is `400 {code:"params-not-previewable"}`. Every refusal's `code` is the contract: the frontend renders it as a short amber label and the `error` message as the tooltip detail, so the message carries the SPECIFICS (which version is open, which the task reads) and never restates the problem. Deadlined client-side (`PREVIEW_RUN_TIMEOUT_MS`) — a wedged worker or viewer must not read as "still previewing" |
| POST | `/api/napari/show-populations` | `projectUid,imageUid,valueName?/valueNames?,popType,pointsSize?,show?` | send each pop's `{value_name, label_ids, colour, name, show}` to the bridge → coloured Points layers, one per (segmentation, pop), named `({popType}) ({value_name}) {path}` so pops from different segmentations coexist. **Scope:** an explicit `valueNames` list (or a single non-blank `valueName`) refreshes ONLY those segmentations and the bridge prunes stale layers only within them (`scoped`); **blank → ALL** real segmentations (full refresh + global prune). Live gate edits pass the edited segmentation so a keystroke doesn't recompute every segmentation; open / the master toggle pass blank. Membership per segmentation comes from `resolve_pops` (cached by gating-map + h5ad mtime → an unchanged segmentation is free even on a full push). `200 {ok,n}`; `400` if napari not running. `show:false` sends empty pops → bridge removes the (in-scope) layers |
| POST | `/api/napari/show-tracks` | `projectUid,imageUid,valueNames?,showGatedTracks?,showTrackclust?,tailWidth?,colorBy?` | send the full desired ribbon set to the bridge → napari **Tracks** layers: per-segmentation `_tracked` (from `valueNames`), gated `track` pops (`showGatedTracks`), and `trackclust` cluster pops (`showTrackclust`). Each pop carries `{value_name, pop_type, track_ids, colour, name, show}`; the bridge names layers `({pop_type}) ({value_name}) Tracks {path}` so all coexist, and reconciles (removes any not sent). When `colorBy` is a categorical column, colours follow the **`colour_by_palette` rule** — a value a user population **filters for** on that column takes that pop's colour, else an Okabe–Ito default (server sends the `{value→colour}` overrides; response returns the `legend` `{value→hex}`). `200 {ok,n,legend}`; `400` if napari not running. Requires a time axis. Ports R `show_tracks` (`splitTracks`) |
| POST | `/api/napari/colour-labels` | `projectUid,imageUid,valueName?,column` | recolour the Labels layer by an obs `column` (`""`=reset). Categorical → the same `colour_by_palette` rule (population colour where one filters the value, else Okabe–Ito); continuous → viridis. Server computes `{value→colour}` overrides from the cell pops; response returns the categorical `legend` `{value→hex}`. `200 {ok,legend}` |
| POST | `/api/napari/start-selection` | `projectUid,imageUid,valueName,apiUrl?,zMode?,zWindow?` | tell the bridge to add a `Cell selection` Shapes layer; drawing on it POSTs back to `/event` (`apiUrl` defaults to `http://localhost:8080`). `zMode:"slice"` restricts the selection to ±`zWindow` slices around the live z; `"stack"` (default) selects across the whole z-stack |
| POST | `/api/napari/selection-scope` | `zMode,zWindow` | change the z scope of the **active** selection and re-evaluate the drawn polygon live (bridge re-runs point-in-polygon + z filter → POSTs the new labels). No-op when nothing is drawn (`200 {ok}`) |
| POST | `/api/napari/stop-selection` | `projectUid,imageUid,valueName,popType` | clear the transient "Napari selection" pop (registry + re-broadcast) **and** remove the `Cell selection` Shapes layer from napari (best-effort). Backs the manager's trash button (`200 {ok}`) |
| POST | `/api/napari/event` | `type:"cellSelection",projectUid,imageUid,valueName,popType,labels:[…]` | store the selected label IDs as the transient "Napari selection" pop and **broadcast `gating:popmap`** (empty `labels` clears it) |
| POST | `/api/napari/crop-start` | — | 3D crop (Imaris-style slicing): drop napari to 2-D, hide data layers, show a Z max-projection + an editable rectangle to draw the XY crop footprint over the whole structure. `200 {ok}`; `400` if napari not running |
| POST | `/api/napari/crop-apply` | `zLo?,zHi?` (fractions 0..1 of z depth) | **preview only** — read the drawn rectangle + z-range → set axis-aligned `experimental_clipping_planes` on image/labels/tracks/points, drop the helper layers, return to 3-D. Nothing saved. `200 {ok,worldBox}` |
| POST | `/api/napari/crop-box` | `zLo?,zHi?,tLo?,tHi?` (fractions 0..1) | resolve the drawn rectangle + z/t ranges to a **full-res pixel bbox** `{x0,x1,y0,y1,z0?,z1?,t0?,t1?}` — the params for the `editImages.cropImage` task that writes the cropped image as a new image in the set. View-only. `200 {ok,box}` |
| POST | `/api/napari/crop-clear` | — | remove the 3D crop (clear clipping planes + any leftover helper layers). `200 {ok}` |

The transient pop appears in `gating:popmap` with `"transient": true` and explicit-label
membership; it is queryable like any pop (`plotdata`/`stats`/`membership`) but is never
persisted (`docs/POPULATION.md`).
