# Cloud migration: tenancy scope + headless-render spike

> **STATUS: assessed and declined, 2026-08-21.** Dominik's call: not realistic. This is kept as a
> decision record, not a plan — nothing here is scheduled. If the cloud mandate returns, start from
> sections 3b and 3c, which are the parts that decide it. The one code change that came out of this
> is a correction to `docs/INSTALL.md` (the headless `offscreen` advice was wrong); it is unrelated
> to the cloud question and stands on its own.
>
> The findings that bear on "not realistic", so they need not be re-derived:
> - A network-hosted working store is not viable (3b): 12.4 ms/chunk on CIFS, saturating at ~748
>   chunks/s; 9.7 s/chunk on Google Drive. Making it work requires conceding instance-local NVMe.
> - The viewer cannot be run headless as shipped (2): both `offscreen` and `eglfs` are dead ends, so
>   an X server plus a forwarder becomes a hard dependency in the image.
> - Interactive latency — the thing that decides whether a remoted viewer is usable at all — was
>   never measured. My throughput numbers were invalid and I did not replace them.
> - Against that, multi-user itself is *cheap* (1): per-process singletons mean container-per-user
>   needs no route refactor. That was the one pleasant surprise, and it is not enough on its own.

Context: IT mandates cloud, no new local analysis machines. The question raised was "ship a stripped
Cecelia + Napari as a web version". This document answers the two things that decision depends on:
can Napari render server-side (spike), and what does serving N users actually cost (scope).

Measured on `origin/main` @ 2e5b89fd. Numbers are counted, not estimated, unless marked.

## 1. Headline

**Do not rewrite the viewer.** Remote the existing one. The expensive-sounding half (multi-user)
is much cheaper than it looks, and the cheap-sounding half (headless GL) has two hard blockers.

The reason multi-user is cheap is counter-intuitive and worth stating precisely:

- 193 distinct `/api/…` paths exist. **Zero** carry a project/image uid.
- The current project, viewer, preview worker and notebook process are all **process-global**
  (`api/src/napari_api.jl:5,7,27,28`, `api/src/preview_api.jl:15`, `api/src/notebooks_api.jl:23`,
  `app/src/config.jl:8` `const _CONF = Ref(Dict{String,Any}())`).
- But the **model layer is already uid-addressable**: `load_project(proj_uid)`
  (`app/src/model/project.jl:47`).

So "one of everything" is a property of the *process*, not of the code's structure. That means
**one backend process per user, isolated, behind a router** gets you multi-user without touching
those 193 route handlers. Making the app genuinely multi-tenant in one process would mean
threading a session through every one of them — that IS a rewrite, and it is not necessary.

## 2. Spike: can Napari render with no display server?

Harness: `docs/todo/spike/qpa_gl_probe.py` (GL context per QPA platform),
`docs/todo/spike/headless_render_spike.py` (3D+time throughput).

### Established

1. **`QT_QPA_PLATFORM=offscreen` has no GL.** Probed with a live `QApplication`: "no GL context".
   Same for `vnc` and `minimal`. Not a viable path.
2. **`eglfs` is closed with the shipped Qt.** PyQt5 5.15.14 ships `libqeglfs.so` but
   `qt.qpa.egldeviceintegration: EGL device integration plugin keys: ()` — no KMS/EGLDevice
   backends at all. It falls back to the base integration, which demands `/dev/fb0` and aborts.
   This is a *packaging* fact, not a property of the host, so the usual "cloud GPU + eglfs_kms_
   egldevice, no X server" recipe does not apply without changing the Qt binding.
3. **Therefore an X server is a hard requirement** — `xcb` against Xorg-headless, TurboVNC or
   KasmVNC. Neither `Xvfb` nor `vglrun` is in the pixi env today (`docs/INSTALL.md` gains a
   dependency; this is a build change, not just an ops choice).
4. **Hardware GL is reachable, but only via the app's own PRIME env.** On the same X display, with
   no env the renderer is `llvmpipe (Mesa)`; with `__NV_PRIME_RENDER_OFFLOAD=1` +
   `__GLX_VENDOR_LIBRARY_NAME=nvidia` it is `NVIDIA RTX 2000 Ada … 4.6.0`. Cecelia already owns
   this switch (`app/src/napari.jl:55-59`, `napari_discrete_gpu`) — the cloud image must set it or
   it will silently software-render.
5. **Headless screenshot ordering trap.** With `Viewer(show=False)`, the *first* screenshot must be
   an unsized `screenshot()`. A sized `screenshot(size=…)` first returns all-zero pixels and
   poisons the canvas permanently — later unsized *and* sized calls stay blank. Plain-first, then
   sized, renders correctly in both 2D and 3D. Relevant because `save_screenshot`,
   `record_timelapse` and `record_keyframes` (`napari/napari_bridge.py:1790,1815,1860`) all pass
   sizes; production shows a real window so it is fine there, but a server-side render path that
   ever runs windowless will hit this.

### NOT established — do not quote a frame rate

The throughput numbers this harness produces are **invalid** and are deliberately not recorded
here. Two tells: frames alternate valid/blank within a single sweep, and hardware NVIDIA vs forced
software `llvmpipe` land within 5% of each other on a 1080p MIP over 48x512x512. Both mean the
readback is racing the draw, so the timer measures Qt plumbing, not rendering. A trustworthy
number needs an explicit `glFinish()`/render-complete sync before stopping the clock. **Until that
is fixed there is no evidence about interactive latency, which is the one thing that decides
whether remoting is tolerable for judging segmentation params.** That is the next task, and it
should be run on a real cloud instance over the real WAN path, not on a laptop.

## 3. Scope: serving N users

Counted touchpoints. Sizes are my estimate; the counts are not.

| # | Workstream | Counted surface | Size | Note |
|---|---|---|---|---|
| 1 | Session router + per-user backend | new component | M | Reverse proxy maps user -> container. Nothing in-app changes. |
| 2 | Dynamic port allocation for the 5 sidecars | 5 fixed ports, 30 `localhost`/`127.0.0.1` sites in 15 files (half in 2 test files) | M | `napari 7655`, `preview 7656`, `runner 7657`, `notebooks 7660`, api `8080`. Only 8080 is already env-driven (`CECELIA_HOST`/`CECELIA_PORT`, `api/src/server.jl:623`). |
| 3 | Per-user config + project root | `const _CONF = Ref(...)` (`config.jl:8`); 182 refs to `config_dir`/`projects_dir`/`homedir()` across `app/src`+`api/src` | M-L | Already has `CECELIA_DEV_DIR` as the isolation seam — this is the lever, not a new concept. |
| 4 | Shared storage for projects | 192.6 GB zarr, 200,998 chunks/store | L | **MEASURED, section 3b.** Network store is not viable; needs instance-local NVMe + share as archive. |
| 5 | X server + GPU in the image | `docs/INSTALL.md`, pixi deps | S-M | Blockers 2-4 above. |
| 6 | Auth | 0 existing sites | M | No auth anywhere in `app/`, `api/`, `frontend/src` today — greenfield, so free choice of an off-the-shelf proxy-level provider. |

Explicitly **out** of this scope: making the API multi-tenant in-process. See section 1.

## 3a. Does the h5ad data have to be streamed too? No — measured

Asked because Napari shows populations, tracks and colour-by columns that live in h5ad, not in the
pixels. It does not change the plan, and it strengthens the case for remoting.

- **The bridge reads h5ad itself, server-side**, through the canonical `LabelPropsView`
  (`napari/napari_bridge.py:1027,1166,1223,1247` -> `task_dir/labelProps/{value_name}.h5ad`).
  What crosses the WS is identifiers only: `pops`, `value_name`, `track_ids`, `column`, `color_by`.
  `show_tracks` reads its vertices locally (docstring, :1301). Nothing h5ad-shaped reaches a client.
- **Cost is negligible against the pixels.** Measured on the real projects dir:

  | | size |
  |---|---|
  | h5ad, 55 files | 139.3 MB total; median 0.77 MB, p90 8.14 MB, max 30.41 MB |
  | OME-Zarr, 81 stores | 192.6 GB total; largest single store 7.96 GB (`zolIMa`) |

  A ratio of ~1380:1.
- `label_props_utils.py:139` is `ad.read_h5ad(filepath)` — a full load, NOT backed mode. But the
  bridge caches (`_centroid_cache`, `_tracks_cache`, `_colcol_cache` keyed by
  `(value_name, column)`, invalidated on re-run, `:103-108,424`), so the full read is per *cold*
  column per session, not per interaction. At these file sizes that is a non-issue.
- **This is an argument for remoting.** Remoting keeps the 8 GB store and the h5ad adjacent to
  compute and ships one 1080p frame. A browser-side viewer would have to pull zarr chunks to the
  client AND reimplement the h5ad reads the bridge already does — work in the expensive direction.

Consequence for workstream 4: its h5ad half is closed. The zarr half stands, unmeasured.

## 3b. OME-Zarr over network storage — MEASURED. This is the blocker.

Workstream 4's open half, closed. Two locations tested: CIFS/SMB 3.1.1 (`//staging/Staging`,
20T) and Google Drive over rclone FUSE (`~/gdrive`).

**Why it hurts here specifically.** `zolIMa/0/ldYr8J/ccidSmoothed.ome.zarr` is shape
`[181 t, 4 c, 45 z, 1095 y, 1106 x]`, chunks `[1,1,1,512,512]`, zarr v2, flat keys (`.`),
**200,998 chunk files at a 20 KB median**, and **only one multiscale level** — no coarse level to
browse from. One channel-timepoint = 45 z x 3 y x 3 x = **405 chunk opens**; all four channels =
1620. The workload is pure small-file IOPS, which is the worst case for both backends.

Per-chunk cost at the current 20 KB geometry, and what it implies:

| store | cold per-chunk | best concurrent | 1 timepoint (4 ch, 1620 chunks) | whole store |
|---|---|---|---|---|
| local NVMe | 0.008 ms (buffered), 0.077 ms (O_DIRECT) | 125k chunks/s, 1 thread | 0.013 s | ~1.6 s |
| CIFS `//staging` | 12.4 ms | 748 chunks/s @ 64 threads | 2.2 s | 4.5 min |
| gdrive (rclone) | 9,730 ms | 8.45 chunks/s @ 24 threads (optimistic, see caveats) | 3.2 min | 6.6 h |

CIFS concurrency curve (fresh corpus per point, every file read once ever):
1 thread 71 chunks/s -> 8 thr 288 -> 16 thr 449 -> 32 thr 618 -> **64 thr 748 (saturated, ~15 MB/s)**.
Sub-linear and it plateaus: the mount is round-trip bound, not bandwidth bound. Serial CIFS reads
do not benefit from cache at all (12.7 / 12.4 / 12.2 ms across fresh, warm and O_DIRECT passes).

### Conclusions

1. **`projects_dir` must be instance-local NVMe.** Neither network location can host a working
   store. On CIFS, dragging the t slider costs 2.2 s/timepoint at 64-way concurrency and 22.9 s
   single-threaded — and napari's reads are not guaranteed to be 64-way concurrent. That is not
   interactive, and it is ~170x slower than local even at best concurrency.
2. **CIFS is fine as archive/staging.** Large-file write throughput reaches ~35 MB/s (below).
3. **Google Drive is not viable anywhere in the analysis path**, at any chunk size.
4. **The 20 KB chunk geometry is pathological for any network backend.** Holding slab bytes
   constant and growing per-file size, CIFS *write* throughput (the uncontaminated signal — writes
   cannot be served from local cache): 20 KB files **1.6 MB/s** -> 160 KB **13.9 MB/s** ->
   900 KB **32.7 MB/s** -> 8 MB **34.7 MB/s**. **~20x from chunking alone.** Cecelia already owns
   this decision surface (`store_layout` / `store_compressor`, `app/src/config.jl`). Worth doing
   for archive and transfer even though the working store stays local.
5. This store having a **single multiscale level** compounds it — a pyramid would give napari a
   cheap browse level. Worth revisiting independently of cloud.

**What this means for the IT director:** cloud is fine; "no local disk" is not. Each analysis VM
needs local NVMe scratch, with the share as archive. That is a normal cloud shape, not an
exception — but it has to be said before the storage is procured.

### Method caveats

- Synthetic corpora matched to the real chunk-size distribution, not the real 8 GB store copied
  over — at 1.6 MB/s that copy alone is ~83 min. Geometry and file sizes are real; layout is not.
- Two cache traps were hit and corrected, and are called out so the numbers are not re-derived
  wrongly: (a) `actimeo=1` on CIFS means a second pass within ~1 s reads from local cache — an
  early run showed a fake 65x jump at 16 threads. Every number above uses a fresh corpus read
  exactly once. (b) rclone's VFS cache means the gdrive *concurrent* figure (8.45 chunks/s) is
  read-after-write and therefore optimistic; the honest cold gdrive number is the 9,730 ms/file
  serial read.
- All measured from Dominik's laptop over the lab network, not from a cloud instance inside the
  same datacentre as the storage. A cloud VM adjacent to the share would do better on CIFS — how
  much is unknown, and it does not change conclusions 1, 3 or 4.

## 3c. "Cloud is fine" — what that actually means, concretely

Section 3b says the working store cannot sit on the share. That does not block cloud; it fixes the
*shape*. The transport it needs already exists in this repo — `app/src/project_io.jl`, whose header
states the premise section 3b measured from the outside: a bundle packs each zarr store into ONE
`.zarr.tar` so "every copy/sync/backup of it [is] O(stores) instead of O(chunks) — a big win on
SSD/NVMe and network storage".

Verified on a real 249 MB / 3,849-file store (`zolIMa/0/fXgbTl/ccidSmoothed.ome.zarr`) to CIFS:

| transport | throughput | 8 GB / 200,998-file store (extrapolated) |
|---|---|---|
| raw directory copy (`cp -r`, i.e. rsync of a live store) | 4.9 MB/s, 75.7 files/s | ~44 min |
| packed into one `.zarr.tar` (the `.ccbundle` shape) | 30.3 MB/s | ~4.4 min |

**6.1x measured**, and it grows with chunk count.

### The deployment

1. **Two storage tiers, strictly separated.** The CIFS share holds `.ccbundle` archives *only* —
   never a live store. `projects_dir` is instance-local NVMe on the analysis VM. This is the one
   non-negotiable, and it is what section 3b actually proves.
2. **Session lifecycle = import / work / export.** Import the bundle to local NVMe at session
   start, work entirely locally at full speed, export back at the end. This is existing
   functionality: background jobs on `jobs.jl` with progress, `cancel_job!`, bounded parallel
   pack/unpack, and — importantly — incremental, because "a finished store is immutable, so
   re-exports/backups only pack what's new". Write-back is therefore cheap unless a task re-ran.
3. **One backend process per user, in its own container.** This is why the section 1 finding
   matters: because every singleton is per-*process*, per-user isolation needs only a distinct
   `CECELIA_DEV_DIR` + `projects_dir` per container. The 193 route handlers are untouched.
4. **Viewer stays Napari, remoted.** X server + GPU inside the container (section 2: `offscreen`
   and `eglfs` are both dead, so Xorg-headless or KasmVNC, and the PRIME env must be set or it
   silently software-renders). Browser receives pixels. Both the zarr and the h5ad stay
   server-side next to compute (section 3a).
5. **Reverse proxy routes user -> container, and carries auth.** Greenfield (0 existing auth
   sites), so an off-the-shelf provider at the proxy.

### What it costs, plainly

- Session start pays one bundle import: ~4.4 min for the largest current store, less for most.
  Background job with progress, not a stall.
- The VM needs local NVMe sized for the working set, not for all 192.6 GB.
- Share capacity is a non-issue today: 6.3 TB free vs 192.6 GB of projects.
- A task re-run rewrites its store, so that store re-packs on export.

### The distinction to hold with IT

"No new local analysis machines" is not the same as "no local disk". VM-attached ephemeral NVMe
scratch is standard cloud practice, not a workstation under a desk. Conceding that one point is
what makes the rest work; conceding a share-backed `projects_dir` makes the product unusable at
the numbers in section 3b.

## 4. Reservations

- Everything above was measured on a laptop with a live GNOME session, not a cloud GPU instance.
  Blockers 1-2 are packaging facts and travel; the *absence* of a working eglfs path would need
  re-checking if the Qt binding changes.
- Workstream 4 is no longer an unknown: both halves are measured (sections 3a, 3b). The
  remaining uncertainty is how much a cloud VM adjacent to the storage improves the CIFS
  numbers — measured from a laptop over the lab network here. It does not change the
  conclusion that the working store must be instance-local.
- Sizes in the workstream table are judgement, not traced. The counts in "Counted surface" are
  traced, as are every number in sections 3a and 3b.
