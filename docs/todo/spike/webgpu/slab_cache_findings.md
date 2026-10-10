# `/api/viewer/slab` cold vs warm cost — findings

Spike, 2026-10-10. Question: is the brick path disk bound, decode/Julia bound, or pipe/JS bound, so we
know which optimisation is worth building. No code in `app/` or `api/src/` was changed.

## Verdict

**Decode/Julia bound, not disk bound.** Cold is only 1.3x warm, and warm is slow (~195 ms per 4 MB
brick, ~20 MB/s). Two independent causes, both server-side:

1. **Chunk/brick misalignment — ~61x read amplification.** The store's chunks are whole 1024² planes
   (`1,1,1,1024,1024`); the viewer's brick is 128x128 x all z x all channels. One 4.06 MB brick
   therefore decompresses 4 c x 31 z = 124 chunks x 2 MB = **~248 MB**, and throws away 60/61 of it.
   The decoder itself is not slow: 248 MB / ~174 ms ≈ 1.4 GB/s, normal single-core blosc-zstd.
2. **Slab reads are serialised server-side.** 16 concurrent requests finish in the same total time as
   16 sequential ones; the Julia process keeps ~1.5 of 16 cores busy at either concurrency. Same over
   HTTP/2 (one multiplexed connection) and HTTP/1.1 (16 connections), so it is NOT the transport.
   Two layers: (1) `try_serve_slab` runs on HTTP.jl's single interactive thread instead of hopping to
   the default pool like other routes (found in code, fix untested), and (2) c-blosc 1.x's global
   decompress mutex — **measured**: without HTTP, 16 threads reading bricks are **0.34x** of serial
   (3x SLOWER, lock contention); with `BLOSC_NOLOCK=1` they are **7.0x** faster, bytes identical.
   **Fixing layer 1 alone would make the server slower, not faster** — the two must ship together.
   See *Open questions* 1 and *Parallel read test*.

A RAM tier / OS-level preload will not help much (it only removes the A→B gap, ~60 ms of ~255 ms).
The pipe (TLS + HTTP/2 + socket) is ~20 ms per brick — not the bottleneck.

## Setup

| | |
|---|---|
| Store | project `e1Mn6X`, image `8eapy6`, `ccidImage.ome.zarr/0` (bioformats2raw layout) |
| Shape | (t, c, z, y, x) = (181, 4, 31, 1024, 1024), `<u2` — 47 GB uncompressed, 3.4 GB on disk |
| Chunks | L0 `1,1,1,1024,1024`; L1–L3 likewise one full plane per chunk (512², 256², 128²) |
| Codec | blosc / zstd, dimension separator `/` |
| Brick | 128 x 128 x 31 z x 4 c, level 0 = 4,063,232 bytes. Matches `brickVolumeRenderer.ts` (`BRICK_XY=128`, `brickZ=min(128, nZ)`, `cTo=nC-1`) |
| Request list | 64 bricks, one per timepoint (t spread evenly over 0–180), x/y brick position varied. Every request touches chunks no other request in the list touches |
| Transport | `https://127.0.0.1:8080`, HTTP/2 (the server is TLS-only when a cert exists; plain http gets a connection reset) |
| Machine | 16 cores, 247 GB RAM, store on NVMe (`nvme0n1`) |
| Server | running `pixi` dev server, Julia 1.13.1, `-t auto` |

## Results

| Run | Concurrency | Wall median | Wall p95 | Server-read median | Server-read p95 | Per-request MB/s | Aggregate MB/s | Run wall |
|---|---|---|---|---|---|---|---|---|
| A cold | 1 | 254.8 ms | 287.0 ms | 237.9 ms | 253.9 ms | 15.9 | 15.7 | 16.6 s |
| B warm | 1 | 195.1 ms | 222.9 ms | 173.8 ms | 199.4 ms | 20.8 | 20.6 | 12.6 s |
| C warm | 16 (HTTP/2) | 3366 ms | 3419 ms | 3263 ms | 3361 ms | 1.2 | 19.2 | 13.5 s |
| C_h1 warm | 16 (HTTP/1.1) | 3500 ms | 3605 ms | 3321 ms | 3439 ms | 1.2 | 18.6 | 14.0 s |

- **Server-read** = the route's existing `X-Server-Read-Ms` header: chunk read + decompress + slice +
  `slab_bytes`, timed inside Julia. Wall minus server-read = pipe.
- Total 260 MB per run; 0 errors in every run.
- C's server-read (~3.3 s) ≈ 16 x B's (~195 ms): each request waits on the other 15 *inside* the read.

### Server CPU during runs

Julia process utime+stime delta over the run, divided by run wall:

| Run | Cores busy (of 16) |
|---|---|
| B (conc 1) | 1.37 |
| C (conc 16) | 1.58 |

### Disk during run A

Before A, the store's pages were evicted and verified: **3391.9 MB resident → 0.0 MB** (`mincore`).
After A: 860.9 MB resident (the 64 timepoints read).

`iostat -x -m 1 nvme0n1` over A (19 samples): **821 MB read**, steady 45–50 MB/s, peak 51.5 MB/s,
`r_await` 0.15 ms, `rareq-sz` ~57 KB, **%util ~12%** (peak 14.6%). `vmstat 1`: `bi` ~52 MB/s, `us` 7–8%,
`wa` ~1%. The disk was genuinely hit but nowhere near saturated — the read rate is set by the decoder
consuming chunks, not by the NVMe.

Representative `iostat` lines during A:

```
Device   r/s     rMB/s  rrqm/s %rrqm r_await rareq-sz ... aqu-sz %util
nvme0n1  809.00  45.40  0.00   0.00  0.15    57.46    ... 0.12   11.80
nvme0n1  897.00  50.56  0.00   0.00  0.15    57.72    ... 0.14   12.70
nvme0n1  897.00  50.23  0.00   0.00  0.15    57.34    ... 0.14   12.20
```

Full logs: `slab_cache_results/A_iostat.txt`, `slab_cache_results/A_vmstat.txt`.

## Relation to the HTTPS/HTTP2 switch (WEBGPU_UPLOAD_PATH_PLAN U4)

U4 removed the **browser's** 6-connections-per-origin queue — it gets 16+ requests onto the wire at
once, and it does that here. It never made the **server** compute them in parallel. U4 was justified on
per-brick wire/queue time (~60 ms at N=16) for bricks that are cheap to read; on this store the
~175 ms server decode dominates and is paid one brick at a time, so U4's win is invisible.

## Does it apply to the flat path? Yes — serialisation yes, amplification no

The flat renderer uses the same route (`try_serve_slab`) and the same reader (`read_slab` →
`read_native` → Zarr.jl → blosc), so both serialisation layers apply to it. Its client fires one
request per channel with `Promise.all` — `fetchTimepoint` and the 2D tile path (`ViewerWindow.vue`
around lines 2084 and 2433), commented *"Channels in parallel … independent reads on the server's
thread pool"*. That assumption is wrong today: those reads run one after another on the interactive
thread.

Measured warm through the server, 8 timepoints x 4 channels = 32 whole-volume (t, c) slabs (no
z/x/y — 31x1024x1024 u16 = 65 MB each), HTTP/2:

| Run | Concurrency | Run wall | Mean server-read | Aggregate |
|---|---|---|---|---|
| `flat_c1` | 1 | 8.00 s | 102 ms | 260 MB/s |
| `flat_c4` | 4 (= the client's per-timepoint fan-out) | 7.64 s | 410 ms (≈ 4 x 102) | 272 MB/s |

- **No parallel gain (1.05x)**: same signature as bricks — server-read grows by the concurrency.
- **No amplification**: a whole (t, c) volume uses every byte of every plane chunk it decodes, which
  is why flat is ~13x the bricks' MB/s (260 vs 20) on the same store. The chunk-shape problem is
  brick-path only.
- **Expected gain from the layer 1 + `BLOSC_NOLOCK` fix:** up to ~4x per timepoint (4 channels
  decoding at once instead of in turn) — a projection from `slab_parallel_bench.jl`'s 3.3x at 4 threads,
  not measured on volumes.
- The 2D plane path (`z=N`, one chunk per channel) and the overview thumbnail make the same per-channel
  fan-out, so the same applies, at smaller absolute cost.
- Raw: `slab_cache_results/flat_c1.tsv`, `slab_cache_results/flat_c4.tsv`.

## Blast radius — what else the two layers hit

**Layer 1 (interactive thread)** — these run inline in `handle_stream` (`api/src/server.jl`), BEFORE the
`fetch(Threads.@spawn ...)` hop, so on HTTP.jl's single `:interactive` thread:

- `/api/viewer/slab` — bricks, flat volumes, 2D planes/tiles, the viewer's overview thumbnail (it is a
  slab request), label masks, live/preview masks, AF previews
- `/api/movies/file`, `/api/board-assets`, static frontend files (file I/O, no blosc — but they queue
  behind slab decodes)

Everything else in the route table (incl. `/api/viewer/thumbnail`, `/api/viewer/meta`, card stills,
gating views, crop) is hopped to the default pool.

**But every request starts on the interactive thread** — HTTP.jl runs `handle_stream` there for all
requests before the hop, and a blosc decode never yields. Measured: a trivial `GET /api/version`
during a 16-way brick burst:

| | Mean | Max |
|---|---|---|
| Idle (n=10) | 15.1 ms | 74.7 ms |
| During `slab_cache_bench.sh` at conc 16 (n=30) | **159.9 ms** | **415.5 ms** |

So while the viewer streams bricks, every API call in the app is ~10x slower — each waits for one or
two brick decodes to finish before it can even be dispatched. (WebSocket reader tasks are `@async`
in HTTP.jl and plausibly share the same fate; not measured.)

**Layer 2 (blosc global mutex) is process-wide**, not slab-specific. Every Julia zarr read contends on it,
including the default-pool routes above. Files that read pixels via `read_native` / `open_level`:
`image_render.jl`, `viewer_api.jl`, `behaviour_cards.jl`, `crop_api.jl`, `gating_views_api.jl`,
`optical_flow_api.jl`, `landscape_api.jl`, `movie_render.jl`, `movie_rail.jl`. Under concurrency those
reads are slower than serial (0.34x at 16 in `slab_parallel_bench.jl`). Python tasks use their own
blosc in a separate process — not affected.

## Optimisation options (projections, NOT measured)

| Option | Attacks | Expected effect | Notes |
|---|---|---|---|
| Cache decoded planes/chunks server-side (LRU, keyed store+level+t+c+z) | amplification | At L0 all 64 bricks of a timepoint share the same 124 chunks → 63 of 64 bricks become memcpy. Order of ~1 GB/s+ while the working set fits | 1 timepoint at L0 = 4 x 31 x 2 MB = 248 MB decoded. RAM budget decides how many t fit. Cheapest fix that needs no data rewrite |
| Rechunk stores to brick-shaped chunks (e.g. `1,1,31,128,128` or `1,1,1,128,128`) | amplification | Each brick decodes ~its own 4 MB → a few ms per brick | Store write path (`zarr_utils` / `store_compressor`) + migration of existing stores. Changes every other reader's access pattern (movie, 2D plane view prefers plane chunks) — needs a decision |
| `BLOSC_NOLOCK=1` in the server env **+** move `try_serve_slab`'s read off the interactive thread (`Threads.@spawn`, as `handle_stream` does for other routes) | serialisation (both layers) | **Measured 7x** on reads without HTTP at 16 threads; multiplies with the cache/rechunk fixes | **Ship together** — layer 1 alone measured 3x SLOWER (mutex contention). Env var must be set before blosc is first called (server launch / `pixi.toml` task / `ENV` at the top of startup); check every launch path (dev, prod, installed app, Windows). Same hop applies to `try_serve_movie`, `try_serve_board_asset` |
| Request plane-aligned work instead of bricks at L0 for plane-chunked stores (e.g. one request per (t, z-range) covering full XY) | amplification | Decode once, serve many bricks | Viewer/scheduler change; overlaps with the decoded cache |
| OS preload / RAM tier of compressed bytes | disk | ≤ ~60 ms/brick (A→B gap) | Low value — confirmed by this spike |

## Open questions

1. **What serialises concurrent slab reads?** Two layers found by reading code (not yet tested by a fix):

   **Layer 1 — every slab read runs on Julia's single interactive thread. (Found in code; matches all
   measurements.)**
   - HTTP.jl 2.x runs every connection/stream handler on the `:interactive` pool: `@_spawn_interactive`
     = `Threads.@spawn :interactive` (`~/.julia/packages/HTTP/*/src/HTTP.jl:29`), used for HTTP/1.1
     connections (`http_server.jl:1283`) and HTTP/2 streams (`http2_server.jl:1627`). `julia -t auto`
     gives N default threads + **1** interactive thread (inferred from the Julia default, not queried
     on the live process).
   - `handle_stream` (`api/src/server.jl`) hops ordinary routes onto the default pool with
     `fetch(Threads.@spawn ...)` — its comment says why: so a blocking handler "doesn't stall the accept
     loop or other in-flight requests". `try_serve_slab` (and `try_serve_movie`,
     `try_serve_board_asset`) are dispatched BEFORE that hop, so they run inline on the interactive
     thread. Blosc decompression is a C call that never yields, so 16 slab requests decode one after
     another.
   - Fits every number: ~1.4–1.6 cores busy; C's per-request server-read ≈ 16 x one warm brick;
     identical over HTTP/2 and HTTP/1.1 (both go through the same pool).
   - Likely fix: run the read/encode part of `try_serve_slab` inside `Threads.@spawn` (default pool),
     like every other route. Small change in `api/src/`, not made here.

   **Layer 2 — c-blosc's global mutex. (CONFIRMED by measurement — see *Parallel read test* below;
   the mechanism notes that follow were written before the test and are kept for the reasoning.)**
   - The loaded library is c-blosc **1.21.6** (`~/.julia/artifacts/b50f03cd.../lib/libblosc.so.1.21.6`)
     and contains the `BLOSC_NOLOCK` env var string. As understood (not checked against the c-blosc
     source): plain `blosc_decompress` — what Blosc.jl / Zarr.jl call — takes a process-wide mutex
     around the global context unless `BLOSC_NOLOCK` is set, in which case each call goes through the
     thread-safe `blosc_decompress_ctx`. The running server does not set `BLOSC_NOLOCK`.
   - Consistent with prior data: `WEB_VIEWER_PLAN.md` (*Eliminated hypotheses*) measured "parallelism
     barely works at `blosc=1` (537 ms serial vs 469 ms across 32 threads)" — 1.15x from 32 threads.
   - Bears on `WEB_VIEWER_PLAN.md` → *Rejected: `Blosc.set_num_threads(n > 1)`*, which rejected
     blosc's internal threads as a data race because "`blosc_decompress` uses the global context …
     Zarr.jl adds no lock" and concluded the safe route (`blosc_decompress_ctx`) is "upstream work, not
     ours". If the mutex exists, the global-context calls are serialised (safe but serial) rather than
     racy, and `BLOSC_NOLOCK=1` routes through the `_ctx` path with no Blosc.jl change. That rejection
     should be re-examined once this is verified. (Its 0.86x-at-`blosc=8` "no global mutex" argument
     is the piece that conflicts — re-measure rather than reason.)
   - Measured worse than "caps at ~1 core": under contention the mutex makes parallel reads 3x
     slower than serial.

   **Still to do:** confirm layer 1 through the real server — add the `Threads.@spawn` hop in
   `try_serve_slab`, start the server with `BLOSC_NOLOCK=1`, rerun `slab_cache_bench.sh C 16`. Expect
   C's run wall to drop from ~13.5 s towards ~2 s. Needs an `api/src/` change, so not done in this spike.

## Parallel read test (layer 2) — `slab_parallel_bench.jl`

Standalone Julia, **no HTTP** (so layer 1 is out of the picture), `-t 16`, blosc internal threads = 1.
16 viewer bricks (128x128x31x4, level 0, distinct t), read through the route's own `open_level` +
`read_native`, page cache warm. Batch of 16 with at most N in flight, median of 3:

| In flight | Default (mutex) | Speedup | `BLOSC_NOLOCK=1` | Speedup |
|---|---|---|---|---|
| 1 | 2970 ms | 1.00x | 2864 ms | 1.00x |
| 2 | 8238 ms | **0.36x** | 1593 ms | 1.80x |
| 4 | 8502 ms | 0.35x | 868 ms | 3.30x |
| 8 | 8654 ms | 0.34x | 561 ms | 5.10x |
| 16 | 8765 ms | **0.34x** | 408 ms | **7.01x** (159 MB/s) |

- **Default: parallel is 3x slower than serial.** Threads contend on c-blosc's global mutex; this
  also explains `WEB_VIEWER_PLAN.md`'s "parallelism barely works at `blosc=1`".
- **`BLOSC_NOLOCK=1`: near-linear to 8, 7x at 16.** The remaining gap to 16x is not investigated —
  plausibly memory bandwidth / allocation from decoding ~248 MB of chunks per brick (the
  amplification problem again).
- **Correctness:** 5 rounds x 16 bricks read 16-way parallel under `BLOSC_NOLOCK=1`, SHA-256 compared to
  serial reads — **0 mismatches**. (A clean stress run cannot prove the absence of a race, but the
  `_ctx` path is c-blosc's documented form for multithreaded callers, so there is no shared state to race on.)
- Raw: `slab_cache_results/parallel_default_8eapy6.json`, `slab_cache_results/parallel_nolock_8eapy6.json`.
  Correctness check: `slab_nolock_check.jl`.
- **Second machine, second store (2026-10-10):** same script, `zolIMa`/`Dml3RG` raw import (identical
  geometry and chunking), a 32-logical-core box, `-t 16`, warm:

  | In flight | Default (mutex) | Speedup | `BLOSC_NOLOCK=1` | Speedup |
  |---|---|---|---|---|
  | 1 | 2410 ms | 1.00x | 2376 ms | 1.00x |
  | 2 | 2431 ms | 0.99x | 1243 ms | 1.91x |
  | 4 | 2475 ms | 0.97x | 675 ms | 3.52x |
  | 8 | 2530 ms | 0.95x | 436 ms | 5.44x |
  | 16 | 2537 ms | **0.95x** | 273 ms | **8.69x** (238 MB/s) |

  NOLOCK reproduces (8.7x). The default does **not** collapse to 0.34x here — it serialises flat at
  ~0.95x. So the mutex caps parallel reads at ~1x everywhere, but how much worse than serial contention
  makes it depends on the machine. "Layer 1 alone is 3x slower" is the 16-core workstation's number,
  not a general one; "layer 1 alone gains nothing" holds on both. `slab_nolock_check.jl` on this store:
  0 mismatches over 5x16. Raw: `slab_cache_results/parallel_{default,nolock}_Dml3RG.json`.
- Consequence for `WEB_VIEWER_PLAN.md` → *Rejected: `Blosc.set_num_threads(n > 1)`*: its premise
  ("the safe route, `blosc_decompress_ctx`, is upstream work, not ours") does not hold —
  `BLOSC_NOLOCK=1` reaches it through the existing Blosc.jl call. Worth revisiting, including whether
  blosc internal threads > 1 are now safe under NOLOCK (each call gets its own context).
2. **Decode cost confirmed directly?** The 61x figure is arithmetic from chunk/brick shapes + the
   measured server-read. A standalone timing of one 1024² chunk read vs a 128² read would confirm it.
3. **Other levels.** Only level 0 was measured. L1–L3 have the same one-plane-per-chunk layout, so the
   amplification ratio shrinks with level (L3's 128² chunk *is* the brick footprint) — expect L3 to be
   near-aligned.
4. **How common is plane chunking?** Is `1,1,1,Y,X` the default for every bioformats2raw import / our
   own writer, i.e. is this every store or just this one?

## Phase 0 baseline (2026-10-10, `SLAB_READ_PERF_PLAN.md`)

Second machine (32 logical cores), through the running dev server (`cecelia-feijoa` @ `650bfe7f` —
behind `main` in `try_serve_slab`'s routing, not its read path), plain HTTP/1.1 (no cert on this box).
Store `zolIMa`/`Dml3RG`: raw import `valueName=default` (same geometry and chunking as 8eapy6) and the
image's default store, a derived one at `1,1,1,512,512` (35 z). Page cache warm.

| Run | Store | Conc | Run wall | Server-read median | Server-read p95 |
|---|---|---|---|---|---|
| `p0_baseline_raw_B` | raw | 1 | 9.89 s | 148.6 ms | 182.5 ms |
| `p0_baseline_raw_C` | raw | 16 | 9.98 s | 2430 ms | 2511 ms |
| `p0_baseline_raw_flat_c1` | raw | 1 | 6.10 s | 103.6 ms | 164.5 ms |
| `p0_baseline_raw_flat_c4` | raw | 4 | 5.20 s | 409.2 ms | 587.1 ms |
| `p0_baseline_derived_B` | derived | 1 | 1.52 s | 19.1 ms | 28.5 ms |
| `p0_baseline_derived_C` | derived | 16 | 1.63 s | 359.5 ms | 401.8 ms |

- **Serialisation reproduces on both stores**: 16 in flight take as long as 16 in turn (raw 9.98 vs
  9.89 s; derived 1.63 vs 1.52 s). Flat c4 gains 1.17x.
- **`GET /api/version`**: idle 0.7 ms mean (n=10) → **59.5 ms mean, 101 ms max** during raw C (n=30).
- **The store a user actually views is often the derived one**, and it is ~8x cheaper per brick
  (19 vs 149 ms): 16x amplification instead of 64x, and its chunks decode faster.

### Amplification, measured directly — `slab_amplification.jl`

Whole chunk vs a 128² sub-read of the same chunk (middle z, one channel), then a full brick;
single thread, warm. Raw: `slab_cache_results/amplification_*.json`.

| Store | Level | Chunk (x,y) | Chunk | 128² sub-read | Brick | Decoded per brick | Amplification |
|---|---|---|---|---|---|---|---|
| Dml3RG raw | L0 | 1024² | 1.50 ms | 0.94 ms (0.62x) | 200 ms | 260 MB | 64x |
| Dml3RG derived | L0 | 512² | 0.12 ms | 0.13 ms (1.07x) | 26 ms | 73 MB | 16x |
| 4rNbMp/FtGoJO raw | L0 | 1024² | 3.57 ms | 3.32 ms (0.93x) | 574 ms | 327 MB | 64x |
| | L1 | 1012² | 3.44 ms | 3.40 ms (0.99x) | 564 ms | 320 MB | 62.5x |
| | L2 | 506² | 0.83 ms | 0.80 ms (0.96x) | 141 ms | 80 MB | 15.6x |
| | L3 | 253² | 0.21 ms | 0.21 ms (1.03x) | 34 ms | 20 MB | 3.9x |

- **Confirmed**: a 128² sub-read costs 0.6–1.1x of the whole chunk — the decode is the cost.
- **Amplification = chunk XY area / brick XY area**, per level. It does not shrink with level until the
  level's plane drops under the chunk cap: bioformats2raw caps chunks at 1024², so a 2024² image's L1
  (1012²) is still one chunk per plane, 62.5x. Dml3RG's stores have no pyramid (L0 only).
- The Dml3RG raw brick measured 149 ms in one run and 200 ms in the next on the same machine — read
  single-run brick times as ±30%.

### Chunk shapes — every store is one z-plane per chunk

- **bioformats2raw imports**: `1,1,1,min(Y,1024),min(X,1024)` — whole planes up to 1024², tiled
  1024² above (4rNbMp slides: 3271x6488 at `1,1,1,1024,1024`).
- **Our writer** (`zarr_utils.plane_chunks`): `1,1,1,min(Y,512),min(X,512)`, every level. Its
  docstring gives the reason: napari slices per (t, c, z). napari is being retired; the browser brick
  renderer reads all z at once, which is the opposite access pattern.
- No brick-shaped store exists in the projects dir sampled (`~/cecelia-feijoa/projects`, 10 projects).

### Windows: `libblosc.dll` reads `getenv` from msvcrt

`Blosc_jll` 1.21.7 (the manifest's) is a Yggdrasil rebuild of c-blosc **1.21.6** (commit
`616f4b73`), so the per-call `getenv("BLOSC_NOLOCK")` read in `blosc/blosc.c` v1.21.6 applies.
`objdump -p` on its x86_64-w64-mingw32 `libblosc.dll`: imports `getenv` from **`msvcrt.dll`**. Julia's
`ENV[...] =` on Windows calls `SetEnvironmentVariableW`, which updates the Win32 environment block but
not msvcrt's own copy (built at process start). So on Windows, in-process, the variable must be set
with msvcrt's `_putenv` (which updates both), or inherited from the parent; and the startup check
should read it back through msvcrt's `getenv` — exactly what blosc sees. (The msvcrt-vs-Win32 split is
documented CRT behaviour; not yet exercised on a Windows box.)

## Phase 1 — `BLOSC_NOLOCK` + slab read on the default pool (2026-10-10)

Same machine, store and bench as *Phase 0 baseline*, through a dev server running `perf/slab-nolock`
(`/api/diagnostics` → `bloscNolock: true`, 32 threads), HTTP/1.1, warm.

| Run | Conc | Run wall before → after | Server-read median before → after |
|---|---|---|---|
| raw B | 1 | 9.89 → 10.34 s | 148.6 → 151.3 ms |
| **raw C** | 16 | **9.98 → 1.37 s (7.3x)** | 2430 → 296 ms |
| raw `flat_c1` | 1 | 6.10 → 6.06–6.32 s (3 re-runs; one outlier at 7.38) | 103.6 → 113.7 ms |
| raw `flat_c4` | 4 | 5.20 → 4.05 s (1.28x) | 409.2 → 198.6 ms |
| derived B | 1 | 1.52 → 1.71 s | 19.1 → 20.7 ms |
| **derived C** | 16 | **1.63 → 0.53 s (3.1x)** | 359.5 → 45.5 ms |

- **Bricks scale.** Raw C lands under the ~2 s projection. Serial runs are unchanged within noise —
  the hop costs nothing measurable.
- **`GET /api/version` during sustained 16-way raw bursts:** 59.5 ms mean / 101 ms max →
  **11.1 ms mean, 5.2 ms median, 63 ms max** (n=30; idle 0.8 ms). Not back to idle — the connection
  task still parses and writes on the interactive thread — but the 10x stall is gone.
- **Flat gains less than projected (1.28x, not 3–4x), and the read is no longer why.** Server-read per
  request halves (409 → 199 ms at c4: four channels now decode at once), but each 65 MB response then
  spends ~300 ms in transfer (wall − server-read: ~100 ms at c1, ~300 ms at c4), ~0.9 GB/s aggregate.
  That is the HTTP.jl body path — a `Content-Length` response is buffered whole before it is written
  (see `_stream_file!` in `server.jl`) — not decode. Out of this plan's scope (Decision 9); recorded
  for whoever looks at the flat pipe next.
- Raw: `slab_cache_results/p1_*.tsv`.

## Method deviations

- **Cache drop:** `sudo` could not authenticate from the agent session, so instead of
  `echo 3 > /proc/sys/vm/drop_caches` the store's files were evicted with
  `posix_fadvise(POSIX_FADV_DONTNEED)` (`page_cache_evict.py`) and residency verified at 0% with
  `mincore`. Equivalent for this test — only this store's pages matter.
- **Per-stage instrumentation (chunk read / decompress / slice / write) not added** — `api/src/` was
  out of bounds. The existing `X-Server-Read-Ms` header splits server vs pipe only.

## Reproduce

All from `docs/todo/spike/webgpu/`:

```bash
S=~/cecelia-projects/e1Mn6X/0/8eapy6/ccidImage.ome.zarr; R=slab_cache_results
python3 -I page_cache_evict.py "$S"                       # cold: evict + verify 0% resident
iostat -x -m 1 nvme0n1 > $R/A_iostat.txt & vmstat 1 > $R/A_vmstat.txt &
./slab_cache_bench.sh A 1 $R; kill %1 %2
./slab_cache_bench.sh B 1 $R                               # warm
./slab_cache_bench.sh C 16 $R                              # warm, 16 parallel
python3 -I slab_cache_summary.py $R A B C
```

`slab_cache_bench.sh` takes `CECELIA_URL`, `PROJ`, `IMG`, `VN` (valueName) and `MODE=flat` (8 t x NC whole volumes) env overrides; the store geometry
(`NT/NZ/NC`, 8x8 bricks) is hardcoded for `8eapy6` at the top of the script.
The two `.jl` scripts take the store path as their argument and read the geometry from it. Another
store with the same layout, for a run on a different machine: `zolIMa`/`Dml3RG`'s raw import
(`ccidImage.ome.zarr`) is also (181, 4, 31, 1024, 1024) with `1,1,1,1024,1024` chunks, so the shell
bench's hardcoded geometry fits it too (`PROJ=zolIMa IMG=Dml3RG`).

| File | What |
|---|---|
| `slab_cache_bench.sh` | the bench: 64-brick list, curl HTTP/2, per-request TSV |
| `slab_cache_summary.py` | median / p95 / MB/s per run |
| `page_cache_evict.py` | no-sudo page-cache eviction for one directory, verified with `mincore` |
| `slab_parallel_bench.jl` | brick reads from N threads without HTTP; run as-is and with `BLOSC_NOLOCK=1`, store path as the argument (commands in its header) |
| `slab_amplification.jl` | whole chunk vs 128² sub-read vs full brick, per level; store path as the argument |
| `slab_nolock_check.jl` | SHA-256 of 16-way parallel vs serial brick reads, 5 rounds; store path as the argument |
| `slab_cache_results/parallel_{default,nolock}_<image>.json` | `slab_parallel_bench.jl` results |
| `slab_cache_results/flat_c{1,4}.tsv` | flat-path whole-volume slabs, concurrency 1 vs 4 (same columns as the brick TSVs) |
| `slab_cache_results/{A,B,C,C_h1}.tsv` | raw per-request rows: label, conc, t, http, wall_ms, bytes, server_read_ms |
| `slab_cache_results/A_{iostat,vmstat}.txt` | disk/CPU logs during the cold run |
