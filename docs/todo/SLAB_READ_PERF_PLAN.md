# Slab read performance plan

Status: **in progress** (2026-10-10). Phases 0–1 shipped (#1572, #1575); Phase 2 built on
`perf/slab-chunk-cache`; Phase 3 decided for new images (#1576). Results in
`spike/webgpu/slab_cache_findings.md` → *Phase 0 baseline*, *Phase 1*, *Phase 2*.

## Goal

Make `/api/viewer/slab` brick reads fast on plane-chunked zarr stores, without regressing the flat
(whole-volume) path, and stop slab reads from stalling every other API call while the viewer streams.

Evidence: `docs/todo/spike/webgpu/slab_cache_findings.md` (measured, 2026-10-10). Brief:
`docs/archive/SLAB_READ_PERF_FIX_PROMPT.md` (archived, with one premise corrected here, Decision 3).
The GPU upload path is not the bottleneck (`WEBGPU_UPLOAD_PATH_PLAN.md`).

## The problem in four numbers

- **~61x read amplification.** Raw bioformats2raw imports chunk one full plane per chunk
  (`1,1,1,1024,1024`; 8eapy6 and Dml3RG both). A 128² x 31z x 4c brick (4 MB) decodes 124 chunks
  (~248 MB). Our own writer's derived stores use `1,1,1,512,512` → ~16x (Dml3RG `ccidDenoised`,
  `ccidDriftCorrected`; writer origin to confirm in Phase 0).
- **Serial reads.** `try_serve_slab` / `try_serve_movie` / `try_serve_board_asset` run inline on
  HTTP.jl's single `:interactive` thread (`api/src/server.jl` `handle_stream`, before the
  `Threads.@spawn` hop). While bricks stream, `GET /api/version` goes from 15 ms to 160 ms mean.
- **c-blosc 1.21.6 global mutex.** 16 threads reading bricks without HTTP: 0.34x of serial on the
  16-core workstation, 0.95x on the 32-thread box. `BLOSC_NOLOCK=1`: 7.0x / 8.7x, SHA-identical.
- **Disk is not it.** Cold is 1.3x warm; NVMe at ~12% util.

## Decisions (2026-10-10)

1. **Phase 1 ships the thread hop and `BLOSC_NOLOCK` together, or not at all.** The hop alone gains
   nothing (0.95x) and on the workstation loses 3x (0.34x).
2. **Threading is not the fix for amplification.** After Phase 1 every brick still decodes ~248 MB.
   The problem is not solved until Phase 2 lands.
3. **`BLOSC_NOLOCK` is read per call, so the trap is the C runtime, not ordering.** c-blosc 1.21.6
   `blosc_decompress` (and `blosc_compress`) `getenv("BLOSC_NOLOCK")` on every call (`blosc/blosc.c`,
   tag v1.21.6) — the brief's "must be set before blosc's first call" is wrong. Consequences:
   - Linux/macOS: `ENV["BLOSC_NOLOCK"] = "1"` at the top of the server (and runner) entry point works.
     Set it before the thread pool is busy — `setenv` racing `getenv` in other threads is unsafe.
   - Windows: `libblosc.dll` imports `getenv` from **msvcrt.dll** (Phase 0, `objdump`). Julia's `ENV`
     writes the Win32 block, which msvcrt's copy does not see. In-process, set it with
     `ccall((:_putenv, "msvcrt"), …)`; the startup check reads it back with msvcrt's `getenv` — what
     blosc reads. Verified on CI `windows-latest` by the testset that asserts `blosc_nolock()`.
   - **One setter, in-process, at load** (`enable_blosc_nolock!` in `image_render.jl`, the file that
     owns `using Zarr`). Only the API server process decodes zarr in Julia — the runner and `app/` do
     not — and every launcher (`dev.jl`, `prod`, `app.py`, `bundle_check.sh`) loads that file, so it
     covers them all by construction. Copies at each spawn site were rejected: four places to drift,
     none needed.
   - The startup check logs the effective mode; in dev (`CECELIA_DEV`) a missing NOLOCK is an error.
   - It is process-wide: Julia-side zarr writes take the `_ctx` path too. That is the documented
     multithreaded form, not a new risk, but it is a behaviour change to name in the PR.
4. **Race-freedom is argued from source, plus a permanent regression test.** `blosc_decompress_ctx`
   builds a local context per call (no shared state). The SHA-256 serial-vs-parallel test (5 rounds x
   16 bricks) becomes a permanent test. Docs and PR text claim no more than that.
5. **Cache decoded *chunks*, not planes — and only for reads that use part of a chunk.** Keyed at the
   zarr chunk so it covers 512² tiled stores and any later chunk shape. One cache, process-wide. A
   read that covers every chunk it touches in full (a whole volume, a 2D plane, a chunk-aligned tile)
   bypasses it: measured 1.4–2.7x slower cold and no faster on a revisit through the cache, since
   there is no amplification to save. So the flat path is left alone by construction.
6. **Cache key = (level array directory, its inode + mtime, chunk index).** A stale decoded chunk is
   a correctness bug. Every writer stages and promotes (`test_store_staging_convention`), so a rewrite
   arrives as a new directory; a chunk file added in place would bump the directory's mtime too. One
   `stat` per read, not per chunk. The one store read while being written — a running segmentation's
   `.partial` staging store — is never cached. (Planned as a per-chunk-file stat; the directory key
   needs no knowledge of chunk key encoding and no metadata filename — the zarr-access ratchet bans
   those literals outside the import reader.)
7. **Single-flight per key, claim-first.** The 64 bricks of one timepoint share the same 124 chunks;
   concurrent misses on one key wait on one decode (measured: exactly 124 decodes). A read decodes the
   chunks nobody else holds first, from a random start, and only then waits — walking chunks in one
   shared order left every brick but one waiting and made a cold scrub slower than no cache.
8. **Byte budget, LRU, observable.** Settings → Storage → *Server read cache* (`[viewer].chunkCache`:
   `auto` / `off` / 512 MB–32 GB), applied to the running server without a restart. `auto` = 1/16 of
   physical RAM clamped to 256 MiB–4 GiB: 1 GiB on a 16 GB laptop (four 248 MB raw timepoints), and a
   workstation raises it by hand. Counters in `/api/diagnostics` → `chunkCache`. Named *server read
   cache* because the viewer already has a *viewer cache* — the browser's VRAM budget (`viewerCacheMB`).
9. **Scope.** `api/src/`, launch config, tests, docs. No renderer WGSL, atlas eviction, upload path
   (`writeBrick`, payload ring), or HTTP/2 changes. No rechunker without sign-off (Phase 3).
10. **Measure before and after every phase, same bench.** A result that contradicts the plan stops
    the phase. Projections are labelled as projections. Through-server numbers need a server running
    the branch — Dominik starts it; agents do not start or kill servers.
11. **The cache widens `image_render.jl`'s Zarr carve-out, deliberately and narrowly.** It lives in
    `api/src/chunk_cache.jl`, included by `image_render.jl` (still the only file with `using Zarr`),
    and `read_native` (`image_geometry.jl`) reads through it — so the byte-order swap still applies
    after assembly. It reads chunks; it does not open stores or parse metadata, and it adds no reader
    a caller could use instead of `read_native`.

## Phases

### Phase 0 — baseline + confirmations (no behaviour change) — DONE 2026-10-10

Results: serialisation reproduces on raw and derived stores; `/api/version` 0.7 → 60 ms during a
burst; a 128² sub-read costs as much as the whole chunk; amplification = chunk XY / brick XY per level
(64x raw, 16x our writer); every store is one z-plane per chunk; Windows blosc reads msvcrt's
`getenv`. Done:


- Through the current server, on Dml3RG (same geometry as 8eapy6): runs B (warm, conc 1), C (warm,
  conc 16), `flat_c1`, `flat_c4`, plus `/api/version` latency during C. Save as
  `slab_cache_results/p0_baseline_*`.
- Confirm the amplification directly: one 1024² chunk vs a 128² sub-read via `open_level` +
  `read_native`. Report the ratio.
- Levels L1–L3: amplification per level.
- Chunk shapes: which writer produced `1,1,1,512,512`; what a stock bioformats2raw import produces;
  whether any store in the projects dirs is brick-shaped.
- Windows: which CRT `libblosc.dll` imports (from the artifact), to fix how Decision 3's check reads it.

### Phase 1 — `BLOSC_NOLOCK` + hop off the interactive thread (one PR)

Built 2026-10-10. Raw bricks at 16 in flight 9.98 → 1.37 s, derived 1.63 → 0.53 s, `/api/version`
during a burst 60 → 11 ms mean; serial unchanged. Flat c4 only 1.28x: its read halves, but the 65 MB
responses are then transfer-bound in HTTP.jl's buffered body path — outside this plan (Decision 9).

- Read/encode of `try_serve_slab` inside `Threads.@spawn` on the default pool, mirroring
  `handle_stream`; the stream write stays on the connection task. `try_serve_movie` and
  `try_serve_board_asset` are NOT hopped: they read files (64 KB slices, small PNGs) and make no
  long C call, so they only ever waited behind slab decodes — which this hop removes.
- NOLOCK per Decision 3: one in-process setter at load; `blosc_nolock()` in `/api/diagnostics`
  (`bloscNolock`); startup error in dev / warning in prod if it did not land.
- Permanent tests: `blosc_nolock()` in-process, and a 5 x 16 serial-vs-parallel SHA-256 check in a
  child with `-t 4` (the API suite runs single-threaded, where a race check cannot bite).
- **Accept:** C 16 run wall well down from ~13.5 s (projection ~2 s); `flat_c4` real parallel gain
  (projection up to ~3–4x); `/api/version` during C back near idle; 0 mismatches, 0 errors. Far off
  → stop and investigate before Phase 2.

### Phase 2 — decoded chunk cache with single-flight — BUILT 2026-10-10

Viewer scrub without HTTP (every brick of 4 timepoints, 16 in flight; `slab_cache_scrub.jl`): raw
4.78 → 0.62 s cold, 0.34 s revisit; derived 1.10 → 0.51 s cold, 0.25 s revisit. A whole-movie sweep
(181 timepoints) stays at the 1 GB budget and decodes each chunk once. Flat volumes unchanged (bypass,
Decision 5). Prefetch not built: the claim-first cold pass already lands near the revisit. Details:
findings → *Phase 2*.

- Decisions 5–8. Brick reads assemble from cached chunks. Flat path uses it only if measured to help.
- Risk to resolve first: Zarr.jl decodes chunks inside `read_native`'s call; the cache needs a
  chunk-level read path under our control, not a Zarr.jl fork. It **extends `read_native`** in
  `api/src/image_geometry.jl` (with `open_level`): `read_native` assembles the block from cached
  decoded chunks, then applies the stored byte order exactly as today — a decoder that skipped that
  step would serve raw bioformats2raw (`>u2`) stores byte-swapped. `image_geometry.jl` already reads
  Zarr under `image_render.jl`'s narrow carve-out (the `zarr-access ratchet` exempts only
  `image_render.jl` from the `using Zarr` ban, and both headers say not to grow a general reader), so a
  chunk cache widens that carve-out: record it as a numbered decision when Phase 2 lands.
- Optional: on a brick miss for timepoint t, prefetch t's remaining chunks in the background —
  bounded, cancellable, never ahead of interactive requests.
- Tests: 124-decode single-flight test; rewrite-invalidates test.
- **Accept:** second and later bricks of a timepoint are slice-bound; a realistic scrub (many bricks
  per timepoint) reported, not one-brick-per-timepoint; memory under budget through a 45 GB movie scrub.

### Phase 3 — chunk-shape decision (write-up, not necessarily code)

Phase 0 answered the premise: it is the norm — bioformats2raw `1,1,1,≤1024,≤1024` and our writer's
`zarr_utils.plane_chunks` `1,1,1,≤512,≤512`, the latter justified by napari's per-plane slicing,
which is being retired. So write up the options: brick-aligned chunks for new writes (`plane_chunks`),
rechunk on import, a rechunk task for existing stores, or the cache as the whole fix — with measured read
costs (including the movie and 2D-plane readers, which prefer plane chunks). Needs sign-off.

### Phase 4 — docs

- `slab_cache_findings.md` post-fix numbers; `WEB_VIEWER_PLAN.md` → *Rejected:
  `Blosc.set_num_threads(n > 1)`* revisited with this evidence (blosc internal threads > 1 under
  NOLOCK is a separate, optional follow-up); the `ViewerWindow.vue` comments (~2014, ~2392) that say
  channel reads run "on the server's thread pool".
- Durable rules into `docs/ARCHITECTURE.md`: NOLOCK at launch, slab routes hop to the default pool,
  cache keying and budget.
- Outcome note under the archived brief's banner (Decision 3's correction).
