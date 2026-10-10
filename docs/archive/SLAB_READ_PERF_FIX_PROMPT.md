> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. The measured
> findings it was built on live in `docs/todo/spike/webgpu/slab_cache_findings.md`.
>
> **Outcome (2026-10-10).** Built as `docs/todo/SLAB_READ_PERF_PLAN.md`: Phase 1 (NOLOCK + pool hop,
> #1575 via #1572), Phase 2 (decoded-chunk cache, #1581), Phase 3 decided for new images (import chunk
> 512, derived stores inherit, #1576). Durable rules: `docs/ARCHITECTURE.md` → *Slab reads — server
> side*. Premises that turned out wrong:
> - **Concern 2** — `BLOSC_NOLOCK` is read on *every* `blosc_decompress`, so setting it late does take
>   effect. The real trap was Windows: `libblosc.dll` reads msvcrt's environment, which a Julia `ENV`
>   write does not update (fixed with `_putenv`, verified on CI).
> - **Concern 1's "3x slower"** — the 0.34x was one machine; another measured 0.95x. "No gain from the
>   hop alone" held on both.
> - **Concerns 6–7** — the cache is keyed on the level directory (inode + mtime), not per-chunk files;
>   it caches only partial-chunk reads (whole-volume reads were slower through it); and plain
>   single-flight made cold scrubs slower until reads claimed unclaimed chunks first.
> - **Phase 1 accept for flat (3–4x)** was not met (1.28x): the read halved, then the
>   response write became the limit, for bricks too. Not buffering, as first guessed: the write ran on
>   HTTP.jl's interactive thread and converted a `reinterpret` body byte by byte. Plan Phase 5 fixed both.

# Prompt: fix `/api/viewer/slab` read performance

You are working in `schienstockd/cecelia`. Read this whole file before touching code. Then read
`docs/todo/spike/webgpu/slab_cache_findings.md` (the measured findings this prompt is built on) and
`docs/todo/WEBGPU_UPLOAD_PATH_PLAN.md` (for context: the GPU upload path is NOT the bottleneck).

## Goal

Make brick reads through `/api/viewer/slab` fast on plane-chunked zarr stores, without regressing the
flat (whole-volume) path. Work in phases. Each phase is independently shippable, measured before and
after, and gets its own PR.

## What was measured (do not re-argue, re-verify only where stated)

Store: `ccidImage.ome.zarr/0`, shape (t,c,z,y,x) = (181,4,31,1024,1024) u16, blosc/zstd, chunks `1,1,1,1024,1024`
(one full plane per chunk). Brick = 128x128 x 31z x 4c = 4.06 MB.

- Cold is only 1.3x warm (255 ms vs 195 ms per brick). NVMe at ~12% util. **Disk is not the bottleneck.**
  A RAM tier of compressed bytes is rejected.
- **Read amplification ~61x.** One brick decodes 4c x 31z = 124 chunks x 2 MB = ~248 MB, keeps 4 MB.
  Decoder speed itself is normal (~1.4 GB/s).
- **Serialisation, two layers:**
  1. `try_serve_slab` (also `try_serve_movie`, `try_serve_board_asset`) runs inline on HTTP.jl's single
     `:interactive` thread; `handle_stream` hops other routes to the default pool, these are dispatched before that hop.
  2. c-blosc 1.21.6 global mutex. Measured without HTTP at 16 threads: default **0.34x** of serial (3x slower),
     `BLOSC_NOLOCK=1` **7.0x** faster, SHA-256 identical to serial over 5 rounds x 16 bricks.
- Flat path (whole (t,c) volumes) has no amplification (260 MB/s) but has the same serialisation: 4 channels
  fetched with `Promise.all` decode one after another (1.05x at concurrency 4).
- Transport (TLS + HTTP/2) is ~20 ms per brick. Not the bottleneck on this store.

## Concerns that shape the plan (treat as hard constraints)

1. **Layer 1 alone makes things slower.** Moving the read off the interactive thread without
   `BLOSC_NOLOCK=1` puts threads on the blosc mutex (measured 0.34x). Phase 1 must ship both together or not at all.
2. **`BLOSC_NOLOCK` must be set before blosc's first call.** Setting it late silently does nothing and you will
   measure no gain. Check *every* launch path: `pixi run dev`, prod, the installed app, Windows. Add a startup
   check that logs whether the effective mode is NOLOCK, and fail loudly in dev if it is not.
3. **Race-freedom is argued, not proven.** The hash-compare stress run cannot prove absence of a race. The `_ctx`
   path is blosc's documented multithreaded form, which is the justification. Do not claim more than that in
   docs or PR text. Keep the SHA-256 serial-vs-parallel test as a permanent regression test.
4. **Threading does not fix amplification.** Even at 7x, each brick still decodes ~248 MB. Phase 2 (decoded plane
   cache) is what removes the waste. Do not call the problem solved after Phase 1.
5. **The 64 bricks of one timepoint share the same 124 chunks.** The benchmark used one brick per timepoint
   (worst case). Real scrubbing requests many bricks per timepoint concurrently, which creates a
   **thundering herd**: 64 requests all missing the same plane at once. The cache needs **single-flight**
   (one decode per key, other waiters block on it), or the cache decodes the same plane many times in parallel.
6. **Cache memory must be bounded and observable.** Decoded planes are 2 MB each; one timepoint is ~248 MB. Needs an
   explicit byte budget, LRU eviction, and a stats endpoint or debug log (hits, misses, bytes, evictions).
   Pick a default that is safe on a laptop (8 GB VRAM machine, much less RAM than the workstation); the workstation
   (256 GB RAM) should be able to raise it via setting. Propose the default and justify it.
7. **Cache keys and invalidation.** Key on (store path, level, t, c, z) plus something that changes when the store
   is rewritten (mtime or a version token). A stale decoded plane after a re-run of an upstream task is a
   correctness bug, not a perf bug.
8. **Layer 1 fix is untested through the real server.** The 7x number is from a standalone Julia bench. The real
   expectation (16-concurrent run wall ~13.5 s -> ~2 s) is a projection. Verify before claiming it.
9. **Do not regress the flat path.** It already runs at 260 MB/s. Re-run `flat_c1` / `flat_c4` before and after.
10. **Scope.** Do not touch renderer WGSL, LRU atlas eviction policy, or the upload path (`writeBrick`, payload
    ring). Those are not the bottleneck. Do not re-open HTTP/2.
11. **Contradicts an earlier decision.** `WEB_VIEWER_PLAN.md` -> "Rejected: `Blosc.set_num_threads(n > 1)`" claimed the
    safe `_ctx` route is "upstream work". `BLOSC_NOLOCK=1` reaches it through the existing Blosc.jl call. Update
    that section with the new evidence; also check whether blosc internal threads > 1 are now safe under NOLOCK, but
    treat that as a separate, optional follow-up, not part of Phase 1.

## Phases

### Phase 0: baseline and two cheap confirmations (no behaviour change)

- Re-run the existing bench from `docs/todo/spike/webgpu/` against the current server and save results as
  `slab_cache_results/p0_baseline_*`: runs B (warm, conc 1), C (warm, conc 16), `flat_c1`, `flat_c4`.
- Confirm the 61x figure directly (open question 2): time one 1024^2 chunk read vs a 128^2 sub-read via the route's
  `open_level` + `read_native`. Report the ratio.
- Measure L1-L3 (open question 3). Expect amplification to shrink with level; report actual numbers.
- Check open question 4: what chunk shape does our own writer produce, and what does a stock bioformats2raw import
  produce? State whether plane chunking affects every store or only this one.

### Phase 1: `BLOSC_NOLOCK=1` + move reads off the interactive thread (ship together)

- Run the read/encode part of `try_serve_slab` inside `Threads.@spawn` on the default pool, mirroring
  `handle_stream`. Apply the same hop to `try_serve_movie` and `try_serve_board_asset`.
- Set `BLOSC_NOLOCK=1` at the earliest point in every launch path (see concern 2). Add the startup log/check.
- Add the permanent serial-vs-parallel SHA-256 test (5 rounds x 16 bricks).
- Re-run C 16 (HTTP/2) and `flat_c4`. Report before/after.

**Accept:** C 16 run wall drops substantially (target ~2 s from ~13.5 s; if it lands far from that, stop and
investigate before Phase 2). `flat_c4` shows real parallel gain (target up to ~3-4x). 0 hash mismatches. 0 errors.

### Phase 2: decoded plane cache with single-flight

- Server-side LRU of decoded planes keyed per concern 7, bounded per concern 6, with single-flight per concern 5.
- Brick reads assemble from cached planes (slice only). Flat path may use it too if it measurably helps; if it does
  not, leave flat alone.
- Optional but valuable: when a brick request arrives for (t), prefetch the rest of that timepoint's planes in the
  background (bounded, cancellable, low priority so it never starves interactive requests).
- Test: concurrent requests for 64 different bricks of one timepoint produce exactly 124 decodes, not 64 x 124.
- Test: rewriting the store invalidates cached planes.

**Accept:** per-brick warm median drops by a large factor once the timepoint's planes are resident; second and later
bricks of a timepoint are slice-bound. Report a realistic scrub workload (many bricks per timepoint), not just
one-brick-per-timepoint. Memory stays under budget under a 45 GB-movie scrub.

### Phase 3: chunk-shape audit (decision, not necessarily code)

- Based on Phase 0 findings: is plane chunking the default for imports and for our writer?
- If yes, write up the options (brick-aligned rechunk on import, a rechunk task for existing stores, or accept the
  cache as the fix) with measured read costs. Do not implement a rechunker without sign-off.

### Phase 4: docs

- Update `slab_cache_findings.md` with post-fix numbers, `WEB_VIEWER_PLAN.md` (concern 11), and the client comment
  near `ViewerWindow.vue` ~2084 / ~2433 that claims channel reads run "on the server's thread pool".
- Promote durable rules (NOLOCK must be set at launch; slab routes must hop to the default pool; cache keying and
  budget) into `docs/ARCHITECTURE.md`.

## Process rules

- Measure before and after every phase with the same bench. Numbers go in the PR description.
- One PR per phase. Phase 1 is one PR containing both halves.
- If a measured result contradicts this plan (for example Phase 1 shows no gain, or NOLOCK is not taking effect),
  **stop and report** rather than pushing ahead to the next phase.
- Do not claim a speedup you did not measure. Projections are labelled as projections.
- Keep changes in `api/src/`, launch config, tests, and docs. No renderer, no upload-path changes.

## Final report format

For each phase: what changed (files), before/after table (median, p95, MB/s, run wall, cores busy), tests added,
anything surprising, and what you chose not to do and why. End with a one-line verdict per concern 1-11 above
(held / violated / not applicable).
