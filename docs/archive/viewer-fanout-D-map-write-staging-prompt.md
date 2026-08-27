# Session D — cold-path upload optimisation via MAP_WRITE staging

Branch: `feat/viewer-map-write-staging` (fork from origin/main).

## Goal

The WebGPU viewer's cold path (first fetch of a timepoint) is upload-dominated:
**535 ms for 351 MB is ~0.8 GB/s marginal** (see `docs/todo/WEB_VIEWER_PLAN.md` →
"Open, with a mechanism behind it"). Row alignment was measured and **refuted**
(padding to 256 changes nothing), and the GPU copy itself is under a ~100 ms
measurement floor.

The remaining candidate is the JS-heap → GPU-visible staging copy. Right now the
fetched slab lands in an ArrayBuffer, then gets copied into a GPU buffer, then
copied into the 3D texture. Two hops through the CPU.

Fix: use a `MAP_WRITE` buffer, write the fetched slab straight into
`getMappedRange()`, unmap, `copyBufferToTexture`. One hop.

## What to build

1. Rewrite the slab → texture upload path in `frontend/src/lib/webgpu/volumeRenderer.ts`
   to use `MAP_WRITE`:
   - `device.createBuffer({ usage: MAP_WRITE | COPY_SRC, mappedAtCreation: true, ... })`
     (or `mapAsync` if the buffer is pooled)
   - Write the fetched bytes into `getMappedRange()`
   - `unmap()`
   - `commandEncoder.copyBufferToTexture(...)`
2. Batch N operations per submit so wall time drops below the ~100 ms
   `onSubmittedWorkDone` quantum (see the plan's note — "batch N operations per
   submit to get under it, as G2 did").
3. **Measure** before and after. This is a perf change; without numbers it's a
   regression waiting to happen. Add the measurement to
   `docs/todo/spike/webgpu/` as its own JSON (mirror `p1_slab_bench.json`'s shape).

## Files to touch

- `frontend/src/lib/webgpu/volumeRenderer.ts` — the primary file
- `frontend/src/utils/volumeCache.ts` — if the staging buffer is pooled here
- `docs/todo/spike/webgpu/` — new bench JSON
- `docs/todo/WEB_VIEWER_PLAN.md` — flip the "Open, with a mechanism behind it" entry
  to CLOSED with the measured numbers

## Files NOT to touch

- `api/src/**` — this is a client-side optimisation
- `frontend/src/plots/plot.ts` — **Session C** owns it
- `frontend/src/utils/viewerOverlays.ts` — **Session C** owns it
- The record/movie path — **Session A** owns it
- The task preview path — **Session B** owns it

## Existing code to lean on

- `volumeRenderer.ts`'s existing upload — see how the ArrayBuffer currently lands in
  the GPU. Grep for `writeBuffer` / `copyBufferToTexture`.
- The measurement harness pattern: `docs/todo/spike/webgpu/slab_bench.jl` and its JSON.
- The WGSL shader itself doesn't change — this is upload plumbing.

## Test obligation

- Frontend test: assert the new upload path produces the SAME texture bytes as the
  old one for a known slab. Byte-for-byte equality — this is a plumbing change; a
  visual regression is a real risk. `pixi run test-frontend`.
- Measurement JSON: cold-path (first fetch) + warm-path (re-fetch after eviction)
  times, before and after, on at least two stores (small + real).

## Success criteria

1. `pixi run test-frontend` passes.
2. Cold-path wall time dropped measurably (record the number).
3. No visual regression on a browser eyeball test.
4. `WEB_VIEWER_PLAN.md`'s "Open" entry closed with numbers.

## Reservations to state before commit

- WebGPU browser support: `mapAsync` behaviour differs subtly across Chrome/Firefox;
  Dominik is on Linux. Don't caveat Safari — he doesn't use it. Do caveat Firefox
  if you can't verify it there.
- Measurement floor: `onSubmittedWorkDone` is ~100 ms; anything below that is
  UNMEASURED, not IMPROVED. Say so.
- The `MAP_WRITE` buffer is a per-frame allocation risk; if you pool it, describe
  the pool's eviction rule.

## Explicit constraints

- **Never start/kill the dev server.** Dominik owns 8080/5173/7655.
- **Never write shared dev config.**
- **Ask before commit; state reservations first.**
- **Branches + PRs only, never push to main.**
- **Copy `.env`** into your worktree.
- **Perf changes without numbers are regressions.** Measure both directions.
