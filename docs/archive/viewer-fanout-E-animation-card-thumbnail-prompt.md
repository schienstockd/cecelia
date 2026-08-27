# Session E — animation card thumbnail via WebGPU canvas.toBlob()

Branch: `feat/viewer-animation-thumbnail` (fork from origin/main).

## Goal

Animation cards (the entries on `/movies` or wherever animations are listed) currently
have no thumbnail — either they show a placeholder or the first frame gets rendered
server-side. Add a thumbnail generated from the browser's WebGPU canvas via
`canvas.toBlob()` at the moment the animation is saved.

This is the same "pixels-across-time uses the browser" pattern
`VIEWER_PARITY_PLAN.md` calls out for single-clip export — the browser's canvas
already shows exactly what the animation is going to render, so
`canvas.toBlob('image/png')` and post it to a thumbnails endpoint. No new render
pipeline.

Referenced in `WEB_VIEWER_PLAN.md` P5's remaining items as one of the parked
follow-ups — "Animation-card thumbnail via WebGPU canvas `toBlob()`".

## What to build

1. On animation SAVE (frontend): call `canvas.toBlob('image/png')` on the WebGPU
   canvas at the frame the animation opens on (or a mid-frame — Dominik's call).
2. POST the blob to a new endpoint (or extend the existing animation-save endpoint
   to accept a thumbnail multipart part).
3. Server-side: store the thumbnail alongside the animation config
   (mirror where movie thumbnails live if any exist; else `{animation}.thumb.png`).
4. Wire the animation card component to load the thumbnail; fall back to the current
   placeholder if the file is missing.

## Files to touch

- The animation card Vue component (grep for `AnimationCard.vue` or similar in
  `frontend/src/`)
- The animation-save flow (frontend + `api/src/`)
- A new small endpoint in `api/src/` if the save flow doesn't already accept binary
- `docs/todo/WEB_VIEWER_PLAN.md` — flip the "Animation-card thumbnail" note to BUILT

## Files NOT to touch

- Everything the other four sessions own (see A, B, C, D briefs)
- The offline renderer path — this is a live-canvas capture, not an offline render
- `frontend/src/lib/webgpu/volumeRenderer.ts` — treat the canvas as an opaque
  surface; do NOT reach into the render pipeline

## Existing code to lean on

- Whatever the animation card currently uses for its placeholder — the swap-in point
  is there.
- If movies have thumbnails today (grep `movies.json` handling), copy the shape.
- `canvas.toBlob()` is standard DOM — no library needed.

## Test obligation

- Frontend test: the toBlob path produces a valid PNG blob (assert on size + magic
  bytes)
- API test: the endpoint accepts a multipart POST and writes the file
- No end-to-end test needed — this is a one-shot capture, not a pipeline

## Success criteria

1. `pixi run test-frontend` + `pixi run test-api` pass.
2. Save an animation → its card shows the actual first frame as thumbnail on reload.
3. Delete the thumbnail file → the card falls back to the placeholder without
   error.

## Reservations to state before commit

- If the canvas is behind a `preserveDrawingBuffer: false` context, `toBlob` returns
  an EMPTY image. Whichever way the viewer is configured today, state it — and if
  you had to flip the flag, note the perf implication (per-frame buffer preservation
  is a real cost).
- Chrome/Firefox parity on `toBlob` timing — it's async; make sure the animation
  save waits for it before completing.
- Thumbnail size — a full-canvas PNG can be large. Downscale to a reasonable card
  size before encoding.

## Explicit constraints

- **Never start/kill the dev server.** Dominik owns 8080/5173/7655.
- **Never write shared dev config.**
- **Ask before commit; state reservations first.**
- **Branches + PRs only, never push to main.**
- **Copy `.env`** into your worktree.
- **This is a small chunk.** If it grows past a day, stop and check with Dominik.
