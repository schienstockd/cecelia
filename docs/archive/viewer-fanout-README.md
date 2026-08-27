# Viewer P5+ fan-out — session briefs

Five briefs, each self-contained and hand-off-ready. Each session runs on its own
worktree + branch. The "files NOT to touch" list in each brief protects the other
sessions' surfaces.

## Independent, can start today

| Brief | Chunk | Files primarily touched |
|---|---|---|
| [viewer-fanout-A-p5-movie-rail-prompt.md](viewer-fanout-A-p5-movie-rail-prompt.md) | P5-a movie rail wiring off napari | `api/src/viewer_api.jl`, `app/src/jobs.jl`, record UI |
| [viewer-fanout-B-p7-task-previews-prompt.md](viewer-fanout-B-p7-task-previews-prompt.md) | P7 task preview overlays off napari | `api/src/preview_api.jl`, module page Vue |
| [viewer-fanout-C-parity-palette-json-prompt.md](viewer-fanout-C-parity-palette-json-prompt.md) | VIEWER_PARITY Phases 1 + 2 (palette + track modes JSON) | `frontend/src/plots/`, `api/src/overlay_author.jl`, `viewerOverlays.ts` |
| [viewer-fanout-D-map-write-staging-prompt.md](viewer-fanout-D-map-write-staging-prompt.md) | Cold-path upload optimisation via MAP_WRITE | `frontend/src/lib/webgpu/volumeRenderer.ts` |
| [viewer-fanout-E-animation-card-thumbnail-prompt.md](viewer-fanout-E-animation-card-thumbnail-prompt.md) | Animation card thumbnail via canvas.toBlob() | animation card Vue + one small API endpoint |

## Gated on the above

- **PAR-3 + PAR-4** (parity test + settings defaults JSON) — one session, after C ships.
- **P8 decommission** — one session, after A + B ship.

## Not fan-outable

- **Vertical orientation verify** — one line kept or reverted after Dominik clicks
  through `docs/todo/spike/webgpu/shader_check.mjs`. He owns it.

## Handoff notes

- Every brief starts with the branch name and asks the session to fork from
  `origin/main`. Do NOT fork from `feat/viewer-p5-overlay-author`.
- Every brief has a "Files NOT to touch" list keyed to which OTHER session owns the
  code. If two sessions collide anyway, the earliest-in-flight one wins by default.
- Every brief ends with the standard constraints: never start/kill Dominik's dev
  server, ask before commit, state reservations first, branches + PRs only.

## Cost estimate (rough)

- A (movie rail): 1–2 days — Julia + Vue, touches persisted config
- B (task previews): 1–2 days — depends on how many module pages had previews
- C (palette JSON): 0.5–1 day — small, cross-cutting
- D (MAP_WRITE): 1 day + a measurement session
- E (thumbnail): 0.5 day
