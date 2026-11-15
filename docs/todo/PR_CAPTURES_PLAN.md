# GUI captures on PRs, without growing the repo or the package

**Status:** planning (2026-11-15). Nothing built. Brief: [`../archive/pr_gui_captures_brief.md`](../archive/pr_gui_captures_brief.md).

A PR that changes what a user sees should show it, and the images that show it should not end up in git
history or in the installed app. The screenshot engine already exists (`user-docs/shots/`,
`scripts/docshots/`, `pixi run doc-fixture`, `doc-shots`, `doc-images`); this plan adds where review
captures go and the checks that keep committed images small.

## What was measured (2026-11-15, during #2243)

- **History is 75 MB packed, and images are a small part of it.** Every image blob ever committed adds up
  to 3.5 MB (74 blobs). Most of the history is `app/test/suite.jl`, a ~1 MB file rewritten many times.
- **Committed images now:** `user-docs/images/` 1.3 MB (largest `readme-viewer.png`, 338 kB);
  `docs/` 1.1 MB of mocks and lab JPEGs (`docs/todo/gui-layout/`, `docs/todo/gui-audit/`).
- **Growth comes from regenerating images, not from new ones.** The `ai-*.png` files are in history
  twice, because #2243 recomposed them 1 px wider.
- **Composing is byte-deterministic.** `pixi run doc-images` run twice from the same captures wrote
  identical files, and git showed no change. A new capture can still differ by a byte without a visible
  change; this was not measured.
- **Packaging.** The stable tarball (`.github/workflows/release.yml`, an allow-list) does not include
  `user-docs/`. It **does** include all of `docs/`, so the 1.1 MB of mocks and audit JPEGs reach every
  install. `frontend/dist` includes the in-app shots (`app-*`, about 330 kB), as it should: the app
  shows them. The dev channel is a full branch archive and ships everything.
- **`gh` 2.45 cannot upload an image** to a PR body or comment; it only sends text. An agent cannot
  attach a capture to a PR.
- **CI cannot capture.** The fixtures are lab projects on the developer's machine (4kS67f, 4rNbMp), and
  viewer shots need a headed Chromium with WebGPU on a display.

## Decisions to lock

1. **Review captures stay local.** The agent captures on its isolated instance and builds a per-PR
   contact sheet (the "needs your eyes" sheet), written to the scratchpad or `~/Downloads/TMP`, never
   to git. The maintainer drags an image into the PR only when one is worth keeping in public. Release
   assets and a screenshots branch are both permanent and public, so neither is used.
2. **The PR names its shots; nothing maps a diff to a control.** A PR that changes visible UI lists the
   shot ids it changed (or ad-hoc `data-guide` anchors). The sheet captures those.
3. **"Before" only when it changes something that was already there.** The sheet captures main and the
   branch for a shot that already existed or a control that changed. A new control needs "after" only.
   Before/after for every change doubles the capture time.
4. **Only images a page shows are committed**, under `user-docs/images/`, within the size limits below.
5. **No WebP.** Pictures of the image are already JPEG q80 at the width they are shown; the UI shots are
   small PNGs.

## Checks (numbers from the state above)

| Check | Where | Fails on |
|---|---|---|
| Size limit, `user-docs/images/` | vitest, next to `docShots.test.ts` | a file over 350 kB, or more than 2 MB in total; names the files |
| No orphans | vitest | an image file no shot composes, or a page citing an image that is missing. Turns compose's current warning into a failure |
| Stray binaries | `test_doc_*_convention.py` style | a committed image outside `user-docs/images/`, `frontend/public/icons/`, `frontend/src/assets/`; the existing `docs/todo` images listed by name |
| Package | `release.yml` | `docs/todo/**` images taken out of the tarball's `docs` path (or the stray-binary list moves them out of `docs/`) |
| No rewrite when unchanged | `compose.mjs` | (not a failure) skip writing a file whose pixels match the committed one |

## PR template line

```
- [ ] GUI: shots on the contact sheet (<shot ids>), or "no visible change".
```

A visible change is a change to a `.vue` file, to a style, or to any text a user reads. Nothing is added
to the recital.

## Phases

- **P1** checks: size limit, orphans, stray binaries; the template line.
- **P2** the contact sheet: `pixi run doc-sheet -- --shots <ids> [--before main]`, captures on the
  isolated instance, writes one HTML or PNG sheet per PR outside the repo.
- **P3** packaging: `docs/todo` images out of the stable tarball; no-rewrite in compose.

## Open

- **Fixture data rights.** The lab images are already public in the README. Whether they may also appear
  in PR attachments is the maintainer's call.
- **Re-capture determinism.** Not measured; P3's pixel compare covers it either way.
