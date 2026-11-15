# GUI captures on PRs, without growing the repo or the package

**Status:** built (2026-11-15): P1 checks, P2 contact sheet (`pixi run doc-sheet`), P3 packaging; the no-rewrite compare dropped (captures are now deterministic, so an identical capture makes no new blob). Open questions answered below.

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

## Decisions (locked 2026-11-15)

1. **Review captures stay local.** The agent captures on its isolated instance and builds a per-PR
   contact sheet (the "needs your eyes" sheet) in `docshots-out/sheet/`, git-ignored, never in git and
   never attached to a PR: the fixtures are lab data. Release assets and a screenshots branch are both
   permanent and public, so neither is used.
2. **The PR names its shots; nothing maps a diff to a control.** A PR that changes visible UI lists the
   shot ids it changed (or ad-hoc `data-guide` anchors). The sheet captures those.
3. **"Before" is the committed image.** The sheet compares the branch's images with the ref's, so only
   the branch is captured; a new shot has no before.
4. **Only images a page shows are committed**, under `user-docs/images/`, within the size limits below.
5. **No WebP.** Pictures of the image are already JPEG q80 at the width they are shown; the UI shots are
   small PNGs.

## Checks (numbers from the state above)

| Check | Where | Fails on |
|---|---|---|
| Size limit, `user-docs/images/` | `test_repo_images_convention.py` (vitest cannot read file sizes under vue-tsc) | a file over 350 kB, or more than 2 MB in total; names the files |
| No orphans | `docShots.test.ts` | an image file no shot composes, or a page citing an image that is missing. Turns compose's current warning into a failure |
| Stray images | `test_repo_images_convention.py` | a committed image outside `user-docs/images/`, `frontend/public/icons/`, `frontend/src/assets/`; the existing `docs/todo` images listed by name |
| Package | `release.yml` | (not a failure) `docs/todo` PNG/JPEG left out of the tarball with `--exclude` |
| No rewrite when unchanged | `compose.mjs` | (not a failure) skip writing a file whose pixels match the committed one |

## PR template line

```
- [ ] GUI: shots on the contact sheet (<shot ids>), or "no visible change".
```

A visible change is a change to a `.vue` file, to a style, or to any text a user reads. Nothing is added
to the recital.

## Phases

- **P1** (built) checks: size limit and stray images (`python/cecelia/tests/test_repo_images_convention.py`),
  orphans (`frontend/src/utils/docShots.test.ts`); the template line; whole-window `overview` shots as JPEG
  (the README hero went from 942 kB to 244 kB across both schemes).
- **P2** (built) the contact sheet: `pixi run doc-sheet [-- --ref origin/main] [--after DIR] [--all]`
  (`scripts/docshots/sheet.mjs`). Before is the image at the ref (git), after the working file; it lists
  every new, changed or removed image and writes `docshots-out/sheet/index.html` and `sheet.png`
  (git-ignored). A review capture that is not a docs image is composed outside the repo and the sheet
  pointed at it (`--after`). "Before" is the committed image, so nothing is captured twice.
- **P3** packaging: `docs/todo` images out of the stable tarball (built, in P1); no-rewrite in compose (dropped, see *Answered*).

## Answered (2026-11-15)

- **Fixture data rights: no.** Fixture images never go into a PR body or comment. The contact sheet
  stays on the developer's machine; images a page shows stay in `user-docs/images/` (the README's and
  the docs' are already public).
- **Re-capture determinism: yes, after two fixes.** Measured by capturing the same shots twice:
  - the Gate page (headless) came out byte-identical;
  - the viewer window (headed, WebGPU) differed in 0.17% of its pixels: not the image, but its channel
    histograms and the loaded-range strip, still filling in as data streamed, and in one run the focus
    ring of the last toggle clicked. `capture.mjs` now screenshots until two in a row are identical
    (up to 10 s, failing the shot otherwise) and blurs the focused control first. Two more runs per
    scheme were then byte-identical.
  - compose was already byte-identical from the same captures.

  So the planned no-rewrite compare in compose is dropped: an unchanged capture writes the same bytes,
  and git stores nothing new.
