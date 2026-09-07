# viewState cx/cy sign-fix — consumer audit

## What changed (shipped as its own commit; see the branch `fix/viewstate-cxcy-sign`)

`frontend/src/utils/viewer/viewState.ts` — the `buildViewState` ↔ `applyViewStateToBrowser`
pair was internally consistent but INVERTED from the docstring on
`ViewerViewState.camera.center` / `FocusOnCellTarget`. Docstring says cx/cy are the L0
image pixel the camera looks at; the implementation stored the MIRROR around image centre
(`W/2 - panXpx`). A `buildFocusViewState({cx: 6})` panned to `nX - 6` — bottom-right corner
for a cell at the mid-left.

The fix (symmetric, so round-trip is preserved):

- `buildViewState`: `cx = (nX/2) + panXpx`, `cy = (nY/2) + panYpx` (was `-`)
- `applyViewStateToBrowser`: `panXpx = cx - nX/2`, `panYpx = cy - nY/2` (was `nX/2 - cx`)

Tests updated: `viewState.test.ts:39-48` now asserts `256 + 200`, and the round-trip test
at :117-135 passes unchanged. All 23 tests green. Typecheck clean.

**Do not revert.** The docstring is now the source of truth; every consumer must agree.

## Why you're here

Round-trips through `buildViewState → applyViewStateToBrowser` are fine — both sides
flipped together. What we need to audit is EVERY OTHER place that either produces or
consumes `viewState.camera.center[1]` / `[2]`, because any code that hand-rolled the
old-convention math is now off by `2 * panXpx` (or the mirror around the image, depending
on direction).

The `buildViewState` file header used to say (paraphrasing): "same arithmetic as
`publishRegionSink` in ViewerWindow, kept in ONE place so a bug in one publish path is a
bug in the other." That comment is the smoking gun — there's at least one other publisher
somewhere that we need to bring into line.

## Audit tasks (in order)

### 1. Grep the frontend

```bash
cd /home/dominik/cc-workspace/cecelia/cecelia-vizsla-review/frontend
rg -n 'camera\.center\[[12]\]' src/
rg -n 'panXpx|panYpx' src/
rg -n 'panX\s*[/*]\s*umPer' src/
rg -n 'nX\s*[/*]\s*2' src/
rg -n 'publishRegionSink|publishViewState' src/
```

Every hit outside `viewState.ts` and its tests needs a look. For each:
- Does it PRODUCE a `camera.center` (write side)? Then it needs `+ panXpx` / `+ panYpx`.
- Does it CONSUME a `camera.center` to derive pan (read side)? Then it needs
  `cx - W/2` / `cy - H/2`.
- Is it a snapshot fixture with hardcoded numbers? Probably needs the value flipped.

### 2. ViewerWindow.vue in particular

There's a formula around `ViewerWindow.vue:1971` — `cxImg = cam.value.panX + ex / 2`
(or similar). That's the local shader-space mapping, unrelated to viewState — but the
FILE also has (or had) a `publishRegionSink` or similar that emits camera state to
subscribers. Find it and confirm it either delegates to `buildViewState` or matches its
new convention. If it hand-rolls the math, migrate it to `buildViewState`.

### 3. Backend / renderer

```bash
cd /home/dominik/cc-workspace/cecelia/cecelia-vizsla-review
rg -n 'camera.*center|center.*camera' api/src/ python/ julia/
rg -n 'viewstate_to_render_args' api/
```

The offline movie renderer (`api/src/movie_render.jl`) reads `camera.center` per the
schema in the file header of `viewState.ts`. If it interprets cx/cy as "L0 pixel" (which
is what the schema says), it was already RIGHT and every browser-authored keyframe was
rendering in the mirror location. Confirm what convention the renderer actually uses —
this may already be aligned with the new (fixed) frontend convention. If it is,
previously-captured keyframes replayed against the new build will jump; document this in
the commit message so Dominik knows keyframes recorded before this commit are stale.

### 4. Keyframe restore path

Grep for `webgpu\.cam` and `keyframe.*restore` to confirm keyframes deserialise via the
`webgpu` sidecar (which stores the raw `OrbitCamera`), not via `camera.center`. If they
DO use `camera.center`, they now round-trip cleanly through the fixed applier — so no
change needed, just verification.

### 5. Test fixtures

```bash
rg -n 'camera.*center.*\[' src/**/*.test.*
```

Any test that hardcodes a `center: [z, y, x]` value with an assumption about what that
means will need updating.

## Reporting

Before committing, post a report:

- Files touched (with one-line reason each)
- Which consumers were already correct (no change needed) vs. which had latent bugs
- Whether the offline renderer needs a compensating flip
- Whether previously-saved keyframes / animations will replay in the same place or jump

Then hand the commit off to Dominik — he calls the commit.

## Rules of engagement

- This is a shared worktree (`cecelia-vizsla-review`). Other sessions may be working
  here. Verify branch before you commit; stage explicit paths, not `git add -A`.
- Never start / kill Dominik's dev server. He runs the app on 8080/5173/7655.
- If you find that the fix is wrong (e.g. the renderer disagrees and the previous
  convention was actually correct given the schema), STOP and post — do not silently
  revert; the docstring was clearly wrong even if the math was right.
- Cheap reservations: close them. Don't ship the audit with a "might also affect X"
  hanging — grep X.

## Not your problem

The correction cockpit (`CorrectionCockpit.vue`, `correctionCockpit` store,
`showTracksInViewer.ts`) is being continued in another session. Leave those files alone
unless the audit turns up a bug in them. The temporary `_diagCx / _diagCy / _diagUmCx`
etc. logging in `showTracksInViewer.ts` will be cleaned up there — don't touch it.

Also skip: the P5 click-a-cell-to-correct feature, the sidebar accordion migration, and
the icon-only launcher redesign. All landed and not part of this audit.
