# Guide screenshots — plan

**Status:** **parked** (2026-11-13) — trial runs T0–T4, two screenshot passes and three draft booklets
(intravital, confocal, PhenoCycler) done locally (below); no pipeline code yet, tutorials not committed. Brief: [`docs/archive/GUIDE_SCREENSHOTS_PROMPT.md`](../archive/GUIDE_SCREENSHOTS_PROMPT.md);
read its banner first, because six of the brief's premises had moved by the time it was archived.
Related: [`GUI_CLARITY_PLAN.md`](GUI_CLARITY_PLAN.md) (the layout these shots follow),
[`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md), [`WORKFLOW_RECIPES_PLAN.md`](WORKFLOW_RECIPES_PLAN.md),
[`FIGURES_PLAN.md`](FIGURES_PLAN.md).

## Goal

Generate screenshots from the real app by script, so they follow the UI as it changes. They are used in
three places: the in-app guides, the docs site and one-slide summaries for talks. Nothing is drawn, mocked
up or illustrated: a picture of an interface that differs from the real one is worse than no picture.

## What already exists (checked against `origin/main` 2026-11-12)

| Piece | Where | What it gives this plan |
|---|---|---|
| Page capture runner | `scripts/gui_capture.mjs` (`pixi run gui-capture`, #2084) | `playwright-core` + system Chromium, sign-in with the instance token, opens a project through `projectMeta`, routes through the app's router, closes launch pop-ups, one PNG per page. Refuses 8080/5173. Used for the GUI clarity census (projects RFtn9f, 4kS67f) |
| Headless light plot PNGs | `scripts/agent_eval/board_render.py` → `BoardRenderView.vue` | Analysis-board slots as plot-only light-theme PNGs through the board's PDF export path, from `frontend/dist`, write-guarded |
| Viewer frames without a browser | offline renderer (`render_view_frame`, `POST /api/viewer/thumbnail`, `wgpu_host`) | Viewer image area rendered headless, golden-pinned to the viewer's shaders |
| User-shared viewer frames | `api/src/captures_api.jl` | Share button → `<proj>/captures/<id>/frame.png`. A user path, not a capture source for this plan |
| Colour schemes | `frontend/src/utils/colourScheme.ts`, `data-theme` on `<html>` | Dark (default) and Light, switchable by `localStorage` key `cc.colourScheme` before first paint |
| Guide steps | `frontend/src/lib/guides/types.ts` `GuideStep` | Anchor, text, bullets, gates. **No image field yet** |
| Recipes | `frontend/src/lib/guides/recipes.ts` | Already map the R version's missions; the candidate "one path per image type" source |

Not found: the readthedocs source (not in this repo or the local old-R checkout). Headless WebGPU was only
tried in Firefox, where it failed (`docs/archive/napari-webgpu-audit.md` G0); Chromium is untested.

## Decisions (proposed; none locked yet)

1. **Extend `gui_capture.mjs`, don't write a second runner.** Add a manifest mode beside the census mode.
2. **One manifest of shots** (`docs/guide-shots/shots.json` or a `.ts` beside the guides — open): shot id,
   route, project + image, setup steps, selector to crop to, selector to highlight, scheme, viewport,
   device scale factor, alt text, masks.
3. ~~Highlight drawn by the capture step with a CSS class in `--cc-accent`~~ — **superseded (2026-11-13)**: the purple ring was invisible among the app's own purple; the pointer is drawn after capture from the recorded rect, in the guide-bubble style (see *Booklet direction*). Never baked in by hand.
4. **Dark for guides; Light only for the slide kit**, to keep the extra baselines to a handful.
5. **Viewer image area from the offline renderer** unless T0 shows headless Chromium WebGPU works. Either way
   the shot is a crop of the image area, which sidesteps the viewer chrome not following the Light scheme.
6. **A guide step names a shot id; a missing image renders nothing** (no empty frame, no broken icon).
7. **Slides reuse `board_render.py` for plot tiles** and an HTML slide template rendered by the same
   Chromium (PNG 1920×1080 + PDF). No PPTX.
8. **No AI dependency.** Guides and shots work with the AI toggle off.

## Trial runs (before any design is locked)

Smallest first. Each says what it needs and what it settles.

| # | Run | Needs | Settles |
|---|---|---|---|
| T0 | Headless Chromium WebGPU probe: `adapter_probe_standalone.html` through `playwright-core`, default flags vs. the documented WebGPU/Vulkan flags vs. software (SwiftShader) | Nothing running; seconds | Decision 5: can the pop-out viewer be shot in a browser at all, and with which flags |
| T1 | `gui-capture` as it is, 3 routes, one project, run twice, pixel-diff the pairs | An isolated instance (own `CECELIA_DEV_DIR`, slot ≠ 0, project copies with symlinked zarr) | What varies between runs (spinners, timestamps, fonts), so the determinism rules are measured, not guessed |
| T2 | One manifest-style shot from a throwaway script: Gate page, crop to the gate plot, highlight one control, 2× scale, Dark and Light | T1's instance | Element crop, highlight, scale and scheme switch work on the real app |
| T3 | Viewer image area: pop-out in Chromium if T0 passed, else `POST /api/viewer/thumbnail` | T1's instance | The viewer tile for the first slide |
| T4 | `board_render.py` on a board with a gate plot and a population list | T1's instance + a board in the copied project | Whether slide plot tiles need any new code |

### Trial results (2026-11-12, workstation: RTX 2000 Ada laptop, NVIDIA 580, snap Chromium 157, `playwright-core` 1.65)

Isolated instance on slot 5 (`CECELIA_DEV_DIR` in a scratch dir, metadata copies of RFtn9f + 4kS67f with
symlinked zarr stores, one user profile). Throwaway scripts, nothing merged.

| # | Verdict | Evidence |
|---|---|---|
| T0 | **PASS** (adapter) | No flags: `requestAdapter()` → null. `--enable-unsafe-webgpu` alone → SwiftShader. **`--enable-unsafe-webgpu --enable-features=Vulkan --use-angle=vulkan` → NVIDIA "lovelace"**, 3/3 runs. Chrome's guide also lists `--disable-vulkan-surface`; **on this machine that flag removes the hardware adapter**. Compute job: GPU 11 ms vs SwiftShader 660–910 ms |
| T1 | **PARTIAL** | First run captured the profile picker on every page (two profiles ⇒ "Who's driving?"); one profile skips it. Then two runs differ in: the console line (last log message), a panel still loading ("server may still be starting") on Manage images, and state carried between runs (sidebar groups saved to the profile; "Last opened" written by opening the project). Settled pages were pixel-identical |
| T2 | **PASS** | Gate page via `project.focusImages('gate', set, [image])` + router; plot waited on until "Loading…" is gone (a fixed 2.5 s wait shot the spinner); ring on `[data-guide="gate.drawTool"]`; panel crop at 2× (870×1336). Scheme must be set through the `settings` store — `localStorage cc.colourScheme` alone is overridden by the profile. Light findings: the gate label keeps a dark halo; a sliver of the toolbar's button bleeds into the crop |
| T3 | **FAIL in the browser; offline renderer PASS** | Pop-out loads, but drawing to a WebGPU canvas kills the device in headless: Vulkan shared-image import fails (`Requested allocation size … smaller than the image requires`, then `VK_ERROR_OUT_OF_DEVICE_MEMORY` with heights padded to 128). Not VRAM (103 MiB of 8 GB used; 1 GB buffers allocate fine), not the viewer's budget (`?cacheMB=512` same), not canvas size; Graphite / `--use-vulkan=native` same. SwiftShader loses the device too. `POST /api/viewer/thumbnail` renders the image area headless: 6 s cold / 1.4 s warm, two runs byte-identical, size from `viewState.canvas = {width, height}`. Gaps: **channels only, no overlays** (the slide needs the population overlay), and the only stored viewState (napari era) framed the image off-centre |
| T4 | **PASS** | `board_render.py` (needs `frontend/dist`, `--api-url` to the isolated backend) → 10 plot-only light PNGs for one board; blocked two writes (`profile/settings/patch`, `viewer/plots/register`). The T legend swatch is white on white |

What this changes: Decision 5 becomes "offline renderer" (the browser path is blocked by Chromium,
not by us). Determinism rules come from T1: one profile, fresh config + project copy per run, mask
the console line, wait on the shot's own loading state (not a timer). Open for the viewer tile: the route, not
the renderer — `render_view_state_still(...; img, overlays_config)` (`api/src/movie_render.jl`) already draws
populations / mask / tracks as for a movie, but `POST /api/viewer/thumbnail` (`api/src/viewer_api.jl`) calls it
without `img` / `overlays_config`; plus a current-viewer viewState per shot.

### First tutorial pass (2026-11-12)

Goal from the maintainer: a reader looks at a shot and thinks "I do this and that, and I will see xyz" —
the spirit of the old tutorial (https://cecelia.readthedocs.io/en/latest/tutorial.html), not its layout.
Each shot is one step: **Do** (the ringed control) + **You'll see** (the result, on the same screen).
Throwaway manifest + runner (`shots.json`, `run.mjs`), 12 shots; review sheet with captions as HTML + PDF.

**Data.** Live: 4kS67f image yFNlw2 (B and T cells, every result exists: tracks, gates, HMM, clusters,
board). Static: 4rNbMp image FtGoJO, which **is the old tutorial's static sample** (XCR1-Venus / LCMV
section), segmented but with unnamed channels and no gates. RFtn9f has no segmentation, so it can't show
results.

**Shot list (v1).** Live: 1 import (Manage images, ring Add images) · 2 channels (Metadata, ring channel
names) · 3 checks (hover a row's QC flag → findings with their fix) · 4 cleanup (ring the function picker;
new version "driftCorrected") · 5 segment (ring Run; **result not visible**, it is in the viewer) · 6 gate
(plot + gate, "qc 404, 2.5 %") · 7 track (MSD diagnostics with its verdict line) · 8 behaviour (speed B vs T)
· 9 cluster tracks (cluster map + HMM states) · 10 analysis board (box plots with p-values). Static: name
channels (script types the old tutorial's names and Assigns; table shows them) · gate (script draws a box
on LCMV-AF594 vs P14-CTDR and names it P14 → "P14 1281, 2.6 %"). Phenotype was dropped (its summary of
the qc pops is proportion 1.0, says nothing).

**Rules this pass established.**
- One fresh browser context per shot. In one long session, state leaks between pages (a floating Viewer
  panel restored on every page, empty Gate/Track plots); fresh contexts fixed every one.
- Land on an image with `project.focusImages(scope, set, [image])`; `scope` is the page's `ModuleLayout`
  `module` prop.
- A shot may run **user actions** (click, fill, drag, hover, Enter) before the capture. A shot that needs a
  result the data lacks makes it the way a user would (channel names, a gate) — on the isolated copy only.
  Shots that build on each other run in path order on one copy.
- Settle on the page's own loading state; don't auto-close dialogs after a step (it closed the popup the
  step opened); park the mouse unless the shot is a hover; hide `.console-bar .bar-last`.
- Ring the `data-guide` anchor the guide step already names (`task.run`, `task.fun`, `gate.drawTool`,
  `metadata.channels`, `manageImages.addImages`, `images.qcDot`, `board.export`) — one id for guide and shot.

**Gaps (after the first pass).** The viewer (segment labels, gate overlay, tracks): browser capture blocked
(T3); the offline still has no overlays (the thumbnail route does not pass `overlays_config`). Saved panel layouts overlap on Behaviour and Cluster tracks. No
crops yet: every shot is the full window.

### Second pass (2026-11-13, rebased onto current `origin/main`, with the new viewer layout)

- **Light theme** for every shot (maintainer's call for the guides), set through `settings.colourScheme`.
- **Plot pages get a second, maximised shot**: `settings.plotsMaximised = true`, then the canvas's own Tile
  columns → 3, then drag the floating Populations panel into the empty grid cell (tiling ignores it; there
  is no dock on module pages). Fixes the squished cluster map / overlapping saved layouts. Set the flag
  explicitly on EVERY shot — it persists in the profile.
- **The viewer IS capturable in a real window.** T3's device loss is not headless-specific: a headed
  Chromium on Wayland loses the device the same way. What works is ANGLE on Vulkan:
  `--ozone-platform=x11 --use-angle=vulkan --enable-features=Vulkan,VulkanFromANGLE,DefaultANGLEVulkan`
  (headed, X11 via Xwayland so `--window-position` can put it on a second screen). Headless with those
  flags gets no adapter. So a viewer shot needs a display — fine on a workstation, not in CI.
- **Viewer shot recipe**: pause playback; set `input[aria-label="z plane"]` to a plane with cells (a
  drift-corrected store's edge planes are mostly empty — unwritten zero chunks — so the default plane 4
  rendered black); Reset view; Reset + Auto contrast only after `networkidle` (Auto on not-yet-loaded zeros
  sets 0–1 and the image saturates white — retry until the canvas PNG isn't near-uniform);
  `settings.viewerLabelContour = 2` (2 px outlines); open Segmentation.
- Review set: `~/Downloads/TMP/guide-screenshots-trials/tutorial-v2/` (17 images, scripts alongside).

### After the second pass — maintainer review (2026-11-13)

"Good otherwise", three asks:

1. **The "do" highlight is not visible enough.** The CSS ring in `--cc-accent` disappears among the app's
   own purple. Proposal (mock: `~/Downloads/TMP/guide-screenshots-trials/pointer-options.png`): draw the
   pointer AFTER capture, from the target's rect recorded by the runner — never CSS in the app. That way
   the look can change without recapturing, and the shot stays the real UI. Options mocked:
   **A** spotlight (dim the rest, thick ring in a colour the app doesn't use, numbered badge),
   **B** arrow + label, **C** zoomed crop of A with a where-on-screen thumbnail. Lean: A for the full
   window, C when the control is small; several numbered badges for a multi-step shot.
2. **Presentation / layout** needs work to be attractive: a card per step (number, Do, You'll see, the
   shot), crops instead of full windows where the point is one panel, a consistent frame.
3. **Stories to cover** — one tutorial per acquisition type, each on its own project:
   | Story | Candidate data | Shape |
   |---|---|---|
   | Intravital (2P timelapse) | `4kS67f` (B/T, spleen) — or `XbeT2l` (Immunemap, public data) | import → channels → cleanup/drift → segment → viewer check → gate → track → behaviour → cluster tracks → board |
   | Confocal (static) | `4rNbMp` / FtGoJO (the old tutorial's sample) | import → channels → AF → segment → gate (P14) → phenotype → spatial |
   | PhenoCycler (multiplex) | `mjphFQ` (cycle registration) | import → cycle registration → channels (many markers) → segment → gate / cluster cells → spatial regions |
   Open: which data may appear in public docs (Immunemap is public; the others are lab data).

### Booklet direction (maintainer, 2026-11-13)

- **A booklet to hand people**, one per story (intravital, confocal, PhenoCycler). **Lab data is fine.**
  **The tutorials themselves are not committed yet** — the content settles first; only this plan is.
- **Crop to what needs attention**, not the full window every time: full windows all look ~80% the same.
  The runner records the rects of the pointer target and of `doCrop` / `seeCrop` anchors (`data-guide`
  ids, `.panel@@n`, `.canvas-side-panel`); a compose step crops their union. Every crop prints at ONE
  scale (0.19 mm per CSS px, capped at the text width) so the app's text is the same size on every page.
- **Pointer = option B (arrow + label), in the app's guide style, not red and not `--cc-accent`**: the
  in-app guide bubble's look (`GuideBubble.vue`: always `cc-dark`, `--cc-surface-1` fill), a dark ring
  with a light halo, a dark arrow. Drawn after capture, never CSS in the app.
  Relation to the in-app pointer: `PointerBubble.vue` (`pushUiMark`, `stores/viewer.ts`) already points at a
  `data-guide` anchor live, through `utils/guideAnchor.ts::resolveAnchor`. The booklet pointer is a drawn copy of
  that job on purpose (restyle without recapturing); when this becomes pipeline code, share the anchor
  resolution and the look's tokens with it rather than a second definition.
- Draft: `~/Downloads/TMP/guide-screenshots-trials/booklet-v1/` (`intravital.pdf`, `confocal.pdf`,
  scripts alongside).

### Draft booklets (2026-11-13)

Three A4 drafts in `~/Downloads/TMP/guide-screenshots-trials/booklet-v1/` — not committed (maintainer: content
settles first):
- **Intravital** (`4kS67f`): 11 steps, import → … → Analysis board.
- **Confocal** (`4rNbMp` / FtGoJO): import → channels → segment → outlines in the viewer at 100 µm → gate P14.
  "P14 on the image" is waiting on the viewer-panel question below.
- **PhenoCycler** (`mjphFQ`, for microscope users — no analysis yet): bring in each cycle → Register on DAPI →
  channel grid by cycle → whole slide in the viewer → alignment check (the three cycles' DAPI at 100 µm,
  white where they agree) → markers from different cycles.

What these drafts settled:
- **Viewer pointers need recorded rects, not hand-typed ones** (hand-typed ones were a toggle-width off). The headed
  viewer script records each named toggle's rect next to its screenshot, as the page runner does.
- **Viewer zoom = wheel steps toward a point**, about ×1.1 per step. "Scale bar reads 100 µm" is the target the
  maintainer picked for tissue close-ups.
- **Showing a population in the pop-out viewer takes the main window's Viewer panel** (population points on +
  the population's "show in viewer" icon); the viewer's own Populations section only says "enable in the viewer
  panel". Open question to the maintainer: should that panel's drawing controls live in the viewer? A
  read-only proposal was fanned out.

**App findings from the shots** (not fixed here): Track page throws `Cannot read properties of null
(reading 'replaceChildren')` (intermittent); the QC flag tooltip repeats findings ("Some cells not in the
graph" ×2, "Ran at resolution 0.5" ×2), shows the raw id `qcProbe` as a label, and says "Only 0 cells
measured" on an image with 404 gated cells. **Registered PhenoCycler channel grid shows cycle 1's
names in every cycle row**: the registered image's `meta.acquisition.channels` restart `order` at 1 per cycle,
and `acquisitionChannelTable.ts` `chName` looks up `names[order - 1]` in the stacked 11-name list (seen on
`mjphFQ`/bPkrUs). **FtGoJO's `cells` labels contain a 7×7 block of perfectly square labels** (top-left of plane
13), which looks like a tiling artefact.

## Phases (after the trials)

- **P1** manifest mode in `gui_capture.mjs`: crop, highlight, scale, scheme, masks. Needs eyes on output.
- **P2** guide link: `GuideStep.shot`, served images, missing-image handling.
- **P3** freshness check: a guide references a missing shot, or a manifest selector no longer matches.
  Pixel thresholds only if T1 shows runs are stable.
- **P4** the two tutorial paths (static, live) with 6–8 shots each, and the old tutorial map.
- **P5** slide composer + first slide ("gate cells and see the populations in the viewer"). Needs eyes.
- **Docs site** waits on decision D3.

## Decisions needed from the maintainer

- **D1** Which projects are the capture fixtures for the trials: RFtn9f (T cells) and 4kS67f (3P spleen),
  as in the clarity audit? Later: a public, versioned sample dataset per path (Immunemap is a candidate).
- **D2** Who starts the isolated instance for T1–T4 (agents don't start servers by default).
- **D3** Where the docs site source lives, and whether it is rebuilt from the same shots.
- **D4** Light scheme for projectors: needs an on-screen review and a decision on the stage/identity hues
  that fail CVD separation.
