> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/GUI_CLARITY_PLAN.md`](../todo/GUI_CLARITY_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-11, `origin/main` aea92b67f). The maintainer asked
> for final layout mocks of the main window and the viewer first, built against together. Read
> against main, two premises had moved:
>
> 1. **The check accent is plum, not coral.** GUI_CLARITY_PLAN Decision 15 gave the checks cluster
>    a plum frame and label (plum = "act on this", shared with the Next button).
> 2. **`JOURNEY_WORKFLOW_IDEA.md` is archived.** Its review is
>    [`docs/todo/JOURNEY_WORKFLOW_REVIEW.md`](../todo/JOURNEY_WORKFLOW_REVIEW.md).

# VIEWER_CLARITY_AUDIT_PROMPT

**Model:** Opus. **Mode:** audit only. No code changes, no new features.
**Output:** `docs/todo/VIEWER_CLARITY_AUDIT.md` (+ screenshots in `docs/todo/viewer-audit/`).

## Why

The main GUI has been audited and revised toward a light, structured, clear surface
(`GUI_CLARITY_PLAN.md`, `GUI_CLARITY_AUDIT.md`, `JOURNEY_WORKFLOW_IDEA.md`). The viewer is the other half of the product:
it is where users see what every module did to the image. It is now a pop-out window only (never docked),
so it has to stand on its own and still feel like the same app.

Apply the same principles here: **light and loose on the surface, firm skeleton underneath, never lost,
nothing important buried, playful colour with a job.** Same two failure modes, translated:

1. A user asks "what am I looking at, and what do I do now?" in the viewer.
2. A user misses something important (a check, a QC flag, a layer that is hiding) because it is buried in the viewer.

## Read first

`CLAUDE.md`, `INVENTORY.md`, `WEB_VIEWER_PLAN.md`, `GUI_CLARITY_PLAN.md`, `GUI_CLARITY_AUDIT.md`,
`docs/todo/FEATURE_MATURITY_PLAN.md`, the AI surfaces audit and the capture-surface docs (if present).
Reference their findings; do not repeat them. The main-window audit already notes the viewer window has 7 entry points.

## The image is the hero

The viewer's job is the image. Treat chrome as something that recedes: calm, quiet controls around a neutral image area,
revealed on hover or focus where that is safe. UI colour must never be confused with channel colours, overlay colours or data colours.
Channel pseudo-colours and label colours are sacred; the palette lives only in the chrome.

## Method (all phases; check in after Phase 3)

### Phase 1: Viewer surface census (measured)

Run the viewer. For every control, panel, toolbar, overlay, mode, shortcut and gesture record:

- location and how it is reached (and which of the 7 entry points lead here)
- control count visible on first paint vs. after expansion
- state it owns (channels, LUTs, contrast, z/t, 3D, layers, labels, tracks, populations, ROIs, annotation tools, captures, export)
- maturity tier and whether the badge shows (label and track correction are early alpha)
- AI-related elements (feeds the global AI toggle)
- screenshot at laptop size and at a large display

One table. Rank by control count and by how often the control is likely used (use telemetry or logs if they exist; otherwise "unknown").

### Phase 2: Journeys from inside the viewer ("what do I do now?")

Walk each as a first-time user, using only what the window shows:

1. Open the viewer from a module and orient: which image, which set, which version, what is overlaid, and where did it come from?
2. Check a segmentation overlay and judge it (toggle layers, compare with raw, spot flagged cells)
3. Inspect a gated population (selection round trip with the plot in the main window)
4. Inspect and correct tracks or labels
5. Take a capture for a figure, the board, or Kiwi
6. Export a movie
7. Ask Kiwi about what is on screen
8. Close and reopen: is state restored?

Per step: click path, whether the next action is signposted, dead ends, places where the user must already know the answer,
and whether they can get back to the task page that sent them. Score PASS / WEAK / FAIL on the 5-second test.

### Phase 3: The pairing with the main window

The two windows must feel like one app.

- **Provenance:** does the viewer say what is shown and which module pushed it? ("Segment, Cellpose-SAM, Image 12")
- **Acknowledgement and handshake:** how does the main page know the viewer received the push, and what happens when the viewer is closed, busy or on another screen?
- **Viewer chip states:** what must the chip show (closed, open, loading, image mismatch, error)?
- **Division of labour:** what belongs in the viewer, what in the main window (plots, tables, parameters)? Any duplicated controls?
- **Return path:** one clear way from the viewer back to the task, recipe step or `#next`.
- **Sync:** selection, time and z, population highlights, gate changes, layer visibility. Where it is bidirectional, is that obvious?
- **Windowing:** size and position memory, bring-to-front, single monitor, pop-up blocking, Tauri shell, remote or cloud sessions.

**Check-in:** summarise Phases 1 to 3 in 20 lines or fewer before continuing.

### Phase 4: Buried-in-the-viewer audit

List everything that protects interpretation or prevents silent error and where it lives in the viewer today:

- QC and check flags (for example flagged cells or tracks highlighted on the image), acquisition or calibration warnings, scale and unit correctness
- hidden or dimmed layers, display-range and gamma changes that alter appearance, downsampled or partially loaded data
- stale overlays (overlay from an older version than the image shown), unsaved edits, experimental or alpha tool state
- performance states (loading, decoding, GPU memory limits) so users never think it froze

For each: visible at the moment it matters? Would a user who never opens that panel be exposed to it? Propose where it surfaces
(a quiet status row, an on-image badge, a banner for validity-breaking cases). Advisory, never blocking, unless an edit could destroy data.

### Phase 5: Density and cut candidates

With evidence (paths, counts):

- always-visible controls that should be progressive disclosure
- duplicate entry points to the same action
- overlay and layer management that scales badly (many layers, many populations)
- mode confusion (view vs. edit vs. annotate): can the user always tell which mode they are in?
- hand-rolled components that diverge from the shared ones in `INVENTORY.md`
- surfaces that are Coming soon or Experimental taking prime space
- AI elements that should vanish under the global AI toggle

Every cut or merge candidate needs "who would miss it and what replaces it".

### Phase 6: Lightness, consistency and colour

- Lightness pass: spacing scale, panel chrome, borders vs. whitespace, nested boxes, type variety, always-visible vs. on-hover, helper text volume.
- Shared tokens with the main window; what the viewer must not inherit (anything that tints or competes with the image).
- Colour roles in the chrome: stage hues, check accent (the same coral as the main app), maturity tiers, selection and focus, empty and loading states.
- Separate three colour systems and propose rules so they never collide: UI chrome, overlay/label/population colours, channel pseudo-colours.
- Accessibility: contrast on the dark chrome, status never by colour alone, colour-vision-safe overlay defaults.
- Terminology: same thing, same word as the main window.

### Phase 7: Proposal

Propose a skeleton, not a redesign. Describe it as a text wireframe and evaluate at least:

- a recessive chrome frame: image fills the window; a slim top row (image, set, provenance, back to task), a layers rail, a playback row (t, z), and an inspector that appears on demand
- an always-visible **status row** (checks, layer or display warnings, load state) in the viewer's fixed spot, matching the main app's checks cluster
- explicit **modes** (view, edit) with an unmistakable mode indicator and maturity badge on alpha tools
- layers as a short list with grouping, not a long panel
- capture and export as one "Share" area
- consistent keyboard shortcuts, discoverable from one place

State trade-offs, WebGPU performance constraints (data transfer is the known bottleneck), and what each costs in the current architecture.

## Output format

1. **Top 10 findings** ranked by user impact: finding, evidence, recommendation (cut / merge / move / surface / rename / hide), effort S/M/L.
2. **Census table** (Phase 1) and **journey table** (Phase 2) with PASS/WEAK/FAIL.
3. **Pairing table** (Phase 3): handshake, provenance, return path, sync, windowing, each with gap and fix.
4. **Buried-in-the-viewer table** (Phase 4).
5. **Cut/merge list** (Phase 5) with "who would miss it".
6. **Consistency and colour rules** (Phase 6), grouped by fix-once-centrally vs. per-surface.
7. **Proposed skeleton** (Phase 7) as a text wireframe, with the smallest first slice.
8. **Sliced PR plan:** independent, ordered, small enough to review by eye; mark which need eyes on the real viewer before merge.
9. **Decisions needed from D:** only questions that change the plan, each with your recommended answer.

## Rules

- Evidence over opinion: counts, paths, screenshots. Label anything inferred.
- Existing design is not gospel. Challenge it.
- Never recommend a change that alters how image data is displayed (colours, scaling, interpolation) without flagging it as a scientific-validity decision.
- Cecelia works fully without Claude; judge AI surfaces as an optional layer.
- Reuse canonical shared components and tokens; no new primitive without showing none fits.
- If you cannot run the viewer or capture screenshots (for example no GPU), say so at the top and mark every finding that depends on it.
