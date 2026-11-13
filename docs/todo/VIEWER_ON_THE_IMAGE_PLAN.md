# Viewer — the main window's Viewer panel moves into the viewer

Status: **in progress** (2026-11-13) — S0 #2206, S1 #2207, S2 #2208, S3 #2209; auto-update handlers to app level on `feat/viewer-auto-update-app-level`. Supersedes
[`VIEWER_CONTROLS_SPLIT_PLAN.md`](VIEWER_CONTROLS_SPLIT_PLAN.md) locked decisions 1 and 3 ("ViewerWindow
does NOT host ViewerPanel", "THE VIEWER HAS NO SELECTORS"). Its invariant that **no source of truth lives
in the viewer** still holds: every control keeps writing the same settings bags.

## Goal

Everything that decides what the viewer draws is set in the viewer. Today you open the viewer, then go
back to the main window's floating Viewer panel to turn on populations, ribbons or a segmentation —
the viewer's own sections say "enable in the viewer panel". Found while scripting the confocal tutorial
(`GUIDE_SCREENSHOTS_PLAN.md`): ticking P14 in the population manager drew nothing, because the
Gated population type was off in the other window.

When this is done the main window has no Viewer panel. It keeps the launcher (open or raise the viewer)
and the population manager's per-population 👁.

## Decisions (2026-11-13, Dominik)

1. **Move everything from the Viewer panel; delete the panel.** Simplify by moving, never cutting
   (`GUI_LAYOUT_PLAN` Decision 1): every control gets a home below, or is deleted because it already
   exists elsewhere.
2. **What is shown vs how it is shown — never mixed.**
   - **Left overlay, "On the image" = what is shown.** Every control picks something from the project:
     a segmentation, a population type, a ribbon family, a column to colour by, a highlight to clear.
     No sliders, no colours, no sizes.
   - **Right sidebar = how it is shown.** The layer list (the napari model of the old plan): channels,
     mask, populations, tracks, annotations — eyes, opacity, outline, size, tail, colour. No pickers.
   - Test for a new control: if it lists names from the project it goes left; if it acts on a row the
     right sidebar already shows, it goes right. A control that seems to need both is two controls.
3. **The left overlay floats on the image and folds, like the play bar.** Folded it is a pill naming
   what is on ("On the image · cells · Gated (2)"). Fold state per profile, as `cc.vw.playbackBar`.
   It does not squeeze the right sidebar.
4. **Duplicates are deleted, not moved:** Screenshot (= Share → Save image), 3D (= the viewer's 2D/3D
   switch, same per-set key), Auto-update (already in Preferences; its task handlers move to app level,
   which also makes the preference work with the panel closed).
5. **The population manager's 👁 is enough on its own.** Turning a population on also turns its
   population type on in the viewer (S0).

6. **Movie is a quick movie in the viewer** (2026-11-13): Share → Record movie… holds the panel's form
   (`components/QuickMovie.vue`: record the view, ROIs or a version comparison, or save a figure). It
   says "just a quick movie here" — the Movies page stays for batches and remakes, not a detour.
7. **What a click does is one menu button** (Move / Select / Edit), not three chips.

## Where each control goes

| Viewer panel today (`ViewerPanel.vue`) | New home |
|---|---|
| Image name, open viewer | main window launcher (stays) |
| Image version picker + note | viewer header chip ("Image: default · Labels: cells") becomes the picker |
| Segmentation rows: labels / tracks / branches, label files, fold, mismatch warnings, live preview | left overlay → Segmentation |
| Population-type switches (Gated, Clusters, Regions) | left overlay → Populations |
| Cell-track ribbons, track-cluster ribbons | left overlay → Tracks |
| Clear highlighted tracks / cells | left overlay, last row |
| Colour by + recolour legend + Reset | left overlay → Colour by (the column is a pick); the swatch recolour sits in the right Populations legend (a look) |
| Save contrast / T / Z as you change; re-read pixels from disk | viewer ⚙ |
| Auto-update, 3D, Screenshot | deleted (Decision 4) |
| Movie | viewer Share → Record movie… (`QuickMovie`, Decision 6) |

The right sidebar's empty states ("No populations shown — enable in the viewer panel", "No mask shown
— tick one in the viewer panel", "No tracks shown — tick a segmentation in the viewer panel") point at
the left overlay instead.

## Cross-window state

Writes stay in the localStorage settings bags (`settings.ts` storage listener, `viewerBagChannel.ts`),
so movies and stills keep matching the viewer (`drawSpec.ts` `RECORDED` is unchanged).
- `storage` events do not fire in the window that wrote them. Controls that move INTO the viewer need
  watches on their own writes (pop types, gated tracks, label/track visibility → `loadOverlays` /
  `loadTracks`). Today only colour-by and version have them.
- Most `pingViewerOverlays` calls go away with the panel.
- The main window's auto-show (`useOverlayAutoShow.ts`) writes the same bags on image open; check it
  cannot race the viewer's own writes.
- ~~Live preview state is a main-window ref~~ — moved: the viewer window owns it (`useLivePreview.ts`, its own WS), the one writer of `liveLabels`.

## Slices

| Slice | What | Notes |
|---|---|---|
| S0 | Population manager 👁 on → its population type on (`setPopVisible`, track clusters → `trackclust`) | ~10 lines + test. Fixes the tutorial trip on its own |
| S1 | Left overlay shell (fold, pill, persisted) + population-type and ribbon switches + self-write watches | right-sidebar empty states re-pointed |
| S2 | Segmentation rows in the overlay: mask (one at a time), tracks, label-file chips, mismatch warnings, fold to rows in use (`utils/viewer/segRows.ts`, shared with the panel) | **Left in the panel for now:** branch toggles (the WebGPU viewer draws no skeletons — `specSource.branchNames` is empty) (live preview moved in S4: ⚡ rows in the overlay). Guide anchors `viewer.toggleLabels` / `viewer.toggleTracks` stay on the panel until S4 deletes it (`taskGuides.ts`, MCP examples `guidance.py`, `server.py`) |
| S3 | Colour by (column picker in the overlay), version picker in the header chip (active version marked), clear-highlight rows, ⚙ Remember + Re-read from disk | **Recolour legend dropped** (Dominik, 2026-11-13): the panel's swatch legend is dead — `colourLegend` (`useOverlayAutoShow.ts`) is never filled, and the live viewer sends no colour overrides (`overlaysUrl` takes `colourBy` only). It goes with the panel in S4; the colour-by column picker stays |
| S4 | ~~Movie~~ (done: `QuickMovie` in Share); ~~auto-update handlers to app level~~ (done: `useViewerAutoUpdate`); ~~live preview~~ (done: `useLivePreview` in the viewer, ⚡ rows in the overlay); delete `ViewerPanel.vue` and the floating panel; header launcher opens/raises the viewer | `guides.test.ts`, `dataRefreshCoverage.test.ts` pin the panel |

Every slice that changes what the viewer draws needs Dominik's eyes on a real GPU before merge
(headless Chromium cannot draw the WebGPU viewer).

## Risks

- Guides cannot point into the pop-out today (no `data-guide` anchors found there; not tested).
- A movie started from the pop-out runs `taskStore.add` in the pop-out's store; the main window's task
  list may not show it (unchecked).
- Population types `track` and `live` have no viewer master switch of their own; S0 leaves them as
  they are.
