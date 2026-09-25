**Status:** planning (2026-09-25) — branch `feat/panel-primitive`.

## Goal

One canonical **8-handle resize** primitive used by every free-floating panel — both the top-level
viewport windows (`components/FloatingPanel.vue`: Viewer, Lab log, Kiwi, ParamFigure) and the canvas
plot panels (`components/canvas/CanvasPanel.vue`, `components/canvas/CanvasSidePanel.vue`) — so what
PR #1207 shipped for the top-level windows (resize from any edge or corner) is what module-page plot
panels get too. Today the canvas panels use CSS `resize: both`, which is a browser affordance and is
always **SE-corner only**.

## What is NOT the goal

**The two Vue wrappers stay split.** Merging `FloatingPanel.vue` with
`CanvasPanel.vue`/`CanvasSidePanel.vue` is explicitly not the plan — see
`docs/inventory/FRONTEND.md:44` (*Dialog/panel shells*): they are deliberately separate because they
live in different coordinate systems (viewport-fixed vs zoomable-canvas), have different event models
(pointer / zoom-compensated mouse) and different features (localStorage + stacking vs tile/cascade
arrange). Putting a canvas-scoped manager in the top-level system makes it compete with the Viewer and
Lab log for the same corner.

**The shared primitive is a lower-level composable that both wrappers use for the resize gesture,
not a shared wrapper component.**

## Locked decisions

1. **The primitive is `composables/useResizeHandles.ts`** — a Vue composable that installs the
   pointer-move loop, computes the new rect via `utils/panelResize.ts` (already extracted, tested),
   and writes it back via a caller-supplied setter. Not a component: the CSS for the 8 handles is
   scoped per host, and both hosts render the same 8-div template inline.
2. **`utils/panelResize.ts` stays the maths.** It is already pure, unit-tested, and handles anchor
   arithmetic for the W/N edges. The composable adds the pointer loop + zoom compensation on top;
   the arithmetic is unchanged.
3. **Zoom compensation is opt-in via `zoom?: () => number`** — matches the existing
   `useFloatingPanel(zoom)` contract. Top-level panels omit it (zoom = 1). Canvas panels pass
   `useInjectedZoom()` so a pointer delta divides by the current canvas visual scale.
4. **Size ownership stays where it is.** `FloatingPanel.vue` keeps its reactive `st.w/h` (persisted
   via localStorage). `CanvasPanel.vue` keeps writing width/height to `root.style.width/height`
   imperatively (its `ResizeObserver`-driven persist path continues to fire on every resize,
   preserving the current behaviour where size lives in `stores/canvasPanels`). The primitive is
   agnostic — the caller passes `getRect` and `setRect`.
5. **CSS `resize: both` is REMOVED** from `.panel` in `CanvasPanel.vue` and from
   `.canvas-side-panel` in `CanvasSidePanel.vue`. The 8-handle template replaces it. Collapsed /
   docked variants still disable both (`resize: none` becomes just an absence of handles because we
   render them under `v-if="!collapsed && !docked"`).
6. **Handle-template + CSS live per host.** Copied twice (`FloatingPanel.vue`, canvas panels). The
   template is 8 short lines; extracting it to a shared component would need a positioned root, and
   both hosts already position their own root differently. The maths + the pointer loop are the
   shared part, and they are what actually drift.
7. **Out of scope for this plan:** `composables/usePanelResize.ts` (single-edge horizontal side-panel
   drag — used by `CollapsiblePanel`, `TaskRunner`, `MetadataPanel`) and `composables/useColumnResize.ts`
   (per-column table resize). Different gesture (one edge, not eight), different persistence, different
   consumers. If a future audit finds them worth folding in, that's a separate plan.

## Phases

### Phase 1 — the primitive + top-level parity (SHIPPABLE)

New file `frontend/src/composables/useResizeHandles.ts` — installs `pointermove`/`pointerup`, tracks
`startRect + startPointer`, calls `resizeRect(...)` from `utils/panelResize.ts` on every move, writes
back via caller's setter. Signature:

```ts
export function useResizeHandles(opts: {
  getRect: () => Rect
  setRect: (r: Rect) => void
  bounds: () => PanelBounds          // clamp floor for N/W edges (viewport-fixed OR offsetParent)
  viewportSize?: () => { w: number; h: number }   // clamp ceiling for E/S; defaults to window
  zoom?: () => number                              // divide screen delta by this; default 1
  min?: { w: number; h: number }
  onActivate?: () => void
}): { onResizeDown: (e: PointerEvent, edges: Edges) => void }
```

Refactor `FloatingPanel.vue` to use it: remove its inline `mode === 'resize'` branch of `onMove` and
its `resizeEdges/startRect/startPointerX/Y` locals; keep the drag branch. Behavior unchanged; PR #1207
tests keep passing. Tests: extract the pointer-loop harness into a small utility if needed for
`test-frontend` (Vitest is `src/utils/*.ts` only — see `frontend/CLAUDE.md`). The composable calls only
pure functions + DOM listeners, so the DOM half stays unmounted and the pure half is `panelResize.ts`
which is already tested. If nothing new to assert, no new `.test.ts` (the composable is glue).

### Phase 2 — adopt on canvas plot panels (SHIPPABLE, the user-visible change)

`CanvasPanel.vue`:
- Import `useResizeHandles`. `getRect` reads `pos.value + root.offsetWidth/Height`; `setRect` writes
  `pos.value = {x,y}` and `root.style.width/height = w/h + 'px'` (so the existing `ResizeObserver` +
  `persist()` continues to fire).
- Add the 8-handle template inside `.panel`, guarded by `v-if="!docked && !collapsed"` (matches the
  existing `resize: both` gating). CSS copied from `FloatingPanel.vue` with the `.panel-` prefix.
- Remove `resize: both` from `.panel` (line 212), keep `min-width/min-height`.
- Pass `zoom: injectedZoom` (already present via `useInjectedZoom()`).
- Bounds function returns the panel's `offsetParent.clientWidth/Height` (canvas), matching what
  `useFloatingPanel.clamp` uses today — factor a small helper if the two ever drift.

`CanvasSidePanel.vue`: same recipe. `resize: both` on line 126 goes away, 8-handle template added, no
zoom (it sits at the top-right of the canvas above the plots' transform), same `getRect/setRect`.

Verify: gating pages (`.gate*/`), Analysis board, PopulationManager, SeriesPicker, FlowModelVault,
BatchMovies — every canvas host must still resize (now from any edge) and Tile/Cascade must still
work.

### Phase 3 (optional) — retire `useFloatingPanel.ts` misnomer

Since `useFloatingPanel` is now only the **drag** half for canvas panels (resize moved out), rename it
to `useCanvasPanelDrag` to match its actual job — the inventory line at
`docs/inventory/FRONTEND.md:44` already flags the name as a source of confusion (a fresh session sees
"useFloatingPanel" and assumes it drives `FloatingPanel.vue`). Rename + update the four consumers
(`CanvasPanel`, `CanvasSidePanel`, `PopulationManager`, `CellCardsView`). Small mechanical change;
independently shippable.

## Test / verification

- `pixi run test-frontend` — must stay green (Vitest, `src/utils/*.ts` only).
- Manual check on a running dev server:
  - Kiwi (FloatingPanel) — resize N/S/E/W/corners, unchanged.
  - Gating page plot (CanvasPanel :square) — every edge resizes; the square-height enforcement still
    fires after a resize.
  - PopulationManager (CanvasSidePanel) — every edge resizes; collapsed state hides handles.
  - Analysis board Tile / Cascade — panels still adopt the arrange cell.
- Repo-wide grep: `resize:\s*both` should return only cases outside the panel family (not `.panel`,
  not `.canvas-side-panel`).

## References

- PR #1207 (`feat/fp-resize-edges`) — the top-level 8-handle shipped 2026-09-24.
- `frontend/src/utils/panelResize.ts` (+ `.test.ts`) — the maths this plan reuses.
- `frontend/src/utils/panelBounds.ts` — the top-clamp used for viewport-fixed panels; canvas panels
  compute their own from `offsetParent`.
- `docs/inventory/FRONTEND.md:44` — the "deliberately separate" line the plan honours.
- `docs/inventory/FRONTEND.md:46-47` — the sibling resize primitives (`usePanelResize`,
  `useColumnResize`) that are OUT of scope here.
- `docs/UI.md` → *Two-half side panels* / *Collapsible side panels* — related composables.
