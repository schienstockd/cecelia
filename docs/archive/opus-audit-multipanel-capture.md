# Audit prompt: multi-panel capture + cross-referenced discuss/refine

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context

PR #1063 (bidir) built the capture → discuss → refine loop for the viewer:
DrawSurface annotates a frame, `composeFrameWithOverlay` bakes marks into
the PNG, capture posts with `previousCaptureId` chaining back to its
source, Kiwi's captures list surfaces thumbnails/refocus/refined-from.

Module pages need the same *mechanism* but a different *shape*: several
plots open as floating panels, annotated and cross-referenced against
each other, discussed with Claude, then refined.

This is not a copy of the viewer flow. Audit the plan below before any
implementation — panels are ephemeral UI state, but marks must outlive
them.

## Decisions already made (don't re-litigate without cause)

1. **Capture is a panel set, not a single plot.**
   ```
   capture = {
     panels: [{ panelId, plotRef, dataSlice, position }, ...],
     marks: [{ panelId, dataRef, xy_snapshot, linkedTo: [dataRef, ...] }]
   }
   ```
2. **Cross-panel reference (`linkedTo`) is keyed by stable data identity**
   (e.g. `{datasetId, rowKey}`), never `panelId`. Panels can be
   rearranged, closed, or not open at all — the link must still resolve
   later.
3. **Each mark keeps both:**
   - `dataRef` — stable identity, used to re-resolve/relocate the point
     on reopen, in a different layout, or in a later session.
   - `xy_snapshot` — the data-space position *at capture time*, needed
     to faithfully re-paint that specific historical capture even if
     current scale/layout has since changed.
4. **Send-time payload to Claude is dual:** a flattened PNG (whole panel
   layout composited, marks drawn per-panel, cross-panel links shown as
   connectors/legend) *and* the structured mark list with exact
   `dataRef`s — not pixels alone.
5. **`previousCaptureId` chaining is reused unchanged** — a re-annotate
   on a multi-panel capture carries the full panel set forward,
   additive-only, same as the viewer loop.
6. **Refocus is redefined**: not "seek viewer to t/z" but "restore the
   panel layout + highlight the panels/points referenced by this
   capture's marks," resolved via data identity, not saved panelIds.

## Open questions for the audit

- **Rendering base**: rasterize from each plot's SVG/Vega spec via the
  existing `loadImg`/`toDataURL` path (reuses `composeFrameWithOverlay`
  as-is), vs. a new compositor for arbitrary panel grid layouts. Confirm
  the grid/positions in `panels[].position` map cleanly onto a single
  canvas without a new layout engine.
- **Annotation UI**: freehand draw (current DrawSurface) doesn't map to
  "point at this datum." Does cross-referencing need a click-to-select
  data point interaction distinct from DrawSurface, or can DrawSurface
  marks be snapped to nearest data point post-hoc?
- **Resolution failure**: what renders when a `dataRef` no longer
  resolves (dataset changed, row deleted, module re-run with new data)?
  Decide the degraded-but-non-broken display before building it, not
  after.
- **Cross-panel connector rendering**: is a visible line/legend between
  linked marks required at send-time, or is the structured `linkedTo`
  list sufficient for Claude and the connector is only an in-app
  affordance?
- **Storage cost**: multi-panel captures are heavier (N plot renders +
  mark graph) than a single viewer frame. Any cap needed beyond the
  existing captures-list cap, given #1061 already added delete/clear?
- **BroadcastChannel scope**: viewer refocus is same-origin single
  target. Module-page refocus may need to address multiple panels at
  once (restore N panels, highlight M of them) — confirm the channel
  message shape supports a batch, not just one seek.

## Ask

Produce an implementation plan (endpoints, payload shapes, storage
migration if any, UI changes to DrawSurface/Kiwi) that answers the open
questions above, reuses everything reusable from #1063 explicitly named,
and flags anything that requires a new primitive rather than an
extension of the existing capture discipline.
