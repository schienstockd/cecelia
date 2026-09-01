/**
 * The shared VOCABULARY for scene-style visual aids — a small schematic frame that shows dots,
 * curves and corner overlays, so a control panel can say "here's what this config will produce"
 * without generating the real artefact.
 *
 * A SIBLING of `VisualAid` (`components/VisualAid.vue` + `tasks/paramVis.ts`), which draws numbers
 * as shapes in a labelled grid. That grid answers "are these numbers different from those numbers".
 * A scene aid answers a different question — "what would this look like" — and needs a picture, not
 * a table of shapes.
 *
 * **Same discipline as `paramVis`/`smoothVis`.** The producer is where every rule lives, so a test
 * can pin the picture without mounting the component. The component owns only the SVG. When a batch
 * movie decides that `showTracks && showPops` favours pops (PR #751), the OVERLAY PRODUCER moves —
 * the component doesn't.
 *
 * **Coordinates are normalised 0..1.** The renderer scales to whatever pixel size the panel gives
 * it, so producers don't need to know the on-screen box. A ribbon step at `x: 0.5` sits at the
 * middle of the frame regardless of size.
 *
 * **Not for legends.** These schematics carry generic colours (see the producer's own palette) —
 * they show STRUCTURE, not identities. A user comparing "what will draw" doesn't need the exact hex
 * of the pop they picked; they need to see that four pops will draw in four distinguishable
 * colours. Piping the real palette in would make the preview a second legend to keep in step.
 */

/** One dot in the schematic. `ringed` draws a thin outline (a mask-outline hint) alongside the
 *  filled dot. `mode: 'ring-only'` draws just the ring — for a mask-outline-only view (no points),
 *  or a pop hint before points are on. */
export interface SceneAidPoint {
  x: number         // 0..1
  y: number         // 0..1
  colour: string    // any CSS colour
  ringed?: boolean
  mode?: 'ring-only'
}

/** A short polyline — a "ribbon" or track tail in the schematic. Two or more points; the renderer
 *  draws them with `stroke-linejoin: round`, so a smooth curve emerges without a bezier. */
export interface SceneAidRibbon {
  points: Array<{ x: number; y: number }>
  colour: string
}

/** What sits in the corners of the frame — timestamp top-left, scale bar bottom-right, an optional
 *  "TITLE CARD" chip above the frame (its own row, so it doesn't overlap the schematic). */
export interface SceneAidCorners {
  showTimestamp?: boolean
  showScaleBar?: boolean
  showTitleChip?: boolean
  /** Text for the timestamp — a producer can pin a value, or let the default `0:00:00` stand. */
  timestampText?: string
  /** Text for the scale bar — same. */
  scaleBarText?: string
}

/** The full render — a producer builds one of these, `SceneAid.vue` draws it. `caption` sits under
 *  the frame in muted type, for a producer to say "no pops selected" without adding another
 *  component. */
export interface SceneAidRender {
  points: SceneAidPoint[]
  ribbons: SceneAidRibbon[]
  corners: SceneAidCorners
  caption?: string
}
