<!--
  canvas2D gate layer over the base dot canvas (PlotLayers). Two jobs:
   1. DRAW new gates (mode = rectangle | polygon | slice | quadrant) — drag a rect, click polygon
      vertices (double-click / click-near-start to close), click a slice line's vertices (double-click
      to finish; its ends run on to the border), or click a quadrant's centre. Esc cancels. Emits `draw`
      on completion. Drawing
      reaches a little past the plot border and pins those points to it, so a gate can enclose a cloud
      piled against an axis (plots/gateGeometry.ts → "Drawing past the plot border").
   2. EDIT existing gates (mode = off) — move / resize rectangles (corner + edge handles),
      drag / insert (dbl-click edge) / delete (right-click vertex) polygon vertices. A split gate
      (slice / quadrant, plots/splitGeometry.ts) has no body to grab — its regions tile the plot: drag
      its line or centre to move it, a slice vertex or a quadrant arm handle to reshape it; all of its
      region populations follow (the server moves the group). A 1D gate (range / bisector, on a
      histogram — plots/histGeometry.ts) is drawn by dragging the interval / clicking the cut; its ends,
      its cut, or a rule's window ends drag sideways, and a double-click on a bisector switches it
      between a fixed cut and following the density valley. An outline or
      corner grabs on hover; a gate's BODY moves it only once the gate is SELECTED — click inside it
      (the smallest gate there wins, `gateAt`); click outside every gate, outside the plot, or Esc
      deselects — and while nothing is selected, presses inside gates reach the plot (pan/zoom). Live local
      redraw while dragging; emits `edit` with the new spec only on release (server recomputes +
      broadcasts → smooth cross-plot propagation). See docs/UI.md "rendering & UX hacks".
      Holding Shift while a draw tool is armed enters this edit path too (grab the gate under the
      cursor) without disarming the tool — release Shift to keep drawing.

  Everything is mapped data→px through the LIVE (zoom-synced) extents, so gates track pan/zoom.

  We sit above the base dot canvas and must not swallow its clicks: in edit (off) mode the overlay
  sets its own pointer-events to 'auto' ONLY while the cursor is over a gate handle/body (detected
  via a bubbled mousemove on the parent), and 'none' otherwise, so empty-space clicks pass through.
-->
<script setup lang="ts">
import { ref, watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import { observeBoxChanges } from '../../composables/usePlotResize'
import { useWindowListener } from '../../composables/useKeepAlive'
import { dataToPx as mapDataToPx, pxToData as mapPxToData, type PxBox } from '../../plots/axisMap'
import type { GateSpec } from '../../stores/gating'
import { svgPolygon, svgEsc, svgClip, svgLine } from '../../plots/export'
import { gateLabelPos, type GateLabelPos } from '../../utils/gateLabel'
import { isClickNotDrag, isDegeneratePolygon, clampToBox, withinReach, hitGateShape, gateAt } from '../../plots/gateGeometry'
import { handleRadius, distToSeg, HANDLE_R_PX } from '../../utils/drawGeometry'
import { parseHex, luminance } from '../../utils/colour'
import { isSplitGate, dividerSegments, splitLabelPoint, armsCcw, defaultArms, withDivider, type Box } from '../../plots/splitGeometry'
import { isHistGate, histRegion, histHandles, moveHistHandle, toggleRule } from '../../plots/histGeometry'
import { PAPER_INK, HALO_LIGHT, HALO_DARK, STORED_WHITES, WHITE, NEUTRAL } from '../../plots/fixedColours'

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
const props = withDefaults(defineProps<{
  extents: Ext                              // LIVE (zoom-synced) view extents
  mode: 'off' | 'rectangle' | 'polygon' | 'slice' | 'quadrant' | 'range' | 'bisector'
  gates?: { path: string; colour: string; gate: GateSpec; label?: string }[]
  viewTick?: number                          // bump → redraw (camera moved)
  lineWidth?: number                         // gate stroke width
  showLabels?: boolean                       // draw the population name on each gate
  readonly?: boolean                         // fully static: draw outlines only, never move/resize/draw
  flipY?: boolean                            // y is an IMAGE row coordinate (grows downward)
}>(), { lineWidth: 1.5, showLabels: false, readonly: false })
const emit = defineEmits<{ draw: [Partial<GateSpec>]; edit: [{ path: string; gate: GateSpec }]; cancel: [] }>()

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
let ctx: CanvasRenderingContext2D | null = null
let ro: ResizeObserver | null = null

const pe = ref<'none' | 'auto'>('none')      // overlay pointer-events (see header)
const cursor = ref('default')

// ── coordinate mapping (data ⇄ px) via the live extents ──
function size() { const c = canvasEl.value!; return { w: c.clientWidth, h: c.clientHeight } }
// a white/default gate outline is invisible on the light PDF export — resolve those to the themed
// `--cc-text` var so they flip DARK-on-white for export and stay white on the dark screen (same trick
// as the contour ink). Gates with a real pop colour are left as-is (visible on either ground).
function themed(token: string, fallback: string): string {
  const el = canvasEl.value
  const v = el && getComputedStyle(el).getPropertyValue(token).trim()
  return v || fallback
}
const ink = () => themed('--cc-text', PAPER_INK)
// a label halo is the ground's tone, the opposite of the ink: dark on a dark plot, light on a light one
const halo = () => parseHex(ink()) && luminance(ink()) < 0.4 ? HALO_LIGHT : HALO_DARK
// COLOUR-OK: a tint computed from a token's hex
const tint = (hex: string, a: number) => { const c = parseHex(hex); return c ? `rgba(${c[0]},${c[1]},${c[2]},${a})` : hex }
function gateColour(colour?: string): string {
  const c = (colour ?? '').trim().toLowerCase()
  return (!c || STORED_WHITES.has(c)) ? ink() : colour!
}
// ONE mapping, forward AND inverse, from plots/axisMap.ts — and this is the file that makes it
// matter. `pxToData` is how a dragged rectangle becomes the numbers written to
// gating/{value_name}.json, so if the two directions ever disagreed about `flipY` every new position
// gate would be stored MIRRORED: wrong on disk, applied to every future image, and nothing looking
// wrong on screen at the moment it happens. `axisMap.test.ts` pins the round trip in both modes.
const box = (): PxBox => ({ ...size(), flipY: props.flipY })
const dataToPx = (vx: number, vy: number) => mapDataToPx(props.extents, box(), vx, vy)
const pxToData = (px: number, py: number) => mapPxToData(props.extents, box(), px, py)
function evtPx(e: MouseEvent): [number, number] {
  const r = canvasEl.value!.getBoundingClientRect()
  return [e.clientX - r.left, e.clientY - r.top]
}
// a new-gate point: pinned to the plot border when the cursor is past it
function drawPx(e: MouseEvent): [number, number] { const { w, h } = size(); return clampToBox(evtPx(e), w, h) }

// ── geometry helpers ──
const clone = (g: GateSpec): GateSpec => JSON.parse(JSON.stringify(g))
// rect data corners, CCW: 0:(min,min) 1:(max,min) 2:(max,max) 3:(min,max)
function rectCorners(g: GateSpec): [number, number][] {
  return [[g.x_min!, g.y_min!], [g.x_max!, g.y_min!], [g.x_max!, g.y_max!], [g.x_min!, g.y_max!]]
}
// a range as the box it shades: its interval, the plot's full height
function rangeCorners(g: GateSpec): [number, number][] {
  const v = viewBox()
  return [[g.lo!, v.yMin], [g.hi!, v.yMin], [g.hi!, v.yMax], [g.lo!, v.yMax]]
}
// a 1D gate's vertical lines (the ends / the cut), top to bottom of the plot, in px
function histLinesPx(g: GateSpec): [[number, number], [number, number]][] {
  const { h } = size()
  const xs = g.kind === 'range' ? [g.lo!, g.hi!] : [g.cut!]
  return xs.map(x => { const px = dataToPx(x, 0)[0]; return [[px, 0], [px, h]] })
}
// the live view as a data box — a split gate's divider runs out to its border
const viewBox = (): Box => ({ xMin: Math.min(props.extents.xMin, props.extents.xMax), xMax: Math.max(props.extents.xMin, props.extents.xMax),
                              yMin: Math.min(props.extents.yMin, props.extents.yMax), yMax: Math.max(props.extents.yMin, props.extents.yMax) })
const segsPx = (g: GateSpec) => dividerSegments(g, viewBox()).map(([a, b]) => [dataToPx(a[0], a[1]), dataToPx(b[0], b[1])] as [[number, number], [number, number]])
// a quadrant arm's handle: partway along the arm's visible run, so it is on screen however far the
// stored arm point (it only gives the direction) lies
const ARM_HANDLE_AT = 0.4
function armHandlesPx(g: GateSpec): [number, number][] {
  return segsPx(g).map(([a, b]) => [a[0] + ARM_HANDLE_AT * (b[0] - a[0]), a[1] + ARM_HANDLE_AT * (b[1] - a[1])])
}
// ── hit testing for editing ──
type Handle =
  | { path: string; kind: 'rect-move' } | { path: string; kind: 'rect-corner'; ix: number }
  | { path: string; kind: 'rect-edge'; edge: number }
  | { path: string; kind: 'poly-move' } | { path: string; kind: 'poly-vertex'; vi: number }
  | { path: string; kind: 'poly-edge'; ei: number }
  // split gates: drag the whole divider (`seg` = the divider segment grabbed — a slice inserts a vertex
  // there on double-click), a slice vertex, or a quadrant arm
  | { path: string; kind: 'split-drag'; seg: number } | { path: string; kind: 'slice-vertex'; vi: number }
  | { path: string; kind: 'quad-arm'; ai: number }
  // 1D gates: an end / the cut / a rule window end (`part`), a range's body, a rule bisector's line
  | { path: string; kind: 'hist-handle'; part: string } | { path: string; kind: 'hist-move' }
  | { path: string; kind: 'hist-line' }

// grab-zone radius + vertex/edge/body priority: plots/gateGeometry.ts (`hitGateShape`)
// `anyBody`: the Shift-while-armed grab (header) takes ANY gate's body; otherwise only the selected one's
function hitGate(entry: { path: string; gate: GateSpec }, p: [number, number], anyBody = false): Handle | null {
  const bodyOk = anyBody || entry.path === selected.value
  const g = entry.gate
  if (isHistGate(g)) {
    // 1D: the handles are vertical lines, grabbed anywhere along them
    const near = (x: number) => Math.abs(dataToPx(x, 0)[0] - p[0]) < HANDLE_R_PX
    const hh = histHandles(g).find(h => near(h.x))
    if (hh) return { path: entry.path, kind: 'hist-handle', part: hh.part }
    if (g.kind === 'bisector' && near(g.cut!)) return { path: entry.path, kind: 'hist-line' }
    if (g.kind === 'range' && bodyOk) {
      const a = dataToPx(g.lo!, 0)[0], b = dataToPx(g.hi!, 0)[0]
      if (p[0] > Math.min(a, b) && p[0] < Math.max(a, b)) return { path: entry.path, kind: 'hist-move' }
    }
    return null
  }
  if (g.kind === 'rectangle') {
    const hit = hitGateShape(rectCorners(g).map(c => dataToPx(c[0], c[1])) as [number, number][], p)
    if (hit?.part === 'vertex') return { path: entry.path, kind: 'rect-corner', ix: hit.i }
    if (hit?.part === 'edge') return { path: entry.path, kind: 'rect-edge', edge: hit.i }
    if (hit?.part === 'body' && bodyOk) return { path: entry.path, kind: 'rect-move' }
  } else if (g.kind === 'polygon' && g.vertices) {
    const hit = hitGateShape(g.vertices.map(v => dataToPx(v[0], v[1])) as [number, number][], p)
    if (hit?.part === 'vertex') return { path: entry.path, kind: 'poly-vertex', vi: hit.i }
    if (hit?.part === 'edge') return { path: entry.path, kind: 'poly-edge', ei: hit.i }
    if (hit?.part === 'body' && bodyOk) return { path: entry.path, kind: 'poly-move' }
  } else if (isSplitGate(g)) {
    // the shared reach, uncapped: `handleRadius` shrinks it for a small shape, and a split spans the plot
    const r = HANDLE_R_PX
    const near = (q: [number, number]) => Math.hypot(q[0] - p[0], q[1] - p[1]) < r
    if (g.kind === 'slice') {
      const vi = (g.vertices ?? []).findIndex(v => near(dataToPx(v[0], v[1])))
      if (vi >= 0) return { path: entry.path, kind: 'slice-vertex', vi }
    } else {
      if (near(dataToPx(g.centre![0], g.centre![1]))) return { path: entry.path, kind: 'split-drag', seg: -1 }
      const ai = armHandlesPx(g).findIndex(near)
      if (ai >= 0) return { path: entry.path, kind: 'quad-arm', ai }
    }
    const seg = segsPx(g).findIndex(([a, b]) => distToSeg(p, a, b) < r)
    if (seg >= 0) return { path: entry.path, kind: 'split-drag', seg }
  }
  return null
}
function hitTest(p: [number, number], anyBody = false): Handle | null {
  for (const e of props.gates ?? []) { const h = hitGate(e, p, anyBody); if (h) return h }
  return null
}
function cursorFor(h: Handle | null): string {
  if (!h) return 'default'
  // the corner indices are DATA corners, so a flipped y axis puts corner 0 at the top of the screen
  // instead of the bottom — the diagonal the cursor should point along is the other one
  if (h.kind === 'rect-corner')
    return (h.ix % 2 === 0) === !props.flipY ? 'nwse-resize' : 'nesw-resize'
  if (h.kind === 'rect-edge') return h.edge % 2 === 0 ? 'ns-resize' : 'ew-resize'
  if (h.kind === 'poly-vertex' || h.kind === 'slice-vertex' || h.kind === 'quad-arm') return 'grab'
  if (h.kind === 'poly-edge') return 'copy'
  if (h.kind === 'hist-handle') return 'ew-resize'
  if (h.kind === 'hist-line') return 'pointer'
  return 'move'
}

// ── selection: the one gate whose body moves it (header) ──
const selected = ref<string | null>(null)
function outlinePx(g: GateSpec): [number, number][] {
  const pts = g.kind === 'rectangle' ? rectCorners(g) : g.kind === 'range' ? rangeCorners(g) : (g.vertices ?? [])
  return pts.map(v => dataToPx(v[0], v[1])) as [number, number][]
}
function select(path: string | null) { if (selected.value !== path) { selected.value = path; draw() } }
// a press-and-release on the plot that did not drag selects the gate under it (or none) — tracked on
// the PARENT, so it also sees clicks that pass through to the dot canvas while nothing is grabbed
let pressAt: [number, number] | null = null
function onParentDown(e: MouseEvent) { pressAt = e.button === 0 ? evtPx(e) : null }
function onParentUp(e: MouseEvent) {
  const from = pressAt; pressAt = null
  if (!from || props.readonly || props.mode !== 'off' || e.shiftKey) return
  const p = evtPx(e); if (!isClickNotDrag(from, p)) return
  const h = hitTest(p)
  if (h && !h.kind.endsWith('-move')) return      // a click on an outline / corner keeps the selection
  // a split gate's regions cover the whole plot, so it is never selected by a click inside it (nothing
  // could be deselected, and the plot could not pan) — its line and handles grab it instead
  select(gateAt((props.gates ?? []).filter(g => !isSplitGate(g.gate) && g.gate.kind !== 'bisector')
    .map(g => ({ path: g.path, pts: outlinePx(g.gate) })), p))
}
// a press anywhere outside this plot deselects
function onWindowDown(e: MouseEvent) {
  const host = canvasEl.value?.parentElement
  if (selected.value && host && !host.contains(e.target as Node)) select(null)
}
useWindowListener('mousedown', onWindowDown)

// ── editing state ──
const draft = ref<GateSpec | null>(null)     // gate being edited (shown instead of stored)
let editPath: string | null = null
let editHandle: Handle | null = null
let orig: GateSpec | null = null             // gate at drag start
let dragStart: [number, number] | null = null // cursor data at drag start
const hover = ref<Handle | null>(null)

function gateOf(path: string): { path: string; colour: string; gate: GateSpec } | undefined {
  return (props.gates ?? []).find(g => g.path === path)
}

// ── drawing state (new gates) ──
let dragging = false
let start: [number, number] | null = null
let cur: [number, number] | null = null
const polyPts = ref<[number, number][]>([])

// while a draw tool is armed, holding Shift temporarily switches to edit: grab/move/resize any gate
// under the cursor without disarming the tool. (Shift, not Alt — Alt opens the menu bar in Firefox
// and is the menu-access key in other browsers.) `shiftEdit` = Shift held over the plot while armed →
// show handles + edit cursors. An `editHandle` (mid-drag) counts too, so handles stay while resizing.
const shiftEdit = ref(false)
const editIntent = (e: MouseEvent) => props.mode !== 'off' && e.shiftKey && !props.readonly

// ── render ──
function strokeShape(g: GateSpec, colour: string, lw = props.lineWidth) {
  const c = ctx!; c.strokeStyle = colour; c.lineWidth = lw
  if (isHistGate(g)) return strokeHist(g, colour, lw)
  c.beginPath()
  if (isSplitGate(g)) {
    for (const [a, b] of segsPx(g)) { c.moveTo(a[0], a[1]); c.lineTo(b[0], b[1]) }
  } else if (g.kind === 'rectangle') {
    const cpx = rectCorners(g).map(p => dataToPx(p[0], p[1]))
    c.moveTo(cpx[0][0], cpx[0][1]); for (let i = 1; i < 4; i++) c.lineTo(cpx[i][0], cpx[i][1]); c.closePath()
  } else if (g.kind === 'polygon' && g.vertices) {
    g.vertices.forEach((v, i) => { const [px, py] = dataToPx(v[0], v[1]); i === 0 ? c.moveTo(px, py) : c.lineTo(px, py) })
    c.closePath()
  }
  c.stroke()
}
// a 1D gate: its lines; a range's interval shaded faintly; a rule bisector's search window shaded and
// its cut dashed when the rule found no valley on this parent (the stored cut stands in)
function strokeHist(g: GateSpec, colour: string, lw: number) {
  const c = ctx!; const { h } = size()
  if (g.kind === 'bisector' && g.rule) {
    const a = dataToPx(g.rule.lo, 0)[0], b = dataToPx(g.rule.hi, 0)[0]
    c.fillStyle = tint(colour, 0.08); c.fillRect(Math.min(a, b), 0, Math.abs(b - a), h)
  } else if (g.kind === 'range') {
    const a = dataToPx(g.lo!, 0)[0], b = dataToPx(g.hi!, 0)[0]
    c.fillStyle = tint(colour, 0.08); c.fillRect(Math.min(a, b), 0, Math.abs(b - a), h)
  }
  c.save(); c.strokeStyle = colour; c.lineWidth = lw
  if (g.kind === 'bisector' && g.rule && g.rule_ok === false) c.setLineDash([5, 4])
  c.beginPath()
  for (const [a, b] of histLinesPx(g)) { c.moveTo(a[0], a[1]); c.lineTo(b[0], b[1]) }
  c.stroke(); c.restore()
}
// subtle population-name label centred at the gate's top edge. An explicit `label` overrides the
// derived name (the gating-strategy plot passes "name  pct%").
// Where a gate's name goes: above / below / inside its outline (utils/gateLabel.ts), or, for a split
// gate, centred on the visible part of its region.
function labelPos(g: GateSpec, labelW: number, w: number, h: number): GateLabelPos | null {
  if (isHistGate(g)) {              // the middle of its interval, near the top — a range's a line lower,
    const r = histRegion(g, viewBox()); if (!r) return null    // so it can't sit on a bisector half's
    const x = dataToPx((r[0] + r[1]) / 2, 0)[0]
    return { x: Math.min(w - labelW / 2, Math.max(labelW / 2, x)), y: g.kind === 'range' ? 22 : 6, baseline: 'top' }
  }
  if (isSplitGate(g)) {
    const q = splitLabelPoint(g, viewBox()); if (!q) return null
    const [x, y] = dataToPx(q[0], q[1])
    return { x: Math.min(w - labelW / 2, Math.max(labelW / 2, x)), y: y - 6, baseline: 'top' }
  }
  const pts = (g.kind === 'rectangle' ? rectCorners(g) : (g.vertices ?? [])).map(p => dataToPx(p[0], p[1])) as [number, number][]
  return gateLabelPos(pts, labelW, w, h)
}
function drawGateLabel(g: GateSpec, path: string, colour: string, label?: string) {
  const name = label ?? (path.split('/').filter(Boolean).pop() ?? '')
  const c = ctx!; c.save()
  c.font = 'bold 12px system-ui, sans-serif'; c.textAlign = 'center'
  c.lineJoin = 'round'; c.lineWidth = 3; c.strokeStyle = halo()   // ground-toned halo for legibility
  const { w, h } = size()
  const pos = labelPos(g, c.measureText(name).width, w, h)
  if (!pos) { c.restore(); return }
  c.textBaseline = pos.baseline
  c.strokeText(name, pos.x, pos.y)
  c.fillStyle = colour; c.fillText(name, pos.x, pos.y); c.restore()
}
function drawHandles(g: GateSpec, colour: string) {
  const c = ctx!; c.fillStyle = WHITE; c.strokeStyle = colour; c.lineWidth = 1.5
  if (isHistGate(g)) {              // a square on each draggable line, halfway up
    const r = HANDLE_R_PX, y = size().h / 2
    for (const hh of histHandles(g)) { const x = dataToPx(hh.x, 0)[0]; c.beginPath(); c.rect(x - r / 2, y - r / 2, r, r); c.fill(); c.stroke() }
    return
  }
  if (isSplitGate(g)) {
    const r = HANDLE_R_PX                        // drawn at the reach `hitGate` grabs with, like the others
    const dot = (p: [number, number]) => { c.beginPath(); c.arc(p[0], p[1], r * 0.6, 0, 2 * Math.PI); c.fill(); c.stroke() }
    if (g.kind === 'slice') (g.vertices ?? []).forEach(v => dot(dataToPx(v[0], v[1])))
    else {
      const [cx, cy] = dataToPx(g.centre![0], g.centre![1])
      c.beginPath(); c.rect(cx - r / 2, cy - r / 2, r, r); c.fill(); c.stroke()
      armHandlesPx(g).forEach(dot)
    }
    return
  }
  const pts = (g.kind === 'rectangle' ? rectCorners(g) : (g.vertices ?? [])).map(p => dataToPx(p[0], p[1])) as [number, number][]
  // drawn at the size of the grab zone (a square of side r, a dot of radius 0.6 r), so a small gate's
  // shrunken reach shows as smaller handles
  const r = handleRadius(pts)
  const sq = (px: number, py: number) => { c.beginPath(); c.rect(px - r / 2, py - r / 2, r, r); c.fill(); c.stroke() }
  const dot = (px: number, py: number) => { c.beginPath(); c.arc(px, py, r * 0.6, 0, 2 * Math.PI); c.fill(); c.stroke() }
  if (g.kind === 'rectangle') {
    pts.forEach(p => sq(p[0], p[1]))
    for (let e = 0; e < 4; e++) { const a = pts[e], b = pts[(e + 1) % 4]; sq((a[0] + b[0]) / 2, (a[1] + b[1]) / 2) }
  } else if (g.kind === 'polygon' && g.vertices) {
    pts.forEach(p => dot(p[0], p[1]))
  }
}
// the committed gate outlines + labels (substitute the draft for the one being edited). Split out so
// the hi-res export can re-paint just these — handles / in-progress shapes are live-interaction only.
// the gate as shown: the draft while it is dragged — and a split gate's group members follow the draft
function shownSpec(g: { path: string; gate: GateSpec }): GateSpec {
  if (!draft.value) return g.gate
  return g.path === editPath ? draft.value : withDivider(g.gate, draft.value)
}
function paintGates() {
  const drawn = new Set<string>()       // a split's regions share one divider: stroke it once, in ink
  for (const g of props.gates ?? []) {
    const spec = shownSpec(g)
    // a white/default gate flips to the ground's ink, outline and label alike — the label's halo is
    // ground-toned, so a white label would vanish on a light plot.
    // a split's / bisector's regions share one divider: stroked once, in ink
    if (!isSplitGate(spec) && spec.kind !== 'bisector') strokeShape(spec, gateColour(g.colour))
    else if (!spec.group || !drawn.has(spec.group)) { strokeShape(spec, ink()); if (spec.group) drawn.add(spec.group) }
    if (props.showLabels) drawGateLabel(spec, g.path, gateColour(g.colour), g.label)
  }
}
// ── TRUE-VECTOR SVG export (docs/PLOTS.md) ──────────────────────────────────────────────────────────
// The committed gate outlines (+ labels) as SVG in LOCAL plot-area coords [0..w,0..h] — <polygon> per
// gate, reusing rectCorners/vertices + dataToPx (same geometry as strokeShape). Handles / in-progress
// shapes are interaction-only and never exported. The host translates this into the capture.
function gateLabelSvg(g: GateSpec, path: string, colour: string, label: string | undefined, w: number, h: number): string {
  const name = label ?? (path.split('/').filter(Boolean).pop() ?? '')
  if (!name) return ''
  const SIZE = 12
  // measured with the same font the canvas label uses, on the overlay's own canvas
  const c = ctx; let labelW = name.length * SIZE * 0.6
  if (c) { c.save(); c.font = 'bold 12px system-ui, sans-serif'; labelW = c.measureText(name).width; c.restore() }
  const pos = labelPos(g, labelW, w, h)
  if (!pos) return ''
  // canvas `textBaseline` → SVG: a 'top' label's baseline sits one glyph height lower
  const y = pos.baseline === 'top' ? pos.y + SIZE : pos.y
  const r1 = (n: number) => Math.round(n * 10) / 10
  // paint-order:stroke → ground-toned halo BEHIND the coloured fill (legible over dense dots), like the canvas
  return `<text x="${r1(pos.x)}" y="${r1(y)}" font-family="system-ui, sans-serif" font-size="${SIZE}" ` +
         `font-weight="bold" text-anchor="middle" paint-order="stroke" stroke="${halo()}" ` +
         `stroke-width="3" stroke-linejoin="round" fill="${colour}">${svgEsc(name)}</text>`
}
// Outlines are clipped to the plot area like the canvas (an open-ended gate runs far off-axis).
function exportSvgContent(): string {
  const { w, h } = size(); if (!w || !h) return ''
  let outlines = '', labels = ''
  const drawn = new Set<string>()
  for (const g of props.gates ?? []) {
    if (isHistGate(g.gate)) {           // 1D: its lines (a bisector's once per pair, in ink)
      const shared = g.gate.kind === 'bisector'
      if (!shared || !g.gate.group || !drawn.has(g.gate.group)) {
        for (const [a, b] of histLinesPx(g.gate))
          outlines += svgLine(a[0], a[1], b[0], b[1], { stroke: shared ? ink() : gateColour(g.colour), width: props.lineWidth })
        if (shared && g.gate.group) drawn.add(g.gate.group)
      }
      if (props.showLabels) labels += gateLabelSvg(g.gate, g.path, gateColour(g.colour), g.label, w, h)
      continue
    }
    if (isSplitGate(g.gate)) {          // the divider once per split, as lines; each region's name
      if (!g.gate.group || !drawn.has(g.gate.group)) {
        for (const [a, b] of segsPx(g.gate)) outlines += svgLine(a[0], a[1], b[0], b[1], { stroke: ink(), width: props.lineWidth })
        if (g.gate.group) drawn.add(g.gate.group)
      }
      if (props.showLabels) labels += gateLabelSvg(g.gate, g.path, gateColour(g.colour), g.label, w, h)
      continue
    }
    const pts = (g.gate.kind === 'rectangle' ? rectCorners(g.gate) : (g.gate.vertices ?? []))
      .map(p => dataToPx(p[0], p[1])) as [number, number][]
    if (pts.length < 2) continue
    outlines += svgPolygon(pts, { stroke: gateColour(g.colour), width: props.lineWidth })
    if (props.showLabels) labels += gateLabelSvg(g.gate, g.path, gateColour(g.colour), g.label, w, h)
  }
  return svgClip({ w, h }, outlines) + labels
}

function draw() {
  if (!ctx || !canvasEl.value) return
  const dpr = window.devicePixelRatio || 1
  const { w, h } = size()
  canvasEl.value.width = Math.max(1, w * dpr); canvasEl.value.height = Math.max(1, h * dpr)
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.clearRect(0, 0, w, h)

  paintGates()
  // handles on the edited / hovered / selected gate
  const active = editPath ? gateOf(editPath) : hover.value ? gateOf(hover.value.path)
               : selected.value ? gateOf(selected.value) : undefined
  if (active && (props.mode === 'off' || shiftEdit.value || editHandle)) {
    const spec = active.path === editPath && draft.value ? draft.value : active.gate
    drawHandles(spec, active.colour || themed('--cc-accent', NEUTRAL))
  }

  // in-progress NEW shape (accent)
  const accent = themed('--cc-accent', NEUTRAL)
  ctx.strokeStyle = accent; ctx.fillStyle = tint(accent, 0.12); ctx.lineWidth = 1.5
  if (props.mode === 'rectangle' && start && cur) {
    const x = Math.min(start[0], cur[0]), y = Math.min(start[1], cur[1])
    ctx.fillRect(x, y, Math.abs(cur[0] - start[0]), Math.abs(cur[1] - start[1]))
    ctx.strokeRect(x, y, Math.abs(cur[0] - start[0]), Math.abs(cur[1] - start[1]))
  } else if ((props.mode === 'polygon' || props.mode === 'slice') && polyPts.value.length) {
    ctx.beginPath()
    polyPts.value.forEach((p, i) => (i === 0 ? ctx!.moveTo(p[0], p[1]) : ctx!.lineTo(p[0], p[1])))
    if (cur) ctx.lineTo(cur[0], cur[1])
    ctx.stroke()
    for (const p of polyPts.value) { ctx.beginPath(); ctx.arc(p[0], p[1], 3, 0, 2 * Math.PI); ctx.fill() }
    // a slice runs on past its ends: preview that, dashed
    const line = cur ? [...polyPts.value, cur] : polyPts.value
    if (props.mode === 'slice' && line.length >= 2 && Math.hypot(line[0][0] - line[line.length - 1][0], line[0][1] - line[line.length - 1][1]) > 0) {
      const segs = segsPx({ ...previewBase, kind: 'slice', vertices: line.map(p => pxToData(p[0], p[1])) as [number, number][] })
      dashed(() => { for (const s of [segs[0], segs[segs.length - 1]]) { ctx!.moveTo(s[0][0], s[0][1]); ctx!.lineTo(s[1][0], s[1][1]) } })
    }
  } else if (props.mode === 'range' && start && cur) {
    const { h } = size(), x = Math.min(start[0], cur[0])
    ctx.fillRect(x, 0, Math.abs(cur[0] - start[0]), h); ctx.strokeRect(x, 0, Math.abs(cur[0] - start[0]), h)
  } else if (props.mode === 'bisector' && cur && !shiftEdit.value) {
    const { h } = size(), x0 = cur[0]
    dashed(() => { ctx!.moveTo(x0, 0); ctx!.lineTo(x0, h) })
  } else if (props.mode === 'quadrant' && cur && !shiftEdit.value) {
    // the crosshair a click would place
    const { w, h } = size(), c0 = cur
    dashed(() => { ctx!.moveTo(0, c0[1]); ctx!.lineTo(w, c0[1]); ctx!.moveTo(c0[0], 0); ctx!.lineTo(c0[0], h) })
  }
}
const previewBase = { x_channel: '', y_channel: '', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const
function dashed(path: () => void) {
  const c = ctx!; c.save(); c.setLineDash([4, 4]); c.beginPath(); path(); c.stroke(); c.restore()
}

// hi-res export (see plots/export.ts): re-paint the committed gates onto a scale× offscreen canvas
// so gate outlines/labels are crisp instead of the compositor upscaling the screen-DPR canvas. Swap
// the module `ctx` to the offscreen context (draw helpers target it via toPx/size), then restore.
async function exportCanvas(scale: number): Promise<HTMLCanvasElement | null> {
  if (!canvasEl.value) return null
  const { w, h } = size(); if (!w || !h) return null
  const off = document.createElement('canvas')
  off.width = Math.max(1, Math.round(w * scale)); off.height = Math.max(1, Math.round(h * scale))
  const octx = off.getContext('2d'); if (!octx) return null
  const saved = ctx
  ctx = octx
  octx.setTransform(scale, 0, 0, scale, 0, 0)
  octx.clearRect(0, 0, w, h)
  paintGates()
  ctx = saved                                  // the live canvas's context object was never touched
  return off
}
defineExpose({ exportCanvas, getCanvas: () => canvasEl.value, exportSvgContent })

// ── editing: apply a handle drag to the draft ──
function applyEdit(p: [number, number]) {
  if (!editHandle || !orig || !dragStart) return
  const [cx, cy] = pxToData(p[0], p[1])
  const d = clone(orig)
  const h = editHandle
  if (h.kind === 'rect-corner') {
    let xmin = orig.x_min!, xmax = orig.x_max!, ymin = orig.y_min!, ymax = orig.y_max!
    ;(h.ix === 0 || h.ix === 3) ? (xmin = cx) : (xmax = cx)
    ;(h.ix === 0 || h.ix === 1) ? (ymin = cy) : (ymax = cy)
    d.x_min = Math.min(xmin, xmax); d.x_max = Math.max(xmin, xmax)
    d.y_min = Math.min(ymin, ymax); d.y_max = Math.max(ymin, ymax)
  } else if (h.kind === 'rect-edge') {
    let xmin = orig.x_min!, xmax = orig.x_max!, ymin = orig.y_min!, ymax = orig.y_max!
    if (h.edge === 0) ymin = cy; else if (h.edge === 2) ymax = cy; else if (h.edge === 1) xmax = cx; else xmin = cx
    d.x_min = Math.min(xmin, xmax); d.x_max = Math.max(xmin, xmax)
    d.y_min = Math.min(ymin, ymax); d.y_max = Math.max(ymin, ymax)
  } else if (h.kind === 'rect-move') {
    const dx = cx - dragStart[0], dy = cy - dragStart[1]
    d.x_min = orig.x_min! + dx; d.x_max = orig.x_max! + dx; d.y_min = orig.y_min! + dy; d.y_max = orig.y_max! + dy
  } else if (h.kind === 'poly-vertex') {
    d.vertices = orig.vertices!.map(v => [v[0], v[1]] as [number, number]); d.vertices[h.vi] = [cx, cy]
  } else if (h.kind === 'poly-move') {
    const dx = cx - dragStart[0], dy = cy - dragStart[1]
    d.vertices = orig.vertices!.map(v => [v[0] + dx, v[1] + dy] as [number, number])
  } else if (h.kind === 'slice-vertex') {
    d.vertices = orig.vertices!.map(v => [v[0], v[1]] as [number, number]); d.vertices[h.vi] = [cx, cy]
  } else if (h.kind === 'split-drag') {
    const dx = cx - dragStart[0], dy = cy - dragStart[1]
    const mv = (v: [number, number]) => [v[0] + dx, v[1] + dy] as [number, number]
    if (orig.kind === 'slice') d.vertices = orig.vertices!.map(mv)
    else { d.centre = mv(orig.centre!); d.arms = orig.arms!.map(mv) }
  } else if (h.kind === 'hist-handle') {
    Object.assign(d, moveHistHandle(orig, h.part, cx))
  } else if (h.kind === 'hist-move') {
    const dx = cx - dragStart[0]
    d.lo = orig.lo! + dx; d.hi = orig.hi! + dx
  } else if (h.kind === 'hist-line') {
    return                                         // a rule places the cut — only its window drags
  } else if (h.kind === 'quad-arm') {
    const arms = orig.arms!.map(a => [a[0], a[1]] as [number, number]); arms[h.ai] = [cx, cy]
    // an arm swung past its neighbour would swap two populations' regions — hold it at the last good spot
    if (!armsCcw(orig.centre!, arms)) return
    d.arms = arms
  }
  draft.value = d
}

let editDownPx: [number, number] | null = null
function startEdit(h: Handle, p: [number, number]) {
  const entry = gateOf(h.path); if (!entry) return
  editDownPx = p
  editPath = h.path; editHandle = h; orig = clone(entry.gate)
  dragStart = pxToData(p[0], p[1]); draft.value = clone(entry.gate)
  window.addEventListener('mousemove', onEditMove); window.addEventListener('mouseup', onEditUp)
}
function onEditMove(e: MouseEvent) { applyEdit(evtPx(e)); draw() }
function onEditUp(e: MouseEvent) {
  window.removeEventListener('mousemove', onEditMove); window.removeEventListener('mouseup', onEditUp)
  // a click on a grab zone is not an edit: re-storing the same gate would add an undo step for nothing
  // a rule bisector's line grabs (for its double-click) but has nothing to drag — the rule places it
  const moved = editHandle?.kind !== 'hist-line' && (!editDownPx || !isClickNotDrag(editDownPx, evtPx(e)))
  if (editPath && draft.value && moved) emit('edit', { path: editPath, gate: draft.value })
  if (!moved) { draft.value = null; editPath = null; draw() }
  editDownPx = null
  editHandle = null; orig = null; dragStart = null
  // keep draft + editPath until the authoritative tree arrives (gates watch clears them) → no flash
}

// ── proximity: toggle overlay pointer-events so regl keeps pan/zoom in empty space ──
function onParentMove(e: MouseEvent) {
  if (props.readonly) return   // read-only: overlay never becomes interactive (no move/resize)
  if (props.mode !== 'off' || editHandle) return
  const h = hitTest(evtPx(e))
  hover.value = h
  pe.value = h ? 'auto' : 'none'
  cursor.value = cursorFor(h)
  draw()
}

// ── canvas handlers ──
function onDown(e: MouseEvent) {
  if (editIntent(e)) {                            // armed + Shift → edit the gate under the cursor, never draw
    const h = hitTest(evtPx(e), true); if (h) startEdit(h, evtPx(e))
    return
  }
  if (props.mode === 'rectangle' || props.mode === 'range') return startRectDrag(drawPx(e))
  if (props.mode !== 'off') return                // polygon / slice / quadrant / bisector: placed on click
  const h = hover.value ?? hitTest(evtPx(e))      // off mode → begin editing
  if (h) startEdit(h, evtPx(e))
}
function onMove(e: MouseEvent) {
  if (props.mode !== 'off') {                     // armed: Shift gives edit-hover feedback, else draw
    if (editIntent(e) && !dragging && !editHandle) {   // (mid Shift-drag is driven by onEditMove)
      shiftEdit.value = true
      const h = hitTest(evtPx(e), true); hover.value = h; cursor.value = h ? cursorFor(h) : 'crosshair'
      return draw()
    }
    if (shiftEdit.value) { shiftEdit.value = false; hover.value = null; cursor.value = 'crosshair'; draw() }
    if (dragging) return                          // a rectangle drag is tracked on the window (onDrawMove)
    cur = drawPx(e)
    if (props.mode === 'rectangle' || props.mode === 'range') return
    return draw()
  }
}
// the quadrant tool's crosshair follows the cursor only while it is over the plot
function onLeave() { if ((props.mode === 'quadrant' || props.mode === 'bisector') && cur) { cur = null; draw() } }
// the drag is tracked on the WINDOW, so it keeps following a cursor that leaves the plot (the point is
// pinned to the border) and a release out there still commits the gate
function startRectDrag(p: [number, number]) {
  dragging = true; start = p; cur = p
  window.addEventListener('mousemove', onDrawMove); window.addEventListener('mouseup', onUp)
}
function onDrawMove(e: MouseEvent) { cur = drawPx(e); draw() }
function onUp() {
  window.removeEventListener('mousemove', onDrawMove); window.removeEventListener('mouseup', onUp)
  if (props.mode === 'rectangle' && dragging && start && cur) {
    dragging = false
    const from = start, to = cur
    start = cur = null
    // A CLICK IS NOT A GATE. `onDown` seeds `cur = start`, so releasing without moving used to emit a
    // zero-area rectangle: a population with no cells and an outline too thin to see. The tool stays
    // armed on purpose (gate repeatedly without re-selecting it), so ANY stray click on the plot did
    // it — which is what "I drew a gate and it fell to zero, redrawing is fine" was. Swallowed, not
    // cancelled: a mis-click shouldn't disarm the tool either. (plots/gateGeometry.ts)
    if (isClickNotDrag(from, to)) { draw(); return }
    const [ax, ay] = pxToData(from[0], from[1]); const [bx, by] = pxToData(to[0], to[1])
    emit('draw', { kind: 'rectangle', x_min: Math.min(ax, bx), x_max: Math.max(ax, bx), y_min: Math.min(ay, by), y_max: Math.max(ay, by) })
  } else if (props.mode === 'range' && dragging && start && cur) {
    dragging = false
    const from = start, to = cur
    start = cur = null
    // only the sideways travel counts: a range has no height
    if (isClickNotDrag([from[0], 0], [to[0], 0])) { draw(); return }
    const a = pxToData(from[0], from[1])[0], b = pxToData(to[0], to[1])[0]
    emit('draw', { kind: 'range', lo: Math.min(a, b), hi: Math.max(a, b) })
  }
}
function onClick(e: MouseEvent) {
  if (e.shiftKey) return                          // Shift+click is an edit grab (onDown), not a vertex
  if (props.mode === 'polygon' || props.mode === 'slice') placeVertex(drawPx(e))
  else if (props.mode === 'quadrant') placeQuadrant(drawPx(e))
  else if (props.mode === 'bisector') placeBisector(drawPx(e))
}
// a bisector is one click: its cut
function placeBisector(p: [number, number]) { emit('draw', { kind: 'bisector', cut: pxToData(p[0], p[1])[0] }) }
// a quadrant is one click: its centre, arms along the axes out to the border
function placeQuadrant(p: [number, number]) {
  const c = pxToData(p[0], p[1]) as [number, number]
  emit('draw', { kind: 'quadrant', centre: c, arms: defaultArms(c, viewBox()) })
}
function placeVertex(p: [number, number]) {
  if (props.mode === 'polygon' && polyPts.value.length >= 3) {
    const [fx, fy] = polyPts.value[0]
    if (Math.hypot(p[0] - fx, p[1] - fy) < 10) return finishPolygon()
  }
  polyPts.value.push(p); draw()
}
function onDblClick(e: MouseEvent) {
  if (props.mode === 'polygon') return finishPolygon()
  if (props.mode === 'slice') return finishSlice()
  // off mode: insert a vertex on the nearest polygon edge — or slice segment — under the cursor; on a
  // bisector's cut, switch it between a fixed cut and following the valley
  const p = evtPx(e); const h = hitTest(p)
  if (h && (h.kind === 'hist-line' || (h.kind === 'hist-handle' && h.part === 'cut'))) {
    const entry = gateOf(h.path); if (entry) emit('edit', { path: h.path, gate: toggleRule(clone(entry.gate), viewBox()) })
  } else if (h && h.kind === 'poly-edge') {
    const entry = gateOf(h.path); if (!entry?.gate.vertices) return
    const d = clone(entry.gate); const [dx, dy] = pxToData(p[0], p[1])
    d.vertices!.splice(h.ei + 1, 0, [dx, dy]); emit('edit', { path: h.path, gate: d })
  } else if (h && h.kind === 'split-drag') {
    // divider segment i of a slice runs vertex i-1 → i (segment 0 and the last are the run-outs to the
    // border — a vertex there extends the line instead)
    const entry = gateOf(h.path); if (entry?.gate.kind !== 'slice' || !entry.gate.vertices) return
    const n = entry.gate.vertices.length
    const d = clone(entry.gate); const at = pxToData(p[0], p[1]) as [number, number]
    if (h.seg === 0) d.vertices!.unshift(at); else if (h.seg > n - 1) d.vertices!.push(at)
    else d.vertices!.splice(h.seg, 0, at)
    emit('edit', { path: h.path, gate: d })
  }
}
function onContextMenu(e: MouseEvent) {
  if (props.mode !== 'off') return
  const h = hitTest(evtPx(e))
  if (h && (h.kind === 'poly-vertex' || h.kind === 'slice-vertex')) {
    // a polygon keeps 3 vertices, a slice line 2
    const entry = gateOf(h.path); const min = h.kind === 'slice-vertex' ? 2 : 3
    if (!entry?.gate.vertices || entry.gate.vertices.length <= min) return
    e.preventDefault()
    const d = clone(entry.gate); d.vertices!.splice(h.vi, 1); emit('edit', { path: h.path, gate: d })
  }
}
function finishPolygon() {
  // < 3 points isn't a polygon; neither is one closed on the spot or drawn in a straight line — the
  // polygon tool's version of the zero-area rectangle above, and just as invisible once stored.
  if (polyPts.value.length < 3 || isDegeneratePolygon(polyPts.value)) {
    polyPts.value = []; emit('cancel'); return
  }
  const verts = polyPts.value.map(p => pxToData(p[0], p[1])) as [number, number][]
  polyPts.value = []; cur = null
  emit('draw', { kind: 'polygon', vertices: verts })
}
function finishSlice() {
  // a double-click lands two clicks on one spot first — drop points that repeat the one before
  const pts = polyPts.value.filter((p, i, a) => i === 0 || !isClickNotDrag(a[i - 1], p))
  polyPts.value = []; cur = null
  // one point, or a line too short to see, is no slice — the tool's version of the zero-area rectangle
  if (pts.length < 2 || isClickNotDrag(pts[0], pts[pts.length - 1])) { emit('cancel'); draw(); return }
  emit('draw', { kind: 'slice', vertices: pts.map(p => pxToData(p[0], p[1])) as [number, number][] })
}
// ── the reach band: just outside the plot, where the tick labels sit ──
// A press there while a draw tool is armed starts the rectangle / places the vertex as if it were on
// the border. Only within this plot's own capture box (not a neighbouring plot's) and never on a
// control, so the band cannot steal a click meant for something else.
function inReachBand(e: MouseEvent): boolean {
  const c = canvasEl.value
  if (!c || props.readonly || props.mode === 'off' || e.shiftKey || e.button !== 0) return false
  const t = e.target as Element | null
  if (!t || t === c || !(c.closest('.plot-capture') ?? c.parentElement)?.contains(t)) return false
  if (t.closest('button, input, select, textarea, a, label, [role="button"], .gate-overlay')) return false
  const { w, h } = size()
  return withinReach(evtPx(e), w, h)
}
function onBandDown(e: MouseEvent) {
  if (!inReachBand(e)) return
  e.preventDefault()                              // no text selection off the tick labels
  if (props.mode === 'rectangle' || props.mode === 'range') startRectDrag(drawPx(e))
}
function onBandClick(e: MouseEvent) {
  if (!inReachBand(e)) return
  if (props.mode === 'polygon' || props.mode === 'slice') placeVertex(drawPx(e))
  else if (props.mode === 'quadrant') placeQuadrant(drawPx(e))
  else if (props.mode === 'bisector') placeBisector(drawPx(e))
}
function onBandDblClick(e: MouseEvent) {
  if (!inReachBand(e)) return
  if (props.mode === 'polygon') finishPolygon(); else if (props.mode === 'slice') finishSlice()
}
// the polygon's / slice's rubber band keeps following the cursor out into the band
function onBandMove(e: MouseEvent) {
  if ((props.mode !== 'polygon' && props.mode !== 'slice') || !polyPts.value.length || e.target === canvasEl.value) return
  cur = drawPx(e); draw()
}
useWindowListener('mousedown', onBandDown)
useWindowListener('click', onBandClick)
useWindowListener('dblclick', onBandDblClick)
useWindowListener('mousemove', onBandMove)

function onKey(e: KeyboardEvent) {
  // an Escape a popover or dialog on top already took (a plot's settings menu) is not ours
  if (e.defaultPrevented) return
  // idle Esc deselects (claimed only when there is a selection, so the maximised-layout Esc still works)
  if (e.key === 'Escape' && props.mode === 'off' && !editHandle && selected.value) {
    e.preventDefault(); select(null); return
  }
  if (e.key === 'Escape') {
    // claim the key only while drawing, so an idle overlay does not block the maximised-layout Esc
    if (props.mode !== 'off' || dragging || polyPts.value.length) e.preventDefault()
    window.removeEventListener('mousemove', onDrawMove); window.removeEventListener('mouseup', onUp)
    dragging = false; start = cur = null; polyPts.value = []; emit('cancel'); draw()
  }
}
useWindowListener('keydown', onKey)

// drawing modes always capture; off mode toggles via proximity
watch(() => props.mode, m => { pe.value = m === 'off' ? 'none' : 'auto'; cursor.value = m === 'off' ? 'default' : 'crosshair'; shiftEdit.value = false })
// authoritative gates arrived → drop the local draft (unless mid-drag)
watch(() => props.gates, () => {
  if (!editHandle) { draft.value = null; editPath = null }
  if (selected.value && !gateOf(selected.value)) selected.value = null   // deleted / moved off these axes
  draw()
}, { deep: true })
watch(() => [props.extents, props.viewTick, props.lineWidth, props.showLabels], draw, { deep: true })

onMounted(() => {
  ctx = canvasEl.value!.getContext('2d')
  draw()
  ro = observeBoxChanges(canvasEl.value!, draw)
  canvasEl.value!.parentElement?.addEventListener('mousemove', onParentMove)
  canvasEl.value!.parentElement?.addEventListener('mousedown', onParentDown)
  canvasEl.value!.parentElement?.addEventListener('mouseup', onParentUp)
})
onBeforeUnmount(() => {
  ro?.disconnect(); ro = null
  window.removeEventListener('mousemove', onEditMove); window.removeEventListener('mouseup', onEditUp)
  window.removeEventListener('mousemove', onDrawMove); window.removeEventListener('mouseup', onUp)
  canvasEl.value?.parentElement?.removeEventListener('mousemove', onParentMove)
  canvasEl.value?.parentElement?.removeEventListener('mousedown', onParentDown)
  canvasEl.value?.parentElement?.removeEventListener('mouseup', onParentUp)
})
</script>

<template>
  <canvas
    ref="canvasEl"
    class="gate-overlay"
    :style="{ pointerEvents: pe, cursor }"
    @mousedown="onDown" @mousemove="onMove" @mouseleave="onLeave"
    @click="onClick" @dblclick="onDblClick" @contextmenu="onContextMenu"
  />
</template>

<style scoped>
.gate-overlay { position: absolute; inset: 0; width: 100%; height: 100%; }
</style>
