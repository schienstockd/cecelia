<!--
  canvas2D gate layer over ScatterGL. Two jobs:
   1. DRAW new gates (mode = rectangle | polygon) — drag a rect, or click polygon vertices
      (double-click / click-near-start to close, Esc cancels). Emits `draw` on completion.
   2. EDIT existing gates (mode = off) — move / resize rectangles (corner + edge handles),
      drag / insert (dbl-click edge) / delete (right-click vertex) polygon vertices. Live local
      redraw while dragging; emits `edit` with the new spec only on release (server recomputes +
      broadcasts → smooth cross-plot propagation). See docs/UI.md "rendering & UX hacks".

  Everything is mapped data→px through the LIVE (zoom-synced) extents, so gates track pan/zoom.

  Pan/zoom belong to regl below us. To not steal them, in edit (off) mode the overlay sets its
  own pointer-events to 'auto' ONLY while the cursor is over a gate handle/body (detected via a
  bubbled mousemove on the parent), and 'none' otherwise — so clicks in empty space reach regl.
-->
<script setup lang="ts">
import { ref, watch, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'
import type { GateSpec } from '../../stores/gating'

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
const props = withDefaults(defineProps<{
  extents: Ext                              // LIVE (zoom-synced) view extents
  mode: 'off' | 'rectangle' | 'polygon'
  gates?: { path: string; colour: string; gate: GateSpec }[]
  viewTick?: number                          // bump → redraw (camera moved)
  lineWidth?: number                         // gate stroke width
  showLabels?: boolean                       // draw the population name on each gate
}>(), { lineWidth: 1.5, showLabels: false })
const emit = defineEmits<{ draw: [Partial<GateSpec>]; edit: [{ path: string; gate: GateSpec }]; cancel: [] }>()

const canvasEl = useTemplateRef<HTMLCanvasElement>('canvasEl')
let ctx: CanvasRenderingContext2D | null = null
let ro: ResizeObserver | null = null
const HANDLE_R = 6

const pe = ref<'none' | 'auto'>('none')      // overlay pointer-events (see header)
const cursor = ref('default')

// ── coordinate mapping (data ⇄ px) via the live extents ──
function size() { const c = canvasEl.value!; return { w: c.clientWidth, h: c.clientHeight } }
function dataToPx(vx: number, vy: number): [number, number] {
  const { w, h } = size(); const { xMin, xMax, yMin, yMax } = props.extents
  const xs = xMax > xMin ? xMax - xMin : 1, ys = yMax > yMin ? yMax - yMin : 1
  return [((vx - xMin) / xs) * w, (1 - (vy - yMin) / ys) * h]
}
function pxToData(px: number, py: number): [number, number] {
  const { w, h } = size(); const { xMin, xMax, yMin, yMax } = props.extents
  return [xMin + (px / w) * (xMax - xMin), yMin + (1 - py / h) * (yMax - yMin)]
}
function evtPx(e: MouseEvent): [number, number] {
  const r = canvasEl.value!.getBoundingClientRect()
  return [e.clientX - r.left, e.clientY - r.top]
}

// ── geometry helpers ──
const clone = (g: GateSpec): GateSpec => JSON.parse(JSON.stringify(g))
// rect data corners, CCW: 0:(min,min) 1:(max,min) 2:(max,max) 3:(min,max)
function rectCorners(g: GateSpec): [number, number][] {
  return [[g.x_min!, g.y_min!], [g.x_max!, g.y_min!], [g.x_max!, g.y_max!], [g.x_min!, g.y_max!]]
}
function dist(a: [number, number], b: [number, number]) { return Math.hypot(a[0] - b[0], a[1] - b[1]) }
function distToSeg(p: [number, number], a: [number, number], b: [number, number]) {
  const dx = b[0] - a[0], dy = b[1] - a[1]
  const l2 = dx * dx + dy * dy
  let t = l2 ? ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / l2 : 0
  t = Math.max(0, Math.min(1, t))
  return Math.hypot(p[0] - (a[0] + t * dx), p[1] - (a[1] + t * dy))
}
function pointInPoly(p: [number, number], poly: [number, number][]) {
  let inside = false
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const [xi, yi] = poly[i], [xj, yj] = poly[j]
    if (((yi > p[1]) !== (yj > p[1])) && (p[0] < ((xj - xi) * (p[1] - yi)) / (yj - yi) + xi)) inside = !inside
  }
  return inside
}

// ── hit testing for editing ──
type Handle =
  | { path: string; kind: 'rect-move' } | { path: string; kind: 'rect-corner'; ix: number }
  | { path: string; kind: 'rect-edge'; edge: number }
  | { path: string; kind: 'poly-move' } | { path: string; kind: 'poly-vertex'; vi: number }
  | { path: string; kind: 'poly-edge'; ei: number }

function hitGate(entry: { path: string; gate: GateSpec }, p: [number, number]): Handle | null {
  const g = entry.gate
  if (g.kind === 'rectangle') {
    const cpx = rectCorners(g).map(c => dataToPx(c[0], c[1])) as [number, number][]
    for (let ix = 0; ix < 4; ix++) if (dist(cpx[ix], p) < HANDLE_R) return { path: entry.path, kind: 'rect-corner', ix }
    for (let e = 0; e < 4; e++) if (distToSeg(p, cpx[e], cpx[(e + 1) % 4]) < HANDLE_R) return { path: entry.path, kind: 'rect-edge', edge: e }
    if (pointInPoly(p, cpx)) return { path: entry.path, kind: 'rect-move' }
  } else if (g.kind === 'polygon' && g.vertices) {
    const vpx = g.vertices.map(v => dataToPx(v[0], v[1])) as [number, number][]
    for (let vi = 0; vi < vpx.length; vi++) if (dist(vpx[vi], p) < HANDLE_R) return { path: entry.path, kind: 'poly-vertex', vi }
    for (let ei = 0; ei < vpx.length; ei++) if (distToSeg(p, vpx[ei], vpx[(ei + 1) % vpx.length]) < HANDLE_R) return { path: entry.path, kind: 'poly-edge', ei }
    if (pointInPoly(p, vpx)) return { path: entry.path, kind: 'poly-move' }
  }
  return null
}
function hitTest(p: [number, number]): Handle | null {
  for (const e of props.gates ?? []) { const h = hitGate(e, p); if (h) return h }
  return null
}
function cursorFor(h: Handle | null): string {
  if (!h) return 'default'
  if (h.kind === 'rect-corner') return h.ix % 2 === 0 ? 'nwse-resize' : 'nesw-resize'
  if (h.kind === 'rect-edge') return h.edge % 2 === 0 ? 'ns-resize' : 'ew-resize'
  if (h.kind === 'poly-vertex') return 'grab'
  if (h.kind === 'poly-edge') return 'copy'
  return 'move'
}

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

// ── render ──
function strokeShape(g: GateSpec, colour: string, lw = props.lineWidth) {
  const c = ctx!; c.strokeStyle = colour; c.lineWidth = lw; c.beginPath()
  if (g.kind === 'rectangle') {
    const cpx = rectCorners(g).map(p => dataToPx(p[0], p[1]))
    c.moveTo(cpx[0][0], cpx[0][1]); for (let i = 1; i < 4; i++) c.lineTo(cpx[i][0], cpx[i][1]); c.closePath()
  } else if (g.kind === 'polygon' && g.vertices) {
    g.vertices.forEach((v, i) => { const [px, py] = dataToPx(v[0], v[1]); i === 0 ? c.moveTo(px, py) : c.lineTo(px, py) })
    c.closePath()
  }
  c.stroke()
}
// subtle population-name label centred just above the gate's top edge
function drawGateLabel(g: GateSpec, path: string, colour: string) {
  const pts = (g.kind === 'rectangle' ? rectCorners(g) : (g.vertices ?? [])).map(p => dataToPx(p[0], p[1]))
  if (!pts.length) return
  const xs = pts.map(p => p[0]), ys = pts.map(p => p[1])
  const cx = (Math.min(...xs) + Math.max(...xs)) / 2, top = Math.min(...ys)
  const name = path.split('/').filter(Boolean).pop() ?? ''
  const c = ctx!; c.save()
  c.font = 'bold 12px system-ui, sans-serif'; c.textAlign = 'center'; c.textBaseline = 'bottom'
  c.lineJoin = 'round'; c.lineWidth = 3; c.strokeStyle = 'rgba(0,0,0,0.7)'   // dark halo for legibility
  c.strokeText(name, cx, top - 4)
  c.fillStyle = colour; c.fillText(name, cx, top - 4); c.restore()
}
function drawHandles(g: GateSpec, colour: string) {
  const c = ctx!; c.fillStyle = '#fff'; c.strokeStyle = colour; c.lineWidth = 1.5
  const sq = (px: number, py: number) => { c.beginPath(); c.rect(px - 3, py - 3, 6, 6); c.fill(); c.stroke() }
  const dot = (px: number, py: number) => { c.beginPath(); c.arc(px, py, 4, 0, 2 * Math.PI); c.fill(); c.stroke() }
  if (g.kind === 'rectangle') {
    const cpx = rectCorners(g).map(p => dataToPx(p[0], p[1]))
    cpx.forEach(p => sq(p[0], p[1]))
    for (let e = 0; e < 4; e++) { const a = cpx[e], b = cpx[(e + 1) % 4]; sq((a[0] + b[0]) / 2, (a[1] + b[1]) / 2) }
  } else if (g.kind === 'polygon' && g.vertices) {
    g.vertices.forEach(v => { const [px, py] = dataToPx(v[0], v[1]); dot(px, py) })
  }
}
function draw() {
  if (!ctx || !canvasEl.value) return
  const dpr = window.devicePixelRatio || 1
  const { w, h } = size()
  canvasEl.value.width = Math.max(1, w * dpr); canvasEl.value.height = Math.max(1, h * dpr)
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.clearRect(0, 0, w, h)

  // existing gates (substitute the draft for the one being edited)
  for (const g of props.gates ?? []) {
    const spec = g.path === editPath && draft.value ? draft.value : g.gate
    strokeShape(spec, g.colour || '#fafafa')
    if (props.showLabels) drawGateLabel(spec, g.path, g.colour || '#fafafa')
  }
  // handles on the hovered / edited gate
  const active = editPath ? gateOf(editPath) : hover.value ? gateOf(hover.value.path) : undefined
  if (active && props.mode === 'off') {
    const spec = active.path === editPath && draft.value ? draft.value : active.gate
    drawHandles(spec, active.colour || '#a78bfa')
  }

  // in-progress NEW shape (accent)
  ctx.strokeStyle = '#a78bfa'; ctx.fillStyle = 'rgba(167,139,250,0.12)'; ctx.lineWidth = 1.5
  if (props.mode === 'rectangle' && start && cur) {
    const x = Math.min(start[0], cur[0]), y = Math.min(start[1], cur[1])
    ctx.fillRect(x, y, Math.abs(cur[0] - start[0]), Math.abs(cur[1] - start[1]))
    ctx.strokeRect(x, y, Math.abs(cur[0] - start[0]), Math.abs(cur[1] - start[1]))
  } else if (props.mode === 'polygon' && polyPts.value.length) {
    ctx.beginPath()
    polyPts.value.forEach((p, i) => (i === 0 ? ctx!.moveTo(p[0], p[1]) : ctx!.lineTo(p[0], p[1])))
    if (cur) ctx.lineTo(cur[0], cur[1])
    ctx.stroke()
    for (const p of polyPts.value) { ctx.beginPath(); ctx.arc(p[0], p[1], 3, 0, 2 * Math.PI); ctx.fill() }
  }
}

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
  }
  draft.value = d
}

function startEdit(h: Handle, p: [number, number]) {
  const entry = gateOf(h.path); if (!entry) return
  editPath = h.path; editHandle = h; orig = clone(entry.gate)
  dragStart = pxToData(p[0], p[1]); draft.value = clone(entry.gate)
  window.addEventListener('mousemove', onEditMove); window.addEventListener('mouseup', onEditUp)
}
function onEditMove(e: MouseEvent) { applyEdit(evtPx(e)); draw() }
function onEditUp() {
  window.removeEventListener('mousemove', onEditMove); window.removeEventListener('mouseup', onEditUp)
  if (editPath && draft.value) emit('edit', { path: editPath, gate: draft.value })
  editHandle = null; orig = null; dragStart = null
  // keep draft + editPath until the authoritative tree arrives (gates watch clears them) → no flash
}

// ── proximity: toggle overlay pointer-events so regl keeps pan/zoom in empty space ──
function onParentMove(e: MouseEvent) {
  if (props.mode !== 'off' || editHandle) return
  const h = hitTest(evtPx(e))
  hover.value = h
  pe.value = h ? 'auto' : 'none'
  cursor.value = cursorFor(h)
  draw()
}

// ── canvas handlers ──
function onDown(e: MouseEvent) {
  if (props.mode === 'rectangle') { dragging = true; start = evtPx(e); cur = start; return }
  if (props.mode === 'polygon') return
  const h = hover.value ?? hitTest(evtPx(e))      // off mode → begin editing
  if (h) startEdit(h, evtPx(e))
}
function onMove(e: MouseEvent) {
  if (props.mode === 'off') return
  cur = evtPx(e)
  if (props.mode === 'rectangle' && !dragging) return
  draw()
}
function onUp() {
  if (props.mode === 'rectangle' && dragging && start && cur) {
    dragging = false
    const [ax, ay] = pxToData(start[0], start[1]); const [bx, by] = pxToData(cur[0], cur[1])
    start = cur = null
    emit('draw', { kind: 'rectangle', x_min: Math.min(ax, bx), x_max: Math.max(ax, bx), y_min: Math.min(ay, by), y_max: Math.max(ay, by) })
  }
}
function onClick(e: MouseEvent) {
  if (props.mode !== 'polygon') return
  const p = evtPx(e)
  if (polyPts.value.length >= 3) {
    const [fx, fy] = polyPts.value[0]
    if (Math.hypot(p[0] - fx, p[1] - fy) < 10) return finishPolygon()
  }
  polyPts.value.push(p); draw()
}
function onDblClick(e: MouseEvent) {
  if (props.mode === 'polygon') return finishPolygon()
  // off mode: insert a vertex on the nearest polygon edge under the cursor
  const p = evtPx(e); const h = hitTest(p)
  if (h && h.kind === 'poly-edge') {
    const entry = gateOf(h.path); if (!entry?.gate.vertices) return
    const d = clone(entry.gate); const [dx, dy] = pxToData(p[0], p[1])
    d.vertices!.splice(h.ei + 1, 0, [dx, dy]); emit('edit', { path: h.path, gate: d })
  }
}
function onContextMenu(e: MouseEvent) {
  if (props.mode !== 'off') return
  const h = hitTest(evtPx(e))
  if (h && h.kind === 'poly-vertex') {
    const entry = gateOf(h.path); if (!entry?.gate.vertices || entry.gate.vertices.length <= 3) return
    e.preventDefault()
    const d = clone(entry.gate); d.vertices!.splice(h.vi, 1); emit('edit', { path: h.path, gate: d })
  }
}
function finishPolygon() {
  if (polyPts.value.length < 3) { polyPts.value = []; emit('cancel'); return }
  const verts = polyPts.value.map(p => pxToData(p[0], p[1])) as [number, number][]
  polyPts.value = []; cur = null
  emit('draw', { kind: 'polygon', vertices: verts })
}
function onKey(e: KeyboardEvent) {
  if (e.key === 'Escape') { dragging = false; start = cur = null; polyPts.value = []; emit('cancel'); draw() }
}

// drawing modes always capture; off mode toggles via proximity
watch(() => props.mode, m => { pe.value = m === 'off' ? 'none' : 'auto'; cursor.value = m === 'off' ? 'default' : 'crosshair' })
// authoritative gates arrived → drop the local draft (unless mid-drag)
watch(() => props.gates, () => { if (!editHandle) { draft.value = null; editPath = null } draw() }, { deep: true })
watch(() => [props.extents, props.viewTick, props.lineWidth, props.showLabels], draw, { deep: true })

onMounted(() => {
  ctx = canvasEl.value!.getContext('2d')
  draw()
  ro = new ResizeObserver(draw); ro.observe(canvasEl.value!)
  window.addEventListener('keydown', onKey)
  canvasEl.value!.parentElement?.addEventListener('mousemove', onParentMove)
})
onBeforeUnmount(() => {
  ro?.disconnect(); ro = null
  window.removeEventListener('keydown', onKey)
  window.removeEventListener('mousemove', onEditMove); window.removeEventListener('mouseup', onEditUp)
  canvasEl.value?.parentElement?.removeEventListener('mousemove', onParentMove)
})
</script>

<template>
  <canvas
    ref="canvasEl"
    class="gate-overlay"
    :style="{ pointerEvents: pe, cursor }"
    @mousedown="onDown" @mousemove="onMove" @mouseup="onUp"
    @click="onClick" @dblclick="onDblClick" @contextmenu="onContextMenu"
  />
</template>

<style scoped>
.gate-overlay { position: absolute; inset: 0; width: 100%; height: 100%; }
</style>
