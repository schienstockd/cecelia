<!--
  DrawSurface — the annotation overlay for BIDIRECTIONAL share-in (docs/todo/BIDIR_CONTEXT_PLAN.md
  Part 2). Sits DIRECTLY on the viewer (mounted as a peer of GridOverlay / StillOverlay inside
  ViewerWindow.vue), so users draw on what they're actually looking at — no modal, no separate
  window, no frozen frame. On Save, the parent captures the current WebGPU frame and posts the
  envelope; on Cancel, the marks are discarded. Rectangle / polygon / freehand tools; edit-mode
  (empty tool OR Shift-held) lets the user click-to-move + drag corners to resize a rect; a small
  ✕ per mark deletes it without disturbing the others.

  DIVISION OF LABOUR.
    • `utils/drawGeometry.ts` — the state machines (click-vs-drag, RDP simplify, polygon close).
    • `utils/captureAddress.ts` — the payload-relative 0..1 normalisation (frame-agnostic).
    • This SFC — DOM, hit-testing, edit UI, floating toolbar.
    • The parent (ViewerWindow) — capture the WebGPU pixels + build the address + POST.

  COORD SYSTEMS.
    • SVG viewBox = its own client CSS px, so pointer events run in what the user sees.
    • On save, marks are normalised to 0..1 against the SVG's own box (`rectToOverlayGeom` /
      `pointsToOverlayGeom` with the box dims). The parent's captured PNG shares the same view,
      so the same 0..1 coords land on the pixels.
-->
<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import {
  beginPoly, beginRect, beginStroke, addVertex, extendStroke, updateCursor, updateRect,
  finishPoly, finishRect, finishStroke,
  type Point,
} from '../utils/drawGeometry'
import {
  rectToOverlayGeom, pointsToOverlayGeom,
  type OverlayMark, type OverlayColor,
} from '../utils/captureAddress'
import { ANNOTATION_PALETTE, ANNOTATION_COLOR_ORDER, resolveMarkColor } from '../utils/overlayCompose'

const props = defineProps<{
  visible: boolean
  // Short label shown in the floating toolbar (e.g. "IMG1 · t=3 · z=5") — orientation only.
  addressLine?: string
  // Optional busy flag (parent is capturing + POSTing). Disables Save so a fast double-click
  // doesn't send twice.
  busy?: boolean
}>()
const emit = defineEmits<{
  // `notes` is the free-text context the user attaches to the whole capture (BIDIR follow-up
  // 2026-09-20): what they're pointing at + why, delivered to Claude alongside the pixels. Empty
  // string when nothing was typed — the parent decides whether to include it on the envelope.
  (e: 'save', payload: { overlay: OverlayMark[]; notes: string }): void
  (e: 'cancel'): void
}>()

// '' = EDIT mode (no draw tool armed — click a mark to move it, drag a rect corner to resize).
// Same convention as `modules/gate/GatePlotPanel.vue`: re-clicking the active chip disarms it, and
// disarmed = edit. Shift-held while a tool is armed also enters edit for one gesture.
type Tool = 'rect' | 'poly' | 'stroke' | ''
// Icons + tips match the gating draw-tool chips exactly (GatePlotPanel.vue :: DRAW_MODES), so a
// user who has drawn a gate reads this as the same action. Freehand is annotation-only (gating has
// no analogue); pi-pencil is the neighbouring icon in the primeicons "drawing" set.
const TOOL_OPTIONS: ChipOption[] = [
  { value: 'rect',   label: '', icon: 'pi pi-stop',      tip: 'Rectangle' },
  { value: 'poly',   label: '', icon: 'pi pi-share-alt', tip: 'Polygon (click vertices, Enter or click first vertex to close)' },
  { value: 'stroke', label: '', icon: 'pi pi-pencil',    tip: 'Freehand' },
]
// Default to freehand: it's the most natural first gesture ("scribble around this"), and the
// user can always click a different chip. Rect / poly were the earlier default because they read
// as "gate-like" — that mattered less than starting with the tool the user reaches for.
const tool = ref<Tool>('stroke')

// CVD-safe / microscopy-neutral palette (see utils/overlayCompose.ts). Five chips, coloured
// swatches — the value IS the palette name; the label carries the swatch via inline style so
// the picker looks like paint chips, not a text menu. Default `magenta`: it reads unambiguously
// on both dark microscopy (viewer share) and white plot composites (canvas share), so one
// default fits both surfaces. `DEFAULT_ANNOTATION_COLOR` stays `white` for legacy captures that
// have no `color` field — the ROUND-TRIP fallback and the UI's INITIAL PICK are two different
// jobs and can drift.
const COLOR_OPTIONS: ChipOption[] = ANNOTATION_COLOR_ORDER.map(name => ({
  value: name,
  label: '',
  tip: `Mark colour: ${name}`,
  swatch: ANNOTATION_PALETTE[name],
  accent: ANNOTATION_PALETTE[name],
}))
const color = ref<OverlayColor>('magenta')

// Committed marks + one live draft. Kept as three parallel refs — a discriminated union would need
// a class per kind and the state machines are already the source of truth.
const marks = ref<OverlayMark[]>([])
// Session-wide free-text notes on the capture — sent to Claude alongside the pixels + overlay.
// Replaces the per-mark `label` input from the shipped version: peers using this in practice have
// been colour-coding shapes (yellow = missed cells, white = caught cells) rather than typing a
// label per shape, and what they actually want to send is a SINGLE context line about the whole
// share ("look at the T-cell channel here, segmentation looks under-called").
const notes = ref('')
const rectDraft   = ref<ReturnType<typeof beginRect>   | null>(null)
const polyDraft   = ref<ReturnType<typeof beginPoly>   | null>(null)
const strokeDraft = ref<ReturnType<typeof beginStroke> | null>(null)
const draftKind = computed<Tool | null>(() =>
  rectDraft.value ? 'rect' : polyDraft.value ? 'poly' : strokeDraft.value ? 'stroke' : null)

// SVG root — we read its bounding box to convert pointer clientX/Y into SVG-box coords.
const svgRoot = ref<SVGSVGElement | null>(null)
// The SVG viewBox tracks its own client size. We update `boxW/boxH` on mount + ResizeObserver so
// the viewBox is 1:1 with CSS px — thresholds and hit-tests then live in CSS-px space directly and
// don't need per-frame rescaling.
const boxW = ref(0)
const boxH = ref(0)
function measureBox() {
  const r = svgRoot.value?.getBoundingClientRect()
  if (!r) return
  boxW.value = r.width; boxH.value = r.height
}
function toSvgPoint(ev: PointerEvent): Point {
  const r = svgRoot.value?.getBoundingClientRect()
  if (!r || r.width === 0) return [0, 0]
  return [ev.clientX - r.left, ev.clientY - r.top]
}

// A polygon closes when a click lands within CLOSE_RADIUS_PX of the first vertex — same visual
// radius as the on-screen marker. In CSS-px (viewBox is 1:1 with CSS), no scaling needed.
const CLOSE_RADIUS_PX = 15
function isNearFirstScreenPx(vertices: Point[], p: Point): boolean {
  if (vertices.length < 3) return false
  const dx = vertices[0][0] - p[0], dy = vertices[0][1] - p[1]
  return dx * dx + dy * dy <= CLOSE_RADIUS_PX * CLOSE_RADIUS_PX
}

function clearDraft() { rectDraft.value = null; polyDraft.value = null; strokeDraft.value = null }

// ── Hit test + edit drag (matches gating; see GateOverlay.vue :: hitTest / applyDrag) ───────────
type Handle =
  | { idx: number; kind: 'body' }
  | { idx: number; kind: 'rect-corner'; corner: 0 | 1 | 2 | 3 }   // 0=NW 1=NE 2=SE 3=SW
const CORNER_HIT_PX  = 8   // half-size of a corner hit-box (matches the on-screen handle)
const STROKE_NEAR_PX = 8   // click-tolerance around a stroke path — a 1-px line is unhittable

function rectFromMark(i: number): { x: number; y: number; w: number; h: number } | null {
  const m = marks.value[i]; if (m.kind !== 'rect') return null
  const g = m.geom as { x: number; y: number; w: number; h: number }
  return { x: g.x * boxW.value, y: g.y * boxH.value, w: g.w * boxW.value, h: g.h * boxH.value }
}
function pointsFromMark(i: number): Point[] | null {
  const m = marks.value[i]; if (m.kind !== 'poly' && m.kind !== 'stroke') return null
  const g = m.geom as { pts?: [number, number][] }
  return (g.pts ?? []).map(p => [p[0] * boxW.value, p[1] * boxH.value] as Point)
}
function pointInRect(p: Point, r: { x: number; y: number; w: number; h: number }): boolean {
  return p[0] >= r.x && p[0] <= r.x + r.w && p[1] >= r.y && p[1] <= r.y + r.h
}
function pointInPoly(p: Point, pts: Point[]): boolean {   // ray-cast, standard odd-crossings
  let inside = false
  for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
    const [xi, yi] = pts[i], [xj, yj] = pts[j]
    const intersect = ((yi > p[1]) !== (yj > p[1])) &&
      (p[0] < ((xj - xi) * (p[1] - yi)) / (yj - yi) + xi)
    if (intersect) inside = !inside
  }
  return inside
}
function distToPolyline2(p: Point, pts: Point[]): number {
  let best = Infinity
  for (let i = 1; i < pts.length; i++) {
    const [ax, ay] = pts[i - 1], [bx, by] = pts[i]
    const dx = bx - ax, dy = by - ay
    const denom = dx * dx + dy * dy
    let t = denom === 0 ? 0 : ((p[0] - ax) * dx + (p[1] - ay) * dy) / denom
    t = Math.max(0, Math.min(1, t))
    const cx = ax + t * dx, cy = ay + t * dy
    const ex = p[0] - cx, ey = p[1] - cy
    const d2 = ex * ex + ey * ey
    if (d2 < best) best = d2
  }
  return best
}
// Newest-first so a shape drawn on top of another is picked. Rect corners hit before body so a
// click on the corner resizes rather than moves.
function hitTest(p: Point): Handle | null {
  for (let i = marks.value.length - 1; i >= 0; i--) {
    const m = marks.value[i]
    if (m.kind === 'rect') {
      const r = rectFromMark(i)!
      const corners: [number, number][] = [[r.x, r.y], [r.x + r.w, r.y], [r.x + r.w, r.y + r.h], [r.x, r.y + r.h]]
      for (let c = 0; c < 4; c++) {
        if (Math.abs(p[0] - corners[c][0]) <= CORNER_HIT_PX &&
            Math.abs(p[1] - corners[c][1]) <= CORNER_HIT_PX)
          return { idx: i, kind: 'rect-corner', corner: c as 0 | 1 | 2 | 3 }
      }
      if (pointInRect(p, r)) return { idx: i, kind: 'body' }
    } else if (m.kind === 'poly') {
      const pts = pointsFromMark(i); if (pts && pointInPoly(p, pts)) return { idx: i, kind: 'body' }
    } else if (m.kind === 'stroke') {
      const pts = pointsFromMark(i); if (!pts) continue
      if (distToPolyline2(p, pts) <= STROKE_NEAR_PX * STROKE_NEAR_PX)
        return { idx: i, kind: 'body' }
    }
  }
  return null
}

// Edit-drag state — flat refs, not another union, since the handler chain is short.
const editHover = ref<Handle | null>(null)
let editDragging: Handle | null = null
let editStart: Point = [0, 0]
let editOrigMark: OverlayMark | null = null
const shiftHeld = ref(false)
const editModeActive = computed(() => tool.value === '' || shiftHeld.value)
function onKeyDownGlobal(ev: KeyboardEvent) { if (ev.key === 'Shift') shiftHeld.value = true }
function onKeyUpGlobal(ev: KeyboardEvent)   { if (ev.key === 'Shift') shiftHeld.value = false }
const clamp01 = (v: number) => Math.max(0, Math.min(1, v))
function applyEdit(cur: Point) {
  if (!editDragging || !editOrigMark || boxW.value === 0) return
  const dx = cur[0] - editStart[0], dy = cur[1] - editStart[1]
  const w = boxW.value, h = boxH.value
  const m = { ...editOrigMark }
  if (m.kind === 'rect') {
    const g = editOrigMark.geom as { x: number; y: number; w: number; h: number }
    if (editDragging.kind === 'body') {
      m.geom = { x: clamp01(g.x + dx / w), y: clamp01(g.y + dy / h), w: g.w, h: g.h }
    } else {
      // Corner resize: the OPPOSITE corner stays put; a drag past the anchor flips the sign cleanly.
      const px = { x: g.x * w, y: g.y * h, X: (g.x + g.w) * w, Y: (g.y + g.h) * h }
      const anchor = editDragging.corner === 0 ? [px.X, px.Y]
                   : editDragging.corner === 1 ? [px.x, px.Y]
                   : editDragging.corner === 2 ? [px.x, px.y]
                   :                              [px.X, px.y]
      const moving = editDragging.corner === 0 ? [px.x + dx, px.y + dy]
                   : editDragging.corner === 1 ? [px.X + dx, px.y + dy]
                   : editDragging.corner === 2 ? [px.X + dx, px.Y + dy]
                   :                              [px.x + dx, px.Y + dy]
      const x0 = Math.min(anchor[0], moving[0]), y0 = Math.min(anchor[1], moving[1])
      const x1 = Math.max(anchor[0], moving[0]), y1 = Math.max(anchor[1], moving[1])
      m.geom = { x: clamp01(x0 / w), y: clamp01(y0 / h),
                 w: clamp01((x1 - x0) / w), h: clamp01((y1 - y0) / h) }
    }
  } else if (m.kind === 'poly' || m.kind === 'stroke') {
    const g = editOrigMark.geom as { pts: [number, number][] }
    m.geom = { pts: g.pts.map(([x, y]) => [clamp01(x + dx / w), clamp01(y + dy / h)] as [number, number]) }
  }
  marks.value = marks.value.map((mm, j) => j === editDragging!.idx ? m : mm)
}
function cursorForHandle(h: Handle | null): string {
  if (!h) return editModeActive.value ? 'default' : 'crosshair'
  if (h.kind === 'body') return 'move'
  return (h.corner === 0 || h.corner === 2) ? 'nwse-resize' : 'nesw-resize'
}
const svgCursor = computed(() => cursorForHandle(editDragging ?? editHover.value))

// ── Pointer events ─────────────────────────────────────────────────────────────────────────────
function onPointerDown(ev: PointerEvent) {
  if (ev.button !== 0) return
  const p = toSvgPoint(ev)
  if (editModeActive.value) {
    const h = hitTest(p)
    if (h) {
      editDragging = h; editStart = p; editOrigMark = { ...marks.value[h.idx] }
      svgRoot.value?.setPointerCapture?.(ev.pointerId)
      return
    }
    if (tool.value === '') return   // edit mode + missed = nothing to do
  }
  const t = tool.value
  if (t === 'rect')   rectDraft.value = beginRect(p)
  if (t === 'stroke') strokeDraft.value = beginStroke(p)
  if (t === 'poly') {
    if (!polyDraft.value) polyDraft.value = addVertex(beginPoly(), p)
    else if (isNearFirstScreenPx(polyDraft.value.vertices, p)) commitPoly()
    else polyDraft.value = addVertex(polyDraft.value, p)
  }
  svgRoot.value?.setPointerCapture?.(ev.pointerId)
}
function onPointerMove(ev: PointerEvent) {
  const p = toSvgPoint(ev)
  if (editDragging) { applyEdit(p); return }
  if (editModeActive.value && !draftKind.value) editHover.value = hitTest(p)
  if (rectDraft.value)   rectDraft.value = updateRect(rectDraft.value, p)
  if (strokeDraft.value) strokeDraft.value = extendStroke(strokeDraft.value, p)
  if (polyDraft.value)   polyDraft.value = updateCursor(polyDraft.value, p)
}
function onPointerUp(ev: PointerEvent) {
  const p = toSvgPoint(ev)
  if (editDragging) {
    editDragging = null; editOrigMark = null
    svgRoot.value?.releasePointerCapture?.(ev.pointerId)
    return
  }
  if (rectDraft.value) {
    const r = finishRect(rectDraft.value, p)
    if (r) marks.value.push({ kind: 'rect',
      geom: rectToOverlayGeom(r, boxW.value, boxH.value),
      color: color.value })
    rectDraft.value = null
  }
  if (strokeDraft.value) {
    const pts = finishStroke(strokeDraft.value)
    if (pts.length >= 2) marks.value.push({ kind: 'stroke',
      geom: pointsToOverlayGeom(pts, boxW.value, boxH.value),
      color: color.value })
    strokeDraft.value = null
  }
  svgRoot.value?.releasePointerCapture?.(ev.pointerId)
}
function onDblClick() { if (polyDraft.value) commitPoly() }
function commitPoly() {
  const pts = finishPoly(polyDraft.value!)
  if (pts) marks.value.push({ kind: 'poly',
    geom: pointsToOverlayGeom(pts, boxW.value, boxH.value),
    color: color.value })
  polyDraft.value = null
}

// ── Actions ────────────────────────────────────────────────────────────────────────────────────
function undo() { if (marks.value.length) marks.value = marks.value.slice(0, -1); clearDraft() }
function clearAll() { marks.value = []; notes.value = ''; clearDraft() }
function save() { emit('save', { overlay: marks.value, notes: notes.value.trim() }); clearAll() }
function cancel() { emit('cancel'); clearAll() }
function removeMark(i: number) { marks.value = marks.value.filter((_, j) => j !== i) }
function onKey(ev: KeyboardEvent) {
  if (!props.visible) return
  if (ev.key === 'Enter'  && polyDraft.value) { commitPoly(); ev.preventDefault() }
  if (ev.key === 'Escape') { draftKind.value ? clearDraft() : cancel(); ev.preventDefault() }
}

// ── Lifecycle ──────────────────────────────────────────────────────────────────────────────────
// Measure on visibility change and on window resize. A ResizeObserver here would be structurally
// safe (no DOM write in the callback, so no self-resize loop) but the ratchet in
// `utils/continuousControls.test.ts` mandates every new ResizeObserver goes through
// `usePlotResize` — a size-guarded observer that only makes sense when you actually RENDER on
// resize. We don't; we only recompute a coord frame, and a `resize` on the pop-out window is the
// event that actually matters. Cheaper too — no observer, no rAF.
onMounted(() => {
  window.addEventListener('keydown', onKey)
  window.addEventListener('keydown', onKeyDownGlobal)
  window.addEventListener('keyup',   onKeyUpGlobal)
  window.addEventListener('resize',  measureBox)
})
onBeforeUnmount(() => {
  window.removeEventListener('keydown', onKey)
  window.removeEventListener('keydown', onKeyDownGlobal)
  window.removeEventListener('keyup',   onKeyUpGlobal)
  window.removeEventListener('resize',  measureBox)
})
// Reset marks + measure on open.
watch(() => props.visible, async (v) => {
  if (!v) return
  marks.value = []; notes.value = ''; clearDraft()
  tool.value = 'stroke'
  color.value = 'magenta'
  await Promise.resolve()   // let the DOM mount before measuring
  measureBox()
}, { immediate: true })

// ── Render helpers ─────────────────────────────────────────────────────────────────────────────
const draftRect = computed(() => {
  const d = rectDraft.value; if (!d) return null
  return {
    x: Math.min(d.start[0], d.cur[0]), y: Math.min(d.start[1], d.cur[1]),
    w: Math.abs(d.cur[0] - d.start[0]), h: Math.abs(d.cur[1] - d.start[1]),
  }
})
const draftPolyPath = computed(() => {
  const d = polyDraft.value; if (!d || !d.vertices.length) return ''
  const withCursor = d.cursor ? [...d.vertices, d.cursor] : d.vertices
  return withCursor.map((p, i) => (i === 0 ? 'M' : 'L') + p[0] + ',' + p[1]).join(' ')
})
const draftStrokePath = computed(() => {
  const d = strokeDraft.value; if (!d || !d.pts.length) return ''
  return d.pts.map((p, i) => (i === 0 ? 'M' : 'L') + p[0] + ',' + p[1]).join(' ')
})
const committedShapes = computed(() => marks.value.map((m, i) => {
  const g = m.geom as Record<string, number> & { pts?: [number, number][] }
  const w = boxW.value, h = boxH.value
  const stroke = resolveMarkColor(m)
  if (m.kind === 'rect') {
    const x = g.x * w, y = g.y * h, width = g.w * w, height = g.h * h
    return { key: i, idx: i, kind: 'rect', label: m.label, stroke, x, y, width, height,
             labelX: x, labelY: y - 6, deleteX: x + width, deleteY: y }
  }
  if (m.kind === 'poly' || m.kind === 'stroke') {
    const pts = (g.pts ?? []).map(p => [p[0] * w, p[1] * h] as [number, number])
    const cmd = pts.map((p, j) => (j === 0 ? 'M' : 'L') + p[0] + ',' + p[1]).join(' ')
    const x0 = pts[0]?.[0] ?? 0, y0 = pts[0]?.[1] ?? 0
    return { key: i, idx: i, kind: m.kind, label: m.label, stroke,
             d: m.kind === 'poly' ? cmd + ' Z' : cmd,
             labelX: x0, labelY: y0 - 6, deleteX: x0, deleteY: y0 }
  }
  return { key: i, idx: i, kind: 'unknown', stroke } as never
}))
// Live draft strokes use the current colour so the WIP shape reads with the same identity as the
// committed mark it becomes. Not tied to `resolveMarkColor` — the draft has no mark yet.
const draftStroke = computed(() => ANNOTATION_PALETTE[color.value])
const firstVertexMarker = computed(() => {
  const d = polyDraft.value; if (!d || !d.vertices.length) return null
  const [x, y] = d.vertices[0]
  const armed = d.vertices.length >= 3
  return { x, y, radius: armed ? CLOSE_RADIUS_PX : 6, armed }
})
</script>

<template>
  <div v-if="visible" class="ds-root">
    <!-- Drawing surface FIRST in DOM so the toolbar and edit-mode hint (rendered after) paint on
         top of it — otherwise a mark drawn near the top-left would cover the tool chips, and
         clicks on the chips would land on the SVG instead. Same-z-index elements paint in DOM
         order; putting the toolbar after the SVG is what gives it the top layer, without a manual
         z-index shuffle. -->
    <svg ref="svgRoot" class="ds-svg" :viewBox="`0 0 ${boxW || 1} ${boxH || 1}`"
         preserveAspectRatio="none" :style="{ cursor: svgCursor }"
         @pointerdown="onPointerDown" @pointermove="onPointerMove" @pointerup="onPointerUp"
         @dblclick="onDblClick">
      <g class="ds-committed">
        <template v-for="s in committedShapes" :key="s.key">
          <rect v-if="s.kind === 'rect'" :x="s.x" :y="s.y" :width="s.width" :height="s.height"
                :style="{ stroke: s.stroke }" />
          <path v-else-if="s.kind === 'poly' || s.kind === 'stroke'" :d="s.d"
                :style="{ stroke: s.stroke }" />
          <text v-if="s.label" :x="s.labelX" :y="s.labelY" :style="{ fill: s.stroke }">{{ s.label }}</text>
          <g class="ds-delete" @pointerdown.stop @click.stop="removeMark(s.idx)"
             v-tooltip.top="'Delete this mark'">
            <circle :cx="s.deleteX" :cy="s.deleteY" :r="7" />
            <text :x="s.deleteX" :y="s.deleteY + 2" text-anchor="middle"
                  dominant-baseline="middle" font-size="10">×</text>
          </g>
        </template>
      </g>
      <rect v-if="draftKind === 'rect' && draftRect" class="ds-draft"
            :style="{ stroke: draftStroke }"
            :x="draftRect.x" :y="draftRect.y" :width="draftRect.w" :height="draftRect.h" />
      <path v-if="draftKind === 'poly' && draftPolyPath" class="ds-draft"
            :style="{ stroke: draftStroke }" :d="draftPolyPath" />
      <path v-if="draftKind === 'stroke' && draftStrokePath" class="ds-draft"
            :style="{ stroke: draftStroke }" :d="draftStrokePath" />
      <circle v-if="firstVertexMarker" class="ds-poly-first"
              :class="{ armed: firstVertexMarker.armed }" :style="{ stroke: draftStroke }"
              :cx="firstVertexMarker.x" :cy="firstVertexMarker.y" :r="firstVertexMarker.radius" />
      <g v-if="editModeActive" class="ds-corners">
        <template v-for="s in committedShapes" :key="'c' + s.key">
          <template v-if="s.kind === 'rect'">
            <rect v-for="(c, ci) in [[s.x, s.y], [s.x + s.width, s.y],
                                      [s.x + s.width, s.y + s.height], [s.x, s.y + s.height]]"
                  :key="ci" :x="c[0] - 6" :y="c[1] - 6" :width="12" :height="12" />
          </template>
        </template>
      </g>
    </svg>

    <!-- Floating toolbar (after the SVG so it paints on top of any marks; opaque background so a
         white mark drawn behind it can't leak through). -->
    <div class="ds-toolbar cc-row cc-row-tight">
      <ChipSelect variant="segmented" allow-empty :options="TOOL_OPTIONS" :model-value="tool"
                  @update:modelValue="(v: string | string[]) =>
                    tool = (Array.isArray(v) ? v[0] : v) as Tool" />
      <ChipSelect variant="segmented" :options="COLOR_OPTIONS" :model-value="color"
                  aria-label="Mark colour"
                  @update:modelValue="(v: string | string[]) =>
                    color = (Array.isArray(v) ? v[0] : v) as OverlayColor" />
      <textarea class="cc-input ds-notes-input" v-model="notes" rows="1" maxlength="800"
                placeholder="Notes for Claude (optional)"
                v-tooltip.bottom="'Free-text context sent with the frame — what you are pointing at + why'"></textarea>
      <button v-if="draftKind === 'poly' && (polyDraft?.vertices.length ?? 0) >= 3"
              class="cc-btn cc-btn-ghost cc-btn-sm" @click="commitPoly"
              v-tooltip.bottom="'Close the polygon (or press Enter, or click near the first vertex)'">Finish</button>
      <button class="cc-btn cc-btn-ghost cc-btn-sm" :disabled="!marks.length" @click="undo"
              v-tooltip.bottom="'Remove the last mark'">Undo</button>
      <button class="cc-btn cc-btn-ghost cc-btn-sm" :disabled="!marks.length" @click="clearAll"
              v-tooltip.bottom="'Remove all marks'">Clear</button>
      <span class="cc-spacer" />
      <span v-if="addressLine" class="ds-address cc-fs-2xs">{{ addressLine }}</span>
      <button class="cc-btn cc-btn-ghost" @click="cancel" v-tooltip.bottom="'Close without saving (Esc)'">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="busy" @click="save"
              v-tooltip.bottom="marks.length
                ? 'Save the frame + overlay for Claude'
                : 'Share the whole frame — no region specified'">
        <i v-if="busy" class="pi pi-spin pi-spinner" />
        <span v-else>{{ marks.length ? 'Save' : 'Save frame' }}</span>
      </button>
    </div>
    <span v-if="editModeActive && !draftKind" class="ds-mode cc-fs-2xs">
      <i class="pi pi-arrows-alt" /> Edit — click a mark to move, drag a corner to resize
    </span>
  </div>
</template>

<style scoped>
/* The root fills its parent (mounted inside .vw-canvas-wrap, a peer of the canvas). Only the
   toolbar itself receives pointer events; the SVG receives them too (for drawing). The rest of
   the root is transparent to hover states. */
.ds-root { position: absolute; inset: 0; pointer-events: none; z-index: 20; }
.ds-toolbar {
  position: absolute; top: 0.5rem; left: 0.5rem; right: 0.5rem;
  padding: 0.35rem 0.5rem;
  /* Opaque — a semi-transparent background lets a white mark drawn behind it show through. */
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  pointer-events: auto;
}
.ds-notes-input {
  flex: 1; min-width: 12rem; max-width: 30rem;
  min-height: 1.6rem; max-height: 4.5rem;
  resize: vertical;
  font-family: inherit;   /* textarea default is monospace on some UAs */
}
.ds-address { color: var(--cc-text-dim); font-family: var(--cc-mono); }
.ds-mode {
  position: absolute; top: 3.4rem; left: 0.75rem;
  padding: 0.2rem 0.5rem;
  background: rgba(0, 0, 0, 0.55); color: var(--cc-text-dim);
  border-radius: var(--cc-radius-md);
  pointer-events: none;
}
.ds-svg {
  position: absolute; inset: 0; width: 100%; height: 100%;
  touch-action: none; pointer-events: auto;
}
/* Committed marks: white stroke with a dark halo — visible on both dark and bright pixels. */
.ds-committed rect, .ds-committed path {
  fill: none; stroke: #fff; stroke-width: 2; vector-effect: non-scaling-stroke;
  paint-order: stroke;
}
.ds-committed text {
  fill: #fff; stroke: rgba(0, 0, 0, 0.7); stroke-width: 3; paint-order: stroke;
  font-size: var(--cc-fs-sm); font-family: var(--cc-mono);
  vector-effect: non-scaling-stroke;
}
.ds-draft { fill: rgba(255, 255, 255, 0.08); stroke: var(--cc-accent); stroke-width: 2;
            stroke-dasharray: 4 4; vector-effect: non-scaling-stroke; }
.ds-poly-first { fill: none; stroke: var(--cc-accent); stroke-width: 2;
                 stroke-dasharray: 3 3; vector-effect: non-scaling-stroke; }
.ds-poly-first.armed { fill: var(--cc-accent); fill-opacity: 0.25; stroke-dasharray: none; }
.ds-corners rect {
  fill: #fff; stroke: rgba(0, 0, 0, 0.75); stroke-width: 1.5; vector-effect: non-scaling-stroke;
}
.ds-delete { cursor: pointer; }
.ds-delete circle { fill: rgba(0, 0, 0, 0.7); stroke: #fff; stroke-width: 1.5;
                    vector-effect: non-scaling-stroke; }
.ds-delete:hover circle { fill: var(--cc-accent); }
.ds-delete text { fill: #fff; stroke: none; font-family: var(--cc-mono); font-weight: bold;
                  pointer-events: none; }
</style>
