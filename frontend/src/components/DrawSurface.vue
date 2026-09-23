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
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import TeleportPopover from './TeleportPopover.vue'
import {
  beginPoly, beginRect, beginStroke, addVertex, extendStroke, updateCursor, updateRect,
  finishPoly, finishRect, finishStroke,
  type Point,
} from '../utils/drawGeometry'
import {
  rectToOverlayGeom, pointsToOverlayGeom,
  type OverlayMark, type OverlayColor, type OverlayStrokeWidth,
} from '../utils/captureAddress'
import {
  ANNOTATION_PALETTE, ANNOTATION_COLOR_ORDER, resolveMarkColor,
  ANNOTATION_STROKE_WIDTH_ORDER, DEFAULT_STROKE_WIDTH,
} from '../utils/overlayCompose'

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

// Stroke thickness chips — three presets (thin / medium / thick), mapped through
// `STROKE_WIDTH_SCALES` at composite time so a preset scales with the frame width. Compact S/M/L
// labels keep the toolbar tight next to the 5-swatch colour strip; tooltips carry the full name.
const STROKE_WIDTH_LABEL: Record<OverlayStrokeWidth, string> = { thin: 'Thin', medium: 'Medium', thick: 'Thick' }
const STROKE_WIDTH_SHORT: Record<OverlayStrokeWidth, string> = { thin: 'S',    medium: 'M',      thick: 'L'     }
const SIZE_OPTIONS: ChipOption[] = ANNOTATION_STROKE_WIDTH_ORDER.map(name => ({
  value: name,
  label: STROKE_WIDTH_SHORT[name],
  tip: `Stroke thickness: ${STROKE_WIDTH_LABEL[name]}`,
}))
const size = ref<OverlayStrokeWidth>('medium')

// SVG preview stroke-width (in CSS px) for each preset. Not the composite width — the SVG
// canvas is always the DrawSurface's own client box, and the composite scales by the frame's
// pixel width at export time. Kept in sync with `STROKE_WIDTH_SCALES` proportionally so the
// on-screen mark reads as the same relative weight the composite will render.
const SVG_STROKE_PX: Record<OverlayStrokeWidth, number> = { thin: 1, medium: 2, thick: 4 }
const strokeWidthFor = (m: OverlayMark): number =>
  SVG_STROKE_PX[m.strokeWidth ?? DEFAULT_STROKE_WIDTH]
const draftStrokeWidth = computed(() => SVG_STROKE_PX[size.value])

// Committed marks + one live draft. Kept as three parallel refs — a discriminated union would need
// a class per kind and the state machines are already the source of truth.
const marks = ref<OverlayMark[]>([])
// Session-wide free-text notes on the capture — sent to Claude alongside the pixels + overlay.
// Replaces the per-mark `label` input from the shipped version: peers using this in practice have
// been colour-coding shapes (yellow = missed cells, white = caught cells) rather than typing a
// label per shape, and what they actually want to send is a SINGLE context line about the whole
// share ("look at the T-cell channel here, segmentation looks under-called").
const notes = ref('')
// Toolbar has a single-line input for the short case; the ⤢ button opens a popover with a real
// textarea for longer notes. Both bind to `notes` — typing in either syncs. Kept as a popover
// (not a full BaseModal) to stay close to the frame the user is annotating.
const notesExpanded = ref(false)
const notesExpandBtn = ref<HTMLElement | null>(null)
const notesTextarea = ref<HTMLTextAreaElement | null>(null)
// A single-line <input> silently swallows `\n` — a two-line note authored in the popover shows as
// two words run together in the toolbar. When notes contain a newline, swap the input for a
// read-only preview that separates lines with ` · ` and opens the popover on click.
const notesHasNewlines = computed(() => /\n/.test(notes.value))
const notesPreview = computed(() => notes.value.replace(/\s*\n+\s*/g, ' · '))
watch(notesExpanded, async (open) => {
  if (!open) return
  await nextTick()
  notesTextarea.value?.focus()
  // Place caret at end so a user who typed something inline can keep going in the popover.
  const el = notesTextarea.value
  if (el) el.setSelectionRange(el.value.length, el.value.length)
})
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
  | { idx: number; kind: 'rect-corner'; corner: 0 | 1 | 2 | 3 }    // 0=NW 1=NE 2=SE 3=SW
  | { idx: number; kind: 'bbox-corner'; corner: 0 | 1 | 2 | 3 }    // poly / stroke bbox — scales all vertices around the opposite corner
  | { idx: number; kind: 'poly-vertex'; vertex: number }           // draggable polygon vertex (finer than bbox-corner; hits first)
  | { idx: number; kind: 'rotate' }                                // rotate handle (any kind)
const CORNER_HIT_PX  = 8   // half-size of a corner hit-box (matches the on-screen handle)
const VERTEX_HIT_PX  = 8   // radius around a polygon vertex handle — mirrors CORNER_HIT_PX
const STROKE_NEAR_PX = 8   // click-tolerance around a stroke path — a 1-px line is unhittable
const ROTATE_HANDLE_OFFSET_PX = 22   // rotate handle sits this far above the AA top edge
const ROTATE_HIT_PX  = 10  // hit radius around the rotate handle centre
// ✕ sits diagonally OUTSIDE the top-right corner handle — same offset for both rect and
// poly/stroke bbox. Chosen so the ✕ (radius 7) and the corner handle (half-size 6) do not overlap
// and a resize drag on the corner is unambiguous.
const DELETE_OFFSET_PX = 10

// Rotation helpers. Rotation is stored in degrees, clockwise, around the mark's centroid; the
// SVG surface wraps the shape + its handles in a `<g transform="rotate(deg cx cy)">` so both the
// visual AND the click targets share the same transform. `unrotateAround` maps a SCREEN point
// back into the mark's LOCAL (axis-aligned) frame so hit-test and drag math can stay AA.
const DEG2RAD = Math.PI / 180
function unrotateAround(p: Point, cx: number, cy: number, deg: number): Point {
  if (!deg) return p
  const rad = -deg * DEG2RAD, dx = p[0] - cx, dy = p[1] - cy
  const c = Math.cos(rad), s = Math.sin(rad)
  return [cx + dx * c - dy * s, cy + dx * s + dy * c]
}
function unrotateVec(v: Point, deg: number): Point {
  if (!deg) return v
  const rad = -deg * DEG2RAD, c = Math.cos(rad), s = Math.sin(rad)
  return [v[0] * c - v[1] * s, v[0] * s + v[1] * c]
}
function rectCentroidPx(i: number): Point | null {
  const r = rectFromMark(i); if (!r) return null
  return [r.x + r.w / 2, r.y + r.h / 2]
}
/** AA bounding box of a poly / stroke in DOM px. `null` for degenerate shapes (empty vertex
 *  list) so the caller can skip rendering handles rather than crash on Infinity math. */
function polyBboxPx(i: number): { x: number; y: number; w: number; h: number } | null {
  const pts = pointsFromMark(i); if (!pts || pts.length === 0) return null
  let xMin = Infinity, yMin = Infinity, xMax = -Infinity, yMax = -Infinity
  for (const [x, y] of pts) {
    if (x < xMin) xMin = x; if (x > xMax) xMax = x
    if (y < yMin) yMin = y; if (y > yMax) yMax = y
  }
  return { x: xMin, y: yMin, w: xMax - xMin, h: yMax - yMin }
}
/** Centroid of a mark in DOM px (rect centre, or poly/stroke bbox centre). Used as the rotation
 *  origin so all kinds rotate the same way (matches `markCentroid01`'s composite-side rule). */
function markCentroidPx(i: number): Point | null {
  const m = marks.value[i]; if (!m) return null
  if (m.kind === 'rect') return rectCentroidPx(i)
  const b = polyBboxPx(i); if (!b) return null
  return [b.x + b.w / 2, b.y + b.h / 2]
}
function markRotate(m: OverlayMark): number {
  return typeof m.rotate === 'number' && isFinite(m.rotate) ? m.rotate : 0
}

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
// Newest-first so a shape drawn on top of another is picked. Corner / vertex handles hit before
// bodies so a click on a handle resizes or reshapes rather than moves. Poly vertex handles are
// only offered on the SELECTED poly — otherwise every poly would have to render its full vertex
// set in edit mode and the surface would quickly turn into a dot field. Rect corners stay
// always-on because a rect only has four handles regardless.
function hitTest(p: Point): Handle | null {
  for (let i = marks.value.length - 1; i >= 0; i--) {
    const m = marks.value[i]
    const rot = markRotate(m)
    const cent = markCentroidPx(i)
    // Every hit-test check below runs in the mark's LOCAL (un-rotated) frame — unrotate the
    // pointer once, up front, so a rotated shape's handles + body are all checked in AA coords.
    const pL = cent ? unrotateAround(p, cent[0], cent[1], rot) : p
    if (m.kind === 'rect') {
      const r = rectFromMark(i)!
      if (i === selectedIdx.value) {
        const rhx = r.x + r.w / 2, rhy = r.y - ROTATE_HANDLE_OFFSET_PX
        const dx = pL[0] - rhx, dy = pL[1] - rhy
        if (dx * dx + dy * dy <= ROTATE_HIT_PX * ROTATE_HIT_PX)
          return { idx: i, kind: 'rotate' }
      }
      const corners: [number, number][] = [[r.x, r.y], [r.x + r.w, r.y], [r.x + r.w, r.y + r.h], [r.x, r.y + r.h]]
      for (let c = 0; c < 4; c++) {
        if (Math.abs(pL[0] - corners[c][0]) <= CORNER_HIT_PX &&
            Math.abs(pL[1] - corners[c][1]) <= CORNER_HIT_PX)
          return { idx: i, kind: 'rect-corner', corner: c as 0 | 1 | 2 | 3 }
      }
      if (pointInRect(pL, r)) return { idx: i, kind: 'body' }
    } else if (m.kind === 'poly' || m.kind === 'stroke') {
      const pts = pointsFromMark(i); if (!pts) continue
      // A rotated poly/stroke stores AA vertices in local coords, so once `pL` is un-rotated
      // both the vertex list AND the bbox live in the same AA frame — no per-vertex rotate
      // math needed downstream, either.
      const ptsL = pts
      if (i === selectedIdx.value) {
        const bb = polyBboxPx(i)
        if (bb) {
          const rhx = bb.x + bb.w / 2, rhy = bb.y - ROTATE_HANDLE_OFFSET_PX
          const dx = pL[0] - rhx, dy = pL[1] - rhy
          if (dx * dx + dy * dy <= ROTATE_HIT_PX * ROTATE_HIT_PX)
            return { idx: i, kind: 'rotate' }
        }
        // Poly-vertex takes precedence over bbox-corner: a stray click on a bbox corner is
        // harmless (starts a resize), but stealing a vertex-precision drag would be worse. So
        // vertex handles first.
        if (m.kind === 'poly') {
          for (let v = 0; v < ptsL.length; v++) {
            const dx = pL[0] - ptsL[v][0], dy = pL[1] - ptsL[v][1]
            if (dx * dx + dy * dy <= VERTEX_HIT_PX * VERTEX_HIT_PX)
              return { idx: i, kind: 'poly-vertex', vertex: v }
          }
        }
        if (bb) {
          const bc: [number, number][] = [[bb.x, bb.y], [bb.x + bb.w, bb.y],
                                          [bb.x + bb.w, bb.y + bb.h], [bb.x, bb.y + bb.h]]
          for (let c = 0; c < 4; c++) {
            if (Math.abs(pL[0] - bc[c][0]) <= CORNER_HIT_PX &&
                Math.abs(pL[1] - bc[c][1]) <= CORNER_HIT_PX)
              return { idx: i, kind: 'bbox-corner', corner: c as 0 | 1 | 2 | 3 }
          }
        }
      }
      if (m.kind === 'poly') {
        if (pointInPoly(pL, ptsL)) return { idx: i, kind: 'body' }
      } else {
        if (distToPolyline2(pL, ptsL) <= STROKE_NEAR_PX * STROKE_NEAR_PX)
          return { idx: i, kind: 'body' }
      }
    }
  }
  return null
}

// Edit-drag state — flat refs, not another union, since the handler chain is short.
const editHover = ref<Handle | null>(null)
let editDragging: Handle | null = null
let editStart: Point = [0, 0]
let editOrigMark: OverlayMark | null = null
// The mark the toolbar chips are currently retargeting. A short click on a mark body in edit mode
// promotes it to selected — the chips then mutate that mark instead of just seeding new marks
// (PowerPoint-style: click a shape, then set colour / thickness). Cleared on empty-space click,
// on Escape, and when a new draw begins. `null` ⇒ chips control DEFAULTS for the next new mark.
const selectedIdx = ref<number | null>(null)
// Distance the pointer has moved since the current edit-drag began. Kept as a plain scalar (not a
// ref) because it isn't rendered — it only decides "was this a real drag or a click?" on release,
// so we don't want reactive churn every pointermove.
let editDragDistPx = 0
const shiftHeld = ref(false)
const editModeActive = computed(() => tool.value === '' || shiftHeld.value)
const selectedMark = computed(() =>
  selectedIdx.value !== null ? marks.value[selectedIdx.value] ?? null : null)
function onKeyDownGlobal(ev: KeyboardEvent) { if (ev.key === 'Shift') shiftHeld.value = true }
function onKeyUpGlobal(ev: KeyboardEvent)   { if (ev.key === 'Shift') shiftHeld.value = false }
const clamp01 = (v: number) => Math.max(0, Math.min(1, v))
// Minimum scaled bbox extent (frame-relative) — prevents a resize drag from collapsing the
// shape to a zero-area line the user can't grab again. Small enough to stay unobtrusive at any
// reasonable frame size.
const MIN_BBOX_EXTENT_01 = 0.01
function applyEdit(cur: Point) {
  if (!editDragging || !editOrigMark || boxW.value === 0) return
  const dxS = cur[0] - editStart[0], dyS = cur[1] - editStart[1]     // SCREEN delta
  const w = boxW.value, h = boxH.value
  const m = { ...editOrigMark }
  const rot = markRotate(editOrigMark)
  // Screen delta → local delta once, up front. Every non-rotate branch reads (dxL, dyL) so a
  // rotated shape's math stays AA in the mark's own frame.
  const [dxL, dyL] = unrotateVec([dxS, dyS], rot)

  // ── Rotate handle — applies to any kind, around the mark's centroid ──────────────────────
  if (editDragging.kind === 'rotate') {
    const cent = editOrigCentroidPx()
    if (!cent) return
    const startAng = Math.atan2(editStart[1] - cent[1], editStart[0] - cent[0]) / DEG2RAD
    const curAng   = Math.atan2(cur[1]        - cent[1], cur[0]        - cent[0]) / DEG2RAD
    let next = (editOrigMark.rotate ?? 0) + (curAng - startAng)
    // Normalise into (-180, 180] so a full-circle drag doesn't accumulate past ±360; the server
    // safelist accepts up to ±360 anyway but a tighter live value keeps the toolbar readable if
    // we ever surface a degree readout.
    while (next > 180)  next -= 360
    while (next <= -180) next += 360
    m.rotate = next
    marks.value = marks.value.map((mm, j) => j === editDragging!.idx ? m : mm)
    return
  }

  if (m.kind === 'rect') {
    const g = editOrigMark.geom as { x: number; y: number; w: number; h: number }
    if (editDragging.kind === 'body') {
      // Body drag is a pure translation and commutes with the rotation-around-centroid, so a
      // screen delta = a geometry delta. See the comment above: rotating around a translated
      // centroid returns the same shape, translated.
      m.geom = { x: clamp01(g.x + dxS / w), y: clamp01(g.y + dyS / h), w: g.w, h: g.h }
    } else if (editDragging.kind === 'rect-corner') {
      // Corner resize under rotation: (dxL, dyL) is already in the mark's LOCAL frame, so the
      // standard AA corner math (opposite corner stays anchored in local coords) still holds.
      // Screen-space appearance drifts a little because the centroid moves during the resize
      // and the rotation origin follows it, but the effect is small enough not to feel wrong.
      const corner = editDragging.corner
      const px = { x: g.x * w, y: g.y * h, X: (g.x + g.w) * w, Y: (g.y + g.h) * h }
      const anchor = corner === 0 ? [px.X, px.Y]
                   : corner === 1 ? [px.x, px.Y]
                   : corner === 2 ? [px.x, px.y]
                   :                [px.X, px.y]
      const moving = corner === 0 ? [px.x + dxL, px.y + dyL]
                   : corner === 1 ? [px.X + dxL, px.y + dyL]
                   : corner === 2 ? [px.X + dxL, px.Y + dyL]
                   :                [px.x + dxL, px.Y + dyL]
      const x0 = Math.min(anchor[0], moving[0]), y0 = Math.min(anchor[1], moving[1])
      const x1 = Math.max(anchor[0], moving[0]), y1 = Math.max(anchor[1], moving[1])
      m.geom = { x: clamp01(x0 / w), y: clamp01(y0 / h),
                 w: clamp01((x1 - x0) / w), h: clamp01((y1 - y0) / h) }
    }
    // No bbox-corner branch — hitTest only emits it for poly / stroke.
  } else if (m.kind === 'poly' || m.kind === 'stroke') {
    const g = editOrigMark.geom as { pts: [number, number][] }
    if (editDragging.kind === 'poly-vertex' && m.kind === 'poly') {
      const vi = editDragging.vertex
      m.geom = { pts: g.pts.map(([x, y], i) => i === vi
        ? [clamp01(x + dxL / w), clamp01(y + dyL / h)] as [number, number]
        : [x, y] as [number, number]) }
    } else if (editDragging.kind === 'bbox-corner') {
      // Scale every vertex around the OPPOSITE bbox corner. Compute the original bbox from the
      // orig mark (not the live one — a mid-drag re-computation would compound the scale on
      // every pointermove and rocket the shape off-screen). All coords are frame-relative
      // [0,1]; local delta (dxL, dyL) is in DOM px so divide by (w, h) first.
      const bbox01 = polyBboxFromPts01(g.pts)
      if (!bbox01) return
      const corner = editDragging.corner
      const dx01 = dxL / w, dy01 = dyL / h
      // Anchor corner (fixed) + original moving corner (before drag), both in [0,1]. Same
      // NW/NE/SE/SW indexing as rect corners.
      const bx = bbox01.x, by = bbox01.y, bX = bbox01.x + bbox01.w, bY = bbox01.y + bbox01.h
      const anchor = corner === 0 ? [bX, bY]
                   : corner === 1 ? [bx, bY]
                   : corner === 2 ? [bx, by]
                   :                [bX, by]
      const orig   = corner === 0 ? [bx, by]
                   : corner === 1 ? [bX, by]
                   : corner === 2 ? [bX, bY]
                   :                [bx, bY]
      // Moving corner's TARGET position — the drag delta on the corner being grabbed.
      const moving = [orig[0] + dx01, orig[1] + dy01]
      // SIGNED scale relative to the anchor. `(v - anchor)` already carries the sign; a signed
      // ratio (newExtent / origExtent) yields a negative sx when the drag has crossed past the
      // anchor and the shape should mirror. The previous flipX/flipY multiplier on top of
      // `(x - anchor)` double-signed the offset and flipped every vertex to the opposite side of
      // the anchor on any drag, regardless of direction.
      const origExtentX = orig[0] - anchor[0]  // signed original half-diagonal
      const origExtentY = orig[1] - anchor[1]
      const rawExtentX  = moving[0] - anchor[0]
      const rawExtentY  = moving[1] - anchor[1]
      // Clamp the magnitude so a corner dropped on top of the anchor doesn't collapse to a
      // zero-area shape the user can't grab again. Preserve the drag direction's sign.
      const signX = Math.sign(rawExtentX) || Math.sign(origExtentX) || 1
      const signY = Math.sign(rawExtentY) || Math.sign(origExtentY) || 1
      const newExtentX = signX * Math.max(Math.abs(rawExtentX), MIN_BBOX_EXTENT_01)
      const newExtentY = signY * Math.max(Math.abs(rawExtentY), MIN_BBOX_EXTENT_01)
      // Guard degenerate starting extent (horizontal-line freehand has bbox.h = 0 ⇒
      // origExtentY = 0). Fall back to identity on that axis so the drag still moves the other.
      const sx = Math.abs(origExtentX) < 1e-6 ? 1 : newExtentX / origExtentX
      const sy = Math.abs(origExtentY) < 1e-6 ? 1 : newExtentY / origExtentY
      m.geom = { pts: g.pts.map(([x, y]) => [
        clamp01(anchor[0] + (x - anchor[0]) * sx),
        clamp01(anchor[1] + (y - anchor[1]) * sy),
      ] as [number, number]) }
    } else {
      // Body drag — translate every vertex by the LOCAL delta so a rotated shape still moves in
      // the direction the user dragged (rotation origin travels with the vertex mean). Clamp the
      // DELTA (not each vertex individually) so the shape stays intact at the frame edge — a
      // per-vertex clamp lets the leading edge pin to 1.0 while the trailing edge keeps moving,
      // collapsing the shape onto the wall. Because `editOrigMark` retains the untouched
      // vertices, that collapse also snaps back on drag-return, giving the "resize wasn't
      // baked in" feel.
      const bbox01 = polyBboxFromPts01(g.pts)
      if (!bbox01) return
      const rawDx01 = dxL / w, rawDy01 = dyL / h
      // Only clamp the axis that actually fits inside the frame; a shape wider than the surface
      // (bbox.w > 1) shouldn't have the drag inverted, so let it pass through freely.
      const clampedDx = bbox01.w >= 1 ? rawDx01
        : Math.max(-bbox01.x, Math.min(1 - (bbox01.x + bbox01.w), rawDx01))
      const clampedDy = bbox01.h >= 1 ? rawDy01
        : Math.max(-bbox01.y, Math.min(1 - (bbox01.y + bbox01.h), rawDy01))
      m.geom = { pts: g.pts.map(([x, y]) =>
        [x + clampedDx, y + clampedDy] as [number, number]) }
    }
  }
  marks.value = marks.value.map((mm, j) => j === editDragging!.idx ? m : mm)
}

// Centroid of the ORIGINAL mark being dragged (DOM px). Kept as its own tiny helper so the
// rotate branch above doesn't repeat the kind switch.
function editOrigCentroidPx(): Point | null {
  if (!editOrigMark) return null
  if (editOrigMark.kind === 'rect') {
    const g = editOrigMark.geom as { x: number; y: number; w: number; h: number }
    return [(g.x + g.w / 2) * boxW.value, (g.y + g.h / 2) * boxH.value]
  }
  const g = editOrigMark.geom as { pts?: [number, number][] }
  const b = polyBboxFromPts01(g.pts ?? [])
  return b ? [(b.x + b.w / 2) * boxW.value, (b.y + b.h / 2) * boxH.value] : null
}
function polyBboxFromPts01(pts: [number, number][]): { x: number; y: number; w: number; h: number } | null {
  if (!pts || pts.length === 0) return null
  let xMin = Infinity, yMin = Infinity, xMax = -Infinity, yMax = -Infinity
  for (const [x, y] of pts) {
    if (x < xMin) xMin = x; if (x > xMax) xMax = x
    if (y < yMin) yMin = y; if (y > yMax) yMax = y
  }
  return { x: xMin, y: yMin, w: xMax - xMin, h: yMax - yMin }
}
function cursorForHandle(h: Handle | null): string {
  if (!h) return editModeActive.value ? 'default' : 'crosshair'
  if (h.kind === 'body') return 'move'
  if (h.kind === 'poly-vertex') return 'grab'
  if (h.kind === 'rotate') return 'grab'
  // rect-corner + bbox-corner both use the same diagonal-resize cursors, indexed by corner slot.
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
      editDragDistPx = 0
      svgRoot.value?.setPointerCapture?.(ev.pointerId)
      return
    }
    // Empty-space click in edit mode = deselect. Cheap and lets the user get back to the "chips
    // control DEFAULTS for the next mark" state without picking a tool first.
    if (tool.value === '') { selectedIdx.value = null; return }
  }
  const t = tool.value
  // Starting a new draft ⇒ the selection is no longer the chip target; clear it so a chip click
  // during the draw doesn't retroactively mutate the previously-selected mark.
  if (t !== '') selectedIdx.value = null
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
  if (editDragging) {
    const dx = p[0] - editStart[0], dy = p[1] - editStart[1]
    const d = Math.sqrt(dx * dx + dy * dy)
    if (d > editDragDistPx) editDragDistPx = d
    applyEdit(p); return
  }
  if (editModeActive.value && !draftKind.value) editHover.value = hitTest(p)
  if (rectDraft.value)   rectDraft.value = updateRect(rectDraft.value, p)
  if (strokeDraft.value) strokeDraft.value = extendStroke(strokeDraft.value, p)
  if (polyDraft.value)   polyDraft.value = updateCursor(polyDraft.value, p)
}
function onPointerUp(ev: PointerEvent) {
  const p = toSvgPoint(ev)
  if (editDragging) {
    // Click-not-drag on a body promotes the mark to selected. A handle click (rect corner, poly
    // vertex) never selects — its whole job was to reshape, so a zero-distance release there is
    // just a mis-click. Threshold matches the click-vs-drag primitive so a real drag doesn't
    // masquerade as a select.
    if (editDragging.kind === 'body' && editDragDistPx < 3) {
      selectedIdx.value = editDragging.idx === selectedIdx.value ? null : editDragging.idx
    }
    editDragging = null; editOrigMark = null
    svgRoot.value?.releasePointerCapture?.(ev.pointerId)
    return
  }
  if (rectDraft.value) {
    const r = finishRect(rectDraft.value, p)
    if (r) marks.value.push({ kind: 'rect',
      geom: rectToOverlayGeom(r, boxW.value, boxH.value),
      color: color.value, strokeWidth: size.value })
    rectDraft.value = null
  }
  if (strokeDraft.value) {
    const pts = finishStroke(strokeDraft.value)
    if (pts.length >= 2) marks.value.push({ kind: 'stroke',
      geom: pointsToOverlayGeom(pts, boxW.value, boxH.value),
      color: color.value, strokeWidth: size.value })
    strokeDraft.value = null
  }
  svgRoot.value?.releasePointerCapture?.(ev.pointerId)
}
function onDblClick() { if (polyDraft.value) commitPoly() }
function commitPoly() {
  const pts = finishPoly(polyDraft.value!)
  if (pts) marks.value.push({ kind: 'poly',
    geom: pointsToOverlayGeom(pts, boxW.value, boxH.value),
    color: color.value, strokeWidth: size.value })
  polyDraft.value = null
}

// ── Actions ────────────────────────────────────────────────────────────────────────────────────
function undo() { if (marks.value.length) marks.value = marks.value.slice(0, -1); clearDraft(); selectedIdx.value = null }
function clearAll() { marks.value = []; notes.value = ''; clearDraft(); selectedIdx.value = null; notesExpanded.value = false }
function save() { emit('save', { overlay: marks.value, notes: notes.value.trim() }); clearAll() }
function cancel() { emit('cancel'); clearAll() }
function removeMark(i: number) {
  marks.value = marks.value.filter((_, j) => j !== i)
  // Selection index is positional — dropping index i shifts everything above it down by one; a
  // stale higher index would then point at the wrong mark. Clear on remove-selected, decrement on
  // remove-above-selected, leave alone otherwise.
  if (selectedIdx.value === i) selectedIdx.value = null
  else if (selectedIdx.value !== null && selectedIdx.value > i) selectedIdx.value = selectedIdx.value - 1
}

// Chip retargeting. If a mark is selected, mutate it AND update the ref so the next new mark keeps
// this choice (matches PowerPoint: change a shape's colour, then the next new one is the same
// colour). If nothing is selected, the ref is a pure default for the next new mark.
function setColor(next: OverlayColor) {
  color.value = next
  const i = selectedIdx.value
  if (i !== null && marks.value[i])
    marks.value = marks.value.map((m, j) => j === i ? { ...m, color: next } : m)
}
function setSize(next: OverlayStrokeWidth) {
  size.value = next
  const i = selectedIdx.value
  if (i !== null && marks.value[i])
    marks.value = marks.value.map((m, j) => j === i ? { ...m, strokeWidth: next } : m)
}

function onKey(ev: KeyboardEvent) {
  if (!props.visible) return
  // Text-input focus wins keyboard events — Backspace / Delete inside the notes textarea must edit
  // text, not delete the selected mark.
  const tgt = ev.target as HTMLElement | null
  const inField = !!tgt && (tgt.tagName === 'TEXTAREA' || tgt.tagName === 'INPUT' || tgt.isContentEditable)
  if (ev.key === 'Enter'  && polyDraft.value) { commitPoly(); ev.preventDefault(); return }
  if ((ev.key === 'Delete' || ev.key === 'Backspace') && !inField && selectedIdx.value !== null) {
    removeMark(selectedIdx.value); ev.preventDefault(); return
  }
  if (ev.key === 'Escape') {
    if (draftKind.value) clearDraft()
    else if (selectedIdx.value !== null) selectedIdx.value = null
    else cancel()
    ev.preventDefault()
  }
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
  size.value = 'medium'
  selectedIdx.value = null
  await Promise.resolve()   // let the DOM mount before measuring
  measureBox()
}, { immediate: true })

// Chip strips visually reflect the selected mark, so clicking a shape shows its own colour /
// thickness on the ribbon (PowerPoint again — the ribbon retargets to the selection). When
// nothing is selected the chips return to whatever the user last set for new marks.
watch(selectedIdx, (i) => {
  if (i === null) return
  const m = marks.value[i]; if (!m) return
  if (m.color)       color.value = m.color
  if (m.strokeWidth) size.value  = m.strokeWidth
})

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
  const strokeW = strokeWidthFor(m)
  const selected = selectedIdx.value === i
  const rot = markRotate(m)
  if (m.kind === 'rect') {
    const x = g.x * w, y = g.y * h, width = g.w * w, height = g.h * h
    // The whole mark group (rect + corner handles + rotate handle + ✕) is wrapped in one SVG
    // rotate transform, so all clicks and paints share the same coord frame. The rotate handle
    // itself lives in the mark's LOCAL frame at top-centre offset — rotating the group also
    // rotates the handle position, which is what the user expects (it always sticks out from
    // the top of the shape, in the shape's frame).
    const cx = x + width / 2, cy = y + height / 2
    const transform = rot ? `rotate(${rot} ${cx} ${cy})` : null
    return { key: i, idx: i, kind: 'rect', label: m.label, stroke, strokeW, selected, transform,
             x, y, width, height, bbox: null as { x: number; y: number; w: number; h: number } | null,
             rotateX: cx, rotateY: y - ROTATE_HANDLE_OFFSET_PX, rotateAnchorY: y,
             labelX: x, labelY: y - 6,
             // ✕ sits outside the top-right corner handle so a click resizes vs deletes without
             // ambiguity — same offset used by the poly / stroke bbox branch below.
             deleteX: x + width + DELETE_OFFSET_PX, deleteY: y - DELETE_OFFSET_PX,
             pts: null as [number, number][] | null }
  }
  if (m.kind === 'poly' || m.kind === 'stroke') {
    const pts = (g.pts ?? []).map(p => [p[0] * w, p[1] * h] as [number, number])
    const cmd = pts.map((p, j) => (j === 0 ? 'M' : 'L') + p[0] + ',' + p[1]).join(' ')
    // Selection frame = AA bbox of the vertex list (in DOM px). Same primitive as the rect's
    // own {x,y,w,h}, so the four corner handles + rotate arm read as the same affordance across
    // shape kinds. Rotation wraps everything (shape + bbox + handles + ✕) — a rotated
    // freehand's selection frame appears at the shape's angle.
    let bbox: { x: number; y: number; w: number; h: number } | null = null
    if (pts.length > 0) {
      let xMin = Infinity, yMin = Infinity, xMax = -Infinity, yMax = -Infinity
      for (const [x, y] of pts) {
        if (x < xMin) xMin = x; if (x > xMax) xMax = x
        if (y < yMin) yMin = y; if (y > yMax) yMax = y
      }
      bbox = { x: xMin, y: yMin, w: xMax - xMin, h: yMax - yMin }
    }
    const cx = bbox ? bbox.x + bbox.w / 2 : 0
    const cy = bbox ? bbox.y + bbox.h / 2 : 0
    const transform = rot && bbox ? `rotate(${rot} ${cx} ${cy})` : null
    // Rotate/delete anchors sit on the bbox (matches rect: rotate handle above top-centre,
    // ✕ at the top-right corner). Fall through to first-vertex when the bbox is missing.
    const x0 = pts[0]?.[0] ?? 0, y0 = pts[0]?.[1] ?? 0
    const rotateX = bbox ? cx : x0
    const rotateAnchorY = bbox ? bbox.y : y0
    const rotateY = rotateAnchorY - ROTATE_HANDLE_OFFSET_PX
    const deleteX = (bbox ? bbox.x + bbox.w : x0) + DELETE_OFFSET_PX
    const deleteY = (bbox ? bbox.y : y0) - DELETE_OFFSET_PX
    const labelX = bbox ? bbox.x : x0
    const labelY = bbox ? bbox.y - 6 : y0 - 6
    return { key: i, idx: i, kind: m.kind, label: m.label, stroke, strokeW, selected, transform,
             d: m.kind === 'poly' ? cmd + ' Z' : cmd,
             bbox,
             rotateX, rotateY, rotateAnchorY,
             labelX, labelY, deleteX, deleteY,
             pts }
  }
  return { key: i, idx: i, kind: 'unknown', stroke, strokeW: 2, selected, transform: null,
           bbox: null, pts: null } as never
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
      <!-- One per-mark group so the mark's shape, corner handles, rotate handle and ✕ share the
           same SVG rotate transform. Layout is: shape / label / corner handles / rotate handle /
           ✕. Poly vertex handles stay in their own loop below since poly doesn't rotate. -->
      <g class="ds-committed">
        <template v-for="s in committedShapes" :key="s.key">
          <g :transform="s.transform ?? undefined">
            <rect v-if="s.kind === 'rect'" :x="s.x" :y="s.y" :width="s.width" :height="s.height"
                  :class="{ 'ds-selected': s.selected }"
                  :style="{ stroke: s.stroke, strokeWidth: s.strokeW }" />
            <path v-else-if="s.kind === 'poly' || s.kind === 'stroke'" :d="s.d"
                  :class="{ 'ds-selected': s.selected }"
                  :style="{ stroke: s.stroke, strokeWidth: s.strokeW }" />
            <text v-if="s.label" :x="s.labelX" :y="s.labelY" :style="{ fill: s.stroke }">{{ s.label }}</text>
            <template v-if="editModeActive && s.kind === 'rect'">
              <rect v-for="(c, ci) in [[s.x, s.y], [s.x + s.width, s.y],
                                        [s.x + s.width, s.y + s.height], [s.x, s.y + s.height]]"
                    :key="'c' + ci" class="ds-corner"
                    :x="c[0] - 6" :y="c[1] - 6" :width="12" :height="12" />
            </template>
            <!-- Poly / stroke selection frame — dashed bbox outline + 4 corner handles.
                 Selected only, so an unselected freehand scribble doesn't show a rectangle. -->
            <template v-if="s.selected && (s.kind === 'poly' || s.kind === 'stroke') && s.bbox">
              <rect class="ds-bbox-frame" :x="s.bbox.x" :y="s.bbox.y"
                    :width="s.bbox.w" :height="s.bbox.h" />
              <rect v-for="(c, ci) in [[s.bbox.x, s.bbox.y], [s.bbox.x + s.bbox.w, s.bbox.y],
                                        [s.bbox.x + s.bbox.w, s.bbox.y + s.bbox.h], [s.bbox.x, s.bbox.y + s.bbox.h]]"
                    :key="'bc' + ci" class="ds-corner"
                    :x="c[0] - 6" :y="c[1] - 6" :width="12" :height="12" />
            </template>
            <!-- Rotate handle — small circle above the top-centre of the AA rect (or bbox for
                 poly / stroke), connected to the shape by a short line. Selected only. -->
            <template v-if="s.selected && (s.kind === 'rect' || ((s.kind === 'poly' || s.kind === 'stroke') && s.bbox))">
              <line class="ds-rotate-arm"
                    :x1="s.rotateX" :y1="s.rotateAnchorY" :x2="s.rotateX" :y2="s.rotateY" />
              <circle class="ds-rotate-handle" :cx="s.rotateX" :cy="s.rotateY" :r="6"
                      v-tooltip.top="'Rotate — drag around the shape'" />
            </template>
            <!-- Poly vertex handles — inside the per-mark rotated group so they sit on the
                 rotated vertices, not the un-rotated AA positions. Selected poly only (strokes
                 have too many RDP-simplified vertices to be worth handles). -->
            <template v-if="s.selected && s.kind === 'poly' && s.pts">
              <circle v-for="(pt, vi) in s.pts" :key="'v' + vi" class="ds-vertex"
                      :cx="pt[0]" :cy="pt[1]" :r="5" />
            </template>
            <g class="ds-delete" @pointerdown.stop @click.stop="removeMark(s.idx)"
               v-tooltip.top="'Delete this mark (Del)'">
              <circle :cx="s.deleteX" :cy="s.deleteY" :r="7" />
              <text :x="s.deleteX" :y="s.deleteY + 2" text-anchor="middle"
                    dominant-baseline="middle" font-size="10">×</text>
            </g>
          </g>
        </template>
      </g>
      <rect v-if="draftKind === 'rect' && draftRect" class="ds-draft"
            :style="{ stroke: draftStroke, strokeWidth: draftStrokeWidth }"
            :x="draftRect.x" :y="draftRect.y" :width="draftRect.w" :height="draftRect.h" />
      <path v-if="draftKind === 'poly' && draftPolyPath" class="ds-draft"
            :style="{ stroke: draftStroke, strokeWidth: draftStrokeWidth }" :d="draftPolyPath" />
      <path v-if="draftKind === 'stroke' && draftStrokePath" class="ds-draft"
            :style="{ stroke: draftStroke, strokeWidth: draftStrokeWidth }" :d="draftStrokePath" />
      <circle v-if="firstVertexMarker" class="ds-poly-first"
              :class="{ armed: firstVertexMarker.armed }" :style="{ stroke: draftStroke }"
              :cx="firstVertexMarker.x" :cy="firstVertexMarker.y" :r="firstVertexMarker.radius" />
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
                    setColor((Array.isArray(v) ? v[0] : v) as OverlayColor)" />
      <ChipSelect variant="segmented" :options="SIZE_OPTIONS" :model-value="size"
                  aria-label="Stroke thickness"
                  @update:modelValue="(v: string | string[]) =>
                    setSize((Array.isArray(v) ? v[0] : v) as OverlayStrokeWidth)" />
      <input v-if="notesHasNewlines" class="cc-input ds-notes-input ds-notes-preview" type="text"
             readonly :value="notesPreview"
             @click="notesExpanded = true"
             v-tooltip.bottom="'Multi-line note — click to open the editor'" />
      <input v-else class="cc-input ds-notes-input" type="text" v-model="notes" maxlength="800"
             placeholder="Notes for Claude (optional)"
             v-tooltip.bottom="'Free-text context sent with the frame — what you are pointing at + why. Click ⤢ for more room.'" />
      <button ref="notesExpandBtn" class="cc-btn cc-btn-ghost cc-btn-icon"
              :class="{ 'cc-btn-on cc-btn-on-tint': notesExpanded }"
              @click="notesExpanded = !notesExpanded"
              v-tooltip.bottom="'Expand notes for longer text'">
        <i class="pi pi-window-maximize" />
      </button>
      <TeleportPopover v-model="notesExpanded" :anchor="notesExpandBtn" placement="bottom-end">
        <div class="ds-notes-popover">
          <textarea ref="notesTextarea" class="cc-input ds-notes-textarea" v-model="notes"
                    rows="8" maxlength="800"
                    placeholder="Notes for Claude (optional)"></textarea>
          <div class="ds-notes-footer cc-fs-2xs">{{ notes.length }} / 800</div>
        </div>
      </TeleportPopover>
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
      <i class="pi pi-arrows-alt" />
      <template v-if="selectedMark">
        Selected — colour / thickness apply to this mark. Drag the shape to move, a corner to
        resize, the top handle to rotate. Del removes.
      </template>
      <template v-else>
        Edit — click a mark to select, drag to move, drag a corner or vertex to reshape
      </template>
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
  font-family: inherit;   /* textarea default is monospace on some UAs */
}
/* Read-only preview shown when the note has newlines — visibly non-editable + a pointer cursor so
   the ⤢ affordance is obvious. */
.ds-notes-preview { cursor: pointer; color: var(--cc-text-dim); }
/* Popover content — a proper textarea for longer notes. Width is fixed; height is user-resizable
   within the popover, so the toolbar row NEVER grows. */
.ds-notes-popover { display: flex; flex-direction: column; gap: 0.35rem; width: 24rem; }
.ds-notes-textarea {
  resize: vertical; min-height: 6rem; max-height: 60vh;
  font-family: inherit;
}
.ds-notes-footer { color: var(--cc-text-dim); text-align: right; }
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
/* Committed marks: white stroke with a dark halo — visible on both dark and bright pixels.
   `stroke-width` is a fallback for marks that arrived without a preset; live drawing overrides
   it inline. `vector-effect: non-scaling-stroke` keeps the on-screen width in CSS px regardless
   of the SVG's viewBox scale, so the same preset reads the same across zooms. */
.ds-committed rect, .ds-committed path {
  fill: none; stroke: #fff; stroke-width: 2; vector-effect: non-scaling-stroke;
  paint-order: stroke;
}
/* Selection halo — a second stroke painted underneath via `filter: drop-shadow`. Simple, cheap,
   works on any shape kind (rect / poly / stroke) without a second path element. Colour uses
   `--cc-accent` so it reads on both dark and bright underlays. */
.ds-committed rect.ds-selected, .ds-committed path.ds-selected {
  filter: drop-shadow(0 0 3px var(--cc-accent)) drop-shadow(0 0 3px var(--cc-accent));
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
.ds-corner {
  fill: #fff; stroke: rgba(0, 0, 0, 0.75); stroke-width: 1.5; vector-effect: non-scaling-stroke;
}
/* Poly vertex handles — same visual language as rect corners (white fill, dark ring) so a user
   who has drawn a rect reads them as the same affordance. */
.ds-vertex {
  fill: #fff; stroke: rgba(0, 0, 0, 0.75); stroke-width: 1.5; vector-effect: non-scaling-stroke;
  cursor: grab;
}
/* Poly / stroke selection frame — dashed accent outline showing the AA bounding box that the
   corner handles resize. Non-interactive on its own (clicks go to the handles or the body). */
.ds-bbox-frame {
  fill: none; stroke: var(--cc-accent); stroke-width: 1;
  stroke-dasharray: 4 3; vector-effect: non-scaling-stroke;
  pointer-events: none;
}
/* Rotate handle — small ring above the shape, connected by a hairline arm. Uses --cc-accent so
   it reads as a "control", not part of the mark. `cursor: grab` telegraphs the drag; on
   pointerdown the browser flips to `grabbing` on its own. */
.ds-rotate-handle {
  fill: var(--cc-surface-1); stroke: var(--cc-accent); stroke-width: 2;
  vector-effect: non-scaling-stroke; cursor: grab;
}
.ds-rotate-arm {
  stroke: var(--cc-accent); stroke-width: 1.5; vector-effect: non-scaling-stroke;
  pointer-events: none;
}
.ds-delete { cursor: pointer; }
.ds-delete circle { fill: rgba(0, 0, 0, 0.7); stroke: #fff; stroke-width: 1.5;
                    vector-effect: non-scaling-stroke; }
.ds-delete:hover circle { fill: var(--cc-accent); }
.ds-delete text { fill: #fff; stroke: none; font-family: var(--cc-mono); font-weight: bold;
                  pointer-events: none; }
</style>
