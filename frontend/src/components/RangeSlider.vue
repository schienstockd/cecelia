<script setup lang="ts">
// A compact dual-thumb range slider (min + max on one track). Native <input type="range"> is
// single-thumb only, so this is a small pointer-driven control that matches the app's accent styling.
// Drag a thumb to move one end; drag the span between them to move the whole range.
// Reusable — v-model:lo / v-model:hi. Used for the 3D-crop z/t ranges (ViewerPanel) and, with
// `scale="log"` + a `samples` buffer, for the viewer's per-channel contrast window.
import { computed, ref } from 'vue'
import { posFromValue, valueFromPos, binSamplesByPos, type SliderScale } from '../utils/rangeScale'

const props = withDefaults(defineProps<{
  lo: number
  hi: number
  min?: number
  max?: number
  step?: number
  // Log gives the tiny [0, hi] band its own travel when the full slider spans a dtype range whose
  // useful signal lives in the first ~0.3%. Values stay linear;
  // only the pixel↔value mapping changes. Log requires min >= 0 — see rangeScale.ts.
  scale?: SliderScale
  // Sample values whose distribution is drawn as a faint histogram behind the rail. Strided
  // subsample is fine (~100k–200k values); binning is one linear pass, memoized. Typed array to
  // avoid wrapping in a Vue proxy.
  samples?: Uint16Array | Uint8Array | null
}>(), { min: 0, max: 100, step: 1, scale: 'linear', samples: null })

// `change` fires ONCE, when the drag ends — the canonical shape for a control whose effect is
// expensive (docs/UI.md → Continuous controls). `update:lo`/`update:hi` fire per pointer move, which is
// right for a cheap effect (a contrast window) and ruinous for one that refetches every cached
// timepoint: the volume viewer's z range reallocates the whole cache, so it commits on release.
const emit = defineEmits<{
  (e: 'update:lo', v: number): void
  (e: 'update:hi', v: number): void
  // Both ends in ONE event, after any lo/hi update. A range drag moves both; a parent that rebuilds
  // its pair from its own (not-yet-updated) props on each half would drop the first — listen here.
  (e: 'update:pair', v: [number, number]): void
  (e: 'change'): void
}>()

const track = ref<HTMLElement | null>(null)
// 'range' = dragging the span between the thumbs: both move together, width kept.
let active: 'lo' | 'hi' | 'range' | null = null

// Half the thumb width — must match `.rs-thumb { width }` and the `.rs-rail` inset. The rail spans
// from THUMB_HALF to width-THUMB_HALF, so value → position must map into that inset track.
const THUMB_HALF = 5.5

const frac = (v: number) => posFromValue(v, props.min, props.max, props.scale)
// A thumb's `left` places its CENTRE at `5.5px + frac * (100% - 11px)`, then `translateX(-50%)` in
// CSS centres the 11px thumb on that point. Result: thumb is fully inside `.rs` for any value.
const thumbLeft = (v: number) =>
  `calc(${THUMB_HALF}px + (100% - ${2 * THUMB_HALF}px) * ${frac(v)})`
const fillLeft  = (v: number) => thumbLeft(v)
const fillRight = (v: number) =>
  `calc(${THUMB_HALF}px + (100% - ${2 * THUMB_HALF}px) * ${1 - frac(v)})`

function clampSnap(v: number): number {
  const s = props.step || 1
  return Math.max(props.min, Math.min(props.max, Math.round(v / s) * s))
}
// Pointer x → rail position in [0, 1] (unclamped; valueFromPos clamps).
function posFromEvent(e: PointerEvent): number {
  const el = track.value
  if (!el) return 0
  const r = el.getBoundingClientRect()
  // Pointer at the rail's left end (r.left + THUMB_HALF) is value=min, right end value=max.
  const trackW = r.width - 2 * THUMB_HALF
  return trackW > 0 ? (e.clientX - r.left - THUMB_HALF) / trackW : 0
}
function valueFromEvent(e: PointerEvent): number {
  return clampSnap(valueFromPos(posFromEvent(e), props.min, props.max, props.scale))
}

// Range drag state: where the pointer went down and where the thumbs were, in POSITION space, so
// the thumbs track the cursor pixel-for-pixel on a log rail too (linear: same as a value shift).
// A press that never moves past DRAG_SLOP is a click, and keeps the rail's click behaviour —
// jump the nearer thumb there — so a click inside the range can still narrow it.
const DRAG_SLOP = 3
let downX = 0
let downPos = 0
let loPos0 = 0
let hiPos0 = 0
let rangeMoved = false

function onMove(e: PointerEvent) {
  if (!active) return
  if (active === 'range') {
    if (!rangeMoved && Math.abs(e.clientX - downX) < DRAG_SLOP) return
    rangeMoved = true
    // Clamp the shift so neither end runs off the rail — the width is kept, the span stops at the end.
    const d = Math.max(-loPos0, Math.min(1 - hiPos0, posFromEvent(e) - downPos))
    const lo = clampSnap(valueFromPos(loPos0 + d, props.min, props.max, props.scale))
    const hi = clampSnap(valueFromPos(hiPos0 + d, props.min, props.max, props.scale))
    if (lo !== props.lo || hi !== props.hi) {
      emit('update:lo', lo)
      emit('update:hi', Math.max(hi, lo))
      emit('update:pair', [lo, Math.max(hi, lo)])
    }
    return
  }
  const v = valueFromEvent(e)
  if (active === 'lo') {
    const lo = Math.min(v, props.hi)
    emit('update:lo', lo)
    emit('update:pair', [lo, props.hi])
  } else {
    const hi = Math.max(v, props.lo)
    emit('update:hi', hi)
    emit('update:pair', [props.lo, hi])
  }
}
function onUp(e: PointerEvent) {
  if (active === 'range' && !rangeMoved) {
    // A click on the span, not a drag — behave like a click on the rail.
    active = nearer(valueFromEvent(e))
    onMove(e)
  }
  active = null
  window.removeEventListener('pointermove', onMove)
  window.removeEventListener('pointerup', onUp)
  emit('change')
}
function grab(which: 'lo' | 'hi' | 'range', e: PointerEvent) {
  active = which
  window.addEventListener('pointermove', onMove)
  window.addEventListener('pointerup', onUp)
  e.preventDefault()
}
const nearer = (v: number): 'lo' | 'hi' =>
  Math.abs(v - props.lo) <= Math.abs(v - props.hi) ? 'lo' : 'hi'
// click/drag on the rail → move whichever thumb is nearer, then keep dragging it
function onTrackDown(e: PointerEvent) {
  grab(nearer(valueFromEvent(e)), e)
  onMove(e)
}
// press on the span between the thumbs → drag the whole range (or, if it's just a click, see onUp)
function onSpanDown(e: PointerEvent) {
  downX = e.clientX
  downPos = posFromEvent(e)
  loPos0 = frac(props.lo)
  hiPos0 = frac(props.hi)
  rangeMoved = false
  grab('range', e)
}

// Histogram behind the rail — a smooth filled density curve, not bars: at 16px tall the bar
// widths and gaps just read as noise, so a single stroked path (a purely visual density hint)
// is more legible. Binned in POSITION space (see rangeScale.ts) so log's
// left third splits into many bins, not one. `viewBox="0 0 <N> 10"` scales to fill the
// container, so no resize observer is needed. `computed` memoises, so a drag (which doesn't
// change samples/min/max/scale) is one cached read per frame.
//
// log1p compression, not linear or sqrt: microscopy channels are ~99% background pixels, so
// linear draws a spike at 0 and everything else near invisible; sqrt was still too aggressive
// (the second bin sat at ~5% of a 16px height and vanished under the rail).
const DISPLAY_BINS = 64
const DISPLAY_H = 10
const histPaths = computed<{ area: string; line: string } | null>(() => {
  if (!props.samples || props.samples.length === 0) return null
  const bins = binSamplesByPos(
    props.samples, DISPLAY_BINS, props.min, props.max, props.scale)
  let mx = 0
  for (const b of bins) if (b > mx) mx = b
  if (mx <= 0) return null
  const denom = Math.log1p(mx)
  const N = bins.length
  // Point at each bin's centre in x; y bottom-anchored (SVG y grows downward).
  const pts = new Array<{ x: number; y: number }>(N)
  for (let i = 0; i < N; i++) {
    const norm = bins[i] > 0 ? Math.log1p(bins[i]) / denom : 0
    pts[i] = { x: i + 0.5, y: DISPLAY_H - norm * DISPLAY_H }
  }
  // Smooth via midpoint-quadratic — the segment goes from midpoint(pts[i], pts[i+1]) with
  // pts[i+1] as the control, a standard cheap smoothing that needs no tangent estimates.
  let curve = ''
  for (let i = 0; i < N - 1; i++) {
    const mx2 = (pts[i].x + pts[i + 1].x) / 2
    const my2 = (pts[i].y + pts[i + 1].y) / 2
    curve += ` Q ${pts[i].x} ${pts[i].y} ${mx2} ${my2}`
  }
  const first = `${pts[0].x} ${pts[0].y}`
  const last = `${pts[N - 1].x} ${pts[N - 1].y}`
  // Two paths, not one: the stroke on a filled-and-closed path would draw along the baseline
  // too, so keep the OUTLINE (line) open on top and the FILL (area) closed to the baseline.
  const line = `M ${first}${curve} L ${last}`
  const area = `M 0 ${DISPLAY_H} L ${first}${curve} L ${last} L ${N} ${DISPLAY_H} Z`
  return { area, line }
})
</script>

<template>
  <div class="rs" :class="{ 'rs-hist-on': !!histPaths }" ref="track" @pointerdown="onTrackDown">
    <svg v-if="histPaths" class="rs-hist" :viewBox="`0 0 ${DISPLAY_BINS} ${DISPLAY_H}`"
         preserveAspectRatio="none" aria-hidden="true">
      <path class="rs-hist-area" :d="histPaths.area" />
      <!-- Line drawn twice — the wider dark stroke is the "border", the narrower light stroke is
           the "core". Mirrors the thumb's light-fill + dark-ring style; SVG can't put a literal
           border on a 1D stroke, but two coincident strokes give the same visual language. -->
      <path class="rs-hist-line-border" :d="histPaths.line" vector-effect="non-scaling-stroke" />
      <path class="rs-hist-line" :d="histPaths.line" vector-effect="non-scaling-stroke" />
    </svg>
    <div class="rs-rail" />
    <div class="rs-fill" :style="{ left: fillLeft(lo), right: fillRight(hi) }" />
    <!-- Full-height grab band over the span between the thumbs: drag it to move the range. Thumbs
         come after it in the DOM, so where they overlap the band the thumb wins. -->
    <div class="rs-span" :style="{ left: fillLeft(lo), right: fillRight(hi) }"
         @pointerdown.stop="onSpanDown" />
    <div class="rs-thumb" :style="{ left: thumbLeft(lo) }" @pointerdown.stop="grab('lo', $event)" />
    <div class="rs-thumb" :style="{ left: thumbLeft(hi) }" @pointerdown.stop="grab('hi', $event)" />
  </div>
</template>

<style scoped>
/* Thumbs are 11px round; the rail is inset by half-thumb (5.5px) each side so the thumb CENTRE at
   value=min lands on the rail's left end and the thumb CENTRE at value=max lands on the rail's right
   end, with the whole thumb still inside `.rs`. `translateX(-50%)` on the thumb centres it on `left`;
   the inline `left: calc(5.5px + (100% - 11px) * frac)` positions that centre along the inset track,
   so a hi=max thumb no longer pokes past `.rs`. */
.rs {
  position: relative;
  height: 1rem;
  flex: 1;
  min-width: 3rem;
  cursor: pointer;
  touch-action: none;
  display: flex;
  align-items: center;
}
/* Histogram-present variant: grow the row and STACK — density silhouette on top, rail + thumbs
   at the bottom — so the rail's opaque bar doesn't slice through the density. Rail/thumbs get explicit `bottom` because the base flex-centre no longer applies
   once we've stopped using it here. */
.rs.rs-hist-on {
  height: 1.75rem;
}
.rs.rs-hist-on .rs-rail,
.rs.rs-hist-on .rs-fill {
  top: auto;
  bottom: 4px;                   /* rail centre = 5.5px from bottom = thumb centre */
}
.rs.rs-hist-on .rs-thumb {
  top: auto;
  bottom: 0;                     /* thumb centre = 5.5px from bottom */
}
/* Histogram sits behind the rail at the same left/right inset so a bar's x lines up with the value
   the thumb at that x reports. `pointer-events: none` so clicks pass through to the slider. Bottom
   inset leaves clearance for the 11px thumb + a couple px of breathing room. */
.rs-hist {
  position: absolute;
  left: 5.5px; right: 5.5px;
  top: 0;
  bottom: 13px;
  width: calc(100% - 11px);
  pointer-events: none;
  overflow: visible;
}
.rs-hist-area {
  /* Light-blue feather-shaped density. Opacity keeps it a background hint
     rather than competing with the rail/thumbs for attention. */
  fill: var(--cc-active);
  opacity: 0.55;
}
.rs-hist-line-border {
  fill: none;
  stroke: color-mix(in srgb, var(--cc-active) 35%, black);
  stroke-width: 2.4;
  stroke-linejoin: round;
  stroke-linecap: round;
}
.rs-hist-line {
  fill: none;
  stroke: var(--cc-active);
  stroke-width: 1;
  stroke-linejoin: round;
  stroke-linecap: round;
}
.rs-rail {
  position: absolute;
  left: 5.5px; right: 5.5px;
  height: 3px;
  border-radius: var(--cc-radius-xs);
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
}
.rs-fill {
  position: absolute;
  height: 3px;
  border-radius: var(--cc-radius-xs);
  background: var(--cc-accent);
  /* Bg-coloured hairline around the fill — same visual language as the thumb's `border`, but
     via `box-shadow` so it doesn't add to the fill's 3px height. Gives the purple range a clean
     edge against the rail and (with the histogram on) against the density silhouette behind. */
  box-shadow: 0 0 0 1px var(--cc-bg);
}
/* Invisible hit band, the full height of the control, between the thumbs. */
.rs-span {
  position: absolute;
  top: 0;
  bottom: 0;
  cursor: grab;
}
.rs-span:active { cursor: grabbing; }
.rs-thumb {
  position: absolute;
  width: 11px;
  height: 11px;
  border-radius: var(--cc-radius-pill);
  background: var(--cc-accent);
  border: 1px solid var(--cc-bg);
  transform: translateX(-50%);
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.5);
}
.rs-thumb:hover { filter: brightness(1.15); }
</style>
