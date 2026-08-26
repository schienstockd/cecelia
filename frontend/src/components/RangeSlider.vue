<script setup lang="ts">
// A compact dual-thumb range slider (min + max on one track). Native <input type="range"> is
// single-thumb only, so this is a small pointer-driven control that matches the app's accent styling.
// Reusable — v-model:lo / v-model:hi. Used for the 3D-crop z/t ranges (ViewerPanel).
import { ref } from 'vue'

const props = withDefaults(defineProps<{
  lo: number
  hi: number
  min?: number
  max?: number
  step?: number
}>(), { min: 0, max: 100, step: 1 })

// `change` fires ONCE, when the drag ends — the canonical shape for a control whose effect is
// expensive (docs/UI.md → Continuous controls). `update:lo`/`update:hi` fire per pointer move, which is
// right for a cheap effect (a contrast window) and ruinous for one that refetches every cached
// timepoint: the volume viewer's z range reallocates the whole cache, so it commits on release.
const emit = defineEmits<{
  (e: 'update:lo', v: number): void
  (e: 'update:hi', v: number): void
  (e: 'change'): void
}>()

const track = ref<HTMLElement | null>(null)
let active: 'lo' | 'hi' | null = null

// Half the thumb width — must match `.rs-thumb { width }` and the `.rs-rail` inset. The rail spans
// from THUMB_HALF to width-THUMB_HALF, so value → position must map into that inset track.
const THUMB_HALF = 5.5

// Clamp `frac` to [0, 1] so a caller passing lo/hi outside `min`/`max` (e.g. the viewer's ch.hi
// briefly exceeding a growing `chMax`) can't produce a negative `right` on the fill and paint the
// whole rail beyond the box.
const frac = (v: number) =>
  Math.max(0, Math.min(1, (v - props.min) / (props.max - props.min || 1)))
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
function valueFromEvent(e: PointerEvent): number {
  const el = track.value
  if (!el) return props.min
  const r = el.getBoundingClientRect()
  // Pointer at the rail's left end (r.left + THUMB_HALF) is value=min, right end value=max.
  const trackW = r.width - 2 * THUMB_HALF
  const f = trackW > 0 ? (e.clientX - r.left - THUMB_HALF) / trackW : 0
  return clampSnap(props.min + Math.max(0, Math.min(1, f)) * (props.max - props.min))
}
function onMove(e: PointerEvent) {
  if (!active) return
  const v = valueFromEvent(e)
  if (active === 'lo') emit('update:lo', Math.min(v, props.hi))
  else emit('update:hi', Math.max(v, props.lo))
}
function onUp() {
  active = null
  window.removeEventListener('pointermove', onMove)
  window.removeEventListener('pointerup', onUp)
  emit('change')
}
function grab(which: 'lo' | 'hi', e: PointerEvent) {
  active = which
  window.addEventListener('pointermove', onMove)
  window.addEventListener('pointerup', onUp)
  e.preventDefault()
}
// click/drag on the rail → move whichever thumb is nearer, then keep dragging it
function onTrackDown(e: PointerEvent) {
  const v = valueFromEvent(e)
  grab(Math.abs(v - props.lo) <= Math.abs(v - props.hi) ? 'lo' : 'hi', e)
  onMove(e)
}
</script>

<template>
  <div class="rs" ref="track" @pointerdown="onTrackDown">
    <div class="rs-rail" />
    <div class="rs-fill" :style="{ left: fillLeft(lo), right: fillRight(hi) }" />
    <div class="rs-thumb" :style="{ left: thumbLeft(lo) }" @pointerdown.stop="grab('lo', $event)" />
    <div class="rs-thumb" :style="{ left: thumbLeft(hi) }" @pointerdown.stop="grab('hi', $event)" />
  </div>
</template>

<style scoped>
/* Thumbs are 11px round; the rail is inset by half-thumb (5.5px) each side so the thumb CENTRE at
   value=min lands on the rail's left end and the thumb CENTRE at value=max lands on the rail's right
   end, with the whole thumb still inside `.rs`. `translateX(-50%)` on the thumb centres it on `left`;
   the inline `left: calc(5.5px + (100% - 11px) * frac)` positions that centre along the inset track,
   so a hi=max thumb no longer pokes past `.rs` (Dominik, 2026-08-26). */
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
}
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
