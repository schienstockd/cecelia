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

const emit = defineEmits<{ (e: 'update:lo', v: number): void; (e: 'update:hi', v: number): void }>()

const track = ref<HTMLElement | null>(null)
let active: 'lo' | 'hi' | null = null

const pct = (v: number) => ((v - props.min) / (props.max - props.min || 1)) * 100

function clampSnap(v: number): number {
  const s = props.step || 1
  return Math.max(props.min, Math.min(props.max, Math.round(v / s) * s))
}
function valueFromEvent(e: PointerEvent): number {
  const el = track.value
  if (!el) return props.min
  const r = el.getBoundingClientRect()
  const frac = r.width ? (e.clientX - r.left) / r.width : 0
  return clampSnap(props.min + frac * (props.max - props.min))
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
    <div class="rs-fill" :style="{ left: pct(lo) + '%', right: (100 - pct(hi)) + '%' }" />
    <div class="rs-thumb" :style="{ left: pct(lo) + '%' }" @pointerdown.stop="grab('lo', $event)" />
    <div class="rs-thumb" :style="{ left: pct(hi) + '%' }" @pointerdown.stop="grab('hi', $event)" />
  </div>
</template>

<style scoped>
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
  left: 0; right: 0;
  height: 3px;
  border-radius: 2px;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
}
.rs-fill {
  position: absolute;
  height: 3px;
  border-radius: 2px;
  background: var(--cc-accent);
}
.rs-thumb {
  position: absolute;
  width: 11px;
  height: 11px;
  border-radius: 50%;
  background: var(--cc-accent);
  border: 1px solid var(--cc-bg);
  transform: translateX(-50%);
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.5);
}
.rs-thumb:hover { filter: brightness(1.15); }
</style>
