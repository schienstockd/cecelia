<!--
  Vector scale bar + timestamp for a captured still (Phase E2). An SVG whose viewBox is the frame's
  physical extent (µm) with preserveAspectRatio "xMidYMid meet" — the SAME fit as the frame <img>'s
  object-fit: contain — so annotations stay geometrically correct AND aligned to the image content even
  when the frame is letterboxed. The scale bar length is drawn in µm (viewBox units), so it's correct by
  construction; text/bar sizes are fractions of the extent so they scale proportionally with the frame.
  Drawn on the CLEAN capture (the viewer's own scale bar/timestamp hidden — see E1). Absolutely fills the
  parent (which must be position:relative and the frame's box).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { niceScaleBar } from '../utils/stillOverlay'
import { usePlotResize } from '../composables/usePlotResize'

const props = withDefaults(defineProps<{
  extentUm?: { x?: number; y?: number; unit?: string | null } | null
  timeLabel?: string        // '' → no timestamp
  showScaleBar?: boolean
  showTimestamp?: boolean
  /**
   * How the text and bar THICKNESS are sized — the bar's LENGTH is physical either way.
   *
   * `'proportional'` scales them with the frame, which is what a strip thumbnail wants: the same still
   * reads the same at any card size. `'fixed'` sizes them in screen pixels, which is what a full-bleed
   * interactive canvas wants — proportional chrome on a 700 px viewer renders a 35 px label
   * ("massive scale bar, tiny timestamp" — Dominik, 2026-08-24) and, worse, changes size as you zoom.
   */
  chrome?: 'proportional' | 'fixed'
  /** Screen px for the scale-bar label / the timestamp, under `chrome: 'fixed'`. Separate numbers
   *  because they annotate different things and readable is not one size — the viewer exposes both. */
  barFontPx?: number
  timeFontPx?: number
}>(), {
  showScaleBar: true, showTimestamp: true, chrome: 'proportional',
  barFontPx: 20, timeFontPx: 20,
})

const ex = computed(() => props.extentUm?.x ?? 0)
const ey = computed(() => props.extentUm?.y ?? 0)
const ok = computed(() => ex.value > 0 && ey.value > 0)
const bar = computed(() => niceScaleBar(ex.value, props.extentUm?.unit ?? 'µm'))

// Margin in screen px for `chrome: 'fixed'`; the bar's thickness follows its label so a large label
// does not sit on a hairline. The 20 px default is Dominik's call (2026-08-24) — the viewer's own overlays
// are 12 px, which read as too small on a full window.
const MARGIN_PX = 12
const barPx = computed(() => Math.max(3, props.barFontPx * 0.3))

// px per viewBox unit. `min` because preserveAspectRatio="xMidYMid meet" letterboxes — for a full-bleed
// canvas the two ratios are equal, so this is exact in both cases. The box is measured through the
// canonical resize observer rather than a second hand-rolled one.
const host = ref<HTMLElement | null>(null)
const boxPx = ref({ w: 0, h: 0 })
usePlotResize(host, () => {
  const el = host.value
  if (el) boxPx.value = { w: el.clientWidth, h: el.clientHeight }
})
const perUnit = computed(() => {
  const { w, h } = boxPx.value
  if (!ok.value || w <= 0 || h <= 0) return 0
  return Math.min(w / ex.value, h / ey.value)
})
/** `n` screen px expressed in viewBox units, or `fallback` before the box has been measured. */
const asPx = (n: number, fallback: number) => (perUnit.value > 0 ? n / perUnit.value : fallback)
const fixed = computed(() => props.chrome === 'fixed' && perUnit.value > 0)

// geometry in viewBox (µm) units — margins/sizes as fractions of the extent so they scale with the frame
const mx = computed(() => (fixed.value ? asPx(MARGIN_PX, 0) : ex.value * 0.045))
const my = computed(() => (fixed.value ? asPx(MARGIN_PX, 0) : ey.value * 0.045))
const barH = computed(() => (fixed.value ? asPx(barPx.value, 0) : ey.value * 0.012))
const font = computed(() =>
  fixed.value ? asPx(props.barFontPx, 0) : Math.min(ex.value, ey.value) * 0.05)
const barX1 = computed(() => ex.value - mx.value - (bar.value?.um ?? 0))
const barX2 = computed(() => ex.value - mx.value)
const barY = computed(() => ey.value - my.value - barH.value)
</script>

<template>
  <div class="still-ovl" :class="{ 'is-fixed': chrome === 'fixed' }" ref="host">
    <!-- elapsed-time timestamp, top-left: plain HTML so it draws even when the frame has NO physical
         extent — a timestamp needs no µm scale (the scale bar below does). Previously it lived inside the
         extent-gated <svg v-if="ok">, so ticking "timestamp" on a frame without an extent did nothing. -->
    <div v-if="showTimestamp && timeLabel" class="ovl-ts"
         :style="chrome === 'fixed' ? { fontSize: timeFontPx + 'px', top: '8px', left: '10px' } : undefined"
    >{{ timeLabel }}</div>
    <!-- vector scale bar, bottom-right: an SVG whose viewBox IS the frame's physical extent (µm), so the
         bar length is correct by construction and stays aligned to the letterboxed image. Needs the extent.
         Under `chrome: 'fixed'`, hold rendering until the host box has been measured (`perUnit > 0`):
         with no measurement `fixed` collapses to false and the sizes fall back to the PROPORTIONAL branch
         for one frame, which flashes a large label + thick bar before snapping to the fixed sizes on the
         next tick. Waiting one frame is invisible; the flash isn't. -->
    <svg v-if="ok && showScaleBar && bar && (chrome !== 'fixed' || perUnit > 0)"
         class="ovl-svg" :viewBox="`0 0 ${ex} ${ey}`" preserveAspectRatio="xMidYMid meet">
      <rect :x="barX1" :y="barY" :width="bar.um" :height="barH" class="ovl-fill" />
      <text :x="(barX1 + barX2) / 2" :y="barY - font * 0.35" :font-size="font"
            class="ovl-text" text-anchor="middle">{{ bar.label }}</text>
    </svg>
  </div>
</template>

<style scoped>
.still-ovl { position: absolute; inset: 0; pointer-events: none; }
.ovl-svg { position: absolute; inset: 0; width: 100%; height: 100%; }
/* timestamp: white with a dark outline (four text-shadows ≈ SVG paint-order stroke) so it reads on any
   background, matching the channel legend's on-image styling (.is-legend). */
.ovl-ts { position: absolute; top: 5px; left: 6px; color: #fff; font-weight: 700; line-height: 1;
  font-family: system-ui, sans-serif; font-size: var(--cc-fs-xs);
  text-shadow: -1px -1px 0 rgba(0,0,0,0.85), 1px -1px 0 rgba(0,0,0,0.85),
               -1px 1px 0 rgba(0,0,0,0.85), 1px 1px 0 rgba(0,0,0,0.85); }
/* white with a dark outline so it reads on any background (like the viewer's overlays) */
.ovl-text { fill: #fff; paint-order: stroke; stroke: rgba(0,0,0,0.85); stroke-width: 0.5;
  font-weight: 700; font-family: system-ui, sans-serif; }
.ovl-fill { fill: #fff; stroke: rgba(0,0,0,0.85); stroke-width: 0.3; }
/* In fixed mode the outline is a screen hairline, not a fraction of the image — `non-scaling-stroke`
   is what keeps it one px at any zoom, and the same for the label's outline. */
.still-ovl.is-fixed .ovl-fill, .still-ovl.is-fixed .ovl-text {
  vector-effect: non-scaling-stroke; stroke-width: 1.5; }
</style>
