<!--
  ONE-ROW horizontal mini-boxplot for a card's measure. Value axis is X (long side of the row) so a
  card reads top-to-bottom as [row of speed, row of duration, …] — the layout the eye already knows
  from a heatmap table. Drawn against the SHARED per-measure scale so a box's X position AND its fill
  are both comparable card-to-card.

  Encoding, one perceptually-uniform ramp (viridis, magma-adjacent):
    • Rail: full [scaleMin, scaleMax] as a faint horizontal line (implicit — no ticks).
    • Whiskers: raw stat.min → stat.max, thin line + short T-caps.
    • Box (q25–q75): filled with the viridis colour keyed by the MEDIAN's rank on the shared scale.
    • Median: a bright vertical line inside the box (light on dark end of the ramp, dark on light end).

  Contract: `stat` is a `CardStat` (5-number summary in native units); `scale` is `[globalMin,
  globalMax]` for that measure across every card in the response (the FE's `statScales`). Row height
  ~14–20px depending on host; the label is drawn by the caller (a `<td>` in the layout table), so this
  file is only the plot cell.
-->
<script setup lang="ts">
import { computed } from 'vue'
import type { CardStat } from './cellCards'

const props = defineProps<{
  stat: CardStat
  scale: [number, number]
  /** Row width in px (host controls layout; the cell fills what it's given). */
  w?: number
  h?: number
}>()

const W = computed(() => props.w ?? 140)
const H = computed(() => props.h ?? 16)
const PAD_X = 4
const AXIS_Y = computed(() => H.value / 2)
const BOX_H = computed(() => Math.max(6, H.value - 6))
const BOX_Y = computed(() => (H.value - BOX_H.value) / 2)

const x = computed(() => {
  const [lo, hi] = props.scale
  const inner = W.value - 2 * PAD_X
  const map = (v: number) => {
    if (!(hi > lo)) return W.value / 2
    const t = Math.max(0, Math.min(1, (v - lo) / (hi - lo)))
    return PAD_X + t * inner
  }
  return {
    min:    map(props.stat.min),
    q25:    map(props.stat.q25),
    median: map(props.stat.median),
    q75:    map(props.stat.q75),
    max:    map(props.stat.max),
    lo:     PAD_X,
    hi:     W.value - PAD_X,
  }
})

// ── viridis-ish 5-stop ramp — perceptually uniform, colour-blind safe, high contrast at both ends
// (unlike a single-hue lightness ramp). Interpolated in RGB — good enough for this scale.
const VIRIDIS: Array<[number, [number, number, number]]> = [
  [0.00, [ 68,  1, 84]],
  [0.25, [ 59, 82,139]],
  [0.50, [ 33,144,141]],
  [0.75, [ 93,201, 98]],
  [1.00, [253,231, 37]],
]
function viridis(t: number): string {
  t = Math.max(0, Math.min(1, t))
  for (let i = 1; i < VIRIDIS.length; i++) {
    const [t1, c1] = VIRIDIS[i]
    if (t <= t1) {
      const [t0, c0] = VIRIDIS[i - 1]
      const u = (t - t0) / (t1 - t0)
      const r = Math.round(c0[0] + u * (c1[0] - c0[0]))
      const g = Math.round(c0[1] + u * (c1[1] - c0[1]))
      const b = Math.round(c0[2] + u * (c1[2] - c0[2]))
      return `rgb(${r},${g},${b})`
    }
  }
  return 'rgb(253,231,37)'
}

const boxFill = computed(() => {
  const [lo, hi] = props.scale
  if (!(hi > lo)) return viridis(0.5)
  return viridis((props.stat.median - lo) / (hi - lo))
})

// median line contrast: keep it readable across the ramp — white on the dark end, black on the light.
const medianStroke = computed(() => {
  const [lo, hi] = props.scale
  const t = hi > lo ? (props.stat.median - lo) / (hi - lo) : 0.5
  return t < 0.6 ? '#ffffff' : '#0f172a'
})

const shortName = computed(() => props.stat.name.replace(/^live\.track\./, ''))
const fmt = (v: number) => Math.abs(v) >= 100 ? v.toFixed(0) : Math.abs(v) >= 10 ? v.toFixed(1) : v.toFixed(2)
const tip = computed(() =>
  `${shortName.value}\nmin ${fmt(props.stat.min)} · q25 ${fmt(props.stat.q25)} · median ${fmt(props.stat.median)} · q75 ${fmt(props.stat.q75)} · max ${fmt(props.stat.max)}`)
</script>

<template>
  <svg :width="W" :height="H" :viewBox="`0 0 ${W} ${H}`" preserveAspectRatio="none"
       class="sb" role="img" :aria-label="shortName" v-tooltip.top="tip">
    <line :x1="x.lo" :x2="x.hi" :y1="AXIS_Y" :y2="AXIS_Y" class="sb-rail" />
    <!-- whiskers -->
    <line :x1="x.min" :x2="x.max" :y1="AXIS_Y" :y2="AXIS_Y" class="sb-whisk" />
    <line :x1="x.min" :x2="x.min" :y1="BOX_Y" :y2="BOX_Y + BOX_H" class="sb-whisk" />
    <line :x1="x.max" :x2="x.max" :y1="BOX_Y" :y2="BOX_Y + BOX_H" class="sb-whisk" />
    <!-- box (q25 → q75), fill = viridis(median rank on shared scale) -->
    <rect :x="x.q25" :y="BOX_Y" :width="Math.max(1, x.q75 - x.q25)" :height="BOX_H"
          :fill="boxFill" class="sb-box" />
    <!-- median -->
    <line :x1="x.median" :x2="x.median" :y1="BOX_Y" :y2="BOX_Y + BOX_H"
          :stroke="medianStroke" class="sb-median" />
  </svg>
</template>

<style scoped>
.sb { display: block; }
.sb-rail { stroke: var(--cc-border); stroke-width: 1; }
.sb-whisk { stroke: var(--cc-text-dim); stroke-width: 1; stroke-linecap: butt; }
.sb-box { stroke: var(--cc-text-dim); stroke-width: 0.5; }
.sb-median { stroke-width: 1.5; }
</style>
