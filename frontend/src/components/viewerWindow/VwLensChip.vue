<!--
  The channel lens's chip (`composables/useVwLens.ts`, `docs/UI.md` → *Channel lens*): beside the lens,
  never over it — what it draws (its channels, or "Spotlight") and what it holds (cells per
  population; track clusters and regions as their own groups, so each share column means one thing).
  Pinned, it holds still and takes clicks: unpin, @ Kiwi, Draw and note, and Find similar — score
  every cell of the region run against this lens and save it as a lens figure
  (`composables/useVwLensQuery.ts`); the lenses and their Show scores are the Lenses section.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import AddToKiwiButton from '../kiwi/AddToKiwiButton.vue'
import type { KiwiRef } from '../../utils/kiwiRef'
import { placeBox } from '../../utils/anchorPosition'
import type { LensChip } from '../../composables/useVwLens'
import type { LensRegionRun } from '../../utils/viewer/lensQuery'
import { lensReadinessText, type LensReadiness } from '../../utils/viewer/lensReadiness'

/** Find similar's state, from `useVwLensQuery`. */
export interface LensQueryChip {
  runs: LensRegionRun[]
  /** the picked run's key (`vn|suffix`) */
  run: string
  busy: boolean
  error: string
  /** after a run: the lens figure it saved (its name), '' = none yet */
  saved: string
}

const props = defineProps<{
  chip: LensChip
  /** the lens centre on the canvas, CSS px */
  at: { cx: number; cy: number } | null
  radiusPx: number
  canvasW: number
  canvasH: number
  kiwiRef: () => KiwiRef | null
  projectUid?: string
  query?: LensQueryChip
}>()
const emit = defineEmits<{ unpin: []; draw: []; find: []; pickRun: [key: string]; ready: [r: LensReadiness]
                         allFrames: [on: boolean] }>()

const runLabel = (r: LensRegionRun) => props.query && new Set(props.query.runs.map(x => x.vn)).size > 1
  ? `${r.vn} · ${r.suffix}` : r.suffix

/** The channels one per row, this many, then "+n more" — a 20-channel aperture otherwise widened the
 *  chip across the image. */
const CHANNELS_SHOWN = 4
const channelsAll = ref(false)
const size = ref({ width: 0, height: 0 })
function measure(el: unknown) {
  if (!(el instanceof HTMLElement)) return
  const { offsetWidth: width, offsetHeight: height } = el
  if (width !== size.value.width || height !== size.value.height) size.value = { width, height }
}
/** Beside the lens, never over it: right of its circle, flipping left at the canvas edge. */
const style = computed(() => {
  const a = props.at, r = props.radiusPx
  if (!a) return undefined
  const p = placeBox({
    anchor: { left: a.cx - r, top: a.cy - r, width: 2 * r, height: 2 * r }, box: size.value,
    viewport: { width: props.canvasW, height: props.canvasH }, placement: 'right-start', gap: 8, margin: 4,
  })
  return { left: p.left + 'px', top: p.top + 'px' }
})
const extra = computed(() => props.chip.picked.length - CHANNELS_SHOWN)
</script>

<template>
  <div :ref="measure" :class="['cc-card vw-readout', { 'vw-lens-pinned': chip.pinned }]" :style="style">
    <span class="cc-row cc-row-tight vw-readout-head">
      <span class="vw-readout-cell">{{ chip.picked.length ? 'Lens' : 'Spotlight' }}</span>
      <span v-if="chip.timed && !chip.pinned" class="vw-readout-dim">· {{ chip.allFrames ? 'all frames' : 'this frame' }}</span>
      <!-- pinned: the chip holds still, so it takes clicks — unpin, ask Kiwi, note it for Kiwi or a Claude session -->
      <span v-if="chip.pinned" class="cc-row cc-row-tight vw-lens-actions">
        <!-- a time-lapse: count the frame on screen, or every frame (cells inside at any frame) — Find similar too -->
        <button v-if="chip.timed" :class="['cc-btn cc-btn-bare cc-btn-micro vw-lens-frames', { 'cc-btn-on': chip.allFrames }]"
                :aria-pressed="chip.allFrames" aria-label="Count all frames" @click.stop="emit('allFrames', !chip.allFrames)"
                v-tooltip.bottom="chip.allFrames ? 'Counting every frame — click for this frame' : 'Counting this frame — click for every frame'">
          {{ chip.allFrames ? 'All frames' : 'This frame' }}
        </button>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro cc-btn-on" @click.stop="emit('unpin')"
                v-tooltip.bottom="'Unpin — follow the pointer'" aria-label="Unpin the lens">
          <i class="pi pi-thumbtack" />
        </button>
        <AddToKiwiButton :kiwi-ref="kiwiRef" :project-uid="projectUid" tip="Add the lens to Kiwi" />
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click.stop="emit('draw')"
                v-tooltip.bottom="'Draw and note — to Kiwi or a Claude session'" aria-label="Draw and note on the lens">
          <i class="pi pi-pencil" />
        </button>
      </span>
    </span>
    <span v-for="ch in chip.picked.slice(0, channelsAll ? undefined : CHANNELS_SHOWN)" :key="ch.name" class="vw-readout-row">
      <span class="cc-dot" :style="{ background: ch.colour }" /><span class="vw-readout-name">{{ ch.name }}</span>
    </span>
    <!-- the cut-off: a pinned chip takes clicks, so there it opens the rest -->
    <button v-if="chip.pinned && extra > 0"
            class="cc-btn cc-btn-bare cc-btn-micro vw-lens-more" @click.stop="channelsAll = !channelsAll">
      {{ channelsAll ? 'Fewer' : `+${extra} more` }}
    </button>
    <span v-else-if="extra > 0" class="vw-readout-dim">+{{ extra }} more</span>
    <template v-if="chip.cells !== null">
      <span class="vw-readout-dim">{{ chip.cells }} {{ chip.cells === 1 ? 'cell' : 'cells' }}</span>
      <span v-for="p in chip.shown" :key="p.key" class="vw-readout-row">
        <span class="cc-dot" :style="{ background: p.colour }" />
        <span class="vw-readout-name">{{ p.name }}</span><span class="vw-readout-val vw-lens-count">{{ p.n }}</span>
        <span class="vw-readout-val vw-lens-share">{{ p.share }}</span>
      </span>
      <span v-if="chip.more" class="vw-readout-dim">+{{ chip.more }} more</span>
    </template>
    <!-- track clusters, then regions: their own groups, so each share column means one thing -->
    <template v-for="g in chip.groups" :key="g.kind">
      <span class="vw-readout-dim vw-lens-group">{{ g.kind === 'tracks' ? (chip.allFrames ? `${g.of} cells in tracks` : `${g.of} ${g.of === 1 ? 'track' : 'tracks'}`) : 'Regions' }}</span>
      <span v-for="p in g.shown" :key="p.key" class="vw-readout-row">
        <span class="cc-dot" :style="{ background: p.colour }" />
        <span class="vw-readout-name">{{ p.name }}</span><span class="vw-readout-val vw-lens-count">{{ p.n }}</span>
        <span class="vw-readout-val vw-lens-share">{{ p.share }}</span>
      </span>
      <span v-if="g.more" class="vw-readout-dim">+{{ g.more }} more</span>
    </template>
    <!-- a group it cannot show, and why: the layer is off, or never run (`lensReadiness`) — actionable once pinned -->
    <span v-for="r in chip.ready" :key="r.popType" class="vw-readout-dim vw-lens-group vw-lens-ready">
      {{ lensReadinessText(r) }} ·
      <button v-if="chip.pinned" class="cc-btn cc-btn-bare cc-btn-micro vw-lens-ready-act" @click.stop="emit('ready', r)"
              v-tooltip.bottom="r.state === 'notDrawn' ? 'Show the layer' : 'Open the task — it does not start'">{{ r.action }}</button>
      <template v-else>{{ r.action }}</template>
    </span>
    <!-- Find similar: a score column over the region run, then the view coloured by it -->
    <!-- the region run, on its own row above the button — only when there is a choice -->
    <select v-if="chip.pinned && query && query.runs.length > 1" class="cc-fs-2xs vw-lens-query" :value="query.run"
            aria-label="Region run" v-tooltip.bottom="'Region run the lens reads'"
            @change="emit('pickRun', ($event.target as HTMLSelectElement).value)">
      <option v-for="r in query.runs" :key="`${r.vn}|${r.suffix}`" :value="`${r.vn}|${r.suffix}`">{{ runLabel(r) }}</option>
    </select>
    <span v-if="chip.pinned && query" class="cc-row cc-row-tight vw-lens-query">
      <button class="cc-btn cc-btn-micro" :disabled="!query.runs.length || query.busy" @click.stop="emit('find')"
              v-tooltip.bottom="query.runs.length ? 'Score every cell by how like the lens its neighbourhood is' : 'Needs a region clustering run'">
        <i :class="query.busy ? 'pi pi-spin pi-spinner' : 'pi pi-search'" /> {{ query.busy ? 'Scoring…' : 'Find similar' }}
      </button>
    </span>
    <span v-if="chip.pinned && query?.error" class="vw-readout-dim cc-muted-warn">{{ query.error }}</span>
    <span v-else-if="chip.pinned && query?.saved && !query.busy" class="vw-readout-dim"
          v-tooltip.bottom="'Saved to Movies & figures — listed under Lenses'"><i class="pi pi-check" /> {{ query.saved }}</span>
    <span v-if="!chip.pinned" class="vw-readout-dim">Click to pin</span>
  </div>
</template>

<style scoped src="./vwReadout.css"></style>
<style scoped>
/* a pinned lens's chip holds still, so it takes the pointer (its buttons). Two classes, not one: the
   chip's root also carries the window's scope, and the window's own `.vw-readout { pointer-events:
   none }` (vwReadout.css, loaded after this) would otherwise win and send the clicks to the canvas. */
.vw-readout.vw-lens-pinned { pointer-events: auto; }
.vw-lens-actions { margin-left: auto; padding-left: 0.6rem; }
.vw-lens-more { align-self: flex-start; padding: 0; font-size: var(--cc-fs-2xs); }
/* the share column: right-aligned, so the percentages line up under each other */
.vw-lens-count { min-width: 3ch; margin-left: 0.6rem; text-align: right; }
.vw-lens-share { min-width: 4ch; text-align: right; opacity: 0.7; }
.vw-lens-group { margin-top: 0.3rem; }
.vw-lens-query { margin-top: 0.3rem; }
.vw-lens-ready-act { padding: 0; font-size: inherit; text-decoration: underline; }
.vw-lens-frames { padding: 0 0.3rem; font-size: var(--cc-fs-2xs); }
</style>
