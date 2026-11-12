<!--
  The viewer window's time controls, on the playback bar (docs/todo/GUI_LAYOUT_PLAN.md, V2). The loop
  is composables/useVwPlayback.ts. Mount only when the image has time (`nT > 1`): a still image has
  nothing to scrub, buffer or loop.

  Default: the bar's time row — play, loop, the scrubber with the cache painted on its own track, t,
  and an Fps chip whose slider is in a popover. Each control sits in a fixed column of the bar's grid
  (`VwPlaybackBar`), so the z row below lines up and nothing shifts while it plays.
  `pill`: the folded bar — play and t.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
import { useSettingsStore } from '../../stores/settings'
import { cacheGradient, cacheRuns, type CellState } from '../../utils/volumeCache'

const props = defineProps<{
  /** play and t only, for the folded playback bar (default: the bar's time row, grid columns 1–5) */
  pill?: boolean
  t: number
  nT: number
  playing: boolean
  /** playing, but held on an uncached frame — the Fps chip turns amber */
  throttled: boolean
  /** something is loading — a dot blinks beside t */
  busy: boolean
  cells: { state: CellState }[]
}>()
const emit = defineEmits<{ togglePlay: []; stop: []; goto: [t: number] }>()
const settings = useSettingsStore()
// the bar's scrubber track: which timepoints are instant to scrub to
const track = computed(() => cacheGradient(cacheRuns(props.cells)))
const fpsOpen = ref(false)
const fpsAnchor = ref<HTMLElement | null>(null)
</script>

<template>
  <template v-if="pill">
    <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="emit('togglePlay')"
            v-tooltip.top="playing ? 'Pause' : 'Play through the timecourse'"
            :aria-label="playing ? 'Pause' : 'Play'">
      <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
    </button>
    <span class="cc-readout cc-fs-2xs vw-tro">t {{ t }} / {{ nT - 1 }}<span class="cc-dot vw-dot" :class="busy ? 'cc-dot-warn' : 'vw-dot-off'" /></span>
  </template>
  <template v-else>
    <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" style="grid-column: 1" @click="emit('togglePlay')"
            v-tooltip.top="playing ? 'Pause' : 'Play through the timecourse'"
            :aria-label="playing ? 'Pause' : 'Play'">
      <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
    </button>
    <button :class="['cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense', { 'cc-btn-on cc-btn-on-tint': settings.viewerLoop }]"
            style="grid-column: 2"
            @click="settings.viewerLoop = !settings.viewerLoop" :aria-pressed="settings.viewerLoop"
            v-tooltip.top="'Loop — restart from the first timepoint at the end'" aria-label="Loop playback">
      <i class="pi pi-repeat" />
    </button>
    <input
      type="range" class="cc-range-paint vw-scrub" style="grid-column: 3" :style="{ '--cc-range-track': track }"
      :min="0" :max="nT - 1" :step="1"
      :value="t" @pointerdown="emit('stop')"
      @input="emit('goto', Number(($event.target as HTMLInputElement).value))"
      v-tooltip.top="'Scrub the timecourse — the filled track is cached and instant'"
      aria-label="Timepoint"
    >
    <span class="cc-readout cc-fs-2xs vw-tro" style="grid-column: 4">t {{ t }} / {{ nT - 1 }}<span
      class="cc-dot vw-dot" :class="busy ? 'cc-dot-warn' : 'vw-dot-off'" /></span>
    <button ref="fpsAnchor" class="cc-btn cc-btn-ghost cc-btn-dense vw-chip" style="grid-column: 5"
            :class="{ 'cc-btn-on': fpsOpen, 'vw-fps-warn': throttled }" aria-haspopup="dialog"
            v-tooltip.top="throttled
              ? 'Playback throttled — fetches are behind the requested Fps'
              : 'Playback rate'"
            @click="fpsOpen = !fpsOpen">{{ settings.viewerFps }} fps</button>
    <TeleportPopover v-model="fpsOpen" :anchor="fpsAnchor" placement="bottom-end">
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs">Fps</span>
        <input
          type="range" class="vw-fps-slider" :min="1" :max="30" :step="1"
          v-model.number="settings.viewerFps"
          v-tooltip.top="'Playback rate — waits rather than skip an uncached frame'"
          aria-label="Playback rate (fps)"
        >
        <span class="cc-readout cc-fs-2xs">{{ settings.viewerFps }}</span>
      </div>
    </TeleportPopover>
  </template>
</template>

<style scoped>
.vw-tro { text-align: right; white-space: nowrap; }
.vw-chip { width: 100%; justify-content: center; border: 1px solid var(--cc-border); font-variant-numeric: tabular-nums; }
.vw-fps-slider { width: 7rem; }
.vw-fps-warn { color: var(--cc-sev-warn); font-weight: 600; }
/* the scrubber's track IS the cache strip (`.cc-range-paint`) — the thumb marks where t is */
.vw-scrub { width: 100%; min-width: 0; }
/* the loading light blinks — added to .cc-dot, the shape and tone stay canonical */
/* always laid out, only shown while loading — so t never shifts when it lights up */
.vw-dot { margin-left: 0.25rem; vertical-align: middle; }
.vw-dot-off { visibility: hidden; }
.vw-dot.cc-dot-warn { animation: vw-blink 0.9s ease-in-out infinite; }
@keyframes vw-blink { 0%, 100% { opacity: 1 } 50% { opacity: 0.15 } }
@media (prefers-reduced-motion: reduce) { .vw-dot.cc-dot-warn { animation: none } }
</style>
