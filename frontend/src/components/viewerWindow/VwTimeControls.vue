<!--
  The viewer window's time controls (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer; V2 moves them
  onto the image as the playback bar): play / loop, the timepoint scrubber, the cached-timepoints
  strip and Fps. Moved out of `modules/ViewerWindow.vue` unchanged; the loop is
  composables/useVwPlayback.ts. Mount only when the image has time (`nT > 1`): a still image has
  nothing to scrub, buffer or loop, and an `nT == 1` slider stuck at "0 / 0" looks broken.

  The root is `display: contents`, so the rows stay direct flex items of the side panel and lay out as
  they did inline.
-->
<script setup lang="ts">
import { useSettingsStore } from '../../stores/settings'

defineProps<{
  t: number
  nT: number
  /** the stack has z — the rows keep a spacer the width of the Z row's ± button */
  hasZ: boolean
  playing: boolean
  /** playing, but held on an uncached frame — the Fps readout turns amber */
  throttled: boolean
  /** something is loading — the strip's dot blinks */
  busy: boolean
  cells: { state: string }[]
}>()
const emit = defineEmits<{ togglePlay: []; stop: []; goto: [t: number] }>()
const settings = useSettingsStore()
</script>

<template>
  <div class="vw-time">
    <div class="cc-eyebrow cc-fs-2xs">Timepoint</div>
    <!-- Transport (play + loop) fills the label column, so this slider starts where Plane and
         Fps start; readout and the ±-column spacer end it the same way. -->
    <div class="cc-row cc-row-tight vw-slider-row">
      <span class="cc-lbl-col vw-transport">
        <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="emit('togglePlay')"
                v-tooltip.bottom="playing ? 'Pause' : 'Play through the timecourse'"
                :aria-label="playing ? 'Pause' : 'Play'">
          <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
        </button>
        <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': settings.viewerLoop }]"
                @click="settings.viewerLoop = !settings.viewerLoop" :aria-pressed="settings.viewerLoop"
                v-tooltip.top="'Loop — restart from the first timepoint at the end'" aria-label="Loop playback">
          <i class="pi pi-repeat" />
        </button>
      </span>
      <input
        type="range" class="vw-grow" :min="0" :max="nT - 1" :step="1"
        :value="t" @pointerdown="emit('stop')"
        @input="emit('goto', Number(($event.target as HTMLInputElement).value))"
        v-tooltip.bottom="'Scrub the timecourse — cached timepoints are instant'"
      >
      <span class="cc-readout cc-fs-2xs vw-readout-col">{{ t }} / {{ nT - 1 }}</span>
      <span v-if="hasZ" class="cc-btn-icon vw-icon-spacer" aria-hidden="true" />
    </div>

    <!-- Which timepoints are in VRAM: the answer to "will scrubbing there be instant". Bucketed,
         so a long movie does not put one element per frame in the DOM.
         The dot shares this row rather than earning its own: playback holds rather than skip an
         uncached frame (~400 ms each in 3D), so without a cue a working playback looks like a hang —
         but a line that appears and disappears above the strip shoves it up and down every tick,
         which is what read as the buffer trail jiggling. In-row, nothing can reflow. -->
    <div class="vw-striprow" v-tooltip.bottom="'Cached timepoints — the dot blinks while loading'">
      <span class="cc-dot vw-dot" :class="{ 'cc-dot-warn': busy }" />
      <div class="vw-strip">
        <span v-for="(c, i) in cells" :key="i" class="vw-cell" :class="'is-' + c.state" />
      </div>
    </div>

    <!-- Fps keeps its own row — it is a slider. -->
    <div class="cc-row cc-row-tight vw-slider-row">
      <span class="cc-muted cc-fs-2xs cc-lbl-col" :class="{ 'vw-fps-warn': throttled }">Fps</span>
      <input
        type="range" class="vw-grow" :min="1" :max="30" :step="1"
        v-model.number="settings.viewerFps"
        v-tooltip.bottom="'Playback rate — waits rather than skip an uncached frame'"
        aria-label="Playback rate (fps)"
      >
      <!-- Amber readout is the throttled cue; carry the state tooltip HERE — the label /
           slider have the plain description so a hover on either still explains the
           control (`uiCopy.ts` requires an `<input>` to carry its own tooltip). -->
      <span class="cc-readout cc-fs-2xs vw-readout-col" :class="{ 'vw-fps-warn': throttled }"
            v-tooltip.left="throttled
              ? 'Playback throttled — fetches are behind the requested Fps'
              : 'Requested playback rate'">{{ settings.viewerFps }}</span>
      <span v-if="hasZ" class="cc-btn-icon vw-icon-spacer" aria-hidden="true" />
    </div>
  </div>
</template>

<style scoped>
/* The rows are the side panel's flex items, as they were inline: the side panel's
   `.vw-side > * { flex-shrink: 0; flex-grow: 0 }` is scoped to the window, so it is restated here. */
.vw-time { display: contents; }
.vw-time > * { flex-shrink: 0; flex-grow: 0; }
.vw-grow { flex: 1; min-width: 0; }
/* Slider rows: label column, slider, readout column, then the ± button's square (or a spacer the
   same size) — so every slider starts and ends on one x, and the fixed readout slot stops digits
   jittering the slider on every playback tick. Same rules as the window's Z rows. */
.vw-slider-row { flex-wrap: nowrap; }
.vw-readout-col { min-width: 2.6rem; text-align: right; flex: none; white-space: nowrap; }
.vw-icon-spacer { display: inline-block; flex: none; }
.vw-transport { display: inline-flex; align-items: center; gap: 0.15rem; }
.vw-fps-warn { color: var(--cc-sev-warn); font-weight: 600; }
.vw-striprow { display: flex; align-items: center; gap: 0.3rem; }
.vw-strip { display: flex; gap: 1px; height: 0.35rem; flex: 1; min-width: 0; }
/* the waiting light blinks — added to .cc-dot, the shape and tone stay canonical. Always laid out,
   only ever recoloured — the space is reserved so nothing reflows when it lights up. */
.vw-dot.cc-dot-warn { animation: vw-blink 0.9s ease-in-out infinite; }
@keyframes vw-blink { 0%, 100% { opacity: 1 } 50% { opacity: 0.15 } }
@media (prefers-reduced-motion: reduce) { .vw-dot.cc-dot-warn { animation: none } }
.vw-cell { flex: 1; background: var(--cc-surface-2); border-radius: var(--cc-radius-xs); }
.vw-cell.is-resident { background: var(--cc-accent); }
.vw-cell.is-loading { background: var(--cc-accent-tint); }
.vw-cell.is-current { background: var(--cc-text); }
</style>
