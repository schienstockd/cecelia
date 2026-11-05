<!--
  What landed in the file, per channel — the counterpart of the light path (AcquisitionLightPath.vue),
  which shows what the microscope was asked to do. One accordion section per channel (one open at a
  time, so a multiplex image stays readable), marked with the warning icon when something measured
  needs a look; the first such channel opens first. Inside: three small series (intensity histogram on
  a log scale, brightest 1 % by depth, by frame) and the numbers behind them, each with its source
  marker. Content: utils/acquisitionDataPath.ts.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { dataPathChannels, dataPathNotes, firstDataPathChannel, sparkPath } from '../utils/acquisitionDataPath'
import type { LightPathImage } from '../utils/acquisitionLightPath'
import { SEVERITY } from '../lib/severity'
import LightPathSourceGlyph from './LightPathSourceGlyph.vue'
import CollapsibleSection from './CollapsibleSection.vue'

const props = defineProps<{ image: LightPathImage }>()

const channels = computed(() => dataPathChannels(props.image))
const notes = computed(() => dataPathNotes(props.image))
const firstOpen = computed(() => firstDataPathChannel(channels.value))
const WARN = SEVERITY.warn

const W = 72
const H = 22
</script>

<template>
  <div class="dp">
    <p class="dp-cap cc-muted cc-fs-xs">Measured on the imported store, before any processing</p>
    <ul v-if="notes.length" class="dp-items">
      <li v-for="(it, i) in notes" :key="i" class="dp-item">
        <LightPathSourceGlyph :source="it.source" class="dp-src" />
        <span>{{ it.text }}</span>
      </li>
    </ul>
    <CollapsibleSection v-for="c in channels" :key="c.index" :label="c.name"
      :tip="c.attention.length ? c.attention.join(' · ') : 'Nothing measured needs a look'"
      accordion="cc.imageMeta.dataChannel" :section="`ch${c.index}`" :default-open="c.index === firstOpen"
      max-height="none">
      <template #after-label>
        <i v-if="c.attention.length" :class="['pi', WARN.icon, 'dp-warn']" :aria-label="WARN.label" />
      </template>
      <div class="dp-ch">
        <div class="dp-sparks">
          <figure v-if="c.hist" class="dp-spark" v-tooltip="'Intensity histogram, 0 to max (log counts)'">
            <svg :viewBox="`0 0 ${W} ${H}`" :width="W" :height="H" aria-hidden="true">
              <path class="dp-bar" :d="sparkPath(c.hist, W, H, { bars: true, log: true })" />
            </svg>
            <figcaption class="cc-muted cc-fs-2xs">histogram</figcaption>
          </figure>
          <figure v-if="c.byZ" class="dp-spark" v-tooltip="'Brightest 1 % per plane, top to bottom'">
            <svg :viewBox="`0 0 ${W} ${H}`" :width="W" :height="H" aria-hidden="true">
              <path class="dp-line" :d="sparkPath(c.byZ, W, H)" />
            </svg>
            <figcaption class="cc-muted cc-fs-2xs">by depth</figcaption>
          </figure>
          <figure v-if="c.byT" class="dp-spark" v-tooltip="'Brightest 1 % per frame, first to last'">
            <svg :viewBox="`0 0 ${W} ${H}`" :width="W" :height="H" aria-hidden="true">
              <path class="dp-line" :d="sparkPath(c.byT, W, H)" />
            </svg>
            <figcaption class="cc-muted cc-fs-2xs">by frame</figcaption>
          </figure>
        </div>
        <ul class="dp-items">
          <li v-for="(it, i) in c.items" :key="i" class="dp-item">
            <LightPathSourceGlyph :source="it.source" class="dp-src" />
            <span :class="{ 'cc-muted': it.source === 'missing' }">{{ it.text }}</span>
          </li>
        </ul>
      </div>
    </CollapsibleSection>
  </div>
</template>

<style scoped>
.dp { display: flex; flex-direction: column; gap: 0.3rem; }
.dp-cap { margin: 0; font-style: italic; }
.dp-ch { display: flex; flex-direction: column; gap: 0.3rem; padding: 0.2rem 0 0.4rem; }
.dp-warn { color: var(--cc-sev-warn); margin-left: 0.35rem; }
.dp-sparks { display: flex; gap: 0.8rem; }
.dp-spark { margin: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.dp-spark svg { display: block; border-bottom: 1px solid var(--cc-border); }
.dp-bar { stroke: var(--cc-text-dim); stroke-width: 1.6; fill: none; }
.dp-line { stroke: var(--cc-text); stroke-width: 1.3; fill: none; stroke-linejoin: round; stroke-linecap: round; }
.dp-items { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.dp-item { display: flex; align-items: flex-start; gap: 0.35rem; word-break: break-word; }
.dp-src { margin-top: 0.25em; }
</style>
