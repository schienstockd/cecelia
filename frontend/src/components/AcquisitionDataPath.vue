<!--
  What landed in the file, per channel — the counterpart of the light path (AcquisitionLightPath.vue),
  which shows what the microscope was asked to do. Three small series per channel (intensity
  histogram on a log scale, brightest 1 % by depth, brightest 1 % by frame) and the numbers behind them,
  each with its source marker. Content: utils/acquisitionDataPath.ts.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { dataPathChannels, dataPathNotes, sparkPath } from '../utils/acquisitionDataPath'
import type { LightPathImage } from '../utils/acquisitionLightPath'
import LightPathSourceGlyph from './LightPathSourceGlyph.vue'

const props = defineProps<{ image: LightPathImage }>()

const channels = computed(() => dataPathChannels(props.image))
const notes = computed(() => dataPathNotes(props.image))

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
    <div v-for="c in channels" :key="c.name" class="dp-ch">
      <span class="dp-name">{{ c.name }}</span>
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
  </div>
</template>

<style scoped>
.dp { display: flex; flex-direction: column; gap: 0.6rem; }
.dp-cap { margin: 0; font-style: italic; }
.dp-ch { display: grid; grid-template-columns: 6.6rem 1fr; column-gap: 0.6rem; row-gap: 0.3rem; }
.dp-name { grid-row: span 2; font-weight: 600; word-break: break-word; }
.dp-sparks { display: flex; gap: 0.8rem; }
.dp-spark { margin: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.dp-spark svg { display: block; border-bottom: 1px solid var(--cc-border); }
.dp-bar { stroke: var(--cc-text-dim); stroke-width: 1.6; fill: none; }
.dp-line { stroke: var(--cc-text); stroke-width: 1.3; fill: none; stroke-linejoin: round; stroke-linecap: round; }
.dp-items { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.dp-item { display: flex; align-items: flex-start; gap: 0.35rem; word-break: break-word; }
.dp-src { margin-top: 0.25em; }
</style>
