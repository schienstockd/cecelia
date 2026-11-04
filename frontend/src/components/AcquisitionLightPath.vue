<!--
  Where each acquisition setting acts: a schematic two-photon microscope (line art in the guide
  modal's style — ClaudeWaysIn.vue: theme-token strokes, round caps) with numbered stages, and under it
  this image's values per stage, each marked with where it came from. Content: utils/acquisitionLightPath.ts.

  Colours are Okabe–Ito (plots/palettes.json), and nothing relies on colour alone: excitation is a
  solid line, emission a dashed one, and each value source has its own shape. The optics are generic,
  not this scope's real layout — the caption says so.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import palettes from '../plots/palettes.json'
import CollapsibleSection from './CollapsibleSection.vue'
import { lightPathStages, LIGHT_PATH_SOURCES, type LightPathImage } from '../utils/acquisitionLightPath'

const props = defineProps<{ image: LightPathImage }>()

const stages = computed(() => lightPathStages(props.image))

const oi = palettes.palettes['okabe-ito']
const C = { exc: oi[5], em: oi[4], ome: oi[4], file: oi[0], measured: oi[6] }

// Stage markers on the drawing, by stage number.
const MARKERS: Record<number, [number, number]> = {
  1: [38, 12], 2: [113, 12], 3: [178, 10], 4: [276, 124], 5: [234, 168],
  6: [282, 70], 7: [470, 98], 8: [370, 134], 9: [368, 176],
}

// Which stage the pointer is over in the list — lights up its marker. Transient, not a setting.
const hover = ref<number | null>(null)

</script>

<template>
  <div class="lp">
    <CollapsibleSection label="Light path" storage-key="cc.imageMeta.lightPathOpen" max-height="none"
                        tip="Show the schematic microscope">
    <svg class="lp-fig" viewBox="0 0 490 200" fill="none" role="img"
         aria-label="Schematic two-photon light path with numbered stages">
      <!-- 1 laser -->
      <rect class="lp-frame" x="10" y="22" width="56" height="22" rx="4" />
      <path class="lp-ink" d="M20 33 h14 M40 27 v12 M45 27 v12" />
      <path class="lp-exc" :stroke="C.exc" d="M66 33 H100" />
      <!-- 2 power control -->
      <rect class="lp-frame" x="100" y="24" width="26" height="18" rx="3" />
      <path class="lp-dim" d="M105 38 l4 -10 l4 10 l4 -10 l4 10" />
      <path class="lp-exc" :stroke="C.exc" d="M126 33 H168" />
      <!-- 3 scan mirrors -->
      <circle class="lp-dim" cx="172" cy="33" r="10" />
      <circle class="lp-dim" cx="188" cy="33" r="10" />
      <path class="lp-ink" d="M165 42 l14 -18 M181 42 l14 -18" />
      <path class="lp-exc" :stroke="C.exc" d="M188 33 H226" />
      <path class="lp-exc lp-thin" :stroke="C.exc" d="M188 33 L226 26 M188 33 L226 40" />
      <ellipse class="lp-glass" cx="230" cy="33" rx="4" ry="14" />
      <path class="lp-exc" :stroke="C.exc" d="M234 33 H260" />
      <ellipse class="lp-glass" cx="264" cy="33" rx="4" ry="17" />
      <path class="lp-exc" :stroke="C.exc" d="M268 33 H300" />
      <!-- fold down; 6 primary dichroic passes the infrared down, sends emission right -->
      <path class="lp-ink" d="M292 42 l16 -18" />
      <path class="lp-exc" :stroke="C.exc" d="M300 33 V112" />
      <path class="lp-ink lp-heavy" d="M288 90 l26 -20" />
      <!-- 4 objective -->
      <path class="lp-frame" d="M282 112 h36 v8 l-6 20 h-24 l-6 -20 z" />
      <path class="lp-exc" :stroke="C.exc" d="M291 140 L300 166 L309 140" />
      <!-- 5 sample, focus at depth -->
      <rect class="lp-tissue" x="246" y="150" width="108" height="40" rx="4" />
      <path class="lp-dim lp-hair" d="M246 160 h108 M246 172 h108 M246 182 h108" />
      <circle cx="300" cy="166" r="3" :fill="C.exc" />
      <circle cx="300" cy="166" r="7" class="lp-em lp-thin" :stroke="C.em" />
      <!-- emission back up to the dichroic, then out to the detectors -->
      <path class="lp-em lp-thin" :stroke="C.em" d="M305 160 L305 82" />
      <path class="lp-em" :stroke="C.em" d="M305 80 H448" />
      <g v-for="x in [340, 380, 420]" :key="x">
        <path class="lp-ink" :d="`M${x - 7} ${88} l14 -16`" />
        <path class="lp-em lp-thin" :stroke="C.em" :d="`M${x} 80 V94`" />
      </g>
      <path class="lp-em lp-thin" :stroke="C.em" d="M448 80 H460 V94" />
      <!-- filters + 7 detectors -->
      <path v-for="x in [340, 380, 420, 460]" :key="`f${x}`" class="lp-dim lp-filter" :d="`M${x - 6} 92 h12`" />
      <rect v-for="x in [340, 380, 420, 460]" :key="`d${x}`" class="lp-frame" :x="x - 9" y="96" width="18" height="16" rx="3" />
      <!-- 8 digitisation -->
      <path class="lp-ink" d="M340 112 V118 H460 V112 M380 112 V118 M420 112 V118 M400 118 V126" />
      <rect class="lp-frame" x="384" y="126" width="32" height="16" rx="3" />
      <path class="lp-dim" d="M390 138 v-3 M395 138 v-7 M400 138 v-5 M405 138 v-8 M410 138 v-2" />
      <path class="lp-ink" d="M400 142 V152" />
      <!-- 9 stack and time -->
      <path v-for="dy in [0, 8, 16]" :key="dy" class="lp-frame" :d="`M386 ${160 + dy} h32 l10 -6 h-32 z`" />
      <path class="lp-dim" d="M434 178 h32 M461 174 l5 4 l-5 4" />

      <!-- numbered stage markers -->
      <g v-for="(xy, k) in MARKERS" :key="k" :class="['lp-mark', { on: hover === +k }]">
        <circle :cx="xy[0]" :cy="xy[1]" r="7" />
        <text :x="xy[0]" :y="xy[1] + 3">{{ k }}</text>
      </g>
    </svg>
    <p class="lp-cap cc-muted cc-fs-xs">Schematic, not this microscope's layout</p>
    </CollapsibleSection>

    <ol class="lp-stages">
      <li v-for="s in stages" :key="s.key" class="lp-stage"
          @mouseenter="hover = s.n" @mouseleave="hover = null">
        <span class="lp-n">{{ s.n }}</span>
        <span class="lp-t cc-muted">{{ s.title }}</span>
        <ul class="lp-items">
          <li v-for="(it, i) in s.items" :key="i" class="lp-item">
            <svg class="lp-src" viewBox="0 0 10 10" aria-hidden="true">
              <circle v-if="it.source === 'ome'" cx="5" cy="5" r="3.5" :fill="C.ome" />
              <path v-else-if="it.source === 'file'" d="M5 1 L9 5 L5 9 L1 5 Z" :fill="C.file" />
              <path v-else-if="it.source === 'measured'" d="M5 1.2 L9 8.6 H1 Z" :fill="C.measured" />
              <circle v-else cx="5" cy="5" r="3.2" class="lp-missing" />
            </svg>
            <span :class="{ 'cc-muted': it.source === 'missing' }">{{ it.text }}</span>
          </li>
          <li v-if="!s.items.length" class="lp-item cc-muted">—</li>
        </ul>
      </li>
    </ol>

    <div class="lp-legend cc-row cc-row-loose cc-muted cc-fs-xs">
      <span class="lp-key"><svg class="lp-src" viewBox="0 0 10 10" aria-hidden="true"><path d="M1 5 H9" :stroke="C.exc" stroke-width="2" /></svg>excitation</span>
      <span class="lp-key"><svg class="lp-src" viewBox="0 0 10 10" aria-hidden="true"><path d="M0 5 H4 M6 5 H10" :stroke="C.em" stroke-width="2" /></svg>emission</span>
      <span v-for="src in LIGHT_PATH_SOURCES" :key="src.key" class="lp-key">
        <svg class="lp-src" viewBox="0 0 10 10" aria-hidden="true">
          <circle v-if="src.key === 'ome'" cx="5" cy="5" r="3.5" :fill="C.ome" />
          <path v-else-if="src.key === 'file'" d="M5 1 L9 5 L5 9 L1 5 Z" :fill="C.file" />
          <path v-else-if="src.key === 'measured'" d="M5 1.2 L9 8.6 H1 Z" :fill="C.measured" />
          <circle v-else cx="5" cy="5" r="3.2" class="lp-missing" />
        </svg>{{ src.label }}
      </span>
    </div>
  </div>
</template>

<style scoped>
.lp { display: flex; flex-direction: column; gap: 0.4rem; }
.lp-fig { width: 100%; max-width: 560px; height: auto; stroke-linecap: round; stroke-linejoin: round; stroke-width: 1.3; }
.lp-frame { stroke: var(--cc-border); stroke-width: 1.3; fill: var(--cc-surface-1); }
.lp-ink { stroke: var(--cc-text); }
.lp-dim { stroke: var(--cc-text-dim); }
.lp-heavy { stroke-width: 2.2; }
.lp-hair { stroke-width: 0.6; stroke-dasharray: 3 3; }
.lp-filter { stroke-width: 2.5; }
.lp-glass { stroke: var(--cc-text); stroke-width: 1.1; fill: var(--cc-surface-2); }
.lp-tissue { fill: var(--cc-surface-2); stroke: var(--cc-text-dim); }
.lp-exc { stroke-width: 2.4; }
.lp-em { stroke-width: 2.4; stroke-dasharray: 6 3; }
.lp-thin { stroke-width: 1.2; }
.lp-mark circle { fill: var(--cc-text); stroke: none; }
.lp-mark text { fill: var(--cc-bg); font-size: var(--cc-fs-3xs); font-weight: 700; text-anchor: middle; stroke: none; }
.lp-mark.on circle { fill: var(--cc-accent); }
.lp-cap { margin: 0; font-style: italic; }

.lp-stages { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.3rem; }
.lp-stage { display: grid; grid-template-columns: 1.1rem 6.6rem 1fr; column-gap: 0.4rem; align-items: baseline; }
.lp-n { width: 1.1rem; height: 1.1rem; border-radius: 50%; background: var(--cc-text); color: var(--cc-bg);
        font-size: var(--cc-fs-xs); font-weight: 700; display: inline-flex; align-items: center; justify-content: center; }
.lp-stage:hover .lp-n { background: var(--cc-accent); }
.lp-t { white-space: nowrap; }
.lp-items { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.lp-item { display: flex; align-items: flex-start; gap: 0.35rem; word-break: break-word; }
.lp-src { flex: none; width: 10px; height: 10px; margin-top: 0.25em; }
.lp-key .lp-src { margin-top: 0; }
.lp-missing { fill: none; stroke: var(--cc-text-dim); stroke-width: 1.2; }
.lp-key { display: inline-flex; align-items: center; gap: 0.3rem; }
</style>
