<!--
  Where each acquisition setting acts: a schematic light path per imaging technology (two-photon,
  confocal, Airyscan, widefield — lightPath/LightPathFigure*.vue, line art in the guide modal's style)
  with numbered stages, and under it this image's values per stage, each marked with where it came
  from. Content: utils/acquisitionLightPath.ts (`TECHNOLOGY_STAGES`, `lightPathStages`).

  Colours are Okabe–Ito (plots/palettes.json), and nothing relies on colour alone: excitation is a
  solid line, emission a dashed one, and each value source has its own shape. The optics are generic,
  not this scope's real layout — the caption says so.
-->
<script setup lang="ts">
import { computed, ref, type Component } from 'vue'
import LightPathSourceGlyph from './LightPathSourceGlyph.vue'
import LightPathFigureTwoPhoton from './lightPath/LightPathFigureTwoPhoton.vue'
import LightPathFigureConfocal from './lightPath/LightPathFigureConfocal.vue'
import LightPathFigureAiryscan from './lightPath/LightPathFigureAiryscan.vue'
import LightPathFigureWidefield from './lightPath/LightPathFigureWidefield.vue'
import { LIGHT_PATH_COLOURS as C } from './lightPath/colours'
import './lightPath/art.css'
import { acquisitionTechnology, lightPathStages, LIGHT_PATH_SOURCES, type LightPathImage, type Technology } from '../utils/acquisitionLightPath'

const props = defineProps<{ image: LightPathImage }>()

const stages = computed(() => lightPathStages(props.image))

const FIGURES: Record<Technology, Component> = {
  'two-photon': LightPathFigureTwoPhoton, confocal: LightPathFigureConfocal,
  airyscan: LightPathFigureAiryscan, widefield: LightPathFigureWidefield,
}
const figure = computed(() => FIGURES[acquisitionTechnology(props.image.acquisition) ?? 'two-photon'])

// Which stage the pointer is over in the list — lights up its marker. Transient, not a setting.
const hover = ref<number | null>(null)

</script>

<template>
  <div class="lp">
    <component :is="figure" :hover="hover" />
    <p class="lp-cap cc-muted cc-fs-xs">Schematic, not this microscope's layout</p>

    <ol class="lp-stages">
      <li v-for="s in stages" :key="s.key" class="lp-stage"
          @mouseenter="hover = s.n" @mouseleave="hover = null">
        <span class="lp-n">{{ s.n }}</span>
        <span class="lp-t cc-muted">{{ s.title }}</span>
        <ul class="lp-items">
          <li v-for="(it, i) in s.items" :key="i" class="lp-item">
            <LightPathSourceGlyph :source="it.source" class="lp-src" />
            <span :class="{ 'cc-muted': it.source === 'missing' }">{{ it.text }}</span>
          </li>
          <li v-if="!s.items.length" class="lp-item cc-muted">—</li>
        </ul>
      </li>
    </ol>

    <div class="lp-legend cc-row cc-row-loose cc-muted cc-fs-xs">
      <span class="lp-key"><svg class="lp-line" viewBox="0 0 10 10" aria-hidden="true"><path d="M1 5 H9" :stroke="C.exc" stroke-width="2" /></svg>excitation</span>
      <span class="lp-key"><svg class="lp-line" viewBox="0 0 10 10" aria-hidden="true"><path d="M0 5 H4 M6 5 H10" :stroke="C.em" stroke-width="2" /></svg>emission</span>
      <span v-for="src in LIGHT_PATH_SOURCES" :key="src.key" class="lp-key">
        <LightPathSourceGlyph :source="src.key" />{{ src.label }}
      </span>
    </div>
  </div>
</template>

<style scoped>
.lp { display: flex; flex-direction: column; gap: 0.4rem; }
.lp-cap { margin: 0; font-style: italic; }

.lp-stages { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.3rem; }
.lp-stage { display: grid; grid-template-columns: 1.1rem 6.6rem 1fr; column-gap: 0.4rem; align-items: baseline; }
.lp-n { width: 1.1rem; height: 1.1rem; border-radius: 50%; background: var(--cc-text); color: var(--cc-bg);
        font-size: var(--cc-fs-xs); font-weight: 700; display: inline-flex; align-items: center; justify-content: center; }
.lp-stage:hover .lp-n { background: var(--cc-accent); }
.lp-t { white-space: nowrap; }
.lp-items { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.lp-item { display: flex; align-items: flex-start; gap: 0.35rem; word-break: break-word; }
.lp-src { margin-top: 0.25em; }
.lp-line { flex: none; width: 10px; height: 10px; }
.lp-key { display: inline-flex; align-items: center; gap: 0.3rem; }
</style>
