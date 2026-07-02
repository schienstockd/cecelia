<!--
  Shared plot-styling controls (VisProps) for every canvas population panel — the collapsible
  Layout / Points / Colours / Labels sub-sections ported from the old R plotCharts adjustments.
  Presentational only: reads `vis`, emits `update:vis` patches. Embedded by SeriesPicker (summary
  canvas) and PopulationManager (gating / cluster canvas) so the styling UI lives in ONE place — the
  same knobs everywhere, and the future universal analysis board gets them for free.

  `sections` optionally restricts which sub-sections show (e.g. a plot family with no raw points hides
  Points). Default = all four.
-->
<script setup lang="ts">
import { ref } from 'vue'
import type { VisProps } from '../../plots/plot'

const props = withDefaults(defineProps<{
  vis: VisProps
  sections?: ('layout' | 'points' | 'colours' | 'labels')[]
}>(), { sections: () => ['layout', 'points', 'colours', 'labels'] })
const emit = defineEmits<{ 'update:vis': [patch: Partial<VisProps>] }>()

const open = ref<Record<string, boolean>>({ layout: false, points: false, colours: false, labels: false })
const set = (patch: Partial<VisProps>) => emit('update:vis', patch)
const has = (s: string) => props.sections.includes(s as 'layout')
</script>

<template>
  <div class="po">
    <!-- Layout / scale -->
    <template v-if="has('layout')">
      <button class="po-toggle" @click="open.layout = !open.layout">
        <i :class="open.layout ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Layout</span>
      </button>
      <div v-show="open.layout" class="po-body">
        <label class="po-row"><span>Legend</span>
          <input type="checkbox" :checked="vis.legend" @change="set({ legend: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row" v-tooltip.left="'Log scale on the measure axis'"><span>Log scale</span>
          <input type="checkbox" :checked="vis.logScale" @change="set({ logScale: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row"><span>Gridlines</span>
          <input type="checkbox" :checked="vis.grid" @change="set({ grid: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row" v-tooltip.left="'Rotate x tick labels 45°'"><span>Rotate X labels</span>
          <input type="checkbox" :checked="vis.rotateXLabel" @change="set({ rotateXLabel: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row" v-tooltip.left="'Flip 90° — measure on X, series labels on Y (R coord_flip)'"><span>Rotate 90°</span>
          <input type="checkbox" :checked="vis.rotate"
                 @change="set({ rotate: ($event.target as HTMLInputElement).checked, ...(($event.target as HTMLInputElement).checked ? { facet: false } : {}) })" /></label>
        <label class="po-row" v-tooltip.left="'One small-multiple panel per series (mutually exclusive with rotate)'"><span>Facet</span>
          <input type="checkbox" :checked="vis.facet"
                 @change="set({ facet: ($event.target as HTMLInputElement).checked, ...(($event.target as HTMLInputElement).checked ? { rotate: false } : {}) })" /></label>
        <label class="po-row"><span>Dark theme</span>
          <input type="checkbox" :checked="vis.darkTheme" @change="set({ darkTheme: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row" v-tooltip.left="'Measure-axis range (blank = auto)'"><span>Y min</span>
          <input class="po-txt" type="text" :value="vis.yMin" @change="set({ yMin: ($event.target as HTMLInputElement).value })" /></label>
        <label class="po-row"><span>Y max</span>
          <input class="po-txt" type="text" :value="vis.yMax" @change="set({ yMax: ($event.target as HTMLInputElement).value })" /></label>
      </div>
    </template>

    <!-- Points / data -->
    <template v-if="has('points')">
      <button class="po-toggle" @click="open.points = !open.points">
        <i :class="open.points ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Points</span>
      </button>
      <div v-show="open.points" class="po-body">
        <label class="po-row" v-tooltip.left="'Data offset (beeswarm / random / none)'"><span>Offset</span>
          <select class="po-sel" :value="vis.jitter" @change="set({ jitter: ($event.target as HTMLSelectElement).value as VisProps['jitter'] })">
            <option value="beeswarm">beeswarm</option><option value="random">random</option><option value="none">none</option>
          </select></label>
        <label class="po-row" v-tooltip.left="'Colour points by series (else grey)'"><span>Colour data</span>
          <input type="checkbox" :checked="vis.colorData" @change="set({ colorData: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="po-row"><span>Point size</span>
          <input type="range" min="0.5" max="8" step="0.5" :value="vis.pointSize"
                 @input="set({ pointSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.pointSize }}</span></label>
        <label class="po-row"><span>Point opacity</span>
          <input type="range" min="0.05" max="1" step="0.05" :value="vis.pointOpacity"
                 @input="set({ pointOpacity: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.pointOpacity.toFixed(2) }}</span></label>
      </div>
    </template>

    <!-- Colours -->
    <template v-if="has('colours')">
      <button class="po-toggle" @click="open.colours = !open.colours">
        <i :class="open.colours ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Colours</span>
      </button>
      <div v-show="open.colours" class="po-body">
        <label class="po-row"><span>Palette</span>
          <select class="po-sel" :value="vis.palette" @change="set({ palette: ($event.target as HTMLSelectElement).value as VisProps['palette'] })">
            <option value="standard">standard (population)</option><option value="distinct">distinct</option>
            <option value="okabe-ito">Okabe-Ito</option>
            <option value="tol-bright">Tol bright</option><option value="tol-muted">Tol muted</option>
            <option value="tol-light">Tol light</option><option value="user">user</option>
          </select></label>
        <label v-if="vis.palette === 'user'" class="po-row po-col" v-tooltip.left="'Comma-separated colours/hex, in series order'">
          <span>Colours</span>
          <input class="po-txt wide" type="text" :value="vis.userColors" placeholder="#4477AA,#EE6677,…"
                 @change="set({ userColors: ($event.target as HTMLInputElement).value })" /></label>
      </div>
    </template>

    <!-- Labels / captions -->
    <template v-if="has('labels')">
      <button class="po-toggle" @click="open.labels = !open.labels">
        <i :class="open.labels ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Labels</span>
      </button>
      <div v-show="open.labels" class="po-body">
        <label class="po-row po-col"><span>Title</span>
          <input class="po-txt wide" type="text" :value="vis.title" @change="set({ title: ($event.target as HTMLInputElement).value })" /></label>
        <label class="po-row po-col"><span>X label</span>
          <input class="po-txt wide" type="text" :value="vis.labX" @change="set({ labX: ($event.target as HTMLInputElement).value })" /></label>
        <label class="po-row po-col"><span>Y label</span>
          <input class="po-txt wide" type="text" :value="vis.labY" @change="set({ labY: ($event.target as HTMLInputElement).value })" /></label>
        <label class="po-row"><span>Font size</span>
          <input type="range" min="8" max="20" step="1" :value="vis.fontSize"
                 @input="set({ fontSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.fontSize }}</span></label>
      </div>
    </template>
  </div>
</template>

<style scoped>
.po { display: flex; flex-direction: column; }
.po-toggle { display: flex; align-items: center; gap: 6px; width: 100%; background: none; border: none;
  color: var(--cc-text-dim); cursor: pointer; padding: 6px 8px; font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; }
.po-toggle:hover { color: var(--cc-text); }
.po-body { padding: 4px 10px 10px; display: flex; flex-direction: column; gap: 8px; }
.po-row { display: flex; align-items: center; gap: 8px; color: var(--cc-text-dim); font-size: 11px; }
.po-row > span:first-child { flex: 1; }
.po-row input[type="range"] { flex: 1; max-width: 110px; }
.po-val { width: 2.2rem; text-align: right; font-variant-numeric: tabular-nums; }
.po-sel { font-size: 11px; max-width: 7rem; }
.po-txt { font-size: 11px; width: 4rem; padding: 1px 4px; }
.po-txt.wide { width: 100%; }
.po-col { flex-direction: column; align-items: stretch; gap: 3px; }
.po-col > span:first-child { flex: none; }
</style>
