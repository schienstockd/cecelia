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
import { ref, computed } from 'vue'
import { facetMode, type VisProps } from '../../plots/plot'
import CcToggle from '../CcToggle.vue'
import { emptyReadout, overrideFor, type PlotReadout } from '../../plots/plotReadout'
import { overrideTooltip, effectiveOf } from '../../plots/autoOverride'
import { useFieldDraft } from '../../composables/useFieldDraft'

const props = withDefaults(defineProps<{
  vis: VisProps
  sections?: ('layout' | 'points' | 'colours' | 'labels' | 'stats')[]
  // What the active plot's last render actually DID, as opposed to what these controls asked for: which
  // stats test `auto` resolved to (and why), and any setting the renderer had to substitute. A control
  // whose value was overridden marks itself amber and explains in its tooltip — see plots/plotReadout.ts.
  readout?: PlotReadout
}>(), { sections: () => ['layout', 'points', 'colours', 'labels'], readout: emptyReadout })
const emit = defineEmits<{ 'update:vis': [patch: Partial<VisProps>] }>()

// the facet mode, migrating the legacy boolean so a canvas saved before the mode existed still
// shows as faceted rather than silently reverting to None
const facetBy = computed(() => facetMode(props.vis))
// …and when the chart can't facet at all, SHOW the effective value ('none') while the select still
// WRITES the preference — the same contract as the gating transform selects (see effectiveOf). An
// ambered control still displaying the mode that was NOT used reads as "your setting is ignored".
const facetOverrideNote = computed(() => overrideFor(props.readout, 'Facet by'))
const facetShown = computed(() => effectiveOf(facetOverrideNote.value, facetBy.value, 'none' as const))

// the x-tick-label rotation the renderer applied without being asked (labels wouldn't fit their bands)
const xLabelOverride = computed(() => overrideFor(props.readout, 'X labels'))
// …and the toggle DISPLAYS that, while still writing the user's preference — same contract as the gating
// transform selects (see effectiveOf). An ambered control showing the value that was NOT used reads as
// "your setting is being ignored".
const rotateXShown = computed<boolean>({
  get: () => effectiveOf(xLabelOverride.value, !!props.vis.rotateXLabel, true),
  set: v => set({ rotateXLabel: v }),
})
const open = ref<Record<string, boolean>>({ layout: false, points: false, colours: false, labels: false, stats: false })
const set = (patch: Partial<VisProps>) => emit('update:vis', patch)

// Free-text fields commit on `@change` (blur / Enter) — an axis caption applied per keystroke would
// re-render the plot on every letter. That leaves them uncontrolled while focused, and Vue force-patches
// an input's `value` on every element patch, so a board re-render mid-typing replaced what was typed
// with the stored value. Same defect (and fix) as the movie filename field. See useFieldDraft.
const titleDraft  = useFieldDraft(() => props.vis.title)
const labXDraft   = useFieldDraft(() => props.vis.labX)
const labYDraft   = useFieldDraft(() => props.vis.labY)
const yMinDraft   = useFieldDraft(() => props.vis.yMin)
const yMaxDraft   = useFieldDraft(() => props.vis.yMax)
const coloursDraft = useFieldDraft(() => props.vis.userColors)
const has = (s: string) => props.sections.includes(s as 'layout')
</script>

<template>
  <div class="po">
    <!-- Layout / scale -->
    <template v-if="has('layout')">
      <button class="po-toggle cc-section-toggle" @click="open.layout = !open.layout">
        <i :class="open.layout ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span class="cc-eyebrow">Layout</span>
      </button>
      <div v-show="open.layout" class="po-body">
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Show the series key beside the plot'"><span>Legend</span>
          <CcToggle aria-label="Legend" :model-value="vis.legend" @update:model-value="set({ legend: $event })" /></div>
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Log scale on the measure axis'"><span>Log scale</span>
          <CcToggle aria-label="Log scale" :model-value="vis.logScale" @update:model-value="set({ logScale: $event })" /></div>
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Draw axis gridlines behind the data'"><span>Gridlines</span>
          <CcToggle aria-label="Gridlines" :model-value="vis.grid" @update:model-value="set({ grid: $event })" /></div>
        <div class="po-row cc-muted cc-fs-xs" :class="{ 'cc-auto-override': !!xLabelOverride }"
             v-tooltip.top="overrideTooltip(xLabelOverride, 'Rotate the x tick labels (angle below)')">
          <span>Rotate X labels<i v-if="xLabelOverride" class="pi pi-exclamation-triangle po-warn" /></span>
          <CcToggle aria-label="Rotate X labels" v-model="rotateXShown" /></div>
        <!-- the angle applies whenever labels are rotated, however that came about -->
        <label v-if="rotateXShown" class="po-row cc-muted cc-fs-xs" v-tooltip.top="'X tick-label angle (degrees)'"><span>X angle</span>
          <input type="range" min="0" max="90" step="5" :value="vis.rotateXAngle ?? 45"
                 @input="set({ rotateXAngle: parseInt(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.rotateXAngle ?? 45 }}°</span></label>
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Flip 90° — measure on X, series labels on Y (R coord_flip)'"><span>Rotate 90°</span>
          <CcToggle aria-label="Rotate 90 degrees" :model-value="vis.rotate"
                 @update:model-value="set({ rotate: $event, ...($event ? { facetBy: 'none' } : {}) })" /></div>
        <!-- WHAT a small-multiple panel is one OF. Image = compare the selected images side by side,
             each panel holding that image's segmentations/populations. -->
        <label class="po-row cc-muted cc-fs-xs" :class="{ 'cc-auto-override': !!facetOverrideNote }"
               v-tooltip.top="overrideTooltip(facetOverrideNote, 'Split into small multiples (mutually exclusive with rotate)')">
          <span>Facet by<i v-if="facetOverrideNote" class="pi pi-exclamation-triangle po-warn" /></span>
          <select class="po-sel" :value="facetShown"
                  @change="set({ facetBy: ($event.target as HTMLSelectElement).value as VisProps['facetBy'],
                                 ...(($event.target as HTMLSelectElement).value !== 'none' ? { rotate: false } : {}) })">
            <option value="none">None</option>
            <option value="image">Image</option>
            <option value="series">Series</option>
          </select></label>
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Dark plot background; export always uses light'"><span>Dark theme</span>
          <CcToggle aria-label="Dark theme" :model-value="vis.darkTheme" @update:model-value="set({ darkTheme: $event })" /></div>
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Measure-axis range (blank = auto)'"><span>Y min</span>
          <input class="po-txt" type="text" v-model="yMinDraft" @change="set({ yMin: yMinDraft })" /></label>
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Measure-axis range (blank = auto)'"><span>Y max</span>
          <input class="po-txt" type="text" v-model="yMaxDraft" @change="set({ yMax: yMaxDraft })" /></label>
      </div>
    </template>

    <!-- Points / data -->
    <template v-if="has('points')">
      <button class="po-toggle cc-section-toggle" @click="open.points = !open.points">
        <i :class="open.points ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span class="cc-eyebrow">Points</span>
      </button>
      <div v-show="open.points" class="po-body">
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Data offset (beeswarm / random / none)'"><span>Offset</span>
          <select class="po-sel" :value="vis.jitter" @change="set({ jitter: ($event.target as HTMLSelectElement).value as VisProps['jitter'] })">
            <option value="beeswarm">beeswarm</option><option value="random">random</option><option value="none">none</option>
          </select></label>
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Colour points by series (else grey)'"><span>Colour data</span>
          <CcToggle aria-label="Colour data" :model-value="vis.colorData" @update:model-value="set({ colorData: $event })" /></div>
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Radius of each plotted cell'"><span>Point size</span>
          <input type="range" min="0.5" max="8" step="0.5" :value="vis.pointSize"
                 @input="set({ pointSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.pointSize }}</span></label>
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Lower it where points overlap heavily'"><span>Point opacity</span>
          <input type="range" min="0.05" max="1" step="0.05" :value="vis.pointOpacity"
                 @input="set({ pointOpacity: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.pointOpacity.toFixed(2) }}</span></label>
      </div>
    </template>

    <!-- Colours -->
    <template v-if="has('colours')">
      <button class="po-toggle cc-section-toggle" @click="open.colours = !open.colours">
        <i :class="open.colours ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span class="cc-eyebrow">Colours</span>
      </button>
      <div v-show="open.colours" class="po-body">
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Colour scheme series are drawn in'"><span>Palette</span>
          <select class="po-sel" :value="vis.palette" @change="set({ palette: ($event.target as HTMLSelectElement).value as VisProps['palette'] })">
            <option value="standard">standard (population)</option><option value="distinct">distinct</option>
            <option value="cecelia">Cecelia</option>
            <option value="okabe-ito">Okabe-Ito</option>
            <option value="tol-bright">Tol bright</option><option value="tol-muted">Tol muted</option>
            <option value="tol-light">Tol light</option><option value="user">user</option>
          </select></label>
        <label v-if="vis.palette === 'user'" class="po-row po-col cc-muted cc-fs-xs" v-tooltip.top="'Comma-separated colours/hex, in series order'">
          <span>Colours</span>
          <input class="po-txt wide" type="text" v-model="coloursDraft" placeholder="#4477AA,#EE6677,…"
                 @change="set({ userColors: coloursDraft })" /></label>
      </div>
    </template>

    <!-- Stats — between-group hypothesis test (applies to bar/boxplot/violin/strip) -->
    <template v-if="has('stats')">
      <button class="po-toggle cc-section-toggle" @click="open.stats = !open.stats">
        <i :class="open.stats ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span class="cc-eyebrow">Stats</span>
      </button>
      <div v-show="open.stats" class="po-body">
        <div class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Between-group test — Mann-Whitney (2 groups) / Kruskal-Wallis (>2) by default'"><span>Compare groups</span>
          <CcToggle aria-label="Compare groups" :model-value="!!vis.statsEnabled" @update:model-value="set({ statsEnabled: $event })" /></div>
        <label v-if="vis.statsEnabled" class="po-row cc-muted cc-fs-xs" v-tooltip.top="'auto = Mann-Whitney (2) / Kruskal-Wallis (>2)'"><span>Test</span>
          <select class="po-sel" :value="vis.statsTest ?? 'auto'"
                  @change="set({ statsTest: ($event.target as HTMLSelectElement).value as VisProps['statsTest'] })">
            <option value="auto">auto</option>
            <option value="mannwhitney">Mann-Whitney U</option>
            <option value="ttest">Welch's t-test</option>
            <option value="kruskal">Kruskal-Wallis</option>
            <option value="anova">One-way ANOVA</option>
          </select></label>
        <div v-if="vis.statsEnabled && (vis.statsTest ?? 'auto') === 'auto' && readout.stats.note"
             class="po-note cc-muted cc-fs-2xs"
             v-tooltip.top="readout.stats.reason || 'Test the active plot ran'">{{ readout.stats.note }}</div>
        <div v-if="vis.statsEnabled" class="po-row cc-muted cc-fs-xs" v-tooltip.top="'One letter per group; shared letter = no difference'"><span>Compact letters</span>
          <CcToggle aria-label="Compact letters" :model-value="!!vis.statsUseLetters" @update:model-value="set({ statsUseLetters: $event })" /></div>
        <div v-if="vis.statsEnabled && !vis.statsUseLetters" class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Also show non-significant brackets'"><span>Show ns</span>
          <CcToggle aria-label="Show ns" :model-value="!!vis.statsShowNs" @update:model-value="set({ statsShowNs: $event })" /></div>
        <div v-if="vis.statsEnabled && !vis.statsUseLetters" class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Swap p-values for the * / ** / *** ladder'"><span>Stars only</span>
          <CcToggle aria-label="Stars only" :model-value="!!vis.statsUseStars" @update:model-value="set({ statsUseStars: $event })" /></div>
      </div>
    </template>

    <!-- Labels / captions -->
    <template v-if="has('labels')">
      <button class="po-toggle cc-section-toggle" @click="open.labels = !open.labels">
        <i :class="open.labels ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span class="cc-eyebrow">Labels</span>
      </button>
      <div v-show="open.labels" class="po-body">
        <label class="po-row po-col cc-muted cc-fs-xs" v-tooltip.top="'Heading above the plot (blank = none)'"><span>Title</span>
          <input class="po-txt wide" type="text" v-model="titleDraft" @change="set({ title: titleDraft })" /></label>
        <label class="po-row po-col cc-muted cc-fs-xs" v-tooltip.top="'X axis caption (blank = the measure name)'"><span>X label</span>
          <input class="po-txt wide" type="text" v-model="labXDraft" @change="set({ labX: labXDraft })" /></label>
        <label class="po-row po-col cc-muted cc-fs-xs" v-tooltip.top="'Y axis caption (blank = the measure name)'"><span>Y label</span>
          <input class="po-txt wide" type="text" v-model="labYDraft" @change="set({ labY: labYDraft })" /></label>
        <label class="po-row cc-muted cc-fs-xs" v-tooltip.top="'Type size for titles, axes and tick labels'"><span>Font size</span>
          <input type="range" min="8" max="20" step="1" :value="vis.fontSize"
                 @input="set({ fontSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="po-val">{{ vis.fontSize }}</span></label>
      </div>
    </template>
  </div>
</template>

<style scoped>
/* inline warning glyph on an auto-overridden row (amber comes from .cc-auto-override) */
.po-warn { margin-left: 4px; }
.po { display: flex; flex-direction: column; }
/* + cc-section-toggle (row) — this keeps only the padding and the uppercase section-label tier */
.po-toggle { padding: 6px 8px; }
.po-body { padding: 4px 10px 10px; display: flex; flex-direction: column; gap: 8px; }
.po-row { display: flex; align-items: center; gap: 8px; }
.po-row > span:first-child { flex: 1; }
.po-row input[type="range"] { flex: 1; max-width: 110px; }
.po-val { width: 2.2rem; text-align: right; font-variant-numeric: tabular-nums; }
.po-sel { font-size: var(--cc-fs-xs); max-width: 7rem; }
.po-txt { font-size: var(--cc-fs-xs); width: 4rem; padding: 1px 4px; }
.po-txt.wide { width: 100%; }
/* readout under the Test select — the test `auto` actually resolved to (right-aligned to the select) */
.po-note { margin-top: -5px; text-align: right; }
.po-col { flex-direction: column; align-items: stretch; gap: 3px; }
.po-col > span:first-child { flex: none; }
</style>
