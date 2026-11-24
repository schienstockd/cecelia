<!--
  The ONE "+ Plot…" picker every plot surface uses (module pages, the Analysis board, the cluster,
  model-training and track canvases). Plots are grouped by the question they answer
  (plots/plotPicker.ts → PLOT_CATEGORIES), in the same order everywhere, with a search that matches
  the plot's name and its question (and a few user words: "velocity" → speed).

  Two looks, one switch (`settings.plotPickerPreviews`, also in Preferences → Plots):
    • previews on  — the KINDS of plot in a column on the left, each with its chart shape
      (PlotKindSketch — every "Compare groups" plot opens as a box plot, so the shape is drawn once
      there), and the picked kind's plots beside it under the kind's one-line blurb, a card each with
      what it is about (PlotSketch: a cell, a track, two images) and the question it answers — in the
      card, never a tooltip, so nothing hovers over the next card or column. Hovering a kind shows its plots (after a
      short pause, so crossing the column on the way to a card doesn't flip it), clicking pins it.
      Typing shows every match beside the column, and the hovered card's kind lights up in it; a host
      with one kind only drops the column;
    • previews off — a plain grouped menu, the question on hover.

  The host hands in `PlotOption`s and gets the picked `value` back through `@pick`; what a value
  means (a spec id, `summary:<id>`, a view key) stays the host's business.
-->
<script setup lang="ts">
import { ref, computed, watch, nextTick } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
import PlotSketch from './PlotSketch.vue'
import PlotKindSketch from './PlotKindSketch.vue'
import { useSettingsStore } from '../../stores/settings'
import { groupPlotOptions, matchesPlotQuery, type PlotOption, type PlotCategory } from '../../plots/plotPicker'

const props = withDefaults(defineProps<{
  options: PlotOption[]
  label?: string
  tip?: string
  dataGuide?: string
}>(), { label: '+ Plot…', tip: 'Add a plot', dataGuide: undefined })

const emit = defineEmits<{ pick: [value: string] }>()

const settings = useSettingsStore()
const open = ref(false)
const btn = ref<HTMLElement | null>(null)
const searchEl = ref<HTMLInputElement | null>(null)
const query = ref('')
const picked = ref<PlotCategory | null>(null)   // the kind clicked in the column; null = the first

const allGroups = computed(() => groupPlotOptions(props.options))
const groups = computed(() =>
  groupPlotOptions(props.options.filter(o => matchesPlotQuery(o, query.value))))
const hoveredCard = ref<PlotCategory | null>(null)   // a card's kind, lit in the column while searching
const searching = computed(() => !!query.value.trim())
const showKinds = computed(() => allGroups.value.length > 1)
const activeKind = computed(() =>
  searching.value ? null : allGroups.value.find(g => g.id === picked.value)?.id ?? allGroups.value[0]?.id ?? null)
const activeGroup = computed(() => allGroups.value.find(g => g.id === activeKind.value) ?? null)
// the cards beside the column: every match while searching, else the active kind's plots
const cardGroups = computed(() =>
  searching.value ? groups.value : groups.value.filter(g => g.id === activeKind.value))

watch(open, async v => {
  if (!v) { query.value = ''; picked.value = null; hoveredCard.value = null; clearTimeout(hoverTimer); return }
  await nextTick()
  searchEl.value?.focus()
})

function pick(o: PlotOption) {
  open.value = false
  emit('pick', o.value)
}

// Enter picks the first match — type "velocity", press Enter, get Track measures
function pickFirst() {
  const first = groups.value[0]?.options[0]
  if (first) pick(first)
}

function showKind(id: PlotCategory) {
  clearTimeout(hoverTimer)
  picked.value = id
  query.value = ''
}

// hover intent: a kind shows its plots only once the pointer rests on it, never while searching
let hoverTimer: ReturnType<typeof setTimeout> | undefined
function hoverKind(id: PlotCategory) {
  clearTimeout(hoverTimer)
  if (searching.value) return
  hoverTimer = setTimeout(() => { picked.value = id }, 150)
}
function leaveKind() { clearTimeout(hoverTimer) }
</script>

<template>
  <button ref="btn" type="button" class="cc-btn cc-btn-ghost plot-picker-btn" :class="{ 'cc-btn-on': open }"
          :data-guide="dataGuide" v-tooltip.bottom="tip" :aria-expanded="open" @click="open = !open">
    {{ label }} <i class="pi pi-chevron-down" />
  </button>
  <TeleportPopover v-model="open" :anchor="btn" placement="bottom-start" flush>
    <div class="pp" :class="{ 'pp-wide': settings.plotPickerPreviews }">
      <div class="pp-head cc-row">
        <input ref="searchEl" v-model="query" type="search" class="pp-search" placeholder="Search plots…"
               v-tooltip.bottom="'Search by name or question, e.g. speed'"
               @keydown.enter.prevent="pickFirst" />
        <button type="button" class="cc-btn cc-btn-bare cc-btn-icon"
                :class="{ 'cc-btn-on cc-btn-on-tint': settings.plotPickerPreviews }"
                v-tooltip.bottom="settings.plotPickerPreviews ? 'Hide plot previews' : 'Show plot previews'"
                @click="settings.plotPickerPreviews = !settings.plotPickerPreviews">
          <i class="pi pi-images" />
        </button>
      </div>

      <div v-if="!groups.length" class="cc-empty-inline pp-empty">No plot matches “{{ query }}”</div>

      <!-- previews: the kinds of plot in a column, the active kind's plots beside it -->
      <div v-else-if="settings.plotPickerPreviews" class="pp-body">
        <nav v-if="showKinds" class="pp-kinds">
          <button v-for="g in allGroups" :key="g.id" type="button" class="pp-kind"
                  :class="{ 'pp-kind-on': activeKind === g.id || (searching && hoveredCard === g.id) }"
                  @click="showKind(g.id)" @mouseenter="hoverKind(g.id)" @mouseleave="leaveKind">
            <PlotKindSketch :category="g.id" />
            <span class="pp-card-label">{{ g.label }}</span>
          </button>
        </nav>
        <div class="pp-cards">
          <!-- the kind's blurb heads its plots: a tip on the kind would cover the cards beside it -->
          <div v-if="showKinds && activeGroup" class="cc-muted cc-fs-2xs pp-blurb">{{ activeGroup.blurb }}</div>
          <section v-for="g in cardGroups" :key="g.id" class="pp-section">
            <div v-if="searching" class="cc-eyebrow cc-fs-2xs pp-section-head">{{ g.label }}</div>
            <button v-for="o in g.options" :key="o.value" type="button" class="pp-card"
                    @click="pick(o)"
                    @mouseenter="hoveredCard = o.category" @mouseleave="hoveredCard = null">
              <PlotSketch :kind="o.sketch" />
              <span class="pp-card-text">
                <span class="pp-card-label">{{ o.label }}</span>
                <span v-if="o.answers" class="pp-card-answers cc-fs-2xs">{{ o.answers }}</span>
              </span>
            </button>
          </section>
        </div>
      </div>

      <!-- plain: grouped menu -->
      <div v-else class="cc-actions-menu pp-list">
        <template v-for="g in groups" :key="g.id">
          <div class="cc-actions-head">{{ g.label }}</div>
          <button v-for="o in g.options" :key="o.value" type="button" class="cc-actions-item"
                  v-tooltip.right="o.answers ?? ''" @click="pick(o)">
            {{ o.label }}
          </button>
        </template>
      </div>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.plot-picker-btn { gap: 0.35rem; }
.plot-picker-btn .pi { font-size: var(--cc-fs-2xs); color: var(--cc-text-dim); }

.pp { display: flex; flex-direction: column; width: 240px; max-height: min(70vh, 560px); }
.pp-wide { width: min(660px, 94vw); }
.pp-head { padding: 0.4rem; gap: 0.3rem; border-bottom: 1px solid var(--cc-border); }
.pp-search { flex: 1; min-width: 0; }
.pp-empty { padding: 0.8rem; }

.pp-body { display: flex; min-height: 0; flex: 1; }
.pp-kinds { display: flex; flex-direction: column; gap: 0.25rem; flex-shrink: 0; width: 190px; padding: 0.4rem;
            border-right: 1px solid var(--cc-border); background: var(--cc-surface-1); overflow-y: auto; }
.pp-kind { display: flex; align-items: center; gap: 0.5rem; text-align: left; padding: 0.3rem 0.4rem;
           background: none; border: 1px solid transparent; border-radius: var(--cc-radius-sm);
           color: var(--cc-text); cursor: pointer; }
.pp-kind .plot-kind-sketch { width: 70px; height: 42px; }
.pp-kind:hover, .pp-kind:focus-visible { background: var(--cc-surface-2); }
.pp-kind-on { border-color: var(--cc-accent); background: var(--cc-surface-2); }
.pp-kind-on .pp-card-label { color: var(--cc-accent); }

.pp-cards { flex: 1; min-width: 0; min-height: 0; overflow-y: auto; padding: 0.4rem; }
.pp-section { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.25rem; }
.pp-blurb { padding: 0.1rem 0.2rem 0.35rem; }
.pp-section + .pp-section { margin-top: 0.5rem; }
.pp-section-head { grid-column: 1 / -1; padding: 0.3rem 0.2rem 0.1rem; }
.pp-card { display: flex; gap: 0.4rem; align-items: center; text-align: left; padding: 0.25rem 0.35rem;
           background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm);
           color: var(--cc-text); cursor: pointer; }
.pp-card:hover, .pp-card:focus-visible { border-color: var(--cc-accent); background: var(--cc-surface-2); }
.pp-card-text { display: flex; flex-direction: column; gap: 0.15rem; min-width: 0; }
.pp-card-label { font-size: var(--cc-fs-sm); font-weight: 600; }
.pp-card-answers { color: var(--cc-text-dim); line-height: 1.25;
                   display: -webkit-box; -webkit-line-clamp: 3; line-clamp: 3; -webkit-box-orient: vertical; overflow: hidden; }

.pp-list { overflow-y: auto; }
</style>
