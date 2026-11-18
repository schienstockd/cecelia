<!--
  The ONE "+ Plot…" picker every plot surface uses (module pages, the Analysis board, the cluster,
  model-training and track canvases). Plots are grouped by the question they answer
  (plots/plotPicker.ts → PLOT_CATEGORIES), in the same order everywhere, with a search that matches
  the plot's name and its question (and a few user words: "velocity" → speed).

  Two looks, one switch (`settings.plotPickerPreviews`, also in Preferences → Plots):
    • previews on  — a category list on the left (when there are two or more), a card per plot on the
      right with a schematic of its shape (PlotSketch) and the question it answers;
    • previews off — a plain grouped menu, the question on hover.

  The host hands in `PlotOption`s and gets the picked `value` back through `@pick`; what a value
  means (a spec id, `summary:<id>`, a view key) stays the host's business.
-->
<script setup lang="ts">
import { ref, computed, watch, nextTick } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
import PlotSketch from './PlotSketch.vue'
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
const sectionEls = new Map<PlotCategory, HTMLElement>()
const activeCat = ref<PlotCategory | null>(null)

const groups = computed(() =>
  groupPlotOptions(props.options.filter(o => matchesPlotQuery(o, query.value))))
const showRail = computed(() => settings.plotPickerPreviews && groupPlotOptions(props.options).length > 1)

watch(open, async v => {
  if (!v) { query.value = ''; activeCat.value = null; return }
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

function jumpTo(id: PlotCategory) {
  activeCat.value = id
  query.value = ''
  nextTick(() => sectionEls.get(id)?.scrollIntoView({ block: 'start', behavior: 'smooth' }))
}

function setSectionEl(id: PlotCategory, el: unknown) {
  if (el instanceof HTMLElement) sectionEls.set(id, el)
  else sectionEls.delete(id)
}
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

      <!-- previews: category rail + cards -->
      <div v-else-if="settings.plotPickerPreviews" class="pp-body">
        <nav v-if="showRail" class="pp-rail">
          <button v-for="g in groupPlotOptions(options)" :key="g.id" type="button" class="pp-rail-item"
                  :class="{ 'pp-rail-on': activeCat === g.id }" @click="jumpTo(g.id)">
            {{ g.label }}
          </button>
        </nav>
        <div class="pp-cards">
          <section v-for="g in groups" :key="g.id" :ref="el => setSectionEl(g.id, el)" class="pp-section">
            <div class="cc-eyebrow cc-fs-2xs pp-section-head">{{ g.label }}</div>
            <button v-for="o in g.options" :key="o.value" type="button" class="pp-card" @click="pick(o)">
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
.pp-wide { width: min(600px, 92vw); }
.pp-head { padding: 0.4rem; gap: 0.3rem; border-bottom: 1px solid var(--cc-border); }
.pp-search { flex: 1; min-width: 0; }
.pp-empty { padding: 0.8rem; }

.pp-body { display: flex; min-height: 0; flex: 1; }
.pp-rail { display: flex; flex-direction: column; flex-shrink: 0; width: 150px; padding: 0.3rem 0;
           border-right: 1px solid var(--cc-border); background: var(--cc-surface-1); overflow-y: auto; }
.pp-rail-item { text-align: left; padding: 0.35rem 0.7rem; background: none; border: none; color: var(--cc-text);
                font-size: var(--cc-fs-sm); cursor: pointer; }
.pp-rail-item:hover { background: var(--cc-surface-2); }
.pp-rail-on { color: var(--cc-accent); font-weight: 600; }

.pp-cards { flex: 1; min-width: 0; overflow-y: auto; padding: 0.3rem 0.4rem 0.5rem; }
.pp-section { display: grid; grid-template-columns: 1fr 1fr; gap: 0.3rem; scroll-margin-top: 0.3rem; }
.pp-section + .pp-section { margin-top: 0.5rem; }
.pp-section-head { grid-column: 1 / -1; padding: 0.3rem 0.2rem 0.1rem; }
.pp-card { display: flex; gap: 0.5rem; align-items: flex-start; text-align: left; padding: 0.4rem;
           background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm);
           color: var(--cc-text); cursor: pointer; }
.pp-card:hover, .pp-card:focus-visible { border-color: var(--cc-accent); background: var(--cc-surface-2); }
.pp-card-text { display: flex; flex-direction: column; gap: 0.15rem; min-width: 0; }
.pp-card-label { font-size: var(--cc-fs-sm); font-weight: 600; }
.pp-card-answers { color: var(--cc-text-dim); line-height: 1.25;
                   display: -webkit-box; -webkit-line-clamp: 3; line-clamp: 3; -webkit-box-orient: vertical; overflow: hidden; }

.pp-list { overflow-y: auto; }
</style>
