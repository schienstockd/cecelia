<!--
  Shared two-column module layout.
  Left column: SetBar + count bar + optional attr filter + collapsible ImageTable
               + a consistent collapsible #plots canvas + optional #below-table slot.
  Right column: slot #right — TaskRunner, MetadataPanel, or a custom panel.

  Props
  ─────
    module        string?   Passed to ImageTable for per-module column config.
    allowManage   bool      SetBar: show New/Rename/Delete set controls (default: false).
    showAttrs     bool      ImageTable: show attr columns (default: false).
    editableMeta  bool      ImageTable: allow inline attr/channel-name editing — Metadata page ONLY
                            (default: false; every other page shows these read-only).
    showFilter    bool      Show the attr-value filter panel (default: true).
    plotsLabel    string    Heading for the #plots section (default: 'Plots').
    noSetHint     string    Custom empty-state message.

  Slots
  ─────
    #actions  { hasSet, setUid, selectedUids,
                selectUids }                    — extra items in the action bar. The selection is
                                                  passed in so a bar item can ACT on it (Import's
                                                  copy/move/remove); `selectUids([])` clears it.
    #right    { setUid, selectedUids,
                selectedNames }                 — the right-hand panel.
    #plots    { setUid, selectedUids,
                selectedNames, selectUids,
                orderedUids }                   — the module's plot canvas. ModuleLayout wraps it in
                                                  ONE consistent, collapse-persisted CollapsibleSection
                                                  (labelled `plotsLabel`) — do NOT wrap it yourself.
                                                  This is how every module page gets the SAME
                                                  collapsible plot canvas.
    #below-table { setUid, selectedUids,
                   selectedNames, selectUids }  — extra custom content below the plots (rare).
                                                  Wrap each piece in <CollapsibleSection> yourself.
                                                  `selectUids(uids)` drives the image selection.

  Emits
  ─────
    selectionChange(uids: string[])             — whenever the image selection changes.

  See docs/UI.md for the full module page authoring guide.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore } from '../stores/taskDefs'
import { isExcluded } from '../utils/inclusion'
import { ROW_FILTERS, rowFilterKey, anyRowFilterActive, hiddenByRowFilters } from '../utils/rowFilters'
import { emptyAttrFilter, attrFilterActive, matchesAttrFilter, attrKeysOf,
         type AttrFilterState } from '../utils/attrFilter'
import AttrFilterPanel from './AttrFilterPanel.vue'
import { funsRunAcross, wasProcessedWith, funModuleLabel, type ProcMode } from '../utils/runLog'
import { imageTableCsvRows } from '../utils/imageTable'
import { rowsToCsv, downloadBlob } from '../plots/export'
import SetBar from './SetBar.vue'
import CohortCheckButton from './CohortCheckButton.vue'
import ImageTable from './ImageTable.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import CollapsiblePanel from './CollapsiblePanel.vue'
import HintCallout from './HintCallout.vue'

const props = withDefaults(defineProps<{
  module?:      string
  allowManage?: boolean
  showAttrs?:   boolean
  editableMeta?: boolean
  showFilter?:  boolean
  singleSelect?: boolean   // radio-style image selection (e.g. gating works on one image)
  plotsLabel?:  string
  noSetHint?:   string
  // NO PAGE USES THESE TODAY, and that is the intended state — see docs/UI.md → First-use hints. The
  // four that existed were pipeline ordering and prerequisites, which the guide prereqs answer LIVE;
  // a static sentence asserting "segment first" is wrong for the user who already did. The mechanism
  // stays so the next real case reuses it instead of hand-rolling a callout. The bar: an interaction
  // affordance with no other surface, that no live check could answer.
  hint?:        string   // first-use-only one-liner shown above the panel (dismissed per hintKey)
  hintKey?:     string   // stable id for the hint's localStorage dismissal (required if hint set)
  // The #right panel's width bounds. These used to live inside the panels themselves, back when each
  // owned its own width — TaskRunner 200/600/280, MetadataPanel 260/520/280. The host owns the width
  // now (one panel, one handle), so the numbers have to come from here or they are silently lost:
  // the default 280 is what both pinned themselves to, and the min/max are what each CHOSE.
  // A floor is content-driven — the metadata panel's labelled fields stop being usable below ~260.
  rightDefaultWidth?: number
  rightMinWidth?: number
  rightMaxWidth?: number
  cohortFuns?:  string[]  // explicit cohort funs for the "Check cohort" button (custom pages); overrides COHORT_STAGES
}>(), {
  allowManage: false,
  showAttrs:   false,
  editableMeta: false,
  showFilter:  true,
  singleSelect: false,
  plotsLabel:  'Plots',
  noSetHint:   'Select a set to get started.',
})

const emit = defineEmits<{
  selectionChange: [uids: string[]]
}>()

// #right panel width — persisted per module, so a page's runner keeps the width the user gave IT.
// The panel itself (handle + collapse + resize) is `CollapsiblePanel`; this only names the key.
// Width stays per module, collapse is the one shared flag — see that component.
const rightWidthKey = computed(() => props.module ? `cc.rightw.${props.module}` : 'cc.rightw.default')

const project    = useProjectStore()
const taskDefs   = useTaskDefsStore()
const activeSet  = computed(() => project.activeSet())
// namespace remembered selections per module so they don't bleed across pages (docs/UI.md)
const selScope   = computed(() => props.module ?? 'default')
const selectedUids = ref<string[]>(
  activeSet.value ? project.getImageSelection(selScope.value, activeSet.value.uid) : []
)

const selectedNames = computed(() =>
  selectedUids.value.map(uid =>
    activeSet.value?.images.find(i => i.uid === uid)?.name ?? uid
  )
)

// ── Attr filter ────────────────────────────────────────────────────────────────

// The chips, the draft/applied split and the matching rule are `utils/attrFilter.ts` +
// `AttrFilterPanel.vue` — shared with the Movies list, which asks the same question of rows joined
// back to their image. This holds the state and decides what it narrows.
const attrFilter = ref<AttrFilterState>(emptyAttrFilter())

// Attributes and processing-history are two separate dropdowns off the action bar (Filter / Task),
// each COLLAPSED by default with its own open-state persisted per module (UI.md).
const filterKey = computed(() => `cc-filters-open:${props.module ?? 'default'}`)
const filtersOpen = ref(localStorage.getItem(`cc-filters-open:${props.module ?? 'default'}`) === '1')
watch(filtersOpen, v => { try { localStorage.setItem(filterKey.value, v ? '1' : '0') } catch { /* ignore */ } })
const taskKey = computed(() => `cc-task-open:${props.module ?? 'default'}`)
const taskOpen = ref(localStorage.getItem(`cc-task-open:${props.module ?? 'default'}`) === '1')
watch(taskOpen, v => { try { localStorage.setItem(taskKey.value, v ? '1' : '0') } catch { /* ignore */ } })

// ── Row filters (Excluded / Imported / Starred) ─────────────────────────────────
// One persisted on/off toggle per filter, declared as data in utils/rowFilters.ts and rendered by a
// single v-for below — see that file for why they aren't three hand-written blocks. Excluded images
// are shown greyed by DEFAULT (not hidden); each toggle hides its rows only when switched on.
const rowFilterActive = ref<Record<string, boolean>>(Object.fromEntries(
  ROW_FILTERS.map(f => [f.id, localStorage.getItem(rowFilterKey(f.id, props.module)) === '1'])))
watch(rowFilterActive, v => {
  for (const f of ROW_FILTERS) {
    try { localStorage.setItem(rowFilterKey(f.id, props.module), v[f.id] ? '1' : '0') } catch { /* ignore */ }
  }
}, { deep: true })
// the filters worth showing, with their current count resolved once for the button + its tooltip
const rowFilters = computed(() => {
  const imgs = activeSet.value?.images ?? []
  return ROW_FILTERS.filter(f => f.visible(imgs))
    .map(f => ({ def: f, count: f.count(imgs), images: imgs }))
})
// separate from the filter toggle above: the set header always states how many images are excluded,
// whether or not the Excluded filter is being used to hide them
const excludedCount = computed(() => (activeSet.value?.images ?? []).filter(isExcluded).length)

// ── Processed-with filter ────────────────────────────────────────────────────────
// "Which images have been processed with function X?" — derived from each image's automatic run log
// (the single source of truth), so no separate status attribute to keep in sync. Pick a function
// (only funs that have actually been run across the set are offered) + a mode: 'ever' (any run) or
// 'last' (the most recent run). Persisted per module alongside the other filter toggles.
const procFunKey  = computed(() => `cc-proc-fun:${props.module ?? 'default'}`)
const procModeKey = computed(() => `cc-proc-mode:${props.module ?? 'default'}`)
const procFun  = ref(localStorage.getItem(`cc-proc-fun:${props.module ?? 'default'}`) ?? '')
const procMode = ref<ProcMode>(
  (localStorage.getItem(`cc-proc-mode:${props.module ?? 'default'}`) as ProcMode) || 'ever')
watch(procFun,  v => { try { localStorage.setItem(procFunKey.value, v) } catch { /* ignore */ } })
watch(procMode, v => { try { localStorage.setItem(procModeKey.value, v) } catch { /* ignore */ } })
onMounted(() => { taskDefs.ensureLoaded() })   // for pretty function labels in the dropdown

// funs that have actually been run across the set — the candidate list for the "processed with" filter
const runFuns = computed(() =>
  funsRunAcross((activeSet.value?.images ?? []).map(i => i.runLog)))
const procFunLabel = (fun: string) => `${funModuleLabel(fun)} · ${taskDefs.labelFor(fun)}`
// the picked fun is only active as a filter while it's still a real candidate (a set switch may drop it)
const procActive = computed(() => !!procFun.value && runFuns.value.includes(procFun.value))

// Still needed here for the Filter button (nothing to offer without them) and the CSV export's
// one-column-per-attribute layout; the chips themselves are the panel's.
const attrKeys = computed(() => attrKeysOf(activeSet.value?.images ?? []))

const hasApplied = computed(() => attrFilterActive(attrFilter.value))
const filteredUids = computed<string[] | undefined>(() => {
  const attrActive = props.showFilter && hasApplied.value
  const procOn     = props.showFilter && procActive.value
  // Nothing narrowing the list → let ImageTable show everything (excluded still render, greyed).
  if (!attrActive && !procOn && !anyRowFilterActive(rowFilterActive.value)) return undefined
  const imgs = activeSet.value?.images ?? []
  return imgs
    .filter(img => {
      if (hiddenByRowFilters(img, rowFilterActive.value)) return false
      if (procOn && !wasProcessedWith(img.runLog, procFun.value, procMode.value)) return false
      return !attrActive || matchesAttrFilter(img.attr, attrFilter.value)
    })
    .map(img => img.uid)
})

// Drop out-of-view selections when filter changes
watch(filteredUids, (uids) => {
  if (uids == null) return
  const keep = new Set(uids)
  const next = selectedUids.value.filter(uid => keep.has(uid))
  if (next.length !== selectedUids.value.length) {
    selectedUids.value = next
    emit('selectionChange', next)
  }
})

// Reset filters on set switch, but restore that set's remembered selection (ImageTable reseeds
// and emits too; reading the store here keeps the slot props correct without an intermediate empty)
watch(activeSet, (s) => {
  attrFilter.value   = emptyAttrFilter()
  selectedUids.value = s ? project.getImageSelection(selScope.value, s.uid) : []
  emit('selectionChange', selectedUids.value)
})

function onSelectionChange(uids: string[]) {
  selectedUids.value = uids
  emit('selectionChange', uids)
}

// Let below-table content drive the image selection (e.g. the cluster page's "select clustered
// images"). Writes the shared selection store; ImageTable watches it and re-seeds its checkboxes,
// which emits back through onSelectionChange to keep selectedUids in sync.
function selectUids(uids: string[]) {
  if (activeSet.value) project.setImageSelection(selScope.value, activeSet.value.uid, uids)
}

// Export the whole image table to CSV — EVERY image including excluded ones (flagged + their note),
// one column per attr. Row-building is a pure util (tested); rowsToCsv/downloadBlob are the shared
// export plumbing (plots/export.ts).
function exportCsv() {
  const imgs = activeSet.value?.images ?? []
  if (!imgs.length) return
  const rows = imageTableCsvRows(imgs, attrKeys.value)
  const name = `${activeSet.value?.name ?? 'images'}.csv`.replace(/[^\w.-]+/g, '_')
  downloadBlob(name, new Blob([rowsToCsv(rows)], { type: 'text/csv' }))
}

// Visible images in table order (filtered list when a filter/hide-excluded is active, else all).
// Exposed to #plots so a canvas can step selection through the list (gating prev/next navigation).
const visibleUids = computed<string[]>(() =>
  filteredUids.value ?? (activeSet.value?.images ?? []).map(i => i.uid))
</script>

<template>
  <div class="module-root">

    <SetBar :allow-manage="allowManage" />

    <div class="module-body">

      <!-- ── Left: image panel ─────────────────────────────────────── -->
      <!-- no #right panel (e.g. the Analysis page) → the panel runs flush to the viewport edge, jamming
           the right-aligned controls (filter toggle, board export, pop picker) + their tooltips against
           it. `no-right` adds a small right gutter so they have room. -->
      <div class="image-panel" :class="{ 'no-right': !$slots.right }">

        <!-- first-use hint (dismissed permanently per hintKey) -->
        <HintCallout v-if="hint && hintKey" :hint-key="hintKey" :text="hint" />

        <!-- action bar: image count + (merged) filter toggle on the right -->
        <div class="action-bar">
          <slot name="actions" :has-set="!!activeSet" :set-uid="activeSet?.uid"
            :selected-uids="selectedUids" :select-uids="selectUids" />

          <span class="image-count cc-muted" v-if="activeSet">
            <template v-if="showFilter && filteredUids">
              {{ filteredUids.length }} / {{ activeSet.images.length }}
            </template>
            <template v-else>
              {{ activeSet.images.length }}
            </template>
            image{{ activeSet.images.length !== 1 ? 's' : '' }}
            <!-- an excluded image is still a "done" set member — call it out so the count isn't silently
                 over (excluded images drop out of analysis / cohort denominators). -->
            <span v-if="excludedCount > 0" class="cc-muted-warn"
                  v-tooltip.top="`${excludedCount} image(s) excluded from analysis — not counted downstream`">
              &nbsp;({{ excludedCount }} excluded)
            </span>
            <template v-if="selectedUids.length">
              &nbsp;·&nbsp;{{ selectedUids.length }} selected
            </template>
          </span>
          <span class="no-set-hint cc-muted" v-else>{{ noSetHint }}</span>

          <div class="table-tools" v-if="activeSet && activeSet.images.length > 0">
            <!-- Cohort consistency: self-hides unless this module banks cohort metrics (cohortStages) -->
            <CohortCheckButton :module="module" :set-uid="activeSet.uid" :funs="cohortFuns" />

            <!-- CSV export: the whole table, including excluded images + their notes -->
            <button class="filter-toggle" @click="exportCsv"
              v-tooltip.left="'Export the image table to CSV (includes excluded images and their notes)'">
              <i class="pi pi-download" />
              <span class="filter-label">CSV</span>
            </button>

            <!-- Row filters (Excluded / Imported / Starred) — declared in utils/rowFilters.ts -->
            <button v-for="f in rowFilters" :key="f.def.id"
              class="filter-toggle" :class="{ active: rowFilterActive[f.def.id] }"
              @click="rowFilterActive[f.def.id] = !rowFilterActive[f.def.id]"
              v-tooltip.left="f.def.tip(rowFilterActive[f.def.id], f.images)">
              <i :class="['pi', rowFilterActive[f.def.id] ? f.def.iconOn : f.def.iconOff]" />
              <span class="filter-label">{{ f.def.label }} {{ f.count }}</span>
            </button>

            <!-- Task: filter to images a given function has been run on (own dropdown) -->
            <button v-if="showFilter && runFuns.length > 0"
              class="filter-toggle" :class="{ active: procActive || taskOpen }"
              @click="taskOpen = !taskOpen"
              v-tooltip.left="taskOpen ? 'Hide task filter' : 'Filter images by processing history'">
              <i class="pi pi-list-check" />
              <span class="filter-label">Task{{ procActive ? ' •' : '' }}</span>
              <i :class="['pi', taskOpen ? 'pi-chevron-up' : 'pi-chevron-down']" class="filter-caret" />
            </button>

            <!-- Filter: filter to images by attribute value (own dropdown) -->
            <button v-if="showFilter && attrKeys.length > 0"
              class="filter-toggle" :class="{ active: hasApplied || filtersOpen }"
              @click="filtersOpen = !filtersOpen"
              v-tooltip.left="filtersOpen ? 'Hide filters' : 'Filter images by attribute'">
              <i class="pi pi-filter" />
              <span class="filter-label">Filter{{ hasApplied ? ' •' : '' }}</span>
              <i :class="['pi', filtersOpen ? 'pi-chevron-up' : 'pi-chevron-down']" class="filter-caret" />
            </button>
          </div>
        </div>

        <!-- Task dropdown: filter to images a given function has been run on (ever / on last run) -->
        <div v-if="showFilter && activeSet && runFuns.length > 0 && taskOpen" class="cc-filter-panel">
          <div class="cc-filter-row proc-row">
            <span class="cc-filter-key cc-eyebrow cc-fs-sm" v-tooltip.right="'Filter to images processed with a function'">Processed with</span>
            <div class="proc-controls cc-row">
              <select v-model="procFun" class="proc-select"
                v-tooltip.bottom="'Only show images this function has been run on'">
                <option value="">any function…</option>
                <option v-for="fun in runFuns" :key="fun" :value="fun">{{ procFunLabel(fun) }}</option>
              </select>
              <div class="proc-mode cc-muted" :class="{ disabled: !procFun }">
                <label v-tooltip.bottom="'Match images the function has EVER been run on'">
                  <input type="radio" value="ever" v-model="procMode" :disabled="!procFun" /> ever
                </label>
                <label v-tooltip.bottom="'Match only images whose most recent run was this function'">
                  <input type="radio" value="last" v-model="procMode" :disabled="!procFun" /> last run
                </label>
              </div>
            </div>
          </div>
        </div>

        <!-- Filter dropdown: attribute-value chips — only when open. The panel renders nothing when
             the set has no attributes, so the guard here is just "is it open". -->
        <AttrFilterPanel v-if="showFilter && activeSet && filtersOpen" noun="images"
          :rows="activeSet.images" v-model="attrFilter" />

        <!-- scrollable body: image table + below-table content -->
        <div class="panel-scroll">
          <CollapsibleSection label="Images" max-height="none"
            :storage-key="`cc-images-open:${module ?? 'default'}`">
            <div v-if="!activeSet" class="no-set cc-empty">
              <i class="pi pi-folder-open" style="font-size:2rem; opacity:0.2" />
              <p>No set selected.</p>
            </div>
            <ImageTable
              v-else
              :setUid="activeSet.uid"
              :module="module"
              :selection-scope="selScope"
              :show-attrs="showAttrs"
              :editable-meta="editableMeta"
              :single-select="singleSelect"
              :filter-uids="filteredUids"
              @selectionChange="onSelectionChange"
            />
          </CollapsibleSection>

          <!-- Plot canvas — ONE consistent, collapse-persisted section for every module page.
               ModuleLayout owns the wrapper so no module can forget it or diverge. -->
          <CollapsibleSection v-if="$slots.plots && activeSet"
            data-guide="layout.plotsSection"
            :label="plotsLabel" max-height="none"
            :storage-key="`cc-plots-open:${module ?? 'default'}`">
            <slot name="plots"
              :set-uid="activeSet.uid"
              :selected-uids="selectedUids"
              :selected-names="selectedNames"
              :select-uids="selectUids"
              :ordered-uids="visibleUids"
            />
          </CollapsibleSection>

          <slot name="below-table"
            :set-uid="activeSet?.uid"
            :selected-uids="selectedUids"
            :selected-names="selectedNames"
            :select-uids="selectUids"
          />
        </div>
      </div>

      <!-- ── Right: module-specific panel (collapsible + resizable) ── -->
      <CollapsiblePanel v-if="$slots.right" :storage-key="rightWidthKey" label="functions panel"
                        :min="rightMinWidth ?? 200" :max="rightMaxWidth ?? 680"
                        :default-width="rightDefaultWidth ?? 280">
        <slot
          name="right"
          :set-uid="activeSet?.uid"
          :selected-uids="selectedUids"
          :selected-names="selectedNames"
        />
      </CollapsiblePanel>

    </div>
  </div>
</template>

<style scoped>
.module-root {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
}

.module-body {
  flex: 1;
  display: flex;
  overflow: hidden;
}

/* ── Image panel ──────────────────────────────────────────────────────────── */

.image-panel {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  min-width: 0;
}
/* Right gutter when there's no #right panel to provide one — inset the content of the toolbar + plot
   area so the right-aligned controls (and their tooltips/floating pickers) aren't flush to the edge.
   Padding on the inner boxes (not image-panel) keeps the action-bar divider spanning full width. */
.image-panel.no-right > .action-bar { padding-right: 1.1rem; }
.image-panel.no-right > .cc-filter-panel,
.image-panel.no-right > .panel-scroll { padding-right: 0.9rem; }

/* The right panel's own chrome (handle / resizer / slot) lives in CollapsiblePanel.vue */

.action-bar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.55rem 0.5rem 0.55rem 0.5rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-bg);
  flex-shrink: 0;
}

.image-count { display: flex; align-items: center; gap: 0.4rem; }

.filter-badge {
  font-size: var(--cc-fs-2xs);
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  padding: 0.1rem 0.45rem;
  border-radius: var(--cc-radius-pill);
  background: var(--cc-accent-tint);
  color: var(--cc-accent-soft);
}

.no-set-hint { font-style: italic; }

/* ── Attr filter ──────────────────────────────────────────────────────────── */


/* filter/excluded toggles: grouped, pushed to the right edge of the action bar */
.table-tools {
  margin-left: auto;
  display: flex;
  align-items: center;
  gap: 0.4rem;
  flex-shrink: 0;
}

.filter-toggle {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  padding: 0.2rem 0.5rem;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  background: var(--cc-surface-1);
  cursor: pointer;
  color: var(--cc-text-dim);
  flex-shrink: 0;
}
.filter-toggle:hover { color: var(--cc-text); border-color: #484f58; }
.filter-toggle.active { color: var(--cc-accent-soft); border-color: var(--cc-accent-strong); }
.filter-toggle .pi { font-size: var(--cc-fs-xs); }
.filter-toggle .filter-caret { font-size: var(--cc-fs-3xs); opacity: 0.7; }

.filter-label {
  font-size: var(--cc-fs-xs);
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: inherit;
}


/* processed-with filter row (function picker + ever/last mode) — its own dropdown, no divider */
.proc-row .cc-filter-key { min-width: 104px; }

.proc-select {
  padding: 0.15rem 0.4rem;
  border-radius: var(--cc-radius-sm);
  background: var(--cc-surface-1);
  max-width: 240px;
}
.proc-mode { display: flex; align-items: center; gap: 0.55rem; }
.proc-mode label { display: inline-flex; align-items: center; gap: 0.25rem; cursor: pointer; }
.proc-mode.disabled { opacity: 0.4; }
.proc-mode input { cursor: pointer; }


/* ── Scrollable panel body (image table + below-table) ────────────────────── */

.panel-scroll {
  flex: 1;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  min-height: 0;
}

.no-set p { margin: 0; }
</style>
