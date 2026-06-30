<!--
  Shared two-column module layout.
  Left column: SetBar + count bar + optional attr filter + collapsible ImageTable
               + optional #below-table slot (module supplies CollapsibleSection wrappers).
  Right column: slot #right — TaskRunner, MetadataPanel, or a custom panel.

  Props
  ─────
    module        string?   Passed to ImageTable for per-module column config.
    allowManage   bool      SetBar: show New/Rename/Delete set controls (default: false).
    allowDelete   bool      ImageTable: show per-image delete button (default: false).
    showAttrs     bool      ImageTable: show attr columns (default: false).
    showFilter    bool      Show the attr-value filter panel (default: true).
    noSetHint     string    Custom empty-state message.

  Slots
  ─────
    #actions  { hasSet }                        — extra items in the action bar.
    #right    { setUid, selectedUids,
                selectedNames }                 — the right-hand panel.
    #below-table { setUid, selectedUids,
                   selectedNames }              — content below the image table.
                                                  Wrap each piece in <CollapsibleSection>.

  Emits
  ─────
    selectionChange(uids: string[])             — whenever the image selection changes.

  See docs/UI.md for the full module page authoring guide.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'
import SetBar from './SetBar.vue'
import ImageTable from './ImageTable.vue'
import CollapsibleSection from './CollapsibleSection.vue'

const props = withDefaults(defineProps<{
  module?:      string
  allowManage?: boolean
  allowDelete?: boolean
  showAttrs?:   boolean
  showFilter?:  boolean
  singleSelect?: boolean   // radio-style image selection (e.g. gating works on one image)
  noSetHint?:   string
}>(), {
  allowManage: false,
  allowDelete: false,
  showAttrs:   false,
  showFilter:  true,
  singleSelect: false,
  noSetHint:   'Select a set to get started.',
})

const emit = defineEmits<{
  selectionChange: [uids: string[]]
}>()

const project    = useProjectStore()
const settings   = useSettingsStore()
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

const attrFilters    = ref<Record<string, string[]>>({})
const appliedFilters = ref<Record<string, string[]>>({})
const filterInvert   = ref(false)

// the filter is a dropdown off the action bar, COLLAPSED by default; state persists per module (UI.md)
const filterKey = computed(() => `cc-filters-open:${props.module ?? 'default'}`)
const filtersOpen = ref(localStorage.getItem(`cc-filters-open:${props.module ?? 'default'}`) === '1')
watch(filtersOpen, v => { try { localStorage.setItem(filterKey.value, v ? '1' : '0') } catch { /* ignore */ } })

const attrKeys = computed(() => {
  const imgs = activeSet.value?.images ?? []
  const keys = new Set<string>()
  for (const img of imgs)
    for (const k of Object.keys(img.attr ?? {})) keys.add(k)
  return [...keys].sort()
})

const attrValueMap = computed(() => {
  const imgs = activeSet.value?.images ?? []
  const map: Record<string, Set<string>> = {}
  for (const img of imgs)
    for (const [k, v] of Object.entries(img.attr ?? {})) {
      if (!map[k]) map[k] = new Set()
      if (v != null) map[k].add(String(v))
    }
  return Object.fromEntries(
    Object.entries(map).map(([k, s]) => [k, [...s].sort()])
  ) as Record<string, string[]>
})

const hasFilters = computed(() => Object.values(attrFilters.value).some(v => v.length > 0))
const hasApplied = computed(() => Object.values(appliedFilters.value).some(v => v.length > 0))

function toggleAttrFilter(key: string, val: string) {
  const cur  = attrFilters.value[key] ?? []
  const next = cur.includes(val) ? cur.filter(v => v !== val) : [...cur, val]
  attrFilters.value = { ...attrFilters.value, [key]: next }
}
function applyFilters() {
  appliedFilters.value = Object.fromEntries(
    Object.entries(attrFilters.value).filter(([, v]) => v.length > 0)
  )
}
function resetFilters() {
  attrFilters.value    = {}
  appliedFilters.value = {}
  filterInvert.value   = false
}

const filteredUids = computed<string[] | undefined>(() => {
  if (!props.showFilter || !hasApplied.value) return undefined
  const imgs = activeSet.value?.images ?? []
  return imgs
    .filter(img => {
      const matches = Object.entries(appliedFilters.value).every(([key, vals]) =>
        vals.includes(String(img.attr?.[key] ?? ''))
      )
      return filterInvert.value ? !matches : matches
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
  attrFilters.value    = {}
  appliedFilters.value = {}
  filterInvert.value   = false
  selectedUids.value   = s ? project.getImageSelection(selScope.value, s.uid) : []
  emit('selectionChange', selectedUids.value)
})

function onSelectionChange(uids: string[]) {
  selectedUids.value = uids
  emit('selectionChange', uids)
}
</script>

<template>
  <div class="module-root">

    <SetBar :allow-manage="allowManage" />

    <div class="module-body">

      <!-- ── Left: image panel ─────────────────────────────────────── -->
      <div class="image-panel">

        <!-- action bar: image count + (merged) filter toggle on the right -->
        <div class="action-bar">
          <slot name="actions" :has-set="!!activeSet" />

          <span class="image-count" v-if="activeSet">
            <template v-if="showFilter && filteredUids">
              {{ filteredUids.length }} / {{ activeSet.images.length }}
            </template>
            <template v-else>
              {{ activeSet.images.length }}
            </template>
            image{{ activeSet.images.length !== 1 ? 's' : '' }}
            <template v-if="selectedUids.length">
              &nbsp;·&nbsp;{{ selectedUids.length }} selected
            </template>
          </span>
          <span class="no-set-hint" v-else>{{ noSetHint }}</span>

          <button v-if="showFilter && activeSet && attrKeys.length > 0"
            class="filter-toggle" :class="{ active: hasApplied || filtersOpen }"
            @click="filtersOpen = !filtersOpen"
            v-tooltip.left="filtersOpen ? 'Hide filters' : 'Filter images by attribute'">
            <i class="pi pi-filter" />
            <span class="filter-label">Filter{{ hasApplied ? ' •' : '' }}</span>
            <i :class="['pi', filtersOpen ? 'pi-chevron-up' : 'pi-chevron-down']" class="filter-caret" />
          </button>
        </div>

        <!-- attr filter dropdown — only when open -->
        <div v-if="showFilter && activeSet && attrKeys.length > 0 && filtersOpen" class="attr-filter">
          <div class="filter-rows">
            <div v-for="key in attrKeys" :key="key" class="filter-row">
              <span class="filter-key" v-tooltip.right="`Filter by ${key}`">{{ key }}</span>
              <div class="filter-chips">
                <span
                  v-for="val in attrValueMap[key]" :key="val"
                  class="filter-chip"
                  :class="{ active: attrFilters[key]?.includes(val) }"
                  @click="toggleAttrFilter(key, val)"
                  v-tooltip.bottom="val"
                >{{ val }}</span>
              </div>
            </div>
          </div>
          <div class="filter-actions">
            <button class="cc-btn cc-btn-ghost" :disabled="!hasFilters" @click="applyFilters"
              v-tooltip.top="'Apply selected filters to the image list.'">Apply</button>
            <button class="cc-btn cc-btn-ghost" :disabled="!hasApplied && !hasFilters" @click="resetFilters"
              v-tooltip.top="'Clear all filters.'">Reset</button>
            <label class="filter-invert" v-tooltip.top="'Invert the filter — show images that do NOT match.'">
              <input type="checkbox" v-model="filterInvert" :disabled="!hasApplied" />
              Invert
            </label>
          </div>
        </div>

        <!-- scrollable body: image table + below-table content -->
        <div class="panel-scroll">
          <CollapsibleSection label="Images" max-height="none">
            <div v-if="!activeSet" class="no-set">
              <i class="pi pi-folder-open" style="font-size:2rem; opacity:0.2" />
              <p>No set selected.</p>
            </div>
            <ImageTable
              v-else
              :setUid="activeSet.uid"
              :module="module"
              :selection-scope="selScope"
              :allow-delete="allowDelete"
              :show-attrs="showAttrs"
              :single-select="singleSelect"
              :filter-uids="filteredUids"
              @selectionChange="onSelectionChange"
            />
          </CollapsibleSection>

          <slot name="below-table"
            :set-uid="activeSet?.uid"
            :selected-uids="selectedUids"
            :selected-names="selectedNames"
          />
        </div>
      </div>

      <!-- ── Right: module-specific panel (collapsible to free up space) ── -->
      <div v-if="$slots.right" class="right-panel" :class="{ collapsed: settings.rightPanelCollapsed }">
        <button class="right-handle"
          @click="settings.rightPanelCollapsed = !settings.rightPanelCollapsed"
          v-tooltip.left="settings.rightPanelCollapsed ? 'Show functions panel' : 'Hide functions panel'"
          :aria-label="settings.rightPanelCollapsed ? 'Show functions panel' : 'Hide functions panel'">
          <i :class="['pi', settings.rightPanelCollapsed ? 'pi-angle-double-left' : 'pi-angle-double-right']" />
        </button>
        <div v-show="!settings.rightPanelCollapsed" class="right-slot">
          <slot
            name="right"
            :set-uid="activeSet?.uid"
            :selected-uids="selectedUids"
            :selected-names="selectedNames"
          />
        </div>
      </div>

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

/* ── Right panel (collapsible) ──────────────────────────────────────────────
   A thin always-visible handle on the left edge toggles the slot; when collapsed
   only the handle remains, so the function/tasks panel folds away to the right. */
.right-panel {
  display: flex;
  flex-shrink: 0;
  overflow: hidden;
}
.right-handle {
  flex-shrink: 0;
  width: 1.1rem;
  border: none;
  border-left: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  color: var(--cc-text-dim);
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: background 0.12s, color 0.12s;
}
.right-handle:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.right-handle .pi { font-size: 0.7rem; }
.right-slot { display: flex; min-height: 0; overflow: hidden; }

.action-bar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.55rem 0.5rem 0.55rem 0.5rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-bg);
  flex-shrink: 0;
}

.image-count {
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  display: flex;
  align-items: center;
  gap: 0.4rem;
}

.filter-badge {
  font-size: 0.65rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  padding: 0.1rem 0.45rem;
  border-radius: 999px;
  background: #2d1b69;
  color: #c4b5fd;
}

.no-set-hint {
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  font-style: italic;
}

/* ── Attr filter ──────────────────────────────────────────────────────────── */

.attr-filter {
  flex-shrink: 0;
  border-bottom: 1px solid var(--cc-border);
  padding: 0.5rem 1rem;
  background: var(--cc-bg);
}

/* filter toggle: a small button pushed to the right edge of the action bar */
.filter-toggle {
  margin-left: auto;
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  padding: 0.2rem 0.5rem;
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
  background: var(--cc-surface-1);
  cursor: pointer;
  color: var(--cc-text-dim);
  flex-shrink: 0;
}
.filter-toggle:hover { color: var(--cc-text); border-color: #484f58; }
.filter-toggle.active { color: #c4b5fd; border-color: #7c3aed; }
.filter-toggle .pi { font-size: 0.7rem; }
.filter-toggle .filter-caret { font-size: 0.55rem; opacity: 0.7; }

.filter-label {
  font-size: 0.7rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: inherit;
}

.filter-actions {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  margin-top: 0.5rem;
  padding-top: 0.4rem;
  border-top: 1px solid var(--cc-border);
}

.filter-invert {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  cursor: pointer;
  user-select: none;
  margin-left: 0.25rem;
}
.filter-invert input { cursor: pointer; }
.filter-invert:has(input:disabled) { opacity: 0.4; cursor: not-allowed; }

.filter-rows   { display: flex; flex-direction: column; gap: 0.3rem; }

.filter-row {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  min-height: 1.6rem;
}

.filter-key {
  font-size: 0.72rem;
  font-weight: 600;
  color: var(--cc-text-dim);
  min-width: 80px;
  flex-shrink: 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.filter-chips  { display: flex; flex-wrap: wrap; gap: 0.25rem; }

.filter-chip {
  font-size: 0.72rem;
  padding: 0.15rem 0.55rem;
  border-radius: 999px;
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  color: var(--cc-text-dim);
  cursor: pointer;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
  user-select: none;
  white-space: nowrap;
}
.filter-chip:hover  { border-color: #7c3aed; color: var(--cc-text); }
.filter-chip.active { background: #2d1b69; border-color: #7c3aed; color: #c4b5fd; }

/* ── Scrollable panel body (image table + below-table) ────────────────────── */

.panel-scroll {
  flex: 1;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  min-height: 0;
}

.no-set {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 2rem 1rem;
  gap: 0.4rem;
  color: var(--cc-text-dim);
  font-size: 0.85rem;
}
.no-set p { margin: 0; }
</style>
