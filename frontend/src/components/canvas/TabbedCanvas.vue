<!--
  Multipage canvas: a tab bar over N independent boards, each an isolated SummaryCanvas
  (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A). The tab LIST lives in the `analysisTabs` store; each
  board's plots persist in `canvasPanels` under the canvas key `${groupKey}:tab:${id}`, so tabs reuse
  the whole existing canvas machinery. Only the ACTIVE board is mounted (keyed by its canvas key, so
  switching tabs re-binds that board's stored panels); inactive boards persist in the store.

  Boards are per-project (`groupKey = analysis:{projectUid}`); the parent MUST `:key` this component by
  projectUid so a project switch remounts it. Add / rename (double-click) / drag-reorder / close tabs.
-->
<script setup lang="ts">
import { ref, computed, nextTick, useTemplateRef } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useAnalysisTabsStore } from '../../stores/analysisTabs'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { exportTabsToPdf } from '../../plots/pdf'
import { downloadBlob } from '../../plots/export'
import { useLogStore } from '../../stores/log'
import LayoutCanvas from './LayoutCanvas.vue'
import ConfirmButton from '../ConfirmButton.vue'

const props = defineProps<{ imageUids: string[]; module?: string | null }>()

const meta = useProjectMetaStore()
const tabsStore = useAnalysisTabsStore()
const panelsStore = useCanvasPanelsStore()
const log = useLogStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const groupKey = `analysis:${projectUid.value}`
tabsStore.ensure(groupKey)   // seed the first board

// derive reactively FROM THE STORE (not a captured return value) so mutations re-render
const group = computed(() => tabsStore.entries[groupKey])
const tabs = computed(() => group.value?.tabs ?? [])
const activeId = computed(() => group.value?.activeId ?? 0)
const canvasKey = (id: number) => `${groupKey}:tab:${id}`
const activeKey = computed(() => canvasKey(activeId.value))

// inline rename
const editingId = ref<number | null>(null)
const editText = ref('')
function startRename(id: number, name: string) { editingId.value = id; editText.value = name }
function commitRename() {
  if (editingId.value != null) tabsStore.renameTab(groupKey, editingId.value, editText.value)
  editingId.value = null
}

function addTab() { tabsStore.addTab(groupKey) }

const plotCount = (id: number) => panelsStore.entries[canvasKey(id)]?.panels.length ?? 0
// no native confirm — the close button is a ConfirmButton that arms only when the board has plots
function closeTab(id: number) {
  panelsStore.drop(canvasKey(id))
  tabsStore.removeTab(groupKey, id)
}

// drag-reorder
const dragId = ref<number | null>(null)
function onDrop(targetId: number) {
  if (dragId.value == null || dragId.value === targetId) { dragId.value = null; return }
  const toIndex = tabs.value.findIndex(t => t.id === targetId)
  tabsStore.reorderTab(groupKey, dragId.value, toIndex)
  dragId.value = null
}

// ── Export every tab to a multipage PDF (one page per tab = its grid) ──────────────────────────────
// Only the ACTIVE board is mounted, so we visit each tab in turn, let it render, capture its slots, then
// restore the original tab. (Timing is a fixed settle delay — plots have no unified "loaded" signal.)
type CapturedPage = { aspect: number; slots: { rect: { x: number; y: number; w: number; h: number }; png: string | null; name?: string; csv?: string | null }[] }
const layoutRef = useTemplateRef<{ capturePage: () => Promise<CapturedPage>
  collectCsvs: () => { name: string; csv: string | null }[] }>('layoutRef')
const exporting = ref(false)
const safe = (s: string) => s.replace(/[^\w.-]+/g, '_')
async function exportPdf() {
  if (exporting.value || !group.value || !tabs.value.length) return
  exporting.value = true
  const original = group.value.activeId
  try {
    const pages = []
    for (const t of tabs.value) {
      tabsStore.setActive(groupKey, t.id)
      await nextTick()
      await new Promise(r => setTimeout(r, 1400))   // let the board's plots fetch + render before capture
                                                    // (a mid-render WebGL frame exports sparse/blurry)
      const page = await layoutRef.value?.capturePage?.()
      if (page) pages.push({ title: t.name, aspect: page.aspect, slots: page.slots })
    }
    tabsStore.setActive(groupKey, original)
    await nextTick()
    if (pages.length) await exportTabsToPdf(pages, 'analysis.pdf')
    log.info('Exported analysis boards to PDF.', { source: 'analysis' })
  } catch (e) {
    log.error(`PDF export failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'analysis' })
  } finally { exporting.value = false }
}

// Export the shown data of every summary plot as one CSV per plot (ready to re-plot in Prism). Same
// visit-each-tab dance as the PDF (only the active board is mounted); the browser may prompt once to
// allow multiple downloads.
async function exportCsv() {
  if (exporting.value || !group.value || !tabs.value.length) return
  exporting.value = true
  const original = group.value.activeId
  try {
    let n = 0
    for (const t of tabs.value) {
      tabsStore.setActive(groupKey, t.id)
      await nextTick()
      await new Promise(r => setTimeout(r, 600))   // let the board's plots fetch before reading their data
      for (const { name, csv } of layoutRef.value?.collectCsvs?.() ?? []) {
        if (!csv) continue
        downloadBlob(`${safe(t.name)}_${safe(name)}.csv`, new Blob([csv], { type: 'text/csv' }))
        n++
      }
    }
    tabsStore.setActive(groupKey, original)
    await nextTick()
    log.info(n ? `Exported ${n} plot CSV${n === 1 ? '' : 's'}.` : 'No summary-plot data to export.', { source: 'analysis' })
  } catch (e) {
    log.error(`CSV export failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'analysis' })
  } finally { exporting.value = false }
}
</script>

<template>
  <div class="tabbed-canvas">
    <div class="tab-bar" role="tablist">
      <div
        v-for="t in tabs" :key="t.id"
        class="tab" :class="{ active: t.id === activeId }"
        role="tab" :aria-selected="t.id === activeId"
        draggable="true"
        @click="tabsStore.setActive(groupKey, t.id)"
        @dblclick="startRename(t.id, t.name)"
        @dragstart="dragId = t.id"
        @dragover.prevent
        @drop.prevent="onDrop(t.id)"
        v-tooltip.bottom="'Double-click to rename · drag to reorder'"
      >
        <input
          v-if="editingId === t.id" class="tab-edit" :value="editText"
          @input="editText = ($event.target as HTMLInputElement).value"
          @blur="commitRename" @keydown.enter="commitRename" @keydown.esc="editingId = null"
          @click.stop :ref="el => (el as HTMLInputElement | null)?.focus()"
        />
        <template v-else>
          <span class="tab-name">{{ t.name }}</span>
          <span v-if="tabs.length > 1" @click.stop>
            <ConfirmButton trigger-class="tab-close" confirm-class="tab-close" cancel-class="tab-close"
                           :needs-confirm="plotCount(t.id) > 0" @confirm="closeTab(t.id)"
                           trigger-tooltip="Close board"
                           :confirm-tooltip="`Confirm — close board and its ${plotCount(t.id)} plot${plotCount(t.id) === 1 ? '' : 's'}`">
              <i class="pi pi-times" />
              <template #confirm><i class="pi pi-check" /></template>
              <template #cancel><i class="pi pi-replay" /></template>
            </ConfirmButton>
          </span>
        </template>
      </div>
      <button class="tab-add" type="button" @click="addTab" v-tooltip.bottom="'New board'" aria-label="New board">
        <i class="pi pi-plus" />
      </button>
      <button class="tab-pdf" type="button" @click="exportPdf" :disabled="exporting"
              v-tooltip.bottom="'Export all boards to a multipage PDF (one page per board)'">
        <i class="pi pi-file-pdf" /> {{ exporting ? 'exporting…' : 'PDF' }}
      </button>
      <button class="tab-csv" type="button" @click="exportCsv" :disabled="exporting"
              v-tooltip.bottom="'Export the shown data of every summary plot as CSV (one file per plot)'">
        <i class="pi pi-file-excel" /> CSV
      </button>
    </div>

    <!-- only the active board is mounted; keyed by its canvas key so a tab switch re-binds the layout -->
    <LayoutCanvas ref="layoutRef" :key="activeKey" :canvas-key="activeKey" :module="module" :image-uids="imageUids" />
  </div>
</template>

<style scoped>
.tabbed-canvas { display: flex; flex-direction: column; height: 100%; }
.tab-bar {
  display: flex; align-items: stretch; gap: 2px; flex-wrap: wrap;
  border-bottom: 1px solid var(--cc-border); padding: 4px 2px 0; flex-shrink: 0;
}
.tab {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 5px 10px; font-size: 12px; cursor: pointer; user-select: none;
  color: var(--cc-text-dim); background: var(--cc-surface-1);
  border: 1px solid var(--cc-border); border-bottom: none;
  border-radius: 6px 6px 0 0; max-width: 16rem;
}
.tab:hover { color: var(--cc-text); }
.tab.active { color: var(--cc-text); background: var(--cc-bg); border-color: #7c3aed; }
.tab-name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.tab-edit { font-size: 12px; width: 8rem; background: var(--cc-surface-2); color: var(--cc-text);
  border: 1px solid #7c3aed; border-radius: 3px; padding: 1px 4px; }
.tab-close {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1rem; height: 1rem; border: none; border-radius: 3px;
  background: transparent; color: var(--cc-text-dim); cursor: pointer; font-size: 0.6rem;
}
.tab-close:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.tab-add {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.8rem; border: none; background: transparent; color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.75rem; margin-left: 2px;
}
.tab-add:hover { color: var(--cc-text); }
.tab-pdf { display: inline-flex; align-items: center; gap: 5px; margin-left: auto; margin-bottom: 2px;
  padding: 3px 10px; font-size: 11px; border: 1px solid var(--cc-border); border-radius: 5px;
  background: var(--cc-surface-1); color: var(--cc-text-dim); cursor: pointer; }
.tab-pdf:hover:not(:disabled) { color: var(--cc-text); border-color: #7c3aed; }
.tab-pdf:disabled { opacity: 0.5; cursor: default; }
/* CSV sits right next to PDF (PDF already carries the margin-left:auto that pushes the pair right) */
.tab-csv { display: inline-flex; align-items: center; gap: 5px; margin-left: 4px; margin-bottom: 2px;
  padding: 3px 10px; font-size: 11px; border: 1px solid var(--cc-border); border-radius: 5px;
  background: var(--cc-surface-1); color: var(--cc-text-dim); cursor: pointer; }
.tab-csv:hover:not(:disabled) { color: var(--cc-text); border-color: #7c3aed; }
.tab-csv:disabled { opacity: 0.5; cursor: default; }
</style>
