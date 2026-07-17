<!--
  Multipage canvas: a tab bar over N independent boards, each a comic-plate LayoutCanvas
  (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A). The tab LIST lives in the `analysisTabs` store; each
  board's grid + slot contents persist in `analysisLayout` under the canvas key `${groupKey}:tab:${id}`.
  Only the ACTIVE board is mounted (keyed by its canvas key, so switching tabs re-binds that board's
  stored layout); inactive boards persist in the store.

  Boards are per-project (`groupKey = analysis:{projectUid}`); the parent MUST `:key` this component by
  projectUid so a project switch remounts it. Add / rename (double-click) / drag-reorder / close tabs.
-->
<script setup lang="ts">
import { ref, computed, nextTick, useTemplateRef } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useAnalysisTabsStore } from '../../stores/analysisTabs'
import { useAnalysisLayoutStore } from '../../stores/analysisLayout'
import { exportTabsToPdf } from '../../plots/pdf'
import { buildBoardSvgs } from '../../plots/boardSvg'
import { downloadBlob, downloadText, svgSizeWarning } from '../../plots/export'
import { zipTextFiles } from '../../utils/zip'
import { waitForPlotsIdle } from '../../utils/plotReady'
import { walkAssetRefs, collectAssetIds } from '../../utils/boardAssets'
import { useLogStore } from '../../stores/log'
import LayoutCanvas from './LayoutCanvas.vue'
import ConfirmButton from '../ConfirmButton.vue'

const props = defineProps<{ imageUids: string[]; module?: string | null }>()

const meta = useProjectMetaStore()
const tabsStore = useAnalysisTabsStore()
const layoutStore = useAnalysisLayoutStore()
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

// Duplicate a board: a new tab with a deep clone of the source board's layout (all plots + their
// state). Sidecar assets (filmstrip/image PNGs) are re-copied to fresh ids so the two boards are
// independent — deleting a frame in one must not orphan the other. Best-effort: a failed asset copy
// leaves that frame sharing the original (still renders). addTab already activates the new board.
async function duplicateBoard(id: number) {
  const src = tabs.value.find(t => t.id === id)
  const newId = tabsStore.addTab(groupKey, src ? `${src.name} copy` : undefined)
  layoutStore.duplicateEntry(canvasKey(id), canvasKey(newId))
  const e = layoutStore.entries[canvasKey(newId)]
  if (e && projectUid.value) await Promise.all(e.contents.map(c => c ? remapAssets(c.state) : Promise.resolve()))
}
// Copy every referenced asset PNG to a new id and rewrite the id in place (so the clone is independent).
async function remapAssets(state: unknown): Promise<void> {
  const jobs: Promise<void>[] = []
  walkAssetRefs(state, (rec, key, id) => {
    jobs.push((async () => {
      try {
        const res = await fetch('/api/board-assets/copy', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: projectUid.value, assetId: id }),
        })
        if (res.ok) { const d = await res.json() as { assetId?: string }; if (d.assetId) rec[key] = d.assetId }
      } catch { /* keep the shared id on failure — the frame still renders */ }
    })())
  })
  await Promise.all(jobs)
}

// # plots on a board = its non-empty slots (board content lives in `analysisLayout`, keyed per tab).
const plotCount = (id: number) => layoutStore.entries[canvasKey(id)]?.contents.filter(Boolean).length ?? 0
// no native confirm — the close button is a ConfirmButton that arms only when the board has plots
async function closeTab(id: number) {
  // delete the board's own sidecar assets (duplicated boards own independent ones) so they don't orphan
  const e = layoutStore.entries[canvasKey(id)]
  if (e && projectUid.value) {
    const ids = e.contents.flatMap(c => c ? collectAssetIds(c.state) : [])
    await Promise.all(ids.map(aid => fetch('/api/board-assets/delete', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, assetId: aid }),
    }).catch(() => {})))
  }
  layoutStore.drop(canvasKey(id))
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
type CapturedPage = { aspect: number; slots: { rect: { x: number; y: number; w: number; h: number }; png: string | null; svg?: string | null; name?: string; csv?: string | null }[] }
const layoutRef = useTemplateRef<{ capturePage: (vector?: boolean) => Promise<CapturedPage>
  collectCsvs: () => Promise<{ name: string; csv: string | null }[]> }>('layoutRef')
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
      await waitForPlotsIdle()   // wait for THIS board's plots to finish fetching + rendering — a fixed
                                 // sleep captured slow plots blank; idle-tracking waits exactly as long
                                 // as needed (a mid-render WebGL frame exports sparse/blurry)
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

// Export every board tab as a VECTOR SVG (docs/ANALYSIS.md) — dots/gates/summary charts are editable in
// Illustrator; image + HMM slots embed as raster. Same visit-each-tab + idle-wait dance as the PDF, but
// captures each slot's vector SVG (`capturePage(true)`) and stitches one SVG per board with the SAME A4
// layout as the PDF. One `.svg` for a single board; a `.zip` of per-board files when there are several.
async function exportSvgBoard() {
  if (exporting.value || !group.value || !tabs.value.length) return
  exporting.value = true
  const original = group.value.activeId
  try {
    const pages = []
    for (const t of tabs.value) {
      tabsStore.setActive(groupKey, t.id)
      await nextTick()
      await waitForPlotsIdle()
      const page = await layoutRef.value?.capturePage?.(true)
      if (page) pages.push({ title: t.name, aspect: page.aspect, slots: page.slots })
    }
    tabsStore.setActive(groupKey, original)
    await nextTick()
    const boards = buildBoardSvgs(pages)
    // non-blocking heads-up if a board came out heavy (a big UMAP/point cloud kept as vector)
    for (const b of boards) { const warn = svgSizeWarning(b.svg, `Board “${b.title}”`); if (warn) log.warn(warn, { source: 'analysis' }) }
    if (boards.length === 1) downloadText(`${safe(boards[0].title) || 'analysis'}.svg`, boards[0].svg, 'image/svg+xml')
    else if (boards.length > 1) {
      downloadBlob('analysis_svgs.zip', zipTextFiles(boards.map((b, i) => ({ name: `${String(i + 1).padStart(2, '0')}_${safe(b.title)}.svg`, text: b.svg }))))
    }
    log.info(`Exported ${boards.length} analysis board${boards.length === 1 ? '' : 's'} to SVG.`, { source: 'analysis' })
  } catch (e) {
    log.error(`SVG export failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'analysis' })
  } finally { exporting.value = false }
}
function exportBoard(kind: string) { if (kind === 'pdf') exportPdf(); else if (kind === 'svg') exportSvgBoard() }

// Export the shown data of every summary plot — collected across all boards into ONE .zip (one CSV
// per plot, ready to re-plot in Prism). Same visit-each-tab dance as the PDF (only the active board is
// mounted); a single zip download replaces the old dozens-of-CSVs "allow multiple downloads" prompt.
async function exportCsv() {
  if (exporting.value || !group.value || !tabs.value.length) return
  exporting.value = true
  const original = group.value.activeId
  try {
    const files: { name: string; text: string }[] = []
    for (const t of tabs.value) {
      tabsStore.setActive(groupKey, t.id)
      await nextTick()
      await waitForPlotsIdle()   // wait for the board's plots to finish fetching before reading their data
      for (const { name, csv } of (await layoutRef.value?.collectCsvs?.()) ?? []) {
        if (!csv) continue
        files.push({ name: `${safe(t.name)}_${safe(name)}.csv`, text: csv })
      }
    }
    tabsStore.setActive(groupKey, original)
    await nextTick()
    if (files.length) {
      downloadBlob('analysis_csvs.zip', zipTextFiles(files))
      log.info(`Exported ${files.length} plot CSV${files.length === 1 ? '' : 's'} → analysis_csvs.zip.`, { source: 'analysis' })
    } else {
      log.info('No summary-plot data to export.', { source: 'analysis' })
    }
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
          <button class="tab-close tab-dup" type="button" @click.stop="duplicateBoard(t.id)"
                  v-tooltip.bottom="'Duplicate board (plots + layout)'" aria-label="Duplicate board"><i class="pi pi-copy" /></button>
          <ConfirmButton v-if="tabs.length > 1" :needs-confirm="plotCount(t.id) > 0" @confirm="closeTab(t.id)"
                         v-slot="{ armed, arm, confirm, cancel }">
            <span @click.stop>
              <button v-if="!armed" class="tab-close" type="button" @click="arm"
                      v-tooltip.bottom="'Close board'" aria-label="Close board"><i class="pi pi-times" /></button>
              <template v-else>
                <button class="tab-close" type="button" @click="confirm"
                        v-tooltip.bottom="`Confirm — close board and its ${plotCount(t.id)} plot${plotCount(t.id) === 1 ? '' : 's'}`"><i class="pi pi-check" /></button>
                <button class="tab-close" type="button" @click="cancel" v-tooltip.bottom="'Keep board'"><i class="pi pi-replay" /></button>
              </template>
            </span>
          </ConfirmButton>
        </template>
      </div>
      <button class="tab-add" type="button" @click="addTab" v-tooltip.bottom="'New board'" aria-label="New board">
        <i class="pi pi-plus" />
      </button>
      <!-- board image export: PDF (raster) or SVG (vector, editable in Illustrator) -->
      <select class="tab-pdf" :disabled="exporting" v-tooltip.bottom="'Export all boards as one file per board'"
              @change="exportBoard(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">{{ exporting ? '⋯ exporting…' : '⤓ Figure' }}</option>
        <option value="pdf">PDF (raster)</option>
        <option value="svg">SVG (vector)</option>
      </select>
      <i class="tab-info pi pi-info-circle" v-tooltip.bottom="'PDF = raster image, best for viewing/printing (data CSVs attached). ' +
         'SVG = vector, editable in Illustrator (recolour dots, edit text/axes). ' +
         'In SVG, image and HMM-transition panels stay raster (no clean vector form). ' +
         'A very large point cloud (e.g. a 100k+ UMAP) makes a heavy SVG that can be slow in Illustrator — ' +
         'you’ll get a warning in the log if a figure is large.'" />
      <button class="tab-csv" type="button" @click="exportCsv" :disabled="exporting"
              v-tooltip.bottom="'Export the raw datapoints of every summary plot as CSV (one file per plot, → Prism)'">
        <i :class="exporting ? 'pi pi-spin pi-spinner' : 'pi pi-file-excel'" /> {{ exporting ? 'exporting…' : 'CSV' }}
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
/* info hint for the figure-export format choice */
.tab-info { align-self: center; margin-left: 4px; margin-bottom: 2px; font-size: 12px; color: var(--cc-text-dim); cursor: help; }
.tab-info:hover { color: var(--cc-text); }
/* CSV sits right next to PDF (PDF already carries the margin-left:auto that pushes the pair right) */
.tab-csv { display: inline-flex; align-items: center; gap: 5px; margin-left: 4px; margin-bottom: 2px;
  padding: 3px 10px; font-size: 11px; border: 1px solid var(--cc-border); border-radius: 5px;
  background: var(--cc-surface-1); color: var(--cc-text-dim); cursor: pointer; }
.tab-csv:hover:not(:disabled) { color: var(--cc-text); border-color: #7c3aed; }
.tab-csv:disabled { opacity: 0.5; cursor: default; }
</style>
