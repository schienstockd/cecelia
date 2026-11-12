<!--
  The per-channel detail of an acquisition with many channels, under the summarised light-path
  stages: a cycle × dye grid for a cyclic run (a cell = marker · exposure, amber when the exposure
  differs from the run plan, blank cycles as muted rows), else one row per channel. Selecting a
  channel (v-model) puts its own lines back into the stages above. Logic: utils/acquisitionChannelTable.ts.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import { cycleGrid, filterGrid, channelList, filterChannelList, type GridCell } from '../utils/acquisitionChannelTable'
import type { AcquisitionSettings } from '../utils/acquisitionSettings'

const props = defineProps<{ acquisition: AcquisitionSettings; channelNames?: string[] }>()
const selected = defineModel<number | null>({ default: null })

// Transient, like the stage hover: what a reader is looking for in this dialog right now.
const q = ref('')

const grid = computed(() => cycleGrid(props.acquisition, props.channelNames))
const shownGrid = computed(() => (grid.value ? filterGrid(grid.value, q.value) : null))
const offPlan = computed(() => grid.value?.rows.some(r => Object.values(r.cells).some(c => c?.offPlan)) ?? false)

// SelectionTable keys its cell slots by column key; band strings ("533-559 nm") make poor slot names.
const gridColumns = computed<SelectionColumn[]>(() => [
  { key: 'cycle', label: '', width: 70 },
  ...(grid.value?.columns ?? []).map((c, i) => ({ key: `c${i}`, label: c.label })),
])
type GridTableRow = { id: string; cycle: string; kind: string; cells: (GridCell | null)[] }
const gridRows = computed<GridTableRow[]>(() => (shownGrid.value?.rows ?? []).map(r => ({
  id: String(r.cycle), cycle: `Cycle ${r.cycle}`, kind: r.kind,
  cells: shownGrid.value!.columns.map(c => r.cells[c.key]),
})))
const colIndex = (key: string) => Number(key.slice(1))
/** The first empty cell of a row with no marker carries its note, as the mock places it. */
const noteAt = (row: GridTableRow) => row.cells.findIndex(c => c == null)

const listColumns: SelectionColumn[] = [
  { key: 'order', label: '#', width: 40, sortable: true },
  { key: 'name', label: 'Name', sortable: true },
  { key: 'ex', label: 'Ex' },
  { key: 'em', label: 'Em' },
  { key: 'detector', label: 'Detector / exposure' },
]
const listRows = computed(() => filterChannelList(channelList(props.acquisition, props.channelNames), q.value))
const listModel = computed({
  get: () => (selected.value == null ? undefined : String(selected.value)),
  set: (id?: string) => { selected.value = id == null ? null : Number(id) },
})

const pick = (order: number) => { selected.value = selected.value === order ? null : order }
const count = computed(() => props.acquisition.channels?.length ?? 0)
</script>

<template>
  <div class="act">
    <div class="act-head cc-row">
      <span class="act-title">Channels · {{ count }}</span>
      <span v-if="grid" class="cc-muted cc-fs-xs">by cycle · marker · exposure</span>
      <input v-model="q" type="search" class="act-filter cc-input-xs" :placeholder="grid ? 'Filter markers…' : 'Filter channels…'"
             v-tooltip.top="grid ? 'Filter by marker, dye or reagent' : 'Filter by channel name'" />
    </div>

    <SelectionTable v-if="grid" :columns="gridColumns" :rows="gridRows" id-key="id" selection-mode="none" density="compact"
                    :row-class="r => ({ 'act-quiet': r.kind !== 'markers' })"
                    :row-tooltip="r => (r.kind === 'markers' ? 'Select a cell to show its settings above' : r.cycle)">
      <template v-for="col in gridColumns.slice(1)" :key="col.key" #[`cell-${col.key}`]="{ row }">
        <button v-if="row.cells[colIndex(col.key)]" class="cc-btn cc-btn-ghost act-cell"
                :class="{ 'act-off': row.cells[colIndex(col.key)]!.offPlan, 'cc-btn-on': row.cells[colIndex(col.key)]!.order === selected,
                          'act-dim': !row.cells[colIndex(col.key)]!.match }"
                v-tooltip.top="row.cells[colIndex(col.key)]!.tip"
                @click.stop="pick(row.cells[colIndex(col.key)]!.order)">{{ row.cells[colIndex(col.key)]!.text }}</button>
        <span v-else-if="row.kind === 'blank' && noteAt(row) === colIndex(col.key)" class="act-note cc-muted"
              v-tooltip.top="'Background only, not in the image'">blank · not in the image</span>
        <span v-else-if="row.kind === 'empty' && noteAt(row) === colIndex(col.key)" class="act-note cc-muted">no marker in the image</span>
        <span v-else />
      </template>
    </SelectionTable>

    <SelectionTable v-else v-model="listModel" :columns="listColumns" :rows="listRows" id-key="id" density="compact"
                    :row-tooltip="() => 'Show its settings above'" />

    <p v-if="offPlan" class="cc-muted cc-fs-xs act-foot"><span class="act-swatch" />exposure differs from the run plan</p>
  </div>
</template>

<style scoped>
.act { display: flex; flex-direction: column; gap: 0.35rem; margin-top: 0.6rem; }
.act-head { align-items: baseline; }
.act-title { font-weight: 600; }
.act-filter { margin-left: auto; width: 12rem; }
.act-cell { width: 100%; justify-content: flex-start; padding: 0.1rem 0.4rem; }
.act-off { border-color: var(--cc-sev-warn); background: color-mix(in srgb, var(--cc-sev-warn) 14%, transparent); }
.act-dim { opacity: 0.35; }
.act-note { white-space: nowrap; }
:deep(.act-quiet) { color: var(--cc-text-dim); }
.act-foot { margin: 0; display: inline-flex; align-items: center; gap: 0.35rem; }
.act-swatch { width: 0.8rem; height: 0.6rem; border: 1px solid var(--cc-sev-warn); border-radius: var(--cc-radius-xs);
              background: color-mix(in srgb, var(--cc-sev-warn) 14%, transparent); }
</style>
