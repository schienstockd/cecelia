<script setup lang="ts">
// THE canonical single-select comparison table (docs/UI.md → UX-primitive catalog).
//
// Use this wherever the user picks ONE option and the reason to prefer one is a set of comparable
// numbers — a codec, a preset, a model, a profile. A dropdown is the wrong control there: it hides
// the very figures the decision rests on behind a label, so the user either takes the default on
// faith or goes looking in the docs. A table puts them on screen at the point of deciding.
//
// Deliberately dumb: it renders whatever display strings it is handed and never formats a number
// itself. The values are measured somewhere real (a backend constant, a benchmark) and should be
// stated in exactly one place — a component that reformatted them would become a second one.
//
// An optional `#actions` scoped slot adds a trailing cell per row for row-scoped buttons (rename,
// delete). Added for the optical-flow model vault, which is exactly this component's stated case —
// "a model" — and had been hand-rolled as a <ul> before that was noticed. Clicks inside it do NOT
// select the row.
//
// Rows are selected by CLICKING ANYWHERE on the row; the radio is a visual + a11y affordance, not
// the hit target (a 12px radio is a poor one). The row carries the tooltip, which is also what
// satisfies the `uncoveredControls` ratchet — see docs/UI.md → Tooltips.
import { computed, ref, watch } from 'vue'
import { sortRows, cycleSort, sortIconFor, type SortState, type SortValue } from '../utils/sortRows'
import { useColumnResize } from '../composables/useColumnResize'

export interface SelectionColumn {
  /** key into the row object; its value is rendered verbatim */
  key: string
  label: string
  /** 'link' renders the value as an external link icon instead of text */
  kind?: 'text' | 'link'
  /** clickable header that sorts by this column (asc → desc → off). Off by default. */
  sortable?: boolean
  /**
   * Row field to SORT by, when `key` holds a formatted string. The table renders display strings
   * verbatim and must not parse them back — sorting "3.4 MB" or "08/08/2026" as text is wrong — so a
   * formatted column carries its raw value (bytes, epoch seconds) in a second field and names it here.
   */
  sortKey?: string
  /** truncate an over-long value with an ellipsis instead of widening the table. Full value on hover. */
  ellipsis?: boolean
}

const props = withDefaults(defineProps<{
  columns: SelectionColumn[]
  rows: Record<string, any>[]
  /** currently selected row id */
  modelValue: string
  /** which row field is the id (defaults to `name`) */
  idKey?: string
  disabled?: boolean
  /** per-row hover help. Falls back to a generic line so the control is never tooltip-less. */
  rowTooltip?: (row: Record<string, any>) => string
  /** header for the trailing `#actions` column; omit when the slot is unused */
  actionsLabel?: string
  /** localStorage key for the chosen sort. Omit and the sort resets on remount. */
  sortStorageKey?: string
  /**
   * Drag-resizable columns, persisted under this localStorage key. Omit and the table sizes to its
   * content as before — which is right for a short options table and wrong for a list of long names.
   */
  columnWidthKey?: string
  /** starting px width per column when `columnWidthKey` is set. */
  defaultColumnWidth?: number
}>(), {
  idKey: 'name',
  disabled: false,
  actionsLabel: '',
  sortStorageKey: '',
  columnWidthKey: '',
  defaultColumnWidth: 140,
})

const emit = defineEmits<{ 'update:modelValue': [string] }>()

const idOf = (row: Record<string, any>) => String(row[props.idKey])
const tipOf = (row: Record<string, any>) =>
  props.rowTooltip ? props.rowTooltip(row) : 'Select this option'

function pick(row: Record<string, any>) {
  if (props.disabled) return
  const id = idOf(row)
  if (id !== props.modelValue) emit('update:modelValue', id)
}

const selected = computed(() => props.modelValue)

// ── Sorting (opt-in per column) ────────────────────────────────────────────────
// A header cycles asc → desc → OFF, where off restores the order the caller handed us — which is
// itself meaningful (the movie list arrives newest-first, the compressor list in the order worth
// reading). The ordering rule is the shared `sortRows`, so this table agrees with the image table
// about blanks, numeric strings and stable ties.
function loadSort(): SortState {
  if (!props.sortStorageKey) return null
  try {
    const raw = localStorage.getItem(props.sortStorageKey)
    const p = raw ? JSON.parse(raw) : null
    return p && typeof p.key === 'string' && (p.dir === 'asc' || p.dir === 'desc') ? p : null
  } catch { return null }
}
const sort = ref<SortState>(loadSort())
watch(sort, s => {
  if (!props.sortStorageKey) return
  try {
    if (s) localStorage.setItem(props.sortStorageKey, JSON.stringify(s))
    else localStorage.removeItem(props.sortStorageKey)
  } catch { /* ignore */ }
})

function toggleSort(c: SelectionColumn) {
  if (c.sortable) sort.value = cycleSort(sort.value, c.key)
}
const sortActive = (key: string) => sort.value?.key === key
const sortIcon = (key: string) => sortIconFor(sort.value, key)

const sortedRows = computed(() => {
  const s = sort.value
  if (!s) return props.rows
  const col = props.columns.find(c => c.key === s.key)
  const field = col?.sortKey ?? s.key
  return sortRows(props.rows, r => r[field] as SortValue, s.dir)
})

// ── Column widths (opt-in) ─────────────────────────────────────────────────────
// Only when the caller asks (`columnWidthKey`). Sizing the columns means `table-layout: fixed`, and
// under fixed layout EVERY column shares the width evenly unless it is given one — which is how the
// radio column ended up as wide as the data columns. So the fixed path declares all of them: a narrow
// constant for the radio, the composable's width per data column, and the rest for `#actions`.
const resizable = computed(() => !!props.columnWidthKey)
const { widthOf, onColumnResizeStart } = useColumnResize({
  defaultWidth: () => props.defaultColumnWidth,
  storageKey: props.columnWidthKey || undefined,
})
</script>

<template>
  <table class="sel-table" :class="{ sized: resizable }">
    <!-- fixed layout needs every column declared, or the radio column claims an equal share -->
    <colgroup v-if="resizable">
      <col class="sel-col-pick">
      <col v-for="c in columns" :key="c.key" :style="{ width: widthOf(c.key) }">
      <col v-if="$slots.actions">
    </colgroup>
    <thead>
      <tr>
        <th></th>
        <th v-for="c in columns" :key="c.key"
            v-tooltip.bottom="c.sortable ? `${c.label} — click to sort` : undefined">
          <span v-if="c.sortable" class="sel-th-sort" :class="{ active: sortActive(c.key) }"
                @click="toggleSort(c)">
            {{ c.kind === 'link' ? '' : c.label }} <i :class="['sel-sort-ico', sortIcon(c.key)]" />
          </span>
          <template v-else>{{ c.kind === 'link' ? '' : c.label }}</template>
          <!-- drag the header's right edge to widen the column (persisted) -->
          <div v-if="resizable" class="sel-col-resize" @mousedown.stop="onColumnResizeStart(c.key, $event)"
               v-tooltip.bottom="'Drag to resize the column'" />
        </th>
        <th v-if="$slots.actions">{{ actionsLabel }}</th>
      </tr>
    </thead>
    <tbody>
      <tr v-for="row in sortedRows" :key="idOf(row)"
          :class="{ 'sel-row': true, 'sel-on': idOf(row) === selected }"
          v-tooltip.top="tipOf(row)"
          @click="pick(row)">
        <td>
          <input type="radio" :checked="idOf(row) === selected" :disabled="disabled" tabindex="-1">
        </td>
        <td v-for="c in columns" :key="c.key" :class="{ 'sel-ellipsis': c.ellipsis }">
          <a v-if="c.kind === 'link' && row[c.key]" :href="row[c.key]" target="_blank"
             rel="noopener" @click.stop><i class="pi pi-external-link" /></a>
          <template v-else-if="c.kind !== 'link'">{{ row[c.key] }}</template>
        </td>
        <!-- Per-row actions (rename, delete, …). `@click.stop` so a button never doubles as a row
             pick — the row hit target is the whole row, which would otherwise swallow the intent. -->
        <td v-if="$slots.actions" class="sel-actions" @click.stop>
          <slot name="actions" :row="row" />
        </td>
      </tr>
    </tbody>
  </table>
</template>

<style scoped>
.sel-table {
  border-collapse: collapse;
  font-size: var(--cc-fs-sm);
}
.sel-actions { white-space: nowrap; }
.sel-table th {
  text-align: left;
  font-weight: 500;
  color: var(--cc-text-dim);
  padding: 0.15rem 0.55rem;
}
.sel-table td {
  padding: 0.2rem 0.55rem;
  border-top: 1px solid var(--cc-border);
  white-space: nowrap;
}
/* a sortable header: the label + its direction arrow, clickable as one target */
.sel-th-sort { cursor: pointer; user-select: none; display: inline-flex; align-items: center; gap: 0.25rem; }
.sel-th-sort:hover, .sel-th-sort.active { color: var(--cc-text); }
.sel-sort-ico { font-size: var(--cc-fs-xs); opacity: 0.55; }
.sel-th-sort.active .sel-sort-ico { opacity: 1; }
/* ── Sized (drag-resizable) columns ───────────────────────────────────────────
   Only when the caller opts in. `fixed` is what makes a column obey its width instead of stretching
   to its content — and is also why the radio column needs one of its own (see the colgroup). */
.sel-table.sized { width: 100%; table-layout: fixed; }
.sel-col-pick { width: 1.75rem; }
.sel-table.sized th { position: relative; }
.sel-table.sized th, .sel-table.sized td { overflow: hidden; text-overflow: ellipsis; }
.sel-col-resize {
  position: absolute; right: 0; top: 0; width: 5px; height: 100%;
  cursor: col-resize; z-index: 1;
}
.sel-col-resize::after {
  content: ''; position: absolute; right: 1px; top: 20%; bottom: 20%; width: 1px;
  background: var(--cc-border);
}
.sel-col-resize:hover::after { background: var(--cc-accent); }
/* an over-long value truncates instead of widening the table; the row tooltip carries the full text */
.sel-ellipsis { max-width: 0; width: 100%; overflow: hidden; text-overflow: ellipsis; }
.sel-row { cursor: pointer; }
.sel-row:hover { background: var(--cc-surface-2); }
/* selected = amber, as a tint + left rule rather than a solid fill — `--cc-selected` is the house
   ACCENT (canvas panels ring with it), so filling the row would read as a different component */
.sel-on { background: color-mix(in srgb, var(--cc-selected) 12%, transparent); }
.sel-on td:first-child { box-shadow: inset 2px 0 0 var(--cc-selected); }
</style>
