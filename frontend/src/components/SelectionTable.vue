<script setup lang="ts" generic="Row extends Record<string, any>">
// THE canonical table (docs/UI.md → UX-primitive catalog). Rows and columns anywhere in the app.
//
// It began as the single-select COMPARISON table — pick one option where the reason to prefer one is
// a set of comparable numbers (a codec, a preset, a model). A dropdown is the wrong control there: it
// hides the very figures the decision rests on behind a label. That case is still what it is best at,
// and `selectionMode: 'single'` is still the default.
//
// It now also covers the other two shapes, because four tables were hand-rolled purely for want of
// them (`docs/todo/MOVIE_MANAGEMENT_PLAN.md` Decision 9): `'multi'` (checkboxes) and `'none'` (a plain
// list, where a row click means whatever the caller says via `@row-click`). None of those four could
// sort or resize as a result — capabilities that live here and are free on the way in.
//
// Deliberately dumb: it renders whatever display strings it is handed and never formats a number
// itself. The values are measured somewhere real (a backend constant, a benchmark) and should be
// stated in exactly one place — a component that reformatted them would become a second one.
//
// Two scoped slots keep a caller from forking it:
//   #actions            a trailing cell per row for row-scoped buttons (rename, delete). Clicks
//                       inside it do NOT select the row.
//   #cell-<columnKey>   render one cell yourself — an inline edit, a badge, an icon — falling back to
//                       the verbatim value, which is what every column was before the slot existed.
//                       NOTE the fallback applies when your slot renders NOTHING (a `v-if` that is
//                       false), not only when you omit it: give such a slot a `v-else` placeholder, or
//                       `row[key]` leaks through. The image table's status column did exactly that.
//
// GENERIC over its row type, so `#cell-<key>`/`#actions`/`rowClass` hand the caller its OWN row type
// rather than a bare record — without it every slot that calls a typed helper needs a cast.
//
// Rows are selected by CLICKING ANYWHERE on the row; the radio/checkbox is a visual + a11y affordance,
// not the hit target (a 12px radio is a poor one). The row carries the tooltip, which is also what
// satisfies the `uncoveredControls` ratchet — see docs/UI.md → Tooltips.
import { computed, ref, watch, getCurrentInstance } from 'vue'
import { sortRows, cycleSort, sortIconFor, type SortState, type SortValue } from '../utils/sortRows'
import { useColumnResize } from '../composables/useColumnResize'
import { allSelected as allChosen, someSelected as someChosen,
         toggleAllSelection, toggleOneSelection } from '../utils/tableSelection'

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
  /** starting px width for THIS column, overriding `defaultColumnWidth`. A size or a count needs far
   *  less room than a name, and one width for all of them is what pushes a table off its panel. */
  width?: number
  /**
   * Pin this column to the left while the rest scrolls horizontally. Only meaningful on a LEADING run
   * of columns — a sticky column after a scrolling one would slide over its neighbour.
   *
   * The offsets are COMPUTED from the pick column plus the widths of the sticky columns before it, so
   * they stay right when a column is dragged. Hand-written `left:` values are what kept this
   * ImageTable-only: they were `0 / 36px / 68px`, correct until one of those columns changed width.
   */
  sticky?: boolean
  /** Exclude from the drag-resize path — a fixed-width column (an icon, a badge, a count). */
  fixed?: boolean
}

const props = withDefaults(defineProps<{
  columns: SelectionColumn[]
  rows: Row[]
  /** the chosen row id (`selectionMode: 'single'`). Ignored by the other two modes. */
  modelValue?: string
  /**
   * The chosen row ids (`selectionMode: 'multi'`), as `v-model:selected`.
   *
   * A SEPARATE model rather than widening `modelValue` to `string | string[]`: that union propagates
   * into every single-select caller's handler signature, so the three existing consumers would each
   * have to start handling an array they can never receive. One id and many ids are different data,
   * and naming them differently keeps each caller's types honest.
   */
  selected?: string[]
  /** which row field is the id (defaults to `name`) */
  idKey?: string
  disabled?: boolean
  /** per-row hover help. Falls back to a generic line so the control is never tooltip-less. */
  rowTooltip?: (row: Row) => string
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
  /**
   * Width of the trailing `#actions` column when the sized path is on.
   *
   * It MUST be declared. `table-layout: fixed` splits only what is left over between columns with no
   * width — so in a panel narrower than the declared columns there is nothing left, and the actions
   * column collapses to zero: the Movies list asked for 3x150px plus the pick column inside a 380px
   * panel, and its star and delete buttons simply were not there.
   */
  actionsWidth?: string
  /**
   * How a row is picked (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 9):
   *
   *  - `single` a radio; `modelValue` is the chosen id. The original behaviour, still the default.
   *  - `multi`  a checkbox per row; `modelValue` is an array of ids.
   *  - `none`   no pick column at all — the table is a LIST, and what a row click means is the
   *             caller's business (`@row-click`: navigate into a directory, open a notebook).
   *
   * Four hand-rolled tables existed only because this axis didn't: a table with no selection, or with
   * checkboxes, had nowhere to go and grew its own `<thead>`/`<tbody>`/hover CSS — and none of them
   * could sort or resize as a result.
   */
  selectionMode?: 'single' | 'multi' | 'none'
  /**
   * Extra classes for a row — `{ 'dir-row': e.isdir, active: … }`, exactly as you'd write on a `<tr>`.
   * Three of the four migrated tables state something about a row in CSS (a directory, an already-open
   * project, an excluded image), and none of it is the table's business to know.
   */
  rowClass?: (row: Row) => string | Record<string, boolean>
  /** Rows the checkbox can't reach in `multi` (already migrated, not an image, …), by id. */
  disabledIds?: string[]
  /**
   * CONTROLLED sort, as `v-model:sort`. Pass it and the caller owns the state and does the ordering —
   * the table only renders the affordance and reports the cycle. Omit it and the table sorts itself,
   * which is what every existing consumer does.
   *
   * It exists because a sort is not always a view preference: the image table persists one per (scope,
   * set) in the project store, and its ordering is domain-aware (`attr:<key>`, a timelapse duration),
   * neither of which a table that reads row fields out of a localStorage key can do.
   */
  sort?: SortState
  /**
   * Which rows show their `#row-detail`. Required for that slot to render anything: without it the
   * table would emit a detail `<tr>` under EVERY row and the caller's `v-if` would leave an empty,
   * bordered, tinted row behind each one. Expansion state stays the CALLER's — the table only asks.
   */
  isExpanded?: (row: Row) => boolean
  /**
   * Offer the header select-all in `multi`. Off for a table that is multi in SHAPE but capped to one
   * by its own rule (the image table's `singleSelect`), where "select all" is a button that cannot do
   * what it says.
   */
  selectAll?: boolean
}>(), {
  idKey: 'name',
  disabled: false,
  actionsLabel: '',
  sortStorageKey: '',
  columnWidthKey: '',
  defaultColumnWidth: 140,
  actionsWidth: '4.5rem',
  selectionMode: 'single',
  disabledIds: () => [],
  selectAll: true,
})

const emit = defineEmits<{
  'update:modelValue': [string]
  'update:selected': [string[]]
  'update:sort': [SortState]
  /** a row was clicked. Always fires, whatever the selection mode — `none` uses only this. */
  'row-click': [Row]
  'row-dblclick': [Row]
}>()

const idOf = (row: Row) => String(row[props.idKey])
const tipOf = (row: Row) =>
  props.rowTooltip ? props.rowTooltip(row) : 'Select this option'

// A `none`-mode row is only interactive if the caller listens for the click, so the pointer cursor and
// the hover highlight follow that rather than being on unconditionally — a list of read-only rows that
// lights up under the mouse promises something it doesn't do.
const inst = getCurrentInstance()
const rowsClickable = computed(() =>
  props.selectionMode !== 'none' || !!inst?.vnode.props?.onRowClick)

/** The chosen ids, whichever mode — one shape for the template so it never branches on the mode. */
const selectedIds = computed<string[]>(() =>
  props.selectionMode === 'multi' ? (props.selected ?? [])
    : props.modelValue ? [props.modelValue] : [])
const isPicked = (row: Row) => selectedIds.value.includes(idOf(row))

function pick(row: Row) {
  emit('row-click', row)
  if (props.disabled || props.selectionMode === 'none') return
  const id = idOf(row)
  // `disabledIds` greys the checkbox; the ROW is the hit target, so it has to refuse here too or a
  // click anywhere else on the row selects what the checkbox says cannot be selected
  if (props.disabledIds.includes(id)) return
  if (props.selectionMode === 'multi') {
    emit('update:selected', toggleOneSelection(id, selectedIds.value))
    return
  }
  if (id !== props.modelValue) emit('update:modelValue', id)
}

// ── Select-all (multi) ─────────────────────────────────────────────────────────
// Lives here rather than being a caller's header cell: it is part of what `multi` MEANS, and both
// tables that had it had also hand-rolled the same tri-state (`allSelected` / `someSelected` /
// `:indeterminate.prop`). It only ever covers the rows currently RENDERED — a select-all that reached
// rows a filter is hiding is the classic way to act on something you cannot see.
const selectableIds = computed(() =>
  sortedRows.value.map(idOf).filter(id => !props.disabledIds.includes(id)))
const allSelected  = computed(() => allChosen(selectableIds.value, selectedIds.value))
const someSelected = computed(() => someChosen(selectableIds.value, selectedIds.value))
function toggleAll() {
  if (props.disabled) return
  emit('update:selected', toggleAllSelection(selectableIds.value, selectedIds.value))
}

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
const ownSort = ref<SortState>(loadSort())
// controlled when the caller passes `sort`, else the table's own persisted state
const controlled = computed(() => props.sort !== undefined)
const sort = computed<SortState>({
  get: () => (controlled.value ? props.sort! : ownSort.value),
  set: v => { controlled.value ? emit('update:sort', v) : (ownSort.value = v) },
})
watch(sort, s => {
  if (!props.sortStorageKey || controlled.value) return
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
  // a controlled sort means the caller has already ordered the rows — re-sorting here would fight it
  if (!s || controlled.value) return props.rows
  const col = props.columns.find(c => c.key === s.key)
  const field = col?.sortKey ?? s.key
  return sortRows(props.rows, (r: Row) => r[field] as SortValue, s.dir)
})

// ── Column widths (opt-in) ─────────────────────────────────────────────────────
// Only when the caller asks (`columnWidthKey`). Sizing the columns means `table-layout: fixed`, and
// under fixed layout EVERY column shares the width evenly unless it is given one — which is how the
// radio column ended up as wide as the data columns. So the fixed path declares all of them: a narrow
// constant for the radio, the composable's width per data column, and the rest for `#actions`.
const resizable = computed(() => !!props.columnWidthKey)

// px offset for each sticky column: the pick column, then every sticky column before this one. Only a
// LEADING run counts — the first non-sticky column ends it, since pinning something after a scrolling
// column would let it slide over its neighbour.
const PICK_COL_PX = 28   // .sel-col-pick is 1.75rem
const stickyLeft = computed<Record<string, number>>(() => {
  const out: Record<string, number> = {}
  let x = props.selectionMode === 'none' ? 0 : PICK_COL_PX
  for (const c of props.columns) {
    if (!c.sticky) break
    out[c.key] = x
    x += parseFloat(widthOf(c.key))
  }
  return out
})
const stickyStyle = (key: string) =>
  key in stickyLeft.value ? { left: `${stickyLeft.value[key]}px` } : undefined
const isResizable = (c: SelectionColumn) => resizable.value && !c.fixed
// Where the width reset lives: the first column with an actual HEADING, not the first column. Callers
// open with a narrow label-less chrome column as often as not (the image table's viewer eye), and the
// icon then floats alone in a blank 32px header while the Movies table has it tucked after "Movie" —
// one rule reading as two placements. Both of those columns are sticky, so anchoring to the heading
// keeps the reset on screen when a wide table is scrolled sideways, which is the point of it.
const resetColIndex = computed(() => {
  const i = props.columns.findIndex(c => !!c.label)
  return i < 0 ? 0 : i          // every column unlabelled → nowhere better than the front
})
const { widthOf, onColumnResizeStart, resetWidths } = useColumnResize({
  defaultWidth: (key: string) =>
    props.columns.find(c => c.key === key)?.width ?? props.defaultColumnWidth,
  storageKey: props.columnWidthKey || undefined,
})

</script>

<template>
  <table class="sel-table" :class="{ sized: resizable }">
    <!-- fixed layout needs every column declared, or the radio column claims an equal share -->
    <colgroup v-if="resizable">
      <col v-if="selectionMode !== 'none'" class="sel-col-pick">
      <col v-for="c in columns" :key="c.key"
           :style="c.fixed ? (c.width ? { width: `${c.width}px` } : undefined) : { width: widthOf(c.key) }">
      <col v-if="$slots.actions" :style="{ width: actionsWidth }">
    </colgroup>
    <thead>
      <tr>
        <th v-if="selectionMode !== 'none'" class="sel-sticky sel-sticky-pick">
          <input v-if="selectionMode === 'multi' && selectAll" type="checkbox" :checked="allSelected"
                 :indeterminate.prop="someSelected" :disabled="disabled" @click.stop="toggleAll"
                 v-tooltip.right="'Select all / none'" />
        </th>
        <th v-for="(c, ci) in columns" :key="c.key" :class="{ 'sel-sticky': c.sticky }"
            :style="stickyStyle(c.key)"
            v-tooltip.bottom="c.sortable ? `${c.label} — click to sort` : undefined">
          <span v-if="c.sortable" class="sel-th-sort" :class="{ active: sortActive(c.key) }"
                @click="toggleSort(c)">
            {{ c.kind === 'link' ? '' : c.label }} <i :class="['sel-sort-ico', sortIcon(c.key)]" />
          </span>
          <template v-else>{{ c.kind === 'link' ? '' : c.label }}</template>
          <!-- extra header chrome for this column (a select-flagged button, a re-sync) -->
          <slot :name="`head-${c.key}`" :column="c" />
          <!-- Widths persist, so there has to be a way back from a drag that left a column unusably
               narrow — and from a stored width whose column key has since changed, which nothing can
               be dragged to fix. Beside the first real HEADING (see `resetColIndex`), which is both
               still on screen when a wide table is scrolled sideways and somewhere the icon reads as
               belonging to something. -->
          <button v-if="resizable && ci === resetColIndex" class="sel-reset-w cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  @click.stop="resetWidths" v-tooltip.bottom="'Reset the column widths'">
            <i class="pi pi-arrows-h" />
          </button>
          <!-- drag the header's right edge to widen the column (persisted) -->
          <div v-if="isResizable(c)" class="sel-col-resize" @mousedown.stop="onColumnResizeStart(c.key, $event)"
               v-tooltip.bottom="'Drag to resize the column'" />
        </th>
        <th v-if="$slots.actions">{{ actionsLabel }}</th>
      </tr>
    </thead>
    <tbody>
      <template v-for="row in sortedRows" :key="idOf(row)">
      <tr :class="[{ 'sel-row': rowsClickable, 'sel-on': isPicked(row) }, rowClass?.(row)]"
          v-tooltip.top="tipOf(row)"
          @click="pick(row)" @dblclick="$emit('row-dblclick', row)">
        <td v-if="selectionMode !== 'none'" class="sel-sticky sel-sticky-pick">
          <input :type="selectionMode === 'multi' ? 'checkbox' : 'radio'"
                 :checked="isPicked(row)"
                 :disabled="disabled || disabledIds.includes(idOf(row))" tabindex="-1">
        </td>
        <td v-for="c in columns" :key="c.key"
            :class="{ 'sel-ellipsis': c.ellipsis, 'sel-sticky': c.sticky }" :style="stickyStyle(c.key)">
          <!-- A caller may render a cell itself — an inline edit, a badge, an icon — without forking
               the table. Falls through to the verbatim value, which is what every column was. -->
          <slot :name="`cell-${c.key}`" :row="row" :value="row[c.key]">
            <a v-if="c.kind === 'link' && row[c.key]" :href="row[c.key]" target="_blank"
               rel="noopener" @click.stop><i class="pi pi-external-link" /></a>
            <template v-else-if="c.kind !== 'link'">{{ row[c.key] }}</template>
          </slot>
        </td>
        <!-- Per-row actions (rename, delete, …). `@click.stop` so a button never doubles as a row
             pick — the row hit target is the whole row, which would otherwise swallow the intent. -->
        <td v-if="$slots.actions" class="sel-actions" @click.stop>
          <slot name="actions" :row="row" />
        </td>
      </tr>
      <!-- An expanded detail row under its row, spanning every column — a version history, a preview,
           a diff. The caller decides which row is open (the slot simply renders nothing for the rest),
           so the table keeps no expansion state of its own. -->
      <tr v-if="$slots['row-detail'] && isExpanded?.(row)" class="sel-detail-row">
        <td :colspan="columns.length + (selectionMode !== 'none' ? 1 : 0) + ($slots.actions ? 1 : 0)">
          <slot name="row-detail" :row="row" />
        </td>
      </tr>
      </template>
      <tr v-if="!sortedRows.length && $slots.empty">
        <td :colspan="columns.length + (selectionMode !== 'none' ? 1 : 0) + ($slots.actions ? 1 : 0)"
            class="sel-empty">
          <slot name="empty" />
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
/* The actions cell: right-aligned and nowrap, and deliberately NOT `display: flex` — that takes the
   <td> out of the table layout, so it stops sharing the row's height and vertical-align, which reads
   as a ragged row. `.cc-btn` is inline-flex already, so margin (not `gap`) spaces them — and it spaces
   EVERY control, since ConfirmDeleteButton's root is `.cc-del`, not `.cc-btn`. */
.sel-actions { white-space: nowrap; text-align: right; }
.sel-actions > * + * { margin-left: 0.3rem; }
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
/* dim until the header is hovered — it is a rescue, not something to reach for */
.sel-reset-w { opacity: 0.25; margin-left: 0.3rem; vertical-align: middle; }
th:hover .sel-reset-w { opacity: 0.7; }
.sel-reset-w:hover { opacity: 1; color: var(--cc-text); background: var(--cc-surface-2); }
/* Pinned columns. They need an OPAQUE background or the scrolled cells show THROUGH them — and it has
   to track the ROW's state, or a hovered or selected row loses its tint exactly where it is frozen.
   Hence `--row-bg` rather than a fixed colour: the selected tint is a `color-mix` onto transparent for
   the normal cells, and the same mix onto the page background here. */
.sel-table { --row-bg: var(--cc-bg); }
.sel-row:hover { --row-bg: var(--cc-surface-2); }
.sel-on { --row-bg: color-mix(in srgb, var(--cc-selected) 12%, var(--cc-bg)); }
.sel-sticky { position: sticky; z-index: 2; background: var(--row-bg); }
.sel-sticky-pick { left: 0; }
thead .sel-sticky { z-index: 3; background: var(--cc-bg); }
/* `relative` only so the resize handle can sit on the header's right edge. It must NOT beat the sticky
   columns: `.sel-table.sized th` is (0,2,0) and `.sel-sticky` is (0,1,0), so a pinned HEADER cell was
   winning `position: relative` while its body cell stayed `sticky` — and `relative` + `left: 60px`
   SHIFTS the cell 60px right without moving the layout, so the header slid over its neighbour and
   stopped agreeing with the rows. A sticky box is a containing block too, so the handle is fine. */
.sel-table.sized th { position: relative; }
.sel-table.sized th.sel-sticky, .sel-table.sized td.sel-sticky { position: sticky; }
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
/* an EMPTY `row-detail` slot still renders a <tr>, so it must not draw a border/hover of its own */
.sel-detail-row:hover { background: none; }
.sel-empty { text-align: center; padding: 0.6rem; }
.sel-row { cursor: pointer; }
.sel-row:hover { background: var(--cc-surface-2); }
/* selected = amber, as a tint + left rule rather than a solid fill — `--cc-selected` is the house
   ACCENT (canvas panels ring with it), so filling the row would read as a different component */
.sel-on { background: color-mix(in srgb, var(--cc-selected) 12%, transparent); }
.sel-on td:first-child { box-shadow: inset 2px 0 0 var(--cc-selected); }
</style>
