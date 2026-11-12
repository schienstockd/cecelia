<!--
  ChipSelect — the ONE canonical inline selection control. Replaces the hand-rolled pill/segmented
  selectors that had drifted into ~a dozen bespoke copies (pool chips, the byte-for-byte-duplicated
  `.seg` block, param chips, filter chips, …). One component, two visual dialects:

    variant="pill"       rounded capsules (border-radius 999px) — the default; wraps.
    variant="segmented"  a joined segmented control (the old `.seg`) — single row, no wrap.
    variant="grid"       joined segmented look but WRAPS into a CSS grid with `columns` cells
                         per row (default 4). Use when a segmented set outgrows its container.

  Selection modes:
    single  (default)     modelValue is a `string`  (or '' when `allowEmpty` and the active one is re-clicked)
    multiple              modelValue is a `string[]` in PICK order
    multiple + reorderable drag the selected chips to reorder the array (pill variant only)

  Per-option extras (all optional): `icon` (icon class), `tip` (tooltip), `disabled`,
  `badge` (a count shown after the label), `accent` (override the active colour — for per-level /
  per-population colours). Pure selection/reorder logic lives in utils/chipSelect.ts (unit-tested).

  Not a fit for: independent-boolean toolbars that also fire actions / open dropdowns (those aren't
  "pick from a set"), colour-swatch grids, and reorderable tab strips (TabbedCanvas) — those stay bespoke.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { toggleValue, moveItem, partitionOptions, selectAllState, chipTree,
         visibleChips, CHIP_COLLAPSE_ABOVE } from '../utils/chipSelect'

export interface ChipOption {
  value: string
  label?: string
  icon?: string
  tip?: string
  disabled?: boolean
  badge?: string | number
  accent?: string          // CSS colour: overrides --cc-accent for this option's active state
  swatch?: string          // CSS colour: renders a small always-visible colour dot inside the chip
                           // (colour-picker chips, palette selectors). Distinct from `accent`
                           // (active-only) and `icon` (glyph). See `DrawSurface.vue` colour strip.
  path?: string[]          // hierarchical options (population paths): the pill dialect lays out as an
                           // indented tree, each chip showing its last segment; `label` → tooltip.
}

const props = withDefaults(defineProps<{
  options: ChipOption[]
  modelValue: string | string[] | null
  multiple?: boolean
  reorderable?: boolean
  variant?: 'pill' | 'segmented' | 'grid'
  columns?: number         // grid variant only: cells per row (default 4).
  disabled?: boolean
  allowEmpty?: boolean     // single-select: re-clicking the active chip clears the selection
  selectAll?: boolean      // multiple: prepend an All chip that fills the selection, or clears a full one
  ariaLabel?: string
  collapseAbove?: number   // pill only: a longer list collapses to its picked chips + filter. 0 = never
}>(), {
  multiple: false,
  reorderable: false,
  variant: 'pill',
  columns: 4,
  disabled: false,
  allowEmpty: false,
  selectAll: false,
  collapseAbove: CHIP_COLLAPSE_ABOVE,
})

const emit = defineEmits<{ 'update:modelValue': [string | string[]] }>()

const selected = computed<string[]>(() =>
  props.multiple ? ((props.modelValue as string[]) ?? []) : [])

// `selectAll` — one chip that fills the selection, or empties a full one. Opt-in, and multiple-only:
// a two-option list does not need it, and on a single-select "all" is meaningless. Logic is in
// utils/chipSelect (unit-tested) so the partial-selection rule can't drift.
const allToggle = computed(() => selectAllState(props.options, selected.value))
const showSelectAll = computed(() => props.selectAll && props.multiple && allToggle.value.enabled)
function toggleAll() {
  if (props.disabled) return
  emit('update:modelValue', allToggle.value.next)
}

// Long lists (see `visibleChips`): collapsed to what is picked, a "+N more" chip expands, a filter
// box narrows. View state of the control itself, not a setting — a fresh mount starts collapsed.
const expanded = ref(false)
const filter = ref('')
const isLong = computed(() => props.variant === 'pill' && !props.reorderable
  && props.collapseAbove > 0 && props.options.length > props.collapseAbove)
const pickedValues = computed(() => props.multiple ? selected.value
  : (typeof props.modelValue === 'string' && props.modelValue ? [props.modelValue] : []))
const shown = computed<ChipOption[]>(() => isLong.value
  ? visibleChips(props.options, pickedValues.value, { expanded: expanded.value, filter: filter.value })
  : props.options)
const hiddenCount = computed(() => props.options.length - shown.value.length)

const byValue = computed(() => {
  const m = new Map<string, ChipOption>()
  for (const o of props.options) m.set(o.value, o)
  return m
})

// Render order. Reorderable multi-select shows picked chips first (in pick order, draggable), then the
// remaining options dimmed. Everything else keeps the caller's option order.
const rendered = computed<{ opt: ChipOption; on: boolean; drag: boolean }[]>(() => {
  if (props.multiple && props.reorderable) {
    const { selected: sel, unselected } = partitionOptions(props.options.map(o => o.value), selected.value)  // never long
    return [
      ...sel.map(v => ({ opt: byValue.value.get(v)!, on: true, drag: true })),
      ...unselected.map(v => ({ opt: byValue.value.get(v)!, on: false, drag: false })),
    ].filter(r => r.opt)
  }
  return shown.value.map(o => ({
    opt: o,
    on: props.multiple ? selected.value.includes(o.value) : props.modelValue === o.value,
    drag: false,
  }))
})

// Hierarchical layout (see `path`). One rendered row per tree line; a flat list is ONE row with
// `display: contents`, so the chip markup below exists once and the non-tree layout is unchanged.
type Item = { kind: 'chip'; opt: ChipOption; on: boolean; drag: boolean; text: string; tip?: string }
          | { kind: 'text'; text: string } | { kind: 'sep' }
const isTree = computed(() => props.variant === 'pill' && !props.reorderable && props.options.some(o => o.path?.length))
const rows = computed<{ depth: number; tree: boolean; items: Item[] }[]>(() => {
  const chip = (r: { opt: ChipOption; on: boolean; drag: boolean }): Item =>
    ({ kind: 'chip', ...r, text: r.opt.label ?? r.opt.value, tip: r.opt.tip })
  if (!isTree.value) return [{ depth: 0, tree: false, items: rendered.value.map(chip) }]
  const on = new Map(rendered.value.map(r => [r.opt.value, r]))
  const short = (o: ChipOption): Item => {
    const r = on.get(o.value)!
    return { kind: 'chip', ...r, text: o.path?.length ? o.path[o.path.length - 1] : (o.label ?? o.value),
             tip: o.tip ?? o.label ?? o.value }
  }
  return chipTree(shown.value).map(row => ({
    depth: row.depth, tree: true,
    items: [
      ...(row.head === null ? [] : [typeof row.head === 'string' ? { kind: 'text' as const, text: row.head } : short(row.head)]),
      ...(row.head !== null && row.leaves.length ? [{ kind: 'sep' as const }] : []),
      ...row.leaves.map(short),
    ],
  }))
})

function pick(o: ChipOption) {
  if (props.disabled || o.disabled) return
  if (props.multiple) {
    emit('update:modelValue', toggleValue(selected.value, o.value))
  } else if (props.allowEmpty && props.modelValue === o.value) {
    emit('update:modelValue', '')
  } else {
    emit('update:modelValue', o.value)
  }
}

// ── drag-to-reorder (pill, multiple, reorderable) — native HTML5 DnD, the repo convention ──
let dragValue: string | null = null
function onDragStart(o: ChipOption) { dragValue = o.value }
function onDrop(target: ChipOption) {
  if (dragValue === null || dragValue === target.value) { dragValue = null; return }
  const arr = selected.value
  const from = arr.indexOf(dragValue)
  const to = arr.indexOf(target.value)
  dragValue = null
  if (from < 0 || to < 0) return
  emit('update:modelValue', moveItem(arr, from, to))
}

// A per-option `accent` marks a SEMANTIC colour (log level, population colour, …). Render it as a
// tinted chip — faint accent fill + accent border + accent text — which stays readable whether the
// accent is a pastel (error/warn levels) or a saturated hue, unlike a solid fill with white text.
function activeStyle(o: ChipOption, on: boolean): Record<string, string> | undefined {
  if (!on || !o.accent) return undefined
  return { background: `color-mix(in srgb, ${o.accent} 20%, transparent)`, borderColor: o.accent, color: o.accent }
}
</script>

<template>
  <div class="chip-select" :class="[variant, { 'is-disabled': disabled, stacked: isTree || isLong }]"
       :style="variant === 'grid' ? { '--cc-chip-cols': String(columns) } : undefined"
       role="group" :aria-label="ariaLabel">
    <!-- Reads "All" and shows the current tally, so it doubles as the answer to "how many of these
         are on?" — which is otherwise a counting exercise on a long channel list. -->
    <div :class="isTree || isLong ? 'cc-row cc-row-tight chip-row chip-ctrl' : 'chip-flat'">
    <button
      v-if="showSelectAll" type="button"
      class="chip chip-all" :class="{ on: allToggle.state === 'all', part: allToggle.state === 'some' }"
      :disabled="disabled"
      v-tooltip.bottom="allToggle.state === 'all' ? 'Clear the selection' : 'Select every option'"
      @click="toggleAll"
    >
      <span class="chip-lbl">All</span>
      <span class="chip-badge">{{ selected.length }}/{{ options.filter(o => !o.disabled).length }}</span>
    </button>
    <template v-if="isLong">
      <button type="button" class="chip chip-all chip-more" :disabled="disabled || !!filter"
              v-tooltip.bottom="expanded ? 'Show only the picked ones' : 'Show every option'"
              @click="expanded = !expanded">
        <span class="chip-lbl">{{ expanded ? 'Less' : `+${hiddenCount} more` }}</span>
      </button>
      <input v-model="filter" type="search" class="chip-filter cc-input-2xs" placeholder="Filter…"
             :aria-label="ariaLabel ? `Filter ${ariaLabel}` : 'Filter options'" :disabled="disabled" />
    </template>
    </div>

    <div v-for="(row, ri) in rows" :key="ri" :class="row.tree ? ['cc-row cc-row-tight chip-row', { nested: row.depth > 0 }] : isLong ? 'cc-row cc-row-tight chip-row' : 'chip-flat'"
         :style="row.tree && row.depth ? { '--cc-chip-depth': String(row.depth) } : undefined">
      <template v-for="(it, ii) in row.items" :key="it.kind === 'chip' ? it.opt.value : `${it.kind}${ii}`">
        <span v-if="it.kind === 'text'" class="chip-head-text">{{ it.text }}</span>
        <span v-else-if="it.kind === 'sep'" class="chip-sep" aria-hidden="true">›</span>
        <button
          v-else type="button"
          class="chip" :class="{ on: it.on, drag: it.drag, disabled: it.opt.disabled }"
          :style="activeStyle(it.opt, it.on)"
          :disabled="disabled || it.opt.disabled"
          :draggable="it.drag && !disabled"
          v-tooltip.bottom="it.tip"
          :aria-label="it.opt.label === '' ? (it.tip || it.opt.value) : undefined"
          @click="pick(it.opt)"
          @dragstart="onDragStart(it.opt)"
          @dragover.prevent
          @drop.prevent="onDrop(it.opt)"
        >
          <span v-if="it.opt.swatch" class="chip-swatch" :style="{ background: it.opt.swatch }" />
          <i v-if="it.opt.icon" :class="it.opt.icon" />
          <span v-if="it.opt.label !== ''" class="chip-lbl">{{ it.text }}</span>
          <span v-if="it.opt.badge !== undefined && it.opt.badge !== ''" class="chip-badge">{{ it.opt.badge }}</span>
        </button>
      </template>
    </div>
  </div>
</template>

<style scoped>
.chip-select { display: flex; align-items: center; }
.chip-select.is-disabled { opacity: 0.55; pointer-events: none; }

.chip {
  display: inline-flex; align-items: center; gap: 0.3rem;
  color: var(--cc-text-dim); cursor: pointer;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2);
  font-size: var(--cc-fs-xs); white-space: nowrap;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
}
.chip:hover:not(.disabled) { color: var(--cc-text); border-color: var(--cc-accent); }
.chip.on { background: var(--cc-accent-fill); border-color: var(--cc-accent-fill); color: var(--cc-on-solid); }
.chip.disabled { opacity: 0.45; cursor: default; }
/* The All chip is a control, not a member of the set — dashed so it reads as acting ON the chips
   rather than being one, and `part` marks a partial selection (neither on nor off). */
.chip-all { border-style: dashed; }
.chip-all.part { color: var(--cc-text); border-color: var(--cc-accent); }
.chip.drag { cursor: grab; }
.chip.drag:active { cursor: grabbing; }
.chip-badge {
  font-size: var(--cc-fs-2xs); font-variant-numeric: tabular-nums; line-height: 1;
  padding: 0.05rem 0.28rem; border-radius: var(--cc-radius-pill);
  background: color-mix(in srgb, currentColor 22%, transparent);
}
/* Optional colour dot — palette pickers (see DrawSurface annotation swatches). Ring around it so a
   white swatch still reads on the chip's own light surface. */
.chip-swatch {
  display: inline-block; width: 0.75rem; height: 0.75rem;
  border-radius: 50%; border: 1px solid var(--cc-swatch-edge);
  flex-shrink: 0;
}

/* ── pill dialect: wrapping capsules ── */
.chip-select.pill { flex-wrap: wrap; gap: 0.25rem; }
.pill .chip { padding: 0.15rem 0.55rem; border-radius: var(--cc-radius-pill); }

/* Flat list: the row wrapper vanishes so chips flow in the container exactly as before. */
.chip-flat { display: contents; }
/* Long lists (> `collapseAbove`) and trees stack: a control line (All · +N more · filter), then rows. */
/* ── tree layout (options with `path`): one line per parent, leaves inline after a `›`; deeper
     lines indent and carry a faint guide so siblings line up under their parent. ── */
.chip-select.pill.stacked { flex-direction: column; align-items: stretch; gap: 0.2rem; }
.chip-row { margin-left: calc(var(--cc-chip-depth, 0) * 0.7rem); }
.chip-row.nested { padding-left: 0.35rem; border-left: 1px solid var(--cc-border); }
.chip-filter { width: 7rem; min-width: 0; }
.chip-more { font-style: normal; }
.chip-head-text { font-size: var(--cc-fs-xs); color: var(--cc-text-dim); padding: 0 0.15rem; }
.chip-sep { font-size: var(--cc-fs-xs); color: var(--cc-text-dim); opacity: 0.6; }

/* ── segmented dialect: one joined row (the old `.seg`) ── */
.chip-select.segmented { display: inline-flex; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm); overflow: hidden; }
.segmented .chip { border: none; border-radius: 0; padding: 4px 8px; font-size: var(--cc-fs-sm); }
.segmented .chip + .chip { border-left: 1px solid var(--cc-border); }
.segmented .chip:hover:not(.disabled) { border-color: transparent; }

/* ── grid dialect: same joined look as segmented but wraps into N cells per row.
     The 1px `gap` fills with the container background, which reads as a border between
     both adjacent columns AND adjacent rows — no `:nth-child(Nn+1)` gymnastics needed. */
.chip-select.grid {
  display: grid;
  grid-template-columns: repeat(var(--cc-chip-cols, 4), minmax(0, 1fr));
  gap: 1px;
  background: var(--cc-border);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  overflow: hidden;
}
.grid .chip {
  border: none; border-radius: 0; padding: 4px 8px; font-size: var(--cc-fs-sm);
  background: var(--cc-surface-2);
  justify-content: center;
}
/* Explicit re-assertion — the `.grid .chip` selector above is more specific than the base
   `.chip.on`, so the accent background gets shadowed without this. */
.grid .chip.on { background: var(--cc-accent-fill); color: var(--cc-on-solid); }
.grid .chip:hover:not(.disabled) { border-color: transparent; }
</style>
