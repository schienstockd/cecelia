<!--
  ChipSelect — the ONE canonical inline selection control. Replaces the hand-rolled pill/segmented
  selectors that had drifted into ~a dozen bespoke copies (pool chips, the byte-for-byte-duplicated
  `.seg` block, param chips, filter chips, …). One component, two visual dialects:

    variant="pill"       rounded capsules (border-radius 999px) — the default; wraps.
    variant="segmented"  a joined segmented control (the old `.seg`) — single row, no wrap.

  Selection modes:
    single  (default)     modelValue is a `string`  (or '' when `allowEmpty` and the active one is re-clicked)
    multiple              modelValue is a `string[]` in PICK order
    multiple + reorderable drag the selected chips to reorder the array (pill variant only)

  Per-option extras (all optional): `icon` (PrimeIcons class), `tip` (tooltip), `disabled`,
  `badge` (a count shown after the label), `accent` (override the active colour — for per-level /
  per-population colours). Pure selection/reorder logic lives in utils/chipSelect.ts (unit-tested).

  Not a fit for: independent-boolean toolbars that also fire actions / open dropdowns (those aren't
  "pick from a set"), colour-swatch grids, and reorderable tab strips (TabbedCanvas) — those stay bespoke.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { toggleValue, moveItem, partitionOptions } from '../utils/chipSelect'

export interface ChipOption {
  value: string
  label?: string
  icon?: string
  tip?: string
  disabled?: boolean
  badge?: string | number
  accent?: string          // CSS colour: overrides --cc-accent for this option's active state
}

const props = withDefaults(defineProps<{
  options: ChipOption[]
  modelValue: string | string[] | null
  multiple?: boolean
  reorderable?: boolean
  variant?: 'pill' | 'segmented'
  disabled?: boolean
  allowEmpty?: boolean     // single-select: re-clicking the active chip clears the selection
  ariaLabel?: string
}>(), {
  multiple: false,
  reorderable: false,
  variant: 'pill',
  disabled: false,
  allowEmpty: false,
})

const emit = defineEmits<{ 'update:modelValue': [string | string[]] }>()

const selected = computed<string[]>(() =>
  props.multiple ? ((props.modelValue as string[]) ?? []) : [])

const byValue = computed(() => {
  const m = new Map<string, ChipOption>()
  for (const o of props.options) m.set(o.value, o)
  return m
})

// Render order. Reorderable multi-select shows picked chips first (in pick order, draggable), then the
// remaining options dimmed. Everything else keeps the caller's option order.
const rendered = computed<{ opt: ChipOption; on: boolean; drag: boolean }[]>(() => {
  if (props.multiple && props.reorderable) {
    const { selected: sel, unselected } = partitionOptions(props.options.map(o => o.value), selected.value)
    return [
      ...sel.map(v => ({ opt: byValue.value.get(v)!, on: true, drag: true })),
      ...unselected.map(v => ({ opt: byValue.value.get(v)!, on: false, drag: false })),
    ].filter(r => r.opt)
  }
  return props.options.map(o => ({
    opt: o,
    on: props.multiple ? selected.value.includes(o.value) : props.modelValue === o.value,
    drag: false,
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
  <div class="chip-select" :class="[variant, { 'is-disabled': disabled }]"
       role="group" :aria-label="ariaLabel">
    <button
      v-for="r in rendered" :key="r.opt.value" type="button"
      class="chip" :class="{ on: r.on, drag: r.drag, disabled: r.opt.disabled }"
      :style="activeStyle(r.opt, r.on)"
      :disabled="disabled || r.opt.disabled"
      :draggable="r.drag && !disabled"
      v-tooltip.bottom="r.opt.tip"
      @click="pick(r.opt)"
      @dragstart="onDragStart(r.opt)"
      @dragover.prevent
      @drop.prevent="onDrop(r.opt)"
    >
      <i v-if="r.opt.icon" :class="r.opt.icon" />
      <span v-if="r.opt.label !== ''" class="chip-lbl">{{ r.opt.label ?? r.opt.value }}</span>
      <span v-if="r.opt.badge !== undefined && r.opt.badge !== ''" class="chip-badge">{{ r.opt.badge }}</span>
    </button>
  </div>
</template>

<style scoped>
.chip-select { display: flex; align-items: center; }
.chip-select.is-disabled { opacity: 0.55; pointer-events: none; }

.chip {
  display: inline-flex; align-items: center; gap: 0.3rem;
  color: var(--cc-text-dim); cursor: pointer;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2);
  font-size: 0.7rem; white-space: nowrap;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
}
.chip:hover:not(.disabled) { color: var(--cc-text); border-color: var(--cc-accent); }
.chip.on { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }
.chip.disabled { opacity: 0.45; cursor: default; }
.chip.drag { cursor: grab; }
.chip.drag:active { cursor: grabbing; }
.chip-badge {
  font-size: 0.62rem; font-variant-numeric: tabular-nums; line-height: 1;
  padding: 0.05rem 0.28rem; border-radius: 999px;
  background: color-mix(in srgb, currentColor 22%, transparent);
}

/* ── pill dialect: wrapping capsules ── */
.chip-select.pill { flex-wrap: wrap; gap: 0.25rem; }
.pill .chip { padding: 0.15rem 0.55rem; border-radius: 999px; }

/* ── segmented dialect: one joined row (the old `.seg`) ── */
.chip-select.segmented { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.segmented .chip { border: none; border-radius: 0; padding: 4px 8px; font-size: 12px; }
.segmented .chip + .chip { border-left: 1px solid var(--cc-border); }
.segmented .chip:hover:not(.disabled) { border-color: transparent; }
</style>
