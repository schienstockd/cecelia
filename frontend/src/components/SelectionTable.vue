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
// Rows are selected by CLICKING ANYWHERE on the row; the radio is a visual + a11y affordance, not
// the hit target (a 12px radio is a poor one). The row carries the tooltip, which is also what
// satisfies the `uncoveredControls` ratchet — see docs/UI.md → Tooltips.
import { computed } from 'vue'

export interface SelectionColumn {
  /** key into the row object; its value is rendered verbatim */
  key: string
  label: string
  /** 'link' renders the value as an external link icon instead of text */
  kind?: 'text' | 'link'
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
}>(), {
  idKey: 'name',
  disabled: false,
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
</script>

<template>
  <table class="sel-table">
    <thead>
      <tr>
        <th></th>
        <th v-for="c in columns" :key="c.key">{{ c.kind === 'link' ? '' : c.label }}</th>
      </tr>
    </thead>
    <tbody>
      <tr v-for="row in rows" :key="idOf(row)"
          :class="{ 'sel-row': true, 'sel-on': idOf(row) === selected }"
          v-tooltip.top="tipOf(row)"
          @click="pick(row)">
        <td>
          <input type="radio" :checked="idOf(row) === selected" :disabled="disabled" tabindex="-1">
        </td>
        <td v-for="c in columns" :key="c.key">
          <a v-if="c.kind === 'link' && row[c.key]" :href="row[c.key]" target="_blank"
             rel="noopener" @click.stop><i class="pi pi-external-link" /></a>
          <template v-else-if="c.kind !== 'link'">{{ row[c.key] }}</template>
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
.sel-row { cursor: pointer; }
.sel-row:hover { background: var(--cc-surface-2); }
/* selected = amber, as a tint + left rule rather than a solid fill — `--cc-selected` is the house
   ACCENT (canvas panels ring with it), so filling the row would read as a different component */
.sel-on { background: color-mix(in srgb, var(--cc-selected) 12%, transparent); }
.sel-on td:first-child { box-shadow: inset 2px 0 0 var(--cc-selected); }
</style>
