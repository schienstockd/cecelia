<!--
  A list of short words you INVENT, typed as chips: type one, Enter (or a comma) makes it a chip, × on
  a chip removes it, Backspace in the empty box takes the last one back. The free-entry twin of
  ChipSelect, which picks from a list that already exists. The value is the ordered `string[]`; a task
  param stores it as one comma-separated string (`utils/chipInput.ts` holds both directions).
-->
<script setup lang="ts">
import { ref } from 'vue'
import { addChips, chipKeyAction } from '../utils/chipInput'

const props = defineProps<{
  modelValue: string[]
  max?: number           // most chips; the box hides once the list is full
  placeholder?: string
  ariaLabel?: string
}>()
const emit = defineEmits<{ 'update:modelValue': [v: string[]] }>()

const draft = ref('')

function commit() {
  const next = addChips(props.modelValue, draft.value, props.max)
  draft.value = ''
  if (next.length !== props.modelValue.length) emit('update:modelValue', next)
}
function remove(i: number) {
  emit('update:modelValue', props.modelValue.filter((_, k) => k !== i))
}
function onKey(e: KeyboardEvent) {
  const act = chipKeyAction(e.key, draft.value)
  if (act === 'commit') { e.preventDefault(); commit() }
  else if (act === 'pop' && props.modelValue.length) remove(props.modelValue.length - 1)
}
// a paste holding commas becomes chips at once; the text after the last comma stays in the box
function onInput(e: Event) {
  const v = (e.target as HTMLInputElement).value
  const cut = v.lastIndexOf(',')
  if (cut < 0) { draft.value = v; return }
  draft.value = v.slice(0, cut)
  commit()
  draft.value = v.slice(cut + 1).trimStart()
}
</script>

<template>
  <div class="chip-input cc-row-tight">
    <span v-for="(c, i) in modelValue" :key="c" class="ci-chip cc-fs-2xs">
      {{ c }}
      <button type="button" class="cc-btn cc-btn-bare ci-x cc-muted cc-fs-3xs" :aria-label="`Remove ${c}`"
              v-tooltip.bottom="'Remove'" @click="remove(i)"><i class="pi pi-times" /></button>
    </span>
    <input v-if="max === undefined || modelValue.length < max"
           type="text" class="cc-input-2xs ci-box" :value="draft" :aria-label="ariaLabel"
           :placeholder="modelValue.length ? '' : placeholder"
           v-tooltip.bottom="'Enter or comma adds; Backspace removes the last'"
           @input="onInput" @keydown="onKey" @blur="commit" />
  </div>
</template>

<style scoped>
.ci-chip { display: inline-flex; align-items: center; gap: 0.1rem; background: var(--cc-surface-2);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill); padding: 0 0.15rem 0 0.4rem;
  color: var(--cc-text); }
.ci-x { padding: 0 0.15rem; }
.ci-x:hover { color: var(--cc-text); }
.ci-box { flex: 1; min-width: 6rem; }
</style>
