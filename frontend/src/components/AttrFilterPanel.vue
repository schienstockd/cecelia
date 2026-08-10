<!--
  AttrFilterPanel — the ONE "filter these rows by their attributes" dropdown: a chip row per attribute
  key, then Apply / Reset / Invert.

  Extracted from `ModuleLayout.vue`, which owned it inline for the image table. It is not about images:
  it needs rows carrying an `attr` bag and nothing else, so the Movies list — whose rows are joined back
  to their source image — asks the same question with the same control. The rule lives in
  `utils/attrFilter.ts` (pure, tested); this is the chrome.

  It renders NOTHING when the rows carry no attributes, so a host can place it unconditionally.

  What it does NOT own: whether it is open. That is the host's action bar (a Filter button next to its
  own toggles), and the two pages phrase it in their own button idiom.

  See docs/UI.md → Filtering rows by attribute.
-->
<script setup lang="ts">
import { computed } from 'vue'
import ChipSelect from './ChipSelect.vue'
import CcToggle from './CcToggle.vue'
import { attrKeysOf, attrValueMap, attrChipOptions, attrFilterActive, attrFilterDrafted,
         applyAttrFilter, emptyAttrFilter,
         type AttrBearing, type AttrFilterState } from '../utils/attrFilter'

const props = withDefaults(defineProps<{
  /** The rows the chips are derived from — what the user can pick is what is actually there. */
  rows: readonly AttrBearing[]
  modelValue: AttrFilterState
  /** What a row IS, for the tooltips: "Show only movies with these …". */
  noun?: string
}>(), { noun: 'rows' })

const emit = defineEmits<{ 'update:modelValue': [AttrFilterState] }>()

const attrKeys  = computed(() => attrKeysOf(props.rows))
const valueMap  = computed(() => attrValueMap(props.rows))
const hasDraft  = computed(() => attrFilterDrafted(props.modelValue))
const hasApplied = computed(() => attrFilterActive(props.modelValue))

const setDraft = (key: string, next: string[]) =>
  emit('update:modelValue', { ...props.modelValue, draft: { ...props.modelValue.draft, [key]: next } })
</script>

<template>
  <div v-if="attrKeys.length" class="cc-filter-panel">
    <div class="cc-filter-rows">
      <div v-for="key in attrKeys" :key="key" class="cc-filter-row">
        <span class="cc-filter-key cc-eyebrow cc-fs-sm" v-tooltip.right="`Filter by ${key}`">{{ key }}</span>
        <ChipSelect class="cc-filter-chips" multiple :options="attrChipOptions(key, valueMap[key] ?? [])"
          v-tooltip.right="`Show only ${noun} with these ${key} values`"
          :model-value="modelValue.draft[key] ?? []"
          @update:model-value="v => setDraft(key, v as string[])" />
      </div>
    </div>
    <div class="filter-actions">
      <button class="cc-btn cc-btn-ghost" :disabled="!hasDraft"
        @click="emit('update:modelValue', applyAttrFilter(modelValue))"
        v-tooltip.top="`Apply selected filters to the ${noun}`">Apply</button>
      <button class="cc-btn cc-btn-ghost" :disabled="!hasApplied && !hasDraft"
        @click="emit('update:modelValue', emptyAttrFilter())"
        v-tooltip.top="'Clear the attribute filters'">Reset</button>
      <CcToggle class="filter-invert" :model-value="modelValue.invert" :disabled="!hasApplied"
        label="Invert" v-tooltip.top="'Invert the filter — show what does NOT match'"
        @update:model-value="v => emit('update:modelValue', { ...modelValue, invert: v })" />
    </div>
  </div>
</template>

<style scoped>
/* The panel shell and its labelled rows are `.cc-filter-*` in style.css — shared with ModuleLayout's
   "Processed with" row, which is the same scenario (a dropdown of labelled filter controls) and would
   otherwise have needed a second copy of them. Only the action row below is this component's own. */
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
  font-size: var(--cc-fs-sm);
  color: var(--cc-text-dim);
  cursor: pointer;
  user-select: none;
  margin-left: 0.25rem;
}
.filter-invert input { cursor: pointer; }
.filter-invert:has(input:disabled) { opacity: 0.4; cursor: not-allowed; }
</style>
