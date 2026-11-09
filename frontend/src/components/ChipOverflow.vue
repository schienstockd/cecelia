<!--
  A row of chips that collapses to the first `max` and a "+N" chip — for a list cell, where wrapping
  every chip grows the row (the Movies list's tags, the project list's owners). The chip itself is the
  caller's (`#chip`), so an owner keeps its icon and a tag its pill. The "+N" tooltip names what it
  hides; `@more` is what a click on it does (a popover of all of them), and without a listener the click
  falls through to whatever the cell does (the Movies list opens its tag editor).
-->
<script setup lang="ts" generic="T">
import { computed } from 'vue'
import { splitChips } from '../utils/chipOverflow'

const props = withDefaults(defineProps<{
  items: readonly T[]
  max?: number
  /** the "+N" chip's classes — the default is the tag pill */
  moreClass?: string
  /** the "+N" tooltip; the default lists the hidden items */
  moreTip?: (hidden: T[]) => string
}>(), { max: 1, moreClass: 'cc-tag cc-fs-2xs' })
const emit = defineEmits<{ more: [MouseEvent] }>()

const split = computed(() => splitChips(props.items, props.max))
const tip = computed(() => props.moreTip ? props.moreTip(split.value.hidden) : split.value.hidden.map(String).join(', '))
</script>

<template>
  <span class="chip-ovf">
    <template v-for="(item, i) in split.shown" :key="i">
      <slot name="chip" :item="item" :index="i"><span class="cc-tag cc-fs-2xs">{{ item }}</span></slot>
    </template>
    <button v-if="split.hidden.length" type="button" class="chip-ovf-more" :class="moreClass"
            v-tooltip.bottom="tip" @click="e => emit('more', e)">+{{ split.hidden.length }}</button>
  </span>
</template>

<style scoped>
.chip-ovf { display: inline-flex; align-items: center; gap: 0.25rem; min-width: 0; white-space: nowrap; }
.chip-ovf-more { cursor: pointer; font-variant-numeric: tabular-nums; font-family: inherit; }
</style>
