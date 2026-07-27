<!--
  Generic N-state CYCLING button — one icon-button that steps through a fixed list of states on
  click, tooltip and icon following the current state. The value returned is the state's `value`
  string, so it composes cleanly with `useViewState` and Pinia.

  Chosen over a segmented `ChipSelect` when the states form a natural cycle rather than a set of
  parallel choices (e.g. "auto → always visible → always hidden → auto"), and header real estate
  is scarce enough that one icon beats three.

  Usage:
    <CcCycleButton
      v-model="chromeMode"
      :options="[
        { value: 'auto',    icon: 'pi pi-thumbtack',  tip: 'Controls: auto' },
        { value: 'visible', icon: 'pi pi-thumbtack',  tip: 'Controls: always visible', on: true },
        { value: 'hidden',  icon: 'pi pi-eye-slash',  tip: 'Controls: always hidden' },
      ]"
    />

  `on: true` toggles the `.cc-btn-on` styling for that state (highlights the "engaged" look for
  visible/pinned states; leave off/undefined for the neutral default). The button reuses the
  `.cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense` chain so it slots into any panel header row.
-->
<script setup lang="ts">
import { computed } from 'vue'

export interface CycleOption {
  value: string
  icon: string    // full pi class chain, e.g. "pi pi-thumbtack"
  tip: string     // tooltip text
  on?: boolean    // when true, current state renders in the "engaged" (.cc-btn-on) look
}

const props = defineProps<{
  modelValue: string
  options: CycleOption[]
}>()
const emit = defineEmits<{ 'update:modelValue': [string] }>()

const currentIdx = computed(() => {
  const i = props.options.findIndex(o => o.value === props.modelValue)
  return i < 0 ? 0 : i
})
const current = computed<CycleOption>(() => props.options[currentIdx.value] ?? props.options[0])

function next() {
  if (!props.options.length) return
  const nextIdx = (currentIdx.value + 1) % props.options.length
  emit('update:modelValue', props.options[nextIdx].value)
}
</script>

<template>
  <button
    type="button"
    class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense"
    :class="{ 'cc-btn-on': !!current.on }"
    v-tooltip.bottom="current.tip"
    :aria-label="current.tip"
    @click.stop="next"
  >
    <i :class="current.icon" />
  </button>
</template>
