<!--
  WHICH POPULATION FAMILY a plot is slicing by — the one control for it.

  A `rail: 'pops'` view declares its families on its registry entry (`InteractiveView.popTypes`) and
  the rail lists that family's populations. The view still has to say which of them it is showing, and
  all three track views had grown their own copy of the same eight lines: `popTypeOptions` →
  `resolvePopType` → a `<select>` of `popTypeLabel`. The third copy is where that stops being
  acceptable, and the timeline needed the control it never had — it was stuck on whichever family the
  registry happened to list first, which is why the canvas's population picks never reached it.

  Renders NOTHING when there is only one family: a select with one option is chrome that asks a
  question with one answer.

  Pairs with `usePopFamily` — that composable owns the resolution (so a panel's request and this
  control can never disagree), this component owns the markup.
-->
<script setup lang="ts">
import { popTypeLabel, type PopTypeOption } from '../../plots/popTypes'

const props = defineProps<{
  /** the families this plot offers, already resolved through `popTypeOptions` */
  options: PopTypeOption[]
  modelValue: string
}>()
const emit = defineEmits<{ 'update:modelValue': [string] }>()
</script>

<template>
  <select v-if="props.options.length > 1" :value="props.modelValue"
          v-tooltip.top="'Which populations this plot slices by'" aria-label="Population family"
          @change="emit('update:modelValue', ($event.target as HTMLSelectElement).value)">
    <option v-for="o in props.options" :key="o.popType" :value="o.popType">{{ popTypeLabel(o) }}</option>
  </select>
</template>
