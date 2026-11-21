<script setup lang="ts">
/*
  The question a set answers and what you expect — asked once, stored in the set's experiment design,
  and shown wherever it is asked: the design dialog's first step and the acquisition plan form
  (docs/todo/EXPERIMENT_DESIGN_PLAN.md → T5). `compact` = labelled rows, for the plan form.
*/
import { computed } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import type { DesignExpect } from '../utils/experimentDesign'

const props = defineProps<{ question: string; expect: DesignExpect; expectIn: string
                            groupValues: string[]; groupLabel: string; compact?: boolean }>()
const emit = defineEmits<{ 'update:question': [string]; 'update:expect': [DesignExpect]; 'update:expectIn': [string] }>()

const EXPECT_OPTIONS: ChipOption[] = [
  { value: 'unsure', label: 'Not sure' },
  { value: 'same', label: 'No difference' },
  { value: 'higher', label: 'Higher in…' },
  { value: 'lower', label: 'Lower in…' },
]
const expectModel = computed({
  get: () => props.expect,
  set: (v: string | string[] | null) => emit('update:expect', (v as DesignExpect) ?? 'unsure'),
})
const directed = computed(() => props.expect === 'higher' || props.expect === 'lower')
</script>

<template>
  <div class="dq" :class="{ 'dq-compact': compact }">
    <label class="dq-field">
      <span :class="compact ? 'cc-muted cc-fs-xs dq-lbl' : 'cc-eyebrow cc-fs-2xs'">{{ compact ? 'Question' : 'What do you want to find out?' }}</span>
      <textarea :value="question" :rows="compact ? 1 : 2" placeholder="One sentence"
                v-tooltip.bottom="'The question this set answers'"
                @input="emit('update:question', ($event.target as HTMLTextAreaElement).value)" />
    </label>
    <div class="dq-field">
      <span :class="compact ? 'cc-muted cc-fs-xs dq-lbl' : 'cc-eyebrow cc-fs-2xs'">{{ compact ? 'Expected' : 'What do you expect?' }}</span>
      <div class="cc-row">
        <ChipSelect v-model="expectModel" :options="EXPECT_OPTIONS" variant="segmented" aria-label="Expectation"
                    v-tooltip.bottom="'What you expect to see'" />
        <select v-if="directed" :value="expectIn" v-tooltip.bottom="'The group you expect it in'"
                @change="emit('update:expectIn', ($event.target as HTMLSelectElement).value)">
          <option value="" disabled>{{ groupLabel }}…</option>
          <option v-for="v in groupValues" :key="v" :value="v">{{ v }}</option>
        </select>
      </div>
    </div>
  </div>
</template>

<style scoped>
.dq { display: flex; flex-direction: column; gap: 12px; }
.dq-field { display: flex; flex-direction: column; gap: 6px; }
.dq-field textarea { width: 100%; resize: vertical; }
.dq-compact { gap: 6px; }
.dq-compact .dq-field { flex-direction: row; align-items: center; gap: 8px; }
.dq-compact .dq-field textarea { flex: 1; width: auto; }
.dq-lbl { width: 4.5rem; flex: none; }
</style>
