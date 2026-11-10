<script setup lang="ts">
// The "Checks" cluster in ModuleLayout's action bar (docs/todo/GUI_CLARITY_PLAN.md, Decision 15; audit
// slice 7): one rounded group with a plum frame and label, one `CheckButton` per check — Acquisition,
// QC (n flagged; a click filters the image table to them) and Cohort (only where the page banks cohort
// metrics, so it sits last and the other two never move).
import { computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { qcCheckView } from '../lib/checks'
import AcqCheckButton from './AcqCheckButton.vue'
import CohortCheckButton from './CohortCheckButton.vue'
import CheckButton from './CheckButton.vue'

const props = defineProps<{ setUid: string; module?: string; cohortFuns?: string[]; qcFilterOn: boolean }>()
defineEmits<{ toggleQcFilter: [] }>()

const project = useProjectStore()
const qc = computed(() => qcCheckView(project.sets.find(s => s.uid === props.setUid)?.images ?? [], props.qcFilterOn))
</script>

<template>
  <div class="checks" role="group" aria-label="Checks">
    <span class="checks-label cc-eyebrow cc-fs-2xs">Checks</span>
    <AcqCheckButton :set-uid="setUid" />
    <CheckButton label="QC" :state="qc.state" :count="qc.count" :tip="qc.tip" :pressed="qcFilterOn"
                 :disabled="!qc.count && !qcFilterOn" @click="$emit('toggleQcFilter')" />
    <CohortCheckButton :module="module" :set-uid="setUid" :funs="cohortFuns" />
  </div>
</template>

<style scoped>
.checks {
  display: inline-flex;
  align-items: center;
  gap: var(--cc-space-2xs);
  padding: var(--cc-space-3xs) var(--cc-space-xs);
  border: 1px solid var(--cc-check);
  border-radius: var(--cc-radius-md);
  flex-shrink: 0;
}
.checks-label { color: var(--cc-check-text); padding-right: var(--cc-space-3xs); }
</style>
