<script setup lang="ts">
// The "Checks" cluster in ModuleLayout's action bar (docs/todo/GUI_CLARITY_PLAN.md, Decision 15; audit
// slice 7): one rounded group with a neutral frame and label (the check icons carry the colour), one `CheckButton` per check — Acquisition,
// Design (the set's experiment design; amber when there is none or images sit outside it), QC (n flagged; a click filters the image table to them) and Cohort (only where the page banks cohort
// metrics, so it sits last and the other two never move).
// The label is the collapse toggle: collapsed (the default, so the set row stays one line beside the
// gesture hints) each check keeps its icon + count and moves its name into the tooltip. A per-viewer
// preference (profileStorage via useStoredSectionOpen), not per set or page.
import { computed, provide } from 'vue'
import { useProjectStore } from '../stores/project'
import { CHECKS_COLLAPSED, qcCheckView } from '../lib/checks'
import { useStoredSectionOpen } from '../utils/sectionOpen'
import AcqCheckButton from './AcqCheckButton.vue'
import CohortCheckButton from './CohortCheckButton.vue'
import DesignCheckButton from './DesignCheckButton.vue'
import CheckButton from './CheckButton.vue'

const props = defineProps<{ setUid: string; module?: string; cohortFuns?: string[]; qcFilterOn: boolean }>()
defineEmits<{ toggleQcFilter: [] }>()

const project = useProjectStore()

const open = useStoredSectionOpen('cc.checksOpen', false)
provide(CHECKS_COLLAPSED, computed(() => !open.value))
const qc = computed(() => qcCheckView(project.sets.find(s => s.uid === props.setUid)?.images ?? [], props.qcFilterOn))
</script>

<template>
  <div class="checks" role="group" aria-label="Checks" data-guide="images.checks">
    <button class="cc-section-toggle cc-eyebrow cc-fs-2xs checks-label" data-guide="checks.toggle" :aria-expanded="open"
            @click="open = !open" v-tooltip.bottom="open ? 'Hide check names' : 'Show check names'">
      Checks<i class="pi" :class="open ? 'pi-chevron-left' : 'pi-chevron-right'" />
    </button>
    <AcqCheckButton :set-uid="setUid" />
    <DesignCheckButton :set-uid="setUid" />
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
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  flex-shrink: 0;
}
.checks-label { width: auto; gap: var(--cc-space-3xs); padding: 0 var(--cc-space-3xs) 0 0; }
</style>
