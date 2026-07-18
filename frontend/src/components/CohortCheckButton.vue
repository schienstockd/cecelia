<script setup lang="ts">
// "Check cohort consistency" — runs cohort QC for the module's stage fun_names over the current set
// (POST /api/qc/cohort/check, which persists per-image outlier findings + a [Cecelia — Cohort check]
// lab-log entry with the cross-image detail). Shown only for modules that bank cohort metrics
// (cohortFunsFor). NO toast — the durable, detailed record is the lab log; the button just goes AMBER
// when the last run flagged something, pointing the user there. See docs/todo/QC_OBSERVER_PLAN.md (A3).
import { computed, ref } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { cohortFunsFor } from '../lib/cohortStages'
import { runCohortCheck } from '../lib/cohortCheck'

// `funs` overrides the built-in COHORT_STAGES lookup — passed by pages whose cohort funs come from the
// backend (custom modules: funNames ∩ COHORT_METRICS), so the button needs no hardcoded per-page entry.
const props = defineProps<{ module?: string; setUid?: string; funs?: string[] }>()
const projectMeta = useProjectMetaStore()
const busy = ref(false)
const foundWarnings = ref(false)   // last run flagged an outlier → amber + "see the lab log" tooltip

const funs = computed(() => props.funs?.length ? props.funs : cohortFunsFor(props.module))
const canCheck = computed(() => funs.value.length > 0 && !!props.setUid && !!projectMeta.current?.uid)
const tip = computed(() => foundWarnings.value
  ? 'Cohort check found warnings — check the lab log for details'
  : 'Flag images whose output is an outlier vs the rest of the set')

async function check() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || busy.value) return
  busy.value = true
  try {
    const r = await runCohortCheck(projectUid, props.setUid, funs.value)
    // No toast: the cross-image detail is written to the [Cecelia — Cohort check] lab-log entry. The
    // button turns amber when something flagged so the user knows to look there; clears when clean.
    foundWarnings.value = r.severity === 'warn'
  } catch (e) {
    console.error('cohort check failed', e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <button v-if="canCheck" class="cohort-check-btn" :class="{ warn: foundWarnings }" :disabled="busy"
          @click="check" v-tooltip.bottom="tip">
    <i :class="['pi', busy ? 'pi-spin pi-spinner' : (foundWarnings ? 'pi-exclamation-triangle' : 'pi-chart-bar')]" />
    {{ busy ? 'Checking…' : 'Check cohort' }}
  </button>
</template>

<style scoped>
.cohort-check-btn {
  display: inline-flex; align-items: center; gap: 0.3rem;
  font-size: 0.72rem; padding: 0.25rem 0.55rem; border-radius: 0.3rem; cursor: pointer;
  color: var(--cc-text); background: var(--cc-surface-2); border: 1px solid var(--cc-border);
}
.cohort-check-btn:hover:not(:disabled) { border-color: var(--cc-accent); }
.cohort-check-btn:disabled { opacity: 0.6; cursor: default; }
.cohort-check-btn .pi { font-size: 0.72rem; }
/* amber when the last check flagged an outlier — the cross-image detail is in the lab log */
.cohort-check-btn.warn { color: var(--cc-sev-warn); border-color: var(--cc-sev-warn); background: rgba(250, 178, 25, 0.1); }
.cohort-check-btn.warn:hover:not(:disabled) { border-color: var(--cc-sev-warn); }
</style>
