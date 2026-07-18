<script setup lang="ts">
// "Check cohort consistency" — runs cohort QC for the module's stage fun_names over the current set
// (POST /api/qc/cohort/check, which persists per-image outlier findings + a [Cecelia] lab-log line).
// Shown only for modules that bank cohort metrics (cohortFunsFor). Feedback is a toast; the durable
// record is the lab log. See docs/todo/QC_OBSERVER_PLAN.md (A3) + the toast convention in INVENTORY.md.
import { computed, ref } from 'vue'
import { useToast } from 'primevue/usetoast'
import { useProjectMetaStore } from '../stores/projectMeta'
import { cohortFunsFor } from '../lib/cohortStages'
import { runCohortCheck } from '../lib/cohortCheck'
import { SEVERITY } from '../lib/severity'

const props = defineProps<{ module?: string; setUid?: string }>()
const toast = useToast()
const projectMeta = useProjectMetaStore()
const busy = ref(false)

const funs = computed(() => cohortFunsFor(props.module))
const canCheck = computed(() => funs.value.length > 0 && !!props.setUid && !!projectMeta.current?.uid)

async function check() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || busy.value) return
  busy.value = true
  toast.add({ severity: 'info', summary: 'Cohort QC', detail: 'Checking cohort consistency…', life: 2000 })
  try {
    const r = await runCohortCheck(projectUid, props.setUid, funs.value)
    // severity maps to the traffic-light scale; 'ok'→success, 'warn'→warn (PrimeVue severities)
    toast.add({ severity: r.severity === 'ok' ? 'success' : 'warn',
                summary: `${SEVERITY[r.severity].emoji} Cohort QC`, detail: r.message, life: 4000 })
  } catch {
    toast.add({ severity: 'error', summary: 'Cohort QC', detail: 'Check failed — see console.', life: 4000 })
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <button v-if="canCheck" class="cohort-check-btn" :disabled="busy" @click="check"
          v-tooltip.bottom="'Flag images whose output is an outlier vs the rest of the set'">
    <i :class="['pi', busy ? 'pi-spin pi-spinner' : 'pi-chart-bar']" />
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
</style>
