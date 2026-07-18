<script setup lang="ts">
// "Check cohort consistency" — runs cohort QC for the module's stage fun_names over the current set
// (POST /api/qc/cohort/check, which persists per-image outlier findings + a [Cecelia — Cohort check]
// lab-log entry with the cross-image detail). Shown only for modules that bank cohort metrics
// (cohortFunsFor). NO toast — the durable, detailed record is the lab log; the button just goes AMBER
// when the last run flagged something, pointing the user there. See docs/todo/QC_OBSERVER_PLAN.md (A3).
import { computed, ref, watch } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useLabCaptureStore } from '../stores/labCapture'
import { useDataRefresh } from '../composables/useDataRefresh'
import { cohortFunsFor } from '../lib/cohortStages'
import { runCohortCheck, fetchCohortRuns } from '../lib/cohortCheck'

// `funs` overrides the built-in COHORT_STAGES lookup — passed by pages whose cohort funs come from the
// backend (custom modules: funNames ∩ COHORT_METRICS), so the button needs no hardcoded per-page entry.
const props = defineProps<{ module?: string; setUid?: string; funs?: string[] }>()
const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const labCapture = useLabCaptureStore()
const busy = ref(false)
const foundWarnings = ref(false)   // last run flagged an outlier → amber + "see the lab log" tooltip

const funs = computed(() => props.funs?.length ? props.funs : cohortFunsFor(props.module))
const canCheck = computed(() => funs.value.length > 0 && !!props.setUid && !!projectMeta.current?.uid)

// Clustering banks QC PER RUN (e.g. "movement"/"test"), so re-checking every past run is overkill.
// Discover the runs and let the user pick which to check; default to the newest. Funs that keep no
// runs (segment/tracking/HMM) return [] → no selector, checks as before. See cohortCheck.ts.
const runs = ref<string[]>([])            // distinct run suffixes across the page's cohort funs, newest first
const selectedRun = ref('')               // '' ⇒ check every value_name (no runs, or user picks "All runs")
const showSelector = computed(() => runs.value.length >= 1)   // show the selector whenever there's a run to pick

async function loadRuns() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || !funs.value.length) { runs.value = []; return }
  const lists = await Promise.all(funs.value.map(f => fetchCohortRuns(projectUid, props.setUid!, f)))
  const seen = new Set<string>(); const merged: string[] = []
  for (const list of lists) for (const r of list) if (!seen.has(r.run)) { seen.add(r.run); merged.push(r.run) }
  runs.value = merged
  if (merged.length && !merged.includes(selectedRun.value)) selectedRun.value = merged[0]  // default: newest run
}
watch([() => props.setUid, funs, () => projectMeta.current?.uid], loadRuns, { immediate: true })
// A new clustering run banks a new run's QC, so refetch when a task finishes on any image in this set —
// otherwise the just-created run only appears after navigating away and back. Uses the shared
// task-refresh framework (gated by autoRefreshOnTask, like the plots).
const setImageUids = computed(() =>
  project.sets.find(s => s.uid === props.setUid)?.images.map(i => i.uid) ?? [])
useDataRefresh(() => setImageUids.value, loadRuns)

const tip = computed(() => foundWarnings.value
  ? 'Cohort check found warnings — check the lab log for details'
  : (showSelector.value
      ? 'Flag images whose output is an outlier vs the rest of the set (for the selected run)'
      : 'Flag images whose output is an outlier vs the rest of the set'))

async function check() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || busy.value) return
  busy.value = true
  try {
    const r = await runCohortCheck(projectUid, props.setUid, funs.value, selectedRun.value)
    // No toast: the cross-image detail is written to the [Cecelia — Cohort check] lab-log entry. The
    // button turns amber when something flagged so the user knows to look there; clears when clean.
    foundWarnings.value = r.severity === 'warn'
    // On a flag, the server appended a lab-log entry — reload an open panel (the server append doesn't
    // route through capture()'s tick). All cohort checks are set-scoped, so this covers every page.
    if (r.severity === 'warn') labCapture.notifyAppended()
  } catch (e) {
    console.error('cohort check failed', e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <span v-if="canCheck" class="cohort-check">
    <label v-if="showSelector" class="cohort-run" v-tooltip.bottom="'Which clustering run to check'">
      run:
      <select v-model="selectedRun" :disabled="busy">
        <option value="">All runs</option>
        <option v-for="r in runs" :key="r" :value="r">{{ r }}</option>
      </select>
    </label>
    <button class="cohort-check-btn" :class="{ warn: foundWarnings }" :disabled="busy"
            @click="check" v-tooltip.bottom="tip">
      <i :class="['pi', busy ? 'pi-spin pi-spinner' : (foundWarnings ? 'pi-exclamation-triangle' : 'pi-chart-bar')]" />
      {{ busy ? 'Checking…' : 'Check cohort' }}
    </button>
  </span>
</template>

<style scoped>
.cohort-check { display: inline-flex; align-items: center; gap: 0.35rem; }
.cohort-run {
  display: inline-flex; align-items: center; gap: 0.25rem;
  font-size: 0.72rem; color: var(--cc-text-muted, var(--cc-text));
}
.cohort-run select {
  font-size: 0.72rem; padding: 0.15rem 0.3rem; border-radius: 0.3rem;
  color: var(--cc-text); background: var(--cc-surface-2); border: 1px solid var(--cc-border); cursor: pointer;
}
.cohort-run select:disabled { opacity: 0.6; cursor: default; }
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
