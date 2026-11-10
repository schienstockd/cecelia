<script setup lang="ts">
// The cohort check in the action bar's Checks cluster: are any images unlike the rest of the set, for
// the module's stage fun_names? The verdict is READ from the server (GET /api/qc/cohort/state — the
// persisted per-image findings + when it last ran), so it survives a reload; the button opens a popover
// with the flagged images, the run picker and "Check now" (POST /api/qc/cohort/check, which persists
// the findings and a [Cecelia — Cohort check] lab-log entry). Shown only for modules that bank cohort
// metrics (cohortFunsFor). See docs/todo/QC_OBSERVER_PLAN.md (A3).
import { computed, ref, watch } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useLabCaptureStore } from '../stores/labCapture'
import { useDataRefresh } from '../composables/useDataRefresh'
import { cohortFunsFor } from '../lib/cohortStages'
import { runCohortCheck, fetchCohortRuns, fetchCohortState } from '../lib/cohortCheck'
import { cohortCheckView, cohortFindingsOf, type CohortState } from '../lib/checks'
import { formatAgo } from '../utils/formatAgo'
import CheckButton from './CheckButton.vue'
import InlineNote from './InlineNote.vue'
import TeleportPopover from './TeleportPopover.vue'

// `funs` overrides the built-in COHORT_STAGES lookup — passed by pages whose cohort funs come from the
// backend (custom modules: the funNames the cohort routes can answer), so the button needs no hardcoded per-page entry.
const props = defineProps<{ module?: string; setUid?: string; funs?: string[] }>()
const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const labCapture = useLabCaptureStore()
const busy = ref(false)
const open = ref(false)
const btn = ref<InstanceType<typeof CheckButton> | null>(null)
const state = ref<CohortState | null>(null)

const funs = computed(() => props.funs?.length ? props.funs : cohortFunsFor(props.module))
const canCheck = computed(() => funs.value.length > 0 && !!props.setUid && !!projectMeta.current?.uid)
const setImages = computed(() => project.sets.find(s => s.uid === props.setUid)?.images ?? [])

// Clustering and HMM bank QC PER RUN (e.g. "movement"/"test"), so re-checking every past run is overkill.
// Discover the runs and let the user pick which to check; default to the newest. Funs that keep no
// runs (segment/tracking) return [] → no picker, checks as before. See cohortCheck.ts.
const runs = ref<string[]>([])            // distinct run suffixes across the page's cohort funs, newest first
const selectedRun = ref('')               // '' ⇒ check every value_name (no runs, or user picks "All runs")

async function load() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || !funs.value.length) { runs.value = []; state.value = null; return }
  const [lists, st] = await Promise.all([
    Promise.all(funs.value.map(f => fetchCohortRuns(projectUid, props.setUid!, f))),
    fetchCohortState(projectUid, props.setUid, funs.value),
  ])
  const seen = new Set<string>(); const merged: string[] = []
  for (const list of lists) for (const r of list) if (!seen.has(r.run)) { seen.add(r.run); merged.push(r.run) }
  runs.value = merged
  if (merged.length && !merged.includes(selectedRun.value)) selectedRun.value = merged[0]  // default: newest run
  state.value = st
}
watch([() => props.setUid, funs, () => projectMeta.current?.uid], load, { immediate: true })
// A task finishing on the set may bank a new run or re-check (a chain ends with a cohort check), so
// reload with the shared task-refresh framework (gated by autoRefreshOnTask, like the plots).
useDataRefresh(() => setImages.value.map(i => i.uid), load)

const view = computed(() => cohortCheckView(state.value, busy.value))
const checkedAt = computed(() => {
  const at = state.value?.checkedAt
  const ago = at != null ? formatAgo(new Date(at * 1000).toISOString()) : ''
  return ago === 'just now' ? ago : ago ? `${ago} ago` : ''
})
// the flagged images, each with its first cohort finding — read from the image's persisted QC
const flagged = computed(() => {
  const stepFuns = state.value?.funs.map(f => f.funName) ?? []
  return (state.value?.flaggedUids ?? []).map(uid => {
    const img = setImages.value.find(i => i.uid === uid)
    const fs = img ? cohortFindingsOf(img, stepFuns) : []
    return { uid, name: img?.name ?? uid, short: fs[0]?.short ?? '', long: fs.map(f => f.long).join('\n') }
  })
})

async function check() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || !props.setUid || busy.value) return
  busy.value = true
  try {
    const r = await runCohortCheck(projectUid, props.setUid, funs.value, selectedRun.value)
    // On a flag, the server appended a lab-log entry — reload an open panel (the server append doesn't
    // route through capture()'s tick). All cohort checks are set-scoped, so this covers every page.
    if (r.severity === 'warn') labCapture.notifyAppended()
    state.value = await fetchCohortState(projectUid, props.setUid, funs.value)
  } catch (e) {
    console.error('cohort check failed', e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <template v-if="canCheck">
    <CheckButton ref="btn" label="Cohort" :state="view.state" :count="view.count" :tip="view.tip"
                 :pressed="open" @click="open = !open" />
    <TeleportPopover v-model="open" :anchor="btn?.el ?? null" placement="bottom-end">
      <div class="cohort-pop cc-fs-sm">
        <span class="cc-muted cc-fs-xs">
          {{ state?.checked ? `Checked ${checkedAt}` : 'Not checked yet' }}
        </span>
        <InlineNote v-for="f in flagged" :key="f.uid" severity="warn"
                    :short="f.short ? `${f.name}: ${f.short}` : f.name" :detail="f.long || undefined" />
        <InlineNote v-if="state?.checked && !flagged.length" severity="ok"
                    :short="`All ${state.nIncluded} images within range`" />
        <div class="cc-row cohort-actions">
          <label v-if="runs.length" class="cc-row cc-muted cc-fs-xs" v-tooltip.bottom="'Which clustering run to check'">
            Run
            <select v-model="selectedRun" :disabled="busy">
              <option value="">All runs</option>
              <option v-for="r in runs" :key="r" :value="r">{{ r }}</option>
            </select>
          </label>
          <button class="cc-btn cc-btn-dense" :disabled="busy" @click="check"
                  v-tooltip.bottom="'Flag images unlike the rest of the set'">
            <i v-if="busy" class="pi pi-spin pi-spinner" /> Check now
          </button>
        </div>
      </div>
    </TeleportPopover>
  </template>
</template>

<style scoped>
.cohort-pop { display: flex; flex-direction: column; gap: var(--cc-space-xs); max-width: 22rem; }
.cohort-actions { justify-content: flex-end; }
</style>
