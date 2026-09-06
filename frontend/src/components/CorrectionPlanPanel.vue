<!--
  Correction-plan preview — slice 3a of docs/todo/CORRECTION_QC_PLAN.md.

  Passive read-only view of the recommended plan for one image. Given a single selected image UID,
  POSTs /api/correction-plan/recommend and renders the included steps (what will run) + the excluded
  ones (with the human reason the plan dropped them, so a user knows the plan considered them).

  What this slice does NOT do — deferred to 3b/c/d:
  - card picker (accept a different card)
  - wizard W1–W6
  - save/load plan.json round-trip
  - "Mount to chain" button

  Placement: sits above the TaskRunner in the cleanup module's right panel; multiple selection or
  no selection shows an empty state, so the panel is unobtrusive when the plan is not relevant.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import CollapsibleSection from './CollapsibleSection.vue'
import { useCorrectionPlan, fetchCorrectionPresets } from '../composables/useCorrectionPlan'
import type { AcquisitionPresetSummary, CorrectionStep, QCScore } from '../types/correctionPlan'
import { useProjectStore } from '../stores/project'

const props = defineProps<{
  selectedUids: string[]
}>()

const project = useProjectStore()
const projectUid = computed(() => project.loadedProjectUid ?? '')

// Slice 3a is per-image. Multi-select shows an empty-state row rather than fanning out — cohort
// plans are §6 of the plan doc, deferred.
const imageUid = computed(() => props.selectedUids.length === 1 ? props.selectedUids[0] : null)

const { plan, loading, error, reload } = useCorrectionPlan({
  projectUid,
  imageUid,
})

// Presets are a constant registry; one fetch per session. Displayed as `${preset.name}` next to the
// raw id so the row reads "Card: Resonance / photon-limited" instead of "Card: resonance".
const presets = ref<AcquisitionPresetSummary[]>([])
fetchCorrectionPresets().then(rows => { presets.value = rows }).catch(() => { /* fall back to id */ })
const presetName = computed(() => {
  const id = plan.value?.presetId
  if (!id) return ''
  return presets.value.find(p => p.id === id)?.name ?? id
})

function shortFn(fn: string): string {
  const i = fn.lastIndexOf('.')
  return i === -1 ? fn : fn.slice(i + 1)
}

function paramSummary(step: CorrectionStep): string {
  const keys = Object.keys(step.params)
  if (!keys.length) return ''
  return keys.map(k => `${k}=${JSON.stringify(step.params[k])}`).join(' · ')
}

function scoreDisplay(s: QCScore): string {
  if (s.score === null) return '—'
  return s.score.toFixed(2)
}
</script>

<template>
  <div class="correction-plan-panel">
    <div class="header">
      <span class="cc-eyebrow cc-fs-sm">Correction plan</span>
      <button
        class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
        :disabled="loading || !imageUid"
        @click="reload"
        v-tooltip.left="'Recompute the recommended plan'">
        <i class="pi pi-refresh" />
      </button>
    </div>

    <div v-if="!imageUid" class="empty cc-muted cc-fs-sm">
      {{ selectedUids.length === 0 ? 'Select one image to see its plan' : 'Select just one image' }}
    </div>

    <div v-else-if="loading && !plan" class="empty cc-muted cc-fs-sm">Recommending…</div>

    <div v-else-if="error" class="empty cc-fs-sm error-msg">{{ error }}</div>

    <template v-else-if="plan">
      <div class="card-row cc-fs-sm">
        <span class="cc-muted">Card:</span>
        <span>{{ presetName }}</span>
      </div>

      <div class="section-label cc-eyebrow cc-fs-2xs">Will run ({{ plan.included.length }})</div>
      <div v-if="!plan.included.length" class="empty cc-muted cc-fs-sm">No steps — image has no T axis and no card override.</div>
      <ul v-else class="steps">
        <li v-for="step in plan.included" :key="`inc-${step.funName}`" class="step">
          <div class="step-head">
            <span class="fn">{{ shortFn(step.funName) }}</span>
            <span class="weight cc-muted cc-fs-2xs" v-tooltip.right="`Bucket ${step.orderWeight} — plan sort order`">{{ step.orderWeight }}</span>
            <span class="source cc-fs-2xs" v-tooltip.right="`Source: ${step.source}`">{{ step.source }}</span>
          </div>
          <div v-if="paramSummary(step)" class="params cc-muted cc-fs-2xs">{{ paramSummary(step) }}</div>
        </li>
      </ul>

      <CollapsibleSection
        v-if="plan.excluded.length"
        :label="`Excluded (${plan.excluded.length})`"
        tip="Steps the plan considered and dropped, with the reason"
        :default-open="false"
        max-height="240px">
        <ul class="steps">
          <li v-for="step in plan.excluded" :key="`exc-${step.funName}`" class="step excluded">
            <div class="step-head">
              <span class="fn">{{ shortFn(step.funName) }}</span>
            </div>
            <div v-if="step.exclusionReason" class="reason cc-muted cc-fs-2xs">{{ step.exclusionReason }}</div>
          </li>
        </ul>
      </CollapsibleSection>

      <CollapsibleSection
        v-if="plan.qcScores.length"
        :label="`QC scores (${plan.qcScores.length})`"
        tip="Metadata-derived scores that fed the rule engine"
        :default-open="false"
        max-height="240px">
        <ul class="scores">
          <li v-for="s in plan.qcScores" :key="s.metric" class="score-row cc-fs-2xs">
            <span class="metric">{{ s.metric }}</span>
            <span class="score-val">{{ scoreDisplay(s) }}</span>
          </li>
        </ul>
      </CollapsibleSection>
    </template>
  </div>
</template>

<style scoped>
.correction-plan-panel {
  display: flex;
  flex-direction: column;
  gap: 6px;
  padding: 8px 10px;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  background: var(--cc-surface-1);
}
.header {
  display: flex;
  align-items: center;
  justify-content: space-between;
}
.empty {
  padding: 4px 0;
}
.error-msg {
  color: var(--cc-danger);
}
.card-row {
  display: flex;
  gap: 6px;
  align-items: baseline;
}
.section-label {
  margin-top: 4px;
}
.steps, .scores {
  list-style: none;
  padding: 0;
  margin: 0;
  display: flex;
  flex-direction: column;
  gap: 4px;
}
.step {
  display: flex;
  flex-direction: column;
  gap: 2px;
}
.step-head {
  display: flex;
  gap: 6px;
  align-items: baseline;
}
.step.excluded .fn {
  color: var(--cc-text-dim);
  text-decoration: line-through;
}
.fn {
  font-weight: 500;
}
.source {
  padding: 0 4px;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs);
  color: var(--cc-text-dim);
}
.params, .reason {
  padding-left: 4px;
}
.score-row {
  display: flex;
  justify-content: space-between;
  gap: 6px;
}
.score-val {
  font-variant-numeric: tabular-nums;
}
</style>
