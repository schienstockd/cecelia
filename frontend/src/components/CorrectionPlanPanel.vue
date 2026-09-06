<!--
  Correction-plan preview + card picker — slices 3a + 3b of docs/todo/CORRECTION_QC_PLAN.md.

  For one selected image: shows the currently-recommended (or saved) plan and lets the user pick a
  different acquisition card. Load-first behaviour: if plan.json exists on disk it's the source of
  truth (that's what the executor would run today); otherwise a fresh recommend is shown as
  "unsaved". Picking a card saves plan.json in one round-trip. A `stale` marker fires when the
  image's `saturationFingerprint` no longer matches the sidecar (a re-import happened).

  What this slice does NOT do — deferred to 3c/d:
  - wizard W1–W6 (would let the user answer questions the card can't imply)
  - "Mount to chain" button (write a ChainTemplate from the plan)

  Placement: sits above the TaskRunner in the cleanup module's right panel; multiple selection or
  no selection shows an empty state, so the panel is unobtrusive when the plan is not relevant.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import CollapsibleSection from './CollapsibleSection.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
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

const { plan, saved, stale, loading, error, save, refresh } = useCorrectionPlan({
  projectUid,
  imageUid,
})

const presets = ref<AcquisitionPresetSummary[]>([])
fetchCorrectionPresets().then(rows => { presets.value = rows }).catch(() => { /* fall back to id */ })

// Card picker options — `custom` last so it reads as the fallback rather than an active choice; the
// natural order for the four opinionated cards is the enum W1 offers.
const CARD_ORDER = ['resonance', 'galvo', 'spinning_disk', 'deep_3d', 'custom']
const cardOptions = computed<ChipOption[]>(() =>
  CARD_ORDER
    .map(id => presets.value.find(p => p.id === id))
    .filter((p): p is AcquisitionPresetSummary => p !== undefined)
    .map(p => ({ value: p.id, label: p.name.split(' /')[0], tip: p.description }))
)

async function pickCard(newId: string): Promise<void> {
  if (!newId || newId === plan.value?.presetId) return
  await save(newId, plan.value?.wizardAnswers ?? {})
}

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

// Rename raw `source` symbols to something a first-time reader can decode. The plan engine emits
// `card` / `wizard` / `computed_qc` / `rule_default` / `user_edit`; keep those on the wire (audit
// trail), map to friendly labels here.
const SOURCE_LABEL: Record<string, string> = {
  card:          'card',
  wizard:        'you',
  computed_qc:   'auto',
  rule_default:  'default',
  user_edit:     'edit',
}
function sourceLabel(s: string): string { return SOURCE_LABEL[s] ?? s }
</script>

<template>
  <div class="correction-plan-panel">
    <div class="header">
      <span class="cc-eyebrow cc-fs-sm">Correction plan</span>
      <span class="header-right cc-fs-2xs">
        <span v-if="saved" class="status-tag saved" v-tooltip.left="'Loaded from plan.json — the executor runs this'">saved</span>
        <span v-else-if="plan" class="status-tag unsaved" v-tooltip.left="'Not saved yet — Select a card to persist'">unsaved</span>
        <span v-if="stale" class="status-tag stale" v-tooltip.left="'Meta changed since save — Select a card to re-save'">stale</span>
        <button
          class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
          :disabled="loading || !imageUid"
          @click="refresh"
          v-tooltip.left="saved ? 'Reload plan.json' : 'Recompute the recommended plan'">
          <i class="pi pi-refresh" />
        </button>
      </span>
    </div>

    <div v-if="!imageUid" class="empty cc-muted cc-fs-sm">
      {{ selectedUids.length === 0 ? 'Select one image to see its plan' : 'Select just one image' }}
    </div>

    <div v-else-if="loading && !plan" class="empty cc-muted cc-fs-sm">Recommending…</div>

    <div v-else-if="error" class="empty cc-fs-sm error-msg">{{ error }}</div>

    <template v-else-if="plan">
      <div class="cc-row cc-fs-sm">
        <span class="cc-muted">Card:</span>
        <ChipSelect
          v-if="cardOptions.length"
          variant="pill"
          :options="cardOptions"
          :model-value="plan.presetId"
          @update:model-value="v => pickCard(String(v ?? ''))"
        />
      </div>

      <div class="section-label cc-eyebrow cc-fs-2xs">Will run ({{ plan.included.length }})</div>
      <div v-if="!plan.included.length" class="empty cc-muted cc-fs-sm">No steps — image has no T axis and this card seeds none.</div>
      <ul v-else class="steps">
        <li v-for="step in plan.included" :key="`inc-${step.funName}`" class="step">
          <div class="step-head">
            <span class="fn">{{ shortFn(step.funName) }}</span>
            <span class="weight cc-muted cc-fs-2xs" v-tooltip.right="`Bucket ${step.orderWeight} — plan sort order`">{{ step.orderWeight }}</span>
            <span class="source cc-fs-2xs" v-tooltip.right="`Source: ${step.source}`">{{ sourceLabel(step.source) }}</span>
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
.header-right {
  display: flex;
  align-items: center;
  gap: 6px;
}
.status-tag {
  padding: 0 4px;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs);
  color: var(--cc-text-dim);
}
.status-tag.saved {
  color: var(--cc-text);
  border-color: var(--cc-accent, var(--cc-border));
}
.status-tag.stale {
  color: var(--cc-danger);
  border-color: var(--cc-danger);
}
.empty {
  padding: 4px 0;
}
.error-msg {
  color: var(--cc-danger);
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
