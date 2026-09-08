<!--
  Correction-plan preview + card picker + wizard + mount — slices 3a + 3b + 3c + 3d of
  docs/todo/CORRECTION_QC_PLAN.md.

  For one selected image: shows the currently-recommended (or saved) plan and lets the user pick a
  different acquisition card. Load-first behaviour: if plan.json exists on disk it's the source of
  truth (that's what the executor would run today); otherwise a fresh recommend is shown as
  "unsaved". Picking a card saves plan.json in one round-trip. A `stale` marker fires when the
  image's `saturationFingerprint` no longer matches the sidecar (a re-import happened).

  The card row is a `CorrectionCardPicker` (single row of image-face tiles with a Custom escape
  row below) rather than a pill list, because the card names ("Resonance", "Galvo") are
  scanner-first while users think image-first — recognition beats recall. Figures live in
  `tasks/cardVis.ts` (pure module, testable).

  Wizard section (slice 3c) exposes the three enum questions from §4 of the plan doc that overlay a
  card independently — W2 (stage rotated → sitkRigid), W3 (frame-to-frame warp → include
  flowRegister), W5 (intra-stack Z drift → include stackAlign). Each answer immediately re-saves the
  plan the same way a card pick does. W1 is intentionally NOT here — the card picker IS W1. W4 and
  W6 were retired (PR #835): the per-channel exclusive question already lives on the afCorrect task
  widget, and cellpose denoising was dropped for SUPPORT.

  Mount writes the saved plan as a ChainTemplate under the project's chains dir (name is fixed
  per-image, `correction-plan-{imageUid}`). It requires a saved plan first — mounting an unsaved
  recommendation would create a chain whose provenance can't be traced back to a card the user
  actually picked. Re-mounting overwrites, gated by a two-step confirm so accidentally clobbering a
  hand-edited chain never happens silently.

  Placement: opened as a FLOATING panel from the icon in the TaskRunner's pane bar (parallel to the
  Viewer / Lab log launchers on the app header, but module-scoped). The parent owns `open`; this
  panel emits `close`. When floating and unopened, the component renders nothing — no ghost row in
  the sidebar layout. Multiple selection or no selection shows an empty state inside the float, so a
  stale open panel doesn't fabricate a plan for the wrong image.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import CollapsibleSection from './CollapsibleSection.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import CorrectionCardPicker from './CorrectionCardPicker.vue'
import FloatingPanel from './FloatingPanel.vue'
import { useCorrectionPlan, fetchCorrectionPresets } from '../composables/useCorrectionPlan'
import type { AcquisitionPresetSummary, CorrectionStep, QCScore } from '../types/correctionPlan'
import { useProjectStore } from '../stores/project'

const props = defineProps<{
  selectedUids: string[]
}>()

const emit = defineEmits<{ close: [] }>()

const project = useProjectStore()
const projectUid = computed(() => project.loadedProjectUid ?? '')

// Slice 3a is per-image. Multi-select shows an empty-state row rather than fanning out — cohort
// plans are §6 of the plan doc, deferred.
const imageUid = computed(() => props.selectedUids.length === 1 ? props.selectedUids[0] : null)

const { plan, saved, stale, loading, error, save, refresh, mount } = useCorrectionPlan({
  projectUid,
  imageUid,
})

// Mount state: 'idle' → 'busy' → back to 'idle' with a status message; a 409 shifts to
// 'confirmOverwrite' which shows Replace/Cancel inline (no modal). 'done' holds the last outcome
// long enough to notice, cleared when the user picks a different card or navigates images.
type MountState = 'idle' | 'busy' | 'confirmOverwrite' | 'done'
const mountState = ref<MountState>('idle')
const mountMsg = ref<string>('')
async function attemptMount(overwrite: boolean): Promise<void> {
  mountState.value = 'busy'
  mountMsg.value = ''
  try {
    const r = await mount(overwrite)
    if (r.status === 'conflict') {
      mountState.value = 'confirmOverwrite'
      mountMsg.value = `Chain '${r.name}' already exists`
      return
    }
    mountState.value = 'done'
    mountMsg.value = r.status === 'created'
      ? `Mounted → ${r.name} (${r.nodeCount} node${r.nodeCount === 1 ? '' : 's'})`
      : `Replaced → ${r.name} (${r.nodeCount} node${r.nodeCount === 1 ? '' : 's'})`
  } catch (e) {
    mountState.value = 'done'
    mountMsg.value = e instanceof Error ? e.message : String(e)
  }
}
function cancelMount(): void { mountState.value = 'idle'; mountMsg.value = '' }
// Clear the outcome banner when the underlying plan changes — a stale message on the previous card
// would misrepresent what the button will actually do next.
watch([() => plan.value?.presetId, imageUid], () => { mountState.value = 'idle'; mountMsg.value = '' })

const presets = ref<AcquisitionPresetSummary[]>([])
fetchCorrectionPresets().then(rows => { presets.value = rows }).catch(() => { /* fall back to id */ })

// Card picker options — `custom` last so it reads as the fallback rather than an active choice; the
// natural order for the four opinionated cards is the enum W1 offers.
const CARD_ORDER = ['resonance', 'galvo', 'spinning_disk', 'deep_3d', 'custom']
const orderedPresets = computed<AcquisitionPresetSummary[]>(() =>
  CARD_ORDER
    .map(id => presets.value.find(p => p.id === id))
    .filter((p): p is AcquisitionPresetSummary => p !== undefined)
)

async function pickCard(newId: string): Promise<void> {
  if (!newId || newId === plan.value?.presetId) return
  await save(newId, plan.value?.wizardAnswers ?? {})
}

// Wizard — §4 of the plan doc. Only the three enum questions that overlay a card independently are
// exposed here; the picker is W1, and W4/W6 are conditional/rare (see the top-of-file comment).
// Options end with `unknown` so the neutral state is the last chip (Fitts-friendly opt-out) and the
// `custom` chip pattern in the card row reads the same way. Tooltips are one short line.
interface WizardQ { key: string; label: string; tip: string; options: ChipOption[] }
const YNU: ChipOption[] = [
  { value: 'no',      label: 'no' },
  { value: 'yes',     label: 'yes' },
  { value: 'unknown', label: 'unknown' },
]
const WIZARD_QUESTIONS: WizardQ[] = [
  { key: 'W2', label: 'Stage rotated?',    tip: 'Yes → drift uses rigid alignment (translation + rotation)', options: YNU },
  { key: 'W3', label: 'Frame-to-frame warp?', tip: 'Yes → include flowRegister for non-rigid deformation',  options: YNU },
  { key: 'W5', label: 'Z-plane breathing?', tip: 'Yes → include stackAlign for intra-stack sample motion',   options: YNU },
]
function wizardValue(key: string): string {
  return String(plan.value?.wizardAnswers?.[key] ?? 'unknown')
}
async function pickWizard(key: string, value: string): Promise<void> {
  const current = wizardValue(key)
  if (value === current) return
  // 'unknown' clears the answer so the plan behaves as if the question was never answered — the
  // engine reads a missing key the same as :unknown, and dropping it keeps plan.json tidy.
  const next: Record<string, string> = { ...(plan.value?.wizardAnswers ?? {}) }
  if (value === 'unknown') delete next[key]
  else                     next[key] = value
  await save(plan.value?.presetId ?? 'custom', next)
}
const wizardAnswered = computed<number>(() => {
  const w = plan.value?.wizardAnswers
  if (!w) return 0
  let n = 0
  for (const q of WIZARD_QUESTIONS) if (w[q.key] && w[q.key] !== 'unknown') n++
  return n
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
  <FloatingPanel
    title="Correction plan"
    icon="pi-clipboard"
    storage-key="correction-plan"
    :default-w="360"
    :default-h="520"
    @close="emit('close')"
  >
  <div class="correction-plan-panel">
    <div class="status-row cc-row cc-fs-2xs">
      <span v-if="saved" class="status-tag saved" v-tooltip.right="'Loaded from plan.json — the executor runs this'">saved</span>
      <span v-else-if="plan" class="status-tag unsaved" v-tooltip.right="'Not saved yet — Select a card to persist'">unsaved</span>
      <span v-if="stale" class="status-tag stale" v-tooltip.right="'Meta changed since save — Select a card to re-save'">stale</span>
      <span class="status-spacer" />
      <button
        class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
        :disabled="loading || !imageUid"
        @click="refresh"
        v-tooltip.left="saved ? 'Reload plan.json' : 'Recompute the recommended plan'">
        <i class="pi pi-refresh" />
      </button>
    </div>

    <div v-if="!imageUid" class="empty cc-muted cc-fs-sm">
      {{ selectedUids.length === 0 ? 'Select one image to see its plan' : 'Select just one image' }}
    </div>

    <div v-else-if="loading && !plan" class="empty cc-muted cc-fs-sm">Recommending…</div>

    <div v-else-if="error" class="empty cc-fs-sm error-msg">{{ error }}</div>

    <template v-else-if="plan">
      <CorrectionCardPicker
        v-if="orderedPresets.length"
        :presets="orderedPresets"
        :model-value="plan.presetId"
        :disabled="loading"
        @update:model-value="v => pickCard(v)"
      />

      <CollapsibleSection
        :label="`Wizard${wizardAnswered ? ' (' + wizardAnswered + ')' : ''}`"
        tip="Overlay a card with three yes/no specimen answers"
        :default-open="wizardAnswered > 0"
        max-height="240px">
        <div class="wizard">
          <div v-for="q in WIZARD_QUESTIONS" :key="q.key" class="wizard-row cc-fs-sm">
            <span class="wizard-label cc-muted" v-tooltip.top="q.tip">{{ q.label }}</span>
            <ChipSelect
              variant="pill"
              :options="q.options"
              :model-value="wizardValue(q.key)"
              @update:model-value="v => pickWizard(q.key, String(v ?? 'unknown'))"
              :aria-label="q.label"
            />
          </div>
        </div>
      </CollapsibleSection>

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

      <div class="mount-row cc-row cc-fs-sm">
        <template v-if="mountState !== 'confirmOverwrite'">
          <button
            class="cc-btn cc-btn-ghost cc-btn-sm"
            :disabled="!saved || mountState === 'busy' || !plan.included.length"
            @click="attemptMount(false)"
            v-tooltip.top="!saved
              ? 'Save the plan first — Select a card'
              : (!plan.included.length ? 'Nothing to mount — the plan has no included steps' : 'Write this plan as a chain template')">
            Mount to chain
          </button>
        </template>
        <template v-else>
          <button
            class="cc-btn cc-btn-danger cc-btn-sm"
            @click="attemptMount(true)"
            v-tooltip.top="'Overwrite the existing chain'">
            Replace
          </button>
          <button
            class="cc-btn cc-btn-ghost cc-btn-sm"
            @click="cancelMount">
            Cancel
          </button>
        </template>
        <span v-if="mountMsg" class="mount-msg cc-fs-2xs" :class="{ warn: mountState === 'confirmOverwrite' }">{{ mountMsg }}</span>
      </div>
    </template>
  </div>
  </FloatingPanel>
</template>

<style scoped>
.correction-plan-panel {
  display: flex;
  flex-direction: column;
  gap: 6px;
  padding: 8px 10px;
}
.status-row {
  align-items: center;
}
.status-spacer {
  flex: 1 1 auto;
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
.mount-row {
  margin-top: 4px;
  padding-top: 6px;
  border-top: 1px dashed var(--cc-border);
}
.mount-msg {
  color: var(--cc-text-dim);
}
.mount-msg.warn {
  color: var(--cc-danger);
}
.wizard {
  display: flex;
  flex-direction: column;
  gap: 4px;
}
.wizard-row {
  display: flex;
  align-items: center;
  gap: 8px;
}
.wizard-label {
  flex: 0 0 auto;
  min-width: 130px;
}
</style>
