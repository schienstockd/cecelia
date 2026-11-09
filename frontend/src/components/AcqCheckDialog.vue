<script setup lang="ts">
// The set-level acquisition check (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the set's plan, what is
// off across the set, and a per-movie table grouped by label design. Runs live on open (nothing
// written); saving the plan writes the set's Blackboard entry. "Review" runs one Kiwi turn on the saved
// plan (api/src/acq_review.jl); its points land on that entry, marked there Good / Bad / Unsure.
// Data: utils/acqCheckApi.ts; logic: utils/acqCheck.ts; rules: app/src/acq_check.jl.
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import AcqPlanForm from './AcqPlanForm.vue'
import ImageMetadataDialog from './ImageMetadataDialog.vue'
import { useProjectStore } from '../stores/project'
import { useToast } from '../composables/useToast'
import { fetchAcqCheck, reviewAcqCheck, saveAcqPlan, type AcqCheckResult, type AcqPlan, type AcqRow } from '../utils/acqCheckApi'
import { brightZText, measuringCount, emptyPlan, findingImageUids, findingLines, findingSeverity, findingsFor, fmt, planSentence, reviewBlocked, rowsByDesign } from '../utils/acqCheck'
import { stepLabel } from '../utils/kiwiTurn'
import ModelReviewPoints from './ModelReviewPoints.vue'
import { useKiwiStore } from '../stores/kiwi'
import { useObserverStore } from '../stores/observer'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { MEASURE_FUN, submitMeasure } from '../utils/acqMeasureRun'
import { useTaskStore } from '../stores/tasks'
import InlineNote from './InlineNote.vue'

const props = defineProps<{ projectUid: string; setUid: string; setName?: string }>()
const emit = defineEmits<{ close: []; changed: [AcqCheckResult] }>()
const project = useProjectStore()
const toast = useToast()
const taskStore = useTaskStore()
const router = useRouter()
const observer = useObserverStore()   // the AI switch: the review is a Kiwi turn
const settings = useSettingsStore()   // …on the model picked for Kiwi
const ws = useWsStore()
const kiwi = useKiwiStore()

const result = ref<AcqCheckResult | null>(null)
const loading = ref(true)
const editing = ref(false)
const saving = ref(false)
const openRow = ref<string | null>(null)   // the movie whose findings are shown under its table
const metaUid = ref<string | null>(null)

async function load() {
  loading.value = true
  result.value = await fetchAcqCheck(props.projectUid, props.setUid)
  loading.value = false
  if (result.value) emit('changed', result.value)
}
onMounted(load)

const plan = computed(() => result.value?.plan ?? null)
const formPlan = computed<AcqPlan>(() => plan.value ?? emptyPlan(result.value?.suggestedDesigns ?? []))
// the designs the form picks channels from: the suggestions carry each design's channels
const formDesigns = computed(() => result.value?.suggestedDesigns ?? [])

async function save(p: AcqPlan) {
  saving.value = true
  const r = await saveAcqPlan(props.projectUid, props.setUid, p)
  saving.value = false
  if (!r) { toast.add({ severity: 'error', life: 5000, summary: 'Couldn’t save the plan' }); return }
  result.value = r; editing.value = false
  emit('changed', r)
}

const setFindings = computed(() => (result.value ? findingsFor(result.value, 'set') : []))

// Measure the movies the probe hasn't read yet: background tasks (io pool), one per movie. The button
// follows the task store, so it stays busy across a closed and reopened modal; the check reloads once
// the last one finishes.
const unmeasured = computed(() => findingImageUids(result.value?.findings.find(f => f.code === 'acq.not_measured')))
const measureLeft = computed(() => measuringCount(taskStore.tasks, MEASURE_FUN, unmeasured.value))
const measuring = computed(() => measureLeft.value > 0)
function measure() { if (!measuring.value) submitMeasure(props.projectUid, props.setUid, unmeasured.value) }
watch(measuring, (now, was) => { if (was && !now) load() })
const groups = computed(() => (result.value ? rowsByDesign(result.value) : []))

const objectNames = computed(() => plan.value?.objects.map(o => o.name) ?? [])
const columns = computed<SelectionColumn[]>(() => [
  { key: 'name', label: 'Movie', ellipsis: true, width: 220 },
  { key: 'lasers', label: 'Lasers', width: 150 },
  { key: 'brightZ', label: 'Bright Z', width: 60, tip: 'Off · ramp saved: a ramp was set but not applied' },
  ...objectNames.value.map(n => ({ key: `o:${n}`, label: n, width: 55, tip: `${n}: p99 photons on its cells` })),
  ...(objectNames.value.length > 1 ? [{ key: 'ratio', label: 'Balance', width: 60, tip: 'Brightest ÷ dimmest object (p99 photons)' }] : []),
  { key: 'sharedR', label: 'Shared r', width: 60, tip: 'Highest structure r between a plan channel and another' },
  { key: 'drift', label: 'Drift', width: 60, tip: 'Largest XY drift over the movie' },
])
// the table renders strings: one display row per movie
const display = (r: AcqRow) => ({
  ...r,
  brightZ: brightZText(r),
  ratio: r.ratio === null ? '—' : `${fmt(r.ratio, 1)}×`,
  sharedR: fmt(r.sharedR, 2),
  drift: r.driftUm === null ? '—' : `${fmt(r.driftUm)} µm${r.registers === false ? ' ⚠' : ''}`,
  ...Object.fromEntries(objectNames.value.map(n => [`o:${n}`, fmt(r.objects[n]?.p99 ?? null)])),
})
const rowFindings = (uid: string) => (result.value ? findingsFor(result.value, uid) : [])
// The model review: one background Kiwi turn on the saved plan. Its steps arrive as `kiwi:step`. The Kiwi
// store follows it to the end (`kiwi:done`, and a reload after a reconnect), and the entry is written
// before `kiwi:done` is sent — so once the turn is in the store's feed, a reload carries the review.
const reviewTurn = ref('')     // the running review turn ('' = none)
const reviewStep = ref('')
const reviewTip = computed(() => reviewBlocked({ ai: observer.ai, hasPlan: !!plan.value && !editing.value, running: !!reviewTurn.value }))
async function review() {
  if (reviewTip.value) return
  reviewStep.value = ''
  const tid = await reviewAcqCheck(props.projectUid, props.setUid, settings.kiwiModel)
  if (!tid) { toast.add({ severity: 'warn', life: 5000, summary: 'Kiwi is busy — try again when it’s done' }); return }
  reviewTurn.value = tid
}
const onStep = (d: Record<string, unknown>) => { if (d.turnId === reviewTurn.value) reviewStep.value = stepLabel(String(d.step ?? '')) }
watch(() => kiwi.turns, ts => {
  if (!reviewTurn.value || !ts.some(t => t.turnId === reviewTurn.value)) return
  reviewTurn.value = ''
  void load()
})
onMounted(() => ws.on('kiwi:step', onStep))
onUnmounted(() => ws.off('kiwi:step', onStep))
function openEntry() {
  if (result.value?.entryId) void router.push({ path: '/blackboard', query: { entry: result.value.entryId } })
}

const metaImage = computed(() => {
  for (const s of project.sets) { const i = s.images.find(x => x.uid === metaUid.value); if (i) return i }
  return null
})
</script>

<template>
  <BaseModal width="1000px" @close="emit('close')">
    <template #title><i class="pi pi-sliders-h" /> Acquisition check · {{ setName ?? setUid }}</template>

    <div v-if="loading" class="cc-empty-inline"><i class="pi pi-spin pi-spinner" /> Checking…</div>
    <div v-else-if="!result" class="cc-empty-inline">Couldn’t run the check.</div>
    <div v-else class="acq">
      <div class="plan cc-row">
        <template v-if="!editing">
          <span class="cc-muted cc-fs-xs">Plan</span>
          <q v-if="plan">{{ plan.text || planSentence(plan) }}</q>
          <span v-else class="cc-muted">None yet — photons, balance and shared structure need one</span>
          <span class="cc-muted cc-fs-xs">{{ result.nMeasured }}/{{ result.nImages }} measured
            <template v-if="result.round > 1"> · round {{ result.round }}</template></span>
          <button class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs edit" @click="editing = true">
            {{ plan ? 'Edit' : 'Set a plan' }}
          </button>
        </template>
        <AcqPlanForm v-else class="form" :plan="formPlan" :designs="formDesigns" :saving="saving"
                     @save="save" @cancel="editing = false" />
      </div>

      <div class="cc-eyebrow cc-fs-2xs">Across the set</div>
      <ul class="findings">
        <li v-for="(f, i) in setFindings" :key="i">
          <InlineNote :short="f.short" :severity="findingSeverity(f)" :lines="findingLines(f)">
            <button v-if="f.code === 'acq.not_measured' && unmeasured.length" class="cc-btn cc-btn-dense cc-fs-xs"
                    :disabled="measuring" @click="measure"
                    v-tooltip.top="'Runs in the background, about a minute per movie — progress in the task manager'">
              <i v-if="measuring" class="pi pi-spin pi-spinner" />
              {{ measuring ? `Measuring… ${measureLeft} left` : `Measure ${unmeasured.length} movie${unmeasured.length === 1 ? '' : 's'}` }}
            </button>
          </InlineNote>
        </li>
        <li v-if="!setFindings.length" class="cc-muted">Nothing across the set.</li>
      </ul>

      <div class="cc-eyebrow cc-fs-2xs cc-row review-head">
        <span>Review</span>
        <span class="cc-muted" v-tooltip.top="'Kiwi’s model reads the plan, the movies and acquisition knowledge — not your notes'">
          · {{ settings.kiwiModel }}, about a minute</span>
        <button class="cc-btn cc-btn-dense cc-fs-xs review-btn" type="button" :disabled="!!reviewTip"
                v-tooltip.top="reviewTip || 'What to change before acquiring the rest'" @click="review">
          <i v-if="reviewTurn" class="pi pi-spin pi-spinner" />
          {{ reviewTurn ? 'Reviewing…' : result.review ? 'Run again' : 'Review' }}
        </button>
      </div>
      <div v-if="reviewTurn" class="cc-muted cc-fs-xs">{{ reviewStep || 'starting' }}</div>
      <template v-if="result.review">
        <div class="cc-fs-xs review-points">
          <ModelReviewPoints :review="result.review" :category-labels="result.categoryLabels" />
        </div>
        <button v-if="result.review.items.length" class="cc-btn cc-btn-bare cc-fs-2xs mark-link" type="button"
                v-tooltip.top="'Open on the Blackboard'" @click="openEntry">Mark each point Good / Bad / Unsure</button>
      </template>

      <div class="cc-eyebrow cc-fs-2xs">Per movie</div>
      <div v-for="g in groups" :key="g.design ?? ''" class="group">
        <div class="cc-muted cc-fs-xs group-label">{{ g.label }}</div>
        <SelectionTable :columns="columns" :rows="g.rows.map(display)" id-key="uid" selection-mode="none"
                        density="compact" :row-tooltip="() => 'Show this movie’s findings'"
                        @row-click="r => openRow = openRow === r.uid ? null : String(r.uid)">
          <template #cell-name="{ row }">
            <button class="cc-btn cc-btn-bare name" @click.stop="metaUid = String(row.uid)"
                    v-tooltip.right="'Open its metadata'">{{ row.name }}</button>
          </template>
        </SelectionTable>
        <ul v-if="openRow && g.rows.some(r => r.uid === openRow)" class="findings row-findings">
          <li v-for="(f, i) in rowFindings(openRow)" :key="i">
            <InlineNote :short="f.short" :severity="findingSeverity(f)" :lines="findingLines(f)" />
          </li>
          <li v-if="!rowFindings(openRow).length" class="cc-muted">Nothing for this movie.</li>
        </ul>
      </div>
    </div>

    <ImageMetadataDialog v-if="metaImage" :image="metaImage" @close="metaUid = null" />
  </BaseModal>
</template>

<style scoped>
.acq { display: flex; flex-direction: column; gap: 0.5rem; }
.plan { padding: 0.45rem 0.6rem; background: var(--cc-surface-2); border-radius: var(--cc-radius-sm); }
.plan q { font-style: italic; }
.plan .edit { margin-left: auto; }
.plan .form { flex: 1; }
.findings { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.35rem; }
.group { display: flex; flex-direction: column; gap: 0.2rem; }
.group-label { margin-top: 0.3rem; }
.row-findings { padding: 0.4rem 0.6rem 0.5rem 1.4rem; background: var(--cc-surface-2); border-radius: var(--cc-radius-sm); }
.name { text-align: left; color: var(--cc-accent); }
.review-head { gap: 0.35rem; }
.review-head .cc-muted { text-transform: none; letter-spacing: normal; }
.review-btn { margin-left: auto; }
.review-points { display: flex; flex-direction: column; gap: 0.35rem; }
.mark-link { align-self: flex-start; color: var(--cc-accent); }
</style>
