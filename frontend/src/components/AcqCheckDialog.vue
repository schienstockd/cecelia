<script setup lang="ts">
// The set-level acquisition check (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the set's plan, what is
// off across the set, and a per-movie table grouped by label design. Runs live on open (nothing
// written); saving the plan writes the set's Blackboard entry. Data: utils/acqCheckApi.ts; logic:
// utils/acqCheck.ts; rules: app/src/acq_check.jl.
import { computed, onMounted, ref } from 'vue'
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import AcqPlanForm from './AcqPlanForm.vue'
import ImageMetadataDialog from './ImageMetadataDialog.vue'
import { useProjectStore } from '../stores/project'
import { useToast } from '../composables/useToast'
import { fetchAcqCheck, saveAcqPlan, type AcqCheckResult, type AcqPlan, type AcqRow } from '../utils/acqCheckApi'
import { emptyPlan, findingLines, findingSeverity, findingsFor, fmt, formatDepth, planSentence, rowsByDesign } from '../utils/acqCheck'
import InlineNote from './InlineNote.vue'

const props = defineProps<{ projectUid: string; setUid: string; setName?: string }>()
const emit = defineEmits<{ close: []; changed: [AcqCheckResult] }>()
const project = useProjectStore()
const toast = useToast()

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
const groups = computed(() => (result.value ? rowsByDesign(result.value) : []))

const objectNames = computed(() => plan.value?.objects.map(o => o.name) ?? [])
const columns = computed<SelectionColumn[]>(() => [
  { key: 'name', label: 'Movie', ellipsis: true, width: 220 },
  { key: 'lasers', label: 'Lasers', width: 150 },
  { key: 'brightZ', label: 'Bright Z', width: 60 },
  { key: 'depth', label: 'Depth p99', width: 90 },
  ...objectNames.value.map(n => ({ key: `o:${n}`, label: n, width: 55 })),
  ...(objectNames.value.length > 1 ? [{ key: 'ratio', label: '÷', width: 45 }] : []),
  { key: 'sharedR', label: 'Shared r', width: 60 },
  { key: 'drift', label: 'Drift', width: 60 },
])
// the table renders strings: one display row per movie
const display = (r: AcqRow) => ({
  ...r,
  brightZ: r.brightZ === null ? '—' : r.brightZ ? 'on' : 'off',
  depth: formatDepth(r.depthPct),
  ratio: r.ratio === null ? '—' : `${fmt(r.ratio, 1)}×`,
  sharedR: fmt(r.sharedR, 2),
  drift: r.driftUm === null ? '—' : `${fmt(r.driftUm)} µm${r.registers === false ? ' ⚠' : ''}`,
  ...Object.fromEntries(objectNames.value.map(n => [`o:${n}`, fmt(r.objects[n]?.p99 ?? null)])),
})
const rowFindings = (uid: string) => (result.value ? findingsFor(result.value, uid) : [])
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
          <InlineNote :short="f.short" :severity="findingSeverity(f)" :lines="findingLines(f, 'These movies')" />
        </li>
        <li v-if="!setFindings.length" class="cc-muted">Nothing across the set.</li>
      </ul>

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
            <InlineNote :short="f.short" :severity="findingSeverity(f)" :lines="findingLines(f, 'This movie')" />
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
</style>
