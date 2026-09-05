<!--
  The model vault: every trained model on this machine, what it was trained on, and rename/delete.

  ONE manager, TWO kinds today: optical-flow (`opticalFlow.train`) and denoise (`opticalFlow.trainSupportDenoise`).
  A chip row at the top switches which vault is being browsed — same list, same rename, same delete,
  same info modal — because a second vault manager with slightly different colours would drift and be
  the second way to do the same thing (root CLAUDE.md → *Watch for divergent re-implementation*).

  Kind determines: which endpoints (`/api/optical-flow/*` vs `/api/denoise/*`), what the empty state
  says, and which details modal opens. The "Load these settings into the form" action is currently
  only wired for flow — the denoise-manifest → form mapper is Phase D — so that button is hidden for
  the denoise kind rather than shown and broken.

  Why the manifest matters: the flow metric set is a SILENT train/inference contract. Inference
  stacks metrics in sorted-key order and zero-fills whatever is missing, so a model used with the
  wrong set produces a confident wrong mask and no error. The manifest is the only way to tell
  whether a model fits an image, so the row leads with the channel and scales.

  Rename/delete move BOTH files — `<name>.pt` and `<name>.json`. An orphaned .pt silently falls back
  to a default and drifts.

  Chrome comes from `CanvasSidePanel` — the same shell the population manager uses — so it lives ON
  the training canvas and is toggled from the canvas bar, exactly like the pop manager. The scope
  footer is on because it means the same thing here as for populations — one pick for every plot, or
  the active plot's own.

  SELECTION DRIVES THE PLOTS: the picked model is the canvas's, held in its `shared` bag by
  `ModelPlots` and handed to every plot through the standard bag. A plot that needs a model does NOT
  carry its own picker — two pickers for one thing is how you get a canvas whose panels disagree.
-->
<script setup lang="ts">
import { ref, computed, onMounted, watch } from 'vue'
import CanvasSidePanel from '../../components/canvas/CanvasSidePanel.vue'
import ChipSelect from '../../components/ChipSelect.vue'
import ConfirmDeleteButton from '../../components/ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from '../../components/SelectionTable.vue'
import FlowModelDetails from './FlowModelDetails.vue'
import DenoiseModelDetails from './DenoiseModelDetails.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useInlineEdit } from '../../composables/useInlineEdit'
import { useProjectStore } from '../../stores/project'
import { useParamHandoffStore } from '../../stores/paramHandoff'
import { paramsFromManifest, unmappedFields } from '../../utils/flowModelParams'
import { VAULT_KIND_OPTIONS, endpointsFor, type VaultKind } from '../../utils/modelVaultKinds'
import type { CanvasManagerChrome, CanvasManagerChromeEmits } from '../../components/canvas/canvasManager'
import type { FlowManifest } from '../../utils/flowManifest'
import type { DenoiseManifest } from '../../utils/denoiseManifest'

// The row shape is a superset of what either vault returns — kind decides which fields matter.
type VaultRow = {
  name: string; label: string; stem: string
  bytes: number; modified: string
  hasManifest: boolean; manifest: FlowManifest | DenoiseManifest
}

// shared manager chrome (canvasManager.ts) + this manager's own selection.
const props = defineProps<CanvasManagerChrome & { selected?: string }>()
const emit = defineEmits<CanvasManagerChromeEmits & { 'update:selected': [string] }>()

// Kind is persisted per canvas — the vault owns it (a bare `ref()` would forget every navigation).
// Default to flow because that is the vault users had before this rename.
const kind = ref<VaultKind>('flowModels')

const models  = ref<VaultRow[]>([])
const vaultDir = ref('')
const loading = ref(false)
const error   = ref('')
const { draft, isEditing, start: startRename, cancel: cancelRename, commit, focusInput } =
  useInlineEdit()

// A finished training run adds a model. The panel floats free of the image table, so it watches
// every image in the project rather than a selection — same shared primitive, same
// `autoRefreshOnTask` opt-out, no polling and no bespoke event.
const project = useProjectStore()
const allUids = computed(() => project.sets.flatMap(s => s.images.map(i => i.uid)))
useDataRefresh(() => allUids.value, load)

async function load() {
  loading.value = true
  error.value = ''
  try {
    const res = await fetch(endpointsFor(kind.value).list)
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    const data = await res.json()
    models.value = data.models ?? []
    vaultDir.value = data.dir ?? ''
  } catch (e) {
    error.value = String(e)
  } finally {
    loading.value = false
  }
}
onMounted(() => { load(); loadMetricOptions() })
// switching kind reloads from the other endpoint; the picked name is dropped because the two vaults
// hold different namespaces.
watch(kind, () => { emit('update:selected', ''); load() })
defineExpose({ load })

const byName = (n: string): VaultRow => models.value.find(m => m.name === n)!

const commitRename = (m: VaultRow) => commit(m.name, m.stem, newName =>
  newName ? post(endpointsFor(kind.value).rename, { name: m.name, newName }) : undefined)

async function remove(m: VaultRow) { await post(endpointsFor(kind.value).delete, { name: m.name }) }

async function post(url: string, body: Record<string, unknown>) {
  error.value = ''
  try {
    const res = await fetch(url, {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
    })
    const data = await res.json().catch(() => ({}))
    if (!res.ok) throw new Error(data.error ?? `HTTP ${res.status}`)
    await load()
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
}

function mb(bytes: number): string { return `${(bytes / 1024 / 1024).toFixed(1)} MB` }

const COLUMNS: SelectionColumn[] = [
  { key: 'stem',     label: 'Model' },
  { key: 'modified', label: 'Date' },
  { key: 'size',     label: 'Size' },
]
const tableRows = computed(() => models.value.map(m => ({
  name: m.name, stem: m.stem, modified: m.modified, size: mb(m.bytes),
})))
const flowDetails = ref<VaultRow | null>(null)
const denoiseDetails = ref<VaultRow | null>(null)
function openDetails(m: VaultRow) {
  if (kind.value === 'denoiseModels') denoiseDetails.value = m
  else flowDetails.value = m
}

// ── "that one looks good, but I want to tweak it" — flow only for now ────────
// A denoise manifest could be mapped back to `opticalFlow.trainSupportDenoise` form values the same
// way, but that mapper is Phase D. Keeping the button off (rather than showing it and having it do
// nothing) is honest.
const handoff = useParamHandoffStore()
const metricOptions = ref<string[]>([])

async function loadMetricOptions() {
  try {
    const res = await fetch('/api/tasks/definitions')
    if (!res.ok) return
    const defs = await res.json()
    const list = Array.isArray(defs) ? defs : (defs.tasks ?? defs.definitions ?? [])
    const def = list.find((d: { fun_name?: string }) => d.fun_name === 'opticalFlow.train')
    const flat = (ps: { key?: string; options?: { value?: string }[]; params?: unknown[] }[]): typeof ps =>
      ps.flatMap(p => (p.params ? flat(p.params as typeof ps) : [p]))
    const metrics = flat(def?.params ?? []).find(p => p.key === 'flowMetrics')
    metricOptions.value = (metrics?.options ?? []).map(o => String(o.value))
  } catch { /* the offer still works, just without the metric chips */ }
}

function useParams(m: VaultRow) {
  if (kind.value !== 'flowModels') return
  handoff.offer({
    funName: 'opticalFlow.train',
    values: paramsFromManifest(m.manifest as FlowManifest, metricOptions.value),
    source: `model ${m.stem}`,
    missing: unmappedFields(m.manifest as FlowManifest),
  })
}

const picked = computed({
  get: () => props.selected ?? '',
  set: v => emit('update:selected', v),
})

const emptyText = computed(() => kind.value === 'denoiseModels'
  ? 'No denoise models yet — run Train denoise model (SUPPORT) on an image set.'
  : 'No models yet — run Train flow model on an image.')
</script>

<template>
  <CanvasSidePanel title="Model vault" icon="pi-database" :count="models.length" :width="340"
                   :scope="scope" :docked="docked" @update:scope="emit('update:scope', $event)">
    <div class="vault">
      <div class="vault-kinds">
        <ChipSelect :options="VAULT_KIND_OPTIONS" v-model="kind" variant="segmented"
                    aria-label="Model kind" v-tooltip.bottom="'Which model vault to browse'" />
      </div>

      <div class="vault-bar">
        <span class="cc-muted vault-dir" v-tooltip.top="vaultDir">{{ vaultDir }}</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Refresh'"
                :disabled="loading" @click="load">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>

      <p v-if="error" class="cc-muted-warn">{{ error }}</p>

      <p v-if="!loading && !models.length" class="cc-muted">{{ emptyText }}</p>

      <SelectionTable v-else :columns="COLUMNS" :rows="tableRows" v-model="picked" actions-label="">
        <template #actions="{ row }">
          <input v-if="isEditing(row.name)" v-model="draft" class="vault-rename" :ref="focusInput"
                 v-tooltip.top="'Enter to rename, Esc to cancel'"
                 @keyup.enter="commitRename(byName(row.name))" @keyup.esc="cancelRename"
                 @blur="commitRename(byName(row.name))" />
          <template v-else>
            <button v-if="kind === 'flowModels'"
                    class="cc-btn cc-btn-bare cc-btn-icon" :disabled="!byName(row.name).hasManifest"
                    v-tooltip.top="'Load these settings into the Train form'"
                    @click="useParams(byName(row.name))">
              <i class="pi pi-sliders-h" />
            </button>
            <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.top="'What it was trained on'"
                    @click="openDetails(byName(row.name))">
              <i class="pi pi-info-circle" />
            </button>
            <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.top="'Rename'"
                    @click="startRename(row.name, byName(row.name).stem)">
              <i class="pi pi-pencil" />
            </button>
            <ConfirmDeleteButton title="Delete model"
                                 armed-title="Click again to delete this model"
                                 @confirm="remove(byName(row.name))" />
          </template>
        </template>
      </SelectionTable>
    </div>
  </CanvasSidePanel>

  <FlowModelDetails v-if="flowDetails" :name="flowDetails.stem"
                    :manifest="(flowDetails.manifest as FlowManifest)"
                    :path="`${vaultDir}/${flowDetails.name}`" @close="flowDetails = null" />
  <DenoiseModelDetails v-if="denoiseDetails" :name="denoiseDetails.stem"
                       :manifest="(denoiseDetails.manifest as DenoiseManifest)"
                       :path="`${vaultDir}/${denoiseDetails.name}`" @close="denoiseDetails = null" />
</template>

<style scoped>
.vault { display: flex; flex-direction: column; gap: 0.4rem; padding: 0.4rem 0.5rem; }
.vault-kinds { display: flex; justify-content: center; }
.vault-bar { display: flex; align-items: center; gap: 0.6rem; }
.vault-dir { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vault-rename { width: 20ch; }
</style>
