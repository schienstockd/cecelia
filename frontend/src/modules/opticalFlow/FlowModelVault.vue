<!--
  The optical-flow model vault: every trained model on this machine, what it was trained on, and
  rename/delete.

  Why it shows the manifest rather than just a name: the flow metric set is a SILENT train/inference
  contract. Inference stacks metrics in sorted-key order and zero-fills whatever is missing, so a
  model used with the wrong set produces a confident wrong mask and no error. The manifest is the
  only way to tell whether a model fits an image, so the row leads with the channel and scales.

  Rename/delete move BOTH files — `<name>.pt` and `<name>.json`. An orphaned .pt silently falls back
  to coastal's default metric set, which is the failure above.

  Chrome comes from the app's `FloatingPanel` (the component Viewer and Lab log use), so drag,
  resize, collapse and per-key persistence are not reimplemented here — this file is only the list.
  NOT `PopulationPanelShell`: that is CANVAS-panel chrome, absolutely positioned inside a zoomable
  board and carrying a global/local PLOT scope footer that means nothing for a model list. The two
  floating mechanisms are a deliberate split, not duplication (see INVENTORY.md).
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import ConfirmDeleteButton from '../../components/ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from '../../components/SelectionTable.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useProjectStore } from '../../stores/project'

type Manifest = {
  channelName?: string
  temporalScales?: number[]
  cumulativeWindow?: number
  droppedMetrics?: string[]
  metricKeys?: string[]
  epochs?: number
  nFrames?: number
  sourceImage?: string
  sourceValueName?: string
}
type FlowModel = {
  name: string; label: string; stem: string
  bytes: number; modified: string
  hasManifest: boolean; manifest: Manifest
}

const models  = ref<FlowModel[]>([])
const vaultDir = ref('')
const loading = ref(false)
const error   = ref('')
const editing = ref<string | null>(null)
const draft   = ref('')

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
    const res = await fetch('/api/optical-flow/models')
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
onMounted(load)
defineExpose({ load })

const byName = (n: string): FlowModel => models.value.find(m => m.name === n)!

function startRename(m: FlowModel) { editing.value = m.name; draft.value = m.stem }

async function commitRename(m: FlowModel) {
  const newName = draft.value.trim()
  editing.value = null
  if (!newName || newName === m.stem) return
  await post('/api/optical-flow/rename', { name: m.name, newName })
}

async function remove(m: FlowModel) { await post('/api/optical-flow/delete', { name: m.name }) }

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

function trainedOn(m: FlowModel): string {
  if (!m.hasManifest) return 'no manifest — assumes default metrics'
  const parts: string[] = []
  if (m.manifest.channelName) parts.push(m.manifest.channelName)
  if (m.manifest.temporalScales?.length) parts.push(`scales ${m.manifest.temporalScales.join(',')}`)
  if (m.manifest.metricKeys?.length) parts.push(`${m.manifest.metricKeys.length} metrics`)
  if (m.manifest.nFrames) parts.push(`${m.manifest.nFrames} frames`)
  return parts.join(' · ')
}

function mb(bytes: number): string { return `${(bytes / 1024 / 1024).toFixed(1)} MB` }

// `SelectionTable` renders display strings verbatim and never formats a number itself, so every
// column is built here.
const COLUMNS: SelectionColumn[] = [
  { key: 'stem',      label: 'Model' },
  { key: 'trainedOn', label: 'Trained on' },
  { key: 'modified',  label: 'Date' },
  { key: 'size',      label: 'Size' },
]
const tableRows = computed(() => models.value.map(m => ({
  name: m.name, stem: m.stem, trainedOn: trainedOn(m), modified: m.modified, size: mb(m.bytes),
})))
const picked = ref('')
</script>

<template>
  <div class="vault">
    <div class="vault-bar">
      <span class="cc-muted">{{ models.length }} model{{ models.length === 1 ? '' : 's' }}</span>
      <span class="cc-muted vault-dir" v-tooltip.top="vaultDir">{{ vaultDir }}</span>
      <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Refresh'"
              :disabled="loading" @click="load">
        <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
      </button>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>

    <p v-if="!loading && !models.length" class="cc-muted">
      No models yet — run Train flow model on an image.
    </p>

    <SelectionTable v-else :columns="COLUMNS" :rows="tableRows" v-model="picked"
                    actions-label=""
                    :row-tooltip="r => `Trained on ${r.trainedOn || 'unknown data'}`">
      <template #actions="{ row }">
        <input v-if="editing === row.name" v-model="draft" class="vault-rename" autofocus
               v-tooltip.top="'Enter to rename, Esc to cancel'"
               @keyup.enter="commitRename(byName(row.name))" @keyup.esc="editing = null"
               @blur="commitRename(byName(row.name))" />
        <template v-else>
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.top="'Rename'"
                  @click="startRename(byName(row.name))">
            <i class="pi pi-pencil" />
          </button>
          <ConfirmDeleteButton title="Delete model"
                               armed-title="Click again to delete this model"
                               @confirm="remove(byName(row.name))" />
        </template>
      </template>
    </SelectionTable>
  </div>
</template>

<style scoped>
.vault { display: flex; flex-direction: column; gap: 0.4rem; }
.vault-bar { display: flex; align-items: center; gap: 0.6rem; }
.vault-dir { margin-left: auto; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
             max-width: 40ch; }
.vault-rename { width: 20ch; }
</style>
