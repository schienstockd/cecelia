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

    <ul v-else class="vault-list">
      <li v-for="m in models" :key="m.name" class="vault-row">
        <div class="vault-main">
          <input v-if="editing === m.name" v-model="draft" class="vault-rename" autofocus
                 v-tooltip.top="'Enter to rename, Esc to cancel'"
                 @keyup.enter="commitRename(m)" @keyup.esc="editing = null" @blur="commitRename(m)" />
          <span v-else class="vault-name" @dblclick="startRename(m)">{{ m.stem }}</span>
          <span class="cc-readout">{{ trainedOn(m) }}</span>
        </div>
        <span class="cc-muted vault-meta">{{ m.modified }} · {{ mb(m.bytes) }}</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.top="'Rename'"
                @click="startRename(m)">
          <i class="pi pi-pencil" />
        </button>
        <ConfirmDeleteButton title="Delete model"
                             armed-title="Click again to delete this model"
                             @confirm="remove(m)" />
      </li>
    </ul>
  </div>
</template>

<style scoped>
.vault { display: flex; flex-direction: column; gap: 0.4rem; }
.vault-bar { display: flex; align-items: center; gap: 0.6rem; }
.vault-dir { margin-left: auto; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
             max-width: 40ch; }
.vault-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column;
              gap: 0.2rem; }
.vault-row { display: flex; align-items: center; gap: 0.5rem; padding: 0.25rem 0.4rem;
             border-radius: var(--cc-radius-sm); }
.vault-row:hover { background: var(--cc-surface-2); }
.vault-main { display: flex; flex-direction: column; min-width: 0; flex: 1; }
.vault-name { font-weight: 500; cursor: text; }
.vault-rename { width: 20ch; }
.vault-meta { white-space: nowrap; }
</style>
