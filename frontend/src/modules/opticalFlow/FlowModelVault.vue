<!--
  The optical-flow model vault: every trained model on this machine, what it was trained on, and
  rename/delete.

  Why it shows the manifest rather than just a name: the flow metric set is a SILENT train/inference
  contract. Inference stacks metrics in sorted-key order and zero-fills whatever is missing, so a
  model used with the wrong set produces a confident wrong mask and no error. The manifest is the
  only way to tell whether a model fits an image, so the row leads with the channel and scales.

  Rename/delete move BOTH files — `<name>.pt` and `<name>.json`. An orphaned .pt silently falls back
  to coastal's default metric set, which is the failure above.

  Chrome comes from `CanvasSidePanel` — the same shell the population manager uses — so it lives ON
  the flow canvas and is toggled from the canvas bar, exactly like the pop manager. It was the app's
  `FloatingPanel` first, which was wrong: that is a top-level VIEWPORT window and the vault would
  collide with the Viewer and the Lab log for the same screen space, over a canvas it belongs to.
  The shell's styling block is plot-only and stays off; the global/local SCOPE footer is on, because
  it means the same thing here as it does for populations — one pick for every plot, or the active
  plot's own. A module page should not need re-learning because the thing being picked is a model.

  SELECTION DRIVES THE PLOTS, exactly as the population manager's does: the picked model is the
  canvas's, held in its `shared` bag by `FlowPlots` and handed to every plot through the standard
  bag. A plot that needs a model does NOT carry its own picker — two pickers for one thing is how
  you get a canvas whose panels disagree about what they are showing.
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import CanvasSidePanel from '../../components/canvas/CanvasSidePanel.vue'
import ConfirmDeleteButton from '../../components/ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from '../../components/SelectionTable.vue'
import FlowModelDetails from './FlowModelDetails.vue'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useInlineEdit } from '../../composables/useInlineEdit'
import { useProjectStore } from '../../stores/project'
import { useParamHandoffStore } from '../../stores/paramHandoff'
import { paramsFromManifest, unmappedFields } from '../../utils/flowModelParams'
import type { CanvasManagerChrome, CanvasManagerChromeEmits } from '../../components/canvas/canvasManager'
import type { FlowManifest as Manifest } from '../../utils/flowManifest'

type FlowModel = {
  name: string; label: string; stem: string
  bytes: number; modified: string
  hasManifest: boolean; manifest: Manifest
}

// shared manager chrome (canvasManager.ts) + this manager's own selection. `docked` is what lets the
// board's rail host this panel at all; the flow canvas leaves it off and gets the draggable box.
const props = defineProps<CanvasManagerChrome & { selected?: string }>()
const emit = defineEmits<CanvasManagerChromeEmits & { 'update:selected': [string] }>()

const models  = ref<FlowModel[]>([])
const vaultDir = ref('')
const loading = ref(false)
const error   = ref('')
// edit-in-place, shared with the population manager and the tables (composables/useInlineEdit)
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
onMounted(() => { load(); loadMetricOptions() })
defineExpose({ load })

const byName = (n: string): FlowModel => models.value.find(m => m.name === n)!

// a model must keep a name, so the empty case is rejected here — `commit` deliberately leaves that
// to the caller (clearing an image note IS a legitimate edit)
const commitRename = (m: FlowModel) => commit(m.name, m.stem, newName =>
  newName ? post('/api/optical-flow/rename', { name: m.name, newName }) : undefined)

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

function mb(bytes: number): string { return `${(bytes / 1024 / 1024).toFixed(1)} MB` }

// `SelectionTable` renders display strings verbatim and never formats a number itself, so every
// column is built here. There is deliberately NO "Trained on" column: a one-line summary of the
// manifest was wide enough to force the panel wider and still too thin to answer anything, so the
// whole manifest lives behind the row's info icon instead.
const COLUMNS: SelectionColumn[] = [
  { key: 'stem',     label: 'Model' },
  { key: 'modified', label: 'Date' },
  { key: 'size',     label: 'Size' },
]
const tableRows = computed(() => models.value.map(m => ({
  name: m.name, stem: m.stem, modified: m.modified, size: mb(m.bytes),
})))
const details = ref<FlowModel | null>(null)

// ── "that one looks good, but I want to tweak it" ────────────────────────────
// A model's manifest is very nearly the form that produced it (13 of 17 controls under the same key),
// so the vault can load it back into the Train form instead of the user reading the details dialog and
// re-typing. Handed over through `paramHandoff` because the vault is on the CANVAS and the form is in
// the module column — siblings with no props between them.
//
// NOT the same as typing a name into the Model name field, which restores from this project's run log
// (`/api/tasks/funparams`). That cannot work for a model trained elsewhere or fetched from a vault,
// which is the whole point of a published model: the manifest travels with the `.pt`, the run log
// does not.
//
// The metric option list comes from the served spec, not a copy here — the manifest records what was
// EXCLUDED, so reconstructing the selection needs to know what is on offer now.
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

function useParams(m: FlowModel) {
  handoff.offer({
    funName: 'opticalFlow.train',
    values: paramsFromManifest(m.manifest, metricOptions.value),
    source: `model ${m.stem}`,
    missing: unmappedFields(m.manifest),
  })
}

// v-model:selected — the canvas owns it (FlowPlots keeps it in the shared bag); this panel is the
// one place it is EDITED, like the pop manager and its highlight set.
const picked = computed({
  get: () => props.selected ?? '',
  set: v => emit('update:selected', v),
})
</script>

<template>
  <CanvasSidePanel title="Model vault" icon="pi-database" :count="models.length" :width="340"
                   :scope="scope" :docked="docked" @update:scope="emit('update:scope', $event)">
    <div class="vault">
      <div class="vault-bar">
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

      <SelectionTable v-else :columns="COLUMNS" :rows="tableRows" v-model="picked" actions-label="">
        <template #actions="{ row }">
          <input v-if="isEditing(row.name)" v-model="draft" class="vault-rename" :ref="focusInput"
                 v-tooltip.top="'Enter to rename, Esc to cancel'"
                 @keyup.enter="commitRename(byName(row.name))" @keyup.esc="cancelRename"
                 @blur="commitRename(byName(row.name))" />
          <template v-else>
            <button class="cc-btn cc-btn-bare cc-btn-icon" :disabled="!byName(row.name).hasManifest"
                    v-tooltip.top="'Load these settings into the Train form'"
                    @click="useParams(byName(row.name))">
              <i class="pi pi-sliders-h" />
            </button>
            <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.top="'What it was trained on'"
                    @click="details = byName(row.name)">
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

  <FlowModelDetails v-if="details" :name="details.stem" :manifest="details.manifest"
                    :path="`${vaultDir}/${details.name}`" @close="details = null" />
</template>

<style scoped>
/* the shell owns the box; this is only the list's own layout */
.vault { display: flex; flex-direction: column; gap: 0.4rem; padding: 0.4rem 0.5rem; }
.vault-bar { display: flex; align-items: center; gap: 0.6rem; }
.vault-dir { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vault-rename { width: 20ch; }
</style>
