<script setup lang="ts">
// "From history…" — build a chain from what already ran on a few exemplar images (no LLM). The backend
// suggests (`GET /api/chains/suggest`); here the user picks the exemplars, ticks the final outputs that
// matter, settles any param the exemplars disagree on, and opens the result on the whiteboard, where
// it is saved like any chain. docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part B.
import { computed, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import InlineNote from './InlineNote.vue'
import CcToggle from './CcToggle.vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore } from '../stores/taskDefs'
import { suggestUrl, conflictOptions, applyChoices, type HistorySuggestion } from '../utils/chainFromHistory'
import type { ChainTemplate } from '../tasks/types'

const props = defineProps<{ projectUid: string }>()
const emit = defineEmits<{ close: []; open: [p: { name: string; template: ChainTemplate; valid: boolean; error: string }] }>()

const project  = useProjectStore()
const taskDefs = useTaskDefsStore()
const labelOf  = (fn: string) => taskDefs.labelFor(fn) || fn
const nameOf   = (uid: string) => project.imageByUid(uid)?.name ?? uid

const imageRows = computed(() => (project.activeSet()?.images ?? []).map(i => ({ uid: i.uid, name: i.name })))
const imageCols: SelectionColumn[] = [{ key: 'name', label: 'Image' }]
const exemplars = ref<string[]>([])

const suggestion = ref<HistorySuggestion | null>(null)
const keep    = ref<string[] | undefined>(undefined)   // undefined = every final output
const reuse   = ref(true)
const loading = ref(false)
const failed  = ref('')
const choices = ref<Record<string, Record<string, string>>>({})
const chainName = ref('From history')

async function suggest() {
  if (!exemplars.value.length) return
  loading.value = true; failed.value = ''
  try {
    const res = await fetch(suggestUrl(props.projectUid, exemplars.value,
                                       { keep: keep.value, retrain: !reuse.value }))
    const body = await res.json()
    if (!res.ok) throw new Error(String(body?.error ?? `HTTP ${res.status}`))
    suggestion.value = body as HistorySuggestion
    choices.value = {}
  } catch (e) {
    failed.value = String(e instanceof Error ? e.message : e)
  } finally {
    loading.value = false
  }
}

// a new set of exemplars starts from every final output again
watch(exemplars, () => { keep.value = undefined; suggestion.value = null })

const finalRows = computed(() => (suggestion.value?.finalOutputs ?? []).map(f => ({
  id: f.id, step: labelOf(f.fn), output: f.output || '—' })))
const finalCols: SelectionColumn[] = [{ key: 'step', label: 'Step' }, { key: 'output', label: 'Output' }]
const keptIds = computed<string[]>({
  get: () => keep.value ?? (suggestion.value?.finalOutputs ?? []).map(f => f.id),
  set: ids => { keep.value = ids; void suggest() },
})
function setReuse(v: boolean) { reuse.value = v; void suggest() }

const nodeLabel = (id: string) => {
  const n = suggestion.value?.template.nodes.find(nn => nn.id === id)
  return n ? labelOf(n.fn) : id
}
function pick(nodeId: string, param: string, key: string) {
  choices.value = { ...choices.value, [nodeId]: { ...(choices.value[nodeId] ?? {}), [param]: key } }
}

function open() {
  const s = suggestion.value
  if (!s || !chainName.value.trim()) return
  const template = applyChoices({ ...s.template, name: chainName.value.trim() }, choices.value)
  emit('open', { name: chainName.value.trim(), template, valid: s.valid, error: s.error })
}
</script>

<template>
  <BaseModal title="Chain from history" icon="pi-history" width="640px" @close="emit('close')">
    <div class="cfh">
      <label class="cc-eyebrow" v-tooltip.top="'Images you tried things on — their runs become the chain'">
        Exemplars
      </label>
      <SelectionTable selection-mode="multi" id-key="uid" :columns="imageCols" :rows="imageRows"
                      v-model:selected="exemplars"
                      :row-tooltip="() => 'Use this image\'s runs'" />
      <div class="cc-row cc-row-tight">
        <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="!exemplars.length || loading"
                v-tooltip.bottom="'Build a chain from what ran on these images'" @click="suggest">
          <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-history']" /> Suggest
        </button>
        <CcToggle v-if="suggestion" :model-value="reuse" @update:model-value="setReuse"
                  label="Reuse trained models" />
      </div>
      <InlineNote v-if="failed" severity="fail" :short="failed" />

      <template v-if="suggestion">
        <label class="cc-eyebrow" v-tooltip.top="'Untick an output you abandoned — its upstream goes too'">
          Final outputs
        </label>
        <SelectionTable selection-mode="multi" id-key="id" :columns="finalCols" :rows="finalRows"
                        v-model:selected="keptIds" :row-tooltip="() => 'Keep this output'" />

        <template v-if="suggestion.conflicts.length">
          <label class="cc-eyebrow" v-tooltip.top="'The exemplars used different values — select one'">
            Different per image
          </label>
          <div v-for="c in suggestion.conflicts" :key="c.nodeId + c.param" class="cfh-conflict">
            <span class="cfh-conflict-label">{{ nodeLabel(c.nodeId) }} · {{ c.param }}</span>
            <select :value="choices[c.nodeId]?.[c.param] ?? JSON.stringify(suggestion.template.nodes.find(n => n.id === c.nodeId)?.params?.[c.param])"
                    v-tooltip.bottom="'Value to run with on every image'"
                    @change="pick(c.nodeId, c.param, ($event.target as HTMLSelectElement).value)">
              <option v-for="o in conflictOptions(c, nameOf)" :key="o.key" :value="o.key">{{ o.label }}</option>
            </select>
          </div>
        </template>

        <InlineNote v-if="suggestion.gateSteps.length" icon="pi-pause"
                    :short="`${suggestion.gateSteps.length} gate step(s) — the run waits for you there`" />
        <InlineNote v-for="m in suggestion.reusedModels" :key="m.model" icon="pi-database"
                    :short="`${labelOf(m.fn)} not repeated — uses ${m.model}`" />
        <InlineNote v-for="n in suggestion.notReplayed" :key="n.fn + n.imageUid" severity="warn"
                    :short="`${labelOf(n.fn)} (${nameOf(n.imageUid)}) left out: ${n.why}`" />
        <InlineNote v-if="!suggestion.valid" severity="warn" :short="suggestion.error" />
      </template>
    </div>

    <template #footer>
      <input v-model="chainName" class="cfh-name" :disabled="!suggestion"
             v-tooltip.top="'Name for the new chain'" />
      <button class="cc-btn cc-btn-primary" :disabled="!suggestion || !chainName.trim()"
              v-tooltip.top="'Create the chain and open it on the whiteboard'" @click="open">
        <i class="pi pi-cog" /> Open in whiteboard
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.cfh { display: flex; flex-direction: column; gap: 0.5rem; }
.cfh-conflict { display: flex; align-items: center; gap: 0.5rem; }
.cfh-conflict-label { flex: 0 0 45%; font-size: var(--cc-fs-sm); }
.cfh-conflict select { flex: 1; }
.cfh-name { flex: 1; }
</style>
