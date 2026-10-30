<script setup lang="ts">
// "From history…" — build a chain from what already ran on a few exemplar images (no LLM). The backend
// suggests (`GET /api/chains/suggest`); here the user picks the exemplars, picks which version of each
// step to keep (one for most steps; several only where that is a real fork — segmentation, tracking per
// population), settles any param the exemplars disagree on, and opens the result on the whiteboard.
// docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part B.
import { computed, onMounted, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import InlineNote from './InlineNote.vue'
import CcToggle from './CcToggle.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore } from '../stores/taskDefs'
import { suggestUrl, conflictOptions, applyChoices, groupSteps, defaultKeep, groupLeftOut, uniqueChainName,
         type HistorySuggestion, type HistoryFinal } from '../utils/chainFromHistory'
import type { ChainTemplate } from '../tasks/types'
import { formatWhen } from '../utils/formatWhen'

const props = defineProps<{ projectUid: string; setUid?: string; chainNames: string[] }>()
const emit = defineEmits<{ close: []; open: [p: { name: string; template: ChainTemplate; valid: boolean; error: string }] }>()

const project  = useProjectStore()
const taskDefs = useTaskDefsStore()
onMounted(() => { void taskDefs.ensureLoaded() })
const labelOf  = (fn: string) => taskDefs.labelFor(fn) || fn
const nameOf   = (uid: string) => project.imageByUid(uid)?.name ?? uid

// Which set the exemplars come from — starts on the set the whiteboard's Run panel has selected
const setUid = ref(props.setUid || project.activeSet()?.uid || project.sets[0]?.uid || '')
const imageRows = computed(() => (project.sets.find(s => s.uid === setUid.value)?.images ?? [])
  .map(i => ({ uid: i.uid, name: i.name })))
const imageCols: SelectionColumn[] = [{ key: 'name', label: 'Image' }]
const exemplars = ref<string[]>([])

const suggestion = ref<HistorySuggestion | null>(null)
const keep    = ref<string[] | undefined>(undefined)   // undefined = the backend's default (every output)
const reuse   = ref(true)
const loading = ref(false)
const failed  = ref('')
const choices = ref<Record<string, Record<string, string>>>({})
const chainName = ref(uniqueChainName('From history', props.chainNames))

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
    // first look: keep one version of each step, all versions of a fork — then rebuild towards that
    if (keep.value === undefined) {
      keep.value = defaultKeep(groupSteps(suggestion.value.finalOutputs))
      if (keep.value.length !== suggestion.value.finalOutputs.length) await suggest()
    }
  } catch (e) {
    failed.value = String(e instanceof Error ? e.message : e)
  } finally {
    loading.value = false
  }
}

watch(setUid, () => { exemplars.value = [] })
watch(exemplars, () => { keep.value = undefined; suggestion.value = null })

// ── steps ──
const steps = computed(() => groupSteps(suggestion.value?.finalOutputs ?? [], labelOf))
const kept  = computed(() => new Set(keep.value ?? []))
const versionLabel = (v: HistoryFinal) => v.variant || `run of ${formatWhen(v.at ? new Date(v.at) : undefined)}`

function setKept(ids: string[], on: boolean) {
  const next = new Set(keep.value ?? [])
  for (const id of ids) on ? next.add(id) : next.delete(id)
  keep.value = [...next]; void suggest()
}
// a single-choice step: which version, or none
const chosen = (versions: HistoryFinal[]) => versions.find(v => kept.value.has(v.id))?.id ?? ''
function choose(versions: HistoryFinal[], id: string) {
  const next = new Set(keep.value ?? [])
  for (const v of versions) next.delete(v.id)
  if (id) next.add(id)
  keep.value = [...next]; void suggest()
}
function setReuse(v: boolean) { reuse.value = v; void suggest() }

const nodeLabel = (id: string) => {
  const n = suggestion.value?.template.nodes.find(nn => nn.id === id)
  return n ? labelOf(n.fn) : id
}
function pick(nodeId: string, param: string, key: string) {
  choices.value = { ...choices.value, [nodeId]: { ...(choices.value[nodeId] ?? {}), [param]: key } }
}

const leftOut = computed(() => groupLeftOut(suggestion.value?.notReplayed ?? [], labelOf))
const leftOutCount = computed(() => suggestion.value?.notReplayed.length ?? 0)

// never overwrite a chain the user already has
const nameTaken = computed(() => props.chainNames.includes(chainName.value.trim()))

function open() {
  const s = suggestion.value
  const name = chainName.value.trim()
  if (!s || !name || nameTaken.value) return
  emit('open', { name, template: applyChoices({ ...s.template, name }, choices.value), valid: s.valid, error: s.error })
}
</script>

<template>
  <BaseModal title="Chain from history" icon="pi-history" width="640px" @close="emit('close')">
    <div class="cfh">
      <div class="cc-row">
        <label class="cc-eyebrow" v-tooltip.top="'Images you tried things on — their runs become the chain'">
          Exemplars
        </label>
        <select v-model="setUid" v-tooltip.bottom="'Set to select the exemplars from'">
          <option v-for="s in project.sets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
        </select>
      </div>
      <SelectionTable selection-mode="multi" id-key="uid" :columns="imageCols" :rows="imageRows"
                      v-model:selected="exemplars"
                      :row-tooltip="() => 'Use this image\'s runs'" />
      <div class="cc-row">
        <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="!exemplars.length || loading"
                v-tooltip.bottom="'Build a chain from what ran on these images'" @click="suggest">
          <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-history']" /> Suggest
        </button>
        <CcToggle v-if="suggestion" :model-value="reuse" @update:model-value="setReuse"
                  label="Reuse trained models" />
      </div>
      <InlineNote v-if="failed" severity="fail" :short="failed" />

      <template v-if="suggestion">
        <label class="cc-eyebrow" v-tooltip.top="'Which version of each step to repeat'">Steps</label>
        <div class="cfh-steps">
          <div v-for="g in steps" :key="g.fn" class="cfh-step">
            <span class="cfh-step-label">{{ g.label }}</span>
            <!-- one version: keep it or not -->
            <label v-if="g.versions.length === 1" class="cc-row-group cfh-version"
                   v-tooltip.top="'Repeat this step'">
              <input type="checkbox" :checked="kept.has(g.versions[0].id)"
                     @change="setKept([g.versions[0].id], ($event.target as HTMLInputElement).checked)" />
              {{ g.versions[0].variant || 'Keep' }}
            </label>
            <!-- a fork (segmentation, tracking per population): keep any of them -->
            <div v-else-if="g.multi" class="cc-row cc-row-loose"
                 v-tooltip.top="'Several can run side by side — keep each you want'">
              <label v-for="v in g.versions" :key="v.id" class="cc-row-group cfh-version">
                <input type="checkbox" :checked="kept.has(v.id)"
                       @change="setKept([v.id], ($event.target as HTMLInputElement).checked)" />
                {{ versionLabel(v) }}
              </label>
            </div>
            <!-- alternatives: keep one -->
            <select v-else :value="chosen(g.versions)"
                    v-tooltip.bottom="'The exemplars ran this more than once — keep one'"
                    @change="choose(g.versions, ($event.target as HTMLSelectElement).value)">
              <option v-for="v in g.versions" :key="v.id" :value="v.id">{{ versionLabel(v) }}</option>
              <option value="">Leave out</option>
            </select>
          </div>
        </div>

        <template v-if="suggestion.conflicts.length">
          <label class="cc-eyebrow" v-tooltip.top="'The exemplars used different values — select one'">
            Different per image
          </label>
          <div v-for="c in suggestion.conflicts" :key="c.nodeId + c.param" class="cfh-step">
            <span class="cfh-step-label">{{ nodeLabel(c.nodeId) }} · {{ c.param }}</span>
            <select :value="choices[c.nodeId]?.[c.param] ?? JSON.stringify(suggestion.template.nodes.find(n => n.id === c.nodeId)?.params?.[c.param])"
                    v-tooltip.bottom="'Value to run with on every image'"
                    @change="pick(c.nodeId, c.param, ($event.target as HTMLSelectElement).value)">
              <option v-for="o in conflictOptions(c, nameOf)" :key="o.key" :value="o.key">{{ o.label }}</option>
            </select>
          </div>
        </template>

        <InlineNote v-if="suggestion.gateSteps.length" icon="pi-pause"
                    :short="`${suggestion.gateSteps.length} gate step(s) — the run waits for you there`" />
        <InlineNote v-if="suggestion.reusedModels.length" icon="pi-database"
                    :short="`Reuses ${suggestion.reusedModels.map(m => m.model).join(', ')} — not retrained`" />
        <InlineNote v-if="!suggestion.valid" severity="warn" :short="suggestion.error" />
        <CollapsibleSection v-if="leftOutCount" :label="`Left out (${leftOutCount})`" :default-open="false"
                            tip="Runs that are not repeated, and why">
          <div v-for="l in leftOut" :key="l.why" class="cfh-leftout">
            <span class="cc-muted">{{ l.why }}:</span> {{ l.tasks.join(', ') }}
            <span class="cc-muted">({{ l.images }} image{{ l.images === 1 ? '' : 's' }})</span>
          </div>
        </CollapsibleSection>
      </template>
    </div>

    <template #footer>
      <InlineNote v-if="nameTaken" severity="warn" :short="`A chain called “${chainName.trim()}” exists`" />
      <input v-model="chainName" class="cfh-name" :disabled="!suggestion"
             v-tooltip.top="'Name for the new chain'" />
      <button class="cc-btn cc-btn-primary" :disabled="!suggestion || !chainName.trim() || nameTaken"
              v-tooltip.top="'Create the chain and open it on the whiteboard'" @click="open">
        <i class="pi pi-cog" /> Open in whiteboard
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.cfh { display: flex; flex-direction: column; gap: 0.5rem; }
.cfh-steps { display: flex; flex-direction: column; gap: 0.25rem; }
.cfh-step { display: flex; align-items: center; gap: 0.5rem; min-height: 1.6rem; }
.cfh-step-label { flex: 0 0 40%; font-size: var(--cc-fs-sm); }
.cfh-step select { flex: 1; }
.cfh-version { font-size: var(--cc-fs-sm); }
.cfh-leftout { font-size: var(--cc-fs-sm); padding: 1px 0; }
.cfh-name { flex: 1; }
</style>
