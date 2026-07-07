<!--
  Generic task runner panel used by every module page.
  Props:
    - defs: TaskDef[]       — available functions for this module (from JSON)
    - module: string        — module key ('import' | 'segment' | ...)
    - selectedUids: string[] — image UIDs to run on (from the module's image table)
    - selectedNames: string[] — matching display names (for task labels)
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import type { TaskDef, ParamValues } from './types'
import { usePanelResize } from '../composables/usePanelResize'
import { useTaskDraftsStore, taskDraftKey, taskDraftScope } from '../stores/taskDrafts'
import ParamRenderer, { type ParamContext } from './ParamRenderer.vue'
import TaskList from './TaskList.vue'
import { useTaskStore } from '../stores/tasks'
import { useLogStore } from '../stores/log'
import { useWsStore } from '../stores/ws'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'

const props = defineProps<{
  defs: TaskDef[]
  module: string
  selectedUids: string[]
  selectedNames: string[]
  onReloadDefs?: () => Promise<void>
}>()

const taskStore    = useTaskStore()
const log          = useLogStore()
const ws           = useWsStore()
const projectMeta  = useProjectMetaStore()
const projectStore = useProjectStore()
const drafts       = useTaskDraftsStore()

// Selected image objects — looked up from the store so ParamRenderer gets filepath metadata
const paramContext = computed<ParamContext>(() => ({
  images: props.selectedUids.flatMap(uid => {
    for (const set of projectStore.sets) {
      const img = set.images.find(i => i.uid === uid)
      if (img) return [img]
    }
    return []
  }),
  projectUid: projectMeta.current?.uid ?? '',
  values: paramValues.value,     // popSelection reads the sibling valueName
}))

// selected function — starts empty; resolved reactively when defs load from the API
const selectedTask = ref<string>('')
const taskDef = computed(() => props.defs.find(d => d.task === selectedTask.value))

// resource profile — auto-selected from task def default; user can override
interface PoolInfo { name: string; limit: number }
const pools = ref<PoolInfo[]>([])
const selectedPool = ref('default')

onMounted(async () => {
  try {
    const res = await fetch('/api/pools')
    if (res.ok) pools.value = await res.json() as PoolInfo[]
  } catch { /* backend may not be ready */ }
})

watch(taskDef, (def) => {
  if (def) selectedPool.value = def.resource_pool ?? 'default'
})

// param values — populated from the object's remembered funParams (image → set → task-defaults),
// mirroring the old R moduleFunParams. Persistence is server-side (saved on run, to each processed
// image and to the set — see api/src/sockets.jl _remember_fun_params), so there is NO localStorage
// for params here: ccid.json is the single source of truth, which is also why params never leak
// across projects.
const paramValues = ref<ParamValues>({})

// Which image's remembered params drive the form: the one selected image, else none. The form is a
// single config applied to ALL selected images, so with several selected we show the set-level
// last-used default rather than any single image's record.
const drivingImageUid = computed(() => props.selectedUids.length === 1 ? props.selectedUids[0] : '')
const setUid = computed(() => projectStore.activeSet()?.uid ?? '')

// Draft key mirrors how funParams are scoped (image → set): per driving image when exactly one is
// selected, else per set. Empty until project/fun/scope are known (→ no draft).
const currentDraftKey = computed(() => taskDraftKey(
  projectMeta.current?.uid ?? '', taskDef.value?.fun_name ?? '',
  taskDraftScope(drivingImageUid.value, setUid.value)))

async function fetchSavedParams(def: TaskDef): Promise<ParamValues> {
  const projectUid = projectMeta.current?.uid ?? ''
  if (!projectUid) return {}
  const qs = new URLSearchParams({ projectUid, fun: def.fun_name })
  if (drivingImageUid.value) qs.set('imageUid', drivingImageUid.value)
  if (setUid.value) qs.set('setUid', setUid.value)
  try {
    const res = await fetch(`/api/tasks/funparams?${qs.toString()}`)
    if (!res.ok) return {}
    const d = await res.json() as { params?: ParamValues | null }
    return d.params ?? {}
  } catch { return {} }
}

function buildParamValues(def: TaskDef, saved: ParamValues): ParamValues {
  const vals: ParamValues = {}
  for (const p of def.params) {
    if (p.type === 'section') {
      // Sections are stored flat (flattenParams hoists children to the top level on run), so read
      // the child from the flat key. savedSection is kept first for any legacy nested records.
      const savedSection = ((saved[p.key] ?? {}) as ParamValues)
      const sectionVals: ParamValues = {}
      for (const sp of p.params ?? []) {
        sectionVals[sp.key] = savedSection[sp.key] ?? saved[sp.key] ?? sp.default ?? null
      }
      vals[p.key] = sectionVals
    } else {
      vals[p.key] = saved[p.key] ?? p.default ?? null
    }
  }
  return vals
}

// Saved params are fetched from the server, so guard against a slower earlier request landing after
// a newer one (function/selection changed mid-flight).
let paramReqSeq = 0
async function initParams(def: TaskDef | undefined) {
  if (!def) return
  // Restore an in-progress draft for this exact scope first; only then fall back to server-saved
  // params + defaults. (Drafts are written on user edits only, so this never masks a newer save.)
  const draft = drafts.get(currentDraftKey.value)
  if (draft) { paramValues.value = draft; return }
  const seq = ++paramReqSeq
  const saved = await fetchSavedParams(def)
  if (seq !== paramReqSeq) return
  paramValues.value = buildParamValues(def, saved)
}

// Persist a param edit as a draft (USER edits only — never on programmatic init) so navigating away
// and back keeps it. Keyed to the current image→set scope.
function onParamEdit(key: string, value: unknown) {
  paramValues.value[key] = value
  drafts.set(currentDraftKey.value, paramValues.value)
}

// Flatten top-level section params before sending — sections are UI containers only.
// Nested keys (e.g. paramValues.labelModifications.matchThreshold) are hoisted flat.
function flattenParams(def: TaskDef, vals: ParamValues): ParamValues {
  const flat: ParamValues = {}
  for (const p of def.params) {
    if (p.type === 'section') {
      const nested = ((vals[p.key] ?? {}) as ParamValues)
      for (const sp of p.params ?? []) {
        flat[sp.key] = nested[sp.key] ?? sp.default ?? null
      }
    } else {
      flat[p.key] = vals[p.key]
    }
  }
  return flat
}

watch(selectedTask, (task) => {
  localStorage.setItem(`cc-fn:${props.module}`, task)   // remember last-used function per module
})

// Populate the form from remembered funParams: on function change, and on project/set switch
// (setUid changes when the active set/project changes — this is what stops one project's params
// leaking into another). Narrowing the selection to a single image reloads that image's record.
watch([taskDef, setUid], () => initParams(taskDef.value), { immediate: true })
watch(drivingImageUid, (uid) => { if (uid) initParams(taskDef.value) })

// When defs load from the API (async), pick the saved function or fall back to the first.
watch(() => props.defs, (defs) => {
  if (!defs.length) return
  if (selectedTask.value && defs.some(d => d.task === selectedTask.value)) return
  const saved = localStorage.getItem(`cc-fn:${props.module}`)
  selectedTask.value = (saved && defs.some(d => d.task === saved)) ? saved : defs[0].task
}, { immediate: true })

// run
const canRun = computed(() =>
  props.selectedUids.length > 0 && !!taskDef.value
)

function run() {
  if (!canRun.value || !taskDef.value) return

  const def = taskDef.value
  const params = flattenParams(def, paramValues.value)

  // params are persisted server-side on run (per image + set); just remember the last-used function
  localStorage.setItem(`cc-fn:${props.module}`, def.task)
  // the in-progress draft is now the canonical saved value (server-side) — drop it so the next visit
  // loads the freshly-saved params. Done before the set-scope early-return so both paths clear it.
  drafts.clear(currentDraftKey.value)

  const projectUid = projectMeta.current?.uid ?? ''

  log.info(
    `Submitting "${def.label}" for ${props.selectedUids.length} image(s)`,
    { source: props.module, detail: JSON.stringify(params, null, 2) }
  )

  // Set-scope tasks (def.scope === 'set', e.g. behaviour.hmm) run ONCE over all selected images
  // (the fit/compute spans the set). Send a single task:run carrying the full imageUids vector,
  // with the first image as the representative for status/labelling.
  if (def.scope === 'set') {
    const uids = props.selectedUids
    const rep  = uids[0]
    const repName = props.selectedNames[0] ?? rep
    const label = uids.length > 1 ? `${def.label} (${uids.length} images)` : def.label
    const t = taskStore.add({
      module: props.module, label, imageUid: rep, imageName: repName,
      status: 'queued', taskName: def.task, funName: def.fun_name,
      params: params as Record<string, unknown>, projectUid,
    })
    ws.send({
      type: 'task:run', taskId: t.id, funName: def.fun_name, params,
      imageUid: rep, imageUids: uids, projectUid, setUid: setUid.value, poolName: selectedPool.value,
    })
    return
  }

  for (let i = 0; i < props.selectedUids.length; i++) {
    const uid  = props.selectedUids[i]
    const name = props.selectedNames[i] ?? uid

    const t = taskStore.add({
      module:     props.module,
      label:      def.label,
      imageUid:   uid,
      imageName:  name,
      status:     'queued',
      taskName:   def.task,
      funName:    def.fun_name,
      params:     params as Record<string, unknown>,
      projectUid,
    })

    ws.send({
      type:       'task:run',
      taskId:     t.id,
      funName:    def.fun_name,
      params:     params,
      imageUid:   uid,
      projectUid,
      setUid:     setUid.value,
      poolName:   selectedPool.value,
    })
    // Status is driven by the backend: 'running' if a slot was free, 'queued' otherwise.
  }
}

const runLabel = computed(() => {
  const n = props.selectedUids.length
  if (n === 0) return 'Select images to run'
  return `Run on ${n} image${n > 1 ? 's' : ''}`
})

// ── Cancel all running/queued tasks for this module ────────────────────────
const activeTasks = computed(() =>
  taskStore.forModule(props.module, projectMeta.current?.uid)
    .filter(t => t.status === 'running' || t.status === 'queued')
)

function cancelAll() {
  const cancelledChainRuns = new Set<string>()
  for (const t of activeTasks.value) {
    if (t.chainRunId) {
      if (cancelledChainRuns.has(t.chainRunId)) continue
      cancelledChainRuns.add(t.chainRunId)
      taskStore.cancelChainRun(t.chainRunId)
      ws.send({ type: 'chain:cancel', runId: t.chainRunId })
    } else {
      taskStore.cancel(t.id)
      ws.send({ type: 'task:cancel', taskId: t.id })
    }
  }
}

// ── Sidebar resize (shared composable; width persisted) ────────────────────────
const { width: sidebarWidth, onResizeStart } =
  usePanelResize({ min: 200, max: 600, default: 280, storageKey: 'cc-taskrunner-width' })
</script>

<template>
  <aside class="task-runner" :style="{ width: sidebarWidth + 'px' }">

    <!-- drag handle on left edge -->
    <div
      class="resize-handle"
      @mousedown="onResizeStart"
      v-tooltip.left="'Drag to resize the task panel'"
    />

    <!-- ── Empty state (server not ready / JSON parse error) ── -->
    <section v-if="!defs.length" class="runner-section defs-empty">
      <p class="defs-empty-msg">No functions available — the server may still be starting.</p>
      <button v-if="onReloadDefs" class="cc-btn cc-btn-secondary" @click="onReloadDefs">
        <i class="pi pi-refresh" /> Reload
      </button>
    </section>

    <!-- ── Function selector ── -->
    <section v-if="defs.length" class="runner-section">
      <h3 class="section-heading">Function</h3>
      <select
        class="fn-select"
        v-model="selectedTask"
        v-tooltip.left="'Select which analysis function to run on the selected images.'"
      >
        <option v-for="d in defs" :key="d.task" :value="d.task">
          {{ d.label }}
        </option>
      </select>

      <div v-if="taskDef" class="fn-meta">
        <span class="env-badge"
          v-for="env in taskDef.env"
          :key="env"
          v-tooltip.right="`Runs in the ${env} environment.`">
          {{ env }}
        </span>
      </div>
    </section>

    <!-- ── Parameters ── -->
    <section class="runner-section params-section" v-if="taskDef">
      <h3 class="section-heading">Parameters</h3>
      <div class="params-list">
        <ParamRenderer
          v-for="p in taskDef.params"
          :key="p.key"
          :param="p"
          :modelValue="paramValues[p.key]"
          @update:modelValue="onParamEdit(p.key, $event)"
          :context="paramContext"
        />
      </div>
    </section>

    <!-- ── Run + Concurrency ── -->
    <section class="runner-section run-section">
      <button
        class="run-btn"
        :disabled="!canRun"
        @click="run"
        v-tooltip.left="canRun
          ? `Run '${taskDef?.label}' on ${selectedUids.length} selected image(s).`
          : 'Select at least one image from the list to enable run.'"
      >
        <i class="pi pi-play" />
        {{ runLabel }}
      </button>

      <div class="pool-row" v-if="pools.length > 0">
        <span class="pool-label"
          v-tooltip.right="'Resource pool controls how many tasks share a concurrency slot. GPU tasks should use the gpu pool to avoid running multiple models at once.'">
          Pool
        </span>
        <select class="pool-select" v-model="selectedPool">
          <option v-for="p in pools" :key="p.name" :value="p.name">
            {{ p.name }} (max {{ p.limit }})
          </option>
        </select>
      </div>
    </section>

    <!-- ── Task list ── -->
    <section class="runner-section tasks-section">
      <div class="tasks-heading">
        <h3 class="section-heading">Tasks</h3>
        <div class="tasks-heading-actions">
          <button
            v-if="activeTasks.length"
            class="clear-btn danger"
            @click="cancelAll"
            v-tooltip.left="`Cancel all ${activeTasks.length} running/queued task(s) in this module.`"
          >
            <i class="pi pi-times-circle" />
          </button>
          <button
            class="clear-btn"
            @click="taskStore.clearFinished(module, projectMeta.current?.uid)"
            v-tooltip.left="'Remove all completed and failed tasks from the list.'"
          >
            <i class="pi pi-filter-slash" />
          </button>
        </div>
      </div>
      <div class="tasks-scroll">
        <TaskList :module="module" />
      </div>
    </section>

  </aside>
</template>

<style scoped>
.task-runner {
  flex-shrink: 0;
  background: var(--cc-surface-1);
  border-left: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  overflow: hidden;
  position: relative;
}

.resize-handle {
  position: absolute;
  left: 0;
  top: 0;
  bottom: 0;
  width: 5px;
  cursor: col-resize;
  z-index: 10;
}
.resize-handle:hover,
.resize-handle:active {
  background: var(--cc-accent);
  opacity: 0.35;
}

.runner-section {
  padding: 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}

.tasks-heading {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 0.5rem;
}
.tasks-heading .section-heading { margin-bottom: 0; }

.tasks-heading-actions {
  display: flex;
  gap: 0.15rem;
}

.clear-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.72rem;
  padding: 0.15rem 0.3rem;
  border-radius: 0.2rem;
}
.clear-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.clear-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }

.tasks-section {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
  border-bottom: none;
  overflow: hidden;
  /* padding-bottom handled by tasks-scroll so items flush with edge */
  padding-bottom: 0;
}

.tasks-scroll {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  padding-bottom: 0.5rem;
}

/* pool row */
.pool-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-top: 0.5rem;
}
.pool-label {
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  white-space: nowrap;
  flex-shrink: 0;
}
.pool-select {
  flex: 1;
  font-size: 0.72rem;
  background: var(--cc-surface-2);
  color: var(--cc-text);
  border: 1px solid var(--cc-border);
  border-radius: 0.25rem;
  padding: 0.2rem 0.3rem;
  cursor: pointer;
  min-width: 0;
}

.section-heading {
  font-size: 0.65rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
  margin: 0 0 0.5rem;
}

/* function selector */
.fn-select {
  width: 100%;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 0.35rem;
  color: var(--cc-text);
  font-size: 0.82rem;
  padding: 0.35rem 0.5rem;
  cursor: pointer;
}
.fn-select:focus { outline: 1px solid var(--cc-accent); }

.defs-empty { display: flex; flex-direction: column; align-items: flex-start; gap: 0.6rem; }
.defs-empty-msg { font-size: 0.8rem; color: var(--cc-text-muted); margin: 0; }

.fn-meta { display: flex; gap: 0.3rem; margin-top: 0.4rem; }
.env-badge {
  font-size: 0.62rem;
  font-weight: 600;
  text-transform: uppercase;
  padding: 0.1rem 0.4rem;
  border-radius: 0.2rem;
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  border: 1px solid var(--cc-border);
}

/* params */
.params-section {
  overflow-y: auto;
  max-height: 45vh;
  flex-shrink: 0;
}
.params-list { display: flex; flex-direction: column; }

/* run */
.run-section { flex-shrink: 0; }
.run-btn {
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.4rem;
  font-size: 0.82rem;
  font-weight: 600;
  padding: 0.55rem;
  border-radius: 0.4rem;
  border: none;
  background: var(--cc-accent);
  color: #fff;
  cursor: pointer;
  transition: filter 0.12s;
}
.run-btn:hover:not(:disabled) { filter: brightness(1.12); }
.run-btn:disabled {
  opacity: 0.35;
  cursor: not-allowed;
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
}
</style>
