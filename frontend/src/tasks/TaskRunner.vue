<!--
  Generic task runner panel used by every module page.
  Props:
    - defs: TaskDef[]       — available functions for this module (from JSON)
    - module: string        — module key ('manageImages' | 'segment' | ...)
    - selectedUids: string[] — image UIDs to run on (from the module's image table)
    - selectedNames: string[] — matching display names (for task labels)

  Two stacked halves — the function runner (function + params + run + pool) as the TOP half, the module's
  task list as the BOTTOM — with the shared `PaneExpandBar` giving either one the whole panel
  (`utils/paneExpand.ts`; the batch-movies panel uses the same primitive). Width is drag-resizable;
  height is not, because there are only three states worth having and a click reaches them faster than a
  drag. The halves are hidden by the `pane-<mode>` rules in this file's CSS, never `v-if`: unmounting the
  params would throw away what each ParamRenderer has loaded (population lists, model lists) and refetch
  it on the way back.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import type { TaskDef, ParamValues } from './types'
import { buildParamValues, flattenParams } from './paramValues'
import { usePanelResize } from '../composables/usePanelResize'
import { usePaneExpand } from '../composables/usePaneExpand'
import PaneExpandBar from '../components/PaneExpandBar.vue'
import { useTaskDraftsStore, taskDraftKey, taskDraftScope } from '../stores/taskDrafts'
import ParamRenderer, { type ParamContext } from './ParamRenderer.vue'
import TaskList from './TaskList.vue'
import { taskGatingReason } from '../utils/taskGating'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import TaskPreviewControls from '../components/TaskPreviewControls.vue'
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

// resource profile — auto-selected from task def default; user can override. We only need the pool
// NAMES here (the chip labels); live limits are the throttle popover's concern, not this picker's.
const pools = ref<string[]>([])
const selectedPool = ref('cpu')
const poolOptions = computed<ChipOption[]>(() => pools.value.map(name => ({ value: name, label: name })))

async function loadPools() {
  try {
    const res = await fetch('/api/pools')
    if (res.ok) pools.value = (await res.json() as { name: string }[]).map(p => p.name)
  } catch { /* backend may not be ready */ }
}
onMounted(loadPools)

// live throttle popover (same PoolThrottle component as the Task Manager)
const throttleBtn  = ref<HTMLElement | null>(null)
const throttleOpen = ref(false)

watch(taskDef, (def) => {
  if (def) selectedPool.value = def.resource_pool ?? 'cpu'
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

// The preview gets what `run()` sends: FLATTENED. A `section` is a UI grouping, so `paramValues` keeps
// its sub-params nested, and everything downstream reads them flat — the run flattens on the way out
// (`run()` below) and the preview did not. Nothing errors; the value is just absent, so each reader
// quietly uses its own default. It made the preview claim "Run would tile this" on a run configured
// for 4096 px (`blockSize` fell back to 512) and made the base-model warning always show its milder
// wording (`removeUnmatched` read as undefined). A computed, so the identity is stable until the
// params actually change — the preview re-runs on any change to this, debounced.
const previewParams = computed<Record<string, unknown> | null>(() =>
  taskDef.value ? flattenParams(taskDef.value, paramValues.value) as Record<string, unknown> : null)

// Can this task be previewed? The task DECLARES it (`task_previewable` in app/src/tasks/task.jl,
// stamped onto the spec by the definitions route), so a new previewable backend lights up by adding one
// line beside its struct — no list here to go stale. Composites resolve through their own overload, so
// `segment.cellposeMeasure` and `cleanupImages.afDriftCorrect` both report true.
//
// One live condition on top of that: exactly one image selected, because the preview shows ONE region
// of ONE image. Deliberately NOT "are the params ready" — that check used to be here as
// `hasPreviewableModel`, which asks a cellpose question ("is there a base model?") of every task, so AF
// correction (no `models`, it has `afCombinations`) never showed a button at all despite the backend
// declaring it previewable. Readiness is now a blocker with a MESSAGE (`paramsBlocker`): a control that
// explains why it cannot run beats one that silently isn't there.
const canPreview = computed(() =>
  (taskDef.value?.previewable ?? false) && drivingImageUid.value !== '')
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

// Saved params are fetched from the server, so guard against a slower earlier request landing after
// a newer one (function/selection changed mid-flight).
let paramReqSeq = 0
async function initParams(def: TaskDef | undefined) {
  if (!def) return
  // Restore an in-progress draft for this exact scope first; only then fall back to server-saved
  // params + defaults. (Drafts are written on user edits only, so this never masks a newer save.)
  const draft = drafts.get(currentDraftKey.value)
  // Reconcile the draft against the CURRENT spec instead of restoring it raw. A draft outlives a param
  // set change, so a raw restore left new params absent from the form — and `undefined` is dropped by
  // JSON.stringify, so those params vanished from the run payload AND from the funParams record the
  // server writes, i.e. the setting silently stopped being remembered. See tasks/paramValues.ts.
  if (draft) { paramValues.value = buildParamValues(def, draft); return }
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

// Axis gating — the frontend twin of the Julia gate. A task with `requires.axes` (e.g. tracking
// on ["T"]) is disabled in the picker + Run button unless every selected image carries those axes.
// The backend refuses to run anyway (TaskApplicabilityError); this removes the surprise.
const selectedImages = computed(() => paramContext.value.images)
const gatingReasonFor = (def: TaskDef) => taskGatingReason(def, selectedImages.value)
const activeTaskGatingReason = computed(() =>
  taskDef.value ? gatingReasonFor(taskDef.value) : ''
)

// run
const canRun = computed(() =>
  props.selectedUids.length > 0 && !!taskDef.value && !activeTaskGatingReason.value
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
  if (activeTaskGatingReason.value) return activeTaskGatingReason.value
  return `Run on ${n} image${n > 1 ? 's' : ''}`
})

// ── Cancel all running/queued tasks for this module ────────────────────────
const activeTasks = computed(() =>
  taskStore.forModule(props.module, projectMeta.current?.uid)
    .filter(t => t.status === 'running' || t.status === 'queued')
)

// What the collapsed task list would have shown. Rendered in the toggle bar ONLY while that half is
// hidden — with the list visible it would just restate it — so expanding the runner doesn't mean losing
// track of whether anything is still going.
const hiddenTaskNote = computed(() => {
  if (pane.value !== 'top') return ''
  const running = activeTasks.value.filter(t => t.status === 'running').length
  const queued  = activeTasks.value.length - running
  if (!activeTasks.value.length) return ''
  return queued ? `${running} running · ${queued} queued` : `${running} running`
})

// ── Sidebar resize (shared composable; width persisted) ────────────────────────
const { widthStyle, onResizeStart } =
  usePanelResize({ min: 200, max: 600, default: 280, storageKey: 'cc-taskrunner-width' })

// ── Which half is expanded — the shared two-half panel primitive (utils/paneExpand.ts) ──
// Vertical space is the scarce one here: a long param list and a busy task list can't both fit on a
// laptop screen. Top half = this runner, bottom half = the module's task list.
const { pane, toggle: togglePane } = usePaneExpand('cc-taskrunner-pane')
</script>

<template>
  <aside class="task-runner" :class="'pane-' + pane" :style="widthStyle">

    <!-- drag handle on left edge -->
    <div
      class="resize-handle"
      @mousedown="onResizeStart"
      v-tooltip.left="'Drag to resize the task panel'"
    />

    <!-- ── Expand one half ── always visible, so whichever half is hidden can be brought back -->
    <PaneExpandBar
      class="runner-pane-bar"
      :pane="pane"
      top-label="function runner" bottom-label="task list"
      top-icon="pi-cog" bottom-icon="pi-bars"
      @toggle="togglePane"
    >
      <span v-if="hiddenTaskNote" class="cc-readout"
            v-tooltip.right="'Tasks still running in this module — expand the task list to see them'">
        {{ hiddenTaskNote }}
      </span>
    </PaneExpandBar>

    <!-- ── Empty state (server not ready / JSON parse error) ── -->
    <section v-if="!defs.length" class="runner-section defs-empty">
      <p class="defs-empty-msg cc-muted">No functions available — the server may still be starting.</p>
      <button v-if="onReloadDefs" class="cc-btn cc-btn-secondary" @click="onReloadDefs">
        <i class="pi pi-refresh" /> Reload
      </button>
    </section>

    <!-- ── Function selector ── -->
    <section v-if="defs.length" class="runner-section">
      <h3 class="section-heading cc-eyebrow cc-fs-2xs">Function</h3>
      <select
        class="fn-select"
        data-guide="task.fun"
        v-model="selectedTask"
        v-tooltip.left="'Select which analysis function to run on the selected images'"
      >
        <option
          v-for="d in defs"
          :key="d.task"
          :value="d.task"
          :disabled="!!gatingReasonFor(d)"
          :title="gatingReasonFor(d) || undefined"
        >
          {{ d.label }}{{ gatingReasonFor(d) ? ` — ${gatingReasonFor(d)}` : '' }}
        </option>
      </select>

      <div v-if="taskDef" class="fn-meta">
        <span class="env-badge"
          v-for="env in taskDef.env"
          :key="env"
          v-tooltip.right="`Runs in the ${env} environment`">
          {{ env }}
        </span>
        <span
          v-if="activeTaskGatingReason"
          class="env-badge"
          v-tooltip.right="'Selected images do not carry the axes this task needs'"
        >{{ activeTaskGatingReason }}</span>
      </div>
    </section>

    <!-- ── Parameters ── -->
    <section class="runner-section params-section" v-if="taskDef">
      <h3 class="section-heading cc-eyebrow cc-fs-2xs">Parameters</h3>
      <div class="params-list" data-guide="task.params">
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
      <!-- Run and Preview share a row: Preview is the choice Run informs, and as a small ghost icon
           BELOW a full-width primary it was invisible. Same height, so it reads as a peer. -->
      <div class="run-row">
        <button
          class="run-btn"
          data-guide="task.run"
          :disabled="!canRun"
          @click="run"
          v-tooltip.left="canRun
            ? `Run '${taskDef?.label}' on ${selectedUids.length} selected image(s)`
            : (activeTaskGatingReason || 'Select at least one image from the list to enable run')"
        >
          <i class="pi pi-play" />
          {{ runLabel }}
        </button>

        <!-- Preview: run these params on the region napari is showing, before committing to a full run -->
        <TaskPreviewControls
          :project-uid="projectMeta.current?.uid ?? ''"
          :image-uid="drivingImageUid"
          :value-name="String(paramValues.valueName ?? 'default')"
          :fun-name="taskDef?.fun_name ?? ''"
          :params="previewParams"
          :previewable="canPreview"
        />
      </div>

      <div class="pool-row" v-if="pools.length > 0">
        <span class="pool-label cc-muted cc-fs-xs"
          v-tooltip.right="'How many tasks share a concurrency slot; GPU tasks use the gpu pool'">
          Pool
        </span>
        <ChipSelect class="pool-chips" v-model="selectedPool" :options="poolOptions" aria-label="Resource pool"
          v-tooltip.right="'Which resource pool this run queues in'" />
        <button ref="throttleBtn" class="pool-throttle cc-btn cc-btn-ghost cc-btn-icon"
          :class="{ 'cc-btn-on cc-btn-on-solid': throttleOpen }"
          @click="throttleOpen = !throttleOpen"
          v-tooltip.left="'Throttle — how many tasks of each kind run at once'">
          <i class="pi pi-sliders-h" />
        </button>
        <TeleportPopover v-model="throttleOpen" :anchor="throttleBtn" placement="bottom-end">
          <PoolThrottle />
        </TeleportPopover>
      </div>
    </section>

    <!-- ── Task list ── (its heading + the two list-wide actions belong to TaskList itself, so the
         other host of this list — BatchMoviesPanel — gets them too) -->
    <section class="runner-section tasks-section" data-guide="task.list">
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

/* the shared bar (PaneExpandBar) sits above the first section heading — only its inset is ours */
.runner-pane-bar { padding: 0.2rem 0.5rem 0; }

/* Which half is showing is one CSS concern, not a guard on every element: each half-member is a direct
   `.runner-section` child of the panel, so two rules cover both halves — including a section added later,
   which per-element guards would miss. Same mechanism as BatchMoviesPanel; `display:none` either way. */
.task-runner.pane-bottom > .runner-section:not(.tasks-section) { display: none; }
.task-runner.pane-top    > .tasks-section                      { display: none; }

/* With the task list hidden, the params are what should grow — otherwise the runner keeps its 45vh cap
   and the reclaimed space just sits empty below it. */
.task-runner.pane-top .params-section {
  flex: 1;
  min-height: 0;
  max-height: none;
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
.pool-label { white-space: nowrap; flex-shrink: 0; }
.pool-chips { flex: 1; min-width: 0; }
.pool-throttle { transition: background 0.1s, color 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon */
.pool-throttle:hover  { color: var(--cc-text); }

.section-heading { margin: 0 0 0.5rem; }

/* function selector */
.fn-select {
  width: 100%;
  border-radius: var(--cc-radius-sm);
  padding: 0.35rem 0.5rem;
  cursor: pointer;
}
.fn-select:focus { outline: 1px solid var(--cc-accent); }

.defs-empty { display: flex; flex-direction: column; align-items: flex-start; gap: 0.6rem; }
.defs-empty-msg { margin: 0; }   /* + .cc-muted (was undefined --cc-text-muted) */

.fn-meta { display: flex; gap: 0.3rem; margin-top: 0.4rem; }
.env-badge {
  font-size: var(--cc-fs-2xs);
  font-weight: 600;
  text-transform: uppercase;
  padding: 0.1rem 0.4rem;
  border-radius: var(--cc-radius-xs);
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
/* Run + Preview on one line. `stretch` is what makes the preview buttons match Run's height without
   either side hardcoding a number — so a change to Run's padding can't silently desync them. */
.run-row { display: flex; align-items: stretch; gap: 0.4rem; flex-wrap: wrap; }
.run-btn {
  flex: 1 1 auto;
  min-width: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.4rem;
  font-size: var(--cc-fs-md);
  font-weight: 600;
  padding: 0.55rem;
  border-radius: var(--cc-radius-md);
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
