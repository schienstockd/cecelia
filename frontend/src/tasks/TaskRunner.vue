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
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { fetchHpcSettings, fetchHpcEnv, installHpcEnv, onHpcEnvChanged, hpcTestBlocker,
  type HpcSettings, type HpcEnvStatus } from '../utils/hpcSettings'
import { isPreferencesOpen } from '../lib/preferencesOpen'
import { profileStorage } from '../utils/profileStorage'
import { hpcEligible, hpcDefaults, parseRunTargetChoice, hpcRunFields, runTargetKey, HPC_RESOURCE_FIELDS, hpcRuleCpus,
  hpcRunDestination, hpcRunEnvNote, hpcFieldsFor, hpcSteps, formatResource, hpcMemoryHint, type FrameSize,
  hpcShownStatus, type RunTargetChoice, type HpcResourcesForm } from '../utils/hpcRun'
import type { TaskDef, ParamValues } from './types'
import { buildParamValues, flattenParams, resolveInitialParams, missingRequired, findParamByKey } from './paramValues'
import { cachedPyramidRecommendation } from './pyramidPeek'
import { useSettingsStore } from '../stores/settings'
import { vaultManifestParams } from '../utils/vaultManifest'
import { syncGroupOrder } from '../utils/chipSelect'
import { usePaneExpand } from '../composables/usePaneExpand'
import { useActiveInterval } from '../composables/useKeepAlive'
import PaneExpandBar from '../components/PaneExpandBar.vue'
import { useTaskDraftsStore, taskDraftKey, taskDraftScope } from '../stores/taskDrafts'
import { useParamHandoffStore } from '../stores/paramHandoff'
import ParamRenderer, { type ParamContext } from './ParamRenderer.vue'
import TaskList from './TaskList.vue'
import { taskGatingReason } from '../utils/taskGating'
import { frameGeometry } from './paramAdvisors'
import { groupTaskDefs } from '../utils/taskGroups'
import { checkLine } from '../utils/taskDiscovery'
import InlineNote from '../components/InlineNote.vue'
import { debouncedLatest } from '../utils/debouncedLatest'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import TaskPreviewControls from '../components/TaskPreviewControls.vue'
import { previewValueName } from '../utils/taskPreview'
import { taskOutput } from '../utils/taskOutput'
import { useTaskStore } from '../stores/tasks'
import { useLogStore } from '../stores/log'
import { useWsStore } from '../stores/ws'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import CopyFunParamsModal, { type CopyFunParamsRow } from '../components/CopyFunParamsModal.vue'
import AddToKiwiButton from '../components/kiwi/AddToKiwiButton.vue'

const props = defineProps<{
  defs: TaskDef[]
  module: string
  selectedUids: string[]
  selectedNames: string[]
  // `form` (optional) re-resolves param options against the current form — see optionRefetch.
  onReloadDefs?: (form?: Record<string, unknown>) => Promise<void>
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
  values: paramValues.value,     // showIf conditions + sibling lookups read these
  params: taskDef.value?.params, // so a widget finds its sibling by TYPE, not by a hardcoded key
}))

// selected function — starts empty; resolved reactively when defs load from the API
const selectedTask = ref<string>('')
const taskDef = computed(() => props.defs.find(d => d.task === selectedTask.value))
// picker sub-headings (Segment / Measure / Correct …); null = render the flat list
const fnGroups = computed(() => groupTaskDefs(props.defs))

// what the task PRODUCES on disk, for the one-line note under the picker. Server-stamped from
// `task_output_effect` (Julia); absent when the output isn't an image (segment / measure / cluster).
// One-line each, per docs/ui/COPY.md — no "each image" plural boilerplate; the picker's row of images
// above already establishes that.
const OUTPUT_EFFECT_LABEL: Record<string, string> = {
  'new-image':   'Creates a new image',
  'new-version': 'Adds a new image version',
  'in-place':    'Modifies in place',
}
const OUTPUT_EFFECT_TIP: Record<string, string> = {
  'new-image':   'A new image is added to the set; the source is untouched',
  'new-version': 'A new version is written on each image; other versions are untouched',
  'in-place':    'Overwrites the selected image data; not undoable',
}
const outputEffectLabel = computed(() =>
  OUTPUT_EFFECT_LABEL[taskDef.value?.outputEffect ?? ''] ?? '')
const outputEffectTip = computed(() =>
  OUTPUT_EFFECT_TIP[taskDef.value?.outputEffect ?? ''] ?? '')

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

// Where the run goes: the env chip is the local ⇄ hpc switch for a task that lists "hpc" in `env`
// (docs/todo/HPC_PLAN.md P3). Remembered per function, with the resources the user changed.
const runChoice = ref<RunTargetChoice>({ target: 'local', resources: {} })
const canHpc = computed(() => hpcEligible(taskDef.value))
const onHpc = computed(() => canHpc.value && runChoice.value.target === 'hpc')
const hpcForm = computed<HpcResourcesForm | null>(() =>
  taskDef.value ? { ...hpcDefaults(taskDef.value), ...runChoice.value.resources } : null)
const TARGET_OPTIONS: ChipOption[] = [
  { value: 'local', label: 'local', tip: 'Run on this computer' },
  { value: 'hpc',   label: 'hpc',   tip: 'Run on the cluster: for many or large images; this computer stays free' },
]
watch(() => taskDef.value?.fun_name, (fn) => {
  runChoice.value = parseRunTargetChoice(fn ? profileStorage.getItem(runTargetKey(fn)) : null)
}, { immediate: true })
function saveRunChoice(next: RunTargetChoice) {
  runChoice.value = next
  if (taskDef.value) profileStorage.setItem(runTargetKey(taskDef.value.fun_name), JSON.stringify(next))
}
function setTarget(v: string | string[]) {
  saveRunChoice({ ...runChoice.value, target: v === 'hpc' ? 'hpc' : 'local' })
}
// One stepped slider per field (HPC_STEPS, with the default and the current size kept as stops). A
// drag only moves `hpcDrag`; the choice is saved once, on release.
const hpcFields = computed(() => taskDef.value ? hpcFieldsFor(taskDef.value) : [])
const hpcDefaultsNow = computed(() => taskDef.value ? hpcDefaults(taskDef.value) : null)
const hpcDrag = ref<Partial<HpcResourcesForm>>({})
const hpcRaw = (key: keyof HpcResourcesForm) => hpcDrag.value[key] ?? hpcForm.value?.[key] ?? 0
// A site rule (CPUs per GPU, Preferences → HPC) fixes the CPUs of a GPU job: shown, not slidable.
const hpcRuleCpusNow = computed(() => taskDef.value ? hpcRuleCpus(hpcSettings.value, taskDef.value, hpcRaw('gpus')) : null)
const hpcRuled = (key: keyof HpcResourcesForm) => key === 'cpus' && hpcRuleCpusNow.value !== null
const hpcValue = (key: keyof HpcResourcesForm) => hpcRuled(key) ? hpcRuleCpusNow.value! : hpcRaw(key)
const hpcStops = computed(() => Object.fromEntries(HPC_RESOURCE_FIELDS.map(f =>
  [f.key, hpcSteps(f.key, hpcDefaultsNow.value?.[f.key] ?? NaN, hpcForm.value?.[f.key] ?? NaN,
                   f.key === 'cpus' ? hpcRuleCpusNow.value ?? NaN : NaN)])) as
  Record<keyof HpcResourcesForm, number[]>)
function commitResource(key: keyof HpcResourcesForm) {
  const n = hpcDrag.value[key]
  hpcDrag.value = {}
  if (n !== undefined) saveRunChoice({ ...runChoice.value, resources: { ...runChoice.value.resources, [key]: n } })
}
const hpcChanged = computed(() => !!hpcForm.value && !!hpcDefaultsNow.value &&
  hpcFields.value.some(f => hpcForm.value![f.key] !== hpcDefaultsNow.value![f.key]))
const hpcResetTip = computed(() => hpcDefaultsNow.value
  ? `Back to this task's size: ${hpcFields.value.map(f => `${f.label} ${formatResource(f.key, hpcDefaultsNow.value![f.key])}`).join(', ')}` : '')
function resetResources() { saveRunChoice({ ...runChoice.value, resources: {} }) }
function setMemory(gb: number) { saveRunChoice({ ...runChoice.value, resources: { ...runChoice.value.resources, memGb: gb } }) }

// The memory the largest selected image needs (the spec's measured model): frame sizes come from the
// cached `/api/images/geometry` reads the param advisors already make.
const hpcFrames = ref<FrameSize[]>([])
watch([onHpc, () => props.selectedUids.join(',')], async ([on]) => {
  if (!on) return
  const uids = props.selectedUids
  const pu = projectMeta.current?.uid ?? ''
  const got = (await Promise.all(uids.map(u => frameGeometry(pu, u)))).filter((g): g is NonNullable<typeof g> => !!g)
  if (uids.join(',') === props.selectedUids.join(',')) hpcFrames.value = got   // a newer selection wins
}, { immediate: true })
const hpcMemory = computed(() => onHpc.value && taskDef.value && hpcForm.value
  ? hpcMemoryHint(taskDef.value, hpcFrames.value, hpcForm.value) : null)
// Where the cluster run goes (class → queue → host), or what is missing: read when hpc is picked and
// again when Preferences closes, since that is where the login and queues are edited.
const hpcSettings = ref<HpcSettings | null>(null)
const hpcSettingsError = ref('')
async function loadHpcSettings() {
  if (!onHpc.value) return
  try { hpcSettings.value = await fetchHpcSettings(projectMeta.current?.uid ?? ''); hpcSettingsError.value = '' }
  catch (e) { hpcSettingsError.value = e instanceof Error ? e.message : String(e) }
  void loadHpcEnv()
}
// Whether this version's Python env is on the cluster (one ssh call): asked when hpc is picked, once
// the settings are complete, and again when an install job ends. Out of date → warn with Install.
const hpcEnv = ref<HpcEnvStatus | null>(null)
async function loadHpcEnv() {
  if (!onHpc.value || hpcSettingsError.value || hpcTestBlocker(hpcSettings.value, 'run')) { hpcEnv.value = null; return }
  try { hpcEnv.value = await fetchHpcEnv() }
  catch { hpcEnv.value = null }   // the run itself refuses a missing env, naming the fix
}
const hpcEnvNote = computed(() => onHpc.value ? hpcRunEnvNote(hpcEnv.value) : null)
async function installClusterEnv() {
  try {
    await installHpcEnv()
    if (hpcEnv.value) hpcEnv.value = { ...hpcEnv.value, installing: true, error: '' }
  } catch (e) { log.error(e instanceof Error ? e.message : String(e), { source: 'hpc' }) }
}
const offHpcEnv = onHpcEnvChanged(() => void loadHpcEnv())
onUnmounted(offHpcEnv)
watch(onHpc, on => { if (on) void loadHpcSettings() }, { immediate: true })
watch(isPreferencesOpen, open => { if (!open) void loadHpcSettings() })
const hpcDestination = computed(() => {
  if (hpcSettingsError.value) return { short: 'Cluster settings could not be read', detail: hpcSettingsError.value, severity: 'warn' as const }
  return taskDef.value ? hpcRunDestination(hpcSettings.value, taskDef.value) : null
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
// `segment.cellposeMeasure` reports true from its first previewable step.
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

const projectUid = computed(() => projectMeta.current?.uid ?? '')

// `null` = the load did NOT happen (no project uid yet, a non-OK response, a throw). `{}` = the server
// answered and there is nothing saved. Collapsing the two into `{}` is what reset filled-in forms:
// `buildParamValues(def, {})` answers every param with its default, so a load that never happened was
// indistinguishable from a first run and silently overwrote the form. See `resolveInitialParams`.
async function fetchSavedParams(
  def: TaskDef, valueName = '',
): Promise<{ params: ParamValues; matched: boolean } | null> {
  if (!projectUid.value) return null
  const qs = new URLSearchParams({ projectUid: projectUid.value, fun: def.fun_name })
  if (drivingImageUid.value) qs.set('imageUid', drivingImageUid.value)
  if (setUid.value) qs.set('setUid', setUid.value)
  // ALL selected images, for the by-name lookup only. `imageUid` is empty unless exactly one is
  // selected (that is what "driving" means), and a batch segmentation is normally several — so
  // without this, asking for `Neutrophil`'s params asked only the set, which holds no run log.
  if (props.selectedUids.length) qs.set('imageUids', props.selectedUids.join(','))
  // The OUTPUT name being named, so the server can prefer what was last run UNDER it. `matched` says
  // it found one rather than falling back — the difference decides whether it is safe to replace a
  // form the user may have edited.
  if (valueName) qs.set('valueName', valueName)
  try {
    const res = await fetch(`/api/tasks/funparams?${qs.toString()}`)
    if (!res.ok) return null
    const d = await res.json() as { params?: ParamValues | null; matched?: boolean }
    return { params: d.params ?? {}, matched: d.matched === true }
  } catch { return null }
}

// Saved params are fetched from the server, so guard against a slower earlier request landing after
// a newer one (function/selection changed mid-flight).
let paramReqSeq = 0
async function initParams(def: TaskDef | undefined) {
  if (!def) return
  // Restore an in-progress draft for this exact scope first; only then fall back to server-saved
  // params + defaults. (Drafts are written on user edits only, so this never masks a newer save.)
  const draft = drafts.get(currentDraftKey.value)
  // Draft first, reconciled against the CURRENT spec instead of restored raw — a draft outlives a param
  // set change, and `undefined` is dropped by JSON.stringify, so a raw restore made new params vanish
  // from the run payload AND from the funParams record. All of that decision is `resolveInitialParams`.
  // Early-returned rather than folded into the call below so a draft costs no request — but the
  // DECISION is still the one helper, so the draft path cannot drift from the saved-record path.
  if (draft) {
    paramValues.value = resolveInitialParams(def, draft, null) as ParamValues
    refreshOptionsForForm(def)
    return
  }
  const seq = ++paramReqSeq
  // The name the form is about to show — its own defaults on a first render, or whatever the last
  // scope change left. Asking WITH it means a form that opens on "Tcell" opens with Tcell's params.
  const got = await fetchSavedParams(def, taskOutput(def, paramValues.value)?.name ?? '')
  const saved = got?.params ?? null
  if (seq !== paramReqSeq) return
  // `null` → the load did not happen, so LEAVE THE FORM ALONE rather than stamping defaults over it.
  // The watches below re-run this once the project/set/selection is known, which is the case that used
  // to arrive too late and find the form already reset.
  const next = resolveInitialParams(def, undefined, applyImportPrefills(def, saved))
  if (next !== null) { paramValues.value = next; refreshOptionsForForm(def) }
}

// Per-image seeds injected on TOP of `saved` before it hits `resolveInitialParams`. One case
// today: the OME-ZARR importer's `pyramidLevels` gets pre-filled from the shape-aware peek
// recommendation (`tasks/pyramidPeek.ts`), primed on add in `ManageImagesModule`. Seeding into
// `saved` — not mutating the spec default — means a user's previously-saved value wins, and a
// user's edit persists (drafts write on edit and load first).
function applyImportPrefills(def: TaskDef, saved: ParamValues | null): ParamValues | null {
  if (def.fun_name !== 'importImages.omezarr') return saved
  if (!useSettingsStore().importPyramidAdvisor) return saved
  const img = paramContext.value.images[0]
  if (!img?.oriPath) return saved
  const rec = cachedPyramidRecommendation(img.oriPath)
  if (typeof rec !== 'number') return saved      // undefined (not yet primed) OR null (nothing to say)
  if (saved && saved.pyramidLevels !== undefined && saved.pyramidLevels !== null) return saved
  return { ...(saved ?? {}), pyramidLevels: rec }
}

// Options were re-resolved only when the user EDITED a `triggersOptions` param, so a form POPULATED
// with one — restored from the last run, or from a draft — carried whatever the un-resolved spec had:
// an importer's column dropdowns came back empty for a file whose headers were read fine a minute
// earlier. Anything the form can decide for itself belongs in `showIf` and needs no request; this is
// only for what genuinely requires the server to look at something.
function refreshOptionsForForm(def: TaskDef) {
  const keys = collectTriggerKeys(def.params ?? [])
  const armed = keys.some(k => {
    const v = paramValues.value[k]
    return v !== undefined && v !== null && v !== '' && !(Array.isArray(v) && v.length === 0)
  })
  if (armed) optionRefetch.schedule(null)
}

// A `valueNameInput` the user has FINISHED entering (blur, or picking a suggestion) — the moment to
// restore that output's parameters. This is the point of the feature: naming `Tcell` again brings
// back the settings Tcell was segmented with, instead of whatever ran last.
//
// Only on COMMIT, never per keystroke: typing toward "Tcell2" passes through "Tcell", and swapping
// every other field mid-word would be worse than not having this at all.
//
// Only when the record was MATCHED. Without that guard the fallback (the last run's params) would be
// stamped over a form the user had just edited — the same bug class as the "my params go back to
// default" report below, arriving by a different route.
//
// Two-layer lookup for the same reason there are two writers:
//   1. `/api/tasks/funparams` — this project's ccid.json (image → set), the primary path.
//   2. Global-vault manifest — for a picker whose `optionsFrom` names a vault (`flowModels` today,
//      custom-cellpose training next). The record there is shared across sets and projects, so a
//      model trained under a different set silently didn't restore before this: the vault UI worked
//      (`FlowModelVault` → *Apply settings*), the picker didn't, and the two paths disagreed on the
//      same input. Dispatched in `utils/vaultManifest.ts` so a new vault plugs in with one entry.
//
// A NOTE either way. The old code was silent when nothing matched, so a user picking a name with
// no known record couldn't tell "reload skipped" from "reload didn't happen" from "no params exist".
async function onParamCommit(key: string, value: unknown) {
  const def = taskDef.value
  if (!def || typeof value !== 'string' || !value) return
  const seq = ++paramReqSeq

  let saved: ParamValues | null = null
  let source = ''

  const got = await fetchSavedParams(def, value)
  if (seq !== paramReqSeq) return
  if (got?.matched) { saved = got.params; source = value }

  if (!saved) {
    saved = await vaultManifestParams(findParamByKey(def.params, key), def.params, value)
    if (seq !== paramReqSeq) return
    if (saved) source = `${value} (vault manifest)`
  }

  if (!saved) {
    handoffNote.value = `No saved settings for "${value}"`
    return
  }
  const next = resolveInitialParams(def, undefined, saved)
  if (next === null) return
  // Keep the name the user just entered. A restored record carries the output name it was saved
  // with, which is normally the same string — but the fallback path (and a record saved before a
  // rename) is not, and silently rewriting the field the user just typed in is never right.
  paramValues.value = { ...next, [key]: value } as ParamValues
  drafts.set(currentDraftKey.value, paramValues.value)
  handoffNote.value = `Settings restored from ${source}`
}

// ── an offer of params from elsewhere ────────────────────────────────────────
// Something that knows a param set (today: the flow-model vault, whose manifest IS the form that
// produced the model) hands it over through `paramHandoff`. Applied here rather than pushed by the
// sender, because reconciling a param bag against the CURRENT spec is `resolveInitialParams`' job and
// there must not be a second answer to it.
//
// `take` consumes, so an offer is applied once: a remount or a selection change cannot re-apply it
// over edits the user has since made. It is also checked against the function on screen, so clicking
// in the vault while a different task is selected leaves the offer for the form that can use it —
// which is why the watcher covers `taskDef` as well as the offer itself.
const handoff = useParamHandoffStore()
const handoffNote = ref('')

function applyOffer() {
  const def = taskDef.value
  if (!def) return
  const offer = handoff.take(def.fun_name)
  if (!offer) return
  const next = resolveInitialParams(def, offer.values, null)
  if (next === null) return
  paramValues.value = next
  // A draft, because this is now an un-run edit like any other — navigating away must not lose it.
  drafts.set(currentDraftKey.value, paramValues.value)
  refreshOptionsForForm(def)
  handoffNote.value = offer.missing?.length
    ? `Settings from ${offer.source} — ${offer.missing.join(', ')} left unchanged`
    : `Settings from ${offer.source}`
}

watch([() => handoff.pending, taskDef], applyOffer, { immediate: true })

// Persist a param edit as a draft (USER edits only — never on programmatic init) so navigating away
// and back keeps it. Keyed to the current image→set scope.
function onParamEdit(key: string, value: unknown) {
  handoffNote.value = ''          // the form has moved on; the note described where it started
  const before = paramValues.value[key]
  paramValues.value[key] = value
  syncGroupOrders(key, before, value)
  drafts.set(currentDraftKey.value, paramValues.value)
  if (optionTriggerKeys.value.has(key)) optionRefetch.schedule(null)
}

// Reset every param to its spec default — the same values `buildParamValues(def, {})` produces on a
// first render. Persists as a draft so the reset is remembered across a navigation, and refreshes
// server-fed dropdowns for the same reason `onParamCommit` does (a defaulted `triggersOptions` key
// still owes its options a look). No confirmation dialog — a form reset writes nothing to disk.
function resetParamsToDefaults() {
  const def = taskDef.value
  if (!def) return
  handoffNote.value = ''
  paramValues.value = buildParamValues(def, {}) as ParamValues
  drafts.set(currentDraftKey.value, paramValues.value)
  refreshOptionsForForm(def)
}

// Which entries of a repeatable group run, and in what order. A sibling key of the group's own —
// `<groupKey>Order` — resolved away server-side by `_apply_group_order`, so no runner sees it.
function onGroupOrderEdit(key: string, value: string[]) {
  paramValues.value[key] = value
  drafts.set(currentDraftKey.value, paramValues.value)
}

// ── Copy settings from a previous run on another image ───────────────────────
// The user has segmented image A with a tuned config; now they want the SAME config on image B.
// funParams are banked per (image, value_name), so B's own "default" is B's settings, not A's — the
// only way to reach A's settings is to name A as the source. The picker (CopyFunParamsModal) surfaces
// the set's (image, value_name) records for the current fun; on pick we fetch the params using the
// existing image+valueName endpoint and route them through the SAME hand-off path the flow-model
// vault uses (`paramHandoff` → `applyOffer`), so reconciliation and the "Settings from …" note
// happen exactly once, in one place.
const copyOpen = ref(false)
async function onCopyPick(row: CopyFunParamsRow) {
  const def = taskDef.value
  if (!def || !projectUid.value) return
  const qs = new URLSearchParams({
    projectUid: projectUid.value, fun: def.fun_name,
    imageUid: row.imageUid, valueName: row.valueName,
  })
  try {
    const r = await fetch(`/api/tasks/funparams?${qs.toString()}`)
    if (!r.ok) { handoffNote.value = `Could not read settings from ${row.imageName}`; return }
    const d = await r.json() as { params?: ParamValues | null; matched?: boolean }
    if (!d.params) { handoffNote.value = `No saved settings for "${row.valueName}" on ${row.imageName}`; return }
    handoff.offer({
      funName: def.fun_name, values: d.params,
      source: `"${row.valueName}" on ${row.imageName}`,
    })
    // `applyOffer` is watching handoff.pending; it fills the form and sets the note.
  } catch {
    handoffNote.value = `Could not read settings from ${row.imageName}`
  }
}

// Editing a repeatable GROUP has to move its order with it. Reconciled here, with the group's keys
// from both before and after the edit, because those two are what tell the cases apart: a key
// missing from the selection is either an entry just added or one deliberately switched off, and
// from the new value alone they are identical — guessing switches a pass back on the moment another
// is added.
//
// Only an EXPLICIT order is reconciled. An unset one means "every entry, in order" (see
// `groupOrderKeys`), which stays true however many entries there are, so a group nobody has
// reordered never acquires a stored order at all.
function syncGroupOrders(editedKey: string, before: unknown, after: unknown) {
  const key = `${editedKey}Order`
  const current = paramValues.value[key]
  if (!Array.isArray(current)) return
  const keysOf = (v: unknown) =>
    Object.keys((v ?? {}) as Record<string, unknown>).sort((a, b) => Number(a) - Number(b))
  paramValues.value[key] = syncGroupOrder(keysOf(before), keysOf(after), current.map(String))
}

// ── Options that depend on the form ───────────────────────────────────────────────────────────────
// A param marked `triggersOptions` re-resolves the task's options server-side against the current
// form — an importer's file path, whose columns become the mapping fields' suggestions.
//
// Coalesced at the SINK, not the call site (docs/UI.md → Continuous controls): a path is TYPED, so a
// per-keystroke refetch would fire a request per character and land them out of order. `debouncedLatest`
// is the canonical scheduler for a request; the `isCurrent` guard means a superseded response is
// discarded rather than overwriting the options for the path the user has since finished typing.
function collectTriggerKeys(ps: TaskDef['params']): string[] {
  const out: string[] = []
  for (const p of ps ?? []) {
    if (p.triggersOptions) out.push(p.key)
    if (p.params) out.push(...collectTriggerKeys(p.params))   // sections nest
  }
  return out
}

const optionTriggerKeys = computed(
  () => new Set(taskDef.value ? collectTriggerKeys(taskDef.value.params ?? []) : []))

const optionRefetch = debouncedLatest<null>(async () => {
  const def = taskDef.value
  if (!def || !props.onReloadDefs) return
  // Read the form INSIDE the run, not at schedule time: the debounce means several keystrokes collapse
  // into one run, and it must resolve against the value the user ended on, not the one that first
  // triggered it.
  await props.onReloadDefs(flattenParams(def, paramValues.value) as Record<string, unknown>)
}, { wait: 400 })

onUnmounted(() => optionRefetch.cancel())

watch(selectedTask, (task) => {
  profileStorage.setItem(`cc-fn:${props.module}`, task)   // remember last-used function per module
})

// Populate the form from remembered funParams: on function change, and on project/set switch
// (setUid changes when the active set/project changes — this is what stops one project's params
// leaking into another). Narrowing the selection to a single image reloads that image's record.
//
// **`projectUid` is in here because it lands AFTER the sets.** `projectMeta.openProject` writes `sets`
// first and `current` second, on purpose (see that store) — so `setUid` goes non-empty while
// `projectMeta.current` is still null, this watch fires, and the load runs with no project uid. It
// used to answer `{}` for that, which `buildParamValues` turns into every param's default: the form
// was stamped with defaults and nothing re-ran the load once the project arrived, so the params
// stayed reset. That is the "my params go back to default when I navigate away" report, and it was
// never AF-specific. The load now reports `null` for "did not happen" (`fetchSavedParams`) and this
// re-runs it when the uid appears.
watch([taskDef, setUid, projectUid], () => initParams(taskDef.value), { immediate: true })
watch(drivingImageUid, (uid) => { if (uid) initParams(taskDef.value) })

// When defs load from the API (async), pick the saved function or fall back to the first.
watch(() => props.defs, (defs) => {
  if (!defs.length) return
  if (selectedTask.value && defs.some(d => d.task === selectedTask.value)) return
  const saved = profileStorage.getItem(`cc-fn:${props.module}`)
  // fall back to the TOP row as displayed — under headings that is the first group's first def
  selectedTask.value = (saved && defs.some(d => d.task === saved)) ? saved
    : (groupTaskDefs(defs)?.[0].defs[0] ?? defs[0]).task
}, { immediate: true })

// Axis gating — the frontend twin of the Julia gate. A task with `requires.axes` (e.g. tracking
// on ["T"]) is disabled in the picker + Run button unless every selected image carries those axes.
// The backend refuses to run anyway (TaskApplicabilityError); this removes the surprise.
const selectedImages = computed(() => paramContext.value.images)
// What the selected task is for, and when it fits, straight under the picker (task spec fields,
// docs/MODULES.md → `purpose`). A line naming a check is run over the selected images — ADVISORY,
// never a gate on Run (utils/taskDiscovery.ts). The lines fold in a CollapsibleSection whose
// open/closed state is remembered per user (open by default).
const useLines = computed(() => (taskDef.value?.useWhen ?? []).map(l => checkLine(l, 'use', selectedImages.value)))
const notLines = computed(() => (taskDef.value?.notWhen ?? []).map(l => checkLine(l, 'not', selectedImages.value)))
const WHEN_OPEN_KEY = 'cc-task-about-open'
const whenOpen = ref(profileStorage.getItem(WHEN_OPEN_KEY) !== '0')
watch(whenOpen, v => { try { profileStorage.setItem(WHEN_OPEN_KEY, v ? '1' : '0') } catch { /* ignore */ } })
// ── Task runner down ──────────────────────────────────────────────────────────
// Only when it is ENABLED but not answering — you turned it on, so a run silently falling back to the
// backend is a surprise: it works, but it dies with the next Restart, which is the one thing you
// enabled it to avoid. Nothing else surfaces that on this page.
// Polled slowly: it changes when a process starts or stops, not continuously, and this panel is on
// every module page.
const runnerDown = ref(false)
async function pollRunner() {
  try {
    const d = await (await fetch('/api/runner/status')).json()
    runnerDown.value = d?.enabled === true && d?.running !== true
  } catch { runnerDown.value = false }   // can't ask → don't cry wolf
}
// only while the page is visible — every kept-alive module page has one of these
useActiveInterval(pollRunner, 20000)

async function startRunner() {
  runnerDown.value = false                       // optimistic: the row shows the truth in a moment
  try { await fetch('/api/runner/restart', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: '{}' }) }
  catch { /* the poll below re-asserts reality */ }
  setTimeout(pollRunner, 2000)
}

const gatingReasonFor = (def: TaskDef) => taskGatingReason(def, selectedImages.value)
const activeTaskGatingReason = computed(() =>
  taskDef.value ? gatingReasonFor(taskDef.value) : ''
)

// A `required` param left empty, checked HERE rather than after the run. The server refuses these
// too, but only once the task has been queued and given a pool slot — so the user learned they had
// picked nothing from a log line, minutes later. First message only: the button is one line, and
// fixing the first usually reveals whether the rest matter.
const missingRequiredReason = computed(() =>
  taskDef.value ? (missingRequired(taskDef.value, paramValues.value, selectedImages.value)[0] ?? '') : '')

// run
const canRun = computed(() =>
  props.selectedUids.length > 0 && !!taskDef.value &&
  !activeTaskGatingReason.value && !missingRequiredReason.value
)

function run() {
  if (!canRun.value || !taskDef.value) return

  const def = taskDef.value
  const params = flattenParams(def, paramValues.value)

  // params are persisted server-side on run (per image + set); just remember the last-used function
  profileStorage.setItem(`cc-fn:${props.module}`, def.task)
  // KEEP the draft, as exactly what was submitted — do not clear it.
  //
  // THE "my params revert to default after I start a task" bug. Clearing it here assumed the
  // server-side record replaces it, but that record is not written synchronously, and starting or
  // finishing a run mutates the project store (`bumpDataVersion`, `refreshImageMeta` in `stores/ws`),
  // which re-fires the watches below and re-runs `initParams`. In that window there was no draft AND
  // no record, so the load answered `{"params":null}` — `{}` after `fetchSavedParams` — and
  // `buildParamValues(def, {})` is every param's spec DEFAULT. The form reset itself, on every task,
  // seconds after the user pressed Run.
  //
  // Writing the submitted values rather than leaving the previous draft: they are the same thing here
  // (the draft is what was just submitted), and being explicit means this stays correct if a future
  // caller transforms params on the way out. It converges with the server record — same values — and
  // the next edit replaces it as before.
  drafts.set(currentDraftKey.value, paramValues.value)

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
      params: params as Record<string, unknown>, projectUid: projectUid.value,
      pool: selectedPool.value,
    })
    ws.send({
      type: 'task:run', taskId: t.id, funName: def.fun_name, params,
      imageUid: rep, imageUids: uids, projectUid: projectUid.value,
      setUid: setUid.value, poolName: selectedPool.value,
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
      projectUid: projectUid.value,
      // what was ASKED for, shown while it waits; the first `task:status` replaces it with the pool
      // the scheduler actually queued it in (an unconfigured name falls back to cpu)
      pool:       onHpc.value ? 'hpc' : selectedPool.value,
    })

    ws.send({
      type:       'task:run',
      taskId:     t.id,
      funName:    def.fun_name,
      params:     params,
      imageUid:   uid,
      projectUid: projectUid.value,
      setUid:     setUid.value,
      poolName:   selectedPool.value,
      ...hpcRunFields(def, runChoice.value),
    })
    // Status is driven by the backend: 'running' if a slot was free, 'queued' otherwise.
  }
}

const runLabel = computed(() => {
  const n = props.selectedUids.length
  if (n === 0) return 'Select images to run'
  if (activeTaskGatingReason.value) return activeTaskGatingReason.value
  if (missingRequiredReason.value) return missingRequiredReason.value
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
  const running = activeTasks.value.filter(t => hpcShownStatus(t.status, t.hpcStage) === 'running').length
  const queued  = activeTasks.value.length - running
  if (!activeTasks.value.length) return ''
  return queued ? `${running} running · ${queued} queued` : `${running} running`
})

// This panel does NOT own its width — `CollapsiblePanel` (its host, via ModuleLayout's `#right`) does.
// It used to have its own `usePanelResize` + drag handle, which meant two widths and two stacked
// handles on the same edge: dragging the host's handle widened the host while this stayed pinned at
// its own stored 280px, so the content shifted instead of reflowing.
// `CollapsiblePanel`'s header already recorded this as the half-finished consolidation.

// ── Which half is expanded — the shared two-half panel primitive (utils/paneExpand.ts) ──
// Vertical space is the scarce one here: a long param list and a busy task list can't both fit on a
// laptop screen. Top half = this runner, bottom half = the module's task list.
const { pane, toggle: togglePane } = usePaneExpand('cc-taskrunner-pane')
</script>

<template>
  <aside class="task-runner" :class="'pane-' + pane">

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
      <template #actions><slot name="bar-actions" /></template>
    </PaneExpandBar>

    <!-- ── Empty state (server not ready / JSON parse error) ── -->
    <section v-if="!defs.length" class="runner-section defs-empty">
      <p class="defs-empty-msg cc-muted">No functions available — the server may still be starting.</p>
      <!-- Called with NO argument on purpose: `onReloadDefs(form?)` treats an argument as form state
           to resolve options against, and `@click="onReloadDefs"` would hand it the PointerEvent. -->
      <button v-if="onReloadDefs" class="cc-btn cc-btn-secondary" @click="() => onReloadDefs?.()">
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
        v-tooltip.bottom="'Select which analysis function to run on the selected images'"
      >
        <template v-if="fnGroups">
          <optgroup v-for="grp in fnGroups" :key="grp.title" :label="grp.title">
            <option
              v-for="d in grp.defs"
              :key="d.task"
              :value="d.task"
              :disabled="!!gatingReasonFor(d)"
              :title="gatingReasonFor(d) || d.purpose || undefined"
            >
              {{ d.label }}{{ gatingReasonFor(d) ? ` — ${gatingReasonFor(d)}` : '' }}
            </option>
          </optgroup>
        </template>
        <template v-else>
          <option
            v-for="d in defs"
            :key="d.task"
            :value="d.task"
            :disabled="!!gatingReasonFor(d)"
            :title="gatingReasonFor(d) || d.purpose || undefined"
          >
            {{ d.label }}{{ gatingReasonFor(d) ? ` — ${gatingReasonFor(d)}` : '' }}
          </option>
        </template>
      </select>

      <!-- what it is for, then when it fits: directly under the picker, above the Kiwi/env row. The
           fold is a bare inline toggle (.cc-section-toggle, as ParamRenderer's group sections), not
           CollapsibleSection: that one carries panel-rail chrome (a surface bar, its own padding) that
           does not sit inside a padded runner section. A check's finding gets its own row under its
           line, so a long line never shares its width with the finding. -->
      <div v-if="taskDef?.purpose" class="fn-about">
        <p class="fn-purpose cc-muted cc-fs-xs">{{ taskDef.purpose }}</p>
        <template v-if="useLines.length || notLines.length">
          <button class="fn-when-toggle cc-section-toggle cc-eyebrow cc-fs-2xs" @click="whenOpen = !whenOpen"
                  v-tooltip.right="whenOpen ? 'Hide when this task fits' : 'Show when this task fits'">
            <i :class="['pi', whenOpen ? 'pi-chevron-down' : 'pi-chevron-right']" />
            When to use
          </button>
          <div v-if="whenOpen" class="fn-when-body">
            <template v-for="block in [{ head: 'Use when', lines: useLines }, { head: 'Not when', lines: notLines }]"
                      :key="block.head">
              <div v-if="block.lines.length" class="fn-lines">
                <span class="fn-lines-head cc-muted cc-fs-2xs">{{ block.head }}</span>
                <ul class="fn-line-list cc-fs-xs">
                  <li v-for="l in block.lines" :key="l.text">
                    <span class="fn-line-text">{{ l.text }}</span>
                    <InlineNote v-if="l.severity && l.summary" class="fn-line-check cc-fs-2xs"
                                :short="l.summary" :severity="l.severity" />
                    <span v-else-if="l.summary" class="fn-line-check cc-muted cc-fs-2xs">{{ l.summary }}</span>
                  </li>
                </ul>
              </div>
            </template>
          </div>
        </template>
      </div>

      <div v-if="taskDef" class="fn-meta">
        <AddToKiwiButton :kiwi-ref="{ kind: 'task', funName: taskDef.fun_name }" tip="Add this function to Kiwi" />
        <ChipSelect v-if="canHpc" class="env-switch" variant="segmented" :options="TARGET_OPTIONS"
          :model-value="runChoice.target" aria-label="Where to run" @update:model-value="setTarget" />
        <template v-else>
          <span class="env-badge"
            v-for="env in taskDef.env"
            :key="env"
            v-tooltip.right="`Runs in the ${env} environment`">
            {{ env }}
          </span>
        </template>
        <span
          v-if="activeTaskGatingReason"
          class="env-badge"
          v-tooltip.right="'Selected images do not carry the axes this task needs'"
        >{{ activeTaskGatingReason }}</span>
      </div>
      <div v-if="onHpc && hpcForm" class="hpc-res cc-fs-xs">
        <label v-for="f in hpcFields" :key="f.key" class="hpc-res-cell">
          <span class="hpc-res-head">
            <span class="cc-muted">{{ f.label }}</span>
            <span class="hpc-res-val">{{ formatResource(f.key, hpcValue(f.key)) }}</span>
          </span>
          <input type="range" min="0" :max="hpcStops[f.key].length - 1" step="1" :aria-label="f.label"
                 :value="hpcStops[f.key].indexOf(hpcValue(f.key))"
                 @input="hpcDrag[f.key] = hpcStops[f.key][+($event.target as HTMLInputElement).value]"
                 @change="commitResource(f.key)" :disabled="hpcRuled(f.key)"
                 v-tooltip.bottom="hpcRuled(f.key)
                   ? `${hpcSettings?.site.gpu.cpusPerGpu} per GPU on this cluster (Preferences → HPC)`
                   : `${f.tip}; default ${formatResource(f.key, hpcDefaultsNow?.[f.key] ?? 0)}`" />
        </label>
      </div>
      <div v-if="hpcMemory" class="cc-row hpc-res-foot">
        <InlineNote class="hpc-note hpc-grow" :short="hpcMemory.short" :detail="hpcMemory.detail" :severity="hpcMemory.severity" />
        <button v-if="hpcMemory.suggestGb" class="cc-btn cc-btn-ghost cc-btn-micro" @click="setMemory(hpcMemory.suggestGb)"
                v-tooltip.bottom="'Set memory to the smallest size that fits'">Use {{ hpcMemory.suggestGb }} GB</button>
      </div>
      <div v-if="onHpc && hpcForm" class="cc-row hpc-res-foot">
        <InlineNote class="hpc-note hpc-grow" short="Per image; too small fails, too big queues longer" />
        <button v-if="hpcChanged" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="resetResources"
                v-tooltip.bottom="hpcResetTip"><i class="pi pi-undo" /></button>
      </div>
      <InlineNote v-if="onHpc && hpcDestination" class="hpc-note" :icon="hpcDestination.severity ? undefined : 'pi-server'"
                  :short="hpcDestination.short" :detail="hpcDestination.detail" :severity="hpcDestination.severity" />
      <div v-if="hpcEnvNote" class="cc-row hpc-res-foot">
        <InlineNote class="hpc-note hpc-grow" :short="hpcEnvNote.short" :detail="hpcEnvNote.detail" :severity="hpcEnvNote.severity" />
        <button v-if="hpcEnvNote.canInstall" class="cc-btn cc-btn-ghost cc-btn-micro" @click="installClusterEnv"
                v-tooltip.bottom="'Install this version\'s Python environment on the cluster, as a job'">Install on cluster</button>
      </div>
      <p v-if="outputEffectLabel" class="fn-effect cc-muted cc-fs-xs"
         v-tooltip.right="outputEffectTip">
        <i class="pi pi-info-circle" />
        <span>{{ outputEffectLabel }}</span>
      </p>
    </section>

    <!-- ── Parameters ── -->
    <section class="runner-section params-section" v-if="taskDef">
      <!-- what the form was just filled from, and what could not be filled. Dismissible because it
           describes an action, not a state — and cleared by the first edit. -->
      <p v-if="handoffNote" class="handoff-note cc-fs-xs">
        <i class="pi pi-sliders-h" />
        <span>{{ handoffNote }}</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" v-tooltip.left="'Dismiss'"
                @click="handoffNote = ''"><i class="pi pi-times" /></button>
      </p>
      <!-- Heading + a bare icon-button to zero the form back to spec defaults. The button follows
           the heading on the same row so a user reaching for it does not have to hunt — a common
           ask when a chain node was tuned once and the next run wants a clean slate. -->
      <div class="params-heading-row">
        <h3 class="section-heading cc-eyebrow cc-fs-2xs">Parameters</h3>
        <div class="params-heading-actions">
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  v-tooltip.left="'Copy settings from a previous run on another image'"
                  @click="copyOpen = true"><i class="pi pi-copy" /></button>
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  v-tooltip.left="'Reset to defaults'"
                  @click="resetParamsToDefaults"><i class="pi pi-refresh" /></button>
        </div>
      </div>
      <div class="params-list" data-guide="task.params">
        <ParamRenderer
          v-for="p in taskDef.params"
          :key="p.key"
          :param="p"
          :modelValue="paramValues[p.key]"
          @update:modelValue="onParamEdit(p.key, $event)"
          @commit="onParamCommit"
          @update:groupOrder="onGroupOrderEdit"
          :context="paramContext"
        />
      </div>
    </section>

    <!-- ── Run + Concurrency ── -->
    <section class="runner-section run-section">
      <!-- Run and Preview share a row: Preview is the choice Run informs, and as a small ghost icon
           BELOW a full-width primary it was invisible. Same height, so it reads as a peer. -->
      <!-- Short = the problem, the action lives in the tooltip and the button beside it. -->
      <div v-if="runnerDown" class="runner-down cc-muted-warn cc-fs-xs">
        <i class="pi pi-exclamation-triangle" />
        <span v-tooltip.left="'Runs in the backend instead — this one stops if you restart'">Task runner down</span>
        <button class="cc-btn cc-btn-bare cc-fs-xs" @click="startRunner"
                v-tooltip.left="'Start the task runner'">Start</button>
      </div>

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

        <!-- Preview: run these params on the region viewer is showing, before committing to a full run -->
        <TaskPreviewControls
          :project-uid="projectMeta.current?.uid ?? ''"
          :image-uid="drivingImageUid"
          :value-name="previewValueName(taskDef, previewParams)"
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
          v-tooltip.bottom="'Which resource pool this run queues in'" />
        <button ref="throttleBtn" class="pool-throttle cc-btn cc-btn-ghost cc-btn-icon"
          :class="{ 'cc-btn-on cc-btn-on-solid': throttleOpen }"
          @click="throttleOpen = !throttleOpen"
          v-tooltip.left="'Throttle — how many tasks run at once, and how wide each may go'">
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

    <CopyFunParamsModal v-if="copyOpen && taskDef && projectUid && setUid"
      :projectUid="projectUid" :setUid="setUid"
      :fun="taskDef.fun_name" :funLabel="taskDef.label"
      :currentImageUid="drivingImageUid"
      @pick="onCopyPick" @close="copyOpen = false" />

  </aside>
</template>

<style scoped>
.handoff-note {
  display: flex; align-items: center; gap: 0.4rem;
  margin: 0 0 0.5rem; padding: 0.3rem 0.45rem;
  border-radius: var(--cc-radius-sm);
  background: var(--cc-surface-2); color: var(--cc-text-dim);
}
.handoff-note span { flex: 1; }
.task-runner {
  /* fills the host panel, which owns the width — see the note by the `pane` setup */
  flex: 1;
  min-width: 0;
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
/* Heading on the left, trailing icon-buttons grouped on the right so they stay together no matter
   how many there are — `justify-content: space-between` with three children would strand a middle
   button in the centre. */
.params-heading-row {
  display: flex; align-items: center; justify-content: space-between;
  margin: 0 0 0.5rem;
}
.params-heading-row .section-heading { margin: 0; }
.params-heading-actions { display: flex; align-items: center; gap: 0.15rem; }

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
.fn-effect {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  margin: 0.3rem 0 0;
}
.fn-effect .pi { font-size: var(--cc-fs-2xs); }
.fn-about { margin-top: 0.3rem; min-width: 0; }
.fn-purpose { margin: 0; overflow-wrap: anywhere; }
/* + cc-section-toggle (row) + cc-eyebrow (label tier); only the inset is ours */
.fn-when-toggle { padding: 0.3rem 0 0.1rem; }
/* the same indented rule as ParamRenderer's .group-section-body */
.fn-when-body {
  padding-left: 0.4rem;
  border-left: 2px solid var(--cc-border);
  margin-left: 0.1rem;
}
.fn-lines + .fn-lines { margin-top: 0.3rem; }
.fn-lines-head { display: block; }
.fn-line-list { margin: 0.1rem 0 0; padding-left: 0.9rem; line-height: 1.35; }
.fn-line-list li { min-width: 0; overflow-wrap: anywhere; }
.fn-line-list li + li { margin-top: 0.1rem; }
.fn-line-text { display: block; }
.fn-line-check { display: flex; margin-top: 0.05rem; }
.runner-down {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  margin-bottom: 0.35rem;
}

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

.hpc-res {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.35rem 1rem;
  margin-top: 0.4rem;
  max-width: 22rem;
}
.hpc-res input[type="range"] { width: 100%; min-width: 0; }
.hpc-res-cell { display: flex; flex-direction: column; gap: 0.1rem; min-width: 0; }
.hpc-res-head { display: flex; justify-content: space-between; gap: 0.4rem; }
.hpc-res-val { font-family: var(--cc-mono); }
.hpc-res-foot { margin-top: 0.35rem; max-width: 22rem; align-items: flex-start; }
.hpc-grow { flex: 1; min-width: 0; }
/* the same shape as a param advisory (ParamRenderer `.param-advisory`): the panel's text size, icon on the first line */
.hpc-note { display: flex; align-items: flex-start; margin-top: 0.35rem; }
.hpc-res-foot .hpc-note { margin-top: 0; }

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
  background: var(--cc-accent-fill);
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
