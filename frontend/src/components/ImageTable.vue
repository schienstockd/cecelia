<script setup lang="ts">
import { useInlineEdit } from '../composables/useInlineEdit'
import { ref, computed, watch, onMounted } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useProjectStore, type CciaImage } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { useTaskStore, type TaskStatus } from '../stores/tasks'
import { useTaskDefsStore } from '../stores/taskDefs'
import { metadataWarning } from '../lib/imageMetadataWarnings'
import { qcSummary } from '../lib/qc'
import { isExcluded, isIncluded, isImported, isStarred } from '../utils/inclusion'
import { timelapseDuration, sortImages } from '../utils/imageTable'
import { type SortState } from '../utils/sortRows'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import { useCopyFlash } from '../composables/useCopyFlash'
import { lastSuccessfulRun, funModuleLabel } from '../utils/runLog'
import { moduleColor, moduleIdFromFun } from '../utils/taskModule'
import { useNapariOpen } from '../composables/useNapariOpen'
import PhysicalSizeDialog from './PhysicalSizeDialog.vue'
import ImageMetadataDialog from './ImageMetadataDialog.vue'
import CropDialog from './CropDialog.vue'
import TeleportPopover from './TeleportPopover.vue'

const props = defineProps<{
  setUid: string
  module?: string      // when provided, shows a per-module status column
  showAttrs?: boolean  // show per-channel + attr columns
  editableMeta?: boolean // allow inline editing of attr + channel-name cells (Metadata page only)
  filterUids?: string[] // when provided, restricts visible rows to these UIDs
  singleSelect?: boolean // radio-style: at most one image selected (e.g. gating)
  selectionScope?: string // namespace for remembering the selection (e.g. module name)
}>()
const emit = defineEmits<{ (e: 'selectionChange', uids: string[]): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()
const route       = useRoute()
const router      = useRouter()
const taskStore   = useTaskStore()
const taskDefs    = useTaskDefsStore()
onMounted(() => { taskDefs.ensureLoaded() })   // for pretty fun/module labels in the Last-run column

// opens right where you are — no page navigation needed
const physSizeDialogUid = ref<string | null>(null)

// read-only "all metadata for this image" dialog (info icon on every row); resolved to the row object
const metaDialogUid = ref<string | null>(null)
const metaDialogImg = computed(() =>
  metaDialogUid.value ? (images.value.find(i => i.uid === metaDialogUid.value) ?? null) : null)

// crop dialog (per-image, napari-free) — draw a rectangle on the coloured MIP, set z/t, save a new image.
// Import page only: crop CREATES an image, which is an import-time operation (see the actions menu).
const cropDialogUid = ref<string | null>(null)
const cropDialogImg = computed(() =>
  cropDialogUid.value ? (images.value.find(i => i.uid === cropDialogUid.value) ?? null) : null)

// Two distinct affordances, kept visually separate: the warning (any module, always visible when
// flagged) sits in front of the name where it's impossible to miss; the neutral "open editor" icon
// lives after the name alongside the other row-hover actions (copy UID) on every row — flagged or
// not — on the pages where reviewing/propagating physical size is a primary task, so a known-good
// image can be opened deliberately and used as the Copy/Fill-flagged reference.
function warnIconFor(img: CciaImage): { tip: string } | null {
  const w = metadataWarning(img)
  return w ? { tip: w.short } : null
}
// QC badge — advisory "we processed this but the output looks off" (docs/todo/QC_PLAN.md). Distinct
// from the metadata warning: any module can emit it, and it's non-blocking (hover for detail).
function qcFor(img: CciaImage): { short: string; long: string; level: 'info' | 'warn' } | null {
  return qcSummary(img)
}
function pageIconFor(): { tip: string } | null {
  if (props.module === 'metadata' || props.module === 'import')
    return { tip: 'View or edit physical size & timing' }
  return null
}

// ── Selection ─────────────────────────────────────────────────────────────────

const selected  = ref<Set<string>>(new Set())
const napariLoading = ref<Set<string>>(new Set())
// Copy UID from the row's actions menu. The menu closes on click, so there's nothing to flash —
// just the copy (via the shared helper, which keeps the non-secure-context fallback).
const { copy: copyToClipboard } = useCopyFlash()
const copyUid = (uid: string) => copyToClipboard(uid)

// ── Inline cell editing ─────────────────────────────────────────────────────────
// One generic core (click a cell → edit → Enter/blur commits, Esc cancels) reused by attributes,
// channel names, AND the exclusion note. Each field only supplies how to persist its value (`save*`
// below), so there's no per-field copy of the edit lifecycle. `key` is namespaced per field type.
// Edit-in-place. This component's version WAS the general one — key, current, save callback, with
// the still-this-cell and unchanged guards — so it moved to `composables/useInlineEdit` and the other
// three hand-rolled copies (NotebookTable, PopulationManager, FlowModelVault) adopted it.
// aliased: this component already has a `commit()` for the row SELECTION
const { draft: editValue, start, cancel: cancelEdit, commit: commitInlineEdit,
        focusInput: focusEditInput, isEditing: isEditingKey } = useInlineEdit()
const cellKey = (uid: string, key: string) => `${uid}:${key}`
const isEditing = (uid: string, key: string) => isEditingKey(cellKey(uid, key))
const startEdit = (uid: string, key: string, current: string) => start(cellKey(uid, key), current)
// a note or a channel name MAY be cleared, so the empty case is a real save here
const commitEdit = (uid: string, key: string, current: string, save: (val: string) => Promise<void>) =>
  commitInlineEdit(cellKey(uid, key), current, save)

// Per-field savers — the only thing that differs between editable cells.
async function saveAttr(img: CciaImage, key: string, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  try {
    const res = await fetch('/api/images/attr/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, attrName: key, values: { [img.uid]: val } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
    // reflect what was STORED (the route trims), not the raw input — otherwise the cell would show
    // " a " while the file holds "a"
    const stored = await res.json().catch(() => ({})) as { values?: Record<string, string> }
    project.setAttrValues(key, stored.values ?? { [img.uid]: val })
  } catch (e) {
    log.error(`Failed to set ${key}: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}
// Channel names are one list per image; editing a single column replaces that index (1-based) and
// re-sends the whole list (the /channelnames endpoint is list-valued). Pads if naming a later channel.
function channelEditable(img: CciaImage, idx: number): boolean {
  return idx <= Math.max(img.channelNames?.length ?? 0, img.sizeC ?? 0)
}
async function saveChannel(img: CciaImage, idx: number, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const names = [...(img.channelNames ?? [])]
  while (names.length < idx) names.push('')
  names[idx - 1] = val
  try {
    const res = await fetch('/api/images/channelnames', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUids: [img.uid], channelNames: names }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
    project.updateImageMeta(img.uid, { channelNames: names })     // reflect immediately
  } catch (e) {
    log.error(`Failed to set channel ${idx}: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}
async function saveNote(img: CciaImage, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const prev = img.note ?? ''
  project.setInclusion(img.uid, { note: val })                   // reflect immediately
  try {
    const res = await fetch('/api/images/inclusion/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, values: { [img.uid]: { note: val } } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
  } catch (e) {
    project.setInclusion(img.uid, { note: prev })                // revert on failure
    log.error(`Failed to save note: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}

// ── Star ──────────────────────────────────────────────────────────────────────
// A plain bookmark — "I like this one". Any number of images can be starred, and nothing downstream
// reads it: it drives the Starred row filter (utils/rowFilters.ts) and nothing else. Same shape as
// include/exclude below, so it rides the same per-image flags route.
async function toggleStarred(img: CciaImage) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const starred = !isStarred(img)
  project.setInclusion(img.uid, { starred })                  // reflect immediately
  try {
    const res = await fetch('/api/images/inclusion/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, values: { [img.uid]: { starred } } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
  } catch (e) {
    project.setInclusion(img.uid, { starred: !starred })      // revert on failure
    log.error(`Failed to star image: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}

// ── Include / exclude ─────────────────────────────────────────────────────────
// Excluded images stay visible but greyed, can't be selected, and are skipped by every run.
const NOTE_KEY = '__note'

// run-history popover (cog after the uid) — automatic per-image provenance (project store `runLog`).
// Uses the shared TeleportPopover (escapes the table's scroll clipping; positions from the cog).
const runLogUid = ref<string | null>(null)
const runLogAnchor = ref<HTMLElement | null>(null)   // the clicked cog (drives popover placement)
const runLogImg = computed(() => runLogUid.value ? (images.value.find(i => i.uid === runLogUid.value) ?? null) : null)
// v-model for TeleportPopover: open when a uid is set; the component sets false on outside-click/Escape
const runLogOpen = computed({ get: () => runLogUid.value !== null, set: v => { if (!v) runLogUid.value = null } })
const fmtRunAt = (at: string) => (at ?? '').replace('T', ' ')

// The task-manager-style tag shown next to the UID — the last SUCCESSFUL run on this image (a failed
// run left no output, so it isn't the image's state). Lets the user see at a glance what was last
// done (e.g. "which ones did I already denoise?") without opening the run-history popover. Coloured
// per module via the shared palette so it reads the same as the task manager. null = no successful run.
function lastRunTag(img: CciaImage) {
  const e = lastSuccessfulRun(img.runLog)
  if (!e) return null
  const colour = moduleColor(moduleIdFromFun(e.fun))
  return {
    module: funModuleLabel(e.fun),           // e.g. "Cleanup"
    fun: taskDefs.labelFor(e.fun),           // e.g. "Cellpose correct" (falls back to fun tail)
    style: { background: colour + '22', color: colour, borderColor: colour + '55' },
    tip: `Last run: ${e.fun}${e.valueName ? ` → ${e.valueName}` : ''} · ${fmtRunAt(e.at)}`,
  }
}
// ── Row actions overflow menu (⋯) ───────────────────────────────────────────────
// Collapses the per-row action icons (metadata, crop, copy-UID, include, run-log) into one popover so
// the name column can stay narrow. Shares the same TeleportPopover as the run-log (escapes the table's
// scroll/transform clipping). PER-IMAGE actions only — the file operations that apply to a SELECTION
// (copy / move / remove) live in the Import page's action bar (ImageFileActions.vue), which is also
// what keeps this menu short.
const actionsUid    = ref<string | null>(null)
const actionsAnchor = ref<HTMLElement | null>(null)   // the clicked ⋯ button (drives placement)
const actionsImg    = computed(() => actionsUid.value ? (images.value.find(i => i.uid === actionsUid.value) ?? null) : null)
const actionsOpen   = computed({ get: () => actionsUid.value !== null, set: v => { if (!v) actionsUid.value = null } })
function toggleActions(uid: string, e: MouseEvent) {
  if (actionsUid.value === uid) { actionsUid.value = null; return }
  actionsAnchor.value = e.currentTarget as HTMLElement
  actionsUid.value = uid
}
// Run the chosen action, THEN close the menu. Order matters: the item closures read `actionsImg`,
// a computed derived from `actionsUid` — clearing it first would null `actionsImg` and the closure's
// `actionsImg!.uid` would throw (silently swallowing the click). Run first, close after.
function runAction(fn: () => void) { fn(); actionsUid.value = null }
// open run-history from the menu, re-anchored to the same ⋯ button the menu came from
function openRunLogFromMenu(uid: string) {
  runLogAnchor.value = actionsAnchor.value
  actionsUid.value = null
  runLogUid.value = uid
}
async function setIncluded(img: CciaImage, included: boolean) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  project.setInclusion(img.uid, { included })                 // reflect immediately
  if (!included && selected.value.has(img.uid)) {             // drop from selection on exclude
    selected.value.delete(img.uid)
    selected.value = new Set(selected.value)
    commit()
  }
  try {
    const res = await fetch('/api/images/inclusion/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, values: { [img.uid]: { included } } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
  } catch (e) {
    project.setInclusion(img.uid, { included: !included })     // revert on failure
    log.error(`Failed to ${included ? 'include' : 'exclude'} image: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}

const images = computed(() => {
  let all = project.sets.find(s => s.uid === props.setUid)?.images ?? []
  if (props.filterUids) {
    const keep = new Set(props.filterUids)
    all = all.filter(i => keep.has(i.uid))
  }
  const s = project.getImageSort(scope.value, props.setUid)
  return s ? sortImages(all, s.key, s.dir) : all
})

// ── Remembered selection ────────────────────────────────────────────────────
// Persist the checkbox selection in the project store (keyed by scope + set) so it survives
// navigating away from the page and back. `commit` is called after every change; `seed` restores
// it on mount and when the set switches (dropping any UIDs no longer present, and capping to one
// in single-select mode).
const scope = computed(() => props.selectionScope ?? 'default')

// ── Sort ──────────────────────────────────────────────────────────────────────
// The header affordance and the asc → desc → off cycle are SelectionTable's; what stays here is where
// the choice LIVES (per page + set in the project store, so it survives navigating away) and how it is
// applied (the pure, tested `sortImages`). See `sortModel` below.

function commit() {
  project.setImageSelection(scope.value, props.setUid, [...selected.value])
  emit('selectionChange', [...selected.value])
}
function seed() {
  const stored = project.getImageSelection(scope.value, props.setUid)
  const imgs = images.value
  // keep only stored uids that are still present AND still included (an image excluded while away
  // shouldn't come back selected)
  let uids = imgs.length
    ? stored.filter(u => imgs.some(i => i.uid === u && (canSelectExcluded.value || isIncluded(i))))
    : stored
  if (props.singleSelect && uids.length > 1) uids = [uids[0]]
  selected.value = new Set(uids)
  emit('selectionChange', [...selected.value])
}
onMounted(seed)
watch(() => props.setUid, seed)
// Re-seed when the stored selection is changed from OUTSIDE the table (e.g. the cluster page's
// "select clustered images" button writes the store directly). Guard against our own commit()
// writes: only re-seed when the store differs from the current checkbox set, so this never loops.
watch(() => project.getImageSelection(scope.value, props.setUid).join(','), (csv) => {
  const stored = csv ? csv.split(',') : []
  if (stored.length === selected.value.size && stored.every(u => selected.value.has(u))) return
  seed()
})

// On the import + metadata pages excluded images ARE selectable (you curate/edit metadata there,
// incl. on excluded ones); everywhere else selection is the runnable (included) subset only.
const canSelectExcluded = computed(() => props.module === 'import' || props.module === 'metadata')

// Select-all and its tri-state header box are SelectionTable's; which rows it may reach is stated
// here, as `unselectableUids`.

// Quick way to batch-fix physical-size/timing warnings: select every flagged image in one click,
// then open a clean reference image's dialog and Copy/Fill flagged onto exactly this selection.
const flaggedUids = computed(() => images.value.filter(i => metadataWarning(i) && isIncluded(i)).map(i => i.uid))
// "Active" = the current selection IS exactly the flagged set — drives both the toggle behaviour
// and the icon colour (gray = not applied, amber = applied).
const flaggedActive = computed(() =>
  flaggedUids.value.length > 0 &&
  selected.value.size === flaggedUids.value.length &&
  flaggedUids.value.every(u => selected.value.has(u))
)
function selectFlagged() {
  selected.value = flaggedActive.value ? new Set() : new Set(flaggedUids.value)
  commit()
}

// For images imported before physical-size/timing metadata was tracked in ccid.json (or whose
// meta lost these fields): the OME-ZARR itself is already correct, so re-derive `meta` straight
// from it (same reader the importer uses) instead of asking the user to re-import or type values
// back in by hand.
const resyncing = ref(false)
async function resyncFlagged() {
  const uids = flaggedUids.value
  const projectUid = projectMeta.current?.uid
  if (!uids.length || !projectUid || resyncing.value) return
  resyncing.value = true
  try {
    const res = await fetch('/api/images/meta/resync', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUids: uids }),
    })
    const body = await res.json().catch(() => ({})) as { ok?: boolean; images?: Record<string, Partial<CciaImage>>; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    for (const [uid, img] of Object.entries(body.images ?? {})) {
      project.updateImageMeta(uid, {
        physicalSizeX: img.physicalSizeX,
        physicalSizeY: img.physicalSizeY,
        physicalSizeZ: img.physicalSizeZ,
        physicalSizeUnit: img.physicalSizeUnit,
        physicalSizeZCorrected: img.physicalSizeZCorrected,
        timeIncrement: img.timeIncrement,
        timeIncrementUnit: img.timeIncrementUnit,
      })
    }
    log.info(`Re-read physical size & timing from file for ${uids.length} image(s).`, { source: 'import' })
  } catch (e) {
    log.error(`Failed to resync metadata: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  } finally {
    resyncing.value = false
  }
}

// ── Napari ────────────────────────────────────────────────────────────────────

const { openInNapari: napariOpen } = useNapariOpen()   // shared open path (see composable)
async function openInNapari(imageUid: string) {
  // reload short-circuits inside the composable too; skip the loading spinner for a reload
  if (project.napariImageUid === imageUid) { project.requestNapariReload(); return }
  napariLoading.value = new Set([...napariLoading.value, imageUid])
  try {
    await napariOpen(imageUid, props.setUid)
  } finally {
    napariLoading.value = new Set([...napariLoading.value].filter(u => u !== imageUid))
  }
}

// ── Status ────────────────────────────────────────────────────────────────────

function imageModuleStatus(img: CciaImage): TaskStatus | 'pending' | null {
  if (!props.module) return null
  const t = taskStore.forModule(props.module, projectMeta.current?.uid).find(t => t.imageUid === img.uid)
  if (t) return t.status
  if (props.module === 'import') {
    const s = img.status as string
    if (s === 'converting') return 'running'
    if (s === 'done')       return 'done'
    if (s === 'failed')     return 'failed'
    return 'pending'
  }
  return null
}

const statusConfig: Record<string, { label: string; cls: string }> = {
  pending:   { label: 'Pending',   cls: 'st-pending'  },
  queued:    { label: 'Queued',    cls: 'st-queued'   },
  running:   { label: 'Active',    cls: 'st-running'  },
  done:      { label: 'Done',      cls: 'st-done'     },
  failed:    { label: 'Failed',    cls: 'st-failed'   },
  cancelled: { label: 'Cancelled', cls: 'st-pending'  },
}

// ── Dynamic columns (attrs + channels) ───────────────────────────────────────

// Max channel count across the set — determines how many channel columns to show.
const channelCount = computed(() => {
  if (!props.showAttrs) return 0
  return images.value.reduce((max, img) =>
    Math.max(max, img.channelNames?.length ?? 0, img.sizeC ?? 0), 0)
})

// 1-based channel indices, e.g. [1, 2, 3]
const channelIndices = computed(() =>
  Array.from({ length: channelCount.value }, (_, i) => i + 1)
)

// Dimension columns only appear when the set actually has a z-stack / timelapse (mirrors the
// metadata-warning z>1 / t>1 tests), so 2D single-timepoint sets aren't cluttered with empty columns.
const anyZStack    = computed(() => images.value.some(i => (i.sizeZ ?? 0) > 1))
const anyTimelapse = computed(() => images.value.some(i => (i.sizeT ?? 0) > 1))

// Union of attr keys across the set, sorted.
const attrKeys = computed(() => {
  if (!props.showAttrs) return []
  const keys = new Set<string>()
  for (const img of images.value)
    for (const k of Object.keys(img.attr ?? {})) keys.add(k)
  return [...keys]
})

// ── Column resize ─────────────────────────────────────────────────────────────

// Keyed by column id (e.g. 'name', 'ch-1', 'attr-condition')

const DEFAULT_WIDTHS: Record<string, number> = {
  name: 160,   // narrower now the per-row actions collapse into one ⋯ menu (drag-resizable)
}
function defaultWidth(key: string): number {
  if (DEFAULT_WIDTHS[key]) return DEFAULT_WIDTHS[key]
  if (key.startsWith('ch-'))   return 90
  if (key.startsWith('attr-')) return 110
  return 100
}
// ── Columns ───────────────────────────────────────────────────────────────────
// The table itself is the shared `SelectionTable` (docs/UI.md): it owns the header, the checkboxes and
// their tri-state select-all, the sticky left columns, the resize handles and the row hit target. What
// stays here is what is ABOUT an image — every cell comes back through a `#cell-` slot.
//
// A column key IS its sort key, so the cycle the table reports goes straight to `sortImages` — hence
// `attr:${k}` rather than the old `attr-${k}` width key. Widths therefore live under a new storage key;
// the old one would have mapped the wrong column.
const COLUMNS = computed<SelectionColumn[]>(() => [
  { key: 'viewer', label: '', fixed: true, width: 32, sticky: true },
  { key: 'name', label: 'Name', sortable: true, sticky: true, width: defaultWidth('name') },
  ...channelIndices.value.map(idx => ({ key: `ch:${idx}`, label: String(idx), width: 90 })),
  ...attrKeys.value.map(k => ({ key: `attr:${k}`, label: k, sortable: true, width: 110 })),
  ...(!props.showAttrs ? [{ key: 'ch', label: 'Ch', sortable: true, fixed: true, width: 48 }] : []),
  ...(anyZStack.value ? [{ key: 'z', label: 'Z', sortable: true, fixed: true, width: 48 }] : []),
  ...(anyTimelapse.value
    ? [{ key: 'duration', label: 'Duration', sortable: true, fixed: true, width: 90 }] : []),
  ...(props.module ? [{ key: 'status', label: 'Status', fixed: true, width: 90 }] : []),
])

// Column widths are persisted, so a drag that leaves one unusably narrow needs a way back. The action
// is SelectionTable's (it owns the widths); the button sits in the Name header, where this table's
// other batch affordances already are.
const tableEl = ref<{ resetWidths: () => void } | null>(null)
const resetLayout = () => tableEl.value?.resetWidths()

// The sort, held where it always was — per (scope, set) in the project store, ordered by the pure
// `sortImages`. The table renders the affordance and reports the cycle; `v-model:sort` is what lets it
// do that without owning the state (see SelectionTable → `sort`).
const sortModel = computed<SortState>({
  get: () => project.getImageSort(scope.value, props.setUid),
  set: v => project.setImageSort(scope.value, props.setUid, v),
})

// Set ↔ array. The stored selection, the excluded guard and the single-select cap are all this
// component's rules, so the table is handed a plain list and hands one back.
const selectedList = computed<string[]>({
  get: () => [...selected.value],
  set: v => {
    selected.value = new Set(props.singleSelect ? v.slice(-1) : v)
    commit()
  },
})
// Excluded images are not runnable, so they cannot be checked — except on Import/Metadata, where
// curating them IS the job.
const unselectableUids = computed(() =>
  canSelectExcluded.value ? [] : images.value.filter(isExcluded).map(i => i.uid))

</script>

<template>
  <div v-if="images.length === 0" class="cc-empty cc-empty-lg">
    <i class="pi pi-images empty-icon" />
    <p class="empty-title">No images yet</p>
    <p class="empty-hint">Import your first image to get started.<br>
      Cecelia reads OME-ZARR, CZI, LIF, ND2 and most microscopy formats (anything bioformats2raw can read).</p>
    <button v-if="route.path !== '/import'" class="cc-btn cc-btn-primary empty-cta" @click="router.push('/import')">
      <i class="pi pi-plus" /> Import image
    </button>
  </div>

  <div v-else class="table-scroll">
  <!-- The canonical table (docs/UI.md). It owns the header, the checkboxes and their tri-state
       select-all, the sticky left columns, the resize handles and the row hit target; every cell below
       is this component's, through a `#cell-` slot. -->
  <SelectionTable
    ref="tableEl"
    class="image-table"
    selection-mode="multi"
    :select-all="!singleSelect"
    :columns="COLUMNS"
    :rows="images"
    id-key="uid"
    v-model:selected="selectedList"
    v-model:sort="sortModel"
    :disabled-ids="unselectableUids"
    column-width-key="cc-imagetable-cols"
    :row-class="img => ({ 'row-excluded': isExcluded(img) })"
    :row-tooltip="img => isExcluded(img) && !canSelectExcluded
      ? 'Excluded — include it to select for a run' : `Select ${img.name}`"
  >
    <!-- Batch-fix affordances live in the NAME header, where the images they act on are read. -->
    <template #head-name>
      <button v-if="!singleSelect && flaggedUids.length" class="select-flagged-btn cc-btn cc-btn-bare cc-btn-icon"
        :class="{ active: flaggedActive }" @click.stop="selectFlagged"
        v-tooltip.bottom="flaggedActive ? 'Deselect flagged images' : `Select all ${flaggedUids.length} flagged image(s)`">
        <i class="pi pi-exclamation-triangle" />
      </button>
      <button v-if="flaggedUids.length && (module === 'metadata' || module === 'import')"
        class="select-flagged-btn cc-btn cc-btn-bare cc-btn-icon" :disabled="resyncing"
        @click.stop="resyncFlagged"
        v-tooltip.bottom="`Re-read size & timing from file for ${flaggedUids.length} flagged image(s)`">
        <i :class="['pi', resyncing ? 'pi-spin pi-spinner' : 'pi-sync']" />
      </button>
      <button class="select-flagged-btn cc-btn cc-btn-bare cc-btn-icon" @click.stop="resetLayout"
        v-tooltip.bottom="'Reset the column widths'">
        <i class="pi pi-arrows-h" />
      </button>
    </template>

    <!-- napari eye -->
    <template #cell-viewer="{ row: img }">
      <button
        class="viewer-btn"
        :class="{ 'viewer-active': project.napariImageUid === img.uid }"
        :disabled="napariLoading.has(img.uid) || !isImported(img)"
        @click.stop="openInNapari(img.uid)"
        v-tooltip.right="!isImported(img)
          ? 'Import this image first'
          : project.napariImageUid === img.uid
            ? 'Currently shown in Napari — click to reload'
            : 'Open this image in Napari viewer'"
      >
        <i v-if="napariLoading.has(img.uid)" class="pi pi-spin pi-spinner" />
        <i v-else class="pi pi-eye" />
      </button>
    </template>

    <!-- name: three stacked rows — the name and its badges, the uid + last run, the note -->
    <template #cell-name="{ row: img }">
      <span class="name-row">
        <button v-if="warnIconFor(img)" class="warn-icon-btn cc-btn cc-btn-bare cc-btn-icon" @click.stop="physSizeDialogUid = img.uid"
          v-tooltip.right="warnIconFor(img)!.tip">
          <i class="pi pi-exclamation-triangle" />
        </button>
        <span v-if="qcFor(img)" class="qc-badge" :class="qcFor(img)!.level"
          v-tooltip.right="qcFor(img)!.long">
          <i class="pi pi-flag" /> QC
        </span>
        <button class="ref-star cc-btn cc-btn-bare cc-btn-icon" :class="{ on: isStarred(img) }"
          @click.stop="toggleStarred(img)"
          v-tooltip.right="isStarred(img) ? 'Unstar' : 'Star this image'">
          <i :class="isStarred(img) ? 'pi pi-star-fill' : 'pi pi-star'" />
        </button>
        <span v-if="isExcluded(img)" class="excl-badge"
          v-tooltip.right="img.note ? `Excluded: ${img.note}` : 'Excluded from processing'">
          <i class="pi pi-ban" /> Excluded
        </span>
        <span class="cell-text" v-tooltip.right="img.filepath ?? img.name">{{ img.name }}</span>
        <!-- all per-row actions collapse into one ⋯ menu (keeps the name column narrow) -->
        <span class="runlog-cell" @click.stop>
          <button class="row-icon-btn cc-btn cc-btn-bare cc-btn-icon actions-btn" :class="{ on: actionsUid === img.uid }"
            @click.stop="toggleActions(img.uid, $event)"
            v-tooltip.right="'Actions'"><i class="pi pi-ellipsis-h" /></button>
        </span>
      </span>
      <span class="uid-row">
        <span class="img-uid cc-muted cc-fs-xs">{{ img.uid }}</span>
        <!-- last successful run — task-manager-style module tag (see run log / taskModule palette) -->
        <span v-if="lastRunTag(img)" class="run-tag" :style="lastRunTag(img)!.style"
          v-tooltip.right="lastRunTag(img)!.tip">
          <span class="run-tag-mod">{{ lastRunTag(img)!.module }}</span>
          <span class="run-tag-fun">{{ lastRunTag(img)!.fun }}</span>
        </span>
      </span>
      <!-- free-text note for ANY image (excluded or not) — for excluded images it doubles as the
           exclusion reason (shown in the badge tooltip + CSV) -->
      <span class="note-row" @click.stop>
        <input v-if="isEditing(img.uid, NOTE_KEY)" v-tooltip.right="'Enter to save, Esc to cancel'"
          class="attr-edit" v-model="editValue" :ref="focusEditInput"
          :placeholder="isExcluded(img) ? 'reason (optional)' : 'note (optional)'"
          @keyup.enter="commitEdit(img.uid, NOTE_KEY, img.note ?? '', v => saveNote(img, v))"
          @keyup.esc="cancelEdit"
          @blur="commitEdit(img.uid, NOTE_KEY, img.note ?? '', v => saveNote(img, v))" />
        <span v-else class="note-text" @click="startEdit(img.uid, NOTE_KEY, img.note ?? '')"
          v-tooltip.right="'Click to edit the note'">
          <i class="pi pi-comment" /> {{ img.note || 'add a note…' }}
        </span>
      </span>
    </template>

    <!-- channel names: editable only on the Metadata page (editableMeta); read-only elsewhere -->
    <template v-for="idx in channelIndices" :key="'ch-' + idx" #[`cell-ch:${idx}`]="{ row: img }">
      <template v-if="editableMeta && channelEditable(img, idx)">
        <input v-if="isEditing(img.uid, 'ch:' + idx)" v-tooltip.right="'Enter to save, Esc to cancel'"
          class="attr-edit" v-model="editValue" :ref="focusEditInput" @click.stop
          @keyup.enter="commitEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '', v => saveChannel(img, idx, v))"
          @keyup.esc="cancelEdit"
          @blur="commitEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '', v => saveChannel(img, idx, v))" />
        <span v-else class="cell-text attr-cell"
          v-tooltip.right="img.channelNames?.[idx - 1] ? `${img.channelNames[idx - 1]} — click to edit` : `Channel ${idx} — click to name`"
          @click.stop="startEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '')">
          {{ img.channelNames?.[idx - 1] || '—' }}
        </span>
      </template>
      <span v-else-if="img.channelNames?.[idx - 1]" class="cell-text"
        v-tooltip.right="img.channelNames[idx - 1]">{{ img.channelNames[idx - 1] }}</span>
      <span v-else class="dim">—</span>
    </template>

    <!-- attributes: editable only on the Metadata page (editableMeta); read-only elsewhere -->
    <template v-for="key in attrKeys" :key="'attr-' + key" #[`cell-attr:${key}`]="{ row: img }">
      <template v-if="editableMeta">
        <input v-if="isEditing(img.uid, 'attr:' + key)" v-tooltip.right="'Enter to save, Esc to cancel'"
          class="attr-edit" v-model="editValue" :ref="focusEditInput" @click.stop
          @keyup.enter="commitEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '', v => saveAttr(img, key, v))"
          @keyup.esc="cancelEdit"
          @blur="commitEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '', v => saveAttr(img, key, v))" />
        <span v-else class="cell-text attr-cell"
          v-tooltip.right="img.attr?.[key] ? `${key}: ${img.attr[key]} — click to edit` : `Set ${key}`"
          @click.stop="startEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '')">
          {{ img.attr?.[key] || '—' }}
        </span>
      </template>
      <span v-else class="cell-text"
        v-tooltip.right="img.attr?.[key] ? `${key}: ${img.attr[key]}` : ''">{{ img.attr?.[key] || '—' }}</span>
    </template>

    <template #cell-ch="{ row: img }">
      <span v-if="img.sizeC">{{ img.sizeC }}</span>
      <span v-else class="dim">—</span>
    </template>

    <template #cell-z="{ row: img }">
      <span v-if="(img.sizeZ ?? 0) > 1">{{ img.sizeZ }}</span>
      <span v-else class="dim">—</span>
    </template>

    <template #cell-duration="{ row: img }">
      <span v-if="timelapseDuration(img.sizeT, img.timeIncrement, img.timeIncrementUnit)">{{
        timelapseDuration(img.sizeT, img.timeIncrement, img.timeIncrementUnit) }}</span>
      <span v-else class="dim">—</span>
    </template>

    <template #cell-status="{ row: img }">
      <span v-if="imageModuleStatus(img)"
        class="status-badge"
        :class="statusConfig[imageModuleStatus(img)!]?.cls"
        v-tooltip.right="statusConfig[imageModuleStatus(img)!]?.label">
        <span v-if="imageModuleStatus(img) === 'running'" class="spinner" />
        {{ statusConfig[imageModuleStatus(img)!]?.label }}
      </span>
    </template>
  </SelectionTable>
  </div>

  <PhysicalSizeDialog v-if="physSizeDialogUid"
    :set-uid="setUid" :focus-uid="physSizeDialogUid" :selected-uids="[...selected]"
    @close="physSizeDialogUid = null" />

  <ImageMetadataDialog v-if="metaDialogImg" :image="metaDialogImg"
    @close="metaDialogUid = null" />

  <CropDialog v-if="cropDialogImg" :image="cropDialogImg" :set-uid="setUid"
    @close="cropDialogUid = null" />

  <!-- row actions menu (⋯) — collapses the per-row action icons; shares TeleportPopover -->
  <TeleportPopover v-model="actionsOpen" :anchor="actionsAnchor" placement="bottom-end" flush>
    <div v-if="actionsImg" class="actions-menu">
      <button v-if="pageIconFor()" class="actions-item"
        @click.stop="runAction(() => physSizeDialogUid = actionsImg!.uid)">
        <i class="pi pi-file-edit" /> {{ pageIconFor()!.tip }}
      </button>
      <button class="actions-item" @click.stop="runAction(() => metaDialogUid = actionsImg!.uid)">
        <i class="pi pi-info-circle" /> Metadata
      </button>
      <!-- crop CREATES an image → Import page only, like copy/move/remove in the action bar -->
      <button v-if="module === 'import'" class="actions-item" :disabled="!isImported(actionsImg)"
        @click.stop="isImported(actionsImg) && runAction(() => cropDialogUid = actionsImg!.uid)">
        <i class="pi pi-image" /> Crop to new image…
      </button>
      <button class="actions-item" @click.stop="runAction(() => copyUid(actionsImg!.uid))">
        <i class="pi pi-copy" /> Copy UID
      </button>
      <button class="actions-item" @click.stop="runAction(() => setIncluded(actionsImg!, isExcluded(actionsImg!)))">
        <i :class="isExcluded(actionsImg) ? 'pi pi-check-circle' : 'pi pi-ban'" />
        {{ isExcluded(actionsImg) ? 'Include in processing' : 'Exclude from processing' }}
      </button>
      <button class="actions-item" @click.stop="openRunLogFromMenu(actionsImg.uid)">
        <i class="pi pi-cog" /> Run history
      </button>
    </div>
  </TeleportPopover>

  <!-- run-history popover — shared TeleportPopover escapes the table's scroll/transform clip and
       positions from the cog rect (was clipped by the following row) -->
  <TeleportPopover v-model="runLogOpen" :anchor="runLogAnchor">
    <div v-if="runLogImg" class="runlog-pop">
      <div class="runlog-hd cc-eyebrow cc-fs-2xs">Run history</div>
      <div v-if="!runLogImg.runLog || !runLogImg.runLog.length" class="runlog-empty cc-muted cc-fs-xs">No functions recorded yet.</div>
      <div v-for="(e, i) in [...(runLogImg.runLog ?? [])].reverse()" :key="i" class="runlog-row">
        <span class="runlog-fun">{{ e.fun }}</span>
        <span v-if="e.valueName" class="runlog-vn">{{ e.valueName }}</span>
        <span class="runlog-at">{{ fmtRunAt(e.at) }}</span>
      </div>
    </div>
  </TeleportPopover>
</template>

<style scoped>
/* ── Layout ──────────────────────────────────────────────────────────────────── */

/* the one genuinely RICH empty state (icon + title + hint + CTA); the chrome comes from
   .cc-empty .cc-empty-lg, only the icon/title/hint/CTA details are local */
.empty-icon  { font-size: 2.5rem; margin-bottom: 0.5rem; opacity: 0.3; }
.empty-title { font-size: 0.95rem; font-weight: 600; color: var(--cc-text); margin: 0; }
.empty-hint  { font-size: var(--cc-fs-md); margin: 0; text-align: center; }
.empty-cta   { margin-top: 0.85rem; }

/* ── Table ───────────────────────────────────────────────────────────────────── */
/* The table is SelectionTable now: the header, the sort affordance, the row hover/selected states,
   the frozen-column offsets and the resize handles all live there. `.image-table` is its ROOT, so it
   takes this component's scope id directly; anything inside it needs `:deep`.

   `width: max-content` is what makes the horizontal scroll work — the shared table sizes to 100%,
   which would compress the columns instead of letting `.table-scroll` scroll them. */
.table-scroll { overflow-x: auto; width: 100%; }
.image-table { width: max-content; min-width: 100%; font-size: var(--cc-fs-md); }

/* Batch-fix buttons in the Name header (select-flagged, re-sync) */
.select-flagged-btn { margin-left: 0.3rem; vertical-align: middle; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.select-flagged-btn:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.select-flagged-btn.active { color: #fbbf24; }
.select-flagged-btn.active:hover { color: #fcd34d; }
.select-flagged-btn:disabled { opacity: 0.5; cursor: not-allowed; }

/* Excluded: greyed but still visible (not hidden) — dim the whole row, un-dim a touch on hover so its
   note + include toggle stay usable. `:deep` because the row is SelectionTable's element. */
.image-table :deep(.row-excluded) { opacity: 0.5; cursor: default; }
.image-table :deep(.row-excluded:hover) { opacity: 0.8; }

/* A name cell is three stacked rows, so it must not be vertically centred like a one-line cell. */
.image-table :deep(td) { vertical-align: top; overflow: hidden; }

/* ── Cell content ────────────────────────────────────────────────────────────── */

.cell-text {
  display: block;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  color: var(--cc-text);
}

/* (`.td-name .cell-text` lived here and set the colour `.cell-text` above already sets — it was a
   no-op before the table moved, so it went with the `td-name` class rather than being ported.) */

.name-row { display: flex; align-items: center; gap: 0.3rem; min-width: 0; }
.name-row .cell-text { flex: 1; min-width: 0; }

/* always visible when flagged — impossible-to-miss, sits in front of the name */
/* Calibration warning icon (metadata.* findings) → click to fix. Warn severity token (colour-blind
   palette); the shape-distinct triangle icon carries the meaning, colour is secondary. */
.warn-icon-btn { color: var(--cc-sev-warn); }   /* + cc-btn cc-btn-bare cc-btn-icon */
.warn-icon-btn:hover { filter: brightness(1.2); }

/* Star — a per-image bookmark, any number per set. Dim until hovered when unset, so a column of
   unset stars doesn't compete with the QC and exclusion badges. */
.ref-star { opacity: .25; }
.ref-star:hover { opacity: .7; }
.ref-star.on { opacity: 1; color: var(--cc-accent); }

/* QC badge — advisory "output looks off" flag (non-metadata findings; distinct from the calibration
   warning icon). Uses the canonical severity tokens. */
.qc-badge {
  flex-shrink: 0; display: inline-flex; align-items: center; gap: 0.2rem;
  font-size: var(--cc-fs-2xs); font-weight: 700; letter-spacing: 0.04em;
  padding: 0.05rem 0.3rem; border-radius: var(--cc-radius-xs); cursor: help;
  border: 1px solid transparent;
}
.qc-badge .pi { font-size: var(--cc-fs-2xs); }
.qc-badge.warn { color: var(--cc-sev-warn); background: color-mix(in srgb, var(--cc-sev-warn) 15%, transparent); border-color: color-mix(in srgb, var(--cc-sev-warn) 40%, transparent); }
.qc-badge.info { color: var(--cc-text-dim); background: var(--cc-surface-2); border-color: var(--cc-border); }

/* excluded badge — persistent "this image is excluded" pill; carries the note as its tooltip */
.excl-badge {
  flex-shrink: 0; display: inline-flex; align-items: center; gap: 0.2rem;
  font-size: var(--cc-fs-2xs); font-weight: 700; letter-spacing: 0.04em;
  padding: 0.05rem 0.3rem; border-radius: var(--cc-radius-xs); cursor: help;
  color: #fca5a5; background: #7f1d1d33; border: 1px solid #7f1d1d66;
}
.excl-badge .pi { font-size: var(--cc-fs-2xs); }

/* include/exclude toggle: hidden until row hover like the other row actions, but ALWAYS visible on
   an excluded row so there's an obvious way back */
.row-excluded .incl-toggle { opacity: 1; }
.incl-toggle:hover { color: #fca5a5; }

/* exclusion note — editable reason under the uid, only on excluded rows */
.note-row { display: flex; align-items: center; margin-top: 0.15rem; }
.note-text {
  font-size: var(--cc-fs-xs); color: var(--cc-text-dim); cursor: text;
  display: inline-flex; align-items: center; gap: 0.25rem;
  border-radius: var(--cc-radius-xs); padding: 0 2px;
}
.note-text:hover { background: var(--cc-surface-2); outline: 1px dashed var(--cc-border); }
.note-text .pi { font-size: var(--cc-fs-2xs); }

/* row-hover actions after the name: the "open editor" page icon + copy-UID — same look, one class */
.row-icon-btn { opacity: 0; transition: opacity 0.1s, color 0.1s, background 0.1s; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.sel-row:hover .row-icon-btn { opacity: 1; }
.row-icon-btn:hover { color: var(--cc-text); background: var(--cc-surface-2); }
/* disabled (e.g. Crop on a not-yet-imported image): visibly greyed, no hover highlight, not-allowed */
.row-icon-btn:disabled { cursor: not-allowed; }
.sel-row:hover .row-icon-btn:disabled { opacity: 0.3; }
.row-icon-btn:disabled:hover { color: var(--cc-text-dim); background: none; }

/* editable attribute cell: click to edit; subtle hover affordance */
.attr-cell { cursor: text; border-radius: var(--cc-radius-xs); padding: 0 2px; }
.attr-cell:hover { background: var(--cc-surface-2); outline: 1px dashed var(--cc-border); }
.attr-edit { width: 100%; box-sizing: border-box; padding: 1px 3px;
  border: 1px solid var(--cc-accent); border-radius: var(--cc-radius-xs); background: var(--cc-surface-1); }

.uid-row {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  min-width: 0;
  margin-top: 0.3rem;
}
/* run-history cog + popover (fixed so it escapes the table's horizontal scroll) */
.runlog-cell { position: relative; display: inline-flex; flex-shrink: 0; }
.runlog-cog.on { color: var(--cc-text); background: var(--cc-surface-2); opacity: 1; }
/* inner layout only — TeleportPopover provides surface/border/shadow/position */
.runlog-pop { min-width: 15rem; max-height: 16rem; overflow-y: auto; }   /* padding: TeleportPopover */
.runlog-hd { margin-bottom: 4px; }

.runlog-row { display: flex; align-items: baseline; gap: 6px; padding: 2px 0; font-size: var(--cc-fs-xs); }
.runlog-fun { font-weight: 600; color: var(--cc-text); font-family: var(--cc-mono); }
.runlog-vn { color: var(--cc-accent); font-size: var(--cc-fs-2xs); }
.runlog-at { margin-left: auto; color: var(--cc-text-dim); font-variant-numeric: tabular-nums; white-space: nowrap; }
.img-uid { font-family: var(--cc-mono); letter-spacing: 0.03em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; min-width: 0; }

/* last-successful-run tag — mirrors the task-manager module pill (colour from taskModule palette),
   pushed to the right of the UID. Module id bold/uppercase, function label alongside. */
.run-tag {
  display: inline-flex; align-items: baseline; gap: 0.3rem; flex-shrink: 0;
  max-width: 60%;
  padding: 0.05rem 0.4rem; border-radius: var(--cc-radius-xs);
  border: 1px solid transparent;
  font-size: var(--cc-fs-2xs); line-height: 1.5;
}
.run-tag-mod { font-weight: 700; text-transform: uppercase; letter-spacing: 0.04em; flex-shrink: 0; }
.run-tag-fun { opacity: 0.85; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }

.dim { color: var(--cc-text-dim); }

/* ── Status badge ────────────────────────────────────────────────────────────── */

.status-badge {
  display: inline-flex; align-items: center; gap: 0.35rem;
  font-size: var(--cc-fs-xs); font-weight: 600; padding: 0.15rem 0.5rem;
  border-radius: var(--cc-radius-pill); text-transform: uppercase; letter-spacing: 0.04em;
}
.st-pending  { background: #27272a;    color: #71717a; }
.st-queued   { background: var(--cc-accent-tint);    color: var(--cc-accent-soft); }
.st-running  { background: #1e3a5f;    color: var(--cc-active); }
.st-done     { background: #14532d44;  color: #86efac; }
.st-failed   { background: #7f1d1d44;  color: #fca5a5; }

.spinner {
  width: 7px; height: 7px; border-radius: var(--cc-radius-pill);
  border: 1.5px solid #93c5fd44; border-top-color: var(--cc-active);
  animation: spin 0.7s linear infinite; flex-shrink: 0;
}
@keyframes spin { to { transform: rotate(360deg); } }

/* ── Napari eye button ───────────────────────────────────────────────────────── */

.viewer-btn {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: var(--cc-fs-md);
  padding: 0.2rem 0.3rem; border-radius: var(--cc-radius-xs);
  opacity: 0; transition: opacity 0.12s, color 0.12s, background 0.12s; line-height: 1;
}
.sel-row:hover .viewer-btn { opacity: 0.6; }
.viewer-btn:hover { opacity: 1 !important; color: var(--cc-active); background: #1e3a5f44; }
.viewer-btn:disabled { opacity: 0.2 !important; cursor: not-allowed; }
.viewer-active { opacity: 1 !important; color: #f97316; }
.viewer-active:hover { color: #fb923c; background: #f9731622; }

/* ⋯ actions button: faintly visible at rest (discoverable), full on hover/open */
.actions-btn { opacity: 0.5; }
.sel-row:hover .actions-btn,
.actions-btn.on { opacity: 1; color: var(--cc-text); background: var(--cc-surface-2); }

/* ⋯ dropdown menu (inside TeleportPopover, which provides the surface/border/shadow) */
.actions-menu { display: flex; flex-direction: column; min-width: 200px; }   /* padding: TeleportPopover (flush) */
.actions-item {
  display: flex; align-items: center; gap: 0.55rem;
  width: 100%; padding: 0.4rem 0.6rem; border: none; border-radius: var(--cc-radius-xs);
  background: none; color: var(--cc-text); font-size: var(--cc-fs-md); text-align: left; cursor: pointer;
}
.actions-item:hover:not(:disabled) { background: var(--cc-surface-2); }
.actions-item:disabled { opacity: 0.4; cursor: not-allowed; }
.actions-item .pi { width: 1rem; text-align: center; color: var(--cc-text-dim); flex-shrink: 0; }
</style>
