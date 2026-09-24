<script setup lang="ts">
// Blackboard — the shared markdown surface for a project. A blackboard entry is a note (with
// optional Mermaid diagrams and attached captureIds) that you and Claude iterate on across sessions.
// Not gated on the Claude/observer connection: entries are read from `/api/blackboard/*` on this
// user's own machine, so a bad MCP day still renders every diagram.
//
// Backend: api/src/blackboard_api.jl. Plan: docs/todo/BIDIR_CONTEXT_PLAN.md → Part 4.
//
// Layout mirrors the pipeline surfaces (ChainModule / TasksModule): a full-height flex column
// with a toolbar on top and content below. The list uses the canonical `SelectionTable`
// (single-select), matching NotebookTable — no bespoke <ul> primitive lives here. Delete uses
// the canonical `ConfirmDeleteButton`. The right pane is the entry viewer / editor.
//
// Attachment thumbnails composite the drawn marks INTO the PNG before rendering (via
// `overlayCompose.ts::composeImageWithOverlay`) — same reason share compositing exists: a raw
// frame + a colourless mark overlay reads as "no annotations" to the eye, which is exactly the
// bug this fixes on the attachment strip.

import { ref, computed, watch, onMounted, onUnmounted, nextTick } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useBlackboardStore } from '../stores/blackboard'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import ConfirmDeleteButton from '../components/ConfirmDeleteButton.vue'
import ChipSelect from '../components/ChipSelect.vue'
import { usePanelResize } from '../composables/usePanelResize'
import {
  listBlackboardEntries, getBlackboardEntry, createBlackboardEntry,
  reviseBlackboardEntry, restoreBlackboardEntry, deleteBlackboardEntry,
  setBlackboardStatus, setBlackboardOutcome,
  formatWhen, type BlackboardEntrySummary, type BlackboardEntry,
  type BlackboardStatus, type BlackboardVerdict,
} from '../utils/blackboardApi'
import {
  filterEntries, PROFILE_ENTRY_ID,
  type StatusChoice, type OutcomeChoice,
} from '../utils/blackboardFilters'
import { renderBlackboardMarkdown, mermaidBlocks } from '../utils/blackboardMd'
import { fetchCaptureEnvelope, type CaptureEnvelope } from '../utils/kiwiCaptures'
import { useRoute } from 'vue-router'
import { useCaptureFocus } from '../composables/useCaptureFocus'
import AddToKiwiButton from '../components/kiwi/AddToKiwiButton.vue'
import { composeImageWithOverlay } from '../utils/overlayCompose'
import { loadImg } from '../plots/export'

const projectMeta = useProjectMetaStore()
const bbStore = useBlackboardStore()
const route = useRoute()

// List-pane width: draggable + persisted, same composable Tasks + Chain use so the "grab the
// divider and pull" gesture reads the same across the app. Handle on the list's RIGHT edge.
const { widthStyle: listWidthStyle, onResizeStart: onListResizeStart } = usePanelResize({
  min: 220, max: 640, default: 352, storageKey: 'cc-blackboard-list-width', edge: 'right',
})

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)

// ── List state ───────────────────────────────────────────────────────────────
const entries = ref<BlackboardEntrySummary[]>([])
const listLoading = ref(false)

// SelectionTable columns. One row per entry — the table sorts by any of these, so `updated` sorts
// by the raw ISO string (lexicographic works because the id-encoded timestamp is fixed-width).
// The `status` / `outcome` / `attachmentsCount` cells use the slot API for compact chips.
// PROJECT_MEMORY_PLAN Decisions 3 (status) + 11 (outcome) travel here as columns.
const LIST_COLUMNS: SelectionColumn[] = [
  { key: 'title',     label: 'Title',       sortable: true, width: 240 },
  { key: 'status',    label: '',            sortable: false, width: 24 },
  { key: 'outcome',   label: '',            sortable: false, width: 24 },
  { key: 'updatedAt', label: 'Updated',     sortable: true, width: 72  },
  { key: 'current',   label: 'Ver',         sortable: true, width: 38  },
  { key: 'attachmentsCount', label: '',     sortable: false, width: 32 },
]

// ── Filter chips — Decision 6 (from the P4 plan): status + outcome are filterable columns on the
// list. Both persist across mounts via localStorage; the profile entry is always visible regardless
// (see `filterEntries` in `utils/blackboardFilters.ts`).
const STATUS_FILTER_OPTS = [
  { value: 'all',      label: 'All' },
  { value: 'open',     label: 'Open' },
  { value: 'resolved', label: 'Resolved' },
  { value: 'parked',   label: 'Parked' },
]
const OUTCOME_FILTER_OPTS = [
  { value: 'all',      label: 'All' },
  { value: 'untagged', label: 'Untagged' },
  { value: 'good',     label: 'Good', icon: 'pi pi-thumbs-up' },
  { value: 'bad',      label: 'Bad',  icon: 'pi pi-thumbs-down' },
]
const STATUS_FILTER_KEY  = 'cc.blackboard.statusFilter'
const OUTCOME_FILTER_KEY = 'cc.blackboard.outcomeFilter'
function loadPref<T extends string>(key: string, allowed: readonly T[], fallback: T): T {
  try {
    const v = localStorage.getItem(key)
    return v && (allowed as readonly string[]).includes(v) ? v as T : fallback
  } catch { return fallback }
}
const statusFilter  = ref<StatusChoice>(loadPref<StatusChoice>(
  STATUS_FILTER_KEY, ['all', 'open', 'resolved', 'parked'], 'all'))
const outcomeFilter = ref<OutcomeChoice>(loadPref<OutcomeChoice>(
  OUTCOME_FILTER_KEY, ['all', 'untagged', 'good', 'bad'], 'all'))
watch(statusFilter,  v => { try { localStorage.setItem(STATUS_FILTER_KEY,  v) } catch { /* private mode */ } })
watch(outcomeFilter, v => { try { localStorage.setItem(OUTCOME_FILTER_KEY, v) } catch { /* private mode */ } })

// Filtered rows that actually feed the SelectionTable. `filterEntries` keeps the profile row
// visible regardless of the filters (Decision 2 — it's the pinned "what is this project" record).
const filteredEntries = computed(() => filterEntries(entries.value, statusFilter.value, outcomeFilter.value))
const filteredOutCount = computed(() => entries.value.length - filteredEntries.value.length)

// ── Selected entry state ─────────────────────────────────────────────────────
// `selected` is the LIVE entry; `viewingVersion` != null when the user has clicked a history
// version to preview it (the pane shows that snapshot's content; live-state fields still drive
// the header/actions).
const selectedId = ref('')
const selected = ref<BlackboardEntry | null>(null)
const viewingVersion = ref<number | null>(null)
const viewingContent = ref('')
const entryLoading = ref(false)

// Editor state — draft fields live outside `selected` so a Cancel discards them without touching
// the fetched state.
type Mode = 'view' | 'edit-existing' | 'edit-new'
const mode = ref<Mode>('view')
const draftTitle = ref('')
const draftContent = ref('')
const savingDraft = ref(false)

// ── Outcome tag control (Decision 11) ────────────────────────────────────────
// Tap → type note → confirm. D5's "one tap" softens because D2 makes the note required — we open
// the inline note input on first tap of good/bad, and only fire the endpoint once the note is
// non-empty. Editing an existing tag pre-fills the note.
const outcomeEditing = ref(false)
const outcomeDraftVerdict = ref<BlackboardVerdict | null>(null)
const outcomeDraftNote = ref('')
const savingOutcome = ref(false)
const OUTCOME_NOTE_MAX = 2 * 1024   // matches server-side cap in api/src/blackboard_api.jl

function beginOutcomeEdit() {
  if (!selected.value) return
  const o = selected.value.outcome
  outcomeDraftVerdict.value = o?.verdict ?? null
  outcomeDraftNote.value    = o?.note ?? ''
  outcomeEditing.value = true
}
function cancelOutcomeEdit() {
  outcomeEditing.value = false
  outcomeDraftVerdict.value = null
  outcomeDraftNote.value = ''
}
async function saveOutcome() {
  if (savingOutcome.value || !selected.value || !projectUid.value) return
  const verdict = outcomeDraftVerdict.value
  const note = outcomeDraftNote.value.trim()
  if (!verdict || !note) return
  savingOutcome.value = true
  try {
    const applied = await setBlackboardOutcome(projectUid.value, selected.value.entryId, verdict, note)
    if (applied) {
      // Reload the entry so `selected.outcome` reflects the new tag; the WS broadcast reloads
      // the list on the other side of the same round-trip (see the `bbStore.tick` watcher).
      await loadEntry(selected.value.entryId)
      cancelOutcomeEdit()
    }
  } finally { savingOutcome.value = false }
}

// Status flip — no snapshot fired, so this can be a direct chip-click without an intermediate.
const savingStatus = ref(false)
async function onStatusChange(next: BlackboardStatus) {
  if (savingStatus.value || !selected.value || !projectUid.value) return
  if (next === selected.value.status) return
  savingStatus.value = true
  try {
    const applied = await setBlackboardStatus(projectUid.value, selected.value.entryId, next)
    if (applied) await loadEntry(selected.value.entryId)
  } finally { savingStatus.value = false }
}

const STATUS_HEAD_OPTS = [
  { value: 'open',     label: 'Open' },
  { value: 'resolved', label: 'Resolved' },
  { value: 'parked',   label: 'Parked' },
]

// Attachment envelopes for the selected entry — fetched lazily per attachment so the thumbnails
// stream in. `thumb` is a data URL of the composited FRAME + MARKS (see `resolveThumbForCache`).
interface AttachmentSlot {
  env: CaptureEnvelope | null
  thumb: string     // composited data URL, '' if the frame isn't available yet
}
const captureCache = ref<Record<string, AttachmentSlot>>({})

// Rendered markdown HTML for the entry pane (viewer OR history preview). `v-html` target.
const paneContent = computed(() => {
  if (mode.value !== 'view' || !selected.value) return ''
  return viewingVersion.value !== null ? viewingContent.value : selected.value.content
})
// Title map for `[[bb-…]]` cross-refs — drives the wiki-link resolver so a bare id in the markdown
// renders as a clickable title. Rebuilt from the CURRENT entries list; if a linked entry has been
// deleted, the resolver falls back to "<id> (deleted?)" so the reference is still visible.
const titleById = computed(() => {
  const m = new Map<string, string>()
  for (const e of entries.value) m.set(e.entryId, e.title || e.entryId)
  return m
})
const paneHtml = computed(() => renderBlackboardMarkdown(paneContent.value, titleById.value))
const hasMermaid = computed(() => mermaidBlocks(paneContent.value).length > 0)

// Click intercept for the wiki links the resolver produces — `<a href="#bb:<id>">…`. Bubbles from
// `v-html` content, so we listen on the pane wrapper. Falls through to the browser for any other
// href (including plain fragment links inside the entry body).
function onPaneClick(ev: MouseEvent) {
  const a = (ev.target as HTMLElement | null)?.closest?.('a') as HTMLAnchorElement | null
  if (!a) return
  const href = a.getAttribute('href') ?? ''
  if (!href.startsWith('#bb:')) return
  ev.preventDefault()
  const id = href.slice('#bb:'.length)
  if (id && entries.value.some(e => e.entryId === id)) selectEntry(id)
  // Unknown id — the resolver already tagged the label "(deleted?)"; do nothing on click.
}

async function loadList() {
  if (!projectUid.value) { entries.value = []; return }
  listLoading.value = true
  try {
    entries.value = await listBlackboardEntries(projectUid.value)
  } finally { listLoading.value = false }
}

/** Compose the mark overlay onto the raw frame — the same trick share uses, so an attachment
 *  chip reads as "this is what Claude drew on" rather than a bare frame the annotations are
 *  invisible on. Falls back to the raw frame when there are no marks / compose fails. */
async function resolveThumbForCache(env: CaptureEnvelope): Promise<string> {
  if (!env.frame) return ''
  if (!env.overlay || env.overlay.length === 0) return env.frame
  const img = await loadImg(env.frame)
  if (!img) return env.frame
  return composeImageWithOverlay(img, env.overlay) ?? env.frame
}

async function loadAttachment(cid: string) {
  if (captureCache.value[cid]) return                 // already in flight or done
  captureCache.value = { ...captureCache.value, [cid]: { env: null, thumb: '' } }
  const env = await fetchCaptureEnvelope(projectUid.value, cid)
  if (!env) return
  const thumb = await resolveThumbForCache(env)
  captureCache.value = { ...captureCache.value, [cid]: { env, thumb } }
}

async function loadEntry(id: string, version?: number) {
  if (!projectUid.value || !id) { selected.value = null; return }
  entryLoading.value = true
  try {
    if (version === undefined) {
      selected.value = await getBlackboardEntry(projectUid.value, id)
      viewingVersion.value = null
      viewingContent.value = ''
    } else {
      const snap = await getBlackboardEntry(projectUid.value, id, version)
      if (snap) {
        viewingVersion.value = version
        viewingContent.value = snap.content
        // Attachments-per-version: preview the RECORDED set (post-#1072 backend). We only render
        // the strip when there's an entry; keeping `selected.attachments` for LIVE + the recorded
        // list here is the split the API already draws.
        selected.value = { ...selected.value!, attachments: snap.attachments }
      }
    }
  } finally { entryLoading.value = false }
  if (selected.value) {
    for (const cid of selected.value.attachments) void loadAttachment(cid)
  }
}

function selectEntry(id: string) {
  if (!id) return
  if (selectedId.value === id && mode.value === 'view' && viewingVersion.value === null) return
  selectedId.value = id
  mode.value = 'view'
  cancelOutcomeEdit()
  void loadEntry(id)
}

function beginEditExisting() {
  if (!selected.value) return
  draftTitle.value = selected.value.title
  draftContent.value = selected.value.content
  mode.value = 'edit-existing'
}

function beginNew() {
  draftTitle.value = ''
  draftContent.value = ''
  selectedId.value = ''
  selected.value = null
  viewingVersion.value = null
  mode.value = 'edit-new'
}

function cancelEdit() {
  mode.value = 'view'
  draftTitle.value = ''
  draftContent.value = ''
}

async function saveDraft() {
  if (savingDraft.value) return
  if (!projectUid.value) return
  const title = draftTitle.value.trim()
  if (!title) return
  savingDraft.value = true
  try {
    if (mode.value === 'edit-new') {
      const id = await createBlackboardEntry(projectUid.value, title, draftContent.value)
      if (id) {
        await loadList()
        selectedId.value = id
        await loadEntry(id)
        mode.value = 'view'
      }
    } else if (mode.value === 'edit-existing' && selected.value) {
      // Content-only save today (backend has no rename route). If nothing changed the server
      // returns `unchanged:true` and no snapshot is spent (#1072).
      const v = await reviseBlackboardEntry(projectUid.value, selected.value.entryId, draftContent.value)
      if (v > 0) await loadEntry(selected.value.entryId)
      mode.value = 'view'
    }
  } finally { savingDraft.value = false }
}

async function restoreVersion(v: number) {
  if (!selected.value || !projectUid.value) return
  const restored = await restoreBlackboardEntry(projectUid.value, selected.value.entryId, v)
  if (restored > 0) await loadEntry(selected.value.entryId)
}

async function onDelete() {
  if (!selected.value || !projectUid.value) return
  const ok = await deleteBlackboardEntry(projectUid.value, selected.value.entryId)
  if (ok) {
    selectedId.value = ''
    selected.value = null
    await loadList()
  }
}

/** Open the capture in its source surface — the shared refocus path (`composables/useCaptureFocus`),
 *  the same one Kiwi's capture rows and claim chips use. */
const { focusCapture: refocusCapture } = useCaptureFocus()
function focusCapture(cid: string) {
  const slot = captureCache.value[cid]
  if (slot?.env) refocusCapture(projectUid.value, slot.env)
}

// Mermaid: dynamic-import only when the current pane contains ```mermaid fences (0 fences ⇒ no cost).
// Rendered post-hoc: `v-html` sets the raw HTML, then we walk `.language-mermaid` code blocks and
// swap in `mermaid.render()` SVGs. Retries on each `paneHtml` change (edit -> save, version pick).
const paneRef = ref<HTMLDivElement | null>(null)
interface MermaidLike {
  initialize: (o: Record<string, unknown>) => void
  render: (id: string, code: string) => Promise<{ svg: string }>
}
let mermaidLoader: Promise<MermaidLike> | null = null
let mermaidRenderSeq = 0
async function renderMermaidInPane() {
  const el = paneRef.value
  if (!el) return
  if (!hasMermaid.value) return
  const seq = ++mermaidRenderSeq
  const nodes = Array.from(el.querySelectorAll<HTMLElement>('code.language-mermaid'))
  if (nodes.length === 0) return
  try {
    if (!mermaidLoader) {
      mermaidLoader = import('mermaid').then(m => (m.default ?? m) as unknown as MermaidLike)
    }
    const mermaid = await mermaidLoader
    mermaid.initialize({ startOnLoad: false, theme: 'dark', securityLevel: 'strict' })
    for (let i = 0; i < nodes.length; i++) {
      if (seq !== mermaidRenderSeq) return
      const code = nodes[i].textContent ?? ''
      const id = `bb-mermaid-${Date.now()}-${i}`
      try {
        const { svg } = await mermaid.render(id, code)
        const holder = document.createElement('div')
        holder.className = 'bb-mermaid'
        holder.innerHTML = svg
        nodes[i].parentElement?.replaceWith(holder)
      } catch { /* keep the raw code visible */ }
    }
  } catch { /* mermaid import blocked; leave the raw code */ }
}
watch(paneHtml, async () => { await nextTick(); await renderMermaidInPane() })

// WS `blackboard:changed` — silent list + entry reload for THIS project.
watch(() => bbStore.tick, async () => {
  if (bbStore.lastProjectUid && bbStore.lastProjectUid !== projectUid.value) return
  await loadList()
  if (selectedId.value) await loadEntry(selectedId.value)
})

// Project switch — per-project page.
watch(projectUid, async () => {
  selectedId.value = ''
  selected.value = null
  mode.value = 'view'
  captureCache.value = {}
  await loadList()
})

// `?entry=<id>` opens that entry — how a Kiwi blackboard chip points here. Same-page clicks change
// only the query, so it is watched as well as read on mount.
const queryEntry = computed(() => typeof route.query.entry === 'string' ? route.query.entry : '')
onMounted(async () => {
  await loadList()
  if (queryEntry.value && entries.value.some(e => e.entryId === queryEntry.value)) selectEntry(queryEntry.value)
  else if (entries.value.length > 0 && !selectedId.value) selectEntry(entries.value[0].entryId)
})
watch(queryEntry, (id) => { if (id && entries.value.some(e => e.entryId === id)) selectEntry(id) })
onUnmounted(() => { mermaidRenderSeq++ })
</script>

<template>
  <div class="bb-module">
    <div v-if="!hasProject" class="bb-empty cc-empty-inline">
      <i class="pi pi-lock" /> Open or create a project first.
    </div>

    <template v-else>
      <!-- Toolbar — mirrors ChainModule's top bar shape. -->
      <div class="bb-bar cc-row cc-row-loose">
        <button class="cc-btn cc-btn-primary cc-btn-dense" @click="beginNew"
                v-tooltip.bottom="'Start a new blackboard entry'">
          <i class="pi pi-plus" /> New entry
        </button>
        <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="loadList"
                :disabled="listLoading"
                v-tooltip.bottom="'Reload the list from disk'">
          <i class="pi" :class="listLoading ? 'pi-spin pi-spinner' : 'pi-refresh'" /> Refresh
        </button>
        <span class="bb-bar-spacer" />
        <!-- Filter chips (status + outcome). Profile row is always shown regardless. -->
        <ChipSelect variant="segmented"
                    aria-label="Filter by status"
                    :options="STATUS_FILTER_OPTS"
                    :model-value="statusFilter"
                    @update:modelValue="v => statusFilter = v as StatusChoice" />
        <ChipSelect variant="segmented"
                    aria-label="Filter by outcome"
                    :options="OUTCOME_FILTER_OPTS"
                    :model-value="outcomeFilter"
                    @update:modelValue="v => outcomeFilter = v as OutcomeChoice" />
      </div>

      <!-- Split: list left, entry right. Border-only divider, no floating panels. -->
      <div class="bb-split">
        <aside class="bb-list" :style="listWidthStyle">
          <!-- drag the list/entry divider (persisted). Handle on the list's RIGHT edge; drag
               right widens the list. Sits OUTSIDE the scroll wrapper so it can't scroll away. -->
          <div class="bb-divider" @mousedown="onListResizeStart"
               v-tooltip.top="'Drag to resize the list'" />
          <div class="bb-list-scroll">
          <SelectionTable class="bb-list-table"
                          selection-mode="single"
                          id-key="entryId"
                          :columns="LIST_COLUMNS"
                          :rows="filteredEntries"
                          :model-value="selectedId"
                          sort-storage-key="cc.blackboard.sort"
                          @update:model-value="v => selectEntry(String(v ?? ''))">
            <template #cell-title="{ row: e }">
              <span class="bb-list-title" :class="{ 'bb-list-title-profile': e.entryId === PROFILE_ENTRY_ID }">
                <i v-if="e.entryId === PROFILE_ENTRY_ID" class="pi pi-thumbtack bb-list-pin"
                   v-tooltip.top="'Project profile — always at the top; describes what this project is'" />
                {{ e.title || '(untitled)' }}
              </span>
            </template>
            <template #cell-status="{ row: e }">
              <span class="bb-chip bb-chip-status" :class="`bb-chip-status-${e.status}`"
                    v-tooltip.top="`Status: ${e.status}`">{{ e.status[0].toUpperCase() }}</span>
            </template>
            <template #cell-outcome="{ row: e }">
              <span v-if="e.outcome" class="bb-chip bb-chip-outcome"
                    :class="`bb-chip-outcome-${e.outcome.verdict}`"
                    v-tooltip.top="`${e.outcome.verdict}: ${e.outcome.note}`">
                <i class="pi" :class="e.outcome.verdict === 'good' ? 'pi-thumbs-up' : 'pi-thumbs-down'" />
              </span>
            </template>
            <template #cell-updatedAt="{ row: e }">
              <span class="cc-muted cc-fs-2xs">{{ formatWhen(e.updatedAt) }}</span>
            </template>
            <template #cell-current="{ row: e }">
              <span class="cc-muted cc-fs-2xs">{{ e.current > 0 ? `v${e.current}` : '—' }}</span>
            </template>
            <template #cell-attachmentsCount="{ row: e }">
              <span v-if="e.attachmentsCount > 0" class="cc-fs-2xs cc-muted"
                    v-tooltip.top="`${e.attachmentsCount} attachment${e.attachmentsCount === 1 ? '' : 's'}`">
                <i class="pi pi-paperclip" /> {{ e.attachmentsCount }}
              </span>
            </template>
            <template #empty>
              <span class="cc-muted">
                <template v-if="filteredOutCount > 0">
                  {{ filteredOutCount }} entr{{ filteredOutCount === 1 ? 'y' : 'ies' }} hidden by filters.
                </template>
                <template v-else>
                  No entries yet. Claude can create them via MCP, or click <strong>New entry</strong>.
                </template>
              </span>
            </template>
          </SelectionTable>
          </div>
        </aside>

        <section class="bb-pane">
          <!-- Editor -->
          <template v-if="mode === 'edit-new' || mode === 'edit-existing'">
            <div class="bb-pane-head cc-row cc-row-loose">
              <input v-if="mode === 'edit-new'" v-model="draftTitle"
                     class="bb-title-input" placeholder="Entry title"
                     maxlength="200" type="text"
                     v-tooltip.bottom="'Short label shown in the entry list (up to 200 chars)'" />
              <span v-else class="bb-pane-title">{{ selected?.title }}</span>
              <span class="bb-bar-spacer" />
              <button class="cc-btn cc-btn-ghost cc-btn-dense" :disabled="savingDraft" @click="cancelEdit">
                Cancel
              </button>
              <button class="cc-btn cc-btn-primary cc-btn-dense"
                      :disabled="savingDraft || !draftTitle.trim()"
                      @click="saveDraft"
                      v-tooltip.bottom="mode === 'edit-existing' ? 'Save; previous version is snapshotted automatically' : 'Create this entry'">
                <i class="pi" :class="savingDraft ? 'pi-spin pi-spinner' : 'pi-save'" />
                {{ mode === 'edit-existing' ? 'Save revision' : 'Create entry' }}
              </button>
            </div>
            <textarea v-model="draftContent"
                      class="bb-editor"
                      :placeholder="'Markdown. Mermaid diagrams via ```mermaid fences.'"
                      spellcheck="false"
                      v-tooltip.bottom="'Markdown body — a snapshot of the previous version is taken automatically on save'"></textarea>
            <p class="bb-hint cc-muted cc-fs-2xs">
              Trusted-source markdown (this project only). {{ (draftContent?.length ?? 0).toLocaleString() }} chars.
            </p>
          </template>

          <!-- Viewer -->
          <template v-else-if="selected">
            <div class="bb-pane-head cc-row cc-row-loose">
              <i v-if="selected.entryId === PROFILE_ENTRY_ID" class="pi pi-thumbtack bb-pane-pin"
                 v-tooltip.top="'Project profile'" />
              <span class="bb-pane-title">{{ selected.title }}</span>
              <span class="bb-pane-sub cc-fs-2xs cc-muted">
                updated {{ formatWhen(selected.updatedAt) }}
                <template v-if="selected.current > 0"> · v{{ selected.current }}</template>
              </span>
              <span class="bb-bar-spacer" />
              <!-- the version being previewed, if any — so Kiwi reads what is on screen -->
              <AddToKiwiButton size="dense" tip="Add this entry to Kiwi"
                               :kiwi-ref="viewingVersion !== null
                                 ? { kind: 'blackboard', entryId: selected.entryId, version: viewingVersion }
                                 : { kind: 'blackboard', entryId: selected.entryId }" />
              <!-- Status flip — a chip select applied inline; no snapshot fired. -->
              <ChipSelect variant="segmented"
                          aria-label="Entry status"
                          :options="STATUS_HEAD_OPTS"
                          :model-value="selected.status"
                          :disabled="savingStatus"
                          @update:modelValue="v => onStatusChange(v as BlackboardStatus)" />
              <select v-if="selected.versions.length > 0"
                      class="cc-input-xs bb-versions"
                      :value="viewingVersion ?? ''"
                      @change="(ev) => {
                        const v = (ev.target as HTMLSelectElement).value
                        if (v === '') loadEntry(selected!.entryId)
                        else loadEntry(selected!.entryId, Number(v))
                      }"
                      v-tooltip.bottom="'Preview an earlier version'">
                <option value="">Current</option>
                <option v-for="v in [...selected.versions].reverse()" :key="v" :value="v">v{{ v }}</option>
              </select>
              <button v-if="viewingVersion !== null"
                      class="cc-btn cc-btn-ghost cc-btn-dense"
                      @click="restoreVersion(viewingVersion!)"
                      v-tooltip.bottom="'Restore this version as current; the current live content is snapshotted first'">
                <i class="pi pi-history" /> Restore v{{ viewingVersion }}
              </button>
              <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="beginEditExisting"
                      v-tooltip.bottom="'Edit; a snapshot is taken automatically before saving'">
                <i class="pi pi-pencil" /> Edit
              </button>
              <ConfirmDeleteButton title="Delete this entry (all versions)"
                                    armed-title="Click again to permanently delete"
                                    @confirm="onDelete"
                                    v-if="selected.entryId !== PROFILE_ENTRY_ID" />
            </div>

            <!-- Outcome row — good/bad tag with required note. Idle state shows the current tag or
                 a compact "Tag" button; editing state shows note input + Good/Bad buttons (each
                 disabled until the note is non-empty). Decision 11. -->
            <div class="bb-outcome cc-row cc-row-loose">
              <template v-if="!outcomeEditing">
                <template v-if="selected.outcome">
                  <span class="bb-chip bb-chip-outcome" :class="`bb-chip-outcome-${selected.outcome.verdict}`">
                    <i class="pi" :class="selected.outcome.verdict === 'good' ? 'pi-thumbs-up' : 'pi-thumbs-down'" />
                    {{ selected.outcome.verdict }}
                  </span>
                  <span class="bb-outcome-note cc-fs-xs" v-tooltip.top="selected.outcome.note">
                    {{ selected.outcome.note }}
                  </span>
                  <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="beginOutcomeEdit"
                          v-tooltip.bottom="'Change the outcome or edit the note'">
                    <i class="pi pi-pencil" /> Edit tag
                  </button>
                </template>
                <template v-else>
                  <span class="cc-muted cc-fs-xs">Outcome not tagged.</span>
                  <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="beginOutcomeEdit"
                          v-tooltip.bottom="'Tag this thread good/bad with a short note'">
                    <i class="pi pi-tag" /> Tag outcome
                  </button>
                </template>
              </template>
              <template v-else>
                <input v-model="outcomeDraftNote"
                       class="bb-outcome-input"
                       :maxlength="OUTCOME_NOTE_MAX"
                       type="text"
                       placeholder="Why — required (a sentence or two)"
                       v-tooltip.bottom="'Note is required; keep it to what a future session needs to know'" />
                <button class="cc-btn cc-btn-dense bb-outcome-good"
                        :class="{ 'bb-outcome-active': outcomeDraftVerdict === 'good' }"
                        :disabled="savingOutcome || !outcomeDraftNote.trim()"
                        @click="outcomeDraftVerdict = 'good'; saveOutcome()"
                        v-tooltip.bottom="'Held up on real data'">
                  <i class="pi pi-thumbs-up" /> Good
                </button>
                <button class="cc-btn cc-btn-dense bb-outcome-bad"
                        :class="{ 'bb-outcome-active': outcomeDraftVerdict === 'bad' }"
                        :disabled="savingOutcome || !outcomeDraftNote.trim()"
                        @click="outcomeDraftVerdict = 'bad'; saveOutcome()"
                        v-tooltip.bottom="'Turned out wrong'">
                  <i class="pi pi-thumbs-down" /> Bad
                </button>
                <button class="cc-btn cc-btn-ghost cc-btn-dense" :disabled="savingOutcome"
                        @click="cancelOutcomeEdit">Cancel</button>
              </template>
            </div>

            <div v-if="entryLoading" class="cc-muted cc-fs-xs bb-pane-loading">Loading…</div>
            <div v-else ref="paneRef" class="bb-body" v-html="paneHtml" @click="onPaneClick" />

            <div v-if="selected.attachments.length > 0" class="bb-attach">
              <div class="bb-attach-label cc-muted cc-fs-2xs">Attachments</div>
              <div class="bb-attach-strip">
                <button v-for="cid in selected.attachments" :key="cid"
                        class="bb-attach-thumb"
                        @click="focusCapture(cid)"
                        v-tooltip.top="`${cid} — click to focus the pop-out viewer + restore the annotation overlay`">
                  <img v-if="captureCache[cid]?.thumb" :src="captureCache[cid].thumb" :alt="cid" />
                  <span v-else class="bb-attach-fallback"><i class="pi pi-image" /></span>
                </button>
              </div>
            </div>
          </template>

          <div v-else class="bb-pane-empty cc-muted">
            <template v-if="entries.length === 0">
              No entries in this project yet.
            </template>
            <template v-else>
              Pick an entry from the list, or click <strong>New entry</strong> to start one.
            </template>
          </div>
        </section>
      </div>
    </template>
  </div>
</template>

<style scoped>
/* Root — matches ChainModule shape: a full-height flex column, so the split fills the page and
   the SelectionTable + entry pane each scroll on their own. */
.bb-module {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
  background: var(--cc-bg);
}
.bb-empty { padding: 2rem 1.25rem; }

.bb-bar {
  padding: 0.55rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}
.bb-bar-spacer { flex: 1 1 auto; }

.bb-split {
  flex: 1 1 auto;
  display: flex;
  min-height: 0;
}

/* ── entry list ─────────────────────────────────────────────────────────────── */
/* Width is driven by `usePanelResize` — `flex-shrink: 0` so the flex parent doesn't fight it,
   `position: relative` so the divider anchors to this pane's right edge. */
.bb-list {
  flex-shrink: 0;
  border-right: 1px solid var(--cc-border);
  display: flex; flex-direction: column;
  min-height: 0;
  overflow: hidden;
  position: relative;
}
/* Scroll wrapper: takes the flex space so the TABLE itself sits at natural height at the top.
   Without this, `flex: 1` on the <table> stretches its rows to fill (one row => full-height row
   because table-layout distributes remaining space across cells). */
.bb-list-scroll { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
.bb-list-table { width: 100%; }
.bb-list-title { color: var(--cc-text); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.bb-list-title-profile { font-weight: 600; }
.bb-list-pin { color: var(--cc-accent); margin-right: 4px; font-size: 0.75em; }

/* ── status + outcome chips (list cells + entry-pane outcome row) ────────────── */
/* A compact one-letter status pill; colour-coded so the list is scannable without reading. */
.bb-chip {
  display: inline-flex; align-items: center; justify-content: center;
  gap: 4px;
  font-size: var(--cc-fs-2xs);
  line-height: 1;
  border-radius: var(--cc-radius-xs);
  padding: 2px 6px;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text);
}
.bb-chip-status { width: 18px; height: 18px; padding: 0; font-weight: 600; }
.bb-chip-status-open     { background: var(--cc-surface-2); color: var(--cc-text); }
.bb-chip-status-resolved { background: rgba(16, 185, 129, 0.15); color: rgb(52, 211, 153); border-color: rgba(16, 185, 129, 0.4); }
.bb-chip-status-parked   { background: rgba(148, 163, 184, 0.15); color: var(--cc-text-dim); border-color: rgba(148, 163, 184, 0.4); }
/* Outcome verdict colours — Okabe-Ito blue (good) / vermilion (bad). Red↔green is textbook
   deuteran/protan hostile (see `frontend/src/utils/overlayCompose.ts` for the same reasoning
   on the annotation palette); this pair reads distinct under every common CVD type AND is
   already Cecelia's canonical CVD-safe qualitative palette (`plots/palettes.json:okabe-ito`).
   Colour is not the sole cue — thumbs-up / thumbs-down icons carry the semantic redundantly. */
.bb-chip-outcome-good { background: rgba(86, 180, 233, 0.18); color: rgb(147, 197, 233); border-color: rgba(86, 180, 233, 0.5); }
.bb-chip-outcome-bad  { background: rgba(213, 94, 0, 0.18);   color: rgb(240, 148, 68);   border-color: rgba(213, 94, 0, 0.5); }
/* The divider: a grab strip on the pane's right edge, over the border it sits on. Same shape
   TasksModule uses — 5px wide, `col-resize` cursor, transient accent on hover. */
.bb-divider {
  position: absolute; top: 0; right: 0; bottom: 0;
  width: 5px;
  cursor: col-resize;
  z-index: 4;
}
.bb-divider:hover { background: var(--cc-accent); opacity: 0.35; }

/* ── entry pane ─────────────────────────────────────────────────────────────── */
.bb-pane {
  flex: 1 1 auto;
  padding: 0.75rem 1rem;
  display: flex; flex-direction: column;
  min-height: 0;
  overflow: hidden;
}
/* + .cc-row-loose — only the head's chrome (border / padding) is this class's. */
.bb-pane-head {
  padding-bottom: 0.5rem; margin-bottom: 0.5rem;
  border-bottom: 1px solid var(--cc-border);
}
.bb-pane-title { font-weight: 600; font-size: var(--cc-fs-lg); }
.bb-pane-pin { color: var(--cc-accent); margin-right: 4px; }
.bb-pane-sub { flex: 0 0 auto; }
.bb-pane-empty { padding: 1.5rem 0; text-align: center; }
.bb-pane-loading { padding: 1rem 0; }

/* Outcome row sits under the head bar, above the body — one line, always visible. */
.bb-outcome {
  padding: 0.35rem 0 0.5rem;
  margin-bottom: 0.4rem;
  border-bottom: 1px dashed var(--cc-border);
  flex-shrink: 0;
}
.bb-outcome-note {
  color: var(--cc-text-dim);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  flex: 1 1 auto;
  min-width: 0;
}
.bb-outcome-input {
  flex: 1 1 12rem;
  min-width: 0;
  font-family: var(--cc-mono);
}
.bb-outcome-good.bb-outcome-active { background: rgba(16, 185, 129, 0.2); border-color: rgba(16, 185, 129, 0.6); }
.bb-outcome-bad.bb-outcome-active  { background: rgba(239, 68, 68, 0.2);  border-color: rgba(239, 68, 68, 0.6); }

.bb-versions { min-width: 5.5rem; }

.bb-title-input { flex: 1 1 100%; }
.bb-editor {
  flex: 1 1 auto;
  min-height: 20rem;
  font-family: var(--cc-mono);
  line-height: 1.5;
  padding: 0.5rem;
  resize: vertical;
}
.bb-hint { margin-top: 0.35rem; }

/* ── rendered body ──────────────────────────────────────────────────────────── */
.bb-body {
  flex: 1 1 auto;
  overflow-y: auto;
  color: var(--cc-text);
  line-height: 1.55;
  padding-right: 0.5rem;
}
.bb-body :deep(h1),
.bb-body :deep(h2),
.bb-body :deep(h3) { font-weight: 600; margin: 1rem 0 0.4rem; }
.bb-body :deep(h1) { font-size: 1.25rem; }
.bb-body :deep(h2) { font-size: 1.1rem; }
.bb-body :deep(h3) { font-size: 1rem; }
.bb-body :deep(p) { margin: 0.4rem 0; }
.bb-body :deep(ul), .bb-body :deep(ol) { margin: 0.4rem 0; padding-left: 1.4rem; }
.bb-body :deep(li) { margin: 0.15rem 0; }
.bb-body :deep(a) { color: var(--cc-accent); }
.bb-body :deep(code) {
  font-family: var(--cc-mono);
  background: var(--cc-surface-2);
  padding: 0 4px;
  border-radius: var(--cc-radius-xs);
}
.bb-body :deep(pre) {
  background: var(--cc-surface-2);
  padding: 0.5rem 0.65rem;
  border-radius: var(--cc-radius-sm);
  overflow-x: auto;
}
.bb-body :deep(pre code) { background: none; padding: 0; }
.bb-body :deep(blockquote) {
  border-left: 3px solid var(--cc-border);
  margin: 0.5rem 0;
  padding: 0.15rem 0.6rem;
  color: var(--cc-text-dim);
}
.bb-body :deep(.bb-mermaid) {
  margin: 0.75rem 0;
  padding: 0.5rem;
  background: var(--cc-surface-2);
  border-radius: var(--cc-radius-sm);
  overflow-x: auto;
  text-align: center;
}
.bb-body :deep(.bb-mermaid svg) { max-width: 100%; height: auto; }

/* ── attachments strip ──────────────────────────────────────────────────────── */
.bb-attach {
  border-top: 1px solid var(--cc-border);
  padding-top: 0.5rem; margin-top: 0.5rem;
  flex-shrink: 0;
}
.bb-attach-label { margin-bottom: 0.3rem; }
.bb-attach-strip { display: flex; gap: 0.4rem; overflow-x: auto; }
/* Bare bordered picture button — canonical square thumbnail (same shape Kiwi's row uses). */
.bb-attach-thumb {
  width: 4rem; height: 4rem; padding: 0;
  background: #000;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  overflow: hidden;
  flex-shrink: 0;
  cursor: pointer;
}
.bb-attach-thumb img { width: 100%; height: 100%; object-fit: contain; display: block; }
.bb-attach-fallback {
  width: 100%; height: 100%;
  display: flex; align-items: center; justify-content: center;
  color: var(--cc-text-dim);
}
.bb-attach-thumb:hover { border-color: var(--cc-kiwi); }
</style>
