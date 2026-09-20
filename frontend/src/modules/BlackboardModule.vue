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
import { usePanelResize } from '../composables/usePanelResize'
import {
  listBlackboardEntries, getBlackboardEntry, createBlackboardEntry,
  reviseBlackboardEntry, restoreBlackboardEntry, deleteBlackboardEntry,
  formatWhen, type BlackboardEntrySummary, type BlackboardEntry,
} from '../utils/blackboardApi'
import { renderBlackboardMarkdown, mermaidBlocks } from '../utils/blackboardMd'
import { fetchCaptureEnvelope, type CaptureEnvelope } from '../utils/kiwiCaptures'
import { openViewerWindow } from '../utils/viewerWindow'
import { useViewerStore } from '../stores/viewer'
import { composeImageWithOverlay } from '../utils/overlayCompose'
import { loadImg } from '../plots/export'

const projectMeta = useProjectMetaStore()
const bbStore = useBlackboardStore()
const viewer = useViewerStore()

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
// `attachmentsCount` renders as a small paperclip chip via the cell slot.
const LIST_COLUMNS: SelectionColumn[] = [
  { key: 'title',     label: 'Title',       sortable: true, width: 260 },
  { key: 'updatedAt', label: 'Updated',     sortable: true, width: 80  },
  { key: 'current',   label: 'Ver',         sortable: true, width: 44  },
  { key: 'attachmentsCount', label: '',     sortable: false, width: 36 },
]

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
const paneHtml = computed(() => renderBlackboardMarkdown(paneContent.value))
const hasMermaid = computed(() => mermaidBlocks(paneContent.value).length > 0)

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

/** Open the capture's source image in the browser viewer (opens if not open; focuses if already
 *  open; no-op if already showing THIS image) + restore the exact view the user saw when they
 *  shared + paint the marks as a read-only overlay. Same mechanism the analysis-board's Zoom-to-
 *  source uses: write `pendingViewState` first (persists via localStorage so a fresh popup mount
 *  reads the seed), then `openViewerWindow(...)`. Modern captures carry `viewStateSnapshot` and
 *  restore camera / channels / t / z verbatim; legacy captures fall into the seek-only path. */
function focusCapture(cid: string) {
  const slot = captureCache.value[cid]
  const a = slot?.env?.address
  if (!slot?.env || !a?.imageUid) return
  const t = Array.isArray(a.t) ? a.t[0] : a.t
  const marks = slot.env.overlay ?? []
  const overlay = marks.length > 0
    ? { captureId: cid, marks: marks as unknown[] }
    : undefined
  if (slot.env.viewStateSnapshot) {
    viewer.setPendingViewState({
      viewState: slot.env.viewStateSnapshot,
      overlay, imageUid: a.imageUid,
    })
  } else {
    viewer.setPendingViewState({
      focus: {
        ...(typeof t === 'number' ? { t } : {}),
        ...(typeof a.z === 'number' ? { z: a.z } : {}),
      },
      overlay, imageUid: a.imageUid,
    })
  }
  openViewerWindow({
    projectUid: projectUid.value,
    imageUid: a.imageUid,
    ...(a.valueName ? { valueName: a.valueName } : {}),
  })
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

onMounted(async () => {
  await loadList()
  if (entries.value.length > 0 && !selectedId.value) selectEntry(entries.value[0].entryId)
})
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
                          :rows="entries"
                          :model-value="selectedId"
                          sort-storage-key="cc.blackboard.sort"
                          :row-tooltip="e => e.entryId"
                          @update:model-value="v => selectEntry(String(v ?? ''))">
            <template #cell-title="{ row: e }">
              <span class="bb-list-title">{{ e.title || '(untitled)' }}</span>
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
                No entries yet. Claude can create them via MCP, or click <strong>New entry</strong>.
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
              <span class="bb-pane-title">{{ selected.title }}</span>
              <span class="bb-pane-sub cc-fs-2xs cc-muted">
                updated {{ formatWhen(selected.updatedAt) }}
                <template v-if="selected.current > 0"> · v{{ selected.current }}</template>
              </span>
              <span class="bb-bar-spacer" />
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
                                    @confirm="onDelete" />
            </div>

            <div v-if="entryLoading" class="cc-muted cc-fs-xs bb-pane-loading">Loading…</div>
            <div v-else ref="paneRef" class="bb-body" v-html="paneHtml" />

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
.bb-pane-sub { flex: 0 0 auto; }
.bb-pane-empty { padding: 1.5rem 0; text-align: center; }
.bb-pane-loading { padding: 1rem 0; }

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
