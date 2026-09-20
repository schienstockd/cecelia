<script setup lang="ts">
// Blackboard — the shared markdown surface for a project. A blackboard entry is a note (with
// optional Mermaid diagrams and attached captureIds) that you and Claude iterate on across sessions.
// Not gated on the Claude/observer connection: entries are read from `/api/blackboard/*` on this
// user's own machine, so a bad MCP day still renders every diagram.
//
// Backend: api/src/blackboard_api.jl. Plan: docs/todo/BIDIR_CONTEXT_PLAN.md → Part 4.
//
// Two-pane layout: entry list left, one entry right (viewer OR editor). "New" opens the editor with
// an empty draft; Save creates + selects. Attachment thumbnails at the bottom of the entry pane
// reuse `fetchCaptureEnvelope` from kiwiCaptures.ts and, on click, publish a
// `publishViewerSeek` — a pop-out viewer for that image jumps to the capture's t / z. Silent no-op
// if no pop-out is listening (fire-and-forget, same rule as Kiwi PR B's Refocus).

import { ref, computed, watch, onMounted, onUnmounted, nextTick } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useBlackboardStore } from '../stores/blackboard'
import ModulePage from '../components/ModulePage.vue'
import ConfirmButton from '../components/ConfirmButton.vue'
import {
  listBlackboardEntries, getBlackboardEntry, createBlackboardEntry,
  reviseBlackboardEntry, restoreBlackboardEntry, deleteBlackboardEntry,
  formatWhen, type BlackboardEntrySummary, type BlackboardEntry,
} from '../utils/blackboardApi'
import { renderBlackboardMarkdown, mermaidBlocks } from '../utils/blackboardMd'
import { fetchCaptureEnvelope, type CaptureEnvelope } from '../utils/kiwiCaptures'
import { publishViewerSeek } from '../utils/viewerSeekChannel'

const projectMeta = useProjectMetaStore()
const bbStore = useBlackboardStore()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)

// List state.
const entries = ref<BlackboardEntrySummary[]>([])
const listLoading = ref(false)

// Selected entry state. `selected` is the LIVE entry; `viewingVersion` != null when the user has
// clicked a history version to preview it (the pane shows that snapshot's content, but selected's
// LIVE-state fields still drive the header/actions).
const selectedId = ref('')
const selected = ref<BlackboardEntry | null>(null)
const viewingVersion = ref<number | null>(null)
const viewingContent = ref('')   // content of the version being previewed (falls back to selected.content)
const entryLoading = ref(false)

// Editor state. `mode` is 'view' | 'edit-existing' | 'edit-new'. Draft fields live outside the
// selected entry so a Cancel discards them without touching the fetched state.
type Mode = 'view' | 'edit-existing' | 'edit-new'
const mode = ref<Mode>('view')
const draftTitle = ref('')
const draftContent = ref('')
const savingDraft = ref(false)

// Attachment envelopes for the selected entry — fetched lazily per attachment so the thumbnails
// stream in. Keyed by captureId. A missing entry ⇒ no thumbnail; the click still works if the
// address is known.
const captureCache = ref<Record<string, CaptureEnvelope | null>>({})

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
      }
    }
  } finally { entryLoading.value = false }
  // Prime the attachment cache for the LIVE entry — always the LIVE attachment list, because a
  // historical snapshot's attachments aren't stored (meta.json's attachments are the CURRENT set).
  if (selected.value) {
    for (const cid of selected.value.attachments) {
      if (captureCache.value[cid] === undefined) {
        captureCache.value[cid] = null       // pending
        fetchCaptureEnvelope(projectUid.value, cid).then(env => {
          captureCache.value = { ...captureCache.value, [cid]: env }
        })
      }
    }
  }
}

function selectEntry(id: string) {
  if (selectedId.value === id && mode.value === 'view' && viewingVersion.value === null) return
  selectedId.value = id
  mode.value = 'view'
  loadEntry(id)
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
      // If the title changed too, we'd need a rename endpoint — none exists yet, so preserve the
      // stored title and only save content in this pass. (Title edits land when the backend grows a
      // rename route — noted in the plan as future.)
      const v = await reviseBlackboardEntry(projectUid.value, selected.value.entryId, draftContent.value)
      if (v > 0) {
        await loadEntry(selected.value.entryId)
        mode.value = 'view'
      }
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

/** Fire-and-forget: publish a viewer-seek for this capture. A pop-out viewer for the same imageUid
 *  will jump to the capture's t / z; nothing listening ⇒ silent no-op (that's the fallback shape
 *  Kiwi PR B already established). */
function focusCapture(cid: string) {
  const env = captureCache.value[cid]
  const a = env?.address
  if (!env || !a?.imageUid) return
  const t = Array.isArray(a.t) ? a.t[0] : a.t
  publishViewerSeek({
    projectUid: projectUid.value,
    imageUid: a.imageUid,
    ...(typeof t === 'number' ? { t } : {}),
    ...(typeof a.z === 'number' ? { z: a.z } : {}),
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
      if (seq !== mermaidRenderSeq) return   // superseded by a newer render
      const code = nodes[i].textContent ?? ''
      const id = `bb-mermaid-${Date.now()}-${i}`
      try {
        const { svg } = await mermaid.render(id, code)
        const holder = document.createElement('div')
        holder.className = 'bb-mermaid'
        holder.innerHTML = svg
        nodes[i].parentElement?.replaceWith(holder)
      } catch {
        // Leave the raw code block in place — a broken diagram shouldn't erase the source.
      }
    }
  } catch {
    // Import failed (offline, blocked); leave the raw code block visible.
  }
}
watch(paneHtml, async () => { await nextTick(); await renderMermaidInPane() })

// Silent reload on WS `blackboard:changed` for THIS project. Matches the captures-store pattern.
watch(() => bbStore.tick, async () => {
  if (bbStore.lastProjectUid && bbStore.lastProjectUid !== projectUid.value) return
  await loadList()
  if (selectedId.value) await loadEntry(selectedId.value)
})

// Reload the list when the user switches projects — this page is per-project.
watch(projectUid, async () => {
  selectedId.value = ''
  selected.value = null
  mode.value = 'view'
  captureCache.value = {}
  await loadList()
})

onMounted(async () => {
  await loadList()
  // Auto-select the newest entry on first open — a page that opens to a blank right pane every time
  // is one extra click for the common "read yesterday's note" motion.
  if (entries.value.length > 0 && !selectedId.value) selectEntry(entries.value[0].entryId)
})
onUnmounted(() => { mermaidRenderSeq++ })
</script>

<template>
  <ModulePage layout="fill" class="bb-page">
    <div v-if="!hasProject" class="bb-empty cc-empty-inline">
      <i class="pi pi-lock" /> Open or create a project first.
    </div>

    <div v-else class="bb-split">
      <!-- Entry list -->
      <aside class="bb-list">
        <div class="bb-list-head">
          <span class="bb-list-title">Entries</span>
          <button class="cc-btn cc-btn-primary cc-btn-dense" @click="beginNew"
                  v-tooltip.top="'Start a new blackboard entry'">
            <i class="pi pi-plus" /> New
          </button>
        </div>
        <div v-if="listLoading" class="bb-list-empty cc-muted cc-fs-xs">Loading…</div>
        <div v-else-if="entries.length === 0" class="bb-list-empty cc-muted cc-fs-xs">
          No entries yet. Claude can create them via MCP, or start one here.
        </div>
        <ul v-else class="bb-list-items">
          <li v-for="e in entries" :key="e.entryId"
              class="bb-list-item"
              :class="{ 'is-selected': e.entryId === selectedId }">
            <button class="bb-list-btn cc-btn cc-btn-bare" @click="selectEntry(e.entryId)">
              <span class="bb-list-item-title">{{ e.title || '(untitled)' }}</span>
              <span class="bb-list-item-meta cc-fs-2xs cc-muted">
                <span>{{ formatWhen(e.updatedAt) }}</span>
                <span v-if="e.attachmentsCount > 0">
                  · <i class="pi pi-paperclip" /> {{ e.attachmentsCount }}
                </span>
                <span v-if="e.current > 0">· v{{ e.current }}</span>
              </span>
            </button>
          </li>
        </ul>
      </aside>

      <!-- Entry pane -->
      <section class="bb-pane">
        <!-- Editor (new OR revise) -->
        <template v-if="mode === 'edit-new' || mode === 'edit-existing'">
          <div class="bb-pane-head cc-row cc-row-loose">
            <input v-if="mode === 'edit-new'" v-model="draftTitle"
                   class="bb-title-input" placeholder="Entry title"
                   maxlength="200" type="text"
                   v-tooltip.top="'Short label shown in the entry list (up to 200 chars)'" />
            <span v-else class="bb-pane-title">{{ selected?.title }}</span>
            <div class="bb-pane-actions">
              <button class="cc-btn cc-btn-ghost cc-btn-dense" :disabled="savingDraft" @click="cancelEdit">
                Cancel
              </button>
              <button class="cc-btn cc-btn-primary cc-btn-dense"
                      :disabled="savingDraft || !draftTitle.trim()"
                      @click="saveDraft"
                      v-tooltip.top="mode === 'edit-existing' ? 'Save; previous version is snapshotted automatically' : 'Create this entry'">
                <i class="pi" :class="savingDraft ? 'pi-spin pi-spinner' : 'pi-save'" />
                {{ mode === 'edit-existing' ? 'Save revision' : 'Create entry' }}
              </button>
            </div>
          </div>
          <textarea v-model="draftContent"
                    class="bb-editor"
                    :placeholder="'Markdown. Mermaid diagrams via ```mermaid fences.'"
                    spellcheck="false"
                    v-tooltip.top="'Markdown body — a snapshot of the previous version is taken automatically on save'"></textarea>
          <p class="bb-hint cc-muted cc-fs-2xs">
            Trusted-source markdown (this project only). {{ (draftContent?.length ?? 0).toLocaleString() }} chars.
          </p>
        </template>

        <!-- Viewer -->
        <template v-else-if="selected">
          <div class="bb-pane-head">
            <span class="bb-pane-title">{{ selected.title }}</span>
            <span class="bb-pane-sub cc-fs-2xs cc-muted">
              updated {{ formatWhen(selected.updatedAt) }}
              <template v-if="selected.current > 0"> · v{{ selected.current }}</template>
            </span>
            <div class="bb-pane-actions">
              <select v-if="selected.versions.length > 0"
                      class="bb-versions cc-input-xs"
                      :value="viewingVersion ?? ''"
                      @change="(ev) => {
                        const v = (ev.target as HTMLSelectElement).value
                        if (v === '') loadEntry(selected!.entryId)
                        else loadEntry(selected!.entryId, Number(v))
                      }"
                      v-tooltip.top="'Preview an earlier version'">
                <option value="">Current</option>
                <option v-for="v in [...selected.versions].reverse()" :key="v" :value="v">v{{ v }}</option>
              </select>
              <button v-if="viewingVersion !== null"
                      class="cc-btn cc-btn-ghost cc-btn-dense"
                      @click="restoreVersion(viewingVersion!)"
                      v-tooltip.top="'Restore this version as current; the current live content is snapshotted first'">
                <i class="pi pi-history" /> Restore v{{ viewingVersion }}
              </button>
              <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="beginEditExisting"
                      v-tooltip.top="'Edit; a snapshot is taken automatically before saving'">
                <i class="pi pi-pencil" /> Edit
              </button>
              <ConfirmButton @confirm="onDelete" v-slot="{ armed, arm, confirm, cancel }">
                <button v-if="!armed" class="cc-btn cc-btn-ghost cc-btn-dense bb-del"
                        @click="arm" v-tooltip.top="'Delete this entry (all versions)'">
                  <i class="pi pi-trash" /> Delete
                </button>
                <template v-else>
                  <button class="cc-btn cc-btn-danger cc-btn-dense" @click="confirm">
                    <i class="pi pi-check" /> Delete for real
                  </button>
                  <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="cancel">
                    <i class="pi pi-times" /> Cancel
                  </button>
                </template>
              </ConfirmButton>
            </div>
          </div>

          <div v-if="entryLoading" class="cc-muted cc-fs-xs">Loading…</div>
          <div v-else ref="paneRef" class="bb-body" v-html="paneHtml" />

          <div v-if="selected.attachments.length > 0" class="bb-attach">
            <div class="bb-attach-label cc-muted cc-fs-2xs">Attachments</div>
            <div class="bb-attach-strip">
              <button v-for="cid in selected.attachments" :key="cid"
                      class="bb-attach-thumb cc-btn cc-btn-bare"
                      @click="focusCapture(cid)"
                      v-tooltip.top="`${cid} — click to focus in the pop-out viewer if one is open`">
                <img v-if="captureCache[cid]?.frame" :src="captureCache[cid]!.frame" :alt="cid" />
                <span v-else class="bb-attach-fallback"><i class="pi pi-image" /></span>
              </button>
            </div>
          </div>
        </template>

        <!-- Empty right pane -->
        <div v-else class="bb-pane-empty cc-muted">
          <template v-if="entries.length === 0">
            No entries in this project yet.
          </template>
          <template v-else>
            Pick an entry from the list, or click <strong>New</strong> to start one.
          </template>
        </div>
      </section>
    </div>
  </ModulePage>
</template>

<style scoped>
.bb-page { max-width: 100%; }
.bb-empty { padding: 2rem 0; }

.bb-split {
  display: grid;
  grid-template-columns: 18rem 1fr;
  gap: 1rem;
  height: 100%;
  min-height: 0;
}

/* ── entry list ─────────────────────────────────────────────────────────────── */
.bb-list {
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  background: var(--cc-surface-1);
  display: flex; flex-direction: column;
  min-height: 0;
}
.bb-list-head {
  display: flex; align-items: center; justify-content: space-between;
  padding: 0.5rem 0.6rem;
  border-bottom: 1px solid var(--cc-border);
}
.bb-list-title { font-weight: 600; font-size: var(--cc-fs-md); }
.bb-list-empty { padding: 0.75rem 0.6rem; }
.bb-list-items {
  list-style: none; margin: 0; padding: 0;
  overflow-y: auto; min-height: 0;
}
.bb-list-item + .bb-list-item { border-top: 1px solid var(--cc-border); }
.bb-list-item.is-selected { background: var(--cc-surface-2); }
.bb-list-btn {
  display: flex; flex-direction: column; gap: 0.15rem;
  width: 100%; padding: 0.45rem 0.6rem; text-align: left;
}
.bb-list-btn:hover { background: var(--cc-surface-2); }
.bb-list-item-title {
  font-size: var(--cc-fs-sm); color: var(--cc-text);
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
.bb-list-item-meta { display: flex; align-items: center; gap: 0.25rem; }

/* ── entry pane ─────────────────────────────────────────────────────────────── */
.bb-pane {
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  background: var(--cc-surface-1);
  padding: 0.75rem 1rem;
  display: flex; flex-direction: column;
  min-height: 0;
  overflow: hidden;
}
/* + .cc-row-loose — only the head's own chrome (border/padding). */
.bb-pane-head {
  padding-bottom: 0.5rem; margin-bottom: 0.5rem;
  border-bottom: 1px solid var(--cc-border);
}
.bb-pane-title { font-weight: 600; font-size: var(--cc-fs-lg); flex: 0 1 auto; }
.bb-pane-sub { flex: 1 1 auto; }
.bb-pane-actions { display: flex; align-items: center; gap: 0.35rem; flex-shrink: 0; }
.bb-pane-empty { padding: 1.5rem 0; text-align: center; }

.bb-versions { padding: 0.15rem 0.3rem; min-width: 5.5rem; }
.bb-del :deep(.pi) { color: var(--cc-sev-fail); }

.bb-title-input {
  flex: 1 1 100%;
  font-size: var(--cc-fs-lg);
  padding: 0.3rem 0.4rem;
}
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
  font-size: var(--cc-fs-md);
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
  font-size: 0.9em;
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
.bb-attach-thumb {
  width: 4rem; height: 4rem; padding: 0;
  background: #000;
  border-radius: var(--cc-radius-sm);
  overflow: hidden;
  flex-shrink: 0;
}
.bb-attach-thumb img { width: 100%; height: 100%; object-fit: contain; display: block; }
.bb-attach-fallback {
  width: 100%; height: 100%;
  display: flex; align-items: center; justify-content: center;
  color: var(--cc-text-dim);
}
.bb-attach-thumb:hover { outline: 1px solid var(--cc-kiwi); }
</style>
