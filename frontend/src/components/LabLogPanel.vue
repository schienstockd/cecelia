<script setup lang="ts">
// The lab-log panel: the human/AI shared analysis memory for the current project. Backed by
// GET/POST /api/lablog (→ {proj}/lab-log.md; see app/src/lab_log.jl, docs/ai-assist/LAB-LOG.md).
// Zero-friction by design: an always-focused entry field at the top, submit on Enter, newest-first
// list with a distinct colour per author, one-click correction (append-only — never edits). Mounted
// as a FloatingPanel in App.vue so it's reachable from any page.
import { ref, computed, watch, nextTick } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import {
  authorKind, correctionPrefill, draftToLines,
  USER_AUTHOR, CORRECTION_AUTHOR, type LabLogEntry,
} from '../utils/labLog'

const pm = useProjectMetaStore()
const settings = useSettingsStore()
const projectUid = computed(() => pm.current?.uid ?? '')

const entries = ref<LabLogEntry[]>([])
const draft = ref('')
const correcting = ref(false)      // next submit is a [User — correction] block
const loading = ref(false)
const busy = ref(false)            // an append is in flight
const capturing = ref(false)       // an activity-capture is in flight
const captureNote = ref('')        // transient result of the last manual capture
const error = ref('')
const inputEl = ref<HTMLTextAreaElement | null>(null)

async function load() {
  error.value = ''
  if (!projectUid.value) { entries.value = []; return }
  loading.value = true
  try {
    const r = await fetch(`/api/lablog?projectUid=${encodeURIComponent(projectUid.value)}`)
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    entries.value = (await r.json()).entries ?? []
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    entries.value = []
  } finally {
    loading.value = false
  }
}

// Append an auto-generated [Cecelia] activity digest. `silent` (auto-on-open) suppresses the
// "nothing new" note. Returns whether anything was captured.
async function capture(silent = false) {
  if (!projectUid.value || capturing.value) return
  capturing.value = true
  error.value = ''
  if (!silent) captureNote.value = ''
  try {
    const r = await fetch('/api/lablog/capture', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    const body = await r.json()
    entries.value = body.entries ?? entries.value
    if (!silent) captureNote.value = body.captured ? 'Captured recent activity.' : 'No new activity.'
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    capturing.value = false
  }
}

// (re)load whenever the open project changes, and on first mount; auto-capture activity if enabled.
watch(projectUid, async () => {
  await load()
  if (settings.labLogAutoContext) capture(true)
}, { immediate: true })

async function submit() {
  const lines = draftToLines(draft.value)
  if (!lines.length || !projectUid.value || busy.value) return
  busy.value = true
  error.value = ''
  try {
    const r = await fetch('/api/lablog/append', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: projectUid.value,
        author: correcting.value ? CORRECTION_AUTHOR : USER_AUTHOR,
        lines,
      }),
    })
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    entries.value = (await r.json()).entries ?? entries.value
    draft.value = ''
    correcting.value = false
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    busy.value = false
  }
}

// Enter submits (zero-friction one-liner); Shift+Enter inserts a newline for multi-line entries.
function onKeydown(e: KeyboardEvent) {
  if (e.key === 'Enter' && !e.shiftKey) { e.preventDefault(); submit() }
}

async function startCorrection(entry: LabLogEntry) {
  correcting.value = true
  draft.value = correctionPrefill(entry)
  await nextTick()
  inputEl.value?.focus()
}

function cancelCorrection() {
  correcting.value = false
  draft.value = ''
}
</script>

<template>
  <div class="ll">
    <!-- entry field: always visible, focused, submit on Enter -->
    <div class="ll-compose" :class="{ correcting }">
      <div v-if="correcting" class="ll-correcting">
        <i class="pi pi-reply" /> correction
        <button class="ll-link" @click="cancelCorrection">cancel</button>
      </div>
      <textarea
        ref="inputEl"
        v-model="draft"
        class="ll-input"
        rows="2"
        :disabled="!projectUid || busy"
        :placeholder="projectUid ? 'Note a decision, the why, an edge case…  (Enter to save)'
                                 : 'Open a project to use the lab log'"
        @keydown="onKeydown"
      />
      <div class="ll-compose-row">
        <span class="ll-hint">Enter to save · Shift+Enter for a new line</span>
        <button class="ll-save" :disabled="!draft.trim() || !projectUid || busy" @click="submit">
          {{ busy ? 'Saving…' : (correcting ? 'Save correction' : 'Save') }}
        </button>
      </div>
    </div>

    <!-- activity capture: manual button + auto-on-open toggle -->
    <div class="ll-toolbar">
      <button class="ll-capture" :disabled="!projectUid || capturing" @click="capture(false)"
              title="Append an app-generated [Cecelia] digest of recent activity (tasks run, …)">
        <i class="pi pi-history" /> {{ capturing ? 'Capturing…' : 'Capture activity' }}
      </button>
      <label class="ll-auto" title="Automatically capture activity when this project opens">
        <input type="checkbox" v-model="settings.labLogAutoContext" /> Auto
      </label>
      <span v-if="captureNote" class="ll-note">{{ captureNote }}</span>
    </div>

    <div v-if="error" class="ll-error">{{ error }}</div>

    <!-- entries, newest-first -->
    <div class="ll-list">
      <div v-if="loading" class="ll-empty">Loading…</div>
      <div v-else-if="!projectUid" class="ll-empty">No project open.</div>
      <div v-else-if="!entries.length" class="ll-empty">
        No entries yet. The first note you save appears here.
      </div>
      <template v-else>
        <div v-for="(e, i) in entries" :key="e.raw + i" class="ll-entry" :class="'k-' + authorKind(e.author)">
          <div class="ll-entry-head">
            <span class="ll-author">{{ e.author }}</span>
            <span class="ll-date">{{ e.date }}</span>
            <button class="ll-link ll-correct" title="Add a correction (never edits the original)"
                    @click="startCorrection(e)">correct</button>
          </div>
          <ul class="ll-lines">
            <li v-for="(ln, j) in e.lines" :key="j">{{ ln }}</li>
          </ul>
        </div>
      </template>
    </div>
  </div>
</template>

<style scoped>
.ll { display: flex; flex-direction: column; height: 100%; font-size: 0.8rem; }

.ll-compose { padding: 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; }
.ll-compose.correcting { background: rgba(210, 153, 34, 0.08); }
.ll-correcting {
  display: flex; align-items: center; gap: 0.35rem;
  font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.04em;
  color: #d29922; margin-bottom: 0.3rem;
}
.ll-input {
  width: 100%; resize: vertical; box-sizing: border-box;
  background: var(--cc-surface-2); color: var(--cc-text);
  border: 1px solid var(--cc-border); border-radius: 0.35rem;
  padding: 0.4rem 0.5rem; font: inherit; line-height: 1.35;
}
.ll-input:focus { outline: none; border-color: var(--cc-accent); }
.ll-input:disabled { opacity: 0.6; }
.ll-compose-row { display: flex; align-items: center; justify-content: space-between; margin-top: 0.35rem; }
.ll-hint { font-size: 0.66rem; color: var(--cc-text-dim); }
.ll-save {
  border: 1px solid var(--cc-accent); background: var(--cc-accent); color: #fff;
  border-radius: 0.35rem; padding: 0.22rem 0.6rem; font-size: 0.72rem; cursor: pointer;
}
.ll-save:disabled { opacity: 0.5; cursor: default; }

.ll-toolbar {
  display: flex; align-items: center; gap: 0.6rem;
  padding: 0.35rem 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0;
}
.ll-capture {
  display: inline-flex; align-items: center; gap: 0.3rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text);
  border-radius: 0.35rem; padding: 0.2rem 0.5rem; font-size: 0.7rem; cursor: pointer;
}
.ll-capture:hover:not(:disabled) { border-color: #8b949e; }
.ll-capture:disabled { opacity: 0.5; cursor: default; }
.ll-auto { display: inline-flex; align-items: center; gap: 0.25rem; font-size: 0.7rem; color: var(--cc-text-dim); cursor: pointer; }
.ll-note { font-size: 0.66rem; color: var(--cc-text-dim); margin-left: auto; }

.ll-error { padding: 0.4rem 0.6rem; color: #f85149; font-size: 0.72rem; }

.ll-list { flex: 1; overflow-y: auto; padding: 0.4rem 0.5rem 0.6rem; }
.ll-empty { color: var(--cc-text-dim); text-align: center; padding: 1.2rem 0.5rem; font-size: 0.74rem; }

.ll-entry {
  border-left: 3px solid var(--cc-border);
  background: var(--cc-surface-2);
  border-radius: 0 0.35rem 0.35rem 0;
  padding: 0.35rem 0.5rem; margin-bottom: 0.4rem;
}
/* distinct colour per author (border + author label), not just a text tag */
.ll-entry.k-claude { border-left-color: var(--cc-accent); }
.ll-entry.k-user { border-left-color: #3fb950; }
.ll-entry.k-correction { border-left-color: #d29922; }
.ll-entry.k-cecelia { border-left-color: #8b949e; }   /* app-generated → muted/ambient */
.ll-entry.k-other { border-left-color: var(--cc-text-dim); }
.k-claude .ll-author { color: var(--cc-accent); }
.k-user .ll-author { color: #3fb950; }
.k-correction .ll-author { color: #d29922; }
.k-cecelia .ll-author { color: #8b949e; }

.ll-entry-head { display: flex; align-items: baseline; gap: 0.5rem; margin-bottom: 0.2rem; }
.ll-author { font-weight: 700; font-size: 0.72rem; }
.ll-date { color: var(--cc-text-dim); font-size: 0.68rem; }
.ll-correct { margin-left: auto; }
.ll-lines { margin: 0; padding-left: 1rem; }
.ll-lines li { margin: 0.05rem 0; line-height: 1.35; color: var(--cc-text); }

.ll-link {
  border: none; background: none; color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.68rem; padding: 0; text-decoration: underline;
}
.ll-link:hover { color: var(--cc-text); }
</style>
