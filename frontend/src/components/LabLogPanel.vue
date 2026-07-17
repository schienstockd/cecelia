<script setup lang="ts">
// The lab-log panel: the human/AI shared analysis memory for the current project. Backed by
// GET/POST /api/lablog (→ {proj}/lab-log.md; see app/src/lab_log.jl, docs/ai-assist/LAB-LOG.md).
// Zero-friction by design: an always-focused entry field at the top, submit on Enter, newest-first
// list with a distinct colour per author, one-click correction (append-only — never edits). Mounted
// as a FloatingPanel in App.vue so it's reachable from any page.
import { ref, computed, watch, nextTick, onUnmounted } from 'vue'
import { useWsStore } from '../stores/ws'
import { isObserverTrigger, OBSERVER_AUTO_FRAME_TYPES } from '../utils/observerAuto'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import {
  authorKind, correctionPrefill, draftToLines, entryId, decisionPrefill, isRatable, muteChips,
  USER_AUTHOR, CORRECTION_AUTHOR, type LabLogEntry, type Vote,
} from '../utils/labLog'
import { observerApi, type ObserverSession } from '../utils/serviceApi'

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
const tuning = ref<Record<string, Vote>>({})   // entryId → tuning vote (config, NOT the log)
const mutes = ref<string[]>([])                // muted digest categories (config, NOT the log)
const categories = ref<string[]>([])           // all digest categories (task-manager tags), for mute chips
const mode = computed(() => settings.labLogMode)
// AI observer (in-app assistant) — availability gates a disabled-with-why button (see
// docs/todo/OBSERVER_INTEGRATION_PLAN.md); the run is one-shot and may append a [Claude] entry.
const observerAvailable = ref(false)
const observerBusy = ref(false)
const observerNote = ref('')
const observerSession = ref<ObserverSession | null>(null)
// running token total for the readout (real usage from the assistant's own output, accumulated
// per project — see docs/todo/OBSERVER_INTEGRATION_PLAN.md Decisions 3/4)
const observerTokens = computed(() => {
  const s = observerSession.value
  if (!s || (s.inputTokens + s.outputTokens) === 0) return ''
  const total = s.inputTokens + s.outputTokens
  const fmt = total >= 1000 ? `${(total / 1000).toFixed(1)}k` : `${total}`
  return `~${fmt} tokens · ${s.turns} turn${s.turns === 1 ? '' : 's'}`
})
// chips include any orphaned mute (category since renamed/removed) so it can always be un-muted
const muteableCategories = computed(() => muteChips(categories.value, mutes.value))
const voteOf = (e: LabLogEntry): Vote | undefined => tuning.value[entryId(e.raw)]

async function load() {
  error.value = ''
  if (!projectUid.value) { entries.value = []; return }
  loading.value = true
  try {
    const r = await fetch(`/api/lablog?projectUid=${encodeURIComponent(projectUid.value)}`)
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    const body = await r.json()
    entries.value = body.entries ?? []
    tuning.value = body.tuning ?? {}
    mutes.value = body.mutes ?? []
    categories.value = body.categories ?? []
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

// Ask the assistant for a one-shot review; it may append a [Claude] entry via the observer MCP.
async function askClaude() {
  if (!projectUid.value || observerBusy.value || !observerAvailable.value) return
  observerBusy.value = true
  observerNote.value = ''
  error.value = ''
  try {
    const res = await observerApi.feedback(projectUid.value)
    if (res.session) observerSession.value = res.session   // updated running token total
    if (res.available === false) {
      observerAvailable.value = false
      observerNote.value = res.error ?? 'Assistant unavailable.'
    } else if (res.ok) {
      observerNote.value = (res.message ?? '').trim() || 'Reviewed — nothing to flag.'
      const claudeBefore = entries.value.filter(e => authorKind(e.author) === 'claude').length
      await load()   // surface any new [Claude] entry the assistant appended
      const claudeNow = entries.value.filter(e => authorKind(e.author) === 'claude')
      // if it actually appended AND the panel is closed, flag the sidebar badge with a preview
      if (claudeNow.length > claudeBefore && !settings.labLogPanelOpen) {
        const line = (claudeNow[0]?.lines ?? []).find(l => l.trim()) ?? 'added a note'
        settings.labLogUnseen = line.replace(/^[-*]\s*/, '').trim()
      }
    } else {
      observerNote.value = res.error ?? 'The assistant could not complete.'
    }
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    observerBusy.value = false
  }
}

// ── "sit next to me" auto-mode: a finished task triggers a debounced observer pass ──────────────
// Frontend-driven (fires while the app is open); subscribes to task/chain terminal frames only while
// the toggle is on + an assistant is available. Debounced so a burst of completions → one pass.
const ws = useWsStore()
const AUTO_DEBOUNCE_MS = 8000
let autoTimer: ReturnType<typeof setTimeout> | null = null
function onTaskFrame(frame: any) {
  if (!settings.labLogObserverAuto || !observerAvailable.value || !projectUid.value) return
  if (!isObserverTrigger(frame)) return
  if (autoTimer) clearTimeout(autoTimer)
  autoTimer = setTimeout(() => {
    autoTimer = null
    if (settings.labLogObserverAuto && observerAvailable.value && !observerBusy.value) askClaude()
  }, AUTO_DEBOUNCE_MS)
}
watch(() => settings.labLogObserverAuto, (on) => {
  OBSERVER_AUTO_FRAME_TYPES.forEach(t => on ? ws.on(t, onTaskFrame) : ws.off(t, onTaskFrame))
}, { immediate: true })
onUnmounted(() => {
  OBSERVER_AUTO_FRAME_TYPES.forEach(t => ws.off(t, onTaskFrame))
  if (autoTimer) clearTimeout(autoTimer)
})

// Clear context: reset the project's assistant session + token totals (next run starts fresh).
async function clearContext() {
  if (!projectUid.value || observerBusy.value) return
  try {
    const res = await observerApi.clear(projectUid.value)
    observerSession.value = res.session ?? null
    observerNote.value = ''
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
}

// (re)load whenever the open project changes, and on first mount; auto-capture activity if enabled.
watch(projectUid, async () => {
  observerNote.value = ''
  observerApi.status(projectUid.value).then(s => {
    observerAvailable.value = s.available
    observerSession.value = s.session ?? null
  })
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

// Notes mode: a thumb on a decision → prefilled [User] note (verdict + a place for the why). The
// note is the recorded content; submit() uses USER_AUTHOR (correcting stays false).
async function rateDecision(entry: LabLogEntry, vote: Vote) {
  correcting.value = false
  draft.value = decisionPrefill(entry, vote)
  await nextTick(); inputEl.value?.focus()
}
async function startComment(entry: LabLogEntry) {
  correcting.value = false
  draft.value = `re ${entry.date} [${entry.author}]: `
  await nextTick(); inputEl.value?.focus()
}

// Tuning mode: a thumb rates the ENTRY TYPE (useful/noise) → config sidecar; clicking the active
// vote again clears it. Never touches the log.
async function tune(entry: LabLogEntry, vote: Vote) {
  if (!projectUid.value) return
  const id = entryId(entry.raw)
  const next: '' | Vote = tuning.value[id] === vote ? '' : vote
  try {
    const r = await fetch('/api/lablog/tune', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, id, vote: next }),
    })
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    tuning.value = (await r.json()).tuning ?? {}
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
}

// Mute/unmute a whole digest category from future captures (config sidecar, not the log).
async function toggleMute(category: string) {
  if (!projectUid.value) return
  const muted = !mutes.value.includes(category)
  try {
    const r = await fetch('/api/lablog/mute', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, category, muted }),
    })
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    mutes.value = (await r.json()).mutes ?? []
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
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
      <button class="ll-capture" :disabled="!projectUid || observerBusy || !observerAvailable"
              @click="askClaude"
              :title="observerAvailable
                ? 'Ask the assistant to review recent activity and note anything worth flagging in the lab log'
                : 'Needs Claude Code — with it, an assistant can watch your analysis and note things in the lab log'">
        <i class="pi pi-sparkles" /> {{ observerBusy ? 'Asking…' : 'Ask Claude' }}
      </button>
      <label v-if="observerAvailable" class="ll-auto"
             title="Sit next to me: after a task finishes, Claude reviews and may note something in the lab log (spends tokens)">
        <input type="checkbox" v-model="settings.labLogObserverAuto" /> Watch
      </label>
      <span v-if="observerTokens" class="ll-tokens"
            title="Assistant token use for this project's observer session (real usage)">{{ observerTokens }}</span>
      <button v-if="observerTokens" class="ll-clearctx" @click="clearContext"
              title="Clear the assistant's session and reset the token count">clear</button>
      <span v-if="captureNote" class="ll-note">{{ captureNote }}</span>
    </div>

    <!-- assistant report: the full text of the last Ask-Claude pass, in a readable block -->
    <div v-if="observerNote" class="ll-observer-report">
      <div class="ll-observer-head"><i class="pi pi-sparkles" /> Claude</div>
      <div class="ll-observer-body">{{ observerNote }}</div>
    </div>

    <!-- feedback mode: what 👍/👎 mean on the auto/AI entries -->
    <div class="ll-modebar">
      <span class="ll-modelabel">Rating:</span>
      <button class="ll-modebtn" :class="{ on: mode === 'notes' }" @click="settings.labLogMode = 'notes'"
              title="Thumbs + comment judge the DECISION → saved as a note in the log">decisions</button>
      <button class="ll-modebtn" :class="{ on: mode === 'tuning' }" @click="settings.labLogMode = 'tuning'"
              title="Thumbs judge the ENTRY TYPE (useful / noise) → tunes what gets logged, not the log">entry types</button>
    </div>

    <!-- mute whole categories from future digests (Tuning mode) -->
    <div v-if="mode === 'tuning' && projectUid" class="ll-mutebar">
      <span class="ll-modelabel">Mute:</span>
      <button v-for="c in muteableCategories" :key="c" class="ll-mutebtn"
              :class="{ muted: mutes.includes(c) }" @click="toggleMute(c)"
              :title="mutes.includes(c) ? `${c} muted — click to log again` : `Stop logging ${c}`">
        <i :class="['pi', mutes.includes(c) ? 'pi-bell-slash' : 'pi-bell']" /> {{ c }}
      </button>
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
            <span class="ll-actions">
              <template v-if="isRatable(e.author)">
                <button class="ll-thumb" :class="{ voted: mode === 'tuning' && voteOf(e) === 'up' }"
                        :title="mode === 'notes' ? 'Good decision — add a note' : 'Useful entry type'"
                        @click="mode === 'notes' ? rateDecision(e, 'up') : tune(e, 'up')">👍</button>
                <button class="ll-thumb" :class="{ voted: mode === 'tuning' && voteOf(e) === 'down' }"
                        :title="mode === 'notes' ? 'Bad decision — add a note' : 'Noisy entry type'"
                        @click="mode === 'notes' ? rateDecision(e, 'down') : tune(e, 'down')">👎</button>
                <button v-if="mode === 'notes'" class="ll-link" title="Comment (saved as a note)"
                        @click="startComment(e)">💬</button>
              </template>
              <button v-else class="ll-link" title="Add a correction (never edits the original)"
                      @click="startCorrection(e)">correct</button>
            </span>
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
.ll-tokens { font-size: 0.66rem; color: var(--cc-text-dim); margin-left: auto; }
.ll-clearctx {
  border: none; background: transparent; color: var(--cc-text-dim);
  font-size: 0.66rem; cursor: pointer; text-decoration: underline; padding: 0;
}
.ll-clearctx:hover { color: var(--cc-text); }
/* the last Ask-Claude report — its own readable block, not crammed in the toolbar */
.ll-observer-report {
  flex-shrink: 0; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-2); padding: 0.4rem 0.6rem; max-height: 8rem; overflow-y: auto;
}
.ll-observer-head {
  display: inline-flex; align-items: center; gap: 0.3rem;
  font-size: 0.66rem; color: var(--cc-text-dim); margin-bottom: 0.2rem;
}
.ll-observer-body {
  font-size: 0.72rem; color: var(--cc-text); line-height: 1.45; white-space: pre-wrap;
}

.ll-modebar {
  display: flex; align-items: center; gap: 0.35rem;
  padding: 0.3rem 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; font-size: 0.68rem;
}
.ll-modelabel { color: var(--cc-text-dim); }
.ll-modebtn {
  border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text-dim);
  border-radius: 0.3rem; padding: 0.1rem 0.45rem; font-size: 0.66rem; cursor: pointer;
}
.ll-modebtn.on { color: var(--cc-text); border-color: #8b949e; background: rgba(139, 148, 158, 0.15); }

.ll-mutebar {
  display: flex; align-items: center; gap: 0.35rem; flex-wrap: wrap;
  padding: 0.3rem 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; font-size: 0.68rem;
}
.ll-mutebtn {
  display: inline-flex; align-items: center; gap: 0.25rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text-dim);
  border-radius: 0.3rem; padding: 0.1rem 0.4rem; font-size: 0.64rem; cursor: pointer;
}
.ll-mutebtn:hover { color: var(--cc-text); }
.ll-mutebtn.muted { color: #d29922; border-color: #d29922; background: rgba(210, 153, 34, 0.12); }

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
/* per-entry actions: hidden until hover; an active tuning vote stays visible so ratings show at a glance */
.ll-actions { margin-left: auto; display: inline-flex; align-items: center; gap: 0.15rem; visibility: hidden; }
.ll-entry:hover .ll-actions { visibility: visible; }
.ll-thumb {
  border: none; background: none; cursor: pointer; font-size: 0.8rem; line-height: 1;
  padding: 0 0.1rem; opacity: 0.8; filter: grayscale(0.5);
}
.ll-thumb:hover { opacity: 1; filter: none; }
.ll-thumb.voted { visibility: visible; opacity: 1; filter: none; }   /* rated → always shown */
.ll-lines { margin: 0; padding-left: 1rem; }
.ll-lines li { margin: 0.05rem 0; line-height: 1.35; color: var(--cc-text); }

.ll-link {
  border: none; background: none; color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.68rem; padding: 0; text-decoration: underline;
}
.ll-link:hover { color: var(--cc-text); }
</style>
