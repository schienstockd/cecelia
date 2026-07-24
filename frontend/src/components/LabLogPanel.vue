<script setup lang="ts">
// The lab-log panel: the human/AI shared analysis memory for the current project. Backed by
// GET/POST /api/lablog (→ {proj}/lab-log.md; see app/src/lab_log.jl, docs/ai-assist/LAB-LOG.md).
// Zero-friction by design: an always-focused entry field at the top, submit on Enter, newest-first
// list with a distinct colour per author, one-click correction (append-only — never edits). Mounted
// as a FloatingPanel in App.vue so it's reachable from any page.
import { ref, computed, watch, nextTick } from 'vue'
import { isAuthError, observerSetupReason } from '../utils/observerSetup'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useObserverStore } from '../stores/observer'
import { useLabCaptureStore } from '../stores/labCapture'
import { buildChatPrompt } from '../lib/chatHandoff'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import ClaudeOverviewDialog from './ClaudeOverviewDialog.vue'
import CcToggle from './CcToggle.vue'
import {
  authorKind, correctionPrefill, draftToLines, entryId, decisionPrefill, isRatable, resolveImageRefs,
  visibleEntries as computeVisibleEntries,
  USER_AUTHOR, CORRECTION_AUTHOR, type LabLogEntry, type Vote,
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
const showClaudeOverview = ref(false)   // the "What can Claude do here?" how-to dialog
const captureNote = ref('')        // transient result of the last manual capture
const error = ref('')
const inputEl = ref<HTMLTextAreaElement | null>(null)
const dismissed = ref<string[]>([])            // hidden entry ids (config sidecar, NOT the log — append-only)
const imageNames = ref<Record<string, string>>({})   // uid → current name, for the "Show names" toggle
// AI observer (in-app assistant, on-demand only). State lives in the observer STORE (survives this
// v-if'd panel closing); the panel just drives the "Ask Claude" pass + shows its activity.
const observer = useObserverStore()
const labCapture = useLabCaptureStore()
const chatCopied = ref(false)              // brief "Prompt copied" state on the Chat-to-Claude button
let chatCopiedTimer: ReturnType<typeof setTimeout> | null = null

// Chat to Claude: copy a starter prompt (project context + MCP pointer) to the clipboard for a full
// external session. Re-copies on each click. Works for any MCP assistant — no `claude` install needed.
// No toast — the button flashes "Prompt copied" (colour + tooltip) for a couple of seconds instead.
async function chatToClaude() {
  if (!projectUid.value) return
  const text = buildChatPrompt(projectUid.value, pm.current?.name)
  try {
    await navigator.clipboard.writeText(text)
  } catch {
    const ta = document.createElement('textarea')
    ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = '0'
    document.body.appendChild(ta); ta.select(); document.execCommand('copy'); document.body.removeChild(ta)
  }
  chatCopied.value = true
  if (chatCopiedTimer) clearTimeout(chatCopiedTimer)
  chatCopiedTimer = setTimeout(() => { chatCopied.value = false }, 2500)
}
const observerAvailable = computed(() => observer.available)
const observerBusy = computed(() => observer.busy)
const observerModels = computed(() => observer.models)
const observerSession = computed(() => observer.session)
const observerPasses = computed(() => observer.session?.passes ?? [])   // activity log (newest-first)
const activityOpen = ref(false)              // Claude activity <details> open state (opens after an Ask)
// Setup guidance: availability only means `claude` is on PATH — not logged in. Show install/login
// steps when the CLI is missing, or when the most recent pass failed with an auth-shaped error.
const observerSetup = computed(() =>
  observerSetupReason(observerAvailable.value,
    (observerPasses.value[0] && !observerPasses.value[0].ok && isAuthError(observerPasses.value[0].note)) || false))
const passTokens = (p: { inputTokens: number; outputTokens: number }) => {
  const total = p.inputTokens + p.outputTokens
  return total >= 1000 ? `${(total / 1000).toFixed(1)}k` : `${total}`
}
// running token total for the readout (real usage from the assistant's own output, accumulated
// per project — see docs/todo/OBSERVER_INTEGRATION_PLAN.md Decisions 3/4)
const observerTokens = computed(() => {
  const s = observerSession.value
  if (!s || (s.inputTokens + s.outputTokens) === 0) return ''
  const total = s.inputTokens + s.outputTokens
  const fmt = total >= 1000 ? `${(total / 1000).toFixed(1)}k` : `${total}`
  return `~${fmt} tokens · ${s.turns} turn${s.turns === 1 ? '' : 's'}`
})
// entries shown in the panel: hidden (dismissed) ids filtered out. The log FILE still contains them —
// hide is view-only (a config sidecar), so the append-only methodology record is preserved.
const visibleEntries = computed(() => computeVisibleEntries(entries.value, dismissed.value))

// A displayed bullet line: image references are stored as stable UIDs; when "Show names" is on, swap
// each known UID for its current name (resolved against live project data — a rename shows through
// with no rewrite of the log). Off (default) shows the compact, stable UIDs as stored.
const hasImageNames = computed(() => Object.keys(imageNames.value).length > 0)
function renderLine(line: string): string {
  return settings.labLogShowNames ? resolveImageRefs(line, imageNames.value) : line
}

async function load() {
  error.value = ''
  if (!projectUid.value) { entries.value = []; return }
  loading.value = true
  try {
    const r = await fetch(`/api/lablog?projectUid=${encodeURIComponent(projectUid.value)}`)
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    const body = await r.json()
    entries.value = body.entries ?? []
    dismissed.value = body.dismissed ?? []
    imageNames.value = body.imageNames ?? {}
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    entries.value = []
  } finally {
    loading.value = false
  }
}

// Manual "Capture activity" button + auto-on-open. Routes through the labCapture store (which owns
// the POST, the sidebar badge, and the app-lifetime auto-capture); the entries reload via the
// captureTick watch below. `silent` (auto-on-open) suppresses the "nothing new" note.
async function capture(silent = false) {
  if (!projectUid.value || capturing.value) return
  capturing.value = true
  if (!silent) captureNote.value = ''
  try {
    const body = await labCapture.capture()
    if (body && !silent) captureNote.value = body.captured ? 'Captured recent activity.' : 'No new activity.'
  } finally {
    capturing.value = false
  }
}

// Reload entries whenever a capture appends — manual, auto-on-open, OR the app-lifetime auto-capture
// firing while this panel is open (the store bumps captureTick).
watch(() => labCapture.captureTick, () => { if (projectUid.value) load() })

// Ask the assistant for a one-shot review; it may append a [Claude] entry via the observer MCP. The
// store owns the run (+ session/tokens/badge); the result (verdict + cost) lands in the Claude
// activity log below (observer.session.passes) — no separate transient report block. Open the log so
// the just-run result is visible.
async function askClaude() {
  if (!projectUid.value || observer.busy || !observer.available) return
  error.value = ''
  activityOpen.value = true
  await observer.runPass()
  // the pass + its verdict note appear in observer.session.passes → the activity log; entries reload
  // via the appendTick watch below when a pass actually appended.
}

// Reload the log when an Ask-Claude pass appends (the store bumps appendTick).
watch(() => observer.appendTick, () => { if (projectUid.value) load() })

// Clear context: reset the project's assistant session + token totals (next run starts fresh).
async function clearContext() {
  if (!projectUid.value || observer.busy) return
  await observer.clear()
}

// (re)load whenever the open project changes, and on first mount; auto-capture activity if enabled.
// (Observer status/session is refreshed app-wide by the store — see App.vue.)
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

// Hide an entry from the panel. The lab-log FILE is never edited (append-only); this writes the id to
// a config sidecar and filters it out of the view. Two-click armed via ConfirmDeleteButton.
async function dismissEntry(entry: LabLogEntry) {
  if (!projectUid.value) return
  const id = entryId(entry.raw)
  try {
    const r = await fetch('/api/lablog/dismiss', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, id, dismissed: true }),
    })
    if (!r.ok) throw new Error((await r.json()).error ?? `HTTP ${r.status}`)
    dismissed.value = (await r.json()).dismissed ?? dismissed.value
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

    <!-- controls in two slots: Cecelia (the app's own activity digest) | Claude (the AI assistant) -->
    <div class="ll-toolbar">
      <!-- Cecelia: manual capture + auto-on-open + the view toggle for its uid-based digest -->
      <div class="ll-tb-group">
        <button class="ll-capture" :disabled="!projectUid || capturing" @click="capture(false)"
                v-tooltip.top="'Append an app-generated [Cecelia] digest of recent activity (tasks run, …)'">
          <i class="pi pi-history" /> {{ capturing ? 'Capturing…' : 'Capture activity' }}
        </button>
        <CcToggle class="ll-auto" v-model="settings.labLogAutoContext" label="Auto"
          v-tooltip.top="'Auto-capture Cecelia activity digests — when this project opens and after tasks finish'" />
        <!-- image refs are stored as stable UIDs; opt in to showing current names -->
        <CcToggle v-if="projectUid && hasImageNames" class="ll-auto" v-model="settings.labLogShowNames" label="Show names"
          v-tooltip.top="'Image references are stored as stable IDs (names can change). Show their current names instead.'" />
      </div>

      <span class="ll-tb-sep" aria-hidden="true" />

      <!-- Claude: the AI assistant (in-app one-shot + external chat handoff) -->
      <div class="ll-tb-group">
        <button class="ll-help" @click="showClaudeOverview = true"
                v-tooltip.top="'What can Claude do here? Ask vs Chat, what it sees / suggests / creates'">
          <i class="pi pi-question-circle" />
        </button>
        <button class="ll-capture" :disabled="!projectUid || observerBusy || !observerAvailable"
                @click="askClaude"
                v-tooltip.top="observerAvailable
                  ? 'Ask the assistant to review recent activity and note anything worth flagging in the lab log'
                  : 'Needs Claude Code — with it, an assistant can watch your analysis and note things in the lab log'">
          <i class="pi pi-sparkles" /> {{ observerBusy ? 'Asking…' : 'Ask Claude' }}
        </button>
        <select v-if="observerAvailable" class="ll-model" v-model="settings.labLogObserverModel"
                v-tooltip.top="'Which model Ask Claude runs. Sonnet is the default; Haiku is cheapest, Opus is overkill here.'">
          <option v-for="m in observerModels" :key="m" :value="m">{{ m }}</option>
        </select>
        <!-- Chat to Claude: hand off to a FULL external session (any MCP assistant), not the in-app
             one-shot. Copies a starter prompt; no `claude` install needed. -->
        <button class="ll-capture" :class="{ copied: chatCopied }" :disabled="!projectUid" @click="chatToClaude"
                v-tooltip.top="chatCopied ? 'Prompt copied — paste it into Claude (or any MCP chat bot)'
                  : 'Copy a starter prompt to your clipboard — paste it into Claude Code (or any MCP assistant) for a full chat about this project'">
          <i :class="['pi', chatCopied ? 'pi-check' : 'pi-comments']" /> {{ chatCopied ? 'Copied' : 'Chat to Claude' }}
        </button>
        <span v-if="observerTokens" class="ll-tokens"
              v-tooltip.top="'Assistant token use for this observer session (real usage)'">{{ observerTokens }}</span>
        <button v-if="observerTokens" class="ll-clearctx" @click="clearContext"
                v-tooltip.top="'Clear the assistant session and reset the token count'">clear</button>
      </div>

      <span v-if="captureNote" class="ll-note">{{ captureNote }}</span>
    </div>

    <!-- Set-up guidance: the integration needs NO config — just Claude Code installed + logged in.
         Shown when the CLI is missing, or when a run failed because it isn't authenticated. -->
    <div v-if="observerSetup" class="ll-setup">
      <template v-if="observerSetup === 'missing'">
        <strong>Claude Code not detected.</strong> To let an assistant watch your analysis and note
        things here: install Claude Code so <code>claude</code> is on your PATH, then run
        <code>claude</code> once to log in. No other setup — Cecelia wires the tools automatically.
      </template>
      <template v-else>
        <strong>Claude Code isn't logged in.</strong> The last run failed to authenticate — run
        <code>claude</code> in a terminal once to log in, then try again. (Nothing else to configure.)
      </template>
      <a href="https://docs.anthropic.com/en/docs/claude-code/setup" target="_blank" rel="noopener">Setup guide ↗</a>
    </div>

    <!-- Claude activity log: every Ask-Claude pass — its verdict (note), token cost, and outcome. This
         is where an Ask-Claude result lands (no separate transient block); opens after an Ask so the
         result is visible. Each entry is tagged "Ask" (sparkles) — an explicit on-demand run. -->
    <details v-if="observerAvailable && observerPasses.length" class="ll-activity"
             :open="activityOpen" @toggle="activityOpen = ($event.target as HTMLDetailsElement).open">
      <summary>Claude activity ({{ observerPasses.length }})</summary>
      <div v-for="(p, i) in observerPasses" :key="i" class="ll-pass" :class="{ appended: p.appended, failed: !p.ok }">
        <div class="ll-pass-head">
          <span class="ll-pass-trig"><i class="pi pi-sparkles" /> Ask</span>
          <span class="ll-pass-meta">{{ p.model }} · {{ passTokens(p) }} tok<span v-if="p.appended"> · wrote</span><span v-else-if="!p.ok"> · error</span></span>
          <span class="ll-pass-at">{{ p.at }}</span>
        </div>
        <div v-if="p.note" class="ll-pass-note">{{ p.note }}</div>
      </div>
    </details>

    <div v-if="error" class="ll-error">{{ error }}</div>

    <!-- entries, newest-first -->
    <div class="ll-list">
      <div v-if="loading" class="ll-empty">Loading…</div>
      <div v-else-if="!projectUid" class="ll-empty">No project open.</div>
      <div v-else-if="!visibleEntries.length" class="ll-empty">
        No entries yet. The first note you save appears here.
      </div>
      <template v-else>
        <div v-for="(e, i) in visibleEntries" :key="e.raw + i" class="ll-entry" :class="'k-' + authorKind(e.author)">
          <div class="ll-entry-head">
            <span class="ll-author">{{ e.author }}</span>
            <span class="ll-date">{{ e.date }}</span>
            <span class="ll-actions">
              <template v-if="isRatable(e.author)">
                <button class="ll-thumb" v-tooltip.top="'Good decision — add a note'"
                        @click="rateDecision(e, 'up')">👍</button>
                <button class="ll-thumb" v-tooltip.top="'Bad decision — add a note'"
                        @click="rateDecision(e, 'down')">👎</button>
                <button class="ll-link" v-tooltip.top="'Comment (saved as a note)'"
                        @click="startComment(e)">💬</button>
              </template>
              <button v-else class="ll-link" v-tooltip.top="'Add a correction (never edits the original)'"
                      @click="startCorrection(e)">correct</button>
              <!-- Hide this entry (view-only — the log file is append-only, never edited). The ONE
                   app-wide delete affordance: single button, arm on first click, hide on second. -->
              <ConfirmDeleteButton title="Hide this entry (view only — the log file is kept)"
                                   armed-title="Click again to hide" @confirm="dismissEntry(e)" />
            </span>
          </div>
          <ul class="ll-lines">
            <li v-for="(ln, j) in e.lines" :key="j">{{ renderLine(ln) }}</li>
          </ul>
        </div>
      </template>
    </div>

    <ClaudeOverviewDialog v-if="showClaudeOverview" @close="showClaudeOverview = false" />
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
  display: flex; flex-wrap: wrap; align-items: center; gap: 0.4rem 0.6rem;
  padding: 0.35rem 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0;
}
/* two control slots (Cecelia | Claude), each an inline row; the divider sits between them and the
   whole bar wraps as a unit when the panel is narrow (a group drops to the next line intact). */
.ll-tb-group { display: inline-flex; align-items: center; gap: 0.5rem; }
.ll-tb-sep { align-self: stretch; width: 1px; min-height: 1.1rem; background: var(--cc-border); }
.ll-capture {
  display: inline-flex; align-items: center; gap: 0.3rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text);
  border-radius: 0.35rem; padding: 0.2rem 0.5rem; font-size: 0.7rem; cursor: pointer;
}
.ll-capture:hover:not(:disabled) { border-color: #8b949e; }
/* brief "copied" flash on the Chat-to-Claude button (replaces the toast) */
.ll-capture.copied { color: var(--cc-sev-ok); border-color: var(--cc-sev-ok); background: rgba(12, 163, 12, 0.1); }
.ll-capture:disabled { opacity: 0.5; cursor: default; }
.ll-help {
  display: inline-flex; align-items: center; border: none; background: none;
  color: var(--cc-text-dim); cursor: pointer; padding: 0.2rem; font-size: 0.85rem;
}
.ll-help:hover { color: var(--cc-accent); }
.ll-auto { display: inline-flex; align-items: center; gap: 0.25rem; font-size: 0.7rem; color: var(--cc-text-dim); cursor: pointer; }
.ll-model {
  font-size: 0.66rem; color: var(--cc-text-dim); cursor: pointer; padding: 0.05rem 0.2rem;
  background: var(--cc-surface); border: 1px solid var(--cc-border); border-radius: 3px;
}
/* capture status: floats to the far right of the whole bar (direct toolbar child) */
.ll-note { font-size: 0.66rem; color: var(--cc-text-dim); margin-left: auto; }
/* token readout sits inline within the Claude group (no auto-margin — it's not a toolbar child) */
.ll-tokens { font-size: 0.66rem; color: var(--cc-text-dim); }
.ll-clearctx {
  border: none; background: transparent; color: var(--cc-text-dim);
  font-size: 0.66rem; cursor: pointer; text-decoration: underline; padding: 0;
}
.ll-clearctx:hover { color: var(--cc-text); }
/* setup hint — install/login guidance when Claude Code is missing or not authenticated */
.ll-setup {
  flex-shrink: 0; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-2); padding: 0.4rem 0.6rem;
  font-size: 0.68rem; color: var(--cc-text-dim); line-height: 1.5;
}
.ll-setup strong { color: var(--cc-text); }
.ll-setup code {
  font-size: 0.64rem; padding: 0 0.2rem; border-radius: 3px;
  background: var(--cc-surface); border: 1px solid var(--cc-border);
}
.ll-setup a { margin-left: 0.3rem; color: var(--cc-accent); white-space: nowrap; }
/* Claude activity log — collapsible; each Ask-Claude pass with its verdict, cost + outcome */
.ll-activity {
  flex-shrink: 0; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-2); padding: 0.3rem 0.6rem; max-height: 11rem; overflow-y: auto;
}
.ll-activity > summary {
  font-size: 0.66rem; color: var(--cc-text-dim); cursor: pointer; user-select: none;
}
.ll-pass { margin-top: 0.35rem; padding-left: 0.4rem; border-left: 2px solid var(--cc-border); }
.ll-pass.appended { border-left-color: var(--cc-accent); }
.ll-pass.failed   { border-left-color: #f85149; }
.ll-pass-head { display: flex; align-items: baseline; gap: 0.35rem; font-size: 0.64rem; }
.ll-pass-trig { display: inline-flex; align-items: center; gap: 0.2rem; font-weight: 600; color: var(--cc-accent); }
.ll-pass-trig .pi { font-size: 0.6rem; }
.ll-pass-meta { color: var(--cc-text-dim); }
.ll-pass-at   { margin-left: auto; color: var(--cc-text-dim); opacity: 0.8; }
.ll-pass-note { font-size: 0.7rem; color: var(--cc-text); line-height: 1.4; white-space: pre-wrap; margin-top: 0.1rem; }

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
/* per-entry actions: hidden until hover (thumbs prefill a note — they carry no persisted state) */
.ll-actions { margin-left: auto; display: inline-flex; align-items: center; gap: 0.15rem; visibility: hidden; }
.ll-entry:hover .ll-actions { visibility: visible; }
.ll-thumb {
  border: none; background: none; cursor: pointer; font-size: 0.8rem; line-height: 1;
  padding: 0 0.1rem; opacity: 0.8; filter: grayscale(0.5);
}
.ll-thumb:hover { opacity: 1; filter: none; }
.ll-lines { margin: 0; padding-left: 1rem; }
.ll-lines li { margin: 0.05rem 0; line-height: 1.35; color: var(--cc-text); }

.ll-link {
  border: none; background: none; color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.68rem; padding: 0; text-decoration: underline;
}
.ll-link:hover { color: var(--cc-text); }
</style>
