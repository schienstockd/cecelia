<script setup lang="ts">
// The lab-log panel: the human/AI shared analysis memory for the current project. Backed by
// GET/POST /api/lablog (→ {proj}/lab-log.md; see app/src/lab_log.jl, docs/ai-assist/LAB-LOG.md).
// Zero-friction by design: an always-focused entry field at the top, submit on Enter, newest-first
// list with a distinct colour per author, one-click correction (append-only — never edits). Mounted
// as a FloatingPanel in App.vue so it's reachable from any page.
import { ref, computed, watch, nextTick, onMounted } from 'vue'
import { isAuthError, observerSetupReason, terminalCta, terminalSetupTooltip } from '../utils/observerSetup'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useObserverStore } from '../stores/observer'
import { useLabCaptureStore } from '../stores/labCapture'
import { buildChatPrompt } from '../lib/chatHandoff'
import { useCopyFlash } from '../composables/useCopyFlash'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import ClaudeOverviewDialog from './ClaudeOverviewDialog.vue'
import CcToggle from './CcToggle.vue'
import {
  authorKind, correctionPrefill, draftToLines, entryId, decisionPrefill, isRatable, resolveImageRefs,
  visibleEntries as computeVisibleEntries,
  hasLabArchives, labArchivesLabel, labArchivesSyncedOn, labArchivesGapText, type LabArchivesCtx,
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
// LabArchives context (the experiment, from the lab's ELN). Rides along with the lab-log payload —
// the card lives in this panel, so a second round-trip buys nothing. See utils/labLog.ts.
const labarchives = ref<LabArchivesCtx>({ present: false })
// AI observer (in-app assistant, on-demand only). State lives in the observer STORE (survives this
// v-if'd panel closing); the panel just drives the "Ask Claude" pass + shows its activity.
const observer = useObserverStore()
const labCapture = useLabCaptureStore()
// brief "Copied" state on the Chat-to-Claude button — 2.5s, longer than the default flash
// because the user has to go and paste it somewhere else. Shared helper (docs/ui/PRIMITIVES.md).
const { isCopied: chatCopied, copy: copyPrompt } = useCopyFlash(2500)

// Chat to Claude: copy a one-line starter (which project + the MCP pointer) to the clipboard for a
// full external session. Re-copies on each click. Works for any MCP assistant — no `claude` install
// needed. No toast — the button flashes "Copied" (colour + tooltip) for a couple of seconds instead.
// The observer's rules are NOT in this line; the MCP server delivers them (see lib/chatHandoff.ts).
async function chatToClaude() {
  if (!projectUid.value) return
  await copyPrompt(buildChatPrompt(projectUid.value, pm.current?.name))
}
// Which terminal button the toolbar shows: 'setup' / 'resync' (not set up, or stale) vs 'chat'.
const terminalCtaMode = computed(() => terminalCta(observer.available, observer.terminalState))

const observerAvailable = computed(() => observer.available)
const observerBusy = computed(() => observer.busy)
const observerModels = computed(() => observer.models)
const observerSession = computed(() => observer.session)
const observerPasses = computed(() => observer.session?.passes ?? [])   // activity log (newest-first)
const activityOpen = ref(false)              // Claude activity <details> open state (opens after an Ask)
// Set-up guidance: availability only means `claude` is on PATH — not logged in. Show install/login
// steps when the CLI is missing, or when the most recent pass failed with an auth-shaped error.
// This is a CONDITIONAL alert at the point of use — it renders only when something is broken, and
// vanishes the moment it works. Settings → MCP connections carries the same state as a durable row
// (via the same `observerSetupReason`), which is where you go to check rather than to be told.
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
    labarchives.value = body.labarchives ?? { present: false }
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
// (Observer status/session is refreshed app-wide by the store — see App.vue.) Refresh it again on
// mount so the terminal-setup button reflects the config as of NOW: the user may have registered (or
// broken) `cecelia-observer` in a terminal since the app loaded, and this panel is where they'd look.
watch(projectUid, async () => {
  await load()
  if (settings.labLogAutoContext) capture(true)
}, { immediate: true })
onMounted(() => observer.refresh())

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
        <button class="ll-link" @click="cancelCorrection">Cancel</button>
      </div>
      <textarea
        ref="inputEl"
        v-model="draft"
        v-tooltip.top="'What you observed; Claude reads this as lab context'"
        class="ll-input"
        rows="2"
        :disabled="!projectUid || busy"
        :placeholder="projectUid ? 'Note a decision, the why, an edge case…  (Enter to save)'
                                 : 'Open a project to use the lab log'"
        @keydown="onKeydown"
      />
      <div class="ll-compose-row">
        <span class="ll-hint cc-muted cc-fs-xs">Enter to save · Shift+Enter for a new line</span>
        <button class="ll-save" :disabled="!draft.trim() || !projectUid || busy" @click="submit">
          {{ busy ? 'Saving…' : (correcting ? 'Save correction' : 'Save') }}
        </button>
      </div>
    </div>

    <!-- controls in two slots: Cecelia (the app's own activity digest) | Claude (the AI assistant) -->
    <div class="ll-toolbar cc-row">
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
          v-tooltip.top="'Show current image names instead of stable IDs'" />
      </div>

      <span class="ll-tb-sep" aria-hidden="true" />

      <!-- Claude: the AI assistant (in-app one-shot + external chat handoff) -->
      <div class="ll-tb-group">
        <button class="ll-help cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" data-guide="lablog.claudeHelp"
                @click="showClaudeOverview = true"
                v-tooltip.top="'What can Claude do here? Ask vs Chat, what it sees / suggests / creates'">
          <i class="pi pi-question-circle" />
        </button>
        <button class="ll-capture" :disabled="!projectUid || observerBusy || !observerAvailable"
                @click="askClaude"
                v-tooltip.top="observerAvailable
                  ? 'Review recent activity and note anything worth flagging'
                  : 'Needs Claude Code'">
          <i class="pi pi-sparkles" /> {{ observerBusy ? 'Asking…' : 'Ask Claude' }}
        </button>
        <select v-if="observerAvailable" class="ll-model" v-model="settings.labLogObserverModel"
                v-tooltip.top="'Model Ask Claude runs'">
          <option v-for="m in observerModels" :key="m" :value="m">{{ m }}</option>
        </select>
        <!-- ONE slot, two states. Until the user's own terminal has the observer MCP, that IS the next
             step, so it takes the slot instead of hiding in the info dialog; once set up it becomes
             Chat to Claude. With no `claude` on PATH we always show Chat (the prompt suits any MCP
             assistant). See utils/observerSetup.ts terminalCta. -->
        <button v-if="terminalCtaMode !== 'chat'" class="ll-capture" :disabled="observer.registering"
                @click="observer.registerMcp()"
                v-tooltip.top="terminalSetupTooltip(observer.terminalState)">
          <i class="pi pi-download" />
          {{ observer.registering ? 'Setting up…'
             : terminalCtaMode === 'resync' ? 'Fix terminal setup' : 'Set up my terminal' }}
        </button>
        <!-- Chat to Claude: hand off to a FULL external session (any MCP assistant), not the in-app
             one-shot. Copies ONE line naming this project — the rules come from the MCP server, so
             asking it to check your project works without this too. No `claude` install needed. -->
        <button v-else class="ll-capture" :class="{ copied: chatCopied() }" :disabled="!projectUid" @click="chatToClaude"
                v-tooltip.top="chatCopied() ? 'Copied — paste it into Claude (or any MCP chat bot)'
                  : 'Copy a one-line opener naming this project'">
          <i :class="['pi', chatCopied() ? 'pi-check' : 'pi-comments']" /> {{ chatCopied() ? 'Copied' : 'Chat to Claude' }}
        </button>
        <span v-if="observerTokens" class="ll-tokens cc-muted cc-fs-xs"
              v-tooltip.top="'Assistant token use for this observer session (real usage)'">{{ observerTokens }}</span>
        <button v-if="observerTokens" class="ll-clearctx" @click="clearContext"
                v-tooltip.top="'Clear the assistant session and reset the token count'">clear</button>
      </div>

      <span v-if="captureNote" class="ll-note cc-muted cc-fs-xs">{{ captureNote }}</span>
    </div>

    <!-- Set-up guidance: the integration needs NO config — just Claude Code installed + logged in.
         Shown when the CLI is missing, or when a run failed because it isn't authenticated. Conditional,
         so it costs nothing once it works; the durable status lives in Settings → MCP connections. -->
    <div v-if="observerSetup" class="ll-setup">
      <template v-if="observerSetup === 'missing'">
        <strong>Claude Code not detected.</strong> Install it, then run <code>claude</code> once to log in.
      </template>
      <template v-else>
        <strong>Claude Code isn't logged in.</strong> Run <code>claude</code> in a terminal, then try again.
      </template>
      <a href="https://docs.anthropic.com/en/docs/claude-code/setup" target="_blank" rel="noopener">Setup guide ↗</a>
    </div>

    <!-- Terminal set-up FAILED: one line, so the click isn't silent. The DIAGNOSTIC (the resolved
         command to run by hand, MCP connection states) lives in Settings → MCP connections — this
         panel keeps the action, not the troubleshooting. Warn-toned so it doesn't merge into the
         setup/activity bands, which share one background. -->
    <div v-if="observer.registerError" class="ll-setup ll-setup-fail cc-row cc-row-tight">
      <strong><i class="pi pi-exclamation-triangle" /> {{ observer.registerError }}</strong>
      <span class="cc-muted">See Settings → MCP connections.</span>
    </div>

    <!-- LabArchives CONTEXT — the experiment as the lab notebook records it. Pinned above the dated
         entries because it is the frame you want before reading any of them (the person analysing the
         images often didn't run the experiment). Deliberately NOT styled as an entry: it's a mirror of
         an external record, replaced on each sync, while everything below is append-only. Collapsed by
         default — the gap count rides in the label, so it stays quiet until something is missing. -->
    <CollapsibleSection v-if="hasLabArchives(labarchives)" class="ll-la"
                        :label="labArchivesLabel(labarchives)" :default-open="false"
                        storage-key="cc.labLogLaCardOpen" max-height="16rem">
      <div v-if="labarchives.readable === false" class="cc-muted cc-fs-xs">
        The LabArchives sidecar couldn't be read.
      </div>
      <template v-else>
        <!-- gaps first: arms the notebook declares that no image covers. An absence, not an error -->
        <div v-if="labarchives.gaps?.length" class="ll-la-gaps">
          <div v-for="(g, i) in labarchives.gaps" :key="i" class="cc-fs-xs">
            <i class="pi pi-exclamation-triangle" /> {{ labArchivesGapText(g) }}
          </div>
        </div>
        <div v-for="(s, i) in labarchives.sections ?? []" :key="i" class="ll-la-sec">
          <div class="ll-la-head cc-eyebrow">{{ s.heading }}</div>
          <ul>
            <li v-for="(l, j) in s.lines ?? []" :key="j" class="cc-fs-xs">{{ l }}</li>
          </ul>
        </div>
        <div class="ll-la-foot cc-muted cc-fs-2xs">
          <span v-if="labArchivesSyncedOn(labarchives.syncedAt)">
            synced {{ labArchivesSyncedOn(labarchives.syncedAt) }}
          </span>
          <a v-if="labarchives.url" :href="labarchives.url" target="_blank" rel="noopener">open notebook ↗</a>
        </div>
      </template>
    </CollapsibleSection>

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
      <div v-if="loading" class="ll-empty cc-muted">Loading…</div>
      <div v-else-if="!projectUid" class="ll-empty cc-muted">No project open.</div>
      <div v-else-if="!visibleEntries.length" class="ll-empty cc-muted">
        No entries yet. The first note you save appears here.
      </div>
      <template v-else>
        <div v-for="(e, i) in visibleEntries" :key="e.raw + i" class="ll-entry" :class="'k-' + authorKind(e.author)">
          <div class="ll-entry-head">
            <span class="ll-author">{{ e.author }}</span>
            <span class="ll-date cc-muted cc-fs-xs">{{ e.date }}</span>
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
.ll { display: flex; flex-direction: column; height: 100%; font-size: var(--cc-fs-md); }

.ll-compose { padding: 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; }
.ll-compose.correcting { background: rgba(210, 153, 34, 0.08); }
.ll-correcting {
  display: flex; align-items: center; gap: 0.35rem;
  font-size: var(--cc-fs-xs); text-transform: uppercase; letter-spacing: 0.04em;
  color: #d29922; margin-bottom: 0.3rem;
}
.ll-input {
  width: 100%; resize: vertical; box-sizing: border-box; border-radius: var(--cc-radius-sm);
  padding: 0.4rem 0.5rem; line-height: 1.35;
}
.ll-input:focus { border-color: var(--cc-accent); }
.ll-input:disabled { opacity: 0.6; }
.ll-compose-row { display: flex; align-items: center; justify-content: space-between; margin-top: 0.35rem; }

.ll-save {
  border: 1px solid var(--cc-accent); background: var(--cc-accent); color: #fff;
  border-radius: var(--cc-radius-sm); padding: 0.22rem 0.6rem; font-size: var(--cc-fs-sm); cursor: pointer;
}
.ll-save:disabled { opacity: 0.5; cursor: default; }

.ll-toolbar { padding: 0.35rem 0.5rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; }
/* two control slots (Cecelia | Claude), each an inline row; the divider sits between them and the
   whole bar wraps as a unit when the panel is narrow (a group drops to the next line intact). */
.ll-tb-group { display: inline-flex; align-items: center; gap: 0.5rem; }
.ll-tb-sep { align-self: stretch; width: 1px; min-height: 1.1rem; background: var(--cc-border); }
.ll-capture {
  display: inline-flex; align-items: center; gap: 0.3rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text);
  border-radius: var(--cc-radius-sm); padding: 0.2rem 0.5rem; font-size: var(--cc-fs-xs); cursor: pointer;
}
.ll-capture:hover:not(:disabled) { border-color: #8b949e; }
/* brief "copied" flash on the Chat-to-Claude button (replaces the toast) */
.ll-capture.copied { color: var(--cc-sev-ok); border-color: var(--cc-sev-ok); background: rgba(12, 163, 12, 0.1); }
.ll-capture:disabled { opacity: 0.5; cursor: default; }
/* .ll-help → cc-btn cc-btn-bare cc-btn-icon cc-btn-lg */
.ll-help:hover { color: var(--cc-accent); }
.ll-auto { display: inline-flex; align-items: center; gap: 0.25rem; font-size: var(--cc-fs-xs); color: var(--cc-text-dim); cursor: pointer; }
.ll-model {
  font-size: var(--cc-fs-xs); color: var(--cc-text-dim); cursor: pointer;
  /* Longhand padding, NOT the shorthand: the global `select` rule sets padding-right: 1.6rem
     to reserve room for the caret, and a `padding:` shorthand here would clobber it — leaving
     the chevron painted on top of the model name. Same reasoning as background-color below. */
  padding-top: 0.05rem; padding-bottom: 0.05rem; padding-left: 0.2rem;
  /* background-COLOR, not the shorthand: the global `select` rule paints the custom caret via
     background-image, and a shorthand here would reset it to none (leaving an arrowless select). */
  background-color: var(--cc-surface-2); border-radius: var(--cc-radius-xs);
}
/* capture status: floats to the far right of the whole bar (direct toolbar child) */
.ll-note { margin-left: auto; }
/* token readout sits inline within the Claude group (no auto-margin — it's not a toolbar child) */

.ll-clearctx {
  border: none; background: transparent; color: var(--cc-text-dim);
  font-size: var(--cc-fs-xs); cursor: pointer; text-decoration: underline; padding: 0;
}
.ll-clearctx:hover { color: var(--cc-text); }
/* setup hint — install/login guidance when Claude Code is missing or not authenticated */
.ll-setup {
  flex-shrink: 0; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-2); padding: 0.4rem 0.6rem;
  font-size: var(--cc-fs-xs); color: var(--cc-text-dim); line-height: 1.5;
}
/* the FAILED-setup variant: warn accent + a left rule so it can't be read as one slab with the
   activity band below it (both otherwise sit on --cc-surface-2 with a bottom border) */
.ll-setup strong { color: var(--cc-text); }
.ll-setup code {
  font-size: var(--cc-fs-2xs); padding: 0 0.2rem; border-radius: var(--cc-radius-xs);
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
}
.ll-setup a { margin-left: 0.3rem; color: var(--cc-accent); white-space: nowrap; }
.ll-setup-fail { background: var(--cc-surface-1); border-left: 3px solid var(--cc-sev-warn); }
.ll-setup-fail strong { color: var(--cc-sev-warn); display: inline-flex; align-items: center; gap: 0.3rem; }
/* Claude activity log — collapsible; each Ask-Claude pass with its verdict, cost + outcome */
.ll-activity {
  flex-shrink: 0; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-2); padding: 0.3rem 0.6rem; max-height: 11rem; overflow-y: auto;
}
.ll-activity > summary {
  font-size: var(--cc-fs-xs); color: var(--cc-text-dim); cursor: pointer; user-select: none;
}
.ll-pass { margin-top: 0.35rem; padding-left: 0.4rem; border-left: 2px solid var(--cc-border); }
.ll-pass.appended { border-left-color: var(--cc-accent); }
.ll-pass.failed   { border-left-color: #f85149; }
.ll-pass-head { display: flex; align-items: baseline; gap: 0.35rem; font-size: var(--cc-fs-2xs); }
.ll-pass-trig { display: inline-flex; align-items: center; gap: 0.2rem; font-weight: 600; color: var(--cc-accent); }
.ll-pass-trig .pi { font-size: var(--cc-fs-2xs); }
.ll-pass-meta { color: var(--cc-text-dim); }
.ll-pass-at   { margin-left: auto; color: var(--cc-text-dim); opacity: 0.8; }
.ll-pass-note { font-size: var(--cc-fs-xs); color: var(--cc-text); line-height: 1.4; white-space: pre-wrap; margin-top: 0.1rem; }

/* LabArchives context card — a MIRROR of an external record, so it must not read as one of the
   append-only entries below. Different chrome on purpose: accent left rule, no author colour, no
   per-entry actions. Collapsed by default; the gap count sits in the label. */
.ll-la { flex-shrink: 0; border-top: none; border-bottom: 1px solid var(--cc-border); }
.ll-la :deep(.cs-body) { padding: 0.35rem 0.6rem 0.5rem; background: var(--cc-surface-1); }
.ll-la-gaps { border-left: 3px solid var(--cc-sev-warn); padding: 0.2rem 0 0.2rem 0.4rem;
  margin-bottom: 0.45rem; color: var(--cc-sev-warn); }
.ll-la-sec { margin-bottom: 0.4rem; }
.ll-la-head { margin-bottom: 0.1rem; }   /* colour comes from .cc-eyebrow — don't shadow the utility */
.ll-la-sec ul { margin: 0; padding-left: 1rem; color: var(--cc-text); line-height: 1.45; }
.ll-la-foot { display: flex; gap: 0.5rem; align-items: center; }
.ll-la-foot a { color: var(--cc-accent); }

.ll-error { padding: 0.4rem 0.6rem; color: #f85149; font-size: var(--cc-fs-sm); }

.ll-list { flex: 1; overflow-y: auto; padding: 0.4rem 0.5rem 0.6rem; }
.ll-empty { padding: 1.2rem 0.5rem; }

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
/* sourced from the ELN, not reasoned from the data — its own colour so it doesn't read as a [Claude] note */
.ll-entry.k-labarchives { border-left-color: #d29922; }
.ll-entry.k-other { border-left-color: var(--cc-text-dim); }
.k-claude .ll-author { color: var(--cc-accent); }
.k-user .ll-author { color: #3fb950; }
.k-correction .ll-author { color: #d29922; }
.k-cecelia .ll-author { color: #8b949e; }
.k-labarchives .ll-author { color: #d29922; }

.ll-entry-head { display: flex; align-items: baseline; gap: 0.5rem; margin-bottom: 0.2rem; }
.ll-author { font-weight: 700; font-size: var(--cc-fs-sm); }

/* per-entry actions: hidden until hover (thumbs prefill a note — they carry no persisted state) */
.ll-actions { margin-left: auto; display: inline-flex; align-items: center; gap: 0.15rem; visibility: hidden; }
.ll-entry:hover .ll-actions { visibility: visible; }
.ll-thumb {
  border: none; background: none; cursor: pointer; font-size: var(--cc-fs-md); line-height: 1;
  padding: 0 0.1rem; opacity: 0.8; filter: grayscale(0.5);
}
.ll-thumb:hover { opacity: 1; filter: none; }
.ll-lines { margin: 0; padding-left: 1rem; }
.ll-lines li { margin: 0.05rem 0; line-height: 1.35; color: var(--cc-text); }

.ll-link {
  border: none; background: none; color: var(--cc-text-dim);
  cursor: pointer; font-size: var(--cc-fs-xs); padding: 0; text-decoration: underline;
}
.ll-link:hover { color: var(--cc-text); }
</style>
