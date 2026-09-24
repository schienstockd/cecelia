<script setup lang="ts">
// Kiwi — the assist cockpit. A floating panel that gathers AI-assistant-adjacent controls
// so the user doesn't scavenger-hunt across ViewerPanel (push chip), LabLogPanel (chat button)
// and Settings → MCP (connection health) to find them.
//
// Rows, in visible order:
//   - Profile picker — active Kiwi profile (LOGIN_CREDENTIAL_ISOLATION_PLAN P3+P6 frontend); the
//     `default` profile maps to ~/.claude, named profiles live under <config_dir>/kiwi-profiles/.
//     `+` opens the create dialog; the terminal icon copies a one-liner that launches `claude`
//     under this profile (so a raw `claude` in it sees this profile's credentials, not ambient
//     env). The pairing path (followup-profile-session-claude-code-pairing-stays) needs this — a
//     raw shell without the one-liner reads ambient credentials on a shared OS login.
//   - Pairing chip (mirrors what ViewerPanel used to show; ViewerPanel's copy is removed too).
//   - Copy chat starter (moved from LabLogPanel).
//   - Share — viewer / canvas buttons.
//   - Ask — the structured duck (KiwiAsk.vue, docs/todo/KIWI_ASSISTANT_PLAN.md Phase 4).
//     Wrapped in a CollapsibleSection so a long conversation thread can be folded away when the
//     other rows need the panel height; default open, since it's the reason to open the panel.
//   - Recent captures — click a row to copy its captureId.
//   - Session identity — sessionLabel + pairedAt + pairedFromPid + "Clear pairing" button.
//   - Assistant — CLI availability + terminal setup state.
// The always-glance status rows (Profile, Pairing, Chat, Share) sit above the collapsibles so
// they never scroll off, no matter how long the Ask thread grows.
//
// The "Clear pairing" button deletes push_target.json rather than "re-pair now" — the plan's
// `register_push_target` MCP tool reads env vars that live only inside the paired MCP process,
// so a browser button cannot force that write. Clearing the record lets the next auto-pair
// rewrite it, which is the honest equivalent for a user-driven reset.
//
// v2 adds MCP health, setup CTA merge, lab-log peek, Blackboard list — see
// docs/todo/KIWI_PLAN.md → *Contents — v2*. The provider-neutral wording is enforced by
// kiwiNamingRatchet.test.ts, which greps this directory for the literal "Claude".
//
// Observer state descope note: the plan mentions surfacedCount/surfaceCap/throttled/enabled, but
// those live only inside the MCP server's Python `_monitor` singleton — there's no HTTP path to
// them today. This row shows what /api/observer/status returns (availability + terminal setup);
// stats surface is a later follow-up once a bridge exists.
import { ref, computed, watch, onUnmounted, onMounted } from 'vue'
import FloatingPanel from '../FloatingPanel.vue'
import CollapsibleSection from '../CollapsibleSection.vue'
import ConfirmButton from '../ConfirmButton.vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { useObserverStore } from '../../stores/observer'
import { fetchPushTarget, pushChipLabel, clearPushTarget, probePushTarget,
         type PairedState } from '../../utils/pushTarget'
import { usePushStore } from '../../stores/push'
import { buildChatPrompt } from '../../lib/chatHandoff'
import { fetchRecentCaptures, formatAddress, formatWhen, fetchCaptureEnvelope, type CaptureEnvelope,
         deleteCapture, clearAllCaptures,
         type CaptureRow } from '../../utils/kiwiCaptures'
import { useShareTargetStore, type BeginShareResult } from '../../stores/shareTarget'
import { useCaptureFocus } from '../../composables/useCaptureFocus'
import InlineNote from '../InlineNote.vue'
import KiwiAsk from './KiwiAsk.vue'
import { useKiwiStore } from '../../stores/kiwi'
import { resolveAnchor } from '../../utils/guideAnchor'
import { rectsOverlap } from '../../utils/panelBounds'
import AddToKiwiButton from './AddToKiwiButton.vue'
// Kiwi is the canonical cockpit for the paired assistant, so the "what can it do here?" how-to
// dialog opens from here (was in the lab log toolbar until 2026-09-22). The dialog itself lives
// outside `kiwi/` and is free to name its provider; the local alias below keeps the ratchet happy.
import AssistantOverviewDialog from '../ClaudeOverviewDialog.vue'
import KiwiCreateProfileDialog from './KiwiCreateProfileDialog.vue'
import { fetchKiwiProfiles, selectKiwiProfile, fetchKiwiTerminalCommand,
         type KiwiProfileRoster } from '../../utils/kiwiProfileApi'

defineEmits<{ (e: 'close'): void }>()

// "What can the assistant do here?" — provider-neutral wording; the dialog itself lives outside
// this directory and is free to name what it explains.
const showAssistantOverview = ref(false)

// ── Profile picker (LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6 frontend) ─────────
// Roster + active profile are server state (custom.toml [ai].profile). Local `roster` is a cache
// so the `<select>` renders while the round-trip runs. Failure ⇒ `default`-only fallback (see
// `fetchKiwiProfiles`), which keeps the picker usable rather than blanking it out.
const roster = ref<KiwiProfileRoster>({
  active: 'default',
  profiles: [{ name: 'default', dir: '', isDefault: true }],
  legacyReserved: ['legacy'],
})
// The `<select>`'s v-model. Kept as a separate ref so a failed select can snap back to
// `roster.active` without triggering another change event.
const activeProfile = ref('default')
const profileSwitching = ref(false)
const profileError = ref<string | null>(null)
const showCreateProfile = ref(false)

async function refreshProfiles() {
  const r = await fetchKiwiProfiles()
  roster.value = r
  activeProfile.value = r.active
}

async function onProfileChange(name: string) {
  if (name === roster.value.active) return
  profileSwitching.value = true
  profileError.value = null
  try {
    const r = await selectKiwiProfile(name)
    if (!r.ok) {
      profileError.value = r.error ?? 'Select failed'
      activeProfile.value = roster.value.active   // snap back
      return
    }
    roster.value = { ...roster.value, active: r.active ?? name }
  } finally { profileSwitching.value = false }
}

function onProfileCreated(newName: string) {
  // The dialog already POSTed /select for us — just refresh the local roster.
  void refreshProfiles().then(() => { activeProfile.value = newName })
}

// The "Open profile terminal" button: fetch the one-liner for the active profile and copy it.
// A brand-new profile without `claude login` yet still gets a valid one-liner — running it opens
// the interactive shell where the user then logs in.
const { isCopied: termCopied, copy: copyTerm } = useCopyFlash()
const termFetching = ref(false)
async function copyTerminalCommand() {
  if (termFetching.value) return
  termFetching.value = true
  try {
    const r = await fetchKiwiTerminalCommand()
    if (r?.command) await copyTerm(r.command)
  } finally { termFetching.value = false }
}

onMounted(() => { void refreshProfiles() })

const pm = useProjectMetaStore()

// Step aside: Kiwi floats over the page, so a plot it points at is often UNDER it — the pointer bubble
// showed on top of Kiwi, over nothing visible. When the target overlaps the panel, roll up to the header
// (the chevron brings it back). Only then: a target elsewhere on screen leaves Kiwi as it was.
const kiwiStore = useKiwiStore()
const panel = ref<InstanceType<typeof FloatingPanel> | null>(null)
watch(() => kiwiStore.pointed, p => {
  const el = p ? resolveAnchor(p.anchor) : null
  const mine = panel.value?.rect()
  if (el && mine && rectsOverlap(el.getBoundingClientRect(), mine)) panel.value?.collapse()
})
const projectUid = computed(() => pm.current?.uid ?? '')
const projectName = computed(() => pm.current?.name ?? undefined)

// ── Pairing chip ──────────────────────────────────────────────────────────────
const pushTarget = ref<PairedState>({ paired: false })
const pushSentFlash = ref(false)
let pushFlashTimer: ReturnType<typeof setTimeout> | null = null
const probing = ref(false)

async function refreshPushTarget() {
  pushTarget.value = projectUid.value ? await fetchPushTarget(projectUid.value) : { paired: false }
}

// Liveness probe. Auto-run on project change (below); the "Check" icon-button beside the chip
// re-runs it on demand. Server clears + broadcasts push_target:changed on a dead socket, so no
// local chip-flip logic is needed — the WS watcher already refreshes on that broadcast.
async function checkPairing() {
  if (!projectUid.value || probing.value) return
  probing.value = true
  try { await probePushTarget(projectUid.value) }
  finally { probing.value = false }
}

const pushStore = usePushStore()
watch(() => pushStore.tick, () => {
  const evt = pushStore.lastEvent
  if (!evt) return
  const evtUid = (evt as { projectUid?: string }).projectUid
  if (evtUid && evtUid !== projectUid.value) return
  if (evt.kind === 'push_target:changed') {
    void refreshPushTarget()
  } else if (evt.kind === 'push:sent') {
    pushSentFlash.value = true
    if (pushFlashTimer) clearTimeout(pushFlashTimer)
    pushFlashTimer = setTimeout(() => { pushSentFlash.value = false; pushFlashTimer = null }, 3000)
    // A successful push means a fresh capture just landed on disk — reload the list without waiting
    // for the user to reopen the panel.
    void refreshCaptures()
  } else if (evt.kind === 'captures:changed') {
    void refreshCaptures()
  }
})

onUnmounted(() => { if (pushFlashTimer) clearTimeout(pushFlashTimer) })
// observer.refresh() is already installed app-wide in App.vue on project change — don't double it.
// The probe runs after refreshPushTarget so the chip renders once with the stored state, then
// silently updates if the socket is dead. Order: read → probe → maybe self-heal via WS broadcast.
watch(projectUid, async () => {
  await refreshPushTarget()
  void refreshCaptures()
  void checkPairing()
}, { immediate: true })

// ── Share with Claude ─────────────────────────────────────────────────────────
// Two targets, one store — see `stores/shareTarget.ts`. Kiwi owns the buttons; the store owns
// availability + the actual `beginShare` handshake so ViewerPanel's transitional copy and Kiwi's
// button run the same code. Poll on mount so the viewer button's disabled state stays honest as
// the pop-out opens / closes over the session.
const shareStore = useShareTargetStore()
onMounted(() => shareStore.startViewerPoll())
onUnmounted(() => shareStore.stopViewerPoll())
const shareNote = ref<Exclude<BeginShareResult, null> | null>(null)
function clickShareViewer() { shareNote.value = shareStore.beginViewerShare() }
function clickShareCanvas() { shareNote.value = shareStore.beginCanvasShare() }

// ── Chat handoff ──────────────────────────────────────────────────────────────
const { isCopied: chatCopied, copy: copyChatPrompt } = useCopyFlash(2500)
async function copyChatStarter() {
  if (!projectUid.value) return
  await copyChatPrompt(buildChatPrompt(projectUid.value, projectName.value))
}

// ── Recent captures ───────────────────────────────────────────────────────────
const captures = ref<CaptureRow[]>([])
const capturesLoading = ref(false)
async function refreshCaptures() {
  if (!projectUid.value) { captures.value = []; envelopes.value = {}; return }
  capturesLoading.value = true
  try {
    captures.value = await fetchRecentCaptures(projectUid.value, 10)
    // Kick envelope fetches for any row not already cached. Fire in parallel — the list caps at 10,
    // and dropping an envelope fetch is cheaper than blocking the row on it.
    for (const row of captures.value) {
      if (envelopes.value[row.captureId]) continue
      void loadThumb(row.captureId)
    }
  }
  finally { capturesLoading.value = false }
}

// ── Row thumbnails + envelope cache (PR B trinity + BIDIR Part 4 follow-up) ─
// One fetch per row, cached: the full envelope carries the thumbnail's frame PNG, the address the
// Refocus button reads, AND the drawn marks so Refocus can restore the annotations onto the live
// viewer. Failure leaves the slot unset — the row falls back to a placeholder icon and refocus is
// still safe (missing envelope ⇒ no publish). Never retry inside one Kiwi session; a refetch would
// re-hammer a stale backend.
const envelopes = ref<Record<string, CaptureEnvelope>>({})
async function loadThumb(id: string) {
  if (!projectUid.value) return
  const env = await fetchCaptureEnvelope(projectUid.value, id)
  if (env) envelopes.value = { ...envelopes.value, [id]: env }
}

// ── Refocus (PR B trinity) — the shared path in composables/useCaptureFocus.ts ────────────────
const { focusCapture } = useCaptureFocus()
function refocusRow(row: CaptureRow) {
  const env = envelopes.value[row.captureId]
  if (env) focusCapture(projectUid.value, env)
}
// Per-row copy flash. Separate `useCopyFlash` instance so the chat-starter flash is independent.
const { isCopied: capCopied, copy: copyCaptureId } = useCopyFlash(2000)
async function copyId(id: string) { await copyCaptureId(id, id) }
async function deleteRow(id: string) {
  // Optimistic: drop from the local list immediately so the row vanishes with the click;
  // server confirmation refreshes the list anyway via `captures:changed`.
  captures.value = captures.value.filter(c => c.captureId !== id)
  await deleteCapture(projectUid.value, id)
}
async function clearAll() {
  captures.value = []
  await clearAllCaptures(projectUid.value)
}

// Re-render relative timestamps every ~30 s so "just now" / "5m" don't grow stale mid-session.
// Cheap — only relabels; captures.value is not refetched. Timer declared in the DECLARED_TIMERS
// inventory (utils/continuousControls.test.ts).
const nowTick = ref(Date.now())
let nowTimer: ReturnType<typeof setInterval> | null = null
onMounted(() => { nowTimer = setInterval(() => { nowTick.value = Date.now() }, 30_000) })
onUnmounted(() => { if (nowTimer) { clearInterval(nowTimer); nowTimer = null } })
function whenLabel(iso: string): string { return formatWhen(iso, new Date(nowTick.value)) }

// ── Session identity (paired-session details + clear button) ─────────────────
const clearing = ref(false)
async function clearPairing() {
  if (!projectUid.value || clearing.value) return
  clearing.value = true
  try {
    await clearPushTarget(projectUid.value)
    // The backend broadcasts push_target:changed on clear, which the WS watcher already
    // handles by re-fetching. Belt-and-braces refresh in case the socket dropped mid-op.
    await refreshPushTarget()
  } finally { clearing.value = false }
}

// ── Observer state (availability + terminal setup) ───────────────────────────
const observer = useObserverStore()
const terminalStateLabel = computed(() => {
  const s = observer.terminalState
  if (s === 'current')   return 'Registered'
  if (s === 'stale')     return 'Stale (re-register)'
  if (s === 'shadowed')  return 'Shadowed (local scope wins)'
  if (s === 'missing' || !s) return 'Not registered'
  return s
})
const terminalStateKind = computed<'ok' | 'warn' | 'fail'>(() => {
  const s = observer.terminalState
  if (s === 'current') return 'ok'
  if (s === 'stale' || s === 'shadowed') return 'warn'
  return 'fail'
})
</script>

<template>
  <FloatingPanel ref="panel" title="Kiwi" icon="pi-comments" storage-key="kiwi"
                 accent="var(--cc-kiwi)"
                 :default-x="260" :default-y="100" :default-w="380" :default-h="640"
                 @close="$emit('close')">
    <!-- Header `?` — the "what does Kiwi do here?" how-to. Sits in the header's action slot so it's
         always reachable, not gated behind opening a project. -->
    <template #header-actions>
      <button class="fp-btn cc-btn cc-btn-bare cc-btn-icon kiwi-help"
              data-guide="kiwi.assistantHelp"
              @click="showAssistantOverview = true"
              v-tooltip.bottom="'What Kiwi does here — Ask vs Chat, captures, chains, blackboard, limits'">
        <i class="pi pi-question-circle" />
      </button>
    </template>
    <div class="kiwi-body">
      <!-- Profile picker — identity is machine-wide, so show it even without a project open. -->
      <div class="kiwi-row" data-guide="kiwi.profile">
        <span class="kiwi-lbl cc-eyebrow cc-fs-2xs">Profile</span>
        <select class="kiwi-profile-select cc-input-xs"
                :value="activeProfile"
                :disabled="profileSwitching"
                @change="onProfileChange(($event.target as HTMLSelectElement).value)"
                v-tooltip.bottom="'Which credential + MCP scope your assistant spawns run under (custom.toml [ai].profile)'">
          <option v-for="p in roster.profiles" :key="p.name" :value="p.name">
            {{ p.name }}{{ p.isDefault ? ' (~/.claude)' : '' }}
          </option>
        </select>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                @click="showCreateProfile = true"
                v-tooltip.bottom="'New profile — a separate credential + MCP scope for this seat login'">
          <i class="pi pi-plus" />
        </button>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                :disabled="termFetching"
                @click="copyTerminalCommand"
                v-tooltip.bottom="termCopied()
                  ? 'Copied — paste into a terminal, then `claude login` inside it'
                  : 'Copy a terminal one-liner scoped to the active profile'">
          <i :class="['pi', termFetching ? 'pi-spin pi-spinner'
                            : termCopied() ? 'pi-check' : 'pi-desktop']" />
        </button>
      </div>
      <p v-if="profileError" class="kiwi-profile-err cc-fs-2xs">
        <i class="pi pi-times-circle" /> {{ profileError }}
      </p>

      <div v-if="!projectUid" class="kiwi-empty cc-muted cc-fs-sm">
        Open a project to pair with your assistant.
      </div>
      <template v-else>
        <!-- Pairing/Chat/Share come FIRST so the always-glance identity + capture affordances
             stay above the fold, no matter how long the Ask thread grows. Ask is a growing
             conversation → below, wrapped in a collapsible so it can be folded away when the
             other rows need the full panel height. -->
        <div class="kiwi-row" data-guide="kiwi.pairing">
          <span class="kiwi-lbl cc-eyebrow cc-fs-2xs">Pairing</span>
          <span class="kiwi-chip cc-fs-xs"
                :class="{ 'kiwi-chip-paired': pushTarget.paired, 'kiwi-chip-sent': pushSentFlash }"
                v-tooltip.bottom="pushSentFlash
                  ? 'Sent to your paired assistant session'
                  : pushTarget.paired
                    ? `Paired — shared frames post to your assistant session\nSocket: ${(pushTarget as Extract<PairedState, { paired: true }>).socketPath}`
                    : 'No paired assistant session — shared frames fall back to the clipboard'">
            {{ pushSentFlash ? 'sent ✓' : pushChipLabel(pushTarget) }}
          </span>
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
                  :disabled="probing || !projectUid" @click="checkPairing"
                  v-tooltip.bottom="'Check pairing — probe the paired socket for liveness'">
            <i :class="['pi', probing ? 'pi-spin pi-spinner' : 'pi-refresh']" />
          </button>
        </div>
        <div class="kiwi-row" data-guide="kiwi.chat">
          <span class="kiwi-lbl cc-eyebrow cc-fs-2xs">Chat</span>
          <button class="cc-btn cc-btn-ghost cc-fs-xs"
                  @click="copyChatStarter"
                  v-tooltip.bottom="chatCopied()
                    ? 'Copied — paste it into your assistant chat'
                    : 'Copy a one-line opener naming this project for your assistant'">
            <i :class="['pi', chatCopied() ? 'pi-check' : 'pi-comments']" />
            {{ chatCopied() ? 'Copied' : 'Copy chat starter' }}
          </button>
        </div>

        <!-- Share — two targets, one row. Viewer = the pop-out's WebGPU frame (existing flow);
             Canvas = whatever module page has registered a shareable plot canvas. Both land in
             the same capture envelope; buttons stay separately enabled so a stateful toggle
             doesn't strand the user in an unclickable mode. -->
        <div class="kiwi-row" data-guide="kiwi.share">
          <span class="kiwi-lbl cc-eyebrow cc-fs-2xs">Share</span>
          <button class="cc-btn cc-btn-ghost cc-btn-icon"
                  :disabled="!shareStore.viewerAvailable"
                  @click="clickShareViewer"
                  v-tooltip.bottom="shareStore.viewerAvailable
                    ? 'Draw on the pop-out viewer and share it'
                    : 'Open the pop-out viewer first'">
            <i class="pi pi-eye" />
          </button>
          <button class="cc-btn cc-btn-ghost cc-btn-icon"
                  :disabled="!shareStore.canvasHost"
                  @click="clickShareCanvas"
                  v-tooltip.bottom="shareStore.canvasHost
                    ? `Select plots to share — ${shareStore.canvasHost.label}`
                    : 'The current page has no shareable plot canvas yet'">
            <i class="pi pi-th-large" />
          </button>
        </div>
        <InlineNote v-if="shareNote" :severity="shareNote.severity"
                    :short="shareNote.short" :detail="shareNote.detail" />

        <CollapsibleSection label="Ask" storage-key="kiwi.ask.open"
                            tip="Ask a structured question — every claim in the reply carries a pointer you can click.">
          <KiwiAsk />
        </CollapsibleSection>

        <CollapsibleSection label="Recent captures" storage-key="kiwi.captures.open"
                            tip="Click a row to copy its captureId; the × button deletes it from disk.">
          <div v-if="capturesLoading" class="kiwi-empty cc-muted cc-fs-xs">Loading…</div>
          <div v-else-if="captures.length === 0" class="kiwi-empty cc-muted cc-fs-xs">
            No shared captures yet.
          </div>
          <template v-else>
            <ul class="kiwi-cap-list">
              <li v-for="c in captures" :key="c.captureId" class="kiwi-cap-row"
                  :class="{ copied: capCopied(c.captureId) }">
                <button class="kiwi-cap-copy cc-btn cc-btn-bare"
                        @click="copyId(c.captureId)"
                        v-tooltip.right="capCopied(c.captureId)
                          ? 'Copied — paste it into your assistant chat'
                          : (c.previousCaptureId
                              ? `Copy captureId · ${c.captureId} (refines an earlier capture)`
                              : `Copy captureId · ${c.captureId}`)">
                  <span class="kiwi-cap-thumb" aria-hidden="true">
                    <img v-if="envelopes[c.captureId]?.frame" :src="envelopes[c.captureId].frame" alt="" />
                    <i v-else class="pi pi-image" />
                  </span>
                  <span class="kiwi-cap-meta">
                    <span class="kiwi-cap-time cc-fs-2xs">
                      <i v-if="c.previousCaptureId" class="pi pi-reply kiwi-cap-refine"
                         aria-hidden="true" />
                      {{ whenLabel(c.createdAt) || '—' }}
                    </span>
                    <span class="kiwi-cap-addr cc-muted cc-fs-2xs">{{ formatAddress(c) }}</span>
                  </span>
                  <i class="pi kiwi-cap-icon"
                     :class="capCopied(c.captureId) ? 'pi-check' : 'pi-copy'" />
                </button>
                <AddToKiwiButton :kiwi-ref="{ kind: 'capture', captureId: c.captureId }" />
                <button v-if="(c.address && c.address.imageUid) || c.surface === 'plot'"
                        class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro kiwi-cap-focus"
                        @click="refocusRow(c)"
                        v-tooltip.right="c.surface === 'plot'
                          ? 'Reshow this capture on its module page'
                          : 'Refocus the pop-out viewer to this capture’s frame'">
                  <i class="pi pi-search" />
                </button>
                <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro kiwi-cap-del"
                        @click="deleteRow(c.captureId)"
                        v-tooltip.right="'Delete this capture from disk'">
                  <i class="pi pi-times" />
                </button>
              </li>
            </ul>
            <div class="kiwi-cap-footer">
              <ConfirmButton @confirm="clearAll" v-slot="{ armed, arm, confirm, cancel }">
                <button v-if="!armed" class="cc-btn cc-btn-ghost cc-fs-2xs"
                        @click="arm"
                        v-tooltip.bottom="'Delete every capture for this project'">
                  <i class="pi pi-trash" /> Clear all captures
                </button>
                <template v-else>
                  <button class="cc-btn cc-btn-danger cc-fs-2xs" @click="confirm"
                          v-tooltip.bottom="'Confirm — delete every capture'">
                    <i class="pi pi-check" /> Delete all
                  </button>
                  <button class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-2xs" @click="cancel"
                          v-tooltip.bottom="'Cancel'">
                    <i class="pi pi-times" />
                  </button>
                </template>
              </ConfirmButton>
            </div>
          </template>
        </CollapsibleSection>

        <CollapsibleSection label="Session identity" storage-key="kiwi.session.open"
                            tip="Which assistant session is paired here — and a button to unpair.">
          <div v-if="!pushTarget.paired" class="kiwi-empty cc-muted cc-fs-xs">
            Not paired. Your assistant session pairs on its next MCP tool call.
          </div>
          <template v-else>
            <div class="kiwi-obs-row">
              <span class="kiwi-obs-lbl cc-eyebrow cc-fs-2xs">Label</span>
              <span class="cc-fs-xs kiwi-mono">{{ (pushTarget as Extract<PairedState, { paired: true }>).sessionLabel || '—' }}</span>
            </div>
            <div class="kiwi-obs-row">
              <span class="kiwi-obs-lbl cc-eyebrow cc-fs-2xs">Paired</span>
              <span class="cc-fs-xs">{{ (pushTarget as Extract<PairedState, { paired: true }>).pairedAt || '—' }}</span>
            </div>
            <div class="kiwi-obs-row">
              <span class="kiwi-obs-lbl cc-eyebrow cc-fs-2xs">PID</span>
              <span class="cc-fs-xs kiwi-mono"
                    v-tooltip.bottom="`Socket: ${(pushTarget as Extract<PairedState, { paired: true }>).socketPath || '—'}`">
                {{ (pushTarget as Extract<PairedState, { paired: true }>).pairedFromPid || '—' }}
              </span>
            </div>
            <div class="kiwi-obs-row kiwi-clear-row">
              <ConfirmButton @confirm="clearPairing" v-slot="{ armed, arm, confirm, cancel }">
                <button v-if="!armed" class="cc-btn cc-btn-danger-ghost cc-fs-xs"
                        :disabled="clearing" @click="arm"
                        v-tooltip.bottom="'Unpair — your session re-pairs on its next MCP tool call'">
                  <i class="pi pi-times-circle" /> Clear pairing
                </button>
                <template v-else>
                  <button class="cc-btn cc-btn-danger cc-fs-xs"
                          @click="confirm" v-tooltip.bottom="'Confirm — clear the pairing record'">
                    <i class="pi pi-check" /> Confirm
                  </button>
                  <button class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-xs"
                          @click="cancel" v-tooltip.bottom="'Cancel'">
                    <i class="pi pi-times" />
                  </button>
                </template>
              </ConfirmButton>
            </div>
          </template>
        </CollapsibleSection>

        <CollapsibleSection label="Assistant" storage-key="kiwi.observer.open"
                            tip="Is the assistant CLI installed, and is its MCP entry registered in your terminal?">
          <div class="kiwi-obs-row">
            <span class="kiwi-obs-lbl cc-eyebrow cc-fs-2xs">CLI</span>
            <span class="kiwi-dot" :class="{ 'kiwi-dot-ok': observer.available,
                                             'kiwi-dot-fail': !observer.available }"
                  aria-hidden="true" />
            <span class="cc-fs-xs">{{ observer.available ? 'Available' : 'Not installed' }}</span>
          </div>
          <div class="kiwi-obs-row" v-if="observer.available">
            <span class="kiwi-obs-lbl cc-eyebrow cc-fs-2xs">Terminal</span>
            <span class="kiwi-dot" :class="{ 'kiwi-dot-ok':   terminalStateKind === 'ok',
                                             'kiwi-dot-warn': terminalStateKind === 'warn',
                                             'kiwi-dot-fail': terminalStateKind === 'fail' }"
                  aria-hidden="true" />
            <span class="cc-fs-xs">{{ terminalStateLabel }}</span>
          </div>
        </CollapsibleSection>
      </template>
    </div>
    <AssistantOverviewDialog v-if="showAssistantOverview" @close="showAssistantOverview = false" />
    <KiwiCreateProfileDialog v-if="showCreateProfile"
                             @close="showCreateProfile = false"
                             @created="onProfileCreated" />
  </FloatingPanel>
</template>

<style scoped>
.kiwi-body { padding: 0.6rem; display: flex; flex-direction: column; gap: 0.55rem; }
.kiwi-empty { text-align: center; padding: 1rem 0.5rem; }
.kiwi-row { display: flex; align-items: center; gap: 0.5rem; }
.kiwi-lbl { min-width: 4rem; }
.kiwi-chip { padding-inline: 0.35rem; line-height: 1; align-self: center;
             transition: color 0.15s ease; color: var(--cc-text-dim); }
.kiwi-chip-paired { color: var(--cc-kiwi); }
.kiwi-chip-sent   { color: var(--cc-sev-ok, var(--cc-kiwi)); font-weight: 600; }
.kiwi-help { color: var(--cc-text-dim); }
.kiwi-help:hover { color: var(--cc-accent); }

/* Recent captures list — each row = copy-button (grid inside) + delete-button */
.kiwi-cap-list { list-style: none; margin: 0; padding: 0;
                 display: flex; flex-direction: column; gap: 0.15rem; }
.kiwi-cap-row { display: flex; align-items: center; gap: 0.25rem; }
.kiwi-cap-copy {
  flex: 1;
  display: grid; grid-template-columns: 2.4rem 1fr auto;
  gap: 0.4rem; align-items: center;
  padding: 0.25rem 0.4rem;
  border-radius: var(--cc-radius-xs);
  text-align: left;
  transition: background 0.1s ease, color 0.1s ease;
}
.kiwi-cap-copy:hover { background: var(--cc-surface-2); }
.kiwi-cap-row.copied .kiwi-cap-copy { background: var(--cc-kiwi-tint); }
.kiwi-cap-row.copied .kiwi-cap-icon { color: var(--cc-sev-ok); }
/* Thumb slot: fixed 2.4-rem square with a dark chip inside, so an unfetched row shows a neutral
   "loading" placeholder and a fetched row shows the composed frame at the correct aspect. */
.kiwi-cap-thumb {
  width: 2.4rem; height: 2.4rem;
  display: inline-flex; align-items: center; justify-content: center;
  border-radius: var(--cc-radius-xs); background: #000; overflow: hidden;
  color: var(--cc-text-dim);
}
.kiwi-cap-thumb img { width: 100%; height: 100%; object-fit: contain; display: block; }
.kiwi-cap-meta { display: flex; flex-direction: column; min-width: 0; }
.kiwi-cap-time { color: var(--cc-text-dim); }
.kiwi-cap-refine { color: var(--cc-kiwi); font-size: 0.7em; margin-right: 0.2em; }
.kiwi-cap-addr { overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
                 font-family: var(--cc-mono); }
.kiwi-cap-icon { color: var(--cc-text-dim); }
.kiwi-cap-focus { color: var(--cc-text-dim); flex-shrink: 0; }
.kiwi-cap-focus:hover { color: var(--cc-kiwi); }
.kiwi-cap-del { color: var(--cc-text-dim); flex-shrink: 0; }
.kiwi-cap-del:hover { color: var(--cc-sev-fail); }
.kiwi-cap-footer { display: flex; justify-content: flex-end; gap: 0.35rem;
                   padding: 0.4rem 0.4rem 0.2rem; }

/* Assistant / observer + session identity sections */
.kiwi-obs-row { display: flex; align-items: center; gap: 0.5rem; padding: 0.15rem 0; }
.kiwi-obs-lbl { min-width: 4rem; }
.kiwi-mono   { font-family: var(--cc-mono); overflow: hidden;
               text-overflow: ellipsis; white-space: nowrap; }
.kiwi-dot { width: 8px; height: 8px; border-radius: 50%; background: var(--cc-text-dim);
            flex-shrink: 0; }
.kiwi-dot-ok   { background: var(--cc-sev-ok); }
.kiwi-dot-warn { background: var(--cc-sev-warn); }
.kiwi-dot-fail { background: var(--cc-sev-fail); }
.kiwi-clear-row { margin-top: 0.4rem; gap: 0.35rem; }

/* Profile picker row — `<select>` takes the remaining width so long names don't clip. */
.kiwi-profile-select { flex: 1; min-width: 0; }
.kiwi-profile-err    { margin: -0.2rem 0 0 4.5rem; color: var(--cc-sev-fail);
                       display: flex; align-items: center; gap: 0.3rem; }
</style>
