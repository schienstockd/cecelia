<script setup lang="ts">
// Kiwi — the assist cockpit. A floating panel that gathers AI-assistant-adjacent controls
// so the user doesn't scavenger-hunt across ViewerPanel (push chip), LabLogPanel (chat button)
// and Settings → MCP (connection health) to find them.
//
// v1 rows:
//   - Pairing chip (mirrors what ViewerPanel used to show; ViewerPanel's copy is removed too)
//   - Copy chat starter (moved from LabLogPanel)
//   - Recent captures — click a row to copy its captureId
//   - Observer state — CLI availability + terminal setup state
//
// PR #3 will add: explicit re-pair button + session identity block.
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
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { useObserverStore } from '../../stores/observer'
import { fetchPushTarget, pushChipLabel, type PairedState } from '../../utils/pushTarget'
import { usePushStore } from '../../stores/push'
import { buildChatPrompt } from '../../lib/chatHandoff'
import { fetchRecentCaptures, formatAddress, formatWhen,
         type CaptureRow } from '../../utils/kiwiCaptures'

defineEmits<{ (e: 'close'): void }>()

const pm = useProjectMetaStore()
const projectUid = computed(() => pm.current?.uid ?? '')
const projectName = computed(() => pm.current?.name ?? undefined)

// ── Pairing chip ──────────────────────────────────────────────────────────────
const pushTarget = ref<PairedState>({ paired: false })
const pushSentFlash = ref(false)
let pushFlashTimer: ReturnType<typeof setTimeout> | null = null

async function refreshPushTarget() {
  pushTarget.value = projectUid.value ? await fetchPushTarget(projectUid.value) : { paired: false }
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
  }
})

onUnmounted(() => { if (pushFlashTimer) clearTimeout(pushFlashTimer) })
// observer.refresh() is already installed app-wide in App.vue on project change — don't double it.
watch(projectUid, () => { void refreshPushTarget(); void refreshCaptures() }, { immediate: true })

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
  if (!projectUid.value) { captures.value = []; return }
  capturesLoading.value = true
  try { captures.value = await fetchRecentCaptures(projectUid.value, 10) }
  finally { capturesLoading.value = false }
}
// Per-row copy flash. Separate `useCopyFlash` instance so the chat-starter flash is independent.
const { isCopied: capCopied, copy: copyCaptureId } = useCopyFlash(2000)
async function copyId(id: string) { await copyCaptureId(id, id) }

// Re-render relative timestamps every ~30 s so "just now" / "5m" don't grow stale mid-session.
// Cheap — only relabels; captures.value is not refetched. Timer declared in the DECLARED_TIMERS
// inventory (utils/continuousControls.test.ts).
const nowTick = ref(Date.now())
let nowTimer: ReturnType<typeof setInterval> | null = null
onMounted(() => { nowTimer = setInterval(() => { nowTick.value = Date.now() }, 30_000) })
onUnmounted(() => { if (nowTimer) { clearInterval(nowTimer); nowTimer = null } })
function whenLabel(iso: string): string { return formatWhen(iso, new Date(nowTick.value)) }

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
  <FloatingPanel title="Kiwi" icon="pi-comments" storage-key="kiwi"
                 accent="var(--cc-kiwi)"
                 :default-x="260" :default-y="100" :default-w="320" :default-h="440"
                 @close="$emit('close')">
    <div class="kiwi-body">
      <div v-if="!projectUid" class="kiwi-empty cc-muted cc-fs-sm">
        Open a project to pair with your assistant.
      </div>
      <template v-else>
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
        </div>
        <div class="kiwi-row" data-guide="kiwi.chat">
          <span class="kiwi-lbl cc-eyebrow cc-fs-2xs">Chat</span>
          <button class="kiwi-btn cc-btn cc-btn-ghost cc-fs-xs"
                  :class="{ copied: chatCopied() }"
                  @click="copyChatStarter"
                  v-tooltip.bottom="chatCopied()
                    ? 'Copied — paste it into your assistant chat'
                    : 'Copy a one-line opener naming this project for your assistant'">
            <i :class="['pi', chatCopied() ? 'pi-check' : 'pi-comments']" />
            {{ chatCopied() ? 'Copied' : 'Copy chat starter' }}
          </button>
        </div>

        <CollapsibleSection label="Recent captures" storage-key="kiwi.captures.open"
                            tip="Click a row to copy its captureId — paste it into your assistant chat.">
          <div v-if="capturesLoading" class="kiwi-empty cc-muted cc-fs-xs">Loading…</div>
          <div v-else-if="captures.length === 0" class="kiwi-empty cc-muted cc-fs-xs">
            No shared captures yet.
          </div>
          <ul v-else class="kiwi-cap-list">
            <li v-for="c in captures" :key="c.captureId" class="kiwi-cap-row"
                :class="{ copied: capCopied(c.captureId) }"
                @click="copyId(c.captureId)"
                v-tooltip.right="capCopied(c.captureId)
                  ? 'Copied — paste it into your assistant chat'
                  : `Copy captureId · ${c.captureId}`">
              <span class="kiwi-cap-time cc-fs-2xs">{{ whenLabel(c.createdAt) || '—' }}</span>
              <span class="kiwi-cap-addr cc-muted cc-fs-2xs">{{ formatAddress(c) }}</span>
              <i class="pi kiwi-cap-icon"
                 :class="capCopied(c.captureId) ? 'pi-check' : 'pi-copy'" />
            </li>
          </ul>
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
.kiwi-btn { flex: 1; }
.kiwi-btn.copied { color: var(--cc-sev-ok); }

/* Recent captures list */
.kiwi-cap-list { list-style: none; margin: 0; padding: 0;
                 display: flex; flex-direction: column; gap: 0.15rem; }
.kiwi-cap-row {
  display: grid; grid-template-columns: 3rem 1fr auto;
  gap: 0.4rem; align-items: center;
  padding: 0.25rem 0.4rem;
  border-radius: var(--cc-radius-xs);
  cursor: pointer;
  transition: background 0.1s ease, color 0.1s ease;
}
.kiwi-cap-row:hover { background: var(--cc-surface-2); }
.kiwi-cap-row.copied { background: var(--cc-kiwi-tint); }
.kiwi-cap-row.copied .kiwi-cap-icon { color: var(--cc-sev-ok); }
.kiwi-cap-time { color: var(--cc-text-dim); text-align: right; }
.kiwi-cap-addr { overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
                 font-family: var(--cc-mono); }
.kiwi-cap-icon { color: var(--cc-text-dim); font-size: var(--cc-fs-xs); }

/* Assistant / observer section */
.kiwi-obs-row { display: flex; align-items: center; gap: 0.5rem; padding: 0.15rem 0; }
.kiwi-obs-lbl { min-width: 4rem; }
.kiwi-dot { width: 8px; height: 8px; border-radius: 50%; background: var(--cc-text-dim);
            flex-shrink: 0; }
.kiwi-dot-ok   { background: var(--cc-sev-ok); }
.kiwi-dot-warn { background: var(--cc-sev-warn); }
.kiwi-dot-fail { background: var(--cc-sev-fail); }
</style>
