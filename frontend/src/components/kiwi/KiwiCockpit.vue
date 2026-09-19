<script setup lang="ts">
// Kiwi — the assist cockpit. A floating panel that gathers AI-assistant-adjacent controls
// so the user doesn't scavenger-hunt across ViewerPanel (push chip), LabLogPanel (chat button)
// and Settings → MCP (connection health) to find them.
//
// v1 rows (this file):
//   - Pairing chip (mirrors what ViewerPanel used to show; ViewerPanel's copy is removed here too)
//   - Copy chat starter (moved from LabLogPanel)
//
// v2 adds MCP health, setup CTA merge, lab-log peek, Blackboard list — see
// docs/todo/KIWI_PLAN.md → *Contents — v2*. The provider-neutral wording is enforced by
// kiwiNamingRatchet.test.ts, which greps this directory for the literal "Claude".
import { ref, computed, watch, onUnmounted } from 'vue'
import FloatingPanel from '../FloatingPanel.vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { fetchPushTarget, pushChipLabel, type PairedState } from '../../utils/pushTarget'
import { usePushStore } from '../../stores/push'
import { buildChatPrompt } from '../../lib/chatHandoff'

defineEmits<{ (e: 'close'): void }>()

const pm = useProjectMetaStore()
const projectUid = computed(() => pm.current?.uid ?? '')
const projectName = computed(() => pm.current?.name ?? undefined)

// Push pairing chip — same source of truth as the old ViewerPanel chip (BIDIR_PUSH_PLAN PR #1 + #3).
// Not extracted to a composable yet: rule of three, and the ViewerPanel copy is being removed in the
// same PR, so this is the only caller by the time the PR lands.
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
  }
})

onUnmounted(() => { if (pushFlashTimer) clearTimeout(pushFlashTimer) })
watch(projectUid, () => { void refreshPushTarget() }, { immediate: true })

const { isCopied: chatCopied, copy: copyChatPrompt } = useCopyFlash(2500)
async function copyChatStarter() {
  if (!projectUid.value) return
  await copyChatPrompt(buildChatPrompt(projectUid.value, projectName.value))
}
</script>

<template>
  <FloatingPanel title="Kiwi" icon="pi-comments" storage-key="kiwi"
                 accent="var(--cc-kiwi)"
                 :default-x="260" :default-y="100" :default-w="300" :default-h="240"
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
</style>
