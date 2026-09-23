<script setup lang="ts">
// Kiwi's prompt box + claims feed (KIWI_ASSISTANT_PLAN Phase 4, *UI shape*). Attached refs ride as
// chips above the box; the reply is a feed of CLAIMS, not a chat transcript — each claim shows its
// kind (an interpretation carries a first-person "I think" flag, Decision 10), its text and its ref
// chips, which point at the thing when clicked. While the engine works, its steps (tool calls) show
// as they happen — a turn takes 30 s to 2 min (Open decision 4). State lives in `stores/kiwi.ts`.
import { ref, computed, nextTick } from 'vue'
import CcToggle from '../CcToggle.vue'
import ConfirmButton from '../ConfirmButton.vue'
import TeleportPopover from '../TeleportPopover.vue'
import KiwiRefChip from './KiwiRefChip.vue'
import { useKiwiStore } from '../../stores/kiwi'
import { useSettingsStore } from '../../stores/settings'
import { useProjectStore } from '../../stores/project'
import { useTaskDefsStore } from '../../stores/taskDefs'
import { searchRefs, stepLabel, turnMeta, type KiwiTurn, type RefCandidate } from '../../utils/kiwiTurn'

const kiwi = useKiwiStore()
const settings = useSettingsStore()
const project = useProjectStore()
const taskDefs = useTaskDefsStore()

const canAsk = computed(() => !kiwi.busy && (kiwi.prompt.trim().length > 0 || kiwi.refs.length > 0))
function onKey(e: KeyboardEvent) {
  if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) { e.preventDefault(); void kiwi.ask() }
}

// newest first; the last few steps of the running turn
const feed = computed<KiwiTurn[]>(() => [...kiwi.turns].reverse())
const recentSteps = computed(() => (kiwi.running?.steps ?? []).slice(-4))

// ── typed search: attach a set / image / task by name ──
const searchOpen = ref(false)
const searchAnchor = ref<HTMLElement | null>(null)
const searchInput = ref<HTMLInputElement | null>(null)
const query = ref('')
const candidates = computed<RefCandidate[]>(() => searchRefs(query.value, {
  sets: project.sets.map(s => ({ uid: s.uid, name: s.name, images: s.images.map(i => ({ uid: i.uid, name: i.name })) })),
  tasks: taskDefs.all().map(t => ({ fun_name: t.fun_name, label: t.label })),
}))
async function openSearch() {
  void taskDefs.ensureLoaded()
  query.value = ''
  searchOpen.value = true
  await nextTick()
  searchInput.value?.focus()
}
function pick(c: RefCandidate) {
  kiwi.addRef(c.ref)
  searchOpen.value = false
}
</script>

<template>
  <div class="kiwi-ask" data-guide="kiwi.ask">
    <div class="kiwi-draft cc-row cc-row-tight">
      <KiwiRefChip v-for="(r, i) in kiwi.refs" :key="i" :kiwi-ref="r" removable @remove="kiwi.removeRef(r)" />
      <button ref="searchAnchor" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="openSearch"
              v-tooltip.bottom="'Attach a set, image or task by name'">
        <i class="pi pi-plus" />
      </button>
    </div>
    <TeleportPopover v-model="searchOpen" :anchor="searchAnchor" flush>
      <div class="kiwi-search">
        <input ref="searchInput" v-model="query" class="cc-input-xs" placeholder="Name or id"
               v-tooltip.bottom="'Search this project’s sets, images and tasks'"
               @keydown.enter.prevent="candidates[0] && pick(candidates[0])" />
        <div class="cc-actions-menu">
          <button v-for="(c, i) in candidates" :key="i" class="cc-actions-item" @click="pick(c)">
            <span class="kiwi-search-label">{{ c.label }}</span>
            <span class="cc-muted cc-fs-2xs">{{ c.hint }}</span>
          </button>
          <div v-if="query && !candidates.length" class="cc-empty-inline cc-fs-2xs">No match</div>
        </div>
      </div>
    </TeleportPopover>

    <textarea v-model="kiwi.prompt" class="kiwi-input" rows="3" :disabled="kiwi.busy"
              placeholder="Ask about what’s attached…" @keydown="onKey"
              v-tooltip.bottom="'Ctrl+Enter to ask'" />

    <div class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs" v-tooltip.bottom="'Write reasoning before the claims — slower, more tokens'">Think first</span>
      <CcToggle v-model="settings.kiwiReasoning" aria-label="Think first" />
      <span class="kiwi-grow" />
      <button v-if="!kiwi.busy" class="cc-btn cc-btn-primary cc-fs-xs" :disabled="!canAsk" @click="kiwi.ask()"
              v-tooltip.bottom="'Ask Kiwi'">
        <i class="pi pi-send" /> Ask
      </button>
      <button v-else class="cc-btn cc-btn-ghost cc-fs-xs" @click="kiwi.cancel()" v-tooltip.bottom="'Stop this turn'">
        <i class="pi pi-times" /> Stop
      </button>
    </div>

    <div v-if="kiwi.error" class="cc-muted-error cc-fs-2xs">{{ kiwi.error }}</div>

    <div v-if="kiwi.running" class="kiwi-running cc-fs-2xs">
      <div class="cc-muted"><i class="pi pi-spin pi-spinner" /> {{ kiwi.running.prompt || 'Looking…' }}</div>
      <div v-for="(s, i) in recentSteps" :key="i" class="cc-muted kiwi-step">{{ stepLabel(s) }}</div>
    </div>

    <div v-for="t in feed" :key="t.turnId" class="kiwi-turn">
      <div class="kiwi-q cc-fs-xs">{{ t.prompt || '(no question — just the attachments)' }}</div>
      <div v-if="t.refs.length" class="cc-row cc-row-tight">
        <KiwiRefChip v-for="(r, i) in t.refs" :key="i" :kiwi-ref="r.ref" :result="r.result" />
      </div>
      <div v-if="t.status === 'cancelled'" class="cc-muted cc-fs-2xs">Stopped</div>
      <div v-else-if="!t.reply" class="cc-muted-error cc-fs-2xs">{{ t.error || 'No reply' }}</div>
      <template v-else>
        <div v-if="t.reply.abstain && !t.reply.claims.length" class="cc-muted cc-fs-xs">
          Can’t say from what I looked at.
        </div>
        <div v-for="(c, i) in t.reply.claims" :key="i" class="kiwi-claim" :class="`kiwi-claim-${c.kind}`">
          <div class="kiwi-claim-text cc-fs-xs">
            <span v-if="c.kind === 'interpretation'" class="kiwi-flag cc-fs-2xs"
                  v-tooltip.bottom="'Kiwi’s reading, not a fact'">I think</span>
            <i v-else-if="c.kind === 'question'" class="pi pi-question kiwi-qicon"
               v-tooltip.bottom="'A next look you can check'" />
            {{ c.text }}
          </div>
          <div class="cc-row cc-row-tight">
            <KiwiRefChip v-for="(r, j) in c.refs" :key="j" :kiwi-ref="r.ref" :result="r.result" :seen="r.seen" />
          </div>
        </div>
        <div v-if="!t.reply.ok && t.reply.errors.length" class="cc-muted-warn cc-fs-2xs"
             v-tooltip.bottom="t.reply.errors.slice(0, 3).join('\n')">
          {{ t.reply.errors.length }} check{{ t.reply.errors.length === 1 ? '' : 's' }} still failing
        </div>
      </template>
      <div class="cc-muted cc-fs-3xs">{{ turnMeta(t) }}</div>
    </div>

    <div v-if="feed.length" class="kiwi-feed-foot">
      <ConfirmButton @confirm="kiwi.clearFeed()" v-slot="{ armed, arm, confirm, cancel }">
        <button v-if="!armed" class="cc-btn cc-btn-ghost cc-fs-2xs" @click="arm"
                v-tooltip.bottom="'Delete every Kiwi reply for this project'">
          <i class="pi pi-trash" /> Clear replies
        </button>
        <template v-else>
          <button class="cc-btn cc-btn-danger cc-fs-2xs" @click="confirm" v-tooltip.bottom="'Confirm — delete every reply'">
            <i class="pi pi-check" /> Delete all
          </button>
          <button class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-2xs" @click="cancel" v-tooltip.bottom="'Cancel'">
            <i class="pi pi-times" />
          </button>
        </template>
      </ConfirmButton>
    </div>
  </div>
</template>

<style scoped>
.kiwi-ask { display: flex; flex-direction: column; gap: 0.4rem; }
.kiwi-input { width: 100%; resize: vertical; min-height: 3.5rem; }
.kiwi-grow { flex: 1; }
.kiwi-search { display: flex; flex-direction: column; gap: 0.3rem; padding: 0.35rem; min-width: 14rem; }
.kiwi-search-label { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.kiwi-running { display: flex; flex-direction: column; gap: 0.1rem; padding: 0.3rem 0.4rem;
                border-left: 2px solid var(--cc-kiwi); }
.kiwi-step { padding-left: 1.1rem; font-family: var(--cc-mono); }
.kiwi-turn { display: flex; flex-direction: column; gap: 0.3rem; padding-top: 0.4rem;
             border-top: 1px solid var(--cc-border); }
.kiwi-q { font-weight: 600; }
.kiwi-claim { display: flex; flex-direction: column; gap: 0.2rem; padding-left: 0.5rem;
              border-left: 2px solid var(--cc-border); }
.kiwi-claim-interpretation { border-left-style: dashed; border-left-color: var(--cc-kiwi); }
.kiwi-claim-question { border-left-color: var(--cc-kiwi); }
/* the "I think" flag reads as Kiwi's voice, not as a warning — severity colours mean QC, never tone */
.kiwi-flag { color: var(--cc-kiwi); font-style: italic; font-weight: 600; margin-right: 0.25rem; }
.kiwi-qicon { color: var(--cc-kiwi); margin-right: 0.2rem; }
.kiwi-feed-foot { display: flex; justify-content: flex-end; }
</style>
