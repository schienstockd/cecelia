<script setup lang="ts">
// Kiwi's prompt box + claims feed (KIWI_ASSISTANT_PLAN Phase 4, *UI shape*). What's attached sits
// above the box as ROWS (the canonical `SelectionTable`, compact, drag-resizable columns), each
// saying what the object is — a plot's measure, series and images, not just "plot". The reply is a
// feed of CLAIMS, not a chat transcript — each claim shows its kind (an interpretation carries a
// first-person "I think" flag, Decision 10), its text and its refs, which point at the thing when
// clicked. While the engine works, one status line shows the latest step and the elapsed time — a turn
// takes 30 s to 2 min (Open decision 4). A reply can be followed up: the next question continues its
// engine session. State lives in `stores/kiwi.ts`.
import { ref, computed, nextTick } from 'vue'
import CcToggle from '../CcToggle.vue'
import ConfirmButton from '../ConfirmButton.vue'
import TeleportPopover from '../TeleportPopover.vue'
import SelectionTable, { type SelectionColumn } from '../SelectionTable.vue'
import InlineNote from '../InlineNote.vue'
import AgentModelSelect from '../AgentModelSelect.vue'
import KiwiRefChip from './KiwiRefChip.vue'
import { useKiwiStore } from '../../stores/kiwi'
import { useSettingsStore } from '../../stores/settings'
import { useProjectStore } from '../../stores/project'
import { useTaskDefsStore } from '../../stores/taskDefs'
import { useObserverStore } from '../../stores/observer'
import { useNowTick } from '../../composables/useNowTick'
import { useKiwiPoint } from '../../composables/useKiwiPoint'
import { TASK_STATUS } from '../../lib/taskStatus'
import { parseRailTime, taskElapsed } from '../../utils/taskElapsed'
import { searchRefs, stepLabel, turnMeta, attachmentRows, claimRows,
         type AttachmentRow, type ClaimRow, type KiwiTurn, type RefCandidate } from '../../utils/kiwiTurn'

const kiwi = useKiwiStore()
const settings = useSettingsStore()
const project = useProjectStore()
const taskDefs = useTaskDefsStore()
const observer = useObserverStore()
const now = useNowTick()

const canAsk = computed(() => !kiwi.busy && (kiwi.prompt.trim().length > 0 || kiwi.refs.length > 0))
function onKey(e: KeyboardEvent) {
  if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) { e.preventDefault(); void kiwi.ask() }
}

// ── attachments ──
const ATTACH_COLUMNS: SelectionColumn[] = [
  { key: 'kind', label: 'Kind', width: 44 },
  { key: 'label', label: 'Attached', width: 220 },
]
const attachments = computed(() => attachmentRows(kiwi.refs, kiwi.draftResults))
const { pointAt } = useKiwiPoint()
const openRow = (r: AttachmentRow) => void pointAt(r.ref, r.label, kiwi.draftResults[r.id])

// ── claims: one row each — what kind, what it says, what it points at ──
const CLAIM_COLUMNS: SelectionColumn[] = [
  { key: 'n', label: '#', width: 22, fixed: true },
  { key: 'text', label: 'Claim', width: 200 },
  { key: 'refs', label: 'Points at', width: 120 },
]
function openClaim(r: ClaimRow) {
  const first = r.refs[0]
  if (first) void pointAt(first.ref, '', first.result)
}

// ── the running turn: one status line, ticking ──
const feed = computed<KiwiTurn[]>(() => [...kiwi.turns].reverse())
const runStep = computed(() => {
  const s = kiwi.running?.steps ?? []
  return s.length ? stepLabel(s[s.length - 1]) : 'starting'
})
const runElapsed = computed(() => taskElapsed(parseRailTime(kiwi.running?.startedAt), undefined, now.value) ?? '')
const runSteps = computed(() => (kiwi.running?.steps ?? []).map(stepLabel).join('\n') || 'No steps yet')

const promptOf = (id?: string) => kiwi.turns.find(t => t.turnId === id)?.prompt ?? ''
const canFollow = (t: KiwiTurn) => t.status !== 'running' && !!t.reply?.sessionId

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
    <!-- drag-resizable columns, widths kept per table (the resize handles live in the header) -->
    <SelectionTable v-if="attachments.length" selection-mode="none" density="compact"
                    column-width-key="cc.kiwi.attach.colw"
                    :columns="ATTACH_COLUMNS" :rows="attachments" id-key="id" actions-width="1.6rem"
                    :row-tooltip="r => r.tip" @row-click="openRow">
      <template #cell-kind="{ row: r }"><span class="cc-muted">{{ r.kind }}</span></template>
      <template #cell-label="{ row: r }">
        <div class="kiwi-att">
          <span class="kiwi-att-label">{{ r.label }}</span>
          <span v-if="r.detail" class="cc-muted cc-fs-2xs kiwi-att-detail">{{ r.detail }}</span>
        </div>
      </template>
      <template #actions="{ row: r }">
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click.stop="kiwi.removeRef(r.ref)"
                v-tooltip.left="'Remove'">
          <i class="pi pi-times" />
        </button>
      </template>
    </SelectionTable>

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

    <div v-if="kiwi.followUpTurn" class="cc-row cc-row-tight">
      <InlineNote icon="pi-reply" :short="`Following up: ${kiwi.followUpTurn.prompt || 'the attachments'}`"
                  detail="Kiwi continues that conversation and can cite what it already looked at" />
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="kiwi.followUp = ''"
              v-tooltip.bottom="'Ask fresh instead'">
        <i class="pi pi-times" />
      </button>
    </div>

    <textarea v-model="kiwi.prompt" class="kiwi-input" rows="3" :disabled="kiwi.busy"
              :placeholder="kiwi.followUpTurn ? 'Your follow-up…' : 'Ask about what’s attached…'" @keydown="onKey"
              v-tooltip.bottom="'Ctrl+Enter to ask'" />

    <div class="cc-row cc-row-tight">
      <button ref="searchAnchor" class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="openSearch"
              v-tooltip.bottom="'Attach a set, image or task by name'">
        <i class="pi pi-plus" />
      </button>
      <span class="cc-row-group">
        <span class="cc-muted cc-fs-2xs">Think first</span>
        <CcToggle v-model="settings.kiwiReasoning" aria-label="Think first"
                  v-tooltip.bottom="'Write reasoning before the claims — slower, more tokens'" />
      </span>
      <AgentModelSelect v-if="observer.available" v-model="settings.kiwiModel" tip="Model Kiwi runs" />
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

    <div v-if="kiwi.running" class="cc-row cc-row-tight cc-fs-2xs">
      <i :class="['pi', TASK_STATUS.running.icon]" :style="{ color: TASK_STATUS.running.color }" />
      <span class="cc-muted kiwi-grow" v-tooltip.bottom="runSteps">{{ runStep }}</span>
      <span class="cc-readout cc-fs-2xs">{{ runElapsed }}</span>
    </div>

    <div v-for="t in feed" :key="t.turnId" class="kiwi-turn">
      <div v-if="t.followUp" class="cc-muted cc-fs-2xs">
        <i class="pi pi-reply" /> {{ promptOf(t.followUp) || 'an earlier reply' }}
      </div>
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
        <InlineNote v-if="t.reply.note" class="cc-fs-xs" :short="t.reply.note" />
        <SelectionTable v-if="t.reply.claims.length" selection-mode="none" density="compact"
                        column-width-key="cc.kiwi.claims.colw"
                        :columns="CLAIM_COLUMNS" :rows="claimRows(t.reply)" id-key="id"
                        @row-click="openClaim">
          <!-- a failed claim's number carries WHY: a ref that doesn't resolve or wasn't looked at -->
          <template #cell-n="{ row: r }">
            <span v-if="r.failed" class="cc-muted-error" v-tooltip.right="r.tip">{{ r.n }}</span>
            <span v-else class="cc-muted">{{ r.n }}</span>
          </template>
          <template #cell-text="{ row: r }">
            <!-- a question is an icon + a line: the app's InlineNote, with the legend's question glyph -->
            <InlineNote v-if="r.kind === 'question'" class="cc-fs-xs" icon="pi-question" :short="r.text" />
            <span v-else class="kiwi-claim-text cc-fs-xs" :class="`kiwi-claim-${r.kind}`">
              <span v-if="r.kind === 'interpretation'" class="kiwi-flag cc-fs-2xs">I think</span>
              {{ r.text }}
            </span>
          </template>
          <template #cell-refs="{ row: r }">
            <div class="kiwi-refs">
              <KiwiRefChip v-for="(x, j) in r.refs" :key="j" :kiwi-ref="x.ref" :result="x.result" :seen="x.seen" />
            </div>
          </template>
        </SelectionTable>
      </template>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-3xs kiwi-grow">{{ turnMeta(t) }}</span>
        <button v-if="canFollow(t)" class="cc-btn cc-btn-ghost cc-fs-2xs" :disabled="kiwi.busy"
                @click="kiwi.followUp = t.turnId"
                v-tooltip.bottom="'Ask a follow-up — Kiwi keeps this conversation'">
          <i class="pi pi-reply" /> Follow up
        </button>
      </div>
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
.kiwi-grow { flex: 1; min-width: 0; }
.kiwi-att { display: flex; flex-direction: column; min-width: 0; }
.kiwi-att-label, .kiwi-att-detail { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.kiwi-search { display: flex; flex-direction: column; gap: 0.3rem; padding: 0.35rem; min-width: 14rem; }
.kiwi-search-label { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.kiwi-turn { display: flex; flex-direction: column; gap: 0.3rem; padding-top: 0.4rem;
             border-top: 1px solid var(--cc-border); }
.kiwi-q { font-weight: 600; }
.kiwi-refs { display: flex; flex-direction: column; align-items: flex-start; gap: 0.1rem; min-width: 0; }
.kiwi-claim-interpretation { font-style: italic; }
/* table cells don't wrap (SelectionTable); a claim is a sentence and must */
.kiwi-claim-text { display: block; white-space: normal; overflow-wrap: anywhere; }
/* the "I think" flag reads as Kiwi's voice, not as a warning — severity colours mean QC, never tone */
.kiwi-flag { color: var(--cc-kiwi); font-style: italic; font-weight: 600; margin-right: 0.25rem; }
.kiwi-feed-foot { display: flex; justify-content: flex-end; }
</style>
