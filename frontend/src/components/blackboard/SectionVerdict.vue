<script setup lang="ts">
// A verdict on one decision of an agent run's record (docs/todo/AGENT_RUN_REVIEW_PLAN.md Decision 7).
// Good / Bad / Unsure, a note (required for Bad). A verdict from a chat session shows as a
// proposal until a person marks the section; clicking the active verdict clears it.
import { ref, computed, watch } from 'vue'
import type { SectionOutcome, SectionVerdict } from '../../utils/blackboardApi'

const props = defineProps<{ outcome?: SectionOutcome; busy?: boolean }>()
const emit = defineEmits<{ save: [verdict: SectionVerdict | '', note: string]; promote: [note: string] }>()

const NOTE_MAX = 2 * 1024        // matches the server cap
const OPTS: { v: SectionVerdict; icon: string; label: string; tip: string }[] = [
  { v: 'good',   icon: 'pi-thumbs-up',   label: 'Good',   tip: 'The decision was right' },
  { v: 'bad',    icon: 'pi-thumbs-down', label: 'Bad',    tip: 'The decision was wrong — say why' },
  { v: 'unsure', icon: 'pi-question',    label: 'Unsure', tip: 'Cannot tell from what is here' },
]

const proposal = computed(() => props.outcome?.by?.via === 'claude')
// the person's verdict; a proposal is not one
const mine = computed(() => proposal.value ? undefined : props.outcome)
const draftVerdict = ref<SectionVerdict | null>(null)
const draftNote = ref('')
watch(() => props.outcome, () => { draftVerdict.value = null; draftNote.value = '' })

function pick(v: SectionVerdict) {
  if (mine.value?.verdict === v && draftVerdict.value === null) { emit('save', '', ''); return }
  draftVerdict.value = v
  draftNote.value = mine.value?.note ?? ''
  if (v !== 'bad' && !draftNote.value) emit('save', v, '')
}
function save() {
  if (!draftVerdict.value) return
  if (draftVerdict.value === 'bad' && !draftNote.value.trim()) return
  emit('save', draftVerdict.value, draftNote.value.trim())
}
</script>

<template>
  <div class="sv cc-row cc-row-tight">
    <button v-for="o in OPTS" :key="o.v"
            class="cc-btn cc-btn-ghost cc-btn-dense sv-btn" :class="[`sv-${o.v}`, {
              'sv-active': (draftVerdict ?? mine?.verdict) === o.v }]"
            :disabled="busy" @click="pick(o.v)"
            v-tooltip.top="mine?.verdict === o.v ? 'Click again to clear' : o.tip">
      <i class="pi" :class="o.icon" /> {{ o.label }}
    </button>
    <template v-if="draftVerdict">
      <input v-model="draftNote" class="sv-note-input" type="text" :maxlength="NOTE_MAX"
             :placeholder="draftVerdict === 'bad' ? 'Why — required' : 'Note (optional)'"
             @keydown.enter="save" v-tooltip.top="'What a reviewer of the next run should know'" />
      <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="busy || (draftVerdict === 'bad' && !draftNote.trim())"
              @click="save" v-tooltip.top="'Save the verdict'">Save</button>
      <button class="cc-btn cc-btn-ghost cc-btn-dense" :disabled="busy" @click="draftVerdict = null">Cancel</button>
    </template>
    <template v-else-if="mine?.note">
      <span class="sv-note cc-fs-xs" v-tooltip.top="mine.note">{{ mine.note }}</span>
      <button class="cc-btn cc-btn-ghost cc-btn-dense sv-btn" :disabled="busy" @click="emit('promote', mine.note)"
              v-tooltip.top="'Promote to lab knowledge: a new entry from this note, carried into later agent runs'">
        <i class="pi pi-book" /> Lesson
      </button>
    </template>
    <span v-if="proposal && outcome" class="sv-proposal cc-fs-xs"
          v-tooltip.top="'A proposal from Claude; it does not count until you mark the section'">
      <i class="pi pi-sparkles" /> Claude proposes {{ outcome.verdict }}<template v-if="outcome.note">: {{ outcome.note }}</template>
    </span>
  </div>
</template>

<style scoped>
.sv { flex-wrap: wrap; margin: 2px 0 6px; }
.sv-btn { opacity: 0.6; }
.sv-btn.sv-active, .sv-btn:hover { opacity: 1; }
.sv-good.sv-active   { background: rgba(86, 180, 233, 0.18); border-color: rgba(86, 180, 233, 0.5); }
.sv-bad.sv-active    { background: rgba(213, 94, 0, 0.18);   border-color: rgba(213, 94, 0, 0.5); }
.sv-unsure.sv-active { background: rgba(148, 163, 184, 0.18); border-color: rgba(148, 163, 184, 0.5); }
.sv-note-input { flex: 1; min-width: 200px; font-size: var(--cc-fs-xs); }
.sv-note { color: var(--cc-text-dim); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 60%; }
.sv-proposal { color: var(--cc-text-dim); font-style: italic; }
</style>
