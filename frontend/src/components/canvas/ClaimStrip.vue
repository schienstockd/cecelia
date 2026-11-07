<script setup lang="ts">
// The claim recital on a summary plot (docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → GUI, B6). State a
// claim from the plot — its scope is the plot's set, populations and measure — and the record checks
// run at once; one chip per stage says what they found. "Review" asks the model (a Kiwi turn, about
// $1) on request. The findings live on a Blackboard entry, marked there Good / Bad / Unsure.
// The entry id is kept in the panel's persisted `ui`, so the claim survives navigation and a restate
// is a new round on the SAME entry, not a second entry.
import { computed, onMounted, ref, useTemplateRef, watch } from 'vue'
import { useRouter } from 'vue-router'
import TeleportPopover from '../TeleportPopover.vue'
import InlineNote from '../InlineNote.vue'
import ChipSelect from '../ChipSelect.vue'
import { useToast } from '../../composables/useToast'
import { useKiwiStore } from '../../stores/kiwi'
import { fetchImageAttrs } from '../../composables/useImageAttrs'
import type { SeriesTarget } from '../../plots/types'
import {
  CLAIM_STAGES, claimScopeOf, claimUnitsOf, defaultUnitAttr, stageCounts, skippedLine,
  stateClaim, checkClaim, reviewClaim, type ClaimResult,
} from '../../utils/claimApi'

const props = defineProps<{
  projectUid: string
  setUid?: string | null
  series: SeriesTarget[]   // the populations the plot draws
  measure?: string
  // the panel's persisted bag — the claim's entry, text and replicate attributes (`claimUnit`: the
  // single attribute a panel kept before a replicate could combine several)
  ui: { claimEntryId?: string; claimText?: string; claimUnits?: string[]; claimUnit?: string }
}>()

const toast = useToast()
const router = useRouter()
const kiwi = useKiwiStore()

const scope = computed(() => claimScopeOf({ setUid: props.setUid, series: props.series, measure: props.measure }))
const result = ref<ClaimResult | null>(null)
const busy = ref(false)
const reviewTurn = ref('')

const open = ref(false)
const btn = useTemplateRef<HTMLElement>('btn')
const draftText = ref('')
const draftUnits = ref<string[]>([])
const attrs = ref<string[]>([])

async function openForm() {
  draftText.value = props.ui.claimText ?? ''
  attrs.value = (await fetchImageAttrs(props.projectUid, props.setUid ?? null, [])).map(a => a.name)
  const kept = claimUnitsOf(props.ui)
  draftUnits.value = kept ?? [defaultUnitAttr(attrs.value)].filter(Boolean)
  open.value = true
}

async function submit() {
  const sc = scope.value
  const text = draftText.value.trim()
  if (!sc || !text) return
  busy.value = true
  const r = await stateClaim({ projectUid: props.projectUid, ...sc, unitAttrs: draftUnits.value, text,
                               ...(props.ui.claimEntryId ? { entryId: props.ui.claimEntryId } : {}) })
  busy.value = false
  if (!r) { toast.add({ severity: 'error', life: 5000, summary: 'Couldn’t check the claim' }); return }
  props.ui.claimEntryId = r.entryId; props.ui.claimText = text; props.ui.claimUnits = [...draftUnits.value]; delete props.ui.claimUnit
  result.value = r
  open.value = false
}

async function recheck() {
  if (!props.ui.claimEntryId) return
  const r = await checkClaim(props.projectUid, props.ui.claimEntryId)
  if (r) result.value = r
  else { props.ui.claimEntryId = undefined; result.value = null }   // the entry was deleted
}

async function review() {
  if (!props.ui.claimEntryId) return
  const tid = await reviewClaim(props.projectUid, props.ui.claimEntryId)
  if (!tid) { toast.add({ severity: 'warn', life: 5000, summary: 'Kiwi is busy — try again when it’s done' }); return }
  reviewTurn.value = tid
}
// The review is a Kiwi turn, so the Kiwi store already follows it (`kiwi:done`, and a reload after a
// reconnect); it has landed once its turn is in the feed. The entry is written before `kiwi:done`.
watch(() => kiwi.turns, ts => {
  if (!reviewTurn.value || !ts.some(t => t.turnId === reviewTurn.value)) return
  reviewTurn.value = ''
  void recheck()
  toast.add({ severity: 'success', life: 5000, summary: 'Review is on the Blackboard', detail: 'Click to open the entry.',
              onClick: openEntry })
})

function openEntry() {
  if (props.ui.claimEntryId) void router.push({ path: '/blackboard', query: { entry: props.ui.claimEntryId } })
}

const counts = computed(() => result.value ? stageCounts(result.value.findings) : null)
function stageTip(stage: string): string {
  const fs = (result.value?.findings ?? []).filter(f => f.stage === stage && f.level === 'warn').map(f => f.short)
  return fs.length ? fs.join('\n') : 'Nothing found by these checks'
}
const notChecked = computed(() => result.value ? skippedLine(result.value.skipped) : '')

onMounted(() => { void recheck() })
watch(() => props.projectUid, () => { result.value = null; void recheck() })
</script>

<template>
  <span class="claim-strip cc-row cc-row-tight">
    <button ref="btn" class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" type="button"
            :class="{ 'cc-btn-on': !!ui.claimEntryId }" :disabled="!scope && !ui.claimEntryId"
            v-tooltip.top="!scope ? 'A claim needs a plot across a set, with a measure' : ui.claimEntryId ? 'Restate the claim' : 'State a claim from this plot'"
            @click.stop="openForm">
      <i class="pi pi-verified" />
    </button>
    <template v-if="result && counts">
      <InlineNote v-for="s in CLAIM_STAGES" :key="s.stage" class="cc-fs-xs"
                  :short="`${s.label} ${counts[s.stage] || '✓'}`" :detail="stageTip(s.stage)"
                  :severity="counts[s.stage] ? 'warn' : 'ok'" />
      <InlineNote v-if="notChecked" class="cc-fs-xs" short="Partial" :detail="notChecked" />
      <button class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" type="button" :disabled="!!reviewTurn"
              v-tooltip.top="'Ask the model to review the claim against the record (~$1)'" @click="review">
        {{ reviewTurn ? 'Reviewing…' : 'Review' }}
      </button>
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" type="button"
              v-tooltip.top="'Open the claim on the Blackboard'" @click="openEntry">
        <i class="pi pi-align-left" />
      </button>
    </template>
    <TeleportPopover v-model="open" :anchor="btn" placement="bottom-start">
      <div class="claim-form" @click.stop>
        <input v-model="draftText" class="claim-text" type="text" placeholder="T cells move faster than B cells"
               aria-label="The claim" v-tooltip.top="'The claim, in your words'" @keydown.enter="submit" />
        <div class="cc-row cc-row-tight cc-muted cc-fs-xs claim-replicate">
          <span v-tooltip.top="'What counts as one independent sample (n) — select several to combine'">Replicate</span>
          <ChipSelect v-model="draftUnits" multiple :options="attrs.map(a => ({ value: a }))" />
        </div>
        <div v-if="scope" class="cc-muted cc-fs-2xs">{{ scope.populations.join(' vs ') }} · {{ scope.measure }}</div>
        <div class="cc-row cc-row-tight claim-actions">
          <button class="cc-btn" type="button" @click="open = false">Cancel</button>
          <button class="cc-btn cc-btn-primary" type="button" :disabled="!draftText.trim() || !scope || busy"
                  @click="submit">{{ ui.claimEntryId ? 'Restate' : 'Check' }}</button>
        </div>
      </div>
    </TeleportPopover>
  </span>
</template>

<style scoped>
.claim-form { display: flex; flex-direction: column; gap: 6px; padding: 8px; min-width: 18rem; }
.claim-text { width: 100%; }
.claim-actions { justify-content: flex-end; }
.claim-replicate { flex-wrap: wrap; }
</style>
