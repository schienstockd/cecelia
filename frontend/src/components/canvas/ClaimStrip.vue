<script setup lang="ts">
// The claim recital on a summary plot (docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → GUI, B6). One
// button in the plot's footer: the claim's open problems as a count. It opens a popover with the
// claim, the rule checks as a list of facts (no model, instant) and the model review (one Kiwi turn,
// about a minute and ~$1, on request). Scope = the plot's set, plotted populations and measure.
// The findings live on a Blackboard entry, marked there Good / Bad / Unsure; the entry id is kept in
// the panel's persisted `ui`, so restating is a new round on the SAME entry, not a second entry.
import { computed, onMounted, ref, useTemplateRef, watch } from 'vue'
import { useRouter } from 'vue-router'
import TeleportPopover from '../TeleportPopover.vue'
import ChipSelect from '../ChipSelect.vue'
import InlineNote from '../InlineNote.vue'
import { SEVERITY } from '../../lib/severity'
import { useToast } from '../../composables/useToast'
import { useKiwiStore } from '../../stores/kiwi'
import { fetchImageAttrs } from '../../composables/useImageAttrs'
import type { SeriesTarget } from '../../plots/types'
import {
  claimScopeOf, claimUnitsOf, defaultUnitAttr, findingsOf, openCount,
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
const checking = ref(false)
const reviewTurn = ref('')     // the review turn running for this claim ('' = none)

const open = ref(false)
const editing = ref(false)
const btn = useTemplateRef<HTMLElement>('btn')
const draftText = ref('')
const draftUnits = ref<string[]>([])
const attrs = ref<string[]>([])

async function openPopover() {
  open.value = !open.value
  if (!open.value) return
  editing.value = !props.ui.claimEntryId
  draftText.value = props.ui.claimText ?? ''
  attrs.value = (await fetchImageAttrs(props.projectUid, props.setUid ?? null, [])).map(a => a.name)
  draftUnits.value = claimUnitsOf(props.ui) ?? [defaultUnitAttr(attrs.value)].filter(Boolean)
}

async function submit() {
  const sc = scope.value
  const text = draftText.value.trim()
  if (!sc || !text || checking.value) return
  checking.value = true
  const r = await stateClaim({ projectUid: props.projectUid, ...sc, unitAttrs: draftUnits.value, text,
                               ...(props.ui.claimEntryId ? { entryId: props.ui.claimEntryId } : {}) })
  checking.value = false
  if (!r) { toast.add({ severity: 'error', life: 5000, summary: 'Couldn’t check the claim' }); return }
  props.ui.claimEntryId = r.entryId; props.ui.claimText = text; props.ui.claimUnits = [...draftUnits.value]
  delete props.ui.claimUnit
  result.value = r
  editing.value = false
}

async function recheck() {
  if (!props.ui.claimEntryId) return
  const r = await checkClaim(props.projectUid, props.ui.claimEntryId)
  if (r) result.value = r
  else { props.ui.claimEntryId = undefined; result.value = null }   // the entry was deleted
  return r
}

// The review is a background Kiwi turn; the Kiwi store follows it (`kiwi:done`, and a reload after
// a reconnect), and the entry is written before `kiwi:done` is sent — so once the turn is in the
// store's feed, a re-check carries the review.
watch(() => kiwi.turns, ts => {
  if (!reviewTurn.value || !ts.some(t => t.turnId === reviewTurn.value)) return
  reviewTurn.value = ''
  void recheck()
})
async function review() {
  if (!props.ui.claimEntryId || reviewTurn.value) return
  const tid = await reviewClaim(props.projectUid, props.ui.claimEntryId)
  if (!tid) { toast.add({ severity: 'warn', life: 5000, summary: 'Kiwi is busy — try again when it’s done' }); return }
  reviewTurn.value = tid
}

function openEntry() {
  if (props.ui.claimEntryId) void router.push({ path: '/blackboard', query: { entry: props.ui.claimEntryId } })
}

const nOpen = computed(() => openCount(result.value))
const busy = computed(() => checking.value || !!reviewTurn.value)
const buttonTip = computed(() => !scope.value && !props.ui.claimEntryId ? 'A claim needs a plot across a set, with a measure'
  : !props.ui.claimEntryId ? 'State a claim from this plot'
  : reviewTurn.value ? 'Reviewing the claim…'
  : `“${props.ui.claimText ?? ''}” — ${nOpen.value || 'no'} open problem${nOpen.value === 1 ? '' : 's'}`)

onMounted(() => { void recheck() })
watch(() => props.projectUid, () => { reviewTurn.value = ''; result.value = null; void recheck() })
</script>

<template>
  <button ref="btn" class="cc-btn cc-btn-ghost cc-btn-dense claim-btn cc-fs-xs" type="button"
          :class="{ 'cc-btn-on': open }" :disabled="!scope && !ui.claimEntryId"
          v-tooltip.top="buttonTip" @click.stop="openPopover">
    <i class="pi" :class="busy ? 'pi-spin pi-spinner' : 'pi-verified'" />
    <span v-if="result && nOpen" :style="{ color: SEVERITY.warn.color }">{{ nOpen }}</span>
    <i v-else-if="result" class="pi" :class="SEVERITY.ok.icon" :style="{ color: SEVERITY.ok.color }" />
  </button>
  <TeleportPopover v-model="open" :anchor="btn" placement="bottom-start">
    <div class="claim-pop cc-fs-xs" @click.stop>
      <!-- the claim: its text, or the form that states / restates it -->
      <div v-if="!editing && ui.claimEntryId" class="cc-row cc-row-tight">
        <strong class="claim-title">{{ ui.claimText }}</strong>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" type="button"
                v-tooltip.top="'Restate the claim'" @click="editing = true"><i class="pi pi-pencil" /></button>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-dense" type="button"
                v-tooltip.top="'Open on the Blackboard, to mark each finding'" @click="openEntry"><i class="pi pi-align-left" /></button>
      </div>
      <template v-else>
        <input v-model="draftText" class="claim-text" type="text" placeholder="T cells move faster than B cells"
               aria-label="The claim" v-tooltip.top="'The claim, in your words'" @keydown.enter="submit" />
        <div class="cc-row cc-row-tight cc-muted claim-replicate">
          <span v-tooltip.top="'What counts as one independent sample (n) — select several to combine'">Replicate</span>
          <ChipSelect v-model="draftUnits" multiple :options="attrs.map(a => ({ value: a }))" />
        </div>
        <div v-if="scope" class="cc-muted cc-fs-2xs">{{ scope.populations.join(' vs ') }} · {{ scope.measure }}</div>
        <div class="cc-row cc-row-tight claim-actions">
          <button v-if="ui.claimEntryId" class="cc-btn" type="button" @click="editing = false">Cancel</button>
          <button class="cc-btn cc-btn-primary" type="button" :disabled="!draftText.trim() || !scope || checking"
                  @click="submit">
            <i v-if="checking" class="pi pi-spin pi-spinner" />
            {{ checking ? 'Checking…' : ui.claimEntryId ? 'Restate' : 'Check' }}
          </button>
        </div>
      </template>

      <template v-if="result">
        <!-- the rule checks: one fact per check -->
        <div class="claim-head cc-row cc-row-tight">
          <span>Checks</span><span class="cc-muted" v-tooltip.top="'Rules over the record — no model, no cost'">· no model</span>
        </div>
        <div class="claim-list">
          <template v-for="l in result.checks" :key="l.id">
            <InlineNote v-if="l.state === 'ok'" :short="l.text" :detail="l.tip" severity="ok" />
            <InlineNote v-else-if="l.state === 'skipped'" :short="`Not checked: ${l.text}`" :detail="l.tip" icon="pi-minus-circle" />
            <template v-else>
              <InlineNote v-for="f in findingsOf(result, l.id)" :key="f.code + f.short" :short="f.short" :detail="f.long"
                          :severity="f.level === 'warn' ? 'warn' : undefined" />
            </template>
          </template>
        </div>

        <!-- the model review: on request -->
        <div class="claim-head cc-row cc-row-tight">
          <span>Model review</span>
          <span class="cc-muted" v-tooltip.top="'One model turn reads the claim and the record, not your notes'">· ~$1, about a minute</span>
          <button class="cc-btn cc-btn-dense claim-review-btn" type="button" :disabled="!!reviewTurn" @click="review">
            <i v-if="reviewTurn" class="pi pi-spin pi-spinner" />
            {{ reviewTurn ? 'Reviewing…' : result.review ? 'Run again' : 'Run' }}
          </button>
        </div>
        <div v-if="result.review" class="claim-list">
          <InlineNote v-for="x in result.review.items" :key="x.sectionId ?? x.text" :short="x.text"
                      :severity="x.kind === 'question' ? undefined : 'warn'" :icon="x.kind === 'question' ? 'pi-question-circle' : undefined" />
          <InlineNote v-if="!result.review.items.length" short="Nothing beyond the checks" icon="pi-minus-circle" />
          <span v-if="result.review.supports" class="cc-muted claim-supports">Would support: {{ result.review.supports }}</span>
        </div>
      </template>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.claim-btn { gap: 3px; }
.claim-pop { display: flex; flex-direction: column; gap: 6px; padding: 8px; width: 26rem; max-height: 70vh; overflow-y: auto; }
.claim-title { flex: 1; }
.claim-text { width: 100%; }
.claim-replicate { flex-wrap: wrap; }
.claim-actions { justify-content: flex-end; }
.claim-head { margin-top: 4px; font-weight: 600; }
.claim-head .cc-muted { font-weight: normal; }
.claim-review-btn { margin-left: auto; }
.claim-list { display: flex; flex-direction: column; gap: 3px; }
.claim-supports { font-style: italic; }
</style>
