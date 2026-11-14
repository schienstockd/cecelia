<!--
  The maturity overview, the body of `MaturityDialog.vue` (docs/todo/FEATURE_MATURITY_PLAN.md): every feature
  by guide-picker group, one expandable row each. Collapsed: the tier badge. Expanded: what it was
  tested on, the evidence, the checklist to the next tier, and the data that would help — linked to its
  call-for-datasets entry. Features with no guide are rows marked "no guide"; exempt features (no
  scientific result) have no row; Coming soon rows come from the task flags, not a record.

  All joins live in `utils/maturity.ts` (tested); this file only renders them. Which rows are expanded
  is a passing look inside a dialog, not a page option, so it is not persisted. A page's or task's
  maturity link opens the dialog on its row (`lib/maturityOpen.ts` → `openMaturityRow`); every expanded
  row carries "Something wrong?".
-->
<script setup lang="ts">
import { computed, nextTick, onMounted, watch } from 'vue'
import { chipDot, tierLabel } from '../utils/guideMap'
import { useTaskDefsStore } from '../stores/taskDefs'
import { maturityGroups, splitColumns, type MaturityRow } from '../utils/maturity'
import { useSingleOpenSection } from '../composables/useSingleOpenSection'
import { tierTip } from '../lib/guides/tiers'
import { findAsk } from '../lib/callForDatasets'
import { openCallForDatasets } from '../lib/callForDatasetsOpen'
import { closeMaturity, maturityFocus } from '../lib/maturityOpen'
import ReportProblemButton from './ReportProblemButton.vue'

const taskDefs = useTaskDefsStore()
onMounted(() => { void taskDefs.ensureLoaded() })
const groups = computed(() => maturityGroups(taskDefs.all()))
const columns = computed(() => splitColumns(groups.value))

// one row open at a time (accordion), remembered per profile
const rowOpen = useSingleOpenSection('cc.guides.maturityRow')
const isOpen = (id: string) => rowOpen.isOpen(id)
const toggle = (id: string) => rowOpen.toggle(id)

// opened from a page's or task's maturity link: open that row, then scroll to it. The row may be a
// Coming soon task, which only exists once the task specs have loaded — hence the watch.
const focus = maturityFocus.value
maturityFocus.value = null
if (focus) rowOpen.set(focus)
function scrollToFocus(): boolean {
  const el = focus ? document.querySelector(`[data-maturity-row="${CSS.escape(focus)}"]`) : null
  el?.scrollIntoView({ block: 'center' })
  return !!el
}
onMounted(() => {
  if (!focus) return
  void nextTick(() => {
    if (scrollToFocus()) return
    const stop = watch(groups, () => void nextTick(() => { if (scrollToFocus()) stop() }))
  })
})

const rowTip = (r: MaturityRow) => tierTip(r.tier)
const ask = (r: MaturityRow) => findAsk(r.maturity?.datasetAsk)

// the call-for-datasets modal is its own dialog: close this one first rather than stack two
function shareData(id: string) {
  closeMaturity()
  openCallForDatasets(id)
}
</script>

<template>
    <div class="mm-cols">
    <div v-for="(col, ci) in columns" :key="ci" class="mm-col">
    <div v-for="g in col" :key="g.heading" class="mm-group">
      <h4 class="mm-head cc-eyebrow cc-fs-2xs">{{ g.heading }}</h4>
      <div v-for="r in g.rows" :key="r.id" class="mm-row" :data-maturity-row="r.id">
        <button v-if="r.maturity" class="mm-line cc-section-toggle cc-fs-xs" :aria-expanded="isOpen(r.id)"
                @click="toggle(r.id)" v-tooltip.right="rowTip(r)">
          <i :class="['pi', isOpen(r.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          <span class="mm-title">{{ r.title }}</span>
          <span v-if="!r.guided" class="cc-muted cc-fs-2xs">no guide</span>
          <span :class="['cc-dot', chipDot(r.tier), 'mm-badge']" :aria-label="tierLabel(r.tier)" />
        </button>
        <div v-else class="mm-line mm-soon cc-fs-xs">
          <span class="mm-title">{{ r.title }}</span>
          <span v-if="r.note" class="cc-muted cc-fs-2xs">{{ r.note }}</span>
          <span :class="['cc-dot', chipDot(r.tier), 'mm-badge']" :aria-label="tierLabel(r.tier)" />
        </div>

        <dl v-if="r.maturity && isOpen(r.id)" class="mm-body cc-fs-2xs">
          <dt class="cc-muted">Tested on</dt><dd>{{ r.maturity.testedOn }}</dd>
          <template v-if="r.maturity.checkedBy">
            <dt class="cc-muted">Checked by</dt>
            <dd>
              {{ r.maturity.checkedBy.who }}, {{ r.maturity.checkedBy.when }}<template
                v-if="r.maturity.checkedBy.version"> (v{{ r.maturity.checkedBy.version }})</template>
              — {{ r.maturity.checkedBy.data }}
            </dd>
          </template>
          <template v-if="r.maturity.handCheck">
            <dt class="cc-muted">Hand check</dt><dd>{{ r.maturity.handCheck }}</dd>
          </template>
          <template v-if="r.maturity.evidence?.length">
            <dt class="cc-muted">Evidence</dt>
            <dd>
              <ul class="mm-list">
                <li v-for="e in r.maturity.evidence" :key="e.path + e.test">
                  fails if {{ e.failsIf }}
                  <span class="cc-muted">— {{ e.path }} → {{ e.test }}</span>
                </li>
              </ul>
            </dd>
          </template>
          <template v-if="r.maturity.next?.length">
            <dt class="cc-muted">Next tier</dt>
            <dd>
              <ul class="mm-list">
                <li v-for="n in r.maturity.next" :key="n.text">
                  <i :class="['pi', n.done ? 'pi-check-circle' : 'pi-circle']" /> {{ n.text }}
                </li>
              </ul>
            </dd>
          </template>
          <template v-if="r.maturity.needs">
            <dt class="cc-muted">Needs data</dt>
            <dd>{{ r.maturity.needs }}</dd>
          </template>
          <dt />
          <dd class="cc-row">
            <button v-if="ask(r)" class="cc-btn cc-btn-bare cc-btn-micro cc-fs-2xs" @click="shareData(ask(r)!.id)"
                    v-tooltip.top="'Open its call for datasets'">
              <i class="pi pi-megaphone" /> Share data
            </button>
            <ReportProblemButton :feature="r.title" :tier="r.tier" text />
          </dd>
        </dl>
      </div>
    </div>
  </div>
  </div>
</template>

<style scoped>
.mm-cols { display: grid; grid-template-columns: repeat(auto-fit, minmax(17rem, 1fr)); gap: 0 20px; align-items: start; }
.mm-group { margin: 4px 0 8px; }

.mm-head { margin: 0 0 2px; }
.mm-line { gap: 6px; padding: 2px 4px; }
.mm-soon { display: flex; align-items: center; }
.mm-title { color: var(--cc-text); }
.mm-badge { margin-left: auto; }
.mm-body { display: grid; grid-template-columns: max-content minmax(0, 1fr); gap: 5px 12px; margin: 4px 0 10px 22px; line-height: 1.35; }
.mm-body dd { margin: 0; min-width: 0; overflow-wrap: anywhere; }
/* a grid/column child defaults to min-width:auto, so a long test path pushed it past its column */
.mm-col, .mm-group, .mm-row, .mm-body { min-width: 0; }
.mm-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 4px; }
</style>
