<!--
  The map's maturity overview (docs/todo/FEATURE_MATURITY_PLAN.md → *The map shows it*): every feature
  by guide-picker group, one expandable row each. Collapsed: the tier badge. Expanded: what it was
  tested on, the evidence, the checklist to the next tier, and the data that would help — linked to its
  call-for-datasets entry. Features with no guide are dimmed "no guide yet" rows; exempt features (no
  scientific result) have no row; Coming soon rows come from the task flags, not a record.

  All joins live in `utils/maturity.ts` (tested); this file only renders them. Which rows are expanded
  is a passing look inside a dialog, not a page option, so it is not persisted; the section's own
  open/closed state is (storage-key).
-->
<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import CollapsibleSection from './CollapsibleSection.vue'
import ComingSoonBadge from './ComingSoonBadge.vue'
import { useTaskDefsStore } from '../stores/taskDefs'
import { maturityGroups, type MaturityRow } from '../utils/maturity'
import { tierTip } from '../lib/guides/tiers'
import { findAsk } from '../lib/callForDatasets'
import { openCallForDatasets } from '../lib/callForDatasetsOpen'
import { closeGuides } from '../lib/guideOpen'

const taskDefs = useTaskDefsStore()
onMounted(() => { void taskDefs.ensureLoaded() })
const groups = computed(() => maturityGroups(taskDefs.all()))

const expanded = ref(new Set<string>())
const isOpen = (id: string) => expanded.value.has(id)
function toggle(id: string) {
  const next = new Set(expanded.value)
  if (!next.delete(id)) next.add(id)
  expanded.value = next
}

const rowTip = (r: MaturityRow) => tierTip(r.tier)
const ask = (r: MaturityRow) => findAsk(r.maturity?.datasetAsk)

// the call-for-datasets modal is its own dialog: close this one first rather than stack two
function shareData(id: string) {
  closeGuides()
  openCallForDatasets(id)
}
</script>

<template>
  <CollapsibleSection label="Maturity" :default-open="false" max-height="none"
                      storage-key="cc.guides.maturityOpen"
                      tip="How far to trust each feature, and what would move it up">
    <div v-for="g in groups" :key="g.heading" class="mm-group">
      <h4 class="mm-head cc-eyebrow cc-fs-2xs">{{ g.heading }}</h4>
      <div v-for="r in g.rows" :key="r.id" :class="['mm-row', { dim: !r.guided && r.maturity }]">
        <button v-if="r.maturity" class="mm-line cc-section-toggle cc-fs-xs" :aria-expanded="isOpen(r.id)"
                @click="toggle(r.id)" v-tooltip.right="rowTip(r)">
          <i :class="['pi', isOpen(r.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          <span class="mm-title">{{ r.title }}</span>
          <span v-if="!r.guided" class="cc-muted cc-fs-2xs">no guide yet</span>
          <ComingSoonBadge :tier="r.tier" class="mm-badge" />
        </button>
        <div v-else class="mm-line mm-soon cc-fs-xs">
          <span class="mm-title">{{ r.title }}</span>
          <ComingSoonBadge :tier="r.tier" :note="r.note" class="mm-badge" />
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
            <dd>
              {{ r.maturity.needs }}
              <button v-if="ask(r)" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-2xs" @click="shareData(ask(r)!.id)"
                      v-tooltip.top="'Open its call for datasets'">
                Share your data
              </button>
            </dd>
          </template>
        </dl>
      </div>
    </div>
  </CollapsibleSection>
</template>

<style scoped>
.mm-group { margin: 4px 0 8px; }
.mm-head { margin: 0 0 2px; }
.mm-line { gap: 6px; padding: 2px 4px; }
.mm-soon { display: flex; align-items: center; }
.mm-title { color: var(--cc-text); }
.mm-badge { margin-left: auto; }
/* no guide yet: dimmed, still readable and still expandable */
.mm-row.dim .mm-line { opacity: 0.7; }
.mm-body { display: grid; grid-template-columns: max-content 1fr; gap: 2px 10px; margin: 2px 0 6px 22px; }
.mm-body dd { margin: 0; }
.mm-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 2px; }
</style>
