<script setup lang="ts">
/**
 * The "Call for Datasets" modal — capabilities Cecelia can build, or check, once a real dataset
 * lands. Two sections by `kind`: "Help us build" (not built yet), then "Help us test" (built; data
 * with a known answer would check it — the maturity map's Share data buttons).
 *
 * One row per ask, one open at a time across both sections (an accordion, the MaturityMap shape:
 * `useSingleOpenSection` + bare `.cc-section-toggle` rows). Collapsed: title + the one-phrase why.
 * Open: a small schematic where one helps (`DatasetAskFigure`), the data we need in one line, and
 * the issue button + plan link. The long text lives in the issue body, `get_guide`, and the plan —
 * not here (docs/ui/COPY.md).
 *
 * Reachable from the `AppHeader` help row, a `?ask=<id>` deep link (App.vue), the maturity map's
 * Share data, and a param figure's On request chip — all of which open the modal AT that row
 * (`docs/todo/CALL_FOR_DATASETS_PLAN.md`). The modal does not host files: the issue asks for a link.
 */
import { computed, nextTick, onMounted, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import DatasetAskFigure from './DatasetAskFigure.vue'
import { asksOfKind, datasetAskUrl, planUrl } from '../lib/callForDatasets'
import { callForDatasetsFocusId, closeCallForDatasets } from '../lib/callForDatasetsOpen'
import { useSingleOpenSection } from '../composables/useSingleOpenSection'

// Not persisted: what's open is where the caller pointed, not a page option.
const rows = useSingleOpenSection('cc.callForDatasets.openAsk', '', { persist: false })

/** Open the deep-link target and scroll its row into view once it has painted. */
async function focusAsk() {
  const id = callForDatasetsFocusId.value
  if (!id) return
  rows.set(id)
  await nextTick()
  document.querySelector(`[data-ask="${id}"]`)?.scrollIntoView({ behavior: 'smooth', block: 'nearest' })
}
onMounted(focusAsk)
watch(callForDatasetsFocusId, focusAsk)

const sections = computed(() => [
  { kind: 'build', heading: 'Help us build', asks: asksOfKind('build') },
  { kind: 'test', heading: 'Help us test', asks: asksOfKind('test') },
].filter(s => s.asks.length))
</script>

<template>
  <BaseModal title="Call for Datasets" icon="pi-megaphone" width="640px" @close="closeCallForDatasets">
    <p class="cfd-intro cc-muted cc-fs-xs">Matching imaging? An issue with a sample link moves it.</p>

    <section v-for="s in sections" :key="s.kind" class="cfd-section">
      <h3 class="cfd-head cc-eyebrow cc-fs-2xs">{{ s.heading }}</h3>
      <div v-for="a in s.asks" :key="a.id" class="cfd-row" :data-ask="a.id">
        <button class="cfd-line cc-section-toggle cc-fs-sm" :aria-expanded="rows.isOpen(a.id)"
                @click="rows.toggle(a.id)">
          <i :class="['pi', rows.isOpen(a.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          <span class="cfd-title">{{ a.title }}</span>
          <span class="cfd-why cc-muted cc-fs-xs">{{ a.oneLiner }}</span>
        </button>

        <div v-if="rows.isOpen(a.id)" class="cfd-body">
          <DatasetAskFigure v-if="a.fig" :fig="a.fig" />
          <div class="cfd-text">
            <p class="cfd-need cc-fs-xs"><span class="cc-muted">We need:</span><span>{{ a.need }}</span></p>
            <div class="cfd-actions">
              <a class="cc-btn cc-btn-ghost" :href="datasetAskUrl(a)" target="_blank" rel="noopener"
                 v-tooltip.top="'Prefilled issue: your imaging and a sample link'">
                <i class="pi pi-github" /> Open an issue
              </a>
              <a class="cfd-plan cc-muted cc-fs-2xs" :href="planUrl(a)" target="_blank" rel="noopener"
                 v-tooltip.top="a.planPath">Plan</a>
            </div>
          </div>
        </div>
      </div>
    </section>
  </BaseModal>
</template>

<style scoped>
.cfd-intro { margin: 0 0 8px; }
.cfd-section { margin-bottom: 10px; }
.cfd-head { margin: 0 0 2px; }
.cfd-row { border-top: 1px solid var(--cc-border); }
.cfd-line { gap: 8px; padding: 6px 4px; min-width: 0; }
.cfd-title { color: var(--cc-text); white-space: nowrap; }
/* the why sits on the same line; a wide font truncates it rather than wrapping the row */
.cfd-why { flex: 1; min-width: 0; text-align: right; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.cfd-body { display: flex; gap: 12px; align-items: center; padding: 2px 4px 10px 22px; }
.cfd-text { display: flex; flex-direction: column; gap: 6px; min-width: 0; }
.cfd-text p { margin: 0; line-height: 1.35; }
/* the need wraps as its own block beside the label, not back under it */
.cfd-need { display: flex; gap: 0.35em; }
.cfd-need > span:first-child { flex: none; }
.cfd-actions { display: flex; align-items: center; gap: 10px; }
</style>
