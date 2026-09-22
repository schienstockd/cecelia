<!--
  HMM state cards — one card per HMM state value on a chosen `live.cell.hmm.state.<measure>` column
  of a chosen segmentation. Medoid = the state-run whose owning track spends the largest fraction
  of its life in this state (Fig 4c pattern; BEHAVIOUR_CARDS_PLAN Decision 3). Thin wrapper around
  `CardsPanelInner` — same shape as MotifCardsView but with TWO pickers (segmentation + hmm
  column), server-discovered via the endpoint's `availableValueNames` + `availableHmmCols` payload.

  Registered in `interactiveViews.ts` under `hmmStateCards`, board group `clustering`,
  `rail: 'none'` — HMM state values are h5ad obs values, not populations.

  See docs/todo/BEHAVIOUR_CARDS_PLAN.md → Decisions 1, 2, 3, 5, 6.
-->
<script setup lang="ts">
import { computed, ref, useTemplateRef, watch } from 'vue'
import CardsPanelInner from './CardsPanelInner.vue'
import { hmmStateFamily } from './cardFamilies'
import { useProjectStore } from '../../stores/project'
import { resolveValueName } from '../../utils/valueName'
import type { Frame } from '../../plots/frame'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { maxPx?: number; padPx?: number; valueName?: string; hmmCol?: string }
  // BIDIR PR #4b — panel's persistKey forwarded by InteractivePanel; forwarded to CardsPanelInner
  // so per-card StripCells filter `(family='hmm-state-cards', plotId=<this>, cell=<state.path>)`.
  plotId?: string
}>()

const project = useProjectStore()
const rootUid = computed(() => props.imageUids[0] ?? '')
const activeVn = computed(() =>
  project.sets.flatMap(s => s.images).find(i => i.uid === rootUid.value)?.activeValueName)

// Server discovery: `availableValueNames` = every segmentation whose h5ad has any
// `live.cell.hmm.state.*` column. `availableHmmCols` = the columns on the resolved vn. Both
// filled by CardsPanelInner's `meta` emit after the first response; until then, `resolveValueName`
// returns the persisted picks unchanged.
const availableVns = ref<string[]>([])
const availableHmmCols = ref<string[]>([])
const valueName = computed(() =>
  resolveValueName(props.state.valueName, availableVns.value, availableVns.value, activeVn.value))
// Same priority for the hmm column: persisted → first eligible. There's no "active" concept for
// HMM columns (unlike segmentations), so `resolveValueName` collapses to `wanted → first`.
const hmmCol = computed(() =>
  resolveValueName(props.state.hmmCol, availableHmmCols.value, availableHmmCols.value))

function onMeta(m: {
  availableValueNames?: string[]; valueName?: string
  availableHmmCols?: string[];    hmmCol?: string
}) {
  if (m.availableValueNames) availableVns.value = m.availableValueNames
  if (m.availableHmmCols)    availableHmmCols.value = m.availableHmmCols
  if (m.valueName && props.state.valueName !== m.valueName) props.state.valueName = m.valueName
  if (m.hmmCol    && props.state.hmmCol    !== m.hmmCol)    props.state.hmmCol    = m.hmmCol
}

// If the resolved pick drifts away from the persisted one (e.g. persisted is no longer eligible),
// write the resolved one back so the picker shows what's actually used.
watch(valueName, v => { if (v && props.state.valueName !== v) props.state.valueName = v })
watch(hmmCol,    v => { if (v && props.state.hmmCol    !== v) props.state.hmmCol    = v })

// Short label for the hmm column dropdown — strip the `live.cell.hmm.state.` prefix so the
// dropdown reads "movement" instead of the fully-qualified path (same convention
// ClusterHmmStatesPanel uses).
const shortHmm = (c: string) => c.replace(/^live\.cell\.hmm\.state\./, '')

// The board's PDF export path calls `exportImage()` on the mounted view via InteractivePanel;
// proxy through to the inner's rasteriser. Point-out Frame proxied the same way as
// MotifCardsView / CellCardsView.
const innerRef = useTemplateRef<InstanceType<typeof CardsPanelInner>>('innerRef')
async function exportImage(): Promise<string | null> {
  return innerRef.value?.exportImage() ?? null
}
const proxyFrame: Frame = {
  toNorm: () => null, fromNorm: () => null,
  subFrames() { return innerRef.value?.getFrame?.().subFrames?.() ?? [] },
}
defineExpose({ exportImage, getFrame: (): Frame => proxyFrame })
</script>

<template>
  <div class="hsv">
    <!-- Same auto-hide toolbar shape as MotifCardsView / FlowMetricsView. Two labelled pickers:
         segmentation + hmm column. Only rendered when there's a real choice to make (>1). -->
    <div v-if="availableVns.length > 1 || availableHmmCols.length > 1" class="hsv-ctrl cc-panel-controls">
      <label v-if="availableVns.length > 1" class="cc-row hsv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Which segmentation to read HMM states from'">Segmentation</span>
        <select class="select-input" :value="valueName"
                v-tooltip.top="'Which segmentation to read HMM states from'"
                @change="state.valueName = ($event.target as HTMLSelectElement).value">
          <option v-for="v in availableVns" :key="v" :value="v">{{ v }}</option>
        </select>
      </label>
      <label v-if="availableHmmCols.length > 1" class="cc-row hsv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Which HMM measure to show state cards for'">HMM measure</span>
        <select class="select-input" :value="hmmCol"
                v-tooltip.top="'Which HMM measure to show state cards for'"
                @change="state.hmmCol = ($event.target as HTMLSelectElement).value">
          <option v-for="c in availableHmmCols" :key="c" :value="c">{{ shortHmm(c) }}</option>
        </select>
      </label>
    </div>
    <CardsPanelInner ref="innerRef" :family="hmmStateFamily"
                     :project-uid="projectUid" :image-uids="imageUids"
                     :value-name="valueName" :hmm-col="hmmCol" :state="state"
                     point-out-family="hmm-state-cards" :plot-id="plotId"
                     @meta="onMeta" />
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .hsv-ctrl (.cc-panel-controls) anchors to the card grid. */
.hsv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.hsv :deep(.ccv) { flex: 1; min-height: 0; }
.hsv-ctrl { display: flex; flex-direction: column; gap: 0.3rem; padding: 4px 6px; }
.hsv-terms { align-items: center; gap: 6px; }
</style>
