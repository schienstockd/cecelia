<!--
  Motif cards — one card per motif class discovered server-side in the image's cells h5ad. Thin
  wrapper around `CardsPanelInner` (no CanvasPanel wrap: `InteractivePanel` in `interactiveViews.ts`
  provides that already). Every layout / fetch / statScales / export concern lives in the inner.

  Registered in `interactiveViews.ts` under `motifCards`, board group `clustering`, `rail: 'none'` —
  motif classes are h5ad obs values, not populations, so there's no rail selection.

  This view owns the segmentation picker (`state.valueName`), populated from the endpoint's
  `availableValueNames` payload. Priority for the effective vn: persisted pick → active segmentation
  → first eligible — via the shared `resolveValueName` helper the track panels use.

  See docs/todo/BEHAVIOUR_CARDS_PLAN.md → Decisions 1, 2, 5, 6 (re-scoped 2026-09-20).
-->
<script setup lang="ts">
import { computed, ref, useTemplateRef, watch } from 'vue'
import CardsPanelInner from './CardsPanelInner.vue'
import { motifFamily } from './cardFamilies'
import { useProjectStore } from '../../stores/project'
import { resolveValueName } from '../../utils/valueName'
import type { Frame } from '../../plots/frame'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { maxPx?: number; padPx?: number; valueName?: string }
  // BIDIR PR #4b — panel's persistKey forwarded by InteractivePanel; forwarded to CardsPanelInner
  // so per-card StripCells filter `(family='motif-cards', plotId=<this>, cell=<motif.path>)`.
  plotId?: string
}>()

const project = useProjectStore()
const rootUid = computed(() => props.imageUids[0] ?? '')
const activeVn = computed(() =>
  project.sets.flatMap(s => s.images).find(i => i.uid === rootUid.value)?.activeValueName)

// Server discovery: `availableValueNames` is the eligible set (every segmentation whose h5ad has
// `motif.class`). Filled by CardsPanelInner's `meta` emit after the first response — until then
// empty, so `resolveValueName` returns the persisted pick unchanged.
const availableVns = ref<string[]>([])
const valueName = computed(() =>
  resolveValueName(props.state.valueName, availableVns.value, availableVns.value, activeVn.value))

function onMeta(m: { availableValueNames?: string[]; valueName?: string }) {
  if (m.availableValueNames) availableVns.value = m.availableValueNames
  // Converge the persisted pick on whatever the server actually rendered — so a first-mount empty
  // state resolves to a real name, and the picker's selected option matches what's on screen.
  if (m.valueName && props.state.valueName !== m.valueName) props.state.valueName = m.valueName
}

// If the resolved vn ever drifts away from the persisted one (e.g. the persisted pick is no longer
// eligible), write the resolved one back so the picker shows what was actually used.
watch(valueName, v => { if (v && props.state.valueName !== v) props.state.valueName = v })

// The board's PDF export path calls `exportImage()` on the mounted view via InteractivePanel;
// proxy through to the inner's rasteriser.
const innerRef = useTemplateRef<InstanceType<typeof CardsPanelInner>>('innerRef')
async function exportImage(): Promise<string | null> {
  return innerRef.value?.exportImage() ?? null
}
// Point-out Frame — proxied through to the inner's per-card subFrames. Same shape as
// CellCardsView / CardsPanelBase so the consumer sees a uniform (family, plotId, cell=<path>)
// address regardless of which wrapper mounted the inner.
const proxyFrame: Frame = {
  toNorm: () => null, fromNorm: () => null,
  subFrames() { return innerRef.value?.getFrame?.().subFrames?.() ?? [] },
}
defineExpose({ exportImage, getFrame: (): Frame => proxyFrame })
</script>

<template>
  <div class="mcv">
    <!-- one auto-hide control strip (docs/UI.md → "Auto-hide panel controls"), same pattern
         FlowMetricsView / TrackDiagnosticsView / UmapView use for their in-body toolbars. Same
         `4px 6px` padding + labelled `<label><span>…</span><select></label>` shape as FlowMetrics'
         "channel" / "region" pickers. -->
    <div v-if="availableVns.length > 1" class="mcv-ctrl cc-panel-controls">
      <label class="cc-row mcv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Which segmentation to read motif classes from'">Segmentation</span>
        <select class="select-input" :value="valueName"
                v-tooltip.top="'Which segmentation to read motif classes from'"
                @change="state.valueName = ($event.target as HTMLSelectElement).value">
          <option v-for="v in availableVns" :key="v" :value="v">{{ v }}</option>
        </select>
      </label>
    </div>
    <CardsPanelInner ref="innerRef" :family="motifFamily"
                     :project-uid="projectUid" :image-uids="imageUids"
                     :value-name="valueName" :state="state"
                     point-out-family="motif-cards" :plot-id="plotId"
                     @meta="onMeta" />
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .mcv-ctrl (.cc-panel-controls) anchors to the card grid,
   mirroring TrackDiagnosticsView / FlowMetricsView. */
.mcv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.mcv :deep(.ccv) { flex: 1; min-height: 0; }
.mcv-ctrl { display: flex; flex-direction: column; gap: 0.3rem; padding: 4px 6px; }
.mcv-terms { align-items: center; gap: 6px; }
</style>
