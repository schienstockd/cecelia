<!--
  Motif cards — one card per motif class discovered server-side in the image's cells h5ad. Thin
  wrapper around `CardsPanelInner` (no CanvasPanel wrap: `InteractivePanel` in `interactiveViews.ts`
  provides that already). Every layout / fetch / statScales / export concern lives in the inner.

  Registered in `interactiveViews.ts` under `motifCards`, board group `clustering`, `rail: 'none'` —
  motif classes are h5ad obs values, not populations, so there's no rail selection. The endpoint
  discovers which segmentation to read when the request omits `valueName`.

  See docs/todo/BEHAVIOUR_CARDS_PLAN.md → Decisions 1, 2, 5, 6 (re-scoped 2026-09-20).
-->
<script setup lang="ts">
import { useTemplateRef } from 'vue'
import CardsPanelInner from './CardsPanelInner.vue'
import { motifFamily } from './cardFamilies'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { maxPx?: number; padPx?: number }
}>()

// The board's PDF export path calls `exportImage()` on the mounted view via InteractivePanel;
// proxy through to the inner's rasteriser.
const innerRef = useTemplateRef<InstanceType<typeof CardsPanelInner>>('innerRef')
async function exportImage(): Promise<string | null> {
  return innerRef.value?.exportImage() ?? null
}
defineExpose({ exportImage })
</script>

<template>
  <CardsPanelInner ref="innerRef" :family="motifFamily"
                   :project-uid="projectUid" :image-uids="imageUids"
                   :state="state" />
</template>
