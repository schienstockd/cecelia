<!--
  Cards panel base — the CanvasPanel-wrapping spine for `cellCards` (mounted bare from
  `CLUSTER_PANELS` by LayoutCanvas, so the wrap has to come from inside the family view). The
  fetch / grid / statScales / PDF export live in `CardsPanelInner.vue`; this file adds the
  CanvasPanel chrome (title, activate/remove/duplicate) and forwards the inner's `exportImage`.

  Motif and HMM card wrappers do NOT go through this file — they mount `CardsPanelInner` directly,
  since `InteractivePanel` already draws its own CanvasPanel.

  Design source: docs/todo/BEHAVIOUR_CARDS_PLAN.md — Decisions 1, 2, 5, 8.
-->
<script setup lang="ts">
import { useTemplateRef } from 'vue'
import CanvasPanel from '../canvas/CanvasPanel.vue'
import CardsPanelInner from './CardsPanelInner.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import type { Card, CardFamily, ShownPop } from './cardsPanel'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  popType: 'clust' | 'trackclust' | 'region'; suffix: string
  shownPops?: ShownPop[]
  state: { maxPx?: number; padPx?: number }
  docked?: boolean
  family: CardFamily
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: []; cardSelect: [Card] }>()

const innerRef = useTemplateRef<InstanceType<typeof CardsPanelInner>>('innerRef')
async function exportImage(): Promise<string | null> {
  return innerRef.value?.exportImage() ?? null
}
defineExpose({ exportImage })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey"
               :title="family.title" :docked="docked"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <CardsPanelInner ref="innerRef" :family="family"
                     :project-uid="projectUid" :image-uids="imageUids"
                     :suffix="suffix" :shown-pops="shownPops" :state="state"
                     @card-select="emit('cardSelect', $event)">
      <template #detail="{ cards, statScales }">
        <slot name="detail" :cards="cards" :stat-scales="statScales" />
      </template>
    </CardsPanelInner>
    <template #footer>
      <button class="ccv-iconbtn cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" type="button"
              @click="emit('duplicate')" v-tooltip.top="'Duplicate this plot'">
        <i class="pi pi-copy" />
      </button>
    </template>
  </CanvasPanel>
</template>
