<!--
  Cell cards — thin wrapper around `CardsPanelBase` that supplies the `cellFamily` config and the
  cell-specific detail panel. Every layout / fetch / statScales / export concern lives in the base.

  See docs/todo/BEHAVIOUR_CARDS_PLAN.md → Decisions 1, 2.
  Historical rationale for the cell-cards Analysis-board entry: docs/todo/CELL_CARDS_PLAN.md.
-->
<script setup lang="ts">
import { ref, useTemplateRef } from 'vue'
import CardsPanelBase from './CardsPanelBase.vue'
import CellCardDetailPanel from './CellCardDetailPanel.vue'
import { cellFamily } from './cardFamilies'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import type { Card, ShownPop } from './cardsPanel'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  popType: 'clust' | 'trackclust' | 'region'; suffix: string
  shownPops?: ShownPop[]
  state: { maxPx?: number; padPx?: number }
  docked?: boolean
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()

const detailCard = ref<Card | null>(null)

const baseRef = useTemplateRef<InstanceType<typeof CardsPanelBase>>('baseRef')
// Board's PDF-export path calls `exportImage()` on the view instance; the base owns the rasteriser,
// so proxy through.
async function exportImage(): Promise<string | null> {
  return baseRef.value?.exportImage() ?? null
}
defineExpose({ exportImage })
</script>

<template>
  <CardsPanelBase ref="baseRef" :family="cellFamily"
                  :index="index" :active="active" :arrange="arrange" :persist-key="persistKey"
                  :project-uid="projectUid" :set-uid="setUid" :image-uids="imageUids"
                  :pop-type="popType" :suffix="suffix" :shown-pops="shownPops"
                  :state="state" :docked="docked"
                  @activate="emit('activate', $event)" @remove="emit('remove')"
                  @duplicate="emit('duplicate')" @card-select="detailCard = $event">
    <template #detail="{ statScales }">
      <CellCardDetailPanel v-if="detailCard" :card="detailCard" :project-uid="projectUid"
                           :stat-scales="statScales"
                           @close="detailCard = null" />
    </template>
  </CardsPanelBase>
</template>
