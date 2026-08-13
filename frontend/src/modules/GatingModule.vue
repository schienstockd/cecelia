<!--
  Gating module page. Pick ONE image in the table; the gating plots render BELOW the table
  (wide left column) as a modular grid (count via slider), with a page-level segmentation
  selector and the floating Population Manager. Mirrors the old flowPlotManager layout.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import GatingPlots from './gate/GatingPlots.vue'
</script>

<template>
  <ModuleLayout module="gate" :show-attrs="true" :show-filter="true" :single-select="true" plots-label="Gating">
    <template #plots="{ selectedUids, orderedUids, selectUids }">
      <GatingPlots :image-uid="selectedUids[0] ?? null" :ordered-uids="orderedUids" :select-uids="selectUids" />
    </template>
  </ModuleLayout>
</template>
