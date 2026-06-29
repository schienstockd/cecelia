<!--
  Gating module page. Pick ONE image in the table; the gating plots render BELOW the table
  (wide left column) as a modular grid (count via slider), with a page-level segmentation
  selector and the floating Population Manager. Mirrors the old flowPlotManager layout.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import GatingPlots from './gate/GatingPlots.vue'
</script>

<template>
  <ModuleLayout module="gate" :show-attrs="true" :show-filter="true" :single-select="true">
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Gating" :max-height="'none'">
        <GatingPlots :image-uid="selectedUids[0] ?? null" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
