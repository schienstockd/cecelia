<!--
  Phenotype module page — the analysis counterpart to Gate, exactly as Behaviour is to Track. Pick one
  OR MORE images (multi-select), then phenotype the GATED populations in the summary-plot canvas below
  the table: counts / proportion of each gated population, per image or pooled. Per-module canvas: only
  `phenotype` plot specs are offered (popType `flow` — any gated cell, not just the derived `live`).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
</script>

<template>
  <ModuleLayout module="phenotype" :show-attrs="true" :show-filter="true">
    <template #plots="{ selectedUids }">
      <SummaryCanvas :image-uids="selectedUids" module="phenotype" />
    </template>
  </ModuleLayout>
</template>
