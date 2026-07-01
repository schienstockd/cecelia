<!--
  Universal "Analysis canvas" page (TODO #00036). The SAME summary-plot canvas as the per-module
  pages, but with the MODULE FILTER OFF: `SummaryCanvas` mounted with no `module` prop offers EVERY
  plot spec, so plots can be combined across modules / images / segmentations on one board. Pick one
  OR MORE images from the active set (multi-select); the populations endpoint unions across the
  selected images/segmentations.

  No TaskRunner (`#right`) — this page only visualises existing results, it doesn't run tasks. Image
  selection + panel state persist under their own scopes (`analysis` for the selection, `summary:universal`
  inside SummaryCanvas), independent of the per-module pages. See docs/UI.md → "Analysis-plot canvas".
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
</script>

<template>
  <ModuleLayout module="analysis" :show-attrs="true" :show-filter="true">
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Plots" :max-height="'none'">
        <SummaryCanvas :image-uids="selectedUids" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
