<!--
  Universal, multipage "Analysis board" page (TODO #00036 + docs/todo/ANALYSIS_CANVAS_PLAN.md). The
  SAME summary-plot canvas as the per-module pages, but with the MODULE FILTER OFF: each board is a
  `SummaryCanvas` mounted with no `module` prop, so it offers EVERY plot spec — plots can be combined
  across modules / images / segmentations. `TabbedCanvas` wraps N independent boards (Phase A). Pick
  one OR MORE images from the active set (multi-select, shared across boards); the populations endpoint
  unions across the selected images/segmentations.

  No TaskRunner (`#right`) — this page only visualises existing results. Image selection persists under
  the `analysis` scope; each board's panels under `analysis:{projectUid}:tab:{id}`. `TabbedCanvas` is
  keyed by projectUid so a project switch remounts it. See docs/UI.md → "Analysis-plot canvas".
-->
<script setup lang="ts">
import { computed } from 'vue'
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import TabbedCanvas from '../components/canvas/TabbedCanvas.vue'
import { useProjectMetaStore } from '../stores/projectMeta'

const meta = useProjectMetaStore()
const projectUid = computed(() => meta.current?.uid ?? '')
</script>

<template>
  <ModuleLayout module="analysis" :show-attrs="true" :show-filter="true">
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Plots" :max-height="'none'">
        <TabbedCanvas :key="projectUid" :image-uids="selectedUids" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
