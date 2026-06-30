<!--
  Cluster tracks module page. Pick one OR MORE images (multi-select), then:
   • run track clustering (clustTracks.cluster) in the right-hand TaskRunner — this is SET-SCOPE
     (Leiden over the pooled set's tracks, so cluster IDs are comparable across images). Features =
     celltrackR motility ⊕ HMM-state/transition frequencies ⊕ per-track cell-measure aggregates;
   • below the table: explore clusters (heatmap + UMAP) and define populations by ticking cluster
     IDs into them (the `trackclust` pop type). [pop-manager + plots are filled in the next step]

  Track counterpart of ClusterCellsModule.vue (one page per granularity, mirroring gate/track).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import ClusterUmapPanel from '../components/cluster/ClusterUmapPanel.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: clustDefs, reload: reloadDefs } = useTaskDefs('clustTracks')
</script>

<template>
  <ModuleLayout module="clustTracks" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="clustDefs"
        :on-reload-defs="reloadDefs"
        module="clustTracks"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="UMAP" :max-height="'none'">
        <ClusterUmapPanel :image-uids="selectedUids" pop-type="trackclust" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
