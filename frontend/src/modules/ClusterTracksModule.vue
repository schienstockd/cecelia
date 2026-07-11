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
import TaskRunner from '../tasks/TaskRunner.vue'
import ClusterPlots from './cluster/ClusterPlots.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: clustDefs, reload: reloadDefs } = useTaskDefs('clustTracks')
</script>

<template>
  <ModuleLayout module="clustTracks" :show-attrs="true" :show-filter="true" plots-label="Clusters">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="clustDefs"
        :on-reload-defs="reloadDefs"
        module="clustTracks"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #plots="{ selectedUids, selectUids }">
      <ClusterPlots :image-uids="selectedUids" :select-uids="selectUids" pop-type="trackclust" />
    </template>
    <!-- population summary of the track-cluster pops (counts / proportion per image; boxplot/beeswarm/bar)
         — same backbone as Phenotype/Behaviour, popType trackclust so it's homogeneous here -->
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Population summary" max-height="none" :storage-key="'cc-popsum-open:clustTracks'">
        <SummaryCanvas :image-uids="selectedUids" module="clustTracks" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
