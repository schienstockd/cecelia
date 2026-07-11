<!--
  Cluster cells module page. Pick one OR MORE images (multi-select), then:
   • run cell clustering (clustPops.cluster) in the right-hand TaskRunner — this is SET-SCOPE
     (Leiden over the pooled set, so cluster IDs are comparable across images);
   • below the table: explore clusters (heatmap + UMAP) and define populations by ticking cluster
     IDs into them (the `clust` pop type). [pop-manager + plots are filled in the next step]

  Mirrors BehaviourModule (set-scope task + #plots canvas); the track counterpart is
  ClusterTracksModule.vue (gating/tracking-style split — one page per granularity).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import ClusterPlots from './cluster/ClusterPlots.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: clustDefs, reload: reloadDefs } = useTaskDefs('clustPops')
</script>

<template>
  <ModuleLayout module="clustPops" :show-attrs="true" :show-filter="true" plots-label="Clusters">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="clustDefs"
        :on-reload-defs="reloadDefs"
        module="clustPops"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #plots="{ selectedUids, selectUids }">
      <ClusterPlots :image-uids="selectedUids" :select-uids="selectUids" pop-type="clust" />
    </template>
    <!-- population summary of the cluster pops (counts / proportion per image; boxplot/beeswarm/bar) —
         same backbone as Phenotype/Behaviour, popType clust so it's homogeneous here -->
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Population summary" max-height="none" :storage-key="'cc-popsum-open:clustPops'">
        <SummaryCanvas :image-uids="selectedUids" module="clustPops" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
