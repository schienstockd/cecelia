<!--
  Cluster cells module page. Pick one OR MORE images (multi-select), then:
   • run cell clustering (clustPops.cluster) in the right-hand TaskRunner — this is SET-SCOPE
     (Leiden over the pooled set, so cluster IDs are comparable across images);
   • below the table: explore clusters (heatmap + UMAP) and define populations by ticking cluster
     IDs into them (the `clust` pop type). [pop-manager + plots are filled in the next step]

  Mirrors BehaviourModule (set-scope task + below-table canvas); the track counterpart is
  ClusterTracksModule.vue (gating/tracking-style split — one page per granularity).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import ClusterPlots from './cluster/ClusterPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: clustDefs, reload: reloadDefs } = useTaskDefs('clustPops')
</script>

<template>
  <ModuleLayout module="clustPops" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="clustDefs"
        :on-reload-defs="reloadDefs"
        module="clustPops"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #below-table="{ selectedUids, selectUids }">
      <CollapsibleSection label="Clusters" :max-height="'none'">
        <ClusterPlots :image-uids="selectedUids" :select-uids="selectUids" pop-type="clust" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
