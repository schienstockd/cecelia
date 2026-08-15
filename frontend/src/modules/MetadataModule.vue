<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import MetadataPanel from './metadata/MetadataPanel.vue'
</script>

<template>
  <!-- the panel's own width bounds, from back when it owned its width: its labelled fields and channel
       rows stop being usable below ~260, and it has nothing to spend more than ~520 on -->
  <ModuleLayout module="metadata" :show-attrs="true" :editable-meta="true" :show-filter="false"
                :right-min-width="260" :right-max-width="520">
    <template #right="{ setUid, selectedUids }">
      <MetadataPanel :setUid="setUid" :selectedUids="selectedUids" />
    </template>
  </ModuleLayout>
</template>
