<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import MetadataPanel from './metadata/MetadataPanel.vue'
</script>

<template>
  <ModuleLayout module="metadata" :show-attrs="true" :editable-meta="true" :show-filter="false">
    <template #right="{ setUid, selectedUids }">
      <MetadataPanel :setUid="setUid" :selectedUids="selectedUids" />
    </template>
  </ModuleLayout>
</template>
