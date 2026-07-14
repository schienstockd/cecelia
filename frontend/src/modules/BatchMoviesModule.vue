<!--
  Batch movies page (/batch-movies): pick images, author one movie config, generate one attr-named mp4
  per image on the shared napari viewer. See batchmovies/BatchMoviesPanel.vue and
  docs/todo/ANIMATION_PLAN.md → F1.3. Multi-select is ModuleLayout's default; attrs shown so the user
  understands the file-naming attributes.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import BatchMoviesPanel from './batchmovies/BatchMoviesPanel.vue'
</script>

<template>
  <ModuleLayout module="batchMovies" :show-attrs="true" :show-filter="true" plots-label="Movies">
    <template #right="{ selectedUids, selectedNames }">
      <BatchMoviesPanel :selected-uids="selectedUids" :selected-names="selectedNames" />
    </template>
  </ModuleLayout>
</template>
