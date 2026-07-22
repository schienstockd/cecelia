<!--
  CropDialog — the in-app crop, opened from the Crop action on each ImageTable row. Wraps CropPanel
  (coloured MIP + rectangle draw + z/t + Save → editImages.cropImage) in a modal. Napari-free: the MIP
  is rendered server-side (Julia /api/crop/*), so cropping doesn't require opening the napari viewer.
  See docs/todo/CROP_PANEL_PLAN.md.
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import CropPanel from './CropPanel.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import type { CciaImage } from '../stores/project'

const props = defineProps<{ image: CciaImage; setUid: string }>()
defineEmits<{ (e: 'close'): void }>()

const projectMeta = useProjectMetaStore()
</script>

<template>
  <BaseModal width="640px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-clone" /> Crop — {{ image.name }}
    </template>
    <!-- crop the ACTIVE version (drift/AF/cellpose-corrected variants are the norm, not 'default');
         empty → the backend falls back to 'default' -->
    <CropPanel :project-uid="projectMeta.current?.uid ?? ''"
               :image-uid="image.uid"
               :image-name="image.name"
               :value-name="image.activeValueName ?? ''"
               :set-uid="setUid" />
  </BaseModal>
</template>
