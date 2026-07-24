<!--
  CropDialog — the in-app crop, opened from the Crop action on each ImageTable row. Wraps CropPanel
  (coloured MIP + rectangle draw + z/t + Save → editImages.cropImage) in a modal. Napari-free: the MIP
  is rendered server-side (Julia /api/crop/*), so cropping doesn't require opening the napari viewer.
  See docs/todo/CROP_PANEL_PLAN.md.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import CropPanel from './CropPanel.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import type { CciaImage } from '../stores/project'

const props = defineProps<{ image: CciaImage; setUid: string }>()
defineEmits<{ (e: 'close'): void }>()

const projectMeta = useProjectMetaStore()

// Every registered version/iteration of this image is a valid crop source (drift/AF/cellpose-corrected
// variants are the norm, not just 'default'). Offer them all; default to the active one. Mirrors the
// version selector in ViewerPanel.vue. Empty → the backend falls back to 'default'.
const valueNames = computed(() => Object.keys(props.image.filepaths ?? {}))
const selectedValueName = ref(defaultValueName())
function defaultValueName(): string {
  const names = valueNames.value
  const nonDefault = names.filter(n => n !== 'default')
  return props.image.activeValueName && names.includes(props.image.activeValueName) ? props.image.activeValueName
    : nonDefault.length > 0 ? nonDefault[nonDefault.length - 1]
    : names.includes('default') ? 'default' : (names[0] ?? '')
}
</script>

<template>
  <BaseModal width="640px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-image" /> Crop — {{ image.name }}
      <span v-if="selectedValueName" class="crop-version-tag">{{ selectedValueName }}</span>
    </template>
    <div v-if="valueNames.length > 1" class="crop-version-row">
      <span class="crop-version-lbl" v-tooltip.right="'Which image version/iteration to crop'">Version</span>
      <select v-model="selectedValueName" class="crop-version-select">
        <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
      </select>
    </div>
    <CropPanel :project-uid="projectMeta.current?.uid ?? ''"
               :image-uid="image.uid"
               :image-name="image.name"
               :value-name="selectedValueName"
               :set-uid="setUid" />
  </BaseModal>
</template>

<style scoped>
.crop-version-row { display: flex; align-items: center; gap: 0.5rem; margin-bottom: 0.5rem; }
.crop-version-lbl { color: var(--cc-text-muted, #888); font-size: 0.7rem; }
.crop-version-select { flex: 0 1 auto; }
.crop-version-tag {
  margin-left: 0.4rem; padding: 0.05rem 0.4rem; border-radius: 0.5rem;
  font-size: 0.62rem; font-weight: 600; vertical-align: middle;
  color: var(--cc-accent, #a855f7);
  background: color-mix(in srgb, var(--cc-accent, #a855f7) 15%, transparent);
}
</style>
