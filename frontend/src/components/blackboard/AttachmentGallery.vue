<script setup lang="ts">
// The pictures of a Blackboard entry, full size, one at a time — for attachments that cannot be
// refocused in the viewer (an agent run's evidence lives in a disposable copy, not this project).
// ← / → step through; Esc closes (BaseModal).
import { ref, computed } from 'vue'
import BaseModal from '../BaseModal.vue'
import { useWindowListener } from '../../composables/useKeepAlive'

const props = defineProps<{ items: { id: string; src: string; caption: string }[]; start: number }>()
const emit = defineEmits<{ close: [] }>()

const i = ref(Math.min(Math.max(props.start, 0), Math.max(props.items.length - 1, 0)))
const cur = computed(() => props.items[i.value])
function step(d: number) {
  const n = props.items.length
  if (n) i.value = (i.value + d + n) % n
}
function onKey(e: KeyboardEvent) {
  if (e.key === 'ArrowLeft') { step(-1); e.preventDefault() }
  if (e.key === 'ArrowRight') { step(1); e.preventDefault() }
}
useWindowListener('keydown', onKey)
</script>

<template>
  <BaseModal :title="`Picture ${i + 1} of ${items.length}`" icon="pi-images" width="min(92vw, 900px)"
             @close="emit('close')">
    <div v-if="cur" class="ag">
      <div class="ag-stage cc-row">
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="items.length < 2" @click="step(-1)"
                v-tooltip.top="'Previous picture'"><i class="pi pi-chevron-left" /></button>
        <img v-if="cur.src" :src="cur.src" :alt="cur.id" class="ag-img" />
        <span v-else class="cc-muted"><i class="pi pi-spin pi-spinner" /></span>
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="items.length < 2" @click="step(1)"
                v-tooltip.top="'Next picture'"><i class="pi pi-chevron-right" /></button>
      </div>
      <p v-if="cur.caption" class="ag-caption cc-fs-xs">{{ cur.caption }}</p>
      <p class="ag-id cc-fs-2xs cc-muted">{{ cur.id }}</p>
    </div>
  </BaseModal>
</template>

<style scoped>
.ag-stage { justify-content: center; align-items: center; gap: 8px; }
.ag-img { max-width: 100%; max-height: 70vh; image-rendering: auto; border: 1px solid var(--cc-border); }
.ag-caption { margin: 8px 0 2px; color: var(--cc-text); text-align: center; }
.ag-id { margin: 0; text-align: center; font-family: var(--cc-mono); }
</style>
