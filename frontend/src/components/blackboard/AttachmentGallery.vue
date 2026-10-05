<script setup lang="ts">
// The pictures of a Blackboard entry, full size, one at a time — for attachments that cannot be
// refocused in the viewer (an agent run's evidence lives in a disposable copy, not this project).
// ← / → step through; Esc closes (BaseModal). The dialog and the picture box are a fixed size, so the
// arrows stay put whatever the picture's shape; the picture scales to fit inside the box.
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
             height="min(90vh, 820px)" @close="emit('close')">
    <div v-if="cur" class="ag">
      <div class="ag-stage cc-row">
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="items.length < 2" @click="step(-1)"
                v-tooltip.top="'Previous picture'"><i class="pi pi-chevron-left" /></button>
        <div class="ag-frame">
          <img v-if="cur.src" :src="cur.src" :alt="cur.id" class="ag-img" />
          <span v-else class="cc-muted"><i class="pi pi-spin pi-spinner" /></span>
        </div>
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="items.length < 2" @click="step(1)"
                v-tooltip.top="'Next picture'"><i class="pi pi-chevron-right" /></button>
      </div>
      <p class="ag-caption cc-fs-xs">{{ cur.caption }}</p>
      <p class="ag-id cc-fs-2xs cc-muted">{{ cur.id }}</p>
    </div>
  </BaseModal>
</template>

<style scoped>
.ag { height: 100%; display: flex; flex-direction: column; }
.ag-stage { flex: 1; min-height: 0; align-items: center; gap: 8px; }
.ag-stage > .cc-btn { flex-shrink: 0; }
.ag-frame { flex: 1; min-width: 0; height: 100%; display: flex; align-items: center; justify-content: center; }
.ag-img { max-width: 100%; max-height: 100%; object-fit: contain; border: 1px solid var(--cc-border); }
/* a fixed caption band (scrolls when long), so a long caption doesn't shrink the picture box */
.ag-caption { height: 3.2em; overflow-y: auto; margin: 8px 0 2px; color: var(--cc-text); text-align: center; flex-shrink: 0; }
.ag-id { margin: 0; text-align: center; font-family: var(--cc-mono); }
</style>
