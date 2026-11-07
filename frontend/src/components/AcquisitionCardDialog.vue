<!--
  Preview + print of an image's acquisition settings card (AcquisitionCardSheet). Printing goes
  through the browser ("Save as PDF" in its print dialog): while this dialog is open a second copy of
  the sheet is teleported to <body>, hidden on screen, and the print stylesheet below hides
  everything else — so only the card prints, on one A4 page.
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from './BaseModal.vue'
import AcquisitionCardSheet from './AcquisitionCardSheet.vue'
import ComingSoonBadge from './ComingSoonBadge.vue'
import { acquisitionCard, type CardImage } from '../utils/acquisitionCard'

const props = defineProps<{ image: CardImage; projectUid: string }>()
const emit = defineEmits<{ (e: 'close'): void }>()

const today = () => {
  const d = new Date()
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}-${String(d.getDate()).padStart(2, '0')}`
}
const print = () => window.print()
const card = computed(() => acquisitionCard(props.image, { projectUid: props.projectUid, printedOn: today() }))
</script>

<template>
  <BaseModal width="calc(186mm + 48px)" @close="emit('close')">
    <template #title><i class="pi pi-print" /> Settings card <ComingSoonBadge tier="experimental" /></template>
    <div class="acd-preview"><AcquisitionCardSheet :card="card" /></div>
    <template #footer>
      <button class="cc-btn cc-btn-primary" v-tooltip="'Print, or save as PDF from the print dialog'" @click="print">
        <i class="pi pi-print" /> Print
      </button>
    </template>
  </BaseModal>
  <Teleport to="body">
    <div class="cc-print-card"><AcquisitionCardSheet :card="card" /></div>
  </Teleport>
</template>

<style scoped>
.acd-preview { background: #fff; padding: 12px; border-radius: var(--cc-radius-sm); overflow: auto; }
</style>

<style>
/* Not scoped: the print rule has to reach <body>'s other children. */
@media screen { .cc-print-card { display: none; } }
@media print {
  @page { size: A4; margin: 12mm; }
  body > *:not(.cc-print-card) { display: none !important; }
  .cc-print-card { display: block; }
}
</style>
