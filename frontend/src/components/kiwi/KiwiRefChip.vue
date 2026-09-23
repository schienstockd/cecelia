<script setup lang="ts">
// One KiwiRef as a chip — the same chip in the prompt box (attached, removable) and on a claim
// (evidence). Clicking it points at the thing (`composables/useKiwiPoint`). Its look says how far the
// ref was checked and no further (plan Decision 10): solid = exists, dashed = only true while open /
// only checkable when shown, red = doesn't resolve or wasn't looked at. Nothing renders "supports the
// claim" — nothing checks that yet.
import { computed } from 'vue'
import type { KiwiRef, KiwiRefResult } from '../../utils/kiwiRef'
import { chipState, refLabel } from '../../utils/kiwiTurn'
import { useKiwiPoint } from '../../composables/useKiwiPoint'
import { useToast } from 'primevue/usetoast'

const props = defineProps<{
  kiwiRef: KiwiRef
  result?: KiwiRefResult
  seen?: boolean
  removable?: boolean
}>()
const emit = defineEmits<{ (e: 'remove'): void }>()

const state = computed(() => chipState(props.result, props.seen))
const label = computed(() => (props.result?.ok && props.result.label) ? props.result.label : refLabel(props.kiwiRef))

const { pointAt } = useKiwiPoint()
const toast = useToast()
async function click() {
  const why = await pointAt(props.kiwiRef, label.value)
  if (why) toast.add({ severity: 'info', summary: why, life: 3000 })
}
</script>

<template>
  <span class="kiwi-ref" :class="`kiwi-ref-${state.tone}`">
    <button class="kiwi-ref-main cc-btn cc-btn-bare cc-fs-2xs" @click="click"
            v-tooltip.bottom="state.tip">
      <span class="kiwi-ref-kind">{{ kiwiRef.kind }}</span>
      <span class="kiwi-ref-label">{{ label }}</span>
    </button>
    <button v-if="removable" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="emit('remove')"
            v-tooltip.bottom="'Remove'">
      <i class="pi pi-times" />
    </button>
  </span>
</template>

<style scoped>
.kiwi-ref { display: inline-flex; align-items: center; max-width: 100%; min-width: 0;
            border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill); padding-right: 0.1rem; }
.kiwi-ref-soft { border-style: dashed; }
.kiwi-ref-fail { border-color: var(--cc-sev-fail); }
.kiwi-ref-main { display: inline-flex; gap: 0.3rem; min-width: 0; padding: 0.05rem 0.45rem; }
.kiwi-ref-kind { color: var(--cc-kiwi); }
.kiwi-ref-label { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
