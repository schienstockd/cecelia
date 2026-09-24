<script setup lang="ts">
// One KiwiRef on a claim (evidence) — a plain `.cc-btn` that points at the thing when clicked
// (`composables/useKiwiPoint`). Attached refs are rows, not chips (`KiwiAsk`'s attachment table).
// How far the ref was checked, and no further (plan Decision 10): normal = exists; dim = only true
// while open / only checkable when shown; error text = doesn't resolve or wasn't looked at. The
// tooltip says which. Nothing renders "supports the claim" — nothing checks that yet.
import { computed } from 'vue'
import type { KiwiRef, KiwiRefResult } from '../../utils/kiwiRef'
import { chipState, refLabel, kindLabel } from '../../utils/kiwiTurn'
import { useKiwiPoint } from '../../composables/useKiwiPoint'

// `seen` defaults to undefined, NOT false: Vue casts an absent boolean prop to false, and false means
// "cited without being looked at" — every attached ref on a past turn rendered as a failure
const props = withDefaults(defineProps<{
  kiwiRef: KiwiRef
  result?: KiwiRefResult
  seen?: boolean
}>(), { seen: undefined })

const state = computed(() => chipState(props.result, props.seen))
const label = computed(() => (props.result?.ok && props.result.label) ? props.result.label : refLabel(props.kiwiRef))
const tone = computed(() => ({ ok: '', soft: 'cc-muted', fail: 'cc-muted-error' })[state.value.tone])

const { pointAt } = useKiwiPoint()
const click = () => void pointAt(props.kiwiRef, label.value, props.result)
</script>

<template>
  <button class="kiwi-ref cc-btn cc-btn-ghost cc-fs-2xs" :class="tone" @click="click"
          v-tooltip.bottom="state.tip">
    <span class="cc-muted cc-fs-2xs">{{ kindLabel(kiwiRef.kind, result) }}</span>
    <span class="kiwi-ref-label">{{ label }}</span>
  </button>
</template>

<style scoped>
.kiwi-ref { max-width: 100%; min-width: 0; }
.kiwi-ref-label { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
