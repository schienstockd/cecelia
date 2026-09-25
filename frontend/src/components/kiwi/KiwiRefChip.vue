<script setup lang="ts">
// One KiwiRef on a claim (evidence) — a plain `.cc-btn` that points at the thing when clicked
// (`composables/useKiwiPoint`). Attached refs are rows, not chips (`KiwiAsk`'s attachment table).
// How far the ref was checked, and no further (plan Decision 10): normal = exists; dim = only true
// while open / only checkable when shown; error text = doesn't resolve or wasn't looked at. The
// tooltip says which. Nothing renders "supports the claim" — nothing checks that yet.
//
// KIWI_CAPTURE_AND_BLACKBOARD_PLAN Decision 10 — a chip rendered on a saved Blackboard entry can
// carry a `sidecar` (its frozen label + kind-specific snapshot). When live resolution fails AND a
// sidecar is present, the chip falls back to `was: <label>` (with the snapshot on hover) rather
// than a bare "gone" state, so a saved turn stays checkable months later even if the underlying
// plot / population is gone.
import { computed } from 'vue'
import type { KiwiRef, KiwiRefResult } from '../../utils/kiwiRef'
import type { KiwiRefSidecar } from '../../utils/kiwiTurnSave'
import { chipState, refLabel, kindLabel } from '../../utils/kiwiTurn'
import { useKiwiPoint } from '../../composables/useKiwiPoint'

// `seen` defaults to undefined, NOT false: Vue casts an absent boolean prop to false, and false means
// "cited without being looked at" — every attached ref on a past turn rendered as a failure
const props = withDefaults(defineProps<{
  kiwiRef: KiwiRef
  result?: KiwiRefResult
  seen?: boolean
  /** Optional frozen sidecar (a saved Blackboard entry hands its `kiwiRefs[refKey]` here). Enables
   *  the "was: <label>" fallback when the live resolver says the underlying object is gone. */
  sidecar?: KiwiRefSidecar
}>(), { seen: undefined })

const state = computed(() => chipState(props.result, props.seen))
// `hasSidecar` gates the fallback rendering — only fire on a chip that was actually saved.
const hasSidecar = computed(() => !!props.sidecar)
// A sidecar fallback fires when live resolution says the object isn't there. `state.tone === 'fail'`
// is the resolver's own signal for that (see `chipState`).
const usingSidecar = computed(() => hasSidecar.value && state.value.tone === 'fail')
const label = computed(() => {
  if (props.result?.ok && props.result.label) return props.result.label
  if (usingSidecar.value) return `was: ${props.sidecar!.label}`
  return refLabel(props.kiwiRef)
})
const tone = computed(() => usingSidecar.value
  ? 'cc-muted'
  : ({ ok: '', soft: 'cc-muted', fail: 'cc-muted-error' })[state.value.tone])
const tip = computed(() => usingSidecar.value ? sidecarTip(props.sidecar!) : state.value.tip)

function sidecarTip(s: KiwiRefSidecar): string {
  const snap = s.snapshot
  if (!snap) return `Was: ${s.label}`
  switch (snap.kind) {
    case 'population': return `Was: ${snap.label} in ${snap.imageName}`
    case 'cells':      return `Was: ${snap.count} cell(s) in ${snap.imageName}`
    case 'tracks':     return `Was: ${snap.count} track(s) in ${snap.imageName}`
    case 'plot':       return `Was: ${snap.label}\n${snap.plotSummary}`
    case 'tile':       return `Was: tile ${snap.cellId} in ${snap.imageName}`
    case 'ui':         return `Was: ${snap.anchor}`
  }
}

const { pointAt } = useKiwiPoint()
const click = () => {
  // A sidecar fallback isn't clickable — the underlying object is gone; there's nothing to point at.
  if (usingSidecar.value) return
  void pointAt(props.kiwiRef, label.value, props.result)
}
</script>

<template>
  <button class="kiwi-ref cc-btn cc-btn-ghost cc-fs-2xs" :class="tone"
          :disabled="usingSidecar" @click="click"
          v-tooltip.bottom="tip">
    <span class="cc-muted cc-fs-2xs">{{ kindLabel(kiwiRef.kind, result) }}</span>
    <span class="kiwi-ref-label">{{ label }}</span>
  </button>
</template>

<style scoped>
.kiwi-ref { max-width: 100%; min-width: 0; }
.kiwi-ref-label { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
