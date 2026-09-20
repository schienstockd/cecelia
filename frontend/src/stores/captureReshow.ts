// One pending "reshow this capture on a module page" bag.
//
// Same shape as `viewer.setPendingViewState` — Kiwi writes it, the destination page reads it on
// mount (or on the next update), and it clears itself once consumed. Held in a store rather than
// query params because the payload is heavy (a PNG data URL plus the whole envelope), and the
// destination is same-app-window so a store is the honest carrier.
//
// SummaryCanvas is the first consumer (behaviour/phenotype/cluster* module pages). Kiwi is the
// writer: refocus on a `plot` capture → set the bag + router.push to `moduleRouteFor(module)`.
//
// Consumption model: a page reads `pending` on mount + on `module` change. If the bag names
// this page's module, it consumes (clears the bag) and mounts CaptureViewSurface. Any staler
// bag whose module doesn't match sits until a new one lands or the page it's for consumes it.

import { defineStore, acceptHMRUpdate } from 'pinia'
import { computed, ref } from 'vue'
import type { CaptureEnvelope } from '../utils/kiwiCaptures'

export interface CaptureReshowBag {
  /** The module tag (`behaviourAnalysis`, `phenotype`, …) — matched against the consumer page's
   *  own `module` prop. Comes from the capture's `address.plotSpec.params.module`. */
  module: string
  /** The full envelope for the destination to mount over its canvas. `panels[]` and marks come
   *  along inside, so the consumer doesn't need a second fetch. */
  envelope: CaptureEnvelope
}

export const useCaptureReshowStore = defineStore('captureReshow', () => {
  const pending = ref<CaptureReshowBag | null>(null)
  const hasPending = computed(() => pending.value !== null)

  function setPending(bag: CaptureReshowBag) { pending.value = bag }
  function clear() { pending.value = null }

  /** Consume the bag IF it names `module`. Returns the envelope or null. Idempotent — a second
   *  call in the same mount cycle returns null. Prevents a mounted SummaryCanvas from re-mounting
   *  a reshow when its parent re-renders. */
  function consumeFor(module: string): CaptureEnvelope | null {
    const bag = pending.value
    if (!bag || bag.module !== module) return null
    pending.value = null
    return bag.envelope
  }

  return { pending, hasPending, setPending, clear, consumeFor }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useCaptureReshowStore, import.meta.hot))
